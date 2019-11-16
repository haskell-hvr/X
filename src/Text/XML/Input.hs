{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- |
-- Module    : Text.XML.Input
-- Copyright : (c) Galois, Inc. 2007
--             (c) Herbert Valerio Riedel 2019
-- SPDX-License-Identifier: BSD-3-Clause AND GPL-3.0-or-later
--
-- Lightweight XML parsing
--

module Text.XML.Input
    ( -- * High-level DOM Parser
      parseXML
    , parseXMLDoc
    , parseXMLRoot

      -- * Token Scanner
    , XmlSource(uncons)
    , Scanner, customScanner

    , Token(..), scanXML
    ) where

import           Common
import           Utils

import           Text.XML.Lexer
import           Text.XML.NS
import           Text.XML.Types

import qualified Data.Text       as T
import qualified Data.Text.Short as TS

-- | Parse a XML document to an 'Element'
--
-- If you need access to the prolog and epilog use 'parseXMLRoot'
--
-- An optional (single) leading BOM (@U+FEFF@) character will be discard (and not counted in the source positions).
parseXMLDoc :: XmlSource s => s -> Either (Pos,String) Element
parseXMLDoc xs0 = rootElement <$> parseXMLRoot xs0

-- | Parse a XML document
--
-- An optional (single) leading BOM (@U+FEFF@) character will be discard (and not counted in the source positions).
parseXMLRoot :: XmlSource s => s -> Either (Pos,String) Root
parseXMLRoot xs0 = do
    (rootXmlDeclaration,ts1) <- case ts0 of
      TokXmlDecl xd : rest -> pure (Just xd, rest)
      rest                 -> pure (Nothing, rest)

    (rootPreElem,ts2) <- mnodes ts1

    (rootDoctype,ts3) <- case ts2 of
      TokDTD dtd : ts3a -> do
        (ns,rest) <- mnodes ts3a
        pure (Just (dtd,ns), rest)
      rest -> pure (Nothing, rest)

    (rootElement,ts4) <- case ts3 of
      TokStart {} : _ -> case parse ts3 of
        ElemF el : rest -> case traverse fromContentF el of
                                Right e' -> pure (e',rest)
                                Left err -> Left err
        Failure pos msg : _ -> Left (pos,msg)
        _                   -> Left (-1,"empty document (i.e. missing root element)")

      _:_ -> Left (-1,"unexpected (non-misc) content nodes before root element")
      [] -> Left (-1,"empty document (i.e. missing root element)")

    (rootPostElem,ts5) <- mnodes2 ts4

    case ts5 of
      []    -> pure Root{..}
      (_:_) -> Left (-1,"unexpected (non-misc) content nodes after root element")
  where
    ts0 = scanXML (dropBOM xs0)

    -- mnodes :: [Token] -> Either _ ([MiscNodes],[Token])

    mnodes = go []
      where
        go _   (TokError n e : _)    = Left (n,e)
        go acc (TokComment x : rest) = go (Left x:acc) rest
        go acc (TokPI _ x : rest)    = go (Right x:acc) rest
        go acc (TokText cdata : rest)
          | isWsCdata cdata          = go acc rest
        go acc xs                    = pure (reverse acc, xs)


    mnodes2 = go []
      where
        go _   (Failure n e : _)     = Left (n,e)
        go acc (CommF x : rest)      = go (Left x:acc) rest
        go acc (ProcF x : rest)      = go (Right x:acc) rest
        go acc (TextF cdata : rest)
          | isWsCdata cdata          = go acc rest
        go acc xs                    = pure (reverse acc, xs)


isWsCdata :: CData -> Bool
isWsCdata (CData _ t) = T.all isS t

-- | parseXML to a list of 'Content' chunks
--
-- __NOTE__: As opposed to 'parseXMLDoc', this function will /not/ discard any BOM characters.
parseXML :: XmlSource s => s -> Either (Pos,String) [Content]
parseXML = traverse fromContentF . parse . scanXML

-- | Drop a single leading @U+FEFF@ character
dropBOM :: XmlSource s => s -> s
dropBOM s0 = case uncons s0 of
               Just ('\xFEFF',s1) -> s1
               Just _             -> s0
               Nothing            -> s0

------------------------------------------------------------------------

-- | Variant of 'Content' that can encode parser 'Failure's
data ContentF
  = ElemF (Element' ContentF)
  | TextF CData
  | CRefF !ShortText
  | ProcF PI
  | CommF Comment
  | Failure !Int String
  deriving (Show, Typeable, Data, Generic)

instance NFData ContentF

fromContentF :: ContentF -> Either (Pos,String) Content
fromContentF (CRefF ref)       = Right (CRef ref)
fromContentF (TextF cd)        = Right (Text cd)
fromContentF (ProcF x)         = Right (Proc x)
fromContentF (CommF x)         = Right (Comm x)
fromContentF (ElemF el)        = Elem <$> traverse fromContentF el
fromContentF (Failure pos err) = Left (pos,err)

------------------------------------------------------------------------

parse :: [Token] -> [ContentF]
parse [] = []
parse ts = let (es,_,ts1) = nodes nsinfo0 [] ts
           in es ++ parse ts1

-- Information about namespaces.
-- The first component is a map that associates prefixes to URIs,
-- the second is the URI for the default namespace, if one was provided.
type NSInfo = ([(ShortText,URI)],URI)

nsinfo0 :: NSInfo
nsinfo0 = ([("xml",xmlNamesNS),("xmlns",xmlnsNS)],nullNs)

nodes :: NSInfo -> [QName] -> [Token] -> ([ContentF], [QName], [Token])
nodes ns ps (TokError pos msg : _) =
  let (es,qs,ts1) = nodes ns ps []
  in (Failure pos msg : es, qs, ts1)

-- TODO
nodes ns ps (TokXmlDecl _ : ts) = nodes ns ps ts
nodes ns ps (TokDTD _     : ts) = nodes ns ps ts

nodes ns ps (TokCRef ref : ts) =
  let (es,qs,ts1) = nodes ns ps ts
  in (CRefF ref : es, qs, ts1)

nodes ns ps (TokComment x : ts) =
  let (es,qs,ts1) = nodes ns ps ts
  in (CommF x : es, qs, ts1)

nodes ns ps (TokPI _ x : ts) =
  let (es,qs,ts1) = nodes ns ps ts
  in (ProcF x : es, qs, ts1)

nodes ns ps (TokText txt : ts) =
  let (es,qs,ts1) = nodes ns ps ts
      (more,es1)  = case es of
                      TextF cd : es1'
                        | cdVerbatim cd == cdVerbatim txt -> (cdData cd,es1')
                      _                                   -> (mempty,es)

  in (TextF txt { cdData = cdData txt `T.append` more } : es1, qs, ts1)

nodes cur_info ps (TokStart pos t as empty' : ts) = (node : siblings, open, toks)
  where
    new_name  = if nsfail then t else annotName new_info t
    prefixes  = filter (/= "xmlns") $ mapMaybe qPrefix (t : [ k | Attr k _ <- as ])
    nsfail    = any (==Nothing) [ lookup pfx (fst new_info) | pfx <- prefixes ]
    rsvnsfail = not (all checkNS as)
    new_info  = foldr addNS cur_info as
    node | rsvnsfail = Failure pos "invalid namespace declaration"
         | nsfail    = Failure pos "undefined namespace prefix"
         | otherwise =
                ElemF Element { elName    = new_name
                              , elAttribs = map (annotAttr new_info) as
                              , elContent = children
                              }

    (children,(siblings,open,toks))
      | empty'    = ([], nodes cur_info ps ts)
      | otherwise = let (es1,qs1,ts1) = nodes new_info (new_name:ps) ts
                    in (es1,
                        case qs1 of
                          []      -> nodes cur_info ps ts1
                          _ : qs3 -> ([],qs3,ts1))

nodes ns ps (TokEnd pos t : ts)
  = case ps of
      p1:_ | qLName t  == qLName p1
           , qPrefix t == qPrefix p1
           -> ([],[],ts)
      _ -> let (es,qs,ts1) = nodes ns ps ts
           in (Failure pos "start/end-tag mismatch" : es, qs, ts1)

nodes _ ps []
  = case ps of
      []  -> ([],[],[]) -- done
      _:_ -> ([Failure (-1) "premature eof before end-tag"], ps, [])

annotName :: NSInfo -> QName -> QName
annotName (namespaces,def_ns) n = n { qURI = lookupNs (qPrefix n) }
  where
    lookupNs Nothing    = def_ns
    lookupNs (Just pfx) = fromMaybe (error "annotName: the impossible") (lookup pfx namespaces)

annotAttr :: NSInfo -> Attr -> Attr
annotAttr ns a@(Attr { attrKey = k}) =
  case (qPrefix k, qLName k) of
    -- see https://www.w3.org/2000/xmlns/
    (Nothing, "xmlns") -> a { attrKey = k { qURI = xmlnsNS } }
    -- Do not apply the default name-space to unqualified
    -- attributes.  See Section 6.2 of <http://www.w3.org/TR/REC-xml-names>.
    (Nothing, _)       -> a
    _                  -> a { attrKey = annotName ns k }

addNS :: Attr -> NSInfo -> NSInfo
addNS (Attr { attrKey = key, attrVal = val }) (ns,def) =
  case (qPrefix key, qLName key) of
    (Nothing,"xmlns")     -> (ns, if T.null val then nullNs else (URI (TS.fromText val)))
    (Just "xmlns", "xml") -> (ns,def)
    (Just "xmlns", k)     -> ((unLName k, URI (TS.fromText val)) : ns, def)
    _                     -> (ns,def)

-- | Check rules imposed on reserved namespaces by https://www.w3.org/TR/xml-names/
checkNS :: Attr -> Bool
checkNS = \case
    (Attr (QName { qPrefix = Just "xmlns", qLName = "xmlns"}) _  ) -> False
    (Attr (QName { qPrefix = Just "xmlns", qLName = "xml"})   uri) -> uri == xmlNamesNS'
    (Attr (QName { qPrefix = Just "xmlns", qLName = _})       uri) -> not (T.null uri) && isNotRsvd uri
    (Attr (QName { qPrefix = Nothing     , qLName = "xmlns"}) "")  -> True
    (Attr (QName { qPrefix = Nothing     , qLName = "xmlns"}) uri) -> isNotRsvd uri
    _                                                              -> True
  where
    xmlNamesNS' = TS.toText (unURI xmlNamesNS)
    xmlnsNS'    = TS.toText (unURI xmlnsNS)
    isNotRsvd uri = not (uri == xmlNamesNS' || uri == xmlnsNS')

xmlNamesNS :: URI
xmlNamesNS = URI ns_xml_uri

xmlnsNS :: URI
xmlnsNS = URI ns_xmlns_uri
