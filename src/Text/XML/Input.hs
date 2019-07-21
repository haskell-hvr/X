{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

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

      -- * Token Scanner
    , XmlSource(uncons)
    , Scanner, customScanner

    , Token(..), scanXML
    ) where

import           Common

import           Text.XML.Lexer
import           Text.XML.Proc
import           Text.XML.Types

import qualified Data.Text       as T
import qualified Data.Text.Short as TS

-- | parseXMLDoc, parse a XML document to an 'Element'
parseXMLDoc :: XmlSource s => s -> Either (Pos,String) Element
parseXMLDoc xs0 = parseXML xs0 >>= strip
  where
    strip cs = case onlyElems cs of
                 e : es
                   | "?xml" `TS.isPrefixOf` unLName (qLName (elName e))
                     -> strip (map Elem es)
                   | otherwise -> Right e
                 [] -> Left (-1,"empty document")

-- | parseXML to a list of 'Content' chunks
parseXML :: XmlSource s => s -> Either (Pos,String) [Content]
parseXML = traverse fromContentF . parse . scanXML

------------------------------------------------------------------------

-- | Variant of 'Content' that can encode parser 'Failure's
data ContentF
  = ElemF (Element' ContentF)
  | TextF CData
  | CRefF !ShortText
  | Failure !Int String
  deriving (Show, Typeable, Data, Generic)

instance NFData ContentF

fromContentF :: ContentF -> Either (Pos,String) Content
fromContentF (CRefF ref)       = Right (CRef ref)
fromContentF (TextF cd)        = Right (Text cd)
fromContentF (ElemF el)        = Elem <$> traverse fromContentF el
fromContentF (Failure pos err) = Left (pos,err)

------------------------------------------------------------------------

parse :: [Token] -> [ContentF]
parse [] = []
parse ts = let (es,_,ts1) = nodes ([],Nothing) [] ts
           in es ++ parse ts1

-- Information about namespaces.
-- The first component is a map that associates prefixes to URIs,
-- the second is the URI for the default namespace, if one was provided.
type NSInfo = ([(ShortText,URI)],Maybe URI)


nodes :: NSInfo -> [QName] -> [Token] -> ([ContentF], [QName], [Token])

nodes ns ps (TokError pos msg : _) =
  let (es,qs,ts1) = nodes ns ps []
  in (Failure pos msg : es, qs, ts1)

nodes ns ps (TokCRef ref : ts) =
  let (es,qs,ts1) = nodes ns ps ts
  in (CRefF ref : es, qs, ts1)

nodes ns ps (TokText txt : ts) =
  let (es,qs,ts1) = nodes ns ps ts
      (more,es1)  = case es of
                      TextF cd : es1'
                        | cdVerbatim cd == cdVerbatim txt -> (cdData cd,es1')
                      _                                   -> (mempty,es)

  in (TextF txt { cdData = cdData txt `T.append` more } : es1, qs, ts1)

nodes cur_info ps (TokStart _ t as empty' : ts) = (node : siblings, open, toks)
  where
    new_name  = annotName new_info t
    new_info  = foldr addNS cur_info as
    node      = ElemF Element { elName    = new_name
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
      p1:_ | t1 == p1 -> ([],[],ts)
      _ -> let (es,qs,ts1) = nodes ns ps ts
           in (Failure pos "start/end-tag mismatch" : es, qs, ts1)
  where
    t1 = annotName ns t

nodes _ ps []                 = ([],ps,[])


annotName :: NSInfo -> QName -> QName
annotName (namespaces,def_ns) n = n { qURI = maybe def_ns (`lookup` namespaces) (qPrefix n) }


annotAttr :: NSInfo -> Attr -> Attr
annotAttr ns a@(Attr { attrKey = k}) =
  case (qPrefix k, qLName k) of
    -- Do not apply the default name-space to unqualified
    -- attributes.  See Section 6.2 of <http://www.w3.org/TR/REC-xml-names>.
    (Nothing, _) -> a
    _            -> a { attrKey = annotName ns k }


addNS :: Attr -> NSInfo -> NSInfo
addNS (Attr { attrKey = key, attrVal = val }) (ns,def) =
  case (qPrefix key, qLName key) of
    (Nothing,"xmlns") -> (ns, if T.null val then Nothing else Just (URI (TS.fromText val)))
    (Just "xmlns", k) -> ((unLName k, URI (TS.fromText val)) : ns, def)
    _                 -> (ns,def)
