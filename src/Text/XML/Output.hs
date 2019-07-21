{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------
-- |
-- Module    : Text.XML.Output
-- Copyright : (c) Galois, Inc. 2007
--             (c) Herbert Valerio Riedel 2019
-- SPDX-License-Identifier: BSD-3-Clause AND GPL-3.0-or-later
--
-- Output handling for the lightweight XML lib.
--

module Text.XML.Output
  ( serializeXML
  , serializeXMLDoc
  ) where

import           Common
import qualified Data.Text       as T
import qualified Data.Text.Lazy  as TL
import qualified Data.Text.Short as TS
import           Text.XML.Types

-- | Serialize XML 1.0 document prefixed by the XML prologue
-- \"@\<?xml version='1.0' ?\>@\"
--
serializeXMLDoc :: Element -> TL.Text
serializeXMLDoc = TL.pack . showTopElement

-- | Serialize a sequence of XML 'Content' nodes
serializeXML :: [Content] -> TL.Text
serializeXML = TL.pack . foldr (ppContentS defaultConfigPP "") ""

-- | The XML 1.0 header
xml_header :: String
xml_header = "<?xml version='1.0' ?>"

--------------------------------------------------------------------------------
data ConfigPP = ConfigPP
  { prettify      :: !Bool
  , allowEmptyTag :: QName -> Bool
  }

-- | Default pretty printing configuration.
--  * Always use abbreviate empty tags.
defaultConfigPP :: ConfigPP
defaultConfigPP = ConfigPP { prettify = False, allowEmptyTag = const True }

{-
-- | A configuration that tries to make things pretty
-- (possibly at the cost of changing the semantics a bit
-- through adding white space.)
prettyConfigPP     :: ConfigPP
prettyConfigPP      = defaultConfigPP { prettify = True }

-- | Pretty printing renders XML documents faithfully,
-- with the exception that whitespace may be added\/removed
-- in non-verbatim character data.
ppTopElement       :: Element -> String
ppTopElement        = ppcTopElement prettyConfigPP

-- | Pretty printing renders XML documents faithfully,
-- with the exception that whitespace may be added\/removed
-- in non-verbatim character data.
ppcTopElement      :: ConfigPP -> Element -> String
ppcTopElement c e   = unlines [xml_header,ppcElement c e]

-- | Pretty printing elements
ppcElement         :: ConfigPP -> Element -> String
ppcElement c e      = ppElementS c "" e ""
-}

-- | Pretty printing content using ShowS
ppContentS         :: ConfigPP -> String -> Content -> ShowS
ppContentS c i x xs = case x of
  Elem e -> ppElementS c i e xs
  Text t -> ppCDataS c i t xs
  CRef r -> showCRefS r xs
  Proc p -> ppProcS p xs
  Comm t -> ppCommS t xs

ppElementS         :: ConfigPP -> String -> Element -> ShowS
ppElementS c i e xs = i ++ (tagStart (elName e) (elAttribs e) $
  case elContent e of
    [] | allowEmptyTag c name -> " />" ++ xs
    [Text t] -> ">" ++ ppCDataS c "" t (tagEnd name xs)
    cs -> '>' : nl ++ foldr ppSub (i ++ tagEnd name xs) cs
      where ppSub e1 = ppContentS c (sp ++ i) e1 . showString nl
            (nl,sp)  = if prettify c then ("\n","  ") else ("","")
  )
  where
    name = elName e

ppCDataS           :: ConfigPP -> String -> CData -> ShowS
ppCDataS c i t xs   = i ++ if cdVerbatim t /= CDataText || not (prettify c)
                             then showCDataS t xs
                             else foldr cons xs (showCData t)

  where cons         :: Char -> String -> String
        cons '\n' ys = "\n" ++ i ++ ys
        cons y ys    = y : ys

ppCommS :: Comment -> ShowS
ppCommS (Comment t) xs = "<!--" ++ T.unpack t ++ "-->" ++ xs

ppProcS :: PI -> ShowS
ppProcS (PI tgt dat) xs = "<?" ++ TS.unpack tgt ++ " " ++ T.unpack dat ++ "?>" ++ xs

--------------------------------------------------------------------------------

-- | Adds the <?xml?> header.
showTopElement     :: Element -> String
showTopElement c    = xml_header ++ showElement c

showElement        :: Element -> String
showElement c       = ppElementS defaultConfigPP "" c ""

showCData          :: CData -> String
showCData c         = ppCDataS defaultConfigPP "" c ""

-- Note: crefs should not contain '&', ';', etc.
showCRefS          :: ShortText -> ShowS
showCRefS r xs      = '&' : TS.unpack r ++ ';' : xs

-- | Convert a text element to characters.
showCDataS         :: CData -> ShowS
showCDataS cd =
 case cdVerbatim cd of
   CDataText     -> escStr (T.unpack $ cdData cd)
   CDataVerbatim -> showString "<![CDATA[" . escCData (T.unpack $ cdData cd)
                                           . showString "]]>"
   CDataRaw      -> \ xs -> T.unpack (cdData cd) ++ xs

--------------------------------------------------------------------------------
escCData :: String -> ShowS
escCData (']' : ']' : '>' : cs) = showString "]]]]><![CDATA[>" . escCData cs
escCData (c : cs)               = showChar c . escCData cs
escCData []                     = id

escChar :: Char -> ShowS
escChar c = case c of
  '<'    -> showString "&lt;"
  '>'    -> showString "&gt;" -- only text-nodes
  '&'    -> showString "&amp;"
  '\x0D' -> showString "&#xD;"
  _      -> showChar c

escCharAttr :: Char -> ShowS
escCharAttr c = case c of
  '<'    -> showString "&lt;"
  '&'    -> showString "&amp;"
  '"'    -> showString "&quot;" -- only attr
  '\x0D' -> showString "&#xD;"
  '\x0A' -> showString "&#xA;" -- only attr
  '\x09' -> showString "&#x9;" -- only attr
  _      -> showChar c


escStr             :: String -> ShowS
escStr cs rs        = foldr escChar rs cs

escStrAttr             :: String -> ShowS
escStrAttr cs rs        = foldr escCharAttr rs cs

tagEnd             :: QName -> ShowS
tagEnd qn rs        = '<':'/':showQName qn ++ '>':rs

tagStart           :: QName -> [Attr] -> ShowS
tagStart qn as rs   = '<':showQName qn ++ as_str ++ rs
 where as_str       = if null as then "" else ' ' : unwords (map showAttr as)

showAttr           :: Attr -> String
showAttr (Attr qn v) = showQName qn ++ '=' : '"' : escStrAttr (T.unpack v) "\""

showQName          :: QName -> String
showQName q         = pre ++ showLName (qLName q)
  where pre = case qPrefix q of
                Nothing -> ""
                Just p  -> TS.unpack p ++ ":"

showLName :: LName -> String
showLName = TS.unpack . unLName
