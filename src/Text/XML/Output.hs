{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-

Copyright (c) 2019  Herbert Valerio Riedel <hvr@gnu.org>

 This file is free software: you may copy, redistribute and/or modify it
 under the terms of the GNU General Public License as published by the
 Free Software Foundation, either version 3 of the License, or (at your
 option) any later version.

 This file is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program (see `LICENSE.GPLv3`).  If not, see
 <https://www.gnu.org/licenses/gpl-3.0.html>.

This file incorporates work covered by the following copyright and
permission notice:

    (c) 2007 Galois Inc.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
    may be used to endorse or promote products derived from this software
    without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.

-}

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
  , serializeXMLRoot
  , SerializeXMLOptions(..), defaultSerializeXMLOptions
  ) where

import           Common
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Short        as TS
import           Text.XML.Types
import           Utils


-- | Serialize XML 1.0 document prefixed by the XML prologue
-- \"@\<?xml version='1.0' ?\>@\"
--
serializeXMLDoc :: Element -> TL.Text
serializeXMLDoc el
  = serializeXMLRoot defaultSerializeXMLOptions
    (Root (Just (XmlDeclaration Nothing Nothing)) [] Nothing el [])

-- | Serialize a sequence of XML 'Content' nodes
serializeXML :: [Content] -> TL.Text
serializeXML = TL.pack . foldr (ppContentS defaultSerializeXMLOptions) ""


-- | Default rendering options
--
--  * Allow empty tags for all non-special elements
defaultSerializeXMLOptions :: SerializeXMLOptions
defaultSerializeXMLOptions = SerializeXMLOptions
  { allowEmptyTag = const True
  }

-- | Options
data SerializeXMLOptions = SerializeXMLOptions
  { allowEmptyTag :: QName -> Bool
  }

-- | Serialize a XML 'Root'
serializeXMLRoot :: SerializeXMLOptions -> Root -> TL.Text
serializeXMLRoot sopts Root{..} = TLB.toLazyText $ go $
    maybeToList xmldecl ++
    map bMisc rootPreElem ++
    (case rootDoctype of
       Nothing -> []
       Just (dtd,moreMisc) -> ("<!DOCTYPE" <+> TLB.fromText dtd <+> ">") : map bMisc moreMisc
    ) ++
    [TLB.fromString (ppElementS sopts rootElement "")] ++
    map bMisc rootPostElem
  where
    xmldecl = case rootXmlDeclaration of
                Nothing -> Nothing
                Just (XmlDeclaration Nothing Nothing) -> Just "<?xml version=\"1.0\"?>"
                Just (XmlDeclaration menc mstand) -> Just $
                  ("<?xml version=\"1.0\"" <+>) $
                  (maybe id (\enc cont -> " encoding=\"" <+> bFromShortText enc <+> "\"" <+> cont) menc) $
                  (maybe id (\b cont -> " standalone=\"" <+> (if b then "yes" else "no") <+> "\"" <+> cont) mstand) $
                  "?>"

    go []           = mempty
    go [x]          = x
    go (x:xs@(_:_)) = x <+> TLB.singleton '\n' <+> go xs

    bMisc (Left (Comment t)) = "<!--" <+> TLB.fromText (T.replace "--" "-~" t) <+> "-->"
    bMisc (Right (PI tgt dat)) = "<?" <+> bFromShortText tgt <+> (if T.null dat then mempty else " ") <+> TLB.fromText dat <+> "?>"

--------------------------------------------------------------------------------

-- | Pretty printing content using ShowS
ppContentS :: SerializeXMLOptions -> Content -> ShowS
ppContentS c x xs = case x of
    Elem e -> ppElementS c e xs
    Text t -> ppCDataS t xs
    CRef r -> showCRefS r xs
    Proc p -> ppProcS p xs
    Comm t -> ppCommS t xs

ppElementS :: SerializeXMLOptions -> Element -> ShowS
ppElementS c e xs = tagStart (elName e) (elAttribs e) $ case elContent e of
    [] | allowEmptyTag c name -> "/>" ++ xs
    [Text t]                  -> ">" ++ ppCDataS t (tagEnd name xs)
    cs                        -> '>' : foldr (ppContentS c) (tagEnd name xs) cs
  where
    name = elName e

ppCDataS :: CData -> ShowS
ppCDataS t xs = showCDataS t xs

ppCommS :: Comment -> ShowS
ppCommS (Comment t) xs = "<!--" ++ T.unpack (T.replace "--" "-~" t) ++ "-->" ++ xs

ppProcS :: PI -> ShowS
ppProcS (PI tgt dat) xs = "<?" ++ TS.unpack tgt ++ (if T.null dat then mempty else " ") ++ T.unpack dat ++ "?>" ++ xs

--------------------------------------------------------------------------------

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

-- escape text in `<![CDATA[  ]]>` blocks
escCData :: String -> ShowS
escCData (']' : ']' : '>' : cs) = showString "]]]]><![CDATA[>" . escCData cs
escCData (c : cs)               = showChar c . escCData cs
escCData []                     = id

-- escape char in text-nodes
escChar :: Char -> ShowS
escChar c = case c of
  '<'    -> showString "&lt;"   -- MUST
  '>'    -> showString "&gt;"   -- MUST ("for compatibility")
  '&'    -> showString "&amp;"  -- MUST
  '\x0D' -> showString "&#xD;"  -- MUST (due to EOL normalization)
  _      -> showChar c

-- escape char in attribute value
escCharAttr :: Char -> ShowS
escCharAttr c = case c of
  '<'    -> showString "&lt;"   -- MUST
  '&'    -> showString "&amp;"  -- MUST
  '"'    -> showString "&quot;" -- MUST (for attr enclosed by ")
  '\x09' -> showString "&#x9;"  -- MUST (due to attr WS normalization)
  '\x0A' -> showString "&#xA;"  -- MUST (due to attr WS normalization)
  '\x0D' -> showString "&#xD;"  -- MUST (due to EOL normalization)
  _      -> showChar c


escStr             :: String -> ShowS
escStr cs rs        = foldr escChar rs cs

escStrAttr         :: String -> ShowS
escStrAttr cs rs    = foldr escCharAttr rs cs

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
