{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}
{-# LANGUAGE LambdaCase #-}

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

-}

-- |
-- Module    : Text.XML.NS
-- Copyright : (c) Herbert Valerio Riedel 2019
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- XML Namespace related helpers
--
module Text.XML.NS
    ( xmlns_attr
    , xmlns_def_attr
    , xmlns_from_attr

    , ns_xmlns_uri, xmlNamesNS
    , ns_xml_uri, xmlnsNS

    , xmlns_attr_wellformed
    ) where

import           Common
import qualified Data.Text.Short     as TS
import           Text.XML.Types.Core
import           Utils
import Data.Either (partitionEithers)
import qualified Data.Text       as T

xmlNamesNS :: URI
xmlNamesNS = URI ns_xml_uri

xmlnsNS :: URI
xmlnsNS = URI ns_xmlns_uri

{-# NOINLINE ns_xml_uri #-}
ns_xml_uri :: ShortText
ns_xml_uri = "http://www.w3.org/XML/1998/namespace"

-- | Smart constructor for @xmlns:\<prefix\> = \<namespace-uri\>@
--
-- Invariant: @\<namespace-uri\>@ MUST be non-empty for non-empty prefixes
--
-- @since 0.3.0
xmlns_attr :: ShortText -- ^ namespace prefix (if empty, denotes the default namespace; see also 'xmlns_def_attr')
           -> URI -- ^ Namespace URI
           -> Attr
xmlns_attr pfx uri
  | TS.null pfx = xmlns_def_attr uri
  | not (isNCName (TS.unpack pfx)) = error "Text.XML.xmlns_attr: non-empty prefix is not a proper NCName"
  | isNullURI uri = error "Text.XML.xmlns_attr: empty namespace URI for non-empty prefix"
  | otherwise = Attr (QName { qPrefix = Just (TS.pack "xmlns"), qLName = LName pfx, qURI = xmlnsNS }) (TS.toText (unURI uri))

-- | Smart constructor for @xmlns = [\<namespace-uri\>|""]@ (i.e. for declaring the default namespace)
--
-- prop> xmlns_attr "" ns == xmlns_def_attr ns
--
-- @since 0.3.0
xmlns_def_attr :: URI -- ^ Default namespace URI (use /empty/ 'URI' to reset default namespace)
               -> Attr
xmlns_def_attr uri
  = Attr (QName { qPrefix = Nothing, qLName = LName (TS.pack "xmlns"), qURI = xmlnsNS })
         (if isNullURI uri then mempty else TS.toText (unURI uri))

-- | Convert @xmlns@ 'Attr' into a @(prefix,namespace-uri)@ pair; returns 'Nothing' if the argument isn't a @xmlns@ attribute.
--
-- An empty prefix denotes the default-namespace
--
-- prop> xmlns_from_attr (xmlns_attr pfx ns) == Just (pfx,ns)
--
-- @since 0.3.0
xmlns_from_attr :: Attr -> Maybe (ShortText,URI)
xmlns_from_attr (Attr (QName ln ns pfx) ns')
  | ns /= URI ns_xmlns_uri = Nothing
  | otherwise = Just $ case pfx of
                  Nothing -> (mempty,     URI (TS.fromText ns'))
                  Just _  -> (unLName ln, URI (TS.fromText ns'))

-- | Check rules imposed on reserved namespaces by <https://www.w3.org/TR/xml-names/>
xmlns_attr_wellformed :: Attr -> Bool
xmlns_attr_wellformed = \case
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
