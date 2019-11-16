{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

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
-- Module    : Text.XML.Types
-- Copyright : (c) Galois, Inc. 2007
--             (c) Herbert Valerio Riedel 2019
-- SPDX-License-Identifier: BSD-3-Clause AND GPL-3.0-or-later
--
-- Basic XML types.
--
module Text.XML.Types.Core where

import           Common
import qualified Data.Text.Short as TS

{-# NOINLINE ns_xmlns_uri #-}
ns_xmlns_uri :: ShortText
ns_xmlns_uri = "http://www.w3.org/2000/xmlns/"

type Root = Root' Content

-- | Represents the implicit root node of an XML document
--
-- @since 0.2.0
data Root' cnode = Root
  { rootXmlDeclaration :: Maybe XmlDeclaration -- ^ (optional) XML declaration
  , rootPreElem        :: MiscNodes            -- ^ Miscellaneous nodes before root element & DOCTYPE declaration
  , rootDoctype        :: Maybe (Text,MiscNodes) -- ^ optional DOCTYPE declaration and more miscellaneous nodes between DOCTYPE and root element
  , rootElement        :: Element' cnode       -- ^ The single root document element
  , rootPostElem       :: MiscNodes            -- ^ Miscellaneous nodes after root element
  } deriving (Show, Typeable, Data, Generic, Functor, Foldable, Traversable)

instance NFData cnode => NFData (Root' cnode)

-- | Sequence of \"miscellaneous\" nodes
--
-- @since 0.2.0
type MiscNodes = [Either Comment PI]

-- | Denotes the @\<?xml version="1.0" encoding="..." standalone="..." ?\>@ declaration
--
-- @since 0.2.0
data XmlDeclaration = XmlDeclaration (Maybe ShortText) (Maybe Bool)
  deriving (Show, Typeable, Data, Generic)

instance NFData XmlDeclaration

-- | Processing instruction
--
-- @since 0.2.0
data PI = PI
  { piTarget :: !ShortText -- ^ Invariant: MUST not be @[Xx][Mm][Ll]@
  , piData   :: !Text      -- ^ Invariant: MUST not contain @?>@
  } deriving (Show, Typeable, Data, Generic)

instance NFData PI

-- | Represents a XML comment
--
-- Invariant: SHOULD not contain @--@ (occurences of @--@ will be automatically substituted by @-~@ on serialization)
--
-- @since 0.2.0
newtype Comment = Comment Text
  deriving (Show, Typeable, Data, Generic, NFData)

-- | XML content
--
-- @since 0.2.0
data Content
  = Elem Element
  | Text CData
  | CRef !ShortText
  | Proc PI
  | Comm Comment
  deriving (Show, Typeable, Data, Generic)

instance NFData Content

type Element = Element' Content

-- | XML elements
data Element' cnode  = Element
  { elName    :: !QName
  , elAttribs :: [Attr]
  , elContent :: [cnode]
  } deriving (Show, Typeable, Data, Generic, Functor, Foldable, Traversable)

instance NFData cnode => NFData (Element' cnode)

-- | XML attributes
data Attr     = Attr
  { attrKey :: !QName
  , attrVal :: !Text
  } deriving (Eq, Ord, Show, Typeable, Data, Generic)

instance NFData Attr

-- | XML CData
data CData    = CData
  { cdVerbatim :: !CDataKind
  , cdData     :: !Text
  } deriving (Show, Typeable, Data, Generic)

instance NFData CData

data CDataKind
  = CDataText      -- ^ Ordinary character data; pretty printer escapes &, < etc.
  | CDataVerbatim  -- ^ Unescaped character data; pretty printer embeds it in <![CDATA[..
  | CDataRaw       -- ^ As-is character data; pretty printer passes it along without any escaping or CDATA wrap-up.
  deriving (Eq, Show, Typeable, Data, Generic)

instance NFData CDataKind

-- | A <https://www.w3.org/TR/xml-names/#NT-NCName NCName>
--
-- NB: Among other properties this means that an 'NCName' shall never be the empty string.
--
-- @since 0.2.0
type NCName = ShortText

-- | XML (expanded) qualified names
--
data QName    = QName
  { qLName  :: !LName -- ^ Local name part
  , qURI    :: !URI -- ^ Invariant: the `qURI' field MUST always be populated with the proper namespace. Specifically, entities belonging to the <http://www.w3.org/2000/xmlns/> or <http://www.w3.org/XML/1998/namespace> must have the 'qURI' field accordingly
  , qPrefix :: Maybe NCName -- ^ Invariant: MUST be a proper <https://www.w3.org/TR/xml-names/#NT-NCName NCName>
  } deriving (Show, Typeable, Data, Generic)

instance NFData QName

-- | Compares namespace URI and local name for equality (i.e. the namespace prefix is ignored)
--
-- @since 0.3.0
instance Eq QName where
  q1 == q2  = xn q1 == xn q2
    where
      xn (QName ln ns _) = (ns,ln)

-- | Compares namespace URI and local name for equality (i.e. the namespace prefix is effectively ignored)
--
-- The <http://www.w3.org/2000/xmlns/> namespace is considered less than any other namespace (including the null namespace)
--
-- @since 0.3.0
instance Ord QName where
  compare = comparing sortKey
    where
      sortKey (QName ln ns pfx) = (not isXmlns,ns,key2)
        where
          isXmlns = URI ns_xmlns_uri == ns
          key2
            | isXmlns = if isNothing pfx then LName mempty else ln
            | otherwise = ln

-- | XML local names
--
-- Invariant: MUST be a proper <https://www.w3.org/TR/xml-names/#NT-NCName NCName>
newtype LName = LName { unLName :: NCName }
  deriving (Ord, Eq, Typeable, Data, IsString, NFData, Generic)

-- due to the IsString instance we can just drop the constructor name
instance Show LName where
  showsPrec p (LName s) = showsPrec p s

-- | URIs resembling @anyURI@
--
-- Invariant: MUST be a valid @URI-reference@ as defined in <https://tools.ietf.org/html/rfc3986 RFC3986>
--
newtype URI = URI { unURI :: ShortText }
  deriving (Ord, Eq, Typeable, Data, IsString, NFData, Generic)

-- | Test for /empty/ 'URI'
--
-- >>> isNullURI (URI mempty)
-- True
--
-- >>> isNullURI (URI "")
-- True
--
-- >>> isNullURI (URI " ")
-- False
--
-- @since 0.3.0
isNullURI :: URI -> Bool
isNullURI (URI u) = TS.null u

-- due to the IsString instance we can just drop the constructor name
instance Show URI where
  showsPrec p (URI s) = showsPrec p s

-- | Position expressed in number of code-points
--
-- A negative value denotes EOF
type Pos = Int

-- blank elements --------------------------------------------------------------

-- | Blank names
blank_name :: QName
blank_name = QName
  { qLName  = LName mempty
  , qURI    = URI mempty
  , qPrefix = Nothing
  }

-- | Blank cdata
blank_cdata :: CData
blank_cdata = CData
  { cdVerbatim = CDataText
  , cdData     = mempty
  }

-- | Blank elements
blank_element :: Element
blank_element = Element
  { elName    = blank_name
  , elAttribs = mempty
  , elContent = mempty
  }
