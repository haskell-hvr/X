{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module    : Text.XML.Types
-- Copyright : (c) Galois, Inc. 2007
--             (c) Herbert Valerio Riedel 2019
-- SPDX-License-Identifier: BSD-3-Clause AND GPL-3.0-or-later
--
-- Basic XML types.
--
module Text.XML.Types where

import           Common

-- | XML content
data Content
  = Elem Element
  | Text CData
  | CRef !ShortText
  deriving (Show, Typeable, Data, Generic)

instance NFData Content

-- | XML elements
data Element  = Element
  { elName    :: !QName
  , elAttribs :: [Attr]
  , elContent :: [Content]
  } deriving (Show, Typeable, Data, Generic)

instance NFData Element

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

-- | XML qualified names
data QName    = QName
  { qLName  :: !LName
  , qURI    :: Maybe URI
  , qPrefix :: Maybe ShortText
  } deriving (Show, Typeable, Data, Generic)

instance NFData QName

instance Eq QName where
  q1 == q2  = compare q1 q2 == EQ

instance Ord QName where
  compare q1 q2 =
    case compare (qLName q1) (qLName q2) of
      EQ  -> case (qURI q1, qURI q2) of
               (Nothing,Nothing) -> compare (qPrefix q1) (qPrefix q2)
               (u1,u2)           -> compare u1 u2
      x   -> x

-- | XML local names
newtype LName = LName { unLName :: ShortText }
  deriving (Show, Ord, Eq, Typeable, Data, IsString, NFData, Generic)

-- | URIs resembling @anyURI@
newtype URI = URI { unURI :: ShortText }
  deriving (Show, Ord, Eq, Typeable, Data, IsString, NFData, Generic)

-- blank elements --------------------------------------------------------------

-- | Blank names
blank_name :: QName
blank_name = QName
  { qLName  = LName mempty
  , qURI    = Nothing
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
