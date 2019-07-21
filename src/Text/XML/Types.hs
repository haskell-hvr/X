{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
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

-- | Denotes the @<?xml version="1.0" encoding="..." standalone="..." ?>@ declaration
data XmlDeclaration = XmlDeclaration (Maybe ShortText) (Maybe Bool)
  deriving (Show, Typeable, Data, Generic)

instance NFData XmlDeclaration

data PI = PI
  { piTarget :: !ShortText
  , piData   :: !Text
  } deriving (Show, Typeable, Data, Generic)

instance NFData PI

newtype Comment = Comment Text
  deriving (Show, Typeable, Data, Generic, NFData)

-- | XML content
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
  deriving (Ord, Eq, Typeable, Data, IsString, NFData, Generic)

-- due to the IsString instance we can just drop the constructor name
instance Show LName where
  showsPrec p (LName s) = showsPrec p s

-- | URIs resembling @anyURI@
newtype URI = URI { unURI :: ShortText }
  deriving (Ord, Eq, Typeable, Data, IsString, NFData, Generic)

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
