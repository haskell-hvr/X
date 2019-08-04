{-# LANGUAGE FlexibleInstances #-}

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
-- Module    : Text.XML.Types.Internal
-- Copyright : (c) Herbert Valerio Riedel 2019
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- @since 0.3
module Text.XML.Types.Internal where

import           Common
import           Text.XML.Types

-- | Convenience class for converting to/from 'Content' values
--
-- @since 0.3.0
class IsContent x where
  -- | /upcast/ or generalize to 'Content'
  toContent :: x -> Content

  -- | /downcast/ or specialize (if possible) to a specific 'Content' subtype
  fromContent :: Content -> Maybe x

  -- | (currently private) Specialize to an 'Element' if possible
  --
  -- This method is included in this class as an optimization over the default impl below
  toElem :: x -> Maybe Element
  toElem = fromContent . toContent

----------------------------------------------------------------------------
-- trivial instance
instance IsContent Content where
  toContent = id
  fromContent = Just

  toElem (Elem el) = Just el
  toElem _         = Nothing

----------------------------------------------------------------------------
-- primitive instances
instance IsContent Element where
  toContent = Elem
  fromContent (Elem x) = Just x
  fromContent _        = Nothing

  toElem = Just

instance IsContent Comment where
  toContent = Comm
  fromContent (Comm x) = Just x
  fromContent _        = Nothing

  toElem _ = Nothing

instance IsContent PI where
  toContent = Proc
  fromContent (Proc x) = Just x
  fromContent _        = Nothing

  toElem _ = Nothing

instance IsContent CData where
  toContent = Text
  fromContent (Text x) = Just x
  fromContent _        = Nothing

  toElem _ = Nothing

----------------------------------------------------------------------------
-- | Convenient for e.g. 'MiscNodes'
instance (IsContent l, IsContent r) => IsContent (Either l r) where
  toContent = either toContent toContent
  fromContent c = (Left <$> fromContent c) <|> (Right <$> fromContent c)
  toElem = either toElem toElem
