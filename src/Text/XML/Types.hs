{-# LANGUAGE Safe #-}

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
-- Module    : Text.XML.Types
-- Copyright : (c) Herbert Valerio Riedel 2019
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- Basic XML types.
--
module Text.XML.Types
    ( -- * Root node representation
      Root, Root'(..)
    , MiscNodes
    , XmlDeclaration(..)

      -- * Element nodes
    , Element
    , Element'(..)
    , blank_element

    , xmlns_elem_wellformed
    , xmlns_elem_wellformed'

      -- ** Element attributes
    , Attr(..)

    , xmlns_attr
    , xmlns_def_attr
    , xmlns_from_attr

      -- * Non-element content nodes
    , Content(..)
    , PI(..)
    , CData(..), CDataKind(..), blank_cdata
    , Comment(..)

      -- * Namespace-qualified names
    , QName(..), blank_name
    , qnameToText
    , qnameFromText

      -- ** Components of 'QName's
    , NCName
    , LName(..)
    , URI(..), isNullURI

      -- * Miscellaneous
    , Pos
    ) where

import           Text.XML.NS
import           Text.XML.Types.Core
import           Text.XML.Types.Internal
