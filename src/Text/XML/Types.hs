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
    ( Root, Root'(..)
    , MiscNodes
    , XmlDeclaration(..)
    , PI(..)
    , Comment(..)
    , Content(..)
    , Element
    , Element'(..)
    , Attr(..)
    , CData(..), CDataKind(..)
    , NCName
    , QName(..)
    , LName(..)
    , URI(..), isNullURI
    , Pos

    , blank_name
    , blank_cdata
    , blank_element

    , xmlns_attr
    , xmlns_def_attr
    , xmlns_from_attr
    ) where

import           Text.XML.NS
import           Text.XML.Types.Core
