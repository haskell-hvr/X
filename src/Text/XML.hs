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
-- Module    : Text.XML
-- Copyright : (c) Galois, Inc. 2007
--             (c) Herbert Valerio Riedel 2019
-- SPDX-License-Identifier: BSD-3-Clause AND GPL-3.0-or-later
--
-- A lightweight XML parsing, filtering and generating library.
--
-- This module reexports functions from:
--
-- * "Text.XML.Types"
--
-- * "Text.XML.Proc"
--
-- * "Text.XML.Input"
--
-- * "Text.XML.Output"
--

module Text.XML (

    module Text.XML,
    module Text.XML.Types,
    module Text.XML.Proc,
    module Text.XML.Input,
    module Text.XML.Output

  ) where

import           Common

import           Text.XML.Input
import           Text.XML.Output
import           Text.XML.Proc
import           Text.XML.Types

import qualified Data.Text.Short as TS

-- | Add an attribute to an element.
add_attr :: Attr -> Element -> Element
add_attr a e = add_attrs [a] e

-- | Add some attributes to an element.
add_attrs :: [Attr] -> Element -> Element
add_attrs as e = e { elAttribs = as ++ elAttribs e }

-- | Create an unqualified name.
unqual :: LName -> QName
unqual x = blank_name { qLName = x }

-- | A smart element constructor which uses the type of its argument
-- to determine what sort of element to make.
class Node t where
  node :: QName -> t -> Element

instance Node [Attr]             where node n as   = node n (as,[]::[Content])
instance Node Attr               where node n a    = node n [a]
instance Node ()                 where node n ()   = node n ([]::[Attr])

instance Node ([Attr],[Content]) where node n (as,cs) = Element n as cs
instance Node ([Attr],Content)   where node n (as,c) = node n (as,[c])
instance Node (Attr,Content)     where node n (a,c)  = node n ([a],[c])
instance Node [Content]          where node n cs     = node n ([]::[Attr],cs)
instance Node Content            where node n c      = node n [c]

instance Node ([Attr],[Element]) where node n (as,cs) = node n (as,map Elem cs)
instance Node ([Attr],Element)   where node n (as,c) = node n (as,[c])
instance Node (Attr,Element)     where node n (a,c)  = node n ([a],c)
instance Node ([Element])        where node n es     = node n ([]::[Attr],es)
instance Node (Element)          where node n e      = node n [e]

instance Node ([Attr],[CData])   where node n (as,cs) = node n (as,map Text cs)
instance Node ([Attr],CData)     where node n (as,c) = node n (as,[c])
instance Node (Attr,CData)       where node n (a,c)  = node n ([a],c)
instance Node [CData]            where node n es     = node n ([]::[Attr],es)
instance Node CData              where node n e      = node n [e]

instance Node ([Attr],Text)      where node n (as,t) = node n (as,blank_cdata { cdData = t })
instance Node (Attr,Text)        where node n (a,t)  = node n ([a],t)
instance Node Text               where node n t      = node n ([]::[Attr],t)

instance Node ([Attr],ShortText) where node n (as,t) = node n (as,blank_cdata { cdData = TS.toText t })
instance Node (Attr,ShortText)   where node n (a,t)  = node n ([a],t)
instance Node ShortText          where node n t      = node n ([]::[Attr],t)

-- | Create node with unqualified name
unode :: Node t => LName -> t -> Element
unode = node . unqual
