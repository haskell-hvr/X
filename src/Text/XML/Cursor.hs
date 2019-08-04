{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
-- Module    : Text.XML.Cursor
-- Copyright : (c) Galois, Inc. 2008
--             (c) Herbert Valerio Riedel 2019
-- SPDX-License-Identifier: BSD-3-Clause AND GPL-3.0-or-later
--
-- XML cursors for working XML content withing the context of
-- an XML document.  This implementation is based on the general
-- tree zipper written by Krasimir Angelov and Iavor S. Diatchki.
--
-- __NOTE__: The Cursor API has been significantly altered in 0.3.0,
-- hence this module's API is to be considered \"since 0.3.0\"
--
-- @since 0.3.0
module Text.XML.Cursor
  ( Tag(..), getTag, setTag, fromTag
  , Cursor, Cursor'(..), Path

  -- * Conversions
  , fromRootElement
  , fromRoot
  , toRootElement
  , toRoot

  , upCast
  , downCast

  -- * Moving around
  , parent
  , root
  , getChild
  , firstChild
  , lastChild
  , left
  , right
  , nextDF

  -- ** Searching
  , findChild
  , findLeft
  , findRight
  , findRec

  -- * Node classification
  , isRoot
  , isFirst
  , isLast
  , isLeaf
  , isChild
  , hasChildren
  , getNodeIndex

  -- ** Inserting content
  , insertLeft
  , insertRight
  , insertGoLeft
  , insertGoRight

  -- ** Removing content
  , removeLeft
  , removeRight
  , removeGoLeft
  , removeGoRight
  , removeGoUp

  ) where

import           Common
import           Text.XML.Types
import           Text.XML.Types.Internal

data Tag = Tag
  { tagName    :: QName
  , tagAttribs :: [Attr]
  } deriving (Show,Generic,Typeable,Data)

instance NFData Tag

getTag :: Element -> Tag
getTag e = Tag { tagName = elName e
               , tagAttribs = elAttribs e
               }

setTag :: Tag -> Element -> Element
setTag t e = fromTag t (elContent e)

fromTag :: Tag -> [Content] -> Element
fromTag t cs = Element { elName    = tagName t
                       , elAttribs = tagAttribs t
                       , elContent = cs
                       }

-- | Parent path (with the root as last element) consisting of list of
-- left siblings, parent, and right siblings
type Path = [([Content],Tag,[Content])]

-- | General cursor
type Cursor = Cursor' Content

-- | The position of a piece of content in an XML document.
--
-- @since 0.3.0
data Cursor' content = Cur
  { current :: content      -- ^ The currently selected content.
  , lefts   :: [Content]    -- ^ Siblings on the left, closest first.
  , rights  :: [Content]    -- ^ Siblings on the right, closest first.
  , parents :: Path         -- ^ The contexts of the parent elements of this location.
  } deriving (Show,Generic,Typeable,Data,Functor,Foldable,Traversable)

instance NFData content => NFData (Cursor' content)

-- Moving around ---------------------------------------------------------------

-- | The parent of the given location.
parent :: IsContent content => Cursor' content -> Maybe (Cursor' Element)
parent loc =
  case parents loc of
    (pls,v,prs) : ps -> Just
      Cur { current = fromTag v $
              combChildren (lefts loc) (toContent (current loc)) (rights loc)
          , lefts   = pls
          , rights  = prs
          , parents = ps
          }
    [] -> Nothing

-- | The top-most parent of the given location.
root :: IsContent content => Cursor' content -> Cursor' Element
root loc = maybe loc' root (parent loc :: Maybe (Cursor' Element))
  where
    loc' :: Cursor' Element
    loc' = case traverse toElem loc of
             Nothing -> error "root: invalid cursor"
             Just x  -> x


-- | The left sibling of the given location.
left :: IsContent content => Cursor' content -> Maybe Cursor
left loc =
  case lefts loc of
    t : ts -> Just loc { current = t
                       , lefts   = ts
                       , rights  = toContent (current loc) : rights loc
                       }
    []     -> Nothing

-- | The right sibling of the given location.
right :: IsContent content => Cursor' content -> Maybe Cursor
right loc =
  case rights loc of
    t : ts -> Just loc { current = t
                       , lefts = toContent (current loc) : lefts loc
                       , rights = ts }
    []     -> Nothing

-- | The first child of the given location.
firstChild :: IsContent content => Cursor' content -> Maybe Cursor
firstChild loc =
  do (t : ts, ps) <- downParents loc
     return Cur { current = t, lefts = [], rights = ts , parents = ps }

-- | The last child of the given location.
lastChild :: IsContent content => Cursor' content -> Maybe Cursor
lastChild loc =
  do (ts, ps) <- downParents loc
     case reverse ts of
       l : ls -> return Cur { current = l, lefts = ls, rights = []
                                                     , parents = ps }
       [] -> Nothing

-- | Find the next left sibling that satisfies a predicate.
findLeft :: IsContent content => (Cursor -> Bool) -> Cursor' content -> Maybe Cursor
findLeft p loc = do
    loc1 <- left loc
    if p loc1 then return loc1 else findLeft p loc1

-- | Find the next right sibling that satisfies a predicate.
findRight :: IsContent content => (Cursor -> Bool) -> Cursor' content -> Maybe Cursor
findRight p loc = do
    loc1 <- right loc
    if p loc1 then return loc1 else findRight p loc1

-- | The first child that satisfies a predicate.
findChild :: IsContent content => (Cursor -> Bool) -> Cursor' content -> Maybe Cursor
findChild p loc = do
    loc1 <- firstChild loc
    if p loc1 then return loc1 else findRight p loc1

-- | The next position in a left-to-right depth-first traversal of a document:
-- either the first child, right sibling, or the right sibling of a parent that
-- has one.
nextDF :: IsContent content => Cursor' content -> Maybe Cursor
nextDF c = firstChild c <|> up (toContent <$> c)
  where
    up :: Cursor -> Maybe Cursor
    up x = right x <|> (up =<< y)
      where
        y = fmap Elem <$> parent x

-- | Perform a depth first search for a descendant that satisfies the
-- given predicate.
findRec :: IsContent content => (Cursor -> Bool) -> Cursor' content -> Maybe Cursor
findRec p c = if p c' then Just c' else findRec p =<< nextDF c'
  where
    c' = upCast c

-- | The child with the given index (starting from 0).
getChild :: IsContent content => Word -> Cursor' content -> Maybe Cursor
getChild n loc =
  do (ts,ps) <- downParents loc
     (ls,t,rs) <- splitChildren ts n
     return Cur { current = t, lefts = ls, rights = rs, parents = ps }


-- | private: computes the parent for "down" operations.
downParents :: IsContent content => Cursor' content -> Maybe ([Content], Path)
downParents loc =
  case toElem (current loc) of
    Just e -> Just ( elContent e
                   , (lefts loc, getTag e, rights loc) : parents loc
                   )
    Nothing -> Nothing

-- Conversions -----------------------------------------------------------------

-- | Generalize content type of current 'Cursor' location
--
-- @since 0.3.0
upCast :: IsContent content => Cursor' content -> Cursor
upCast = fmap toContent

-- | Specialize content type of current 'Cursor' location
--
-- @since 0.3.0
downCast :: IsContent content => Cursor -> Maybe (Cursor' content)
downCast = traverse fromContent

-- | A cursor for the given (root) element.
--
-- @since 0.3.0
fromRootElement :: Element -> Cursor' Element
fromRootElement e = Cur { current = e, lefts = [], rights = [], parents = [] }

-- | Construct cursor from document 'Root'
--
-- @since 0.3.0
fromRoot :: Root -> Cursor' Element
fromRoot r = Cur { current = rootElement r
                 , lefts   = map toContent (rootPreElem r ++ maybe [] snd (rootDoctype r))
                 , rights  = map toContent (rootPostElem r)
                 , parents = []
                 }

-- | Computes the root element containing this location.
--
-- __NOTE__: The root element might have siblings; see 'toRoot' or 'root' if you need to deal with such siblings.
--
-- @since 0.3.0
toRootElement :: IsContent content => Cursor' content -> Element
toRootElement loc = current (root loc)

-- | Constructs the document 'Root' containing this location.
--
-- Returns 'Nothing' if invalid top-level \"miscellaneous\" nodes are encountered.
--
-- @since 0.3.0
toRoot :: IsContent content => Cursor' content -> Maybe Root
toRoot loc = do
    rootPreElem  <- traverse fromContent l
    rootPostElem <- traverse fromContent r
    pure Root{..}
  where
    Cur rootElement l r [] = root loc
    rootXmlDeclaration = Nothing
    rootDoctype        = Nothing

-- Queries ---------------------------------------------------------------------

-- | Are we at the top of the document?
isRoot :: Cursor' content -> Bool
isRoot loc = null (parents loc)

-- | Are we at the left end of the the document (i.e. the locally left-most sibling)?
isFirst :: Cursor' content -> Bool
isFirst loc = null (lefts loc)

-- | Are we at the right end of the document (i.e. the locally right-most sibling)?
isLast :: Cursor' content -> Bool
isLast loc = null (rights loc)

-- | Are we at the bottom of the document?
isLeaf :: IsContent content => Cursor' content -> Bool
isLeaf loc = isNothing (firstChild loc)

-- | Do we have a parent?
isChild :: Cursor' content -> Bool
isChild loc = not (isRoot loc)

-- | Get the node index inside the sequence of children\/siblings
getNodeIndex :: Cursor' content -> Word
getNodeIndex loc = fromIntegral (length (lefts loc))

-- | Do we have children?
hasChildren :: IsContent content => Cursor' content -> Bool
hasChildren loc = not (isLeaf loc)

-- Updates ---------------------------------------------------------------------

-- | Insert content to the left of the current position.
insertLeft :: IsContent c => c -> Cursor' content -> Cursor' content
insertLeft t loc = loc { lefts = toContent t : lefts loc }

-- | Insert content to the right of the current position.
insertRight :: IsContent c => c -> Cursor' content -> Cursor' content
insertRight t loc = loc { rights = toContent t : rights loc }

-- | Remove the content on the left of the current position, if any.
removeLeft :: Cursor' content -> Maybe (Content,Cursor' content)
removeLeft loc = case lefts loc of
                   l : ls -> return (l,loc { lefts = ls })
                   []     -> Nothing

-- | Remove the content on the right of the current position, if any.
removeRight :: Cursor' content -> Maybe (Content,Cursor' content)
removeRight loc = case rights loc of
                    l : ls -> return (l,loc { rights = ls })
                    []     -> Nothing


-- | Insert content to the left of the current position.
-- The new content becomes the current position.
insertGoLeft :: IsContent content2 => content -> Cursor' content2 -> Cursor' content
insertGoLeft t loc = loc { current = t, rights = toContent (current loc) : rights loc }

-- | Insert content to the right of the current position.
-- The new content becomes the current position.
insertGoRight :: IsContent content2 => content -> Cursor' content2 -> Cursor' content
insertGoRight t loc = loc { current = t, lefts = toContent (current loc) : lefts loc }

-- | Remove the current element.
-- The new position is the one on the left.
removeGoLeft :: Cursor' content -> Maybe Cursor
removeGoLeft loc = case lefts loc of
                     l : ls -> Just loc { current = l, lefts = ls }
                     []     -> Nothing

-- | Remove the current element.
-- The new position is the one on the right.
removeGoRight :: Cursor' content -> Maybe Cursor
removeGoRight loc = case rights loc of
                     l : ls -> Just loc { current = l, rights = ls }
                     []     -> Nothing

-- | Remove the current element.
-- The new position is the parent of the old position.
removeGoUp :: Cursor' content -> Maybe Cursor
removeGoUp loc =
  case parents loc of
    (pls,v,prs) : ps -> Just
      Cur { current = Elem (fromTag v (reverse (lefts loc) ++ rights loc))
          , lefts = pls, rights = prs, parents = ps
          }
    [] -> Nothing

-- | private: Gets the given element of a list.
-- Also returns the preceding elements (reversed) and the following elements.
splitChildren :: [a] -> Word -> Maybe ([a],a,[a])
splitChildren = go []
  where
    go acc (x:xs) 0 = Just (acc,x,xs)
    go acc (x:xs) n = go (x:acc) xs $! n-1
    go _   []     _ = Nothing

-- | private: combChildren ls x ys = reverse ls ++ [x] ++ ys
combChildren :: [a] -> a -> [a] -> [a]
combChildren ls t rs = foldl (flip (:)) (t:rs) ls
