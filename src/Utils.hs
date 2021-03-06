{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

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
-- Module    : Utils
-- Copyright : (c) Herbert Valerio Riedel 2019
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- Internal helpers
--

module Utils where

import           Common
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Short        as TS

-- |
--
-- > S ::=  (#x20 | #x9 | #xD | #xA)+
--
isS :: Char -> Bool
isS '\x20' = True
isS '\x09' = True
isS '\x0D' = True
isS '\x0A' = True
isS _      = False

isNCName :: String -> Bool
isNCName []     = False
isNCName (c:cs) = isNameStartChar c && c /= ':' && all (\c' -> isNameChar c' && c' /= ':') cs

-- |
--
-- Char ::= #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
--
isChar :: Char -> Bool
isChar c
  | c <  '\x20'     = c == '\x0A' || c == '\x09' || c == '\x0D'
  | c <  '\xD800'   = True
  | c <  '\xE000'   = False
  | c == '\xFFFE'   = False
  | c == '\xFFFF'   = False
  | otherwise       = True

-- |
--
-- NameStartChar ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
--
-- NameChar ::= NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
--
-- Name ::= NameStartChar (NameChar)*
--
isNameStartChar :: Char -> Bool
isNameStartChar c
  | c == ':'        = True
  | c <  'A'        = False
  | c <= 'Z'        = True
  | c == '_'        = True
  | c <  'a'        = False
  | c <= 'z'        = True
  | c <  '\xC0'     = False
  | c <= '\xD6'     = True
  | c <  '\xD8'     = False
  | c <= '\xF6'     = True
  | c <  '\xF8'     = False
  | c <= '\x2FF'    = True
  | c <  '\x370'    = False
  | c <= '\x37D'    = True
  | c <  '\x37F'    = False
  | c <= '\x1FFF'   = True
  | c <  '\x200C'   = False
  | c <= '\x200D'   = True
  | c <  '\x2070'   = False
  | c <= '\x218F'   = True
  | c <  '\x2C00'   = False
  | c <= '\x2FEF'   = True
  | c <  '\x3001'   = False
  | c <= '\xD7FF'   = True
  | c <  '\xF900'   = False
  | c <= '\xFDCF'   = True
  | c <  '\xFDF0'   = False
  | c <= '\xFFFD'   = True
  | c <  '\x10000'  = False
  | c <= '\xEFFFF'  = True
  | otherwise       = False

-- | See 'isNameStartChar'
isNameChar :: Char -> Bool
isNameChar c
  | c == '.'        = True
  | c == '-'        = True
  | c <  '0'        = False
  | c <= ':'        = True
  | c <  'A'        = False
  | c <= 'Z'        = True
  | c == '_'        = True
  | c <  'a'        = False
  | c <= 'z'        = True
  | c == '\xB7'     = True
  | c <  '\xC0'     = False
  | c <= '\xD6'     = True
  | c <  '\xD8'     = False
  | c <= '\xF6'     = True
  | c <  '\xF8'     = False
  | c <= '\x2FF'    = True
  | c <  '\x300'    = False
  | c <= '\x37D'    = True
  | c <  '\x37F'    = False
  | c <= '\x1FFF'   = True
  | c <  '\x200C'   = False
  | c <= '\x200D'   = True
  | c <  '\x203F'   = False
  | c <= '\x2040'   = True
  | c <  '\x2070'   = False
  | c <= '\x218F'   = True
  | c <  '\x2C00'   = False
  | c <= '\x2FEF'   = True
  | c <  '\x3001'   = False
  | c <= '\xD7FF'   = True
  | c <  '\xF900'   = False
  | c <= '\xFDCF'   = True
  | c <  '\xFDF0'   = False
  | c <= '\xFFFD'   = True
  | c <  '\x10000'  = False
  | c <= '\xEFFFF'  = True
  | otherwise       = False



unsnoc :: [x] -> Maybe ([x],x)
unsnoc [] = Nothing
unsnoc xs = Just (init xs, last xs)


infixr 6 <+>

(<+>) :: TLB.Builder -> TLB.Builder -> TLB.Builder
(<+>) = mappend

bFromShortText :: ShortText -> TLB.Builder
bFromShortText = TLB.fromText . TS.toText

bUnlines :: [TLB.Builder] -> TLB.Builder
bUnlines []           = mempty
bUnlines [x]          = x
bUnlines (x:xs@(_:_)) = x <+> TLB.singleton '\n' <+> bUnlines xs


{-# INLINEABLE noDupes #-}
-- | Return 'True' if list contains duplicate values
noDupes :: Ord a => [a] -> Bool
noDupes []      = True
noDupes [_]     = True
noDupes [x,y]   = x /= y
noDupes [x,y,z] = x /= y && x /= z && y /= z
noDupes xs0
  = case sort xs0 of
      []     -> True -- unreachable
      (x:xs) -> go x xs
  where
    go _  []      = True
    go x1 (x2:xs)
      | x1 == x2  = False
      | otherwise = go x2 xs

-- decode numeric char-reference taking into account 'isChar'
decodeCharRefHex :: [Char] -> Maybe Char
decodeCharRefHex = go 0 . dropWhile (=='0') -- NB: U+00 is invalid
  where
    go acc _ | acc > 0x10ffff = Nothing
    go acc [] = let c = toEnum acc
                in if isChar c then Just c else Nothing
    go acc (c:cs)
      | n < 0     = Nothing
      | otherwise = go ((acc*16) + n) cs
      where
        n = c2n c

    c2n :: Char -> Int
    c2n c
      | fromEnum '0' <= n, n <= fromEnum '9' = n - fromEnum '0'
      | fromEnum 'a' <= n, n <= fromEnum 'f' = n - (fromEnum 'a' - 10)
      | fromEnum 'A' <= n, n <= fromEnum 'F' = n - (fromEnum 'A' - 10)
      | otherwise = -1
      where
        n = fromEnum c


decodeCharRefDec :: [Char] -> Maybe Char
decodeCharRefDec = go 0 . dropWhile (=='0') -- NB: U+00 is invalid
  where
    go acc _ | acc > 0x10ffff = Nothing
    go acc [] = let c = toEnum acc
                in if isChar c then Just c else Nothing
    go acc (c:cs)
      | n < 0     = Nothing
      | otherwise = go ((acc*10) + n) cs
      where
        n = c2n c

    c2n :: Char -> Int
    c2n c
      | fromEnum '0' <= n, n <= fromEnum '9' = n - fromEnum '0'
      | otherwise = -1
      where
        n = fromEnum c

