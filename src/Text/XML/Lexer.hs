{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
-- Module    : Text.XML.Lexer
-- Copyright : (c) Galois, Inc. 2007
--             (c) Herbert Valerio Riedel 2019
-- SPDX-License-Identifier: BSD-3-Clause AND GPL-3.0-or-later
--
module Text.XML.Lexer where

import           Common
import           Text.XML.Types
import           Utils

import           Data.Char       (isAsciiLower, isAsciiUpper, isDigit, toLower)
import qualified Data.Text       as T
import qualified Data.Text.Lazy  as TL
import qualified Data.Text.Short as TS
import           Numeric         (readHex)

nullNs :: URI
nullNs = URI mempty

class XmlSource s where
  uncons :: s -> Maybe (Char,s)

instance XmlSource String where
  uncons (c:s) = Just (c,s)
  uncons ""    = Nothing

instance XmlSource T.Text where
  uncons = T.uncons

instance XmlSource TL.Text where
  uncons = TL.uncons

-- | This type may be used to provide a custom scanning function
-- for extracting characters.
data Scanner s = Scanner (Maybe (Char,s)) (s -> Maybe (Char,s))

-- | This type may be used to provide a custom scanning function
-- for extracting characters.
customScanner :: (s -> Maybe (Char,s)) -> s -> Scanner s
customScanner next s = Scanner (next s) next

instance XmlSource (Scanner s) where
  uncons (Scanner this next) = do
    (c,s1) <- this
    return (c, Scanner (next s1) next)

-- Lexer -----------------------------------------------------------------------

type LChar              = (Pos,Char)
type LString            = [LChar]

-- | XML Lexer token.
data Token              = TokStart !Pos QName [Attr] Bool
                          -- ^ opening start-tag (the 'Bool' field denotes whether this is an empty tag)
                        | TokEnd   !Pos QName     -- ^ closing end-tag
                        | TokCRef       ShortText -- ^ character entity reference
                        | TokText       CData     -- ^ character data
                        | TokError !Pos String    -- ^ Lexer error
                        | TokXmlDecl    XmlDeclaration
                        | TokComment    Comment
                        | TokPI    !Pos PI
                        | TokDTD        Text
                        deriving (Show,Data,Typeable,Generic)

instance NFData Token

eofErr :: [Token]
eofErr = [TokError (-1) "Premature EOF"]

-- | Run XML lexer over 'XmlSource'
scanXML :: XmlSource source => source -> [Token]
scanXML = tokens0 . eolNorm . go 0
  where
    go !n src = case uncons src of
      Just (c,src') -> (n,c) : go (n+1) src'
      Nothing       -> []


{-

> [XML 1.0] 2.11 End-of-Line Handling
>
> [...] the XML processor must behave as if it normalized all line breaks
> in external parsed entities (including the document entity) on input,
> before parsing, by translating both the two-character sequence #xD #xA
> and any #xD that is not followed by #xA to a single #xA character.

-}
eolNorm :: LString -> LString
eolNorm []                         = []
eolNorm ((_,'\xD'):c@(_,'\xA'):cs) = c         : eolNorm cs
eolNorm ((n,'\xD'):cs)             = (n,'\xA') : eolNorm cs
eolNorm (c:cs)                     = c         : eolNorm cs



tokens0 :: LString -> [Token]
-- tokens0 ((_,'<'):(_,'?'):(_,'x'):(_,'m'):(_,'l'):(_,c):cs)
--   | isS c = go1 (dropSpace cs)
--   where
--     go1 ((_,'v'):(_,'e'):(_,'r'):(_,'s'):(_,'i'):(_,'o'):(_,'n'):cs)
tokens0 cs = tokens' cs


tokens' :: LString -> [Token]
tokens' ((_,'<') : (_,'!') : cs) = special cs
tokens' ((n,'<') : (_,'?') : cs) = procins n cs
tokens' ((_,'<') : cs) = tag cs
tokens' [] = []
tokens' cs@((n,_):_) = let (as,bs) = breakn ('<' ==) cs
                       in foldr cvt (tokens' bs) (decode_text as)

  -- XXX: Note, some of the lines might be a bit inacuarate
  where
    cvt (TxtBit x)  cont
      | T.all isChar dat = TokText CData { cdVerbatim = CDataText, cdData = dat } : cont
      | otherwise        = [TokError n "invalid code-point in text content"]
      where
        dat = T.pack x
    cvt (CRefBit x) cont = case cref_to_char x of
      Just c
        | isChar c -> TokText CData { cdVerbatim = CDataText, cdData = T.singleton c } : cont
        | otherwise -> [TokError n "invalid character reference"]
      Nothing -> TokCRef (fromString x) : cont

--
-- PI        ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
-- PITarget  ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
procins :: Pos -> LString -> [Token]
procins n0 = go ""
  where
    go acc ((_,'?') : (_,'>') : ds) = mkPI (reverse acc) (tokens' ds)
    go acc ((_,c) : ds)             = go (c:acc) ds
    go _   []                       = eofErr

    mkPI :: String -> [Token] -> [Token]
    mkPI s0 ts
      | tgt == "xml"  = mkXMLDecl s' ts
      | map toLower (TS.unpack tgt) == "xml" = [TokError (n0+2) "Invalid PI name"]
      | otherwise     = TokPI n0 (PI tgt payload) : ts
      where
        (tgt0,s') = break isS s0
        tgt = TS.fromString tgt0
        payload = T.pack (dropWhile isS s')

    {-
       XMLDecl      ::=  '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
       VersionInfo  ::=  S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
       EncodingDecl ::=  S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
       SDDecl       ::=  S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))

       Eq           ::=  S? '=' S?

       VersionNum   ::=  '1.0'
       EncName      ::=  [A-Za-z] ([A-Za-z0-9._] | '-')*

    -}

    -- needs serious rewrite...
    mkXMLDecl s0 ts
      | n0 > 0    = [TokError n0 "XML declaration allowed only at the start of the document"]
      | otherwise = go1 (simpleTokenize s0)
      where
        go1 ("":"version":"=":ver:rest)
          | Just "1.0" <- unbrack ver = go2 rest
        go1 _ = [TokError n0 "Unsupported or missing 'version' in XML declaration"]

        go2 ("":"encoding":"=":enc:rest)
          | Just enc' <- unbrack enc, isEnc enc' = go3 (Just $ TS.pack enc') rest
          | otherwise = [TokError n0 "Bad 'encoding' value in XML declaration"]
        go2 rest = go3 Nothing rest

        go3 enc ("":"standalone":"=":sd:rest)
          | Just sd' <- unbrack sd, Just sd'' <- isBoo sd' = go4 enc (Just sd'') rest
          | otherwise = [TokError n0 "Bad 'standalone' value in XML declaration"]
        go3 enc rest = go4 enc Nothing rest

        go4 enc sd [] = TokXmlDecl (XmlDeclaration enc sd) : ts
        go4 enc sd [""] = TokXmlDecl (XmlDeclaration enc sd) : ts
        go4 _ _ _ = [TokError n0 "unexpected or malformed attribute in XML declaration"]

        isEnc [] = False
        isEnc (c:cs) = (isAsciiLower c || isAsciiUpper c) &&
                       all (\c' -> isAsciiLower c' || isAsciiUpper c' || isDigit c' || c' `elem` ['.','_','-']) cs

        isBoo "yes" = Just True
        isBoo "no"  = Just False
        isBoo _     = Nothing

        unbrack ('\'':xs) | Just (s,'\'') <- unsnoc xs = Just s
        unbrack ('"':xs)  | Just (s,'"')  <- unsnoc xs = Just s
        unbrack _         = Nothing


special :: LString -> [Token]
-- <!--
--
-- Comment ::=  '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
special ((_,'-') : (_,'-') : cs) = go "" cs
  where
    go acc ((n,'-') : (_,'-') : (_,x) : ds)
      | x == '>' = TokComment (Comment $ T.pack (reverse acc)) : tokens' ds
      | otherwise = [TokError n "double hyphen within comment"]
    go acc ((_,c) : ds) = go (c:acc) ds
    go _ [] = eofErr

-- <![CDATA[
special ((n,'[') : (_,'C') : (_,'D') : (_,'A') : (_,'T') : (_,'A') : (_,'[') : cs) =
  let (xs,ts) = cdata cs
      dat = T.pack xs
  in if T.all isChar dat then TokText CData { cdVerbatim = CDataVerbatim, cdData = dat } : tokens' ts
                         else [TokError (n-2) "invalid code-point in CDATA block"]
  where
    cdata ((_,']') : (_,']') : (_,'>') : ds) = ([],ds)
    cdata ((_,d) : ds)  = let (xs,ys) = cdata ds in (d:xs,ys)
    cdata [] = ([],[])

-- <!DOCTYPE
special ((_,'D') : (_,'O') : (_,'C') : (_,'T') : (_,'Y') : (_,'P') : (_,'E') : cs) =
  let (xs,ts) = munch "" 0 cs in TokDTD (T.pack (reverse xs)) : tokens' ts
  where
    munch acc nesting ((_,'>') : ds)
     | nesting == (0::Int)            = (acc,ds)
     | otherwise                      = munch ('>':acc) (nesting-1) ds
    munch acc nesting ((_,'<') : ds)  = munch ('<':acc) (nesting+1) ds
    munch acc n ((_,x) : ds)          = munch (x:acc) n ds
    munch acc _ []                    = (acc,[]) -- unterminated DTD markup

special ((n,_):_) = [TokError (n-1) "invalid element name"]
special [] = eofErr

qualName :: LString -> (QName,LString)
qualName xs = (QName { qURI    = nullNs
                     , qPrefix = fmap fromString q
                     , qLName  = LName (fromString n)
                     }, bs)
  where
    (as,bs) = breakn endName xs
    (q,n)   = case break (':'==) as of
                (q1,_:n1) -> (Just q1, n1)
                _         -> (Nothing, as)

    endName x = isS x || x == '=' || x == '>' || x == '/'

{-

EmptyElemTag  ::=  '<' Name (S Attribute)* S? '/>'
STag          ::=  '<' Name (S Attribute)* S? '>'
ETag          ::=  '</' Name S? '>'

Attribute     ::=  Name Eq AttValue

-}
tag :: LString -> [Token]
tag ((p,'/') : cs)
  | isValidQName n
  = TokEnd p n : case dropSpace ds of
                   (_,'>') : es -> tokens' es
                   -- tag was not properly closed...
                   (p',_) : _   -> [TokError p' "expected '>'"]
                   []           -> eofErr
  | otherwise = [TokError p "invalid element name"]
  where
    (n,ds) = qualName (dropSpace cs)
tag [] = eofErr
tag cs@((pos,_):_)
  | not (isValidQName n) = [TokError pos "invalid element name"]
  | not (all (isValidQName . attrKey) as) = [TokError pos "invalid attribute name"]
  | not (all (T.all isChar . attrVal) as) = [TokError pos "invalid attribute value"]
  | otherwise            = TokStart pos n as b : ts
  where
    (n,ds)    = qualName cs
    (as,b,ts) = attribs (dropSpace ds)


attribs :: LString -> ([Attr], Bool, [Token])
attribs cs = case cs of
    (_,'>') : ds -> ([], False, tokens' ds)
    (_,'/') : ds -> ([], True, case ds of
                            (_,'>') : es -> tokens' es
                            (pos,_) : _  -> [TokError pos "expected '>'"]
                            []           -> eofErr)
    (_,'?') : (_,'>') : ds -> ([], True, tokens' ds)

    -- doc ended within a tag..
    []       -> ([],False,eofErr)

    _        -> let (a,cs1) = attrib cs
                    (as,b,ts) = attribs cs1
                in (a:as,b,ts)

attrib :: LString -> (Attr,LString)
attrib cs = ((Attr ks (fromString $ decode_attr vs)),dropSpace cs2)
  where
    (vs,cs2) = attr_val (dropSpace cs1)
    (ks,cs1) = qualName cs

{-
AttValue       ::=  '"' ([^<&"] | Reference)* '"'
                 |  "'" ([^<&'] | Reference)* "'"
-}
attr_val           :: LString -> (String,LString)
attr_val ((_,'=') : cs0) = string (dropSpace cs0)
  where
    -- | Match the value for an attribute.
    string :: LString -> (String,LString)
    string ((_,'"') : cs)  = break' ('"' ==) cs
    string ((_,'\'') : cs) = break' ('\'' ==) cs
    -- hack: inject invalid \0 character to trigger failure in caller
    string cs              = ("\0",cs)
attr_val cs = ("\0",cs)

dropSpace :: LString -> LString
dropSpace = dropWhile (isS . snd)

break' :: (a -> Bool) -> [(b,a)] -> ([a],[(b,a)])
break' p xs         = let (as,bs) = breakn p xs
                      in (as, case bs of
                                []     -> []
                                _ : cs -> cs)

breakn :: (a -> Bool) -> [(b,a)] -> ([a],[(b,a)])
breakn p l = (map snd as,bs) where (as,bs) = break (p . snd) l

decode_attr :: String -> String
decode_attr cs = concatMap cvt (decode_text cs)
  where
    cvt (TxtBit x) = norm x
    cvt (CRefBit x)
      | Just c <- cref_to_char x = [c]
      | otherwise                = "\0" -- triggers error lateron

    norm []         = []
    norm ('\x9':xs) = '\x20' : norm xs
    norm ('\xA':xs) = '\x20' : norm xs
    norm (x:xs)     = x : norm xs

data Txt = TxtBit String | CRefBit String deriving Show

decode_text :: [Char] -> [Txt]
decode_text xs@('&' : cs) = case break (';' ==) cs of
                              (as,_:bs) -> CRefBit as : decode_text bs
                              _         -> [TxtBit xs]
decode_text []  = []
decode_text cs  = let (as,bs) = break ('&' ==) cs
                  in TxtBit as : decode_text bs

cref_to_char :: [Char] -> Maybe Char
cref_to_char cs = case cs of
  '#' : ds -> num_esc ds
  "lt"     -> Just '<'
  "gt"     -> Just '>'
  "amp"    -> Just '&'
  "apos"   -> Just '\''
  "quot"   -> Just '"'
  _        -> Nothing

num_esc :: String -> Maybe Char
num_esc cs = case cs of
               'x' : ds -> check (readHex ds)
               _        -> check (reads cs)

  where check [(n,"")] = cvt_char n
        check _        = Nothing

cvt_char :: Int -> Maybe Char
cvt_char x
  | fromEnum (minBound :: Char) <= x && x <= fromEnum (maxBound::Char)
                = Just (toEnum x)
  | otherwise = Nothing


simpleTokenize :: String -> [String]
simpleTokenize [] = []
simpleTokenize (c:cs)
  | isSorEQ c = let (sep,rest) = span isSorEQ (c:cs)
                in  (if ('=' `elem` sep) then "=" else "") : simpleTokenize rest

  | c == '\'' = case break (== '\'') cs of
                  (_,"")       -> [c:cs]
                  (str,_:rest) -> (c:str++"'") : simpleTokenize rest
  | c == '"' = case break (== '"') cs of
                  (_,"")       -> [c:cs]
                  (str,_:rest) -> (c:str++"\"") : simpleTokenize rest
  | otherwise = let (t,rest) = break isSorEQ (c:cs)
                in t : simpleTokenize rest
  where
    isSorEQ x = isS x || x == '='


isValidQName :: QName -> Bool
isValidQName (QName { qPrefix = Just pfx, qLName = LName ln }) = isNCName (TS.unpack pfx) && isNCName (TS.unpack ln)
isValidQName (QName { qPrefix = Nothing, qLName = LName ln })  = isNCName (TS.unpack ln)

