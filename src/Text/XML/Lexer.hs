{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module    : Text.XML.Lexer
-- Copyright : (c) Galois, Inc. 2007
--             (c) Herbert Valerio Riedel 2019
-- SPDX-License-Identifier: BSD-3-Clause AND GPL-3.0-or-later
--
module Text.XML.Lexer where

import           Common
import           Text.XML.Types

import qualified Data.Text      as TS
import qualified Data.Text      as T
import qualified Data.Text.Lazy as TL
import           Numeric        (readHex)

class XmlSource s where
  uncons :: s -> Maybe (Char,s)

instance XmlSource String where
  uncons (c:s) = Just (c,s)
  uncons ""    = Nothing

instance XmlSource TS.Text where
  uncons = TS.uncons

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
                        deriving (Show,Data,Typeable,Generic)

instance NFData Token

eofErr :: [Token]
eofErr = [TokError (-1) "Premature EOF"]

-- | Run XML lexer over 'XmlSource'
scanXML :: XmlSource source => source -> [Token]
scanXML = tokens' . go 0
  where
    go !n src = case uncons src of
      Just (c,src') -> (n,c) : go (n+1) src'
      Nothing       -> []


tokens' :: LString -> [Token]
tokens' ((_,'<') : c@(_,'!') : cs) = special c cs
tokens' ((_,'<') : cs) = tag cs
tokens' [] = []
tokens' cs@((_,_):_) = let (as,bs) = breakn ('<' ==) cs
                       in map cvt (decode_text as) ++ tokens' bs

  -- XXX: Note, some of the lines might be a bit inacuarate
  where cvt (TxtBit x)  = TokText CData { cdVerbatim = CDataText
                                        , cdData = fromString x
                                        }
        cvt (CRefBit x) = case cref_to_char x of
                            Just c -> TokText CData { cdVerbatim = CDataText
                                                    , cdData = T.singleton c
                                                    }
                            Nothing -> TokCRef (fromString x)


special :: LChar -> LString -> [Token]
special (_,_) ((_,'-') : (_,'-') : cs) = skip cs
  where
    skip ((pos,'-') : (_,'-') : (_,x) : ds)
      | x == '>' = tokens' ds
      | otherwise = [TokError pos "double hyphen within comment"]
    skip (_ : ds)                           = skip ds
    skip []                                 = eofErr

special _ ((_,'[') : (_,'C') : (_,'D') : (_,'A') : (_,'T') : (_,'A') : (_,'[') : cs) =
  let (xs,ts) = cdata cs
  in TokText CData { cdVerbatim = CDataVerbatim
                   , cdData = fromString xs
                   } : tokens' ts
  where cdata ((_,']') : (_,']') : (_,'>') : ds) = ([],ds)
        cdata ((_,d) : ds)  = let (xs,ys) = cdata ds in (d:xs,ys)
        cdata [] = ([],[])

special _ cs =
  let (xs,ts) = munch "" 0 cs
  in TokText CData { cdVerbatim = CDataRaw
                   , cdData = fromString ('<':'!':reverse xs)
                   } : tokens' ts
  where munch acc nesting ((_,'>') : ds)
         | nesting == (0::Int) = ('>':acc,ds)
         | otherwise           = munch ('>':acc) (nesting-1) ds
        munch acc nesting ((_,'<') : ds)
         = munch ('<':acc) (nesting+1) ds
        munch acc n ((_,x) : ds) = munch (x:acc) n ds
        munch acc _ [] = (acc,[]) -- unterminated DTD markup
--special c cs = tag (c : cs) -- invalid specials are processed as tags

qualName :: LString -> (QName,LString)
qualName xs = (QName { qURI    = Nothing
                     , qPrefix = fmap fromString q
                     , qLName  = LName (fromString n)
                     }, bs)
  where
    (as,bs) = breakn endName xs
    (q,n)   = case break (':'==) as of
                (q1,_:n1) -> (Just q1, n1)
                _         -> (Nothing, as)

    endName x = isSpace x || x == '=' || x == '>' || x == '/'

{-

EmptyElemTag  ::=  '<' Name (S Attribute)* S? '/>'
STag          ::=  '<' Name (S Attribute)* S? '>'
ETag          ::=  '</' Name S? '>'

Attribute     ::=  Name Eq AttValue

-}
tag :: LString -> [Token]
tag ((p,'/') : cs)
  = TokEnd p n : case dropSpace ds of
                   (_,'>') : es -> tokens' es
                   -- tag was not properly closed...
                   (p',_) : _   -> [TokError p' "expected '>'"]
                   []           -> eofErr
  where
    (n,ds) = qualName (dropSpace cs)
tag [] = eofErr
tag cs
  = TokStart (fst (head cs)) n as b : ts
  where
    (n,ds)    = qualName cs
    (as,b,ts) = attribs (dropSpace ds)


attribs          :: LString -> ([Attr], Bool, [Token])
attribs cs        = case cs of
                      (_,'>') : ds -> ([], False, tokens' ds)
                      (_,'/') : ds -> ([], True, case ds of
                                              (_,'>') : es -> tokens' es
                                              (pos,_) : _ -> [TokError pos "expected '>'"]
                                              [] -> eofErr)
                      (_,'?') : (_,'>') : ds -> ([], True, tokens' ds)

                      -- doc ended within a tag..
                      []       -> ([],False,eofErr)

                      _        -> let (a,cs1) = attrib cs
                                      (as,b,ts) = attribs cs1
                                  in (a:as,b,ts)

attrib             :: LString -> (Attr,LString)
attrib cs           = let (ks,cs1)  = qualName cs
                          (vs,cs2)  = attr_val (dropSpace cs1)
                      in ((Attr ks (fromString $ decode_attr vs)),dropSpace cs2)

{-
AttValue       ::=  '"' ([^<&"] | Reference)* '"'
                 |  "'" ([^<&'] | Reference)* "'"
-}
attr_val           :: LString -> (String,LString)
attr_val ((_,'=') : cs0) = string (dropSpace cs0)
  where
    -- | Match the value for an attribute.  For malformed XML we do
    -- our best to guess the programmer's intention.
    string :: LString -> (String,LString)
    string ((_,'"') : cs)   = break' ('"' ==) cs
    string ((_,'\'') : cs)  = break' ('\'' ==) cs
    -- Allow attributes that are not enclosed by anything.
    string cs           = breakn eos cs
      where eos x = isSpace x || x == '>' || x == '/'
attr_val cs             = ("",cs)


{-

S ::= (#x20 | #x9 | #xD | #xA)+

-}
dropSpace :: LString -> LString
dropSpace = dropWhile (isSpace . snd)

isSpace :: Char -> Bool
isSpace = (`elem` "\x20\x09\x0D\x0A")


break' :: (a -> Bool) -> [(b,a)] -> ([a],[(b,a)])
break' p xs         = let (as,bs) = breakn p xs
                      in (as, case bs of
                                []     -> []
                                _ : cs -> cs)

breakn :: (a -> Bool) -> [(b,a)] -> ([a],[(b,a)])
breakn p l = (map snd as,bs) where (as,bs) = break (p . snd) l


decode_attr :: String -> String
decode_attr cs = concatMap cvt (decode_text cs)
  where cvt (TxtBit x) = x
        cvt (CRefBit x) = case cref_to_char x of
                            Just c  -> [c]
                            Nothing -> '&' : x ++ ";"

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
