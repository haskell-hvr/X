{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- |
-- Module    : Text.XML.XPath
-- Copyright : (c) Herbert Valerio Riedel 2019
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- <https://www.w3.org/TR/1999/REC-xpath-19991116/ XML Path Language (XPath 1.0)>
--
module Text.XML.XPath where

import           Common             as Co hiding ((<|>))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict    as Map
import qualified Data.Text          as T
import qualified Data.Text.Short    as TS
import           Text.Parsec        as P hiding (sepBy1)
import           Text.Parsec.Error  as P
import           Text.Parsec.Pos    as P
import           Text.Parsec.Text
import           Text.XML.Types
import           Utils


data Value = VNumber  Number
           | VString  Text
           | VBool    Bool
           | VNodeSet NodeSet
  deriving Show



data NodeSet = NodeSet NSRoot
  deriving Show

data NSRoot = NSRoot [Bool] -- prolog
                     (Bool,NSElem) -- root element
                     [Bool] -- epilog
  deriving Show

data NSElem = NSElem [Bool] [(Bool,Maybe NSElem)]
  deriving Show

-- | Fails if document contains unresolved CRefs or (currently unsupported) DTDs
nodeSetFromRoot :: Root -> Maybe NodeSet
nodeSetFromRoot Root { rootDoctype = Just _ } = Nothing
nodeSetFromRoot (Root _ prolog Nothing el0 epilog) = do
    el' <- go el0
    pure $! NodeSet $ NSRoot (True <$ prolog) (True,el') (True <$ epilog)
  where
    go :: Element -> Maybe NSElem
    go (Element _ as cs) = do
      cs' <- mapM go2 cs
      pure $! NSElem (True <$ as) cs'

    go2 :: Content -> Maybe (Bool,Maybe NSElem)
    go2 (Text _)  = pure (True,Nothing)
    go2 (CRef _)  = Nothing
    go2 (Proc _)  = pure (True,Nothing)
    go2 (Comm _)  = pure (True,Nothing)
    go2 (Elem el) = ((,) True . Just) <$> go el


data Path0 = Path0Prolog !Word
           | Path0Elem    Path1
           | Path1Epilog !Word
           deriving (Eq,Ord,Show)

data Path1 = Path1Attr !Word
           | Path1Elem !Word (Maybe Path1)
           deriving (Eq,Ord,Show)

data Context = Context
  { ctxNode     :: Path0
  , ctxPosition :: !Word
  , ctxSize     :: !Word
  , ctxVars     :: Map QName Value
  , ctxFuns     :: Map QName ()
  , ctxNsEnv    :: NSEnv
  }

evalExpr :: Context -> Expr -> Either String Value
evalExpr _ _ = Left "FIXME"

-- evalStep :: Context -> Step -> Context

----------------------------------------------------------------------------

parseLocationPath :: NSEnv -> Text -> Either (Pos,String) LocationPath
parseLocationPath nse t = first (cvtErr t) $ P.runParser (p_S0 *> p_LocationPath  <* eof) nse "" t

parseExpr :: NSEnv -> Text -> Either (Pos,String) Expr
parseExpr nse t = first (cvtErr t) (P.runParser (p_S0 *> p_Expr <* eof) nse "" t)


exprFromLocationPath :: LocationPath -> Expr
exprFromLocationPath lp
  = OrExpr
    (AndExpr
     (EqualityExpr
      (RelationalExpr
       (AdditiveExpr
        (MultiplicativeExpr
         (UnaryExpr
          (UnionExpr
           (PathExpr lp :| [])
          ),[]),[]),[]),[]) :| []) :| [])


cvtErr :: Text -> P.ParseError -> (Pos,String)
cvtErr orig err = (go 0 (initialPos "") (T.unpack orig),msg)
  where
    -- reconstruct offset position from line/col
    go :: Pos -> SourcePos -> [Char] -> Pos
    go !pos _spos [] = pos
    go !pos spos (c:cs)
      | epos <= spos = pos
      | otherwise = go (pos+1) spos' cs
      where
        spos' = updatePosChar spos c
        epos = errorPos err

    msg = concatMap lf $
      showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages err)

    lf '\n' = "; "
    lf c    = [c]

----------------------------------------------------------------------------
-- XPath 1.0 grammar productions

{- |

> [1] LocationPath ::= RelativeLocationPath
>                    | AbsoluteLocationPath

-}
p_LocationPath :: GenParser St LocationPath
p_LocationPath = (Left <$> p_RelativeLocationPath) <|> (Right <$> p_AbsoluteLocationPath)

type LocationPath = Either RelativeLocationPath AbsoluteLocationPath

{- |

> [2] AbsoluteLocationPath ::= '/' RelativeLocationPath?
>                            | AbbreviatedAbsoluteLocationPath

> [10] AbbreviatedAbsoluteLocationPath ::= '//' RelativeLocationPath

> '//' == '/descendant-or-self::node()/'

-}
p_AbsoluteLocationPath :: GenParser St AbsoluteLocationPath
p_AbsoluteLocationPath = do
    sep <- pathSep
    ALP <$> case sep of
      NotAbbrev -> P.option [] (unRLP <$> p_RelativeLocationPath)
      Abbrev    -> ((:) abbrevStep . unRLP) <$> p_RelativeLocationPath
  where
    unRLP (RLP (x :| xs)) = x:xs

newtype AbsoluteLocationPath = ALP [Step]
  deriving Show

{- |

> [3] RelativeLocationPath ::= Step
>                            | RelativeLocationPath '/' Step
>                            | AbbreviatedRelativeLocationPath

> [11] AbbreviatedRelativeLocationPath ::= RelativeLocationPath '//' Step

> '//' == '/descendant-or-self::node()/'

-}
p_RelativeLocationPath :: GenParser St RelativeLocationPath
p_RelativeLocationPath = (RLP . go) <$> (p_Step `sepBy1'` pathSep)
  where
    go (x,xs) = x :| go2 xs
    go2 []                     = []
    go2 ((NotAbbrev, step):xs) = step : go2 xs
    go2 ((Abbrev, step):xs)    = abbrevStep : step : go2 xs

data RelativeLocationPath = RLP (NonEmpty Step)
  deriving Show

{- |

> [4] Step ::= AxisSpecifier NodeTest Predicate*
>            | AbbreviatedStep

> [12] AbbreviatedStep ::= '.' | '..'

> '.' == 'self::node()'

> '..' == 'parent::node()'

-}
p_Step :: GenParser St Step
p_Step = choice
    [ abbrevDotDot <$ lxms' ".."
    , abbrevDot <$ lxm (try (P.char '.' <* P.notFollowedBy digit))
    , Step <$> p_AxisSpecifier <*> p_NodeTest <*> P.many p_Predicate
    ]
  where
    abbrevDot    = Step (AS AN_Self)   (NT_NType NT_Node) []
    abbrevDotDot = Step (AS AN_Parent) (NT_NType NT_Node) []

data Step = Step AxisSpecifier NodeTest [Predicate]
  deriving Show

-- | > descendant-or-self::node()
abbrevStep :: Step
abbrevStep = Step (AS AN_DescendantOrSelf) (NT_NType NT_Node) []

{- |

> [5] AxisSpecifier ::= AxisName '::'
>                     | AbbreviatedAxisSpecifier

> [13] AbbreviatedAxisSpecifier ::= '@'?

> '' == 'child::'

> '@' == 'attribute::'

-}
p_AxisSpecifier :: GenParser St AxisSpecifier
p_AxisSpecifier = AS <$> choice
  [ try (p_AxisName <* lxms "::")
  , AN_Attribute <$ lxmc '@'
  , pure AN_Child
  ]

data AxisSpecifier = AS AxisName
  deriving Show

{- |

> [6] AxisName ::= 'ancestor'
>                | 'ancestor-or-self'
>                | 'attribute'
>                | 'child'
>                | 'descendant'
>                | 'descendant-or-self'
>                | 'following'
>                | 'following-sibling'
>                | 'namespace'
>                | 'parent'
>                | 'preceding'
>                | 'preceding-sibling'
>                | 'self'

-}
p_AxisName :: GenParser St AxisName
p_AxisName = choice
  [ AN_AncestorOrSelf    <$ lxms' "ancestor-or-self"
  , AN_Ancestor          <$ lxms' "ancestor"
  , AN_Attribute         <$ lxms' "attribute"
  , AN_Child             <$ lxms' "child"
  , AN_DescendantOrSelf  <$ lxms' "descendant-or-self"
  , AN_Descendant        <$ lxms' "descendant"
  , AN_FollowingSibling  <$ lxms' "following-sibling"
  , AN_Following         <$ lxms' "following"
  , AN_Namespace         <$ lxms' "namespace"
  , AN_Parent            <$ lxms' "parent"
  , AN_PrecedingSibling  <$ lxms' "preceding-sibling"
  , AN_Preceding         <$ lxms' "preceding"
  , AN_Self              <$ lxms' "self"
  ]

data AxisName
  = AN_Ancestor
  | AN_AncestorOrSelf
  | AN_Attribute
  | AN_Child
  | AN_Descendant
  | AN_DescendantOrSelf
  | AN_Following
  | AN_FollowingSibling
  | AN_Namespace
  | AN_Parent
  | AN_Preceding
  | AN_PrecedingSibling
  | AN_Self
  deriving Show


{- |

[7] NodeTest ::= NameTest
               | NodeType '(' ')'
               | 'processing-instruction' '(' Literal ')'

-}

p_NodeTest :: GenParser St NodeTest
p_NodeTest = choice
  [ NT_PI <$> (lxms' "processing-instruction" *> lxmc '(' *> p_Literal <* lxmc ')')
  , NT_NType <$> try (p_NodeType <* P.char '(') <* P.char ')'
  , NT_NTest <$> p_NameTest
  ]

data NodeTest = NT_NTest NameTest
              | NT_NType NodeType
              | NT_PI Literal
  deriving Show

{- |

> [8] Predicate ::= '[' PredicateExpr ']'
> [9] PredicateExpr ::= Expr

-}
p_Predicate :: GenParser St Predicate
p_Predicate = Predicate <$> (P.char '[' *> p_Expr <* P.char ']')

newtype Predicate = Predicate Expr
  deriving Show

----------------------------------------------------------------------------
-- Expressions

{- |

> [14] Expr ::= OrExpr

-}
p_Expr :: GenParser St Expr
p_Expr = p_OrExpr

type Expr = OrExpr

{- |

> [15] PrimaryExpr ::= VariableReference
>                    | '(' Expr ')'
>                    | Literal
>                    | Number
>                    | FunctionCall

> [36] VariableReference ::= '$' QName

-}

p_PrimaryExpr :: GenParser St PrimaryExpr
p_PrimaryExpr = choice
  [ PE_Expr <$> (lxmc '(' *> p_Expr <* lxmc ')')
  , PE_Lit  <$> p_Literal
  , PE_VRef <$> (P.char '$' *> p_QName)
  , PE_Num  <$> p_Number
  , PE_Fun  <$> p_FunctionCall
  ]

data PrimaryExpr = PE_VRef QName
                 | PE_Expr Expr
                 | PE_Lit  Literal
                 | PE_Num !Number
                 | PE_Fun  FunctionCall
                 deriving Show

{- |

> [16] FunctionCall ::= FunctionName '(' ( Argument ( ',' Argument )* )? ')'
> [17] Argument     ::= Expr

> [35] FunctionName ::= QName - NodeType

-}

data FunctionCall = FunctionCall QName [Expr]
                  deriving Show

p_FunctionCall :: GenParser St FunctionCall
p_FunctionCall = FunctionCall <$> p_FunctionName <*> args
  where
    args :: GenParser St [Expr]
    args = lxmc '(' *> (p_Expr `P.sepBy` (lxmc ',')) <* lxmc ')'

    p_FunctionName = lxm $ do
      fn <- p_QName
      when (isNodeType fn) $ fail "reserved FunctionName"
      pure fn

    isNodeType (QName { qPrefix = Nothing, qLName = ln })
      = ln `elem` ["comment", "text", "processing-instruction", "node"]
    isNodeType (QName { qPrefix = Just _ }) = False

{- |

> [18] UnionExpr  ::= PathExpr
>                   | UnionExpr '|' PathExpr
> [19] PathExpr   ::= LocationPath
>                   | FilterExpr
>                   | FilterExpr '/' RelativeLocationPath
>                   | FilterExpr '//' RelativeLocationPath
> [20] FilterExpr ::= PrimaryExpr
>                   | FilterExpr Predicate

-}

p_UnionExpr :: GenParser St UnionExpr
p_UnionExpr = UnionExpr <$> (p_PathExpr `sepBy1` lxmc '|')

newtype UnionExpr = UnionExpr (NonEmpty PathExpr)
                  deriving Show

p_PathExpr :: GenParser St PathExpr
p_PathExpr = choice
  [ PathExpr <$> p_LocationPath
  , PathExprFilter <$> p_FilterExpr <*> P.option [] go
  ]
  where
    go = do
      sep <- pathSep
      case sep of
        NotAbbrev -> unRLP <$> p_RelativeLocationPath
        Abbrev    -> ((:) abbrevStep . unRLP) <$> p_RelativeLocationPath

    unRLP (RLP (x :| xs)) = x:xs

data PathExpr = PathExpr LocationPath
              | PathExprFilter FilterExpr [Step]
              deriving Show

p_FilterExpr :: GenParser St FilterExpr
p_FilterExpr = FilterExpr <$> p_PrimaryExpr <*> P.many p_Predicate

data FilterExpr = FilterExpr PrimaryExpr [Predicate]
                deriving Show

{- |

> [21] OrExpr         ::= AndExpr
>                       | OrExpr 'or' AndExpr
> [22] AndExpr        ::= EqualityExpr
>                       | AndExpr 'and' EqualityExpr
> [23] EqualityExpr   ::= RelationalExpr
>                       | EqualityExpr '=' RelationalExpr
>                       | EqualityExpr '!=' RelationalExpr
> [24] RelationalExpr ::= AdditiveExpr
>                       | RelationalExpr '<' AdditiveExpr
>                       | RelationalExpr '>' AdditiveExpr
>                       | RelationalExpr '<=' AdditiveExpr
>                       | RelationalExpr '>=' AdditiveExpr

-}

p_OrExpr :: GenParser St OrExpr
p_OrExpr = OrExpr <$> (p_AndExpr `sepBy1` try (lxms "or"))

newtype OrExpr = OrExpr (NonEmpty AndExpr) deriving Show

--

p_AndExpr :: GenParser St AndExpr
p_AndExpr = AndExpr <$> (p_EqualityExpr `sepBy1` try (lxms "and"))

newtype AndExpr = AndExpr (NonEmpty EqualityExpr) deriving Show

--

p_EqualityExpr :: GenParser St EqualityExpr
p_EqualityExpr = EqualityExpr <$> (p_RelationalExpr `sepBy1'` sep)
  where
    sep = (Equal <$ lxmc '=') <|> (NotEqual <$ try (lxms "!="))

newtype EqualityExpr = EqualityExpr (RelationalExpr `SepBy1` EqOp) deriving Show
data EqOp = Equal | NotEqual deriving Show

--

p_RelationalExpr :: GenParser St RelationalExpr
p_RelationalExpr = RelationalExpr <$> (p_AdditiveExpr `sepBy1'` sep)
  where
    sep = choice
      [ Rel_LE <$ try (lxms "<=")
      , Rel_GE <$ try (lxms ">=")
      , Rel_LT <$ lxmc '<'
      , Rel_GT <$ lxmc '>'
      ]

newtype RelationalExpr = RelationalExpr (AdditiveExpr `SepBy1` RelOp) deriving Show
data RelOp = Rel_LT | Rel_GT | Rel_LE | Rel_GE deriving Show

{- |

> [25] AdditiveExpr       ::= MultiplicativeExpr
>                           | AdditiveExpr '+' MultiplicativeExpr
>                           | AdditiveExpr '-' MultiplicativeExpr
> [26] MultiplicativeExpr ::= UnaryExpr
>                           | MultiplicativeExpr MultiplyOperator UnaryExpr
>                           | MultiplicativeExpr 'div' UnaryExpr
>                           | MultiplicativeExpr 'mod' UnaryExpr
> [27] UnaryExpr          ::= UnionExpr
>                           | '-' UnaryExpr

-}

p_AdditiveExpr :: GenParser St AdditiveExpr
p_AdditiveExpr = AdditiveExpr <$> (p_MultiplicativeExpr `sepBy1'` sep)
  where
    sep = (Plus <$ lxmc '+') <|> (Minus <$ lxmc '-')

newtype AdditiveExpr = AdditiveExpr (MultiplicativeExpr `SepBy1` AddOp) deriving Show
data AddOp = Plus | Minus deriving Show

--

p_MultiplicativeExpr :: GenParser St MultiplicativeExpr
p_MultiplicativeExpr = MultiplicativeExpr <$> (p_UnaryExpr `sepBy1'` sep)
  where
    sep = choice
      [ Mul <$ lxmc '*'
      , Div <$ try (lxms "div")
      , Mod <$ try (lxms "mod")
      ]

newtype MultiplicativeExpr = MultiplicativeExpr (UnaryExpr `SepBy1` MulOp) deriving Show
data MulOp = Mul | Div | Mod deriving Show

--

p_UnaryExpr :: GenParser St UnaryExpr
p_UnaryExpr = (UnionExprNeg <$> (lxmc '-' *> p_UnaryExpr)) <|>
              (UnaryExpr <$> p_UnionExpr)

data UnaryExpr = UnaryExpr UnionExpr
               | UnionExprNeg UnaryExpr
               deriving Show

----------------------------------------------------------------------------
-- Lexical Structure, i.e. [28] ExprToken

{- |

> [29] Literal ::= '"' [^"]* '"'
>                | "'" [^']* "'"

-}
p_Literal :: GenParser st Literal
p_Literal = quoted '\'' <|> quoted '"'
  where
    quoted :: Char -> GenParser st Literal
    quoted c = id <$ P.char c *> P.many (P.satisfy (/= c)) <* lxmc c

type Literal = String

{- |

> [30] Number ::= Digits ('.' Digits?)?
>               | '.' Digits
> [31] Digits ::= [0-9]+

-}
p_Number :: GenParser st Number
p_Number = (read . ('0':)) <$> lxm p_Number'
  where
    p_Number' = (((++) <$> many1 digit <*> option "" ((:) <$> P.char '.' <*> P.many digit))
                <|> try ((:) <$> P.char '.' <*> P.many1 digit))

type Number = Double -- see section 3.5

{- |

> [37] NameTest ::= '*'
>                 | NCName ':' '*'
>                 | QName

-}
p_NameTest :: GenParser St NameTest
p_NameTest = lxm $ choice
  [ NameTest_Any <$  P.char '*'
  , NameTest_NS  <$> try (p_NCName <* P.char ':' <* P.char '*')
  , NameTest_QN  <$> try (p_QName <* P.notFollowedBy (lxmc '(')) -- see section 3.7
  ]

data NameTest = NameTest_Any
              | NameTest_NS ShortText
              | NameTest_QN QName
              deriving Show

{- |

> [38] NodeType ::= 'comment'
>                 | 'text'
>                 | 'processing-instruction'
>                 | 'node'

-}
p_NodeType :: GenParser st NodeType
p_NodeType = choice
  [ NT_Comment                <$ lxms "comment"
  , NT_Text                   <$ lxms "text"
  , NT_ProcessingInstruction  <$ lxms "processing-instruction"
  , NT_Node                   <$ lxms "node"
  ]

data NodeType = NT_Comment
              | NT_Text
              | NT_ProcessingInstruction
              | NT_Node
              deriving Show

----------------------------------------------------------------------------
-- aux parsers

-- | NB: never `""`
p_Name :: GenParser st ShortText
p_Name = TS.pack <$> ((:) <$> satisfy isNameStartChar <*> P.many (satisfy isNameChar))

-- | NB: never `""`
p_NCName :: GenParser st ShortText
p_NCName = TS.pack <$> ((:) <$> satisfy (nc isNameStartChar) <*> P.many (satisfy (nc isNameChar)))
  where
    nc f c = c /= ':' && f c

p_QName :: GenParser NSEnv QName
p_QName = do
  p1 <- p_NCName
  p2 <- option "" (P.char ':' *> p_NCName)
  let (pfx,ln) = if TS.null p2 then (Nothing,LName p1) else (Just p1,LName p2)
  nsenv <- getState
  maybe (fail $ "undefined namespace prefix '" ++ maybe "" TS.unpack pfx ++ "'") pure $ mkQName nsenv pfx ln

----------------------------------------------------------------------------
-- helpers

pathSep :: GenParser st Abbrev
pathSep = do
  _ <- P.char '/'
  a <- P.option NotAbbrev (Abbrev <$ P.char '/')
  p_S0
  pure a

data Abbrev = NotAbbrev | Abbrev deriving Show

sepBy1' :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m (a `SepBy1` sep)
sepBy1' p sep = do
  x  <- p
  xs <- P.many ((,) <$> sep <*> p)
  pure (x,xs)

sepBy1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m (NonEmpty a)
sepBy1 p sep = (:|) <$> p <*> P.many (sep *> p)

type SepBy1 a sep = (a,[(sep,a)])

-- lexeme helpers

lxm :: GenParser st a -> GenParser st a
lxm p = p <* p_S0

lxmc :: Char -> GenParser st ()
lxmc c = P.char c *> p_S0

lxms :: String -> GenParser st ()
lxms s = P.string s *> p_S0

-- atomic lxms
lxms' :: String -> GenParser st ()
lxms' s = try (lxms s)


-- whitespace helpers

p_S0 :: GenParser st ()
p_S0 = P.skipMany (satisfy isS)

-- p_S1 :: GenParser st ()
-- p_S1 = P.skipMany1 (satisfy isS)

-- namespace environment

type St = NSEnv

data NSEnv = NSEnv (Map ShortText URI) -- key is either "" or a NCName

nsenv0 :: NSEnv
nsenv0 = NSEnv $ Map.fromList [("xml",xmlNamesNS),("xmlns",xmlnsNS)]

xmlNamesNS :: URI
xmlNamesNS = URI "http://www.w3.org/XML/1998/namespace"

xmlnsNS :: URI
xmlnsNS = URI "http://www.w3.org/2000/xmlns/"


mkQName :: NSEnv -> Maybe NCName -> LName -> Maybe QName
mkQName (NSEnv pfxmap) pfx ln = do
  case pfx of
    Nothing   -> (Just . qn) (Map.lookup "" pfxmap)
    Just pfx' -> (qn . Just) <$> Map.lookup pfx' pfxmap
  where
    qn uri = QName { qLName = ln, qPrefix = pfx, qURI = uri }
