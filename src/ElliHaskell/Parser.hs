module ElliHaskell.Parser where

import ElliHaskell.Syntax
import ElliHaskell.Types

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void (Void)
import Data.Map ((!?))

type ErrParser = Void

type Parser = Parsec ErrParser String

keywords :: [String]
keywords =
    [ "case", "of"
    ]

same :: Eq b => (a -> b) -> [a] -> Bool
same _ []       = True
same f (x:xs)   = all (\y -> f x == f y) xs

------------------------------------------------------------------------
--
-- LEXING
--
------------------------------------------------------------------------

sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

paren :: Parser a -> Parser a
paren = between (symbol "(") (symbol ")")

symbol :: String -> Parser String
symbol = L.symbol sc

------------------------------------------------------------------------
--
-- STATEMENTS
--
------------------------------------------------------------------------
pStmt :: Parser Stmt
pStmt = choice
  [ try $ StmtDecl <$> pDecl
  , try $ StmtEval <$> pExpr
  , pStmtType
  , StmtQuit <$ symbol ":q"
  ] <* eof

pStmtType :: Parser Stmt
pStmtType = do
    void $ try $ symbol ":t"
    StmtType <$> pExpr

------------------------------------------------------------------------
--
-- DECLARATIONS
--
------------------------------------------------------------------------

data PreDecl 
    = TypeDecl Name Type
    | FunDecl Name [Pattern] Expr

pDecl :: Parser Decl
pDecl = go >>= unPreDecl
  where
    go :: Parser [PreDecl]
    go = many $ choice
        [ try pTypeDecl
        , pFunDecl
        ]
    
unPreDecl :: [PreDecl] -> Parser Decl
unPreDecl (dec:rest) 
    | (TypeDecl v t) <- dec = do
        rest' <- go v rest
        if same (length . fst) rest'
            then return $ Decl blank v t rest'
            else fail "Not same patterns"
    | (FunDecl v _ _) <- dec = do
        rest' <- go v (dec:rest)
        return $ Decl blank v TypeAny rest'
  where
    go :: String -> [PreDecl] -> Parser [([Pattern], Expr)]
    go _ ((TypeDecl _ _):_)        = fail "type decl too late"
    go v ((FunDecl v' pats e):decs)   
        | v' /= v = fail "Bad name in same dec"
        | otherwise = do
            decs' <- go v decs
            return $ (pats, e):decs'
    go _ [] = return []
unPreDecl _ = fail "nah"

pTypeDecl :: Parser PreDecl
pTypeDecl = do
    v <- pIdentifier
    void $ symbol "::"
    t <- pType
    void $ symbol ";"
    return $ TypeDecl v t

pFunDecl :: Parser PreDecl
pFunDecl = do
    v <- pIdentifier
    pats <- many pPattern
    void $ symbol "="
    e <- pExpr
    void $ symbol ";"
    return $ FunDecl v pats e


------------------------------------------------------------------------
--
-- EXPRESSIONS
--
------------------------------------------------------------------------

pExpr :: Parser Expr
pExpr = makeExprParser pTerm opTable

-- Operators

fillIfxOp :: Info -> Op -> (Expr -> Expr -> Expr)
fillIfxOp info op e1 = Ifx info e1 op

ifxOpSymbol :: Op -> Parser String
ifxOpSymbol op = case ops !? op of
    Nothing     -> fail "This operator doesn't exist in the op table"
    Just opinfo -> symbol (opinfo_ifxname opinfo) <?> opinfo_label opinfo

opTable :: [[Operator Parser Expr]]
opTable =
    [ [ InfixL (App blank <$ (symbol "" <?> "application")) 
      ]
    , [ InfixL (fillIfxOp blank Mul <$ ifxOpSymbol Mul)
      , InfixL (fillIfxOp blank Div <$ ifxOpSymbol Div)
      ]
    , [ InfixL (fillIfxOp blank Add <$ ifxOpSymbol Add)
      , InfixL (fillIfxOp blank Sub <$ ifxOpSymbol Sub)
      ]
    , [ InfixR (fillIfxOp blank Cons <$ try (ifxOpSymbol Cons >> notFollowedBy (symbol ":")))
      ]
    , [ InfixN (fillIfxOp blank Eq <$ ifxOpSymbol Eq)
      , InfixN (fillIfxOp blank Neq <$ ifxOpSymbol Neq)
      , InfixN (fillIfxOp blank Lt <$ ifxOpSymbol Lt)
      , InfixN (fillIfxOp blank Gt <$ ifxOpSymbol Gt)
      , InfixN (fillIfxOp blank Leq <$ ifxOpSymbol Leq)
      , InfixN (fillIfxOp blank Geq <$ ifxOpSymbol Geq)
      ]
    , [ InfixR (fillIfxOp blank And <$ ifxOpSymbol And)
      ]
    , [ InfixR (fillIfxOp blank Or <$ ifxOpSymbol Or)
      ]
    , [ Postfix pTypeSig
      ]
    ]

pTypeSig :: Parser (Expr -> Expr)
pTypeSig = pTypeAnno (TypeSig blank)

-- Terms

pTerm :: Parser Expr
pTerm = choice
    [ Con blank <$> pCon
    , Var blank <$> pIdentifier
    , pAbstr
    , paren pExpr
    , pList
    , pCase
    ]

pCon :: Parser Constant
pCon = (I <$> pInt) <|> (B <$> pBool)
  where
    pInt :: Parser Int
    pInt = lexeme L.decimal <?> "integer"

    pBool :: Parser Bool
    pBool =
            True <$ symbol "True"
        <|> False <$ symbol "False"
        <?> "boolean"

pIdentifier :: Parser Name
pIdentifier = lexeme $ try $ ident >>= check
  where
    -- https://github.com/mrkkrp/megaparsec-site/blob/master/tutorials/parsing-simple-imperative-language.md
    ident = ((:) <$> letterChar <*> many alphaNumChar) <?> "variable"
    check x =
        if x `elem` keywords
        then fail $ show x ++ " is a keyword"
        else return x

pAbstr :: Parser Expr
pAbstr = do
    void $ symbol "\\"
    pat <- pPattern
    void $ lexeme $ symbol "->"
    Abstr blank pat <$> pExpr

pList :: Parser Expr
pList = between (symbol "[") (symbol "]") $ List blank <$> go
  where
    go :: Parser [Expr]
    go = do
        e <- optional pExpr
        case e of
            Nothing -> return []
            Just e' -> do
                comma <- optional $ symbol ","
                case comma of
                    Nothing -> return [e']
                    Just _  -> do
                        es <- go
                        return $ e':es


pPattern :: Parser Pattern
pPattern = makeExprParser pAPat [[Postfix pTypePat]] 

pTypePat :: Parser (Pattern -> Pattern)
pTypePat = pTypeAnno TypedPat

pAPat :: Parser Pattern
pAPat = choice
    [ ConPat <$> pCon
    , VarPat <$> pIdentifier
    , paren pPattern
    ]

pCase :: Parser Expr
pCase = do
    void (symbol "case")
    e_target <- pExpr
    void (symbol "of")
    alts <- between (symbol "{") (symbol "}") pAlts
    return $ Case blank e_target alts
  where
    pAlts :: Parser [(Pattern, Expr)]
    pAlts = many $ pAlt <* symbol ";"

    pAlt :: Parser (Pattern, Expr)
    pAlt = do
        pat <- pPattern
        void (symbol "->")
        e <- pExpr
        return (pat, e)

------------------------------------------------------------------------
--
-- TYPES
--
------------------------------------------------------------------------

pTypeAnno :: (Type -> a) -> Parser a
pTypeAnno f = try $ do
    void $ symbol "::"
    t <- pType
    return (f t)

pType :: Parser Type
pType = makeExprParser pType' [[InfixR $ TypeAbstr <$ symbol "->"]]

pType' :: Parser Type
pType' = choice
    [ paren pType
    , TypeInt <$ symbol "Int"
    , TypeBool <$ symbol "Bool"
    ]