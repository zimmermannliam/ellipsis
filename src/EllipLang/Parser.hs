{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE OverloadedStrings #-}
module EllipLang.Parser where

import EllipLang.Syntax
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad (void)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import Control.Monad.Combinators.Expr
import Text.Megaparsec.Debug (dbg)
import Debug.Trace

type Parser = Parsec Void Text

resw =
    [ "if", "then", "else"
    , "\\", "->"
    , "case", "of"
    , "..."
    ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

------------------------------------------------------------------------
--
-- Terms
--
------------------------------------------------------------------------

pTerm :: Parser Expr
pTerm = choice
    [ parens pExpr
    , pAbstr
    , pIfThenElse
    , pCase
    , pBool
    , pInt
    , try pListElement
    , pVar
    ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

curlyBraces :: Parser a -> Parser a
curlyBraces = between (symbol "{") (symbol "}")

squareBrackets :: Parser a -> Parser a
squareBrackets = between (symbol "[") (symbol "]")

pAbstr :: Parser Expr
pAbstr = do
    void (char '\\')
    names <- some $ lexeme pIdent
    void $ lexeme (string "->")
    e <- pExpr
    return $ foldr Abstr e names

pIfThenElse :: Parser Expr
pIfThenElse = do
    void $ lexeme (string "if")
    pred <- pExpr
    void $ lexeme (string "then")
    trueCase <- pExpr
    void $ lexeme (string "else")
    falseCase <- pExpr
    return $ Case pred [(PVal $ Boolean True, trueCase), (PVal $ Boolean False, falseCase)]

pCase :: Parser Expr
pCase = do
    void $ lexeme (string "case")
    target <- pExpr
    void $ lexeme (string "of")
    alts <- curlyBraces pAlts
    return $ Case target alts
  where
    pAlts :: Parser Alts 
    pAlts = many $ pAlt <* lexeme (char ';')

    pAlt :: Parser (Pattern, Expr)
    pAlt = do
        pat <- pPattern
        void $ lexeme (string "->")
        expr <- pExpr
        return (pat, expr)
    
    pPattern :: Parser Pattern
    pPattern = choice
        [ squareBrackets $ do -- PElli
            begin <- pListElement
            void $ lexeme $ optional $ char ','
            void $ lexeme $ string "..."
            void $ lexeme $ optional $ char ','
            end <- pListElement
            var <- listElementVar begin end
            (_, idxr) <- listElementIdx begin end
            return $ PEllipsis var idxr
        , do -- PVal
            val <- pVal
            return $ PVal val
        , do -- PVar
            ident <- pIdent
            return $ PVar ident
        ]
    pVal :: Parser Val
    pVal = (lexeme . try) (pTerm >>= check)
      where
        check (Value v) = return v
        check t = fail $ show t ++ " is not a value"

    listElementVar :: Expr -> Expr -> Parser String
    listElementVar (ListElement nl _) (ListElement nr _)
        | nl == nr = return nl
        | otherwise = fail "ListElements had different names in pattern"
    listElementVar _  _              = fail "Tried to get list element vars from non-list element"

    listElementIdx :: Expr -> Expr -> Parser (Expr, Expr)
    listElementIdx (ListElement nl idxl) (ListElement nr idxr) = return (idxl, idxr)
    listElementIdx _ _ = fail $ "tried to get list element vars from non-listelements"

pBool :: Parser Expr
pBool = lexeme ((Value (Boolean True)  <$ string "True")
                <|> (Value (Boolean False) <$ string "False"))
        <?> "boolean"

pInt :: Parser Expr
pInt = Value . Con <$> integer
    where
    integer :: Parser Int
    integer = lexeme L.decimal

pVar :: Parser Expr
pVar = Var <$> pIdent

pIdent' :: Parser Text
pIdent' = T.pack <$> pIdent

pIdent :: Parser String
pIdent = (lexeme . try) (ident >>= check)
  where
    -- https://github.com/mrkkrp/megaparsec-site/blob/master/tutorials/parsing-simple-imperative-language.md
    ident = ((:) <$> letterChar <*> many alphaNumChar) <?> "variable"
    check x = 
        if x `elem` resw
        then fail $ show x ++ " is a keyword"
        else return x


pListElement :: Parser Expr
pListElement = do
    ident <- pIdent
    term <- curlyBraces pExpr
    return $ ListElement ident term



------------------------------------------------------------------------
--
-- Operators
--
------------------------------------------------------------------------

operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [ InfixL ifx
      , app
      ] -- 9
    , [] -- 8
    , [ binary "*" Mul
      , binary "`div`" Div
      , binary "`mod`" Mod
      ] -- 7
    , [ binary "-" Sub
      , binary "+" Add
      ] -- 6
    , [ binary ":" Cons
      ] -- 5
    , [ binary "==" Eq
      , binary "/=" Neq
      , binary "<" Lt
      , binary "<=" Leq
      , binary ">" Gt
      , binary ">=" Geq
      ] -- 4
    , [ binary "&&" And
      ] -- 3
    , [ binary "||" Or
      ] -- 2
    , [] -- 1
    , [] -- 0
    ]
  where
    binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
    binary name f = InfixL (f <$ symbol name)

    ifx :: Parser (Expr -> Expr -> Expr)
    ifx = do
        f <- lexeme $ between (char '`') (char '`') (T.unpack <$> pIdent')
        return (\l r -> Var f `App` l `App` r)

app :: Operator Parser Expr
app = InfixL (App <$ (symbol "" <?> "application"))


------------------------------------------------------------------------
--
-- Lexer stuff
--
------------------------------------------------------------------------
sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

