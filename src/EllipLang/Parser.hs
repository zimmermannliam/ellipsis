{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module EllipLang.Parser where

import EllipLang.Syntax
import EllipLang.SmartCons (unConsSafe, cons)

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
import Data.Data (gmapM)
import Data.Generics (mkM, toConstr)
import Data.List (maximumBy)
import Data.List.Extra (uncons, unsnoc)
import qualified Data.Map as Map

type Parser = Parsec Void Text

resw =
    [ "if", "then", "else"
    , "\\", "->"
    , "case", "of"
    , "..."
    , "let", "in"
    ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable >>= secondPass

------------------------------------------------------------------------
--
-- Terms
--
------------------------------------------------------------------------

pTerm :: Parser Expr
pTerm = choice
    [ pPair
    , parens pExpr
    , pAbstr
    , pIfThenElse
    , pCase
    , pBool
    , pInt
    , pListSugar
    , pPreElli
    , try pListElement
    , pVar
    ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

curlyBraces :: Parser a -> Parser a
curlyBraces = between (symbol "{") (symbol "}")

squareBrackets :: Parser a -> Parser a
squareBrackets = between (symbol "[") (symbol "]")

pPair :: Parser Expr
pPair = between (symbol "(") (symbol ")") $ do
    first <- pExpr
    void $ lexeme (string ",")
    second <- pExpr
    return $ Pair first second

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
            begin <- try pListElement <|> pVar
            void $ lexeme $ optional $ char ','
            void $ lexeme $ string "..."
            void $ lexeme $ optional $ char ','
            end <- try pListElement <|> pVar
            (var, idxl, idxr) <- extractListElement begin end
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

    extractListElement :: Expr -> Expr -> Parser (String, Expr, Expr)
    extractListElement (ListElement nl idxl) (ListElement nr idxr)
        | nl == nr  = return (nl, idxl, idxr)
        | otherwise = fail $ "different ellipsis pattern variables" 
            ++ show nl ++ "..." ++ show nr ++ "are different"
    extractListElement (Var vl) (Var vr) = 
        let (name, idxl, idxr) = spanSharedPrefix vl vr
        in  if name == ""
            then fail "different ellipsis pattern variables"
            else case (parseMaybe (pInt <|> pVar) (T.pack idxl), parseMaybe (pInt <|> pVar) (T.pack idxr)) of
                (Just idxl', Just idxr') -> return (name, idxl', idxr')
                _                        -> fail $ "bad indices: " ++ show idxl ++ " ... " ++ show idxr
    extractListElement (ListElement nl idxl) (Var vr) =
        let (name, _, idxr) = spanSharedPrefix nl vr
        in  if name /= nl
            then fail "different ellipsis pattern variables"
            else case parseMaybe (pInt <|> pVar) (T.pack idxr) of
                Just idxr' -> return (name, idxl, idxr')
                Nothing    -> fail $ "bad index: " ++ show idxr
    extractListElement (Var vl) (ListElement nr idxr) =
        let (name, idxl, _) = spanSharedPrefix vl nr
        in  if name /= nr
            then fail "different ellipsis pattern variables"
            else case parseMaybe (pInt <|> pVar) (T.pack idxl) of
                Just idxl' -> return (name, idxl', idxr)
                Nothing    -> fail $ "bad index: " ++ show idxl

pBool :: Parser Expr
pBool = lexeme ((Value (Boolean True)  <$ string "True")
                <|> (Value (Boolean False) <$ string "False"))
        <?> "boolean"

pInt :: Parser Expr
pInt = Value . Con <$> integer
    where
    integer :: Parser Int
    integer = lexeme L.decimal

pPreElli :: Parser Expr
pPreElli = PreElli <$ string "..."

pListSugar :: Parser Expr
pListSugar = squareBrackets go
  where
    go :: Parser Expr
    go = do
        element <- optional $ lexeme pExpr
        case element of
            Nothing         -> return (Value Empty)
            Just element'   -> do
                comma <- optional $ lexeme $ string ","
                case comma of
                    Nothing -> return (Cons element' (Value Empty))
                    Just _  -> do
                        next <- go
                        return $ Cons element' next
        


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

opmap = Map.fromList
    [ ("*",         Mul)
    , ("`div`",     Div)
    , ("`mod`",     Mod)
    , ("-",         Sub)
    , ("+",         Add)
    , (":",         Cons)
    , ("==",        Eq)
    , ("/=",        Neq)
    , ("<",         Lt)
    , ("<=",        Leq)
    , (">",         Gt)
    , (">=",        Geq)
    , ("&&",        And)
    , ("||",        Or)
    ]

opToBinExpr :: String -> (Expr -> Expr -> Expr)
opToBinExpr s = opmap Map.! s

pBinOp :: Parser (Expr -> Expr -> Expr)
pBinOp = do
    f <- choice (symbol . T.pack <$> Map.keys opmap)
    return $ opToBinExpr (T.unpack f)

operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [ InfixL ifx
      , app
      ] -- 9
    , [] -- 8
    , [ binary "*" (opToBinExpr "*")
      , binary "`div`" (opToBinExpr "`div`")
      , binary "`mod`" (opToBinExpr "`mod`")
      ] -- 7
    , [ binary "-" (opToBinExpr "-")
      , binary "+" (opToBinExpr "+")
      ] -- 6
    , [ binaryR ":" (opToBinExpr ":")
      ] -- 5
    , [ binary "==" (opToBinExpr "==")
      , binary "/=" (opToBinExpr "/=")
      , binary "<" (opToBinExpr "<")
      , binary "<=" (opToBinExpr "<=")
      , binary ">" (opToBinExpr ">")
      , binary ">=" (opToBinExpr ">=")
      ] -- 4
    , [ binaryR "&&" (opToBinExpr "&&")
      ] -- 3
    , [ binaryR "||" (opToBinExpr "||")
      ] -- 2
    , [ binary "..." Ellipsis
      , InfixL ifxElli ] -- 1
    , [] -- 0
    ]
  where
    binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
    binary name f = InfixL (f <$ symbol name)

    binaryR :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
    binaryR name f = InfixR (f <$ symbol name)

    ifx :: Parser (Expr -> Expr -> Expr)
    ifx = do
        f <- ifxExpr
        return $ App . App f
    
    ifxExpr :: Parser Expr
    ifxExpr = lexeme $ between (char '`') (char '`') (Var <$> pIdent)
    
    ifxElli :: Parser (Expr -> Expr -> Expr)
    ifxElli = do
        f1 <- pBinOp
        void $ symbol "..."
        f2 <- pBinOp
        let f = Abstr "l" $ Abstr "r" (Var "l" `f1` Var "r")
        if toConstr (f1 undefined undefined) /= toConstr (f2 undefined undefined)
        then fail "different operators on elli-fold"
        else return $ \l r -> ElliFoldr l r f


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

------------------------------------------------------------------------
--
-- Second pass
--
------------------------------------------------------------------------

secondPass :: Expr -> Parser Expr
secondPass t = unsugarElements [] t >>= elliTokenToExpr

-- | Note: bad case:
-- [y1 ... yn] -> yourHouse
-- becomes
-- [y1 ... yn] -> y{ourHouse}
-- I should just get variables bound by our expressions
unsugarElements :: [String] -> Expr -> Parser Expr
unsugarElements cxt (Case target alts) = do
    rhs <- mapM (\case { (PEllipsis n _, t) -> unsugarElements (n:cxt) t; (_, t) -> unsugarElements cxt t; }) alts
    return $ Case target (zip (fmap fst alts) rhs)
unsugarElements cxt (Var v) = 
    let spans = fmap (spanSharedPrefix v) ("":cxt)
    in case maximumBy (\(nl, _, _ ) (nr, _, _) -> compare (length nl) (length nr)) spans of
        ("", _, _)      -> return (Var v)   -- No match at all (only matters in cases where "" `elem` cxt)
        (vn, idxv, "")  -> case parseMaybe (pInt <|> pVar) (T.pack idxv) of
            Just idxv'      -> return $ ListElement vn idxv'
            Nothing         -> return (Var v)
        (_, _, _)       -> return (Var v)   -- v does not contain all of the cxt variable
unsugarElements cxt t = gmapM (mkM (unsugarElements cxt)) t

elliTokenToExpr :: Expr -> Parser Expr
elliTokenToExpr xs@(Cons _ _) = case unConsSafe xs of
    Just xs' ->  return $ foldr1 (App . App (Var "(++)")) (cons <$> preElliToElliList xs')
    Nothing -> fail "second pass: list could not be uncons'd properly"
  where
    preElliToElliList :: [Expr] -> [[Expr]]
    preElliToElliList (l:PreElli:r:xs) = [l `Ellipsis` r]:preElliToElliList xs
    preElliToElliList (x:l:PreElli:xs) = [x]:preElliToElliList (l:PreElli:xs)
    preElliToElliList (x:xs)           =
        let xs' = preElliToElliList xs
        in case uncons xs' of
            Just (mySubList, rest) -> (x:mySubList):rest
            Nothing -> [x]:xs'
    preElliToElliList []               = []
elliTokenToExpr t = gmapM (mkM elliTokenToExpr) t


------------------------------------------------------------------------
--
-- UTILS
--
------------------------------------------------------------------------

-- | Take two lists xs, ys; return a tuple containing:
-- (shared prefix of xs and ys, rest of xs, rest of ys)
-- spanSharedPrefix "hello world" "hello guys" == ("hello ", "world", "guys")
-- spanSharedPrefix "hello" "hello world" == ("hello", "", " world")
-- spanSharedPrefix "hello world" "greetings world" = ("", "hello world", "greetings world")
spanSharedPrefix :: Eq a => [a] -> [a] -> ([a], [a], [a])
spanSharedPrefix xs ys = (p, drop n xs, drop n ys)
  where
    p = fmap fst $ takeWhile (uncurry (==)) (zip xs ys)
    n = length p

zipLongest :: [a] -> [b] -> [(Maybe a, Maybe b)]
zipLongest (x:xs) (y:ys) = (Just x, Just y):zipLongest xs ys
zipLongest xs [] = zip (fmap Just xs) (fmap (const Nothing) xs)
zipLongest [] ys = zip (fmap (const Nothing) ys) (fmap Just ys)

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p [] = []
splitOn p xs = case break p xs of
    ([], rest)      -> splitOn p (drop 1 rest)
    (fails, rest)   -> fails:splitOn p (drop 1 rest)