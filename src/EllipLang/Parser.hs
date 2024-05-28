{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module EllipLang.Parser where

import EllipLang.Syntax
import EllipLang.SmartCons (unConsSafe, cons)
import EllipLang.MHSPrelude


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
import Data.Generics (mkM, everywhere', mkT)
import Data.List (maximumBy, groupBy, isPrefixOf)
import Data.List.Extra (uncons, unsnoc)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Function ((&))

type Parser = Parsec Void Text

processFile :: String -> [String]
processFile s = s
    & lines
    & groupBy (\a b -> a /= "" && b /= "")  -- group empty lines
    & fmap unwords
    & filter (\x -> x /= "" && not (isPrefixOf "--" x) && not (isPrefixOf "{-" x))


resw =
    [ "if", "then", "else"
    , "\\", "->"
    , "case", "of"
    , "...", ".", ".."
    , "let", "in"
    , "from", "to"
    ]

pDecl :: Parser Expr
pDecl = choice
    [ try $ do
        (var, pats) <- getDecl
        innerExpr <- makeExprParser pTerm operatorTable
        let expr = mkDecl var pats innerExpr
        secondPass expr
    , makeExprParser pTerm operatorTable >>= secondPass
    ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable >>= secondPass

getDecl :: Parser (String, [Pattern])
getDecl = do
    ident   <- lexeme pIdent
    pats    <- lexeme $ many pPattern
    void $ lexeme $ string "="
    return (ident, pats)

mkDecl :: String -> [Pattern] -> Expr -> Expr
mkDecl ident pats expr = 
    Decl ident 
    $ foldr mkAbstr id patNames
    $ foldr mkCase id patNames
    $ expr
  where
    patName :: Int -> Pattern -> (Pattern, String)
    patName _ (PVar n)  = (PVar n, n)
    patName x p         = (p, "__" ++ show x)

    mkAbstr :: (Pattern, String) -> (Expr -> Expr) -> (Expr -> Expr)
    mkAbstr (PVar n, _) abstr       = abstr . Abstr n
    mkAbstr (_, s) abstr            = abstr . Abstr s

    mkCase :: (Pattern, String) -> (Expr -> Expr) -> (Expr -> Expr)
    mkCase (PVar n, _) c = c
    mkCase (p, targ) c = \x -> c $ Case (Var targ) [(p, x)]

    patNames = reverse $ zipWith patName [1..] pats


------------------------------------------------------------------------
--
-- Terms
--
------------------------------------------------------------------------

pTerm :: Parser Expr
pTerm = choice
    [ try pPair
    , parens pExpr
    , pAbstr
    , pIfThenElse
    , pCase
    , pLet
    , pFrom
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
pPattern = do
    choice 
        [ try $ do -- PCons
            l <- pIdent
            void $ lexeme $ string ":"
            r <- pIdent
            return $ PCons l r
        , do -- PVal
            val <- pVal
            return $ PVal val
        , do -- PVar
            ident <- pIdent
            return $ PVar ident
        , squareBrackets $ do -- PElli
            begin <- try pListElement <|> pVar
            void $ lexeme $ optional $ char ','
            void $ lexeme $ string "..."
            void $ lexeme $ optional $ char ','
            end <- try pListElement <|> pVar
            (var, idxl, idxr) <- extractListElement begin end
            return $ PEllipsis var idxr

        ]
  where
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


pLet :: Parser Expr
pLet = do
    void $ lexeme $ symbol "let"
    v <- pIdent
    void $ lexeme $ symbol "="
    e1 <- pExpr
    void $ lexeme $ symbol "in"
    e2 <- pExpr
    return $ LetRec v e1 e2

pFrom :: Parser Expr
pFrom = do
    void $ lexeme $ symbol "from"
    el <- pExpr
    void $ lexeme $ symbol "to"
    er <- pExpr
    return $ Ellipsis el er

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
pPreElli = PreElli <$ symbol "..."

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

data Fixity = FixLeft | FixRight | FixMid
    deriving (Show, Eq)

opToFixity s = fromMaybe FixLeft (fixmap Map.!? s)
  where
    fixmap = Map.fromList
        [ ("*",         FixLeft)
        , ("`div`",     FixLeft)
        , ("`mod`",     FixLeft)
        , ("-",         FixLeft)
        , ("+",         FixLeft)
        , (":",         FixRight)
        , ("==",        FixLeft)
        , ("/=",        FixLeft)
        , ("<",         FixLeft)
        , ("<=",        FixLeft)
        , (">",         FixLeft)
        , (">=",        FixLeft)
        , ("&&",        FixRight)
        , ("||",        FixRight)
        , ("$",         FixRight)
        ]

opmap = Map.fromList
    [ ("*",         Op Mul)
    , ("`div`",     Op Div)
    , ("`mod`",     Op Mod)
    , ("-",         Op Sub)
    , ("+",         Op Add)
    , (":",         Cons)
    , ("==",        Op Eq)
    , ("/=",        Op Neq)
    , ("<",         Op Lt)
    , ("<=",        Op Leq)
    , (">",         Op Gt)
    , (">=",        Op Geq)
    , ("&&",        Op And)
    , ("||",        Op Or)
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
    , [ binaryL "*" (opToBinExpr "*")
      , binaryL "`div`" (opToBinExpr "`div`")
      , binaryL "`mod`" (opToBinExpr "`mod`")
      ] -- 7
    , [ binaryL "-" (opToBinExpr "-")
      , InfixL $ Op Add <$ try (symbol "+" >> notFollowedBy (symbol "+"))
      ] -- 6
    , [ binaryR ":" (opToBinExpr ":")
      , binaryR "++" (Op $ VarOp $ unVar catFun)
      ] -- 5
    , [ binaryN "==" (opToBinExpr "==")
      , binaryN "/=" (opToBinExpr "/=")
      , binaryN "<" (opToBinExpr "<")
      , binaryN "<=" (opToBinExpr "<=")
      , binaryN ">" (opToBinExpr ">")
      , binaryN ">=" (opToBinExpr ">=")
      ] -- 4
    , [ binaryR "&&" (opToBinExpr "&&")
      ] -- 3
    , [ binaryR "||" (opToBinExpr "||")
      ] -- 2
    , [ ] -- 1
    , [ binaryR "$" (Op $ VarOp $ unVar appFun)] -- 0
    ]
  where
    binaryL :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
    binaryL name f = InfixL (f <$ symbol name)

    binaryR :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
    binaryR name f = InfixR (f <$ symbol name)

    binaryN :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
    binaryN name f = InfixN (f <$ symbol name)

    ifx :: Parser (Expr -> Expr -> Expr)
    ifx = try $ do
        f <- ifxExpr
        if f `elem` takenOps
            then fail $ "Tried to parse a taken operator: " ++ show takenOps
            else return $ Op (VarOp f)
      where
        takenOps = ["mod", "div"]
    
    ifxExpr :: Parser String
    ifxExpr = lexeme $ between (char '`') (char '`') pIdent

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
secondPass t = elliTokenToExpr <$> unsugarElements [] t

-- | Note: bad case:
-- [y1 ... yn] -> yourHouse
-- becomes
-- [y1 ... yn] -> y{ourHouse}
-- I should just get variables bound by our expressions
unsugarElements :: [String] -> Expr -> Parser Expr
unsugarElements cxt (Case target alts) = do
    rhs <- mapM (\case { (PEllipsis n _, t) -> unsugarElements (n:cxt) t; (_, t) -> unsugarElements cxt t; }) alts
    target' <- unsugarElements cxt target
    return $ Case target' (zip (fmap fst alts) rhs)
unsugarElements cxt (Var v) = 
    let spans = fmap (spanSharedPrefix v) ("":cxt)
    in case maximumBy (\(nl, _, _ ) (nr, _, _) -> compare (length nl) (length nr)) spans of
        ("", _, _)      -> return (Var v)   -- No match at all (only matters in cases where "" `elem` cxt)
        (vn, idxv, "")  -> case parseMaybe (pInt <|> pVar) (T.pack idxv) of
            Just idxv'      -> return $ ListElement vn idxv'
            Nothing         -> return (Var v)
        (_, _, _)       -> return (Var v)   -- v does not contain all of the cxt variable
unsugarElements cxt t = gmapM (mkM (unsugarElements cxt)) t

elliTokenToExpr :: Expr -> Expr
elliTokenToExpr = everywhere' (mkT go)
  where
    go :: Expr -> Expr
    go consList@(Cons _ _) = case unConsSafe consList of
        Just xs -> foldr1 (App . App (catFun)) (cons <$> preElliToElliList xs)
        Nothing -> consList

    go e@(Op op1 l (Op op2 PreElli r))
        | (op1 == op2) = case r of
            (Op op3 r' z)   -> if (op3 == op1) 
                               then ElliFoldr0 l r' z op1
                               else ElliFoldr l r op1
            _               -> ElliFoldr l r op1
        | otherwise = e

    go e@(Op op1 (Op op2 l PreElli) r)
        | (op1 == op2) = case l of
            (Op op3 z l')   -> if (op3 == op1)
                               then ElliFoldl0 l' r z op1
                               else ElliFoldl l r op1
            _               -> ElliFoldl l r op1
        | otherwise = e
    
    go e@(PreElli `App` t) = t
    go e@(t `App` PreElli) = t

    go e = e

    preElliToElliList :: [Expr] -> [[Expr]]
    preElliToElliList (l:PreElli:r:xs) = [l `Ellipsis` r]:preElliToElliList xs
    preElliToElliList (x:l:PreElli:xs) = [x]:preElliToElliList (l:PreElli:xs)
    preElliToElliList (x:xs)           =
        let xs' = preElliToElliList xs
        in case uncons xs' of
            Just (mySubList, rest) -> (x:mySubList):rest
            Nothing -> [x]:xs'
    preElliToElliList []               = []

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
