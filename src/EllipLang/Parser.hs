{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module EllipLang.Parser where

import EllipLang.Syntax
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Debug.Trace
import qualified Data.Map as Map
import qualified Control.Monad.Combinators.Expr as Pexpr
import Data.String.Utils (replace)

type Parser = Parsec Void String

parseString :: String -> Expr
parseString s =  case parse expr "input" <$> [s] of
    [Right ex]    -> ex
    [Left other]  -> error (errorBundlePretty other)
    other         -> error (show other)


testParser :: IO ()
testParser = foldr ((<>) . 
    (\(str,expected) -> let parsed = (parse expr "input" <$> [str])
        in putStrLn $ str ++ ": " ++ (
            case (parsed,expected) of
                ([Right tParsed], Just tExpected) -> 
                    if tParsed == tExpected then "OK"
                    else "ERROR: expected: " ++ show tExpected ++ " but got: " ++ show tParsed
                ([Left errParsed], Nothing) -> 
                    "OK (" ++ replace "\n" " " (errorBundlePretty errParsed) ++ ")"
                ([Left errParsed], Just tExpected) -> 
                    "ERROR: expected: " ++ show tExpected ++ " but got: " ++ replace "\n" " " (errorBundlePretty errParsed)
                ([Right tParsed], Nothing) -> "ERROR: expected error but got: " ++ show tParsed
        )
    )) (return ()) testStrings


testStrings :: [(String, Maybe Expr)]
testStrings = [
    ("\\x.x 5", Just $ App (Abstr "x" (Var "x")) (Value (Con 5))),
    ("(\\x.x) 5", Just $ App (Abstr "x" (Var "x")) (Value (Con 5))),
    ("(\\x.(x+1)) 5", Just $ 
        Abstr "x" (Var "x" `Add` Value (Con 1)) `App` Value (Con 5)),
    ("\\x.(x+1) 5", Just $ 
        Abstr "x" (Var "x" `Add` Value (Con 1)) `App` Value (Con 5)),
    ("() 5", Nothing)
    ]


expr :: Parser Expr
expr = choice
    [
        try app
    ,   abstr
    ,   try infx
    ,   atom
    ]

infx :: Parser Expr
infx = do
    lhs <- atom
    _ <- many space1
    sym <- choice $ map string (Map.keys infxOps)
    _ <- many space1
    rhs <- atom
    return $ (infxOps Map.! sym) lhs rhs
    where
        arithOps    = [("+", Add), ("-", Sub), ("*", Mul), ("/", Div)]
        boolOps     = [("&&", And), ("||", Or)]
        relOps      = [("==", Eq), ("<", Lt), (">", Gt), ("<=", Leq), (">=", Geq), ("!=", Neq)]
        infxOps = Map.fromList (arithOps ++ boolOps ++ relOps)



atom :: Parser Expr
atom = choice
    [
        paren
    ,   int
    ,   bool
    ,   variable
    ]

variable :: Parser Expr
variable = lexeme $ Var <$> variable'

variable' :: Parser String
variable' = lexeme $ do
    first <- letterChar <|> char '_'
    rest <- many (alphaNumChar <|> char '_')
    return $ first:rest

int :: Parser Expr
int = Value . Con <$> lexeme (read <$> some numberChar)

bool :: Parser Expr
bool = Value . Boolean <$> lexeme (False <$ string "false" <|> True <$ string "true")

abstr :: Parser Expr
abstr = Abstr
    <$> between (char '\\') (char '.') variable'
    <*> expr

app :: Parser Expr
app = do
    lhs <- try abstr 
    _ <- many space1
    App lhs <$> expr

paren :: Parser Expr
paren = lexeme $ char '(' *> expr <* char ')'

skipSpace :: Parser ()
skipSpace = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace