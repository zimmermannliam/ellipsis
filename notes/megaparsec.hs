import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

newtype Identifier = Identifier {
    getId :: String
} deriving (Show)

data SExp
    = SSExp     SExp [SExp]
    | SInteger  Integer
    | SString   String
    | SBool     Bool
    | SId       Identifier
    deriving (Show)

type Parser = Parsec
    Void
    String

bool :: Parser Bool
bool = label "boolean" $ lexeme $ False <$ string "false" <|> True <$ string "true"

integer :: Parser Integer
integer = label "integer" $ lexeme $ read <$> some numberChar

str :: Parser String
str = label "string" $ lexeme $ between (char '"') (char '"') (takeWhileP Nothing (/= '"'))

identifier :: Parser Identifier
identifier = label "identifier" $ lexeme $ do
    first <- letterChar <|> char '_'
    rest <- many $ alphaNumChar <|> char '_'
    pure $ Identifier $ first:rest


sexp :: Parser (SExp, [SExp])
sexp = label "S-Expression" $ lexeme $ 
    between (lexeme (char '(')) (char ')') ((,) <$> atom <*> many atom)

skipSpace :: Parser ()
skipSpace = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

atom :: Parser SExp
atom = choice
    [
        SBool <$> bool
    ,   SInteger <$> integer
    ,   SString <$> str
    ,   SId <$> identifier
    ,   uncurry SSExp <$> sexp
    ]