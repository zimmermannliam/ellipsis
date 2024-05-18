import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Control.Monad (void)
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (dbg)

type Parser = Parsec Void Text

-- scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]

data Uri = Uri
    { uriScheme :: Scheme
    , uriAuthority :: Maybe Authority
    , uriPath :: Text
    , uriQuery :: Maybe Text
    , uriFrag :: Maybe Text
    } deriving (Eq, Show)

data Scheme = SchemeData | SchemeFile | SchemeFtp | SchemeHttp | SchemeHttps | SchemeIrc | SchemeMailto
    deriving (Eq, Show)

data Authority = Authority
    { authUser :: Maybe (Text, Text)
    , authHost :: Text
    , authPort :: Maybe Int
    } deriving (Eq, Show)

pScheme :: Parser Scheme
pScheme = choice
    [ SchemeData    <$ string "data"
    , SchemeFile    <$ string "file"
    , SchemeFtp     <$ string "ftp"
    , SchemeHttps   <$ string "https"
    , SchemeHttp    <$ string "http"
    , SchemeIrc     <$ string "irc"
    , SchemeMailto  <$ string "mailto"
    ]

pUser :: Parser (Text, Text)
pUser = do
    user    <- T.pack <$> some alphaNumChar <?> "username"
    void (char ':')
    password<- T.pack <$> some alphaNumChar <?> "password"
    void (char '@')
    return (user, password)

pAuth :: Parser Authority
pAuth = do
    void (string "//")
    authUser        <- optional . try $ pUser
    authHost        <- T.pack <$> some (alphaNumChar <|> char '.') <?> "hostname"
    authPort        <- optional (char ':' *> label "port number" L.decimal)
    return $ Authority {..}


pPath :: Parser Text
pPath = do
    T.pack <$> some (alphaNumChar <|> char '/')

pUri :: Parser Uri
pUri = do
    uriScheme <- pScheme <?> "valid scheme"
    void (char ':')
    uriAuthority <- optional pAuth
    void (optional $ char '/')
    uriPath <- pPath
    uriQuery <- optional $ T.pack <$> (char '?' *> label "valid query" (some (alphaNumChar <|> char '=')))
    uriFrag <- optional $ T.pack <$> (char '#' *> label "fragment" (some alphaNumChar))
    return $ Uri {..}