module EllipLang where

import EllipLang.Eval
import EllipLang.Examples
import EllipLang.Parser
import EllipLang.Pretty
import EllipLang.SmartCons
import EllipLang.Syntax
import EllipLang.Translator
import EllipLang.MHSPrelude (prelude)

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad (void, when)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import Control.Monad.Combinators.Expr
import Text.Megaparsec.Debug (dbg)
import Debug.Trace
import Data.Data (gmapM)
import Data.Generics (mkM, toConstr)
import Data.List (maximumBy)
import Data.List.Extra (uncons, unsnoc)
import Data.Maybe (fromMaybe)
import Data.Function ((&))
import qualified Data.Map as Map
import System.IO

parseTranslate :: Text -> IO ()
parseTranslate s = parseMaybe pExpr s & fromMaybe (error "bad parse") & translate & pp & putStrLn

data ReplArg = ShowTranslate | FileVerbose | ShowParse
    deriving (Eq, Show)

mkReplArg :: String -> Maybe ReplArg
mkReplArg "translate"   = Just ShowTranslate
mkReplArg "parse"       = Just ShowParse
mkReplArg _             = Nothing

repl :: [ReplArg] -> IO ()
repl args = repl' args prelude
            
repl' :: [ReplArg] -> Env -> IO ()
repl' args env = do
    ln <- prompt "> "
    case ln of
        ""                  -> repl' args env
        ':':'t':' ':rest    -> case runParser pDecl "" (T.pack ln) of
            Right expr  -> do
                let translated = translate expr
                putStrLn $ pp translated
                repl' args env
            Left  err   -> do
                putStrLn $ errorBundlePretty err
                repl' args env

        _                   -> go ln
  where
    go ln = case runParser pDecl "" (T.pack ln) of 
            Right (Decl s expr) -> do
                when (ShowParse `elem` args) (putStrLn . pp $ expr)
                let translated = translate expr
                when (ShowTranslate `elem` args) (putStrLn . pp $ translated)

                let res = eval env translated
                putStr $ s ++ " = "
                putStrLn $ ppVal res
                repl' args (Map.fromList [(NamedVar s, BVal res)] `Map.union` env)
            Right expr  -> do
                let translated = translate expr
                when (ShowTranslate `elem` args) (putStrLn . pp $ translated)
                let res = eval env translated
                putStrLn $ ppVal res
                repl' args env
            Left  err   -> do
                putStrLn $ errorBundlePretty err
                repl' args env

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

run :: Bool -> Env -> [String] -> IO Env
run verbose env smts = go env smts
  where
    go :: Env -> [String] -> IO Env
    go env (s:ss) = case runParser pDecl "" (T.pack s) of
        Left err   -> do
            putStrLn $ errorBundlePretty err
            go env ss

        Right (Decl s expr) -> do
            let translated = translate expr
            let res = eval env translated
            when verbose (putStrLn $ "(" ++ show s ++ ", " ++ pp translated ++ ")" )
            let newEnv = Map.fromList [(NamedVar s, BVal res)]
            went <- go (Map.union newEnv env) ss
            return $ Map.union newEnv went

        Right expr  -> do
            let translated = translate expr
            let res = eval env translated
            when verbose (putStrLn $ ppVal res)
            go env ss

    go env [] = return env