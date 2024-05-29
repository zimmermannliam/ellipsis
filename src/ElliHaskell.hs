module ElliHaskell where

import ElliHaskell.Eval
import ElliHaskell.Parser
import ElliHaskell.Syntax
import ElliHaskell.Pretty
import ElliHaskell.TypeChecker

import Text.Megaparsec (errorBundlePretty, parse, eof)
import Data.Map (empty)
import System.Console.Readline (readline)
import Data.Maybe (fromMaybe)

prelude = empty
preludeCxt = empty

data EHSErr = EP ErrParser | EE ErrEval

repl :: IO ()
repl = go preludeCxt prelude
  where
    go :: Cxt -> Env -> IO ()
    go cxt env = do
        s <- readline "...> "
        case parse (pStmt <* eof) "" (fromMaybe "" s) of
            Left err -> do
                putStrLn $ errorBundlePretty err
                go cxt env
            Right (StmtEval e) -> do
                runExpr cxt env e
                go cxt env
            Right (StmtType e) -> do
                runType cxt e
                go cxt env
            Right StmtQuit     -> return ()

runExpr :: Cxt -> Env -> Expr -> IO ()
runExpr cxt env e = case typeInfer cxt e of
    Left err -> putStrLn $ ppTypeErr err
    Right _ -> case eval env e of
        Left err -> putStrLn $ ppEvalErr err
        Right val -> putStrLn $ pp val

runType :: Cxt -> Expr -> IO ()
runType cxt e = case typeInfer cxt e of
    Left err -> putStrLn $ ppTypeErr err
    Right t  -> putStrLn $ ppType t