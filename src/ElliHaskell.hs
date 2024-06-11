module ElliHaskell where

import ElliHaskell.Eval
import ElliHaskell.Parser
import ElliHaskell.Syntax
import ElliHaskell.Pretty
import ElliHaskell.TypeChecker
import ElliHaskell.Types

import Text.Megaparsec (errorBundlePretty, parse, eof)
import qualified Data.Map as Map
import System.Console.Readline (readline)
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans (lift)

-- For repl:
-- import Text.Megaparsec (parseTest)

prelude :: Env
prelude = Map.fromList [("id", Abstr blank (VarPat "x") (Var blank "x"))]
preludeCxt :: Cxt
preludeCxt = Map.fromList [("id", TypeAbstr (TypeSome 1) (TypeSome 1))]

data EHSErr = EP ErrParser | EE ErrEval

repl :: IO ()
repl = runReaderT go (preludeCxt, prelude)
  where
    go :: ReaderT (Cxt, Env) IO ()
    go = do
        s <- lift $ fromMaybe "" <$> readline "...> "
        case parse (pStmt <* eof) "" s of
            Left err -> do
                lift . putStrLn $ errorBundlePretty err
                go
            Right (StmtType e) -> do
                withReaderT fst $ runType e
                go
            Right (StmtEval e) -> do
                runEval e
                go
            Right StmtQuit     -> return ()
            Right (StmtUnsafe e) -> do
                withReaderT snd $ runUnsafe e
                go
            _                  -> undefined


runUnsafe :: Expr -> ReaderT Env IO ()
runUnsafe e = do
    env <- ask
    case runReader (runExceptT $ eval e) env of
        Left err    -> lift $ putStrLn $ ppEvalErr err
        Right val   -> lift $ putStrLn $ pp val

runType :: Expr -> ReaderT Cxt IO ()
runType e = do
    cxt <- ask
    case typeInfer cxt e of
        Left err -> lift $ putStrLn $ ppTypeErr err
        Right t -> lift $ putStrLn $ ppType t

runEval :: Expr -> ReaderT (Cxt, Env) IO ()
runEval e = do
    (cxt, env) <- ask
    case typeInfer cxt e of
        Left err -> lift $ putStrLn $ ppTypeErr err
        Right _ -> case runReader (runExceptT $ eval e) env of
            Left err    -> lift $ putStrLn $ ppEvalErr err
            Right val   -> lift $ putStrLn $ pp val

{-
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
            Right (StmtDecl dec@(Decl _ v _ _)) -> case typeDecl cxt dec of
                Left err     -> do
                    putStrLn $ ppDeclTypeErr err
                    go cxt env
                Right cxt'   -> case evalDecl env dec of
                    Left err    -> do
                        putStrLn $ ppDeclEvalErr err
                        go cxt env
                    Right env'  -> go cxt' env'


runType :: Cxt -> Expr -> IO ()
runType cxt e = case typeInfer cxt e of
    Left err -> putStrLn $ ppTypeErr err
    Right t  -> putStrLn $ ppType t

runExpr :: Cxt -> Env -> Expr -> IO ()
runExpr cxt env e = case typeInfer cxt e of
    Left err -> putStrLn $ ppTypeErr err
    Right _ -> case eval env e of
        Left err -> putStrLn $ ppEvalErr err
        Right val -> putStrLn $ pp val
                    -}