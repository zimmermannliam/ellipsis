{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main where

import EllipLang
import EllipLang.Parser (processFile)
import EllipLang.MHSPrelude (prelude)

import System.Environment (getArgs)
import System.IO
import Data.Function ((&))
import Data.Maybe (catMaybes)

main :: IO ()
main = do
    args <- getArgs
    case args of
        []  -> repl []
        f:rest -> replFile rest f

replFile :: [String] -> String -> IO ()
replFile args f = do
    let args' = catMaybes $ mkReplArg <$> args
    print args'
    handle <- openFile f ReadMode
    contents <- hGetContents handle
    newEnv <- processFile contents & run True (ShowAbstract `elem` args') prelude
    repl' args' newEnv