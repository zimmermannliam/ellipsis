{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main where

import EllipLang
import EllipLang.Parser (processFile)
import EllipLang.MHSPrelude (prelude)

import System.Environment (getArgs)
import System.IO
import Data.Function ((&))

passedArgs = [ShowParse, ShowTranslate]

main :: IO ()
main = do
    args <- getArgs
    case args of
        []  -> repl passedArgs
        [f] -> replFile f
        _   -> error "weird args"

replFile :: String -> IO ()
replFile f = do
    handle <- openFile f ReadMode
    contents <- hGetContents handle
    newEnv <- processFile contents & run True prelude
    repl' passedArgs newEnv