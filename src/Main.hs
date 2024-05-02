{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main where

import EllipLang.Examples
import EllipLang.Pretty ( pp, ppVal )
import EllipLang.Syntax
import EllipLang.Eval
import EllipLang.Translator

{-
favorites :: [(String, Expr)]
favorites = [("binSearch", binSearch'), ("pairAdj", pairAdj'), ("rotL", rotL')]

printFavorites :: IO ()
printFavorites = putStrLn $ foldl (\x y -> x++"\n\n"++y) "\nFavorite examples:" $ map (\(n, e) -> n ++ ": \n" ++ pp e) favorites
-}

main :: IO ()
main = putStrLn $ pp $ translate inits'
-- main = putStrLn $ ppVal $ eval prelude $ combinations `App` exList `App` exList2