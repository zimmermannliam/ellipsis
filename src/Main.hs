{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main where

import EllipLang.Examples
import EllipLang.Pretty ( pp )
import EllipLang.Syntax

favorites :: [(String, Expr)]
favorites = [("binSearch", binSearch'), ("pairAdj", pairAdj'), ("rotL", rotL')]

printFavorites :: IO ()
printFavorites = putStrLn $ foldl (\x y -> x++"\n\n"++y) "\nFavorite examples:" $ map (\(n, e) -> n ++ ": \n" ++ pp e) favorites

main :: IO ()
main = printFavorites