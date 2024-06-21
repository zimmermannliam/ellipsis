{-# LANGUAGE OverloadedStrings #-}
module Main where

import EllipLang.Translator
import EllipLang.Parser
import EllipLang.Eval
import EllipLang.MHSPrelude (prelude)
import EllipLang.Syntax
import EllipLang.SmartCons (vcons, listToVCons,vPairVCons)

import System.IO (openFile, hGetContents, IOMode (ReadMode))

import Data.Function ((&))
import Data.List
import qualified Data.Map as Map
import qualified Data.Text as T

import Text.Megaparsec (parseMaybe, runParser, errorBundlePretty)

import Test.HUnit

main :: IO ()
main = do
    handle <- openFile "tests/test.dotdot" ReadMode
    contents <- hGetContents handle
    let testFns = processFile contents
    env <- runFile prelude testFns
    runTestTTAndExit $ test $ simpleExamples env


------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

list1 = [2,1,4,3,5]
list2 = [6,7,8,9,10]
list3 = [4,3,5,6,4]

simpleExamples env = map (\(s, val) -> s ~: runExpr env s ~?= val)
    [ ("catt list1 list2",          vcons $ list1 ++ list2)
    , ("catt [] list2",             vcons $ [] ++ list2)
    , ("catt list1 []",             vcons $ list1 ++ [])

    , ("lengtht list1",             Con $ length list1)
    , ("lengtht []",                Con $ length [])

    , ("ktht 2 list2",              Con $ list2!!(2-1))

    , ("reverse list1",             vcons $ reverse list1)
    , ("reverse list2",             vcons $ reverse list2)
    , ("reverse list3",             vcons $ reverse list3)
    , ("reverse []",                vcons $ reverse [])

    , ("mapt (\\x -> x+1) list1",   vcons $ map (\x -> x+1) list1)
    , ("mapt (\\x -> x+1) []",      vcons $ map (\x -> x+1) [])

    , ("zipWith (\\x y -> x+y) list1 list2", vcons $ zipWith (+) list1 list2)
    , ("zipWith (\\x y -> x+y) list3 list2", vcons $ zipWith (+) list3 list2)
    , ("zipWith (\\x y -> x+y) [] list2", vcons $ zipWith (+) [] list2)
    , ("zipWith (\\x y -> x+y) list2 []", vcons $ zipWith (+) list2 [])

    , ("zipWith3 (\\x y z -> x+y+z) list1 list2 list3", vcons $ zipWith3 (\x y z -> x+y+z) list1 list2 list3)
    , ("zipWith3 (\\x y z -> x+y+z) list1 list2 []", vcons $ zipWith3 (\x y z -> x+y+z) list1 list2 [])
    
    , ("foldl1t sub list1",         Con $ foldl1 (-) list1)
    , ("foldr1t sub list1",         Con $ foldr1 (-) list1)
    , ("foldlt sub 7 list1",        Con $ foldl (-) 7 list1)
    , ("foldrt sub 7 list1",        Con $ foldr (-) 7 list1)
    , ("foldlt sub 7 []",           Con $ foldl (-) 7 [])
    , ("foldrt sub 7 []",           Con $ foldr (-) 7 [])

    , ("initst list1",              listToVCons $ map vcons $ inits list1)
    , ("initst []",                 listToVCons $ map vcons $ inits [])

    , ("tailst list1",              listToVCons $ map vcons $ tails list1)
    , ("tailst []",                 listToVCons $ map vcons $ tails [])

    , ("taket 3 list1",             vcons $ take 3 list1)
    , ("taket 0 list1",             vcons $ take 0 list1)
    , ("taket 5 list1",             vcons $ take 5 list1)
    , ("taket 8 list1",             vcons $ take 8 list1)
    , ("taket (0-1) list1",         vcons $ take (0-1) list1)

    , ("dropt 3 list1",             vcons $ drop 3 list1)
    , ("dropt 0 list1",             vcons $ drop 0 list1)
    , ("dropt 5 list1",             vcons $ drop 5 list1)
    , ("dropt 8 list1",             vcons $ drop 8 list1)
--    , ("dropt (0-1) list1",         vcons $ drop (0-1) list1)     returns []

    , ("ranget 2 8",                vcons $ [2..8])
    , ("ranget 8 2",                vcons $ reverse [2..8])
    , ("ranget 3 3",                vcons $ [3..3])

    -- slice not tested

    , ("removet 1 list1",           vcons $ remove 1 list1)
    , ("removet 3 list1",           vcons $ remove 3 list1)
    , ("removet 5 list1",           vcons $ remove 5 list1)
    , ("removet 0 list1",           vcons $ remove 0 list1)
    , ("removet 6 list1",           vcons $ remove 6 list1)
    , ("removet 3 []",              vcons $ remove 6 [])

    , ("insertAtt 1 7 list1",       vcons $ insertAt 1 7 list1)
    , ("insertAtt 3 7 list1",       vcons $ insertAt 3 7 list1)
    , ("insertAtt 5 7 list1",       vcons $ insertAt 5 7 list1)
    , ("insertAtt 0 7 list1",       vcons $ insertAt 0 7 list1)
    , ("insertAtt 6 7 list1",       vcons $ insertAt 6 7 list1)

    , ("replacet 1 7 list1",        vcons $ replace 1 7 list1)
    , ("replacet 3 7 list1",        vcons $ replace 3 7 list1)
    , ("replacet 5 7 list1",        vcons $ replace 5 7 list1)
    , ("replacet 0 7 list1",        vcons $ replace 0 7 list1)
    , ("replacet 6 7 list1",        vcons $ replace 6 7 list1)

    , ("appendt 7 list1",           vcons $ list1 ++ [7])
    , ("appendt 7 []",              vcons $ [] ++ [7])

    , ("prependt 7 list1",          vcons $ 7:list1)
    , ("prependt 7 []",             vcons $ 7:[])

    , ("zipt list1 list2",          vPairVCons $ zip list1 list2)
    , ("zipt list1 []",             vPairVCons $ zip list1 [])
    , ("zipt [] list2",             vPairVCons $ zip [] list2)

    , ("fact 1",                    Con 1)
    , ("fact 5",                    Con 120)

    , ("pt 1",                      Con 1)
    , ("pt 5",                      Con 55)

    , ("concatt []",                vcons $ concat [])
    , ("concatt [list1, list2, list3]", vcons $ concat [list1, list2, list3])
    , ("concatt [list1, [], list2]",vcons $ concat [list1, [], list2])

    , ("concatMapt (take 3) []",    vcons $ concatMap (take 3) [])
    , ("concatMapt (take 3) [list1, list2]", vcons $ concatMap (take 3) [list1, list2])

    , ("allt isEven [2,4,6]",       Boolean $ all isEven [2,4,6])
    , ("allt isEven list1",         Boolean $ all isEven list1)

    , ("anyt isEven list1",         Boolean $ any isEven list1)
    , ("anyt isEven [1,3,5]",       Boolean $ any isEven [1,3,5])

    , ("maximumt list1",            Con $ maximum list1)
    , ("maximumt list2",            Con $ maximum list2)
    , ("maximumt list3",            Con $ maximum list3)
    , ("maximumt [5]",              Con $ maximum [5])

    , ("minimumt list1",            Con $ minimum list1)
    , ("minimumt list2",            Con $ minimum list2)
    , ("minimumt list3",            Con $ minimum list3)
    , ("minimumt [5]",              Con $ minimum [5])

    , ("sumt list1",                Con $ sum list1)
    , ("sumt []",                   Con $ sum [])

    , ("filtert isEven list1",      vcons $ filter isEven list1)
    , ("filtert isEven list2",      vcons $ filter isEven list2)
    , ("filtert isEven list3",      vcons $ filter isEven list3)
    , ("filtert isEven []",         vcons $ filter isEven [])

    ]
  where
    remove k xs = (take (k-1) xs) ++ (drop k xs)
    insertAt k y xs = (take (k) xs) ++ [y] ++ (drop k xs)
    replace k y xs = (take (k-1) xs) ++ [y] ++ (drop k xs)
    isEven x = (x `mod` 2) == 0

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

runExpr :: Env -> String -> Val
runExpr env s =
    let Just e = parseMaybe pExpr (T.pack s)
        e_t     = translate e
    in eval env e_t

runFile :: Env -> [String] -> IO Env
runFile env smts = go env smts
  where
    go :: Env -> [String] -> IO Env
    go env (s:ss) = case runParser pDecl "" (T.pack s) of
        Left err   -> do
            putStrLn $ errorBundlePretty err
            go env ss

        Right (Decl s expr) -> do
            let translated = translate expr
            let res = eval env translated
            let newEnv = Map.fromList [(NamedVar s, BVal res)]
            went <- go (Map.union newEnv env) ss
            return $ Map.union newEnv went

        Right expr  -> do
            let translated = translate expr
            let res = eval env translated
            go env ss

    go env [] = return env