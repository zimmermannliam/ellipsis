module Main where

import qualified EllipLang.Examples as Ex
import EllipLang.Translator
import EllipLang.Eval (eval)
import EllipLang.Syntax
import EllipLang.SmartCons (vcons, vPairVCons, listToVCons)

import Test.HUnit
import Data.List (inits)

prelude = Ex.prelude

main = runTestTTAndExit $ test $ testIsCore ++ testTranslate

testIsCore = map ("isCore" ~:)
    [ isCore Ex.foldRecursive ~?= True
    , isCore Ex.reverseRecursive ~?= True
    , isCore Ex.nthRecursive ~?= True
    , isCore Ex.sumRecursive ~?= True
    , isCore Ex.lenRecursive ~?= True
    , isCore Ex.removeNthRecursive ~?= True
    , isCore Ex.firstK ~?= False
    , isCore Ex.removeKth ~?= False
    , isCore Ex.binSearch ~?= False
    , isCore Ex.zip' ~?= False
    , isCore Ex.combinations ~?= False
    , isCore Ex.subLists ~?= False
    , isCore Ex.pairWithHead ~?= False
    , isCore Ex.inits' ~?= False
    , isCore Ex.rotL ~?= False
    , isCore Ex.pairAdj ~?= False
    , isCore Ex.map' ~?= False
    , isCore Ex.fold' ~?= False
    ]

pairAdj :: [a] -> [(a, a)]
pairAdj xs = zip xs (drop 1 xs)

testTranslate = 
    map 
        (\(label, actual, expected) -> "translate" ~: label ~: eval prelude (translate actual) 
            ~?= expected)
        [ ("map succ exList", Ex.map' `App` Ex.succ' `App` Ex.exList, vcons $ map succ Ex.exList')
        , ("map succ exList2", Ex.map' `App` Ex.succ' `App` Ex.exList2, vcons $ map succ Ex.exList2')
        , ("pairAdj exList2", Ex.pairAdj `App` Ex.exList2, vPairVCons $ pairAdj Ex.exList2')
        , ("zip exList1 exList2", Ex.zip' `App` Ex.exList `App` Ex.exList2, vPairVCons $ zip Ex.exList' Ex.exList2')
        , ("inits exList", Ex.inits' `App` Ex.exList, listToVCons $ map vcons $ filter ([] /=) $ inits Ex.exList')
        ]