module Main where

import Test.HUnit

import qualified EllipLang.Examples as Ex
import EllipLang.Translator
import EllipLang.Eval (eval)
import EllipLang.Syntax
import EllipLang.SmartCons (vcons)

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

testTranslate = 
    map 
        (\(actual, expected) -> "translate" ~: eval prelude (translate actual) 
            ~?= expected)
        [ (Ex.map' `App` Ex.succ' `App` Ex.exList, vcons $ map succ Ex.exList')
        , (Ex.map' `App` Ex.succ' `App` Ex.exList2, vcons $ map succ Ex.exList2')
        ]