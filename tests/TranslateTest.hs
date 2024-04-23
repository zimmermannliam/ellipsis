module Main where

import Test.HUnit

import qualified EllipLang.Examples as Ex
import EllipLang.Translator

main = runTestTTAndExit $ test $ testIsCore

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