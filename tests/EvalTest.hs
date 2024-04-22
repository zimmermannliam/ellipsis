module Main where

import Test.HUnit
import System.Exit as Exit
import qualified Data.Map as Map
import Data.Function ((&))
import Data.List (inits)

import EllipLang.Eval
import EllipLang.Syntax
import EllipLang.SmartCons
import EllipLang.Examples
main = runTestTTAndExit $ test $ testSmartCons ++ testEval
    {-do 
    result  <- runTestTT $ test $ testSmartCons ++ testEval
    if failures result > 0 || errors result > 0 then Exit.exitFailure else Exit.exitSuccess-}

testSmartCons = map ("smart cons" ~:)
    [ "<.>" ~:
        x <.> y <.> x `Add` y
        ~?= Abstr "x" (Abstr "y" (Add (Var "x") (Var "y")))
    , "case1, ==>" ~:(
        x <.> case1 x (
            PVar "y" ==> (y `Add` inte 1)
        )
        ) ~?= Abstr "x" (Case x [(PVar "y", Add y (Value $ Con 1))])
    , "ellip" ~:(
        x <.> case1 x (
            (x, 1) <..> (x, n) ==> (x!.1 `Add` inte 1) <...> (x!n `Add` inte 1)
        )
        ) ~?= Abstr "x" (Case x 
            [(PEllipsis "x" (End "n"), 
                Add (ListElement "x" (EPlace $ Value $ Con 1)) (Value $ Con 1)
                `PreEllipsis`
                Add (ListElement "x" (EPlace $ Var "n")) (Value $ Con 1)
            )])
    ]


-- [1,2,13,24,25,45,64,84,99,100],
testEval :: [Test]
testEval = map (\(l, t, expectedV) -> "eval" ~: l ~: eval Map.empty t ~?= expectedV)
    [ ("rotL [8,14,32,0,4] 0",
        rotL `App` exList2 `App` inte 0,
        vcons [8,14,32,0,4])
    , ("rotL [8,14,32,0,4] 1",
        rotL `App` exList2 `App` inte 1,
        vcons [14,32,0,4,8])
    , ("rotL [8,14,32,0,4] 3",
        rotL `App` exList2 `App` inte 3,
        vcons [0,4,8,14,32])
    , ("rotL [8,14,32,0,4] 5",
        rotL `App` exList2 `App` inte 5,
        vcons [8,14,32,0,4])

    , ("binSearch [1,2,13,24,25,45,64,84,99,100] 25",
        binSearch `App` exList3 `App` inte 25,
        Boolean True)
    , ("binSearch [1,2,13,24,25,45,64,84,99,100] 100",
        binSearch `App` exList3 `App` inte 100,
        Boolean True)
    , ("binSearch [1,2,13,24,25,45,64,84,99,100] 1",
        binSearch `App` exList3 `App` inte 1,
        Boolean True)
    , ("binSearch [1,2,13,24,25,45,64,84,99,100] 0",
        binSearch `App` exList3 `App` inte 0,
        Boolean False)
    , ("binSearch [1,2,13,24,25,45,64,84,99,100] 101",
        binSearch `App` exList3 `App` inte 101,
        Boolean False)
    , ("binSearch [1,2,13,24,25,45,64,84,99,100] 26",
        binSearch `App` exList3 `App` inte 26,
        Boolean False)

    , ("zip' [1,2,3,4,5] [8,14,32,0,4]",
        zip' `App` exList `App` exList2,
        zip exList' exList2' 
            & map (\(x,y) -> VPair (Con x) (Con y)) 
            & foldr VCons Empty)
    
    , ("pairWithHead [1,2,3,4,5]",
        pairWithHead `App` exList ,
        foldr VCons Empty [
            VPair (Con 1) (Con 2), 
            VPair (Con 1) (Con 3),
            VPair (Con 1) (Con 4),
            VPair (Con 1) (Con 5)
            ])
    
    , ("inits' [1,2,3,4,5]",
        inits' `App` exList,
        [1,2,3,4,5] 
            & inits 
            & filter (/= []) 
            & foldr (VCons . vcons) Empty)
    
    , ("subLists [1,2,3,4,5]",
        subLists `App` exList,
        let 
            mySubLists [] = []
            mySubLists l@(x:xs) = (filter (/= []) $ inits l) ++ mySubLists xs
        in [1,2,3,4,5] 
            & mySubLists 
            & foldr (VCons . vcons) Empty
        )

{-  BRING ME BACK WHEN WE GET FOLD
    , ("combinations [1,2,3,4,5] [8,14,32,0,4]",
        combinations `App` exList `App` exList2,
        [(x,y) | x<-exList', y<-exList2'] 
            & map (\(x,y) -> VPair (Con x) (Con y)) 
            & foldr VCons Empty)
            -}
    , ("combinations [1,2,3,4,5] [8,14,32,0,4]",
        combinations `App` exList `App` exList2,
        -- Make combination list: [[(1,8), (1,14), ...], ...]
        let listOfListOfPairs = map (\ele1 -> zip (repeat ele1) exList2') exList'
        -- Convert to value
        in listOfListOfPairs
            & map (map (\(a,b) -> VPair (Con a) (Con b)))
            & map (foldr VCons Empty)
            & foldr VCons Empty)

    , ("removeKth [8,14,32,0,4] 3",
        removeKth `App` exList2 `App` inte 3,
        vcons [8,14,0,4])
    , ("removeKth [8,14,32,0,4] 1",
        removeKth `App` exList2 `App` inte 1,
        vcons [14,32,0,4])
    , ("removeKth [8,14,32,0,4] 5",
        removeKth `App` exList2 `App` inte 5,
        vcons [8,14,32,0])
        -- oob?

    , ("pairAdj [1,2,3,4,5]",
        pairAdj `App` exList,
        let xs = exList'
        in zip xs (tail xs) & vPairVCons)
    , ("pairAdj [8,14,32,0,4]",
        pairAdj `App` exList2,
        let xs = exList2'
        in zip xs (tail xs) & vPairVCons)
    , ("pairAdj [1]",
        pairAdj `App` exList5,
        let xs = exList5'
        in zip xs (tail xs) & vPairVCons)
    , ("pairAdj [1,2]",
        pairAdj `App` exList6,
        let xs = exList6'
        in zip xs (tail xs) & vPairVCons)
    
    , ("map' succ' [8,14,32,0,4]",
        map' `App` succ' `App` exList2,
        vcons [9,15,33,1,5])

    , ("fold' add' [1,2,3,4,5]",
        fold' `App` add' `App` exList,
        Con (foldr1 (+) exList'))
    , ("fold' add' [8,14,32,0,4]",
        fold' `App` add' `App` exList2,
        Con (foldr1 (+) exList2'))

    ]