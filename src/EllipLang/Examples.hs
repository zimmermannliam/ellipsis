{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module EllipLang.Examples where

import EllipLang.Syntax
import EllipLang.Eval
import EllipLang.Parser
import EllipLang.Pretty
import qualified Data.Map as Map


exList' :: [Int]
exList' = [1..5]
exList2' :: [Int]
exList2' = [8,14,32,0,4]
exList3' :: [Int]
exList3' = [1,2,13,24,25,45,64,84,99,100]
exList4' :: [Int]
exList4' = [1..10]
exList5' :: [Int]
exList5' = [1]
exList6' :: [Int]
exList6' = [1,2]

myCons :: [Expr]
myCons = map cons [exList', exList2', exList3', exList4', exList5', exList6']

exList, exList2, exList3, exList4, exList5, exList6 :: Expr
[exList, exList2, exList3, exList4, exList5, exList6] = myCons

exListV, exList2V, exList3V, exList4V, exList5V, exList6V :: Val
[exListV, exList2V, exList3V, exList4V, exList5V, exList6V] = map (eval prelude) myCons

-- PRELUDE FUNCTIONS
prelude :: Env
prelude = Map.fromList [(NamedVar "cmp", BVal $ eval Map.empty cmp')]

cmp :: Expr
[cmp] = map Var ["cmp"]

cmp' :: Expr
cmp' = Abstr "l" $ Abstr "r" $ Case (Sub l r) -- of
    [
        (PVal $ Con 0, Value $ Con 0),
        (PVar "x", Div x (Abs x))
    ]



succ' :: Expr
succ' = Abstr "x" $ Add (Var "x") (Value $ Con 1)

-- head' :: Expr
-- head' = Abstr "l" $ Case (Var "l") [(PVal Empty, Value Empty), (PCons "H" "T", Var "H")]

tail' :: Expr
tail' = Abstr "l" $ Case (Var "l") [(PVal Empty, Value Empty), (PCons "H" "T", Var "T")]

tail3' :: Expr
tail3' = Abstr "l" $ Case (Var "l") -- of
    [
        (PEllipsis "x" (End "n"), ellipOne x0 (IPlace 2) (End "n") "x")
    ]

removeNth' :: Expr
removeNth' = Abstr "l" $ Abstr "i" $ 
            LetRec "removeNth" (Abstr "list" $ Abstr "idx" $ Case (Var "idx")
            [
                (PVal $ Con 0, Case (Var "list")
                [
                    (PCons "x" "xs", Var "xs"),
                    (PVal Empty, Error "zero idx out of bounds"),
                    (PVar "_", Error "zero idx match error!")
                ]),
                (PVar "idx'", Case (Var "list")
                [
                    (PCons "x" "xs", Cons (Var "x") 
                                           (App (App (Var "removeNth") 
                                               (Var "xs")) 
                                               (Add (Var "idx'") (Value $ Con (-1)))
                                           )
                    ),
                    (PVal Empty, Error "nonzero idx out of bounds"),
                    (PVar "_", Error "nonzero idx match error!")
                ])
            ])
            -- in
            (App (App (Var "removeNth") (Var "l")) (Var "i"))

removeNth3' :: Expr
removeNth3' = Abstr "l" $ Abstr "n" $
    Case (Var "l") -- of
    [
        (PEllipsis "x" (End "m"), Cat
            (ellipOne x0 (IPlace 1) (EPlace $ Sub (Var "n") (Value $ Con 1)) "x")
            (ellipOne x0 (EPlace $ Add (Var "n") (Value $ Con 1)) (End "m") "x")
            )
    ]

firstN :: Expr
firstN = Abstr "l" $ Abstr "n" $
    Case (Var "l") -- of
    [
        (PEllipsis "x" (End "m"),   ellipOne x0 (IPlace 1) (EPlace $ Var "n") "x")
    ]

add' :: Expr
add' = Abstr "a" $ Abstr "b" $ Add (Var "a") (Var "b")

map3' :: Expr
map3' = Abstr "l" $ Abstr "f" $ Case l -- of
    [
        (PEllipsis "x" (End "n"), ellipOne (App f x0) (IPlace 1) (End "n") "x")
    ]

map' :: Expr
map' = Abstr "l" $ Abstr "f" $
        LetRec "map" (Abstr "list" $ Abstr "fun" $ Case (Var "list")
        [
            (PVal Empty, Value Empty),
            (PCons "x" "xs", Cons (App (Var "fun") (Var "x")) (App (App (Var "map") (Var "xs")) (Var "fun")))
        ])
            -- in
            (App (App (Var "map") (Var "l")) (Var "f"))


fold' :: Expr
fold' = Abstr "l" $ Abstr "f" $
        LetRec "fold" (Abstr "list" $ Abstr "fun" $ Case (Var "list")
        [
            (PCons "x" "xs", Case (Var "xs")
            [
                (PVal Empty, Var "x"),
                (PVar "xs'",    App 
                                (App (Var "fun") (Var "x")) 
                                (App (App (Var "fold") (Var "xs")) (Var "fun"))
                                )
            ])
        ])
        -- in
        (App (App (Var "fold") (Var "l")) (Var "f"))


reverse' :: Expr
reverse'    = Abstr "l" $ LetRec "reverse"
            ( Abstr "list" $ Case (Var "list")
            [
                (PVal Empty, Value Empty),
                (PCons "x" "xs", Cat (App (Var "reverse") (Var "xs")) (Cons (Var "x") (Value Empty)))
            ])
            -- in
            (App (Var "reverse") (Var "l"))


reverse3' :: Expr
reverse3' = Abstr "l" $ Case (Var "l") -- of
    [
        (PEllipsis "x" (End "n"),   ellipOne x0 (End "n") (IPlace 1) "x")
    ]

second' :: Expr
second' = Abstr "l" $ Case (Var "l")
            [
                (PCons "x" "xs", Case (Var "xs")
                                 [ 
                                    (PCons "y" "ys", Var "y"),
                                    (PVal Empty, Value Empty)
                                 ]),
                (PVal Empty, Value Empty)
            ]

second2' :: Expr
second2' = Abstr "l" $ Case (Var "l") -- of
    [
        (PEllipsis "x" (End "n"), ListElement "x" (IPlace 2))
    ]


nth' :: Expr
nth' = Abstr "l" $ Abstr "n" $ 
            LetRec "nth" (Abstr "list" $ Abstr "cur" $ Case (Var "cur") 
                [
                    (PVal $ Con 0, Case (Var "list")
                    [
                        (PCons "x" "xs", Var "x"), 
                        (PVal Empty, Value Empty),
                        (PVar "_", Var "_")
                    ]),
                    (PVar "rem", Case (Var "list")
                    [
                        (PCons "x" "xs", App (App (Var "nth") (Var "xs")) (Add (Var "rem") (Value $ Con (-1)))),
                        (PVal Empty, Value Empty),
                        (PVar "_", Var "_")
                    ])
                ]
            ) $ App (App (Var "nth")  (Var "l")) (Var "n")

nth2' :: Expr
nth2' = Abstr "l" $ Abstr "n" $ Case (Var "l")
        -- of
        [
            (PEllipsis "x" (End "m"), ListElement "x" (EPlace $ Var "n"))
        ]

-- Can't do zip, type-wise. Need a value-list
zip' :: Expr
zip' = Abstr "l1" $ Abstr "l2" $ 
            LetRec "zip" (Abstr "list1" $ Abstr "list2" $ Case (Var "list1")
            [
                (PVal Empty, Value Empty),
                (PCons "x" "xs", Case (Var "list2")
                [
                    (PVal Empty, Value Empty),
                    (PCons "y" "ys", Cons 
                        (Pair (Var "x") (Var "y")) 
                        (App (App (Var "zip") (Var "xs")) (Var "ys"))
                    )
                ])
            ])
            -- in
            (App (App (Var "zip") (Var "l1")) (Var "l2"))

{-
zip2' :: Expr
zip2 = Abstr "l1" $ Abstr "l2" $ Case (Var "l1") -- of
    [
        (PEllipsis "x" (End "n") , Case 
    ]
    -}

find' :: Expr
find' = Value $ Con 1

sum' :: Expr
sum' = Abstr "l" $ LetRec "sum" (Abstr "list" $ Case (Var "list") 
    [
        (PVal Empty, Value $ Con 0), 
        (PCons "x" "xs", Add (Var "x") (App (Var "sum") (Var "xs")))
    ]) 
    -- in
    (App (Var "sum") (Var "l"))

len' :: Expr
len' = Abstr "l" $ LetRec "len" (Abstr "list" $ Case (Var "list")
    [
        (PVal Empty, Value $ Con 0),
        (PCons "x" "xs", Add (Value $ Con 1) (App (Var "len") (Var "xs")))
    ])
    -- in
    (App (Var "len") (Var "l"))

len2' :: Expr
len2' = Abstr "l" $ Case (Var "l")
    [
        (PEllipsis "x" (End "n"), Var "n")
    ]

sumNum' :: Expr
sumNum' = Abstr "n" $ LetRec "sumNum" (Abstr "x" $ Case (Var "x") [(PVal $ Con 0, Value $ Con 0), (PVar "y", Add (Var "y") (App (Var "sumNum") (Add (Var "y") (Value $ Con (-1)))))]) $ App (Var "sumNum") (Var "n")

sucEach' :: Expr
sucEach' = Abstr "l" $ LetRec "sucEach" (Abstr "list" $ Case (Var "list")
    [
        (PVal Empty, Value Empty),
        (PCons "x" "xs", Cons (Add (Var "x") (Value $ Con 1)) (App (Var "sucEach") (Var "xs")))
    ]
    )
    -- in
    (App (Var "sucEach") (Var "l"))

fst' :: Expr
fst' = Abstr "a" $ Abstr "b" $ Var "a"

snd' :: Expr
snd' = Abstr "a" $ Abstr "b" $ Var "b"

id' :: Expr
id' = Abstr "l" (LetRec "id" (Abstr "list" $ Case (Var "list")
    [
        (PVal Empty, Value Empty),
        (PCons "x" "xs", Cons (Var "x") (App (Var "id") (Var "xs")))
    ])
    -- in
    (App (Var "id") (Var "l")))


split' :: Expr
split' = Abstr "l" $ Case l -- of
    [
        (PEllipsis "x" (End "n"), Pair 
                            (ellipOne x0 (IPlace 1) (EPlace $ Div (Var "n") (Value $ Con 2)) "x")
                            (ellipOne x0 (EPlace $ Add (Div (Var "n") (Value $ Con 2)) (Value $ Con 1)) (EPlace $ Var "n") "x"))
    ]

listId' :: Expr
listId' = Abstr "l" $ Case (Var "l") -- of
    [
        (PEllipsis "x" (End "n"),   ellipOne x0 (IPlace 1) (End "n") "x")
    ]

binSearch' :: Expr
binSearch' = Abstr "list" $ Abstr "term" (LetRec "binSearch" (Abstr "l" $ Abstr "t" 
    -- $ Trace "l" l 
    $ Case (Var "l") -- of
    [
        (PVal Empty,    Value $ Boolean False),
        (PEllipsis "x" (End "n"),     Let "k" (Add (Div n (Value $ Con 2)) (Value $ Con 1)) $ 
            Case (ListElement "x" (End "k") `Eq` t)
            [
                (PVal $ Boolean True, Value $ Boolean True),
                (PVal $ Boolean False, Case (ListElement "x" (End "k") `Gt` t)
                [
                    (PVal $ Boolean True, 
                              Var "binSearch" 
                        `App` ellipOne x0 (IPlace 1) (EPlace $ k `Sub` Value (Con 1)) "x" 
                        `App` t),
                    (PVal $ Boolean False, 
                              Var "binSearch"
                        `App` ellipOne x0 (EPlace $ k `Add` Value (Con 1)) (End "n") "x"
                        `App` t)
                ])
            ]
        )
    ]) -- in
    (App (App (Var "binSearch") (Var "list")) (Var "term"))
    )

pairAdj' :: Expr
pairAdj' = Abstr "l" $ Case l -- of
    [
        (PEllipsis "x" (End "n"), Ellipsis (Pair x0 x1) [EllipRange {ident=0, var="x", ib=IPlace 1, ie=EPlace $ Sub n (Value $ Con 1), contentT = BeList}, EllipRange {ident=1, var="x", ib=IPlace 2, ie=End "n", contentT = BeList}])
    ]

rotL' :: Expr
rotL' = Abstr "l" $ Abstr "k" $ Case l -- of
    [
        (PEllipsis "x" (End "n"), Let "k'" (k `Mod` n) $
        ellipOne x0 (EPlace $ k' `Add` Value (Con 1)) (EPlace n) "x"
            `Cat` ellipOne x0 (IPlace 1) (EPlace k') "x")
    ]

-- Unfinsihed
rotR' :: Expr
rotR' = Abstr "l" $ Abstr "k" $ Case l -- of
    [
        (PEllipsis "x" (End "n"), Let "k'" (k `Mod` n) $
        ellipOne x0 (EPlace $ k' `Add` Value (Con 1)) (EPlace n) "x"
            `Cat` ellipOne x0 (IPlace 1) (EPlace k') "x")
    ]

subArrays' :: Expr
subArrays' = Abstr "l" $ Abstr "k" $ Case l -- of
    [
        (PVal Empty, Value Empty),
        (PEllipsis "x" (End "n"), Ellipsis (Ellipsis x0 [EllipRange {ident=0, var="x", ib=EPlace x1, ie=EPlace (x1 `Add` k `Sub` Value (Con 1)), contentT=BeList}]) [EllipRange {ident=1, var="x", ib=IPlace 1, ie=EPlace (n `Sub` k `Add` Value (Con 1)), contentT=BeIndices}])
    ]

indices' :: Expr
indices' = Abstr "l" $ Case l -- of
    [
        (PEllipsis "x" (End "n"), Ellipsis x0 [EllipRange {ident=0, var="", ib=IPlace 1, ie=EPlace n, contentT=BeIndices}])
    ]

{-
mergeSort' :: Expr
mergeSort' = Abstr "l" $ LetRec "mergeSort" -- =
    (Abstr "list" $ Case (Var "list") -- of
    [
        (PVal Empty, Value Empty),
        (PEllipsis "x" (End "n"), 

        )
        (PEllipsis "x" (End "n"), )
    -- in
    (App (Var "mergeSort") (Var "l"))

merge' :: Expr
merge = Abstr "l1" $ Abstr "l2"
-}

myTest :: Expr
myTest = Abstr "l" $ Case l -- of
    [
        (PEllipsis "x" (End "n"), 
            (ListElement "x" (EPlace $ Value $ Con 1) `Add` Value (Con 1)) 
            `PreEllipsis` 
            (ListElement "x" (EPlace n) `Add` Value (Con 1))
        )
    ]

pairAdjPreTerm1 :: Expr
pairAdjPreTerm1 = ListElement "x" (EPlace $ Value $ Con 1) `Pair` ListElement "x" (EPlace $ Value $ Con 2)
pairAdjPreTerm2 :: Expr
pairAdjPreTerm2 = ListElement "x" (EPlace $ n `Sub` Value (Con 1)) `Pair` ListElement "x" (EPlace n)

pairAdjPre :: Expr
pairAdjPre = Abstr "l" $ Case l -- of
    [
        (PEllipsis "x" (End "n"),
            (ListElement "x" (EPlace $ Value $ Con 1) `Pair` ListElement "x" (EPlace $ Value $ Con 2))
            `PreEllipsis`
            (ListElement "x" (EPlace $ n `Sub` Value (Con 1)) `Pair` ListElement "x" (EPlace n))
            )
    ]

enumeratePre :: Expr
enumeratePre = Abstr "l" $ Case l -- of
    [
        (PEllipsis "x" (End "n"),
            ((ListElement "x" (EPlace $ Value $ Con 1))
            `Pair`
            (Index $ EPlace $ Value $ Con 1))
            `PreEllipsis`
            ((ListElement "x" (EPlace $ Var "n"))
            `Pair`
            (Index $ EPlace $ Var "n"))
        )
    ]

zipPre' :: Expr
zipPre' = Abstr "xs" $ Abstr "ys" $ Case xs -- of
    [(PEllipsis "x" (End "n"),
        Case ys -- of
            [(PEllipsis "y" (End "m"),
                ((ListElement "x" $ EPlace $ Value $ Con 1)
                `Pair`
                (ListElement "y" $ EPlace $ Value $ Con 1))
                `PreEllipsis`
                ((ListElement "x" $ EPlace $ Var "n")
                `Pair`
                (ListElement "y" $ EPlace $ Var "n"))
            )]
    )]

zipPre :: Expr
zipPre = Abstr "xs" $ Abstr "ys" $ Case xs -- of
    [(PEllipsis "x" (End "n"),
        Case ys -- of
            [(PEllipsis "y" (End "m"),
                ((x !. 1) <+> (y !. 1)) <...> ((x ! n) <+> (y ! m))
            )]
    )]

{-
combinations (x[1] ... x[n]) (y[1] ... y[m]) = 
    [(x[1], y[1]) ... (x[1], y[m])] ++ ... ++ [(x[n], y[1]) ... (x[n], y[m])]
-} 


combinations :: Expr
combinations = Abstr "xs" $ Abstr "ys" $ Case xs -- of
    [(PEllipsis "x" (End "n"),
        Case ys -- of
            [(PEllipsis "y" (End "m"),
                (((x !. 1) <+> (y !. 1)) <...> ((x !. 1) <+> (y ! m)))
                <...>
                (((x ! n) <+> (y !. 1)) <...> ((x ! n) <+> (y ! m)))
            )]
    )]

testEllip :: Expr
testEllip = Abstr "xs" $ Case xs -- of
    [(PEllipsis "x" (End "n"),
        (x!.1 `Add` inte 1) <...> (x!n `Add` inte 1)
    )]

sublists' :: Expr
sublists' = Abstr "list" $
    LetRec "sublists" (Abstr "xs" $ Case xs -- of
    [
    (PVal Empty, Value Empty),
    (PEllipsis "x" (End "n"),
        ((x!.1 <...> x!.1) <...> (x!.1 <...> x!n))
        `Cat` 
        App (Var "sublists") (x!.2 <...> x!n)
    )])
    --in
    (App (Var "sublists") (Var "list"))

pairWithHead :: Expr
pairWithHead = Abstr "xs" $ Case xs -- of
    [(PEllipsis "x" (End "n"),
        ((x!.1) `Pair` (x!.2))
        <...>
        ((x!.1) `Pair` (x!n))
    )]

inits' :: Expr
inits' = Abstr "xs" $ Case xs -- of
    [(PEllipsis "x" (End "n"),
        (x!.1 <...> x!.1)
        <...>
        (x!.1 <...> x!n)
    )]

rotL2 :: Expr
rotL2 = Abstr "k" $ Abstr "xs" $ Case xs -- of
    [(PEllipsis "x" (End "n"), 
        (x!(k `Add` inte 1) <...> x!n) `Cat` (x!.1 <...> x!k)
    )]