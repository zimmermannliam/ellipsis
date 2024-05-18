{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module EllipLang.Examples where

import qualified Data.Map as Map

import EllipLang.Syntax
import EllipLang.Eval
import EllipLang.Parser
import EllipLang.Pretty
import EllipLang.SmartCons

-- Constants
myLists = [
        [1..5],
        [8,14,32,0,4],
        [1,2,13,24,25,45,64,84,99,100],
        [1..10],
        [1],
        [1,2]
    ]
[exList', exList2', exList3', exList4', exList5', exList6'] = myLists

exList, exList2, exList3, exList4, exList5, exList6 :: Expr
[exList, exList2, exList3, exList4, exList5, exList6] = map icons myLists

exListV, exList2V, exList3V, exList4V, exList5V, exList6V :: Val
[exListV, exList2V, exList3V, exList4V, exList5V, exList6V] = map vcons myLists

-- PRELUDE FUNCTIONS
prelude' :: Env
prelude' = Map.fromList 
    [ (NamedVar "(++)", BVal $ eval Map.empty cat')
    , (NamedVar "length", BVal $ eval Map.empty length')
    , (NamedVar "map", BVal $ eval Map.empty mapRecursive)
    , (NamedVar "zipWith", BVal $ eval Map.empty zipWith')
    , (NamedVar "drop", BVal $ eval Map.empty drop')
    , (NamedVar "take", BVal $ eval Map.empty take')
    , (NamedVar "foldr1", BVal $ eval Map.empty foldr1')
    ]

prelude = Map.union prelude' $ Map.fromList
    [ (NamedVar "listRange", BVal $ eval prelude' listRange')
    , (NamedVar "range", BVal $ eval prelude' range')
    ]

listRange' :: Expr
listRange' = 
    xs <.> begin <.> end <.> Case (begin `Lt` inte 1) --of
    [
        (PVal $ Boolean True, Value Empty),
        (PVal $ Boolean False, Case (begin `Gt` (Var "length" `App` xs))
        [
            (PVal $ Boolean True, Value Empty),
            (PVal $ Boolean False, Case (end `Lt` inte 1)
            [
                (PVal $ Boolean True, Value Empty),
                (PVal $ Boolean False, (Var "drop" `App` (begin `Sub` inte 1)) `App` (Var "take" `App` end `App` xs))
            ])
        ])
    ]


range' :: Expr
range' =
    l <.> r <.> Case (l `Leq` r) -- of
        [ (PVal $ Boolean True,     Btwn l r)
        , (PVal $ Boolean False,    Var "reverse" `App` Btwn r l)
        ]

cat' :: Expr
cat' =
    list1 <.> list2 <.> LetRec "cat" (l1 <.> l2 <.>
        Case l1 -- of
        [
            (PVal Empty, l2),
            (PCons "x" "xs", Cons x (Var "cat" `App` xs `App` l2))
        ]
    )
    -- in
    (Var "cat") `App` list1 `App` list2

drop' :: Expr
drop' =
    n <.> list <.> LetRec "drop" (k <.> xs <.> Case (k `Leq` inte 0) -- of
    [
        (PVal $ Boolean True, xs),
        (PVal $ Boolean False, Case xs -- of
        [
            (PVal Empty, xs),
            (PCons "y" "ys", Var "drop" `App` (k `Sub` inte 1) `App` ys)
        ])
    ])
    -- in
    (Var "drop") `App` n `App` list
    
take' :: Expr
take' =
    n <.> list <.> LetRec "take" (k <.> xs <.> Case (k `Leq` inte 0) -- of
    [
        (PVal $ Boolean True, Value Empty),
        (PVal $ Boolean False, Case xs -- of
        [
            (PVal Empty, Value Empty),
            (PCons "y" "ys", Cons y (Var "take" `App` (k `Sub` inte 1) `App` ys))
        ])
    ])
    -- in
    (Var "take") `App` n `App` list

length' :: Expr
length' =
    list1 <.> LetRec "length" (l1 <.>
        Case l1 -- of
        [
            (PVal Empty, inte 0),
            (PCons "x" "xs", inte 1 `Add` (Var "length" `App` xs))
        ]
    )
    -- in
    (Var "length") `App` list1

mapRecursive :: Expr
mapRecursive =
    g <.> list1 <.> LetRec "map" (f <.> l1 <.>
        Case l1 -- of
        [
            (PVal Empty, Value Empty),
            (PCons "x" "xs", (f `App` x) `Cons` (Var "map" `App` f `App` xs))
        ]
    ) -- in
    (Var "map") `App` g `App` list1

zipWith' :: Expr
zipWith' =
    g <.> list1 <.> list2 <.> LetRec "zipWith" (f <.> l1 <.> l2 <.>
        Case l1 -- of
        [
            (PVal Empty, Value Empty),
            (PCons "x" "xs", Case l2 -- of
            [
                (PVal Empty, Value Empty),
                (PCons "y" "ys", (f `App` x `App` y) `Cons`
                    (Var "zipWith" `App` f `App` xs `App` ys))
            ])
        ])
        -- in
        (Var "zipWith") `App` g `App` list1 `App` list2

-- For map
succ' :: Expr
succ' = x <.> x `Add` inte 1

foldr1' :: Expr
foldr1' = fn <.> list <.> 
    LetRec "foldr1" (f <.> list1 <.> Case list1 -- of
    [ (PCons' x empt, x)
    , (PVal Empty, Error "foldr1: Empty list")
    , (PCons "x" "xs", f `App` x `App` (Var "foldr1" `App` f `App` xs))
    ]
    ) -- in
    (Var "foldr1" `App` fn `App` list)

------------------------------------------------------------------------
-- Example functions
------------------------------------------------------------------------

reverse' :: Expr
reverse' = xs <.> case1 xs (
    (x, 1) <..> (x, n) ==> (x!n) <...> (x!.1)
    )

map' :: Expr
map' = f <.> xs <.> case1 xs (
    (x, 1) <..> (x, n)  ==> (f `App` (x!.1)) <...> (f `App` (x!n))
    )

map2 :: Expr
map2 = f <.> xs <.> case1 xs (
    (x, 1) <..> (x, n) ==> ((f `App` (x!.1)) <:> empt) <++> (f `App` (x!.2) <...> f `App` (x!n))
    )

zip' :: Expr
zip' = 
    xs <.> ys <.> case1 xs (
        (x, 1) <..> (x, n)  ==> case1 ys (
                (y, 1) <..> (y, m) ==> ((x!.1) `Pair` (y!.1)) <...> ((x!n) `Pair` (y!m))
        )
    )
zip3' :: Expr
zip3' =
    xs <.> ys <.> zs <.> case1 xs (
        (x, 1) <..> (x, n) ==> case1 ys (
            (y, 1) <..> (y, m) ==> case1 zs (
                (z, 1) <..> (z, k) ==> ((x!.1) `Pair` (y!.1) `Pair` (z!.1)) <...> ((x!n) `Pair` (y!n) `Pair` (z!n))
            )
        )
    )

fold' :: Expr
fold' = f <.> xs <.> case1 xs (
    (x, 1) <..> (x, n)  ==> ElliFold (x!.1) (x!n) f
    )

pairAdj :: Expr
pairAdj = xs <.> case1 xs (
    (x, 1) <..> (x, n)  ==> ((x!.1) `Pair` (x!.2)) <...> ((x!(n `Sub` inte 1)) `Pair` (x!n))
    )

rotL :: Expr
rotL = xs <.> k <.> case1 xs (
    (x, 1) <..> (x, n)  ==> Let "k'" (k `Mod` n) $
        ((x!(k' `Add` inte 1)) <...> (x!n)) `Cat` ((x!.1) <...> (x!k'))
    )

inits' :: Expr
inits' = Abstr "xs" $ Case xs -- of
    [(PEllipsis "x" (Var "n"),
        ((x!.1) <...> (x!.1))
        <...>
        ((x!.1) <...> (x!n))
    )]

inits2 :: Expr
inits2 = Abstr "xs" $ Case xs -- of
    [(PEllipsis "x" (Var "n"),
        Value Empty
        <...>
        ((x!.1) <...> (x!n))
    )]

inits2Backwards :: Expr
inits2Backwards = Abstr "xs" $ Case xs -- of
    [(PEllipsis "x" (Var "n"),
        ((x!.1) <...> (x!n))
        <...>
        Value Empty
    )]

inits3 :: Expr
inits3 = Abstr "xs" $ Case xs -- of
    [(PEllipsis "x" n,
        (empt <:> empt) <++>
        (((x!.1) <:> empt)
        <...>
        ((x!.1) <...> (x!n)))
    )]

inits4 :: Expr
inits4 = Abstr "xs" $ Case xs -- of
    [(x, 1) <..> (x, n) ==> 
        (empt <:> ((x!.1) <:> empt) <:> empt)
        <++> (((x!.1) <:> (x!.2) <:> empt)
        <...>
        (((x!.1) <:> empt) <++> ((x!.2) <...> (x!n))))
    ]


-- zipInits [x1...xn] [y1...ym] = [[(x1, y1)], [(x1, y1), (x2, y2)], ..., [(x1, y1)...(xn,ym)]]
-- zipInits [x1...xn] [y1...ym] = [[(x1, y1)]]++[[(x1, y1), (x2, y2)], ..., [(x1, y1)...(xn,ym)]]
zipInits :: Expr
zipInits = xs <.> ys <.> Case xs -- of
    [(PEllipsis "x" n,
        Case ys -- of
            [( PEllipsis "y" m, 
                Var "(++)"
                `App` Cons (Cons (Pair (x!.1) (y!.1)) (Value Empty)) (Value Empty)
                `App` (
                    Cons (Pair (x!.1) (y!.1)) (Cons (Pair (x!.2) (y!.2)) (Value Empty))
                    <...>
                    (Pair (x!.1) (y!.1) <...> Pair (x!n) (y!m))
                )
            )]
    )]
binSearch' :: Expr
binSearch' = Var "binSearch"

binSearch :: Expr
binSearch = list <.> term <.> LetRec "binSearch" (xs <.> t <.>
    Case xs -- of
        [   PVal Empty          ==> false
        ,   (x, 1) <..> (x, n)  ==> Let "k" ((n `Div` inte 2) `Add` inte 1) $
                if' ((x!k) `Eq` t)          true
                else' $ if' (x!k `Gt` t)    (binSearch' `App` ((x!.1) <...> (x!(k `Sub` inte 1))) `App` t)
                else'                       (binSearch' `App` ((x!(k `Add` inte 1)) <...> (x!n)) `App` t)
        ]
    ) -- in
    (binSearch' `App` list `App` term)

subLists :: Expr
subLists = Abstr "list" $
    LetRec "sublists" (Abstr "xs" $ Case xs -- of
    [
    (PVal Empty, Value Empty),
    (PEllipsis "x" (Var "n"),
        (((x!.1) <...> (x!.1)) <...> ((x!.1) <...> (x!n)))
        `Cat` 
        App (Var "sublists") ((x!.2) <...> (x!n))
    )])
    --in
    (App (Var "sublists") (Var "list"))


combinations :: Expr
combinations = 
    xs <.> ys <.> 
        case1 xs (
        (x, 1) <..> (x, n)  ==> 
            case1 ys (
                (y, 1) <..> (y, m) ==>
                    (((x!.1) `Pair` (y!.1)) <...> ((x!.1) `Pair` (y!m)))
                    <...>
                    (((x!n) `Pair` (y!.1)) <...> ((x!n) `Pair` (y!m)))
            )
        )

firstK :: Expr
firstK = xs <.> k <.> case1 xs (
    (x, 1) <..> (x, n)  ==> (x!.1) <...> (x!k)
    )

removeKth :: Expr
removeKth = xs <.> k <.> case1 xs (
    (x, 1) <..> (x, n)  ==> 
        ((x!.1) <...> (x!(k `Sub` inte 1))) `Cat` ((x!(k `Add` inte 1)) <...> (x!n))
    )

pairWithHead :: Expr
pairWithHead = Abstr "xs" $ Case xs -- of
    [(PEllipsis "x" (Var "n"),
        ((x!.1) `Pair` (x!.2))
        <...>
        ((x!.1) `Pair` (x!n))
    )]

add' :: Expr
add' = a <.> b <.> (a `Add` b)

enumerate :: Expr
enumerate = xs <.> case1 xs (
    (x, 1) <..> (x, n) ==> 
        ((x!.1) `Pair` (inte 1))
        <...>
        ((x!n) `Pair` n)
    )

tryElliGroup :: Expr
tryElliGroup = xs <.> a <.> b <.> case1 xs (
    (x, 1) <..> (x, n) ==>
        ((x!.1) `Pair` (ElliGroup $ a `Add` (inte 1)))
        <...>
        ((x!n) `Pair` (ElliGroup $ b `Add` (inte 5)))
    )
tryElliGroup2 :: Expr
tryElliGroup2 = xs <.> a <.> b <.> case1 xs (
    (x, 1) <..> (x, n) ==>
        ((x!.1) `Pair` (a `Add` (inte 1)))
        <...>
        ((x!n) `Pair` (b `Add` (inte 5)))
    )

scanl1' :: Expr
scanl1' = f <.> xs <.> case1 xs (
    (x, 1) <..> (x, n) ==>
        Var "(++)"
        `App` Cons (x!.1) (Value Empty)
        `App` ((Var "foldl1" `App` f `App` (Cons (x!.1) (Cons (x!.2) (Value Empty))))
        <...> (Var "foldl1" `App` f `App` ((x!.1) <...> (x!n))))
    )

sum' :: Expr
sum' = xs <.> case1 xs (
    (x, 1) <..> (x, n) ==>
        ElliFold (x!.1) (x!n) (a <.> b <.> a `Add` b)
    )

------------------------------------------------------------------------
-- 
-- Recursive functions
-- 
------------------------------------------------------------------------

foldRecursive :: Expr
foldRecursive = Abstr "l" $ Abstr "f" $
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


reverseRecursive :: Expr
reverseRecursive    = Abstr "l" $ LetRec "reverse"
            ( Abstr "list" $ Case (Var "list")
            [
                (PVal Empty, Value Empty),
                (PCons "x" "xs", Cat (App (Var "reverse") (Var "xs")) (Cons (Var "x") (Value Empty)))
            ])
            -- in
            (App (Var "reverse") (Var "l"))

nthRecursive :: Expr
nthRecursive = Abstr "l" $ Abstr "n" $ 
            LetRec "nth" (Abstr "list" $ Abstr "cur" $ Case (Var "cur") 
                [
                    (PVal $ Con 0, Case (Var "list")
                    [
                        (PCons "x" "xs", Var "x"), 
                        (PVal Empty, Error "Too far"),
                        (PVar "_", Error "Bad")
                    ]),
                    (PVar "rem", Case (Var "list")
                    [
                        (PCons "x" "xs", App (App (Var "nth") (Var "xs")) (Sub (Var "rem") (Value $ Con 1))),
                        (PVal Empty, Error "Too far"),
                        (PVar "_", Error "Bad")
                    ])
                ]
            ) $ App (App (Var "nth")  (Var "l")) (Var "n")

sumRecursive :: Expr
sumRecursive = Abstr "l" $ LetRec "sum" (Abstr "list" $ Case (Var "list") 
    [
        (PVal Empty, Value $ Con 0), 
        (PCons "x" "xs", Add (Var "x") (App (Var "sum") (Var "xs")))
    ]) 
    -- in
    (App (Var "sum") (Var "l"))

lenRecursive :: Expr
lenRecursive = Abstr "l" $ LetRec "len" (Abstr "list" $ Case (Var "list")
    [
        (PVal Empty, Value $ Con 0),
        (PCons "x" "xs", Add (Value $ Con 1) (App (Var "len") (Var "xs")))
    ])
    -- in
    (App (Var "len") (Var "l"))

removeNthRecursive :: Expr
removeNthRecursive = Abstr "l" $ Abstr "i" $ 
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

recursiveModule = [
    ("fold", foldRecursive),
    ("reverse", reverseRecursive),
    ("nth", nthRecursive),
    ("sum", sumRecursive),
    ("len", lenRecursive),
    ("removeNth", removeNthRecursive)
    ]