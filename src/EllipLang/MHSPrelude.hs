module EllipLang.MHSPrelude where

import EllipLang.Syntax
import EllipLang.Eval

import qualified Data.Map as Map

prelude = buildPrelude Map.empty prePrelude

buildPrelude :: Env -> [(String, Expr)] -> Env
buildPrelude env ((s,e):bs) =
    let v = BVal $ eval env e
        b = NamedVar s
        newEnv = Map.insert b v env
    in Map.union newEnv (buildPrelude newEnv bs)
buildPrelude env [] = Map.empty

toZipWithN :: Int -> Expr
toZipWithN 0 = Var "id"
toZipWithN 1 = Var "map"
toZipWithN 2 = Var "zipWith"
toZipWithN i = Var ("zipWith" ++ show i)

unVar (Var s) = s

lengthFun       = Var "length"
subscriptFun    = Var "subscript"
catFun          = Var "cat"
foldr1Fun       = Var "foldr1"
foldl1Fun       = Var "foldl1"


-- PREPRELUDE IS GENERATED FROM THE FILE "prelude..."
prePrelude =
    [ ("error", Abstr "err" (Var "err"))
    , ("add", Abstr "a" (Abstr "b" (Op Add (Var "a") (Var "b"))))
    , ("sub", Abstr "a" (Abstr "b" (Op Sub (Var "a") (Var "b"))))
    , ("succ", Abstr "a" (Op Add (Var "a") (Value (Con 1))))
    , ("negate", Abstr "a" (Op Sub (Value (Con 0)) (Var "a")))
    , ("max", Abstr "a" (Abstr "b" (Case (Op Gt (Var "a") (Var "b")) [(PVal (Boolean True),Var "a"),(PVal (Boolean False),Var "b")])))
    , ("min", Abstr "a" (Abstr "b" (Case (Op Lt (Var "a") (Var "b")) [(PVal (Boolean True),Var "a"),(PVal (Boolean False),Var "b")])))
    , ("abs", Abstr "a" (Op (VarOp "div") (Op Mul (Var "a") (Var "a")) (Var "a")))
    , ("cat", Abstr "list1" (Abstr "list2" (LetRec "go" (Abstr "xs" (Abstr "ys" (Case (Var "xs") [(PVal Empty,Var "ys"),(PCons "x" "xr",Cons (Var "x") (App (App (Var "go") (Var "xr")) (Var "ys")))]))) (App (App (Var "go") (Var "list1")) (Var "list2")))))
    , ("length", Abstr "xs" (LetRec "go" (Abstr "xs" (Case (Var "xs") [(PVal Empty,Value (Con 0)),(PCons "x" "xr",Op Add (Value (Con 1)) (App (Var "go") (Var "xr")))])) (App (Var "go") (Var "xs"))))
    , ("reverse", Abstr "xs" (LetRec "go" (Abstr "xs" (Case (Var "xs") [(PVal Empty,Value Empty),(PCons "x" "xr",App (App (Var "cat") (App (Var "go") (Var "xr"))) (Cons (Var "x") (Value Empty)))])) (App (Var "go") (Var "xs"))))
    , ("drop", Abstr "k" (Abstr "xs" (LetRec "go" (Abstr "k" (Abstr "xs" (Case (Var "xs") [(PVal Empty,Value Empty),(PCons "x" "xr",Case (Op Eq (Var "k") (Value (Con 0))) [(PVal (Boolean True),Var "xs"),(PVal (Boolean False),App (App (Var "go") (Op Sub (Var "k") (Value (Con 1)))) (Var "xr"))])]))) (App (App (Var "go") (Var "k")) (Var "xs")))))
    , ("take", Abstr "k" (Abstr "xs" (LetRec "go" (Abstr "k" (Abstr "xs" (Case (Op Eq (Var "k") (Value (Con 0))) [(PVal (Boolean True),Value Empty),(PVal (Boolean False),Case (Var "xs") [(PVal Empty,Value Empty),(PCons "x" "xr",Cons (Var "x") (App (App (Var "go") (Op Sub (Var "k") (Value (Con 1)))) (Var "xr")))])]))) (App (App (Var "go") (Var "k")) (Var "xs")))))
    , ("first", Abstr "xs" (LetRec "go" (Abstr "xs" (Case (Var "xs") [(PVal Empty,App (Var "error") (Value (Con 100))),(PCons "x" "xr",Case (Op Eq (Var "xr") (Value Empty)) [(PVal (Boolean True),Value Empty),(PVal (Boolean False),Cons (Var "x") (App (Var "go") (Var "xr")))])])) (App (Var "go") (Var "xs"))))
    , ("tail", Abstr "xs" (Case (Var "xs") [(PVal Empty,App (Var "error") (Value (Con 100))),(PCons "x" "xs",Var "xs")]))
    , ("last", Abstr "xs" (LetRec "go" (Abstr "xs" (Case (Var "xs") [(PVal Empty,App (Var "error") (Value (Con 100))),(PCons "x" "xr",Case (Op Eq (Var "xr") (Value Empty)) [(PVal (Boolean True),Var "x"),(PVal (Boolean False),App (Var "go") (Var "xr"))])])) (App (Var "go") (Var "xs"))))
    , ("head", Abstr "xs" (Case (Var "xs") [(PVal Empty,App (Var "error") (Value (Con 100))),(PCons "x" "xs",Var "x")]))
    , ("subscript", Abstr "xs" (Abstr "k" (LetRec "go" (Abstr "xs" (Abstr "k" (Case (Op Eq (Var "k") (Value (Con 0))) [(PVal (Boolean True),App (Var "head") (Var "xs")),(PVal (Boolean False),App (App (Var "go") (App (Var "tail") (Var "xs"))) (Op Sub (Var "k") (Value (Con 1))))]))) (App (App (Var "go") (Var "xs")) (Var "k")))))
    , ("foldr", Abstr "f" (Abstr "acc" (Abstr "xs" (LetRec "go" (Abstr "f" (Abstr "acc" (Abstr "xs" (Case (Var "xs") [(PVal Empty,Var "acc"),(PCons "x" "xs",App (App (Var "f") (Var "x")) (App (App (App (Var "go") (Var "f")) (Var "acc")) (Var "xs")))])))) (App (App (App (Var "go") (Var "f")) (Var "acc")) (Var "xs"))))))
    , ("foldl", Abstr "f" (Abstr "acc" (Abstr "xs" (LetRec "go" (Abstr "f" (Abstr "acc" (Abstr "xs" (Case (Var "xs") [(PVal Empty,Var "acc"),(PCons "x" "xs",App (App (App (Var "go") (Var "f")) (App (App (Var "f") (Var "acc")) (Var "x"))) (Var "xs"))])))) (App (App (App (Var "go") (Var "f")) (Var "acc")) (Var "xs"))))))
    , ("foldl1", Abstr "f" (Abstr "xs" (App (App (App (Var "foldl") (Var "f")) (App (Var "head") (Var "xs"))) (App (Var "tail") (Var "xs")))))
    , ("foldr1", Abstr "f" (Abstr "xs" (App (App (App (Var "foldr") (Var "f")) (App (Var "last") (Var "xs"))) (App (Var "first") (Var "xs")))))
    , ("slice", Abstr "xs" (Abstr "begin" (Abstr "end" (Case (Op Lt (Var "begin") (Value (Con 1))) [(PVal (Boolean True),Value Empty),(PVal (Boolean False),Case (Op Gt (Var "begin") (App (Var "length") (Var "xs"))) [(PVal (Boolean True),Value Empty),(PVal (Boolean False),Case (Op Lt (Var "end") (Value (Con 1))) [(PVal (Boolean True),Value Empty),(PVal (Boolean False),Case (Op Or (Op Lt (Var "begin") (Var "end")) (Op Eq (Var "begin") (Var "end"))) [(PVal (Boolean True),App (App (Var "drop") (Op Sub (Var "begin") (Value (Con 1)))) (App (App (Var "take") (Var "end")) (Var "xs"))),(PVal (Boolean False),LetRec "beginP" (Op Add (Op Sub (App (Var "length") (Var "xs")) (Var "begin")) (Value (Con 1))) (LetRec "endP" (Op Add (Op Sub (App (Var "length") (Var "xs")) (Var "end")) (Value (Con 1))) (App (App (Var "drop") (Op Sub (Var "beginP") (Value (Con 1)))) (App (App (App (Var "take") (Var "endP")) (Var "reverse")) (Var "xs")))))])])])]))))
    , ("range", Abstr "begin" (Abstr "end" (LetRec "go" (Abstr "begin" (Abstr "end" (Case (Op Eq (Var "begin") (Var "end")) [(PVal (Boolean True),Cons (Var "begin") (Value Empty)),(PVal (Boolean False),Case (Op Lt (Var "begin") (Var "end")) [(PVal (Boolean True),Cons (Var "begin") (App (App (Var "go") (Op Add (Var "begin") (Value (Con 1)))) (Var "end"))),(PVal (Boolean False),Cons (Var "begin") (App (App (Var "go") (Op Sub (Var "begin") (Value (Con 1)))) (Var "end")))])]))) (App (App (Var "go") (Var "begin")) (Var "end")))))
    , ("id", Abstr "x" (Var "x"))
    , ("map", Abstr "f" (Abstr "xs" (LetRec "go" (Abstr "xs" (Case (Var "xs") [(PVal Empty,Value Empty),(PCons "x" "xr",Cons (App (Var "f") (Var "x")) (App (Var "go") (Var "xr")))])) (App (Var "go") (Var "xs")))))
    , ("zipWith", Abstr "f" (Abstr "xs" (Abstr "ys" (LetRec "go" (Abstr "xs" (Abstr "ys" (Case (Var "xs") [(PVal Empty,Value Empty),(PCons "x" "xr",Case (Var "ys") [(PVal Empty,Value Empty),(PCons "y" "yr",Cons (App (App (Var "f") (Var "x")) (Var "y")) (App (App (Var "go") (Var "xr")) (Var "yr")))])]))) (App (App (Var "go") (Var "xs")) (Var "ys"))))))
    , ("zipWith3", Abstr "f" (Abstr "xs" (Abstr "ys" (Abstr "zs" (LetRec "go" (Abstr "xs" (Abstr "ys" (Abstr "zs" (Case (Var "xs") [(PVal Empty,Value Empty),(PCons "x" "xr",Case (Var "ys") [(PVal Empty,Value Empty),(PCons "y" "yr",Case (Var "zs") [(PVal Empty,Value Empty),(PCons "z" "zr",Cons (App (App (App (Var "f") (Var "x")) (Var "y")) (Var "z")) (App (App (App (Var "go") (Var "xr")) (Var "yr")) (Var "zr")))])])])))) (App (App (App (Var "go") (Var "xs")) (Var "ys")) (Var "zs")))))))
    , ("zipWith4", Abstr "f" (Abstr "xs" (Abstr "ys" (Abstr "zs" (Abstr "ws" (LetRec "go" (Abstr "xs" (Abstr "ys" (Abstr "zs" (Abstr "ws" (Case (Var "xs") [(PVal Empty,Value Empty),(PCons "x" "xr",Case (Var "ys") [(PVal Empty,Value Empty),(PCons "y" "yr",Case (Var "zs") [(PVal Empty,Value Empty),(PCons "z" "zr",Case (Var "ws") [(PVal Empty,Value Empty),(PCons "w" "wr",Cons (App (App (App (App (Var "f") (Var "x")) (Var "y")) (Var "z")) (Var "w")) (App (App (App (App (Var "go") (Var "xr")) (Var "yr")) (Var "zr")) (Var "wr")))])])])]))))) (App (App (App (App (Var "go") (Var "xs")) (Var "ys")) (Var "zs")) (Var "ws"))))))))
    , ("zipWith5", Abstr "f" (Abstr "xs" (Abstr "ys" (Abstr "zs" (Abstr "ws" (Abstr "vs" (LetRec "go" (Abstr "xs" (Abstr "ys" (Abstr "zs" (Abstr "ws" (Abstr "vs" (Case (Var "xs") [(PVal Empty,Value Empty),(PCons "x" "xr",Case (Var "ys") [(PVal Empty,Value Empty),(PCons "y" "yr",Case (Var "zs") [(PVal Empty,Value Empty),(PCons "z" "zr",Case (Var "ws") [(PVal Empty,Value Empty),(PCons "w" "wr",Case (Var "vs") [(PVal Empty,Value Empty),(PCons "v" "vr",Cons (App (App (App (App (App (Var "f") (Var "x")) (Var "y")) (Var "z")) (Var "w")) (Var "v")) (App (App (App (App (App (Var "go") (Var "xr")) (Var "yr")) (Var "zr")) (Var "wr")) (Var "vr")))])])])])])))))) (App (App (App (App (App (Var "go") (Var "xs")) (Var "ys")) (Var "zs")) (Var "ws")) (Var "vs"))))))))) 
    ]