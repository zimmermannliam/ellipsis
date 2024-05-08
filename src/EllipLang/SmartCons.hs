module EllipLang.SmartCons where

import Data.List (unfoldr)
import Data.Function ((&))

import EllipLang.Syntax

[f,x,n,l,r,m,k,t,y,z,k',xs,ys,zs,list,term,g,a,b,list1,list2,l1,l2,begin,end] = map Var ["f","x","n","l","r","m","k","t","y","z","k'","xs","ys","zs","list","term","g","a","b","list1","list2","l1","l2","begin","end"]
[x0,x1,x2] = map EllipVar [0,1,2]

true = Value $ Boolean True
false = Value $ Boolean False

con :: Int -> Expr
con i = Value $ Con i

icons :: [Int] -> Expr
icons l = foldr1 Cons (map con l ++ [Value Empty])

cons :: [Expr] -> Expr
cons = foldr Cons (Value Empty)

vcons :: [Int] -> Val
vcons = foldr (VCons . Con) Empty

ellipOne :: Expr -> Idx -> Idx -> Name -> Expr
ellipOne t ib ie n = ElliComp t [EllipRange {var=n, ident=0, ib=ib, ie=ie, contentT = BeList}]

-- y combinator
-- \f.( (\x.(f (x x))) (\x.(f (x x))) )
ycomb :: Expr
ycomb = Abstr "f" $
            App (Abstr "x" $ App (Var "f" ) (App (Var "x") (Var "x")))
                (Abstr "x" $ App (Var "f" ) (App (Var "x") (Var "x")))

listToVCons :: [Val] -> Val
listToVCons  = foldr VCons Empty

listToCons :: [Expr] -> Expr
listToCons = foldr Cons (Value Empty)

vPairVCons :: [(Int, Int)] -> Val
vPairVCons xs = 
    xs
        & map (\(l,r) -> VPair (Con l) (Con r))
        & foldr VCons Empty

unConsSafe :: Expr -> Maybe [Expr]
unConsSafe (Value Empty) = Just []
unConsSafe (Cons x xs)   = fmap (x:) (unConsSafe xs)
unConsSafe _             = Nothing


unCons :: Expr -> [Expr]
unCons = unfoldr unCons'
    where
    unCons' :: Expr -> Maybe (Expr, Expr)
    unCons' (Value Empty)   = Nothing
    unCons' (Cons t1 t2)    = Just (t1, t2)
    unCons' t               = error $ "Unexpected unCons': " ++ show t

unVCons :: Val -> [Val]
unVCons = unfoldr unVCons'
    where
    unVCons' :: Val -> Maybe (Val, Val)
    unVCons' Empty          = Nothing
    unVCons' (VCons v1 v2)  = Just (v1, v2)
    unVCons' v              = error $ "Unexpected unVCons': " ++ show v

(!.) :: Expr -> Int -> Expr
(Var n) !. i = ListElement n $ Value $ Con i
t !. i      = error $ "Bad !.: " ++ show t ++ "!." ++ show i
infixl 9 !.

(!) :: Expr -> Expr -> Expr
(Var n) ! t = ListElement n (t)
infixl 9 !

(<+>) :: Expr -> Expr -> Expr
t1 <+> t2 = t1 `Pair` t2
infix 8 <+>

(<...>) :: Expr -> Expr -> Expr
t1 <...> t2 = t1 `Ellipsis` t2
infix 3 <...>

inte :: Int -> Expr
inte i = Value $ Con i

(<.>) :: Expr -> Expr -> Expr
(Var s) <.> t = Abstr s t
infixr 2 <.>

case1 :: Expr -> (Pattern, Expr) -> Expr
case1 to pt = Case to [pt]

(==>) :: Pattern -> Expr -> (Pattern, Expr)
p ==> t = (p, t)
infixr 0 ==>

(<..>) :: (Expr, a) -> (Expr, Expr) -> Pattern
(Var v1, _) <..> (Var v2, Var n)    | v1 /= v2  = error "bad smart constructor"
                                    | otherwise = PEllipsis v1 (Var n)

if' :: Expr -> Expr -> String -> Expr -> Expr
if' pred termTrue _ termFalse = Case pred [(PVal $ Boolean True, termTrue), (PVal $ Boolean False, termFalse)]

else' = "else"