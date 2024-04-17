{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module EllipLang.Eval where

import Debug.Trace
import Data.Maybe
import Data.Generics
import Data.Data
import Data.Either
import Data.List
import qualified Data.Map as Map
import qualified Data.Bifunctor
import Control.Monad
import Control.Monad.State
import Data.Function ((&))

import EllipLang.Syntax
import EllipLang.Pretty (pp, ppVal, ppEnv)
import EllipLang.SmartCons


------------------------------------------------------------------------
-- Semantics
------------------------------------------------------------------------

-- E |- t => v
eval :: Env -> Expr -> Val

-- Lambda Calculus
eval e (Var vn) = evalBinding e (envLookup e vn)
eval e (App t1 t2) = case eval e t1 of
    Closure x t1b e2 -> eval (Map.insert (NamedVar x) (BVal $ eval e t2) e2) t1b
    _           -> errorOut e "Expected fn to be applied"
eval e (Abstr x t) = Closure x t e

eval e (Value v)            = v

-- Basic extensions of LC
eval e (Case tc ps) = patternMatchEval e tc ps
eval e (Let n t1 t2) = eval (Map.insert (NamedVar n) (BVal $ eval e t1) e) t2
eval e (LetRec n t1 t2) = case eval e t1 of
    Closure {}  -> eval (Map.insert (NamedVar n) (BVal $ eval e $ App ycomb (Abstr n t1)) e) t2
    _           -> errorOut e "Expected fn to be letrecced"

-- Ellipsis/list operators
eval e (ListElement n i) = findListFutureElement e (envLookup e n) i
eval e (EllipVar i)   = evalBinding e (e Map.! IdVar i)
eval e ell@(PreEllipsis _ _) = eval e $ processPreEllipsis e ell
eval e (Index idx)    = eval e $ idxToExpr idx
eval e (Ellipsis t rs) = let
    ellipEnv = getEllipsisIterators e rs
    in
    if not (rangesCheck ellipEnv)
        then error $ "Unequal ranges in ellipEnv: " ++ show ellipEnv
    else if not (boundsCheck e ellipEnv)
        then Empty
    else iterateEllipsis e ellipEnv t

-- Built in type constructors
eval e (Cons t1 t2) = VCons (eval e t1) (eval e t2)
eval e (Pair t1 t2) = VPair (eval e t1) (eval e t2)

-- Remove me
eval e (Cat t1 t2)    =
    let et1 = eval e t1
        et2 = eval e t2
    in case (et1, et2) of
        (VCons _ _, VCons _ _) -> catVCons et1 et2
        (VCons _ _, Empty)     -> et1
        (Empty, VCons _ _)     -> et2
        (Empty, Empty)         -> Empty
        (v1, v2)               -> errorOut e $ "Tried to cat non-lists: " ++ show v1 ++ " ++ " ++ show v2

-- Debug operators
eval e (Error s) = errorOut e s
eval e (Trace s tt t) = trace (s ++ ": " ++ ppVal (eval e tt)) $ eval e t

-- Relational operators
eval e (Eq t1 t2) = Boolean (eval e t1 == eval e t2)
eval e (Neq t1 t2) = Boolean (eval e t1 /= eval e t2)
eval e (Lt t1 t2) = case (eval e t1, eval e t2) of
    (Con x1, Con x2)    -> Boolean $ x1 < x2
    (v1, v2)            -> errorOut e $ "Tried to Lt two non-integers: " ++ show v1 ++ " < " ++ show v2
eval e (Gt t1 t2) = case (eval e t1, eval e t2) of
    (Con x1, Con x2)    -> Boolean $ x1 > x2
    (v1, v2)            -> errorOut e $ "Tried to Gt two non-integers: " ++ show v1 ++ " > " ++ show v2
eval e (Leq t1 t2) = case (eval e t1, eval e t2) of
    (Con x1, Con x2)    -> Boolean $ x1 <= x2
    (v1, v2)            -> errorOut e $ "Tried to Leq two non-integers: " ++ show v1 ++ " <= " ++ show v2
eval e (Geq t1 t2) = case (eval e t1, eval e t2) of
    (Con x1, Con x2)    -> Boolean $ x1 >= x2
    (v1, v2)            -> errorOut e $ "Tried to Geq two non-integers: " ++ show v1 ++ " >= " ++ show v2
eval e (Or t1 t2) = case (eval e t1, eval e t2) of
    (Boolean b1, Boolean b2)    -> Boolean $ b1 || b2
    (v1, v2)                    -> errorOut e $ "Tried to OR non-booleans: " ++ show v1 ++ " || " ++ show v2
eval e (And t1 t2) = case (eval e t1, eval e t2) of
    (Boolean b1, Boolean b2)    -> Boolean $ b1 && b2
    (v1, v2)                    -> errorOut e $ "Tried to AND non-booleans: " ++ show v1 ++ " && " ++ show v2
eval e (Not t) = case eval e t of
    Boolean b   -> Boolean $ not b
    v           -> errorOut e $ "Tried to NOT a non-boolean: " ++ show v

-- Arithmetic operators
eval e (Add t1 t2) = case (eval e t1, eval e t2) of
    (Con i1, Con i2)    -> Con (i1 + i2)
    (v1, v2)            -> errorOut' e $ "Bad add terms: " ++ show v1 ++ " + " ++ show v2
eval e (Sub t1 t2) = case (eval e t1, eval e t2) of
    (Con i1, Con i2)    -> Con (i1 - i2)
    (v1, v2)            -> errorOut' e $ "Bad sub terms: " ++ show v1 ++ " - " ++ show v2
eval e (Mul t1 t2) = case (eval e t1, eval e t2) of
    (Con i1, Con i2)    -> Con (i1 * i2)
    _                   -> errorOut' e "Bad mul terms"
eval e (Div t1 t2) = case (eval e t1, eval e t2) of
    (Con i1, Con i2)    -> Con (i1 `div` i2)
    _                   -> errorOut' e "Bad div terms"
eval e (Mod t1 t2) = case (eval e t1, eval e t2) of
    (Con i1, Con i2)    -> Con (i1 `mod` i2)
    _                   -> errorOut' e "Bad mod terms"
eval e (Abs t) = case eval e t of
    (Con i)             -> Con (abs i)
    _                   -> errorOut' e "Bad abs term"

traceWith :: (a -> String) -> a -> a
traceWith f t = t
-- traceWith f t = trace (f t) t

processPreEllipsis :: Env -> Expr -> Expr
processPreEllipsis e (PreEllipsis t1 t2) = traceWith (pp) $ Ellipsis t rs
    where
    (pre_rs, pre_t) = extractStates e
    (t,(rs',_)) = runState (processPreEllipsis' t1 t2) (pre_rs, pre_t)
    rs = rs' \\ pre_rs
processPreEllipsis _ _ = error "Trying to processPreEllipsis on non-preEllipsis"

extractStates :: Env -> (EllipRanges, Int)
extractStates e = let 
    isIterator :: Bindee -> Bool
    isIterator (BIterator {}) = True
    isIterator _              = False
    rs = e  & Map.filter isIterator
            & Map.toList 
            & map (\(IdVar identifier, BIterator {it_ie=it_ie, it_ic=it_ic, vname=vname, content=content}) 
                -> EllipRange {
                    ident=identifier, 
                    var=vname, 
                    ib=IPlace it_ic, 
                    ie=IPlace it_ie, 
                    contentT=if content == Indices then BeIndices else BeList
                })
    nextI = 1 + foldr (max . ident) (-1) rs
    in (rs,nextI)

mergeEllipVars :: Id -> Id -> State (EllipRanges, Int) Expr
mergeEllipVars ib' ie' = return (EllipVar ib')
    {-
    (rs, _) <- get
    let rb = rangesLookup rs ib'
    let re = rangesLookup rs ie'
    if rb `rangesEq` re then return (EllipVar ib')
    else error "Bad ellip vars!"
    where
        rangesEq :: EllipRange -> EllipRange -> Bool
        rangesEq EllipRange {var=varl, ib=ibl, ie=iel} EllipRange {var=varr, ib=ibr, ie=ier} =
            varl==varr && ibl==ibr && iel==ier
            -}

processPreEllipsis' :: Expr -> Expr -> State (EllipRanges, Int) Expr
processPreEllipsis' t1@(ListElement vb idxb) t2@(ListElement ve idxe)
    | vb /= ve      = error $ "PreEllipsis: Unequal lists: "++pp t1++" ... "++pp t2
    | idxb == idxe  = return t1
    | otherwise     = do
        (rs, id) <- get
        let newRange = EllipRange { ident=id, var=ve, ib=idxb, ie=idxe, contentT=BeList}
        put (newRange:rs, id+1)
        return $ EllipVar id

processPreEllipsis' t1@(Index idxb) t2@(Index idxe)
    | idxb == idxe  = return t1
    | otherwise     = do
        (rs, id) <- get
        let newRange = EllipRange {ident=id, var="", ib=idxb, ie=idxe, contentT=BeIndices}
        put (newRange:rs, id+1)
        return $ EllipVar id

processPreEllipsis' (PreEllipsis t1b t2b) (PreEllipsis t1e t2e) 
    = doTwo PreEllipsis (t1b, t2b) (t1e, t2e)

processPreEllipsis' (App t1b t2b) (App t1e t2e)     = doTwo App (t1b, t2b) (t1e, t2e)
processPreEllipsis' (Abstr vb tb) (Abstr ve te)     = if vb == ve
    then doOne (Abstr vb) tb te
    else error $ "PreEllipsis: Unequal arguments: "++vb++ " ... "++ve
processPreEllipsis' (Let vb tib tob) (Let ve tie toe) = if vb == ve
    then doTwo (Let vb) (tib, tob) (tie, toe)
    else error $ "PreEllipsis: Unequal let vars: "++vb++" ... "++ve
processPreEllipsis' (Case _ _) (Case _ _) = error "PreEllipsis: Case not implemented"
processPreEllipsis' (LetRec {}) (LetRec {}) = error "PreEllipsis: LetRec not implemented"
processPreEllipsis' (Ellipsis tb rsb) (Ellipsis te rse) =
    trace "Warning: PreEllipsis: Nested ellipsis are currently awkward" $
    if rsb == rse
        then doOne (`Ellipsis` rsb) tb te
        else error "PreEllipsis: Not equal ranges (This could be changed in the future)"
processPreEllipsis' (Cons t1b t2b) (Cons t1e t2e)   = doTwo Cons (t1b, t2b) (t1e,t2e)
processPreEllipsis' (Cat t1b t2b) (Cat t1e t2e)     = doTwo Cat (t1b, t2b) (t1e,t2e)
processPreEllipsis' (Error s1) (Error s2)             = return $ Error s1
processPreEllipsis' (Add t1b t2b) (Add t1e t2e)     = doTwo Add (t1b, t2b) (t1e, t2e)
processPreEllipsis' (Pair t1b t2b) (Pair t1e t2e)   = doTwo Pair (t1b, t2b) (t1e, t2e)
processPreEllipsis' (EllipVar ib') (EllipVar ie')     = mergeEllipVars ib' ie'
processPreEllipsis' (EllipVar idb') (ListElement ve idxe) = do
    (rs, id) <- get
    let rb = rangesLookup rs idb'
    let newRange = EllipRange {ident=id, var=ve, ib=(ib rb), ie=idxe, contentT=BeList}
    put (newRange:rs, id+1)
    if var rb /= ve
        then error "EllipVar ... ListElement with different lists" 
        else return $ EllipVar id

processPreEllipsis' (ListElement vb idxb) (EllipVar ide')  = do
    (rs, id) <- get
    let re = rangesLookup rs ide'
    let newRange = EllipRange {ident=id, var=vb, ib=idxb, ie=ib re, contentT=BeList}
    put (newRange:rs, id+1)
    if var re /= vb
        then error "ListElement ... EllipVar with different lists" 
        else return $ EllipVar id

processPreEllipsis' t1 t2 = if t1 == t2
    then return t1
    else error $ "Not implemented: "++show t1++" ... "++show t2

doTwo :: (Expr -> Expr -> Expr) -> (Expr, Expr) -> (Expr,Expr) -> State (EllipRanges, Int) Expr
doTwo con (t1b, t2b) (t1e, t2e) = do
    t1eval <- processPreEllipsis' t1b t1e
    t2eval <- processPreEllipsis' t2b t2e
    return $ con t1eval t2eval

doOne :: (Expr -> Expr) -> Expr -> Expr -> State (EllipRanges, Int) Expr
doOne con tb te = do
    t <- processPreEllipsis' tb te
    return $ con t

getEllipsisIterators :: Env -> EllipRanges -> Env
getEllipsisIterators e rs = Map.fromList $ map (IdVar . ident) rs `zip` map (getEllipsisIterator e) rs
    where
    getEllipsisIterator :: Env -> EllipRange -> Bindee
    getEllipsisIterator e EllipRange {ib=ib, ie=ie, var=var, contentT=contentT} = BIterator {
        it_ib = ib_int,
        it_ie = ie_int,
        it_ic = ib_int,
        vname = var,
        content = case contentT of
            BeList      -> List $ drop (ib_int - 1) $ unVCons (evalBinding e (ListFuture var))
            BeIndices   -> Indices
        }
            where
            ib_int = idxToInt e ib
            ie_int = idxToInt e ie


iterateEllipsis :: Env -> Env -> Expr -> Val
iterateEllipsis e ellipEnv t = foldr VCons Empty $ iterateEllipsis' e ellipEnv t range
    where
    range = if ellipEnv == Map.empty 
        then 1
        else (\it -> 1 + it_ie it - it_ib it) $ head $ Map.elems ellipEnv
    iterateEllipsis' :: Env -> Env -> Expr -> Int -> [Val]
    iterateEllipsis' _ _ _ 0 = []
    iterateEllipsis' e ellipEnv t cd = 
        eval (e `Map.union` ellipEnv) t:iterateEllipsis' e (advanceIts ellipEnv) t (cd-1)

advanceIts :: Env -> Env
advanceIts = Map.map advanceIt
    where
    advanceIt :: Bindee -> Bindee
    advanceIt b@(BIterator {it_ic=ic, content=c}) = b{it_ic=ic+1,content = case c of
        List (x:xs) -> List xs
        List []     -> error "advanceIt ran out too soon"
        itc         -> itc}
    advanceIt b = b

rangesCheck :: Env -> Bool
rangesCheck ellipEnv = all (== head ranges) ranges
    where ranges = map (\it -> it_ie it - it_ib it) (Map.elems ellipEnv)

rangesLookup :: EllipRanges -> Id ->  EllipRange
rangesLookup rs i = fromMaybe
    (error $ "Could not find i: " ++ show i ++ " in ranges: " ++ show rs)
    (find (\r -> i == ident r) rs)

boundsCheck :: Env -> Env -> Bool
boundsCheck e ellipEnv = all (boundsCheck' e) iterators
    where
    iterators = Map.elems ellipEnv
    boundsCheck' :: Env -> Bindee -> Bool
    boundsCheck' e BIterator { it_ie=it_ie, it_ib=it_ib, content=content} = it_ie >= it_ib
        && it_ib > 0
        && (case content of
            List l -> it_ie - it_ib <= length l
            _ -> True)
    boundsCheck' e _ = errorOut' e "Tried to boundscheck a non-iterator"

reverseCons :: Val -> Val
reverseCons l = listToVCons $ reverseCons' l
    where
    reverseCons' :: Val -> [Val]
    reverseCons' (VCons x xs)    = reverseCons' xs ++ [x]
    reverseCons' Empty  = []
    reverseCons' v      = error $ "Bad value: " ++ show v

startingAtNthCons :: Val -> Int -> Val
startingAtNthCons l 1            = l
startingAtNthCons (VCons x xs) i  = startingAtNthCons xs (i - 1)
startingAtNthCons Empty _        = Empty
startingAtNthCons l i            = error $ "startingAtNthCons unexpected values: l=" ++ show l ++ "i = " ++ show i

iterateCons :: Env -> Val -> Expr -> String -> Int -> Val
iterateCons _ _ _ _ 0     = Empty
iterateCons e Empty _ _ i = errorOut e $ "Empty too soon; i: " ++ show i
iterateCons e (VCons x xs) t n i   = VCons (eval e $ replaceVar n x t) (iterateCons e xs t n (i-1))
iterateCons e _ _ _ _     = errorOut' e "Non-cons somehow in iterator"

replaceVar :: String -> Val -> Expr -> Expr
replaceVar n valin = everywhere (mkT $ replaceVar' n valin)

replaceVar' :: String -> Val -> Expr -> Expr
replaceVar' n valin (Var n')  = if n == n' then Value valin else Var n'
replaceVar' _ _ t           = t

idxToInt :: Env -> Idx -> Int
idxToInt e (IPlace i)   = i
idxToInt e (End n)      = valEvalInt $ eval e (Var n)
idxToInt e (EPlace t)   = valEvalInt $ eval e t

idxToExpr :: Idx -> Expr
idxToExpr (EPlace t) = t
idxToExpr (IPlace i) = Value $ Con i
idxToExpr (End n)    = Var n

findListFutureElement :: Env -> Bindee -> Idx -> Val
findListFutureElement e (ListFuture n) i = findNthElement
        (evalBinding e (envLookup e n) )
        intIdx
    where   findNthElement :: Val -> Int -> Val
            findNthElement (VCons x xs) i
                | i == 1    = x
                | i > 1     = findNthElement xs (i - 1)
                -- | i == 0    = Empty
                | otherwise = error $ "ListElement: bad i: " ++ show i
            findNthElement Empty i          = error $ "ListElement: not long enough! i: " ++ show i
            findNthElement x _              = error ("ListElement: non-list element detected:" ++ show x)
            intIdx = case i of
                EPlace t    ->  valEvalInt $ eval e t
                IPlace i    ->  i
                End n       ->  valEvalInt $ eval e (Var n)

findListFutureElement e _ _ = errorOut' e "Bad list future element search"


errorOut :: Env -> String -> Val
errorOut e s = error (s ++ "; Environment: " ++ ppEnv e)


errorOut' :: Env -> String -> a
errorOut' e s = error (s ++ "; Environment: " ++ ppEnv e)


catVCons :: Val -> Val -> Val
catVCons (VCons x xs) ys    = VCons x (catVCons xs ys)
catVCons Empty ys           = ys
catVCons _ _                = error "Tried to catVCons non-lists"


valEvalInt :: Val -> Int
valEvalInt (Con t)  = t
valEvalInt t        = error $ "Expected integer but got: " ++ show t

-- Match all alternatives vs Expr
patternMatchEval :: Env -> Expr -> Alts -> Val
patternMatchEval e t (p:ps) = case patternMatch e t p of
                                Nothing     -> patternMatchEval e t ps
                                Just v      -> v
patternMatchEval e t []     = error "Ran out of patterns"

-- Match a single possibility vs Expr
patternMatch :: Env -> Expr -> (Pattern, Expr) -> Maybe Val
patternMatch e t (PVal k, t2)             = if eval e t == k then Just $ eval e t2
                                                else Nothing
patternMatch e t (PCons n ns, t2)         = case eval e t of
                                                VCons x xs  -> Just $ eval (Map.fromList [(NamedVar n, BVal x),(NamedVar ns,BVal xs)] `Map.union` e) t2
                                                _           -> Nothing
patternMatch e t (PVar n, t2)             = Just $ eval (Map.insert (NamedVar n) (BVal $ eval e t) e) t2
patternMatch e t (PEllipsis n i, t2)      = let
    list = eval e t
    n_idx = case i of
        (End n) -> n
        _       -> error "Tried to unpack a bad ending idx for ellipsis pattern"
    n_t = case eval Map.empty t of
        (FreeVar n) -> n
        _       -> error "Tried to unpack a bad name-term for ellipsis pattern"
    in if valIsList list
        then Just $ eval (Map.fromList [(NamedVar n, ListFuture n_t), (NamedVar n_idx, LenFuture n_t)] `Map.union` e) t2
        else Nothing
patternMatch e t (PCons' tl tls, t2) =
    let vl = eval e tl
        vls = eval e tls
        v = eval e t
    in case (vl, vls, v) of
        (FreeVar nl, FreeVar nls, VCons x xs) -> Just $ eval (Map.fromList [(NamedVar nl, BVal x),(NamedVar nls, BVal xs)] `Map.union` e) t2
        (FreeVar nl, vls',        VCons x xs) -> if vls' == xs then Just $ eval (Map.insert (NamedVar nl) (BVal x) e) t2 else Nothing
        (vl',        FreeVar nls, VCons x xs) -> if vl' == x then Just $ eval (Map.insert (NamedVar nls) (BVal xs) e) t2 else Nothing
        (vl',        vls',        VCons x xs) -> if vl' == x && vls' == xs then Just $ eval e t2 else Nothing
        (_,          _,           _         ) -> Nothing

-- patternMatch e t _                        = Nothing

valIsList :: Val -> Bool
valIsList Empty         = True
valIsList (VCons _ xs)  = valIsList xs
valIsList v             = traceShow v False

-- Find a variable in environment
envLookup :: Env -> Name -> Bindee
envLookup e n = Map.findWithDefault (BVal $ FreeVar n) (NamedVar n) e

getLen :: Env -> Val -> Int
getLen _ = getLen'
    where
    getLen' :: Val -> Int
    getLen' (VCons x xs) = 1 + getLen' xs
    getLen' Empty        = 0
    getLen' term         = error ("getLen encountered bad var: " ++ show term)

evalBinding :: Env -> Bindee -> Val
evalBinding e (BVal v)      = v
evalBinding e (LenFuture n) = Con $ getLen e boundList
    where   boundList :: Val
            boundList = case envLookup e n of
                BVal v  -> v
                _       -> error "Tried to bind list without value"
evalBinding e (ListFuture n) = evalBinding e (envLookup e n)
evalBinding e (BIterator {it_ic=ic, content=c}) = case c of
    List (x:xs) -> x
    List []     -> error $ "Empty list in bound iterator" ++ show e
    Indices     -> Con ic