{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use all" #-}
module EllipLang.Translator where
--(translate, translate', isCore, tElliExpr) where

import EllipLang.Syntax
import EllipLang.Pretty (pp, makeElliAlias)
import EllipLang.Eval (idxToExpr)
import EllipLang.SmartCons ((<.>), inte, listToCons, unConsSafe, cons, x, y)
import EllipLang.MHSPrelude

import GenericHelper (everywhereUntil, mkMMMaybeT, gzipM, mkQQ, mkQQMaybe, gzipQ, hoistMaybe, mapWithIdx)

import Data.Generics 
import Control.Monad.State (State, evalState, runState, MonadState(put, get))
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad (mzero, guard)
import Control.Monad.Trans (lift)
import qualified Data.Map as Map
import Data.List
import Data.List.Extra (unsnoc, uncons)
import Data.Maybe (isJust, fromMaybe, maybe)
import qualified Data.Set (Set)
import Data.Bifunctor
import Data.Function ((&))

import Debug.Trace (trace, traceShowId)

data ElliClass = Fold Expr ElliClass
               | ZipWith Int


------------------------------------------------------------------------
-- 
-- Translation Elli-Haskell ==> Mini-Haskell
-- 
------------------------------------------------------------------------

-- | Translate Elli-Haskell into Mini-Haskell, check result.
translate :: Expr -> Expr
translate t = let translated = tExpr t
    in if not (isCore translated)
       then error $ "Translate did not produce core Mini-Haskell: " ++ pp translated
       else translated

-- | Translate Elli-Haskell into Mini-Haskell, check result, but do not error.
translate' :: Expr -> Expr
translate' t = let translated = tExpr t
    in if not (isCore translated) 
       then trace "WARNING: Translate did not produce core!" translated
       else translated

-- | Remove all ElliHaskell expressions.
tExpr :: Expr -> Expr
tExpr = everywhereUntil (False `mkQ` endTraversal) (mkT tExpr' )
  where
    tExpr' :: Expr -> Expr
    tExpr' (Ellipsis b e)      = tElliExpr b e
    tExpr' (ElliFoldr b e f)    = Var "foldr1" `App` (x <.> y <.> Op f x y) `App` tElliExpr b e
    tExpr' (ElliFoldl b e f)    = Var "foldl1" `App` (x <.> y <.> Op f x y) `App` tElliExpr b e
    tExpr' (ElliFoldr0 b e z f) 
        = Var "foldr" 
          `App` (x <.> y <.> Op f x y)
          `App` (tExpr z)
          `App` tElliExpr b e
    tExpr' (ElliFoldl0 b e z f) 
        = Var "foldl" 
          `App` (x <.> y <.> Op f x y)
          `App` (tExpr z)
          `App` tElliExpr b e
    tExpr' (ListElement n idx) = subscriptFun 
        `App` Var n 
        `App` (idx `sub` inte 1)
    tExpr' (Case target alts) =
        let alts' = map tAlt alts
        in Case target alts'
    tExpr' t = t

    endTraversal :: Expr -> Bool
    endTraversal (Ellipsis _ _) = True
    endTraversal (ElliFoldr {}) = True
    endTraversal (ElliFoldl {}) = True
    endTraversal (ElliFoldr0 {}) = True
    endTraversal (ElliFoldl0 {}) = True
    endTraversal _              = False

-- | Takes an elli-haskell alt and turns it into a non-elli-haskell alt.
tAlt :: (Pattern, Expr) -> (Pattern, Expr)
tAlt (PEllipsis listAlias len, t) = 
    let lenLet = case idxToName len of
            Just n  -> Let n (App lengthFun (Var listAlias))
            Nothing -> id
        t' = lenLet (tExpr t)
    in (PVar listAlias, t')
tAlt alt = alt

------------------------------------------------------------------------
--
-- Edit ellipsis expressions
--
------------------------------------------------------------------------

type ElliState = (Id, [ElliRange])

-- | Evaluate and unwrap tElli'.
tElliExpr :: Expr -> Expr -> Expr
tElliExpr begin end =
    case evalState (runMaybeT (tElliExpr' begin end)) (0, []) of
        Just r  -> r
        Nothing -> error "Could not transform ellipsis: Mismatched structures."

-- | Transform both sides of an ellipsis expression, like this:
-- (Example is for three lists)
-- f(x[ix], y[iy], z[iz]) ... f(x[kx], y[ky], z[kz])
-- zipWith3 (\_x _y _z -> f(_x, _y, _z)) (range x ix kx) (range y iy ky) (range z iz kz)
tElliExpr' :: Expr -> Expr -> MaybeT (State ElliState) Expr
tElliExpr' b e = do
    transformedTree <- tInterpolate b e
    tElliOuter transformedTree

tElliOuter :: Expr -> MaybeT (State ElliState) Expr
tElliOuter transformedTree = do
    (_, collectedRanges) <- lift get
    let mappedFn = foldr ((<.>) . Var . makeElliAlias) transformedTree collectedRanges
    let nZipWith = length collectedRanges
    let ranges = map makeRange collectedRanges
    return $ foldl1 App $
        [ toZipWithN nZipWith
        , mappedFn
        ] ++ ranges
  where
    makeRange :: ElliRange -> Expr
    makeRange ElliRange {ed_t=ElliList n, ed_ib=ib, ed_ie=ie} = Var "slice" `App` Var n `App` ib `App` ie
    makeRange ElliRange {ed_t=ElliCounter, ed_ib=ib, ed_ie=ie} = Var "range" `App` ib `App` ie
    makeRange ElliRange {ed_t=ElliExpr t} = t

tInterpolate :: Expr -> Expr -> MaybeT (State ElliState) Expr
tInterpolate b e = gzipM (mkMMMaybeT tInterpolateGo) b e

-- | Used generically to run over every expression
tInterpolateGo :: Expr -> Expr -> MaybeT (State ElliState) Expr
-- xi...xk ==> collect (id, x, i, k) and output _x<id> as a variable
tInterpolateGo (ListElement nl idxl) (ListElement nr idxr)
    | nl /= nr      = error "ListElements refer to different lists"
    | idxl == idxr  = mzero
    | otherwise = do
        (id, rs) <- lift get
        let er = ElliRange { ed_id=id
                           , ed_t=ElliList nl
                           , ed_ib=idxToExpr idxl
                           , ed_ie=idxToExpr idxr
                           }
        lift $ put (id+1, er:rs)
        return $ Var $ makeElliAlias er

-- (xi ... xk) ... (xj ... xn)
-- ==> (id, x, i k) ... (id2, x, j, n)
-- ==> (id, x, a, b) where a= i..j, b=k..n  
tInterpolateGo (Ellipsis ll lr) (Ellipsis rl rr) = do
    s@(id,rs) <- lift get
    -- These computations are contained and do not affect state, other than
    -- increasing the id for the purpose of disambiguation.
    let (l, (idl, _)) = runState (runMaybeT $ tElliExpr' ll lr) (id,[])
    let (r, (idr, _)) = runState (runMaybeT $ tElliExpr' rl rr) (id,[])
    l' <- hoistMaybe l
    r' <- hoistMaybe r
    guard $ trace (pp l' ++ "\n" ++ pp r') idl == idr  -- Just quickly check the state
    lift $ put (id+idl,rs)

    gzipM (mkMMMaybeT tInterpolateGo) l' r'

tInterpolateGo (ElliFoldr ll lr opl) (ElliFoldr rl rr opr)
    | opl /= opr = mzero
    | otherwise  = do
        interpolatedLists <- tInterpolateGo (Ellipsis ll lr) (Ellipsis rl rr)
        return $ Var "foldr1" `App` (x <.> y <.> Op opl x y) `App` interpolatedLists

tInterpolateGo (Op opl el1 el2) (ElliFoldr er1 er2 opr)
    | opl /= opr || el1 /= er1 = mzero
    | otherwise = tInterpolateGo (ElliFoldr el1 el2 opr) (ElliFoldr er1 er2 opr)

tInterpolateGo (el) (ElliFoldr er1 er2 opr)
    | el /= er1 = mzero -- fail
    | otherwise = tInterpolateGo (ElliFoldr el el opr) (ElliFoldr er1 er2 opr)

tInterpolateGo (ElliFoldl ll lr opl) (ElliFoldl rl rr opr)
    | opl /= opr = mzero
    | otherwise  = do
        interpolatedLists <- tInterpolateGo (Ellipsis ll lr) (Ellipsis rl rr)
        return $ Var "foldl1" `App` (x <.> y <.> Op opl x y) `App` interpolatedLists

tInterpolateGo (Op opl el1 el2) (ElliFoldl er1 er2 opr)
    | opl /= opr || el1 /= er1 = mzero
    | otherwise = tInterpolateGo (ElliFoldl el1 el2 opr) (ElliFoldl er1 er2 opr)

tInterpolateGo el (ElliFoldl er1 er2 opr)
    | el /= er1 = mzero -- fail
    | otherwise = tInterpolateGo (ElliFoldl el el opr) (ElliFoldl er1 er2 opr)

-- Just stop computation here, otherwise we can accidentally pollute state
tInterpolateGo (ER l) (ER r) | ed_id l == ed_id r = return $ ER l
                                   | otherwise          = mzero
-- Group up expressions, for example:
-- [k1 + 1] ... [k2 + 5] (which generates k1+1..k2+5)
-- is different from [k1] + [1] ... [k2] + [5], which generates (k1..k2 and 1..5)
tInterpolateGo (ElliGroup l) (ElliGroup r) = do
    (id, rs) <- lift get
    let er = ElliRange { ed_id=id
                       , ed_t=ElliCounter
                       , ed_ib=l
                       , ed_ie=r
                       }
    lift $ put (id+1, er:rs)
    return $ Var $ makeElliAlias er

-- [] ... [x1 ... xn]
tInterpolateGo (Value Empty) (Ellipsis rl rr) = do
    s@(id,rs) <- lift get
    let (r, (idr, _)) = runState (runMaybeT $ tElliExpr' rl rr) (id,[])
    r' <- hoistMaybe r
    let newRange = ElliRange { ed_t = ElliExpr $ Var "inits" `App` r'
                             , ed_ie = Value (Con 0)
                             , ed_ib = Value (Con 0)
                             , ed_id = id+idr+1
                             }
    lift $ put (id+idr+1, newRange:rs)
    return $ Var $ makeElliAlias newRange

-- [x1...xn] ... []
tInterpolateGo (Ellipsis ll lr) (Value Empty) = do
    s@(id,rs) <- lift get
    let (l, (idl, _)) = runState (runMaybeT $ tElliExpr' ll lr) (id,[])
    l' <- hoistMaybe l
    lift $ put (id+idl, rs)
    return $ Var "reverse" `App` (Var "inits" `App` l')

-- (el1:...:elk:[], [er1, ..., er2])
tInterpolateGo eli@(Cons el1 _) ell@(Ellipsis er1 er2) = do
    -- (er1, er2) ~> er_inner
    s@(id,rs) <- lift get
    let (er_innerM, sr@(idr, _)) = runInterpolate s er1 er2
    er_inner <- hoistMaybe er_innerM
    lift $ put (id+idr, rs)

    -- [er1, ..., er2] ~> er
    er <- hoistMaybe $ evalState (runMaybeT $ tElliOuter er_inner) sr

    -- ∀i ∈ {1, ..., k-1}, (eli, el{i+1}) ~> el'i; ssi
    eliList <- hoistMaybe $ unConsSafe eli
    let (eliM', statesl) = traceShowId $ unzip $ uncurry (runInterpolate s) <$> neighbors eliList
    eli' <- hoistMaybe $ sequence eliM'
    let (_, ssi) = unzip statesl

    if -- el1 == er1
        el1 == er1

        -- eli' = er_inner
        && all (== er_inner) eli'

        -- ∀s ∈ ssi, |s| = 1
        && Data.List.and (all (\ElliRange {ed_ib = ib, ed_ie = ie} -> exprConstant ib - exprConstant ie == 1) <$> ssi)
        
        then return $ Var "drop" `App` Value (Con $ length eliList) `App` (Var "inits" `App` er)
        else mzero
  where
    runInterpolate :: ElliState -> Expr -> Expr -> (Maybe Expr, ElliState)
    runInterpolate s l r = runState (runMaybeT $ tInterpolate l r) s


-- Catch-all: if you can take both sides and turn it into a simple counter,
-- then do that. Otherwise, keep iterating in.
tInterpolateGo l r 
    | not (isElliAble l && isElliAble r) = mzero -- Continue
    | l == r    = mzero
    | otherwise = do
        (id, rs) <- lift get
        let er = ElliRange {ed_id=id, ed_t=ElliCounter, ed_ib=l, ed_ie=r}
        lift $ put (id+1, er:rs)
        return $ Var $ makeElliAlias er
  where
    isElliAble :: Expr -> Bool
    isElliAble = isInt

------------------------------------------------------------------------
-- 
-- Core Property
-- 
------------------------------------------------------------------------

-- | isCore is only true if there are no Elli-Haskell extensions
isCore :: Expr -> Bool
isCore = everything (&&) (True `mkQ` isCore')
  where
    isCore' :: Expr -> Bool
    isCore' (Ellipsis _ _)  = False
    isCore' (ElliFoldr {} ) = False
    isCore' (ElliFoldl {} ) = False
    isCore' (ElliComp _ _)  = False
    isCore' (EllipVar _)    = False
    isCore' (ElliGroup _)   = False
    isCore' (PreElli)       = False
    isCore' _               = True

------------------------------------------------------------------------
-- 
-- Other functions
-- 
------------------------------------------------------------------------

idxToName :: Idx -> Maybe Name
idxToName i = case idxToExpr i of
    Var n   -> Just n
    _       -> Nothing

neighbors :: [a] -> [(a, a)]
neighbors xs = zip xs (drop 1 xs)

isInt :: Expr -> Bool
isInt = isJust . exprCalc

exprConstant :: Expr -> Int
exprConstant t = 
    case exprCalc t of
        Just terms -> maybe 0 fst $ find (null . snd) terms
        Nothing -> 0

exprCalc :: Expr -> Maybe [(Int, [String])]
exprCalc t = do
    terms  <- go t
    let terms1 = second sort <$> terms
    let terms2 = sortOn snd terms1
    let terms3 = unzip <$> groupBy (\(_, x) (_, y) -> x == y) terms2
    return $ bimap sum head <$> terms3
  where
    go :: Expr -> Maybe [(Int, [String])]
    go (Value (Con x)) = return [(x, [])]
    go (Var s)         = return [(1, [s])]
    go (Op Add x y) = do
        x' <- go x
        y' <- go y
        return $ x' ++ y'
    go (Op Sub x y) = do
        x' <- go x
        y' <- go y
        return $ x' ++ (first negate <$> y')
    go (Op Mul x y) = do
        x' <- go x
        y' <- go y
        return [(xI * yI, xS ++ yS) | (xI, xS) <- x', (yI, yS) <- y']
    go _ = Nothing