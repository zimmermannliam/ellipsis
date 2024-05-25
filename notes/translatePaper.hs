{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use all" #-}
module EllipLang.Translator (translate, translate', isCore, tElliExpr) where

import EllipLang.Syntax
import EllipLang.Pretty (pp, makeElliAlias)
import EllipLang.Eval (idxToExpr)
import EllipLang.SmartCons ((<.>), inte, listToCons, unConsSafe, cons)

import GenericHelper (everywhereUntil, mkMMMaybeT, gzipM, mkQQ, mkQQMaybe, gzipQ, hoistMaybe, mapWithIdx)

import Data.Generics 
import Control.Monad.State (State, evalState, runState, MonadState(put, get))
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad (mzero, guard)
import Control.Monad.Trans (lift)
import qualified Data.Map as Map
import Data.List (transpose)
import Data.List.Extra (unsnoc, uncons)
import Data.Maybe (isJust)

import Debug.Trace (trace, traceShowId)

data ElliClass = Fold Expr ElliClass
               | ZipWith Int

toZipWithN :: Int -> Expr
toZipWithN i = map Var ["id", "map", "zipWith", "zipWith3", "zipWith4", "zipWith5"] !! i

lengthFun = Var "length"
subscriptFun = Var "(!!)"

------------------------------------------------------------------------
-- 
-- Translation Elli-Haskell ==> Mini-Haskell
-- 
------------------------------------------------------------------------

-- | Translate Elli-Haskell into Mini-Haskell, check result.
translate :: Expr -> Expr
translate t = let translated = tExpr t
    in if not (isCore translated)
       then error "Translate did not produce core Mini-Haskell"
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
    tExpr' (ElliFoldr b e f)    = Var "foldr1" `App` f `App` tElliExpr b e
    tExpr' (ListElement n idx) = subscriptFun 
        `App` Var n 
        `App` (idx `Sub` inte 1)
    tExpr' (Case target alts) =
        let alts' = map tAlt alts
        in Case target alts'
    tExpr' t = t

    endTraversal :: Expr -> Bool
    endTraversal (Ellipsis _ _) = True
    endTraversal (ElliFoldr {}) = True
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
    transformedTree <- gzipM (mkMMMaybeT tInterpolateGo) b e
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
    makeRange ElliRange {ed_t=ElliList n, ed_ib=ib, ed_ie=ie} = Var "listRange" `App` Var n `App` ib `App` ie
    makeRange ElliRange {ed_t=ElliCounter, ed_ib=ib, ed_ie=ie} = Var "range" `App` ib `App` ie

tInterpolate :: Expr -> Expr -> MaybeT (State ElliState) Expr
tInterpolate b e = gzipM (mkMMMaybeT tInterpolateGo) b e

-- | Used generically to run over every expression
tInterpolateGo :: Expr -> Expr -> MaybeT (State ElliState) Expr
-- xi...xk ==> collect (id, x, i, k) and output _x<id> as a variable
tInterpolateGo (ListElement nl idxl) (ListElement nr idxr)
    | nl /= nr      = error "ListElements refer to different lists"
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
    let (l, (idl, _)) = runState (runMaybeT $ tElliExpr' ll lr) s
    let (r, (idr, _)) = runState (runMaybeT $ tElliExpr' rl rr) s
    l' <- hoistMaybe l
    r' <- hoistMaybe r
    guard $ idl == idr  -- Just quickly check the state
    lift $ put (id+idl,rs)
    gzipM (mkMMMaybeT tInterpolateGo) l' r'

tInterpolateGo (ElliFoldr ll lr opl) (ElliFoldr rl rr opr)
    | opl /= opr = mzero
    | otherwise  = do
        s@(id,rs) <- lift get
        -- These computations are contained and do not affect state, other than
        -- increasing the id for the purpose of disambiguation.
        let (l, (idl, _)) = runState (runMaybeT $ tElliExpr' ll lr) s
        let (r, (idr, _)) = runState (runMaybeT $ tElliExpr' rl rr) s
        l' <- hoistMaybe l
        r' <- hoistMaybe r
        guard $ idl == idr  -- Just quickly check the state
        lift $ put (id+idl,rs)
        res <- gzipM (mkMMMaybeT tInterpolateGo) l' r'
        return $ Var "foldr1" `App` opl `App` res
        

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
    let (r, (idr, nestedrs)) = runState (runMaybeT $ tInterpolate rl rr) s
    r' <- hoistMaybe r
    let er = ElliRange { ed_id=id+idr
                        , ed_t=ElliCounter
                        , ed_ib=inte 0
                        , ed_ie=Var "foldr1" `App` Var "max" `App` listToCons (map ed_ie nestedrs)
                        }
    let nestedrs' = map (\range -> range {ed_ie=Var $ makeElliAlias er}) nestedrs
    lift $ put (id+idr+1, er:rs)
    case evalState (runMaybeT $ tElliOuter r') (idr, nestedrs') of
        (Just res) -> return res
        Nothing     -> mzero

-- [x1...xn] ... []
tInterpolateGo (Ellipsis ll lr) (Value Empty) = do
    s@(id,rs) <- lift get
    let (l, (idl, nestedrs)) = runState (runMaybeT $ tInterpolate ll lr) s
    case l of
        (Just l') -> do
            let er = ElliRange { ed_id=id+idl
                               , ed_t=ElliCounter
                               , ed_ib=Var "foldr1" `App` Var "max" `App` cons (map ed_ie nestedrs)
                               , ed_ie=inte 0
                               }
            let nestedrs' = map (\range -> range {ed_ie=Var $ makeElliAlias er}) nestedrs
            lift $ put (id+idl+1, er:rs)
            case evalState (runMaybeT $ tElliOuter l') (idl, nestedrs') of
                (Just res) -> return res
                Nothing     -> mzero
        Nothing -> mzero

-- [x1] ... [x1...xn]       ==> [x1], [x1, x2], [x1,x2,x3], ...
-- [x1, x2] ... [x1...xn]   ==> [x1,x2], [x1,x2,x3], ...
-- and so on
tInterpolateGo xs@(Cons _ _) (Ellipsis rl rr) = do
    {-
    Using this example:
    zipInits (x1...xn) (y1...ym) =
        [[(x1, y1)], [(x1, y1), (x2,y2)], ..., [(x1,y1)...(xn,ym)]]
    before here, unsugar:
        [[(x1, y1)]] ++ [[(x1, y1), (x2,y2)], ..., [(x1,y1)...(xn,ym)]]
    -}
    -- Transform the Ellipsis on the right side.
    -- [(x1, y1) ... (xn, yn)] ==> (_x1, _y0) + (rangeX, rangeY)
    s@(id,rngs) <- lift get
    let (r, (idr, nestedrngs)) = runState (runMaybeT $ tInterpolate rl rr) s
    lift $ put (id+idr, rngs) -- Increase ID so we don't step on feet later
    r' <- hoistMaybe r

    -- For convenience sake, build a list from cons
    -- [(x1, y1), (x2,y2)]
    xs' <- hoistMaybe $ unConsSafe xs

    -- For every member of the cons list, get a list of indices paired to the 
    -- corresponding range
    -- [[(rangeX, 1),(rangeY, 1)],[(rangeX, 2), (rangeY 2)]]
    let fakeEnv = Map.fromList $ map (\range -> (makeElliAlias range, range)) nestedrngs
    idxRangePairs <- hoistMaybe $ getListIdxs fakeEnv xs' r'

    -- Check the first indices to make sure they line up 
    -- a failure would be something like [x2, x3] ... [x1...xn]
    -- 1==begin rangeX? 1==begin rangeY?
    (firstIdxRangePairs, _) <- hoistMaybe $ uncons idxRangePairs
    guard $ all (\(idx, range) -> idx == ed_ib range) firstIdxRangePairs

    -- Check structure: make sure the cons list is increasing.
    -- A failure would be something like [x4, x1, x4] ... [x1...xn]
    guard $ rangesMatchCons idxRangePairs

    (init, lst) <- hoistMaybe $ unsnoc idxRangePairs

    -- For all the ranges-idx pairs, produce new ranges that vary the end idx
    -- zipWith (\_3 _2 -> zipWith (_x1 _y0 -> (_x1, _y0)) 
    --                            (range x 1 _3)
    --                            (range y 1 _2)
    --         ) 
    --         (range 2 n)
    --         (range 2 m)
    (id', rngs') <- lift get
    let (newrngs,nestedrngs') = unzip $ mapWithIdx (makeOuterNewNestedPair id') lst
    lift $ put (id'+length newrngs, newrngs ++ rngs')

    -- return zipped
    hoistMaybe $ evalState (runMaybeT $ tElliOuter r') (length nestedrngs', nestedrngs')
  where
    makeOuterNewNestedPair :: Int -> Int -> (Expr, ElliRange) -> (ElliRange, ElliRange)
    makeOuterNewNestedPair id i (idx, rng) =
        let outer = ElliRange { ed_t=ElliCounter
                              , ed_ib=idx
                              , ed_ie=ed_ie rng
                              , ed_id=id+i}
            newNested = rng {ed_ie=Var $ makeElliAlias outer}
        in (outer, newNested)

    rangesMatchCons :: [[(Expr, ElliRange)]] -> Bool
    rangesMatchCons idxRangePairs = 
        let idxs = map (map fst) idxRangePairs
        in idxs /= []
            -- Does it increase by one every time?
        && all (uncurry oneLT) (concatMap neighbors $ transpose idxs)

    oneLT :: Expr -> Expr -> Bool
    oneLT (Value (Con l)) (Value (Con r)) = l + 1 == r
    oneLT _ _ = False

    getListIdxs :: Map.Map String ElliRange -> [Expr] -> Expr -> Maybe [[(Expr, ElliRange)]]
    getListIdxs env (x:xs) elli = trace (show x ++ "|||" ++ show elli) $
        let res = case fmap sequence $ gzipQ (++) ([] `mkQQMaybe` compareTerm env) x elli of
            { Just (Just res')    -> Just res'
            ; _                   -> Nothing
            }
        in fmap (:) res <*> getListIdxs env xs elli
    getListIdxs _ [] _ = Just []

    -- | Run generically -- collect all (idx, range) corresponding pairs from
    -- the ellipsis expression and the cons expression, but if they do not
    -- match, return Nothing
    -- This is so ugly.
    compareTerm :: Map.Map String ElliRange -> Expr -> Expr -> Maybe [Maybe (Expr, ElliRange)]
    compareTerm env (ListElement n idx) (Var elli) = case env Map.!? elli of
        Just range@(ElliRange {ed_t=ElliList elliName}) -> 
            if n == elliName 
            then Just [Just (idx, range)]
            else Just [Nothing]
        Nothing -> Just [Nothing]
    compareTerm _ l r | toConstr l == toConstr r = Nothing
                      | otherwise = Just [Nothing]
    

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
    isElliAble (Value (Con _))  = True
    isElliAble (Var _)          = True
    isElliAble _                = False

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
    isCore' (ElliFoldr {} )  = False
    isCore' (ElliComp _ _)  = False
    isCore' (EllipVar _)    = False
    isCore' (ElliGroup _)   = False
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
