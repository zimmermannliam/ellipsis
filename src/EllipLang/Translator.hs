module EllipLang.Translator where

import EllipLang.Syntax
import EllipLang.Pretty (pp, makeElliAlias)
import EllipLang.Eval (idxToExpr)
import EllipLang.SmartCons ((<.>), inte)

import GenericHelper (everywhereUntil, mkMMMaybeT, gzipM)

import Data.Generics 
import qualified Data.Map as Map
import Control.Monad.State (State, evalState, runState, MonadState(put, get))
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad (mzero, guard)
import Control.Monad.Trans (lift)

import Debug.Trace (trace, traceShowId)

data ElliClass = Fold Expr ElliClass
               | ZipWith Int

type ListAliases = Map.Map Name Expr

toZipWithN :: Int -> Expr
toZipWithN i = (map Var ["id", "map", "zipWith", "zipWith3", "zipWith4", "zipWith5"]) !! i


------------------------------------------------------------------------
-- 
-- Translation Elli-Haskell ==> Mini-Haskell
-- 
------------------------------------------------------------------------

translate :: Expr -> Expr
translate t = let translated = unElli Map.empty t
    in if not (isCore translated)
        then error "Translate did not produce core Mini-Haskell"
        else translated

translate' :: Expr -> Expr
translate' t = let translated = unElli Map.empty t
    in if not (isCore translated) then trace "WARNING: Translate did not produce core!" translated
    else translated

unElli :: ListAliases -> Expr -> Expr
unElli aliases = everywhereUntil (False `mkQ` isElli) (mkT (unElli' aliases))
  where
    unElli' :: ListAliases -> Expr -> Expr
    unElli' aliases (Ellipsis b e) = elliTransform aliases b e
    unElli' aliases (ListElement n idx) = Var "(!!)" `App` (aliases Map.! n) `App` (Index idx `Sub` inte 1)
    unElli' aliases c@(Case target alts)
        | isElliCase c =
            let alts' = map (unElliAlt aliases target) alts
            in Case target alts'
        | otherwise = c
    unElli' _ t = t
    isElli :: Expr -> Bool
    isElli (Ellipsis _ _) = True
    isElli t@(Case _ _)   = isElliCase t
    isElli _                 = False

------------------------------------------------------------------------
-- Edit ellipsis
------------------------------------------------------------------------

type ElliState = (Id, [ElliRange])

elliTransform :: ListAliases -> Expr -> Expr -> Expr
elliTransform aliases b e =
    case evalState (runMaybeT (elliTransform' aliases b e)) (0, []) of
        Just r  -> r
        Nothing -> error "Could not transform ellipsis: Bad structure."

elliTransform' :: ListAliases -> Expr -> Expr -> MaybeT (State ElliState) Expr
elliTransform' aliases b e = do
    transformedTree <- gzipM (mkMMMaybeT (elliTransformCollect aliases)) b e
    (_, collectedRanges) <- lift get
    let mappedFn = foldr ((<.>) . Var . makeElliAlias) transformedTree collectedRanges
    let nZipWith = length collectedRanges
    let ranges = map (makeRange aliases) collectedRanges
    return $ foldl1 App $
        [ toZipWithN nZipWith
        , mappedFn
        ] ++ ranges


elliTransformCollect :: ListAliases -> Expr -> Expr -> MaybeT (State ElliState) Expr

elliTransformCollect _ (ListElement nl idxl) (ListElement nr idxr)
    | nl /= nr      = error "ListElements are different"
    | otherwise = do
        (id, rs) <- lift get
        let er = ElliRange {ed_id=id, ed_t=ElliList nl, ed_ib=idxToExpr idxl, ed_ie=idxToExpr idxr}
        lift $ put (id+1, er:rs)
        return $ ER er

elliTransformCollect aliases (Ellipsis ll lr) (Ellipsis rl rr) = do
    s@(id,rs) <- lift get
    -- These computations are contained and do not affect state, other than
    -- increasing the id for the purpose of disambiguation.
    let (l, (idl, _)) = runState (runMaybeT $ elliTransform' aliases ll lr) s
    let (r, (idr, _)) = runState (runMaybeT $ elliTransform' aliases rl rr) s
    case (l, r) of
        (Just l', Just r') -> do
            lift $ put (id+idl,rs)
            gzipM (mkMMMaybeT (elliTransformCollect aliases)) l' r'
        _ -> mzero

elliTransformCollect aliases (ER l) (ER r) | ed_id l == ed_id r = return $ ER l
                                           | otherwise          = mzero

elliTransformCollect aliases (ElliGroup l) (ElliGroup r) = do
    (id, rs) <- lift get
    let er = ElliRange {ed_id=id, ed_t=ElliCounter, ed_ib=l, ed_ie=r}
    lift $ put (id+1, er:rs)
    return $ ER er

elliTransformCollect _ l r 
    | not (isElliAble l && isElliAble r) = mzero -- Continue
    | l == r    = mzero
    | otherwise = do
        (id, rs) <- lift get
        let er = ElliRange {ed_id=id, ed_t=ElliCounter, ed_ib=l, ed_ie=r}
        lift $ put (id+1, er:rs)
        return $ ER er
  where
    isElliAble :: Expr -> Bool
    isElliAble (Value (Con _))  = True
    isElliAble (Var _)          = True
    isElliAble _                = False

------------------------------------------------------------------------
-- Edit case
------------------------------------------------------------------------
unElliCase :: ListAliases -> Expr -> Expr
-- Can be used generically; takes a case with elli-haskell patterns
-- and transforms them into mini-haskell expressions
unElliCase aliases c@(Case target alts) = 
    let alts' = map (unElliAlt aliases target) alts
    in Case target alts'
unElliCase aliases t = t

unElliAlt :: ListAliases -> Expr -> (Pattern, Expr) -> (Pattern, Expr)
-- Takes an elli-haskell alt and turns it into a non-elli-haskell alt.
unElliAlt aliases target (PEllipsis listAlias len, t) = 
    let aliases' = addAliases listAlias target len aliases
        lenLet = maybe id (\n -> Let n (aliases' Map.! n)) (idxToName len)
        t' = lenLet $ unElli aliases' t
    in (PVar "_", t')
unElliAlt _ _ alt = alt


------------------------------------------------------------------------
-- 
-- Core Property
-- 
------------------------------------------------------------------------

-- isCore is only true if there are no Elli-Haskell extensions
isCore :: Expr -> Bool
isCore = everything (&&) (True `mkQ` isCore')
    where
    isCore' :: Expr -> Bool
    isCore' (Ellipsis _ _)  = False
    isCore' (ElliFold {} )  = False
    isCore' (ElliComp _ _)  = False
    isCore' (EllipVar _)    = False
    isCore' (ElliGroup _)   = False
    isCore' _               = True

------------------------------------------------------------------------
-- 
-- Other functions
-- 
------------------------------------------------------------------------

isElliCase :: Expr -> Bool
isElliCase (Case _ alts) = (isElliPattern . fst) `any` alts
    where
    isElliPattern :: Pattern -> Bool
    isElliPattern (PEllipsis _ _) = True
    isElliPattern _ = False
isElliCase _ = False

addAliases :: Name -> Expr -> Idx -> ListAliases -> ListAliases
addAliases listAlias target idx aliases =
    let aliasMap = Map.fromList [(listAlias, target)]
        idxAlias = Map.fromList (
            case idxToName idx of {
                Just lenName -> [(lenName, App (Var "length") target)];
                Nothing -> [];
            })
    in aliases `Map.union` aliasMap `Map.union` idxAlias

idxToName :: Idx -> Maybe Name
idxToName i = case idxToExpr i of
    Var n   -> Just n
    _       -> Nothing

makeRange :: ListAliases -> ElliRange -> Expr
makeRange aliases ElliRange {ed_t=ElliList n, ed_ib=ib, ed_ie=ie} = Var "range" `App` (aliases Map.! n) `App` ib `App` ie
makeRange aliases ElliRange {ed_t=ElliCounter, ed_ib=ib, ed_ie=ie} = Btwn ib ie
