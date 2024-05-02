module EllipLang.Translator where

import EllipLang.Syntax
import EllipLang.Pretty (pp, makeElliAlias)
import EllipLang.Eval (idxToExpr)
import GenericHelper (everywhereUntil, mkMMMaybe, gzipM)
import EllipLang.SmartCons ((<.>), inte)

import Data.Generics 
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad.State ( State, evalState, runState, MonadState(put, get), join)
import Debug.Trace (trace, traceShowId)

data ElliClass = Fold Expr ElliClass
               | ZipWith Int

type ListAliases = Map.Map Name Expr

toZipWithN :: Int -> Expr
toZipWithN i = (map Var ["id", "map", "zipWith", "zipWith3", "zipWith4", "zipWith5"]) !! i

type ElliState = (Id, [ElliRange])

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

-- Main function!
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

elliTransform :: ListAliases -> Expr -> Expr -> Expr
elliTransform aliases b e = case runState  (elliTransform' aliases b e) (0, []) of
    (r, s) -> r

elliTransform' :: ListAliases -> Expr -> Expr -> State ElliState Expr
elliTransform' aliases b e = do
    transformedTree <- fromMaybe 
            (error "Elli transform: bad structure") 
            (gzipM (mkMMMaybe (elliTransformCollect aliases)) b e)
    (nZipWith, collectedRanges) <- get
    let mappedFn = foldr ((<.>) . Var . makeElliAlias) transformedTree collectedRanges
    let ranges = map (makeRange aliases) collectedRanges
    return $ foldl1 App $
        [ toZipWithN nZipWith
        , mappedFn
        ] ++ ranges

elliTransformCollect :: ListAliases -> Expr -> Expr -> Maybe (State ElliState Expr)
elliTransformCollect _ (ListElement nl idxl) (ListElement nr idxr) 
    | nl /= nr      = error "ListElements are different"
    | otherwise     = Just x
      where x = do {
          (id, rs) <- get
        ; let er = ElliRange {ed_id=id, ed_t=ElliList nl, ed_ib=idxToExpr idxl, ed_ie=idxToExpr idxr}
        ; put (id+1, er:rs)
        ; return $ ER er
        }
            -- this is bit of a hack. The reason I'm not using MaybeT is because
            -- the outer "maybe" is intended to mean "change nothing and continue
            -- traversing", not "failure".
elliTransformCollect aliases (Ellipsis ll lr) (Ellipsis rl rr) = Just x
    where 
    x = do {
    ; s@(id,rs) <- get
    ; let l = evalState (elliTransform' aliases ll lr) s
    ; let r = evalState (elliTransform' aliases rl rr) s
    ; case gzipM (mkMMMaybe (elliTransformCollect aliases)) l r of
        Just res -> res
        Nothing -> error "bad"
  }
elliTransformCollect aliases (ER l) (ER r) | ed_id l == ed_id r = Just (return $ ER l)
                                           | otherwise          = Nothing
elliTransformCollect aliases (ElliGroup l) (ElliGroup r) = Just x
    where x = do {
          (id, rs) <- get
        ; let er = ElliRange {ed_id=id, ed_t=ElliCounter, ed_ib=l, ed_ie=r}
        ; put (id+1, er:rs)
        ; return $ ER er
    }
elliTransformCollect _ l r 
    | not (isElliAble l && isElliAble r) = Nothing -- Continue
    | l == r    = Nothing
    | otherwise = Just x
    where x = do {
          (id, rs) <- get
        ; let er = ElliRange {ed_id=id, ed_t=ElliCounter, ed_ib=l, ed_ie=r}
        ; put (id+1, er:rs)
        ; return $ ER er
    }

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

-- Eventually: make a gzip that handles this:
data GZipCase a = J a | Continue | BadStructure

idxToName :: Idx -> Maybe Name
idxToName i = case idxToExpr i of
    Var n   -> Just n
    _       -> Nothing

makeRange :: ListAliases -> ElliRange -> Expr
makeRange aliases ElliRange {ed_t=ElliList n, ed_ib=ib, ed_ie=ie} = Var "range" `App` (aliases Map.! n) `App` ib `App` ie
makeRange aliases ElliRange {ed_t=ElliCounter, ed_ib=ib, ed_ie=ie} = Btwn ib ie