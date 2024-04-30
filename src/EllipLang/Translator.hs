module EllipLang.Translator where

import EllipLang.Syntax
import EllipLang.Pretty (pp, makeElliAlias)
import EllipLang.Eval (idxToExpr)
import GenericHelper (everywhereUntil, mkTTMaybe, runStateEverywhere)
import EllipLang.SmartCons ((<.>), inte)

import Data.Generics 
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad.State ( State, runState, MonadState(put, get) )

data ElliClass = Fold Expr ElliClass
               | ZipWith Int

type ListAliases = Map.Map Name Expr

toZipWithN :: Int -> Expr
toZipWithN i = (map Var [error "no 0 map", "map", "zipWith", "zipWith3", "zipWith4", "zipWith5"]) !! i

------------------------------------------------------------------------
-- 
-- Translation Elli-Haskell ==> Mini-Haskell
-- 
------------------------------------------------------------------------

translate :: Expr -> Expr
translate = unElli Map.empty

unElli :: ListAliases -> Expr -> Expr
unElli aliases = everywhereUntil (False `mkQ` isElli) (mkT (unElli' aliases))

-- Main function!
unElli' :: ListAliases -> Expr -> Expr
unElli' aliases (Ellipsis b e) =
    case gzip (\x y -> mkTTMaybe combineElli x y) b e of
        Just combinedT  -> let
            (transformedT, (nZipWith, collection)) = runStateEverywhere transformCollectElli (0, []) combinedT
            ranges = map (makeRange aliases) collection
            in  foldl1 App ([toZipWithN nZipWith,
                foldr ((<.>) . elliAliasData) transformedT collection]
                ++ ranges)

        Nothing         -> error "unElli: bad structures"
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
-- Edit ellipsis
------------------------------------------------------------------------
-- elliExprTransformCollect :: Expr -> Expr -> State (Id, 

combineElli :: Expr -> Expr -> Maybe Expr
combineElli (ListElement nb ib) (ListElement ne ie) 
    | nb /= ne  = error "combineElli nb /= ne" 
    | ib == ie  = Just (ListElement nb ib)
    | otherwise = Just $ EHD $ ElliHaskellData
        { ehs_ib=ib
        , ehs_ie=ie
        , ehs_name=nb
        , ehs_id = Nothing } -- to be set later
combineElli l r = if toConstr l == toConstr r then Nothing -- do nothing
    else error "combineElli bad l r"

transformCollectElli :: ElliHaskellData -> State (Id, [ElliHaskellData]) ElliHaskellData
transformCollectElli ehd@(ElliHaskellData {}) = do
    (id, ehds) <- get
    let new_ehd = ehd { ehs_id=Just id }
    put (id+1, new_ehd:ehds)
    return new_ehd

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
    isCore' (Ellipsis _ _)       = False
    isCore' (ElliFold {} )   = False
    isCore' (ElliComp _ _)          = False
    isCore' (EllipVar _)            = False
    isCore' (EHD _)                 = False
    isCore' _                       = True

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

makeRange :: ListAliases -> ElliHaskellData -> Expr
makeRange aliases ElliHaskellData { ehs_name=n, ehs_ib=ib, ehs_ie=ie } = 
    Var "range" `App` (aliases Map.! n) `App` idxToExpr ib `App` idxToExpr ie

elliAliasData :: ElliHaskellData -> Expr
elliAliasData (ElliHaskellData {ehs_name=n, ehs_id=Just id}) = Var (makeElliAlias n id)
elliAliasData _ = error "elliAliasData"

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