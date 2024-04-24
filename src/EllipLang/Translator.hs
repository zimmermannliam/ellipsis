module EllipLang.Translator where

import Data.Generics 
import EllipLang.Syntax
import EllipLang.Pretty (pp)
import GenericHelper (everywhereUntil, mkTTMaybe)
import EllipLang.Eval (idxToExpr)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data ElliClass = Fold Expr ElliClass
               | ZipWith Int

type ListAliases = Map.Map Name Expr

------------------------------------------------------------------------
-- 
-- Translation Elli-Haskell ==> Mini-Haskell
-- 
------------------------------------------------------------------------

translate :: Expr -> Expr
translate = translate' Map.empty

translate' :: ListAliases -> Expr -> Expr
translate' aliases = everywhereUntil (False `mkQ` isElliCase) (mkT (unElliCase aliases))

isElliCase :: Expr -> Bool
isElliCase (Case _ alts) = (isElliPattern . fst) `any` alts
    where
    isElliPattern :: Pattern -> Bool
    isElliPattern (PEllipsis _ _) = True
    isElliPattern _ = False
isElliCase _ = False

{-
    ... case ...
        / unElliCase
    case <target> of <alts>
                        / unElliAlts
            { x1 ... xn -> <t>; ...}
                           / unElliExpr
                    ... (x1 ... xn) ...
                         / unPreElli
                        
-}

unElliCase :: ListAliases -> Expr -> Expr
-- Can be used generically; takes a case with elli-haskell patterns
-- and transforms them into mini-haskell expressions
unElliCase aliases c@(Case target alts) = 
    let alts' = map (unElliAlt aliases target) alts
    in Case target alts
unElliCase aliases t = t

addAliases :: Name -> Expr -> Idx -> ListAliases -> ListAliases
addAliases listAlias target idx aliases =
    let aliasMap = Map.fromList [(listAlias, target)]
        idxAlias = Map.fromList (
            case idxToName idx of {
                Just lenName -> [(lenName, App (Var "length") target)];
                Nothing -> [];
            })
    in aliases `Map.union` aliasMap `Map.union` idxAlias
    where
    idxToName :: Idx -> Maybe Name
    idxToName i = case idxToExpr i of
        Var n   -> Just n
        _       -> Nothing

unElliAlt :: ListAliases -> Expr -> (Pattern, Expr) -> (Pattern, Expr)
-- Takes an elli-haskell alt and turns it into a non-elli-haskell alt.
unElliAlt aliases target (PEllipsis listAlias len, t) = 
    let aliases' = addAliases listAlias target len aliases
        t' = unElliExpr aliases' t
    in (PVar "_", t')
unElliAlt _ _ alt = alt

unElliExpr :: ListAliases -> Expr -> Expr
unElliExpr aliases = everywhereUntil (False `mkQ` isPreElli) (mkT (unElliExpr' aliases)) -- make until pre ellip

isPreElli :: Expr -> Bool
isPreElli (PreEllipsis _ _) = True
isPreElli _                 = False

unElliExpr' :: ListAliases -> Expr -> Expr
unElliExpr' aliases (PreEllipsis tl tr) =
    let t = fromMaybe (error "Different structure in ellipsis") $ gzip (\l r -> mkTTMaybe (unPreElli aliases) l r) tl tr
    in t
unElliExpr' _ t = t

unPreElli :: ListAliases -> Expr -> Expr -> Maybe Expr
unPreElli aliases (ListElement nl il) (ListElement nr ir) 
    | nl /= nr = error "Different list element terms"
    | otherwise = error "Blah"

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
    isCore' (PreEllipsis _ _)       = False
    isCore' (PreEllipsisFold {} )   = False
    isCore' (Ellipsis _ _)          = False
    isCore' (EllipVar _)            = False
    isCore' (ElliHaskellData {})    = False
    isCore' _                       = True
