{-# LANGUAGE ScopedTypeVariables #-}
module EllipLang.Translator where

import Data.Generics (everywhere, mkT, everything, mkQ, cast, Typeable, gzip)
import Data.Function ((&))
import EllipLang.Syntax
import EllipLang.Examples
import Data.Maybe (fromMaybe)
import EllipLang.Eval (idxToExpr)

data ElliClass = Fold Expr ElliClass
               | ZipWith Int

------------------------------------------------------------------------
-- 
-- Translation Elli-Haskell ==> Mini-Haskell
-- 
------------------------------------------------------------------------

translate :: Expr -> Expr
translate tr = tr
    & everywhere (mkT unEllipsisAlt)
    & everywhere (mkT unEllipsisExpr)

unEllipsisExpr :: Expr -> Expr
unEllipsisExpr (PreEllipsis tl tr) =
    let t = transformPreEllipsis tl tr
        c = classifyPreEllipsis t
    in toCore c t
unEllipsisExpr t = t

toCore :: ElliClass -> Expr -> Expr
toCore (ZipWith 1) t = 
    let f   = ellipTermToAnonFun 1 t
        ehd = head $ collectElliData t
        ib = idxToExpr $ ehs_ib ehd
        ie = idxToExpr $ ehs_ie ehd
    in (Var "map") `App` f `App` (Var "range" `App` (Var "xs") `App` ib `App` ie)

ellipTermToAnonFun :: Int -> Expr -> Expr
ellipTermToAnonFun 1 t = 
    let name = (ehs_name $ head $ collectElliData t) ++ "_elliHaskell_1"
    in Abstr name (everywhere (mkT (replaceElliData name)) t)
    where 
    replaceElliData s (ElliHaskellData {}) = (Var s)
    replaceElliData _ t = t
ellipTermToAnonFun i t = error $ "Weird: i=" ++ show i ++ "; t=" ++ show t

transformPreEllipsis :: Expr -> Expr -> Expr
transformPreEllipsis l r = fromMaybe (error "transformPreEllipsis") (gzip (\x y -> mkTTMaybe toElliData x y) l r)
    where
    toElliData :: Expr -> Expr -> Maybe Expr
    toElliData (ListElement nl il) (ListElement nr ir) = 
        if nl /= nr then error "toElliData" else
        Just ElliHaskellData {
            ehs_ib = il,
            ehs_ie = ir,
            ehs_name = nl,
            ehs_id = Nothing
        }
    toElliData _ _ = Nothing -- Unchanged

classifyPreEllipsis :: Expr -> ElliClass
classifyPreEllipsis transformedTree =
    let collection = collectElliData transformedTree
    in ZipWith (length collection)


collectElliData = everything (++) ([] `mkQ` getElliData)
    where 
    getElliData :: Expr -> [Expr]
    getElliData ehd@(ElliHaskellData {}) = [ehd]
    getElliData _                        = []

unEllipsisAlt :: (Pattern,Expr) -> (Pattern,Expr)
unEllipsisAlt (PEllipsis n (End ni), t) = (PVar ("_"), Let ni (Var "length" `App` Var "xs") t)

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

-- Helper function: Generic 
mkTTMaybe :: (Typeable a, Typeable b, Typeable c)
    => (a -> a -> Maybe a) -> b -> c -> Maybe c
mkTTMaybe f x y = case (cast x, cast y) of
    (Just (x'::a), Just (y'::a))    -> case (f x' y') of
        Just res -> cast res
        Nothing -> Nothing
    _                               -> Nothing
