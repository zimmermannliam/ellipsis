{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module EllipLang.Pretty where

import EllipLang.Syntax
import Data.Generics ( mkT, everywhere )
import Data.List

pNewline :: Int -> String
pNewline i = "\n" ++ pTabs i

pTabs :: Int -> String
pTabs i = concat $ replicate i "\t"

pp :: Expr -> String
pp = pp' 0

pp' :: Int -> Expr -> String
pp' tabs (Var n)      = n
pp' tabs (App e1 e2)  = pp' tabs e1 ++ " " ++ pp' tabs e2
pp' tabs (Abstr n e)  = "\\" ++ n ++ "." ++ pp' tabs e
pp' tabs (Value v)    = ppVal v
pp' tabs (Let n e1 e2)        = "let " ++ n ++ " = " ++ pp' tabs e1  ++ " in " ++ pNewline (tabs+1) ++ pp' (tabs+1) e2
pp' tabs (Case e alts)        = 
    if all (\alt -> case fst alt of
            PVal (Boolean _) -> True
            _ -> False) alts && length alts == 2
    then "if " ++ pp' tabs e ++ " then "  ++ pp' tabs (snd $ head $ filter (\x -> fst x == PVal (Boolean True)) alts) ++ pNewline tabs ++ "else " ++ pp' tabs (snd $ head $ filter (\x-> fst x == PVal (Boolean False)) alts)
    else "case " ++ pp' tabs e ++ " of" ++ ppMatch alts (tabs + 1)
pp' tabs (LetRec  n e1 e2)    = "letrec " ++ n ++ " = (" ++ pp' tabs e1 ++ pNewline tabs ++ ") in " ++ pp' tabs e2
pp' tabs (Cons e1 e2)         = "(" ++ pp' tabs e1 ++ " : " ++ pp' tabs e2 ++ ")"
pp' tabs (Pair t1 t2) = "(" ++ pp' tabs t1 ++ ", " ++ pp' tabs t2 ++ ")"
pp' tabs (Cat t1 t2) = pp' tabs t1 ++ " ++ " ++ pp' tabs t2
pp' tabs (ListElement n i) = n ++ "[" ++ ppIdx i ++ "]"
pp' tabs (Add e1 e2)          = pp' tabs e1 ++ "+" ++ pp' tabs e2
pp' tabs (Sub t1 t2)          = pp' tabs t1 ++ "-" ++ pp' tabs t2
pp' tabs (Mul t1 t2)          = pp' tabs t1 ++ "*" ++ pp' tabs t2
pp' tabs (Div t1 t2)          = pp' tabs t1 ++ "/" ++ pp' tabs t2
pp' tabs (Mod t1 t2)          = pp' tabs t1 ++ "%" ++ pp' tabs t2
pp' tabs (Abs t)              = "|" ++ pp' tabs t ++ "|"
pp' tabs (Trace _ _ t)        = pp' tabs t
pp' tabs (Eq t1 t2)           = pp' tabs t1 ++ " == " ++ pp' tabs t2
pp' tabs (Neq t1 t2)           = pp' tabs t1 ++ " != " ++ pp' tabs t2
pp' tabs (Lt t1 t2)           = pp' tabs t1 ++ " < " ++ pp' tabs t2
pp' tabs (Gt t1 t2)           = pp' tabs t1 ++ " > " ++ pp' tabs t2
pp' tabs (Leq t1 t2)          = pp' tabs t1 ++ " <= " ++ pp' tabs t2
pp' tabs (Geq t1 t2)          = pp' tabs t1 ++ " >= " ++ pp' tabs t2
pp' tabs (Or t1 t2)           = pp' tabs t1 ++ " || " ++ pp' tabs t2
pp' tabs (And t1 t2)          = pp' tabs t1 ++ " && " ++ pp' tabs t2
pp' tabs (Not t)              = "!(" ++ pp' tabs t ++ ")"
pp' tabs (Error s)            = "error " ++ s
pp' tabs ellip@(Ellipsis _ _) = ppEllip ellip
pp' tabs (EllipVar i)         = "EllipVar(" ++ show i ++ ")"
pp' tabs (PreEllipsis t1 t2)  = pp' tabs t1 ++ " ... " ++ pp' tabs t2
pp' tabs (Index idx)          = ppIdx idx
-- pp' tabs _            = "Error -- cannot display expression"

ppEllip :: Expr -> String
ppEllip (Ellipsis t rs) = let
    tb = pp $ ellipExprReplace rs Begin t
    te = pp $ ellipExprReplace rs EndSide t
    in
    "(" ++ tb ++ " ... " ++ te ++ ")"
ppEllip _ = error "Trying to ppEllip non-ellip"

ellipExprReplace :: EllipRanges -> EllipSide -> Expr -> Expr
ellipExprReplace rs side = everywhere (mkT $ ellipVarToListElement rs side)

ellipVarToListElement :: EllipRanges -> EllipSide -> Expr -> Expr
ellipVarToListElement rs side t@(EllipVar id) = let r = rangeLookup' rs id
    in case r of
        Just r'     -> if contentT r' == BeList 
            then ListElement (var r') (if side == Begin then ib r' else ie r')
            else (if side == Begin then idxToExpr $ ib r' else idxToExpr $ ie r')
        Nothing     -> t
ellipVarToListElement rs side (Ellipsis t innerRs) = Ellipsis t mappedInnerRs
    where 
    mappedInnerRs   = map (\x -> x { ib=case ib x of
        EPlace t' -> EPlace $ ellipExprReplace rs side t'
        otherIdx  -> otherIdx
        , ie=case ie x of
        EPlace t' -> EPlace $ ellipExprReplace rs side t'
        otherIdx  -> otherIdx}) innerRs
    -- innerTerm       = ellipExprReplace (rs ++ mappedInnerRs) side t
ellipVarToListElement _ _ t = t

ppMatch :: Alts -> Int -> String
ppMatch [] _                = ""
ppMatch ((p, e):as) tabs    = pNewline tabs ++ ppPattern p ++ " -> " ++ pp' tabs e ++ ppMatch as tabs

ppPattern :: Pattern -> String
ppPattern (PCons n1 n2) = "(" ++ n1 ++ ":" ++ n2 ++ ")"
ppPattern (PVar n)      = n
ppPattern (PVal v)      = ppVal v
ppPattern (PEllipsis n i) = strn ++ "1 ... " ++ strn ++ ppIdx i
                        where strn = n
ppPattern (PCons' t1 t2) = "(" ++ pp' 0 t1 ++ ":" ++ pp' 0 t2 ++ ")"

ppIdx :: Idx -> String
ppIdx (IPlace i)   = show i
ppIdx (End n)     = n
ppIdx (EPlace t) = pp' 0 t

ppVal :: Val -> String
ppVal (Con i)       = show i
ppVal v@(VCons _ _)   = "[" ++ ppVCons v ++ "]"
ppVal Empty       = "[]"
ppVal (Closure n t e)   = "CLOSURE"-- "CLOSURE( " ++ ppEnv e ++ "|-" ++ pp (Abstr n t) ++ ")"
ppVal (FreeVar n)   = n
ppVal (VPair v1 v2) = "(" ++ ppVal v1 ++ ", " ++ ppVal v2 ++ ")"
ppVal (Boolean b)   = if b then "true" else "false"
ppVal _             = "Error"

ppVCons :: Val -> String
ppVCons (VCons v vs)    = ppVal v ++ if vs == Empty then "" else " " ++ ppVCons vs
ppVCons v               = error "Tried to ppVCons non-VCons: " ++ show v

ppBindee :: Bindee -> String
ppBindee (BVal v)   = ppVal v
ppBindee _          = "list future or lenght future"

ppEnv :: Env -> String
ppEnv e     = "<\n" ++ show e ++ ">"

rangeLookup :: EllipRanges -> Int -> EllipRange
rangeLookup rs i = case filter (\r -> ident r == i) rs of
    []      -> error $ "Could not find range ID " ++ show i ++ " in rs: " ++ show rs
    [x]     -> x
    (x:xs)  -> error $ "Somehow too many hits for range ID " ++ show i ++ " in rs: " ++ show rs


rangeLookup' :: EllipRanges -> Int -> Maybe EllipRange
rangeLookup' rs i = find (\r -> ident r == i) rs

idxToExpr :: Idx -> Expr
idxToExpr (EPlace t) = t
idxToExpr (IPlace i) = Value $ Con i
idxToExpr (End n)    = Var n
