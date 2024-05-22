{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module EllipLang.Pretty where

import EllipLang.Syntax

import Data.Generics ( mkT, everywhere )
import Data.List
import qualified Data.Map as Map

pNewline :: Int -> String
pNewline i = "\n" ++ pTabs i

pTabs :: Int -> String
pTabs i = concat $ replicate i "\t"

cOMMA :: Bool
cOMMA = True

pp :: Expr -> String
pp = pp' 0

pp' :: Int -> Expr -> String
pp' tabs (Var n)      = n
pp' tabs (Var "(++)" `App` t@(Cons _ _) `App` (Ellipsis l r)) = 
    "[" ++ ppList tabs t ++ "," ++ pp' tabs l ++ ",...," ++ pp' tabs r ++ "]"
pp' tabs (App e1 e2)  = pp' tabs e1 ++ " (" ++ pp' tabs e2 ++ ")"
pp' tabs t@(Abstr n e)  = ppAbstr tabs t
pp' tabs (Value v)    = ppVal v
pp' tabs (Let n e1 e2)        = "let " ++ n ++ " = " ++ pp' tabs e1  ++ " in " ++ pNewline (tabs+1) ++ pp' (tabs+1) e2
pp' tabs (Case e alts)        = 
    if all (\alt -> case fst alt of
            PVal (Boolean _) -> True
            _ -> False) alts && length alts == 2
    then "if " ++ pp' tabs e ++ " then "  
        ++ pp' tabs (snd $ head $ filter (\x -> fst x == PVal (Boolean True)) alts) 
        ++ pNewline tabs ++ "else " 
        ++ pp' tabs (snd $ head $ filter (\x-> fst x == PVal (Boolean False)) alts)
    else "case " ++ pp' tabs e ++ " of {" ++ ppMatch alts (tabs + 1) ++ pNewline (tabs) ++  "}"
pp' tabs (LetRec  n e1 e2)    = "let " ++ n ++ " = " ++ pp' tabs e1 ++ pNewline tabs ++ "in " ++ pp' tabs e2
pp' tabs t@(Cons _ _)         = "[" ++ ppList tabs t ++ "]"
pp' tabs (Pair t1 t2) = "(" ++ pp' tabs t1 ++ ", " ++ pp' tabs t2 ++ ")"
pp' tabs (Cat t1 t2) = pp' tabs t1 ++ " ++ " ++ pp' tabs t2
pp' tabs (ListElement n (Var i))    = n ++ i
pp' tabs (ListElement n (Value (Con i)))    = n ++ show i
pp' tabs (ListElement n i)    = n ++ "{" ++ ppIdx i ++ "}"
pp' tabs (Op op e1 e2)        = pp' tabs e1 ++ " " ++ ppOp op ++ " " ++ pp' tabs e2 
pp' tabs (Abs t)              = "|" ++ pp' tabs t ++ "|"
pp' tabs (Trace _ _ t)        = pp' tabs t
pp' tabs (Not t)              = "not (" ++ pp' tabs t ++ ")"
pp' tabs (Error s)            = "error \"" ++ s ++ "\""
pp' tabs ellip@(ElliComp _ _) = ppEllip ellip
pp' tabs (EllipVar i)         = "EllipVar(" ++ show i ++ ")"
pp' tabs (Ellipsis t1 t2)  = "[" ++ pp' tabs t1 ++ ",...," ++ pp' tabs t2 ++ "]"
pp' tabs (Index idx)          = ppIdx idx
pp' tabs (ElliFoldr t1 t2 op) 
    = pp' tabs t1 ++ " " ++ ppOp op ++ " (... " ++ ppOp op ++ " " ++ pp' tabs t2 ++ ")"
pp' tabs (ElliFoldl t1 t2 op) 
    = "(" ++ pp' tabs t1 ++ " " ++ ppOp op ++ " ...) " ++ ppOp op ++ " "++ pp' tabs t2
pp' tabs (ER r)             = makeElliAlias r
pp' tabs (Btwn t1 t2) = "[" ++ pp' tabs t1 ++ ".." ++ pp' tabs t2 ++ "]"
pp' tabs (ElliGroup t) = "[" ++ pp' tabs t ++ "]"
-- pp' tabs _            = "Error -- cannot display expression"

ppOp :: IfxOp -> String
ppOp Add    = "+"
ppOp Sub    = "-" 
ppOp Mul    = "*" 
ppOp Div    = "/" 
ppOp Mod    = "%" 
ppOp Eq     = "=="
ppOp Neq    = "!="
ppOp Lt     = "<" 
ppOp Gt     = ">" 
ppOp Leq    = "<="
ppOp Geq    = ">="
ppOp Or     = "||"
ppOp And    = "&&"
ppOp (VarOp s) = "`" ++ s ++ "`"

ppList :: Int -> Expr -> String
ppList tabs (Value Empty) = ""
ppList tabs (Cons t (Value Empty)) = pp' tabs t
ppList tabs (Cons t ts) = pp' tabs t ++ "," ++ ppList tabs ts

ppAbstr :: Int -> Expr -> String
ppAbstr tabs t = let 
    (head, body) = getNestedAbstr t
    in "\\" ++ head ++ "-> " ++ pp' (tabs+1) body
    where
    getNestedAbstr :: Expr -> (String, Expr)
    getNestedAbstr (Abstr n t) = 
        let (head, body) = getNestedAbstr t
        in (n ++ " " ++ head, body)
    getNestedAbstr t = ("", t)

ppEllip :: Expr -> String
ppEllip (ElliComp t rs) = let
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
ellipVarToListElement rs side (ElliComp t innerRs) = ElliComp t mappedInnerRs
    where 
    mappedInnerRs   = map (\x -> x { ib=case ib x of
        t' -> ellipExprReplace rs side t'
        , ie=case ie x of
        t' -> ellipExprReplace rs side t'
        }) innerRs
    -- innerTerm       = ellipExprReplace (rs ++ mappedInnerRs) side t
ellipVarToListElement _ _ t = t

ppMatch :: Alts -> Int -> String
ppMatch [] _                = ""
ppMatch ((p, e):as) tabs    = pNewline tabs ++ ppPattern p ++ " -> " ++ pp' tabs e ++ "; " ++ ppMatch as tabs 

ppPattern :: Pattern -> String
ppPattern (PCons n1 n2) = "(" ++ n1 ++ ":" ++ n2 ++ ")"
ppPattern (PVar n)      = n
ppPattern (PVal v)      = ppVal v
ppPattern (PEllipsis n i) = strn ++ "1 ... " ++ strn ++ ppIdx i
                        where strn = n
ppPattern (PCons' t1 t2) = "(" ++ pp' 0 t1 ++ ":" ++ pp' 0 t2 ++ ")"

ppIdx :: Idx -> String
ppIdx (Value (Con i))   = show i
ppIdx (Var n)     = n
ppIdx (t) = pp' 0 t

ppVal :: Val -> String
ppVal (Con i)       = if i >= 0 then show i else "(" ++ show i ++ ")"
ppVal v@(VCons _ _)   = "[" ++ ppVCons v ++ "]"
ppVal Empty       = "[]"
ppVal (Closure n t e)   = "CLOSURE"-- "CLOSURE( " ++ ppEnv e ++ "|-" ++ pp (Abstr n t) ++ ")"
ppVal (FreeVar n)   = n
ppVal (VPair v1 v2) = "(" ++ ppVal v1 ++ ", " ++ ppVal v2 ++ ")"
ppVal (Boolean b)   = if b then "true" else "false"
ppVal _             = "Error"

spacer :: String
spacer = if cOMMA then "," else " "

ppVCons :: Val -> String
ppVCons (VCons v vs)    = ppVal v ++ if vs == Empty then "" else spacer ++ ppVCons vs
ppVCons v               = error "Tried to ppVCons non-VCons: " ++ show v

ppBindee :: Bindee -> String
ppBindee (BVal v)   = ppVal v
ppBindee _          = "list future or lenght future"

ppEnv :: Env -> String
ppEnv e     = "<\n" ++ foldr (\l r -> l ++ "\n" ++ r) "" (map ppBinding (Map.toList e)) ++ ">"
    where
    ppBinding :: (Identifier, Bindee) -> String
    ppBinding (NamedVar n, r) = n ++ " ==> " ++ ppBindee r
    ppBinding (_, r) = "_ ==> " ++ ppBindee r

    

rangeLookup :: EllipRanges -> Int -> EllipRange
rangeLookup rs i = case filter (\r -> ident r == i) rs of
    []      -> error $ "Could not find range ID " ++ show i ++ " in rs: " ++ show rs
    [x]     -> x
    (x:xs)  -> error $ "Somehow too many hits for range ID " ++ show i ++ " in rs: " ++ show rs


rangeLookup' :: EllipRanges -> Int -> Maybe EllipRange
rangeLookup' rs i = find (\r -> ident r == i) rs

idxToExpr :: Idx -> Expr
idxToExpr = id

makeElliAlias :: ElliRange -> Name
makeElliAlias ElliRange {ed_t=t, ed_id=id} = makeElliAlias' t id
    where
    makeElliAlias' :: ElliType -> Id -> Name
    makeElliAlias' (ElliList n) i = "_" ++ n ++ show i
    makeElliAlias' ElliCounter i = "_" ++ show i


miniHaskellPrelude = foldr1 (\l r -> l ++ "\n" ++ r)
    ["import Prelude ((+), (-), div, (*), mod, (>), (>=), (<), (<=), (==), (&&), (||), not, error, (++), map, reverse, take, drop, length, Int, ($), otherwise)"
    ,
    "range :: [a] -> Int -> Int -> [a] \nrange xs i j     | i < 1        = [] \n                 | j < 1        = [] \n                 | i > length xs = [] \n                 | i <= j       = drop (i-1) $ take j $ xs \n                 | otherwise    = let \n                    i' = length xs - i + 1 \n                    j' = length xs - j + 1 \n                  in drop (i'-1) $ take j' $ reverse xs"
    ]

ppModule :: String -> [(String, Expr)] -> String
ppModule s ms = "module " ++ s ++ " where\n\n" ++ miniHaskellPrelude ++ "\n\n" ++ (foldr (\l r -> r ++ "\n\n" ++ l) "" $ map (\(label, rhs) -> label ++ " = " ++ pNewline 1 ++ pp' 1 rhs) ms)