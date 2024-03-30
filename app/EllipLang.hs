{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module EllipLang where

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


---------------------------------------------------------------------------------
-- SYNTAX
---------------------------------------------------------------------------------

data Expr = Var Name                -- Variable
          | App Expr Expr           -- Application
          | Abstr Name Expr         -- Abstraction
          | Value Val               -- Value Literal
          | Let Name Expr Expr      -- Let expression
          | Case Expr Alts          -- Case expression
          | LetRec Name Expr Expr   -- LetRec "Name" = Expr in Expr
          | Ellipsis Expr EllipRanges
          | Cons Expr Expr          -- expr1:list
          | Cat Expr Expr           -- list1 ++ list2
          | Error String            -- Creates an error
          | Pair Expr Expr          -- (expr1, expr2)
          | ListElement Name Idx    -- x[k]
          | Trace String Expr Expr  -- Debug
          | Eq Expr Expr            -- Relational Operators
          | Lt Expr Expr
          | Gt Expr Expr
          | Leq Expr Expr
          | Geq Expr Expr
          | Neq Expr Expr
          | Or Expr Expr            -- Boolean Operators
          | Not Expr
          | And Expr Expr
          | Add Expr Expr           -- Arithmetic Operators
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Mod Expr Expr
          | Abs Expr
          | EllipVar Id
          | PreEllipsis Expr Expr
          | Index Idx
          deriving (Eq, Show, Data)

data Val    = Con Int 
              | VCons Val Val
              | Empty 
              | Closure Name Expr Env
              | FreeVar Name
              | VPair Val Val
              | VStr String
              | Boolean Bool
             deriving (Eq, Show, Data)

data Pattern = PCons Name Name
             | PCons' Expr Expr
             | PVar Name
             | PVal Val
             | PEllipsis Name Idx -- x1 ... xn -> x2 ... xn
             deriving (Eq, Show, Data)

data Idx    = IPlace Int        -- Reflect work they're doing -- 
            | End Name 
            | EPlace Expr -- Must evaluate to an integer
            deriving (Eq, Show, Data)

-- Bindee?
data Bindee    = BVal Val
               | ListFuture Name
               | LenFuture Name
               | BIterator { it_ie :: Int,
                             it_ib :: Int,
                             it_ic :: Int,
                             vname :: Name,
                             content :: IteratorContent}
                deriving (Eq, Show, Data)

data ContentType = BeList | BeIndices
    deriving (Eq, Show, Data)

data IteratorContent = List [Val] | Indices
    deriving (Eq, Show, Data)

data EllipRange = EllipRange { ident :: Id
                             , var :: Name
                             , ib :: Idx
                             , ie :: Idx
                             , contentT :: ContentType}
                             deriving (Eq, Show, Data)

data EllipSide = Begin | EndSide
    deriving (Eq, Show, Data)

data Identifier         = NamedVar Name | IdVar Id
    deriving (Eq, Show, Data, Ord)

newtype EllipError        = EllipError String
    deriving (Eq, Show, Data)

type EllipRanges        = [EllipRange]
type Name               = String
type Alts               = [(Pattern, Expr)]
type Id                 = Int
type Env                = Map.Map Identifier Bindee



---------------------------------------------------------------------------------
-- SEMANTICS
-- ASSUME EVERY VARIABLE IS NAMED SEPARATELY
---------------------------------------------------------------------------------

head' :: [a] -> a
head' [] = error "Blah blah blah blah error"
head' (x:_) = x

-- y combinator
-- \f.( (\x.(f (x x))) (\x.(f (x x))) )
ycomb :: Expr
ycomb = Abstr "f" $ 
            App (Abstr "x" $ App (Var "f" ) (App (Var "x") (Var "x"))) 
                (Abstr "x" $ App (Var "f" ) (App (Var "x") (Var "x")))

-- Evaluate an Expression in an Environment into a Value
eval :: Env -> Expr -> Val

eval e (Var vn) = evalBinding e (envLookup e vn)

eval e (App t1 t2) = case eval e t1 of 
    Closure x t1b e2 -> eval (Map.insert (NamedVar x) (BVal $ eval e t2) e2) t1b
    _           -> errorOut e "Expected fn to be applied"

eval e (Abstr x t) = Closure x t e

eval e (Let n t1 t2) = eval (Map.insert (NamedVar n) (BVal $ eval e t1) e) t2

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

eval e (LetRec n t1 t2) = case eval e t1 of
    Closure {}  -> eval (Map.insert (NamedVar n) (BVal $ eval e $ App ycomb (Abstr n t1)) e) t2
    _           -> errorOut e "Expected fn to be letrecced"
eval e (Value v)            = v
eval e (Case tc ps) = patternMatchEval e tc ps

eval e (Cons t1 t2) = VCons (eval e t1) (eval e t2)

eval e (Pair t1 t2) = VPair (eval e t1) (eval e t2)

eval e (Cat t1 t2)    = 
    let et1 = eval e t1
        et2 = eval e t2
    in case (et1, et2) of
        (VCons _ _, VCons _ _) -> catVCons et1 et2
        (VCons _ _, Empty)     -> et1
        (Empty, VCons _ _)     -> et2
        (Empty, Empty)         -> Empty
        (v1, v2)               -> errorOut e $ "Tried to cat non-lists: " ++ show v1 ++ " ++ " ++ show v2

eval e (ListElement n i) = findListFutureElement e (envLookup e n) i

eval e (Error s) = errorOut e s

eval e (Ellipsis t rs) = let 
    ellipEnv = getEllipsisIterators e rs
    in
    if not (rangesCheck ellipEnv)
        then error $ "Unequal ranges in ellipEnv: " ++ show ellipEnv
    else if not (boundsCheck e ellipEnv)
        then Empty
    else iterateEllipsis e ellipEnv t 

eval e (Trace s tt t) = trace (s ++ ": " ++ ppVal (eval e tt)) $ eval e t

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


eval e (EllipVar i)   = evalBinding e (e Map.! IdVar i)

eval e ell@(PreEllipsis _ _) = eval e $ processPreEllipsis ell

eval e (Index idx)    = eval e $ idxToExpr idx

processPreEllipsis :: Expr -> Expr
processPreEllipsis (PreEllipsis t1 t2) = Ellipsis t rs
    where
    (t,(rs,_)) = runState (processPreEllipsis' t1 t2) ([],0)
processPreEllipsis _ = error "Trying to processPreEllipsis on non-preEllipsis"

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
            BeList      -> List $ drop (ib_int - 1) $ vConsToList (evalBinding e (ListFuture var))
            BeIndices   -> Indices
        }
            where
            ib_int = idxToInt e ib
            ie_int = idxToInt e ie

vConsToList :: Val -> [Val]
vConsToList (VCons x xs)= x:vConsToList xs
vConsToList Empty       = []
vConsToList _           = error "Tried to convert a non-vcons to list"

iterateEllipsis :: Env -> Env -> Expr -> Val
iterateEllipsis e ellipEnv t = foldr VCons Empty $ iterateEllipsis' e ellipEnv t range
    where
    range = (\it -> 1 + it_ie it - it_ib it) $ head $ Map.elems ellipEnv
    iterateEllipsis' :: Env -> Env -> Expr -> Int -> [Val]
    iterateEllipsis' _ _ _ 0 = []
    iterateEllipsis' e ellipEnv t cd = eval (e `Map.union` ellipEnv) t:iterateEllipsis' e (advanceIts ellipEnv) t (cd-1)

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
valEvalInt _        = error "Expected integer"

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


{-
= Just $ eval (Map.fromList [(NamedVar n, ListFuture n_t), 
                                                          (NamedVar n_idx, LenFuture n_t)
                                                         ] `Map.union` e) t2
                    where   n_idx = case i of
                                (End n) -> n
                                _       -> error "Tried to unpack a bad end idx for ellipsis pattern"
                            n_t = case t of
                                (Var n) -> n
                                _       -> error "Tried to unpack bad name-term for ellipsis pattern"
-}

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
    List []     -> error "Empty list in bound iterator"
    Indices     -> Con ic


rangeLookup :: EllipRanges -> Int -> EllipRange
rangeLookup rs i = case filter (\r -> ident r == i) rs of
    []      -> error $ "Could not find range ID " ++ show i ++ " in rs: " ++ show rs
    [x]     -> x
    (x:xs)  -> error $ "Somehow too many hits for range ID " ++ show i ++ " in rs: " ++ show rs


rangeLookup' :: EllipRanges -> Int -> Maybe EllipRange
rangeLookup' rs i = find (\r -> ident r == i) rs

------------------------------------------------------------------------
-- PARSE
------------------------------------------------------------------------

parse :: String -> Expr
parse _ = Value Empty

------------------------------------------------------------------------
-- PRINT
------------------------------------------------------------------------

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
ppVal (Boolean b)   = if b then "True" else "False"
ppVal _             = "Error"

ppVCons :: Val -> String
ppVCons (VCons v vs)    = ppVal v ++ if vs == Empty then "" else " " ++ ppVCons vs
ppVCons v               = error "Tried to ppVCons non-VCons: " ++ show v

ppBindee :: Bindee -> String
ppBindee (BVal v)   = ppVal v
ppBindee _          = "list future or lenght future"

ppEnv :: Env -> String
ppEnv e     = "<\n" ++ show e ++ ">"

------------------------------------------------------------------------
-- EXAMPLES
------------------------------------------------------------------------

-- SMART CONSTRUCTORS
[f,x,n,l,r,m,k,t,y,k'] = map Var ["f","x","n","l","r","m","k","t","y","k'"]
[x0,x1,x2] = map EllipVar [0,1,2]

con :: Int -> Expr
con i = Value $ Con i

cons :: [Int] -> Expr
cons l = foldr1 Cons (map con l ++ [Value Empty]) 

ellipOne :: Expr -> Idx -> Idx -> Name -> Expr
ellipOne t ib ie n = Ellipsis t [EllipRange {var=n, ident=0, ib=ib, ie=ie, contentT = BeList}]


listToVCons :: [Val] -> Val
listToVCons  = foldr VCons Empty

vConsHead :: Val -> Val
vConsHead (VCons x _) = x
vConsHead Empty       = Empty
vConsHead _           = error "Tried to vConsHead non-VCons"

vConsTail :: Val -> Val
vConsTail (VCons _ xs) = xs
vConsTail Empty       = Empty
vConsTail _           = error "Tried to vConsTail non-VCons"

consHead :: Expr -> Expr
consHead (Cons x _) = x
consHead (Value Empty)     = Value Empty
consHead _           = error "Tried to consHead non-Cons"

consTail :: Expr -> Expr
consTail (Cons _ xs) = xs
consTail (Value Empty)     = Value Empty
consTail _           = error "Tried to consTail non-Cons"

{-
stripVal :: Val -> a
stripVal (Con i) = i
stripVal l@(VCons _ _) = map (stripVal) $ vConsToList l
stripVal Empty = []
stripVal (Closure _ _ _) = error "Can't strip a closure"
stripVar (FreeVar n) = n
stripVar (VPair v1 v2) = (stripVal v1, stripVal v2)
stripVar (VStr s) = s
stripVar (Boolean b) = b
-}

