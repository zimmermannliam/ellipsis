import Debug.Trace
import Data.Maybe
import Data.Generics
import Data.Data
import Data.Either
import Data.List
import qualified Data.Map as Map
import qualified Data.Bifunctor

{-# LANGUAGE DeriveDataTypeable #-}

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
head' (x:xs) = x

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
        then empty
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

-- eval e x              = errorOut e $ "Feature not implemented: " ++ show x

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
boundsCheck e ellipEnv = all ((== True) . boundsCheck' e) iterators
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
patternMatch e t (PEllipsis n i, t2)      = Just $ eval (Map.fromList [(NamedVar n, ListFuture n_t), 
                                                          (NamedVar n_idx, LenFuture n_t)
                                                         ] `Map.union` e) t2
                    where   n_idx = case i of
                                (End n) -> n
                                _       -> error "Tried to unpack a bad end idx for ellipsis pattern"
                            n_t = case t of
                                (Var n) -> n
                                _       -> error "Tried to unpack bad name-term for ellipsis pattern"
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


exList' = [1..5]
exList2' = [8,14,32,0,4]
exList3' = [1,2,13,24,25,45,64,84,99,100]
exList4' = [1..10]
exList5' = [1]
exList6' = [1,2]

myCons = map cons [exList', exList2', exList3', exList4', exList5', exList6']
[exList, exList2, exList3, exList4, exList5, exList6] = myCons
[exListV, exList2V, exList3V, exList4V, exList5V, exList6V] = map (eval prelude) myCons

-- PRELUDE FUNCTIONS
prelude :: Env
prelude = Map.fromList [(NamedVar "cmp", BVal $ eval Map.empty cmp')]
[cmp] = map Var ["cmp"]

cmp' :: Expr
cmp' = Abstr "l" $ Abstr "r" $ Case (Sub l r) -- of
    [
        (PVal $ Con 0, Value $ Con 0),
        (PVar "x", Div x (Abs x))
    ]



succ' :: Expr
succ' = Abstr "x" $ Add (Var "x") (Value $ Con 1)

-- head' :: Expr
-- head' = Abstr "l" $ Case (Var "l") [(PVal Empty, Value Empty), (PCons "H" "T", Var "H")]

tail' :: Expr
tail' = Abstr "l" $ Case (Var "l") [(PVal Empty, Value Empty), (PCons "H" "T", Var "T")]

tail3' :: Expr
tail3' = Abstr "l" $ Case (Var "l") -- of
    [
        (PEllipsis "x" (End "n"), ellipOne x0 (IPlace 2) (End "n") "x")
    ]

removeNth' :: Expr
removeNth' = Abstr "l" $ Abstr "i" $ 
            LetRec "removeNth" (Abstr "list" $ Abstr "idx" $ Case (Var "idx")
            [
                (PVal $ Con 0, Case (Var "list")
                [
                    (PCons "x" "xs", Var "xs"),
                    (PVal Empty, Error "zero idx out of bounds"),
                    (PVar "_", Error "zero idx match error!")
                ]),
                (PVar "idx'", Case (Var "list")
                [
                    (PCons "x" "xs", Cons (Var "x") 
                                           (App (App (Var "removeNth") 
                                               (Var "xs")) 
                                               (Add (Var "idx'") (Value $ Con (-1)))
                                           )
                    ),
                    (PVal Empty, Error "nonzero idx out of bounds"),
                    (PVar "_", Error "nonzero idx match error!")
                ])
            ])
            -- in
            (App (App (Var "removeNth") (Var "l")) (Var "i"))

removeNth3' :: Expr
removeNth3' = Abstr "l" $ Abstr "n" $
    Case (Var "l") -- of
    [
        (PEllipsis "x" (End "m"), Cat
            (ellipOne x0 (IPlace 1) (EPlace $ Sub (Var "n") (Value $ Con 1)) "x")
            (ellipOne x0 (EPlace $ Add (Var "n") (Value $ Con 1)) (End "m") "x")
            )
    ]

firstN :: Expr
firstN = Abstr "l" $ Abstr "n" $
    Case (Var "l") -- of
    [
        (PEllipsis "x" (End "m"),   ellipOne x0 (IPlace 1) (EPlace $ Var "n") "x")
    ]

add' :: Expr
add' = Abstr "a" $ Abstr "b" $ Add (Var "a") (Var "b")

map3' :: Expr
map3' = Abstr "l" $ Abstr "f" $ Case l -- of
    [
        (PEllipsis "x" (End "n"), ellipOne (App f x0) (IPlace 1) (End "n") "x")
    ]

map' :: Expr
map' = Abstr "l" $ Abstr "f" $
        LetRec "map" (Abstr "list" $ Abstr "fun" $ Case (Var "list")
        [
            (PVal Empty, Value Empty),
            (PCons "x" "xs", Cons (App (Var "fun") (Var "x")) (App (App (Var "map") (Var "xs")) (Var "fun")))
        ])
            -- in
            (App (App (Var "map") (Var "l")) (Var "f"))


fold' :: Expr
fold' = Abstr "l" $ Abstr "f" $
        LetRec "fold" (Abstr "list" $ Abstr "fun" $ Case (Var "list")
        [
            (PCons "x" "xs", Case (Var "xs")
            [
                (PVal Empty, Var "x"),
                (PVar "xs'",    App 
                                (App (Var "fun") (Var "x")) 
                                (App (App (Var "fold") (Var "xs")) (Var "fun"))
                                )
            ])
        ])
        -- in
        (App (App (Var "fold") (Var "l")) (Var "f"))


reverse' :: Expr
reverse'    = Abstr "l" $ LetRec "reverse"
            ( Abstr "list" $ Case (Var "list")
            [
                (PVal Empty, Value Empty),
                (PCons "x" "xs", Cat (App (Var "reverse") (Var "xs")) (Cons (Var "x") (Value Empty)))
            ])
            -- in
            (App (Var "reverse") (Var "l"))


reverse3' :: Expr
reverse3' = Abstr "l" $ Case (Var "l") -- of
    [
        (PEllipsis "x" (End "n"),   ellipOne x0 (End "n") (IPlace 1) "x")
    ]

second' :: Expr
second' = Abstr "l" $ Case (Var "l")
            [
                (PCons "x" "xs", Case (Var "xs")
                                 [ 
                                    (PCons "y" "ys", Var "y"),
                                    (PVal Empty, Value Empty)
                                 ]),
                (PVal Empty, Value Empty)
            ]

second2' :: Expr
second2' = Abstr "l" $ Case (Var "l") -- of
    [
        (PEllipsis "x" (End "n"), ListElement "x" (IPlace 2))
    ]


nth' :: Expr
nth' = Abstr "l" $ Abstr "n" $ 
            LetRec "nth" (Abstr "list" $ Abstr "cur" $ Case (Var "cur") 
                [
                    (PVal $ Con 0, Case (Var "list")
                    [
                        (PCons "x" "xs", Var "x"), 
                        (PVal Empty, Value Empty),
                        (PVar "_", Var "_")
                    ]),
                    (PVar "rem", Case (Var "list")
                    [
                        (PCons "x" "xs", App (App (Var "nth") (Var "xs")) (Add (Var "rem") (Value $ Con (-1)))),
                        (PVal Empty, Value Empty),
                        (PVar "_", Var "_")
                    ])
                ]
            ) $ App (App (Var "nth")  (Var "l")) (Var "n")

nth2' :: Expr
nth2' = Abstr "l" $ Abstr "n" $ Case (Var "l")
        -- of
        [
            (PEllipsis "x" (End "m"), ListElement "x" (EPlace $ Var "n"))
        ]

-- Can't do zip, type-wise. Need a value-list
zip' :: Expr
zip' = Abstr "l1" $ Abstr "l2" $ 
            LetRec "zip" (Abstr "list1" $ Abstr "list2" $ Case (Var "list1")
            [
                (PVal Empty, Value Empty),
                (PCons "x" "xs", Case (Var "list2")
                [
                    (PVal Empty, Value Empty),
                    (PCons "y" "ys", Cons 
                        (Pair (Var "x") (Var "y")) 
                        (App (App (Var "zip") (Var "xs")) (Var "ys"))
                    )
                ])
            ])
            -- in
            (App (App (Var "zip") (Var "l1")) (Var "l2"))

{-
zip2' :: Expr
zip2 = Abstr "l1" $ Abstr "l2" $ Case (Var "l1") -- of
    [
        (PEllipsis "x" (End "n") , Case 
    ]
    -}

find' :: Expr
find' = Value $ Con 1

sum' :: Expr
sum' = Abstr "l" $ LetRec "sum" (Abstr "list" $ Case (Var "list") 
    [
        (PVal Empty, Value $ Con 0), 
        (PCons "x" "xs", Add (Var "x") (App (Var "sum") (Var "xs")))
    ]) 
    -- in
    (App (Var "sum") (Var "l"))

len' :: Expr
len' = Abstr "l" $ LetRec "len" (Abstr "list" $ Case (Var "list")
    [
        (PVal Empty, Value $ Con 0),
        (PCons "x" "xs", Add (Value $ Con 1) (App (Var "len") (Var "xs")))
    ])
    -- in
    (App (Var "len") (Var "l"))

len2' :: Expr
len2' = Abstr "l" $ Case (Var "l")
    [
        (PEllipsis "x" (End "n"), Var "n")
    ]

sumNum' :: Expr
sumNum' = Abstr "n" $ LetRec "sumNum" (Abstr "x" $ Case (Var "x") [(PVal $ Con 0, Value $ Con 0), (PVar "y", Add (Var "y") (App (Var "sumNum") (Add (Var "y") (Value $ Con (-1)))))]) $ App (Var "sumNum") (Var "n")

sucEach' :: Expr
sucEach' = Abstr "l" $ LetRec "sucEach" (Abstr "list" $ Case (Var "list")
    [
        (PVal Empty, Value Empty),
        (PCons "x" "xs", Cons (Add (Var "x") (Value $ Con 1)) (App (Var "sucEach") (Var "xs")))
    ]
    )
    -- in
    (App (Var "sucEach") (Var "l"))

fst' :: Expr
fst' = Abstr "a" $ Abstr "b" $ Var "a"

snd' :: Expr
snd' = Abstr "a" $ Abstr "b" $ Var "b"

id' :: Expr
id' = Abstr "l" (LetRec "id" (Abstr "list" $ Case (Var "list")
    [
        (PVal Empty, Value Empty),
        (PCons "x" "xs", Cons (Var "x") (App (Var "id") (Var "xs")))
    ])
    -- in
    (App (Var "id") (Var "l")))


split' :: Expr
split' = Abstr "l" $ Case l -- of
    [
        (PEllipsis "x" (End "n"), Pair 
                            (ellipOne x0 (IPlace 1) (EPlace $ Div (Var "n") (Value $ Con 2)) "x")
                            (ellipOne x0 (EPlace $ Add (Div (Var "n") (Value $ Con 2)) (Value $ Con 1)) (EPlace $ Var "n") "x"))
    ]

listId' :: Expr
listId' = Abstr "l" $ Case (Var "l") -- of
    [
        (PEllipsis "x" (End "n"),   ellipOne x0 (IPlace 1) (End "n") "x")
    ]

binSearch' :: Expr
binSearch' = Abstr "list" $ Abstr "term" (LetRec "binSearch" (Abstr "l" $ Abstr "t" 
    -- $ Trace "l" l 
    $ Case (Var "l") -- of
    [
        (PVal Empty,    Value $ Boolean False),
        (PEllipsis "x" (End "n"),     Let "k" (Add (Div n (Value $ Con 2)) (Value $ Con 1)) $ 
            Case (ListElement "x" (End "k") `Eq` t)
            [
                (PVal $ Boolean True, Value $ Boolean True),
                (PVal $ Boolean False, Case (ListElement "x" (End "k") `Gt` t)
                [
                    (PVal $ Boolean True, App (App 
                        (Var "binSearch") 
                        (ellipOne x0 
                            (IPlace 1) 
                            (EPlace $ Sub k (Value $ Con 1))
                            "x")) 
                        t),
                    (PVal $ Boolean False, App (App
                        (Var "binSearch")
                        (ellipOne x0
                            (EPlace $ Add k (Value $ Con 1)) 
                            (End "n")
                            "x"))
                        t)
                ])
            ]
        )
    ]) -- in
    (App (App (Var "binSearch") (Var "list")) (Var "term"))
    )

pairAdj' :: Expr
pairAdj' = Abstr "l" $ Case l -- of
    [
        (PEllipsis "x" (End "n"), Ellipsis (Pair x0 x1) [EllipRange {ident=0, var="x", ib=IPlace 1, ie=EPlace $ Sub n (Value $ Con 1), contentT = BeList}, EllipRange {ident=1, var="x", ib=IPlace 2, ie=End "n", contentT = BeList}])
    ]

rotL' :: Expr
rotL' = Abstr "l" $ Abstr "k" $ Case l -- of
    [
        (PEllipsis "x" (End "n"), Let "k'" (k `Mod` n) $
        ellipOne x0 (EPlace $ k' `Add` Value (Con 1)) (EPlace n) "x"
            `Cat` ellipOne x0 (IPlace 1) (EPlace k') "x")
    ]

-- Unfinsihed
rotR' :: Expr
rotR' = Abstr "l" $ Abstr "k" $ Case l -- of
    [
        (PEllipsis "x" (End "n"), Let "k'" (k `Mod` n) $
        ellipOne x0 (EPlace $ k' `Add` Value (Con 1)) (EPlace n) "x"
            `Cat` ellipOne x0 (IPlace 1) (EPlace k') "x")
    ]

subArrays' :: Expr
subArrays' = Abstr "l" $ Abstr "k" $ Case l -- of
    [
        (PVal Empty, Value Empty),
        (PEllipsis "x" (End "n"), Ellipsis (Ellipsis x0 [EllipRange {ident=0, var="x", ib=EPlace x1, ie=EPlace (x1 `Add` k `Sub` Value (Con 1)), contentT=BeList}]) [EllipRange {ident=1, var="x", ib=IPlace 1, ie=EPlace (n `Sub` k `Add` Value (Con 1)), contentT=BeIndices}])
    ]

indices' :: Expr
indices' = Abstr "l" $ Case l -- of
    [
        (PEllipsis "x" (End "n"), Ellipsis x0 [EllipRange {ident=0, var="", ib=IPlace 1, ie=EPlace n, contentT=BeIndices}])
    ]

favorites = [("binSearch", binSearch'), ("pairAdj", pairAdj'), ("rotL", rotL')]

printFavorites = putStrLn $ foldl (\x y -> x++"\n\n"++y) "\nFavorite examples:" $ map (\(n, e) -> n ++ ": \n" ++ pp e) favorites

{-
mergeSort' :: Expr
mergeSort' = Abstr "l" $ LetRec "mergeSort" -- =
    (Abstr "list" $ Case (Var "list") -- of
    [
        (PVal Empty, Value Empty),
        (PEllipsis "x" (End "n"), 

        )
        (PEllipsis "x" (End "n"), )
    -- in
    (App (Var "mergeSort") (Var "l"))

merge' :: Expr
merge = Abstr "l1" $ Abstr "l2"
-}

