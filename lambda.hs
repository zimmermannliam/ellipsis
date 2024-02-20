import Debug.Trace
import Data.Maybe
import Data.Generics.Schemes
import Data.Generics
import Data.Data
import Data.Either
import qualified Data.Map
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
          | Ellipsis' Expr EllipRanges
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
          | Or Expr Expr            -- Boolean Operators
          | Not Expr
          | And Expr Expr
          | Add Expr Expr           -- Arithmetic Operators
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Mod Expr Expr
          | Abs Expr
          | EllipVar Name Id
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
                deriving (Eq, Show, Data)

data EllipRange = EllipRange { ident :: Id
                             , var :: Name
                             , ib :: Idx
                             , ie :: Idx}
                             deriving (Eq, Show, Data)


newtype EllipError         = EllipError String
    deriving (Show)

type EllipRanges        = [EllipRange]
type Name               = String
type Env                = [(Name, Bindee)]
type Alts               = [(Pattern, Expr)]
type Id                 = Int



---------------------------------------------------------------------------------
-- SEMANTICS
-- ASSUME EVERY VARIABLE IS NAMED SEPARATELY
---------------------------------------------------------------------------------

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
    Closure x t1b e2 -> eval ((x, BVal $ eval e t2):e2) t1b
    _           -> errorOut e "Expected fn to be applied"

eval e (Abstr x t) = Closure x t e

eval e (Let n t1 t2) = eval ((n, BVal $ eval e t1):e) t2

eval e (Add t1 t2) = case (eval e t1, eval e t2) of
    (Con i1, Con i2)    -> Con (i1 + i2)
    (v1, v2)            -> errorOut' e $ "Bad add terms: " ++ show v1 ++ " + " ++ show v2

eval e (Sub t1 t2) = case (eval e t1, eval e t2) of
    (Con i1, Con i2)    -> Con (i1 - i2)
    _                   -> errorOut' e "Bad sub terms" 

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
    Closure {}  -> eval ((n, BVal $ eval e $ App ycomb (Abstr n t1)):e) t2
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
        _                      -> errorOut e "Tried to cat non-lists"

eval e (ListElement n i) = findListFutureElement e (envLookup e n) i

eval e (Error s) = errorOut e s

eval e (Ellipsis t rs) =
    case evalEllipsis e t rs of
        Right v                 -> v
        Left (EllipError err)   -> errorOut e $ "Error processing Ellipsis: " ++ err

eval e (Ellipsis' t rs) =
    case evalEllipsis e t rs of
        Right v                 -> v
        Left (EllipError err)   -> errorOut e $ "Error processing Ellipsis: " ++ err

eval e (Trace s tt t) = trace (s ++ ": " ++ ppVal (eval e tt)) $ eval e t

eval e (Eq t1 t2) = Boolean (eval e t1 == eval e t2)

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

eval e (EllipVar n i)   = errorOut e "Tried to evaluate an EllipVar"

-- eval e x              = errorOut e $ "Feature not implemented: " ++ show x

evalEllipsis :: Env -> Expr -> EllipRanges -> Either EllipError Val
evalEllipsis e t rs = do
    range <- calcRange e rs
    lists <- parseListErrors $ map (getList e) rs
    zippedList <- if length lists == length rs 
        then Right $ zip rs lists
        else Left $ EllipError "Lists and Ranges somehow unequal"
    iterateLists e zippedList t range

iterateLists :: Env -> [(EllipRange, Val)] -> Expr -> Int -> Either EllipError Val
iterateLists e ls t i 
    | i == 0 = Right Empty
    | otherwise = do
    let idToVal = Data.Map.fromList $ zip (map (ident . fst) ls) (map (vConsHead . snd) ls)
    VCons (eval e $ replaceEllipVars idToVal t) <$> iterateLists e (map (Data.Bifunctor.second vConsTail) ls) t (i-1)

replaceEllipVars :: Data.Map.Map Int Val -> Expr -> Expr
replaceEllipVars rs = everywhere (mkT $ replaceEllipVar rs)
    where   replaceEllipVar :: Data.Map.Map Int Val -> Expr -> Expr
            replaceEllipVar rs (EllipVar _ id)  = Value (rs Data.Map.! id)
            replaceEllipVar _ t                 = t


calcRange :: Env -> EllipRanges -> Either EllipError Int
calcRange e rs = if all (== head ranges) ranges 
    then Right $ head ranges 
    else Left $ EllipError "Unequal ranges"
    where fstIdxs = map (idxToInt e . ib) rs
          lstIdxs = map (idxToInt e . ie) rs
          ranges  = zipWith (\x y -> abs (x-y)+1) lstIdxs fstIdxs

getList :: Env -> EllipRange -> Either EllipError Val
getList e (EllipRange {var=var, ib=ib, ie=ie})
    | ie'' > len    = Left $ EllipError ("Index end went too high: " ++ show ib' ++ ".." ++ show ie' ++ "->"++show ib''++ ".."++show ie''++">"++show len)
    | ib'' < 1 || ie'' < 1  = Left $ EllipError ("Index went below 1: "++ show ib' ++ ".." ++ show ie' ++ "->"++ show ib''++".."++show ie''++"; list: "++show list')
    | otherwise = Right $ startingAtNthCons list' ib''
    where
        list = evalBinding e (ListFuture var)
        len = getLen e list
        ib' = idxToInt e ib
        ie' = idxToInt e ie
        list' = if ib' > ie' then reverseCons list else list
        (ib'', ie'') = if ib' > ie' then (len - ib' + 1, len - ie' + 1) else (ib', ie')

parseListErrors :: [Either EllipError Val] -> Either EllipError [Val]
parseListErrors l =
    let errors = lefts l
    in if null errors
        then Right $ rights l 
        else Left $ EllipError (foldl (\ x y -> show x ++ " " ++ show y) "Errors:" errors)

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

findListFutureElement :: Env -> Bindee -> Idx -> Val
findListFutureElement e (ListFuture n) i = findNthElement
        (evalBinding e (envLookup e n) ) 
        intIdx
    where   findNthElement :: Val -> Int -> Val
            findNthElement (VCons x xs) i    
                | i == 1    = x
                | i > 1     = findNthElement xs (i - 1)
                | i == 0    = Empty
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
                                                VCons x xs  -> Just $ eval ((n, BVal x):(ns,BVal xs):e) t2
                                                _           -> Nothing
patternMatch e t (PVar n, t2)             = Just $ eval ((n, BVal $ eval e t):e) t2 
patternMatch e t (PEllipsis n i, t2)      = Just $ eval ([(n, ListFuture n_t), 
                                                          (n_idx, LenFuture n_t)
                                                         ] ++ e) t2
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
        (FreeVar nl, FreeVar nls, VCons x xs) -> Just $ eval ((nl, BVal x):(nls, BVal xs):e) t2
        (FreeVar nl, vls',        VCons x xs) -> if vls' == xs then Just $ eval ((nl, BVal x):e) t2 else Nothing
        (vl',        FreeVar nls, VCons x xs) -> if vl' == x then Just $ eval ((nls, BVal xs):e) t2 else Nothing
        (vl',        vls',        VCons x xs) -> if vl' == x && vls' == xs then Just $ eval e t2 else Nothing
        (_,          _,           _         ) -> Nothing
        
-- patternMatch e t _                        = Nothing

-- Find a variable in environment
envLookup :: Env -> Name -> Bindee
envLookup [] name                       = BVal $ FreeVar name 
                                        --error $ "envLookup: can't find " ++ name ++ " in env"
envLookup ((env_name, env_val):xs) name = if name == env_name then env_val
                                          else envLookup xs name

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


------------------------------------------------------------------------
-- PARSE
------------------------------------------------------------------------

parse :: String -> Expr
parse _ = Value Empty

------------------------------------------------------------------------
-- PRINT
------------------------------------------------------------------------

pp :: Expr -> String
pp (Var n)      = n
pp (App e1 e2)  = pp e1 ++ " " ++ pp e2
pp (Abstr n e)  = "\\" ++ n ++ ".(" ++ pp e ++ ")"
pp (Value v)    = ppVal v
pp (Let n e1 e2)        = "Let " ++ n ++ " = (" ++ pp e1 ++ ") in " ++ pp e2
pp (Case e a)           = "Case " ++ pp e ++ " of {" ++ ppMatch a ++ "\n}"
pp (LetRec  n e1 e2)    = "Letrec " ++ n ++ " = (" ++ pp e1 ++ ")\nin " ++ pp e2
pp (Cons e1 e2)         = "(" ++ pp e1 ++ " : " ++ pp e2 ++ ")"
-- pp (Ellipsis t start end _) = strt ++ ppIdx start ++ " ... " ++ strt ++ ppIdx end
                                where strt = pp t
pp (Pair t1 t2) = "(" ++ pp t1 ++ ", " ++ pp t2 ++ ")"
pp (Cat t1 t2) = pp t1 ++ " ++ " ++ pp t2
pp (ListElement n i) = n ++ "[" ++ ppIdx i ++ "]"
pp (Add e1 e2)          = pp e1 ++ "+" ++ pp e2
pp (Sub t1 t2)          = pp t1 ++ "-" ++ pp t2
pp (Mul t1 t2)          = pp t1 ++ "*" ++ pp t2
pp (Div t1 t2)          = pp t1 ++ "/" ++ pp t2
pp (Mod t1 t2)          = pp t1 ++ "%" ++ pp t2
pp (Abs t)              = "|" ++ pp t ++ "|"
pp (Trace _ _ t)        = pp t
pp (Eq t1 t2)           = pp t1 ++ " == " ++ pp t2
pp (Lt t1 t2)           = pp t1 ++ " < " ++ pp t2
pp (Gt t1 t2)           = pp t1 ++ " > " ++ pp t2
pp (Leq t1 t2)          = pp t1 ++ " <= " ++ pp t2
pp (Geq t1 t2)          = pp t1 ++ " >= " ++ pp t2
pp (Or t1 t2)           = pp t1 ++ " || " ++ pp t2
pp (And t1 t2)          = pp t1 ++ " && " ++ pp t2
pp (Not t)              = "!(" ++ pp t ++ ")"
pp (Error s)            = "error " ++ s
pp _            = "Error -- cannot display expression"

ppMatch :: Alts -> String
ppMatch []              = ""
ppMatch ((p, e):as)     = "\n\t" ++ ppPattern p ++ " -> " ++ pp e ++ ppMatch as

ppPattern :: Pattern -> String
ppPattern (PCons n1 n2) = "(" ++ n1 ++ ":" ++ n2 ++ ")"
ppPattern (PVar n)      = n
ppPattern (PVal v)      = ppVal v
ppPattern (PEllipsis n i) = strn ++ "1 ... " ++ strn ++ ppIdx i
                        where strn = n
ppPattern (PCons' t1 t2) = "(" ++ pp t1 ++ ":" ++ pp t2 ++ ")"

ppIdx :: Idx -> String
ppIdx (IPlace i)   = show i
ppIdx (End n)     = n
ppIdx (EPlace t) = pp t

ppVal :: Val -> String
ppVal (Con i)       = show i
ppVal (VCons v vs)   = ppVal v ++ " " ++ ppVal vs
ppVal Empty       = "Empty"
ppVal (Closure n t e)   = "CLOSURE"-- "CLOSURE( " ++ ppEnv e ++ "|-" ++ pp (Abstr n t) ++ ")"
ppVal (FreeVar n)   = n
ppVal (VPair v1 v2) = "(" ++ ppVal v1 ++ ", " ++ ppVal v2 ++ ")"
ppVal (Boolean b)   = if b then "True" else "False"
ppVal _             = "Error"

ppBindee :: Bindee -> String
ppBindee (BVal v)   = ppVal v
ppBindee _          = "list future or lenght future"

ppEnv :: Env -> String
ppEnv e     = "<\n" ++ ppEnv' e ++ ">"
    where
    ppEnv' ((n,v):es)   = n ++ " <- " ++ ppBindee v ++ "\n" ++ ppEnv' es
    ppEnv' []           = ""

------------------------------------------------------------------------
-- EXAMPLES
------------------------------------------------------------------------

-- SMART CONSTRUCTORS
[f,x,n,l,r,m,k,t,y] = map Var ["f","x","n","l","r","m","k","t","y"]
[x0,x1] = map (EllipVar "x") [0,1]

con :: Int -> Expr
con i = Value $ Con i

cons :: [Int] -> Expr
cons l = foldr1 Cons (map con l ++ [Value Empty]) 

ellipOne :: Expr -> Idx -> Idx -> Name -> Expr
ellipOne t ib ie vn = Ellipsis t [EllipRange {ib=ib, ident=0, ie=ie, var=vn}]

listToVCons :: [Val] -> Val
listToVCons  = foldr VCons Empty

vConsHead :: Val -> Val
vConsHead (VCons x _) = x
vConsHead _           = error "Tried to vConsHead non-VCons"

vConsTail :: Val -> Val
vConsTail (VCons _ xs) = xs
vConsTail _           = error "Tried to vConsTail non-VCons"

consHead :: Expr -> Expr
consHead (Cons x _) = x
consHead _           = error "Tried to consHead non-Cons"

consTail :: Expr -> Expr
consTail (Cons _ xs) = xs
consTail _           = error "Tried to consTail non-Cons"

exList' = [1..5]
exList2' = [8,14,32,0,4]
exList3' = [1,2,13,24,25,45,64,84,99,100]
exList4' = [1..10]

myCons = map cons [exList', exList2', exList3', exList4']
[exList, exList2, exList3, exList4] = myCons
[exListV, exList2V, exList3V, exList4V] = map (eval prelude) myCons

-- PRELUDE FUNCTIONS
prelude :: Env
prelude = [("cmp", BVal $ eval [] cmp')]
[cmp] = map Var ["cmp"]

cmp' :: Expr
cmp' = Abstr "l" $ Abstr "r" $ Case (Sub l r) -- of
    [
        (PVal $ Con 0, Value $ Con 0),
        (PVar "x", Div x (Abs x))
    ]



succ' :: Expr
succ' = Abstr "x" $ Add (Var "x") (Value $ Con 1)

head' :: Expr
head' = Abstr "l" $ Case (Var "l") [(PVal Empty, Value Empty), (PCons "H" "T", Var "H")]

tail' :: Expr
tail' = Abstr "l" $ Case (Var "l") [(PVal Empty, Value Empty), (PCons "H" "T", Var "T")]

tail3' :: Expr
tail3' = Abstr "l" $ Case (Var "l") -- of
    [
        (PEllipsis "x" (End "n"), ellipOne (Var "x") (IPlace 2) (End "n") "x")
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
            (ellipOne (Var "x") (IPlace 1) (EPlace $ Sub (Var "n") (Value $ Con 1)) "x")
            (ellipOne (Var "x") (EPlace $ Add (Var "n") (Value $ Con 1)) (End "m") "x")
            )
    ]

firstN :: Expr
firstN = Abstr "l" $ Abstr "n" $
    Case (Var "l") -- of
    [
        (PEllipsis "x" (End "m"),   ellipOne (Var "x") (IPlace 1) (EPlace $ Var "n") "x")
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
                            (ellipOne x (IPlace 1) (EPlace $ Div (Var "n") (Value $ Con 2)) "x")
                            (ellipOne x (EPlace $ Add (Div (Var "n") (Value $ Con 2)) (Value $ Con 1)) (EPlace $ Var "n") "x"))
    ]

listId' :: Expr
listId' = Abstr "l" $ Case (Var "l") -- of
    [
        (PEllipsis "x" (End "n"),   ellipOne (Var "x") (IPlace 1) (End "n") "x")
    ]

binSearch' :: Expr
binSearch' = Abstr "list" $ Abstr "term" (LetRec "binSearch" (Abstr "l" $ Abstr "t" $ Trace "l" l $ Case (Var "l") -- of
    [
        (PVal Empty,    Value $ Con 0),
        (PCons' (Var "x") (Value Empty), Case (App (App cmp x) t) -- of
            [
                (PVal $ Con 0, Value $ Con 1),
                (PVar "_", Value $ Con 0)
            ]),
        (PEllipsis "x" (End "n"),     Let "k" (Div n (Value $ Con 2)) $ 
            Case (App (App cmp (ListElement "x" (EPlace k))) t)
            [
                (PVal $ Con 0,    Value $ Con 1),
                (PVal $ Con 1, App (App (Var "binSearch") (ellipOne x0 
                                                                        (IPlace 1) 
                                                                        (EPlace $ Sub k (Value $ Con 1))
                                                                        "x")) 
                                                                        t),
                (PVal $ Con (-1), App (App (Var "binSearch") (ellipOne x0
                                                                        (EPlace $ Add k (Value $ Con 1)) 
                                                                        (End "n")
                                                                        "x"))
                                                                        t)
            ]
        )
    ]) -- in
    (App (App (Var "binSearch") (Var "list")) (Var "term"))
    )


{-
mergeSort' :: Expr
mergeSort' = Abstr "l" $ LetRec "mergeSort" -- =
    (Abstr "list" $ Case (Var "list") -- of
    [
        (PVal Empty, Value Empty),
        (PEllipsis "x" (End "n"), 

        )
    ])
    -- in
    (App (Var "mergeSort") (Var "l"))

merge' :: Expr
merge = Abstr "l1" $ Abstr "l2"
-}

