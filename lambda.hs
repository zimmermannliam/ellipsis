import Debug.Trace
import Data.Maybe
import Data.Generics.Schemes
import Data.Generics
import Data.Data

{-# LANGUAGE DeriveDataTypeable #-}

---------------------------------------------------------------------------------
-- SYNTAX
---------------------------------------------------------------------------------

data Expr = Var Name
          | App Expr Expr
          | Abstr Name Expr
          | Value Val
          | Let Name Expr Expr
          | Case Expr Alts
          | LetRec Name Expr Expr -- LetRec "Name" = Expr in Expr
          | Fix Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Abs Expr
          | Tail Expr
          | Ellipsis Expr Idx Idx -- For y1 ... yn: EllipsesE y 1 n
          | Ellipsis' Expr Idx Idx Name
          | Cons Expr Expr
          | Cat Expr Expr
          | Error String
          | Report Expr
          | Pair Expr Expr
          | ListElement Name Idx
          | Trace String Expr Expr
          deriving (Eq, Show, Data)

data Val = Con Int 
          | ICons Int Val 
          | VCons Val Val
          | Empty 
          | Closure Name Expr Env
          | Fun Name Expr
          | Plus Val Val
          | Fixed Expr
          | FreeVar Name
          | VPair Val Val
          | VStr String
         deriving (Eq, Show, Data)

data Pattern = PCons Name Name
             | PVar Name
             | PVal Val
             | PEllipsis Name Idx -- x1 ... xn -> x2 ... xn
             deriving (Eq, Show, Data)

data Idx    = IPlace Int        -- Reflect work they're doing -- 
            | End Name 
            | EPlace Expr -- Must evaluate to an integer
            deriving (Eq, Show, Data)

-- Bindee?
data Binding    = BVal Val 
                | ListFuture Name 
                | LenFuture Name
                deriving (Eq, Show, Data)

-- create an iterator
-- 

-- \l. case l of
-- x1 ... xn --> [("l", a list), ("x", ListFuture "l"), ("n", LenFuture "l")]
-- 

type Name = String

type Env = [(Name, Binding)]

type Alts = [(Pattern, Expr)]



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
eval e (Var vn)            = evalBinding (envLookup e vn) e
eval e (App t1 t2)         = case eval e t1 of 
                                Closure x t1b e2 -> eval ((x, BVal $ eval e t2):e2) t1b
                                _           -> errorOut e "Expected fn to be applied"
eval e (Abstr x t)         = Closure x t e
eval e (Let n t1 t2)       = eval ((n, BVal $ eval e t1):e) t2
eval e (Add t1 t2)         = case (eval e t1, eval e t2) of
                                (Con i1, Con i2)    -> Con (i1 + i2)
                                _                   -> errorOut' e "Bad add terms" 
eval e (Sub t1 t2)         = case (eval e t1, eval e t2) of
                                (Con i1, Con i2)    -> Con (i1 - i2)
                                _                   -> errorOut' e "Bad sub terms" 
eval e (Mul t1 t2)          = case (eval e t1, eval e t2) of
                                (Con i1, Con i2)    -> Con (i1 * i2)
                                _                   -> errorOut' e "Bad mul terms"
eval e (Div t1 t2)          = case (eval e t1, eval e t2) of
                                (Con i1, Con i2)    -> Con (i1 `div` i2)
                                _                   -> errorOut' e "Bad div terms"
eval e (Abs t)              = case eval e t of
                                (Con i)             -> Con (abs i)
                                _                   -> errorOut' e "Bad abs term"
eval e (LetRec n t1 t2)    = case eval e t1 of
                                Closure {}  -> eval ((n, BVal $ eval e $ App ycomb (Abstr n t1)):e) t2
                                                    -- fn = fix (\fn.t1)
                                _           -> errorOut e "Expected fn to be letrecced"
eval e (Value v)            = v
eval e (Tail t)             = case eval e t of
                                (ICons _ tail)           -> tail
                                _                       -> errorOut e ("not a list " ++ show t)
eval e (Case tc ps)   = patternMatchEval e tc ps
eval e (Cons t1 t2)  = VCons (eval e t1) (eval e t2)
eval e (Pair t1 t2)  = VPair (eval e t1) (eval e t2)
eval e (Cat t1 t2)    = let et1 = iconsToVCons $ eval e t1
                            et2 = iconsToVCons $ eval e t2
                        in case (et1, et2) of
                            (VCons _ _, VCons _ _) -> catVCons et1 et2
                            (VCons _ _, Empty)     -> et1
                            (Empty, VCons _ _)     -> et2
                            _                      -> errorOut e "Tried to cat non-lists"
eval e (ListElement n i)    = findListFutureElement e (envLookup e n) i
eval e (Error s)      = errorOut e s
eval e (Ellipsis' t ib ie vn) =
    let list = evalBinding (envLookup e vn) e
    in evalEllipsis e list t vn ib ie
eval e (Trace s tt t)        = trace (s ++ ": " ++ ppVal (eval e tt)) $ eval e t
eval e x              = errorOut e $ "Feature not implemented: " ++ show x

evalEllipsis' :: Env -> Expr -> Idx -> Idx -> Name -> Val
evalEllipsis' e t ib ie n = eval e $ listToCons realizedList
    where realizedList = [ realizeComprehension n (IPlace i) t | i <- forIdx e ib ie ]

evalEllipsis :: Env -> Val -> Expr -> Name -> Idx -> Idx -> Val
evalEllipsis e l t n ib ie = 
    let ib'     = idxToInt e ib
        ie'     = idxToInt e ie
        range   = ie' - ib'
        l'      = if range < 0 then reverseCons l else l
        ib''    = if range < 0 then getLen l' - ib' + 1 else ib'
        l''     = startingAtNthCons l' ib''
    in 
    if ie' < 1 then Empty
    else iterateCons e l'' t n (abs range + 1)

reverseCons :: Val -> Val
reverseCons l = eval [] $ cons [1,2,3] -- reverseCons' l
    where
    reverseCons' :: Val -> [Val]
    reverseCons' (VCons x xs)    = reverseCons' xs ++ [x]
    reverseCons' (Empty)  = []

startingAtNthCons :: Val -> Int -> Val
startingAtNthCons l 1            = l
startingAtNthCons (VCons x xs) i  = startingAtNthCons xs (i - 1)
startingAtNthCons Empty _        = Empty
startingAtNthCons l i            = error $ "startingAtNthCons unexpected values: l=" ++ show l ++ "i = " ++ show i

iterateCons :: Env -> Val -> Expr -> String -> Int -> Val
iterateCons _ _ _ _ 0     = Empty
iterateCons e (Empty) _ _ i = Empty -- errorOut e $ "Empty too soon; i: " ++ show i
iterateCons e (VCons x xs) t n i   = VCons (eval e $ replaceVar n x t) (iterateCons e xs t n (i-1))
iterateCons e _ _ _ _     = errorOut' e "Non-cons somehow in iterator"

replaceVar :: String -> Val -> Expr -> Expr
replaceVar n valin tout = everywhere (mkT $ replaceVar' n valin) tout

replaceVar' :: String -> Val -> Expr -> Expr
replaceVar' n valin (Var n')  = if n == n' then Value valin else Var n'
replaceVar' _ _ t           = t

forIdx :: Env -> Idx -> Idx -> [Int]
forIdx e ib ie = fill (idxToInt e ib) (idxToInt e ie)

idxToInt :: Env -> Idx -> Int
idxToInt e (IPlace i)   = i
idxToInt e (End n)      = valEvalInt $ eval e (Var n)
idxToInt e (EPlace t)   = valEvalInt $ eval e t

listToCons :: [Expr] -> Expr
listToCons []       = Value Empty
listToCons (t:ts)   = Cons t (listToCons ts)

fill :: Int -> Int -> [Int]
fill b e    = if b < e then [b .. e] else fillBack b e
    where
    fillBack :: Int -> Int -> [Int]
    fillBack e b = if e >= b then [e] ++ fillBack (e - 1) b else []

realizeComprehension :: Name -> Idx -> Expr -> Expr
realizeComprehension n i t = everywhere (mkT $ replaceVarWithElement n i) t
    where   replaceVarWithElement :: Name -> Idx -> Expr -> Expr
            replaceVarWithElement n1 i (Var n2) = if n1 == n2 then ListElement n1 i else Var n2
            replaceVarWithElement _ _ t         = t

findListFutureElement :: Env -> Binding -> Idx -> Val
findListFutureElement e (ListFuture n) i = findNthElement
        (iconsToVCons $ evalBinding (envLookup e n) e) 
        intIdx
    where   findNthElement :: Val -> Int -> Val
            findNthElement (VCons x xs) i    =  if i == 1 
                                                    then x 
                                                else if i > 0
                                                    then findNthElement xs (i - 1)
                                                else if i == 0
                                                    then Empty
                                                else
                                                    error $ "ListElement: bad i: " ++ show i
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

iconsToVCons :: Val -> Val
iconsToVCons (ICons x xs)   = VCons (Con x) (iconsToVCons xs)
iconsToVCons x              = x

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
                                                ICons x xs  -> Just $ eval ((n,BVal $ Con x):(ns,BVal xs):e) t2
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
-- patternMatch e t _                        = Nothing

-- Find a variable in environment
envLookup :: Env -> Name -> Binding
envLookup [] name                       = BVal $ FreeVar name 
                                        --error $ "envLookup: can't find " ++ name ++ " in env"
envLookup ((env_name, env_val):xs) name = if name == env_name then env_val
                                          else envLookup xs name
getLen :: Val -> Int
getLen (VCons x xs) = 1 + getLen xs
getLen (ICons x xs) = 1 + getLen xs
getLen Empty        = 0
getLen term         = error ("getLen encountered bad var: " ++ show term)

evalBinding :: Binding -> Env -> Val
evalBinding (BVal v) e      = v
evalBinding (LenFuture n) e  = Con $ getLen boundList
    where   boundList :: Val
            boundList = case envLookup e n of
                BVal v  -> v
                _       -> error "Tried to bind list without value"
evalBinding (ListFuture n) e = evalBinding (envLookup e n) e

pp :: Expr -> String
pp (Var n)      = n
pp (App e1 e2)  = pp e1 ++ " " ++ pp e2
pp (Abstr n e)  = "\\" ++ n ++ ".(" ++ pp e ++ ")"
pp (Value v)    = ppVal v
pp (Let n e1 e2)        = "Let " ++ n ++ " = (" ++ pp e1 ++ ") in " ++ pp e2
pp (Case e a)           = "Case " ++ pp e ++ " of {" ++ ppMatch a ++ "\n}"
pp (LetRec  n e1 e2)    = "Letrec " ++ n ++ " = (" ++ pp e1 ++ ")\nin " ++ pp e2
pp (Cons e1 e2)         = "(" ++ pp e1 ++ " : " ++ pp e2 ++ ")"
pp (Ellipsis n start end)   = strn ++ ppIdx start ++ " ... " ++ strn ++ ppIdx end
                                where strn = pp n
pp (Ellipsis' t start end _) = strt ++ ppIdx start ++ " ... " ++ strt ++ ppIdx end
                                where strt = pp t
pp (Pair t1 t2) = "(" ++ pp t1 ++ ", " ++ pp t2 ++ ")"
pp (Cat t1 t2) = pp t1 ++ " ++ " ++ pp t2
pp (ListElement n i) = n ++ "[" ++ ppIdx i ++ "]"
pp (Add e1 e2)          = pp e1 ++ "+" ++ pp e2
pp (Sub t1 t2)          = pp t1 ++ "-" ++ pp t2
pp (Mul t1 t2)          = pp t1 ++ "*" ++ pp t2
pp (Div t1 t2)          = pp t1 ++ "/" ++ pp t2
pp (Abs t)              = "|" ++ pp t ++ "|"
pp (Trace _ _ t)        = pp t
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
ppIdx :: Idx -> String
ppIdx (IPlace i)   = show i
ppIdx (End n)     = n
ppIdx (EPlace t) = pp t

ppVal :: Val -> String
ppVal (Con i)       = show i
ppVal (ICons i v)    = show i ++ " " ++ ppVal v
ppVal (VCons v vs)   = ppVal v ++ " " ++ ppVal vs
ppVal Empty       = "Empty"
ppVal (Closure n e _)   = "Function " ++ n ++ " : " ++ pp e
ppVal (Plus v1 v2)  = ppVal v1 ++ " + " ++ ppVal v2
ppVal (FreeVar n)   = n
ppVal (VPair v1 v2) = "(" ++ ppVal v1 ++ ", " ++ ppVal v2 ++ ")"
ppVal _             = "Error"

ppEnv :: Env -> String
ppEnv e     = "<\n" ++ ppEnv' e ++ ">"
    where
    ppEnv' ((n,v):es)   = n ++ " <- " ++ show v ++ "\n" ++ ppEnv' es
    ppEnv' []           = ""

------------------------------------------------------------------------
-- EXAMPLES
------------------------------------------------------------------------

-- SMART CONSTRUCTORS
[f,x,n,l,r,m,k,t,y] = map Var ["f","x","n","l","r","m","k","t","y"]

con :: Int -> Expr
con i = Value $ Con i

cons :: [Int] -> Expr
cons l = foldr1 Cons ((map con l) ++ [Value Empty]) 

exList' = [1..5]
exList2' = [8,14,32,0,4]
exList3' = [1,2,13,24,25,45,64,84,99,100]
exList4' = [1..10]

myCons = map cons [exList', exList2', exList3', exList4']
[exList, exList2, exList3, exList4] = myCons
[exListV, exList2V, exList3V, exList4V] = map (eval prelude) myCons

-- PRELUDE FUNCTIONS
prelude :: Env
prelude = [("cmp", BVal $ eval [] $ cmp')]
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

tail2' :: Expr
tail2' = Abstr "l" $ Case (Var "l") [(PEllipsis "x" (End "n"), Ellipsis (Var "x") (IPlace 2) (End "n"))]

tail3' :: Expr
tail3' = Abstr "l" $ Case (Var "l") -- of
    [
        (PEllipsis "x" (End "n"), Ellipsis' (Var "x") (IPlace 2) (End "n") "x")
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

removeNth2' :: Expr
removeNth2' = Abstr "l" $ Abstr "n" $ 
        Case (Var "l") -- of
        [
            (PEllipsis "x" (End "m"), Cat 
                (Ellipsis (Var "x") (IPlace 1) (EPlace $ Add (Var "n") (Value $ Con (-1))))
                (Ellipsis (Var "x") (EPlace $ Add (Var "n") (Value $ Con 1)) (End "m"))
                )
        ]

removeNth3' :: Expr
removeNth3' = Abstr "l" $ Abstr "n" $
    Case (Var "l") -- of
    [
        (PEllipsis "x" (End "m"), Cat
            (Ellipsis' (Var "x") (IPlace 1) (EPlace $ Sub (Var "n") (Value $ Con (1))) "x")
            (Ellipsis' (Var "x") (EPlace $ Add (Var "n") (Value $ Con 1)) (End "m") "x")
            )
    ]

firstN :: Expr
firstN = Abstr "l" $ Abstr "n" $
    Case (Var "l") -- of
    [
        (PEllipsis "x" (End "m"),   Ellipsis' (Var "x") (IPlace 1) (EPlace $ Var "n") "x")
    ]

add' :: Expr
add' = Abstr "a" $ Abstr "b" $ Add (Var "a") (Var "b")

map2' :: Expr
map2' = Abstr "l" $ Abstr "f" $ Case (Var "l") -- of
        [
            (PEllipsis "x" (End "n"), Ellipsis (App (Var "f") (Var "x")) (IPlace 1) (End "n"))
        ]



map3' :: Expr
map3' = Abstr "l" $ Abstr "f" $ Case l -- of
    [
        (PEllipsis "x" (End "n"), Ellipsis' (App f x) (IPlace 1) (End "n") "x")
    ]

-- x1 ... xn -> (f x1) ... (f xn)

-- (x1 ... xn, y1 ... ym)  -> let k = min m n in (x1, y1) ... (xk, yk)
-- Ellipsis' . . . [Name]

-- \l.case l of (x1 ... xn) -> \l2.case l2 of (y1 ... ym) -> let k = min m n in (x1, y1) ... (xk, yk)


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


reverse2' :: Expr
reverse2' = Abstr "l" $ Case (Var "l")
            [
                (PEllipsis "x" (End "n"),     Ellipsis (Var "x") (End "n") (IPlace 1))
            ]

reverse3' :: Expr
reverse3' = Abstr "l" $ Case (Var "l") -- of
    [
        (PEllipsis "x" (End "n"),   Ellipsis' (Var "x") (End "n") (IPlace 1) "x")
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
                            (Ellipsis' x (IPlace 1) (EPlace $ Div (Var "n") (Value $ Con 2)) "x")
                            (Ellipsis' x (EPlace $ Add (Div (Var "n") (Value $ Con 2)) (Value $ Con 1)) (EPlace $ Var "n") "x"))
    ]

listId' :: Expr
listId' = Abstr "l" $ Case (Var "l") -- of
    [
        (PEllipsis "x" (End "n"),   Ellipsis' (Var "x") (IPlace 1) (End "n") "x")
    ]

binSearch' :: Expr
binSearch' = Abstr "list" $ Abstr "term" $ (LetRec "binSearch" (Abstr "l" $ Abstr "t" $ Case (Var "l") -- of
    [
        (PVal Empty,    Value $ Con 0),
        (PEllipsis "x" (End "n"),     Trace "l" l $ Trace "n" n $ Let "k" (Add (Div n (Value $ Con 2)) (Value $ Con 1)) $ 
            Case (App (App cmp (ListElement "x" (EPlace k))) t)
            [
                (PVal $ Con 0,    Value $ Con 1),
                (PVal $ Con 1, App (App (Var "binSearch") $ (Ellipsis' x 
                                                                        (IPlace 1) 
                                                                        (EPlace $ Sub k (Value $ Con 1))
                                                                        "x")) 
                                                                        t),
                (PVal $ Con (-1), App (App (Var "binSearch") $ (Ellipsis' x 
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

test3' :: Expr
test3' = Abstr "l" $ Case (Var "l") -- of
    [
        (PEllipsis "x" (End "n"), Ellipsis' (Var "x") (IPlace 3) (IPlace (-1)) "x")
    ]

printList :: [String] -> IO ()
printList []    = print ""
printList (x:xs)= do
                    putStrLn "========="
                    putStrLn x
                    printList xs

main :: IO ()
main =
    do
        let examples = [succ', head', tail', map', fold', reverse', second', nth', zip', find', sum', sumNum'] in
            printList (map pp examples)
        let examples2 = [sucEach'] in
            printList (map pp examples2)

testCase :: Expr -> Val -> Bool
testCase t v = eval [] t == v

test :: IO ()
test = print "hi"
{-
    do
        print $ testCase (App (App fst' (Value $ Con 1)) (Value $ Con 2)) (Con 1)
        print $ testCase (App (App snd' (Value $ Con 1)) (Value $ Con 2)) (Con 2)
        print $ testCase (App (App nth' (Value exList)) (Value $ Con 2)) (Con 3)
        print $ testCase (App sucEach' (Value exList)) (ICons 2 $ ICons 3 $ ICons 4 $ ICons 5 $ ICons 6 Empty)
        print $ testCase (App id' (Value exList)) exList
        print $ testCase (App (App map' (Value exList2)) succ') (ICons 9 $ ICons 15 $ ICons 33 $ ICons 1 $ ICons 5 Empty)
        -}
