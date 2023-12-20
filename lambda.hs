import qualified Data.Text as Text
import Data.Maybe

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
          | Tail Expr
          | EllipsisE Expr Idx Idx -- For y1 ... yn: EllipsesE y 1 n
          | ConsE Expr Expr
          | Cat Expr Expr
          | Error String
          | Report Expr
          | EPair Expr Expr
          | EStr String
          deriving (Eq, Show)

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
         deriving (Eq, Show)

data Pattern = PCons Name Name
             | PVar Name
             | PVal Val
             | PEllipsis Name Idx
             deriving (Eq, Show)

data Idx    = IPlace Int 
            | End Name 
            | EPlace Expr -- Must evaluate to an integer
            deriving (Eq, Show)

type Name = String

type Env = [(Name, Val)]

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
eval e (Var vn)            = envLookup e vn
eval e (App t1 t2)         = case eval e t1 of 
                                Closure x t1b e2 -> eval ((x, eval e t2):e2) t1b
                                _           -> errorOut e "Expected fn to be applied"
eval e (Abstr x t)         = Closure x t e
eval e (Let n t1 t2)       = eval ((n, eval e t1):e) t2
eval e (Add t1 t2)         = case (eval e t1, eval e t2) of
                                (Con i1, Con i2)    -> Con (i1 + i2)
                                _                   -> Plus (eval e t1) (eval e t2)
eval e (LetRec n t1 t2)    = case eval e t1 of
                                Closure {}  -> eval ((n, eval e $ App ycomb (Abstr n t1)):e) t2
                                                    -- fn = fix (\fn.t1)
                                _           -> errorOut e "Expected fn to be letrecced"
eval e (Value v)            = v
eval e (Tail t)             = case eval e t of
                                (ICons _ tail)           -> tail
                                _                       -> errorOut e ("not a list " ++ show t)
eval e (Case tc ps)   = patternMatchEval e tc ps
eval e (ConsE t1 t2)  = VCons (eval e t1) (eval e t2)
eval e (EPair t1 t2)  = VPair (eval e t1) (eval e t2)
eval e (Cat t1 t2)    = let et1 = iconsToVCons $ eval e t1
                            et2 = iconsToVCons $ eval e t2
                        in case (et1, et2) of
                            (VCons _ _, VCons _ _) -> catVCons et1 et2
                            _                      -> errorOut e "Tried to cat non-lists"
eval e (Error s)      = errorOut e s
eval e _              = errorOut e "Feature not implemented"

errorOut :: Env -> String -> Val
errorOut e s = error (s ++ "; Environment: " ++ ppEnv e)

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
                                                ICons x xs  -> Just $ eval ((n,Con x):(ns,xs):e) t2
                                                VCons x xs  -> Just $ eval ((n, x):(ns,xs):e) t2
                                                _           -> Nothing
patternMatch e t (PVar n, t2)             = Just $ eval ((n, eval e t):e) t2 
patternMatch e t (PEllipsis n len, t2)    = case eval e t of
                                                ICons x xs   -> Nothing
                                                _           -> Nothing
-- patternMatch e t _                          = Nothing

-- Find a variable in environment
envLookup :: Env -> Name -> Val
envLookup [] name                       = FreeVar name 
                                        --error $ "envLookup: can't find " ++ name ++ " in env"
envLookup ((env_name, env_val):xs) name = if name == env_name then env_val
                                          else envLookup xs name


pp :: Expr -> String
pp (Var n)      = n
pp (App e1 e2)  = pp e1 ++ " " ++ pp e2
pp (Abstr n e)  = "\\" ++ n ++ ".(" ++ pp e ++ ")"
pp (Value v)    = ppVal v
pp (Let n e1 e2) = "Let " ++ n ++ " = (" ++ pp e1 ++ ") in " ++ pp e2
pp (Case e a)   = "Case " ++ pp e ++ " of {" ++ ppMatch a ++ "\n}"
pp (LetRec  n e1 e2)    = "Letrec " ++ n ++ " = (" ++ pp e1 ++ ")\nin " ++ pp e2
pp (Add e1 e2)  = "(" ++ pp e1 ++ " + " ++ pp e2 ++ ")"
pp (ConsE e1 e2) = "(" ++ pp e1 ++ " : " ++ pp e2 ++ ")"
pp (EllipsisE n start end)  = strn ++ ppIdx start ++ " ... " ++ strn ++ ppIdx end
                                where strn = pp n
pp (EPair t1 t2) = "(" ++ pp t1 ++ ", " ++ pp t2 ++ ")"
pp (Cat t1 t2) = pp t1 ++ " ++ " ++ pp t2
pp _            = "Error -- cannot display expression"

ppMatch :: Alts -> String
ppMatch []              = ""
ppMatch ((p, e):as)     = "\n\t" ++ ppPattern p ++ " = " ++ pp e ++ ppMatch as

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
    ppEnv' ((n,v):es)   = n ++ " <- " ++ ppVal v ++ "\n"
    ppEnv' []           = ""

------------------------------------------------------------------------
-- EXAMPLES
------------------------------------------------------------------------

exList = ICons 1 $ ICons 2 $ ICons 3 $ ICons 4 $ ICons 5 Empty
exList2 = ICons 8 $ ICons 14 $ ICons 32 $ ICons 0 $ ICons 4 Empty

succ' :: Expr
succ' = Abstr "x" $ Add (Var "x") (Value $ Con 1)

head' :: Expr
head' = Abstr "l" $ Case (Var "l") [(PVal Empty, Value Empty), (PCons "H" "T", Var "H")]

tail' :: Expr
tail' = Abstr "l" $ Case (Var "l") [(PVal Empty, Value Empty), (PCons "H" "T", Var "T")]

tail2' :: Expr
tail2' = Abstr "l" $ Case (Var "l") [(PEllipsis "x" (End "n"), EllipsisE (Var "x") (IPlace 2) (End "n"))]

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
                    (PCons "x" "xs", ConsE (Var "x") 
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
                (EllipsisE (Var "x") (IPlace 1) (EPlace $ Add (Var "n") (Value $ Con (-1))))
                (EllipsisE (Var "x") (EPlace $ Add (Var "n") (Value $ Con 1)) (End "m"))
                )
        ]

add' :: Expr
add' = Abstr "a" $ Abstr "b" $ Add (Var "a") (Var "b")

map2' :: Expr
map2' = Abstr "l" $ Abstr "f" $ Case (Var "l") -- of
        [
            (PEllipsis "x" (End "n"), EllipsisE (App (Var "f") (Var "x")) (IPlace 1) (End "n"))
        ]

map' :: Expr
map' = Abstr "l" $ Abstr "f" $
        LetRec "map" (Abstr "list" $ Abstr "fun" $ Case (Var "list")
        [
            (PVal Empty, Value Empty),
            (PCons "x" "xs", ConsE (App (Var "fun") (Var "x")) (App (App (Var "map") (Var "xs")) (Var "fun")))
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


-- Need another operator, I think -- cat or snoc?
reverse' :: Expr
reverse' = Value $ Con 1
{- 
reverse' :: Expr
reverse' = Abstr "l" $ 
            LetRec "reverse" (Abstr "list" )
            -}

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


-- Can't do zip, type-wise. Need a value-list
zip' :: Expr
zip' = Value $ Con 1
{-
zip' :: Expr
zip' = Abstr "l1" $ Abstr "l2" $ 
            LetRec "zip" (Abstr "list1" $ Abstr "list2" $ Case (Var "list1")
                [
                    (PCons "x" "xs", ),
                    (PVal Empty, Value Empty)
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

sumNum' :: Expr
sumNum' = Abstr "n" $ LetRec "sumNum" (Abstr "x" $ Case (Var "x") [(PVal $ Con 0, Value $ Con 0), (PVar "y", Add (Var "y") (App (Var "sumNum") (Add (Var "y") (Value $ Con (-1)))))]) $ App (Var "sumNum") (Var "n")

sucEach' :: Expr
sucEach' = Abstr "l" $ LetRec "sucEach" (Abstr "list" $ Case (Var "list")
    [
        (PVal Empty, Value Empty),
        (PCons "x" "xs", ConsE (Add (Var "x") (Value $ Con 1)) (App (Var "sucEach") (Var "xs")))
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
        (PCons "x" "xs", ConsE (Var "x") (App (Var "id") (Var "xs")))
    ])
    -- in
    (App (Var "id") (Var "l")))

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
test =
    do
        print $ testCase (App (App fst' (Value $ Con 1)) (Value $ Con 2)) (Con 1)
        print $ testCase (App (App snd' (Value $ Con 1)) (Value $ Con 2)) (Con 2)
        print $ testCase (App (App nth' (Value exList)) (Value $ Con 2)) (Con 3)
        print $ testCase (App sucEach' (Value exList)) (ICons 2 $ ICons 3 $ ICons 4 $ ICons 5 $ ICons 6 Empty)
        print $ testCase (App id' (Value exList)) exList
        print $ testCase (App (App map' (Value exList2)) succ') (ICons 9 $ ICons 15 $ ICons 33 $ ICons 1 $ ICons 5 Empty)
