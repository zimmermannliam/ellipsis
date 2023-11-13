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
          | LetRec Name Expr Expr
          | Fix Expr
          | Add Expr Expr
          | Tail Expr
          | EllipsisE Expr Val Val -- For ym ... yn, Ellipsis y m n
          | ConsE Expr Expr
          deriving (Eq, Show)

data Val = Con Int 
          | Cons Int Val 
          | Empty 
          | Closure Name Expr Env
          | Fun Name Expr
          | Plus Val Val
          | Fixed Expr
          | FreeVar Name
         deriving (Eq, Show)

data Pattern = PCons Name Name
             | PVar Name
             | PVal Val
             | PEllipsis Name Name -- For y1 ... yn, Ellipsis 
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
                                _           -> error "Expected fn to be applied"
eval e (Abstr x t)         = Closure x t e
eval e (Let n t1 t2)       = eval ((n, eval e t1):e) t2
eval e (Add t1 t2)         = case (eval e t1, eval e t2) of
                                (Con i1, Con i2)    -> Con (i1 + i2)
                                _                   -> Plus (eval e t1) (eval e t2)
eval e (LetRec n t1 t2)    = case eval e t1 of
                                Closure _ _ _   -> eval ((n, eval e $ App ycomb (Abstr n t1)):e) t2
                                                    -- fn = fix (\fn.t1)
                                _           -> error "Expected fn to be letrecced"
eval e (Value v)            = v
eval e (Tail t)             = case eval e t of
                                (Cons _ tail)           -> tail
                                _                       -> error ("not a list " ++ show t)
eval e (Case tc ps)   = patternMatchEval e tc ps
eval e (ConsE t1 t2)  = Cons (valEvalInt $ eval e t1) (eval e t2)
eval e _              = error "Feature not implemented"

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
                                                Cons x xs   -> Just $ eval ((n,Con x):(ns,xs):e) t2
                                                _           -> Nothing
patternMatch e t (PVar n, t2)             = Just $ eval ((n, eval e t):e) t2 
patternMatch e t (PEllipsis n len, t2)    = case eval e t of
                                                Cons x xs   -> Nothing
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
pp (Let n e1 e2)= "Let " ++ n ++ " = (" ++ pp e1 ++ ") in " ++ pp e2
pp (Case e a)   = "Case " ++ pp e ++ " of {" ++ ppMatch a ++ "\n}"
pp (LetRec  n e1 e2)    = "Letrec " ++ n ++ " = (" ++ pp e1 ++ ")\nin " ++ pp e2
pp (Add e1 e2)  = "(" ++ pp e1 ++ " + " ++ pp e2 ++ ")"
pp (ConsE e1 e2) = "(" ++ pp e1 ++ " ++ " ++ pp e2 ++ ")"
pp _            = "Error"

ppMatch :: Alts -> String
ppMatch []              = ""
ppMatch ((p, e):as)     = "\n\t" ++ ppPattern p ++ " = " ++ pp e ++ ppMatch as

ppPattern :: Pattern -> String
ppPattern (PCons n1 n2) = "(" ++ n1 ++ ":" ++ n2 ++ ")"
ppPattern (PVar n)      = n
ppPattern (PVal v)      = ppVal v
ppPattern (PEllipsis n1 n2) = "(" ++ n1 ++ " ... " ++ n2 ++ ")"

ppVal :: Val -> String
ppVal (Con i)       = show i
ppVal (Cons i v)    = show i ++ " " ++ ppVal v
ppVal Empty       = "Empty"
ppVal (Closure n e _)   = "Function " ++ n ++ " : " ++ pp e
ppVal (Plus v1 v2)  = ppVal v1 ++ " + " ++ ppVal v2
ppVal (FreeVar n)   = n
ppVal _             = "Error"

------------------------------------------------------------------------
-- EXAMPLES
------------------------------------------------------------------------

exList = Cons 1 $ Cons 2 $ Cons 3 $ Cons 4 $ Cons 5 Empty
exList2 = Cons 8 $ Cons 14 $ Cons 32 $ Cons 0 $ Cons 4 Empty

succ' :: Expr
succ' = Abstr "x" $ Add (Var "x") (Value $ Con 1)

head' :: Expr
head' = Abstr "l" $ Case (Var "l") [(PVal Empty, Value Empty), (PCons "H" "T", Var "H")]

tail' :: Expr
tail' = Abstr "l" $ Case (Var "l") [(PVal Empty, Value Empty), (PCons "H" "T", Var "T")]


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
fold' = Value $ Con 1

reverse' :: Expr
reverse' = Value $ Con 1

second' :: Expr
second' = Value $ Con 1

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

zip' :: Expr
zip' = Value $ Con 1

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
        print $ testCase (App sucEach' (Value exList)) (Cons 2 $ Cons 3 $ Cons 4 $ Cons 5 $ Cons 6 Empty)
        print $ testCase (App id' (Value exList)) (exList)
