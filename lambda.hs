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
          | Case Expr Possibilities
          | LetRec Name Expr Expr
          | Fix Expr
          | Add Expr Expr
          | Tail Expr 
          deriving (Eq, Show)

data Val = Con Int 
          | Cons Int Val 
          | Empty 
          | Fun Name Expr 
          | Plus Val Val
          | Fixed Expr
          | FreeVar Name
         deriving (Eq, Show)

data Pattern = PCons Name Name
             | PVar Name
             | PVal Val
             | Ellipsis
             deriving (Eq, Show)

type Name = String

type Env = [(Name, Val)]

type Possibilities = [(Pattern, Expr)]



---------------------------------------------------------------------------------
-- SEMANTICS
-- ASSUME EVERY VARIABLE IS NAMED SEPARATELY
---------------------------------------------------------------------------------

ycomb = Abstr "f" $ 
            App (Abstr "x" $ App (Var "f" ) (App (Var "x") (Var "x"))) 
                (Abstr "x" $ App (Var "f" ) (App (Var "x") (Var "x")))

-- \f.( (\x.(f (x x))) (\x.(f (x x))) )

eval :: Env -> Expr -> Val
eval e (Var vn)            = envLookup e vn
eval e (App t1 t2)         = case (eval e t1) of 
                                Fun x t1b   -> eval ((x, eval e t2):e) t1b
                                _           -> error ("Expected fn to be applied")
eval e (Abstr x t)         = Fun x t
eval e (Let n t1 t2)       = eval ((n, eval e t1):e) t2
eval e (Add t1 t2)         = case (eval e t1, eval e t2) of
                                (Con i1, Con i2)    -> Con (i1 + i2)
                                _                   -> Plus (eval e t1) (eval e t2)
eval e (LetRec n t1 t2)    = case (eval e t1) of
                                Fun _ _     -> eval ((n, eval e $ App ycomb (Abstr n t1)):e) t2
                                                    -- fn = fix (\fn.t1)
                                _           -> error "Expected fn to be letrecced"
eval e (Value v)            = v
eval e (Tail t)             = case (eval e t) of
                                (Cons _ tail)           -> tail
                                _                       -> error ("not a list " ++ (show t))
eval e (Case tc ps)   = patternMatchEval e tc ps

{- if (eval e tc) == v
                                then (eval e t1)
                                else (eval e t2) 
        -}

patternMatchEval :: Env -> Expr -> Possibilities -> Val 
patternMatchEval e t (p:ps) = case (patternMatch e t p) of
                                Nothing     -> (patternMatchEval e t ps)
                                Just v      -> v
patternMatchEval e t []     = error "Ran out of patterns"

patternMatch :: Env -> Expr -> (Pattern, Expr) -> Maybe Val
patternMatch e t ((PVal k), t2)   = if (eval e t) == k then Just (eval e t2)
                                   else Nothing
patternMatch e t ((PCons n ns), t2) = case (eval e t) of
                                        Cons x xs   -> Just $ eval ((n,Con x):(ns,xs):e) t2
patternMatch e t ((PVar n), t2)   = Just $ eval ((n, (eval e t)):e) t2 
patternMatch e t _                = Nothing

{-
f (l) = case l of 
        x:xs -> x + 1
        []   -> 0
pattern matching adds bindings to the environment according ot the pattern

evaluate
???
add bindings to environment
-}



envLookup :: Env -> Name -> Val
envLookup [] name                       = FreeVar name 
                                        --error $ "envLookup: can't find " ++ name ++ " in env"
envLookup ((env_name, env_val):xs) name = if name == env_name then env_val
                                          else (envLookup xs name)


{-
plusfive = Abstr "x" $ Add (Var "x") (Value $ Con 5)


plus = Abstr "x" $ Abstr "y" $ Add (Var "x") (Var "y")

stz' = (Abstr "x" $ 
         Case (Var "x") 
             (Con 0)     (Value $ Con 0) 
                         (Add (Var "x") (App (Var "stz") (Add (Var "x") (Value $ Con (-1)))))
       )

stz = LetRec "stz" stz'
-- eval [] $ stz (App (Var "stz") (Value $ Con 10))

len' = (Abstr "l" $
        Case (Var "l")
            Empty       (Value $ Con 0)
                        (Add (Value $ Con 1) (App (Var "len") (Tail (Var "l"))))
       )

len = LetRec "len" len'

pp :: Expr -> String
pp (Var vn)         = vn
pp (App e1 e2)      = "(" ++ (pp e1) ++ " " ++ (pp e2) ++ ")"
pp (Abstr vn e)     = "\\" ++ vn ++ "." ++ (pp e)

main :: IO ()
main = do
  putStrLn $ show $ eval [] $ 
                len (App (Var "len") (Value $ Cons 100 $ Cons 2 $ Cons 3 $ Cons 5 Empty))

pp (Value (Con i))  = show i
pp (Value Empty)    = ""
pp (Value (Cons i e)) = "[" ++ ppCons (Cons i e) ++ "]"
pp (Let vn e1 e2)   = "" -- "let " ++ vn ++ " = (" ++ (pp e1) ++ ") in " ++ (pp e2)
pp (Case vn c1 b1 b2) = "" -- "case " ++ vn ++ " of " ++ (pp c1) ++ " -> "
                        -- ++ (pp b1) ++ "; Otherwise -> " ++ (pp b2)
pp (Value (Fun n e)) = n ++ " " ++ (pp e)

ppCons :: Val -> String
ppCons (Cons i (Cons i2 e))     = show i ++ "," ++ ppCons (Cons i2 e)
ppCons (Cons i _)               = show i

bReduc :: Expr -> Expr
bReduc e = bReduc' e

-- Top level funciton, takes in Expr and outputs reduced expr
bReduc' :: Expr -> Expr
bReduc' (Var vn) = (Var vn)
bReduc' (Abstr vn e) = (Abstr vn (bReduc' e))
bReduc' (App e1 e2) = bApp (bReduc' e1) (bReduc' e2)
bReduc' e = e

-- Helper function for bReduc', for beta applications
bApp :: Expr -> Expr -> Expr
bApp (Abstr vn e1) e2 = let e2' = bReduc' e2 in
                            let e1' = replaceVar vn e2' e1 in
                                bReduc' e1'
bApp e1 e2 = App (bReduc' e1) (bReduc' e2)

-- Helper function for bReduc'.bApp, finds and replaces "needle" with "repl" in
-- "haystack".
--            needle     repl    haystack
replaceVar :: Name -> Expr -> Expr       -> Expr
replaceVar ndl repl (Var hstk) = if ndl == hstk then repl else Var hstk
replaceVar ndl repl (App hstk1 hstk2) = App (replaceVar ndl repl hstk1)
                                            (replaceVar ndl repl hstk2)
replaceVar ndl repl (Abstr hstk_vn hstk_e) = Abstr hstk_vn 
                                                   (replaceVar ndl repl hstk_e)

EXAMPLE I FAILED ON?

(\x.\y.(y x) (y w))         
homework answer:    \z.(z (y w))        
prog answer:        \y.(y (y w))
calculator answer:  with some finagling, \y0.y0 (y w), so y0 = z
hand answer:        \x.\y.(y x) (y w) -> 
"Avoid capturing the free variable y in (y w)"

Does this mean I need to take a first pass and replace any free variables?
-}
