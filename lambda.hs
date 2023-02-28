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
          | Case Expr Val Expr Expr
          | LetRec Name Expr Expr
          deriving (Eq, Show)

data Val = Con Int | Cons Int Val | Empty | Fun Name Expr
         deriving (Eq, Show)

type Name = String

type Env = [(Name, Val)]

pp :: Expr -> String
pp (Var vn)         = vn
pp (App e1 e2)      = "(" ++ (pp e1) ++ " " ++ (pp e2) ++ ")"
pp (Abstr vn e)     = "\\" ++ vn ++ "." ++ (pp e)
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


---------------------------------------------------------------------------------
-- SEMANTICS
-- ASSUME EVERY VARIABLE IS NAMED SEPARATELY
---------------------------------------------------------------------------------

eval :: Env -> Expr -> Val
eval ev ex     = eval' ev (bReduc ex) -- Don't need beta reduction

eval' :: Env -> Expr -> Val
eval' e (Var vn)            = envLookup e vn
eval' e (App e1 e2)         = case (eval' e e1) of 
                                Fun x e'    -> eval' ((x, eval' e e2):e) e'
                                _           -> error "Expected fn to be applied"
eval' e (Abstr x b)         = Fun x b
eval' e (Value v)           = v
eval' e (Let n e1 e2)       = eval' ((n, eval' e e1):e) e2
eval' e (Case ec v e1 e2)   = if (eval' e ec) == v
                                then (eval' e e1)
                                else (eval' e e2)
eval' e (LetRec n e1 e2)    = error "wait wait not yet"

-- case eval Expr of 
--  Val -> eval Expr
--  ow  -> eval Expr

-- try to impelment length
-- add let rec

-- applFun :: Env -> Val -> Expr -> Val
-- applFun e (Fun fn b) i = case (fn, i) of
--                           ("+", (App (Value $ Con i1))

envLookup :: Env -> Name -> Val
envLookup [] _                          = error "envLookup: can't find var name in environment"
envLookup ((env_name, env_val):xs) name = if name == env_name then env_val
                                          else (envLookup xs name)


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

{-
EXAMPLE I FAILED ON?

(\x.\y.(y x) (y w))         
homework answer:    \z.(z (y w))        
prog answer:        \y.(y (y w))
calculator answer:  with some finagling, \y0.y0 (y w), so y0 = z
hand answer:        \x.\y.(y x) (y w) -> 
"Avoid capturing the free variable y in (y w)"

Does this mean I need to take a first pass and replace any free variables?
-}
