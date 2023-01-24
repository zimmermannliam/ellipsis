import Data.Maybe
import qualified Data.Text as Text


---------------------------------------------------------------------------------
-- SYNTAX
---------------------------------------------------------------------------------

data Expr = Var VarName
          | App Expr Expr
          | Abstr VarName Expr
          deriving Show

type VarName = String

pp :: Expr -> String
pp (Var vn) = vn
pp (App e1 e2) = "(" ++ (pp e1) ++ " " ++ (pp e2) ++ ")"
pp (Abstr vn e) = "\\" ++ vn ++ "." ++ (pp e)


---------------------------------------------------------------------------------
-- SEMANTICS
---------------------------------------------------------------------------------

bReduc :: Expr -> Expr
bReduc (Var vn) = (Var vn)
bReduc (Abstr vn e) = (Abstr vn (bReduc e))
bReduc (App e1 e2) = bApp (bReduc e1) (bReduc e2)

bApp :: Expr -> Expr -> Expr
bApp (Abstr vn e1) e2 = let e2' = bReduc e2 in
                            let e1' = replaceVar vn e2' e1 in
                                bReduc e1'
bApp e1 e2 = App (bReduc e1) (bReduc e2)

--            needle     repl    haystack
replaceVar :: VarName -> Expr -> Expr -> Expr
replaceVar ndl repl (Var hstk) = if ndl == hstk then repl else Var hstk
replaceVar ndl repl (App hstk1 hstk2) = App (replaceVar ndl repl hstk1) 
                                            (replaceVar ndl repl hstk2)
replaceVar ndl repl (Abstr hstk_vn hstk_e) = Abstr hstk_vn 
                                                   (replaceVar ndl repl hstk_e)

{-

EXAMPLE I FAILED ON?


(\x.\y.(y x) (y w))         
\z.(z (y w))        
"Avoid capturing the free variable y in (y w)"
-}


main :: IO ()
main = do
    let expr    = App (Abstr "x" (Abstr "y" (App (Var "y") (Var "x")))) (App (Var "y") (Var "w"))
    let expr'   = bReduc expr
    putStrLn $ pp expr
    putStrLn $ pp expr'

