
---------------------------------------------------------------------------------
-- SEMANTICS
---------------------------------------------------------------------------------

bReduc :: Expr -> Expr
bReduc e = bReduc' $ bClean e []

-- Top level funciton, takes in Expr and outputs reduced expr
bReduc' :: Expr -> Expr
bReduc' (Var vn) = (Var vn)
bReduc' (Abstr vn e) = (Abstr vn (bReduc' e))
bReduc' (App e1 e2) = bApp (bReduc' e1) (bReduc' e2)

-- Helper function for bReduc', for beta applications
bApp :: Expr -> Expr -> Expr
bApp (Abstr vn e1) e2 = let e2' = bReduc' e2 in
                            let e1' = replaceVar vn e2' e1 in
                                bReduc' e1'
bApp e1 e2 = App (bReduc' e1) (bReduc' e2)

-- Helper function for bReduc'.bApp, finds and replaces "needle" with "repl" in
-- "haystack".
--            needle     repl    haystack
replaceVar :: VarName -> Expr -> Expr       -> Expr
replaceVar ndl repl (Var hstk) = if ndl == hstk then repl else Var hstk
replaceVar ndl repl (App hstk1 hstk2) = App (replaceVar ndl repl hstk1)
                                            (replaceVar ndl repl hstk2)
replaceVar ndl repl (Abstr hstk_vn hstk_e) = Abstr hstk_vn 
                                                   (replaceVar ndl repl hstk_e)

bClean :: Expr -> [VarName] -> Expr
bClean e [] = e

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
