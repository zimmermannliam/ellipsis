import Data.Maybe
import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- HELPY
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- SYNTAX
--------------------------------------------------------------------------------

data Expr = Var VarName
          | App Expr Expr
          | Abstr VarName Expr
          deriving Show

type VarName = String

pp :: Expr -> String
pp (Var vn) = vn
pp (App e1 e2) = (pp e1) ++ "(" ++ (pp e2) ++ ")"
pp (Abstr vn e) = "\\" ++ vn ++ "." ++ (pp e)


--------------------------------------------------------------------------------
-- SEMANTICS
--------------------------------------------------------------------------------

bReduc :: Expr -> Expr
bReduc (Var vn) = (Var vn)
bReduc (Abstr vn e) = (Abstr vn (bReduc e))
bReduc (App e1 e2) = bApp (bReduc e1) (bReduc e2)

bApp :: Expr -> Expr -> Expr
bApp (Abstr vn (Var vb)) (Var vo) = Var (Text.unpack (
                                        Text.replace
                                        (Text.pack vn)
                                        (Text.pack vo)
                                        (Text.pack vb)
                                        ))
bApp e1 e2 = App e1 e2



main :: IO ()
main = putStrLn (pp (Abstr "x" (Var "x^2")))

-- Need to make semantics.
