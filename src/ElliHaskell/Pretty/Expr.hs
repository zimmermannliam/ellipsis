module ElliHaskell.Pretty.Expr where

import ElliHaskell.Syntax
import ElliHaskell.Types

import Data.List (intercalate)
import qualified Data.Map as Map

pp :: Expr -> String
pp (Var _ v)   = v
pp (App _ e1 e2) = pp e1 ++ " (" ++ pp e2 ++ ")"
pp (Abstr _ pat e) = "(\\" ++ ppPat pat ++ " -> " ++ pp e ++ ")"
pp (Con _ c)   = ppCon c
pp (Closure _ (pat, e) env _) = "{" ++ ppEnv env ++ "} |- " ++ ppPat pat ++ " -> " ++ pp e
pp (Ifx _ e1 op e2) = 
    let s1 = pp e1
        s2 = pp e2
    in s1 ++ " " ++ opinfo_ifxname (getOpInfo op) ++ " " ++ s2
pp (TypeSig _ t e) = "(" ++ pp e ++ " :: " ++ ppType t ++ ")"
pp (List _ es)  = "[" ++ intercalate "," (map (pp) es) ++ "]"
pp (Case _ e alts) = "case " ++ pp e ++ " of {" ++ intercalate ";" (fmap ppAlt alts) ++ "}"
pp (Let _ pat e1 e2) = "let " ++ ppPat pat ++ " = " ++ pp e1 ++ " in " ++ pp e2
-- pp e = "not implemented: " ++ show e


ppDecl :: Decl -> String
ppDecl = show
{-
ppDecl (Decl _ v t alts) 
    =  v ++ " :: " ++ ppType t ++ ";\n"
    ++ concatMap ppAlt alts
  where
    ppAlt :: ([Pattern], Expr) -> String
    ppAlt (pats, e) = v ++ " " ++ unwords (ppPat <$> pats) ++ " = " ++ pp e
    -}

ppCon :: Constant -> String
ppCon (I i) = show i
ppCon (B b) = show b

ppPat :: Pattern -> String
ppPat (ConPat c)        = ppCon c
ppPat (VarPat v)        = v
ppPat (TypedPat t pat)  = "(" ++ (ppPat pat) ++ " :: " ++ ppType t ++ ")"

data Dir = L | R
    deriving (Show, Eq)

ppType :: Type -> String
ppType typ = go R typ
  where
    go :: Dir -> Type -> String
    go R (TypeAbstr t t')     = go L t ++ " -> " ++ go R t'
    go L (TypeAbstr t t')     = "(" ++ go L t ++ " -> " ++ go R t' ++ ")"
    go _ TypeInt              = "Int"
    go _ TypeBool             = "Bool"
    go _ (TypeList t)         = "[" ++ go R t ++ "]"
    go _ (TypeSome i)         = "t" ++ show i

ppEnv :: Env -> String
ppEnv env = intercalate ", " ((uncurry go) <$> (Map.toList env))
  where
    go :: Name -> Expr -> String
    go v e = v ++ " -> " ++ pp e

ppAlt :: Alt -> String
ppAlt (pat, e) = ppPat pat ++ " -> " ++ pp e