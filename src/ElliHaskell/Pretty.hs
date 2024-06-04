module ElliHaskell.Pretty where

import ElliHaskell.Eval
import ElliHaskell.Syntax
import ElliHaskell.Types
import ElliHaskell.TypeChecker (ErrType)

import Data.List (intercalate)

ppEvalErr :: ErrEval -> String
ppEvalErr err = "evaluation error: " ++ go err
  where
    go (ErrEBound v)        = "variable not bound: " ++ show v
    go (ErrECxt err1 err2)  = go err1 ++ ":" ++ go err2
    go (ErrENotVal e)       = "did not produce a value expression for expression: " ++ pp e
    go (ErrENotClosure e val) 
        =  "when trying to run the expression:\n" 
        ++ pp e
        ++ "\neval did not produce a closure, but instead: \n"
        ++ pp val
    go (ErrENoMatch pats val) = "could not match " ++ pp val ++ " with patterns " ++ show (ppPat <$> pats)
    go (ErrENotImpl e)      = pp e ++ " is not implemented"
    go (ErrEOther s)        = s
    go _                    = undefined

ppTypeErr :: ErrType -> String
ppTypeErr = show

{-
ppTypeDeclErr :: ErrDeclType -> String
ppTypeDeclErr = show
-}

pp :: Expr -> String
pp (Var _ v)   = v
pp (App _ e1 e2) = pp e1 ++ " (" ++ pp e2 ++ ")"
pp (Abstr _ pat e) = "(\\" ++ ppPat pat ++ " -> " ++ pp e ++ ")"
pp (Con _ c)   = ppCon c
pp (Closure {}) = "CLOSURE"
pp (Ifx _ e1 op e2) = 
    let s1 = pp e1
        s2 = pp e2
    in s1 ++ " " ++ opinfo_ifxname (getOpInfo op) ++ " " ++ s2
pp (TypeSig _ t e) = "(" ++ pp e ++ " :: " ++ ppType t ++ ")"
pp (List _ es)  = "[" ++ intercalate "," (map (pp) es) ++ "]"
pp _ = "not implemented"

ppDecl :: Decl -> String
ppDecl (Decl _ v t alts) 
    =  v ++ " :: " ++ ppType t ++ ";\n"
    ++ concatMap ppAlt alts
  where
    ppAlt :: ([Pattern], Expr) -> String
    ppAlt (pats, e) = v ++ " " ++ unwords (ppPat <$> pats) ++ " = " ++ pp e

ppCon :: Constant -> String
ppCon (I i) = show i
ppCon (B b) = show b

ppPat :: Pattern -> String
ppPat pat = show pat

ppType :: Type -> String
ppType (TypeAbstr t t')     = "(" ++ ppType t ++ ") -> (" ++ ppType t' ++ ")"
ppType TypeInt              = "Int"
ppType TypeBool             = "Bool"
ppType (TypeList t)         = "[" ++ ppType t ++ "]"
ppType TypeAny              = "any"
ppType (TypeSome i)         = "t" ++ show i
