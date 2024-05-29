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

ppTypeErr :: ErrType -> String
ppTypeErr = show

pp = ppgo 0

ppgo :: Int -> Expr -> String
ppgo tabs (Var _ v)   = v
ppgo tabs (App _ e1 e2) = ppgo tabs e1 ++ " (" ++ ppgo tabs e2 ++ ")"
ppgo tabs (Abstr _ pat e) = "(\\" ++ ppPat pat ++ " -> " ++ ppgo tabs e ++ ")"
ppgo tabs (Con _ c)   = ppCon c
ppgo tabs (Closure {}) = "CLOSURE"
ppgo tabs (Ifx _ e1 op e2) = 
    let s1 = ppgo tabs e1
        s2 = ppgo tabs e2
    in s1 ++ " " ++ opinfo_ifxname (getOpInfo op) ++ " " ++ s2
ppgo tabs (TypeSig _ t e) = "(" ++ ppgo tabs e ++ " :: " ++ ppType t ++ ")"
ppgo tabs (List _ es)  = "[" ++ intercalate "," (map (ppgo tabs) es) ++ "]"
ppgo _ _ = "not implemented"

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
