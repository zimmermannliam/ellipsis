module ElliHaskell.Pretty.Err where

import ElliHaskell.Pretty.Expr

import ElliHaskell.Eval (ErrEval (..))
import ElliHaskell.TypeChecker (ErrType (..))

import Text.Megaparsec (sourcePosPretty)

ppEvalErr :: ErrEval -> String
ppEvalErr err = "evaluation error: " ++ go err
  where
    go (ErrEBound env v)        = "variable not bound: " ++ show v ++ " env: " ++ ppEnv env
    go (ErrENotVal e)       = "did not produce a value expression: " ++ pp e
    go (ErrENotClosure e val) 
        =  "when trying to run the expression:\n" 
        ++ pp e
        ++ "\neval did not produce a closure, but instead: \n"
        ++ pp val
    go (ErrENoMatch pats val) = "could not match " ++ pp val ++ " with patterns " ++ show (ppPat <$> pats)
    go (ErrENotImpl e)      = pp e ++ " is not implemented"
    go (ErrEOther s)        = s
    go (ErrEInfo i err')     = sourcePosPretty i ++ ":" ++ go err'
    go _                    = undefined

ppTypeErr :: ErrType -> String
ppTypeErr = show

{-
ppTypeDeclErr :: ErrDeclType -> String
ppTypeDeclErr = show
-}
