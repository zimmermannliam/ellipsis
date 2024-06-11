module ElliHaskell.Eval where

import ElliHaskell.Syntax
import ElliHaskell.Types
import ElliHaskell.Pretty.Expr

import Data.Map (union, empty, insert)
-- import qualified Data.Map as Map
import Control.Monad.Trans.Except
import Control.Monad.Reader


import Debug.Trace

type Evaluation a = ExceptT ErrEval (Reader Env) a

data ErrEval
    = ErrEBound Env Name
    | ErrEInfo Info ErrEval
    | ErrENotVal Expr
    | ErrENotClosure Expr Expr
    | ErrENoMatch [Pattern] Val
    | ErrENotImpl Expr
    | ErrEOpType Type Type Type
    | ErrEOther String
    deriving (Eq, Show)

isVal :: Expr -> Bool
isVal (Con _ _)   = True
isVal (List _ es) = all isVal es
isVal _           = False

eval :: Expr -> Evaluation Val
eval e = do
    val <- evalExpr e
    if isVal val
        then return val
        else throwE $ ErrENotVal val

evalExpr :: Expr -> Evaluation Val

-- E |- v => val
evalExpr (Var i v)    = do
    env <- ask
    case env `getVal` v of

        -- E(v) = val
        Just val    -> return val
        Nothing     -> throwE $ ErrEInfo i (ErrEBound env v)

-- E |- \pat -> e => (E, pat, e)
evalExpr (Abstr _ pat e) = do
    env <- ask
    return $ Closure blank (pat, e) env empty

-- E |- e1 e2 => val
evalExpr e@(App _ e1 e2) = do
    val1 <- evalExpr e1
    case val1 of
        -- E |- e1 => (alt, E', E'')
        Closure _ alt env' env'' -> do
            -- E |- e2 => val2
            val2 <- evalExpr e2

            -- E' + rec(E'', val2) |- alt => val
            local (const ((recEnv env'') `union` env')) $ evalAlts val2 [alt]
        _                  -> throwE $ ErrENotClosure e val1

evalExpr c@(Con _ _) = return c

evalExpr (Ifx i e1 (VarOp v) e2) = evalExpr $ App i (App i (Var i v) e1) e2

evalExpr (Ifx _ e1 op e2) = do
    val1 <- evalExpr e1
    val2 <- evalExpr e2
    except $ evalOp val1 op val2

evalExpr (TypeSig _ _ e) = evalExpr e

evalExpr (List i es) = do
    es' <- mapM evalExpr es
    return $ List i es'

evalExpr (Case _ e alts) = do
    val <- evalExpr e
    evalAlts val alts

-- E |- let v = e1 in e2 => val2
evalExpr (Let _ pat e1 e2) = do
    -- E |- e1 => val1
    val1 <- evalExpr e1

    -- E,val1 |- pat => E'
    env' <- maybe 
        (throwE $ ErrENoMatch [pat] val1)
        return
        (patternmatch val1 pat)

    -- E + rec E' |- e2 => val2
    local (union (recEnv env')) $ evalExpr e2

evalExpr e              = throwE (ErrENotImpl e)

recEnv :: Env -> Env
recEnv env = fmap (rec' env) env

rec' :: Env -> Val -> Val
rec' env (Closure i alt env' _) = Closure i alt env' env
rec' _   val                    = val

evalOp :: Val -> Op -> Val -> Either ErrEval Val
evalOp (Con _ (I i1)) Add (Con _ (I i2)) = Right $ Con blank $ I $ i1 + i2
evalOp (Con _ (I i1)) Sub (Con _ (I i2)) = Right $ Con blank $ I $ i1 - i2
evalOp (Con _ (I i1)) Mul (Con _ (I i2)) = Right $ Con blank $ I $ i1 * i2
evalOp (Con _ (I i1)) Div (Con _ (I i2)) = Right $ Con blank $ I $ i1 `div` i2
evalOp val1           Eq  val2           = Right $ Con blank $ B $ val1 == val2
evalOp val1           Neq val2           = Right $ Con blank $ B $ val1 /= val2
evalOp (Con _ (I i1)) Lt  (Con _ (I i2)) = Right $ Con blank $ B $ i1 < i2
evalOp (Con _ (I i1)) Gt  (Con _ (I i2)) = Right $ Con blank $ B $ i1 > i2
evalOp (Con _ (I i1)) Leq (Con _ (I i2)) = Right $ Con blank $ B $ i1 <= i2
evalOp (Con _ (I i1)) Geq (Con _ (I i2)) = Right $ Con blank $ B $ i1 >= i2
evalOp (Con _ (B b1)) And (Con _ (B b2)) = Right $ Con blank $ B $ b1 && b2
evalOp (Con _ (B b1)) Or  (Con _ (B b2)) = Right $ Con blank $ B $ b1 || b2
evalOp val1           Cons (List i vals) = Right $ List i (val1:vals)
evalOp val1           _   val2           | not (isVal val1)     = Left $ ErrENotVal val1
                                         | not (isVal val2)     = Left $ ErrENotVal val2
                                         | otherwise            = Left $ ErrEOther "Bad values for operator"

evalAlts :: Val -> [Alt] -> Evaluation Val
evalAlts val alts = go alts
  where
    go :: [Alt] -> Evaluation Val
    go ((pat, e):rest)    = case patternmatch val pat of
        Nothing     -> go rest
        Just env    -> local (union env) (evalExpr e)
    go []                 = throwE (ErrENoMatch (map fst alts) val)

patternmatch :: Val -> Pattern -> Maybe Env
patternmatch val            (VarPat v)      = return $ insertVal v val empty
patternmatch (Con _ c1)     (ConPat c2) = 
    if c1 == c2 
    then return empty 
    else Nothing
patternmatch _                  (ConPat _)  = Nothing
patternmatch val            (TypedPat _ pat)= patternmatch val pat

------------------------------------------------------------------------
--
-- Evaluate declarations
--
------------------------------------------------------------------------