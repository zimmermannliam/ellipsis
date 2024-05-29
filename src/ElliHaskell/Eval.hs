module ElliHaskell.Eval where

import ElliHaskell.Syntax
import ElliHaskell.Types
import ElliHaskell.TypeChecker (typeInfer)

import Data.Map (Map, fromList, union, empty)
import Control.Monad (mapM)

data ErrEval
    = ErrEBound Name
    | ErrECxt ErrEval ErrEval
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

eval :: Env -> Expr -> Either ErrEval Val
eval env e = do
    val <- evalgo env e
    if isVal val
        then return val
        else Left $ ErrENotVal val

evalgo :: Env -> Expr -> Either ErrEval Val

-- E |- v => val
evalgo env (Var _ v)    = case env `getVal` v of
    -- E(v) = val
    Just val    -> Right val
    Nothing     -> Left (ErrEBound v)

-- E |- \pat -> e => (E, pat, e)
evalgo env (Abstr _ pat e) = return $ Closure blank env pat e

-- E |- e1 e2 => val
evalgo env e@(App _ e1 e2) = do
    val <- evalgo env e1
    case val of
        -- E |- e1 => (E', pat', e')
        Closure _ env' pat' e' -> do
            -- E |- e2 => val'
            val' <- evalgo env e2
            -- val' |- pat' => E_pat
            env_pat <- maybe (Left $ ErrENoMatch [pat'] val') Right (patternmatch val' pat')
            -- E_pat + E' |- e' => val
            evalgo (env_pat `union` env') e'
        _                  -> Left $ ErrENotClosure e val

evalgo _ c@(Con _ _) = Right c

evalgo env (Ifx i e1 (VarOp v) e2) = eval env $ App i (App i (Var i v) e1) e2

evalgo env (Ifx _ e1 op e2) = do
    val1 <- eval env e1
    val2 <- eval env e2
    evalOp val1 op val2

evalgo env (TypeSig _ _ e) = evalgo env e

evalgo env (List i es) = do
    es' <- mapM (evalgo env) es
    return $ List i es'

evalgo _ e              = Left (ErrENotImpl e)

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
evalOp val1         op  val2         | not (isVal val1)     = Left $ ErrENotVal val1
                                     | not (isVal val2)     = Left $ ErrENotVal val2
                                     | otherwise            = Left $ ErrEOther "Bad values for operator"

patternmatch :: Val -> Pattern -> Maybe Env
patternmatch val (VarPat v)         = return $ insertVal v val empty
patternmatch (Con _ c1) (ConPat c2) = if c1 == c2 then Just empty else Nothing
patternmatch _ (ConPat _)           = Nothing