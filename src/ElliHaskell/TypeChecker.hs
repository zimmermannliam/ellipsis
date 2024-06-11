module ElliHaskell.TypeChecker (ErrType (..), typeInfer) where

import ElliHaskell.Syntax
import ElliHaskell.Types

import Control.Monad.Trans.Except
import Control.Monad.State.Lazy

import qualified Data.Map as Map
import Data.Bifunctor (first, second)
import Debug.Trace

------------------------------------------------------------------------
--
-- Interface
--
------------------------------------------------------------------------

data ErrType
    = ErrTConflict Type Type
    | ErrTPattern Pattern
    | ErrTNotAbstr Type
    | ErrTNotBound Name
    | ErrTNotImpl
    | ErrTContext String ErrType
    deriving (Eq, Show)

typeInfer :: Cxt -> Expr -> Either ErrType Type
typeInfer cxt e = runTyping cxt (typeExpr e)

------------------------------------------------------------------------
--
-- Type inference
--
------------------------------------------------------------------------

type Typing a = ExceptT ErrType (State (Int, Cxt)) a
runTyping :: Cxt -> Typing a -> Either ErrType a
runTyping cxt typing = evalState (runExceptT typing) (1, cxt)

-- | Type expressions
typeExpr :: Expr -> Typing Type
typeExpr (Con _ c) = typeCon c

-- G |- x : t
typeExpr (Var _ v) = getFromCxt v
    {-do
    -- x : pt \in G
    pt <- getFromCxt v
    -- t = inst(pt)
    inst pt-}

-- G |- e0 e1 : t'
typeExpr (App _ e0 e1) = do
    -- G |- e0 : t0
    t0 <- typeExpr e0
    -- G |- e1 : t1
    t1 <- typeExpr e1
    -- t' = newvar
    t' <- newvar
    -- |- unify(t0, t1 -> t')
    unify t0 (TypeAbstr t1 t') 

    return t'

-- G |- \x -> e : t -> t'
typeExpr (Abstr _ (VarPat v) e) = do
    -- t = newvar
    t <- newvar

    -- G,x:t |- e : t'
    t' <- mapExceptT (localState $ second (Map.insert v t)) (typeExpr e)

    return $ TypeAbstr t t'


typeExpr _ = undefined

-- | Type constants
typeCon :: Constant -> Typing Type
typeCon (I _) = return TypeInt
typeCon (B _) = return TypeBool

------------------------------------------------------------------------
--
-- Sub-inferences
--
------------------------------------------------------------------------

-- | Return a fresh type variable
newvar :: Typing Type
newvar = do
    (i, cxt) <- lift get
    lift $ put (i+1, cxt)
    return $ TypeSome i

-- | Take the left biased type precidence
unify :: Type -> Type -> Typing ()
-- union
unify t1 t2@(TypeSome _) = replaceCxt t2 t1
unify t1@(TypeSome _) t2 = replaceCxt t1 t2
-- terms
unify t1@(TypeAbstr l1 l2) t2@(TypeAbstr r1 r2)  = do
    unify l1 r1
    unify l2 r2
    (_, cxt) <- lift get
    traceM $ show cxt
unify l r
    | l == r    = return ()
    | otherwise = throwE $ ErrTConflict l r

{-
unify (TypeList l) (TypeList r) = do
    unify l r
    -}

union :: Type -> Type -> Typing Type
union t1 (TypeSome _) = return t1
union (TypeSome _) t2 = return t2
union _ _ = undefined

inst :: Type -> Typing Type
inst pt = do
    tNew <- newvar
    unify pt tNew
    union pt tNew

replaceCxt :: Type -> Type -> Typing ()
replaceCxt tOld tNew = modifyCxt (Map.map (go))
  where
    go :: Type -> Type
    go t
        | t == tOld   = tNew
        | (TypeAbstr t1 t2) <- t = TypeAbstr (go t1) (go t2)
        | (TypeList t') <- t = TypeList (go t')
        | otherwise = t

------------------------------------------------------------------------
--
-- Helpers
--
------------------------------------------------------------------------

{-
typeApp :: Type -> Type -> Typing Type
typeApp tFun tArg = case tFun of
    TypeAbstr tParam tBody -> do
        void $ unify tArg tParam
        return tBody
    _ -> throwE $ ErrTNotAbstr tFun
    -}


modifyCxt :: (Cxt -> Cxt) -> Typing ()
modifyCxt = lift . modify . second

getFromCxt :: Name -> Typing Type
getFromCxt v = do
    (_, cxt) <- lift get
    case cxt Map.!? v of
        Nothing -> throwE $ ErrTNotBound v
        Just t  -> return t

------------------------------------------------------------------------
--
-- General
--
------------------------------------------------------------------------

localState :: (s -> s) -> State s a -> State s a
localState f c = do
    temp <- get  -- pop
    modify f
    res <- c
    put temp  -- push
    return res

-- Hidden

-- | Type patterns
typePat :: Pattern -> Typing (Type, Cxt)
typePat (ConPat c) = do
    t <- typeCon c
    return (t, Map.empty)
typePat (VarPat v) = do
    tVar <- newvar
    let cxt' = Map.singleton v tVar
    return (tVar, cxt')
typePat _          = undefined

