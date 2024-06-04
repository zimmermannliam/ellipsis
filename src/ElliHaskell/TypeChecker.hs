module ElliHaskell.TypeChecker (typeInfer, ErrType, typeApp) where

import ElliHaskell.Syntax
import ElliHaskell.Types

import Data.Map (insert)

import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Reader

data ErrType
    = ErrTConflict Type Type
    | ErrTPattern Pattern
    | ErrNotAbstr Type
    | ErrTNotImpl
    | ErrTContext String ErrType
    deriving (Eq, Show)

type Typing a = ExceptT ErrType (StateT Int (Reader Cxt)) a

typeEq :: Type -> Type -> Bool
typeEq TypeAny _    = True
typeEq _ TypeAny    = True
typeEq (TypeAbstr t1 t2) (TypeAbstr t1' t2') = (t1 `typeEq` t1') && (t2 `typeEq` t2')
typeEq t t'        = t == t'

typePrec :: Type -> Type -> Typing Type
typePrec t TypeAny     = return t
typePrec TypeAny t'    = return t'
typePrec (TypeAbstr t1 t2) (TypeAbstr t1' t2') = do
    t1_res <- typePrec t1 t1'
    t2_res <- typePrec t2 t2'
    return $ TypeAbstr t1_res t2_res
typePrec t t'          | t == t' = return t
                       | otherwise = throwE $ ErrTConflict t t'

typeApp :: Type -> Type -> Either ErrType Type
typeApp (TypeAbstr t1 t2) t'
    | t1 `typeEq` t'    = return t2
    | otherwise         = Left $ ErrTConflict t1 t'
typeApp t _ = Left $ ErrNotAbstr t

typeAny :: Typing Type
typeAny = do
    i <- lift get
    lift $ put (i+1)
    return $ TypeSome i

------------------------------------------------------------------------
-- 
-- 
--
------------------------------------------------------------------------

typeInfer :: Expr -> Typing Type

typeInfer (Var _ v)     = do
    cxt <- ask
    case cxt `getType` v of
        Nothing -> return TypeAny
        (Just t)-> return t

typeInfer (App _ e1 e2) = do
    t1 <- typeInfer e1
    t2 <- typeInfer e2
    except $ typeApp t1 t2

typeInfer (Abstr _ pat e) = do
    tpat <- typePat pat
    case patName pat of
        Nothing -> do
            t <- typeInfer e
            return $ TypeAbstr tpat t
        Just [v] -> do
            t <- local (insert v tpat) $ typeInfer e
            return $ TypeAbstr tpat t
        Just _ -> undefined

typeInfer (Con _ c) = return $ typeCon c

typeInfer (Ifx _ e1 op e2) = do
    let top = typeOp op
    t1 <- typeInfer e1
    t2 <- typeInfer e2
    tapp1 <- except $ typeApp top t1
    except $ typeApp tapp1 t2
    

typeInfer (TypeSig _ t e) = do
    t' <- typeInfer e
    if t `typeEq` t'
        then return t
        else throwE $ ErrTConflict t t'

typeInfer (List _ es) = do
    types <- mapM typeInfer es
    totalType <- foldM typePrec TypeAny types
    return $ TypeList totalType

typeInfer (Case _ e_t alts) = do
    t_target <- typeInfer e_t
    (t_pats, t_exprs) <- typeAlts alts
    if t_pats `typeEq` t_target
        then return t_exprs
        else throwE $ ErrTContext "case expression target" $ ErrTConflict t_target t_pats

typeInfer _ = throwE ErrTNotImpl

typeAlts :: [Alt] -> Typing (Type, Type)
typeAlts alts = do
    let (pats, es) = unzip alts
    t_pats  <- mapM typePat pats
    t_pat   <- foldM typePrec TypeAny t_pats
    t_es    <- mapM typeInfer es
    t_e     <- foldM typePrec TypeAny t_es
    return (t_pat, t_e)

typeCon :: Constant -> Type
typeCon (I _) = TypeInt
typeCon (B _) = TypeBool

typeOp :: Op -> Type
typeOp op = opinfo_type $ getOpInfo op

typePat :: Pattern -> Typing Type
typePat (ConPat c) = return $ typeCon c
typePat (VarPat _) = return TypeAny
typePat (TypedPat t pat) = do
    tpat <- typePat pat
    typePrec t tpat

patName :: Pattern -> Maybe [Name]
patName (ConPat _) = Nothing
patName (VarPat v) = return [v]
patName (TypedPat _ pat) = patName pat