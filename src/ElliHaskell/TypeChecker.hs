module ElliHaskell.TypeChecker (typeInfer, ErrType, typeApp) where

import ElliHaskell.Syntax
import ElliHaskell.Types

import Data.Map ((!?), insert, fromList, toList)
import Control.Monad (mapM, foldM)

data ErrType
    = ErrTConflict Type Type
    | ErrTPattern Pattern
    | ErrNotAbstr Type
    | ErrTNotImpl
    deriving (Eq, Show)

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither _ (Just x)    = return x
maybeToEither err Nothing   = Left err

typeEq :: Type -> Type -> Bool
typeEq TypeAny _    = True
typeEq _ TypeAny    = True
typeEq (TypeAbstr t1 t2) (TypeAbstr t1' t2') = (t1 `typeEq` t1') && (t2 `typeEq` t2')
typeEq t t'        = t == t'

typePrec :: Type -> Type -> Either ErrType Type
typePrec t TypeAny     = return t
typePrec TypeAny t'    = return t'
typePrec (TypeAbstr t1 t2) (TypeAbstr t1' t2') = do
    t1_res <- typePrec t1 t1'
    t2_res <- typePrec t2 t2'
    return $ TypeAbstr t1_res t2_res
typePrec t t'          | t == t' = return t
                       | otherwise = Left $ ErrTConflict t t'

typeApp :: Type -> Type -> Either ErrType Type
typeApp (TypeAbstr t1 t2) t'
    | t1 `typeEq` t'    = return t2
    | otherwise         = Left $ ErrTConflict t1 t'
typeApp t _ = Left $ ErrNotAbstr t

------------------------------------------------------------------------
-- 
-- Type checking
--
------------------------------------------------------------------------

typeInfer :: Cxt -> Expr -> Either ErrType Type

typeInfer cxt (Var _ v)     = case cxt `getType` v of
    Nothing -> return TypeAny
    (Just t)-> return t

typeInfer cxt (App _ e1 e2) = do
    t1 <- typeInfer cxt e1
    t2 <- typeInfer cxt e2
    typeApp t1 t2

typeInfer cxt (Abstr _ pat e) = do
    tpat <- typePat pat
    case patName pat of
        Nothing -> do
            t <- typeInfer cxt e
            return $ TypeAbstr tpat t
        Just [v] -> do
            let cxt' = insertType v tpat cxt
            t <- typeInfer cxt' e
            return $ TypeAbstr tpat t


typeInfer _ (Con _ c) = return $ typeCon c


typeInfer cxt (Ifx _ e1 op e2) = do
    let top = typeOp op
    t1 <- typeInfer cxt e1
    t2 <- typeInfer cxt e2
    tapp1 <- typeApp top t1
    typeApp tapp1 t2
    

typeInfer cxt (TypeSig _ t e) = do
    t' <- typeInfer cxt e
    if t `typeEq` t'
        then return t
        else Left $ ErrTConflict t t'

typeInfer cxt (List _ es) = do
    types <- mapM (typeInfer cxt) es
    totalType <- foldM typePrec TypeAny types
    return $ TypeList totalType

typeInfer _ _ = Left ErrTNotImpl

typeCon :: Constant -> Type
typeCon (I _) = TypeInt
typeCon (B _) = TypeBool

typeOp :: Op -> Type
typeOp op = opinfo_type $ getOpInfo op

typePat :: Pattern -> Either ErrType Type
typePat (ConPat c) = return $ typeCon c
typePat (VarPat v) = return TypeAny
typePat (TypedPat t pat) = do
    tpat <- typePat pat
    typePrec t tpat

patName :: Pattern -> Maybe [Name]
patName (ConPat _) = Nothing
patName (VarPat v) = return [v]
patName (TypedPat _ pat) = patName pat