module EllipLang.Types where

import EllipLang.Syntax

data Type
    = TypeBool
    | TypeInt
    | TypePair Type Type
    | TypeList Type
    | TypeFun Type Type
    | TypeSome Int
    | TypeAny   
    deriving (Show, Eq, Data, Typeable)

typeEq :: Type -> Type -> Bool
typeEq TypeAny _ = True
typeEq _ TypeAny = True
typeEq (TypePair t1 t2) (TypePair t1' t2') = (typeEq t1 t1') && (typeEq t2 t2')
typeEq (TypeFun t1 t2) (TypeFun t1' t2') = (typeEq t1 t1') && (typeEq t2 t2')
typeEq (TypeList t) (TypeList t') = typeEq t t'
typeEq t1 t2 = t1 == t2

typeApp :: Type -> Type -> Bool
typeApp (TypeFun t1 _) t' = typeEq t1 t'
typeApp _ _               = False

typeCheck :: Expr -> Maybe Type
typeCheck (Var v)       = TypeAny
typeCheck (App e1 e2)   = do
    t1 <- typeCheck 
typeCheck (Value v)     = typeCheckVal v
typeCheck (Op op e1 e2) = typeCheckOp op e1 e2
typeCheck _             = Nothing

typeCheckOp :: IfxOp -> Expr -> Expr -> Maybe Type
typeCheckOp op e1 e2 = do
    t1 <- typeCheck e1
    t2 <- typeCheck e2
    (t1', t2', tr) <- typeOp
    if typeEq t1 t1' && typeEq t2 t2'
        then return tr
        else Nothing
  where
    typeOp :: Op -> (Type, Type, Type)
    typeOp Eq  = (TypeAny, TypeAny, TypeBool)
    typeOp Neq = (TypeAny, TypeAny, TypeBool)
    typeOp Lt  = (TypeInt, TypeInt, TypeBool)
    typeOp Gt  = (TypeInt, TypeInt, TypeBool)
    typeOp Leq = (TypeInt, TypeInt, TypeBool)
    typeOp Geq = (TypeInt, TypeInt, TypeBool)
    typeOp Or  = (TypeBool, TypeBool, TypeBool)
    typeOp And = (TypeBool, TypeBool, TypeBool)
    typeOp Add = (TypeInt, TypeInt, TypeInt)
    typeOp Sub = (TypeInt, TypeInt, TypeInt)
    typeOp Mul = (TypeInt, TypeInt, TypeInt)
    typeOp Div = (TypeInt, TypeInt, TypeInt)
    typeOp Mod = (TypeInt, TypeInt, TypeInt)
    typeOp VarOp _ = (TypeAny, TypeAny, TypeAny)

typeCheckVal :: Val -> Maybe Type
typeCheckVal (Con _)        = return TypeInt
typeCheckVal (Boolean _)    = return TypeBool
typeCheckVal (Empty)        = return $ TypeList TypeAny
typeCheckVal (VCons val vals)   = do
    t <- typeCheckVal val
    ts <- typeCheckVal vals
    if typeEq t ts
        then Just t
        else Nothing
typeCheckVal (VPair val val)  = do
    t1 <- typeCheckVal val
    t2 <- typeCheckVal val
    return $ TypePair val val
typeCheckVal _ = Nothing
