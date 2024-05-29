module ElliHaskell.Syntax where

import ElliHaskell.Types (Type (TypeInt, TypeBool, TypeAny, TypeAbstr, TypeList, TypeSome))

import Control.Monad.Combinators.Expr (Operator (InfixL, InfixN, InfixR))

import Data.Map (Map, fromList, insert, (!?), (!))

------------------------------------------------------------------------
--
-- AST
--
------------------------------------------------------------------------

data Stmt
    = StmtDecl Decl
    | StmtEval Expr
    | StmtType Expr
    | StmtQuit
    deriving (Eq, Show)

data Decl = Decl Info [Pattern] Expr
    deriving (Eq, Show)

data Expr
    = Var Info Name
    | App Info Expr Expr
    | Abstr Info Pattern Expr
    | Con Info Constant
    | Closure Info Env Pattern Expr
    | Ifx Info Expr Op Expr
    | TypeSig Info Type Expr
    | List Info [Expr]
    deriving (Eq, Show)

data Constant
    = I Int
    | B Bool
    deriving (Eq, Show)

data Pattern
    = ConPat Constant
    | VarPat Name
    | TypedPat Type Pattern
    deriving (Eq, Show)

type Val = Expr
type Name = String
type Info = Int

blank :: Info
blank = 0

------------------------------------------------------------------------
--
-- Environment
--
------------------------------------------------------------------------

type Env = Map Name Val
type Cxt = Map Name Type

getVal :: Env -> Name -> Maybe Val
getVal env v = env !? v

insertVal :: Name -> Val -> Env -> Env
insertVal = insert

getType :: Cxt -> Name -> Maybe Type
getType cxt v = cxt !? v

insertType :: Name -> Type -> Cxt -> Cxt
insertType = insert

------------------------------------------------------------------------
--
-- Operators
--
------------------------------------------------------------------------

data Op
    = Add
    | Sub
    | Mul
    | Div
    | Eq
    | Neq
    | Lt
    | Gt
    | Leq
    | Geq
    | And
    | Or
    | Cons
    | VarOp Name
    deriving (Eq, Show, Ord)


data OpInfo = OpInfo 
    { opinfo_prec :: Int
    , opinfo_ifxname :: Name
    , opinfo_prefixname :: Name
    , opinfo_label :: String
    , opinfo_type :: Type
    , opinfo_assoc :: Assoc
    }

getOpInfo :: Op -> OpInfo
getOpInfo op = ops ! op

data Assoc = AssocL | AssocR | AssocN
    deriving (Eq, Show)

ops :: Map Op OpInfo
ops =   fromList
    [ (Add, OpInfo  { opinfo_prec=4
                    , opinfo_ifxname="+"
                    , opinfo_prefixname="(+)"
                    , opinfo_label="addition"
                    , opinfo_type=TypeAbstr TypeInt (TypeAbstr TypeInt TypeInt)
                    , opinfo_assoc=AssocL
                    })
    , (Sub, OpInfo  { opinfo_prec=4
                    , opinfo_ifxname="-"
                    , opinfo_prefixname="(-)"
                    , opinfo_label="subtraction"
                    , opinfo_type=TypeAbstr TypeInt (TypeAbstr TypeInt TypeInt)
                    , opinfo_assoc=AssocL
                    })
    , (Mul, OpInfo  { opinfo_prec=5
                    , opinfo_ifxname="*"
                    , opinfo_prefixname="(*)"
                    , opinfo_label="multiplication"
                    , opinfo_type=TypeAbstr TypeInt (TypeAbstr TypeInt TypeInt)
                    , opinfo_assoc=AssocL
                    })
    , (Div, OpInfo  { opinfo_prec=5
                    , opinfo_ifxname="`div`"
                    , opinfo_prefixname="div"
                    , opinfo_label="division"
                    , opinfo_type=TypeAbstr TypeInt (TypeAbstr TypeInt TypeInt)
                    , opinfo_assoc=AssocL
                    })
    , (Eq, OpInfo   { opinfo_prec=2
                    , opinfo_ifxname="=="
                    , opinfo_prefixname="(==)"
                    , opinfo_label="equal-to"
                    , opinfo_type=TypeAbstr TypeAny (TypeAbstr TypeAny TypeBool)
                    , opinfo_assoc=AssocN
                    })
    , (Neq, OpInfo  { opinfo_prec=2
                    , opinfo_ifxname="/="
                    , opinfo_prefixname="(/=)"
                    , opinfo_label="not-equal-to"
                    , opinfo_type=TypeAbstr TypeAny (TypeAbstr TypeAny TypeBool)
                    , opinfo_assoc=AssocN
                    })
    , (Lt, OpInfo  { opinfo_prec=2
                    , opinfo_ifxname="<"
                    , opinfo_prefixname="(<)"
                    , opinfo_label="less-than"
                    , opinfo_type=TypeAbstr TypeInt (TypeAbstr TypeInt TypeBool)
                    , opinfo_assoc=AssocN
                    })
    , (Gt, OpInfo  { opinfo_prec=2
                    , opinfo_ifxname=">"
                    , opinfo_prefixname="(>)"
                    , opinfo_label="greater-than"
                    , opinfo_type=TypeAbstr TypeInt (TypeAbstr TypeInt TypeBool)
                    , opinfo_assoc=AssocN
                    })
    , (Leq, OpInfo  { opinfo_prec=2
                    , opinfo_ifxname="<="
                    , opinfo_prefixname="(<=)"
                    , opinfo_label="less-than-or-equal-to"
                    , opinfo_type=TypeAbstr TypeInt (TypeAbstr TypeInt TypeBool)
                    , opinfo_assoc=AssocN
                    })
    , (Geq, OpInfo  { opinfo_prec=2
                    , opinfo_ifxname=">="
                    , opinfo_prefixname="(>=)"
                    , opinfo_label="greater-than-or-equal-to"
                    , opinfo_type=TypeAbstr TypeInt (TypeAbstr TypeInt TypeBool)
                    , opinfo_assoc=AssocN
                    })
    , (And, OpInfo  { opinfo_prec=1
                    , opinfo_ifxname="&&"
                    , opinfo_prefixname="(&&)"
                    , opinfo_label="and"
                    , opinfo_type=TypeAbstr TypeBool (TypeAbstr TypeBool TypeBool)
                    , opinfo_assoc=AssocR
                    })
    , (Or, OpInfo  { opinfo_prec=0
                    , opinfo_ifxname="||"
                    , opinfo_prefixname="(||)"
                    , opinfo_label="or"
                    , opinfo_type=TypeAbstr TypeBool (TypeAbstr TypeBool TypeBool)
                    , opinfo_assoc=AssocR
                    })
    , (VarOp "", OpInfo { opinfo_prec=6
                     , opinfo_ifxname=""
                     , opinfo_prefixname=""
                     , opinfo_label="infix function"
                     , opinfo_type=TypeAbstr TypeAny (TypeAbstr TypeAny TypeAny)
                     , opinfo_assoc=AssocL
                     })
    , (Cons, OpInfo{ opinfo_prec=100
                   , opinfo_ifxname=":"
                   , opinfo_prefixname="(:)"
                   , opinfo_label="list cons"
                   , opinfo_type=TypeAbstr (TypeSome 1) (TypeAbstr (TypeList (TypeSome 1)) (TypeList (TypeSome 1)))
                   , opinfo_assoc=AssocL
                   })
    ]