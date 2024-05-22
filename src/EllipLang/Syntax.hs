{-# LANGUAGE DeriveDataTypeable #-}

module EllipLang.Syntax where

import Data.Generics (Data, Typeable)
import qualified Data.Map as Map

data Expr = Var Name                -- Variable
          | App Expr Expr           -- Application
          | Abstr Name Expr         -- Abstraction
          | Value Val               -- Value Literal
          | Let Name Expr Expr      -- Let expression
          | Case Expr Alts          -- Case expression
          | LetRec Name Expr Expr   -- LetRec "Name" = Expr in Expr
          | ElliComp Expr EllipRanges
          | Cons Expr Expr          -- expr1:list
          | Cat Expr Expr           -- list1 ++ list2
          | Error String            -- Creates an error
          | Pair Expr Expr          -- (expr1, expr2)
          | ListElement Name Idx    -- x[k]
          | Trace String Expr Expr  -- Debug
          | Op IfxOp Expr Expr
          | Not Expr
          | Abs Expr
          | Btwn Expr Expr
          | Ellipsis Expr Expr
          | ElliFoldr Expr Expr IfxOp
          | ElliFoldl Expr Expr IfxOp
          -- Processing stuff
          | EllipVar Id
          | Index Idx
          | Infix Expr Expr Expr
          | ER ElliRange
          | ElliGroup Expr
          | PreElli
          deriving (Eq, Show, Data, Typeable)


data IfxOp = Eq
           | Lt 
           | Gt 
           | Leq
           | Geq
           | Neq
           | Or
           | And
           | Add
           | Sub
           | Mul
           | Div
           | Mod
           | VarOp String
    deriving (Eq, Show, Data, Typeable)

eq,lt,gt,leq,geq,neq,or,and,add,sub,mul,div',mod' :: Expr -> Expr -> Expr
eq = Op Eq
lt  = Op Lt 
gt  = Op Gt 
leq = Op Leq
geq = Op Geq
neq = Op Neq
or = Op Or
and = Op And
add = Op Add
sub = Op Sub
mul = Op Mul
div' = Op Div
mod' = Op Mod


data ElliRange = ElliRange {ed_id::Id, ed_t::ElliType, ed_ib::Expr, ed_ie::Expr}
    deriving (Eq, Show, Data, Typeable)
data ElliType = ElliList Name | ElliCounter
    deriving (Eq, Show, Data, Typeable)

data Val    = Con Int 
              | VCons Val Val
              | Empty 
              | Closure Name Expr Env
              | FreeVar Name
              | VPair Val Val
              | VStr String
              | Boolean Bool
             deriving (Eq, Show, Data, Typeable)

data Pattern = PCons Name Name
             | PCons' Expr Expr
             | PVar Name
             | PVal Val
             | PEllipsis Name Idx -- x1 ... xn -> x2 ... xn
             deriving (Eq, Show, Data, Typeable)

type Idx = Expr

-- Bindee?
data Bindee    = BVal Val
               | ListFuture Name
               | LenFuture Name
               | BIterator { it_ie :: Int,
                             it_ib :: Int,
                             it_ic :: Int,
                             vname :: Name,
                             content :: IteratorContent}
                deriving (Eq, Show, Data, Typeable)

data ContentType = BeList | BeIndices
    deriving (Eq, Show, Data, Typeable)

data IteratorContent = List [Val] | Indices
    deriving (Eq, Show, Data, Typeable)

data EllipRange = EllipRange { ident :: Id
                             , var :: Name
                             , ib :: Idx
                             , ie :: Idx
                             , contentT :: ContentType}
                             deriving (Eq, Show, Data, Typeable)

data EllipSide = Begin | EndSide
    deriving (Eq, Show, Data, Typeable)

data Identifier         = NamedVar Name | IdVar Id
    deriving (Eq, Show, Data, Typeable, Ord)

newtype EllipError        = EllipError String
    deriving (Eq, Show, Data, Typeable)

type EllipRanges        = [EllipRange]
type Name               = String
type Alts               = [(Pattern, Expr)]
type Id                 = Int
type Env                = Map.Map Identifier Bindee