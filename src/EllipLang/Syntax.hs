{-# LANGUAGE DeriveDataTypeable #-}

module EllipLang.Syntax where

import Data.Generics ( Data )
import qualified Data.Map as Map

data Expr = Var Name                -- Variable
          | App Expr Expr           -- Application
          | Abstr Name Expr         -- Abstraction
          | Value Val               -- Value Literal
          | Let Name Expr Expr      -- Let expression
          | Case Expr Alts          -- Case expression
          | LetRec Name Expr Expr   -- LetRec "Name" = Expr in Expr
          | Ellipsis Expr EllipRanges
          | Cons Expr Expr          -- expr1:list
          | Cat Expr Expr           -- list1 ++ list2
          | Error String            -- Creates an error
          | Pair Expr Expr          -- (expr1, expr2)
          | ListElement Name Idx    -- x[k]
          | Trace String Expr Expr  -- Debug
          | Eq Expr Expr            -- Relational Operators
          | Lt Expr Expr
          | Gt Expr Expr
          | Leq Expr Expr
          | Geq Expr Expr
          | Neq Expr Expr
          | Or Expr Expr            -- Boolean Operators
          | Not Expr
          | And Expr Expr
          | Add Expr Expr           -- Arithmetic Operators
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Mod Expr Expr
          | Abs Expr
          | EllipVar Id
          | PreEllipsis Expr Expr
          | Index Idx
          deriving (Eq, Show, Data)

data Val    = Con Int 
              | VCons Val Val
              | Empty 
              | Closure Name Expr Env
              | FreeVar Name
              | VPair Val Val
              | VStr String
              | Boolean Bool
             deriving (Eq, Show, Data)

data Pattern = PCons Name Name
             | PCons' Expr Expr
             | PVar Name
             | PVal Val
             | PEllipsis Name Idx -- x1 ... xn -> x2 ... xn
             deriving (Eq, Show, Data)

data Idx    = IPlace Int        -- Reflect work they're doing -- 
            | End Name 
            | EPlace Expr -- Must evaluate to an integer
            deriving (Eq, Show, Data)

-- Bindee?
data Bindee    = BVal Val
               | ListFuture Name
               | LenFuture Name
               | BIterator { it_ie :: Int,
                             it_ib :: Int,
                             it_ic :: Int,
                             vname :: Name,
                             content :: IteratorContent}
                deriving (Eq, Show, Data)

data ContentType = BeList | BeIndices
    deriving (Eq, Show, Data)

data IteratorContent = List [Val] | Indices
    deriving (Eq, Show, Data)

data EllipRange = EllipRange { ident :: Id
                             , var :: Name
                             , ib :: Idx
                             , ie :: Idx
                             , contentT :: ContentType}
                             deriving (Eq, Show, Data)

data EllipSide = Begin | EndSide
    deriving (Eq, Show, Data)

data Identifier         = NamedVar Name | IdVar Id
    deriving (Eq, Show, Data, Ord)

newtype EllipError        = EllipError String
    deriving (Eq, Show, Data)

type EllipRanges        = [EllipRange]
type Name               = String
type Alts               = [(Pattern, Expr)]
type Id                 = Int
type Env                = Map.Map Identifier Bindee