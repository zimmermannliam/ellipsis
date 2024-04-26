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
          | Ellipsis Expr Expr
          | ElliFold Expr Expr Expr -- (t1) + ... + (tn) would be ElliFold t1 t2 +
          | Index Idx
          | Infix Expr Expr Expr
          | EHD ElliHaskellData
          deriving (Eq, Show, Data, Typeable)
infix 8 `Pair`
infix 8 `Gt`
infix 8 `Lt`
infix 8 `Geq`
infix 8 `Leq`

data ElliHaskellData = ElliHaskellData { ehs_ib :: Idx
                            , ehs_ie :: Idx
                            , ehs_name :: Name
                            , ehs_id :: Maybe Int }
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

data Idx    = IPlace Int        -- Reflect work they're doing -- 
            | End Name 
            | EPlace Expr -- Must evaluate to an integer
            deriving (Eq, Show, Data, Typeable)

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