module ElliHaskell.Types where

data Type
    = TypeSome Int
    | TypeInt
    | TypeBool
    | TypeAbstr Type Type
    | TypeList Type
    deriving (Eq, Show)
