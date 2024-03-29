import Data.Generics.Schemes
import Data.Generics
import Data.Data

{-# LANGUAGE DeriveDataTypeable #-}

data MyTree = Leaf Int
            | LeafSpecial Int
            | NodeA MyTree
            | NodeB MyTree MyTree
            | NodeC MyTree MyTree Int
            deriving (Data, Show)

mytree  = NodeB (Leaf 1) (LeafSpecial 2)
mytree2 = NodeC (NodeB (Leaf 1) (LeafSpecial 2)) (NodeA (LeafSpecial 3)) 4

pp :: MyTree -> String
pp (Leaf i)         = "L" ++ show i
pp (LeafSpecial i)  = "S" ++ show i
pp (NodeA t)        = "(" ++ pp t ++ ")"
pp (NodeB t1 t2)    = "[(" ++ pp t1 ++ ")(" ++ pp t2 ++ ")]"
pp (NodeC t1 t2 i)  = show i ++ "{(" ++ pp t1 ++ ")(" ++ pp t2 ++ ")}" 

replaceSpecialRecursive :: MyTree -> MyTree
replaceSpecialRecursive (Leaf i)         = Leaf i
replaceSpecialRecursive (LeafSpecial i)  = Leaf i
replaceSpecialRecursive (NodeA t)        = NodeA $ replaceSpecialRecursive t
replaceSpecialRecursive (NodeB t1 t2)    = NodeB (replaceSpecialRecursive t1) (replaceSpecialRecursive t2)
replaceSpecialRecursive (NodeC t1 t2 i)  = NodeC (replaceSpecialRecursive t1) (replaceSpecialRecursive t2) i


replaceSpecial :: MyTree -> MyTree
replaceSpecial t = everywhere (mkT replaceSpecial') t
    where   replaceSpecial' :: MyTree -> MyTree
            replaceSpecial' (LeafSpecial i) = Leaf i
            replaceSpecial' t               = t
