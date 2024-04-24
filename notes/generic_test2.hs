import Data.Generics

data Tree = Branch Tree Tree | Leaf Int | Guard Int Tree
    deriving (Data, Show, Typeable)

tr1 = (Guard 0 $ Leaf 1) `Branch` ((Leaf 6) `Branch` (Leaf 7))

succAll :: Tree -> Tree
succAll = everywhereBut (False `mkQ` isGuard) (mkT succLeaf)

isGuard :: Tree -> Bool
isGuard (Guard _ _) = True
isGuard _ = False

succLeaf :: Tree -> Tree
succLeaf (Leaf i) = Leaf (i+1)
succLeaf (Guard i t) = Guard (i+1) t
succLeaf t        = t