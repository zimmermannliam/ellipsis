data Tree = Leaf Int | Branch Tree Tree

f :: Tree -> Maybe Tree
f i = do
    Just (Leaf 1) <- i
    return (Leaf 7)