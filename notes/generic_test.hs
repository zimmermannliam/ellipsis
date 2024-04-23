import Data.Generics

data Tree = Branch Tree Tree | Leaf Int
    deriving (Data, Show, Typeable)

tr1 = (Leaf 5) `Branch` ((Leaf 6) `Branch` (Leaf 7))
tr2 = (Leaf 10) `Branch` ((Leaf 11) `Branch` (Leaf 12))
tr3 = (Leaf 110) `Branch` (Leaf 120)


sumTrees t1 t2 = gzip (\x y -> mkTTMaybe addLeaf x y) t1 t2

getTreePairs t1 t2 = everythingB   

pairLeaf :: Tree -> Tree -> [(Int, Int)]
pairLeaf (Leaf l) (Leaf r) = [(l, r)]
pairLeaf _ _ = []


addLeaf :: Tree -> Tree -> Maybe Tree
addLeaf (Leaf l) (Leaf r) = Just (Leaf (l+r))
addLeaf _ _ = Nothing


mkTT :: (Typeable a, Typeable b, Typeable c)
    => (a -> a -> a) -> b -> c -> Maybe c
mkTT f x y = case (cast x, cast y) of
    (Just (x'::a), Just (y'::a))    -> cast (f x' y')
    _                               -> Nothing

mkTTMaybe :: (Typeable a, Typeable b, Typeable c)
    => (a -> a -> Maybe a) -> b -> c -> Maybe c
mkTTMaybe f x y = case (cast x, cast y) of
    (Just (x'::a), Just (y'::a))    -> case (f x' y') of
        Just res -> cast res
        Nothing -> Nothing
    _                               -> Nothing
