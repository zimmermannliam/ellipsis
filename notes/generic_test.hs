import Data.Generics
import Data.Maybe
import Control.Monad.State
    ( State, runState, MonadState(put, get) )

data Tree = Branch Tree Tree | Leaf Int | Label TreeLabel
    deriving (Data, Show, Typeable)

data TreeLabel = TreeLabel {
    tid :: Int
    , valueL :: Int
    , valueR :: Int
    }
    deriving (Data, Show, Typeable)

labelTrees :: Tree -> Tree -> Maybe Tree
labelTrees = gzipWithMaybe go
    where
    go :: Tree -> Tree -> Maybe Tree
    go (Leaf l) (Leaf r) = Just $ Label $ TreeLabel {tid=0, valueL = l, valueR = r}
    go _ _ = Nothing

tr1 = (Leaf 5) `Branch` ((Leaf 6) `Branch` (Leaf 7))
tr2 = (Leaf 10) `Branch` ((Leaf 11) `Branch` (Leaf 12))
tr3 = (Leaf 110) `Branch` (Leaf 120)

gzipWithMaybe :: (Typeable a, Data a) => (a -> a -> Maybe a) -> a -> a -> Maybe a
gzipWithMaybe f tl tr = gzip (\x y -> mkTTMaybe f x y) tl tr

gzipWithStateT :: (Typeable a, Data a) => (a -> a -> State s a) -> a -> a -> Maybe (State s a)
gzipWIthStateT f tl tr = gzipState f tl tr

gzipState :: GenericQ (GenericM (State s)) -> GenericQ (GenericM 


sumTrees t1 t2 = gzip (\x y -> mkTTMaybe addLeaf x y) t1 t2

myBS :: Int -> Maybe (State Int Int)
myBS i  | i == 5    = Nothing
        | i == 6    = return (return 5)
        | otherwise = return (return i)


pairLeaf :: Tree -> Tree -> [(Int, Int)]
pairLeaf (Leaf l) (Leaf r) = [(l, r)]
pairLeaf _ _ = []


addLeaf :: Tree -> Tree -> Maybe Tree
addLeaf (Leaf l) (Leaf r) = Just (Leaf (l+r))
addLeaf _ _ = Nothing

{-
addLeaf' :: Tree -> a -> Tree
addLeaf' (Leaf l) (Leaf r) = Leaf (l+r)
addLeaf' l _ = l

addLeaves :: Tree -> Tree -> Maybe Tree
addLeaves tl tr = gzipWithM (\l r -> go l r) tl tr
    where
    go :: GenericQ (GenericM Maybe)
    go l r | toConstr l /= toConstr r = Nothing
           | otherwise = (Nothing `mkQ` (mkM addLeaf')) l r
                            `orElse`
                            gzipWithM go l r

isLeaf x = (False `mkQ` isLeaf) x
    where
    isLeaf' :: Tree -> Bool
    isLeaf' (Leaf _) = True
    isLeaf' _ = False

-}



{-
testgzipToList = gzipToList (\l r -> (l, r)) tr1 tr2

resetLeaves :: Tree -> Tree
resetLeaves = everywhere (mkT resetInt)

getLeaves :: Tree -> [Int]
getLeaves = everything (++) ([] `mkQ` getInt)
    where 
    getInt :: Int -> [Int]
    getInt i = [i]

getAndResetLeaves :: Tree -> ([Int], Tree)
getAndResetLeaves t = (getLeaves t, resetLeaves t)

addInt :: Int -> [Int] -> Maybe [Int]
addInt i l = Just (i:l)

resetInt :: Int -> Int
resetInt _ = 0

resetLeaf :: Tree -> Tree
resetLeaf (Leaf _)  = Leaf 0
resetLeaf t         = t
-}

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

mkTTMaybeState :: (Typeable a, Typeable b, Typeable c)
    => (a -> a -> State s (Maybe a)) -> b -> c -> State s (Maybe b)
mkTTMaybeState f x y = case (cast x, cast y) of
    (Just (x'::a), Just (y'::a))    -> f x' y' >>= go
    where
        go :: (Typeable a, Typeable b) => Maybe a -> State s (Maybe b)
        go Nothing = return Nothing
        go (Just x) = return (cast x)