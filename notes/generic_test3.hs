import Data.Generics
import Data.Maybe
import Control.Monad.State
import Debug.Trace

data Tree = Branch Tree Tree | Leaf Int
    deriving (Data, Show, Typeable)

data Tree2 = Branch2 Tree2 Tree2 | Leaf2 LeafThing
    deriving (Data, Show, Typeable)

data LeafThing = LeafThing Int
    deriving (Data, Show, Typeable)

tr1 = (Leaf 5) `Branch` ((Leaf 6) `Branch` (Leaf 7))
tr2 = (Leaf 10) `Branch` ((Leaf 11) `Branch` (Leaf 12))
tr3 = (Leaf 110) `Branch` (Leaf 120)

tr1' :: Tree2
tr1' = (Leaf2 $ LeafThing 5) `Branch2` ((Leaf2 $ LeafThing 6) `Branch2` (Leaf2 $ LeafThing 7))

treeToListSp :: Tree -> [Int]
treeToListSp (Leaf i) = [i]
treeToListSp (Branch l r) = (treeToList l) ++ (treeToList r)

{-
treeToList :: Tree -> [Int]
treeToList = everything (++) (mkQ [] treeToList')
    where
    treeToList' (Leaf i) = [i]
    treeToList' _        = []
    -}

treeToList :: Tree -> [Int]
treeToList t = (treeToList' t) ++ (concat $ gmapQ (mkQ [] treeToList) t)
    where
    treeToList' (Leaf i) = [i]
    treeToList' _        = []

zipLeavesSp :: Tree -> Tree -> Maybe [(Int, Int)]
zipLeavesSp (Leaf l) (Leaf r) = Just [(l, r)]
zipLeavesSp (Branch ll lr) (Branch rl rr) = do
    l <- (zipLeavesSp ll rl)
    r <- (zipLeavesSp lr rr)
    return (l ++ r)
zipLeavesSp _ _ = Nothing

zipLeavesErr :: Tree -> Tree -> [(Int, Int)]
zipLeavesErr l r = zipLeaves' l r
    where
    zipLeaves' :: GenericQ (GenericQ [(Int, Int)])
    zipLeaves' l r = (zipTwoLeaves l r) ++ (concat $ gzipWithQ zipLeaves' l r)

    zipTwoLeaves :: (Data a, Data b) => a -> b -> [(Int, Int)]
    zipTwoLeaves l r = case ((cast l) :: Maybe Tree, (cast r) :: Maybe Tree) of
        (Just (l'::Tree), Just (r'::Tree))  -> zipTwoLeaves' l' r'
        _                                   -> []

    zipTwoLeaves' (Leaf l) (Leaf r) = [(l, r)]
    zipTwoLeaves' _ _ = []

zipLeaves :: Tree -> Tree -> Maybe [(Int, Int)]
zipLeaves l r = zipLeaves' l r
    where
    zipLeaves' :: GenericQ (GenericQ (Maybe [(Int, Int)]))
    zipLeaves' l r 
        | toConstr l /= toConstr r = Nothing
        | otherwise = do
            me <- zipTwoLeaves l r
            let he = concat <$> concat <$> sequence $ gzipWithQ zipLeaves' l r
            return (me ++ he)
    
    zipTwoLeaves :: (Data a, Data b) => a -> b -> Maybe [(Int, Int)]
    zipTwoLeaves l r = case ((cast l) :: Maybe Tree, (cast r) :: Maybe Tree) of
        (Just (l'::Tree), Just (r'::Tree))  -> Just $ zipTwoLeaves' l' r'
        _ -> Nothing
        -- (Nothing, _) -> Nothing
        -- (_, Nothing) -> Nothing

    zipTwoLeaves' (Leaf l) (Leaf r) = [(l, r)]
    zipTwoLeaves' _ _ = []

{-
runStateEverywhere :: Data a => (a -> State s a) -> s -> a -> (a, s)
runStateEverywhere f initState x = runState ((everywhereM f) x) initState
-}

addInts :: Int -> Int -> Int
addInts l r = (l+r)

mkTT :: (Typeable a, Typeable b, Typeable c)
        => (a -> a -> a) -> b -> c -> Maybe c
mkTT (f::a -> a -> a) x y =
    case (cast x,cast y) of
    (Just (x'::a),Just (y'::a)) -> cast (f x' y')
    _                           -> Nothing

numberLeaves' :: Tree -> Tree
numberLeaves' t = evalState (go t) 0
    where
    go :: Tree -> State Int Tree
    go (Branch l r) = do
        l' <- go l
        r' <- go r
        return (Branch l' r')
    go (Leaf _) = do
        id <- get
        put (id+1)
        return (Leaf id)

numberLeaves :: Tree2 -> Tree2
numberLeaves t = evalState (everywhereM (mkM go) t) 0
    where
    go :: LeafThing -> State Int LeafThing
    go (LeafThing _) = do
        id <- get
        put (id+1)
        return (LeafThing id)

runStateEverywhere :: (Data a, Data b) => (b -> State s b) -> s -> a -> (a, s)
runStateEverywhere f initState t = runState (everywhereM (mkM f) t) initState

numLeaf :: LeafThing -> State Int LeafThing
numLeaf (LeafThing _) = do
    id <- get
    put (id+1)
    return (LeafThing id)