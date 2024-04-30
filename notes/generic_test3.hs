{-
gzipM :: (Monad m, Typeable m, Data a, Data b) => (a -> b -> m b) -> GenericQ (GenericM m)
gzipM f x y = case (cast x, cast y) of
        (Just (x' :: a), Just (y' :: b)) -> case cast (f x' y') of
            Just (res :: m a2) -> res
            _                  -> error "gzipM: bad type result"
        _ -> if toConstr x == toConstr y
            then gzipWithM (gzipM f) x y
            else error "gzipM: bad structure"

gzipM :: (Monad m, Typeable m, Data a, Data b) 
    => (a -> b -> m b) -> (forall c.Data c => c -> (forall d. Data d => d-> Maybe (m d)))
gzipM f x y = Just (gzipM' f x y)
    where
    gzipM' :: (Monad m, Typeable m, Data a, Data b) 
        => (a -> b -> m b) -> (forall c. Data c => c -> (forall d. Data d => d-> m d))
    gzipM' f x y = case (cast x, cast y) of
            (Just (x' :: a), Just (y' :: b)) -> case cast (f x' y') of
                Just (res :: m d) -> res
                _                 -> error "gzipM': bad type result"
            _ -> if toConstr x == toConstr y
                then gzipWithM (gzipM' f) x y
                else error "gzipM': bad structure"

gzipM :: (Monad m, Typeable m) 
    => (forall a. Data a => a -> (forall b. Data b => b -> Maybe (m b)))
    -> (forall c. Data c => c -> (forall d. Data d => d -> Maybe (m d)))
gzipM f x y = Just $ gzipM' f x y
    where
    gzipM' :: (Monad m, Typeable m) 
        => (forall a. Data a => a -> (forall b. Data b => b -> Maybe (m b)))
        -> (forall c. Data c => c -> (forall d. Data d => d -> m d))
    gzipM' f x y = case (f x y) of
        Just res    -> res
        Nothing     -> if toConstr x == toConstr y
            then gzipWithM (gzipM' f) x y
            else error $ "gzipM': Bad structures: " ++ gshow x ++ " ||| " ++ gshow y


gzipM :: (Monad m)
    => GenericQ (GenericM (MaybeG m))
    -> GenericQ (GenericM (MaybeG m))

gzipM :: (Monad m, Typeable m)
    => GenericQ (GenericM MaybeT m)
    -> GenericQ (GenericM MaybeT m)
gzipM = Nothing
mkMM' :: (Monad m, Typeable m, Data a)
    => (a -> a -> m a)
    -> (forall b. Data b => b -> (forall c. Data c => c -> MaybeT m c))
    -- -> GenericQ (GenericM (MaybeT m))
mkMM' f x y =
    case (cast x, cast y) of
        (Just (x' :: a), Just (y' :: a)) -> mzero
        _ -> trace ("Cast: " ++ gshow x ++ " ||| " ++ gshow y) mzero

gzipM :: (Monad m, Typeable m, Data a, Data b) => (a -> b -> m a) -> GenericQ (GenericM m)
gzipM f x y = case (cast x, cast y) of
        (Just (x' :: a), Just (y' :: b)) -> case cast (f x' y') of
            Just (res :: m a2) -> res
            _                  -> error "gzipM: bad type result"
        _ -> if toConstr x == toConstr y
            then gzipWithM (gzipM f) x y
            else error "gzipM: bad structure"
gzipM_ :: (Monad m, Typeable m) 
    => (forall a. Data a => a -> (forall b. Data b => b -> Maybe (m b)))
    -> (forall c. Data c => c -> (forall d. Data d => d -> Maybe (m d)))
gzipM_ f x y = Just $ gzipM' f x y
    where
    gzipM' :: (Monad m, Typeable m) 
        => (forall a. Data a => a -> (forall b. Data b => b -> Maybe (m b)))
        -> (forall c. Data c => c -> (forall d. Data d => d -> m d))
    gzipM' f x y = case (f x y) of
        Just res    -> res
        Nothing     -> if toConstr x == toConstr y
            then gzipWithM (gzipM' f) x y
            else error $ "gzipM': Bad structures: " ++ gshow x ++ " ||| " ++ gshow y
-}
import Data.Generics
import Data.Maybe
import Control.Monad.State
import Debug.Trace
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Control.Applicative ((<|>))
import Control.Monad (mzero)

data Tree = Branch Tree Tree | Leaf Int
    deriving (Data, Show, Typeable)

data Tree2 = Branch2 Tree2 Tree2 | Leaf2 LeafThing
    deriving (Data, Show, Typeable)

data LeafThing = LeafThing Int | LeafResult Int (Int, Int)
    deriving (Data, Show, Typeable)

tr1 = (Leaf 5) `Branch` ((Leaf 6) `Branch` (Leaf 7))
tr2 = (Leaf 10) `Branch` ((Leaf 11) `Branch` (Leaf 12))
tr3 = (Leaf 110) `Branch` (Leaf 120)

tr1' :: Tree2
tr1' = (Leaf2 $ LeafThing 5) `Branch2` ((Leaf2 $ LeafThing 6) `Branch2` (Leaf2 $ LeafThing 7))
tr2' = (Leaf2 $ LeafThing 10) `Branch2` ((Leaf2 $ LeafThing 11) `Branch2` (Leaf2 $ LeafThing 12))



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

numTreeNoFive' :: Tree -> Maybe Tree
numTreeNoFive' t = evalState (runMaybeT (go t)) 0
    where
    go :: Tree -> MaybeT (State Int) Tree
    go (Branch l r) = do
        l' <- go l
        r' <- go r
        return (Branch l' r')
    go (Leaf i) = do
        id' <- lift get
        lift $ put (id'+1)
        guard $ i /= 5
        return (Leaf $ id'+1)

numTreeNoFive :: Tree2 -> Maybe Tree2
numTreeNoFive t = evalState (runMaybeT $ everywhereM (mkM go) t) 0
    where
    go :: LeafThing -> MaybeT (State Int) LeafThing
    go (LeafThing i) = do
        guard $ i /= 5
        id' <- lift get
        put (id' + 1)
        return $ LeafThing (id')



eafZip :: LeafThing -> LeafThing -> LeafThing
eafZip (LeafThing l) (LeafThing r) =  LeafResult 0 (l, r)
eafZip _ _ = error "Tried to run leafzip on processed tree"


zipTreeWithState' :: Tree2 -> Tree2 -> Tree2
zipTreeWithState' l r = evalState (treeZip l r) 0
    where
    treeZip :: GenericQ (GenericM (State Int))
    treeZip l r = case (cast l, cast r) of
        (Just (l' :: LeafThing), Just (r' :: LeafThing)) -> case cast (leafZip l' r') of
            Just (res :: State Int a1) -> res
            _                                 -> error "weird"
        _ -> if toConstr l == toConstr r then gzipWithM treeZip l r
        else error "bad"

leafZip :: LeafThing -> LeafThing -> State Int LeafThing
leafZip (LeafThing l) (LeafThing r) = do
    id <- get
    put (id+1)
    return $ LeafResult id (l, r)
leafZip _ _ = error "Tried to run leafzip on processed tree"

leafZip' :: Tree -> Tree -> Maybe (State Int Tree)
leafZip' (Leaf l) (Leaf r) = Just (go l r)
    where
    go :: Int -> Int -> State Int Tree
    go l r = do
        id <- get
        put (id+1)
        return $ Leaf $ l + r + (id * 1000)
leafZip' _ _ = Nothing


gzipM :: (Monad m, Typeable m) 
    => (forall a. Data a => a -> (forall b. Data b => b -> Maybe (m b)))
    -> (forall c. Data c => c -> (forall d. Data d => d -> Maybe (m d)))
gzipM f x y = Just (gzipM' f x y)
    where
    gzipM' :: (Monad m, Typeable m) 
        => (forall a. Data a => a -> (forall b. Data b => b -> Maybe (m b)))
        -> (forall c. Data c => c -> (forall d. Data d => d -> m d))
    gzipM' f x y = case (f x y) of
        Just res    -> res
        Nothing     -> if toConstr x == toConstr y
            then gzipWithM (gzipM' f) x y
            else error "gzipM: Structure mismatch"


mkMM :: (Monad m, Typeable m, Data a)
    => (a -> a -> m a)
    -> (forall b. Data b => b -> (forall c. Data c => c -> Maybe (m c)))
mkMM f x y = 
    case (cast x, cast y) of
        (Just (x' :: a), Just (y' :: a)) -> cast (f x' y')
        _                                -> Nothing

mkMMMaybe :: (Monad m, Typeable m, Data a)
    => (a -> a -> Maybe (m a))
    -> (forall b. Data b => b -> (forall c. Data c => c -> Maybe (m c)))
mkMMMaybe f x y =
    case (cast x, cast y) of
        (Just (x' :: a), Just (y' :: a)) -> case (f x' y') of
            Just res -> cast res
            Nothing  -> Nothing
        _                                -> Nothing