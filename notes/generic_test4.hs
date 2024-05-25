import Data.Generics
import Data.Maybe
import Debug.Trace
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Control.Monad (mzero)

data Tree = Branch Tree Tree | Leaf Int
    deriving (Data, Show, Typeable)

tr1 = (Leaf 5) `Branch` ((Leaf 6) `Branch` (Leaf 7))
tr2 = (Leaf 10) `Branch` ((Leaf 11) `Branch` (Leaf 12))
tr3 = (Leaf 110) `Branch` (Leaf 120)

leafZip :: Tree -> Tree -> MaybeT (State Int) Tree
leafZip (Leaf l) (Leaf r) = do
    id <- lift get
    lift $ put (id+1)
    return $ Leaf $ l+r + (id * 1000)
leafZip _ _ = mzero

gzipM :: (Monad m, Typeable m)
    => (forall a. Data a => a -> (forall b. Data b => b -> MaybeT m b))
    -> (forall c. Data c => c -> (forall d. Data d => d -> m d))
gzipM f x y = do
    res <- runMaybeT (f x y)
    case res of
        Just res'   -> return res'
        Nothing     -> if toConstr x == toConstr y
            then gzipWithM (gzipM f) x y
            else error $ "gzipM: Structure mismatch: " ++ gshow x ++ " ||| " ++ gshow y

mkMMMaybeT :: (Monad m, Typeable m, Data a)
    => (a -> a -> MaybeT m a)
    -> (forall b. Data b => b -> (forall c. Data c => c -> MaybeT m c))
mkMMMaybeT f x y =
    case (cast x, cast y) of
        (Just (x' :: a), Just (y' :: a)) -> case cast (f x' y') of
            Just (res :: MaybeT m c) -> res
            _                        -> mzero
        _                                -> mzero

data TestRip = R0 Int | R1 [Int] | R2 [[Int]] | R3 [[[Int]]]
    deriving (Show, Data, Typeable)

add1 :: Int -> Int
add1 i = i+1