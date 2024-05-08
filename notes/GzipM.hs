module GzipM where

import Data.Generics (Data, Typeable, GenericQ, GenericM, gzipWithM, toConstr, cast)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad (mzero, guard)
import Control.Monad.Trans (lift)
import Control.Applicative ((<|>))

-- (Used in example below)
import Control.Monad.State.Lazy (State, evalState, get, put)
import Control.Monad.Trans.Maybe (runMaybeT)
import Test.HUnit ((~:), (~?=), test, runTestTT)


gzipM :: (Monad m, Typeable m)
    => GenericQ (GenericM (MaybeT m)) -> GenericQ (GenericM (MaybeT m))
-- Run a generic zip through two structures using a generic function f.
-- If f returns MaybeT Nothing (mzero), then zip into that structure.
-- Returns MaybeT Nothing on a structure mismatch.
gzipM f x y = do
    f x y <|> if toConstr x == toConstr y
        then gzipWithM (gzipM f) x y
        else mzero

mkMMMaybeT :: (Monad m, Typeable m, Data a)
    => (a -> a -> MaybeT m a) 
    -> (forall b. Data b => b -> forall c. Data c => c -> MaybeT m c)
-- Make a function :: a -> a -> MaybeT m a (where MaybeT Nothing 
-- indicates "continue zipping") into a general function that zips into
-- types that do not match f.
-- Note that:
-- (forall b. Data b => b -> forall c. Data c => c -> MaybeT m c)
-- is the same as GenericQ (GenericM (MaybeT m))
mkMMMaybeT f x y =
    case (cast x, cast y) of
        (Just (x' :: a), Just (y' :: a)) -> case cast (f x' y') of
                Just (res :: MaybeT m c)    -> res
                _                           -> mzero
        _                                -> mzero
mkMM :: (Monad m, Typeable m, Data a)
    => (a -> a -> m a)
    -> (forall b. Data b => b -> forall c. Data c => c -> MaybeT m c)
-- Make a function :: a -> a -> m a. Will only operate on args of
-- type a, otherwise it will continue zipping.
-- Note that:
-- (forall b. Data b => b -> forall c. Data c => c -> MaybeT m c)
-- is the same as GenericQ (GenericM (MaybeT m))
mkMM f x y =
    case (cast x, cast y) of
        (Just (x' :: a), Just (y' :: a)) -> lift $ go f x' y'
        _                                -> mzero
  where
    go :: (Monad m, Typeable m, Data a, Data c) => (a -> a -> m a) -> a -> a -> m c
    go f x y = case cast (f x y) of
        Just res    -> res
        Nothing     -> error "Should not be here"


------------------------------------------------------------------------
-- Example: 
------------------------------------------------------------------------

data Tree = Branch Tree Tree | Leaf Int | Leaf2 Int
    deriving (Data, Show, Typeable, Eq)

main = runTestTT $ test 
    [ (labL ++ " " ++ labR) ~: gEvalLeafZip l r ~?= allEvalLeafZip l r | (labL, l) <- trs, (labR, r) <- trs ]
tr1 = (Leaf 5) `Branch` ((Leaf 6) `Branch` (Leaf 7))
tr2 = (Leaf 10) `Branch` ((Leaf 11) `Branch` (Leaf 12))
tr3 = (Leaf 110) `Branch` (Leaf 120)
tr4 = (Leaf 100) `Branch` ((Leaf 200) `Branch` (Leaf2 300))

trs = [("tr1", tr1), ("tr2", tr2), ("tr3", tr3), ("tr4", tr4)]

-- Generic
gEvalLeafZip :: Tree -> Tree -> Maybe Tree
gEvalLeafZip tl tr = 
    let res = gzipM (mkMMMaybeT gLeafZip) tl tr
    in evalState (runMaybeT res) 1

gLeafZip :: Tree -> Tree -> MaybeT (State Int) Tree
gLeafZip (Leaf l) (Leaf r) = do
    id <- lift get
    lift $ put (id+1)
    return $ Leaf $ l+r + (id * 1000)
gLeafZip (Leaf l) (Leaf2 r) = do
    id <- lift get
    lift $ put (id+1)
    return $ Leaf2 $ l+r + (id * 1000)
gLeafZip (Leaf2 l) (Leaf2 r) = do
    id <- lift get
    lift $ put (id+1)
    return $ Leaf2 $ l + r + (id * 1000)
gLeafZip _ _ = mzero

-- Expected
allEvalLeafZip :: Tree -> Tree -> Maybe Tree
allEvalLeafZip tl tr = evalState (runMaybeT $ allLeafZip tl tr) 1

-- Note that this one uses mzero to mean "bad structure", not "unmodified"
allLeafZip :: Tree -> Tree -> MaybeT (State Int) Tree
allLeafZip (Leaf l) (Leaf r) = do
    id <- lift get
    lift $ put (id+1)
    return $ Leaf $ l + r + (id * 1000)
allLeafZip (Leaf2 l) (Leaf2 r) = do
    id <- lift get
    lift $ put (id+1)
    return $ Leaf2 $ l + r + (id * 1000)
allLeafZip (Leaf l) (Leaf2 r) = do
    id <- lift get
    lift $ put (id+1)
    return $ Leaf2 $ l+r + (id * 1000)
allLeafZip (Branch l_1 l_2) (Branch r_1 r_2) = do
    _2 <- allLeafZip l_2 r_2
    _1 <- allLeafZip l_1 r_1
    return $ Branch _1 _2
allLeafZip _ _ = mzero
