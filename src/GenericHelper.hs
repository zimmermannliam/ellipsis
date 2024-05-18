{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

------------------------------------------------------------------------
-- 
-- SYB Helper functions
-- Liam Zimmermann
-- 
------------------------------------------------------------------------

module GenericHelper where

import Data.Generics (Data, Typeable, GenericT, GenericQ, GenericM, gzipWithM, toConstr, cast, gmapT, gmapQ, gzipWithQ
    , everything, mkQ)
import Control.Monad.Trans.Maybe
import Control.Monad (mzero, guard)
import Control.Monad.Trans (lift)
import Control.Applicative ((<|>))

everywhereUntil :: GenericQ Bool -> GenericT -> GenericT
-- Variation on everywhere:
-- Cease traversal if q holds for x, but run f x.
everywhereUntil q f = go
    where
    go :: GenericT
    go x
        | q x       = f x
        | otherwise = f (gmapT go x)

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
        (Just (x' :: a), Just (y' :: a)) -> case cast (f x' y') of
                Just (res :: m c)           -> res
                _                           -> mzero
        _                                -> mzero

-- | zip and query like everything
gzipQ :: forall r. (Show r) => (r -> r -> r) -> GenericQ (GenericQ (Either r r)) -> GenericQ (GenericQ (Maybe r))
gzipQ k f x y = go x y 
  where
    go :: (Show r) => GenericQ (GenericQ (Maybe r))
    go x y = case (f x y) of
        Right res -> Just res
        Left res  -> 
            if toConstr x == toConstr y 
            then do
                next <- sequence $ gzipWithQ go x y
                return $ foldl k res next
            else Nothing


-- | Default, query, left, right
-- use with gzipQ
mkQQ :: (Typeable b, Data b) => r -> (b -> b -> r) -> GenericQ (GenericQ (Either r r))
mkQQ dflt q x y = case (cast x, cast y) of
    (Just (x'::b), Just (y'::b)) -> Right (q x' y')
    _                            -> Left (dflt)

-- | Default, query, left, right
-- use with gzipQ
mkQQMaybe :: (Typeable b, Data b) => r -> (b -> b -> Maybe r) -> GenericQ (GenericQ (Either r r))
mkQQMaybe dflt q x y = case (cast x, cast y) of
    (Just (x'::b), Just (y'::b)) -> case q x' y' of
        Just res -> Right res
        Nothing  -> Left dflt
    _                            -> Left dflt

hoistMaybe :: (Applicative m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure

mapWithIdx :: (Int -> a -> b) -> [a] -> [b]
mapWithIdx f xs = map (uncurry f) $ enumerate xs
  where
    enumerate :: [a] -> [(Int, a)]
    enumerate = zip [0..]