{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

------------------------------------------------------------------------
-- 
-- SYB Helper functions
-- Liam Zimmermann
-- 
------------------------------------------------------------------------

module GenericHelper where

import Data.Generics
import Control.Monad.State
import Debug.Trace (trace)

everywhereUntil :: GenericQ Bool -> GenericT -> GenericT
-- Variation on everywhere:
-- Cease traversal if q holds for x, but run f x.
everywhereUntil q f = go
    where
    go :: GenericT
    go x
        | q x       = f x
        | otherwise = f (gmapT go x)



mkTTMaybe :: (Typeable a, Typeable b, Typeable c) => 
    (a -> a -> Maybe a) -> b -> c -> Maybe c
-- Takes a function that, on nothing, will be traversed into.
mkTTMaybe f x y = case (cast x, cast y) of
    (Just (x'::a), Just (y'::a))    -> maybe Nothing cast (f x' y')
    _                               -> Nothing


runStateEverywhere :: (Data a, Data b) => (b -> State s b) -> s -> a -> (a, s)
runStateEverywhere f initState t = runState (everywhereM (mkM f) t) initState

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
            else error $ "gzipM: Structure mismatch: " ++ gshow x ++ " ||| " ++ gshow y


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