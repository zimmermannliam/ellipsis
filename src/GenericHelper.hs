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

everywhereUntil :: GenericQ Bool -> GenericT -> GenericT
-- Variation on everywhere that executes f on the last thing to 
-- trigger q, but does not traverse inside.
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