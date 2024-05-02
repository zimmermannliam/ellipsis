module Main where

import GenericHelper

import Test.HUnit hiding (State)
import Data.Generics
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Control.Monad (mzero)

main = runTestTTAndExit $ test [
      "tr1 tr2" ~: (gzipEvalState tr1 tr2) ~?= (allEvalState tr1 tr2)
    , "tr1 tr1" ~: (gzipEvalState tr1 tr1) ~?= (allEvalState tr1 tr1)
    , "tr2 tr2" ~: (gzipEvalState tr2 tr2) ~?= (allEvalState tr2 tr2)
--    , "tr1 tr3" ~: (gzipEvalState tr1 tr3) ~?= (allEvalState tr1 tr3)
    ]

data Tree = Branch Tree Tree | Leaf Int
    deriving (Data, Show, Typeable, Eq)

tr1 = (Leaf 5) `Branch` ((Leaf 6) `Branch` (Leaf 7))
tr2 = (Leaf 10) `Branch` ((Leaf 11) `Branch` (Leaf 12))
tr3 = (Leaf 110) `Branch` (Leaf 120)

leafZip :: Tree -> Tree -> Maybe (State Int Tree)
leafZip (Leaf l) (Leaf r) = Just (go l r)
    where
    go :: Int -> Int -> State Int Tree
    go l r = do
        id <- get
        put (id+1)
        return $ Leaf $ l + r + (id * 1000)
leafZip _ _ = Nothing

leafZipAll :: Tree -> Tree -> MaybeT (State Int) Tree
leafZipAll (Leaf l) (Leaf r) = do
        id <- lift get
        lift $ put (id+1)
        return $ Leaf $ l + r + (id * 1000)
leafZipAll (Branch l_1 l_2) (Branch r_1 r_2) = do
    _2 <- leafZipAll l_2 r_2
    _1 <- leafZipAll l_1 r_1 -- done in reverse order due to gzipM quirks..
    return $ Branch _1 _2
leafZipAll _ _ = mzero

gzipEvalState :: Tree -> Tree -> Maybe Tree
gzipEvalState tl tr = fmap (\x -> evalState x 0) (gzipM (mkMMMaybe leafZip) tl tr)

allEvalState :: Tree -> Tree -> Maybe Tree
allEvalState tr tl = evalState (runMaybeT $ leafZipAll tl tr) 0