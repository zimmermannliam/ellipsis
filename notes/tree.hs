import GzipM (gzipM, mkMM, mkMMMaybeT)
import Data.Generics    
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Identity
import Control.Monad.State.Lazy

data Tree = Branch2 Tree Tree | Branch3 Tree Tree Tree | Leaf Int
        deriving (Data, Show, Eq)


t1 = (Leaf 10) `Branch2` (Branch3 (Leaf 1) (Leaf 2) (Leaf 3))
{-
        .
       / \
      /   \
     10    .
          /|\
         / | \
        1  2  3
-}

t2 = (Leaf 12) `Branch2` (Branch3 (Leaf 4) (Leaf 5) (Leaf 6))
{-
        .
       / \
      /   \
     12    .
          /|\
         / | \
        4  5  6
-}

t3 = (Leaf 1) `Branch2` (Leaf 2)
{-  .
   / \
  1   2
-}

addM :: Int -> Int -> Identity Int
addM l r = return $ l + r

-- gzipM (mkMM addM) t1 t2

{-
        .
       / \
      /   \
   10+12   .
          /|\
         / | \
        /  |  \
      1+4 2+5 3+6
-}

addState :: Int -> Int -> State Int Int
addState l r = do
    id <- get
    put (id+1)
    return $ l + r + 1000 * id