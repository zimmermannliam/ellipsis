module Mytest where

import Prelude ((+), (-), div, (*), mod, (>), (>=), (<), (<=), (==), (&&), (||), not, error, (++), map, reverse, take, drop, length, Int, ($), otherwise)
range :: [a] -> Int -> Int -> [a] 
range xs i j     | i < 1        = [] 
                 | j < 1        = [] 
                 | i > length xs = [] 
                 | i <= j       = drop (i-1) $ take j $ xs 
                 | otherwise    = let 
                    i' = length xs - i + 1 
                    j' = length xs - j + 1 
                  in drop (i'-1) $ take j' $ reverse xs



map' = 
        \f -> (\xs -> (case xs of {
                _ -> let n = length (xs) in 
                        map (\x_elliHaskell_1 -> (f (x_elliHaskell_1))) (range (xs) (1) (n)); 
        }))