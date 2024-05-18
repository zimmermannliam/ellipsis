module Mytest where

import Prelude ((+), (-), div, (*), mod, (>), (>=), (<), (<=), (==), (&&), (||), not, error, (++), map, reverse, take, drop, length, Int, ($), otherwise, zipWith, foldr1, zipWith3, foldl1, max, (!!))
listRange :: [a] -> Int -> Int -> [a] 
listRange xs i j     | i < 1        = [] 
                 | j < 1        = [] 
                 | i > length xs = [] 
                 | i <= j       = drop (i-1) $ take j $ xs 
                 | otherwise    = let 
                    i' = length xs - i + 1 
                    j' = length xs - j + 1 
                  in drop (i'-1) $ take j' $ reverse xs

range l r = [l..r]

map' = 
        \f -> (\xs -> (case xs of {
                _ -> let n = length (xs) in 
                        map (\__x0 -> (f (__x0))) (listRange (xs) (1) (n)); 
        }))

zip' =
        \xs -> (\ys -> (case xs of {
                _ -> let n = length (xs) in 
                        case ys of {
                                _ -> let m = length (ys) in 
                                        zipWith (\__y1 -> (\__x0 -> ((__x0, __y1)))) (listRange (ys) (1) (m)) (listRange (xs) (1) (n)); 
                        }; 
        }))

pairAdj = 
        \xs -> (case xs of {
                _ -> let n = length (xs) in 
                        zipWith (\__x1 -> (\__x0 -> ((__x0, __x1)))) (listRange (xs) (2) (n)) (listRange (xs) (1) (n-1)); 
        })

rotL =
        \xs -> (\k -> (case xs of {
                _ -> let n = length (xs) in 
                        let k' = k `mod` n in 
                                map (\__x0 -> (__x0)) (listRange (xs) (k'+1) (n)) ++ map (\__x0 -> (__x0)) (listRange (xs) (1) (k')); 
        }))

inits = \xs -> case xs of {
                _ -> let n = length (xs) in 
                        map (\__0 -> map (\__x0 -> __x0) (listRange (xs) (1) (__0))) ([1..n]); 
        }
inits' = \xs -> case xs of {
                _ -> let n = length (xs) in 
                        map (\__0 -> map (\__x0 -> __x0) (listRange (xs) (1) (__0))) ([1..n]); 
        }

blah1= \xs a b -> case xs of {
                _ -> let n = length (xs) in 
                        zipWith3 (\__x2 __1 __0 -> (__x2, __1+__0)) (listRange (xs) (1) (n)) ([a..b]) ([1..5]); 
        }

blah2= \xs a b -> case xs of {
                _ -> let n = length (xs) in 
                        zipWith (\__x1 __0 -> (__x1, __0)) (listRange (xs) (1) (n)) ([a+1..b+5]); 
        }

scanl1' = \f xs -> case xs of {
                x -> let n = length (x) in 
                        map (\_1 -> foldl1 (f) (map (\_x0 -> _x0) (listRange (x) (1) (_1)))) ([1..n]); 
        }

inits2 = \xs -> case xs of {
                x -> let n = length (x) in 
                        map (\_1 -> map (\_x0 -> _x0) (listRange (x) (1) (_1))) ([0..foldr1 (max) ((n) : ([]))]); 
        }

reverse' = \xs -> case xs of {
                x -> let n = length (x) in 
                        map (\_x0 -> _x0) (listRange (x) (n) (1)); 
        }

inits3 = \xs -> case xs of {
                x -> let n = length (x) in 
                        map (\_1 -> map (\_x0 -> _x0) (listRange (x) (1) (_1))) [(1)..(n)]; 
        }
zipInits = \xs ys -> case xs of {
                x -> let n = length (x) in 
                        case ys of {
                                y -> let m = length (y) in 
                                        (++) (((((!!) (x) (1-1), (!!) (y) (1-1))) : ([])) : ([])) (zipWith (\_2 _3 -> zipWith (\_x1 _y0 -> (_x1, _y0)) (listRange (x) (1) (_2)) (listRange (y) (1) (_3))) (range (2) (n)) (range (2) (m))); 
                        }; 
        }

scanl1'' = \f xs -> case xs of {
                x -> let n = length (x) in 
                        (++) ([(!!) (x) (1-1)]) (map (\_1 -> foldl1 (f) (map (\_x0 -> _x0) (listRange (x) (1) (_1)))) (range (2) (n))); 
        }

sum = \xs -> case xs of {
        x -> let n = length (x) in
                foldr1 (\a b -> a+b) (map (\_x0 -> _x0) (listRange (x) (1) (n)))
        }