pairAdj :: [a] -> [(a, a)]
pairAdj l = let n = length l in
    zip (take (n-1) l) (drop 1 l)

-- zip, take, drop, length

rotL :: [a] -> Int -> [a]
rotL l k = (drop k l) ++ (take k l)

-- take, drop

length' :: [a] -> Int
length' l = length l

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\y ys -> (f y):ys) [] xs
-- foldr or map

zip' :: [a] -> [b] -> [(a, b)]