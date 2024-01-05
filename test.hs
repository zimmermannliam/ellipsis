map' :: [Int] -> (Int -> Int) -> [Int]
map' [] _       = []
map' (x:xs) f   = f x : map' xs f

succ' :: Int -> Int
succ' x = x + 1

fold' :: [Int] -> (Int -> Int -> Int) -> Int
fold' [x]    _  = x
fold' (x:xs) f  = f x (fold' xs f)
fold' []     _  = error "fold too few"

add' :: Int -> Int -> Int
add' a b    = a + b

zip' :: [Int] -> [Int] ->[[Int]]
zip' l1 l2 = case l1 of
                (x:xs)  -> case l2 of
                                (y:ys)  -> [x,y]: zip' xs ys
                                []      -> []
                [] -> []


exList1 :: [Int]
exList1 = [1,2,3,4,5]

exList2 :: [Int]
exList2 = [11,12,13,14,15]


delete' :: [Int] -> Int -> [Int]
delete' l i = case i of
                0 -> case l of
                        (x:xs)  -> xs
                        []      -> error "out of range"
                j -> case l of
                        (x:xs)  -> x : delete' xs (j-1)
                        []      -> error "out of range"

testCase' :: Int -> Int -> Int
testCase' i j = case (i, j) of
                (5, j) -> i + j
                _      -> 0

reverse' :: [Int] -> [Int]
reverse' l = case l of
                (x:xs)  -> reverse xs ++ [x]
                []      -> []

len' :: [Int] -> Int
len' l = case l of
            (x:xs)  -> 1 + len' xs
            []      -> 0
