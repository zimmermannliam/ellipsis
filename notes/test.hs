import Debug.Trace
import Data.Function (&)

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

{-
split :: [Int] -> Int -> ([Int], [Int])
split xs n  = (take n xs, drop n xs)

sort :: [Int] -> [Int]
sort []        = []
sort [x]       = [x]
sort xs        = merge (sort left) (sort right)
    where
    (left, right) = split xs ((length xs) `div` 2)

merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)     = if x <= y then (x:merge xs (y:ys))
                          else (y:merge (x:xs) ys)
                          -}

cmp :: Int -> Int -> Int
cmp r l = let quo = (r - l) in case quo of
                0 -> 0
                _ -> quo `div` abs quo

binSearch :: [Int] -> Int -> Bool
binSearch l t   = case l of
                    [] -> False
                    _ -> let k = length l `div` 2 in
                        trace (show l) $ case cmp (l!!k) t of
                            0   -> True
                            1   -> let newl = (fst $ splitAt (k-1) l) in binSearch newl t
                            -1  -> let newl = (snd $ splitAt (k+1) l) in binSearch newl t
                            _   -> error "bad cmp"


unSum :: Int -> Maybe (Int, Int)
unSum 0 = Nothing
unSum x = Just (1, x-1)


data Expr = Cons Expr Expr
          | Val Int
          | Empty
          deriving (Eq, Show)

unExpr :: Expr -> Maybe (Expr, Expr)
unExpr (Cons x xs)  = Just (x, xs)
unExpr Empty        = Nothing
unExpr _            = error "wtf"

intuitiveButWrongZip :: [a] -> [b] -> [(a,b)]
intuitiveButWrongZip xs ys = [(x,y) | x <- xs, y <- ys]

betwixt :: Int -> Int -> [a] -> [a]
betwixt begin end l = drop (begin-1) $ take end l

ellipsis1 :: [a] -> (Int -> a -> pf) -> (Int -> Int) -> (Int -> Int) -> (pf -> out -> out) -> out -> out
ellipsis1 xs f getBegin getEnd folder foldLast = 
    let n = length xs
        begin = getBegin n
        end = getEnd n
        xs' = betwixt begin end (zip [1..] xs)
        xs'' = map (uncurry f) xs'
    in foldr folder foldLast xs''


combinations' :: [a] -> [b] -> [(a, b)]
combinations' xs ys = foldr (++) [] [(x, y) | x <- xs | y <- ys]


combinations :: [a] -> [b] -> [(a, b)]
combinations xs ys = 

-- combinations [x_1 ... x_n] [y_1 ... y_m] = 
--    [(x_1, y_1) ... (x_1, y_m)] ... [(x_n, y_1) ... (x_n, y_m)]
