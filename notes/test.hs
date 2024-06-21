{-# LANGUAGE MultiLineStrings #-}

import Debug.Trace
import Data.Function ((&))
import Data.Foldable (Foldable)
import Data.Maybe (fromMaybe)
import Data.Generics

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


{-
combinations' :: [a] -> [b] -> [(a, b)]
combinations' xs ys = foldr (++) [] [(x, y) | x <- xs | y <- ys]


combinations :: [a] -> [b] -> [(a, b)]
combinations xs ys = 
    -}

-- combinations [x_1 ... x_n] [y_1 ... y_m] = 
--    [(x_1, y_1) ... (x_1, y_m)] ... [(x_n, y_1) ... (x_n, y_m)]

-- ellip :: (b -> b -> b) -> (a -> b) -> [([a], Int, Int)] -> b
-- ellip g f terms = foldr1 (g) $ map f $ map range $ terms

{-
ellip1Fold :: (b -> b -> b) -> (a -> b) -> ([a], Int, Int) -> b
ellip1Fold g f terms = foldr1 g $ map f $ range terms

ellip1 :: (a -> b) -> ([a], Int, Int) -> [b]
ellip1 f terms = id $ map f $ range terms

ellip2Fold :: (b -> b -> b) -> ((a0, a1) -> b) -> (([a0], Int, Int), ([a1], Int, Int)) -> b
ellip2Fold g f (terms1, terms2) = foldr1 g $ map f $ uncurry zip $ (range terms1, range terms2)

ellip2 :: ((a0, a1) -> b) -> (([a0], Int, Int), ([a0], Int, Int)) -> [b]
ellip1 f terms = id $ map f $ range terms

range :: ([a], Int, Int) -> [a]
range (xs, i, j) = drop (i-1) $ take j $ xs

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f [] = []
groupBy' f xs = 
    let k = fromMaybe (length xs) (idxWhere (\l r -> not (f l r)))
        (grp, rst) = splitAt k xs
    in grp:(groupBy' f rst)

idxWhere :: (a -> a -> Bool) -> [a] -> Maybe Int
idxWhere _ []       = Nothing
idxWhere p xs   = foldr1 (+) $ 
-}


myMap = 
    let myMap = \f -> (\list -> (case list of {
        [] -> [];
        (x:xs) -> (f x):(myMap f xs)
    }))
    in myMap

{-
addNodes :: Tree -> Tree -> Tree
addNodes (INode il) (INode ir) = INode (il + ir)
addNodes l r = l

incZ :: (Int, Tree) -> (Int, Tree)
incZ (i, t) = (i+1, t)


incINode :: Tree -> Tree
incINode (INode i)  = INode (i+1)
incINode (Blah t)   = traceShow t $ SNode "Blah" `Branch` INode 0
incINode tr         = tr

incAll :: Tree -> Tree
incAll = everywhere (mkT incINode)

sumINodes :: Tree -> Int
sumINodes = everything (+) (mkQ 0 (fromMaybe 0 . iNodeI))


iNodeI :: Tree -> Maybe Int
iNodeI (INode i)    = Just i
iNodeI _            = Nothing


data Tree = Branch Tree Tree | Leaf Int
    deriving (Data, Show)

tr1 = (Leaf 5) `Branch` ((Leaf 6) `Branch` (Leaf 7))
tr2 = (Leaf 10) `Branch` ((Leaf 11) `Branch` (Leaf 12))

gcollect :: Tree -> [Int]
gcollect = everything (++) ([] `mkQ` fromLeaf)

fromLeaf :: Tree -> [Int]
fromLeaf (Leaf x)   = [x]
fromLeaf _          = []

gCollectSum :: Tree -> Tree -> [Int]
gCollectSum x y = gCollectSum' x y
    where
    gCollectSum :: GenericQ (GenericQ Int)
    gCollectSum x y = 
-}
    

range begin end =
    if begin == end     then [begin]
    else if begin < end then begin:(range (begin+1) end)
    else                     begin:(range (begin-1) end)

listRange xs begin end =
    if begin < 1                then []
    else if begin > length xs   then []
    else if end < 1             then []
    else if begin <= end        then drop (begin-1) (take end xs)
    else
        let begin' = (length xs) - begin + 1 in
        let end'   = (length xs) - end + 1
        in drop (begin' - 1) (take end' (reverse xs))

myfoldr1 = \fn list ->
    let myfoldr1' = (\f list1 -> case list1 of
        { [x]     -> x
        ; (x:xs)  -> f x (myfoldr1' f xs)
        ; []      -> error "foldr1: Empty list"
        })
    in myfoldr1' fn list


myfoldl1 = \fn list ->
    let myfoldl1' = (\f list1 -> case list1 of
        { [x]       -> x
        ; [x1,x2]   -> f x1 x2
        ; (x1:x2:xs)-> f (f x1 x2) (myfoldl1 f xs)
        ; []        -> error "foldl1: Empty list"
        })
    in myfoldl1' fn list

inits' :: [a] -> [[a]]
inits' xs = 
    let go prev list = case list of {
        []      -> [];
        (x:xs)  -> let now = prev ++ [x] in now:go now xs;
    }
    in []:go [] xs

st = """
hello my name is
jeff !
"""