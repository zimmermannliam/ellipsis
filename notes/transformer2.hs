
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c

range :: ([a], Int, Int) -> [a]
range (xs, i, j) | i < 1        = []
                 | i <= j       = drop (i-1) $ take j $ xs
                 | otherwise    = let
                    i' = length xs - i + 1
                    j' = length xs - j + 1
                  in drop (i'-1) $ take j' $ reverse xs

ellip1Fold :: (b -> b -> b) -> (a -> b) -> ([a], Int, Int) -> b
ellip1Fold g f terms = foldr1 g $ map f $ range terms

ellip1 :: (a -> b) -> ([a], Int, Int) -> [b]
ellip1 f terms = id $ map f $ range terms

ellip2Fold :: (b -> b -> b) -> (a0 -> a1 -> b) -> ([a0], Int, Int) -> ([a1], Int, Int) -> b
ellip2Fold g f terms1 terms2 = 
    foldr1 g 
    $ zipWith f (range terms1) (range terms2)

ellip2 :: (a0 -> a1 -> b) -> ([a0], Int, Int) -> ([a1], Int, Int) -> [b]
ellip2 f terms1 terms2 = 
    id 
    $ zipWith f (range terms1) (range terms2)

ellip3Fold :: (b -> b -> b) -> (a0 -> a1 -> a2 -> b) -> ([a0], Int, Int) -> ([a1], Int, Int) -> ([a2], Int, Int) -> b
ellip3Fold g f terms1 terms2 terms3 = 
    foldr1 g 
    $ zipWith3 f (range terms1) (range terms2) (range terms3)

ellip3 :: (a0 -> a1 -> a2 -> b) -> ([a0], Int, Int) -> ([a1], Int, Int) -> ([a2], Int, Int) -> [b]
ellip3 f terms1 terms2 terms3 = 
    id 
    $ zipWith3 f (range terms1) (range terms2) (range terms3)

enumerate :: [a] -> [(Int, a)]
enumerate xs = zip [0..] xs

rangeSkip :: ([a], Int, Int, Int) -> [a]
rangeSkip (xs, ifst, isnd, ilst) =
    let xs' = range (xs, ifst, ilst)
        skipN = abs (isnd - ifst)
    in map snd $ filter (\(i, _) -> i `mod` skipN == 0) $ enumerate xs'

ellip2Skip :: (a0 -> a1 -> b) -> ([a0], Int, Int, Int) -> ([a1], Int, Int, Int) -> [b]
ellip2Skip f terms1 terms2 = 
    id 
    $ zipWith f (rangeSkip terms1) (rangeSkip terms2)

rangeSkipArb :: (Int -> Int) -> ([a], Int, Int) -> [a]
rangeSkipArb skipFn (xs, ifst, ilst) =
    let xs' = range (xs, ifst, ilst)
        skipArb :: [Int] -> [a] -> [a]
        skipArb [] _ = []
        skipArb (i:is) xs = (xs!!i):(skipArb is xs)
    in skipArb (takeWhile (\i -> i < ilst) $ dropWhile (\i -> i < ifst-1) $ map skipFn [0..]) xs'

mylog :: Int -> Int
mylog i = floor (logBase 2 (fromIntegral i :: Float)) :: Int

ellip2SkipArb :: (a0 -> a1 -> b) -> ([a0], Int, Int) -> ([a1], Int, Int) -> (Int -> Int) -> [b]
ellip2SkipArb f terms1 terms2 skipFn =
    id
    $ zipWith f (rangeSkipArb skipFn terms1) (rangeSkipArb skipFn terms2)

xs = [1..5]
nx = length xs

ys = [2..8]
ny = length ys

zs = [1..100]
nz = length zs

ws = [8,14,32,0,4]
nw = length ws