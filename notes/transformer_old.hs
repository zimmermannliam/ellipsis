uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c

range :: ([a], Int, Int) -> [a]
range (xs, i, j) = drop (i-1) $ take j $ xs

ellip1Fold :: (b -> b -> b) -> (a -> b) -> ([a], Int, Int) -> b
ellip1Fold g f terms = foldr1 g $ map f $ range terms

ellip1 :: (a -> b) -> ([a], Int, Int) -> [b]
ellip1 f terms = id $ map f $ range terms

ellip2Fold :: (b -> b -> b) -> ((a0, a1) -> b) -> (([a0], Int, Int), ([a1], Int, Int)) -> b
ellip2Fold g f (terms1, terms2) = 
    foldr1 g 
    $ map f 
    $ uncurry zip 
    $ (range terms1, range terms2)

ellip2 :: ((a0, a1) -> b) -> (([a0], Int, Int), ([a1], Int, Int)) -> [b]
ellip2 f (terms1, terms2) = 
    id 
    $ map f 
    $ uncurry zip 
    $ (range terms1, range terms2)

ellip3Fold :: (b -> b -> b) -> ((a0, a1, a2) -> b) -> (([a0], Int, Int), ([a1], Int, Int), ([a2], Int, Int)) -> b
ellip3Fold g f (terms1, terms2, terms3) = 
    foldr1 g 
    $ map f 
    $ uncurry3 zip3 
    $ (range terms1, range terms2, range terms3)

ellip3 :: ((a0, a1, a2) -> b) -> (([a0], Int, Int), ([a1], Int, Int), ([a2], Int, Int)) -> [b]
ellip3 f (terms1, terms2, terms3) = 
    id 
    $ map f 
    $ uncurry3 zip3
    $ (range terms1, range terms2, range terms3)