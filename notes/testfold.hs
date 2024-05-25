import Prelude hiding (foldl1, foldl)

{-
-- bad
foldl1 f [x,y]     = f x y
foldl1 f (x:y:zs)  = f (f x y) (foldl1 f zs)
foldl1 f [x]       = x
foldl1 _ []        = error "foldl1 undefined on []"
-}

foldl1 f [x]        = x
foldl1 f (x1:x2:xs) = foldl1 f ((f x1 x2):xs)