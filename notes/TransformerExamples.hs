import Transformer
import Debug.Trace

binSearch :: Int -> [Int] -> Bool
binSearch _ [] = False
binSearch t xs =
    let n = length xs
        k = n `div` 2 + 1
    in  
        -- trace ("k: " ++ show k) $ 
        if xs!!(k-1) == t   then True
        else if xs!!(k-1) > t   then 
            -- trace (show xs ++ ":" ++  show 1 ++  "..." ++ show (k-1)) $
            binSearch t $ ellip1 (\x->x) (xs, 1, k-1)
        else 
            -- trace (show xs ++ ":" ++  show (k+1) ++  "..." ++ show n) $
            binSearch t $ ellip1 (\x->x) (xs, k+1, n)