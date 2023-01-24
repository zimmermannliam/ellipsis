import Data.List

explain :: (Show a) => (a -> a) -> [a] -> String
explain f lst = concat $ map (\(x,y) -> show x ++ " => " ++ if y then "..." else show (f x)) $ zip lst (ellipsize lst)

ellipsize :: [a] -> [Bool]
ellipsize lst = take (length lst) $ cycle [True, False]

main :: IO ()
main = do
  let f = (+1)
  let lst = [1..10]
  putStrLn $ explain f lst
