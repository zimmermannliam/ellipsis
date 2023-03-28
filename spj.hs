

-- Guards
factorial :: Int -> Int
factorial n | n == 0    = 1
            | otherwise = n * factorial (n-1)

funnyLastElt :: [Int] -> Int
funnyLastElt (x:xs) | (x < 0) = x
funnyLastElt (x:[]) = x
funnyLastElt (x:xs) = funnyLastElt xs

main :: IO ()
main = do
  putStrLn "Yuhhh"
  phrase <- getLine
  putStrLn ("You said: " ++ phrase)
