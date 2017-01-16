module Golf where

-- Hopscotch

skips :: [a] -> [[a]]
skips xs = map (\n -> doSkip xs n)
              $ [1..length xs] 

doSkip :: [t] -> Int -> [t]
doSkip _ 0 = []
doSkip xs n = map (\i -> snd i)
                $ filter (\i -> fst i `mod` n == 0) 
                $ zip [1..] xs 

--skipsN :: Int -> Int -> [a] -> [a]
--skipsN 0 _ _ = []
--skipsN n iter (x:xs)
--    | iter `mod` n == 0 = x : (skipsN n (iter + 1) xs)
--    | otherwise = skipsN n (iter + 1) xs
--skipsN _ _ _ = 


localMaxima :: [Integer] -> [Integer]
localMaxima x = map (\(x, y, z) -> y)
             $ filter (\(x, y, z) -> y > x && y > z)
             $ localMaximaXform x

localMaximaXform :: [a] -> [(a, a, a)]
localMaximaXform (x:y:z:xs) = (x,y,z) : (localMaximaXform $ y:z:xs)
localMaximaXform _ = []