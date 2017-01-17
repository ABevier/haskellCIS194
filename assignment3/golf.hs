module Golf where

import Data.List
import qualified Data.Map as M

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


-- Local Maxima

localMaxima :: [Integer] -> [Integer]
localMaxima x = map (\(x, y, z) -> y)
             $ filter (\(x, y, z) -> y > x && y > z)
             $ localMaximaXform x

localMaximaXform :: [a] -> [(a, a, a)]
localMaximaXform (x:y:z:xs) = (x,y,z) : (localMaximaXform $ y:z:xs)
localMaximaXform _ = []


-- Histogram
histogram :: [Integer] -> String
histogram xs = intercalate "\n" (buildIt xs ++ ["==========", "0123456789\n"])

buildIt :: [Integer] -> [String]
buildIt xs = map (\n -> line n grid) $ reverse [1..maximum grid]
          where grid = doGrid xs

line :: Integer -> [Integer] -> String
line y xs = map (\x -> if x >= y then '*' else ' ')  xs

doGrid :: [Integer] -> [Integer]
doGrid xs = map (\x -> doCount x xs) [0..9]

doCount :: Integer -> [Integer] -> Integer
doCount y xs = sum 
                 $ map (\x -> if x == y then 1 else 0) xs



{-
test2 :: [Integer] -> [String]
test2 xs = (map (\n -> "**********") [1..(sMax $ res)]) 
              where res = countM xs

countM :: [Integer] -> [(Integer,Integer)]
countM n = M.toList 
         $ M.fromListWith (\a b -> a + b) 
         $ map (\x -> (x, 1)) n


sMax :: [(Integer, Integer)] -> Integer
sMax [] = 0 
sMax xs = maximum $ map (\(a,b) -> b) xs
-}

