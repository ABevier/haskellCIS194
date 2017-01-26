module Permute where

--permutations :: String -> [String]
--permutations xs = permutations' xs

permute :: String -> [String]
permute [] = []
permute (x:[]) = [[x]]
permute xs = [ n:ys | (n, ns) <- subStringTuple xs, ys <- permute ns]

subStringTuple :: String -> [(Char, String)]
subStringTuple [] = []
subStringTuple (n:ns) = (n,ns) : [(x, n:xs) | (x, xs) <- subStringTuple ns]


