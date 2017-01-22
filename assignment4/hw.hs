
import Data.List

--Exercise 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
         | even x = (x - 2) * fun1 xs
         | otherwise = fun1 xs


fun1' :: [Integer] -> Integer
fun1' xs = product . map (\x -> x - 2) $ filter (even) xs


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)


fun2' :: Integer -> Integer
fun2' n = sum . filter (even)  $ takeWhile (/=1)
              $ iterate (\ i -> if even i then i `div` 2 else 3 * i + 1) n



-- Exercise 2
data Tree a = Leaf 
              | Node Integer (Tree a) a (Tree a)
              deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree xs = foldr insertElem Leaf xs

getHeight :: Tree a -> Integer
getHeight Leaf = -1
getHeight (Node h _ _ _) = h

insertElem :: a -> Tree a -> Tree a
insertElem x Leaf = Node 0 Leaf x Leaf
insertElem x (Node height left value right) 
                              | getHeight left > getHeight right = Node height left value (insertElem x right)
                              | getHeight left < getHeight right = Node height (insertElem x left) value right
                              | getHeight left == getHeight right = let
                                                         newLeft = insertElem x left
                                                         leftHeight = getHeight newLeft
                                                         in Node (leftHeight + 1) newLeft value right

{-
insertElem x (Node height Leaf value Leaf) = let newElem = insertElem x Leaf
                                                 childHeight = getHeight newElem
                                                      in Node 1 newElem value Leaf
insertElem x (Node height left value right)
               | (getHeight left == getHeight right) = Node (height + 1) left value right
               | (getHeight left) > (getHeight right) = Node height left value (insertElem x right)
               | (getHeight right) > (getHeight left) = Node height (insertElem x left) value right
-}

--Exercise 3
xor :: [Bool] -> Bool
xor xs = odd $ foldr (\x y -> if x then y+1 else y) 0 xs

map' :: (a->b) -> [a] -> [b]
map' f xs = foldr (\x ys -> (f x) : ys) [] xs


--Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map(\i -> i * 2 + 1) $ [1..n] \\ (map(\(i, j) -> i + j + 2 * i * j)
                   $ filter(\(i, j) -> i + j + 2 * i * j <= n)
                   $ cartProd[1..n][1..n])


cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]



