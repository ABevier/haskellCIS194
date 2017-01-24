
module Fibonacci where

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [1..]

testFibs1 :: Int -> [Integer]
testFibs1 n = take n fibs1

-- Exercise 2
fibs2 :: [Integer]
fibs2 = fib2 1 1

fib2 :: Integer -> Integer -> [Integer]
fib2 a b = a : (fib2 b (a + b))

testFibs2 :: Int -> [Integer]
testFibs2 n = take n fibs2

-- Exercise 3
data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
    show xs = show (take 50 $ streamToList xs)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : (streamToList xs)

--Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Cons s (streamFromSeed f (f s))

--Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = ruler' 0

ruler' :: Integer -> Stream Integer
ruler' x = interleaveStreams (streamRepeat x) (ruler'(x+1))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = (Cons x (interleaveStreams ys xs))

