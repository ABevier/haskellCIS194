{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

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
    show xs = show (take 25 $ streamToList xs)

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

--this works
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = (Cons x (interleaveStreams ys xs))

--this doesn't work
interleaveStreams' :: Stream a -> Stream a -> Stream a
interleaveStreams' (Cons x xs) (Cons y ys) = (Cons x (Cons y (interleaveStreams xs ys)))

--Exercise 6
x :: Stream Integer
x = Cons 0 (Cons 1 $ streamRepeat 0)

instance Num(Stream Integer) where
    fromInteger n = Cons n (streamRepeat 0)
    negate (Cons n ns) = Cons (-n) (negate ns)
    (+) (Cons a as) (Cons b bs) = Cons (a + b) (as + bs)
    (*) (Cons a as) s2@(Cons b bs) = Cons (a * b) ((streamMap (*a) bs) + (as * s2))

instance Fractional(Stream Integer) where
    (/) s1@(Cons a as) s2@(Cons b bs) = Cons (a `div` b) (streamMap (`div` b) (as - (s1 / s2) * bs))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - (x^2))

--Exercise 7
data Matrix = Mat Integer Integer Integer Integer
    deriving (Show)

instance Num(Matrix) where
    (*) (Mat a11 a12 a21 a22) (Mat b11 b12 b21 b22) = Mat (a11 * b11 + a12 * b12)
                                                          (a11 * b12 + a12 * b22)
                                                          (a21 * b11 + a22 * b21)
                                                          (a21 * b12 + a22 * b22)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 x = fibMatrix $ (Mat 1 1 1 0) ^ x


fibMatrix :: Matrix -> Integer
fibMatrix (Mat _ x _ _) = x

fibs4 :: Integer -> [Integer]
fibs4 n = map (fib4) [0..n]
