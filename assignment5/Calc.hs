{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import qualified StackVM as StackVM

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr s = eval <$> parseExp Lit Add Mul s

-- Exercise 3
reify :: ExprT -> ExprT
reify = id

class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    mul = Mul
    add = Add


-- Exercise 4
instance Expr Integer where
    lit x = x
    mul = (*)
    add = (+)

    
instance Expr Bool where
    lit x = if x <= 0 then False else True
    mul = (&&)
    add = (||)


newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit x = MinMax x
    mul (MinMax x) (MinMax y) = MinMax (min x y)
    add (MinMax x) (MinMax y) = MinMax (max x y)


newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit x = Mod7 (x `mod` 7)
    mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

--Exercise 5

instance Expr StackVM.Program where
    lit x = [StackVM.PushI x]
    add x y = x ++ y ++ StackVM.Add : []
    mul x y = x ++ y ++ StackVM.Mul : []

compile :: String -> Maybe StackVM.Program
compile x = parseExp lit add mul x
