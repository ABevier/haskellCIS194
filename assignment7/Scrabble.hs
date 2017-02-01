{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Monoid

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

score :: Char -> Score
score n | elem n "aeilnorstu" = Score 1
        | elem n "dg" = Score 2
        | elem n "bcmp" = Score 3
        | elem n "fhvwy" = Score 4
        | elem n "k" = Score 5
        | elem n "jx" = Score 8
        | elem n "qz" = Score 10
        | otherwise = Score 0

scoreString :: String -> Score
scoreString xs = foldr (\x y -> y + score x) 0 xs 

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)