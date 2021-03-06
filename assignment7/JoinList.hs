{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                    | Single m a
                    | Append m (JoinList m a) (JoinList m a)
                    deriving (Eq, Show)

--Exercise 1
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (mappend (tag a) (tag b)) a b

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m


--Exercise 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single m a) = Just a
indexJ i (Append m a b) | (i + 1) > getSize(size m) || i < 0 = Nothing
                        | (i + 1) > getSize(size (tag a)) = indexJ (i - getSize(size(tag a))) b
                        | otherwise = indexJ i a


jlbToList :: (Sized b, Monoid b) => JoinList b a -> [a]
jlbToList Empty = []
jlbToList (Single _ a) = [a]
jlbToList (Append _ a b) = (jlbToList a) ++ (jlbToList b)


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 a = a
dropJ n (Single m _) = Empty
dropJ n (Append m a b)  | n >= sz = Empty
                        | n <= szLeft = (dropJ n a) +++ b 
                        | otherwise = dropJ (n - szLeft) b
                        where sz = getSize(size m)
                              szLeft = getSize(size (tag a))
dropJ _ a = a


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 _ = Empty
takeJ n s@(Single _ _) = s
takeJ n s@(Append m a b) | n >= sz = s
                         | n <= szLeft = takeJ n a
                         | otherwise = a +++ (takeJ (n - szLeft) b)
                       where sz = getSize(size m)
                             szLeft = getSize(size (tag a))


--Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine xs = Single (scoreString xs) xs


--Exercise 4

instance Buffer (JoinList(Score, Size) String) where
  toString Empty = []
  toString (Single _ a) = a
  toString (Append _ a b) = (toString a) ++ " " ++ (toString b) 

  fromString s = foldl (\acc x -> acc +++ (Single ((scoreString x), 1) x)) Empty (lines s)

  line n b = indexJ n b

  replaceLine n l b = (takeJ n b) +++ (fromString l) +++ (dropJ (n+1) b)

  numLines b = getSize $ snd (tag b)
  value b = getScore $ fst (tag b)


main = runEditor editor $ (fromString (unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]) :: (JoinList(Score, Size) String)) 
