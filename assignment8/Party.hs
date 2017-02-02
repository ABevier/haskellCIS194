module Party where

import Data.Tree
import Employee

--Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + empFun e)


instance Monoid GuestList where
  mempty  = GL [] 0
  mappend (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)


moreFun :: GuestList -> GuestList -> GuestList
moreFun g1@(GL _ f1) g2@(GL _ f2) | f1 >= f2 = g1
                            | otherwise = g2

--Exercise 2

companyAdd :: Tree Employee -> Integer
companyAdd (Node e sf) = (empFun e) + foldr (\x acc-> acc + (companyAdd x)) 0 sf

--treeFold' :: (a -> b -> b) -> b -> Tree a -> b
--treeFold' f b (Node e sf) = f e $ foldr (\x acc -> treeFold' f b x) b sf   


--Is this really a fold? it seems like I have to pass a fold to it to get it to work -- ie CompanyAdd2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node r sf) = f r $ map (\x -> (treeFold f x)) sf

companyAdd2 :: Tree Employee -> Integer
companyAdd2 tree = treeFold (\a xs -> foldr (\x acc -> x + acc) (empFun a) xs) tree