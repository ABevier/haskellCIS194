module Party where

import Debug.Trace
import Data.Tree
import Data.List
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

moreFunComparator :: GuestList -> GuestList -> Ordering
moreFunComparator g1@(GL _ f1) g2@(GL _ f2) = compare f1 f2

--Exercise 2

companyAdd :: Tree Employee -> Integer
companyAdd (Node e sf) = (empFun e) + foldr (\x acc-> acc + (companyAdd x)) 0 sf

--treeFold' :: (a -> b -> b) -> b -> Tree a -> b
--treeFold' f b (Node e sf) = f e $ foldr (\x acc -> treeFold' f b x) b sf   


--Is this really a fold? it seems like I have to pass a fold to it to get it to work -- ie CompanyAdd2
--I had to find this online because I was stumped...
treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f initial (Node r []) = f r [initial]
treeFold f initial (Node r sf) = f r $ map (\x -> (treeFold f initial x)) sf

companyAdd2 :: Tree Employee -> Integer
companyAdd2 tree = treeFold (\a xs -> foldr (\x acc -> x + acc) (empFun a) xs) 0 tree


--Exercise 3
--First item in tuple is Max with the boss of that subtree - IE. This boss's immediate subordinate...
-- ...meaning that adding the current boss to the list removes the value of the subordinate
--Second item in tuple is max without the sub tree boss, meaning I can add the current boss with no penalty
--nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
--nextLevel boss lstPairs = trace ("boss:" ++ show boss ++ " pairs: " ++ show lstPairs) $
--                          (safeMax $ map (glCons boss) $ map snd lstPairs,
--                           safeMax $ map fst lstPairs)


--safeMax :: [GuestList] -> GuestList
--safeMax [] = mempty
--safeMax lst = maximumBy moreFunComparator lst

--Left = max WITH Boss
--Right = max WITHOUT Boss
--Current Boss can't combine with immediately lower boss
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss lstPairs = -- trace ("currBoss:" ++ show boss ++ " pairs: " ++ show lstPairs) $
                            (boss `glCons` (mconcat $ map snd lstPairs), -- Can't choose any of the first pairs because they have an immediate sub boss
                            mconcat $ map (uncurry moreFun) lstPairs) -- Can't add current boss, but can get the best of each list           

testConcat :: [GuestList] -> GuestList
testConcat gl = mconcat gl



--Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun employees = moreFun `uncurry` (treeFold nextLevel (mempty, mempty) employees)



---Exercise 5
main :: IO()
main = readFile "company.txt" >>= (putStrLn . displayTree . read)

displayTree :: Tree Employee -> String
displayTree tree = show $ maxFun tree



