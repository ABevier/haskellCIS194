toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = (toDigits (n `div` 10)) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n = (n `mod` 10) : toDigitsRev (n `div` 10)


revList :: [Integer] -> [Integer]
revList [] = []
revList (x:xs) = (revList xs) ++ [x]


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:xs) = revList (doubleEveryOther' (revList (x:xs)))

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' [] = []
doubleEveryOther' (x:[]) = x:[]
doubleEveryOther' (x:y:xs) = x : (y * 2) : (doubleEveryOther' xs)


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sumSingleNumberDigits x) + (sumDigits xs)


sumSingleNumberDigits :: Integer -> Integer
sumSingleNumberDigits n = sumList(toDigits n);


sumList :: [Integer] -> Integer
sumList []   = 0
sumList (x:xs) = x + (sumList xs)


validate :: Integer -> Bool
validate n = (sumDigits(doubleEveryOther(toDigits n)) `mod` 10) == 0


main = print(validate 4012888888881881)