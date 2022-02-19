import Distribution.Verbosity (verboseHasFlags)
eeny :: Int -> String 
eeny x = if even x
            then "eeny"
         else "meeny"


fizzbuzz :: Int -> String
fizzbuzz x 
    | x `mod` 15 == 0 = "fizzbuzz"
    | x `mod` 3 == 0 = "fizz"
    | x `mod` 5 == 0 = "buzz"
    | otherwise = ""


-- Varianta 2
-- fizzbuzz' :: Int -> String
-- fizzbuzz' x = if x `mod` 15 == 0
--                 then "fizzbuzz"
--               else 
--                 if x `mod` 3 == 0
--                   then "fizz"
--                 else 
--                     if x `mod` 5 == 0
--                         then "buzz"
--                     else ""

-- Ex 4

tribonacci :: Integer -> Integer 
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci 3 = 2
tribonacci x = tribonacci(x-3) + tribonacci (x-2) + tribonacci (x-1)


tribonacci' :: Integer -> Integer 
tribonacci' x 
    | x == 0 = undefined
    | x == 1 = 1
    | x == 2 = 1
    | x == 3 = 1
    | otherwise = tribonacci' (x-3) + tribonacci' (x-2) + tribonacci (x-1)



binomial :: Integer -> Integer -> Integer
binomial n k 
    | k == 0 = 1
    | n == 0 = 0
    | otherwise = binomial (n - 1) k + binomial (n - 1) (k - 1)



verifl :: [Int] -> String 
verifl x = if even (length x)
                then "par"
            else "impar"
            
takefinal :: [Int] -> Int -> [Int]
takefinal l n 
    | length l < n = l
    | otherwise = drop (length l - n) l 


remove :: [Int] -> Int -> [Int]
remove l n
    | 0 > n || n > length l = l
    | n == 0 = tail l
    | otherwise = take (n - 1) l ++ drop n l



myreplicate :: Integer -> Integer -> [Integer]
myreplicate n v
    | n < 0 = undefined
    | n == 0 = []
    | otherwise = v : myreplicate (n - 1) v


sumImp :: [Integer] -> Integer
sumImp [] = 0
sumImp (h:t)
    | h `mod` 2 == 1 = h + s
    | otherwise = s
    where s = sumImp t

totalLen :: [String] -> Int
totalLen [] = 0
totalLen (h:t)
    | take 1 h == "A" = length h + t'
    | otherwise = t'
    where t' = totalLen t
