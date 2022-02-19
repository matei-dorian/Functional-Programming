
-- 1
firstEl :: [(a,b)] -> [a]
firstEl  = map fst

-- 2
sumList :: [[Int]] -> [Int]
sumList = map sum

sumList' :: [[Int]] -> [Int]
sumList' [] = []
sumList' xs = map sum xs


-- 3
verifPar :: Integer -> Integer
verifPar x = if even x
                then x `div` 2
             else x * 2

prel2 :: [Integer] -> [Integer]
prel2 =  map verifPar

prel2v2 :: [Int] -> [Int]
prel2v2 = map (\x -> if even x then div x 2 else x * 2)


-- 4
siruri::Char -> [String] -> [String]
siruri x []=[]
siruri x xs = filter (x `elem`) xs

filterChar :: Char -> [String] -> [String]
filterChar x = filter (elem x)

-- 5
squareFromOdds :: Integral a => [a] -> [a]
squareFromOdds l = map (^2) (filter odd l)

squareFromOdds' :: Integral a => [a] -> [a]
squareFromOdds' l = [x * x | x <- l, odd x]

squareFromOdds'' :: Integral a => [a] -> [a]
squareFromOdds'' [] = []
squareFromOdds'' (h : t)
    | odd h     = h ^ 2 : squareFromOdds'' t
    | otherwise = squareFromOdds'' t

-- 6
squarePozOdd :: Num b => [b] -> [b]
squarePozOdd l = map (\x -> snd x ^ 2) (filter (odd . fst) (zip [0.. ] l))


-- 7
elimVowels :: [String] -> [String]
elimVowels = map (filter (not . isVowel))
    where
        isVowel :: Char -> Bool
        isVowel x = x `elem` "aeiou"

elimConsonants :: [String] -> [String]
elimConsonants = map (filter isVowel)
    where
        isVowel :: Char -> Bool
        isVowel x = x `elem` "aeiou"


-- 8
myMap :: (t -> a) -> [t] -> [a]
myMap f l = [f x | x <- l]

myMap' :: (t -> a) -> [t] -> [a]
myMap' _ [] = []
myMap' f (h : t) = f h : myMap' f t

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f l = [x | x <- l, f x]

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' _ [] = []
myFilter' f (h : t)
    | f h       = h : myFilter' f t
    | otherwise = myFilter' f t


-- 9
sumOfSquares :: Integral a => [a] -> a
sumOfSquares l = sum (map (^2) (filter odd l))


sumOfSquares' :: Integral a => [a] -> a
sumOfSquares' l = sum [x * x | x <- l, odd x]


sumOfSquares'' :: Integral a => [a] -> a
sumOfSquares'' [] = 0
sumOfSquares'' (h : t)
    | odd h     = h * h + sumOfSquares t
    | otherwise = sumOfSquares t


-- 10
allTrue :: [Bool] -> Bool
allTrue = and

allTrue' :: [Bool] -> Bool
allTrue' = foldr (&&) True


-- 11
-- a
rmChar :: Char -> String -> String
rmChar c = filter (/= c)

-- b
rmCharsRec :: String -> String -> String
rmCharsRec s1 s2 = foldl (flip rmChar) s2 s1

--c
rmCharsFold :: String -> String -> String
rmCharsFold c x = foldr rmChar x c


