import Data.List

factori :: Int -> [Int]
factori x = [k | k <- [1.. x], x `rem` k == 0]

prim :: Int -> Bool
prim 0 = False
prim n
    | length(factori n)==2 = True
    | otherwise = False

myzip3rec :: [a] -> [b] -> [c] -> [(a,b,c)]
myzip3rec [] _ _ = []
myzip3rec _ [] _ = []
myzip3rec _ _ [] = []
myzip3rec (l1:l1s) (l2:l2s) (l3:l3s) = (l1,l2,l3) : myzip3rec l1s l2s l3s

myzip3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
myzip3 a b c = [(x, y, z) | x <- a, y <- b, z <- c, elemIndex x a == elemIndex y b && elemIndex y b == elemIndex z c]

ordonataNat ::[Int] -> Bool
ordonataNat [] = True
ordonataNat [x] = True
ordonataNat (x:xs) =and [ a<b | (a,b) <- zip (take (length (x:xs)-1) (x:xs)) xs ]
-- ordonataNat (x:xs) =and [ a<b | (a,b) <- zip (x:xs) xs ]

ordonataNat1 :: [Int] -> Bool
ordonataNat1 [] = True
ordonataNat1 [x] = True
ordonataNat1 (x:xs)
    | x < head xs = ordonataNat1 xs
    | otherwise = False


ordonata :: [a] -> (a -> a -> Bool) -> Bool
ordonata (x:xs) rel = and [rel ((x:xs)!! k) ((x:xs)!!(k+1)) | let indici = zip [0..] (x:xs), k <- [0..(length indici-2)]]


(*<*) :: (Integer, Integer) -> (Integer, Integer) -> Bool
(a, b) *<* (c, d) = a<c && b<d
-- (*<*) (a, b) (c, d) = a<c && b<d
infix 6 *<*

--8
compuneList :: (b -> c) -> [a -> b] -> [a -> c]
compuneList f l = [f . g | g <- l]

--9
compuneList :: (b -> c) -> [a -> b] -> [a -> c]
compuneList f l = [f . g | g <- l]

--10
aplicaList :: a -> [a -> b] -> [b]
aplicaList x l = [f x | f <- l]