import Data.List
import Data.Char
import System.Console.Haskeline (Interrupt(Interrupt))


divizori :: Int -> [Int] 
divizori x = [d | d <- [1.. x], x `mod` d == 0]

listadiv :: [Int] -> [[Int]]
listadiv [] = []
listadiv l = [divizori x | x <- l]

inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec _ _ [] = []
inIntervalRec x y (h : t) 
    | h >= x && h <= y = h : inIntervalRec x y t
    | otherwise = inIntervalRec x y t


inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp x y l = [elem | elem <- l, x <= elem && elem <= y]

pozitiveRec :: [Int] -> Int 
pozitiveRec [] = 0
pozitiveRec (h : t)
    | h > 0 = 1 + pozitiveRec t
    | otherwise = pozitiveRec t

pozitiveComp :: [Int] -> Int 
pozitiveComp l = length [x | x <- l, x > 0]

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec [] = []
pozitiiImpareRec (h : t) = f 0 (h : t) 
    where 
        f :: Int -> [Int] -> [Int]
        f _ [] = []
        f x (h:t)
            | odd h = x : f (x + 1) t
            | otherwise = f (x + 1) t

pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp lista = [fst elem | elem <- zip [0..] lista, odd (snd elem)] 


multDigitsRec :: [Char] -> Int
multDigitsRec [] = 1
multDigitsRec (h : t)
    | isDigit h = digitToInt h * multDigitsRec t
    | otherwise = multDigitsRec t


multDigitsComp :: [Char] -> Int
multDigitsComp lista = product [digitToInt c | c <- lista, isDigit c]

