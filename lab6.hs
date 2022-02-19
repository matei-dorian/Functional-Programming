import Data.Char

-- 1
rotate :: Int -> [a] -> [a]
rotate 0 x = x
rotate n (h:t)
    | n < 0            = error "n must be positive"
    | n > length(h:t)  = error "n greater then length of list"
    | otherwise        = rotate (n - 1) (t ++ [h])


-- 3
makeKey :: Int -> [(Char, Char)]
makeKey step = zip alphabet (rotate step alphabet)
    where alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"


-- 4
lookUp :: Char -> [(Char, Char)] -> Char
--lookUp c l = head ([snd pair | pair <- l, fst pair == c])
lookUp c l = if not (null r) then snd (head r) else c
    where r = filter (\(x,y) -> x == c) l


-- 5
encipher :: Int -> Char -> Char
encipher step c =  lookUp c (makeKey step)


-- 6
normalize :: [Char] -> [Char]
normalize [] = []
normalize (h:t) 
    | h == ' '    = h : normalize t
    | isNumber h  = h : normalize t
    | isAlpha h   = toUpper h : normalize t
    | otherwise   = normalize t


-- 7
encipherStr :: Int -> String -> String
encipherStr step text = [encipher step c | c <- normalize text]


-- 8
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey l = [(snd x, fst x) | x <- l]


-- 10
decipher :: Int -> Char -> Char
decipher step c =  lookUp c (reverseKey (makeKey step))

decipherStr :: Int -> String -> String
decipherStr step text = [decipher step c | c <- normalize text]


