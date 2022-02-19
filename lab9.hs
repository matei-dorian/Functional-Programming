-- 1
-- a

type Text = String

sfChr :: Char -> Bool
sfChr c = c `elem` endingChr
    where
        endingChr = ".?!:"


countProp :: Text -> Int
countProp [] = 0
countProp (h : t)
    | sfChr h   = 1 + countProp t
    | otherwise = countProp t

-- b
countProp' :: Text -> Int
countProp' txt = length [chr | chr <- txt , chr `elem` ".?!:"]

-- 2
type Matrix = [[Int]]
type Row = [Int]

prop :: Row -> Bool
prop = all (> 0)

liniiN :: Matrix -> Int -> Bool
liniiN m n = all prop (filter (\row -> length row == n) m)

-- 3

data Punct = Pt [Int]
    deriving Show

data Arb = Vid | F Int | N Arb Arb
    deriving Show

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a

instance ToFromArb Punct where
    toArb (Pt []) = Vid
    toArb (Pt (h:t)) = N (F h) (toArb (Pt t))

    fromArb Vid = Pt []
    fromArb (F x) = Pt [x]
    fromArb (N a1 a2)= Pt (l1 ++ l2)
        where 
            Pt l1 = fromArb a1
            Pt l2 = fromArb a2  
