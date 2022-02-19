
data Fruct
    = Mar String Bool
    | Portocala String Int
    deriving Show

newtype Linie = L [Int]
    deriving Show
newtype Matrice = M [Linie]
    deriving Show

x :: Fruct
x = Mar "mar" True

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala s _) = s `elem` soiuri
    where soiuri = ["Tarocco", "Moro", "Sanguinello"]
ePortocalaDeSicilia _ = False

listaFructe :: [Fruct]
listaFructe = [Mar "Ionatan" False,
    Portocala "Sanguinello" 10,
    Portocala "Valencia" 22,
    Mar "Golden Delicious" True,
    Portocala "Sanguinello" 15,
    Portocala "Moro" 12,
    Portocala "Tarocco" 3,
    Portocala "Moro" 12,
    Portocala "Valencia" 2,
    Mar "Golden Delicious" False,
    Mar "Golden" False,
    Mar "Golden" True]

getNumOfSlices :: Fruct -> Int
getNumOfSlices (Portocala _ x) = x
getNumOfSlices _ = 0;

nrFeliiSicilia' :: [Fruct] -> Int
nrFeliiSicilia' l = sum [getNumOfSlices fruct | fruct <- l, ePortocalaDeSicilia fruct]

nrFeliiSicilia'' :: [Fruct] -> Int
nrFeliiSicilia'' l = sum $ map getNumOfSlices $ filter ePortocalaDeSicilia l

getWorm :: Fruct -> Bool
getWorm (Mar _ worm) = worm;
getWorm _ = False

nrMereViermi :: [Fruct] -> Int
nrMereViermi l = length $ filter getWorm l




type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

vorbeste :: Animal -> String
vorbeste (Pisica _)  = "Meow!"
vorbeste (Caine _ _) = "Woof!"

rasa :: Animal -> Maybe String
rasa (Caine _ r) = Just r
rasa _           = Nothing





getElements :: Linie -> [Int]
getElements (L l) = l

sumOnRow :: Linie -> Int
-- sumOnRow l = foldr (+) 0 (getElements l)
sumOnRow l = sum (getElements l)

verifica :: Matrice -> Int -> Bool
verifica (M (h : t)) x =  sumOnRow h == x && verifica (M t) x



getLengthOfLine :: Linie -> Int
getLengthOfLine (L l) = length l

isPositive :: Int -> Bool 
isPositive = ( > 0)

allPozitives :: Linie -> Bool
-- allPozitives (L l) = foldr (&&) True [True | x <- l, x > 0]
allPozitives (L l) = and [isPositive x | x <- l]

isEmpty :: Linie -> Bool
isEmpty (L l) = null l

doarPozN :: Matrice -> Int -> Bool
doarPozN (M []) _ = True
doarPozN (M (h : t)) n
    | isEmpty h               = True
    | getLengthOfLine h == n  = allPozitives h && doarPozN(M t) n
    | otherwise               = doarPozN (M t) n

len :: Linie -> Int
len (L l) = length l

lengths :: Matrice -> [Int]
lengths (M x) = [len y| y <-x]

corect :: Matrice -> Bool
corect (M x) = all (== head xs) (tail xs) 
              where xs = lengths (M x)


