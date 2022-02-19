import Data.Monoid
    ( (<>),
      Monoid(mempty),
      Sum(Sum, getSum),
      Any(Any, getAny),
      All(All, getAll) )
import Prelude hiding (elem, null, length, const)


data BinaryTree a =
    Empty
    | Leaf a
    | Node (BinaryTree a) (BinaryTree a)
    deriving Show

foldTree :: ( a -> b -> b ) -> b -> BinaryTree a -> b
foldTree f i Empty = i
foldTree f i ( Leaf x ) = f x i
foldTree f i (Node l r ) = foldTree f ( foldTree f i r ) l

myTree :: BinaryTree Integer
myTree = Node (Node ( Leaf 1) ( Leaf 2) ) (Node ( Leaf 3) (Leaf 4) )

instance Foldable BinaryTree where
  foldr = foldTree


-- 1
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = foldr (\y acc -> acc || x == y) False

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x xs = getAny $ foldMap (\y -> Any (y == x)) xs

------------------------------------------------------
null :: (Foldable t) => t a -> Bool
null = foldr (\y acc -> False) True

null':: (Foldable t) => t a -> Bool
null' x = getAll $ foldMap (\y -> All False) x

------------------------------------------------------
length :: (Foldable t) => t a -> Int
length = foldr (\y acc -> 1 + acc) 0

length' :: (Foldable t) => t a -> Int
length' x = getSum $ foldMap (\y -> Sum 1 ) x

------------------------------------------------------
toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

toList' :: (Foldable t) => t a -> [a]
toList' = foldMap (: [])

------------------------------------------------------
fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id



-- 2
data Constant a b = Constant b
    deriving Show
instance Foldable (Constant a) where
    foldMap f (Constant x) = f x

data Two a b = Two a b
    deriving Show
instance Foldable (Two a) where
    foldMap f (Two x y) = f y


data Three a b c = Three a b c
    deriving Show
instance Foldable (Three a b) where
    foldMap f (Three x y z) = f z


data Three' a b = Three' a b b
    deriving Show
instance Foldable (Three' a) where
    foldMap f (Three' x y y') = f y <> f y'


data Four' a b = Four' a b b b
    deriving Show
instance Foldable (Four' a) where
    foldMap f (Four' x y y' y'') = f y <> f y' <> f y''


data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
    deriving Show
instance Foldable GoatLord where
    foldMap f NoGoat = mempty
    foldMap f (OneGoat x) = f x
    foldMap f (MoreGoats glx gly glz) = foldMap f glx <> foldMap f gly <> foldMap f glz


f :: String -> String -> String
f [] _ = []
f _ [] = []
f (h1:t1) (h2:t2) 
    | h1 == h2 = h1 : f t1 t2
    | otherwise = []

f' :: [Int] -> [Int] -> Int
f' lx ly = if length lx == length ly
                then sum [x ^ 2 * y ^ 2 | x <- lx, y <- ly]
           else error "listele au lungimi diferite"