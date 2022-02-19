{-
class Functor f where
fmap : : ( a -> b ) -> f a -> f b
-}
newtype Identity a = Identity a
    deriving Show

instance Functor Identity where
    fmap fct (Identity a1) = Identity (fct a1)

data Pair a = Pair a a

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)


data Constant a b = Constant b
instance Functor (Constant a) where
    fmap f (Constant x) = Constant (f x)

data Two a b = Two a b
instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

data Three a b c = Three a b c
instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)

data Three' a b = Three' a b b
instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)

data Four a b c d = Four a b c d
instance Functor (Four a b c ) where
    fmap f (Four x y z t) = Four x y z (f t)

data Four'' a b = Four'' a a a b
instance Functor (Four'' a ) where
    fmap f (Four'' x y z t) = Four'' x y z (f t)

data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant a ) where
    fmap f Finance   = Finance
    fmap f (Desk x)  = Desk x
    fmap f (Bloor y) = Bloor (f y)

data LiftItOut f a = LiftItOut (f a)
instance (Functor f) => Functor (LiftItOut f) where
    fmap g (LiftItOut f) = LiftItOut (fmap g f)

data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where 
    fmap h (DaWrappa f g) = DaWrappa (fmap h f) (fmap h g)
    
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap h (IgnoringSomething x y) = IgnoringSomething x (fmap h y)

data Notorious g o a t = Notorious (g o) (g a) (g t)
instance (Functor g) => Functor (Notorious g o a) where
    fmap f (Notorious b i g) = Notorious b i (fmap f g)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
    fmap f NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)

data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor TalkToMe where
    fmap f Halt = Halt
    fmap f (Print str x) = Print str (f x)
    fmap f (Read g) = Read (f . g)