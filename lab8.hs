-- 1 
import Data.List ( nub )

type Nume = String
data Prop
    = Var Nume
    | F
    | T
    | Not Prop
    | Prop :|:   Prop
    | Prop :&:   Prop
    | Prop :->:  Prop
    | Prop :<->: Prop
    deriving Eq
infixr 0 :<->:
infixr 1 :->:
infixr 2 :|:
infixr 3 :&:

p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not ( Var "Q"))

p3 :: Prop
p3 = (Var "P" :&: (Var "Q"  :|: Var "R")) :&: (Not (Var "P") :|: Not ( Var "Q") :&: (Not (Var "Q") :|: Not (Var "R")))

-- 2
instance Show Prop where
    show (Var p) = p
    show F = "F"
    show T = "T"
    show (Not prop) = "(~" ++ show prop ++ ")"
    show (prop1 :|: prop2)   = "(" ++ show prop1 ++ "|" ++ show prop2 ++ ")"
    show (prop1 :&: prop2)   = "(" ++ show prop1 ++ "&" ++ show prop2 ++ ")"
    show (prop1 :->: prop2)  = "(" ++ show prop1 ++ "->" ++ show prop2 ++ ")"
    show (prop1 :<->: prop2) = "(" ++ show prop1 ++ "<->" ++ show prop2 ++ ")"

test_ShowProp :: Bool
test_ShowProp =
    show (Not (Var "P") :&: Var "Q") == "((~P)&Q)"

-- 3
type Env = [(Nume, Bool)]

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust _ = undefined

impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a

eval :: Prop -> Env -> Bool
eval (Var p) l = impureLookup p l
eval F _ = False
eval T _ = True
eval (Not prop) l = not (eval prop l)
eval (prop1 :&: prop2) l = eval prop1 l && eval prop2 l
eval (prop1 :|: prop2) l = eval prop1 l || eval prop2 l
eval (prop1 :->: prop2) l = x <= y
    where
        x = eval prop1 l
        y = eval prop2 l
eval (prop1 :<->: prop2) l = x == y 
    where
        x = eval prop1 l
        y = eval prop2 l

test_eval :: Bool
test_eval = eval (Var "P" :|: Var "Q") [("P", True), ("Q", False)]
test_eval2 :: Bool
test_eval2 = eval (Not (Var "P" :|: Var "Q") :&: T) [("P", True), ("Q", False)]

-- 4
variabile :: Prop -> [Nume]
variabile p = nub $ f p
    where
        f :: Prop -> [Nume]
        f (Var p) = [p]
        f F = []
        f T = []
        f (Not prop) = f prop
        f (prop1 :&: prop2)   = f prop1 ++ f prop2
        f (prop1 :|: prop2)   = f prop1 ++ f prop2
        f (prop1 :->: prop2)  = f prop1 ++ f prop2
        f (prop1 :<->: prop2) = f prop1 ++ f prop2

-- 5
envs :: [Nume] -> [Env]
envs [] = [[]]
envs (h : t) = [(h, False) : e | e <- envs t] ++ [ (h, True) : e | e <- envs t]

-- 6
satisfiabila :: Prop -> Bool
satisfiabila p = or [eval p env | env <- listOfEnv]
    where listOfEnv = envs $ variabile p

-- 7
valida :: Prop -> Bool
valida prop = not $ satisfiabila (Not prop)

-- 10
echivalenta :: Prop -> Prop -> Bool
echivalenta p1 p2 = and [eval p env | env <- listOfEnv]
    where 
        listOfEnv = envs $ variabile p
        p = p1 :<->: p2 

