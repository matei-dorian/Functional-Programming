import Data.Maybe ( fromMaybe )
import Data.List  ( lookup )


type Name = String

data  Value  =  VBool Bool
     |VInt Int
     |VFun (Value -> Value)
     |VError

data  Hask  = HTrue | HFalse
     |HIf Hask Hask Hask
     |HLit Int
     |Hask :==: Hask
     |Hask :+:  Hask
     | Hask :*: Hask
     |HVar Name
     |HLam Name Hask
     |Hask :$: Hask

infix 4 :==:
infixl 6 :+:
infixl 9 :$:
infixl 7 :*:

type  HEnv  =  [(Name, Value)]

instance Show Value where
     show (VInt x)  = show x
     show (VBool x) = show x
     show (VFun f)  = error "cannot show a function"
     show VError    = error "cannot show an error"

instance Eq Value where
    (VInt x) == (VInt y)   = x == y
    (VBool x) == (VBool y) = x == y
    _ == _                 = error "invalid opertion"



hEval :: Hask -> HEnv -> Value
hEval HTrue r           =  VBool True
hEval HFalse r          =  VBool False
hEval (HIf c d e) r     = hif (hEval c r) (hEval d r) (hEval e r)
  where  hif (VBool b) v w  =  if b then v else w
         hif _ _ _ = error "invalid if statement"
hEval (HLit x) r        = VInt x
hEval (x :==: y) r      = VBool (hEval x r == hEval y r)
hEval (x :+: y) r       = VInt (a + b)
    where
        (VInt a) = hEval x r
        (VInt b) = hEval y r
hEval (x :*: y) r       = VInt (a * b)
    where
        (VInt a) = hEval x r
        (VInt b) = hEval y r
hEval (HVar x) r        = fromMaybe VError (lookup x r)
-- hEval (HLam name hask) r = f (hEval hask r)
--     where  
--         VFun f = fromMaybe VError (lookup name r)
hEval (HLam x e) r    =  VFun (\v -> hEval e ((x,v):r))
hEval (d :$: e) r     =  happ (hEval d r) (hEval e r)
  where
      happ (VFun f) v  =  f v
      happ _ _ = error "invalid parameters for $"


run :: Hask -> String
run pg = show (hEval pg [])

p1 :: Hask
p1 = (HLit 4 :+: HLit 3 :==: HLit 6) :==: HTrue -- "False"

p2 :: Hask
p2 = (HLit 10 :+: HLit (-4) :==: HLit 6) :==: HTrue -- "True"

p3 :: Hask
p3 = HLit 10 :+: HLit (-4) :==: (HLit 6 :==: HTrue) -- "Exception: invalid operation"

p4 :: Hask
p4 = (HLam "x" (HLam "y" (HVar "x" :+: HVar "y")) :$: HLit 2) :$: HLit 3 -- "5"

p5 :: Hask
p5 = (HLam "x" (HLam "y" (HVar "x" :*: HVar "y")) :$: HLit 5) :$: HLit 4 -- "20"

p6 :: Hask
p6= ((HLam "z" (HLam "y" (HLam "x" (HVar "x" :==: (HVar "y" :+: HVar "z")))) :$: HLit 5) :$: HLit 3) :$: HLit 2 -- "False"

p7 :: Hask
p7= ((HLam "z" (HLam "y" (HLam "x" (HVar "x" :==: (HVar "y" :+: HVar "z")))) :$: HLit 5) :$: HLit 3) :$: HTrue -- "Exception: invalid operation"

p8 :: Hask
p8 = HIf (HLit 3 :+: HLit 2 :==: HLit 5) HTrue HFalse -- "True"