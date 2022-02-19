
data Expr = Const Int      -- integer constant
          | Expr :+: Expr  -- addition
          | Expr :*: Expr  -- multiplication
           deriving Eq

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int                    -- leaf
          | Node Operation Tree Tree  -- branch
           deriving (Eq, Show)

instance Show Expr where
    show (Const i) = show i
    show (e1 :+: e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (e1 :*: e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"


evalExp :: Expr -> Int
evalExp (Const i)   = i
evalExp (e1 :*: e2) = evalExp e1 * evalExp e2
evalExp (e1 :+: e2) = evalExp e1 + evalExp e2

evalArb :: Tree -> Int
evalArb (Lf x)                 = x
evalArb (Node Add left right)  = evalArb left + evalArb right
evalArb (Node Mult left right) = evalArb left * evalArb right

expToArb :: Expr -> Tree
expToArb (Const i)   = Lf i
expToArb (e1 :*: e2) = Node Mult left right
    where
        left  = expToArb e1
        right = expToArb e2
expToArb (e1 :+: e2) = Node Add left right
    where
        left  = expToArb e1
        right = expToArb e2

-------------------------------------------------------------------------------------------------------------------------------

class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert
      :: Ord key
      => key -> value -> c key value -> c key value
  clookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  values :: c key value -> [value]
  toList :: c key value -> [(key, value)]
  fromList :: Ord key => [(key,value)] -> c key value

  -- get the list of keys from collection
  keys c  = [k | (k, v) <- toList c]

  -- get the list of values from collection
  values c  = [v | (k, v) <- toList c]

  -- tranfsorm a list of tuples in a collection
  fromList []             = empty
  fromList ((hk, hv) : t) = insert hk hv (fromList t)

-------------------------------------------------------------------------------------------------------------------------------

newtype PairList k v
    = PairList { getPairList :: [(k, v)] }

instance Collection PairList where
    empty = PairList []
    singleton k v = PairList [(k, v)]

    -- insert an element in a collection
    insert k v c =  fromList $ (k, v) : toList c

    -- return value from specified key
    clookup k c = lookup k (toList c)

    -- delete the element with specified key
    delete k c = fromList [(key, value) | (key,value) <- toList c, key /= k]

    -- make a list from the collection
    toList = getPairList

-------------------------------------------------------------------------------------------------------------------------------

data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare

instance Collection SearchTree where
    empty = Empty
    singleton k v = BNode Empty k (Just v) Empty

    -- insert an element in a collection
    insert k v Empty = singleton k v

    insert k v (BNode left key val right)
        | k > key                          = insert k v right
        | otherwise                        = insert k v left

    -- return value from specified key
    clookup k Empty = Nothing
    clookup k (BNode left key value right)
        | k < key    = clookup k left
        | k > key    = clookup k right
        | otherwise  = value

    -- delete the element with specified key
    delete k Empty = Empty
    delete k (BNode left key value right)
        | k < key   = BNode (delete k left) key value right
        | k > key   = BNode left key value (delete k right)
        | otherwise = BNode left key Nothing right

    -- make a list from the collection
    toList Empty                         = []
    toList (BNode left k (Just v) right) = [(k, v)] ++ toList left ++ toList right
    toList (BNode left k Nothing right)  = toList left ++ toList right
