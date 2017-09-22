
-- | A data type-based implementation of object-oriented integer sets
--   from William Cook's essay "On Understanding Data Abstraction, Revisted"
module DataTypeObjects where

--
-- * Interface for integer sets
--

-- | Interface.
data Set = Set {
  isEmpty  :: Bool,
  contains :: Int -> Bool,
  insert   :: Int -> Set,
  union    :: Set -> Set
}


--
-- * Instances
--

-- | Empty set.
empty = this where
  this = Set {
    isEmpty  = True,
    contains = const False,
    insert   = insertObj this,
    union    = id
  }

-- | Insert an integer into a set.
insertObj s n = this where
  this = Set {
    isEmpty  = False,
    contains = \m -> if n == m then True else contains s m,
    insert   = insertObj this,
    union    = unionObj  this
  }

-- | Union of two sets.
unionObj s t = this where
  this = Set {
    isEmpty  = isEmpty s && isEmpty t,
    contains = \n -> contains s n || contains t n,
    insert   = insertObj this,
    union    = unionObj  this
  }

-- | All even numbers.
evens = this where
  this = Set {
    isEmpty = False,
    contains = even,
    insert   = insertObj this,
    union    = unionObj this
 }

-- | All odd numbers.
odds = this where
  this = Set {
    isEmpty = False,
    contains = odd,
    insert   = insertObj this,
    union    = unionObj this
 }


--
-- * Examples
--

-- | Some simple sets.
s1 = empty `insert` 1
s2 = empty `insert` 2 `insert` 3
s3 = s1 `union` s2
s4 = evens `insert` 1
s5 = evens `union` s3
s6 = evens `union` odds

-- | Tests - should be true.
t1 = s1 `contains` 1
t2 = all (contains s2) [2,3]
t3 = all (contains s3) [1,2,3]
t4 = all (contains s4) [0,1,2,4,6,8,10]
t5 = all (contains s5) [0,1,2,3,4,6,8,10]
t6 = all (contains s6) [0..10]

-- | Tests - should be false.
f1 = not $ any (contains s1) [0,2,3,4,5]
f2 = not $ any (contains s2) [0,1,4,5]
f3 = not $ any (contains s3) [0,4,5]
f4 = not $ any (contains s4) [3,5,7,9]
f5 = not $ any (contains s5) [5,7,9]
f6 = not $ any (contains s6) []

-- | All tests.
tests = and [t1,t2,t3,t4,t5,t6,f1,f2,f3,f4,f5,f6]
