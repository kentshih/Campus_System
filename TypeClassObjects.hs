{-# LANGUAGE ExistentialQuantification #-}

-- | A type class-based implementation of object-oriented integer sets
--   from William Cook's essay "On Understanding Data Abstraction, Revisted"
module TypeClassObjects where

--
-- * Interface for integer sets
--

-- | Interface.
class Set' s where
  isEmpty'  :: s -> Bool
  contains' :: s -> Int -> Bool
  insert'   :: s -> Int -> Set
  union'    :: s -> Set -> Set

-- | Set instance wrapper.
data Set = forall s. Set' s => Set s

-- | Operations on wrapped instances.
isEmpty  (Set s) = isEmpty'  s
contains (Set s) = contains' s
insert   (Set s) = insert'   s
union    (Set s) = union'    s


--
-- * Basic instances
--


-- ** Empty set

data Empty = Empty
empty = Set Empty

instance Set' Empty where
  isEmpty'  _   = True
  contains' _ _ = False
  insert'       = insertWrap
  union'    _ s = s


-- ** Insert an integer into a set

data Insert = Insert Set Int

instance Set' Insert where
  isEmpty' _ = False
  contains' (Insert s n) m
      | n == m    = True
      | otherwise = contains s m
  insert' = insertWrap
  union'  = unionWrap

-- | Helper function for use in other instances.
insertWrap s n = Set (Insert (Set s) n)


-- ** Union of two sets

data Union = Union Set Set

instance Set' Union where
  isEmpty'  (Union s t)   = isEmpty  s   && isEmpty  t
  contains' (Union s t) n = contains s n || contains t n
  insert' = insertWrap
  union'  = unionWrap

-- | Helper function for use in other instances.
unionWrap s t = Set (Union (Set s) t)


--
-- * More instances
--

-- ** All even numbers

data Evens = Evens
evens = Set Evens

instance Set' Evens where
  isEmpty'  _   = False
  contains' _ n = even n
  insert'       = insertWrap
  union'        = unionWrap


-- ** All odd numbers

data Odds = Odds
odds = Set Odds

instance Set' Odds where
  isEmpty'  _   = False
  contains' _ n = odd n
  insert'       = insertWrap
  union'        = unionWrap


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
