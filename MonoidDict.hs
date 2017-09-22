
module MonoidDict where

import Prelude hiding (Monoid(..))


--
-- * Monoid dictionary
--

-- | A monoid is an algebraic structure with:
--    1. an identity element
--    2. an associative binary operator
data Monoid a = Mon a (a -> a -> a)

-- | The identity element of a given monoid.
mempty :: Monoid a -> a
mempty (Mon a _) = a

-- | The associative binary operator of a given monoid.
mappend :: Monoid a -> a -> a -> a 
mappend (Mon _ f) = f

-- Laws:
--   mappend d (mempty d) s       <=>  s
--   mappend d s (mempty d)       <=>  s
--   mappend d (mappend d s t) u  <=>  mappend d s (mappend d t u)


-- NOTE: Can re-write the above definitions using "record syntax":
--
-- data Monoid a = Mon {
--   mempty  :: a,
--   mappend :: a -> a -> a
-- }


--
-- * Instances
--

-- | Addition over the integers.
add :: Monoid Int
add = Mon 0 (+)

-- | Multiplication over the integers.
mul :: Monoid Int
mul = Mon 1 (*)

-- | Monoid instance for lists.
list :: Monoid [a]
list = Mon [] (++)

-- | Here's a fun one!
fun :: Monoid (a -> a)
fun = Mon id (.)

-- | Pairs of monoids.
pair :: Monoid a -> Monoid b -> Monoid (a,b)
pair da db = Mon empty append
  where
    empty = (mempty da, mempty db)
    append (la,lb) (ra,rb) = (mappend da la ra, mappend db lb rb)


--
-- * Derived operations
--

-- | Fold the monoid operator over a list of elements.
mconcat :: Monoid a -> [a] -> a
mconcat d = foldr (mappend d) (mempty d)

-- Now use this in GHCi to do cool things!
