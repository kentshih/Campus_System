{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}

module MonoidClass where

import Prelude hiding (Monoid(..))


--
-- * Monoid type class
--

-- | A monoid is an algebraic structure with:
--    1. an identity element
--    2. an associative binary operator
class Monoid a where
  mempty  :: a
  mappend :: a -> a -> a

-- Laws:
--   mappend mempty s         <=>  s
--   mappend s mempty         <=>  s
--   mappend (mappend s t) u  <=>  mappend s (mappend t u)


--
-- * Instances
--

newtype IntAdd = IA Int
  deriving (Eq,Enum,Num,Show)

newtype IntMul = IM Int
  deriving (Eq,Enum,Num,Show)

instance Monoid IntAdd where
  mempty  = 0
  mappend = (+)

instance Monoid IntMul where
  mempty  = 1
  mappend = (*)

instance Monoid [a] where
  mempty  = []
  mappend = (++)

instance Monoid (a -> a) where
  mempty  = id
  mappend = (.)

instance (Monoid a, Monoid b) => Monoid (a,b) where
  mempty = (mempty, mempty)
  mappend (la,lb) (ra,rb) = (mappend la ra, mappend lb rb)


--
-- * Derived operations
--

-- | Fold mappend over a list of elements.
mconcat :: Monoid a => [a] -> a
mconcat = foldr mappend mempty

-- Now use this in GHCi to do cool things!
