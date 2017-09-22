{-# LANGUAGE
      GeneralizedNewtypeDeriving,
      OverloadedStrings
  #-}

module TaglessFinal where

import Data.String (IsString)


-- * Super-simple core language

-- ** Syntax

class Add r where
  lit :: Int -> r Int
  neg :: r Int -> r Int
  add :: r Int -> r Int -> r Int


-- ** Evaluation

newtype EvalInt t = EvalInt Int
  deriving (Eq,Num,Ord,Show)

instance Add EvalInt where
  lit = EvalInt
  neg = negate
  add = (+)


-- ** Pretty printing

newtype Pretty t = Pretty String
  deriving (Eq,IsString,Monoid,Ord,Show)

instance Add Pretty where
  lit = Pretty . show
  neg = mappend "-"
  add l r = mconcat ["(", l, " + ", r, ")"]
  -- add (Pretty l) (Pretty r) = Pretty ("(" ++ l ++ " + " ++ r ++ ")")


-- * Compatible extension

class Mul r where
  mul :: r Int -> r Int -> r Int

instance Mul EvalInt where
  mul = (*)

instance Mul Pretty where
  mul l r = mconcat ["(", l, " * ", r, ")"]

-- | Example program illustrating that we can arbitrarily mix addition and
--   multiplication.
ex1 :: (Add r, Mul r) => r Int
ex1 = mul (add (lit 2) (lit 3)) (add (mul (lit 4) (lit 5)) (lit 6))


-- * Incompatible extension

-- ** Syntax

class Cond r where
  lte  :: r Int -> r Int -> r Bool
  cond :: r Bool -> r a -> r a -> r a


-- ** Pretty printing

instance Cond Pretty where
  lte (Pretty l) (Pretty r)
    = Pretty (concat [l, " <= ", r])
  cond (Pretty c) (Pretty t) (Pretty e)
    = Pretty (concat ["(if ", c, " then ", t, " else ", e, ")"])


-- ** Evaluation

data EvalIntBool t = I Int | B Bool
  deriving (Eq,Show)

instance Add EvalIntBool where
  lit = I
  neg (I i)       = I (negate i)
  add (I l) (I r) = I (l + r)

instance Mul EvalIntBool where
  mul (I l) (I r) = I (l * r)

instance Cond EvalIntBool where
  lte (I l) (I r) = B (l <= r)
  cond (B c) t e = if c then t else e

-- | Example program illustrating that we can arbitrarily mix addition and
--   multiplication.
ex2:: (Add r, Mul r, Cond r) => r Int
ex2 = add ex1 (cond (lte (lit 5) (mul (lit 2) (lit 3))) (lit 9) (lit 10))
