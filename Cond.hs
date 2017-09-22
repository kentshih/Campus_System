{-# LANGUAGE
      DeriveFunctor,
      FlexibleContexts,
      MultiParamTypeClasses,
      TypeOperators
  #-}

module Cond where

import AlaCarte
import Prim


--
-- * Syntax
--

-- | Conditional expressions.
data Cond t = If t t t
  deriving (Eq,Functor,Show)

-- | Smart constructor.
cond :: (Cond :<: t) => Term t -> Term t -> Term t -> Term t
cond c t e = inject (If c t e)


--
-- * Pretty printing
--

instance Pretty Cond where
  prettyAlg (If c t e) = unwords ["if", c, "then", t, "else", e, "end"]


--
-- * Evaluation
--

instance PEval Cond where
  pevalAlg (If c t e) = case c of
    B True  -> t
    B False -> e
    _ -> error "Type error: non-boolean condition"


--
-- * Examples
--

type CExpr = Term (Cond :+: Prim :+: PVal)

ex3 :: (Cond :<: t, Prim :<: t, PVal :<: t) => Term t
ex3 = cond (op2 LTE (int 3) (int 4)) ex1 ex2
