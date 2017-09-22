{-# LANGUAGE
      DeriveFunctor,
      FlexibleContexts,
      FlexibleInstances,
      MultiParamTypeClasses,
      TypeOperators,
      UndecidableInstances
  #-}

module AlaCarte where

--
-- * Data types a la carte infrastructure
--

-- | Fixed point of a signature.
data Term s = Fix (s (Term s))

instance Show (s (Term s)) => Show (Term s) where
  show (Fix x) = "(Fix " ++ show x ++ ")"

-- | Sum of two signatures.
data (s1 :+: s2) e = InL (s1 e) | InR (s2 e)
  deriving (Eq,Functor,Show)
infixr :+:

-- Subset relation on signatures.
class sub :<: sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)

instance (s :<: s) where
  inj = id
  prj = Just

instance {-# OVERLAPPING #-} (s1 :<: (s1 :+: s2)) where
  inj         = InL
  prj (InL a) = Just a
  prj _       = Nothing

instance {-# OVERLAPPING #-} (s1 :<: s3) => (s1 :<: (s2 :+: s3)) where
  inj         = InR . inj
  prj (InR b) = prj b
  prj _       = Nothing

-- | Fold an interpretation over a composed term.
foldTerm :: Functor s => (s a -> a) -> Term s -> a
foldTerm f (Fix t) = f (fmap (foldTerm f) t)

-- | Inject a case from a component signature into the composed term type.
inject :: (s1 :<: s2) => s1 (Term s2) -> Term s2
inject = Fix . inj

-- | Project from a composed term type to one of the cases.
project :: (s1 :<: s2) => (Term s2) -> Maybe (s1 (Term s2))
project (Fix t) = prj t
