{-# LANGUAGE
      DeriveFunctor,
      FlexibleContexts,
      MultiParamTypeClasses,
      TypeOperators
  #-}

module PCF where

import Prelude hiding (abs)

import AlaCarte
import Prim
import Cond


--
-- * Syntax
--

-- | Variables.
type Var = String

-- | Lambda calculus.
data Lam e
   = Ref Var
   | Abs Var e
   | App e e
  deriving (Eq,Functor,Show)

ref :: (Lam :<: t) => Var -> Term t
ref = inject . Ref

abs :: (Lam :<: t) => Var -> Term t -> Term t
abs x t = inject (Abs x t)

abs2 :: (Lam :<: t) => Var -> Var -> Term t -> Term t
abs2 x y t = abs x (abs y t)

app :: (Lam :<: t) => Term t -> Term t -> Term t
app f x = inject (App f x)

app2 :: (Lam :<: t) => Term t -> Term t -> Term t -> Term t
app2 f x y = app (app f x) y


--
-- * Pretty printing
--

instance Pretty Lam where
  prettyAlg (Ref x)   = x
  prettyAlg (Abs x e) = concat ["(λ", x, ". ", e, ")"]
  prettyAlg (App l r) = concat ["(", l, " ", r, ")"]


--
-- * Evaluation
--

-- ** Environments

-- | Variable environment.
type Env a = Var -> a

-- | The empty environment.
empty :: Env a
empty = \x -> error ("Unbound variable: " ++ x)

-- | Extend the environment with a new binding.
set :: Var -> a -> Env a -> Env a
set x a m = \y -> if x == y then a else m y

-- | Lookup a binding in the environment.
get :: Var -> Env a -> a
get x m = m x


-- ** Evaluation semantics

-- | A value is a primitive value or a closure.
type Value = Term (PVal :+: Closure)

-- | Semantic domain for evaluation.
type EvalSem = Env Value -> Value

-- | A closure bundles a function with its environment.
data Closure t = Close (Env Value) Var EvalSem
  deriving Functor

instance Pretty Closure where
  prettyAlg _ = "** closure **"

-- | Signature for evaluation semantics.
class Functor t => Eval t where
  evalAlg :: t EvalSem -> EvalSem

-- Boilerplate needed for each new interpretation.
instance (Eval s1, Eval s2) => Eval (s1 :+: s2) where
  evalAlg (InL a) = evalAlg a
  evalAlg (InR b) = evalAlg b

-- | Evaluate to a value.
eval :: Eval t => Term t -> Value
eval t = foldTerm evalAlg t empty

instance Eval Prim where
  evalAlg (P1 o e) m =
    case project (e m) of
      Just pv -> inject (evalP1 o pv)
      _ -> error "Type error: unary operator applied to non-primitive value"
  evalAlg (P2 o l r) m =
    case (project (l m), project (r m)) of
      (Just pl, Just pr) -> inject (evalP2 o pl pr)
      _ -> error "Type error: binary operator applied to non-primitive value"

instance Eval Cond where
  evalAlg (If c t e) m =
    case project (c m) of
      Just (B b) -> if b then t m else e m
      _ -> error "Type error: non-boolean condition"

instance Eval PVal where
  evalAlg (B b) _ = bool b
  evalAlg (I i) _ = int i

instance Eval Lam where
  evalAlg (Ref x)   m = get x m
  evalAlg (Abs x e) m = inject (Close m x e)
  evalAlg (App l r) m =
    case project (l m) of
      Just (Close m' x e) -> e (set x (r m) m')


--
-- * Examples
--

type PCF = Term (Lam :+: Cond :+: Prim :+: PVal)

ycomb :: PCF
ycomb = abs "f" (app term term)
  where
    term = abs "x" (app (ref "f") (app (ref "x") (ref "x")))

factorial :: PCF
factorial
  = app ycomb
  $ abs2 "f" "n"
  $ cond (op2 LTE (ref "n") (int 1))
      (int 1)
      (op2 Mul (ref "n")
               (app (ref "f") (op2 Add (ref "n") (op1 Neg (int 1)))))

ex4 :: PCF
ex4 = app factorial (int 8)
