module Arith where

import Data.List (union)

data Expr
   = Lit Int
   | Neg Expr
   | Add Expr Expr
  deriving (Eq,Show)

eval :: Expr -> Int
eval (Lit n)   = n
eval (Neg e)   = negate (eval e)
eval (Add l r) = eval l + eval r

pretty :: Expr -> String
pretty (Lit n)   = show n
pretty (Neg e)   = "-" ++ pretty e
pretty (Add l r) = concat ["(", pretty l, " + ", pretty r, ")"]

lits :: Expr -> [Int]
lits (Lit n) = [n]
lits (Neg e) = lits e
lits (Add l r) = lits l `union` lits r
    
ex1 :: Expr
ex1 = Add (Add (Lit 3) (Lit 4)) (Neg (Lit 5))
