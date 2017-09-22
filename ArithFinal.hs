module ArithFinal where

type Expr = (Int, String)

eval :: Expr -> Int
eval = fst

pretty :: Expr -> String
pretty = snd

lit :: Int -> Expr
lit n = (n, show n)

neg :: Expr -> Expr
neg e = (negate (eval e), "-" ++ pretty e)

add :: Expr -> Expr -> Expr
add l r = (eval l + eval r, concat ["(", pretty l, " + ", pretty r, ")"])

mul :: Expr -> Expr -> Expr
mul l r = (eval l * eval r, concat ["(", pretty l, " * ", pretty r, ")"])

ex1 :: Expr
ex1 = add (add (lit 3) (lit 4)) (neg (lit 5))

ex2 :: Expr
ex2 = mul ex1 ex1
