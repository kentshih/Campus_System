{-# LANGUAGE
      FlexibleInstances,
      MultiParamTypeClasses,
      TypeFamilies
  #-}

module BinaryMethod where


type Scalar = Int
type Vector2D = (Int,Int)
type Vector3D = (Int,Int,Int)

class Add a where
  (+++) :: a -> a -> a

class Mult a b where
  type Result a b
  (***) :: a -> b -> Result a b

instance Add Scalar where
  l +++ r = l + r

instance Add Vector2D where
  (lx,ly) +++ (rx,ry) = (lx + rx, ly + ry)

instance Add Vector3D where
  (lx,ly,lz) +++ (rx,ry,rz) = (lx + rx, ly + ry, lz + rz)

instance Mult Scalar Scalar where
  type Result Scalar Scalar = Scalar
  l *** r = l * r

instance Mult Scalar Vector2D where
  type Result Scalar Vector2D = Vector2D
  c *** (x,y) = (c*x, c*y)

instance Mult Scalar Vector3D where
  type Result Scalar Vector3D = Vector3D
  c *** (x,y,z) = (c*x, c*y, c*z)

instance Mult Vector2D Scalar where
  type Result Vector2D Scalar = Vector2D
  (x,y) *** c = (c*x, c*y)

instance Mult Vector3D Scalar where
  type Result Vector3D Scalar = Vector3D
  (x,y,z) *** c = (c*x, c*y, c*z)

-- Result of multiplying two 2D vectors is the determinant.
instance Mult Vector2D Vector2D where
  type Result Vector2D Vector2D = Scalar
  (lx,ly) *** (rx,ry) = lx*ry - rx*ly

-- Result of multiplying two 3D vectors is the cross product.
instance Mult Vector3D Vector3D where
  type Result Vector3D Vector3D = Vector3D
  (lx,ly,lz) *** (rx,ry,rz) = (ly*rz - lz*ry, lz*rx - lx*rz, lx*ry - ly*rx)

c1, c2 :: Scalar
c1 = 10
c2 = 100

v1, v2 :: Vector2D
v1 = (2,3)
v2 = (4,5)

w1, w2 :: Vector3D
w1 = (2,3,4)
w2 = (5,6,7)

ex1 = c1 *** (v1 *** v2)
ex2 = c1 *** (v1 +++ v2)

ex3 = c1 *** (w1 +++ w2)
ex4 = c1 *** (w1 *** w2)
