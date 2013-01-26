--
-- lattice
--
module Fiddle.Lattice where

class Rotatable a where
  rotate :: a -> a

-- 2D Bravais lattices
data LatticeType2D = Oblique | Rectangular | Rhombic | Hexagonal | Square
  deriving (Show, Eq)

data Lattice2D = Lattice2D LatticeType2D Vec2D Double
  deriving (Show, Eq)

data Vec2D = Vec2D Double Double
  deriving (Show, Eq)

data OffsetLattice2D = OffsetLattice2D Lattice2D Vec2D
  deriving (Show, Eq)

instance Rotatable Vec2D where
  rotate (Vec2D a b) = Vec2D (-b) a 

-- TODO: rotations for hex are different than for square
instance Rotatable OffsetLattice2D where
  rotate (OffsetLattice2D l v) = OffsetLattice2D l (rotate v)

(@@) :: Lattice2D -> Vec2D -> OffsetLattice2D
(@@) l v = OffsetLattice2D l v

{-
rect = Lattice2D Rectangular (Vec2D 1.0 2.0) 90.0

x = rect@@(Vec2D 0.0 1.0)
-}
