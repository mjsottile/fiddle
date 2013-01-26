{- base primitive is a vector space -}
data Vector a = Vec2 a a
              | Vec3 a a a
              | Vec4 a a a a
              | Vec5 a a a a a
  deriving (Show, Eq)

{- lattice defined by a set of basis vectors -}
data Lattice a = Lattice [Vector a]
  deriving (Show, Eq)

cartesian2d :: Lattice Float
cartesian2d = Lattice [Vec2 1 0, Vec2 0 1]

{- some constants -}
m = 100
n = 100

lo_x = 0
lo_y = 0
hi_x = m
hi_y = n

{- a region is a lattice constrained to the upper and lower bounds for each dimension, or a point set
   defined by sampling a set of lattice points in the given lattice -}
data Region a = Region (Lattice a) [(Int,Int)]
              | PointSet (Lattice a) [(Int,Int)]
  deriving (Show, Eq)

{- define a fluid region as our cartesian2d grid constrainted between bounds -}
fluidRegion = Region cartesian2d [(lo_x, hi_x),(lo_y,hi_y)]

type BdFunc = Float -> Float

{- a boundary defined on a region with a function for each boundary corresponding to the bounded
   region dimensions -}
data Boundary a = Boundary (Region a) [(BdFunc, BdFunc)]

bd_lo = id
bd_hi = id

boundaryRegion = Boundary fluidRegion [(bd_lo, bd_hi), (bd_lo, bd_hi)]

data CellType = Empty | Solid
  deriving (Show, Eq)

data Treatment a = Treatment CellType (Region a)
  deriving (Show, Eq)

emptySpace = Treatment Empty fluidRegion

circle :: Float -> [(Int,Int)]
circle r = filter (\(a,b) -> (sqrt $ fromIntegral (a*a + b*b))<=r) 
                  $ concat [[(i,j) | i <- [lo_x..hi_x]] | j <- [lo_y..hi_y]]

obstacle1 = undefined
obstacle2 = undefined

data RegionOperator a = Union [Treatment a]
                      | Intersection [Treatment a]
                      | Difference [Treatment a]
  deriving (Show, Eq)

cellTreatments = Union [emptySpace, obstacle1, obstacle2]

