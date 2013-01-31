module Fiddle2.Fiddle where

data Vec3 = Vec3 Double Double Double
  deriving (Show, Eq)

data IVec3 = IVec3 Int Int Int
  deriving (Show, Eq)

data PrimType =
    TYScalar
  | TYVector
  deriving (Show, Eq)

data Lattice = Lattice { lBasis :: [Vec3],
                         lType  :: PrimType,
                         lSym   :: Maybe [Vec3] }
  deriving (Show, Eq) 

data Grid = Grid { gLattice :: Lattice,
                   gExtent  :: (Vec3, Vec3) }
  deriving (Show, Eq)

type Symbol = String

data Field = Field { fLattice :: Lattice,
                     fSymbol :: Symbol }
  deriving (Show, Eq)

data Expr =
    LatticeOperator Field IVec3
  | BinOp BOp Expr Expr
  | Const Constant
  | Expon Expr Constant
  | Sqrt  Expr
  | Abs   Expr
  | Scale Constant Expr
  deriving (Show, Eq)

data Constant = 
    FConstant Float
  | IConstant Int
  deriving (Show, Eq)

data BOp = OpPlus | OpMinus | OpTimes | OpDivide
  deriving (Show, Eq)

(@@) :: Field -> (Int, Int, Int) -> Expr
(@@) l (a,b,c) = LatticeOperator l (IVec3 a b c)

instance Num Expr where
  a + b = BinOp OpPlus a b
  a - b = BinOp OpMinus a b
  a * b = BinOp OpTimes a b
  abs a = Abs a
  fromInteger a = Const (IConstant $ fromIntegral a)
  signum      a = error "signum not supported"

instance Fractional Expr where
  a / b = BinOp OpDivide a b
  recip a = BinOp OpDivide (Const $ IConstant 1) a
  fromRational a = Const $ FConstant (fromRational a)


