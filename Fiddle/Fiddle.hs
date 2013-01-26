module Fiddle.Fiddle where

{-

FiDdLe: An EDSL for expressing finite difference problems.

Matthew Sottile // matt@galois.com
Dec. 2011

-}

--
-- ======================================================================
--
-- types
--
-- ======================================================================
--

{-
 Type represents a symbol type.  They can be primitives
 or aggregates.
-}

-- primitive types
data PrimType = TYFloat | TYInteger | TYBool | TYVoid
  deriving (Show, Eq)

-- aggregate types           
data AggregateType = TYArray Type Int          -- type and dimension
                   | TYRecord [(String, Type)] -- set of fields
  deriving (Show, Eq)

-- enumerated type
data EnumeratedType = TYEnumerated [(String,Int)]
  deriving (Show, Eq)

-- types
data Type = TYPrim PrimType
          | TYAgg AggregateType
          | TYEnum EnumeratedType 
  deriving (Show, Eq)

--
-- ======================================================================
--
-- arrays
--
-- ======================================================================
--

{- array abstrator handles ::

 -> representation
 -> dimension access
 -> symbol resolution
 -> indexing and elemental assignment
 -> bulk update
 -> data parallel update

-}

{-

Array represented by:

struct arrayHandle {
  type *basepointer;
  int ndim;
  int *dimlower;
  int *dimupper;
}

-}

--
-- Array descriptor 
--
data Array2D = Array2D Symbol
  deriving (Show, Eq)

data ArrayBound2D = LowerRows | UpperRows | LowerCols | UpperCols
  deriving (Show, Eq)

arrName :: Array2D -> Symbol
arrName (Array2D s) = s

data Indexable2D = Indexable2D Array2D Expr Expr
  deriving (Show, Eq)

arrayDataField :: String
arrayDataField = "->data"

arrayBoundField :: ArrayBound2D -> String
arrayBoundField f =
  case f of
    LowerRows -> "->dimlower[0]"
    LowerCols -> "->dimlower[1]"
    UpperRows -> "->dimupper[0]"
    UpperCols -> "->dimupper[1]"


--
-- ======================================================================
--
-- symbols
--
-- ======================================================================
--

data Symbol = Symbol {
  symName :: String,
  symType :: Type
  } deriving (Show, Eq)

type SymbolTable = [(String,Symbol)]

findSymbol :: SymbolTable -> String -> Maybe Symbol
findSymbol table s = lookup s table

makeScalarSymbol :: String -> PrimType -> Symbol
makeScalarSymbol s t = Symbol { symName = s, symType = (TYPrim t) }

makeScalarSymbols :: [String] -> PrimType -> [Symbol]
makeScalarSymbols ss t = map (\s -> makeScalarSymbol s t) ss

makeArraySymbol :: String -> PrimType -> Int -> Symbol
makeArraySymbol s t d = Symbol { symName = s, 
                                 symType = TYAgg (TYArray (TYPrim t) d) }

makeArraySymbols :: [String] -> PrimType -> Int -> [Symbol]
makeArraySymbols ss t d = map (\s -> makeArraySymbol s t d) ss

--
-- ======================================================================
--
-- expressions
--
-- ======================================================================
--

data Expr = Variable    Symbol
          | FConstant   Float
          | IConstant   Integer
          | FunCall     String [Expr]
          | Multiply    Expr Expr
          | Divide      Expr Expr
          | Minus       Expr Expr
          | Plus        Expr Expr
          | Square      Expr
          | Sqrt        Expr
          | ArrayBound  Array2D ArrayBound2D
          | BoolTest    Tester Expr Expr
          | Indexed2D   Array2D Expr Expr Expr Expr
          deriving (Show, Eq)

data Tester = LessThan | LessEqual | GreaterThan | GreaterEqual 
            | Equal | NotEqual | BoolAnd | BoolOr
            deriving (Show, Eq)

data Function = Function Type String [Symbol] ScopedRegion

data ScopedRegion = ScopedRegion (Maybe SymbolTable) [SymbolTable -> Statement]

data Statement = 
    Sequence [SymbolTable -> Statement]
  | ForRange1 (Expr, Expr, Maybe Expr) [Expr -> Statement]
  | ForRange2 (Expr, Expr, Maybe Expr) (Expr, Expr, Maybe Expr) 
              [Expr -> Expr -> Statement]
  | Assign Expr Expr
  | Conditional Expr Statement Statement
  | Return Expr
  | AddressOf Expr
  | Empty

instance Num Expr where
  a + b = Plus a b
  a - b = Minus a b
  a * b = Multiply a b
  abs a = FunCall "fabs" [a]
  fromInteger a = IConstant a
  signum a = undefined

instance Fractional Expr where
  a / b = Divide a b
  recip a = Divide (FConstant 1) a
  fromRational a = FConstant (fromRational a)

(<>>) :: Expr -> Expr -> Expr
(<>>) a b = BoolTest GreaterThan a b

(<<>) :: Expr -> Expr -> Expr
(<<>) a b = BoolTest LessThan a b

(<>=>) :: Expr -> Expr -> Expr
(<>=>) a b = BoolTest GreaterEqual a b

(<<=>) :: Expr -> Expr -> Expr
(<<=>) a b = BoolTest LessEqual a b

(<==>) :: Expr -> Expr -> Expr
(<==>) a b = BoolTest Equal a b

(<!=>) :: Expr -> Expr -> Expr
(<!=>) a b = BoolTest NotEqual a b

--
-- helpers for common operators
--

rot_indices :: Expr -> Expr
rot_indices (Indexed2D a i j io jo) = (Indexed2D a i j jo io)
rot_indices (Square e) = Square (rot_indices e)
rot_indices (Sqrt e) = Sqrt (rot_indices e)
rot_indices (Minus a b) = Minus (rot_indices a) (rot_indices b)
rot_indices (Plus a b) = Plus (rot_indices a) (rot_indices b)
rot_indices (Multiply a b) = Multiply (rot_indices a) (rot_indices b)
rot_indices (Divide a b) = Divide (rot_indices a) (rot_indices b)
rot_indices a@(ArrayBound arr bd) = a
rot_indices v@(Variable var) = v
rot_indices f@(FConstant fc) = f
rot_indices i@(IConstant ic) = i
rot_indices (FunCall s args) = FunCall s (map rot_indices args)
rot_indices (BoolTest t a b) = BoolTest t (rot_indices a) (rot_indices b)

-- indexing relative to some location (i,j) in an array by offsets
-- (ioff,joff)
(@@) :: Indexable2D -> (Int, Int) -> Expr
(@@) (Indexable2D a i j) ( ioff, joff) = 
  Indexed2D a i j (IConstant (fromIntegral $ ioff)) 
                  (IConstant (fromIntegral $ joff))

infixl 9 @@
infix 4 <>>
infix 4 <<>
infix 4 <<=>
infix 4 <>=>
infix 4 <==>
infix 4 <!=>

-- square
sq :: Expr -> Expr
sq a = Square a

symsToTable :: [Symbol] -> SymbolTable
symsToTable ss = map (\s -> (symName s, s)) ss


minfun :: Expr -> Expr -> Expr
minfun e1 e2 = FunCall "min" [e1,e2]

var :: Symbol -> Expr
var s = Variable s

invoke :: Symbol 
          -> (Expr -> Expr) 
          -> Expr
invoke s f = f (var s)          

invoke2 :: Symbol -> Symbol 
           -> (Expr -> Expr -> Expr) 
           -> Expr
invoke2 s1 s2 f = f (var s1) (var s2)

invoke3 :: Symbol -> Symbol -> Symbol
           -> (Expr -> Expr -> Expr -> Expr) 
           -> Expr
invoke3 s1 s2 s3 f = f (var s1) (var s2) (var s3)

invoke4 :: Symbol -> Symbol -> Symbol -> Symbol
           -> (Expr -> Expr -> Expr -> Expr -> Expr) 
           -> Expr
invoke4 s1 s2 s3 s4 f = f (var s1) (var s2) (var s3) (var s4)

invoke5 :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol
           -> (Expr -> Expr -> Expr -> Expr -> Expr -> Expr) 
           -> Expr
invoke5 s1 s2 s3 s4 s5 f = f (var s1) (var s2) (var s3) (var s4) (var s5)

invoke6 :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
           -> (Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr) 
           -> Expr
invoke6 s1 s2 s3 s4 s5 s6 f = 
  f (var s1) (var s2) (var s3) (var s4) (var s5) (var s6)

invoke7 :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol
           -> (Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr) 
           -> Expr
invoke7 s1 s2 s3 s4 s5 s6 s7 f = 
  f (var s1) (var s2) (var s3) (var s4) (var s5) (var s6) (var s7)


