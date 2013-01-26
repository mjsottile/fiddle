module Fiddle.Types (
  PrimType (..),
  AggregateType (..),
  EnumeratedType (..),
  Type (..),
  Expr (..),
  Tester (..),
  Function (..),
  ScopedRegion (..),
  Statement (..),
  SymbolTable,
  Symbol (..),
  Array1D (..),
  Indexable1D (..)
  Array2D (..),
  Indexable2D (..)
) where

	
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
data AggregateType = 
	  TYArray Type Int          -- type and dimension
	| TYRecord [(String, Type)] -- set of fields
	deriving (Show, Eq)

-- enumerated type
data EnumeratedType = TYEnumerated [(String,Int)]
	deriving (Show, Eq)

-- types
data Type = 
	  TYPrim PrimType
	| TYAgg AggregateType
	| TYEnum EnumeratedType 
	deriving (Show, Eq)

data Expr = Variable  Symbol
          | FConstant Float
          | IConstant Int
          | FunCall   String [Expr]
          | Multiply  Expr Expr
          | Divide    Expr Expr
          | Minus     Expr Expr
          | Plus      Expr Expr
          | BoolTest  Tester Expr Expr
          | Indexed1D Array1D Expr
          | Indexed2D Array2D Expr Expr            
          deriving (Show, Eq)

data Tester = LessThan | LessEqual | GreaterThan | GreaterEqual 
            | Equal | NotEqual | BoolAnd | BoolOr
            deriving (Show, Eq)

data Function = Function Type String [Symbol] ScopedRegion

data ScopedRegion = ScopedRegion (Maybe SymbolTable) [SymbolTable -> Statement]

data Statement = 
    Sequence [SymbolTable -> Statement]
  | ForRange1 (Expr, Expr) [Expr -> Statement]
  | ForRange2 (Expr, Expr) (Expr, Expr) [Expr -> Expr -> Statement]
  | Assign Expr Expr
  | Conditional Expr Statement Statement
  | Return Expr
  | Empty

type SymbolTable = [(String,Symbol)]

data Symbol = Symbol {
  symName :: String,
  symType :: Type
  } deriving (Show, Eq)

--
-- Array descriptor 
--
data Array1D = Array1D {
  arrName :: Symbol,
  arrLength :: Expr
  } deriving (Show, Eq)
             
data Indexable1D = Indexable1D Array1D Expr
  deriving (Show, Eq)

data Array2D = Array2D {
  arrName :: Symbol, -- the symbol for the array variable
  arrNRows :: Expr,  -- expression defining the number of rows
  arrNCols :: Expr   -- expression defining the number of columns
  } deriving (Show, Eq)

data Indexable2D = Indexable2D Array2D Expr Expr
  deriving (Show, Eq)
