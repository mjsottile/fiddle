module AbstractC where

import SimpleC

data AbstractStmt =
	OrderedLoop Variable Bounds AbstractStmt
  | EmbeddedStmt Stmt
  deriving (Show, Eq)

data Variable = Variable Symbol CType
  deriving (Show, Eq)

data Bounds =
    BNumeric  Int Int
  | BSymbolic Symbol Symbol
  deriving (Show, Eq)

transAbs :: AbstractStmt -> Stmt
transAbs (OrderedLoop (Variable n _) bds body) =
  let counter = VarRef n
      (lo,hi) = (case bds of
  	  	           BNumeric l h  -> (Constant $ IConst l, Constant $ IConst h)
  	  	           BSymbolic l h -> (VarRef l, VarRef h))
  in
    For (Assign AEqual counter lo)
        (BinOp BLT counter hi)
        (Incr counter)
        (transAbs body)
transAbs (EmbeddedStmt s) = s

