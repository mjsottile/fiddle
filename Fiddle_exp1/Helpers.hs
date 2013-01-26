module Fiddle.Helpers where

import Fiddle.Types
import Fiddle.AST.Expressions

--
-- helpers for common operators
--

ic1 :: Expr
ic1 = IConstant 1

fc1 :: Expr
fc1 = FConstant 1

fc2 :: Expr
fc2 = FConstant 2

-- square
sq :: Expr -> Expr
sq a = a <*> a 

-- given X, return 1/X
recipro :: Expr -> Expr
recipro a = fc1 </> a

-- average two values
avg2 :: Expr -> Expr -> Expr
avg2 a b = (a <+> b) </> fc2

-- average three values
avg3 :: Expr -> Expr -> Expr -> Expr
avg3 a b c = (a <+> b <+> c) </> (FConstant 3)

-- average four values
avg4 :: Expr -> Expr -> Expr -> Expr -> Expr
avg4 a b c d = (a <+> b <+> c <+> d) </> (FConstant 4)

--
-- equations
--

fabs :: Expr -> Expr
fabs e = FunCall "fabs" [e]

minfun :: Expr -> Expr -> Expr
minfun e1 e2 = FunCall "min" [e1,e2]