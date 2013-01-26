module Fiddle.AST.Expressions (
  (<*>), (</>), (<+>), (<->), (<<>), (<>>), (<<=>), (<>=>), (<==>), (<!=>)
) where
	
import Fiddle.Types

--
-- ======================================================================
--
-- expressions
--
-- ======================================================================
--



(<*>) :: Expr -> Expr -> Expr
(<*>) a b = Multiply a b

(</>) :: Expr -> Expr -> Expr
(</>) a b = Divide a b

(<+>) :: Expr -> Expr -> Expr
(<+>) a b = Plus a b

(<->) :: Expr -> Expr -> Expr
(<->) a b = Minus a b

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

-- precedence rules based on http://www.haskell.org/onlinereport/decls.html#fixity
infixl 7 <*>
infixl 7 </>
infixl 6 <+>
infixl 6 <->
infix 4 <>>
infix 4 <<>
infix 4 <<=>
infix 4 <>=>
infix 4 <==>
infix 4 <!=>
