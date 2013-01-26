module Fiddle.Arrays (
  indexArray,
  (@),
  (@@)
) where

import Fiddle.AST.Expressions
import Fiddle.Types
import Fiddle.Helpers

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



--
-- given a 2D array and the two dimensional indices, generate the indexing
-- string in C
--
indexArray :: Array2D -> (Expr -> String) -> Expr -> Expr -> String
indexArray arr pP i j =
  name++"["++(pP i)++"]["++(pP j)++"]"
  where name = symName $ arrName arr
{-  name++"["++(pP $ Plus (Multiply i ncols) j)++"]"
  where name = symName $ arrName arr
        ncols = arrNCols arr
-}


(@) :: Indexable1D -> Int -> Expr
(@) (Indexable1D a i) 0    = Indexed1D a i
(@) (Indexable1D a i) 1    = Indexed1D a (i <+> ic1)
(@) (Indexable1D a i) (-1) = Indexed1D a (i <-> ic1) 
(@) _ _                    = undefined

(@@) :: Indexable2D -> (Int, Int) -> Expr
(@@) (Indexable2D a i j) ( 0,  0) = Indexed2D a i j
(@@) (Indexable2D a i j) ( 0,  1) = Indexed2D a i (j <+> ic1)
(@@) (Indexable2D a i j) ( 0, -1) = Indexed2D a i (j <-> ic1)
(@@) (Indexable2D a i j) ( 1,  0) = Indexed2D a (i <+> ic1) j
(@@) (Indexable2D a i j) ( 1,  1) = Indexed2D a (i <+> ic1) (j <+> ic1)
(@@) (Indexable2D a i j) ( 1, -1) = Indexed2D a (i <+> ic1) (j <-> ic1)
(@@) (Indexable2D a i j) (-1,  0) = Indexed2D a (i <-> ic1) j
(@@) (Indexable2D a i j) (-1,  1) = Indexed2D a (i <-> ic1) (j <+> ic1)
(@@) (Indexable2D a i j) (-1, -1) = Indexed2D a (i <-> ic1) (j <-> ic1)
(@@) _ _                          = undefined



infixl 9 @@

