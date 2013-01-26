module Fiddle.PrettyPrint where

import Text.PrettyPrint.HughesPJ

import Fiddle.Fiddle
import Data.List
import System.IO

indexArray :: Array2D -> Expr -> Expr -> Doc
indexArray arr i j =
  (text name)<>(brackets $ pP i)<>(brackets $ pP j)
  where name = symName $ arrName arr

--
-- given a 2D array and the two dimensional indices, generate the indexing
-- string in C
--
programToFile :: [Function] -> String -> IO ()
programToFile fs fname = do
  h <- openFile fname WriteMode
  hPutStrLn h $ render $ hcat $ map pPFunc fs
  hClose h

--
-- pretty print statement
--
pPS :: SymbolTable -> Statement -> Doc
pPS stab (ForRange2 (ilo,ihi,iincr) (jlo,jhi,jincr) body) =
  let i = makeScalarSymbol "i__" TYInteger
      j = makeScalarSymbol "j__" TYInteger
      innerstab = [("i__",i),("j__",j)]++stab
      iincrs = case iincr of
                   Just e  -> (text "i__ = i__ +")<+>(pP e)
                   Nothing -> text "i__++"
      jincrs = case jincr of
                   Just e  -> (text "j__ = j__ +")<+>(pP e)
                   Nothing -> text "j__++"
      (ils,ihs) = (pP ilo, pP ihi)
      (jls,jhs) = (pP jlo, pP jhi)
      i_decl = (text "int i__=")<>ils
      i_test = (text "i__<=")<>ihs
      i_incr = iincrs
      j_decl = (text "int j__=")<>jls
      j_test = (text "j__<=")<>jhs
      j_incr = jincrs
  in (text $ "for")<>(parens $ ssl [i_decl, i_test, i_incr])<+>
     (braces $
       (text $ "for")<>(parens $ ssl [j_decl, j_test, j_incr])<+>
       (braces $ ssl $ map (pPS innerstab) 
                           (map (\f -> f (var i) (var j)) body)))
pPS stab (ForRange1 (ilo,ihi,iincr) body) =
  let i = makeScalarSymbol "i__" TYInteger
      innerstab = [("i__",i)]++stab
      iincrs = case iincr of
                   Just e  -> (text "i__ = i__ +")<+>(pP e)
                   Nothing -> text "i__++"
      (ils,ihs) = (pP ilo, pP ihi)
      i_decl = (text "int i__=")<>ils
      i_test = (text "i__<=")<>ihs
      i_incr = iincrs
  in (text $ "for")<>(parens $ ssl [i_decl, i_test, i_incr])<+>
     (braces $ ssl $ map (pPS innerstab) 
                         (map (\f -> f (var i)) body))
pPS stab (AddressOf s) = (text "&")<>(parens $ pP s)
pPS stab (Return s) = (text "return")<+>(pP s)<>semi
pPS stab (Assign lhs rhs) = (pP lhs)<+>(text "=")<+>(pP rhs)<>semi
pPS stab Empty = empty
pPS stab (Conditional e st sf) = 
  (text "if")<+>(parens $ pP e)<+>(braces $ pPS stab st)<+>rest
  where rest = case sf of
                 Empty -> empty
                 _     -> (text "else")<+>(braces $ pPS stab sf)
pPS stab (Sequence ss) = hsep $ map (pPS stab) (map (\f -> f stab) ss)
  
--
-- scoped region
--
pSR (ScopedRegion syms body) =
  let (decls,stab) = case syms of
        (Just slist) -> (ssl $ map (\i -> (pPSym $ snd i)) slist, slist)
        Nothing      -> (empty, [])
  in braces $ decls <+> (ssl (map (\x -> pPS stab $ x stab) body)) 

--
-- function
--
pPFunc (Function t s args body) =
  (pPType t)<+>(text s)<>(parens $ csl $ map pPSym args)<>
  (pSR body)

--
-- types
--
pPType :: Type -> Doc
pPType (TYPrim TYFloat) = text "float"
pPType (TYPrim TYInteger) = text "int"
pPType (TYPrim TYBool) = text "int"
pPType (TYPrim TYVoid) = text "void"
pPType (TYAgg (TYArray (TYPrim TYFloat) 2)) = text "FloatArray2D_t *"
pPType (TYAgg (TYArray (TYPrim TYInteger) 2)) = text "IntArray2D_t *"
pPType _ = error "Unsupported"

pPSym :: Symbol -> Doc
pPSym s = stype<+>(text sname)
  where stype = pPType $ symType s
        sname = symName s

--
-- paren helper
--
pRen :: String -> String
pRen s = "("++s++")"

--
-- bool testers
--
pTest :: Tester -> Doc
pTest LessThan     = text "<"
pTest LessEqual    = text "<="
pTest GreaterThan  = text ">"
pTest GreaterEqual = text ">="
pTest Equal        = text "=="
pTest NotEqual     = text "!="
pTest BoolAnd      = text "&&"
pTest BoolOr       = text "||"

csl :: [Doc] -> Doc
csl ds = hsep $ punctuate comma ds

ssl :: [Doc] -> Doc
ssl ds = hsep $ punctuate semi ds

--
-- expressions
--
pP :: Expr -> Doc
pP (Variable s) = text $ symName s
pP (FConstant f) = float f
pP (IConstant f) = integer f
pP (FunCall f args) = (text f)<>(parens $ csl $ map pP args)
pP (BoolTest t a b) = (pP a)<>(pTest t)<>(pP b)
pP (Multiply a b) = (parens $ pP a)<>(text "*")<>(parens $ pP b)
pP (Divide a b) = (parens $ pP a)<>(text "/")<>(parens $ pP b)
pP (Minus a b) = (parens $ pP a)<>(text "-")<>(parens $ pP b)
pP (Plus a b) = (parens $ pP a)<>(text "+")<>(parens $ pP b)
pP (Indexed2D arr i j io jo) = indexArray arr (i + io) (j + jo)
pP (Square a) = (pP $ Multiply a a)
pP (Sqrt a) = undefined
pP (ArrayBound a b) = (text $ symName $ arrName a)<>(text $ arrayBoundField b)
