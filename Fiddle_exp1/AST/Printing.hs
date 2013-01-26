module Fiddle.AST.Printing
where

import Data.List
import Fiddle.Symbols
import Fiddle.Types
import Fiddle.Arrays

pPS :: SymbolTable -> Statement -> String
pPS stab (ForRange2 (ilo,ihi) (jlo,jhi) body) =
  let i = makeScalarSymbol "i__" TYInteger
      j = makeScalarSymbol "j__" TYInteger
      innerstab = [("i__",i),("j__",j)]++stab
      (ils,ihs) = (pP ilo, pP ihi)
      (jls,jhs) = (pP jlo, pP jhi)
  in "for (int i__="++ils++"; i__<="++ihs++"; i__++) {\n"++
     "for (int j__="++jls++"; j__<="++jhs++"; j__++) {\n"++
     (intercalate ";\n" $ map (pPS innerstab) (map (\f -> f (var i) (var j)) body))++";\n}\n}\n"
pPS stab (ForRange1 (ilo,ihi) body) =
  let i = makeScalarSymbol "i__" TYInteger
      innerstab = [("i__",i)]++stab
      (ils,ihs) = (pP ilo, pP ihi)
  in "for (i__="++ils++"; i__<"++ihs++"; i__++) {\n"++
     (intercalate ";\n" $ map (pPS innerstab) (map (\f -> f (var i)) body))++"\n}\n"
pPS stab (Return s) = "return "++(pP s)++";\n"
pPS stab (Assign lhs rhs) = (pP lhs)++" = "++(pP rhs)++";\n"
pPS stab Empty = ""
pPS stab (Conditional e st sf) = "if ("++(pP e)++") {\n"++(pPS stab st)++"}"++rest
  where rest = case sf of                            
          Empty -> "\n"
          _     -> " else { "++(pPS stab sf)++" }\n"
pPS stab (Sequence ss) = concat $ map (pPS stab) (map (\f -> f stab) ss)
  
--
-- scoped region
--
pSR (ScopedRegion syms body) =
  let (decls,stab) = case syms of
        (Just slist) -> (concat $ map (\i -> (pPSym $ snd i)++";\n") slist, slist)
        Nothing      -> ("", [])
  in "{\n"++decls++"\n\n"++
     (intercalate ";\n" (map (\x -> pPS stab $ x stab) body))++";\n}\n"

pPFunc (Function t s args body) =
  (pPType t)++" "++s++"("++(intercalate ", " (map pPSym args))++")\n"++
  (pSR body)++"\n"

pPType :: Type -> String
pPType (TYPrim TYFloat) = "float"
pPType (TYPrim TYInteger) = "int"
pPType (TYPrim TYBool) = "int"
pPType (TYPrim TYVoid) = "void"
pPType (TYAgg (TYArray t d)) = (pPType t)++(concat $ take d $ repeat "*")
pPType _ = error "Unsupported"

pPSym :: Symbol -> String
pPSym s = stype++" "++sname
  where stype = pPType $ symType s
        sname = symName s

--
-- pretty printers
--
pRen :: String -> String
pRen s = "("++s++")"

pTest :: Tester -> String
pTest LessThan     = "<"
pTest LessEqual    = "<="
pTest GreaterThan  = ">"
pTest GreaterEqual = ">="
pTest Equal        = "=="
pTest NotEqual     = "!="
pTest BoolAnd      = "&&"
pTest BoolOr       = "||"

pP :: Expr -> String
pP (Variable s) = symName s
pP (FConstant f) = show f
pP (IConstant f) = show f
pP (FunCall f args) = f++"("++(intercalate "," (map pP args))++")"
pP (BoolTest t a b) = (pP a)++(pTest t)++(pP b)
pP (Multiply a b) = (pRen $ pP a)++"*"++(pRen $ pP b)
pP (Divide a b) = (pRen $ pP a)++"/"++(pRen $ pP b)
pP (Minus a b) = (pRen $ pP a)++"-"++(pRen $ pP b)
pP (Plus a b) = (pRen $ pP a)++"+"++(pRen $ pP b)
pP (Indexed2D arr i j) = indexArray arr (pP) i j
