module Fiddle.Symbols (
  findSymbol,
  makeScalarSymbol,
  makeArraySymbol,
  makeScalarSymbols,
  makeArraySymbols,
  var,
  symsToTable
) where

import Fiddle.Types

--
-- ======================================================================
--
-- symbols
--
-- ======================================================================
--


findSymbol :: SymbolTable -> String -> Maybe Symbol
findSymbol table s = lookup s table

makeScalarSymbols :: [String] -> PrimType -> [Symbol]
makeScalarSymbols names ty = map (\n -> makeScalarSymbol n ty) names

makeArraySymbols :: [String] -> PrimType -> Int -> [Symbol]
makeArraySymbols names ty d = map (\n -> makeArraySymbol n ty d) names

makeScalarSymbol :: String -> PrimType -> Symbol
makeScalarSymbol s t = Symbol { symName = s, symType = (TYPrim t) }

makeArraySymbol :: String -> PrimType -> Int -> Symbol
makeArraySymbol s t d = Symbol { symName = s, 
                                 symType = TYAgg (TYArray (TYPrim t) d) }

symsToTable :: [Symbol] -> SymbolTable
symsToTable ss = map (\s -> (symName s, s)) ss

var :: Symbol -> Expr
var s = Variable s
