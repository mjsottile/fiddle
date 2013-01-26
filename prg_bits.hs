import Fiddle.BitFields

flagBitSet =
  let
      -- border
      a = registerBitField emptyBitSet "b" 4 Nothing ["n","s","w","o"]
      b = addCombinations a "b" [["n","w"],["s","w"],["n","o"],["s","o"]]

      -- fluid or not fluid
      c = registerBitField b "f" 1 Nothing ["f"]

      -- internal boundary conditions
      d = registerBitField c "c" 4 (Just "e") ["n","s","w","o"]
      e = permuteBitField d "c"
  in e

