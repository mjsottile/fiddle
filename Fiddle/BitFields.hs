module Fiddle.BitFields where
--
-- it is not uncommon in a finite difference code to want to assign some
-- interpretation to mesh elements such as fluid vs not fluid, and different
-- types of boundary cells (e.g., flat horiz, flat vertical, or some
-- combination that correspond to corners).  These are easily represented as
-- bit fields such that we can make an array of integers that is the same
-- shape as the array of state variables and allow for element-wise
-- conditionals to choose the appropriate way to treat a cell in a given
-- computation.
--
-- mjsottile@gmail.com / jan 2012
-- 

import Data.Bits
import Data.List
import Data.Ord
import Data.Maybe

{-

A bit set has:

 - a list of distinct named fields

 - for each named field, there is a number of possible values that it can
   take on, each of which is assigned a bit mask and a name.

   for example, a field called "direction" may have 4 values,
   each of which has a bit associated with it and a field span of 4
   bits.

 - a field may optionally have a name associated with the zero case in which
   none of its bits are set.

-}
data BitSet = BitSet [(String,            -- field name
                       (Int,              -- field width
                        Maybe String,     -- optional zero field name
                        [(String,Int)]    -- list of mask/name combinations
                        ))] 
  deriving (Show, Eq)

-- count the number of fields in a bit set
numBitFields :: BitSet -> Int
numBitFields (BitSet bset) = length bset

-- empty bit set
emptyBitSet :: BitSet
emptyBitSet = BitSet []

-- given a bit set, a field name with a number of bits, zero name (optional),
-- and name list, return a new bitset with that field added
registerBitField :: BitSet -> String -> Int -> (Maybe String) -> [String] 
                    -> BitSet
registerBitField (BitSet fields) fieldname numentries zeroname names =
   BitSet (fields++[(fieldname,(numentries,zeroname,zip names entries))])
   where
      entries = map (shiftL 1) [0..(numentries-1)]

--
-- given a registered bit field, permute it such that the field is the same width
-- but now contains a named entry for every permutation of bits.
--
-- note: this assumes that it is applied to a bit field in which the only name/value
-- entries present are for single bits ONLY.
--
permuteBitField :: BitSet -> String -> BitSet
permuteBitField (BitSet fields) field =
   BitSet (map (\(n,(num,z,f)) -> if (n==field) then (n,(num,z,newentries))
                                                else (n,(num,z,f))) fields)
   where
      (_,_,entries) = fromJust $ lookup field fields
      permutes = sortBy (comparing length) $ 
                 sort $ 
                 filter (\i -> (length i) > 0)$
                 subsequences entries
      newentries = map (\i -> let (ns,vs) = unzip i
                              in (concat ns, foldl1 (.|.) vs)) permutes

--
-- given a bit field, add a set of combinations of named bits to form aggregates.
-- this is useful in addition to the permutation operator above in instances in which
-- all combinations are not meaningful.  This allows only those that are meaningful to
-- be specified.
--
addCombinations :: BitSet -> String -> [[String]] -> BitSet
addCombinations (BitSet fields) field combos =  
   BitSet (map (\(n,(num,z,f)) -> if (n==field) then (n,(num,z,f++newentries))
                                                else (n,(num,z,f))) fields)
   where
      (_,_,entries) = fromJust $ lookup field fields
      vals = map (\i -> map (\j -> fromJust $ lookup j entries) i) combos
      names = map concat combos
      newentries = zip names (map (foldl1 (.|.)) vals)
