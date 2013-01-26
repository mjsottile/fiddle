--
-- very simple Fermi-Pasta-Ulam model
--
-- matt@galois.com
--

import Fiddle.Types
import Fiddle.Symbols
import Fiddle.Arrays
import Fiddle.AST.Expressions
import Fiddle.Helpers

-- 1D arrays for position, velocity, and acceleration
[q, q', q''] = makeArraySymbols ["q", "qvel", "qacc"] TYFloat 1

-- Step size
dt = makeScalarSymbol "dt" TYFloat

-- Concrete arrays
n :: Int
n = 32

