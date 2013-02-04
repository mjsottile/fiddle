import Control.Monad.RWS.Strict
import qualified Data.Map as Map
import Debug.Trace

import SimpleC

-- parameters for the moment are strings
type Parameters = String

-- log strings
type LogType = String

-- a scoping unit is a counter plus all of the variables in
-- scope at the moment
type Scope = [(Integer, Map.Map String FType)]

-- initial scope set is just one scope with a zero counter
-- and empty variable map
initialScope = [(0, Map.empty)]

-- custom RWS monad type
type MyRWS = RWS Parameters LogType Scope

enterScope :: MyRWS ()
enterScope = do
  (s:ss) <- get
  put (s:s:ss)

exitScope :: MyRWS ()
exitScope = do
  (s:ss) <- get
  put ss

tvar :: FType -> MyRWS Variable
tvar t = do
  ((i,m):ss) <- get
  let newvarname = "tmp_"++(show i)
      m' = Map.insert newvarname t m
  put ((i+1,m'):ss)
  return $ Variable newvarname t

nvar :: String -> FType -> MyRWS Variable
nvar n t = do
  ((i,m):ss) <- get
  if (Map.member n m) then
    error $ "Attempted to instantiate variable that exists: "++n
  else
    do let m' = Map.insert n t m
       put ((i,m'):ss)
       return $ Variable n t

getGoing f =
  evalRWST f "" initialScope

t1 :: MyRWS ()
t1 = do
  v1 <- tvar TyFloat
  v2 <- tvar TyFloat
  enterScope
  v3 <- tvar TyFloat
  v4 <- tvar TyFloat
  exitScope
  v5 <- tvar TyFloat

  return (trace ((show v1)++"\n"++(show v2)++"\n"++(show v3)++"\n"++(show v4)++"\n"++(show v5)++"\n") ())
