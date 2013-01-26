import Fiddle.Fiddle
import Fiddle.PrettyPrint
import Fiddle.BitFields
import Text.PrettyPrint.HughesPJ

--
-- ======================================================================
--
-- code for 2D NS 
--
-- based on equations from book
--
-- matthew sottile // matt@galois.com
--
-- ======================================================================
--


--
-- variables
--
{-
  gx, gy : body forces
  gamma  : donor cell parameter
  tau    : ??
  dt     : timestep
  dx, dy : discretization in x and y dimension
  re     : Reynolds number
-}
[gx, gy, gamma, tau, dt, dx, dy, re, eps] =
  makeScalarSymbols ["gx","gy","gamma","tau","dt","dx","dy","re","eps"] TYFloat

{-
  m,n,i,j : indices
-}
[m, n, i, j, maxiter] = 
  makeScalarSymbols ["m", "n", "i", "j", "maxiter"] TYInteger

{-
  U,V : X and Y components of velocity field
  F,G : tentative X and Y velocity field computations
  P   : pressure
-}
[rhs, u, v, f, g, p] = 
  makeArraySymbols ["rhs", "u", "v", "f", "g", "p"] TYFloat 2

flag = makeArraySymbol "flag" TYInteger 2

[ua, va, pa, fa, ga, rhsa, flaga] =
  map Array2D [u, v, p, f, g, rhs, flag]

flagBitSet = bs
  where
    -- border
    a = registerBitField emptyBitSet "b" 4 Nothing ["n","s","w","o"]
    b = addCombinations a "b" [["n","w"],["s","w"],["n","o"],["s","o"]]
    
    -- fluid or not fluid
    c = registerBitField b "f" 1 Nothing ["f"]

    -- internal boundary conditions
    d = registerBitField c "c" 4 (Just "e") ["n","s","w","o"]
    bs = permuteBitField d "c"

--
-- the program
--

program = [computeMax ua,  
           timeStepControl tau re dx dy u v,
           comp_FG fa ga ua va re dx dy dt gx gy gamma, 
           comp_RHS fa ga rhsa flaga dt dx dy,
           comp_RBSOR ua eps maxiter]

-- helper
nosyms f s = f

(===) a b = Assign a b

--
-- FUNCTION:: computeMax
--
-- given an array, generate a function to compute its maximum
-- value
computeMax :: Array2D -> Function
computeMax u = Function
               (symType accum)
               "computeMax"
               [(arrName u)]
               (ScopedRegion (Just symtab) [nosyms accum_init, 
                                            nosyms theloop, 
                                            nosyms ret])
  where 
    -- make an accumulator
    accum = makeScalarSymbol "accum" TYFloat
    
    -- add it to the symbol table
    symtab = symsToTable [accum]
    
    -- define the bounds for the loop based on the array
    rbounds = (0, (ArrayBound u UpperRows) - 1, Nothing)
    cbounds = (0, (ArrayBound u UpperCols) - 1, Nothing)
    
    -- initialize the accumulator to the element at (0,0) in the array
    accum_init = (var accum) === (Indexed2D u 0 0 0 0)
    
    -- for loop is defined over the row and column bounds and the given body
    theloop = ForRange2 rbounds cbounds [body]
    
    -- loop body
    body i j = Conditional ((Indexed2D u i j 0 0) <>> (Variable accum))
                           ((var accum) === (Indexed2D u i j 0 0))
                           Empty
               
    -- return
    ret = Return $ var accum

--
-- FUNCTION: timestepControl
--
-- timestep control, eq. 3.50
--
timeStepControl :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Symbol 
                -> Function    
timeStepControl tau re dx dy u v = Function
                                   (symType dt)
                                   "timestepControl"
                                   [tau, re, dx, dy, u, v]
                                   (ScopedRegion (Just symtab)
                                    (map nosyms body))
  where
    syms@[umax,vmax,dt] = makeScalarSymbols ["umax","vmax","dt"] TYFloat
    
    symtab = symsToTable syms
    
    body = [(var umax) === (FunCall "computeMax" [var u]),
            (var vmax) === (FunCall "computeMax" [var v]),
            (var dt === (invoke6 tau re dx dy umax vmax eq350)),
            Return (var dt)]

--
-- compute F and G
--
comp_FG f g u v re dx dy dt gx gy gamma =
  Function 
    (TYPrim TYVoid)
    "comp_fg"
    [arrName f, arrName g, arrName u, arrName v, re, dx, dy, dt, gx, gy, gamma]
    (ScopedRegion Nothing
       [nosyms theloop,
        nosyms loop2,
        nosyms loop3])
  where
    rbounds = (1, ArrayBound f UpperRows, Nothing)
    cbounds = (1, ArrayBound f UpperCols, Nothing)
    
    theloop = ForRange2 rbounds cbounds body
    
    body = [\i j -> (Indexed2D f i j 0 0) === 
                    (eq336 (Indexable2D u i j) (Indexable2D v i j) 
                           vre vdx vdy vdt vgx vgamma),
            \i j -> (Indexed2D g i j 0 0) === 
                    (eq337 (Indexable2D u i j) (Indexable2D v i j) 
                           vre vdx vdy vdt vgy vgamma)]
    
    loop2 = ForRange1 cbounds body2
    loop3 = ForRange1 rbounds body3 
    
    rmax = ArrayBound f UpperRows
    cmax = ArrayBound f UpperCols
    
    body2 = [\j -> (Indexed2D f 0 j 0 j) === (Indexed2D u 0 j 0 j),
             \j -> (Indexed2D f rmax j 0 0) === (Indexed2D u rmax j 0 0)]
    body3 = [\i -> (Indexed2D g i 0 0 0) === (Indexed2D v i 0 0 0),
             \i -> (Indexed2D g i cmax 0 0) === (Indexed2D v i cmax 0 0)]
    
    [vre, vdx, vdy, vdt, vgx, vgy, vgamma] = map var [re,dx,dy,dt,gx,gy,gamma]
    
--
-- Poisson equation solver
--    
comp_RHS :: Array2D -> Array2D -> Array2D -> Array2D -> Symbol -> Symbol -> Symbol
            -> Function
comp_RHS f g rhs flag dt dx dy =Function
                                (TYPrim TYVoid)
                                "comp_rhs"
                                [arrName f, arrName g, arrName rhs, arrName flag, dt, dx, dy]
                                (ScopedRegion Nothing
                                 [nosyms theloop])
  where
    rbounds = (1, ArrayBound f UpperRows, Nothing)
    cbounds = (1, ArrayBound f UpperCols, Nothing)
    theloop = ForRange2 rbounds cbounds [body]
    body i j = Assign (Indexed2D rhs i j 0 0) (eq338_RHS (Indexable2D f i j) (Indexable2D g i j) (var dx) (var dy) (var dt))

--
-- red-black SOR
--
comp_RBSOR :: Array2D -> Symbol -> Symbol
            -> Function
comp_RBSOR u eps maxiters =
  Function
    (TYPrim TYVoid)
    "comp_rbsor"
    [arrName u, eps, maxiters]
    (ScopedRegion Nothing
                  [nosyms theloop_odd, nosyms theloop_evn])
  where
    unrows = ArrayBound u UpperRows
    uncols = ArrayBound u UpperCols
    rbounds_odd = (1, unrows, Just 2)
    cbounds_odd = (1, uncols, Just 2)
    rbounds_evn = (2, unrows, Just 2)
    cbounds_evn = (2, uncols, Just 2)
    theloop_odd = ForRange2 rbounds_odd cbounds_odd [body]
    theloop_evn = ForRange2 rbounds_evn cbounds_evn [body]
    body i j = Assign (Indexed2D u i j 0 0) (fourpoint (Indexable2D u i j))


--
-- ======================================================================
--
--
-- equations from book
--
--
-- ======================================================================
--

eq350 :: Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr
eq350 tau re dx dy umax vmax =
  tau * (minfun 
           ((re / 2) * (1 / ((1 / (dx * dx)) +
                                       (1 / (dy * dy)))))
           (minfun (dx / (abs umax)) 
                   (dy / (abs vmax))))


eq338_RHS :: Indexable2D -> Indexable2D -> Expr -> Expr -> Expr
             -> Expr
eq338_RHS f g dx dy dt = (1 / dt) *
                         ( ((f@@(0,0) - f@@(-1,0)) / dx) +
                           ((g@@(0,0) - g@@(0,-1)) / dy) )

-- discrete momentum equations
eq334 :: Indexable2D -> Indexable2D ->
         Expr -> Expr ->
         Expr
eq334 f p dx dt =
  f@@(0,0) - (dt / dx) * (p@@(1,0) - p@@(0,0))

eq335 :: Indexable2D -> Indexable2D ->
         Expr -> Expr ->
         Expr
eq335 g p dy dt =
  g@@(0,0) - (dt / dy) * (p@@(0,1) - p@@(0,0))

eq336 :: Indexable2D -> Indexable2D ->
         Expr -> Expr -> Expr -> Expr -> Expr -> Expr ->
         Expr
eq336 u v re dx dy dt gx gamma = 
  u@@(0,0) +
  (dt * (
      (1 / re) * ((d2dx2 u dx) + (d2dy2 u dy)) -
      (d2dx u dx gamma) - (duvdy u v dy gamma) + gx))
        
eq337 :: Indexable2D -> Indexable2D ->
         Expr -> Expr -> Expr -> Expr -> Expr -> Expr ->
         Expr
eq337 u v re dx dy dt gy gamma =
  v@@(0,0) +
  (dt * (
      (1 / re) * ((d2dx2 v dx) + (d2dy2 v dy)) -
      (duvdx u v dx gamma) - (d2dy v dy gamma) + gy))

eq344 p rhs dx dy omega =
  (1 - omega) * p@@(0,0) + undefined

--
-- derivitaves
--

ddx u dx = (u@@(0,0) - u@@(-1,0)) / dx

d2dx u dx gamma = 
  (1 / dx) *
   ((sq ((u@@(0,0) + u@@(1,0))/2)) - (sq ((u@@(-1,0) + u@@(0,0))/2)))
  +
  (gamma / dx) *
   ( (((abs (u@@(0,0) + u@@(1,0))) / 2) * ((u@@(0,0) - u@@(1,0)) / 2)) - 
     (((abs (u@@(-1,0) + u@@(0,0))) / 2) * ((u@@(-1,0) - u@@(0,0)) / 2)))
  
d2dx2 u dx = 
  (u@@(1,0) - (2 * u@@(0,0)) + u@@(-1,0)) / (dx * dx)

duvdx u v dx gamma = 
  (1 / dx) * (((u@@(0,0) + u@@(0,1)) / 2) * ((v@@(0,0) + v@@(1,0)) / 2) -
              ((u@@(-1,0) + u@@(-1,1)) / 2) * ((v@@(-1,0) + v@@(0,0)) / 2))
  +
  (gamma / dx) *
   ( ((abs (u@@(0,0) + u@@(0,1))) / 2) * ((v@@(0,0) - v@@(1,0)) / 2) -
     ((abs (u@@(-1,0) + u@@(-1,1))) / 2) * ((v@@(-1,0) - v@@(0,0)) / 2))  
  
-- 
-- rotations: derivatives in the y direction come for free by exploiting
--            symmetry of underlying grid and rotating derivatives in the
--            x direction appropriately
--
ddy v dy           = rot_indices $ ddx v dy
d2dy v dy gamma    = rot_indices $ d2dx v dy gamma
d2dy2 v dy         = rot_indices $ d2dx2 v dy
duvdy u v dy gamma = rot_indices $ duvdx v u dy gamma

-- pressure
dpdx p dx = 
  (p@@(1,0) - p@@(0,0)) / dx

dpdy p dy = 
  (p@@(0,1) - p@@(0,0)) / dy

-- four-point SOR stencil
fourpoint :: Indexable2D -> Expr
fourpoint u = (u@@(-1,0) + u@@(1,0) + u@@(0,-1) + u@@(0,1)) / 4
  
{-

set t = 0
set n = 0
while t < t_end
  select delta_t (eq 3.50)
  set boundary values for u and v
  compute f^n and g^n according to (eq 3.36) and (3.37)
  compure RHS of pressure equation (eq 3.38)
  set it = 0
  while (it < it_max) && (r^ij > eps) 
    perform SOR according to (eq 3.44)
    compute residual norm for pressure equation ||r^ij||
    it = it + 1
  compute u^(n+1) and v^(n+1) according to (eq 3.34) and (3.35)
  t = t = delta_t
  n = n + 1

-}


{-
-- DEAD CODE : replaced via rotations

d2dy2 u dy = 
  (u@@(0,1) - (2 * u@@(0,0)) + u@@(0,-1)) / (dy * dy)

dvdy v dy = (v@@(0,0) - v@@(0,-1)) / dy

dv2dy v dy gamma = 
  (1 / dy) *
   ((sq ((v@@(0,0) + v@@(0,1))/2)) - (sq ((v@@(0,-1) + v@@(0,0))/2)))
  +
  (gamma / dy) *
   ( (((abs (v@@(0,0) + v@@(0,1))) / 2) * ((v@@(0,0) - v@@(0,1)) / 2)) - 
     (((abs (v@@(0,-1) + v@@(0,0))) / 2) * ((v@@(0,-1) - v@@(0,0)) / 2)))

duvdy u v dy gamma = 
  (1 / dy) * (((v@@(0,0) + v@@(1,0)) / 2) * ((u@@(0,0) + u@@(0,1)) / 2) -
              ((v@@(0,-1) + v@@(1,-1)) / 2) * ((u@@(0,-1) + u@@(0,0)) / 2))
  +
  (gamma / dy) *
    ( ((abs (v@@(0,0) + v@@(1,0))) / 2) * ((u@@(0,0) - u@@(0,1)) / 2) -
      ((abs (v@@(0,-1) + v@@(1,-1))) / 2) * ((u@@(0,-1) - u@@(0,0)) / 2))

-}

main :: IO ()  
main = do
  mapM (\i -> putStrLn $ i++"\n\n") $ map render $ map pPFunc program
  return ()
