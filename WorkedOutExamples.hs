module WorkedOutExamples where

import DSL.GraphDSL
import DSL.SolverExports
import QPNModeler
{-
  === Worked out examples
-}
{-
  Example from Druzdzel & Henrion
-}
druzdzelHenrion = do
  a <- mkNode "A"
  b <- mkNode "B"
  c <- mkNode "C"
  d <- mkNode "D"
  e <- mkNode "E"
  f <- mkNode "F"
  
  link b >+> c >+> d >-> e
  link b >+> d
  link b >+> a
  link f >-> e
  
  constrain $ a := P

{-
  Example from Van Kouwen, The Quasta Approach
  p.51, Figure 3.6
-}
vanKouwen1 = do
  a <- mkNode "A"
  b <- mkNode "B"
  c <- mkNode "C"
  d <- mkNode "D"
  e <- mkNode "E"
  f <- mkNode "F"
  g <- mkNode "G"
  
  link a >+> b >+> d
  link e >+> b >+> g
  link c >-> b
  link c >+> d
  link e >+> f >+> g
  
  constrain $ g := P

{-
  Example from Van Kouwen, The Quasta Approach
  p.52, Figure 3.8
-}
vanKouwen2 = do
  a <- mkNode "A"
  b <- mkNode "B"
  c <- mkNode "C"
  d <- mkNode "D"
  e <- mkNode "E"
  f <- mkNode "F"
  g <- mkNode "G"
  
  link a >+> b >+> d
  link e >+> b >+> g
  link c >-> b
  link c >+> d
  link e >+> f >+> g
  
  constrain $ d := P
  
{-
  Example from Van Kouwen, The Quasta Approach
  p.68, Figure 4.5
-}
vanKouwen3 = do
  a <- mkNode "Sea level rise"
  b <- mkNode "Risk of flooding"
  c <- mkNode "Flooding"
  d <- mkNode "Measures to prevent flooding"
  e <- mkNode "Ecology in coastal zones"
  f <- mkNode "Investments"
  
  link a >+> b >+> c >+> d >-> e
  link d >-> b
  link f >+> d
  
  constrain $ e := P

{-
  Example from Van Kouwen, The Quasta Approach
  p.71, Figure 4.8
-}
vanKouwen4 = do
  a <- mkNode "Turbidity"
  b <- mkNode "Resuspended sedimentation"
  c <- mkNode "Algae"
  d <- mkNode "Water depth"
  e <- mkNode "Nutrients"
  f <- mkNode "Vegetation"
  g <- mkNode "Waves"
  h <- mkNode "Allelopathic subs."
  i <- mkNode "Fish"
  k <- mkNode "Zooplankton"
  
  link a >-> f >+> k >-> e >+> c >+> a
  link d >-> f >-> g >+> b >+> a 
  link i >+> b
  link i >-> k
  link f >+> h >-> e
  link f >-> e
  
  constrain $ a := M
  
{-
  Example from Van Kouwen, The Quasta Approach
  p.72, Figure 4.9
-}
vanKouwen5 = do                                            -- Node enumaration
  a <- mkNode "Loss of species diversity"                  -- 0
  b <- mkNode "Increased overexploitation of vegetation"   -- 1
  c <- mkNode "Regional climate change"                    -- 2
  d <- mkNode "Migrating agricultural impacts on soils"    -- 3
  e <- mkNode "Ecosystem conversion"                       -- 4
  f <- mkNode "Enhanced greenhouse effect"                 -- 5
  g <- mkNode "Loss of fertility"                          -- 6
  h <- mkNode "Fresh water scarcity"                       -- 7
  i <- mkNode "Terrestrial run-off changes"                -- 8
  k <- mkNode "Population growth"                          -- 9
  l <- mkNode "Expantion of agriculturaly used lands"      -- 10
  m <- mkNode "Sinking of groundwater level"               -- 11
  n <- mkNode "Soil erosion"                               -- 12
  o <- mkNode "Migration"                                  -- 13
  p <- mkNode "International indeptedness"                 -- 14
  q <- mkNode "Impoverishment"                             -- 15
  r <- mkNode "Intensification of agrigulture"             -- 16
  t <- mkNode "Greater regulation by economic policies"    -- 17
  u <- mkNode "Women's emansipation"                       -- 18
  v <- mkNode "Widening international social [...]"        -- 19
  w <- mkNode "Globalization of markets"                   -- 20
  x <- mkNode "Knowledge and technology transfer"          -- 21
  y <- mkNode "Combating poverty"                          -- 22
  z <- mkNode "Increasing international conventions [...]" -- 23
  
  
  link b >+> a
  link b >+> e
  link b >+> n

  link e >+> a
  link e >+> c
  link e >+> f
  link e >+> n
  link e >+> i

  link i >+> n

  link c >+> b
  link c >+> n
  link c >+> h

  link f >+> c

  link d >-> n

  link n >+> q

  link k >+> b
  link k >+> l
  link k >+> r
  link k >+> q
  link k >+> o

  link q >+> o
  link q >+> k
  link q >+> b
  link q >+> l
  link q >+> r

  link u >-> k

  link y >-> q

  link v >+> q
  link v >+> m
  link v >+> z

  link l >+> e
  link l >+> m
  link l >-> q
  link l >+> n

  link g >+> q

  link r >+> n
  link r >+> g
  link r >+> m
  link r >-> q

  link z >+> x
  link z >?> t

  link x >-> n
  link x >-> g

  link h >+> q

  link m >+> h

  link p >+> v
  link p >+> t

  link t >+> q
  link t >?> z
  link t >?> w

  link w >+> v
  link w >?> t
  link w >+> p

  constrain $ k := M
  constrain $ q := M