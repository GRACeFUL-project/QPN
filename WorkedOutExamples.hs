module WorkedOutExamples where

import DSL.GraphDSL
import DSL.SolverExports
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
vanKouwen5 = do
  a <- mkNode "Loss of species diversity"
  b <- mkNode "Increased overexploitation of vegetation"
  c <- mkNode "Regional climate change"
  d <- mkNode "Migrating agricultural impacts on soils"
  e <- mkNode "Ecosystem conversion"
  f <- mkNode "Enhanced greenhouse effect"
  g <- mkNode "Loss of fertility"
  h <- mkNode "Fresh water scarcity"
  i <- mkNode "Terrestrial run-off changes"
  k <- mkNode "Population growth"
  l <- mkNode "Expantion of agriculturaly used lands"
  m <- mkNode "Sinking of groundwater level"
  n <- mkNode "Soil erosion"
  o <- mkNode "Migration"
  p <- mkNode "International indeptedness"
  q <- mkNode "Impoverishment"
  r <- mkNode "Intensification of agrigulture"
  t <- mkNode "Greater regulation by economic policies"
  u <- mkNode "Women's emansipation"
  v <- mkNode "Widening international social [...]"
  w <- mkNode "Globalization of markets"
  x <- mkNode "Knowledge and technology transfer"
  y <- mkNode "Combating poverty"
  z <- mkNode "Increasing international conventions [...]"
  
  
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