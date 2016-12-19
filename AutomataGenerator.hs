module AutomataGenerator where

import Interfaces.MZAST
import Automata.NFA
import Qualitatives
{-
mzCond :: String -> (String, Bop, String) -> Sign -> Sign -> Item
mzCond v (i, o, j) t f = Constraint $ ITE [(Bi o (Var i) (Var j), Bi Eq (Var v) (IConst $ stateInt t))] (Bi Eq (Var v) (IConst $ stateInt f))

condPQ :: String -> (String, Bop, String) -> Item
condPQ v (i, o, j) = mzCond v (i, o, j) PQ' Q'

condMQ :: String -> (String, Bop, String) -> Item
condMQ v (i, o, j) = mzCond v (i, o, j) MQ' Q'

-- Cases described in 'Ehancing QPNs for trade-off resolution', figure 9.
caseA :: String -> String -> Item
caseA i j = condPQ "S_A" (i, Lte, j)

caseB :: String -> String -> Item
caseB i j = condPQ "S_B" (j, Lte, i)

caseC :: String -> String -> Item
caseC i j = condMQ "S_C" (i, Lte, j)

caseD :: String -> String -> Item
caseD i j = condMQ "S_C" (j, Lte, i)
-}

{-
  Type of the automaton's states.
  
-}
type State = (Int, Maybe Degree)

signToStateInt :: Sign -> Int
signToStateInt = (+ 1) . fromEnum

signStateInts :: [Int]
signStateInts = map signToStateInt signDomain

-- Initial state
initState :: State
initState = (0, Nothing)

-- For each sign, a transition from initial value to a different state
deltaInitAdd :: State -> Sign -> State
deltaInitAdd (0, _) v = (signToStateInt v, Nothing)