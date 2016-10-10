module QPNModeler (
  iSolveCLD,
  solveCLD,
  OW
) where

import Interfaces.MZAST
import Interfaces.MZinHaskell
import DSL.SolverExports
import Data.List
import WorkedOutExamples
--import Examples

data ConstrainedCLD
  = ConstrainedCLD { cld     :: [NodeInfo]
                   , actions :: ([ActionCost], Budget)
                   , goals   :: [GoalProfit]
                   , restr   :: [Item]
                   , obsrv   :: [Obsrv]
                   }
         
type Budget = Maybe Int

type NodeInfo = (Node, [(Node, Sign)], [(Node, Sign)])

type Obsrv = (Node, Sign)

-- Attention!
-- Code assumes that 'values' has a pair for each possible sign.
-- See declareWeightArrays.
data OW = OW { node   :: Node
             , values :: [(Sign, Int)]
             }

type ActionCost = OW

type GoalProfit = OW

weight :: OW -> Sign -> Int
weight o s = head [n | (v, n) <- values o, s == v]

signToInt :: Sign -> Int
signToInt M = 1
signToInt Z = 2
signToInt P = 3
signToInt Q = 4

signDomain = [1..4]

helper :: [[Int]] -> Expr
helper = ArrayLit2D . map (map IConst)

-- Transition function for automaton A_+
transitionPlus :: Item
transitionPlus
  = Declare (Par, Array [Int, Int] (Par, Int)) "trans_plus"
            (Just (helper  [ [2, 3, 4, 5]
                           , [6, 2, 5, 5]
                           , [2, 7, 4, 5]
                           , [5, 4, 8, 5]
                           , [5, 5, 5, 9]
                           , [6, 2, 5, 5]
                           , [2, 7, 4, 5]
                           , [5, 4, 8, 5]
                           , [5, 5, 5, 9] ]))

-- Transition function for automaton A_x_+
transitionComb :: Item
transitionComb
  = Declare (Par, Array [Int, Int] (Par, Int)) "trans_comb"
            (Just (helper  [ [ 2,  3,  4,  5]
                           , [ 7,  6, 11, 13]
                           , [ 6,  8,  9, 12]
                           , [11,  9, 10, 13]
                           , [13, 12, 13, 13]
                           , [ 7, 14, 11, 13]
                           , [ 7,  6, 18, 13]
                           , [ 6, 15,  9, 12]
                           , [11, 16, 10, 13]
                           , [11,  9, 17, 13]
                           , [20, 12, 13, 13]
                           , [13, 19, 13, 13]
                           , [13, 12, 13, 20]
                           , [ 7, 14, 11, 13]
                           , [ 6, 15,  9, 12]
                           , [11, 16, 10, 13]
                           , [11,  9, 17, 13]
                           , [20, 12, 13, 13]
                           , [13, 19, 13, 13]
                           , [13, 12, 13, 20] ]))

regularPlus :: Expr -> Item
regularPlus al = Constraint $ Call (userD "regular")
                   [al, IConst 9, IConst $ last signDomain, Var "trans_plus", IConst 1,
                    SetLit [IConst 6, IConst 7, IConst 8, IConst 9]]

regularComb :: Expr -> Item
regularComb al = Constraint $ Call (userD "regular")
                   [al, IConst 20, IConst $ last signDomain, Var "trans_comb", IConst 1,
                    SetLit [IConst 14, IConst 15, IConst 16, IConst 17,
                            IConst 18, IConst 19, IConst 20]]

includeRegular :: Item
includeRegular = Include "regular.mzn"

nodeIdent :: Node -> Ident
nodeIdent z = "V_" ++ show z

-- The actual naming of the propagation variable
propIdent :: Char -> Node -> Node -> Ident
propIdent c z n = "Q" ++ [c] ++ show z ++ "_" ++ show n

-- Distinction between direction of propagation, for each edge
propIdentD = propIdent 'd'
propIdentOD = propIdent 'o'

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe = fmap

declareVars :: [(Node, Maybe Sign, [(Node, Sign)], [(Node, Sign)])] -> [Item]
declareVars [] = []
declareVars ((z, ms, outs, ins):rs)
  = Declare (Dec, Range (IConst $ head signDomain)
                        (IConst $ last signDomain))
            (nodeIdent z)
            (mapMaybe (IConst . signToInt) ms)
  : declareVars rs ++ declarePropVars outs z

-- Declare propagation variables for each edge
declarePropVars outs z =
  -- A propagation variable on the direction of the edge
  [Declare (Dec, Range (IConst $ head signDomain)
                       (IConst $ last signDomain))
           (propIdentD z n)
           Nothing |
    (n,_) <- outs] ++
  -- and one to the opposite direction
  [Declare (Dec, Range (IConst $ head signDomain)
                       (IConst $ last signDomain))
           (propIdentOD z n)
           Nothing |
    (n,_) <- outs]

-- Declare goal utilities
obsWeightIdent :: Node -> Ident
obsWeightIdent n = "V_" ++ show n ++ "Arr"

declareWeightArrays :: [OW] -> [Item]
declareWeightArrays gs =
  [Declare (Par, Array [Range (IConst $ head signDomain) 
                              (IConst $ last signDomain)]
                       (Par, Int))
           (obsWeightIdent (node g))
           (Just (ArrayLit [IConst $ snd p | 
                             p <- sortOn (signToInt . fst) (values g)])) |
    g <- gs]
{-
declareWeightArrays gs name =
  Declare (Par, Array [Int, Range (IConst $ head signDomain)
                                  (IConst $ last signDomain)]
                      (Par, Int))
          name
          (Just (ArrayLit2D [ [IConst $ snd p |
                                p <- sortOn (signToInt . fst) (values g)] |
                              g <- gs]))
-}

-- Utility object function for a 'solve maximize' problem
utility :: [GoalProfit] -> Item
utility gs =
  Solve (Maximize
           (foldl1' (Bi BPlus)
           (map (\obs -> ArrayElem (obsWeightIdent $ node obs) [Var $ nodeIdent (node obs)])
           gs)))
{-
utility gs =
  Solve (Maximize (GenCall 
                   mz_sum 
                  ([(["k"], SetLit $ map (IConst . node) gs)], Nothing)
                  (ArrayElem "Utilities" [Var "k", Var "k"])))
-}
-- Budget constraint
--budget :: Budget -> Item


-- Node z is observed
constraint2 :: Node -> [(Node, Sign)] -> [(Node, Sign)] -> [Item]
constraint2 z outs ins =
  [regularComb $ ArrayLit [ Var $ nodeIdent z
                          , IConst $ signToInt os
                          , Var $ propIdentD z n] |
    (n, os) <- outs] ++
  [regularComb $ ArrayLit [ Var $ nodeIdent z
                          , IConst $ signToInt os
                          , Var $ propIdentOD n z] |
    (n, os) <- ins]

-- Node z not observed
constraint1a :: Node -> [(Node, Sign)] -> [(Node, Sign)] -> [Item]
constraint1a z outs ins = constraint1a1 z outs ins ++
                          constraint1a2 z outs ins

constraint1a1 :: Node -> [(Node, Sign)] -> [(Node, Sign)] -> [Item]
constraint1a1 z [(x, s1)] [] =
  [regularComb $ ArrayLit [ IConst $ signToInt Z
                          , IConst $ signToInt s1
                          , Var $ propIdentD z x]]
constraint1a1 z outs ins     =
  [regularComb $ ArrayLit ([Var $ propIdentOD z y |
                             (y, _) <- delete (x, s1) outs] ++
                           [Var $ propIdentD y z | 
                             (y,_) <- filter (((/=) x). fst) ins] ++
                           [IConst $ signToInt s1, Var $ propIdentD z x]) | 
    (x, s1) <- outs]

constraint1a2 :: Node -> [(Node, Sign)] -> [(Node, Sign)] -> [Item]
constraint1a2 z [] ins =
  [regularComb $ ArrayLit [ IConst $ signToInt Z
                          , IConst $ signToInt s1
                          , Var $ propIdentOD x z] |
    (x, s1) <- ins]
constraint1a2 z outs ins =
  [regularComb $ ArrayLit ([Var $ propIdentOD z y | 
                             (y,_) <- outs] ++
                           [IConst $ signToInt s1
                           , Var $ propIdentOD x z]) | 
    (x, s1) <- ins]

constraint1b :: Node -> [(Node, Sign)] -> [(Node, Sign)] -> [Item]
constraint1b z outs ins = [regularPlus $ ArrayLit ([Var $ propIdentOD z x | (x,s) <- outs] ++
                                                   [Var $ propIdentD x z | (x,s) <- ins] ++
                                                   [Var $ nodeIdent z])]
                                                   
-- Other constraints
among :: [Node] -> Int -> Int -> [Sign] -> Item
among ns min max vs = Constraint $
  Bi In
     (Call (userD "among")
           [ArrayLit [Var $ nodeIdent n | n <- ns]
           ,SetLit [IConst (signToInt s) | s <- vs]]) 
     (Interval (IConst min) (IConst max))

makePost :: (Node, Maybe Sign, [(Node, Sign)], [(Node, Sign)]) -> [Item]
makePost (z, Just _, outs, ins)  = constraint2  z outs ins
makePost (z, Nothing, outs, ins) = constraint1a z outs ins ++ constraint1b z outs ins

-- Model mode
makeSimpleModel :: [(Node, Maybe Sign, [(Node, Sign)], [(Node, Sign)])] -> MZModel
makeSimpleModel cld@(l:ls) = [includeRegular, Empty] ++
                       declareVars cld ++
                       [Empty, transitionPlus, Empty, transitionComb, Empty] ++
                       concatMap makePost cld ++ [Empty, Solve Satisfy]

iSolveCLD cld = iTestModel $ makeSimpleModel (getNodeContexts cld)
solveCLD cld =  testModel (makeSimpleModel (getNodeContexts cld))
iSolveCLDwithPath cld = do
  putStrLn "Minizinc filepath:"
  p <- getLine
  testModel (makeSimpleModel (getNodeContexts cld)) p 1 10