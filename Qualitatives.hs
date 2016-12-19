module Qualitatives where

data Sign = SM' | M' | MQ' | Z' | PQ' | P' | SP' | Q' 
          -- | Cond (Bool -> Sign)
  deriving (Show, Eq, Enum, Bounded)

signDomain :: [Sign]
signDomain = [minBound .. maxBound]
  
stateInt :: Sign -> Int
stateInt = (+ 1) . fromEnum

type Degree = Int

{-
  Encodes the two basic comparison operators used in the cases described in 'Ehancing 
  QPNs for trade-off resolution', figure 9.
-}
data DegreeCond = LTE | GTE

{-
  1st component: the Sign returned if the degree conditional is true
  2nd component: the Sign returned if the degree conditional is false
-}
type CondResult = (Sign, Sign)

type CondSign = Either (DegreeCond, CondResult) Sign

posPred :: Bool -> Sign
posPred True  = PQ'
posPred False = Q'

negPred :: Bool -> Sign
negPred True  = MQ'
negPred False = Q'

-- Sign addition operation
add :: Sign -> Sign -> CondSign
add Z'  a   = Right a
add a   Z'  = Right a
add Q'  a   = Right Q'
add a   Q'  = Right Q'
add SP' SP' = Right SP' -- (min i j)
add SP' P'  = Right SP' --i
add SP' PQ' = Right SP' --i
add SP' MQ' = Right Q'
add SP' M'  = Left (LTE, (PQ', Q')) -- (\i j -> posPred (i <= j))
add SP' SM' = Right Q'
add P'  SP' = Right SP' --j
add P'  P'  = Right PQ'
add P'  PQ' = Right PQ'
add P'  MQ' = Right Q'
add P'  M'  = Right Q'
add P'  SM' = Left (GTE, (MQ', Q')) -- (\i j -> negPred (j <= i))
add PQ' SP' = Right SP' -- j
add PQ' P'  = Right PQ'
add PQ' PQ' = Right PQ'
add PQ' MQ' = Right Q'
add PQ' M'  = Right Q'
add PQ' SM' = Right Q'
add MQ' SP' = Right Q'
add MQ' P'  = Right Q'
add MQ' PQ' = Right Q'
add MQ' MQ' = Right MQ'
add MQ' M'  = Right MQ'
add MQ' SM' = Right SM' -- j
add M'  SP' = Left (GTE, (PQ', Q')) -- (\i j -> posPred (j <= i))
add M'  P'  = Right Q'
add M'  PQ' = Right Q'
add M'  MQ' = Right MQ'
add M'  M'  = Right MQ'
add M'  SM' = Right SM' -- j
add SM' SP' = Right Q'
add SM' P'  = Left (LTE, (MQ', Q')) -- (\i j -> negPred (i <= j))
add SM' PQ' = Right Q'
add SM' MQ' = Right SM' -- i
add SM' M'  = Right SM' -- i

-- type Value = (Sign, Degree)

data EnhVal
  = SM Degree
  | M Degree
  | MQ
  | Z
  | PQ
  | P Degree
  | SP Degree
  | Q
  deriving (Show, Eq)

-- Definition of addition operation over enhanced values
addS :: EnhVal -> EnhVal -> EnhVal
addS (Z)    (a)    = a
addS (a)    (Z)    = a
addS (Q)    (a)    = Q
addS (a)    (Q)    = Q
addS (SP i) (SP j) = SP (min i j)
addS (SP i) (P j)  = SP i
addS (SP i) (PQ)   = SP i
addS (SP i) (MQ)   = Q
addS (SP i) (M j)
  | i <= j        = PQ
  | otherwise     = Q
addS (SP i) (SM j) = Q
addS (P i)  (SP j) = SP j
addS (P i)  (P j)  = PQ
addS (P i)  (PQ)   = PQ
addS (P i)  (MQ)   = Q
addS (P i)  (M j)  = Q
addS (P i)  (SM j)
  | j <= i        = MQ
  | otherwise     = Q
addS (PQ)   (SP j) = SP j
addS (PQ)   (P j)  = PQ
addS (PQ)   (PQ)   = PQ
addS (PQ)   (MQ)   = Q
addS (PQ)   (M j)  = Q
addS (PQ)   (SM j) = Q
addS (MQ)   (SP j) = Q
addS (MQ)   (P j)  = Q
addS (MQ)   (PQ)   = Q
addS (MQ)   (MQ)   = MQ
addS (MQ)   (M j)  = MQ
addS (MQ)   (SM j) = SM j
addS (M i)  (SP j)
  | j <= i        = PQ
  | otherwise     = Q
addS (M i)  (P j)  = Q
addS (M i)  (PQ)   = Q
addS (M i)  (MQ)   = MQ
addS (M i)  (M j)  = MQ
addS (M i)  (SM j) = SM j
addS (SM i) (SP j) = Q
addS (SM i) (P j)
  | i <= j        = MQ
  | otherwise     = Q
addS (SM i) (PQ)   = Q
addS (SM i) (MQ)   = SM i
addS (SM i) (M j)  = SM i
addS (SM i) (SM j) = SM (min i j)