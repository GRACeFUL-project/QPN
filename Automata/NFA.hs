module Automata.NFA where

import Data.List

data NFA st l = NFA { states :: [st]
                    , lang   :: [l]
                    , delta  :: st -> (Maybe l) -> [st]
                    , initSt :: st
                    , inF    :: st -> Bool
                    }

type Delta st l = st -> (Maybe l) -> [st]

deltaStar :: Delta st l -> [st] -> [l] -> [st]
deltaStar delta qs [] = qs
deltaStar delta qs (v:vs) = deltaStar delta (concatMap (\q -> evalStep delta q v) qs) vs

evalNFA :: NFA st l -> [l] -> Bool
evalNFA nfa input = any (inF nfa) (deltaStar (delta nfa) [initSt nfa] input)

evalStep :: Delta st l -> st -> l -> [st]
evalStep delta s c = (delta s Nothing) ++ (delta s (Just c))

-- |Closure under star. Compare to the proof of Theorem 1.50
starNFA :: NFA st l -> NFA (Either () st) l
starNFA nfa =
  let q0 = Left ()
      qs1 = [q0] ++ (map Right (states nfa))
      inF1 (Left ()) = True
      inF1 (Right q) = inF nfa q
      delta1 (Left ()) Nothing  = [Right (initSt nfa)]              -- eps transition from the new start state to the old start state
      delta1 (Right q) Nothing | (inF nfa q) = map Right (delta nfa q Nothing ++ [initSt nfa]) 
      delta1 (Right q) x = map Right (delta nfa q x)       -- otherswise, just like in original NFA
      delta1 _ _ = []
  in NFA qs1 (lang nfa) delta1 q0 inF1

-- |Closure under concatenation. Compare to proof of Theorem 1.47 in the textbook
concatNFA :: Eq l => NFA st1 l -> NFA st2 l -> NFA (Either st1 st2) l
concatNFA nfa1 nfa2 =
  let qs = (map Left (states nfa1)) ++ (map Right (states nfa2))
      q0 = Left (initSt nfa1)
      sigma = nub (lang nfa1 ++ lang nfa2) -- union of the alphabets. nub removes duplicates
      inF_ (Left  r) = False
      inF_ (Right r) = inF nfa2 r
      delta_ (Right q) x = map Right (delta nfa2 q x)
      delta_ (Left  q) Nothing | inF nfa1 q = (map Left (delta nfa1 q Nothing)) ++ [Right (initSt nfa2)]
      delta_ (Left  q) x = map Left (delta nfa1 q x) -- for all other q and x
  in NFA qs sigma delta_ q0 inF_