module DSL.SolverExports (
    -- Types
    CLD,
    Node,
    Sign (..),

    -- Utility functions
    getNodeContexts,
    --cld2Graph
) where
import Data.Graph.Inductive.Graph
import DSL.ConstraintDSL
import DSL.GraphDSL
import Data.Graph as G
import Data.Array

-- | Get the observed sign for each node
--getNodeContexts :: GraphSyntax a -> [(Node, Maybe Sign, [(Node, Sign)], [(Node, Sign)])]
getNodeContexts g = map (\n -> (n, getObservedSign (constraints cld) n, outs n, ins n)) $ nodes (graph cld)
    where
        cld    = compile g
        gr     = graph $ cld
        outs n = map (\(a, b, c) -> (b, c)) $ out gr n
        ins  n = map (\(a, b, c) -> (a, c)) $ inn gr n

-- Get the observed codomain
getObservedSign :: [Constraint] -> Node -> Maybe Sign
getObservedSign xs x = safeHead [c | Equality v (S c) <- xs, v == x]

-- Safe head of a list
safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

-- Convert a CLD to a G.Graph
-- To be passed to Tarjan's algorithm

--cld2Graph :: GraphSyntax a -> G.Graph
-- cld2Graph g = let cld = graph $ compile g
                  -- inter = map (\n -> (n, out cld n)) $ nodes cld
                  -- outs (_, ls) = map (\(a, b, c) -> b) ls
              -- in listArray (0,length inter - 1) (map outs inter)
              