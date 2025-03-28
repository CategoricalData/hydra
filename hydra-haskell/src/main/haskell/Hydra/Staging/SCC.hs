-- This implementation of Tarjan's algorithm is adapted from GraphSCC by Iavor S. Diatchki:
--   https://hackage.haskell.org/package/GraphSCC

module Hydra.Staging.SCC(
  adjacencyListsToGraph,
  stronglyConnectedComponents,
) where

import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.Maybe as Y
import qualified Control.Monad.ST as ST
import qualified Data.Array.ST as AST
import qualified Control.Monad as CM


type Vertex = Int
type Graph = A.Array Vertex [Vertex]

-- | Compute the list of strongly connected components of a graph.
-- The components are topologically sorted:
-- if v1 in C1 points to v2 in C2, then C2 will come before C1 in the list.
stronglyConnectedComponents :: Graph -> [[Vertex]]
stronglyConnectedComponents g = reverse $ fmap (L.sort . snd) scc
  where
    scc = ST.runST $ do
      ixes <- AST.newArray (A.bounds g) 0
      lows <- AST.newArray (A.bounds g) 0
      s <- roots g ixes lows (S [] 1 [] 1) (A.indices g)
      return $ sccs s

--------------------------------------------------------------------------------

adjacencyListsToGraph :: Ord key
  => [(key, [key])]
  -> (Graph, Vertex -> key)
adjacencyListsToGraph edges0
  = (graph, \v -> fst  $ vertex_map A.! v)
  where
    max_v           = length edges0 - 1
    bounds0         = (0, max_v) :: (Vertex, Vertex)
    sorted_edges    = L.sortBy lt edges0
    edges1          = zipWith (,) [0..] sorted_edges

    graph           = A.array bounds0 [(,) v (Y.mapMaybe key_vertex ks) | (,) v (_, ks) <- edges1]
    key_map         = A.array bounds0 [(,) v k                       | (,) v (k, _ ) <- edges1]
    vertex_map      = A.array bounds0 edges1

    (k1,_) `lt` (k2,_) = k1 `compare` k2

    key_vertex k   = findVertex 0 max_v
       where
         findVertex a b | a > b
                  = Nothing
         findVertex a b = case compare k (key_map A.! mid) of
                       LT -> findVertex a (mid-1)
                       EQ -> Just mid
                       GT -> findVertex (mid+1) b
                  where
                    mid = a + (b - a) `div` 2

--------------------------------------------------------------------------------

type Func s a =
     Graph                    -- The original graph
  -> AST.STUArray s Vertex Int    -- Index in DFS traversal, or SCC for vertex.
    -- Legend for the index array:
    --    0:    Node not visited
    --    -ve:  Node is on the stack with the given number
    --    +ve:  Node belongs to the SCC with the given number
  -> AST.STUArray s Vertex Int    -- Least reachable node
  -> S                        -- State
  -> a

data S = S { stack    :: ![Vertex]          -- ^ Traversal stack
           , num      :: !Int               -- ^ Next node number
           , sccs     :: ![(Int,[Vertex])]  -- ^ Finished SCCs
           , next_scc :: !Int               -- ^ Next SCC number
           }

roots :: Func s ([Vertex] -> ST.ST s S)
roots g ixes lows st (v:vs) =
  do i <- AST.readArray ixes v
     if i == 0 then do s1 <- from_root g ixes lows st v
                       roots g ixes lows s1 vs
               else roots g ixes lows st vs
roots _ _ _ s [] = return s

from_root :: Func s (Vertex -> ST.ST s S)
from_root g ixes lows s v =
  do let me = num s
     AST.writeArray ixes v (negate me)
     AST.writeArray lows v me
     newS <- check_adj g ixes lows
                        s { stack = v : stack s, num = me + 1 } v (g A.! v)

     x <- AST.readArray lows v
     if x < me then return newS else
       case span (/= v) (stack newS) of
         (as,b:bs) ->
           do let this = b : as
                  n = next_scc newS
              mapM_ (\i -> AST.writeArray ixes i n) this
              return S { stack = bs
                       , num = num newS
                       , sccs = (n,this) : sccs newS
                       , next_scc = n + 1
                       }
         _ -> error ("bug in scc---vertex not on the stack: " ++ show v)

check_adj :: Func s (Vertex -> [Vertex] -> ST.ST s S)
check_adj g ixes lows st v (v':vs) =
  do i <- AST.readArray ixes v'
     case () of
       _ | i == 0 ->
             do newS <- from_root g ixes lows st v'
                new_low <- min `fmap` AST.readArray lows v `CM.ap` AST.readArray lows v'
                AST.writeArray lows v new_low
                check_adj g ixes lows newS v vs
         | i < 0 ->
             do j <- AST.readArray lows v
                AST.writeArray lows v (min j (negate i))
                check_adj g ixes lows st v vs
         | otherwise -> check_adj g ixes lows st v vs
check_adj _ _ _ st _ [] = return st


