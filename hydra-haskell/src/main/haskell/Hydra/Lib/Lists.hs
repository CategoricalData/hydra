module Hydra.Lib.Lists where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Extras
import Hydra.Impl.Haskell.Dsl.Prims
import Hydra.Impl.Haskell.Dsl.Terms

import qualified Data.List as L


_hydra_lib_lists :: Name
_hydra_lib_lists = "hydra/lib/lists"

_lists_concat :: Name
_lists_concat = qname _hydra_lib_lists "concat"

_lists_length :: Name
_lists_length = qname _hydra_lib_lists "length"

_lists_map :: Name
_lists_map = qname _hydra_lib_lists "map"

hsConcat :: [[a]] -> [a]
hsConcat = L.concat

hsLength :: [a] -> Int
hsLength = L.length

hsMap :: (a -> b) -> [a] -> [b]
hsMap = fmap

hydraLibListsPrimitives :: (Default m, Show m) => [PrimitiveFunction m]
hydraLibListsPrimitives = [
  prim1 _lists_concat (listInput (typeVariable "a") expectListPoly) (listOutputPoly "a") hsConcat,
  prim1 _lists_length (listInputPoly "a") int32Output hsLength
--  ,
--  PrimitiveFunction _lists_map
--    (FunctionType
--      (functionType (typeVariable "a") (typeVariable "b"))
--      (functionType (listType $ typeVariable "a") (listType $ typeVariable "b")))
--    $ \args -> do
--      expectNArgs 2 args
--      a1 <- expectString $ L.head args
--      a2 <- expectListPoly $ args !! 1
--      
  
  ]
