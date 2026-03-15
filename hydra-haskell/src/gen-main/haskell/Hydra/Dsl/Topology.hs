-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.topology

module Hydra.Dsl.Topology where

import qualified Hydra.Topology as Topology
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

orderingIsomorphism :: (([t0] -> [t0]) -> ([t0] -> [t0]) -> Topology.OrderingIsomorphism t0)
orderingIsomorphism encode decode = Topology.OrderingIsomorphism {
  Topology.orderingIsomorphismEncode = encode,
  Topology.orderingIsomorphismDecode = decode}

orderingIsomorphismEncode :: (Topology.OrderingIsomorphism t0 -> [t0] -> [t0])
orderingIsomorphismEncode = Topology.orderingIsomorphismEncode

orderingIsomorphismDecode :: (Topology.OrderingIsomorphism t0 -> [t0] -> [t0])
orderingIsomorphismDecode = Topology.orderingIsomorphismDecode

orderingIsomorphismWithEncode :: (Topology.OrderingIsomorphism t0 -> ([t0] -> [t0]) -> Topology.OrderingIsomorphism t0)
orderingIsomorphismWithEncode original newVal = Topology.OrderingIsomorphism {
  Topology.orderingIsomorphismEncode = newVal,
  Topology.orderingIsomorphismDecode = (Topology.orderingIsomorphismDecode original)}

orderingIsomorphismWithDecode :: (Topology.OrderingIsomorphism t0 -> ([t0] -> [t0]) -> Topology.OrderingIsomorphism t0)
orderingIsomorphismWithDecode original newVal = Topology.OrderingIsomorphism {
  Topology.orderingIsomorphismEncode = (Topology.orderingIsomorphismEncode original),
  Topology.orderingIsomorphismDecode = newVal}

tarjanState :: (Int -> M.Map Topology.Vertex Int -> M.Map Topology.Vertex Int -> [Topology.Vertex] -> S.Set Topology.Vertex -> [[Topology.Vertex]] -> Topology.TarjanState)
tarjanState counter indices lowLinks stack onStack sccs = Topology.TarjanState {
  Topology.tarjanStateCounter = counter,
  Topology.tarjanStateIndices = indices,
  Topology.tarjanStateLowLinks = lowLinks,
  Topology.tarjanStateStack = stack,
  Topology.tarjanStateOnStack = onStack,
  Topology.tarjanStateSccs = sccs}

tarjanStateCounter :: (Topology.TarjanState -> Int)
tarjanStateCounter = Topology.tarjanStateCounter

tarjanStateIndices :: (Topology.TarjanState -> M.Map Topology.Vertex Int)
tarjanStateIndices = Topology.tarjanStateIndices

tarjanStateLowLinks :: (Topology.TarjanState -> M.Map Topology.Vertex Int)
tarjanStateLowLinks = Topology.tarjanStateLowLinks

tarjanStateStack :: (Topology.TarjanState -> [Topology.Vertex])
tarjanStateStack = Topology.tarjanStateStack

tarjanStateOnStack :: (Topology.TarjanState -> S.Set Topology.Vertex)
tarjanStateOnStack = Topology.tarjanStateOnStack

tarjanStateSccs :: (Topology.TarjanState -> [[Topology.Vertex]])
tarjanStateSccs = Topology.tarjanStateSccs

tarjanStateWithCounter :: (Topology.TarjanState -> Int -> Topology.TarjanState)
tarjanStateWithCounter original newVal = Topology.TarjanState {
  Topology.tarjanStateCounter = newVal,
  Topology.tarjanStateIndices = (Topology.tarjanStateIndices original),
  Topology.tarjanStateLowLinks = (Topology.tarjanStateLowLinks original),
  Topology.tarjanStateStack = (Topology.tarjanStateStack original),
  Topology.tarjanStateOnStack = (Topology.tarjanStateOnStack original),
  Topology.tarjanStateSccs = (Topology.tarjanStateSccs original)}

tarjanStateWithIndices :: (Topology.TarjanState -> M.Map Topology.Vertex Int -> Topology.TarjanState)
tarjanStateWithIndices original newVal = Topology.TarjanState {
  Topology.tarjanStateCounter = (Topology.tarjanStateCounter original),
  Topology.tarjanStateIndices = newVal,
  Topology.tarjanStateLowLinks = (Topology.tarjanStateLowLinks original),
  Topology.tarjanStateStack = (Topology.tarjanStateStack original),
  Topology.tarjanStateOnStack = (Topology.tarjanStateOnStack original),
  Topology.tarjanStateSccs = (Topology.tarjanStateSccs original)}

tarjanStateWithLowLinks :: (Topology.TarjanState -> M.Map Topology.Vertex Int -> Topology.TarjanState)
tarjanStateWithLowLinks original newVal = Topology.TarjanState {
  Topology.tarjanStateCounter = (Topology.tarjanStateCounter original),
  Topology.tarjanStateIndices = (Topology.tarjanStateIndices original),
  Topology.tarjanStateLowLinks = newVal,
  Topology.tarjanStateStack = (Topology.tarjanStateStack original),
  Topology.tarjanStateOnStack = (Topology.tarjanStateOnStack original),
  Topology.tarjanStateSccs = (Topology.tarjanStateSccs original)}

tarjanStateWithStack :: (Topology.TarjanState -> [Topology.Vertex] -> Topology.TarjanState)
tarjanStateWithStack original newVal = Topology.TarjanState {
  Topology.tarjanStateCounter = (Topology.tarjanStateCounter original),
  Topology.tarjanStateIndices = (Topology.tarjanStateIndices original),
  Topology.tarjanStateLowLinks = (Topology.tarjanStateLowLinks original),
  Topology.tarjanStateStack = newVal,
  Topology.tarjanStateOnStack = (Topology.tarjanStateOnStack original),
  Topology.tarjanStateSccs = (Topology.tarjanStateSccs original)}

tarjanStateWithOnStack :: (Topology.TarjanState -> S.Set Topology.Vertex -> Topology.TarjanState)
tarjanStateWithOnStack original newVal = Topology.TarjanState {
  Topology.tarjanStateCounter = (Topology.tarjanStateCounter original),
  Topology.tarjanStateIndices = (Topology.tarjanStateIndices original),
  Topology.tarjanStateLowLinks = (Topology.tarjanStateLowLinks original),
  Topology.tarjanStateStack = (Topology.tarjanStateStack original),
  Topology.tarjanStateOnStack = newVal,
  Topology.tarjanStateSccs = (Topology.tarjanStateSccs original)}

tarjanStateWithSccs :: (Topology.TarjanState -> [[Topology.Vertex]] -> Topology.TarjanState)
tarjanStateWithSccs original newVal = Topology.TarjanState {
  Topology.tarjanStateCounter = (Topology.tarjanStateCounter original),
  Topology.tarjanStateIndices = (Topology.tarjanStateIndices original),
  Topology.tarjanStateLowLinks = (Topology.tarjanStateLowLinks original),
  Topology.tarjanStateStack = (Topology.tarjanStateStack original),
  Topology.tarjanStateOnStack = (Topology.tarjanStateOnStack original),
  Topology.tarjanStateSccs = newVal}
