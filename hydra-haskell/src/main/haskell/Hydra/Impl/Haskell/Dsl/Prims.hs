module Hydra.Impl.Haskell.Dsl.Prims where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Impl.Haskell.Extras

import qualified Data.List as L


int32Input :: Show m => InputSpec Int m
int32Input = InputSpec int32Type expectInt32

int32Output :: Default m => OutputSpec Int m
int32Output = OutputSpec int32Type int32Value

prim1 :: Name -> InputSpec a1 m -> OutputSpec b m -> (a1 -> b) -> PrimitiveFunction m
prim1 name input1 output compute = PrimitiveFunction name ft impl
  where
    ft = FunctionType (inputSpecType input1) (outputSpecType output)
    impl args = do
      expectNArgs 1 args
      arg1 <- inputSpecUnmarshal input1 $ L.head args
      return $ outputSpecMarshal output $ compute arg1

prim2 :: Name -> InputSpec a1 m -> InputSpec a2 m -> OutputSpec b m -> (a1 -> a2 -> b) -> PrimitiveFunction m
prim2 name input1 input2 output compute = PrimitiveFunction name ft impl
  where
    ft = FunctionType (inputSpecType input1) (functionType (inputSpecType input2) $ outputSpecType output)
    impl args = do
      expectNArgs 2 args
      arg1 <- inputSpecUnmarshal input1 $ L.head args
      arg2 <- inputSpecUnmarshal input2 $ args !! 1
      return $ outputSpecMarshal output $ compute arg1 arg2

stringInput :: Show m => InputSpec String m
stringInput = InputSpec stringType expectString

stringListOutput :: Default m => OutputSpec [String] m
stringListOutput = OutputSpec (listType stringType) stringList

stringOutput :: Default m => OutputSpec String m
stringOutput = OutputSpec stringType stringValue
