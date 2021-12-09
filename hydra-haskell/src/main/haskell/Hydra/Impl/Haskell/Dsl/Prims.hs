module Hydra.Impl.Haskell.Dsl.Prims where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Impl.Haskell.Extras

import qualified Data.List as L
import qualified Data.Set as S


booleanOutput :: Default m => OutputSpec Bool m
booleanOutput = OutputSpec booleanType booleanValue

int32Input :: Show m => InputSpec Int m
int32Input = InputSpec int32Type expectInt32

int32Output :: Default m => OutputSpec Int m
int32Output = OutputSpec int32Type int32Value

listInput :: Show m => (Term m -> Result a) -> TypeVariable -> InputSpec [a] m
listInput f v = InputSpec (listType $ typeVariable v) (expectList f)

listInputPoly :: Show m => TypeVariable -> InputSpec [Term m] m
listInputPoly v = InputSpec (listType $ typeVariable v) expectListPoly

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

setInput :: (Ord a, Show m) => (Term m -> Result a) -> TypeVariable -> InputSpec (S.Set a) m
setInput f v = InputSpec (setType $ typeVariable v) (expectSet f)

setOutput :: (Default m, Ord m) => (a -> Term m) -> TypeVariable -> OutputSpec (S.Set a) m
setOutput f v = OutputSpec (setType $ typeVariable v) (\s -> set (S.fromList (f <$> S.toList s)))

stringInput :: Show m => InputSpec String m
stringInput = InputSpec stringType expectString

stringListOutput :: Default m => OutputSpec [String] m
stringListOutput = OutputSpec (listType stringType) stringList

stringOutput :: Default m => OutputSpec String m
stringOutput = OutputSpec stringType stringValue
