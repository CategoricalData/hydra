module Hydra.Impl.Haskell.Dsl.Prims where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.Terms
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Extras

import qualified Data.List as L
import qualified Data.Set as S


booleanOutput :: Default m => OutputSpec Bool m
booleanOutput = OutputSpec Types.boolean booleanValue

inputPoly :: TypeVariable -> InputSpec (Term m) m
inputPoly v = InputSpec (Types.variable v) pure

int32Input :: Show m => InputSpec Int m
int32Input = InputSpec Types.int32 expectInt32

int32Output :: Default m => OutputSpec Int m
int32Output = OutputSpec Types.int32 int32Value

listInput :: Show m => Type -> (Term m -> Result a) -> InputSpec [a] m
listInput lt f = InputSpec (Types.list lt) (expectList f)

listOutput :: Default m => Type -> (a -> Term m) -> OutputSpec [a] m
listOutput lt f = OutputSpec (Types.list lt) $ \l -> list (f <$> l)

listInputPoly :: Show m => TypeVariable -> InputSpec [Term m] m
listInputPoly v = InputSpec (Types.list $ Types.variable v) expectListPoly

listOutputPoly :: Default m => TypeVariable -> OutputSpec [Term m] m
listOutputPoly v = OutputSpec (Types.list $ Types.variable v) list

outputPoly :: TypeVariable -> OutputSpec (Term m) m
outputPoly v = OutputSpec (Types.variable v) id

prim1 :: Name -> InputSpec a m -> OutputSpec b m -> (a -> b) -> PrimitiveFunction m
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
    ft = FunctionType (inputSpecType input1) (Types.function (inputSpecType input2) $ outputSpecType output)
    impl args = do
      expectNArgs 2 args
      arg1 <- inputSpecUnmarshal input1 $ L.head args
      arg2 <- inputSpecUnmarshal input2 $ args !! 1
      return $ outputSpecMarshal output $ compute arg1 arg2

setInput :: (Ord a, Show m) => (Term m -> Result a) -> TypeVariable -> InputSpec (S.Set a) m
setInput f v = InputSpec (Types.set $ Types.variable v) (expectSet f)

setOutput :: (Default m, Ord m) => (a -> Term m) -> TypeVariable -> OutputSpec (S.Set a) m
setOutput f v = OutputSpec (Types.set $ Types.variable v) (\s -> set (S.fromList (f <$> S.toList s)))

stringInput :: Show m => InputSpec String m
stringInput = InputSpec Types.string expectString

stringListOutput :: Default m => OutputSpec [String] m
stringListOutput = OutputSpec (Types.list Types.string) stringList

stringOutput :: Default m => OutputSpec String m
stringOutput = OutputSpec Types.string stringValue
