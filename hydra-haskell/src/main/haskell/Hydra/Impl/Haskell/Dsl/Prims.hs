module Hydra.Impl.Haskell.Dsl.Prims where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.Terms
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Extras

import qualified Data.List as L
import qualified Data.Set as S


booleanOutput :: OutputSpec Bool m
booleanOutput = OutputSpec Types.boolean boolean

inputPoly :: String -> InputSpec (Term m) m
inputPoly v = InputSpec (Types.variable v) pure

int32Input :: (Show m) => Context m -> InputSpec Int m
int32Input cx = InputSpec Types.int32 (expectInt32 cx)

int32Output :: OutputSpec Int m
int32Output = OutputSpec Types.int32 int32

listInput :: (Show m) => Context m -> Type m -> (Term m -> Result a) -> InputSpec [a] m
listInput cx lt f = InputSpec (Types.list lt) (expectList cx f)

listOutput :: Type m -> (a -> Term m) -> OutputSpec [a] m
listOutput lt f = OutputSpec (Types.list lt) $ \l -> list (f <$> l)

listInputPoly :: (Show m) => Context m -> String -> InputSpec [Term m] m
listInputPoly cx v = InputSpec (Types.list $ Types.variable v) (expectListPoly cx)

listOutputPoly :: String -> OutputSpec [Term m] m
listOutputPoly v = OutputSpec (Types.list $ Types.variable v) list

outputPoly :: String -> OutputSpec (Term m) m
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

setInput :: (Ord a, Show m) => Context m -> (Term m -> Result a) -> String -> InputSpec (S.Set a) m
setInput cx f v = InputSpec (Types.set $ Types.variable v) (expectSet cx f)

setOutput :: (Ord m) => (a -> Term m) -> String -> OutputSpec (S.Set a) m
setOutput f v = OutputSpec (Types.set $ Types.variable v) (\s -> set (S.fromList (f <$> S.toList s)))

stringInput :: (Show m) => Context m -> InputSpec String m
stringInput cx = InputSpec Types.string (expectString cx)

stringListOutput :: OutputSpec [String] m
stringListOutput = OutputSpec (Types.list Types.string) stringList

stringOutput :: OutputSpec String m
stringOutput = OutputSpec Types.string string
