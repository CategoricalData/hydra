module Hydra.Impl.Haskell.Dsl.Prims where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Impl.Haskell.Extras

import qualified Data.List as L


prim :: String -> String -> [Type] -> ([Term a] -> Result (Term a)) -> PrimitiveFunction a
prim lib nm types impl = PrimitiveFunction {
    primitiveFunctionName = lib ++ "." ++ nm,
    primitiveFunctionType = ftype types,
    primitiveFunctionImplementation  = impl}
  where
    ftype types = case types of
      [dom, cod] -> FunctionType dom cod
      (h:r) -> FunctionType h (TypeFunction $ ftype r)

withInt32 :: Show a => (b -> Term a) -> (Int -> b) -> [Term a] -> Result (Term a)
withInt32 toTerm func args = do
  expectNArgs 1 args
  toTerm <$> (func <$> expectInt32 (L.head args))

withString :: Show a => (b -> Term a ) -> (String -> b) -> [Term a] -> Result (Term a)
withString toTerm func args = do
  expectNArgs 1 args
  toTerm <$> (func <$> expectString (L.head args))

withTwoInt32s :: Show a => (b -> Term a) -> (Int -> Int -> b) -> [Term a] -> Result (Term a)
withTwoInt32s toTerm func args = do
  expectNArgs 2 args
  toTerm <$> (func <$> expectInt32 (L.head args) <*> expectInt32 (args !! 1))

withTwoStrings :: Show a => (b -> Term a) -> (String -> String -> b) -> [Term a] -> Result (Term a)
withTwoStrings toTerm func args = do
  expectNArgs 2 args
  toTerm <$> (func <$> expectString (args !! 0) <*> expectString (args !! 1))


int32Input :: Show m => InputSpec Int m
int32Input = InputSpec int32Type expectInt32

int32Output :: Default m => OutputSpec Int m
int32Output = OutputSpec int32Type int32Value

stringInput :: Show m => InputSpec String m
stringInput = InputSpec stringType expectString

stringListOutput :: Default m => OutputSpec [String] m
stringListOutput = OutputSpec (listType stringType) stringList

stringOutput :: Default m => OutputSpec String m
stringOutput = OutputSpec stringType stringValue


prim2 :: Name -> String -> InputSpec a1 m -> InputSpec a2 m -> OutputSpec b m -> (a1 -> a2 -> Result b) -> PrimitiveFunction m
prim2 ns name input1 input2 output compute = PrimitiveFunction (ns ++ "." ++ name) ft impl
  where
    ft = FunctionType (inputSpecType input1) (functionType (inputSpecType input2) $ outputSpecType output)
    impl args = do
      expectNArgs 2 args
      arg1 <- inputSpecUnmarshal input1 $ L.head args
      arg2 <- inputSpecUnmarshal input2 $ args !! 1
      outputSpecMarshal output <$> compute arg1 arg2

prim1 :: Name -> String -> InputSpec a1 m -> OutputSpec b m -> (a1 -> Result b) -> PrimitiveFunction m
prim1 ns name input1 output compute = PrimitiveFunction (ns ++ "." ++ name) ft impl
  where
    ft = FunctionType (inputSpecType input1) (outputSpecType output)
    impl args = do
      expectNArgs 1 args
      arg1 <- inputSpecUnmarshal input1 $ L.head args
      outputSpecMarshal output <$> compute arg1
