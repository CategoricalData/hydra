module Hydra.Impl.Haskell.Dsl.Prims where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.Terms

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
  toTerm <$> (func <$> expectInt32Term (L.head args))

withString :: Show a => (b -> Term a ) -> (String -> b) -> [Term a] -> Result (Term a)
withString toTerm func args = do
  expectNArgs 1 args
  toTerm <$> (func <$> expectStringTerm (L.head args))

withTwoInt32s :: Show a => (b -> Term a) -> (Int -> Int -> b) -> [Term a] -> Result (Term a)
withTwoInt32s toTerm func args = do
  expectNArgs 2 args
  toTerm <$> (func <$> expectInt32Term (L.head args) <*> expectInt32Term (args !! 1))

withTwoStrings :: Show a => (b -> Term a) -> (String -> String -> b) -> [Term a] -> Result (Term a)
withTwoStrings toTerm func args = do
  expectNArgs 2 args
  toTerm <$> (func <$> expectStringTerm (args !! 0) <*> expectStringTerm (args !! 1))
