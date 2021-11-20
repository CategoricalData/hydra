module Hydra.Impl.Haskell.Lib.Strings (
  stringPrimitives,
  stringsFunc,
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl

import qualified Data.Char as C
import qualified Data.List as L


stringPrimitives :: (Default a, Show a) => [PrimitiveFunction a]
stringPrimitives = [
  prim "cat" stringType (functionType stringType stringType)
    $ withTwoStrings $ \x y -> stringTerm $ x ++ y,
  prim "length" stringType stringType
    $ withString $ int32Value . L.length,
  prim "toLower" stringType stringType
    $ withString $ stringValue . fmap C.toLower,
  prim "toUpper" stringType stringType
    $ withString $ stringValue . fmap C.toUpper]

stringsFunc :: String -> Name
stringsFunc local = "hydra/lib/strings." ++ local

prim :: String -> Type -> Type -> ([Term a] -> Result (Term a)) -> PrimitiveFunction a
prim nm dom cod impl = PrimitiveFunction {
  primitiveFunctionName = stringsFunc nm,
  primitiveFunctionType = FunctionType dom cod,
  primitiveFunctionImplementation  = impl}

withString :: Show a => (String -> Term a) -> [Term a] -> Result (Term a)
withString func args = do
  expectNArgs 1 args
  func <$> expectStringTerm (L.head args)

withTwoStrings :: Show a => (String -> String -> Term a) -> [Term a] -> Result (Term a)
withTwoStrings func args = do
  expectNArgs 2 args
  func <$> expectStringTerm (L.head args) <*> expectStringTerm (args !! 1)
