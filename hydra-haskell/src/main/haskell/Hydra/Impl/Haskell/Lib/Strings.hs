module Hydra.Impl.Haskell.Lib.Strings (
  stringPrimitives,
  stringsFunc,
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl

import qualified Data.Char as C
import qualified Data.List as L


stringPrimitives :: [PrimitiveFunction]
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

prim :: String -> Type -> Type -> ([Term] -> Result Term) -> PrimitiveFunction
prim nm dom cod impl = PrimitiveFunction {
  primitiveFunctionName = stringsFunc nm,
  primitiveFunctionType = FunctionType dom cod,
  primitiveFunctionImplementation  = impl}
 
withString :: (String -> Term) -> [Term] -> Result Term
withString func args = do
  expectNArgs 1 args
  func <$> expectStringTerm (L.head args)

withTwoStrings :: (String -> String -> Term) -> [Term] -> Result Term
withTwoStrings func args = do
  expectNArgs 2 args
  func <$> expectStringTerm (L.head args) <*> expectStringTerm (args !! 1)
