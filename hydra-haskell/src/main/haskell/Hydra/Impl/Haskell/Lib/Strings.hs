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
  stringsCat,
  stringsLength,
  stringsToLower,
  stringsToUpper]

stringsFunc :: String -> Name
stringsFunc local = "hydra/lib/strings." ++ local

stringsCat :: PrimitiveFunction
stringsCat = PrimitiveFunction {
    primitiveFunctionName = stringsFunc "cat",
    primitiveFunctionType = FunctionType stringType (TypeFunction $ FunctionType stringType stringType),
    primitiveFunctionImplementation = withTwoStrings $ \x y -> stringTerm $ x ++ y
  }

stringsLength :: PrimitiveFunction
stringsLength = PrimitiveFunction {
    primitiveFunctionName = stringsFunc "length",
    primitiveFunctionType = FunctionType stringType int32Type,
    primitiveFunctionImplementation = withString $ int32Value . L.length
  }

stringsToLower :: PrimitiveFunction
stringsToLower = PrimitiveFunction {
    primitiveFunctionName = stringsFunc "toLower",
    primitiveFunctionType = FunctionType stringType stringType,
    primitiveFunctionImplementation = withString $ stringValue . fmap C.toLower
  }

stringsToUpper :: PrimitiveFunction
stringsToUpper = PrimitiveFunction {
    primitiveFunctionName = stringsFunc "toUpper",
    primitiveFunctionType = FunctionType stringType stringType,
    primitiveFunctionImplementation = withString $  stringValue . fmap C.toUpper
  }

withString :: (String -> Term) -> [Term] -> Result Term
withString func args = do
  expectNArgs 1 args
  func <$> expectStringTerm (L.head args)

withTwoStrings :: (String -> String -> Term) -> [Term] -> Result Term
withTwoStrings func args = do
  expectNArgs 2 args
  func <$> expectStringTerm (L.head args) <*> expectStringTerm (args !! 1)
