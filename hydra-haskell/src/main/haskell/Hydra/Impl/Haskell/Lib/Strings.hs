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
    primitiveFunctionImplementation = \[x, y] -> case (x, y) of
      (TermAtomic (AtomicValueString x'), TermAtomic (AtomicValueString y'))
        -> pure $ stringValue $ x' ++ y'
  }

stringsLength :: PrimitiveFunction
stringsLength = PrimitiveFunction {
  primitiveFunctionName = stringsFunc "length",
  primitiveFunctionType = FunctionType stringType int32Type,
  primitiveFunctionImplementation = \[x] -> case x of
    TermAtomic (AtomicValueString s)
      -> pure $ int32Value $ L.length s
  }

stringsToLower :: PrimitiveFunction
stringsToLower = PrimitiveFunction {
  primitiveFunctionName = stringsFunc "toLower",
  primitiveFunctionType = FunctionType stringType stringType,
  primitiveFunctionImplementation = \[x] -> case x of
    TermAtomic (AtomicValueString s)
      -> pure $ stringValue $ fmap C.toLower s
  }

stringsToUpper :: PrimitiveFunction
stringsToUpper = PrimitiveFunction {
  primitiveFunctionName = stringsFunc "toUpper",
  primitiveFunctionType = FunctionType stringType stringType,
  primitiveFunctionImplementation = \[x] -> case x of
    TermAtomic (AtomicValueString s)
      -> pure $ stringValue $ fmap C.toUpper s
  }
