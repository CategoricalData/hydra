module Hydra.Impl.Haskell.Lib.Strings (
  hsCat,
  hsLength,
  hsSplitOn,
  hsToLower,
  hsToUpper,
  stringPrimitives,
  stringsFunc,
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Impl.Haskell.Extras

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as LS


stringPrimitives :: (Default a, Show a) => [PrimitiveFunction a]
stringPrimitives = [
  prim "cat" [stringType, stringType, stringType]
    $ withTwoStrings stringValue hsCat,
  prim "length" [stringType, int32Type]
    $ withString int32Value hsLength,
  prim "splitOn" [stringType, stringType, listType stringType]
    $ withTwoStrings (\l -> list (stringValue <$> l)) hsSplitOn,
  prim "toLower" [stringType, stringType]
    $ withString stringValue hsToLower,
  prim "toUpper" [stringType, stringType]
    $ withString stringValue hsToUpper]

hsCat :: String -> String -> String
hsCat x y = x ++ y

hsLength :: String -> Int
hsLength = L.length

hsSplitOn :: String -> String -> [String]
hsSplitOn = LS.splitOn

hsToLower :: String -> String
hsToLower = fmap C.toLower

hsToUpper :: String -> String
hsToUpper = fmap C.toUpper


stringsFunc :: String -> Name
stringsFunc local = "hydra/lib/strings." ++ local

prim :: String -> [Type] -> ([Term a] -> Result (Term a)) -> PrimitiveFunction a
prim nm types impl = PrimitiveFunction {
    primitiveFunctionName = stringsFunc nm,
    primitiveFunctionType = ftype types,
    primitiveFunctionImplementation  = impl}
  where
    ftype types = case types of
      [dom, cod] -> FunctionType dom cod
      (h:r) -> FunctionType h (TypeFunction $ ftype r)

withString :: Show a => (b -> Term a ) -> (String -> b) -> [Term a] -> Result (Term a)
withString toTerm func args = do
  expectNArgs 1 args
  toTerm <$> (func <$> expectStringTerm (L.head args))

withTwoStrings :: Show a => (b -> Term a) -> (String -> String -> b) -> [Term a] -> Result (Term a)
withTwoStrings toTerm func args = do
  expectNArgs 2 args
  toTerm <$> (func <$> expectStringTerm (args !! 0) <*> expectStringTerm (args !! 1))
