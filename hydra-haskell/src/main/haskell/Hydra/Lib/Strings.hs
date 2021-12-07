module Hydra.Lib.Strings where

import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Impl.Haskell.Extras
import Hydra.Impl.Haskell.Dsl.Prims

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as LS


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

stringPrimitives :: (Default a, Show a) => [PrimitiveFunction a]
stringPrimitives = [
    stringPrim "cat"     [stringType, stringType, stringType]
      $ withTwoStrings stringValue hsCat,
    stringPrim "length"  [stringType, int32Type]
      $ withString int32Value hsLength,
    stringPrim "splitOn" [stringType, stringType, listType stringType]
      $ withTwoStrings (\l -> list (stringValue <$> l)) hsSplitOn,
    stringPrim "toLower" [stringType, stringType]
      $ withString stringValue hsToLower,
    stringPrim "toUpper" [stringType, stringType]
      $ withString stringValue hsToUpper]
  where
    stringPrim = prim "hydra/lib/strings"
