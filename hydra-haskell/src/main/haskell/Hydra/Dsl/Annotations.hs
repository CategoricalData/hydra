-- | A DSL which is used as a basis for some of the other DSLs

module Hydra.Dsl.Annotations where

import Hydra.Core
import Hydra.Compute
import Hydra.Kv
import Hydra.Tools.Formatting
import Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import qualified Data.Map as M
import qualified Data.Maybe as Y


key_deprecated = "deprecated"
key_maxLength = "maxLength"
key_minLength = "minLength"
key_preserveFieldName = "preserveFieldName"

annotateTerm :: String -> Y.Maybe (Term Kv) -> Term Kv -> Term Kv
annotateTerm = setTermAnnotation

annotateType :: String -> Y.Maybe (Term Kv) -> Type Kv -> Type Kv
annotateType = setTypeAnnotation

bounded :: Maybe Int -> Maybe Int -> Type Kv -> Type Kv
bounded min max = annotMin . annotMax
  where
    annotMax t = Y.maybe t (`setMaxLength` t) max
    annotMin t = Y.maybe t (`setMinLength` t) max

boundedList :: Maybe Int -> Maybe Int -> Type Kv -> Type Kv
boundedList min max et = bounded min max $ Types.list et

boundedSet :: Maybe Int -> Maybe Int -> Type Kv -> Type Kv
boundedSet min max et = bounded min max $ Types.set et

boundedString :: Maybe Int -> Maybe Int -> Type Kv
boundedString min max = bounded min max Types.string

deprecated :: Type Kv -> Type Kv
deprecated = setTypeAnnotation key_deprecated (Just $ Terms.boolean True)

doc :: String -> Type Kv -> Type Kv
doc s = setTypeDescription (Just s)

doc70 :: String -> Type Kv -> Type Kv
doc70 = doc . wrapLine 70

doc80 :: String -> Type Kv -> Type Kv
doc80 = doc . wrapLine 80

dataDoc :: String -> Term Kv -> Term Kv
dataDoc s = setTermDescription (Just s)

minLengthList :: Int -> Type Kv -> Type Kv
minLengthList len = boundedList (Just len) Nothing

nonemptyList :: Type Kv -> Type Kv
nonemptyList = minLengthList 1

note :: String -> Type Kv -> Type Kv
note s = doc $ "Note: " ++ s

preserveFieldName :: Type Kv -> Type Kv
preserveFieldName = setTypeAnnotation key_preserveFieldName (Just $ Terms.boolean True)

see :: String -> Type Kv -> Type Kv
see s = doc $ "See " ++ s

setMaxLength :: Int -> Type Kv -> Type Kv
setMaxLength m = setTypeAnnotation key_maxLength (Just $ Terms.int32 m)

setMinLength :: Int -> Type Kv -> Type Kv
setMinLength m = setTypeAnnotation key_minLength (Just $ Terms.int32 m)

twoOrMoreList :: Type Kv -> Type Kv
twoOrMoreList = boundedList (Just 2) Nothing
