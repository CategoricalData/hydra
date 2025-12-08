-- | A DSL which is used as a basis for some of the other DSLs

module Hydra.Dsl.Annotations where

import Hydra.Core
import Hydra.Compute
import Hydra.Constants
import Hydra.Annotations
import Hydra.Dsl.AsType
import Hydra.Formatting
import Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import qualified Data.Map as M
import qualified Data.Maybe as Y


annotateTerm :: Name -> Y.Maybe Term -> Term -> Term
annotateTerm = setTermAnnotation

annotateType :: Name -> Y.Maybe Term -> Type -> Type
annotateType = setTypeAnnotation

bounded :: Maybe Int -> Maybe Int -> Type -> Type
bounded min max = annotMin . annotMax
  where
    annotMax t = Y.maybe t (`setMaxLength` t) max
    annotMin t = Y.maybe t (`setMinLength` t) max

boundedList :: AsType a => Maybe Int -> Maybe Int -> a -> Type
boundedList min max et = bounded min max $ Types.list (asType et)

boundedMap :: (AsType a, AsType b) => Maybe Int -> Maybe Int -> a -> b -> Type
boundedMap min max kt vt = bounded min max $ Types.map (asType kt) (asType vt)

boundedSet :: AsType a => Maybe Int -> Maybe Int -> a -> Type
boundedSet min max et = bounded min max $ Types.set (asType et)

boundedString :: Maybe Int -> Maybe Int -> Type
boundedString min max = bounded min max Types.string

deprecated :: AsType a => a -> Type
deprecated = setTypeAnnotation key_deprecated (Just $ Terms.boolean True) . asType

doc :: AsType a => String -> a -> Type
doc s = setTypeDescription (Just s) . asType

doc70 :: AsType a => String -> a -> Type
doc70 = doc . wrapLine 70

doc80 :: AsType a => String -> a -> Type
doc80 = doc . wrapLine 80

dataDoc :: String -> Term -> Term
dataDoc s = setTermDescription (Just s)

exclude :: AsType a => a -> Type
exclude = setTypeAnnotation key_exclude (Just $ Terms.boolean True) . asType

minLengthList :: AsType a => Int -> a -> Type
minLengthList len = boundedList (Just len) Nothing

nonemptyList :: AsType a => a -> Type
nonemptyList = minLengthList 1

nonemptyMap :: (AsType a, AsType b) => a -> b -> Type
nonemptyMap = boundedMap (Just 1) Nothing

note :: AsType a => String -> a -> Type
note s = doc $ "Note: " ++ s

preserveFieldName :: AsType a => a -> Type
preserveFieldName = setTypeAnnotation key_preserveFieldName (Just $ Terms.boolean True) . asType

see :: AsType a => String -> a -> Type
see s = doc $ "See " ++ s

setMaxLength :: Int -> Type -> Type
setMaxLength m = setTypeAnnotation key_maxLength (Just $ Terms.int32 m)

setMinLength :: Int -> Type -> Type
setMinLength m = setTypeAnnotation key_minLength (Just $ Terms.int32 m)

twoOrMoreList :: AsType a => a -> Type
twoOrMoreList = boundedList (Just 2) Nothing
