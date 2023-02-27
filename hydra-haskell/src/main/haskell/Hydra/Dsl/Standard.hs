-- | A DSL which is used as a basis for some of the other DSLs

module Hydra.Dsl.Standard (
  module Hydra.Dsl.Standard,
  module Hydra.Dsl.Bootstrap
) where

import Hydra.Kernel
import Hydra.Kv
import Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries
import Hydra.Sources.Core
import Hydra.Dsl.Bootstrap
import qualified Hydra.Dsl.Types as Types

import qualified Data.Map as M
import qualified Data.Maybe as Y


key_maxSize = "maxLength"
key_minSize = "minLength"

annotateTerm :: String -> Y.Maybe (Term Kv) -> Term Kv -> Term Kv
annotateTerm = setTermAnnotation coreContext

annotateType :: String -> Y.Maybe (Term Kv) -> Type Kv -> Type Kv
annotateType = setTypeAnnotation coreContext

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

coreContext :: Context Kv
coreContext = bootstrapContext {
  contextGraph = hydraCore,
  contextPrimitives = M.fromList $ fmap (\p -> (primitiveName p, p)) standardPrimitives}

doc :: String -> Type Kv -> Type Kv
doc s = setTypeDescription coreContext (Just s)

dataDoc :: String -> Term Kv -> Term Kv
dataDoc s = setTermDescription coreContext (Just s)

dataterm :: Namespace -> String -> Type Kv -> Term Kv -> Element Kv
dataterm gname lname = termElement (qualify gname (Name lname))

graphContext :: Graph Kv -> Context Kv
graphContext g = coreContext {contextGraph = g}

nonemptyList :: Type Kv -> Type Kv
nonemptyList = boundedList (Just 1) Nothing

note :: String -> Type Kv -> Type Kv
note s = doc $ "Note: " ++ s

see :: String -> Type Kv -> Type Kv
see s = doc $ "See " ++ s

setMaxLength :: Int -> Type Kv -> Type Kv
setMaxLength m = setTypeAnnotation coreContext key_maxSize (Just $ Terms.int32 m)

setMinLength :: Int -> Type Kv -> Type Kv
setMinLength m = setTypeAnnotation coreContext key_minSize (Just $ Terms.int32 m)

standardGraph :: [Element Kv] -> Graph Kv
standardGraph = elementsToGraph (Just hydraCore)

twoOrMoreList :: Type Kv -> Type Kv
twoOrMoreList = boundedList (Just 2) Nothing
