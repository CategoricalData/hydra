module Hydra.Impl.Haskell.Dsl.Standard (
  module Hydra.Impl.Haskell.Dsl.Standard,
  module Hydra.Impl.Haskell.Dsl.Bootstrap
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Terms as Terms
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Meta
import Hydra.Impl.Haskell.Sources.Libraries
import Hydra.Impl.Haskell.Sources.Core
import Hydra.Lexical
import Hydra.Impl.Haskell.Dsl.Bootstrap
import Hydra.Monads

import qualified Data.Map as M
import qualified Data.Maybe as Y


key_maxSize = "maxLength"
key_minSize = "minLength"

annotateTerm :: String -> Y.Maybe (Term Meta) -> Term Meta -> Term Meta
annotateTerm = setTermAnnotation coreContext

annotateType :: String -> Y.Maybe (Term Meta) -> Type Meta -> Type Meta
annotateType = setTypeAnnotation coreContext

bounded :: Maybe Int -> Maybe Int -> Type Meta -> Type Meta
bounded min max = annotMin . annotMax
  where
    annotMax t = Y.maybe t (`setMaxLength` t) max
    annotMin t = Y.maybe t (`setMinLength` t) max

boundedList :: Maybe Int -> Maybe Int -> Type Meta -> Type Meta
boundedList min max et = bounded min max $ Types.list et

boundedSet :: Maybe Int -> Maybe Int -> Type Meta -> Type Meta
boundedSet min max et = bounded min max $ Types.set et

boundedString :: Maybe Int -> Maybe Int -> Type Meta
boundedString min max = bounded min max Types.string

coreContext :: Context Meta
coreContext = bootstrapContext {
  contextGraph = hydraCore,
  contextFunctions = M.fromList $ fmap (\p -> (primitiveFunctionName p, p)) standardPrimitives}

doc :: String -> Type Meta -> Type Meta
doc s = setTypeDescription coreContext (Just s)

dataDoc :: String -> Term Meta -> Term Meta
dataDoc s = setTermDescription coreContext (Just s)

dataterm :: Namespace -> String -> Type Meta -> Term Meta -> Element Meta
dataterm gname lname = termElement (qualify gname (Name lname))

nonemptyList :: Type Meta -> Type Meta
nonemptyList = boundedList (Just 1) Nothing

note :: String -> Type Meta -> Type Meta
note s = doc $ "Note: " ++ s

see :: String -> Type Meta -> Type Meta
see s = doc $ "See " ++ s

setMaxLength :: Int -> Type Meta -> Type Meta
setMaxLength m = setTypeAnnotation coreContext key_maxSize (Just $ Terms.int32 m)

setMinLength :: Int -> Type Meta -> Type Meta
setMinLength m = setTypeAnnotation coreContext key_minSize (Just $ Terms.int32 m)

standardGraph :: [Element Meta] -> Graph Meta
standardGraph = elementsToGraph (Just hydraCore)

twoOrMoreList :: Type Meta -> Type Meta
twoOrMoreList = boundedList (Just 2) Nothing
