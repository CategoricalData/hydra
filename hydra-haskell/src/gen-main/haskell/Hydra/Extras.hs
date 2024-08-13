-- | Basic functions which depend on primitive functions

module Hydra.Extras where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

functionArity :: (Core.Function -> Int)
functionArity x = case x of
  Core.FunctionElimination _ -> 1
  Core.FunctionLambda v275 -> (Math.add 1 (termArity (Core.lambdaBody v275)))
  Core.FunctionPrimitive _ -> 42

lookupPrimitive :: (Graph.Graph -> Core.Name -> Maybe Graph.Primitive)
lookupPrimitive g name = (Maps.lookup name (Graph.graphPrimitives g))

-- | Find the arity (expected number of arguments) of a primitive constant or function
primitiveArity :: (Graph.Primitive -> Int)
primitiveArity x = ((\x -> typeArity (Core.typeSchemeType x)) (Graph.primitiveType x))

-- | Construct a qualified (dot-separated) name
qname :: (Module.Namespace -> String -> Core.Name)
qname ns name = (Core.Name (Strings.cat [
  Module.unNamespace ns,
  ".",
  name]))

termArity :: (Core.Term -> Int)
termArity x = case x of
  Core.TermApplication v277 -> ((\x -> (\x -> Math.sub x 1) (termArity x)) (Core.applicationFunction v277))
  Core.TermFunction v278 -> (functionArity v278)
  _ -> 0

typeArity :: (Core.Type -> Int)
typeArity x = case x of
  Core.TypeAnnotated v279 -> (typeArity (Core.annotatedTypeSubject v279))
  Core.TypeApplication v280 -> (typeArity (Core.applicationTypeFunction v280))
  Core.TypeLambda v281 -> (typeArity (Core.lambdaTypeBody v281))
  Core.TypeFunction v282 -> (Math.add 1 (typeArity (Core.functionTypeCodomain v282)))
  _ -> 0

-- | Uncurry a type expression into a list of types, turning a function type a -> b into cons a (uncurryType b)
uncurryType :: (Core.Type -> [Core.Type])
uncurryType t = ((\x -> case x of
  Core.TypeAnnotated v283 -> (uncurryType (Core.annotatedTypeSubject v283))
  Core.TypeApplication v284 -> (uncurryType (Core.applicationTypeFunction v284))
  Core.TypeLambda v285 -> (uncurryType (Core.lambdaTypeBody v285))
  Core.TypeFunction v286 -> (Lists.cons (Core.functionTypeDomain v286) (uncurryType (Core.functionTypeCodomain v286)))
  _ -> [
    t]) t)

getAnnotation :: (String -> Map String Core.Term -> Maybe Core.Term)
getAnnotation key ann = (Maps.lookup key ann)