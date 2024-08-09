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
  Core.FunctionLambda v268 -> (Math.add 1 (termArity (Core.lambdaBody v268)))
  Core.FunctionPrimitive _ -> 42

lookupPrimitive :: (Graph.Graph -> Core.Name -> Maybe Graph.Primitive)
lookupPrimitive g name = (Maps.lookup name (Graph.graphPrimitives g))

-- | Find the arity (expected number of arguments) of a primitive constant or function
primitiveArity :: (Graph.Primitive -> Int)
primitiveArity x = (typeArity (Graph.primitiveType x))

-- | Construct a qualified (dot-separated) name
qname :: (Module.Namespace -> String -> Core.Name)
qname ns name = (Core.Name (Strings.cat [
  Module.unNamespace ns,
  ".",
  name]))

termArity :: (Core.Term -> Int)
termArity x = case x of
  Core.TermApplication v270 -> ((\x -> Math.sub x 1) (termArity (Core.applicationFunction v270)))
  Core.TermFunction v271 -> (functionArity v271)
  _ -> 0

typeArity :: (Core.Type -> Int)
typeArity x = case x of
  Core.TypeAnnotated v272 -> (typeArity (Core.annotatedTypeSubject v272))
  Core.TypeApplication v273 -> (typeArity (Core.applicationTypeFunction v273))
  Core.TypeLambda v274 -> (typeArity (Core.lambdaTypeBody v274))
  Core.TypeFunction v275 -> (Math.add 1 (typeArity (Core.functionTypeCodomain v275)))
  _ -> 0

-- | Uncurry a type expression into a list of types, turning a function type a -> b into cons a (uncurryType b)
uncurryType :: (Core.Type -> [Core.Type])
uncurryType t = ((\x -> case x of
  Core.TypeAnnotated v276 -> (uncurryType (Core.annotatedTypeSubject v276))
  Core.TypeApplication v277 -> (uncurryType (Core.applicationTypeFunction v277))
  Core.TypeLambda v278 -> (uncurryType (Core.lambdaTypeBody v278))
  Core.TypeFunction v279 -> (Lists.cons (Core.functionTypeDomain v279) (uncurryType (Core.functionTypeCodomain v279)))
  _ -> [
    t]) t)

getAnnotation :: (String -> Map String Core.Term -> Maybe Core.Term)
getAnnotation key ann = (Maps.lookup key ann)