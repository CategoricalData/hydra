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
  Core.FunctionLambda v236 -> (Math.add 1 (termArity (Core.lambdaBody v236)))
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
  Core.TermApplication v238 -> ((\x -> Math.sub x 1) (termArity (Core.applicationFunction v238)))
  Core.TermFunction v239 -> (functionArity v239)
  _ -> 0

typeArity :: (Core.Type -> Int)
typeArity x = case x of
  Core.TypeAnnotated v240 -> (typeArity (Core.annotatedTypeSubject v240))
  Core.TypeApplication v241 -> (typeArity (Core.applicationTypeFunction v241))
  Core.TypeLambda v242 -> (typeArity (Core.lambdaTypeBody v242))
  Core.TypeFunction v243 -> (Math.add 1 (typeArity (Core.functionTypeCodomain v243)))
  _ -> 0

-- | Uncurry a type expression into a list of types, turning a function type a -> b into cons a (uncurryType b)
uncurryType :: (Core.Type -> [Core.Type])
uncurryType t = ((\x -> case x of
  Core.TypeAnnotated v244 -> (uncurryType (Core.annotatedTypeSubject v244))
  Core.TypeApplication v245 -> (uncurryType (Core.applicationTypeFunction v245))
  Core.TypeLambda v246 -> (uncurryType (Core.lambdaTypeBody v246))
  Core.TypeFunction v247 -> (Lists.cons (Core.functionTypeDomain v247) (uncurryType (Core.functionTypeCodomain v247)))
  _ -> [
    t]) t)

getAnnotation :: (String -> Map String Core.Term -> Maybe Core.Term)
getAnnotation key ann = (Maps.lookup key ann)