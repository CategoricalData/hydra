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
  Core.FunctionLambda v234 -> (Math.add 1 (termArity (Core.lambdaBody v234)))
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
  Core.TermApplication v236 -> ((\x -> Math.sub x 1) (termArity (Core.applicationFunction v236)))
  Core.TermFunction v237 -> (functionArity v237)
  _ -> 0

typeArity :: (Core.Type -> Int)
typeArity x = case x of
  Core.TypeAnnotated v238 -> (typeArity (Core.annotatedSubject v238))
  Core.TypeApplication v239 -> (typeArity (Core.applicationTypeFunction v239))
  Core.TypeLambda v240 -> (typeArity (Core.lambdaTypeBody v240))
  Core.TypeFunction v241 -> (Math.add 1 (typeArity (Core.functionTypeCodomain v241)))
  _ -> 0

-- | Uncurry a type expression into a list of types, turning a function type a -> b into cons a (uncurryType b)
uncurryType :: (Core.Type -> [Core.Type])
uncurryType t = ((\x -> case x of
  Core.TypeAnnotated v242 -> (uncurryType (Core.annotatedSubject v242))
  Core.TypeApplication v243 -> (uncurryType (Core.applicationTypeFunction v243))
  Core.TypeLambda v244 -> (uncurryType (Core.lambdaTypeBody v244))
  Core.TypeFunction v245 -> (Lists.cons (Core.functionTypeDomain v245) (uncurryType (Core.functionTypeCodomain v245)))
  _ -> [
    t]) t)

emptyKv :: Core.Kv
emptyKv = Core.Kv {
  Core.kvAnnotations = Maps.empty}

getAnnotation :: (String -> Core.Kv -> Maybe Core.Term)
getAnnotation key ann = (Maps.lookup key (Core.kvAnnotations ann))