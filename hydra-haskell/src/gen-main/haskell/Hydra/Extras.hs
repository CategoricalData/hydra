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
  Core.FunctionLambda v1 -> (Math.add 1 (termArity (Core.lambdaBody v1)))
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
  Core.TermApplication v3 -> ((\x -> Math.sub x 1) (termArity (Core.applicationFunction v3)))
  Core.TermFunction v4 -> (functionArity v4)
  _ -> 0

typeArity :: (Core.Type -> Int)
typeArity x = case x of
  Core.TypeAnnotated v5 -> (typeArity (Core.annotatedSubject v5))
  Core.TypeApplication v6 -> (typeArity (Core.applicationTypeFunction v6))
  Core.TypeLambda v7 -> (typeArity (Core.lambdaTypeBody v7))
  Core.TypeFunction v8 -> (Math.add 1 (typeArity (Core.functionTypeCodomain v8)))
  _ -> 0

-- | Uncurry a type expression into a list of types, turning a function type a -> b into cons a (uncurryType b)
uncurryType :: (Core.Type -> [Core.Type])
uncurryType t = ((\x -> case x of
  Core.TypeAnnotated v9 -> (uncurryType (Core.annotatedSubject v9))
  Core.TypeApplication v10 -> (uncurryType (Core.applicationTypeFunction v10))
  Core.TypeLambda v11 -> (uncurryType (Core.lambdaTypeBody v11))
  Core.TypeFunction v12 -> (Lists.cons (Core.functionTypeDomain v12) (uncurryType (Core.functionTypeCodomain v12)))
  _ -> [
    t]) t)

emptyKv :: Core.Kv
emptyKv = Core.Kv {
  Core.kvAnnotations = Maps.empty}

getAnnotation :: (String -> Core.Kv -> Maybe Core.Term)
getAnnotation key ann = (Maps.lookup key (Core.kvAnnotations ann))
