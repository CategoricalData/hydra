-- | Functions dealing with arguments and arity.

module Hydra.Arity where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Math as Math
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

functionArity :: (Core.Function -> Int)
functionArity x = case x of
  Core.FunctionElimination _ -> 1
  Core.FunctionLambda v1 -> ((\i -> Math.add 1 i) (termArity (Core.lambdaBody v1)))
  Core.FunctionPrimitive _ -> 42

-- | Find the arity (expected number of arguments) of a primitive constant or function
primitiveArity :: (Graph.Primitive -> Int)
primitiveArity arg_ = ((\arg_ -> typeArity (Core.typeSchemeType arg_)) (Graph.primitiveType arg_))

termArity :: (Core.Term -> Int)
termArity x = case x of
  Core.TermApplication v1 -> ((\arg_ -> (\xapp -> Math.sub xapp 1) (termArity arg_)) (Core.applicationFunction v1))
  Core.TermFunction v1 -> (functionArity v1)
  _ -> 0

typeArity :: (Core.Type -> Int)
typeArity x = case x of
  Core.TypeAnnotated v1 -> (typeArity (Core.annotatedTypeSubject v1))
  Core.TypeApplication v1 -> (typeArity (Core.applicationTypeFunction v1))
  Core.TypeLambda v1 -> (typeArity (Core.lambdaTypeBody v1))
  Core.TypeFunction v1 -> (Math.add 1 (typeArity (Core.functionTypeCodomain v1)))
  _ -> 0

-- | Uncurry a type expression into a list of types, turning a function type a -> b into cons a (uncurryType b)
uncurryType :: (Core.Type -> [Core.Type])
uncurryType t = ((\x -> case x of
  Core.TypeAnnotated v1 -> (uncurryType (Core.annotatedTypeSubject v1))
  Core.TypeApplication v1 -> (uncurryType (Core.applicationTypeFunction v1))
  Core.TypeLambda v1 -> (uncurryType (Core.lambdaTypeBody v1))
  Core.TypeFunction v1 -> (Lists.cons (Core.functionTypeDomain v1) (uncurryType (Core.functionTypeCodomain v1)))
  _ -> [
    t]) t)
