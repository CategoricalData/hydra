-- Note: this is an automatically generated file. Do not edit.

-- | Functions dealing with arguments and arity.

module Hydra.Arity where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Math as Math
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Find the arity (expected number of arguments) of a function
functionArity :: Core.Function -> Int
functionArity x =
    case x of
      Core.FunctionElimination _ -> 1
      Core.FunctionLambda v0 -> (\i -> Math.add 1 i) (termArity (Core.lambdaBody v0))
      Core.FunctionPrimitive _ -> 42

-- | Find the arity (expected number of arguments) of a primitive constant or function
primitiveArity :: Graph.Primitive -> Int
primitiveArity arg_ = (\arg_ -> typeArity (Core.typeSchemeType arg_)) (Graph.primitiveType arg_)

-- | Find the arity (expected number of arguments) of a term
termArity :: Core.Term -> Int
termArity x =
    case x of
      Core.TermApplication v0 -> (\arg_ -> (\xapp -> Math.sub xapp 1) (termArity arg_)) (Core.applicationFunction v0)
      Core.TermFunction v0 -> functionArity v0
      _ -> 0

-- | Find the arity (expected number of arguments) of a type
typeArity :: Core.Type -> Int
typeArity x =
    case x of
      Core.TypeAnnotated v0 -> typeArity (Core.annotatedTypeBody v0)
      Core.TypeApplication v0 -> typeArity (Core.applicationTypeFunction v0)
      Core.TypeForall v0 -> typeArity (Core.forallTypeBody v0)
      Core.TypeFunction v0 -> Math.add 1 (typeArity (Core.functionTypeCodomain v0))
      _ -> 0

-- | Find the arity (expected number of arguments) of a type scheme
typeSchemeArity :: Core.TypeScheme -> Int
typeSchemeArity arg_ = typeArity (Core.typeSchemeType arg_)

-- | Uncurry a type expression into a list of types, turning a function type a -> b into cons a (uncurryType b)
uncurryType :: Core.Type -> [Core.Type]
uncurryType t =
    case t of
      Core.TypeAnnotated v0 -> uncurryType (Core.annotatedTypeBody v0)
      Core.TypeApplication v0 -> uncurryType (Core.applicationTypeFunction v0)
      Core.TypeForall v0 -> uncurryType (Core.forallTypeBody v0)
      Core.TypeFunction v0 -> Lists.cons (Core.functionTypeDomain v0) (uncurryType (Core.functionTypeCodomain v0))
      _ -> [
        t]
