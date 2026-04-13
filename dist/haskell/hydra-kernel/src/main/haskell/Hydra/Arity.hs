-- Note: this is an automatically generated file. Do not edit.

-- | Functions dealing with arguments and arity.

module Hydra.Arity where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Math as Math
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | Find the arity (expected number of arguments) of a primitive constant or function
primitiveArity :: Graph.Primitive -> Int
primitiveArity arg_ = (\arg_2 -> typeArity (Core.typeSchemeType arg_2)) (Graph.primitiveType arg_)

-- | Find the arity (expected number of arguments) of a term
termArity :: Core.Term -> Int
termArity x =
    case x of
      Core.TermApplication v0 -> (\arg_2 -> (\xapp -> Math.sub xapp 1) (termArity arg_2)) (Core.applicationFunction v0)
      Core.TermCases _ -> 1
      Core.TermLambda v0 -> (\i -> Math.add 1 i) (termArity (Core.lambdaBody v0))
      Core.TermProject _ -> 1
      Core.TermUnwrap _ -> 1
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
