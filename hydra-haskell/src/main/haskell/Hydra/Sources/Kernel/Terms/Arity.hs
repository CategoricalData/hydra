{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Arity where

-- Standard Tier-2 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Mantle        as Mantle
import qualified Hydra.Dsl.Module        as Module
import qualified Hydra.Dsl.TTerms        as TTerms
import qualified Hydra.Dsl.TTypes        as TTypes
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y


module_ :: Module
module_ = Module (Namespace "hydra.arity") elements
    []
    kernelTypesModules $
    Just ("Functions dealing with arguments and arity.")
  where
    elements = [
      el functionArityDef,
      el primitiveArityDef,
      el termArityDef,
      el typeArityDef,
      el uncurryTypeDef]

define :: String -> TTerm a -> TElement a
define = definitionInModule module_

functionArityDef :: TElement (Function -> Int)
functionArityDef = define "functionArity" $
  match _Function Nothing [
    _Function_elimination>>: constant (int32 1),
    _Function_lambda>>: (lambda "i" $ Math.add (int32 1) (var "i")) <.> (ref termArityDef <.> unaryFunction Core.lambdaBody),
    _Function_primitive>>: constant $
      doc "TODO: This function needs to be monadic, so we can look up the primitive" (int32 42)]

primitiveArityDef :: TElement (Primitive -> Int)
primitiveArityDef = define "primitiveArity" $
  doc "Find the arity (expected number of arguments) of a primitive constant or function" $
  (ref typeArityDef <.> unaryFunction Core.typeSchemeType <.> unaryFunction Graph.primitiveType)

termArityDef :: TElement (Term -> Int)
termArityDef = define "termArity" $
  match _Term (Just $ int32 0) [
    _Term_application>>: (lambda "xapp" $ Math.sub (var "xapp") (int32 1)) <.> ref termArityDef <.> unaryFunction Core.applicationFunction,
    _Term_function>>: ref functionArityDef]
    -- Note: ignoring variables which might resolve to functions

typeArityDef :: TElement (Type -> Int)
typeArityDef = define "typeArity" $
  match _Type (Just $ int32 0) [
    _Type_annotated>>: ref typeArityDef <.> unaryFunction Core.annotatedTypeSubject,
    _Type_application>>: ref typeArityDef <.> unaryFunction Core.applicationTypeFunction,
    _Type_forall>>: ref typeArityDef <.> unaryFunction Core.forallTypeBody,
    _Type_function>>: lambda "f" $
      Math.add (int32 1) (ref typeArityDef @@ (Core.functionTypeCodomain $ var "f"))]

uncurryTypeDef :: TElement (Type -> [Type])
uncurryTypeDef = define "uncurryType" $
  doc "Uncurry a type expression into a list of types, turning a function type a -> b into cons a (uncurryType b)" $
  lambda "t" ((match _Type (Just $ list [var "t"]) [
    _Type_annotated>>: ref uncurryTypeDef <.> unaryFunction Core.annotatedTypeSubject,
    _Type_application>>: ref uncurryTypeDef <.> unaryFunction Core.applicationTypeFunction,
    _Type_forall>>: ref uncurryTypeDef <.> unaryFunction Core.forallTypeBody,
    _Type_function>>: lambda "ft" $ Lists.cons
      (Core.functionTypeDomain $ var "ft")
      (ref uncurryTypeDef @@ (Core.functionTypeCodomain $ var "ft"))]) @@ var "t")
