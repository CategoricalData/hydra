module Hydra.Sources.Kernel.Terms.Arity where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (functionArity, primitiveArity, termArity, typeArity, typeSchemeArity, uncurryType)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y


ns :: Namespace
ns = Namespace "hydra.arity"

define :: String -> TTerm a -> TBinding a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    []
    kernelTypesNamespaces $
    Just "Functions dealing with arguments and arity."
  where
    elements = [
      toBinding functionArity,
      toBinding primitiveArity,
      toBinding termArity,
      toBinding typeArity,
      toBinding typeSchemeArity,
      toBinding uncurryType]

functionArity :: TBinding (Function -> Int)
functionArity = define "functionArity" $
  doc "Find the arity (expected number of arguments) of a function" $
  match _Function Nothing [
    _Function_elimination>>: constant (int32 1),
    _Function_lambda>>: (lambda "i" $ Math.add (int32 1) (var "i")) <.> (termArity <.> unaryFunction Core.lambdaBody),
    _Function_primitive>>: constant $
      doc "TODO: This function needs to be monadic, so we can look up the primitive" (int32 42)]

primitiveArity :: TBinding (Primitive -> Int)
primitiveArity = define "primitiveArity" $
  doc "Find the arity (expected number of arguments) of a primitive constant or function" $
  (typeArity <.> unaryFunction Core.typeSchemeType <.> unaryFunction Graph.primitiveType)

termArity :: TBinding (Term -> Int)
termArity = define "termArity" $
  doc "Find the arity (expected number of arguments) of a term" $
  match _Term (Just $ int32 0) [
    _Term_application>>: (lambda "xapp" $ Math.sub (var "xapp") (int32 1)) <.> termArity <.> unaryFunction Core.applicationFunction,
    _Term_function>>: functionArity]
    -- Note: ignoring variables which might resolve to functions

typeArity :: TBinding (Type -> Int)
typeArity = define "typeArity" $
  doc "Find the arity (expected number of arguments) of a type" $
  match _Type (Just $ int32 0) [
    _Type_annotated>>: typeArity <.> unaryFunction Core.annotatedTypeBody,
    _Type_application>>: typeArity <.> unaryFunction Core.applicationTypeFunction,
    _Type_forall>>: typeArity <.> unaryFunction Core.forallTypeBody,
    _Type_function>>: lambda "f" $
      Math.add (int32 1) (typeArity <.> unaryFunction Core.functionTypeCodomain @@ var "f")]

typeSchemeArity :: TBinding (TypeScheme -> Int)
typeSchemeArity = define "typeSchemeArity" $
  doc "Find the arity (expected number of arguments) of a type scheme" $
  typeArity <.> unaryFunction Core.typeSchemeType

uncurryType :: TBinding (Type -> [Type])
uncurryType = define "uncurryType" $
  doc "Uncurry a type expression into a list of types, turning a function type a -> b into cons a (uncurryType b)" $
  lambda "t" ((match _Type (Just $ list [var "t"]) [
    _Type_annotated>>: uncurryType <.> unaryFunction Core.annotatedTypeBody,
    _Type_application>>: uncurryType <.> unaryFunction Core.applicationTypeFunction,
    _Type_forall>>: uncurryType <.> unaryFunction Core.forallTypeBody,
    _Type_function>>: lambda "ft" $ Lists.cons
      (Core.functionTypeDomain $ var "ft")
      (uncurryType <.> unaryFunction Core.functionTypeCodomain @@ var "ft")]) @@ var "t")
