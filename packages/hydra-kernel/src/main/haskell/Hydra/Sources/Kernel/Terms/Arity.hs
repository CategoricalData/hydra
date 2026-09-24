module Hydra.Sources.Kernel.Terms.Arity where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (functionArity, primitiveArity, termArity, typeArity, typeSchemeArity, uncurryType)
import qualified Hydra.Core.Dsl.Paths    as Paths
import qualified Hydra.Core.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Core.Dsl.Ast          as Ast
import qualified Hydra.Core.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Core.Dsl.Coders       as Coders
import qualified Hydra.Core.Dsl.Util      as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core         as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Graph        as Graph
import qualified Hydra.Core.Dsl.Json.Model         as Json
import qualified Hydra.Core.Dsl.Lib.Chars    as Chars
import qualified Hydra.Core.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Core.Dsl.Lib.Equality as Equality
import qualified Hydra.Core.Dsl.Lib.Lists    as Lists
import qualified Hydra.Core.Dsl.Lib.Literals as Literals
import qualified Hydra.Core.Dsl.Lib.Logic    as Logic
import qualified Hydra.Core.Dsl.Lib.Maps     as Maps
import qualified Hydra.Core.Dsl.Lib.Math     as Math
import qualified Hydra.Core.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Core.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Core.Dsl.Lib.Sets     as Sets
import qualified Hydra.Core.Dsl.Lib.Strings  as Strings
import qualified Hydra.Core.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Core.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Core.Overlay.Haskell.Dsl.Base         as MetaBase
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Core.Dsl.Packaging       as Packaging
import qualified Hydra.Core.Dsl.Parsing      as Parsing
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms     as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing      as Testing
import qualified Hydra.Core.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Core.Dsl.Topology     as Topology
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Core.Dsl.Typing       as Typing
import qualified Hydra.Core.Dsl.Util         as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y


ns :: ModuleName
ns = ModuleName "hydra.core.arity"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> (kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Functions dealing with arguments and arity.")}
  where
    definitions = [
      toDefinition primitiveArity,
      toDefinition termArity,
      toDefinition typeArity,
      toDefinition typeSchemeArity,
      toDefinition uncurryType]

primitiveArity :: TypedTermDefinition (Primitive -> Int)
primitiveArity = define "primitiveArity" $
  doc "Find the arity (expected number of arguments) of a primitive constant or function" $
  "prim" ~>
  Lists.length $ Typing.termSignatureParameters $ Packaging.primitiveDefinitionSignature $ Graph.primitiveDefinition (var "prim")

termArity :: TypedTermDefinition (Term -> Int)
termArity = define "termArity" $
  doc "Find the arity (expected number of arguments) of a term" $
  cases _Term (Just $ int32 0) [
    _Term_application>>: (lambda "xapp" $ Math.sub (var "xapp") (int32 1)) <.> termArity <.> reify Core.applicationFunction,
    _Term_cases>>: constant (int32 1),
    _Term_lambda>>: (lambda "i" $ Math.add (int32 1) (var "i")) <.> (termArity <.> reify Core.lambdaBody),
    _Term_project>>: constant (int32 1),
    _Term_unwrap>>: constant (int32 1)]
    -- Note: ignoring variables which might resolve to functions

typeArity :: TypedTermDefinition (Type -> Int)
typeArity = define "typeArity" $
  doc "Find the arity (expected number of arguments) of a type" $
  cases _Type (Just $ int32 0) [
    _Type_annotated>>: typeArity <.> reify Core.annotatedTypeBody,
    _Type_application>>: typeArity <.> reify Core.applicationTypeFunction,
    _Type_forall>>: typeArity <.> reify Core.forallTypeBody,
    _Type_function>>: lambda "f" $
      Math.add (int32 1) (typeArity <.> reify Core.functionTypeCodomain @@ var "f")]

typeSchemeArity :: TypedTermDefinition (TypeScheme -> Int)
typeSchemeArity = define "typeSchemeArity" $
  doc "Find the arity (expected number of arguments) of a type scheme" $
  typeArity <.> reify Core.typeSchemeBody

uncurryType :: TypedTermDefinition (Type -> [Type])
uncurryType = define "uncurryType" $
  doc "Uncurry a type expression into a list of types, turning a function type a -> b into cons a (uncurryType b)" $
  lambda "t" ((cases _Type (Just $ list [var "t"]) [
    _Type_annotated>>: uncurryType <.> reify Core.annotatedTypeBody,
    _Type_application>>: uncurryType <.> reify Core.applicationTypeFunction,
    _Type_forall>>: uncurryType <.> reify Core.forallTypeBody,
    _Type_function>>: lambda "ft" $ Lists.cons
      (Core.functionTypeDomain $ var "ft")
      (uncurryType <.> reify Core.functionTypeCodomain @@ var "ft")]) @@ var "t")
