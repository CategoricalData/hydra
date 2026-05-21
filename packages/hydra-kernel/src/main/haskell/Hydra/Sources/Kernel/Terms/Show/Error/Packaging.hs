module Hydra.Sources.Kernel.Terms.Show.Error.Packaging where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import qualified Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import qualified Hydra.Dsl.Error.Packaging   as ErrorPackaging
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Show.Util as ShowUtil
import Hydra.Error.Packaging


ns :: Namespace
ns = Namespace "hydra.show.error.packaging"

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([ShowUtil.ns] L.++ kernelTypesNamespaces),
            moduleDescription = Just "String representations of hydra.error.packaging types"}
  where
   definitions = [
     toDefinition conflictingModuleNamespaceError,
     toDefinition conflictingVariantNameError,
     toDefinition definitionNotInModuleNamespaceError,
     toDefinition definitionsOutOfOrderError,
     toDefinition duplicateDefinitionNameError,
     toDefinition duplicateModuleNamespaceError,
     toDefinition invalidDefinitionNameError,
     toDefinition invalidModuleError,
     toDefinition invalidNamespaceConventionError,
     toDefinition invalidPackageError,
     toDefinition invalidPackageNameError,
     toDefinition missingDocumentationError]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

conflictingModuleNamespaceError :: TTermDefinition (ConflictingModuleNamespaceError -> String)
conflictingModuleNamespaceError = define "conflictingModuleNamespaceError" $
  doc "Show a conflicting module namespace error as a string" $
  "e" ~> Strings.cat $ list [
    string "module namespaces conflict when mapped to target language path: ",
    Packaging.unNamespace $ project _ConflictingModuleNamespaceError _ConflictingModuleNamespaceError_first @@ var "e",
    string " vs ",
    Packaging.unNamespace $ project _ConflictingModuleNamespaceError _ConflictingModuleNamespaceError_second @@ var "e"]

conflictingVariantNameError :: TTermDefinition (ConflictingVariantNameError -> String)
conflictingVariantNameError = define "conflictingVariantNameError" $
  doc "Show a conflicting variant name error as a string" $
  "e" ~> Strings.cat $ list [
    string "in module ",
    Packaging.unNamespace $ project _ConflictingVariantNameError _ConflictingVariantNameError_namespace @@ var "e",
    string ": variant ",
    Core.unName $ project _ConflictingVariantNameError _ConflictingVariantNameError_variantName @@ var "e",
    string " of type ",
    Core.unName $ project _ConflictingVariantNameError _ConflictingVariantNameError_typeName @@ var "e",
    string " produces constructor name ",
    Core.unName $ project _ConflictingVariantNameError _ConflictingVariantNameError_conflictingName @@ var "e",
    string " which conflicts with another type definition"]

definitionNotInModuleNamespaceError :: TTermDefinition (DefinitionNotInModuleNamespaceError -> String)
definitionNotInModuleNamespaceError = define "definitionNotInModuleNamespaceError" $
  doc "Show a definition-not-in-module-namespace error as a string" $
  "e" ~> Strings.cat $ list [
    string "definition ",
    Core.unName $ project _DefinitionNotInModuleNamespaceError _DefinitionNotInModuleNamespaceError_name @@ var "e",
    string " is not in module namespace ",
    Packaging.unNamespace $ project _DefinitionNotInModuleNamespaceError _DefinitionNotInModuleNamespaceError_namespace @@ var "e"]

definitionsOutOfOrderError :: TTermDefinition (DefinitionsOutOfOrderError -> String)
definitionsOutOfOrderError = define "definitionsOutOfOrderError" $
  doc "Show a definitions-out-of-order error as a string" $
  "e" ~> Strings.cat $ list [
    string "in module ",
    Packaging.unNamespace $ project _DefinitionsOutOfOrderError _DefinitionsOutOfOrderError_namespace @@ var "e",
    string ": definitions out of order: ",
    Core.unName $ project _DefinitionsOutOfOrderError _DefinitionsOutOfOrderError_precedingName @@ var "e",
    string " precedes ",
    Core.unName $ project _DefinitionsOutOfOrderError _DefinitionsOutOfOrderError_followingName @@ var "e"]

duplicateDefinitionNameError :: TTermDefinition (DuplicateDefinitionNameError -> String)
duplicateDefinitionNameError = define "duplicateDefinitionNameError" $
  doc "Show a duplicate definition name error as a string" $
  "e" ~> Strings.cat $ list [
    string "in module ",
    Packaging.unNamespace $ project _DuplicateDefinitionNameError _DuplicateDefinitionNameError_namespace @@ var "e",
    string ": duplicate definition name ",
    Core.unName $ project _DuplicateDefinitionNameError _DuplicateDefinitionNameError_name @@ var "e"]

duplicateModuleNamespaceError :: TTermDefinition (DuplicateModuleNamespaceError -> String)
duplicateModuleNamespaceError = define "duplicateModuleNamespaceError" $
  doc "Show a duplicate module namespace error as a string" $
  "e" ~> Strings.cat $ list [
    string "duplicate module namespace ",
    Packaging.unNamespace $ project _DuplicateModuleNamespaceError _DuplicateModuleNamespaceError_namespace @@ var "e"]

invalidDefinitionNameError :: TTermDefinition (InvalidDefinitionNameError -> String)
invalidDefinitionNameError = define "invalidDefinitionNameError" $
  doc "Show an invalid definition name error as a string" $
  "e" ~> Strings.cat $ list [
    string "in module ",
    Packaging.unNamespace $ project _InvalidDefinitionNameError _InvalidDefinitionNameError_namespace @@ var "e",
    string ": definition ",
    Core.unName $ project _InvalidDefinitionNameError _InvalidDefinitionNameError_name @@ var "e",
    string " does not match expected ",
    ShowUtil.caseConvention @@ (project _InvalidDefinitionNameError _InvalidDefinitionNameError_expectedConvention @@ var "e")]

invalidModuleError :: TTermDefinition (InvalidModuleError -> String)
invalidModuleError = define "invalidModuleError" $
  doc "Show an invalid module error as a string" $
  "e" ~> Strings.cat2 (string "invalid module: ") $
    cases _InvalidModuleError (var "e") Nothing [
      _InvalidModuleError_conflictingVariantName>>: conflictingVariantNameError,
      _InvalidModuleError_definitionNotInModuleNamespace>>: definitionNotInModuleNamespaceError,
      _InvalidModuleError_definitionsOutOfOrder>>: definitionsOutOfOrderError,
      _InvalidModuleError_duplicateDefinitionName>>: duplicateDefinitionNameError,
      _InvalidModuleError_invalidDefinitionName>>: invalidDefinitionNameError,
      _InvalidModuleError_invalidNamespaceConvention>>: invalidNamespaceConventionError,
      _InvalidModuleError_missingDocumentation>>: missingDocumentationError]

invalidNamespaceConventionError :: TTermDefinition (InvalidNamespaceConventionError -> String)
invalidNamespaceConventionError = define "invalidNamespaceConventionError" $
  doc "Show an invalid namespace convention error as a string" $
  "e" ~> Strings.cat $ list [
    string "namespace ",
    Packaging.unNamespace $ project _InvalidNamespaceConventionError _InvalidNamespaceConventionError_namespace @@ var "e",
    string " does not match the dotted-camelCase naming convention"]

invalidPackageError :: TTermDefinition (InvalidPackageError -> String)
invalidPackageError = define "invalidPackageError" $
  doc "Show an invalid package error as a string" $
  "e" ~> Strings.cat2 (string "invalid package: ") $
    cases _InvalidPackageError (var "e") Nothing [
      _InvalidPackageError_conflictingModuleNamespace>>: conflictingModuleNamespaceError,
      _InvalidPackageError_duplicateModuleNamespace>>: duplicateModuleNamespaceError,
      _InvalidPackageError_invalidModule>>: invalidModuleError,
      _InvalidPackageError_invalidPackageName>>: invalidPackageNameError]

invalidPackageNameError :: TTermDefinition (InvalidPackageNameError -> String)
invalidPackageNameError = define "invalidPackageNameError" $
  doc "Show an invalid package name error as a string" $
  "e" ~> Strings.cat $ list [
    string "package name ",
    Packaging.unPackageName $ project _InvalidPackageNameError _InvalidPackageNameError_packageName @@ var "e",
    string " does not match the hyphen-separated lowercase naming convention"]

missingDocumentationError :: TTermDefinition (MissingDocumentationError -> String)
missingDocumentationError = define "missingDocumentationError" $
  doc "Show a missing documentation error as a string" $
  "e" ~> Strings.cat $ list [
    string "in module ",
    Packaging.unNamespace $ project _MissingDocumentationError _MissingDocumentationError_namespace @@ var "e",
    string ": definition ",
    Core.unName $ project _MissingDocumentationError _MissingDocumentationError_name @@ var "e",
    string " lacks a description annotation"]
