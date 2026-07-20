module Hydra.Sources.Kernel.Terms.Print.Error.Packaging where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core         as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Lib.Chars    as Chars
import qualified Hydra.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Maps     as Maps
import qualified Hydra.Dsl.Lib.Math     as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Lib.Sets     as Sets
import qualified Hydra.Dsl.Lib.Strings  as Strings
import qualified Hydra.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Base         as MetaBase
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms        as MetaTerms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import qualified Hydra.Dsl.Error.Packaging   as ErrorPackaging
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Tabular           as Tabular
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing      as Testing
import qualified Hydra.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Print.Util as PrintUtil
import Hydra.Error.Packaging


ns :: ModuleName
ns = ModuleName "hydra.print.error.packaging"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([PrintUtil.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "String representations of hydra.error.packaging types")}
  where
   definitions = [
     toDefinition conflictingModuleNameError,
     toDefinition conflictingVariantNameError,
     toDefinition definitionNotInModuleNameError,
     toDefinition definitionsOutOfOrderError,
     toDefinition duplicateDefinitionNameError,
     toDefinition duplicateModuleNameError,
     toDefinition invalidDefinitionNameError,
     toDefinition invalidModuleError,
     toDefinition invalidModuleNameConventionError,
     toDefinition invalidPackageError,
     toDefinition invalidPackageNameError,
     toDefinition missingDocumentationError,
     toDefinition undeclaredDependencyError]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

conflictingModuleNameError :: TypedTermDefinition (ConflictingModuleNameError -> String)
conflictingModuleNameError = define "conflictingModuleNameError" $
  doc "Show a conflicting module namespace error as a string" $
  "e" ~> Strings.cat $ list [
    string "module namespaces conflict when mapped to target language path: ",
    Packaging.unModuleName $ project _ConflictingModuleNameError _ConflictingModuleNameError_first @@ var "e",
    string " vs ",
    Packaging.unModuleName $ project _ConflictingModuleNameError _ConflictingModuleNameError_second @@ var "e"]

conflictingVariantNameError :: TypedTermDefinition (ConflictingVariantNameError -> String)
conflictingVariantNameError = define "conflictingVariantNameError" $
  doc "Show a conflicting variant name error as a string" $
  "e" ~> Strings.cat $ list [
    string "in module ",
    Packaging.unModuleName $ project _ConflictingVariantNameError _ConflictingVariantNameError_moduleName @@ var "e",
    string ": variant ",
    Core.unName $ project _ConflictingVariantNameError _ConflictingVariantNameError_variantName @@ var "e",
    string " of type ",
    Core.unName $ project _ConflictingVariantNameError _ConflictingVariantNameError_typeName @@ var "e",
    string " produces constructor name ",
    Core.unName $ project _ConflictingVariantNameError _ConflictingVariantNameError_conflictingName @@ var "e",
    string " which conflicts with another type definition"]

definitionNotInModuleNameError :: TypedTermDefinition (DefinitionNotInModuleNameError -> String)
definitionNotInModuleNameError = define "definitionNotInModuleNameError" $
  doc "Show a definition-not-in-module-namespace error as a string" $
  "e" ~> Strings.cat $ list [
    string "definition ",
    Core.unName $ project _DefinitionNotInModuleNameError _DefinitionNotInModuleNameError_name @@ var "e",
    string " is not in module namespace ",
    Packaging.unModuleName $ project _DefinitionNotInModuleNameError _DefinitionNotInModuleNameError_moduleName @@ var "e"]

definitionsOutOfOrderError :: TypedTermDefinition (DefinitionsOutOfOrderError -> String)
definitionsOutOfOrderError = define "definitionsOutOfOrderError" $
  doc "Show a definitions-out-of-order error as a string" $
  "e" ~> Strings.cat $ list [
    string "in module ",
    Packaging.unModuleName $ project _DefinitionsOutOfOrderError _DefinitionsOutOfOrderError_moduleName @@ var "e",
    string ": definitions out of order: ",
    Core.unName $ project _DefinitionsOutOfOrderError _DefinitionsOutOfOrderError_precedingName @@ var "e",
    string " precedes ",
    Core.unName $ project _DefinitionsOutOfOrderError _DefinitionsOutOfOrderError_followingName @@ var "e"]

duplicateDefinitionNameError :: TypedTermDefinition (DuplicateDefinitionNameError -> String)
duplicateDefinitionNameError = define "duplicateDefinitionNameError" $
  doc "Show a duplicate definition name error as a string" $
  "e" ~> Strings.cat $ list [
    string "in module ",
    Packaging.unModuleName $ project _DuplicateDefinitionNameError _DuplicateDefinitionNameError_moduleName @@ var "e",
    string ": duplicate definition name ",
    Core.unName $ project _DuplicateDefinitionNameError _DuplicateDefinitionNameError_name @@ var "e"]

duplicateModuleNameError :: TypedTermDefinition (DuplicateModuleNameError -> String)
duplicateModuleNameError = define "duplicateModuleNameError" $
  doc "Show a duplicate module namespace error as a string" $
  "e" ~> Strings.cat $ list [
    string "duplicate module namespace ",
    Packaging.unModuleName $ project _DuplicateModuleNameError _DuplicateModuleNameError_moduleName @@ var "e"]

invalidDefinitionNameError :: TypedTermDefinition (InvalidDefinitionNameError -> String)
invalidDefinitionNameError = define "invalidDefinitionNameError" $
  doc "Show an invalid definition name error as a string" $
  "e" ~> Strings.cat $ list [
    string "in module ",
    Packaging.unModuleName $ project _InvalidDefinitionNameError _InvalidDefinitionNameError_moduleName @@ var "e",
    string ": definition ",
    Core.unName $ project _InvalidDefinitionNameError _InvalidDefinitionNameError_name @@ var "e",
    string " does not match expected ",
    PrintUtil.caseConvention @@ (project _InvalidDefinitionNameError _InvalidDefinitionNameError_expectedConvention @@ var "e")]

invalidModuleError :: TypedTermDefinition (InvalidModuleError -> String)
invalidModuleError = define "invalidModuleError" $
  doc "Show an invalid module error as a string" $
  "e" ~> Strings.cat2 (string "invalid module: ") $
    cases _InvalidModuleError (var "e") Nothing [
      _InvalidModuleError_conflictingVariantName>>: conflictingVariantNameError,
      _InvalidModuleError_definitionNotInModuleName>>: definitionNotInModuleNameError,
      _InvalidModuleError_definitionsOutOfOrder>>: definitionsOutOfOrderError,
      _InvalidModuleError_duplicateDefinitionName>>: duplicateDefinitionNameError,
      _InvalidModuleError_invalidDefinitionName>>: invalidDefinitionNameError,
      _InvalidModuleError_invalidModuleNameConvention>>: invalidModuleNameConventionError,
      _InvalidModuleError_missingDocumentation>>: missingDocumentationError]

invalidModuleNameConventionError :: TypedTermDefinition (InvalidModuleNameConventionError -> String)
invalidModuleNameConventionError = define "invalidModuleNameConventionError" $
  doc "Show an invalid namespace convention error as a string" $
  "e" ~> Strings.cat $ list [
    string "namespace ",
    Packaging.unModuleName $ project _InvalidModuleNameConventionError _InvalidModuleNameConventionError_moduleName @@ var "e",
    string " does not match the dotted-camelCase naming convention"]

invalidPackageError :: TypedTermDefinition (InvalidPackageError -> String)
invalidPackageError = define "invalidPackageError" $
  doc "Show an invalid package error as a string" $
  "e" ~> Strings.cat2 (string "invalid package: ") $
    cases _InvalidPackageError (var "e") Nothing [
      _InvalidPackageError_conflictingModuleName>>: conflictingModuleNameError,
      _InvalidPackageError_duplicateModuleName>>: duplicateModuleNameError,
      _InvalidPackageError_invalidModule>>: invalidModuleError,
      _InvalidPackageError_invalidPackageName>>: invalidPackageNameError,
      _InvalidPackageError_undeclaredDependency>>: undeclaredDependencyError]

invalidPackageNameError :: TypedTermDefinition (InvalidPackageNameError -> String)
invalidPackageNameError = define "invalidPackageNameError" $
  doc "Show an invalid package name error as a string" $
  "e" ~> Strings.cat $ list [
    string "package name ",
    Packaging.unPackageName $ project _InvalidPackageNameError _InvalidPackageNameError_packageName @@ var "e",
    string " does not match the hyphen-separated lowercase naming convention"]

missingDocumentationError :: TypedTermDefinition (MissingDocumentationError -> String)
missingDocumentationError = define "missingDocumentationError" $
  doc "Show a missing documentation error as a string" $
  "e" ~> Strings.cat $ list [
    string "in module ",
    Packaging.unModuleName $ project _MissingDocumentationError _MissingDocumentationError_moduleName @@ var "e",
    string ": definition ",
    Core.unName $ project _MissingDocumentationError _MissingDocumentationError_name @@ var "e",
    string " lacks a description annotation"]

undeclaredDependencyError :: TypedTermDefinition (UndeclaredDependencyError -> String)
undeclaredDependencyError = define "undeclaredDependencyError" $
  doc "Show an undeclared dependency error as a string" $
  "e" ~> Strings.cat $ list [
    string "module ",
    Packaging.unModuleName $ project _UndeclaredDependencyError _UndeclaredDependencyError_moduleName @@ var "e",
    string " references ",
    Core.unName $ project _UndeclaredDependencyError _UndeclaredDependencyError_referencedName @@ var "e",
    string ", whose owning module ",
    Packaging.unModuleName $ project _UndeclaredDependencyError _UndeclaredDependencyError_owningModuleName @@ var "e",
    string " is not among its declared dependencies; add it to moduleDependencies"]
