module Hydra.Sources.Kernel.Terms.Validate.Packaging where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import Hydra.Error.Packaging (InvalidModuleError, InvalidPackageError)
import Hydra.Packaging (Package)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Error.Packaging       as ErrorPackaging
import qualified Hydra.Dsl.Meta.Core             as Core
import qualified Hydra.Dsl.Meta.Lib.Equality     as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists        as Lists
import qualified Hydra.Dsl.Meta.Lib.Logic        as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps         as Maps
import qualified Hydra.Dsl.Meta.Lib.Maybes       as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs        as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets         as Sets
import qualified Hydra.Dsl.Meta.Lib.Strings      as Strings
import qualified Hydra.Dsl.Packaging                as Packaging
import qualified Hydra.Dsl.Packaging             as Packaging
import           Hydra.Dsl.Meta.Phantoms         as Phantoms
import           Hydra.Sources.Kernel.Types.All
import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting
import qualified Hydra.Sources.Kernel.Terms.Names      as Names
import           Prelude hiding ((++))
import qualified Data.Set                        as S


ns :: Namespace
ns = Namespace "hydra.validate.packaging"

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleTermDependencies = [Formatting.ns, Names.ns],
            moduleTypeDependencies = kernelTypesNamespaces,
            moduleDescription = Just "Validation functions for modules and packages"}
  where
    definitions = [
      toDefinition checkConflictingModuleNamespaces,
      toDefinition checkConflictingVariantNames,
      toDefinition checkDefinitionNamespaces,
      toDefinition checkDuplicateDefinitionNames,
      toDefinition checkDuplicateModuleNamespaces,
      toDefinition definitionName,
      toDefinition module',
      toDefinition package]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

-- | Extract the name from a Definition (term or type)
definitionName :: TTermDefinition (Definition -> Name)
definitionName = define "definitionName" $
  doc "Extract the name from a definition" $
  "def" ~>
  cases _Definition (var "def") Nothing [
    _Definition_term>>: "td" ~> Packaging.termDefinitionName (var "td"),
    _Definition_type>>: "td" ~> Packaging.typeDefinitionName (var "td")]

-- | Check for module namespaces that would conflict when mapped to a target language's
-- directory structure. Two namespaces conflict if they are identical when lowercased,
-- e.g. hydra.fooBar and hydra.foobar, or hydra.Foo.Bar and hydra.foo.bar.
-- Fails on the first conflict found.
checkConflictingModuleNamespaces :: TTermDefinition (Package -> Maybe InvalidPackageError)
checkConflictingModuleNamespaces = define "checkConflictingModuleNamespaces" $
  doc "Check for module namespaces that conflict when mapped to target language paths" $
  "pkg" ~>
  -- Build a map from lowercased namespace strings to original namespaces.
  -- If a lowercased version is already in the map, that's a conflict.
  "result" <~ Lists.foldl
    ("acc" ~> "mod" ~>
      "seen" <~ Pairs.first (var "acc") $
      "err" <~ Pairs.second (var "acc") $
      Maybes.cases (var "err")
        ("ns" <~ Packaging.moduleNamespace (var "mod") $
          "key" <~ Strings.toLower (Packaging.unNamespace $ var "ns") $
          "existing" <~ Maps.lookup (var "key") (var "seen") $
          Maybes.cases (var "existing")
            -- No conflict: add to map
            (pair (Maps.insert (var "key") (var "ns") (var "seen")) nothing)
            -- Conflict found
            ("first" ~>
              pair (var "seen") (just $
                ErrorPackaging.invalidPackageErrorConflictingModuleNamespace $
                  ErrorPackaging.conflictingModuleNamespaceError (var "first") (var "ns"))))
        (constant $ var "acc"))
    (pair Maps.empty nothing)
    (Packaging.packageModules $ var "pkg") $
  Pairs.second (var "result")

-- | Check for union variant names that conflict with type definition names.
-- For each union type, the capitalized type local name concatenated with
-- the capitalized variant field name must not collide with any other type
-- definition's local name in the same module.
checkConflictingVariantNames :: TTermDefinition (Module -> Maybe InvalidModuleError)
checkConflictingVariantNames = define "checkConflictingVariantNames" $
  doc "Check for union variant names that, when mapped to constructor names, conflict with other type definitions" $
  "mod" ~>
  "ns" <~ Packaging.moduleNamespace (var "mod") $
  "defs" <~ Packaging.moduleDefinitions (var "mod") $
  -- Collect all definition local names into a set
  "defNames" <~ Lists.foldl
    ("acc" ~> "def" ~>
      Sets.insert (Names.localNameOf @@ (definitionName @@ var "def")) (var "acc"))
    Sets.empty
    (var "defs") $
  -- For each type definition that is a union, check each field
  Lists.foldl
    ("acc" ~> "def" ~>
      Maybes.cases (var "acc")
        (cases _Definition (var "def") (Just nothing) [
          _Definition_type>>: "td" ~>
            "typeName" <~ Packaging.typeDefinitionName (var "td") $
            "localTypeName" <~ (Names.localNameOf @@ var "typeName") $
            "typ" <~ (Core.typeSchemeBody $ Packaging.typeDefinitionTypeScheme (var "td")) $
            cases _Type (var "typ") (Just nothing) [
              _Type_union>>: "fields" ~>
                -- Check each field of the union
                Lists.foldl
                  ("innerAcc" ~> "field" ~>
                    Maybes.cases (var "innerAcc")
                      ("fieldName" <~ Core.fieldTypeName (var "field") $
                        "localFieldName" <~ (Names.localNameOf @@ var "fieldName") $
                        "constructorName" <~ Strings.cat2
                          (Formatting.capitalize @@ var "localTypeName")
                          (Formatting.capitalize @@ var "localFieldName") $
                        Logic.ifElse (Sets.member (var "constructorName") (var "defNames"))
                          (just $ ErrorPackaging.invalidModuleErrorConflictingVariantName $
                            ErrorPackaging.conflictingVariantNameError
                              (var "ns")
                              (var "typeName")
                              (var "fieldName")
                              (Core.name $ var "constructorName"))
                          nothing)
                      (constant $ var "innerAcc"))
                  nothing
                  (var "fields")]])
        (constant $ var "acc"))
    nothing
    (var "defs")

-- | Check that all definition names in a module have the module's namespace as a prefix.
-- For a module with namespace foo.bar, every definition name must have the form foo.bar.xyz.
-- Fails on the first definition that violates this constraint.
checkDefinitionNamespaces :: TTermDefinition (Module -> Maybe InvalidModuleError)
checkDefinitionNamespaces = define "checkDefinitionNamespaces" $
  doc "Check that all definition names in a module have the module's namespace as a prefix" $
  "mod" ~>
  "ns" <~ Packaging.moduleNamespace (var "mod") $
  "prefix" <~ (Strings.cat2 (Packaging.unNamespace $ var "ns") (string ".")) $
  "prefixLen" <~ Strings.length (var "prefix") $
  Lists.foldl
    ("acc" ~> "def" ~>
      Maybes.cases (var "acc")
        -- No error yet: check this definition
        ("name" <~ (definitionName @@ var "def") $
          "nameStr" <~ Core.unName (var "name") $
          "namePrefix" <~ Lists.take (var "prefixLen") (Strings.toList $ var "nameStr") $
          Logic.ifElse (Equality.equal (Strings.fromList $ var "namePrefix") (var "prefix"))
            nothing
            (just $ ErrorPackaging.invalidModuleErrorDefinitionNotInModuleNamespace $
              ErrorPackaging.definitionNotInModuleNamespaceError (var "ns") (var "name")))
        -- Already have an error: stop
        (constant $ var "acc"))
    nothing
    (Packaging.moduleDefinitions $ var "mod")

-- | Check for duplicate definition names in a module.
-- Fails on the first duplicate found.
checkDuplicateDefinitionNames :: TTermDefinition (Module -> Maybe InvalidModuleError)
checkDuplicateDefinitionNames = define "checkDuplicateDefinitionNames" $
  doc "Check for duplicate definition names in a module" $
  "mod" ~>
  "ns" <~ Packaging.moduleNamespace (var "mod") $
  -- Fold through definitions tracking seen names in a set.
  -- Accumulator is (Set Name, Maybe InvalidModuleError).
  "result" <~ Lists.foldl
    ("acc" ~> "def" ~>
      "seen" <~ Pairs.first (var "acc") $
      "err" <~ Pairs.second (var "acc") $
      Maybes.cases (var "err")
        -- No error yet: check this definition
        ("name" <~ (definitionName @@ var "def") $
          Logic.ifElse (Sets.member (var "name") (var "seen"))
            (pair (var "seen") (just $
              ErrorPackaging.invalidModuleErrorDuplicateDefinitionName $
                ErrorPackaging.duplicateDefinitionNameError (var "ns") (var "name")))
            (pair (Sets.insert (var "name") (var "seen")) nothing))
        -- Already have an error: stop
        (constant $ var "acc"))
    (pair Sets.empty nothing)
    (Packaging.moduleDefinitions $ var "mod") $
  Pairs.second (var "result")

-- | Check for duplicate module namespaces in a package.
-- Fails on the first duplicate found.
checkDuplicateModuleNamespaces :: TTermDefinition (Package -> Maybe InvalidPackageError)
checkDuplicateModuleNamespaces = define "checkDuplicateModuleNamespaces" $
  doc "Check for duplicate module namespaces in a package" $
  "pkg" ~>
  "result" <~ Lists.foldl
    ("acc" ~> "mod" ~>
      "seen" <~ Pairs.first (var "acc") $
      "err" <~ Pairs.second (var "acc") $
      Maybes.cases (var "err")
        ("ns" <~ Packaging.moduleNamespace (var "mod") $
          Logic.ifElse (Sets.member (var "ns") (var "seen"))
            (pair (var "seen") (just $
              ErrorPackaging.invalidPackageErrorDuplicateModuleNamespace $
                ErrorPackaging.duplicateModuleNamespaceError (var "ns")))
            (pair (Sets.insert (var "ns") (var "seen")) nothing))
        (constant $ var "acc"))
    (pair Sets.empty nothing)
    (Packaging.packageModules $ var "pkg") $
  Pairs.second (var "result")

-- | Validate a module, returning the first error found or nothing if valid.
-- Checks are run in order; stops at the first failure.
module' :: TTermDefinition (Module -> Maybe InvalidModuleError)
module' = define "module" $
  doc "Validate a module, returning the first error found or nothing if valid" $
  "mod" ~>
  "r1" <~ (checkDefinitionNamespaces @@ var "mod") $
  Maybes.cases (var "r1")
    ("r2" <~ (checkDuplicateDefinitionNames @@ var "mod") $
      Maybes.cases (var "r2")
        (checkConflictingVariantNames @@ var "mod")
        (constant $ var "r2"))
    (constant $ var "r1")

-- | Validate a package, returning the first error found or nothing if valid.
-- Package-level checks run first, then each module is validated in order.
-- Stops at the first failure.
package :: TTermDefinition (Package -> Maybe InvalidPackageError)
package = define "package" $
  doc "Validate a package, returning the first error found or nothing if valid" $
  "pkg" ~>
  "r1" <~ (checkDuplicateModuleNamespaces @@ var "pkg") $
  Maybes.cases (var "r1")
    ("r2" <~ (checkConflictingModuleNamespaces @@ var "pkg") $
      Maybes.cases (var "r2")
        -- No package-level errors: validate each module
        (Lists.foldl
      ("acc" ~> "mod" ~>
        Maybes.cases (var "acc")
          (Maybes.map
            ("err" ~> ErrorPackaging.invalidPackageErrorInvalidModule (var "err"))
            (module' @@ var "mod"))
          (constant $ var "acc"))
          nothing
          (Packaging.packageModules $ var "pkg"))
        -- Conflicting namespace error found: stop
        (constant $ var "r2"))
    -- Duplicate namespace error found: stop
    (constant $ var "r1")
