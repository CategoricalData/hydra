module Hydra.Sources.Kernel.Terms.Validate.Packaging where

-- Standard imports for kernel terms modules
import Hydra.Kernel
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
import qualified Hydra.Dsl.Module                as Module
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
module_ = Module ns elements
    [Formatting.ns, Names.ns]
    kernelTypesNamespaces $
    Just "Validation functions for modules and packages"
  where
    elements = [
      toTermDefinition checkConflictingVariantNames,
      toTermDefinition checkDefinitionNamespaces,
      toTermDefinition checkDuplicateDefinitionNames,
      toTermDefinition checkDuplicateModuleNamespaces,
      toTermDefinition definitionName,
      toTermDefinition module',
      toTermDefinition package]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

-- | Extract the name from a Definition (term or type)
definitionName :: TBinding (Definition -> Name)
definitionName = define "definitionName" $
  doc "Extract the name from a definition" $
  "def" ~>
  cases _Definition (var "def") Nothing [
    _Definition_term>>: "td" ~> Module.termDefinitionName (var "td"),
    _Definition_type>>: "td" ~> Module.typeDefinitionName (var "td")]

-- | Check for union variant names that conflict with type definition names.
-- For each union type, the capitalized type local name concatenated with
-- the capitalized variant field name must not collide with any other type
-- definition's local name in the same module.
checkConflictingVariantNames :: TBinding (Module -> Maybe InvalidModuleError)
checkConflictingVariantNames = define "checkConflictingVariantNames" $
  doc "Check for union variant names that, when mapped to constructor names, conflict with other type definitions" $
  "mod" ~>
  "ns" <~ Module.moduleNamespace (var "mod") $
  "defs" <~ Module.moduleDefinitions (var "mod") $
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
            "typeName" <~ Module.typeDefinitionName (var "td") $
            "localTypeName" <~ (Names.localNameOf @@ var "typeName") $
            "typ" <~ Module.typeDefinitionType (var "td") $
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
checkDefinitionNamespaces :: TBinding (Module -> Maybe InvalidModuleError)
checkDefinitionNamespaces = define "checkDefinitionNamespaces" $
  doc "Check that all definition names in a module have the module's namespace as a prefix" $
  "mod" ~>
  "ns" <~ Module.moduleNamespace (var "mod") $
  "prefix" <~ (Strings.cat2 (Module.unNamespace $ var "ns") (string ".")) $
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
    (Module.moduleDefinitions $ var "mod")

-- | Check for duplicate definition names in a module.
-- Fails on the first duplicate found.
checkDuplicateDefinitionNames :: TBinding (Module -> Maybe InvalidModuleError)
checkDuplicateDefinitionNames = define "checkDuplicateDefinitionNames" $
  doc "Check for duplicate definition names in a module" $
  "mod" ~>
  "ns" <~ Module.moduleNamespace (var "mod") $
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
    (Module.moduleDefinitions $ var "mod") $
  Pairs.second (var "result")

-- | Check for duplicate module namespaces in a package.
-- Fails on the first duplicate found.
checkDuplicateModuleNamespaces :: TBinding (Package -> Maybe InvalidPackageError)
checkDuplicateModuleNamespaces = define "checkDuplicateModuleNamespaces" $
  doc "Check for duplicate module namespaces in a package" $
  "pkg" ~>
  "result" <~ Lists.foldl
    ("acc" ~> "mod" ~>
      "seen" <~ Pairs.first (var "acc") $
      "err" <~ Pairs.second (var "acc") $
      Maybes.cases (var "err")
        ("ns" <~ Module.moduleNamespace (var "mod") $
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
module' :: TBinding (Module -> Maybe InvalidModuleError)
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
package :: TBinding (Package -> Maybe InvalidPackageError)
package = define "package" $
  doc "Validate a package, returning the first error found or nothing if valid" $
  "pkg" ~>
  "r1" <~ (checkDuplicateModuleNamespaces @@ var "pkg") $
  Maybes.cases (var "r1")
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
    -- Package-level error found: stop
    (constant $ var "r1")
