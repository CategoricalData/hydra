-- Note: this is an automatically generated file. Do not edit.

-- | Validation functions for modules and packages

module Hydra.Validate.Packaging where

import qualified Hydra.Core as Core
import qualified Hydra.Error.Packaging as Packaging
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging_
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | Check for module namespaces that conflict when mapped to target language paths
checkConflictingModuleNamespaces :: Packaging_.Package -> Maybe Packaging.InvalidPackageError
checkConflictingModuleNamespaces pkg =

      let result =
              Lists.foldl (\acc -> \mod ->
                let seen = Pairs.first acc
                    err = Pairs.second acc
                in (Maybes.cases err (
                  let ns = Packaging_.moduleNamespace mod
                      key = Strings.toLower (Packaging_.unNamespace ns)
                      existing = Maps.lookup key seen
                  in (Maybes.cases existing (Maps.insert key ns seen, Nothing) (\first -> (seen, (Just (Packaging.InvalidPackageErrorConflictingModuleNamespace (Packaging.ConflictingModuleNamespaceError {
                    Packaging.conflictingModuleNamespaceErrorFirst = first,
                    Packaging.conflictingModuleNamespaceErrorSecond = ns}))))))) (\_ -> acc))) (Maps.empty, Nothing) (Packaging_.packageModules pkg)
      in (Pairs.second result)

-- | Check for union variant names that, when mapped to constructor names, conflict with other type definitions
checkConflictingVariantNames :: Packaging_.Module -> Maybe Packaging.InvalidModuleError
checkConflictingVariantNames mod =

      let ns = Packaging_.moduleNamespace mod
          defs = Packaging_.moduleDefinitions mod
          defNames = Lists.foldl (\acc -> \def -> Sets.insert (Names.localNameOf (definitionName def)) acc) Sets.empty defs
      in (Lists.foldl (\acc -> \def -> Maybes.cases acc (case def of
        Packaging_.DefinitionType v0 ->
          let typeName = Packaging_.typeDefinitionName v0
              localTypeName = Names.localNameOf typeName
              typ = Core.typeSchemeType (Packaging_.typeDefinitionType v0)
          in case typ of
            Core.TypeUnion v1 -> Lists.foldl (\innerAcc -> \field -> Maybes.cases innerAcc (
              let fieldName = Core.fieldTypeName field
                  localFieldName = Names.localNameOf fieldName
                  constructorName = Strings.cat2 (Formatting.capitalize localTypeName) (Formatting.capitalize localFieldName)
              in (Logic.ifElse (Sets.member constructorName defNames) (Just (Packaging.InvalidModuleErrorConflictingVariantName (Packaging.ConflictingVariantNameError {
                Packaging.conflictingVariantNameErrorNamespace = ns,
                Packaging.conflictingVariantNameErrorTypeName = typeName,
                Packaging.conflictingVariantNameErrorVariantName = fieldName,
                Packaging.conflictingVariantNameErrorConflictingName = (Core.Name constructorName)}))) Nothing)) (\_ -> innerAcc)) Nothing v1
            _ -> Nothing
        _ -> Nothing) (\_ -> acc)) Nothing defs)

-- | Check that all definition names in a module have the module's namespace as a prefix
checkDefinitionNamespaces :: Packaging_.Module -> Maybe Packaging.InvalidModuleError
checkDefinitionNamespaces mod =

      let ns = Packaging_.moduleNamespace mod
          prefix = Strings.cat2 (Packaging_.unNamespace ns) "."
          prefixLen = Strings.length prefix
      in (Lists.foldl (\acc -> \def -> Maybes.cases acc (
        let name = definitionName def
            nameStr = Core.unName name
            namePrefix = Lists.take prefixLen (Strings.toList nameStr)
        in (Logic.ifElse (Equality.equal (Strings.fromList namePrefix) prefix) Nothing (Just (Packaging.InvalidModuleErrorDefinitionNotInModuleNamespace (Packaging.DefinitionNotInModuleNamespaceError {
          Packaging.definitionNotInModuleNamespaceErrorNamespace = ns,
          Packaging.definitionNotInModuleNamespaceErrorName = name}))))) (\_ -> acc)) Nothing (Packaging_.moduleDefinitions mod))

-- | Check for duplicate definition names in a module
checkDuplicateDefinitionNames :: Packaging_.Module -> Maybe Packaging.InvalidModuleError
checkDuplicateDefinitionNames mod =

      let ns = Packaging_.moduleNamespace mod
          result =
                  Lists.foldl (\acc -> \def ->
                    let seen = Pairs.first acc
                        err = Pairs.second acc
                    in (Maybes.cases err (
                      let name = definitionName def
                      in (Logic.ifElse (Sets.member name seen) (seen, (Just (Packaging.InvalidModuleErrorDuplicateDefinitionName (Packaging.DuplicateDefinitionNameError {
                        Packaging.duplicateDefinitionNameErrorNamespace = ns,
                        Packaging.duplicateDefinitionNameErrorName = name})))) (Sets.insert name seen, Nothing))) (\_ -> acc))) (Sets.empty, Nothing) (Packaging_.moduleDefinitions mod)
      in (Pairs.second result)

-- | Check for duplicate module namespaces in a package
checkDuplicateModuleNamespaces :: Packaging_.Package -> Maybe Packaging.InvalidPackageError
checkDuplicateModuleNamespaces pkg =

      let result =
              Lists.foldl (\acc -> \mod ->
                let seen = Pairs.first acc
                    err = Pairs.second acc
                in (Maybes.cases err (
                  let ns = Packaging_.moduleNamespace mod
                  in (Logic.ifElse (Sets.member ns seen) (seen, (Just (Packaging.InvalidPackageErrorDuplicateModuleNamespace (Packaging.DuplicateModuleNamespaceError {
                    Packaging.duplicateModuleNamespaceErrorNamespace = ns})))) (Sets.insert ns seen, Nothing))) (\_ -> acc))) (Sets.empty, Nothing) (Packaging_.packageModules pkg)
      in (Pairs.second result)

-- | Extract the name from a definition
definitionName :: Packaging_.Definition -> Core.Name
definitionName def =
    case def of
      Packaging_.DefinitionTerm v0 -> Packaging_.termDefinitionName v0
      Packaging_.DefinitionType v0 -> Packaging_.typeDefinitionName v0

-- | Validate a module, returning the first error found or nothing if valid
module_ :: Packaging_.Module -> Maybe Packaging.InvalidModuleError
module_ mod =

      let r1 = checkDefinitionNamespaces mod
      in (Maybes.cases r1 (
        let r2 = checkDuplicateDefinitionNames mod
        in (Maybes.cases r2 (checkConflictingVariantNames mod) (\_ -> r2))) (\_ -> r1))

-- | Validate a package, returning the first error found or nothing if valid
package :: Packaging_.Package -> Maybe Packaging.InvalidPackageError
package pkg =

      let r1 = checkDuplicateModuleNamespaces pkg
      in (Maybes.cases r1 (
        let r2 = checkConflictingModuleNamespaces pkg
        in (Maybes.cases r2 (Lists.foldl (\acc -> \mod -> Maybes.cases acc (Maybes.map (\err -> Packaging.InvalidPackageErrorInvalidModule err) (module_ mod)) (\_ -> acc)) Nothing (Packaging_.packageModules pkg)) (\_ -> r2))) (\_ -> r1))
