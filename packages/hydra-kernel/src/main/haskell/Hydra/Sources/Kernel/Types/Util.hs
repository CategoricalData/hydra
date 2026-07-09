module Hydra.Sources.Kernel.Types.Util where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations (doc)
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types ((>:))
import qualified Hydra.Overlay.Haskell.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Packaging as Packaging


ns :: ModuleName
ns = ModuleName "hydra.util"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns, Packaging.ns],
            moduleMetadata = descriptionMetadata (Just "General-purpose utility types used across Hydra.")}
  where
    -- Note: adapter, bicoder, and coder have been moved to hydra.coders.
    definitions = [
      caseConvention,
      comparison,
      moduleNames,
      precision,
      qualifiedName]

caseConvention :: TypeDefinition
caseConvention = define "CaseConvention" $
  doc "A naming convention for symbols, such as camelCase or snake_case" $
  T.enum ["camel", "pascal", "lowerSnake", "upperSnake"]

comparison :: TypeDefinition
comparison = define "Comparison" $
  doc "An equality judgement: less than, equal to, or greater than" $
  T.enum [
    "lessThan",
    "equalTo",
    "greaterThan"]

moduleNames :: TypeDefinition
moduleNames = define "ModuleNames" $
  doc "A mapping from module names to values of type n, with a focus on one module name" $
  T.forAll "n" $ T.record [
    "focus">:
      doc "The module name in focus, together with its associated value" $
      T.pair Packaging.moduleNameDef "n",
    "mapping">:
      doc "A mapping of module names to values" $
      T.map Packaging.moduleNameDef "n"]

precision :: TypeDefinition
precision = define "Precision" $
  doc "Numeric precision: arbitrary precision, or precision to a specified number of bits" $
  T.union [
    "arbitrary">:
      doc "Arbitrary precision" $
      T.unit,
    "bits">:
      doc "Precision to a specified number of bits" $
      T.int32]

qualifiedName :: TypeDefinition
qualifiedName = define "QualifiedName" $
  doc "A qualified name consisting of an optional module name together with a mandatory local name" $
  T.record [
    "moduleName">:
      doc "The optional module name" $
      T.optional Packaging.moduleNameDef,
    "local">:
      doc "The local name"
      T.string]
