module Hydra.Sources.Kernel.Types.Util where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
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
    -- Note: either_ and pair are NOT included here because they correspond to built-in
    -- type constructors (TypeEither, TypePair) which are handled natively by all target languages.
    -- Including them would cause conflicts with Haskell's Prelude.Either.
    -- Note: adapter, bicoder, and coder have been moved to hydra.coders.
    definitions = [
      caseConvention,
      comparison,
      fileExtension,
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

either_ :: TypeDefinition
either_ = define "Either" $
  doc "A named union type equivalent to the built-in Either type constructor, for use in languages that lack anonymous sum types" $
  T.forAlls ["a", "b"] $ T.union [
    "left">:
      doc "The left alternative"
      (T.var "a"),
    "right">:
      doc "The right alternative"
      (T.var "b")]

fileExtension :: TypeDefinition
fileExtension = define "FileExtension" $
  doc "A file extension (without the dot), e.g. \"json\" or \"py\"" $
  T.wrap T.string

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

pair :: TypeDefinition
pair = define "Pair" $
  doc "A named record type equivalent to the built-in Pair type constructor, for use in languages that lack anonymous product types" $
  T.forAlls ["a", "b"] $ T.record [
    "first">:
      doc "The first component"
      (T.var "a"),
    "second">:
      doc "The second component"
      (T.var "b")]

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
      T.maybe Packaging.moduleNameDef,
    "local">:
      doc "The local name"
      T.string]
