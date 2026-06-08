module Hydra.Sources.Kernel.Types.Variants where

-- Standard type-level kernel imports
import           Hydra.Kernel hiding (literalVariant, termVariant, typeVariant)
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: ModuleName
ns = ModuleName "hydra.variants"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleMetadata = descriptionMetadata (Just "Variant types which describe the structure of Hydra core types and terms.")}
  where
    definitions = [
      literalVariant,
      termVariant,
      typeVariant]

literalVariant :: TypeDefinition
literalVariant = define "LiteralVariant" $
  doc "The identifier of a literal constructor" $
  T.enum [
    "binary",
    "boolean",
    "decimal",
    "float",
    "integer",
    "string"]

termVariant :: TypeDefinition
termVariant = define "TermVariant" $
  doc "The identifier of a term expression constructor" $
  T.enum [
    "annotated",
    "application",
    "cases",
    "either",
    "inject",
    "lambda",
    "let",
    "list",
    "literal",
    "map",
    "optional",
    "pair",
    "project",
    "record",
    "set",
    "typeApplication",
    "typeLambda",
    "unit",
    "unwrap",
    "variable",
    "wrap"]

typeVariant :: TypeDefinition
typeVariant = define "TypeVariant" $
  doc "The identifier of a type constructor" $
  T.enum [
    "annotated",
    "application",
    "either",
    "forall",
    "function",
    "list",
    "literal",
    "map",
    "optional",
    "pair",
    "record",
    "set",
    "union",
    "unit",
    "variable",
    "void",
    "wrap"]
