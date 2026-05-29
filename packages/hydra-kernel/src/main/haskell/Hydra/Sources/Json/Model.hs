-- | A simple JSON model. This model is part of the Hydra kernel, despite JSON being an external language; JSON support is built in to Hydra

module Hydra.Sources.Json.Model where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: ModuleName
ns = ModuleName "hydra.json.model"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleDescription = Just "A JSON syntax model. See the BNF at https://www.json.org"}
  where
    definitions = [
      value]

value :: TypeDefinition
value = define "Value" $
  doc "A JSON value" $
  T.union [
    "array">:
      doc "A JSON array" $
      T.list value,
    "boolean">:
      doc "A boolean value" $
      T.boolean,
    "null">:
      doc "JSON's null value" $
      T.unit,
    "number">:
      doc "A numeric value" $
      T.decimal,
    "object">:
      doc "A JSON object as a set of key/value pairs" $
      T.map T.string value,
    "string">:
      doc "A string value" $
      T.string]
