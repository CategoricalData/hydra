module Hydra.Sources.Kernel.Types.Typed where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: ModuleName
ns = ModuleName "hydra.typed"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleMetadata = descriptionMetadata (Just "Typed (phantom) wrappers for use with Hydra DSLs")}
  where
    definitions = [
      typedBinding,
      typedTerm,
      typedTermDefinition]

typedBinding :: TypeDefinition
typedBinding = define "TypedBinding" $
  doc "An association of a named term (element) with a phantom type" $
  T.forAll "a" $ T.record [
    "name">:
      doc "The name of the term"
      Core.name,
    "term">:
      doc "The term with its phantom type" $
      typedTerm @@ "a"]

typedTerm :: TypeDefinition
typedTerm = define "TypedTerm" $
  doc "An association of a term with a phantom type" $
  T.forAll "a" $ T.wrap Core.term

typedTermDefinition :: TypeDefinition
typedTermDefinition = define "TypedTermDefinition" $
  doc "An association of a term definition with a phantom type" $
  T.forAll "a" $ T.record [
    "name">:
      doc "The name of the term"
      Core.name,
    "term">:
      doc "The term with its phantom type" $
      typedTerm @@ "a"]
