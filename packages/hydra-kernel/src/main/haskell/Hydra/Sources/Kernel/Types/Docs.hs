module Hydra.Sources.Kernel.Types.Docs where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations (doc)
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Overlay.Haskell.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Packaging as Packaging


ns :: ModuleName
ns = ModuleName "hydra.docs"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Packaging.ns],
            moduleMetadata = descriptionMetadata (Just "A model for Hydra documentation strings, including inline annotations and entity references")}
  where
    definitions = [
      docSegment]

docSegment :: TypeDefinition
docSegment = define "DocSegment" $
  doc ("A segment of a documentation string, either a raw text fragment or an inline entity reference."
    ++ " Documentation strings are parsed into a sequence of DocSegments for host-specific rendering.") $
  T.union [
    "ref">:
      doc "An inline reference to a Hydra entity"
      Packaging.entityReference,
    "text">:
      doc "A raw prose fragment, passed through verbatim"
      T.string]
