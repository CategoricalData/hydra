
module Hydra.Sources.Kernel.Terms.Classes where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Typing            as Typing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.List                   as L


-- | The hydra.classes registry: term-level bindings, each a TypeClass value
-- describing one of Hydra's built-in type classes.
--
-- The binding's local name (e.g. "equality") is the marker used in
-- TypeVariableConstraints.classes :: Set Name. The binding's body provides a
-- human-readable description for tooling and documentation.
--
-- Adding a new built-in type class is just adding a binding here and updating
-- callers that need to construct or recognize the marker name.
ns :: ModuleName
ns = ModuleName "hydra.classes"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Registry of Hydra's built-in type classes.")}
  where
    definitions = [
      toDefinition equality,
      toDefinition ordering]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

equality :: TypedTermDefinition TypeClass
equality = define "equality" $
  doc "The equality type class: instances support structural equality." $
  Typing.typeClass (string "Equality: instances support structural equality.")

ordering :: TypedTermDefinition TypeClass
ordering = define "ordering" $
  doc "The ordering type class: instances support total ordering (and equality)." $
  Typing.typeClass (string "Ordering: instances support total ordering (and equality).")
