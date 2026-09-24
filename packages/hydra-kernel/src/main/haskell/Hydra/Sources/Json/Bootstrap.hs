-- | A module which provides a minimal typing environment for decoding other Modules from JSON.
--
-- This module contains a single binding, typesByName, which maps type Names to Types
-- for all types defined in the kernel modules needed to decode the Module type
-- and to provide schema types for inference tests:
-- hydra.core.coders, hydra.core.model, hydra.core.error, hydra.core.graph, hydra.core.packaging, and hydra.core.util.
--
-- By consolidating these types into a single map, we avoid the need to generate
-- individual Source modules for each kernel type module (which can exceed the JVM's
-- 64KB method size limit for large modules like hydra.core.model).

module Hydra.Sources.Json.Bootstrap where

-- Note: non-standard imports; this module is constructed dynamically from other modules.
import Hydra.Kernel
import           Hydra.Core.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import qualified Hydra.Core.Encode.Model as EncodeCore
import qualified Hydra.Sources.Kernel.Types.Coders as Coders
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Errors as Error
import qualified Hydra.Sources.Kernel.Types.Graph as Graph
import qualified Hydra.Sources.Kernel.Types.Packaging as Packaging
import qualified Hydra.Sources.Kernel.Types.Typing as Typing
import qualified Hydra.Sources.Kernel.Types.Util as Util

import qualified Data.Map as M


ns :: ModuleName
ns = ModuleName "hydra.core.json.bootstrap"

-- | The kernel type modules whose types are needed to decode Module from JSON
-- and to provide schema types for inference tests.
bootstrapTypeModules :: [Module]
bootstrapTypeModules = [
  Coders.module_,
  Core.module_,
  Error.module_,
  Graph.module_,
  Packaging.module_,
  Typing.module_,
  Util.module_]

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = [DefinitionTerm typesByNameDefinition],
            moduleDependencies = unqualifiedDep <$> [ModuleName "hydra.core.coders", ModuleName "hydra.core.model", ModuleName "hydra.core.error.checking", ModuleName "hydra.core.error.model", ModuleName "hydra.core.errors", ModuleName "hydra.core.graph", ModuleName "hydra.core.packaging", ModuleName "hydra.core.paths", ModuleName "hydra.core.typing", ModuleName "hydra.core.util", ModuleName "hydra.core.variants"],
            moduleMetadata = descriptionMetadata (Just ("A module which provides a minimal typing environment for decoding other modules from JSON."
      ++ " This avoids certain problems with generating entire source modules into target languages like Java,"
      ++ " which is subject to method size limits for large modules like hydra.core.model."))}
typesByNameDefinition :: TermDefinition
typesByNameDefinition = TermDefinition {
    termDefinitionName = Name "hydra.core.json.bootstrap.typesByName",
    termDefinitionMetadata = Nothing,
    termDefinitionBody = typesByNameTerm,
    termDefinitionSignature = Nothing}

-- | Build a Term-level map from Name to Type, by extracting all type
-- definitions from the bootstrap type modules. Each type definition's
-- type scheme body is encoded as a Term keyed by the definition's name,
-- and tagged with a "type" annotation pointing at hydra.core.model.Type so that
-- consumers can recognize the value as an encoded type.
typesByNameTerm :: Term
typesByNameTerm = TermAnnotated $ AnnotatedTerm {
    annotatedTermBody = TermMap $ M.fromList entries,
    annotatedTermAnnotation = TermMap $ M.fromList [
      (TermVariable (Name "description"), TermLiteral $ LiteralString
        ("A bootstrap typing environment for decoding modules from JSON."
          ++ " Maps each kernel type name to its encoded type, used to seed JSON"
          ++ " decoding before the full kernel graph is available."))]}
  where
    entries = concatMap moduleEntries bootstrapTypeModules
    moduleEntries mod = [defEntry td | DefinitionType td <- moduleDefinitions mod]
    defEntry td = (
      EncodeCore.name (typeDefinitionName td),
      annotateAsType (EncodeCore.type_ (typeSchemeBody (typeDefinitionBody td))))
    annotateAsType encoded = TermAnnotated $ AnnotatedTerm {
      annotatedTermBody = encoded,
      annotatedTermAnnotation = TermMap $ M.fromList [
        (TermVariable (Name "type"), TermVariable (Name "hydra.core.model.Type"))]}
