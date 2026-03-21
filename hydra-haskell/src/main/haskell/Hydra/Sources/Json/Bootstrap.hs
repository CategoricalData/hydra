-- | A module which provides a minimal typing environment for decoding other Modules from JSON.
--
-- This module contains a single binding, typesByName, which maps type Names to Types
-- for all types defined in the kernel modules needed to decode the Module type
-- and to provide schema types for inference tests:
-- hydra.coders, hydra.context, hydra.core, hydra.error, hydra.graph, hydra.module, and hydra.util.
--
-- By consolidating these types into a single map, we avoid the need to generate
-- individual Source modules for each kernel type module (which can exceed the JVM's
-- 64KB method size limit for large modules like hydra.core).

module Hydra.Sources.Json.Bootstrap where

-- Note: non-standard imports; this module is constructed dynamically from other modules.
import Hydra.Kernel
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Sources.Kernel.Types.Coders as Coders
import qualified Hydra.Sources.Kernel.Types.Context as Context
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Errors as Error
import qualified Hydra.Sources.Kernel.Types.Graph as Graph
import qualified Hydra.Sources.Kernel.Types.Module as Module
import qualified Hydra.Sources.Kernel.Types.Util as Util

import qualified Data.Map as M


ns :: Namespace
ns = Namespace "hydra.json.bootstrap"

-- | The kernel type modules whose types are needed to decode Module from JSON
-- and to provide schema types for inference tests.
bootstrapTypeModules :: [Module]
bootstrapTypeModules = [
  Coders.module_,
  Context.module_,
  Core.module_,
  Error.module_,
  Graph.module_,
  Module.module_,
  Util.module_]

module_ :: Module
module_ = Module ns [typesByNameBinding] [] [Namespace "hydra.core"] $
    Just ("A module which provides a minimal typing environment for decoding other modules from JSON."
      ++ " This avoids certain problems with generating entire source modules into target languages like Java,"
      ++ " which is subject to method size limits for large modules like hydra.core.")

typesByNameBinding :: Binding
typesByNameBinding = Binding {
    bindingName = Name "hydra.json.bootstrap.typesByName",
    bindingTerm = typesByNameTerm,
    bindingType = Nothing}

-- | Build a Term-level map from Name to Type, by extracting all bindings
-- from the bootstrap type modules. Each binding in a kernel type module
-- is a type definition whose bindingTerm is the type encoded as a Term.
typesByNameTerm :: Term
typesByNameTerm = TermMap $ M.fromList entries
  where
    entries = concatMap moduleEntries bootstrapTypeModules
    moduleEntries mod = fmap bindingEntry (moduleElements mod)
    bindingEntry b = (EncodeCore.name (bindingName b), bindingTerm b)
