-- | A module which provides a minimal typing environment for decoding other Modules from JSON.
--
-- This module contains a single binding, typesByName, which maps type Names to Types
-- for all types defined in the four kernel modules needed to decode the Module type:
-- hydra.core, hydra.compute, hydra.graph, and hydra.module.
--
-- By consolidating these types into a single map, we avoid the need to generate
-- individual Source modules for each kernel type module (which can exceed the JVM's
-- 64KB method size limit for large modules like hydra.core).

module Hydra.Sources.Json.Bootstrap where

-- Note: non-standard imports; this module is constructed dynamically from other modules.
import Hydra.Kernel
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Compute as Compute
import qualified Hydra.Sources.Kernel.Types.Graph as Graph
import qualified Hydra.Sources.Kernel.Types.Module as Module

import qualified Data.Map as M


ns :: Namespace
ns = Namespace "hydra.json.bootstrap"

-- | The four kernel type modules whose types are needed to decode Module from JSON.
bootstrapTypeModules :: [Module]
bootstrapTypeModules = [
  Core.module_,
  Compute.module_,
  Graph.module_,
  Module.module_]

module_ :: Module
module_ = Module ns [typesByNameBinding] [] [Namespace "hydra.core"] $
    Just "A module which provides a minimal typing environment for decoding other modules from JSON"

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
