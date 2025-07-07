-- | All of Hydra's type-level kernel modules
module Hydra.Sources.Kernel.Types.All(
  module Hydra.Sources.Kernel.Types.Core,
  module Hydra.Sources.Kernel.Types.All,
  module Hydra.Sources.Kernel.Types.Accessors,
  module Hydra.Sources.Kernel.Types.Ast,
  module Hydra.Sources.Kernel.Types.Coders,
  module Hydra.Sources.Kernel.Types.Compute,
  module Hydra.Sources.Kernel.Types.Constraints,
  module Hydra.Sources.Kernel.Types.Grammar,
  module Hydra.Sources.Kernel.Types.Graph,
  module Hydra.Sources.Kernel.Types.Json,
  module Hydra.Sources.Kernel.Types.Mantle,
  module Hydra.Sources.Kernel.Types.Module,
  module Hydra.Sources.Kernel.Types.Phantoms,
  module Hydra.Sources.Kernel.Types.Query,
  module Hydra.Sources.Kernel.Types.Relational,
  module Hydra.Sources.Kernel.Types.Tabular,
  module Hydra.Sources.Kernel.Types.Testing,
  module Hydra.Sources.Kernel.Types.Topology,
  module Hydra.Sources.Kernel.Types.Typing,
  module Hydra.Sources.Kernel.Types.Workflow,
) where

import Hydra.Sources.Kernel.Types.Core
import Hydra.Sources.Kernel.Types.Accessors
import Hydra.Sources.Kernel.Types.Ast
import Hydra.Sources.Kernel.Types.Coders
import Hydra.Sources.Kernel.Types.Compute
import Hydra.Sources.Kernel.Types.Constraints
import Hydra.Sources.Kernel.Types.Grammar
import Hydra.Sources.Kernel.Types.Graph
import Hydra.Sources.Kernel.Types.Json
import Hydra.Sources.Kernel.Types.Mantle
import Hydra.Sources.Kernel.Types.Module
import Hydra.Sources.Kernel.Types.Phantoms
import Hydra.Sources.Kernel.Types.Relational
import Hydra.Sources.Kernel.Types.Query
import Hydra.Sources.Kernel.Types.Tabular
import Hydra.Sources.Kernel.Types.Testing
import Hydra.Sources.Kernel.Types.Topology
import Hydra.Sources.Kernel.Types.Typing
import Hydra.Sources.Kernel.Types.Workflow


kernelTypesModules :: [Module]
kernelTypesModules = [
  hydraAccessorsModule,
  hydraAstModule,
  hydraCodersModule,
  hydraComputeModule,
  hydraConstraintsModule,
  hydraGrammarModule,
  hydraGraphModule,
  hydraMantleModule,
  hydraModuleModule,
  hydraPhantomsModule,
  hydraQueryModule,
  hydraRelationalModule,
  hydraTabularModule,
  hydraTestingModule,
  hydraTopologyModule,
  hydraTypingModule,
  hydraWorkflowModule,
  jsonModelModule]
