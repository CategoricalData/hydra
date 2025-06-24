-- | Hydra's tier-1 modules build upon hydra.core by adding type definitions for graphs, modules, flows,
--   and other essential Hydra constructs, as well as programming logic which depends on hydra.core.
module Hydra.Sources.Tier1.All(
  module Hydra.Sources.Tier0.Core,
  module Hydra.Sources.Tier1.All,
  module Hydra.Sources.Tier1.Ast,
  module Hydra.Sources.Tier1.Coders,
  module Hydra.Sources.Tier1.Compute,
  module Hydra.Sources.Tier1.Constants,
  module Hydra.Sources.Tier1.Constraints,
  module Hydra.Sources.Tier1.Encode.Core,
  module Hydra.Sources.Tier1.Decode,
  module Hydra.Sources.Tier1.Formatting,
  module Hydra.Sources.Tier1.Functions,
  module Hydra.Sources.Tier1.Grammar,
  module Hydra.Sources.Tier1.Graph,
  module Hydra.Sources.Tier1.Json,
  module Hydra.Sources.Tier1.Literals,
  module Hydra.Sources.Tier1.Mantle,
  module Hydra.Sources.Tier1.Messages,
  module Hydra.Sources.Tier1.Module,
  module Hydra.Sources.Tier1.Phantoms,
  module Hydra.Sources.Tier1.Query,
  module Hydra.Sources.Tier1.Strip,
  module Hydra.Sources.Tier1.Testing,
  module Hydra.Sources.Tier1.Topology,
  module Hydra.Sources.Tier1.Typing,
  module Hydra.Sources.Tier1.Workflow,
) where

import Hydra.Sources.Tier0.Core
import Hydra.Sources.Tier1.Ast
import Hydra.Sources.Tier1.Coders
import Hydra.Sources.Tier1.Compute
import Hydra.Sources.Tier1.Constants
import Hydra.Sources.Tier1.Constraints
import Hydra.Sources.Tier1.Encode.Core hiding (ref)
import Hydra.Sources.Tier1.Decode
import Hydra.Sources.Tier1.Formatting
import Hydra.Sources.Tier1.Functions
import Hydra.Sources.Tier1.Grammar
import Hydra.Sources.Tier1.Graph
import Hydra.Sources.Tier1.Json
import Hydra.Sources.Tier1.Literals
import Hydra.Sources.Tier1.Mantle
import Hydra.Sources.Tier1.Messages
import Hydra.Sources.Tier1.Module
import Hydra.Sources.Tier1.Phantoms
import Hydra.Sources.Tier1.Query
import Hydra.Sources.Tier1.Strip
import Hydra.Sources.Tier1.Testing
import Hydra.Sources.Tier1.Topology
import Hydra.Sources.Tier1.Typing
import Hydra.Sources.Tier1.Workflow

tier1Modules :: [Module]
tier1Modules = tier1TypeModules ++ tier1TermModules

tier1TermModules :: [Module]
tier1TermModules = [
  coreEncodingModule,
  hydraDecodeModule,
  hydraConstantsModule,
  hydraFormattingModule,
  hydraFunctionsModule,
  hydraLiteralsModule,
  hydraMessagesModule,
  hydraStripModule]

tier1TypeModules :: [Module]
tier1TypeModules = [
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
  hydraTestingModule,
  hydraTopologyModule,
  hydraTypingModule,
  hydraWorkflowModule,
  jsonModelModule]
