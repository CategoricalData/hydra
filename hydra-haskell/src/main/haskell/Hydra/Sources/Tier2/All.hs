-- | Hydra's tier-2 sources add logic for flows (stateful operations), which are defined in tier-1,
--   as well as operations which depend on flows.
module Hydra.Sources.Tier2.All(
  module Hydra.Sources.Tier1.All,
  module Hydra.Sources.Tier2.All,
  module Hydra.Sources.Tier2.Accessors,
  module Hydra.Sources.Tier2.Annotations,
  module Hydra.Sources.Tier2.Arity,
  module Hydra.Sources.Tier2.CoreLanguage,
  module Hydra.Sources.Tier2.Errors,
--  module Hydra.Sources.Tier2.Expect,
  module Hydra.Sources.Tier2.Flows,
  module Hydra.Sources.Tier2.Lexical,
  module Hydra.Sources.Tier2.Printing,
  module Hydra.Sources.Tier2.Qnames,
  module Hydra.Sources.Tier2.Rewriting,
--  module Hydra.Sources.Tier2.Schemas,
  module Hydra.Sources.Tier2.Serialization,
  module Hydra.Sources.Tier2.Substitution,
  module Hydra.Sources.Tier2.Unification,
  module Hydra.Sources.Tier2.Variants,
) where

import Hydra.Sources.Tier1.All hiding (mapDef) -- hydra.decode and hydra.flows both export 'map'
import Hydra.Sources.Tier2.Accessors
import Hydra.Sources.Tier2.Annotations
import Hydra.Sources.Tier2.Arity
import Hydra.Sources.Tier2.CoreLanguage
import Hydra.Sources.Tier2.Errors
--import Hydra.Sources.Tier2.Expect
import Hydra.Sources.Tier2.Flows
import Hydra.Sources.Tier2.Lexical
import Hydra.Sources.Tier2.Printing
import Hydra.Sources.Tier2.Qnames
import Hydra.Sources.Tier2.Rewriting
--import Hydra.Sources.Tier2.Schemas
import Hydra.Sources.Tier2.Serialization
import Hydra.Sources.Tier2.Substitution
import Hydra.Sources.Tier2.Unification
import Hydra.Sources.Tier2.Variants

kernelModules :: [Module]
kernelModules = [hydraCoreModule] ++ tier1Modules ++ tier2Modules

-- | A convenient list of the Hydra kernel modules which contain primarily type definitions.
--   Not guaranteed to be up to date.
kernelTypeModules :: [Module]
kernelTypeModules = [
  hydraCoreModule,
  hydraAstModule,
  hydraCodersModule,
  hydraComputeModule,
  hydraConstraintsModule,
  hydraGrammarModule,
  hydraGraphModule,
  jsonModelModule,
  hydraMantleModule,
  hydraModuleModule,
  hydraPhantomsModule,
  hydraQueryModule,
  hydraTestingModule,
  hydraWorkflowModule]

tier2Modules :: [Module]
tier2Modules = [
  hydraAccessorsModule,
  hydraAnnotationsModule,
  hydraArityModule,
  hydraCoreLanguageModule,
  hydraErrorsModule,
--  hydraExpectModule,
  hydraFlowsModule,
  hydraLexicalModule,
  hydraPrintingModule,
  hydraQnamesModule,
  hydraRewritingModule,
--  hydraSchemasModule,
  hydraSerializationModule,
  hydraStripModule,
  hydraUnificationModule,
  hydraVariantsModule]
