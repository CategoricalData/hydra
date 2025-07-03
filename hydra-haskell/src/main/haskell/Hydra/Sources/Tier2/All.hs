-- | Hydra's tier-2 sources add logic for flows (stateful operations), which are defined in tier-1,
--   as well as operations which depend on flows.
module Hydra.Sources.Tier2.All(
  module Hydra.Sources.Tier1.All,
  module Hydra.Sources.Tier2.All,
  module Hydra.Sources.Tier2.AdapterUtils,
  module Hydra.Sources.Tier2.Adapters,
  module Hydra.Sources.Tier2.Annotations,
  module Hydra.Sources.Tier2.Arity,
  module Hydra.Sources.Tier2.CoreLanguage,
--  module Hydra.Sources.Tier2.Decode.Core,
--  module Hydra.Sources.Tier2.Describe.Core,
  module Hydra.Sources.Tier2.Errors,
--  module Hydra.Sources.Tier2.Extract.Core,
  module Hydra.Sources.Tier2.Monads,
  module Hydra.Sources.Tier2.GrammarToModule,
  module Hydra.Sources.Tier2.Inference,
  module Hydra.Sources.Tier2.Lexical,
  module Hydra.Sources.Tier2.LiteralAdapters,
  module Hydra.Sources.Tier2.Qnames,
  module Hydra.Sources.Tier2.Reduction,
  module Hydra.Sources.Tier2.Rewriting,
  module Hydra.Sources.Tier2.Schemas,
  module Hydra.Sources.Tier2.Serialization,
--  module Hydra.Sources.Tier2.Show.Core,
  module Hydra.Sources.Tier2.Sorting,
  module Hydra.Sources.Tier2.Substitution,
  module Hydra.Sources.Tier2.Tarjan,
  module Hydra.Sources.Tier2.Templating,
  module Hydra.Sources.Tier2.TermAdapters,
  module Hydra.Sources.Tier2.Unification,
  module Hydra.Sources.Tier2.Variants,
) where

import Hydra.Sources.Tier1.All hiding (mapDef) -- hydra.decode, hydra.expect, and hydra.flows all export 'map'
import Hydra.Sources.Tier2.AdapterUtils
import Hydra.Sources.Tier2.Adapters
import Hydra.Sources.Tier2.Annotations
import Hydra.Sources.Tier2.Arity
import Hydra.Sources.Tier2.CoreLanguage
import Hydra.Sources.Tier2.Decode.Core
import Hydra.Sources.Tier2.Describe.Core
import Hydra.Sources.Tier2.Errors
import Hydra.Sources.Tier2.Extract.Core
import Hydra.Sources.Tier2.Monads
import Hydra.Sources.Tier2.GrammarToModule
import Hydra.Sources.Tier2.Inference
import Hydra.Sources.Tier2.Lexical
import Hydra.Sources.Tier2.LiteralAdapters
import Hydra.Sources.Tier2.Qnames
import Hydra.Sources.Tier2.Reduction
import Hydra.Sources.Tier2.Rewriting
import Hydra.Sources.Tier2.Schemas
import Hydra.Sources.Tier2.Serialization
import Hydra.Sources.Tier2.Show.Accessors
import Hydra.Sources.Tier2.Show.Core
import Hydra.Sources.Tier2.Sorting
import Hydra.Sources.Tier2.Substitution
import Hydra.Sources.Tier2.Tarjan
import Hydra.Sources.Tier2.Templating
import Hydra.Sources.Tier2.TermAdapters hiding (optionalToListDef)
import Hydra.Sources.Tier2.Unification
import Hydra.Sources.Tier2.Variants


kernelModules :: [Module]
kernelModules = kernelTypeModules ++ kernelTermModules

kernelTypeModules :: [Module]
kernelTypeModules = [hydraCoreModule] ++ tier1TypeModules ++ tier2TypeModules

kernelTermModules :: [Module]
kernelTermModules = tier1TermModules ++ tier2TermModules

tier2Modules :: [Module]
tier2Modules = tier2TypeModules ++ tier2TermModules

-- There are no tier-2 type modules at this time; all types are defined in Hydra Core and tier-1.
tier2TypeModules = []

tier2TermModules = [
  decodeCoreModule,
  describeCoreModule,
  extractCoreModule,
  showCoreModule,
  showAccessorsModule,
  hydraAdaptersModule,
  hydraAdapterUtilsModule,
  hydraAnnotationsModule,
  hydraArityModule,
  hydraCoreLanguageModule,
  hydraErrorsModule,
  hydraMonadsModule,
  hydraGrammarToModuleModule,
  hydraInferenceModule,
  hydraLexicalModule,
  hydraLiteralAdaptersModule,
  hydraQnamesModule,
  hydraReductionModule,
  hydraRewritingModule,
  hydraSchemasModule,
  hydraSerializationModule,
  hydraSortingModule,
  hydraSubstitutionModule,
  tarjanModule,
  hydraTemplatingModule,
  hydraTermAdaptersModule,
  hydraUnificationModule,
  hydraVariantsModule]
