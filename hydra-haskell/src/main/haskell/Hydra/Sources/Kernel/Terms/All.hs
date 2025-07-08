-- | All of Hydra's term-level kernel modules
module Hydra.Sources.Kernel.Terms.All(
  module Hydra.Sources.Kernel.Types.All,
  module Hydra.Sources.Kernel.Terms.All,
--  module Hydra.Sources.Kernel.Terms.Adapt.Literals,
--  module Hydra.Sources.Kernel.Terms.Adapt.Modules,
--  module Hydra.Sources.Kernel.Terms.Adapt.Terms,
--  module Hydra.Sources.Kernel.Terms.Adapt.Utils,
--  module Hydra.Sources.Kernel.Terms.Annotations,
--  module Hydra.Sources.Kernel.Terms.Arity,
--  module Hydra.Sources.Kernel.Terms.Constants,
--  module Hydra.Sources.Kernel.Terms.Formatting,
--  module Hydra.Sources.Kernel.Terms.Grammars,
--  module Hydra.Sources.Kernel.Terms.Inference,
--  module Hydra.Sources.Kernel.Terms.Languages,
--  module Hydra.Sources.Kernel.Terms.Lexical,
--  module Hydra.Sources.Kernel.Terms.Literals,
--  module Hydra.Sources.Kernel.Terms.Monads,
--  module Hydra.Sources.Kernel.Terms.Names,
--  module Hydra.Sources.Kernel.Terms.Reduction,
--  module Hydra.Sources.Kernel.Terms.Rewriting,
--  module Hydra.Sources.Kernel.Terms.Schemas,
--  module Hydra.Sources.Kernel.Terms.Serialization,
--  module Hydra.Sources.Kernel.Terms.Sorting,
--  module Hydra.Sources.Kernel.Terms.Substitution,
--  module Hydra.Sources.Kernel.Terms.Tarjan,
--  module Hydra.Sources.Kernel.Terms.Templates,
--  module Hydra.Sources.Kernel.Terms.Unification,
--  module Hydra.Sources.Kernel.Terms.Variants,
) where

import Hydra.Sources.Kernel.Types.All hiding (mapDef) -- hydra.decode, hydra.expect, and hydra.flows all export 'map'
import Hydra.Sources.Kernel.Terms.Adapt.Literals
import Hydra.Sources.Kernel.Terms.Adapt.Modules
import Hydra.Sources.Kernel.Terms.Adapt.Terms hiding (optionalToListDef)
import Hydra.Sources.Kernel.Terms.Adapt.Utils
import Hydra.Sources.Kernel.Terms.Annotations
import Hydra.Sources.Kernel.Terms.Arity
import Hydra.Sources.Kernel.Terms.Constants
import Hydra.Sources.Kernel.Terms.Decode.Core
import Hydra.Sources.Kernel.Terms.Decoding
import Hydra.Sources.Kernel.Terms.Describe.Core
import Hydra.Sources.Kernel.Terms.Describe.Mantle
import Hydra.Sources.Kernel.Terms.Encode.Core hiding (ref)
import Hydra.Sources.Kernel.Terms.Extract.Core
import Hydra.Sources.Kernel.Terms.Extract.Mantle
import Hydra.Sources.Kernel.Terms.Formatting
import Hydra.Sources.Kernel.Terms.Grammars
import Hydra.Sources.Kernel.Terms.Inference
import Hydra.Sources.Kernel.Terms.Languages
import Hydra.Sources.Kernel.Terms.Lexical
import Hydra.Sources.Kernel.Terms.Literals
import Hydra.Sources.Kernel.Terms.Monads
import Hydra.Sources.Kernel.Terms.Names
import Hydra.Sources.Kernel.Terms.Reduction
import Hydra.Sources.Kernel.Terms.Rewriting
import Hydra.Sources.Kernel.Terms.Schemas
import Hydra.Sources.Kernel.Terms.Serialization
import Hydra.Sources.Kernel.Terms.Show.Accessors
import Hydra.Sources.Kernel.Terms.Show.Core
import Hydra.Sources.Kernel.Terms.Show.Graph
import Hydra.Sources.Kernel.Terms.Show.Mantle
import Hydra.Sources.Kernel.Terms.Show.Typing
import Hydra.Sources.Kernel.Terms.Sorting
import Hydra.Sources.Kernel.Terms.Substitution
import Hydra.Sources.Kernel.Terms.Tarjan
import Hydra.Sources.Kernel.Terms.Templates
import Hydra.Sources.Kernel.Terms.Unification
import Hydra.Sources.Kernel.Terms.Variants


kernelModules :: [Module]
kernelModules = kernelTypesModules ++ kernelTermsModules

kernelTermsModules :: [Module]
kernelTermsModules = [
  decodeCoreModule,
  describeCoreModule,
  describeMantleModule,
  encodeCoreModule,
  extractCoreModule,
  extractMantleModule,
  showCoreModule,
  showGraphModule,
  showMantleModule,
  showTypingModule,
  showAccessorsModule,
  adaptModulesModule,
  hydraAdaptUtilsModule,
  hydraAnnotationsModule,
  hydraArityModule,
  hydraConstantsModule,
  hydraDecodingModule,
  hydraFormattingModule,
  languagesModule,
  hydraLiteralsModule,
  hydraMonadsModule,
  hydraGrammarsModule,
  hydraInferenceModule,
  hydraLexicalModule,
  adaptLiteralsModule,
  hydraNamesModule,
  hydraReductionModule,
  hydraRewritingModule,
  hydraSchemasModule,
  hydraSerializationModule,
  hydraSortingModule,
  hydraSubstitutionModule,
  tarjanModule,
  hydraTemplatingModule,
  adaptTermsModule,
  hydraUnificationModule,
  hydraVariantsModule]
