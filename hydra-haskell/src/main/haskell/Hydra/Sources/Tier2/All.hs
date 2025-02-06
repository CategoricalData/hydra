module Hydra.Sources.Tier2.All(
  module Hydra.Sources.Tier1.All,
  module Hydra.Sources.Tier2.All,
  module Hydra.Sources.Tier2.Annotations,
  module Hydra.Sources.Tier2.Arity,
  module Hydra.Sources.Tier2.CoreLanguage,
  module Hydra.Sources.Tier2.Errors,
  module Hydra.Sources.Tier2.Flows,
  module Hydra.Sources.Tier2.Lexical,
  module Hydra.Sources.Tier2.Printing,
  module Hydra.Sources.Tier2.Qnames,
  module Hydra.Sources.Tier2.Rewriting,
  module Hydra.Sources.Tier2.Variants,
) where

import Hydra.Sources.Tier1.All
import Hydra.Sources.Tier2.Annotations
import Hydra.Sources.Tier2.Arity
import Hydra.Sources.Tier2.CoreLanguage
import Hydra.Sources.Tier2.Errors
import Hydra.Sources.Tier2.Flows
import Hydra.Sources.Tier2.Lexical
import Hydra.Sources.Tier2.Printing
import Hydra.Sources.Tier2.Qnames
import Hydra.Sources.Tier2.Rewriting
import Hydra.Sources.Tier2.Variants

tier2Modules :: [Module]
tier2Modules = [
  hydraAnnotationsModule,
  hydraArityModule,
  hydraCoreLanguageModule,
  hydraErrorsModule,
  hydraFlowsModule,
  hydraLexicalModule,
  hydraPrintingModule,
  hydraQnamesModule,
  hydraRewritingModule,
  hydraStripModule,
  hydraVariantsModule]
