-- | A proxy for the Hydra kernel, i.e. the code which must be present in every Hydra implementation.
--   This currently includes most modules in the top-level Hydra.* namespace, as well as some Hydra.Util.* modules.
--   The adapter and inference frameworks, as well as Hydra.CoreDecoding, Hydra.CoreEncoding, Hydra.Kv, and Hydra.Reduction are logically part of the kernel,
--   but are excluded for now due to circular dependencies.

module Hydra.Kernel (
  module Hydra.Adapters.Utils,
  module Hydra.Basics,
  module Hydra.Common,
  module Hydra.Compute,
  module Hydra.Core,
--  module Hydra.CoreDecoding,
--  module Hydra.CoreEncoding,
  module Hydra.CoreLanguage,
  module Hydra.Graph,
  module Hydra.Grammar,
  module Hydra.Lexical,
  module Hydra.Mantle,
--  module Hydra.Kv,
  module Hydra.Module,
  module Hydra.Monads,
  module Hydra.Phantoms,
--  module Hydra.Reduction,
  module Hydra.Rewriting,
  module Hydra.Util.Debug,
  module Hydra.Util.Formatting,
--  module Hydra.Util.GrammarToModule,
  module Hydra.Util.Sorting,
) where

import Hydra.Adapters.Utils
import Hydra.Basics
import Hydra.Common
import Hydra.Compute
import Hydra.Core
--import Hydra.CoreDecoding
--import Hydra.CoreEncoding
import Hydra.CoreLanguage
import Hydra.Graph
import Hydra.Grammar
import Hydra.Lexical
import Hydra.Mantle
--import Hydra.Kv
import Hydra.Module
import Hydra.Monads
import Hydra.Phantoms
--import Hydra.Reduction
import Hydra.Rewriting
import Hydra.Util.Debug
import Hydra.Util.Formatting
--import Hydra.Util.GrammarToModule
import Hydra.Util.Sorting
