-- | A proxy for the Hydra kernel, i.e. the code which must be present in every Hydra implementation.
--   This includes most modules in the top-level Hydra.* namespace, as well as some Hydra.Util.* modules.
--   Hydra.CoreDecoding and Hydra.CoreEncoding, Hydra.Meta, and Hydra.Reduction are logically part of the kernel,
--   but are excluded for now due to their dependencies on the Hydra DSLs.
--   The adapter and inference frameworks are currently excluded so as to avoid circular dependencies.

module Hydra.Kernel (
  module Hydra.Adapters.Utils,
  module Hydra.Basics,
  module Hydra.Common,
  module Hydra.Compute,
  module Hydra.Core,
--  module Hydra.CoreDecoding,
--  module Hydra.CoreEncoding,
  module Hydra.CoreLanguage,
  module Hydra.Grammar,
  module Hydra.Lexical,
  module Hydra.Mantle,
--  module Hydra.Meta,
  module Hydra.Module,
  module Hydra.Monads,
  module Hydra.Phantoms,
--  module Hydra.Reduction,
  module Hydra.Rewriting,
  module Hydra.Util.Sorting,
  module Hydra.Util.Formatting,
  module Hydra.Util.Debug,
--  module Hydra.Util.GrammarToModule,
) where

import Hydra.Adapters.Utils
import Hydra.Basics
import Hydra.Common
import Hydra.Compute
import Hydra.Core
--import Hydra.CoreDecoding
--import Hydra.CoreEncoding
import Hydra.CoreLanguage
import Hydra.Grammar
import Hydra.Lexical
import Hydra.Mantle
--import Hydra.Meta
import Hydra.Module
import Hydra.Monads
import Hydra.Phantoms
--import Hydra.Reduction
import Hydra.Rewriting
import Hydra.Util.Sorting
import Hydra.Util.Formatting
import Hydra.Util.Debug
--import Hydra.Util.GrammarToModule
