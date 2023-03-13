-- | A proxy for the Hydra kernel, i.e. the code which must be present in every Hydra implementation, and can be imported as a unit.

module Hydra.Kernel (
  module Hydra.Adapters,
  module Hydra.LiteralAdapters,
  module Hydra.TermAdapters,
  module Hydra.Printing,
  module Hydra.AdapterUtils,
  module Hydra.Basics,
  module Hydra.Coders,
  module Hydra.Common,
  module Hydra.Compute,
  module Hydra.Core,
  module Hydra.CoreDecoding,
  module Hydra.CoreEncoding,
  module Hydra.CoreLanguage,
  module Hydra.Graph,
  module Hydra.Grammar,
  module Hydra.Lexical,
  module Hydra.Mantle,
  module Hydra.Kv,
  module Hydra.Module,
  module Hydra.Flows,
  module Hydra.Phantoms,
  module Hydra.Reduction,
  module Hydra.Rewriting,
  module Hydra.Inference,
  -- module Hydra.Ast,
  module Hydra.Tools.Debug,
  module Hydra.Tools.Formatting,
--  module Hydra.Tools.GrammarToModule,
  module Hydra.Tools.Sorting,
  module Hydra.Workflow,
) where

import Hydra.Adapters
import Hydra.LiteralAdapters
import Hydra.TermAdapters
import Hydra.Printing
import Hydra.AdapterUtils
import Hydra.Basics
import Hydra.Coders
import Hydra.Common
import Hydra.Compute
import Hydra.Core
import Hydra.CoreDecoding
import Hydra.CoreEncoding
import Hydra.CoreLanguage
import Hydra.Graph
import Hydra.Grammar
import Hydra.Lexical
import Hydra.Mantle
import Hydra.Kv
import Hydra.Module
import Hydra.Flows
import Hydra.Phantoms
import Hydra.Reduction
import Hydra.Rewriting
import Hydra.Inference
-- import Hydra.Ast
import Hydra.Tools.Debug
import Hydra.Tools.Formatting
--import Hydra.Tools.GrammarToModule
import Hydra.Tools.Sorting
import Hydra.Workflow
