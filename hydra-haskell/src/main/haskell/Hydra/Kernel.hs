-- | A proxy for the Hydra kernel, i.e. the code which must be present in every Hydra implementation, and can be imported as a unit.

-- Note: Hydra.Ast, Hydra.Grammar, Hydra.GrammarToModule, and Hydra.Topology are part of the kernel,
--       but they are not default imports because the names of their types clash with those of other types.
module Hydra.Kernel (
  module Hydra.Accessors,
  module Hydra.AdapterUtils,
  module Hydra.Staging.Adapters,
  module Hydra.Annotations,
  module Hydra.Arity,
--  module Hydra.Ast,
  module Hydra.Coders,
  module Hydra.Compute,
  module Hydra.Constants,
  module Hydra.Constraints,
  module Hydra.Core,
  module Hydra.CoreDecoding,
  module Hydra.CoreEncoding,
  module Hydra.CoreLanguage,
  module Hydra.Errors,
  module Hydra.Flows,
  module Hydra.Formatting,
  module Hydra.Graph,
--  module Hydra.Grammar,
--  module Hydra.GrammarToModule,
  module Hydra.Staging.Inference,
  module Hydra.Lexical,
  module Hydra.Staging.LiteralAdapters,
  module Hydra.Literals,
  module Hydra.Mantle,
  module Hydra.Messages,
  module Hydra.Module,
  module Hydra.Phantoms,
  module Hydra.Printing,
  module Hydra.Qnames,
  module Hydra.Query,
  module Hydra.Reduction,
  module Hydra.Rewriting,
  module Hydra.Schemas,
  module Hydra.Serialization,
  module Hydra.Settings,
  module Hydra.Sorting,
  module Hydra.Strip,
  module Hydra.Substitution,
  module Hydra.Templating,
  module Hydra.Staging.TermAdapters,
--  module Hydra.Topology,
  module Hydra.Typing,
  module Hydra.Unification,
  module Hydra.Variants,
  module Hydra.Workflow,
) where

import Hydra.Accessors
import Hydra.AdapterUtils
import Hydra.Staging.Adapters
import Hydra.Annotations
import Hydra.Arity
--import Hydra.Ast
import Hydra.Coders
import Hydra.Compute
import Hydra.Constants
import Hydra.Constraints
import Hydra.Core
import Hydra.CoreDecoding
import Hydra.CoreEncoding
import Hydra.CoreLanguage
import Hydra.Errors
import Hydra.Flows
import Hydra.Formatting
import Hydra.Graph
--import Hydra.Grammar
--import Hydra.GrammarToModule
import Hydra.Staging.Inference
import Hydra.Lexical
import Hydra.Staging.LiteralAdapters
import Hydra.Literals
import Hydra.Mantle
import Hydra.Messages
import Hydra.Module
import Hydra.Phantoms
import Hydra.Printing
import Hydra.Qnames
import Hydra.Query
import Hydra.Reduction
import Hydra.Rewriting
import Hydra.Schemas
import Hydra.Serialization
import Hydra.Settings
import Hydra.Sorting
import Hydra.Strip
import Hydra.Substitution
import Hydra.Templating
import Hydra.Staging.TermAdapters
--import Hydra.Topology
import Hydra.Typing
import Hydra.Unification
import Hydra.Variants
import Hydra.Workflow
