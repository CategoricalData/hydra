-- | A proxy for the Hydra kernel, i.e. the code which must be present in every Hydra implementation, and can be imported as a unit.
{-
Note: the following modules are part of the kernel, but they are not default imports because of name collisions:
- Hydra.Ast
- Hydra.Decode.Core
- Hydra.Describe.Core
- Hydra.Encode.Core
- Hydra.Extract.Core
- Hydra.Grammar
- Hydra.GrammarToModule
- Hydra.Topology
-}

module Hydra.Kernel (
  module Hydra.Accessors,
  module Hydra.AdapterUtils,
  module Hydra.Adapters,
  module Hydra.Annotations,
  module Hydra.Arity,
  module Hydra.Coders,
  module Hydra.Compute,
  module Hydra.Constants,
  module Hydra.Constraints,
  module Hydra.Core,
  module Hydra.CoreLanguage,
  module Hydra.Errors,
  module Hydra.Monads,
  module Hydra.Formatting,
  module Hydra.Graph,
  module Hydra.Inference,
  module Hydra.Lexical,
  module Hydra.LiteralAdapters,
  module Hydra.Literals,
  module Hydra.Mantle,
  module Hydra.Module,
  module Hydra.Phantoms,
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
  module Hydra.TermAdapters,
  module Hydra.Typing,
  module Hydra.Unification,
  module Hydra.Variants,
  module Hydra.Workflow,
) where

import Hydra.Accessors
import Hydra.AdapterUtils
import Hydra.Adapters
import Hydra.Annotations
import Hydra.Arity
import Hydra.Coders
import Hydra.Compute
import Hydra.Constants
import Hydra.Constraints
import Hydra.Core
import Hydra.CoreLanguage
import Hydra.Errors
import Hydra.Monads hiding (fail, pure)
import Hydra.Formatting
import Hydra.Graph
import Hydra.Inference
import Hydra.Lexical
import Hydra.LiteralAdapters
import Hydra.Literals
import Hydra.Mantle hiding (Either)
import Hydra.Module
import Hydra.Phantoms
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
import Hydra.TermAdapters hiding (optionalToList)
import Hydra.Typing
import Hydra.Unification
import Hydra.Variants
import Hydra.Workflow
