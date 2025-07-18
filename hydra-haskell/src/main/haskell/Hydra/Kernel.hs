-- | A proxy for the Hydra kernel, i.e. the code which must be present in every Hydra implementation, and can be imported as a unit.
{-
Note: the following modules are part of the kernel, but they are not default imports because of name collisions:
- Hydra.Ast
- Hydra.Decode.Core
- Hydra.Describe.Core
- Hydra.Encode.Core
- Hydra.Extract.Core
- Hydra.Grammar
- Hydra.Grammars
- Hydra.Topology
-}

module Hydra.Kernel (
  module Hydra.Accessors,
  module Hydra.Adapt.Literals,
  module Hydra.Adapt.Modules,
  module Hydra.Adapt.Terms,
  module Hydra.Adapt.Utils,
  module Hydra.Annotations,
  module Hydra.Arity,
  module Hydra.Coders,
  module Hydra.Compute,
  module Hydra.Constants,
  module Hydra.Constraints,
  module Hydra.Core,
  module Hydra.Languages,
  module Hydra.Monads,
  module Hydra.Formatting,
  module Hydra.Graph,
  module Hydra.Inference,
  module Hydra.Lexical,
  module Hydra.Literals,
  module Hydra.Mantle,
  module Hydra.Module,
  module Hydra.Phantoms,
  module Hydra.Names,
  module Hydra.Query,
  module Hydra.Reduction,
  module Hydra.Relational,
  module Hydra.Rewriting,
  module Hydra.Schemas,
  module Hydra.Serialization,
  module Hydra.Settings,
  module Hydra.Sorting,
  module Hydra.Substitution,
  module Hydra.Tabular,
  module Hydra.Templates,
  module Hydra.Typing,
  module Hydra.Unification,
  module Hydra.Variants,
  module Hydra.Workflow,
) where

import Hydra.Accessors
import Hydra.Adapt.Literals
import Hydra.Adapt.Modules
import Hydra.Adapt.Terms hiding (optionalToList)
import Hydra.Adapt.Utils
import Hydra.Annotations
import Hydra.Arity
import Hydra.Coders
import Hydra.Compute
import Hydra.Constants
import Hydra.Constraints
import Hydra.Core
import Hydra.Languages
import Hydra.Monads hiding (bind, exec, fail, pure)
import Hydra.Formatting
import Hydra.Graph
import Hydra.Inference
import Hydra.Lexical
import Hydra.Literals
import Hydra.Mantle hiding (Either)
import Hydra.Module
import Hydra.Phantoms
import Hydra.Names
import Hydra.Query
import Hydra.Reduction
import Hydra.Relational
import Hydra.Rewriting
import Hydra.Schemas
import Hydra.Serialization
import Hydra.Settings
import Hydra.Sorting
import Hydra.Substitution
import Hydra.Tabular
import Hydra.Templates
import Hydra.Typing
import Hydra.Unification
import Hydra.Variants
import Hydra.Workflow
