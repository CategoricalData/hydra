-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.lib.defaults

module Hydra.Dsl.Lib.Defaults where

import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Lib.Eithers as Eithers
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Lists as Lists
import qualified Hydra.Dsl.Lib.Logic as Logic
import qualified Hydra.Dsl.Lib.Maps as Maps
import qualified Hydra.Dsl.Lib.Math as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import qualified Hydra.Dsl.Lib.Pairs as Pairs
import qualified Hydra.Dsl.Lib.Sets as Sets
import qualified Hydra.Lib.Defaults as Defaults
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M

-- | DSL reference to hydra.lib.defaults.defaultImplementations
defaultImplementations :: Typed.TypedTerm (M.Map Core.Name Core.Term)
defaultImplementations = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.lib.defaults.defaultImplementations"))
