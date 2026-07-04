-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.json.bootstrap

module Hydra.Dsl.Json.Bootstrap where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Coders as DslCoders
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Error.Checking as ErrorChecking
import qualified Hydra.Dsl.Error.Core as DslErrorCore
import qualified Hydra.Dsl.Errors as DslErrors
import qualified Hydra.Dsl.Graph as DslGraph
import qualified Hydra.Dsl.Packaging as DslPackaging
import qualified Hydra.Dsl.Paths as DslPaths
import qualified Hydra.Dsl.Typing as DslTyping
import qualified Hydra.Dsl.Util as DslUtil
import qualified Hydra.Dsl.Variants as DslVariants
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Bootstrap as Bootstrap
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Paths as Paths
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M

-- | DSL reference to hydra.json.bootstrap.typesByName
typesByName :: Typed.TypedTerm (M.Map Core.Name Core.Type)
typesByName = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.json.bootstrap.typesByName"))
