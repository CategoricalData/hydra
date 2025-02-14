-- | A basis for other DSLs which deal with term-encoded expressions.

module Hydra.Dsl.TBase (
  module Hydra.Dsl.Base,
  module Hydra.Dsl.ShorthandTypes,
  module Hydra.Dsl.TBase,
) where

import Hydra.Kernel
import Hydra.Dsl.ShorthandTypes
import Hydra.Dsl.Base(definitionInModule, el, firstClassType, functionN, lambdas, ref)
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Dsl.Core as Core

import qualified Data.Map as M
import qualified Data.Maybe as Y


name :: String -> TTerm Name
name s = Core.name $ Name s
