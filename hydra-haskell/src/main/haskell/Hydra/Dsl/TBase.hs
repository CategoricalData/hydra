-- | A basis for other DSLs which deal with term-encoded expressions.

module Hydra.Dsl.TBase (
  module Hydra.Dsl.Base,
  module Hydra.Dsl.ShorthandTypes,
  module Hydra.Dsl.TBase,
  module Hydra.Sources.Libraries,
) where

import Hydra.Kernel
import Hydra.Dsl.ShorthandTypes
import Hydra.Dsl.Base(definitionInModule, el, firstClassType, opt, ref, variant)
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Core as Core
import Hydra.Sources.Libraries

import qualified Data.Map as M
import qualified Data.Maybe as Y


infixr 0 >:
(>:) :: String -> a -> (TTerm Name, a)
n >: d = (name n, d)

name :: String -> TTerm Name
name s = Core.name $ Name s
