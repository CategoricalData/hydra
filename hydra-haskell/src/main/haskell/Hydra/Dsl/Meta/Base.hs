-- | A basis for other DSLs which deal with term-encoded expressions.

module Hydra.Dsl.Meta.Base (
  module Hydra.Dsl.Meta.Phantoms,
  module Hydra.Dsl.Meta.Base,
  module Hydra.Sources.Libraries,
) where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms(asTerm, AsTerm(..), definitionInModule, el, firstClassType, opt)
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Meta.Core as Core
import Hydra.Sources.Libraries

import qualified Data.Map as M
import qualified Data.Maybe as Y


infixr 0 >:
(>:) :: String -> a -> (TTerm Name, a)
n >: d = (name n, d)

infixr 0 >>:
(>>:) :: Name -> a -> (TTerm Name, a)
n >>: d = (Core.nameLift n, d)

name :: String -> TTerm Name
name s = Core.nameLift $ Name s
