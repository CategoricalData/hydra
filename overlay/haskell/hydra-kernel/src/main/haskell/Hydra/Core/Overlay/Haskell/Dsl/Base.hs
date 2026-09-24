-- | A basis for other DSLs which deal with term-encoded expressions.

module Hydra.Core.Overlay.Haskell.Dsl.Base (
  module Hydra.Core.Overlay.Haskell.Dsl.Phantoms,
  module Hydra.Core.Overlay.Haskell.Dsl.Base,
  module Hydra.Core.Overlay.Haskell.Libraries,
) where

import Hydra.Kernel
import Hydra.Core.Overlay.Haskell.Dsl.Phantoms(asTerm, AsTerm(..), definitionInModule, el, firstClassType, opt)
import qualified Hydra.Core.Overlay.Haskell.Dsl.Terms as Terms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core as Core
import Hydra.Core.Overlay.Haskell.Libraries

import qualified Data.Map as M
import qualified Data.Maybe as Y


infixr 0 >:
(>:) :: String -> a -> (TypedTerm Name, a)
n >: d = (name n, d)

infixr 0 >>:
(>>:) :: Name -> a -> (TypedTerm Name, a)
n >>: d = (Core.nameLift n, d)

name :: String -> TypedTerm Name
name s = Core.nameLift $ Name s
