-- | Hydra: graphs are programs, and programs are graphs.
-- |
-- | This is the convenience entry point of the @hydra@ umbrella package. It
-- | re-exports the Hydra kernel (via "Hydra.Kernel") together with the Haskell
-- | coder's top-level entry points, so that
-- |
-- | > import Hydra
-- |
-- | gives a newcomer the kernel API plus the ability to emit Haskell source
-- | from Hydra modules, in one import.
-- |
-- | The surface here is deliberately small. Everything else in the kernel and
-- | the Haskell coder remains available by direct, qualified import — e.g.
-- | @import qualified Hydra.Haskell.Syntax as H@ for the Haskell AST types, or
-- | @import Hydra.Ast@ for the modules "Hydra.Kernel" omits to avoid name
-- | clashes. Depend on @hydra-kernel@ / @hydra-haskell@ directly for
-- | finer-grained APIs.
--
-- Note: this is a hand-written umbrella module. Its canonical home is
-- overlay/haskell/hydra/ (the top-level overlay/ tree); sync-haskell.sh overlays
-- it onto dist/haskell/hydra/, from which the publishable sdist is assembled.

module Hydra (
  module Hydra.Kernel,
  -- * Haskell code generation
  -- | From "Hydra.Haskell.Coder". @moduleToHaskell@ renders a Hydra module to a
  -- map of file path -> Haskell source; @moduleToHaskellModule@ returns the
  -- structured Haskell AST instead.
  moduleToHaskell,
  moduleToHaskellModule,
) where

import Hydra.Kernel
import Hydra.Haskell.Coder (moduleToHaskell, moduleToHaskellModule)
