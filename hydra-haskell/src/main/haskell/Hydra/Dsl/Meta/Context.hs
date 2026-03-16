-- | Haskell-specific convenience layer over the generated Hydra.Dsl.Context module.

module Hydra.Dsl.Meta.Context (
  module Hydra.Dsl.Context,
  module Hydra.Dsl.Meta.Context,
  DslCtx.contextTrace,
  DslCtx.contextMessages,
  DslCtx.contextOther,
) where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms
import Hydra.Dsl.Context hiding (contextTrace, contextMessages, contextOther)
import qualified Hydra.Dsl.Context as DslCtx

import qualified Hydra.Dsl.Meta.Lib.Eithers as Eithers
import qualified Hydra.Dsl.Meta.Lib.Lists as Lists


-- | Wrap an Either's Left side with a Context
withContext :: TTerm Context -> TTerm (Either e a) -> TTerm (Either (InContext e) a)
withContext cx e = Eithers.bimap ("_wc_e" ~> inContext (var "_wc_e") cx) ("_wc_a" ~> var "_wc_a") e

-- | Push a trace frame onto a Context
pushTrace :: TTerm String -> TTerm Context -> TTerm Context
pushTrace label cx = context
    (Lists.cons label (DslCtx.contextTrace cx))
    (DslCtx.contextMessages cx)
    (DslCtx.contextOther cx)

-- | Create a Left (InContext error cx)
failInContext :: TTerm e -> TTerm Context -> TTerm (Either (InContext e) a)
failInContext err cx = left (inContext err cx)
