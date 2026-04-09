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


-- | Identity function on Either (Context wrapping removed; retained for API compatibility during migration)
withContext :: TTerm Context -> TTerm (Either e a) -> TTerm (Either e a)
withContext _cx e = e

-- | Push a trace frame onto a Context
pushTrace :: TTerm String -> TTerm Context -> TTerm Context
pushTrace label cx = context
    (Lists.cons label (DslCtx.contextTrace cx))
    (DslCtx.contextMessages cx)
    (DslCtx.contextOther cx)

-- | Create a Left error (Context parameter is ignored; retained for API compatibility during migration)
failInContext :: TTerm e -> TTerm Context -> TTerm (Either e a)
failInContext err _cx = left err
