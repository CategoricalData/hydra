-- | Haskell-specific convenience layer over the generated Hydra.Dsl.Accessors module.

module Hydra.Dsl.Meta.Accessors (
  module Hydra.Dsl.Accessors,
  module Hydra.Dsl.Meta.Accessors,
) where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms
import Hydra.Dsl.Accessors

-- | Semantic alias for letBody accessor
termAccessorLetEnvironment :: TTerm TermAccessor
termAccessorLetEnvironment = injectUnit _TermAccessor _TermAccessor_letBody

-- | Semantic alias for maybeTerm accessor
termAccessorOptionalTerm :: TTerm TermAccessor
termAccessorOptionalTerm = injectUnit _TermAccessor _TermAccessor_maybeTerm
