-- | Haskell-specific convenience layer over the generated Hydra.Dsl.Coders module.

module Hydra.Dsl.Meta.Coders (
  module Hydra.Dsl.Coders,
  module Hydra.Dsl.Meta.Coders,
) where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms
import qualified Data.Set as S
import Hydra.Dsl.Coders hiding (languageName, languageConstraints)
import qualified Hydra.Dsl.Coders as DslCoders

-- | Alias for languageName accessor (avoids name conflicts at call sites)
languageNameProjection :: TTerm Language -> TTerm LanguageName
languageNameProjection = DslCoders.languageName

-- | Alias for languageConstraints accessor (avoids name conflicts at call sites)
languageConstraintsProjection :: TTerm Language -> TTerm LanguageConstraints
languageConstraintsProjection = DslCoders.languageConstraints

-- | Constructors (renamed from generated languageName_/languageConstraints_ due to deduplication)
languageName :: TTerm String -> TTerm LanguageName
languageName = DslCoders.languageName_

languageConstraints :: TTerm (S.Set EliminationVariant) -> TTerm (S.Set LiteralVariant) -> TTerm (S.Set FloatType)
  -> TTerm (S.Set FunctionVariant) -> TTerm (S.Set IntegerType) -> TTerm (S.Set TermVariant)
  -> TTerm (S.Set TypeVariant) -> TTerm (Type -> Bool) -> TTerm LanguageConstraints
languageConstraints = DslCoders.languageConstraints_
