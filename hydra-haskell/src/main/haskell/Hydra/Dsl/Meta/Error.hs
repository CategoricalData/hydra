-- | Haskell-specific convenience layer over the generated Hydra.Dsl.Error module.
-- Provides shortened aliases for CheckingError constructors.

module Hydra.Dsl.Meta.Error (
  module Hydra.Dsl.Error,
  module Hydra.Dsl.Meta.Error,
) where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms
import Hydra.Dsl.Error hiding (unDecodingError, unOtherError)

-- | Shortened aliases for CheckingError constructors
checkingIncorrectUnification :: TTerm IncorrectUnificationError -> TTerm CheckingError
checkingIncorrectUnification = checkingErrorIncorrectUnification

checkingNotAForallType :: TTerm NotAForallTypeError -> TTerm CheckingError
checkingNotAForallType = checkingErrorNotAForallType

checkingNotAFunctionType :: TTerm NotAFunctionTypeError -> TTerm CheckingError
checkingNotAFunctionType = checkingErrorNotAFunctionType

checkingTypeArityMismatch :: TTerm TypeArityMismatchError -> TTerm CheckingError
checkingTypeArityMismatch = checkingErrorTypeArityMismatch

checkingTypeMismatch :: TTerm TypeMismatchError -> TTerm CheckingError
checkingTypeMismatch = checkingErrorTypeMismatch

checkingUnboundTypeVariables :: TTerm UnboundTypeVariablesError -> TTerm CheckingError
checkingUnboundTypeVariables = checkingErrorUnboundTypeVariables

checkingUnequalTypes :: TTerm UnequalTypesError -> TTerm CheckingError
checkingUnequalTypes = checkingErrorUnequalTypes

checkingUnsupportedTermVariant :: TTerm UnsupportedTermVariantError -> TTerm CheckingError
checkingUnsupportedTermVariant = checkingErrorUnsupportedTermVariant

checkingUntypedLambda :: TTerm UntypedLambdaError -> TTerm CheckingError
checkingUntypedLambda = checkingErrorUntypedLambda

checkingUntypedLetBinding :: TTerm UntypedLetBindingError -> TTerm CheckingError
checkingUntypedLetBinding = checkingErrorUntypedLetBinding

-- | TTerm-level unwrap functions (used with @@ operator)
unDecodingError :: TTerm (DecodingError -> String)
unDecodingError = unwrap _DecodingError

unOtherError :: TTerm (OtherError -> String)
unOtherError = unwrap _OtherError
