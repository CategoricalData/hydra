-- | Meta-DSL for constructing error-related terms (DecodingError, OtherError, UnificationError, Error, etc.)

module Hydra.Dsl.Meta.Error where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms


decodingError :: TTerm String -> TTerm DecodingError
decodingError = wrap _DecodingError

unDecodingError :: TTerm (DecodingError -> String)
unDecodingError = unwrap _DecodingError

otherError :: TTerm String -> TTerm OtherError
otherError = wrap _OtherError

unOtherError :: TTerm (OtherError -> String)
unOtherError = unwrap _OtherError

unificationError :: TTerm Type -> TTerm Type -> TTerm String -> TTerm UnificationError
unificationError lt rt msg = record _UnificationError [
    _UnificationError_leftType>>: lt,
    _UnificationError_rightType>>: rt,
    _UnificationError_message>>: msg]

unificationErrorLeftType :: TTerm UnificationError -> TTerm Type
unificationErrorLeftType ue = project _UnificationError _UnificationError_leftType @@ ue

unificationErrorRightType :: TTerm UnificationError -> TTerm Type
unificationErrorRightType ue = project _UnificationError _UnificationError_rightType @@ ue

unificationErrorMessage :: TTerm UnificationError -> TTerm String
unificationErrorMessage ue = project _UnificationError _UnificationError_message @@ ue

errorDecoding :: TTerm DecodingError -> TTerm Error
errorDecoding = inject _Error _Error_decoding

errorOther :: TTerm OtherError -> TTerm Error
errorOther = inject _Error _Error_other

errorUnification :: TTerm UnificationError -> TTerm Error
errorUnification = inject _Error _Error_unification
