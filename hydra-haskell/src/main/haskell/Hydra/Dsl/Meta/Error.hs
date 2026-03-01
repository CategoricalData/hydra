-- | Meta-DSL for constructing error-related terms (DecodingError, etc.)

module Hydra.Dsl.Meta.Error where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms


decodingError :: TTerm String -> TTerm DecodingError
decodingError = wrap _DecodingError

unDecodingError :: TTerm (DecodingError -> String)
unDecodingError = unwrap _DecodingError
