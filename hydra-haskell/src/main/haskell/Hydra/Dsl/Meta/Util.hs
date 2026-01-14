-- | Meta-DSL for constructing utility-related terms (CaseConvention, etc.)

module Hydra.Dsl.Meta.Util where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms
import Hydra.Util

import qualified Data.Map as M
import qualified Data.Maybe as Y


caseConventionCamel = injectUnit _CaseConvention _CaseConvention_camel
caseConventionPascal = injectUnit _CaseConvention _CaseConvention_pascal
caseConventionLowerSnake = injectUnit _CaseConvention _CaseConvention_lowerSnake
caseConventionUpperSnake = injectUnit _CaseConvention _CaseConvention_upperSnake

decodingError :: TTerm String -> TTerm DecodingError
decodingError = wrap _DecodingError

unDecodingError :: TTerm (DecodingError -> String)
unDecodingError = unwrap _DecodingError

precisionArbitrary :: TTerm Precision
precisionArbitrary = injectUnit _Precision _Precision_arbitrary

precisionBits :: TTerm Int -> TTerm Precision
precisionBits = inject _Precision _Precision_bits
