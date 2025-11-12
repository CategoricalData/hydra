module Hydra.Dsl.Util where

import Hydra.Kernel
import Hydra.Dsl.Phantoms
import Hydra.Util

import qualified Data.Map as M
import qualified Data.Maybe as Y


caseConventionCamel = unitVariant _CaseConvention _CaseConvention_camel
caseConventionPascal = unitVariant _CaseConvention _CaseConvention_pascal
caseConventionLowerSnake = unitVariant _CaseConvention _CaseConvention_lowerSnake
caseConventionUpperSnake = unitVariant _CaseConvention _CaseConvention_upperSnake

precisionArbitrary :: TTerm Precision
precisionArbitrary = unitVariant _Precision _Precision_arbitrary

precisionBits :: TTerm Int -> TTerm Precision
precisionBits = variant _Precision _Precision_bits
