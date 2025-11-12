# Note: this is an automatically generated file. Do not edit.

r"""General-purpose utility types used across Hydra."""

from __future__ import annotations
from enum import Enum
from hydra.dsl.python import Node
import hydra.core

class CaseConvention(Enum):
    r"""A naming convention for symbols, such as camelCase or snake_case."""
    
    CAMEL = "camel"
    
    PASCAL = "pascal"
    
    LOWER_SNAKE = "lowerSnake"
    
    UPPER_SNAKE = "upperSnake"

CASE_CONVENTION__NAME = hydra.core.Name("hydra.util.CaseConvention")
CASE_CONVENTION__CAMEL__NAME = hydra.core.Name("camel")
CASE_CONVENTION__PASCAL__NAME = hydra.core.Name("pascal")
CASE_CONVENTION__LOWER_SNAKE__NAME = hydra.core.Name("lowerSnake")
CASE_CONVENTION__UPPER_SNAKE__NAME = hydra.core.Name("upperSnake")

class Comparison(Enum):
    r"""An equality judgement: less than, equal to, or greater than."""
    
    LESS_THAN = "lessThan"
    
    EQUAL_TO = "equalTo"
    
    GREATER_THAN = "greaterThan"

COMPARISON__NAME = hydra.core.Name("hydra.util.Comparison")
COMPARISON__LESS_THAN__NAME = hydra.core.Name("lessThan")
COMPARISON__EQUAL_TO__NAME = hydra.core.Name("equalTo")
COMPARISON__GREATER_THAN__NAME = hydra.core.Name("greaterThan")

class PrecisionArbitrary:
    r"""Arbitrary precision."""

class PrecisionBits(Node[int]):
    r"""Precision to a specified number of bits."""

# Numeric precision: arbitrary precision, or precision to a specified number of bits.
type Precision = PrecisionArbitrary | PrecisionBits

PRECISION__NAME = hydra.core.Name("hydra.util.Precision")
PRECISION__ARBITRARY__NAME = hydra.core.Name("arbitrary")
PRECISION__BITS__NAME = hydra.core.Name("bits")
