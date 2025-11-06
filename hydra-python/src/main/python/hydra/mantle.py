# Note: this is an automatically generated file. Do not edit.

r"""A set of types which supplement hydra.core, but are not referenced by hydra.core."""

from __future__ import annotations
from enum import Enum
from hydra.dsl.python import Node
import hydra.core

class CaseConvention(Enum):
    CAMEL = "camel"
    
    PASCAL = "pascal"
    
    LOWER_SNAKE = "lowerSnake"
    
    UPPER_SNAKE = "upperSnake"

CASE_CONVENTION__NAME = hydra.core.Name("hydra.mantle.CaseConvention")
CASE_CONVENTION__CAMEL__NAME = hydra.core.Name("camel")
CASE_CONVENTION__PASCAL__NAME = hydra.core.Name("pascal")
CASE_CONVENTION__LOWER_SNAKE__NAME = hydra.core.Name("lowerSnake")
CASE_CONVENTION__UPPER_SNAKE__NAME = hydra.core.Name("upperSnake")

class Comparison(Enum):
    r"""An equality judgement: less than, equal to, or greater than."""
    
    LESS_THAN = "lessThan"
    
    EQUAL_TO = "equalTo"
    
    GREATER_THAN = "greaterThan"

COMPARISON__NAME = hydra.core.Name("hydra.mantle.Comparison")
COMPARISON__LESS_THAN__NAME = hydra.core.Name("lessThan")
COMPARISON__EQUAL_TO__NAME = hydra.core.Name("equalTo")
COMPARISON__GREATER_THAN__NAME = hydra.core.Name("greaterThan")

class EliminationVariant(Enum):
    r"""The identifier of an elimination constructor."""
    
    PRODUCT = "product"
    
    RECORD = "record"
    
    UNION = "union"
    
    WRAP = "wrap"

ELIMINATION_VARIANT__NAME = hydra.core.Name("hydra.mantle.EliminationVariant")
ELIMINATION_VARIANT__PRODUCT__NAME = hydra.core.Name("product")
ELIMINATION_VARIANT__RECORD__NAME = hydra.core.Name("record")
ELIMINATION_VARIANT__UNION__NAME = hydra.core.Name("union")
ELIMINATION_VARIANT__WRAP__NAME = hydra.core.Name("wrap")

class FunctionVariant(Enum):
    r"""The identifier of a function constructor."""
    
    ELIMINATION = "elimination"
    
    LAMBDA = "lambda"
    
    PRIMITIVE = "primitive"

FUNCTION_VARIANT__NAME = hydra.core.Name("hydra.mantle.FunctionVariant")
FUNCTION_VARIANT__ELIMINATION__NAME = hydra.core.Name("elimination")
FUNCTION_VARIANT__LAMBDA__NAME = hydra.core.Name("lambda")
FUNCTION_VARIANT__PRIMITIVE__NAME = hydra.core.Name("primitive")

class LiteralVariant(Enum):
    r"""The identifier of a literal constructor."""
    
    BINARY = "binary"
    
    BOOLEAN = "boolean"
    
    FLOAT = "float"
    
    INTEGER = "integer"
    
    STRING = "string"

LITERAL_VARIANT__NAME = hydra.core.Name("hydra.mantle.LiteralVariant")
LITERAL_VARIANT__BINARY__NAME = hydra.core.Name("binary")
LITERAL_VARIANT__BOOLEAN__NAME = hydra.core.Name("boolean")
LITERAL_VARIANT__FLOAT__NAME = hydra.core.Name("float")
LITERAL_VARIANT__INTEGER__NAME = hydra.core.Name("integer")
LITERAL_VARIANT__STRING__NAME = hydra.core.Name("string")

class PrecisionArbitrary: ...

class PrecisionBits(Node[int]): ...

# Numeric precision: arbitrary precision, or precision to a specified number of bits.
type Precision = PrecisionArbitrary | PrecisionBits

PRECISION__NAME = hydra.core.Name("hydra.mantle.Precision")
PRECISION__ARBITRARY__NAME = hydra.core.Name("arbitrary")
PRECISION__BITS__NAME = hydra.core.Name("bits")

class TermVariant(Enum):
    r"""The identifier of a term expression constructor."""
    
    ANNOTATED = "annotated"
    
    APPLICATION = "application"
    
    EITHER = "either"
    
    FUNCTION = "function"
    
    LET = "let"
    
    LIST = "list"
    
    LITERAL = "literal"
    
    MAP = "map"
    
    MAYBE = "maybe"
    
    PAIR = "pair"
    
    PRODUCT = "product"
    
    RECORD = "record"
    
    SET = "set"
    
    SUM = "sum"
    
    TYPE_APPLICATION = "typeApplication"
    
    TYPE_LAMBDA = "typeLambda"
    
    UNION = "union"
    
    UNIT = "unit"
    
    VARIABLE = "variable"
    
    WRAP = "wrap"

TERM_VARIANT__NAME = hydra.core.Name("hydra.mantle.TermVariant")
TERM_VARIANT__ANNOTATED__NAME = hydra.core.Name("annotated")
TERM_VARIANT__APPLICATION__NAME = hydra.core.Name("application")
TERM_VARIANT__EITHER__NAME = hydra.core.Name("either")
TERM_VARIANT__FUNCTION__NAME = hydra.core.Name("function")
TERM_VARIANT__LET__NAME = hydra.core.Name("let")
TERM_VARIANT__LIST__NAME = hydra.core.Name("list")
TERM_VARIANT__LITERAL__NAME = hydra.core.Name("literal")
TERM_VARIANT__MAP__NAME = hydra.core.Name("map")
TERM_VARIANT__MAYBE__NAME = hydra.core.Name("maybe")
TERM_VARIANT__PAIR__NAME = hydra.core.Name("pair")
TERM_VARIANT__PRODUCT__NAME = hydra.core.Name("product")
TERM_VARIANT__RECORD__NAME = hydra.core.Name("record")
TERM_VARIANT__SET__NAME = hydra.core.Name("set")
TERM_VARIANT__SUM__NAME = hydra.core.Name("sum")
TERM_VARIANT__TYPE_APPLICATION__NAME = hydra.core.Name("typeApplication")
TERM_VARIANT__TYPE_LAMBDA__NAME = hydra.core.Name("typeLambda")
TERM_VARIANT__UNION__NAME = hydra.core.Name("union")
TERM_VARIANT__UNIT__NAME = hydra.core.Name("unit")
TERM_VARIANT__VARIABLE__NAME = hydra.core.Name("variable")
TERM_VARIANT__WRAP__NAME = hydra.core.Name("wrap")

class TypeClass(Enum):
    r"""Any of a small number of built-in type classes."""
    
    EQUALITY = "equality"
    
    ORDERING = "ordering"

TYPE_CLASS__NAME = hydra.core.Name("hydra.mantle.TypeClass")
TYPE_CLASS__EQUALITY__NAME = hydra.core.Name("equality")
TYPE_CLASS__ORDERING__NAME = hydra.core.Name("ordering")

class TypeVariant(Enum):
    r"""The identifier of a type constructor."""
    
    ANNOTATED = "annotated"
    
    APPLICATION = "application"
    
    EITHER = "either"
    
    FORALL = "forall"
    
    FUNCTION = "function"
    
    LIST = "list"
    
    LITERAL = "literal"
    
    MAP = "map"
    
    MAYBE = "maybe"
    
    PAIR = "pair"
    
    PRODUCT = "product"
    
    RECORD = "record"
    
    SET = "set"
    
    SUM = "sum"
    
    UNION = "union"
    
    UNIT = "unit"
    
    VARIABLE = "variable"
    
    WRAP = "wrap"

TYPE_VARIANT__NAME = hydra.core.Name("hydra.mantle.TypeVariant")
TYPE_VARIANT__ANNOTATED__NAME = hydra.core.Name("annotated")
TYPE_VARIANT__APPLICATION__NAME = hydra.core.Name("application")
TYPE_VARIANT__EITHER__NAME = hydra.core.Name("either")
TYPE_VARIANT__FORALL__NAME = hydra.core.Name("forall")
TYPE_VARIANT__FUNCTION__NAME = hydra.core.Name("function")
TYPE_VARIANT__LIST__NAME = hydra.core.Name("list")
TYPE_VARIANT__LITERAL__NAME = hydra.core.Name("literal")
TYPE_VARIANT__MAP__NAME = hydra.core.Name("map")
TYPE_VARIANT__MAYBE__NAME = hydra.core.Name("maybe")
TYPE_VARIANT__PAIR__NAME = hydra.core.Name("pair")
TYPE_VARIANT__PRODUCT__NAME = hydra.core.Name("product")
TYPE_VARIANT__RECORD__NAME = hydra.core.Name("record")
TYPE_VARIANT__SET__NAME = hydra.core.Name("set")
TYPE_VARIANT__SUM__NAME = hydra.core.Name("sum")
TYPE_VARIANT__UNION__NAME = hydra.core.Name("union")
TYPE_VARIANT__UNIT__NAME = hydra.core.Name("unit")
TYPE_VARIANT__VARIABLE__NAME = hydra.core.Name("variable")
TYPE_VARIANT__WRAP__NAME = hydra.core.Name("wrap")
