# Note: this is an automatically generated file. Do not edit.

r"""Variant types which describe the structure of Hydra core types and terms."""

from __future__ import annotations
from enum import Enum
from typing import TypeAlias
import hydra.core

class EliminationVariant(Enum):
    r"""The identifier of an elimination constructor."""
    
    RECORD = hydra.core.Name("record")
    
    UNION = hydra.core.Name("union")
    
    WRAP = hydra.core.Name("wrap")

EliminationVariant.TYPE_ = hydra.core.Name("hydra.variants.EliminationVariant")

class FunctionVariant(Enum):
    r"""The identifier of a function constructor."""
    
    ELIMINATION = hydra.core.Name("elimination")
    
    LAMBDA = hydra.core.Name("lambda")
    
    PRIMITIVE = hydra.core.Name("primitive")

FunctionVariant.TYPE_ = hydra.core.Name("hydra.variants.FunctionVariant")

class LiteralVariant(Enum):
    r"""The identifier of a literal constructor."""
    
    BINARY = hydra.core.Name("binary")
    
    BOOLEAN = hydra.core.Name("boolean")
    
    FLOAT = hydra.core.Name("float")
    
    INTEGER = hydra.core.Name("integer")
    
    STRING = hydra.core.Name("string")

LiteralVariant.TYPE_ = hydra.core.Name("hydra.variants.LiteralVariant")

class TermVariant(Enum):
    r"""The identifier of a term expression constructor."""
    
    ANNOTATED = hydra.core.Name("annotated")
    
    APPLICATION = hydra.core.Name("application")
    
    EITHER = hydra.core.Name("either")
    
    FUNCTION = hydra.core.Name("function")
    
    LET = hydra.core.Name("let")
    
    LIST = hydra.core.Name("list")
    
    LITERAL = hydra.core.Name("literal")
    
    MAP = hydra.core.Name("map")
    
    MAYBE = hydra.core.Name("maybe")
    
    PAIR = hydra.core.Name("pair")
    
    RECORD = hydra.core.Name("record")
    
    SET = hydra.core.Name("set")
    
    TYPE_APPLICATION = hydra.core.Name("typeApplication")
    
    TYPE_LAMBDA = hydra.core.Name("typeLambda")
    
    UNION = hydra.core.Name("union")
    
    UNIT = hydra.core.Name("unit")
    
    VARIABLE = hydra.core.Name("variable")
    
    WRAP = hydra.core.Name("wrap")

TermVariant.TYPE_ = hydra.core.Name("hydra.variants.TermVariant")

class TypeVariant(Enum):
    r"""The identifier of a type constructor."""
    
    ANNOTATED = hydra.core.Name("annotated")
    
    APPLICATION = hydra.core.Name("application")
    
    EITHER = hydra.core.Name("either")
    
    FORALL = hydra.core.Name("forall")
    
    FUNCTION = hydra.core.Name("function")
    
    LIST = hydra.core.Name("list")
    
    LITERAL = hydra.core.Name("literal")
    
    MAP = hydra.core.Name("map")
    
    MAYBE = hydra.core.Name("maybe")
    
    PAIR = hydra.core.Name("pair")
    
    RECORD = hydra.core.Name("record")
    
    SET = hydra.core.Name("set")
    
    UNION = hydra.core.Name("union")
    
    UNIT = hydra.core.Name("unit")
    
    VARIABLE = hydra.core.Name("variable")
    
    WRAP = hydra.core.Name("wrap")

TypeVariant.TYPE_ = hydra.core.Name("hydra.variants.TypeVariant")
