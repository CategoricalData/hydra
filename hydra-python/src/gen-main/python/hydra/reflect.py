# Note: this is an automatically generated file. Do not edit.

r"""Reflection functions for working with term, type, and literal type variants, as well as numeric precision."""

from __future__ import annotations
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Maybe, frozenlist
from typing import cast
import hydra.core
import hydra.lib.lists
import hydra.util
import hydra.variants

def elimination_variant(v1: hydra.core.Elimination) -> hydra.variants.EliminationVariant:
    r"""Find the elimination inject (constructor) for a given elimination term."""
    
    match v1:
        case hydra.core.EliminationRecord():
            return hydra.variants.EliminationVariant.RECORD
        
        case hydra.core.EliminationUnion():
            return hydra.variants.EliminationVariant.UNION
        
        case hydra.core.EliminationWrap():
            return hydra.variants.EliminationVariant.WRAP
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

# All elimination variants (constructors), in a canonical order.
elimination_variants = (hydra.variants.EliminationVariant.RECORD, hydra.variants.EliminationVariant.UNION, hydra.variants.EliminationVariant.WRAP)

def float_type_precision(v1: hydra.core.FloatType) -> hydra.util.Precision:
    r"""Find the precision of a given floating-point type."""
    
    match v1:
        case hydra.core.FloatType.BIGFLOAT:
            return cast(hydra.util.Precision, hydra.util.PrecisionArbitrary())
        
        case hydra.core.FloatType.FLOAT32:
            return cast(hydra.util.Precision, hydra.util.PrecisionBits(32))
        
        case hydra.core.FloatType.FLOAT64:
            return cast(hydra.util.Precision, hydra.util.PrecisionBits(64))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

# All floating-point types in a canonical order.
float_types = (hydra.core.FloatType.BIGFLOAT, hydra.core.FloatType.FLOAT32, hydra.core.FloatType.FLOAT64)

def float_value_type(v1: hydra.core.FloatValue) -> hydra.core.FloatType:
    r"""Find the float type for a given floating-point value."""
    
    match v1:
        case hydra.core.FloatValueBigfloat():
            return hydra.core.FloatType.BIGFLOAT
        
        case hydra.core.FloatValueFloat32():
            return hydra.core.FloatType.FLOAT32
        
        case hydra.core.FloatValueFloat64():
            return hydra.core.FloatType.FLOAT64
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def function_variant(v1: hydra.core.Function) -> hydra.variants.FunctionVariant:
    r"""Find the function inject (constructor) for a given function."""
    
    match v1:
        case hydra.core.FunctionElimination():
            return hydra.variants.FunctionVariant.ELIMINATION
        
        case hydra.core.FunctionLambda():
            return hydra.variants.FunctionVariant.LAMBDA
        
        case hydra.core.FunctionPrimitive():
            return hydra.variants.FunctionVariant.PRIMITIVE
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

# All function variants (constructors), in a canonical order.
function_variants = (hydra.variants.FunctionVariant.ELIMINATION, hydra.variants.FunctionVariant.LAMBDA, hydra.variants.FunctionVariant.PRIMITIVE)

def integer_type_is_signed(v1: hydra.core.IntegerType) -> bool:
    r"""Find whether a given integer type is signed (true) or unsigned (false)."""
    
    match v1:
        case hydra.core.IntegerType.BIGINT:
            return True
        
        case hydra.core.IntegerType.INT8:
            return True
        
        case hydra.core.IntegerType.INT16:
            return True
        
        case hydra.core.IntegerType.INT32:
            return True
        
        case hydra.core.IntegerType.INT64:
            return True
        
        case hydra.core.IntegerType.UINT8:
            return False
        
        case hydra.core.IntegerType.UINT16:
            return False
        
        case hydra.core.IntegerType.UINT32:
            return False
        
        case hydra.core.IntegerType.UINT64:
            return False
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def integer_type_precision(v1: hydra.core.IntegerType) -> hydra.util.Precision:
    r"""Find the precision of a given integer type."""
    
    match v1:
        case hydra.core.IntegerType.BIGINT:
            return cast(hydra.util.Precision, hydra.util.PrecisionArbitrary())
        
        case hydra.core.IntegerType.INT8:
            return cast(hydra.util.Precision, hydra.util.PrecisionBits(8))
        
        case hydra.core.IntegerType.INT16:
            return cast(hydra.util.Precision, hydra.util.PrecisionBits(16))
        
        case hydra.core.IntegerType.INT32:
            return cast(hydra.util.Precision, hydra.util.PrecisionBits(32))
        
        case hydra.core.IntegerType.INT64:
            return cast(hydra.util.Precision, hydra.util.PrecisionBits(64))
        
        case hydra.core.IntegerType.UINT8:
            return cast(hydra.util.Precision, hydra.util.PrecisionBits(8))
        
        case hydra.core.IntegerType.UINT16:
            return cast(hydra.util.Precision, hydra.util.PrecisionBits(16))
        
        case hydra.core.IntegerType.UINT32:
            return cast(hydra.util.Precision, hydra.util.PrecisionBits(32))
        
        case hydra.core.IntegerType.UINT64:
            return cast(hydra.util.Precision, hydra.util.PrecisionBits(64))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

# All integer types, in a canonical order.
integer_types = (hydra.core.IntegerType.BIGINT, hydra.core.IntegerType.INT8, hydra.core.IntegerType.INT16, hydra.core.IntegerType.INT32, hydra.core.IntegerType.INT64, hydra.core.IntegerType.UINT8, hydra.core.IntegerType.UINT16, hydra.core.IntegerType.UINT32, hydra.core.IntegerType.UINT64)

def integer_value_type(v1: hydra.core.IntegerValue) -> hydra.core.IntegerType:
    r"""Find the integer type for a given integer value."""
    
    match v1:
        case hydra.core.IntegerValueBigint():
            return hydra.core.IntegerType.BIGINT
        
        case hydra.core.IntegerValueInt8():
            return hydra.core.IntegerType.INT8
        
        case hydra.core.IntegerValueInt16():
            return hydra.core.IntegerType.INT16
        
        case hydra.core.IntegerValueInt32():
            return hydra.core.IntegerType.INT32
        
        case hydra.core.IntegerValueInt64():
            return hydra.core.IntegerType.INT64
        
        case hydra.core.IntegerValueUint8():
            return hydra.core.IntegerType.UINT8
        
        case hydra.core.IntegerValueUint16():
            return hydra.core.IntegerType.UINT16
        
        case hydra.core.IntegerValueUint32():
            return hydra.core.IntegerType.UINT32
        
        case hydra.core.IntegerValueUint64():
            return hydra.core.IntegerType.UINT64
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def literal_type(v1: hydra.core.Literal) -> hydra.core.LiteralType:
    r"""Find the literal type for a given literal value."""
    
    match v1:
        case hydra.core.LiteralBinary():
            return cast(hydra.core.LiteralType, hydra.core.LiteralTypeBinary())
        
        case hydra.core.LiteralBoolean():
            return cast(hydra.core.LiteralType, hydra.core.LiteralTypeBoolean())
        
        case hydra.core.LiteralFloat(value=arg_):
            return cast(hydra.core.LiteralType, hydra.core.LiteralTypeFloat(float_value_type(arg_)))
        
        case hydra.core.LiteralInteger(value=arg_2):
            return cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(integer_value_type(arg_2)))
        
        case hydra.core.LiteralString():
            return cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def literal_type_variant(v1: hydra.core.LiteralType) -> hydra.variants.LiteralVariant:
    r"""Find the literal type inject (constructor) for a given literal value."""
    
    match v1:
        case hydra.core.LiteralTypeBinary():
            return hydra.variants.LiteralVariant.BINARY
        
        case hydra.core.LiteralTypeBoolean():
            return hydra.variants.LiteralVariant.BOOLEAN
        
        case hydra.core.LiteralTypeFloat():
            return hydra.variants.LiteralVariant.FLOAT
        
        case hydra.core.LiteralTypeInteger():
            return hydra.variants.LiteralVariant.INTEGER
        
        case hydra.core.LiteralTypeString():
            return hydra.variants.LiteralVariant.STRING
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

@lru_cache(1)
def literal_types() -> frozenlist[hydra.core.LiteralType]:
    r"""All literal types, in a canonical order."""
    
    return hydra.lib.lists.concat(((cast(hydra.core.LiteralType, hydra.core.LiteralTypeBinary()), cast(hydra.core.LiteralType, hydra.core.LiteralTypeBoolean())), hydra.lib.lists.map((lambda x: cast(hydra.core.LiteralType, hydra.core.LiteralTypeFloat(x))), float_types), hydra.lib.lists.map((lambda x: cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(x))), integer_types), (cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()),)))

def literal_variant(arg_: hydra.core.Literal) -> hydra.variants.LiteralVariant:
    r"""Find the literal inject (constructor) for a given literal value."""
    
    return literal_type_variant(literal_type(arg_))

# All literal variants, in a canonical order.
literal_variants = (hydra.variants.LiteralVariant.BINARY, hydra.variants.LiteralVariant.BOOLEAN, hydra.variants.LiteralVariant.FLOAT, hydra.variants.LiteralVariant.INTEGER, hydra.variants.LiteralVariant.STRING)

def term_variant(v1: hydra.core.Term) -> hydra.variants.TermVariant:
    r"""Find the term inject (constructor) for a given term."""
    
    match v1:
        case hydra.core.TermAnnotated():
            return hydra.variants.TermVariant.ANNOTATED
        
        case hydra.core.TermApplication():
            return hydra.variants.TermVariant.APPLICATION
        
        case hydra.core.TermEither():
            return hydra.variants.TermVariant.EITHER
        
        case hydra.core.TermFunction():
            return hydra.variants.TermVariant.FUNCTION
        
        case hydra.core.TermLet():
            return hydra.variants.TermVariant.LET
        
        case hydra.core.TermList():
            return hydra.variants.TermVariant.LIST
        
        case hydra.core.TermLiteral():
            return hydra.variants.TermVariant.LITERAL
        
        case hydra.core.TermMap():
            return hydra.variants.TermVariant.MAP
        
        case hydra.core.TermMaybe():
            return hydra.variants.TermVariant.MAYBE
        
        case hydra.core.TermPair():
            return hydra.variants.TermVariant.PAIR
        
        case hydra.core.TermRecord():
            return hydra.variants.TermVariant.RECORD
        
        case hydra.core.TermSet():
            return hydra.variants.TermVariant.SET
        
        case hydra.core.TermTypeApplication():
            return hydra.variants.TermVariant.TYPE_APPLICATION
        
        case hydra.core.TermTypeLambda():
            return hydra.variants.TermVariant.TYPE_LAMBDA
        
        case hydra.core.TermUnion():
            return hydra.variants.TermVariant.UNION
        
        case hydra.core.TermUnit():
            return hydra.variants.TermVariant.UNIT
        
        case hydra.core.TermVariable():
            return hydra.variants.TermVariant.VARIABLE
        
        case hydra.core.TermWrap():
            return hydra.variants.TermVariant.WRAP
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

# All term (expression) variants, in a canonical order.
term_variants = (hydra.variants.TermVariant.ANNOTATED, hydra.variants.TermVariant.APPLICATION, hydra.variants.TermVariant.EITHER, hydra.variants.TermVariant.FUNCTION, hydra.variants.TermVariant.LIST, hydra.variants.TermVariant.LITERAL, hydra.variants.TermVariant.MAP, hydra.variants.TermVariant.MAYBE, hydra.variants.TermVariant.PAIR, hydra.variants.TermVariant.RECORD, hydra.variants.TermVariant.SET, hydra.variants.TermVariant.TYPE_LAMBDA, hydra.variants.TermVariant.TYPE_APPLICATION, hydra.variants.TermVariant.UNION, hydra.variants.TermVariant.UNIT, hydra.variants.TermVariant.VARIABLE, hydra.variants.TermVariant.WRAP)

def type_variant(v1: hydra.core.Type) -> hydra.variants.TypeVariant:
    r"""Find the type inject (constructor) for a given type."""
    
    match v1:
        case hydra.core.TypeAnnotated():
            return hydra.variants.TypeVariant.ANNOTATED
        
        case hydra.core.TypeApplication():
            return hydra.variants.TypeVariant.APPLICATION
        
        case hydra.core.TypeEither():
            return hydra.variants.TypeVariant.EITHER
        
        case hydra.core.TypeFunction():
            return hydra.variants.TypeVariant.FUNCTION
        
        case hydra.core.TypeForall():
            return hydra.variants.TypeVariant.FORALL
        
        case hydra.core.TypeList():
            return hydra.variants.TypeVariant.LIST
        
        case hydra.core.TypeLiteral():
            return hydra.variants.TypeVariant.LITERAL
        
        case hydra.core.TypeMap():
            return hydra.variants.TypeVariant.MAP
        
        case hydra.core.TypeMaybe():
            return hydra.variants.TypeVariant.MAYBE
        
        case hydra.core.TypePair():
            return hydra.variants.TypeVariant.PAIR
        
        case hydra.core.TypeRecord():
            return hydra.variants.TypeVariant.RECORD
        
        case hydra.core.TypeSet():
            return hydra.variants.TypeVariant.SET
        
        case hydra.core.TypeUnion():
            return hydra.variants.TypeVariant.UNION
        
        case hydra.core.TypeUnit():
            return hydra.variants.TypeVariant.UNIT
        
        case hydra.core.TypeVariable():
            return hydra.variants.TypeVariant.VARIABLE
        
        case hydra.core.TypeWrap():
            return hydra.variants.TypeVariant.WRAP
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

# All type variants, in a canonical order.
type_variants = (hydra.variants.TypeVariant.ANNOTATED, hydra.variants.TypeVariant.APPLICATION, hydra.variants.TypeVariant.EITHER, hydra.variants.TypeVariant.FUNCTION, hydra.variants.TypeVariant.FORALL, hydra.variants.TypeVariant.LIST, hydra.variants.TypeVariant.LITERAL, hydra.variants.TypeVariant.MAP, hydra.variants.TypeVariant.WRAP, hydra.variants.TypeVariant.MAYBE, hydra.variants.TypeVariant.PAIR, hydra.variants.TypeVariant.RECORD, hydra.variants.TypeVariant.SET, hydra.variants.TypeVariant.UNION, hydra.variants.TypeVariant.UNIT, hydra.variants.TypeVariant.VARIABLE)
