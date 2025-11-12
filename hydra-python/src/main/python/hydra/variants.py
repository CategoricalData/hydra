# Note: this is an automatically generated file. Do not edit.

r"""Functions for working with term, type, and literal type variants, as well as numeric precision."""

from __future__ import annotations
from hydra.dsl.python import frozenlist
from typing import cast
import hydra.core
import hydra.lib.lists
import hydra.meta
import hydra.util

def elimination_variant(v1: hydra.core.Elimination) -> hydra.meta.EliminationVariant:
    r"""Find the elimination variant (constructor) for a given elimination term."""
    
    match v1:
        case hydra.core.EliminationProduct():
            return hydra.meta.EliminationVariant.PRODUCT
        
        case hydra.core.EliminationRecord():
            return hydra.meta.EliminationVariant.RECORD
        
        case hydra.core.EliminationUnion():
            return hydra.meta.EliminationVariant.UNION
        
        case hydra.core.EliminationWrap():
            return hydra.meta.EliminationVariant.WRAP
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

# All elimination variants (constructors), in a canonical order.
elimination_variants = (hydra.meta.EliminationVariant.PRODUCT, hydra.meta.EliminationVariant.RECORD, hydra.meta.EliminationVariant.UNION, hydra.meta.EliminationVariant.WRAP)

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

def function_variant(v1: hydra.core.Function) -> hydra.meta.FunctionVariant:
    r"""Find the function variant (constructor) for a given function."""
    
    match v1:
        case hydra.core.FunctionElimination():
            return hydra.meta.FunctionVariant.ELIMINATION
        
        case hydra.core.FunctionLambda():
            return hydra.meta.FunctionVariant.LAMBDA
        
        case hydra.core.FunctionPrimitive():
            return hydra.meta.FunctionVariant.PRIMITIVE
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

# All function variants (constructors), in a canonical order.
function_variants = (hydra.meta.FunctionVariant.ELIMINATION, hydra.meta.FunctionVariant.LAMBDA, hydra.meta.FunctionVariant.PRIMITIVE)

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
            return (lambda injected_: cast(hydra.core.LiteralType, hydra.core.LiteralTypeFloat(injected_)))(float_value_type(arg_))
        
        case hydra.core.LiteralInteger(value=arg_2):
            return (lambda injected_: cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(injected_)))(integer_value_type(arg_2))
        
        case hydra.core.LiteralString():
            return cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def literal_type_variant(v1: hydra.core.LiteralType) -> hydra.meta.LiteralVariant:
    r"""Find the literal type variant (constructor) for a given literal value."""
    
    match v1:
        case hydra.core.LiteralTypeBinary():
            return hydra.meta.LiteralVariant.BINARY
        
        case hydra.core.LiteralTypeBoolean():
            return hydra.meta.LiteralVariant.BOOLEAN
        
        case hydra.core.LiteralTypeFloat():
            return hydra.meta.LiteralVariant.FLOAT
        
        case hydra.core.LiteralTypeInteger():
            return hydra.meta.LiteralVariant.INTEGER
        
        case hydra.core.LiteralTypeString():
            return hydra.meta.LiteralVariant.STRING
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

# All literal types, in a canonical order.
literal_types = hydra.lib.lists.concat(((cast(hydra.core.LiteralType, hydra.core.LiteralTypeBinary()), cast(hydra.core.LiteralType, hydra.core.LiteralTypeBoolean())), hydra.lib.lists.map((lambda x: cast(hydra.core.LiteralType, hydra.core.LiteralTypeFloat(x))), float_types), hydra.lib.lists.map((lambda x: cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(x))), integer_types), (cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()),)))

def literal_variant(arg_: hydra.core.Literal) -> hydra.meta.LiteralVariant:
    r"""Find the literal variant (constructor) for a given literal value."""
    
    return literal_type_variant(literal_type(arg_))

# All literal variants, in a canonical order.
literal_variants = (hydra.meta.LiteralVariant.BINARY, hydra.meta.LiteralVariant.BOOLEAN, hydra.meta.LiteralVariant.FLOAT, hydra.meta.LiteralVariant.INTEGER, hydra.meta.LiteralVariant.STRING)

def term_variant(v1: hydra.core.Term) -> hydra.meta.TermVariant:
    r"""Find the term variant (constructor) for a given term."""
    
    match v1:
        case hydra.core.TermAnnotated():
            return hydra.meta.TermVariant.ANNOTATED
        
        case hydra.core.TermApplication():
            return hydra.meta.TermVariant.APPLICATION
        
        case hydra.core.TermEither():
            return hydra.meta.TermVariant.EITHER
        
        case hydra.core.TermFunction():
            return hydra.meta.TermVariant.FUNCTION
        
        case hydra.core.TermLet():
            return hydra.meta.TermVariant.LET
        
        case hydra.core.TermList():
            return hydra.meta.TermVariant.LIST
        
        case hydra.core.TermLiteral():
            return hydra.meta.TermVariant.LITERAL
        
        case hydra.core.TermMap():
            return hydra.meta.TermVariant.MAP
        
        case hydra.core.TermMaybe():
            return hydra.meta.TermVariant.MAYBE
        
        case hydra.core.TermPair():
            return hydra.meta.TermVariant.PAIR
        
        case hydra.core.TermProduct():
            return hydra.meta.TermVariant.PRODUCT
        
        case hydra.core.TermRecord():
            return hydra.meta.TermVariant.RECORD
        
        case hydra.core.TermSet():
            return hydra.meta.TermVariant.SET
        
        case hydra.core.TermSum():
            return hydra.meta.TermVariant.SUM
        
        case hydra.core.TermTypeApplication():
            return hydra.meta.TermVariant.TYPE_APPLICATION
        
        case hydra.core.TermTypeLambda():
            return hydra.meta.TermVariant.TYPE_LAMBDA
        
        case hydra.core.TermUnion():
            return hydra.meta.TermVariant.UNION
        
        case hydra.core.TermUnit():
            return hydra.meta.TermVariant.UNIT
        
        case hydra.core.TermVariable():
            return hydra.meta.TermVariant.VARIABLE
        
        case hydra.core.TermWrap():
            return hydra.meta.TermVariant.WRAP
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

# All term (expression) variants, in a canonical order.
term_variants = (hydra.meta.TermVariant.ANNOTATED, hydra.meta.TermVariant.APPLICATION, hydra.meta.TermVariant.EITHER, hydra.meta.TermVariant.FUNCTION, hydra.meta.TermVariant.LIST, hydra.meta.TermVariant.LITERAL, hydra.meta.TermVariant.MAP, hydra.meta.TermVariant.MAYBE, hydra.meta.TermVariant.PAIR, hydra.meta.TermVariant.PRODUCT, hydra.meta.TermVariant.RECORD, hydra.meta.TermVariant.SET, hydra.meta.TermVariant.SUM, hydra.meta.TermVariant.TYPE_LAMBDA, hydra.meta.TermVariant.TYPE_APPLICATION, hydra.meta.TermVariant.UNION, hydra.meta.TermVariant.UNIT, hydra.meta.TermVariant.VARIABLE, hydra.meta.TermVariant.WRAP)

def type_variant(v1: hydra.core.Type) -> hydra.meta.TypeVariant:
    r"""Find the type variant (constructor) for a given type."""
    
    match v1:
        case hydra.core.TypeAnnotated():
            return hydra.meta.TypeVariant.ANNOTATED
        
        case hydra.core.TypeApplication():
            return hydra.meta.TypeVariant.APPLICATION
        
        case hydra.core.TypeEither():
            return hydra.meta.TypeVariant.EITHER
        
        case hydra.core.TypeFunction():
            return hydra.meta.TypeVariant.FUNCTION
        
        case hydra.core.TypeForall():
            return hydra.meta.TypeVariant.FORALL
        
        case hydra.core.TypeList():
            return hydra.meta.TypeVariant.LIST
        
        case hydra.core.TypeLiteral():
            return hydra.meta.TypeVariant.LITERAL
        
        case hydra.core.TypeMap():
            return hydra.meta.TypeVariant.MAP
        
        case hydra.core.TypeMaybe():
            return hydra.meta.TypeVariant.MAYBE
        
        case hydra.core.TypePair():
            return hydra.meta.TypeVariant.PAIR
        
        case hydra.core.TypeProduct():
            return hydra.meta.TypeVariant.PRODUCT
        
        case hydra.core.TypeRecord():
            return hydra.meta.TypeVariant.RECORD
        
        case hydra.core.TypeSet():
            return hydra.meta.TypeVariant.SET
        
        case hydra.core.TypeSum():
            return hydra.meta.TypeVariant.SUM
        
        case hydra.core.TypeUnion():
            return hydra.meta.TypeVariant.UNION
        
        case hydra.core.TypeUnit():
            return hydra.meta.TypeVariant.UNIT
        
        case hydra.core.TypeVariable():
            return hydra.meta.TypeVariant.VARIABLE
        
        case hydra.core.TypeWrap():
            return hydra.meta.TypeVariant.WRAP
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

# All type variants, in a canonical order.
type_variants = (hydra.meta.TypeVariant.ANNOTATED, hydra.meta.TypeVariant.APPLICATION, hydra.meta.TypeVariant.EITHER, hydra.meta.TypeVariant.FUNCTION, hydra.meta.TypeVariant.FORALL, hydra.meta.TypeVariant.LIST, hydra.meta.TypeVariant.LITERAL, hydra.meta.TypeVariant.MAP, hydra.meta.TypeVariant.WRAP, hydra.meta.TypeVariant.MAYBE, hydra.meta.TypeVariant.PAIR, hydra.meta.TypeVariant.PRODUCT, hydra.meta.TypeVariant.RECORD, hydra.meta.TypeVariant.SET, hydra.meta.TypeVariant.SUM, hydra.meta.TypeVariant.UNION, hydra.meta.TypeVariant.UNIT, hydra.meta.TypeVariant.VARIABLE)
