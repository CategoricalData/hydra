"""Functions for working with term, type, and literal type variants, as well as numeric precision."""

from __future__ import annotations
import hydra.core
import hydra.lib.lists
import hydra.mantle

def elimination_variant(v1: hydra.core.Elimination) -> hydra.mantle.EliminationVariant:
    """Find the elimination variant (constructor) for a given elimination term."""
    
    match v1:
        case hydra.core.EliminationProduct():
            return hydra.mantle.EliminationVariant.PRODUCT
        
        case hydra.core.EliminationRecord():
            return hydra.mantle.EliminationVariant.RECORD
        
        case hydra.core.EliminationUnion():
            return hydra.mantle.EliminationVariant.UNION
        
        case hydra.core.EliminationWrap():
            return hydra.mantle.EliminationVariant.WRAP

# All elimination variants (constructors), in a canonical order.
elimination_variants = (hydra.mantle.EliminationVariant.PRODUCT, hydra.mantle.EliminationVariant.RECORD, hydra.mantle.EliminationVariant.UNION, hydra.mantle.EliminationVariant.WRAP)

def float_type_precision(v1: hydra.core.FloatType) -> hydra.mantle.Precision:
    """Find the precision of a given floating-point type."""
    
    match v1:
        case hydra.core.FloatType.BIGFLOAT:
            return hydra.mantle.PrecisionArbitrary(None)
        
        case hydra.core.FloatType.FLOAT32:
            return hydra.mantle.PrecisionBits(32)
        
        case hydra.core.FloatType.FLOAT64:
            return hydra.mantle.PrecisionBits(64)

# All floating-point types in a canonical order.
float_types = (hydra.core.FloatType.BIGFLOAT, hydra.core.FloatType.FLOAT32, hydra.core.FloatType.FLOAT64)

def float_value_type(v1: hydra.core.FloatValue) -> hydra.core.FloatType:
    """Find the float type for a given floating-point value."""
    
    match v1:
        case hydra.core.FloatValueBigfloat():
            return hydra.core.FloatType.BIGFLOAT
        
        case hydra.core.FloatValueFloat32():
            return hydra.core.FloatType.FLOAT32
        
        case hydra.core.FloatValueFloat64():
            return hydra.core.FloatType.FLOAT64

def function_variant(v1: hydra.core.Function) -> hydra.mantle.FunctionVariant:
    """Find the function variant (constructor) for a given function."""
    
    match v1:
        case hydra.core.FunctionElimination():
            return hydra.mantle.FunctionVariant.ELIMINATION
        
        case hydra.core.FunctionLambda():
            return hydra.mantle.FunctionVariant.LAMBDA
        
        case hydra.core.FunctionPrimitive():
            return hydra.mantle.FunctionVariant.PRIMITIVE

# All function variants (constructors), in a canonical order.
function_variants = (hydra.mantle.FunctionVariant.ELIMINATION, hydra.mantle.FunctionVariant.LAMBDA, hydra.mantle.FunctionVariant.PRIMITIVE)

def integer_type_is_signed(v1: hydra.core.IntegerType) -> bool:
    """Find whether a given integer type is signed (true) or unsigned (false)."""
    
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

def integer_type_precision(v1: hydra.core.IntegerType) -> hydra.mantle.Precision:
    """Find the precision of a given integer type."""
    
    match v1:
        case hydra.core.IntegerType.BIGINT:
            return hydra.mantle.PrecisionArbitrary(None)
        
        case hydra.core.IntegerType.INT8:
            return hydra.mantle.PrecisionBits(8)
        
        case hydra.core.IntegerType.INT16:
            return hydra.mantle.PrecisionBits(16)
        
        case hydra.core.IntegerType.INT32:
            return hydra.mantle.PrecisionBits(32)
        
        case hydra.core.IntegerType.INT64:
            return hydra.mantle.PrecisionBits(64)
        
        case hydra.core.IntegerType.UINT8:
            return hydra.mantle.PrecisionBits(8)
        
        case hydra.core.IntegerType.UINT16:
            return hydra.mantle.PrecisionBits(16)
        
        case hydra.core.IntegerType.UINT32:
            return hydra.mantle.PrecisionBits(32)
        
        case hydra.core.IntegerType.UINT64:
            return hydra.mantle.PrecisionBits(64)

# All integer types, in a canonical order.
integer_types = (hydra.core.IntegerType.BIGINT, hydra.core.IntegerType.INT8, hydra.core.IntegerType.INT16, hydra.core.IntegerType.INT32, hydra.core.IntegerType.INT64, hydra.core.IntegerType.UINT8, hydra.core.IntegerType.UINT16, hydra.core.IntegerType.UINT32, hydra.core.IntegerType.UINT64)

def integer_value_type(v1: hydra.core.IntegerValue) -> hydra.core.IntegerType:
    """Find the integer type for a given integer value."""
    
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

def literal_type(v1: hydra.core.Literal) -> hydra.core.LiteralType:
    """Find the literal type for a given literal value."""
    
    match v1:
        case hydra.core.LiteralBinary():
            return hydra.core.LiteralTypeBinary(None)
        
        case hydra.core.LiteralBoolean():
            return hydra.core.LiteralTypeBoolean(None)
        
        case hydra.core.LiteralFloat(value=arg_):
            return hydra.core.LiteralTypeFloat(float_value_type(arg_))
        
        case hydra.core.LiteralInteger(value=arg_2):
            return hydra.core.LiteralTypeInteger(integer_value_type(arg_2))
        
        case hydra.core.LiteralString():
            return hydra.core.LiteralTypeString(None)

def literal_type_variant(v1: hydra.core.LiteralType) -> hydra.mantle.LiteralVariant:
    """Find the literal type variant (constructor) for a given literal value."""
    
    match v1:
        case hydra.core.LiteralTypeBinary():
            return hydra.mantle.LiteralVariant.BINARY
        
        case hydra.core.LiteralTypeBoolean():
            return hydra.mantle.LiteralVariant.BOOLEAN
        
        case hydra.core.LiteralTypeFloat():
            return hydra.mantle.LiteralVariant.FLOAT
        
        case hydra.core.LiteralTypeInteger():
            return hydra.mantle.LiteralVariant.INTEGER
        
        case hydra.core.LiteralTypeString():
            return hydra.mantle.LiteralVariant.STRING

# All literal types, in a canonical order.
literal_types = hydra.lib.lists.concat(((hydra.core.LiteralTypeBinary(None), hydra.core.LiteralTypeBoolean(None)), hydra.lib.lists.map(lambda x: hydra.core.LiteralTypeFloat(x), float_types), hydra.lib.lists.map(lambda x: hydra.core.LiteralTypeInteger(x), integer_types), (hydra.core.LiteralTypeString(None),)))

def literal_variant(arg_: hydra.core.Literal) -> hydra.mantle.LiteralVariant:
    """Find the literal variant (constructor) for a given literal value."""
    
    return literal_type_variant(literal_type(arg_))

# All literal variants, in a canonical order.
literal_variants = (hydra.mantle.LiteralVariant.BINARY, hydra.mantle.LiteralVariant.BOOLEAN, hydra.mantle.LiteralVariant.FLOAT, hydra.mantle.LiteralVariant.INTEGER, hydra.mantle.LiteralVariant.STRING)

def term_variant(v1: hydra.core.Term) -> hydra.mantle.TermVariant:
    """Find the term variant (constructor) for a given term."""
    
    match v1:
        case hydra.core.TermAnnotated():
            return hydra.mantle.TermVariant.ANNOTATED
        
        case hydra.core.TermApplication():
            return hydra.mantle.TermVariant.APPLICATION
        
        case hydra.core.TermFunction():
            return hydra.mantle.TermVariant.FUNCTION
        
        case hydra.core.TermLet():
            return hydra.mantle.TermVariant.LET
        
        case hydra.core.TermList():
            return hydra.mantle.TermVariant.LIST
        
        case hydra.core.TermLiteral():
            return hydra.mantle.TermVariant.LITERAL
        
        case hydra.core.TermMap():
            return hydra.mantle.TermVariant.MAP
        
        case hydra.core.TermOptional():
            return hydra.mantle.TermVariant.OPTIONAL
        
        case hydra.core.TermProduct():
            return hydra.mantle.TermVariant.PRODUCT
        
        case hydra.core.TermRecord():
            return hydra.mantle.TermVariant.RECORD
        
        case hydra.core.TermSet():
            return hydra.mantle.TermVariant.SET
        
        case hydra.core.TermSum():
            return hydra.mantle.TermVariant.SUM
        
        case hydra.core.TermTypeLambda():
            return hydra.mantle.TermVariant.TYPE_LAMBDA
        
        case hydra.core.TermTypeApplication():
            return hydra.mantle.TermVariant.TYPE_APPLICATION
        
        case hydra.core.TermUnion():
            return hydra.mantle.TermVariant.UNION
        
        case hydra.core.TermUnit():
            return hydra.mantle.TermVariant.UNIT
        
        case hydra.core.TermVariable():
            return hydra.mantle.TermVariant.VARIABLE
        
        case hydra.core.TermWrap():
            return hydra.mantle.TermVariant.WRAP

# All term (expression) variants, in a canonical order.
term_variants = (hydra.mantle.TermVariant.ANNOTATED, hydra.mantle.TermVariant.APPLICATION, hydra.mantle.TermVariant.LITERAL, hydra.mantle.TermVariant.FUNCTION, hydra.mantle.TermVariant.LIST, hydra.mantle.TermVariant.MAP, hydra.mantle.TermVariant.OPTIONAL, hydra.mantle.TermVariant.PRODUCT, hydra.mantle.TermVariant.RECORD, hydra.mantle.TermVariant.SET, hydra.mantle.TermVariant.SUM, hydra.mantle.TermVariant.TYPE_LAMBDA, hydra.mantle.TermVariant.TYPE_APPLICATION, hydra.mantle.TermVariant.UNION, hydra.mantle.TermVariant.UNIT, hydra.mantle.TermVariant.VARIABLE, hydra.mantle.TermVariant.WRAP)

def type_variant(v1: hydra.core.Type) -> hydra.mantle.TypeVariant:
    """Find the type variant (constructor) for a given type."""
    
    match v1:
        case hydra.core.TypeAnnotated():
            return hydra.mantle.TypeVariant.ANNOTATED
        
        case hydra.core.TypeApplication():
            return hydra.mantle.TypeVariant.APPLICATION
        
        case hydra.core.TypeFunction():
            return hydra.mantle.TypeVariant.FUNCTION
        
        case hydra.core.TypeForall():
            return hydra.mantle.TypeVariant.FORALL
        
        case hydra.core.TypeList():
            return hydra.mantle.TypeVariant.LIST
        
        case hydra.core.TypeLiteral():
            return hydra.mantle.TypeVariant.LITERAL
        
        case hydra.core.TypeMap():
            return hydra.mantle.TypeVariant.MAP
        
        case hydra.core.TypeOptional():
            return hydra.mantle.TypeVariant.OPTIONAL
        
        case hydra.core.TypeProduct():
            return hydra.mantle.TypeVariant.PRODUCT
        
        case hydra.core.TypeRecord():
            return hydra.mantle.TypeVariant.RECORD
        
        case hydra.core.TypeSet():
            return hydra.mantle.TypeVariant.SET
        
        case hydra.core.TypeSum():
            return hydra.mantle.TypeVariant.SUM
        
        case hydra.core.TypeUnion():
            return hydra.mantle.TypeVariant.UNION
        
        case hydra.core.TypeUnit():
            return hydra.mantle.TypeVariant.UNIT
        
        case hydra.core.TypeVariable():
            return hydra.mantle.TypeVariant.VARIABLE
        
        case hydra.core.TypeWrap():
            return hydra.mantle.TypeVariant.WRAP

# All type variants, in a canonical order.
type_variants = (hydra.mantle.TypeVariant.ANNOTATED, hydra.mantle.TypeVariant.APPLICATION, hydra.mantle.TypeVariant.FUNCTION, hydra.mantle.TypeVariant.FORALL, hydra.mantle.TypeVariant.LIST, hydra.mantle.TypeVariant.LITERAL, hydra.mantle.TypeVariant.MAP, hydra.mantle.TypeVariant.WRAP, hydra.mantle.TypeVariant.OPTIONAL, hydra.mantle.TypeVariant.PRODUCT, hydra.mantle.TypeVariant.RECORD, hydra.mantle.TypeVariant.SET, hydra.mantle.TypeVariant.SUM, hydra.mantle.TypeVariant.UNION, hydra.mantle.TypeVariant.UNIT, hydra.mantle.TypeVariant.VARIABLE)
