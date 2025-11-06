# Note: this is an automatically generated file. Do not edit.

r"""Natural-language descriptions for hydra.core types."""

from __future__ import annotations
import hydra.core
import hydra.describe.mantle
import hydra.lib.strings
import hydra.variants

def float_type(t: hydra.core.FloatType) -> str:
    r"""Display a floating-point type as a string."""
    
    return hydra.lib.strings.cat(((lambda arg_: hydra.describe.mantle.precision(hydra.variants.float_type_precision(arg_)))(t), " floating-point number"))

def integer_type(t: hydra.core.IntegerType) -> str:
    r"""Display an integer type as a string."""
    
    return hydra.lib.strings.cat(((lambda arg_: hydra.describe.mantle.precision(hydra.variants.integer_type_precision(arg_)))(t), " integer"))

def literal_type(v1: hydra.core.LiteralType) -> str:
    r"""Display a literal type as a string."""
    
    match v1:
        case hydra.core.LiteralTypeBinary():
            return "binary string"
        
        case hydra.core.LiteralTypeBoolean():
            return "boolean value"
        
        case hydra.core.LiteralTypeFloat(value=v12):
            return float_type(v12)
        
        case hydra.core.LiteralTypeInteger(value=v122):
            return integer_type(v122)
        
        case hydra.core.LiteralTypeString():
            return "character string"

def type(v1: hydra.core.Type) -> str:
    r"""Display a type as a string."""
    
    match v1:
        case hydra.core.TypeAnnotated(value=a):
            return hydra.lib.strings.cat(("annotated ", type(a.body)))
        
        case hydra.core.TypeApplication(value=at):
            return hydra.lib.strings.cat((type(at.function), " applied to ", type(at.argument)))
        
        case hydra.core.TypeEither(value=et):
            return hydra.lib.strings.cat((hydra.lib.strings.cat((hydra.lib.strings.cat(("either ", type(et.left))), " or ")), type(et.right)))
        
        case hydra.core.TypeLiteral(value=v12):
            return literal_type(v12)
        
        case hydra.core.TypeFunction(value=ft):
            return hydra.lib.strings.cat((hydra.lib.strings.cat((hydra.lib.strings.cat(("function from ", type(ft.domain))), " to ")), type(ft.codomain)))
        
        case hydra.core.TypeForall(value=fat):
            return hydra.lib.strings.cat2("polymorphic ", type(fat.body))
        
        case hydra.core.TypeList(value=t):
            return hydra.lib.strings.cat(("list of ", type(t)))
        
        case hydra.core.TypeMap(value=mt):
            return hydra.lib.strings.cat((hydra.lib.strings.cat((hydra.lib.strings.cat(("map from ", type(mt.keys))), " to ")), type(mt.values)))
        
        case hydra.core.TypeMaybe(value=ot):
            return hydra.lib.strings.cat(("maybe ", type(ot)))
        
        case hydra.core.TypeProduct():
            return "tuple"
        
        case hydra.core.TypeRecord():
            return "record"
        
        case hydra.core.TypeSet(value=st):
            return hydra.lib.strings.cat(("set of ", type(st)))
        
        case hydra.core.TypeSum():
            return "variant tuple"
        
        case hydra.core.TypeUnion():
            return "union"
        
        case hydra.core.TypeUnit():
            return "unit"
        
        case hydra.core.TypeVariable():
            return "instance of a named type"
        
        case hydra.core.TypeWrap(value=n):
            return hydra.lib.strings.cat(("wrapper for ", type(n.body)))
        
        case _:
            raise TypeError("Unsupported Type")
