# Note: this is an automatically generated file. Do not edit.

"""Functions dealing with arguments and arity."""

from __future__ import annotations
from hydra.dsl.python import frozenlist
import hydra.core
import hydra.graph
import hydra.lib.lists
import hydra.lib.math

def function_arity(v1: hydra.core.Function) -> int:
    match v1:
        case hydra.core.FunctionElimination():
            return 1
        
        case hydra.core.FunctionLambda(value=arg_):
            return hydra.lib.math.add(1, term_arity(arg_.body))
        
        case hydra.core.FunctionPrimitive():
            return 42

def term_arity(v1: hydra.core.Term) -> int:
    match v1:
        case hydra.core.TermApplication(value=arg_):
            return hydra.lib.math.sub(term_arity(arg_.function), 1)
        
        case hydra.core.TermFunction(value=v12):
            return function_arity(v12)
        
        case _:
            return 0

def type_arity(v1: hydra.core.Type) -> int:
    match v1:
        case hydra.core.TypeAnnotated(value=arg_):
            return type_arity(arg_.subject)
        
        case hydra.core.TypeApplication(value=arg_2):
            return type_arity(arg_2.function)
        
        case hydra.core.TypeForall(value=arg_3):
            return type_arity(arg_3.body)
        
        case hydra.core.TypeFunction(value=f):
            return hydra.lib.math.add(1, type_arity(f.codomain))
        
        case _:
            return 0

def primitive_arity(arg_: hydra.graph.Primitive) -> int:
    """Find the arity (expected number of arguments) of a primitive constant or function."""
    
    return type_arity(arg_.type.type)

def uncurry_type(t: hydra.core.Type) -> frozenlist[hydra.core.Type]:
    """Uncurry a type expression into a list of types, turning a function type a -> b into cons a (uncurryType b)."""
    
    match t:
        case hydra.core.TypeAnnotated(value=arg_):
            return uncurry_type(arg_.subject)
        
        case hydra.core.TypeApplication(value=arg_2):
            return uncurry_type(arg_2.function)
        
        case hydra.core.TypeForall(value=arg_3):
            return uncurry_type(arg_3.body)
        
        case hydra.core.TypeFunction(value=ft):
            return hydra.lib.lists.cons(ft.domain, uncurry_type(ft.codomain))
        
        case _:
            return (t,)
