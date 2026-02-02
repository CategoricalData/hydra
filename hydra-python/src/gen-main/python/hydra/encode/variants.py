# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.variants."""

from __future__ import annotations
from typing import cast
import hydra.core
import hydra.variants

def elimination_variant(v1: hydra.variants.EliminationVariant) -> hydra.core.Type:
    match v1:
        case hydra.variants.EliminationVariant.RECORD:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.EliminationVariant"), hydra.core.Field(hydra.core.Name("record"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.EliminationVariant.UNION:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.EliminationVariant"), hydra.core.Field(hydra.core.Name("union"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.EliminationVariant.WRAP:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.EliminationVariant"), hydra.core.Field(hydra.core.Name("wrap"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def function_variant(v1: hydra.variants.FunctionVariant) -> hydra.core.Type:
    match v1:
        case hydra.variants.FunctionVariant.ELIMINATION:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.FunctionVariant"), hydra.core.Field(hydra.core.Name("elimination"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.FunctionVariant.LAMBDA:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.FunctionVariant"), hydra.core.Field(hydra.core.Name("lambda"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.FunctionVariant.PRIMITIVE:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.FunctionVariant"), hydra.core.Field(hydra.core.Name("primitive"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def literal_variant(v1: hydra.variants.LiteralVariant) -> hydra.core.Type:
    match v1:
        case hydra.variants.LiteralVariant.BINARY:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.LiteralVariant"), hydra.core.Field(hydra.core.Name("binary"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.LiteralVariant.BOOLEAN:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.LiteralVariant"), hydra.core.Field(hydra.core.Name("boolean"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.LiteralVariant.FLOAT:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.LiteralVariant"), hydra.core.Field(hydra.core.Name("float"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.LiteralVariant.INTEGER:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.LiteralVariant"), hydra.core.Field(hydra.core.Name("integer"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.LiteralVariant.STRING:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.LiteralVariant"), hydra.core.Field(hydra.core.Name("string"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def term_variant(v1: hydra.variants.TermVariant) -> hydra.core.Type:
    match v1:
        case hydra.variants.TermVariant.ANNOTATED:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("annotated"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TermVariant.APPLICATION:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("application"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TermVariant.EITHER:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("either"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TermVariant.FUNCTION:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("function"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TermVariant.LET:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("let"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TermVariant.LIST:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("list"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TermVariant.LITERAL:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("literal"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TermVariant.MAP:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("map"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TermVariant.MAYBE:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("maybe"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TermVariant.PAIR:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("pair"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TermVariant.RECORD:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("record"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TermVariant.SET:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("set"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TermVariant.TYPE_APPLICATION:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("typeApplication"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TermVariant.TYPE_LAMBDA:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("typeLambda"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TermVariant.UNION:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("union"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TermVariant.UNIT:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("unit"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TermVariant.VARIABLE:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("variable"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TermVariant.WRAP:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("wrap"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def type_variant(v1: hydra.variants.TypeVariant) -> hydra.core.Type:
    match v1:
        case hydra.variants.TypeVariant.ANNOTATED:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("annotated"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TypeVariant.APPLICATION:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("application"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TypeVariant.EITHER:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("either"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TypeVariant.FORALL:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("forall"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TypeVariant.FUNCTION:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("function"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TypeVariant.LIST:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("list"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TypeVariant.LITERAL:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("literal"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TypeVariant.MAP:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("map"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TypeVariant.MAYBE:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("maybe"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TypeVariant.PAIR:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("pair"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TypeVariant.RECORD:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("record"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TypeVariant.SET:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("set"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TypeVariant.UNION:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("union"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TypeVariant.UNIT:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("unit"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TypeVariant.VARIABLE:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("variable"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.variants.TypeVariant.WRAP:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("wrap"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")
