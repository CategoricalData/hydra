# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.variants."""

from __future__ import annotations
from typing import cast
import hydra.core
import hydra.variants

def elimination_variant(v1: hydra.variants.EliminationVariant) -> hydra.core.Type:
    match v1:
        case hydra.variants.EliminationVariant.RECORD:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.EliminationVariant"), hydra.core.Field(hydra.core.Name("record"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v)))))
        
        case hydra.variants.EliminationVariant.UNION:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.EliminationVariant"), hydra.core.Field(hydra.core.Name("union"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v2)))))
        
        case hydra.variants.EliminationVariant.WRAP:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.EliminationVariant"), hydra.core.Field(hydra.core.Name("wrap"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v3)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def function_variant(v1: hydra.variants.FunctionVariant) -> hydra.core.Type:
    match v1:
        case hydra.variants.FunctionVariant.ELIMINATION:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.FunctionVariant"), hydra.core.Field(hydra.core.Name("elimination"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v)))))
        
        case hydra.variants.FunctionVariant.LAMBDA:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.FunctionVariant"), hydra.core.Field(hydra.core.Name("lambda"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v2)))))
        
        case hydra.variants.FunctionVariant.PRIMITIVE:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.FunctionVariant"), hydra.core.Field(hydra.core.Name("primitive"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v3)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def literal_variant(v1: hydra.variants.LiteralVariant) -> hydra.core.Type:
    match v1:
        case hydra.variants.LiteralVariant.BINARY:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.LiteralVariant"), hydra.core.Field(hydra.core.Name("binary"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v)))))
        
        case hydra.variants.LiteralVariant.BOOLEAN:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.LiteralVariant"), hydra.core.Field(hydra.core.Name("boolean"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v2)))))
        
        case hydra.variants.LiteralVariant.FLOAT:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.LiteralVariant"), hydra.core.Field(hydra.core.Name("float"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v3)))))
        
        case hydra.variants.LiteralVariant.INTEGER:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.LiteralVariant"), hydra.core.Field(hydra.core.Name("integer"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v4)))))
        
        case hydra.variants.LiteralVariant.STRING:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.LiteralVariant"), hydra.core.Field(hydra.core.Name("string"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v5)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def term_variant(v1: hydra.variants.TermVariant) -> hydra.core.Type:
    match v1:
        case hydra.variants.TermVariant.ANNOTATED:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("annotated"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v)))))
        
        case hydra.variants.TermVariant.APPLICATION:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("application"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v2)))))
        
        case hydra.variants.TermVariant.EITHER:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("either"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v3)))))
        
        case hydra.variants.TermVariant.FUNCTION:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("function"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v4)))))
        
        case hydra.variants.TermVariant.LET:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("let"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v5)))))
        
        case hydra.variants.TermVariant.LIST:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("list"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v6)))))
        
        case hydra.variants.TermVariant.LITERAL:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("literal"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v7)))))
        
        case hydra.variants.TermVariant.MAP:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("map"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v8)))))
        
        case hydra.variants.TermVariant.MAYBE:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("maybe"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v9)))))
        
        case hydra.variants.TermVariant.PAIR:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("pair"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v10)))))
        
        case hydra.variants.TermVariant.RECORD:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("record"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v11)))))
        
        case hydra.variants.TermVariant.SET:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("set"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v12)))))
        
        case hydra.variants.TermVariant.TYPE_APPLICATION:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("typeApplication"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v13)))))
        
        case hydra.variants.TermVariant.TYPE_LAMBDA:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("typeLambda"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v14)))))
        
        case hydra.variants.TermVariant.UNION:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("union"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v15)))))
        
        case hydra.variants.TermVariant.UNIT:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("unit"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v16)))))
        
        case hydra.variants.TermVariant.VARIABLE:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("variable"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v17)))))
        
        case hydra.variants.TermVariant.WRAP:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("wrap"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v18)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def type_variant(v1: hydra.variants.TypeVariant) -> hydra.core.Type:
    match v1:
        case hydra.variants.TypeVariant.ANNOTATED:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("annotated"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v)))))
        
        case hydra.variants.TypeVariant.APPLICATION:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("application"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v2)))))
        
        case hydra.variants.TypeVariant.EITHER:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("either"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v3)))))
        
        case hydra.variants.TypeVariant.FORALL:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("forall"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v4)))))
        
        case hydra.variants.TypeVariant.FUNCTION:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("function"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v5)))))
        
        case hydra.variants.TypeVariant.LIST:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("list"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v6)))))
        
        case hydra.variants.TypeVariant.LITERAL:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("literal"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v7)))))
        
        case hydra.variants.TypeVariant.MAP:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("map"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v8)))))
        
        case hydra.variants.TypeVariant.MAYBE:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("maybe"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v9)))))
        
        case hydra.variants.TypeVariant.PAIR:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("pair"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v10)))))
        
        case hydra.variants.TypeVariant.RECORD:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("record"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v11)))))
        
        case hydra.variants.TypeVariant.SET:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("set"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v12)))))
        
        case hydra.variants.TypeVariant.UNION:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("union"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v13)))))
        
        case hydra.variants.TypeVariant.UNIT:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("unit"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v14)))))
        
        case hydra.variants.TypeVariant.VARIABLE:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("variable"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v15)))))
        
        case hydra.variants.TypeVariant.WRAP:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("wrap"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v16)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")
