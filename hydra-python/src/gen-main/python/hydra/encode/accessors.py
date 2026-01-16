# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.accessors."""

from __future__ import annotations
from hydra.dsl.python import frozenlist
from typing import cast
import hydra.accessors
import hydra.core
import hydra.encode.core
import hydra.lib.lists

def accessor_node(x: hydra.accessors.AccessorNode) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.accessors.AccessorNode"), (hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name)), hydra.core.Field(hydra.core.Name("label"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.label)), hydra.core.Field(hydra.core.Name("id"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.id))))))

def term_accessor(v1: hydra.accessors.TermAccessor) -> hydra.core.Type:
    match v1:
        case hydra.accessors.TermAccessorAnnotatedBody():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("annotatedBody"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.accessors.TermAccessorApplicationFunction():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("applicationFunction"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.accessors.TermAccessorApplicationArgument():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("applicationArgument"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.accessors.TermAccessorLambdaBody():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("lambdaBody"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.accessors.TermAccessorUnionCasesDefault():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("unionCasesDefault"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.accessors.TermAccessorUnionCasesBranch(value=y6):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("unionCasesBranch"), hydra.encode.core.name(y6)))))
        
        case hydra.accessors.TermAccessorLetBody():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("letBody"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.accessors.TermAccessorLetBinding(value=y8):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("letBinding"), hydra.encode.core.name(y8)))))
        
        case hydra.accessors.TermAccessorListElement(value=y9):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("listElement"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x)))))))(y9)))))
        
        case hydra.accessors.TermAccessorMapKey(value=y10):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("mapKey"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x)))))))(y10)))))
        
        case hydra.accessors.TermAccessorMapValue(value=y11):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("mapValue"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x)))))))(y11)))))
        
        case hydra.accessors.TermAccessorMaybeTerm():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("maybeTerm"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.accessors.TermAccessorProductTerm(value=y13):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("productTerm"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x)))))))(y13)))))
        
        case hydra.accessors.TermAccessorRecordField(value=y14):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("recordField"), hydra.encode.core.name(y14)))))
        
        case hydra.accessors.TermAccessorSetElement(value=y15):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("setElement"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x)))))))(y15)))))
        
        case hydra.accessors.TermAccessorSumTerm():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("sumTerm"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.accessors.TermAccessorTypeLambdaBody():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("typeLambdaBody"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.accessors.TermAccessorTypeApplicationTerm():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("typeApplicationTerm"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.accessors.TermAccessorInjectionTerm():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("injectionTerm"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.accessors.TermAccessorWrappedTerm():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("wrappedTerm"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def accessor_path(x: hydra.accessors.AccessorPath) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.accessors.AccessorPath"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(term_accessor, xs))))(x.value))))

def accessor_edge(x: hydra.accessors.AccessorEdge) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.accessors.AccessorEdge"), (hydra.core.Field(hydra.core.Name("source"), accessor_node(x.source)), hydra.core.Field(hydra.core.Name("path"), accessor_path(x.path)), hydra.core.Field(hydra.core.Name("target"), accessor_node(x.target))))))

def accessor_graph(x: hydra.accessors.AccessorGraph) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.accessors.AccessorGraph"), (hydra.core.Field(hydra.core.Name("nodes"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(accessor_node, xs))))(x.nodes)), hydra.core.Field(hydra.core.Name("edges"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(accessor_edge, xs))))(x.edges))))))
