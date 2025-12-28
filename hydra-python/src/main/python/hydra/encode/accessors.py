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
        case hydra.accessors.TermAccessorAnnotatedBody(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("annotatedBody"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v)))))
        
        case hydra.accessors.TermAccessorApplicationFunction(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("applicationFunction"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v2)))))
        
        case hydra.accessors.TermAccessorApplicationArgument(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("applicationArgument"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v3)))))
        
        case hydra.accessors.TermAccessorLambdaBody(value=v4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("lambdaBody"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v4)))))
        
        case hydra.accessors.TermAccessorUnionCasesDefault(value=v5):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("unionCasesDefault"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v5)))))
        
        case hydra.accessors.TermAccessorUnionCasesBranch(value=v6):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("unionCasesBranch"), hydra.encode.core.name(v6)))))
        
        case hydra.accessors.TermAccessorLetBody(value=v7):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("letBody"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v7)))))
        
        case hydra.accessors.TermAccessorLetBinding(value=v8):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("letBinding"), hydra.encode.core.name(v8)))))
        
        case hydra.accessors.TermAccessorListElement(value=v9):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("listElement"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x)))))))(v9)))))
        
        case hydra.accessors.TermAccessorMapKey(value=v10):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("mapKey"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x)))))))(v10)))))
        
        case hydra.accessors.TermAccessorMapValue(value=v11):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("mapValue"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x)))))))(v11)))))
        
        case hydra.accessors.TermAccessorMaybeTerm(value=v12):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("maybeTerm"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v12)))))
        
        case hydra.accessors.TermAccessorProductTerm(value=v13):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("productTerm"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x)))))))(v13)))))
        
        case hydra.accessors.TermAccessorRecordField(value=v14):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("recordField"), hydra.encode.core.name(v14)))))
        
        case hydra.accessors.TermAccessorSetElement(value=v15):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("setElement"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x)))))))(v15)))))
        
        case hydra.accessors.TermAccessorSumTerm(value=v16):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("sumTerm"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v16)))))
        
        case hydra.accessors.TermAccessorTypeLambdaBody(value=v17):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("typeLambdaBody"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v17)))))
        
        case hydra.accessors.TermAccessorTypeApplicationTerm(value=v18):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("typeApplicationTerm"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v18)))))
        
        case hydra.accessors.TermAccessorInjectionTerm(value=v19):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("injectionTerm"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v19)))))
        
        case hydra.accessors.TermAccessorWrappedTerm(value=v20):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("wrappedTerm"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v20)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def accessor_path(x: hydra.accessors.AccessorPath) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.accessors.AccessorPath"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(term_accessor, xs))))(x.value))))

def accessor_edge(x: hydra.accessors.AccessorEdge) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.accessors.AccessorEdge"), (hydra.core.Field(hydra.core.Name("source"), accessor_node(x.source)), hydra.core.Field(hydra.core.Name("path"), accessor_path(x.path)), hydra.core.Field(hydra.core.Name("target"), accessor_node(x.target))))))

def accessor_graph(x: hydra.accessors.AccessorGraph) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.accessors.AccessorGraph"), (hydra.core.Field(hydra.core.Name("nodes"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(accessor_node, xs))))(x.nodes)), hydra.core.Field(hydra.core.Name("edges"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(accessor_edge, xs))))(x.edges))))))
