# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.accessors."""

from __future__ import annotations
from typing import cast
import hydra.accessors
import hydra.core
import hydra.encode.core
import hydra.lib.lists

def accessor_node(x: hydra.accessors.AccessorNode) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.accessors.AccessorNode"), (hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name)), hydra.core.Field(hydra.core.Name("label"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.label))))), hydra.core.Field(hydra.core.Name("id"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.id)))))))))

def term_accessor(v1: hydra.accessors.TermAccessor) -> hydra.core.Term:
    match v1:
        case hydra.accessors.TermAccessorAnnotatedBody():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("annotatedBody"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.accessors.TermAccessorApplicationFunction():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("applicationFunction"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.accessors.TermAccessorApplicationArgument():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("applicationArgument"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.accessors.TermAccessorLambdaBody():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("lambdaBody"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.accessors.TermAccessorUnionCasesDefault():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("unionCasesDefault"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.accessors.TermAccessorUnionCasesBranch(value=y6):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("unionCasesBranch"), hydra.encode.core.name(y6)))))
        
        case hydra.accessors.TermAccessorLetBody():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("letBody"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.accessors.TermAccessorLetBinding(value=y8):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("letBinding"), hydra.encode.core.name(y8)))))
        
        case hydra.accessors.TermAccessorListElement(value=y9):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("listElement"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(y9))))))))))
        
        case hydra.accessors.TermAccessorMapKey(value=y10):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("mapKey"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(y10))))))))))
        
        case hydra.accessors.TermAccessorMapValue(value=y11):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("mapValue"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(y11))))))))))
        
        case hydra.accessors.TermAccessorMaybeTerm():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("maybeTerm"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.accessors.TermAccessorProductTerm(value=y13):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("productTerm"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(y13))))))))))
        
        case hydra.accessors.TermAccessorRecordField(value=y14):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("recordField"), hydra.encode.core.name(y14)))))
        
        case hydra.accessors.TermAccessorSetElement(value=y15):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("setElement"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(y15))))))))))
        
        case hydra.accessors.TermAccessorSumTerm():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("sumTerm"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.accessors.TermAccessorTypeLambdaBody():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("typeLambdaBody"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.accessors.TermAccessorTypeApplicationTerm():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("typeApplicationTerm"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.accessors.TermAccessorInjectionTerm():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("injectionTerm"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.accessors.TermAccessorWrappedTerm():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("wrappedTerm"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def accessor_path(x: hydra.accessors.AccessorPath) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.accessors.AccessorPath"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: term_accessor(x1)), x.value))))))

def accessor_edge(x: hydra.accessors.AccessorEdge) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.accessors.AccessorEdge"), (hydra.core.Field(hydra.core.Name("source"), accessor_node(x.source)), hydra.core.Field(hydra.core.Name("path"), accessor_path(x.path)), hydra.core.Field(hydra.core.Name("target"), accessor_node(x.target))))))

def accessor_graph(x: hydra.accessors.AccessorGraph) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.accessors.AccessorGraph"), (hydra.core.Field(hydra.core.Name("nodes"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: accessor_node(x1)), x.nodes)))), hydra.core.Field(hydra.core.Name("edges"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: accessor_edge(x1)), x.edges))))))))
