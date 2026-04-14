# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.paths."""

from __future__ import annotations
from functools import lru_cache
from typing import cast
import hydra.core
import hydra.encode.core
import hydra.lib.lists
import hydra.paths

def subterm_node(x: hydra.paths.SubtermNode) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtermNode"), (hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name)), hydra.core.Field(hydra.core.Name("label"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.label))))), hydra.core.Field(hydra.core.Name("id"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.id)))))))))

def subterm_step(v1: hydra.paths.SubtermStep) -> hydra.core.Term:
    match v1:
        case hydra.paths.SubtermStepAnnotatedBody():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("annotatedBody"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtermStepApplicationFunction():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("applicationFunction"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtermStepApplicationArgument():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("applicationArgument"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtermStepLambdaBody():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("lambdaBody"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtermStepUnionCasesDefault():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("unionCasesDefault"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtermStepUnionCasesBranch(value=y6):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("unionCasesBranch"), hydra.encode.core.name(y6)))))

        case hydra.paths.SubtermStepLetBody():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("letBody"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtermStepLetBinding(value=y8):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("letBinding"), hydra.encode.core.name(y8)))))

        case hydra.paths.SubtermStepListElement(value=y9):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("listElement"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(y9))))))))))

        case hydra.paths.SubtermStepMapKey(value=y10):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("mapKey"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(y10))))))))))

        case hydra.paths.SubtermStepMapValue(value=y11):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("mapValue"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(y11))))))))))

        case hydra.paths.SubtermStepMaybeTerm():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("maybeTerm"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtermStepProductTerm(value=y13):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("productTerm"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(y13))))))))))

        case hydra.paths.SubtermStepRecordField(value=y14):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("recordField"), hydra.encode.core.name(y14)))))

        case hydra.paths.SubtermStepSetElement(value=y15):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("setElement"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(y15))))))))))

        case hydra.paths.SubtermStepSumTerm():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("sumTerm"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtermStepTypeLambdaBody():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("typeLambdaBody"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtermStepTypeApplicationTerm():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("typeApplicationTerm"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtermStepInjectionTerm():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("injectionTerm"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtermStepWrappedTerm():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("wrappedTerm"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def subterm_path(x: hydra.paths.SubtermPath) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.paths.SubtermPath"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: subterm_step(x1)), x.value))))))

def subterm_edge(x: hydra.paths.SubtermEdge) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtermEdge"), (hydra.core.Field(hydra.core.Name("source"), subterm_node(x.source)), hydra.core.Field(hydra.core.Name("path"), subterm_path(x.path)), hydra.core.Field(hydra.core.Name("target"), subterm_node(x.target))))))

def subterm_graph(x: hydra.paths.SubtermGraph) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtermGraph"), (hydra.core.Field(hydra.core.Name("nodes"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: subterm_node(x1)), x.nodes)))), hydra.core.Field(hydra.core.Name("edges"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: subterm_edge(x1)), x.edges))))))))

def subtype_node(x: hydra.paths.SubtypeNode) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtypeNode"), (hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name)), hydra.core.Field(hydra.core.Name("label"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.label))))), hydra.core.Field(hydra.core.Name("id"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.id)))))))))

def subtype_step(v1: hydra.paths.SubtypeStep) -> hydra.core.Term:
    match v1:
        case hydra.paths.SubtypeStepAnnotatedBody():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("annotatedBody"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtypeStepApplicationFunction():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("applicationFunction"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtypeStepApplicationArgument():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("applicationArgument"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtypeStepEitherLeft():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("eitherLeft"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtypeStepEitherRight():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("eitherRight"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtypeStepForallBody():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("forallBody"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtypeStepFunctionDomain():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("functionDomain"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtypeStepFunctionCodomain():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("functionCodomain"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtypeStepListElement():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("listElement"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtypeStepMapKeys():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("mapKeys"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtypeStepMapValues():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("mapValues"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtypeStepMaybeElement():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("maybeElement"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtypeStepPairFirst():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("pairFirst"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtypeStepPairSecond():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("pairSecond"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtypeStepRecordField(value=y15):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("recordField"), hydra.encode.core.name(y15)))))

        case hydra.paths.SubtypeStepSetElement():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("setElement"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.paths.SubtypeStepUnionField(value=y17):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("unionField"), hydra.encode.core.name(y17)))))

        case hydra.paths.SubtypeStepWrappedType():
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("wrappedType"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def subtype_path(x: hydra.paths.SubtypePath) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.paths.SubtypePath"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: subtype_step(x1)), x.value))))))

def subtype_edge(x: hydra.paths.SubtypeEdge) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtypeEdge"), (hydra.core.Field(hydra.core.Name("source"), subtype_node(x.source)), hydra.core.Field(hydra.core.Name("path"), subtype_path(x.path)), hydra.core.Field(hydra.core.Name("target"), subtype_node(x.target))))))

def subtype_graph(x: hydra.paths.SubtypeGraph) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtypeGraph"), (hydra.core.Field(hydra.core.Name("nodes"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: subtype_node(x1)), x.nodes)))), hydra.core.Field(hydra.core.Name("edges"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: subtype_edge(x1)), x.edges))))))))
