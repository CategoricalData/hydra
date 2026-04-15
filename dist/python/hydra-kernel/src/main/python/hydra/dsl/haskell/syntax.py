# Note: this is an automatically generated file. Do not edit.

r"""DSL functions for hydra.haskell.syntax."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Maybe, frozenlist
from typing import cast
import hydra.core
import hydra.phantoms

def alternative(pattern: hydra.phantoms.TTerm[hydra.haskell.syntax.Pattern], rhs: hydra.phantoms.TTerm[hydra.haskell.syntax.CaseRhs], binds: hydra.phantoms.TTerm[Maybe[hydra.haskell.syntax.LocalBindings]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.Alternative"), (hydra.core.Field(hydra.core.Name("pattern"), pattern.value), hydra.core.Field(hydra.core.Name("rhs"), rhs.value), hydra.core.Field(hydra.core.Name("binds"), binds.value))))))

def alternative_binds(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Alternative]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Alternative"), hydra.core.Name("binds")))), x.value))))

def alternative_pattern(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Alternative]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Alternative"), hydra.core.Name("pattern")))), x.value))))

def alternative_rhs(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Alternative]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Alternative"), hydra.core.Name("rhs")))), x.value))))

def alternative_with_binds(original: hydra.phantoms.TTerm[hydra.haskell.syntax.Alternative], new_val: hydra.phantoms.TTerm[Maybe[hydra.haskell.syntax.LocalBindings]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.Alternative"), (hydra.core.Field(hydra.core.Name("pattern"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Alternative"), hydra.core.Name("pattern")))), original.value)))), hydra.core.Field(hydra.core.Name("rhs"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Alternative"), hydra.core.Name("rhs")))), original.value)))), hydra.core.Field(hydra.core.Name("binds"), new_val.value))))))

def alternative_with_pattern(original: hydra.phantoms.TTerm[hydra.haskell.syntax.Alternative], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Pattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.Alternative"), (hydra.core.Field(hydra.core.Name("pattern"), new_val.value), hydra.core.Field(hydra.core.Name("rhs"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Alternative"), hydra.core.Name("rhs")))), original.value)))), hydra.core.Field(hydra.core.Name("binds"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Alternative"), hydra.core.Name("binds")))), original.value)))))))))

def alternative_with_rhs(original: hydra.phantoms.TTerm[hydra.haskell.syntax.Alternative], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.CaseRhs]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.Alternative"), (hydra.core.Field(hydra.core.Name("pattern"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Alternative"), hydra.core.Name("pattern")))), original.value)))), hydra.core.Field(hydra.core.Name("rhs"), new_val.value), hydra.core.Field(hydra.core.Name("binds"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Alternative"), hydra.core.Name("binds")))), original.value)))))))))

def application_declaration_head(function: hydra.phantoms.TTerm[hydra.haskell.syntax.DeclarationHead], operand: hydra.phantoms.TTerm[hydra.haskell.syntax.Variable]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ApplicationDeclarationHead"), (hydra.core.Field(hydra.core.Name("function"), function.value), hydra.core.Field(hydra.core.Name("operand"), operand.value))))))

def application_declaration_head_function(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ApplicationDeclarationHead]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ApplicationDeclarationHead"), hydra.core.Name("function")))), x.value))))

def application_declaration_head_operand(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ApplicationDeclarationHead]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ApplicationDeclarationHead"), hydra.core.Name("operand")))), x.value))))

def application_declaration_head_with_function(original: hydra.phantoms.TTerm[hydra.haskell.syntax.ApplicationDeclarationHead], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.DeclarationHead]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ApplicationDeclarationHead"), (hydra.core.Field(hydra.core.Name("function"), new_val.value), hydra.core.Field(hydra.core.Name("operand"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ApplicationDeclarationHead"), hydra.core.Name("operand")))), original.value)))))))))

def application_declaration_head_with_operand(original: hydra.phantoms.TTerm[hydra.haskell.syntax.ApplicationDeclarationHead], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Variable]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ApplicationDeclarationHead"), (hydra.core.Field(hydra.core.Name("function"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ApplicationDeclarationHead"), hydra.core.Name("function")))), original.value)))), hydra.core.Field(hydra.core.Name("operand"), new_val.value))))))

def application_expression(function: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression], argument: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ApplicationExpression"), (hydra.core.Field(hydra.core.Name("function"), function.value), hydra.core.Field(hydra.core.Name("argument"), argument.value))))))

def application_expression_argument(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ApplicationExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ApplicationExpression"), hydra.core.Name("argument")))), x.value))))

def application_expression_function(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ApplicationExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ApplicationExpression"), hydra.core.Name("function")))), x.value))))

def application_expression_with_argument(original: hydra.phantoms.TTerm[hydra.haskell.syntax.ApplicationExpression], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ApplicationExpression"), (hydra.core.Field(hydra.core.Name("function"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ApplicationExpression"), hydra.core.Name("function")))), original.value)))), hydra.core.Field(hydra.core.Name("argument"), new_val.value))))))

def application_expression_with_function(original: hydra.phantoms.TTerm[hydra.haskell.syntax.ApplicationExpression], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ApplicationExpression"), (hydra.core.Field(hydra.core.Name("function"), new_val.value), hydra.core.Field(hydra.core.Name("argument"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ApplicationExpression"), hydra.core.Name("argument")))), original.value)))))))))

def application_pattern(name: hydra.phantoms.TTerm[hydra.haskell.syntax.Name], args: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Pattern]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ApplicationPattern"), (hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("args"), args.value))))))

def application_pattern_args(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ApplicationPattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ApplicationPattern"), hydra.core.Name("args")))), x.value))))

def application_pattern_name(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ApplicationPattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ApplicationPattern"), hydra.core.Name("name")))), x.value))))

def application_pattern_with_args(original: hydra.phantoms.TTerm[hydra.haskell.syntax.ApplicationPattern], new_val: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Pattern]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ApplicationPattern"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ApplicationPattern"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("args"), new_val.value))))))

def application_pattern_with_name(original: hydra.phantoms.TTerm[hydra.haskell.syntax.ApplicationPattern], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ApplicationPattern"), (hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("args"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ApplicationPattern"), hydra.core.Name("args")))), original.value)))))))))

def application_type(context: hydra.phantoms.TTerm[hydra.haskell.syntax.Type], argument: hydra.phantoms.TTerm[hydra.haskell.syntax.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ApplicationType"), (hydra.core.Field(hydra.core.Name("context"), context.value), hydra.core.Field(hydra.core.Name("argument"), argument.value))))))

def application_type_argument(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ApplicationType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ApplicationType"), hydra.core.Name("argument")))), x.value))))

def application_type_context(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ApplicationType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ApplicationType"), hydra.core.Name("context")))), x.value))))

def application_type_with_argument(original: hydra.phantoms.TTerm[hydra.haskell.syntax.ApplicationType], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ApplicationType"), (hydra.core.Field(hydra.core.Name("context"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ApplicationType"), hydra.core.Name("context")))), original.value)))), hydra.core.Field(hydra.core.Name("argument"), new_val.value))))))

def application_type_with_context(original: hydra.phantoms.TTerm[hydra.haskell.syntax.ApplicationType], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ApplicationType"), (hydra.core.Field(hydra.core.Name("context"), new_val.value), hydra.core.Field(hydra.core.Name("argument"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ApplicationType"), hydra.core.Name("argument")))), original.value)))))))))

def as_pattern(name: hydra.phantoms.TTerm[hydra.haskell.syntax.Name], inner: hydra.phantoms.TTerm[hydra.haskell.syntax.Pattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.AsPattern"), (hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("inner"), inner.value))))))

def as_pattern_inner(x: hydra.phantoms.TTerm[hydra.haskell.syntax.AsPattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.AsPattern"), hydra.core.Name("inner")))), x.value))))

def as_pattern_name(x: hydra.phantoms.TTerm[hydra.haskell.syntax.AsPattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.AsPattern"), hydra.core.Name("name")))), x.value))))

def as_pattern_with_inner(original: hydra.phantoms.TTerm[hydra.haskell.syntax.AsPattern], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Pattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.AsPattern"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.AsPattern"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("inner"), new_val.value))))))

def as_pattern_with_name(original: hydra.phantoms.TTerm[hydra.haskell.syntax.AsPattern], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.AsPattern"), (hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("inner"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.AsPattern"), hydra.core.Name("inner")))), original.value)))))))))

def assertion_class(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ClassAssertion]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Assertion"), hydra.core.Field(hydra.core.Name("class"), x.value)))))

def assertion_tuple(x: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Assertion]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Assertion"), hydra.core.Field(hydra.core.Name("tuple"), x.value)))))

def case_expression(case: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression], alternatives: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Alternative]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.CaseExpression"), (hydra.core.Field(hydra.core.Name("case"), case.value), hydra.core.Field(hydra.core.Name("alternatives"), alternatives.value))))))

def case_expression_alternatives(x: hydra.phantoms.TTerm[hydra.haskell.syntax.CaseExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.CaseExpression"), hydra.core.Name("alternatives")))), x.value))))

def case_expression_case(x: hydra.phantoms.TTerm[hydra.haskell.syntax.CaseExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.CaseExpression"), hydra.core.Name("case")))), x.value))))

def case_expression_with_alternatives(original: hydra.phantoms.TTerm[hydra.haskell.syntax.CaseExpression], new_val: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Alternative]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.CaseExpression"), (hydra.core.Field(hydra.core.Name("case"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.CaseExpression"), hydra.core.Name("case")))), original.value)))), hydra.core.Field(hydra.core.Name("alternatives"), new_val.value))))))

def case_expression_with_case(original: hydra.phantoms.TTerm[hydra.haskell.syntax.CaseExpression], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.CaseExpression"), (hydra.core.Field(hydra.core.Name("case"), new_val.value), hydra.core.Field(hydra.core.Name("alternatives"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.CaseExpression"), hydra.core.Name("alternatives")))), original.value)))))))))

def case_rhs(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.haskell.syntax.CaseRhs"), x.value))))

def class_assertion(name: hydra.phantoms.TTerm[hydra.haskell.syntax.Name], types: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Type]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ClassAssertion"), (hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("types"), types.value))))))

def class_assertion_name(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ClassAssertion]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ClassAssertion"), hydra.core.Name("name")))), x.value))))

def class_assertion_types(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ClassAssertion]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ClassAssertion"), hydra.core.Name("types")))), x.value))))

def class_assertion_with_name(original: hydra.phantoms.TTerm[hydra.haskell.syntax.ClassAssertion], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ClassAssertion"), (hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("types"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ClassAssertion"), hydra.core.Name("types")))), original.value)))))))))

def class_assertion_with_types(original: hydra.phantoms.TTerm[hydra.haskell.syntax.ClassAssertion], new_val: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Type]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ClassAssertion"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ClassAssertion"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("types"), new_val.value))))))

def construct_record_expression(name: hydra.phantoms.TTerm[hydra.haskell.syntax.Name], fields: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.FieldUpdate]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ConstructRecordExpression"), (hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("fields"), fields.value))))))

def construct_record_expression_fields(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ConstructRecordExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ConstructRecordExpression"), hydra.core.Name("fields")))), x.value))))

def construct_record_expression_name(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ConstructRecordExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ConstructRecordExpression"), hydra.core.Name("name")))), x.value))))

def construct_record_expression_with_fields(original: hydra.phantoms.TTerm[hydra.haskell.syntax.ConstructRecordExpression], new_val: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.FieldUpdate]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ConstructRecordExpression"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ConstructRecordExpression"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("fields"), new_val.value))))))

def construct_record_expression_with_name(original: hydra.phantoms.TTerm[hydra.haskell.syntax.ConstructRecordExpression], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ConstructRecordExpression"), (hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("fields"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ConstructRecordExpression"), hydra.core.Name("fields")))), original.value)))))))))

def constructor_ordinary(x: hydra.phantoms.TTerm[hydra.haskell.syntax.OrdinaryConstructor]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Constructor"), hydra.core.Field(hydra.core.Name("ordinary"), x.value)))))

def constructor_record(x: hydra.phantoms.TTerm[hydra.haskell.syntax.RecordConstructor]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Constructor"), hydra.core.Field(hydra.core.Name("record"), x.value)))))

def constructor_with_comments(body: hydra.phantoms.TTerm[hydra.haskell.syntax.Constructor], comments: hydra.phantoms.TTerm[Maybe[str]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ConstructorWithComments"), (hydra.core.Field(hydra.core.Name("body"), body.value), hydra.core.Field(hydra.core.Name("comments"), comments.value))))))

def constructor_with_comments_body(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ConstructorWithComments]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ConstructorWithComments"), hydra.core.Name("body")))), x.value))))

def constructor_with_comments_comments(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ConstructorWithComments]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ConstructorWithComments"), hydra.core.Name("comments")))), x.value))))

def constructor_with_comments_with_body(original: hydra.phantoms.TTerm[hydra.haskell.syntax.ConstructorWithComments], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Constructor]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ConstructorWithComments"), (hydra.core.Field(hydra.core.Name("body"), new_val.value), hydra.core.Field(hydra.core.Name("comments"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ConstructorWithComments"), hydra.core.Name("comments")))), original.value)))))))))

def constructor_with_comments_with_comments(original: hydra.phantoms.TTerm[hydra.haskell.syntax.ConstructorWithComments], new_val: hydra.phantoms.TTerm[Maybe[str]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ConstructorWithComments"), (hydra.core.Field(hydra.core.Name("body"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ConstructorWithComments"), hydra.core.Name("body")))), original.value)))), hydra.core.Field(hydra.core.Name("comments"), new_val.value))))))

def context_type(ctx: hydra.phantoms.TTerm[hydra.haskell.syntax.Assertion], type: hydra.phantoms.TTerm[hydra.haskell.syntax.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ContextType"), (hydra.core.Field(hydra.core.Name("ctx"), ctx.value), hydra.core.Field(hydra.core.Name("type"), type.value))))))

def context_type_ctx(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ContextType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ContextType"), hydra.core.Name("ctx")))), x.value))))

def context_type_type(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ContextType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ContextType"), hydra.core.Name("type")))), x.value))))

def context_type_with_ctx(original: hydra.phantoms.TTerm[hydra.haskell.syntax.ContextType], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Assertion]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ContextType"), (hydra.core.Field(hydra.core.Name("ctx"), new_val.value), hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ContextType"), hydra.core.Name("type")))), original.value)))))))))

def context_type_with_type(original: hydra.phantoms.TTerm[hydra.haskell.syntax.ContextType], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ContextType"), (hydra.core.Field(hydra.core.Name("ctx"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ContextType"), hydra.core.Name("ctx")))), original.value)))), hydra.core.Field(hydra.core.Name("type"), new_val.value))))))

def data_declaration(keyword: hydra.phantoms.TTerm[hydra.haskell.syntax.DataOrNewtype], context: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Assertion]], head: hydra.phantoms.TTerm[hydra.haskell.syntax.DeclarationHead], constructors: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.ConstructorWithComments]], deriving: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Deriving]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), (hydra.core.Field(hydra.core.Name("keyword"), keyword.value), hydra.core.Field(hydra.core.Name("context"), context.value), hydra.core.Field(hydra.core.Name("head"), head.value), hydra.core.Field(hydra.core.Name("constructors"), constructors.value), hydra.core.Field(hydra.core.Name("deriving"), deriving.value))))))

def data_declaration_constructors(x: hydra.phantoms.TTerm[hydra.haskell.syntax.DataDeclaration]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), hydra.core.Name("constructors")))), x.value))))

def data_declaration_context(x: hydra.phantoms.TTerm[hydra.haskell.syntax.DataDeclaration]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), hydra.core.Name("context")))), x.value))))

def data_declaration_deriving(x: hydra.phantoms.TTerm[hydra.haskell.syntax.DataDeclaration]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), hydra.core.Name("deriving")))), x.value))))

def data_declaration_head(x: hydra.phantoms.TTerm[hydra.haskell.syntax.DataDeclaration]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), hydra.core.Name("head")))), x.value))))

def data_declaration_keyword(x: hydra.phantoms.TTerm[hydra.haskell.syntax.DataDeclaration]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), hydra.core.Name("keyword")))), x.value))))

def data_declaration_with_constructors(original: hydra.phantoms.TTerm[hydra.haskell.syntax.DataDeclaration], new_val: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.ConstructorWithComments]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), (hydra.core.Field(hydra.core.Name("keyword"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), hydra.core.Name("keyword")))), original.value)))), hydra.core.Field(hydra.core.Name("context"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), hydra.core.Name("context")))), original.value)))), hydra.core.Field(hydra.core.Name("head"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), hydra.core.Name("head")))), original.value)))), hydra.core.Field(hydra.core.Name("constructors"), new_val.value), hydra.core.Field(hydra.core.Name("deriving"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), hydra.core.Name("deriving")))), original.value)))))))))

def data_declaration_with_context(original: hydra.phantoms.TTerm[hydra.haskell.syntax.DataDeclaration], new_val: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Assertion]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), (hydra.core.Field(hydra.core.Name("keyword"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), hydra.core.Name("keyword")))), original.value)))), hydra.core.Field(hydra.core.Name("context"), new_val.value), hydra.core.Field(hydra.core.Name("head"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), hydra.core.Name("head")))), original.value)))), hydra.core.Field(hydra.core.Name("constructors"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), hydra.core.Name("constructors")))), original.value)))), hydra.core.Field(hydra.core.Name("deriving"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), hydra.core.Name("deriving")))), original.value)))))))))

def data_declaration_with_deriving(original: hydra.phantoms.TTerm[hydra.haskell.syntax.DataDeclaration], new_val: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Deriving]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), (hydra.core.Field(hydra.core.Name("keyword"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), hydra.core.Name("keyword")))), original.value)))), hydra.core.Field(hydra.core.Name("context"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), hydra.core.Name("context")))), original.value)))), hydra.core.Field(hydra.core.Name("head"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), hydra.core.Name("head")))), original.value)))), hydra.core.Field(hydra.core.Name("constructors"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), hydra.core.Name("constructors")))), original.value)))), hydra.core.Field(hydra.core.Name("deriving"), new_val.value))))))

def data_declaration_with_head(original: hydra.phantoms.TTerm[hydra.haskell.syntax.DataDeclaration], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.DeclarationHead]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), (hydra.core.Field(hydra.core.Name("keyword"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), hydra.core.Name("keyword")))), original.value)))), hydra.core.Field(hydra.core.Name("context"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), hydra.core.Name("context")))), original.value)))), hydra.core.Field(hydra.core.Name("head"), new_val.value), hydra.core.Field(hydra.core.Name("constructors"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), hydra.core.Name("constructors")))), original.value)))), hydra.core.Field(hydra.core.Name("deriving"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), hydra.core.Name("deriving")))), original.value)))))))))

def data_declaration_with_keyword(original: hydra.phantoms.TTerm[hydra.haskell.syntax.DataDeclaration], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.DataOrNewtype]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), (hydra.core.Field(hydra.core.Name("keyword"), new_val.value), hydra.core.Field(hydra.core.Name("context"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), hydra.core.Name("context")))), original.value)))), hydra.core.Field(hydra.core.Name("head"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), hydra.core.Name("head")))), original.value)))), hydra.core.Field(hydra.core.Name("constructors"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), hydra.core.Name("constructors")))), original.value)))), hydra.core.Field(hydra.core.Name("deriving"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), hydra.core.Name("deriving")))), original.value)))))))))

data_or_newtype_data = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.DataOrNewtype"), hydra.core.Field(hydra.core.Name("data"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

data_or_newtype_newtype = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.DataOrNewtype"), hydra.core.Field(hydra.core.Name("newtype"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def declaration_data(x: hydra.phantoms.TTerm[hydra.haskell.syntax.DataDeclaration]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Declaration"), hydra.core.Field(hydra.core.Name("data"), x.value)))))

def declaration_head_application(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ApplicationDeclarationHead]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.DeclarationHead"), hydra.core.Field(hydra.core.Name("application"), x.value)))))

def declaration_head_parens(x: hydra.phantoms.TTerm[hydra.haskell.syntax.DeclarationHead]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.DeclarationHead"), hydra.core.Field(hydra.core.Name("parens"), x.value)))))

def declaration_head_simple(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.DeclarationHead"), hydra.core.Field(hydra.core.Name("simple"), x.value)))))

def declaration_type(x: hydra.phantoms.TTerm[hydra.haskell.syntax.TypeDeclaration]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Declaration"), hydra.core.Field(hydra.core.Name("type"), x.value)))))

def declaration_typed_binding(x: hydra.phantoms.TTerm[hydra.haskell.syntax.TypedBinding]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Declaration"), hydra.core.Field(hydra.core.Name("typedBinding"), x.value)))))

def declaration_value_binding(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ValueBinding]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Declaration"), hydra.core.Field(hydra.core.Name("valueBinding"), x.value)))))

def declaration_with_comments(body: hydra.phantoms.TTerm[hydra.haskell.syntax.Declaration], comments: hydra.phantoms.TTerm[Maybe[str]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.DeclarationWithComments"), (hydra.core.Field(hydra.core.Name("body"), body.value), hydra.core.Field(hydra.core.Name("comments"), comments.value))))))

def declaration_with_comments_body(x: hydra.phantoms.TTerm[hydra.haskell.syntax.DeclarationWithComments]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DeclarationWithComments"), hydra.core.Name("body")))), x.value))))

def declaration_with_comments_comments(x: hydra.phantoms.TTerm[hydra.haskell.syntax.DeclarationWithComments]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DeclarationWithComments"), hydra.core.Name("comments")))), x.value))))

def declaration_with_comments_with_body(original: hydra.phantoms.TTerm[hydra.haskell.syntax.DeclarationWithComments], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Declaration]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.DeclarationWithComments"), (hydra.core.Field(hydra.core.Name("body"), new_val.value), hydra.core.Field(hydra.core.Name("comments"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DeclarationWithComments"), hydra.core.Name("comments")))), original.value)))))))))

def declaration_with_comments_with_comments(original: hydra.phantoms.TTerm[hydra.haskell.syntax.DeclarationWithComments], new_val: hydra.phantoms.TTerm[Maybe[str]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.DeclarationWithComments"), (hydra.core.Field(hydra.core.Name("body"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.DeclarationWithComments"), hydra.core.Name("body")))), original.value)))), hydra.core.Field(hydra.core.Name("comments"), new_val.value))))))

def deriving(x: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Name]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.haskell.syntax.Deriving"), x.value))))

def export_declaration(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ImportExportSpec]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Export"), hydra.core.Field(hydra.core.Name("declaration"), x.value)))))

def export_module(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ModuleName]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Export"), hydra.core.Field(hydra.core.Name("module"), x.value)))))

def expression_application(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ApplicationExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Expression"), hydra.core.Field(hydra.core.Name("application"), x.value)))))

def expression_case(x: hydra.phantoms.TTerm[hydra.haskell.syntax.CaseExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Expression"), hydra.core.Field(hydra.core.Name("case"), x.value)))))

def expression_construct_record(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ConstructRecordExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Expression"), hydra.core.Field(hydra.core.Name("constructRecord"), x.value)))))

def expression_do(x: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Statement]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Expression"), hydra.core.Field(hydra.core.Name("do"), x.value)))))

def expression_if(x: hydra.phantoms.TTerm[hydra.haskell.syntax.IfExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Expression"), hydra.core.Field(hydra.core.Name("if"), x.value)))))

def expression_infix_application(x: hydra.phantoms.TTerm[hydra.haskell.syntax.InfixApplicationExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Expression"), hydra.core.Field(hydra.core.Name("infixApplication"), x.value)))))

def expression_lambda(x: hydra.phantoms.TTerm[hydra.haskell.syntax.LambdaExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Expression"), hydra.core.Field(hydra.core.Name("lambda"), x.value)))))

def expression_left_section(x: hydra.phantoms.TTerm[hydra.haskell.syntax.SectionExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Expression"), hydra.core.Field(hydra.core.Name("leftSection"), x.value)))))

def expression_let(x: hydra.phantoms.TTerm[hydra.haskell.syntax.LetExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Expression"), hydra.core.Field(hydra.core.Name("let"), x.value)))))

def expression_list(x: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Expression]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Expression"), hydra.core.Field(hydra.core.Name("list"), x.value)))))

def expression_literal(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Literal]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Expression"), hydra.core.Field(hydra.core.Name("literal"), x.value)))))

def expression_parens(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Expression"), hydra.core.Field(hydra.core.Name("parens"), x.value)))))

def expression_prefix_application(x: hydra.phantoms.TTerm[hydra.haskell.syntax.PrefixApplicationExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Expression"), hydra.core.Field(hydra.core.Name("prefixApplication"), x.value)))))

def expression_right_section(x: hydra.phantoms.TTerm[hydra.haskell.syntax.SectionExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Expression"), hydra.core.Field(hydra.core.Name("rightSection"), x.value)))))

def expression_tuple(x: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Expression]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Expression"), hydra.core.Field(hydra.core.Name("tuple"), x.value)))))

def expression_type_signature(x: hydra.phantoms.TTerm[hydra.haskell.syntax.TypeSignatureExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Expression"), hydra.core.Field(hydra.core.Name("typeSignature"), x.value)))))

def expression_update_record(x: hydra.phantoms.TTerm[hydra.haskell.syntax.UpdateRecordExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Expression"), hydra.core.Field(hydra.core.Name("updateRecord"), x.value)))))

def expression_variable(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Expression"), hydra.core.Field(hydra.core.Name("variable"), x.value)))))

def field(name: hydra.phantoms.TTerm[hydra.haskell.syntax.Name], type: hydra.phantoms.TTerm[hydra.haskell.syntax.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.Field"), (hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("type"), type.value))))))

def field_name(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Field]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Field"), hydra.core.Name("name")))), x.value))))

def field_type(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Field]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Field"), hydra.core.Name("type")))), x.value))))

def field_update(name: hydra.phantoms.TTerm[hydra.haskell.syntax.Name], value: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.FieldUpdate"), (hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("value"), value.value))))))

def field_update_name(x: hydra.phantoms.TTerm[hydra.haskell.syntax.FieldUpdate]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.FieldUpdate"), hydra.core.Name("name")))), x.value))))

def field_update_value(x: hydra.phantoms.TTerm[hydra.haskell.syntax.FieldUpdate]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.FieldUpdate"), hydra.core.Name("value")))), x.value))))

def field_update_with_name(original: hydra.phantoms.TTerm[hydra.haskell.syntax.FieldUpdate], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.FieldUpdate"), (hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("value"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.FieldUpdate"), hydra.core.Name("value")))), original.value)))))))))

def field_update_with_value(original: hydra.phantoms.TTerm[hydra.haskell.syntax.FieldUpdate], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.FieldUpdate"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.FieldUpdate"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("value"), new_val.value))))))

def field_with_comments(field: hydra.phantoms.TTerm[hydra.haskell.syntax.Field], comments: hydra.phantoms.TTerm[Maybe[str]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.FieldWithComments"), (hydra.core.Field(hydra.core.Name("field"), field.value), hydra.core.Field(hydra.core.Name("comments"), comments.value))))))

def field_with_comments_comments(x: hydra.phantoms.TTerm[hydra.haskell.syntax.FieldWithComments]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.FieldWithComments"), hydra.core.Name("comments")))), x.value))))

def field_with_comments_field(x: hydra.phantoms.TTerm[hydra.haskell.syntax.FieldWithComments]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.FieldWithComments"), hydra.core.Name("field")))), x.value))))

def field_with_comments_with_comments(original: hydra.phantoms.TTerm[hydra.haskell.syntax.FieldWithComments], new_val: hydra.phantoms.TTerm[Maybe[str]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.FieldWithComments"), (hydra.core.Field(hydra.core.Name("field"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.FieldWithComments"), hydra.core.Name("field")))), original.value)))), hydra.core.Field(hydra.core.Name("comments"), new_val.value))))))

def field_with_comments_with_field(original: hydra.phantoms.TTerm[hydra.haskell.syntax.FieldWithComments], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Field]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.FieldWithComments"), (hydra.core.Field(hydra.core.Name("field"), new_val.value), hydra.core.Field(hydra.core.Name("comments"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.FieldWithComments"), hydra.core.Name("comments")))), original.value)))))))))

def field_with_name(original: hydra.phantoms.TTerm[hydra.haskell.syntax.Field], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.Field"), (hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Field"), hydra.core.Name("type")))), original.value)))))))))

def field_with_type(original: hydra.phantoms.TTerm[hydra.haskell.syntax.Field], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.Field"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Field"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("type"), new_val.value))))))

def function_type(domain: hydra.phantoms.TTerm[hydra.haskell.syntax.Type], codomain: hydra.phantoms.TTerm[hydra.haskell.syntax.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.FunctionType"), (hydra.core.Field(hydra.core.Name("domain"), domain.value), hydra.core.Field(hydra.core.Name("codomain"), codomain.value))))))

def function_type_codomain(x: hydra.phantoms.TTerm[hydra.haskell.syntax.FunctionType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.FunctionType"), hydra.core.Name("codomain")))), x.value))))

def function_type_domain(x: hydra.phantoms.TTerm[hydra.haskell.syntax.FunctionType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.FunctionType"), hydra.core.Name("domain")))), x.value))))

def function_type_with_codomain(original: hydra.phantoms.TTerm[hydra.haskell.syntax.FunctionType], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.FunctionType"), (hydra.core.Field(hydra.core.Name("domain"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.FunctionType"), hydra.core.Name("domain")))), original.value)))), hydra.core.Field(hydra.core.Name("codomain"), new_val.value))))))

def function_type_with_domain(original: hydra.phantoms.TTerm[hydra.haskell.syntax.FunctionType], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.FunctionType"), (hydra.core.Field(hydra.core.Name("domain"), new_val.value), hydra.core.Field(hydra.core.Name("codomain"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.FunctionType"), hydra.core.Name("codomain")))), original.value)))))))))

def if_expression(condition: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression], then: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression], else_: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.IfExpression"), (hydra.core.Field(hydra.core.Name("condition"), condition.value), hydra.core.Field(hydra.core.Name("then"), then.value), hydra.core.Field(hydra.core.Name("else"), else_.value))))))

def if_expression_condition(x: hydra.phantoms.TTerm[hydra.haskell.syntax.IfExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.IfExpression"), hydra.core.Name("condition")))), x.value))))

def if_expression_else(x: hydra.phantoms.TTerm[hydra.haskell.syntax.IfExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.IfExpression"), hydra.core.Name("else")))), x.value))))

def if_expression_then(x: hydra.phantoms.TTerm[hydra.haskell.syntax.IfExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.IfExpression"), hydra.core.Name("then")))), x.value))))

def if_expression_with_condition(original: hydra.phantoms.TTerm[hydra.haskell.syntax.IfExpression], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.IfExpression"), (hydra.core.Field(hydra.core.Name("condition"), new_val.value), hydra.core.Field(hydra.core.Name("then"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.IfExpression"), hydra.core.Name("then")))), original.value)))), hydra.core.Field(hydra.core.Name("else"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.IfExpression"), hydra.core.Name("else")))), original.value)))))))))

def if_expression_with_else(original: hydra.phantoms.TTerm[hydra.haskell.syntax.IfExpression], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.IfExpression"), (hydra.core.Field(hydra.core.Name("condition"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.IfExpression"), hydra.core.Name("condition")))), original.value)))), hydra.core.Field(hydra.core.Name("then"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.IfExpression"), hydra.core.Name("then")))), original.value)))), hydra.core.Field(hydra.core.Name("else"), new_val.value))))))

def if_expression_with_then(original: hydra.phantoms.TTerm[hydra.haskell.syntax.IfExpression], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.IfExpression"), (hydra.core.Field(hydra.core.Name("condition"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.IfExpression"), hydra.core.Name("condition")))), original.value)))), hydra.core.Field(hydra.core.Name("then"), new_val.value), hydra.core.Field(hydra.core.Name("else"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.IfExpression"), hydra.core.Name("else")))), original.value)))))))))

def import_(qualified: hydra.phantoms.TTerm[bool], module: hydra.phantoms.TTerm[hydra.haskell.syntax.ModuleName], as_: hydra.phantoms.TTerm[Maybe[hydra.haskell.syntax.ModuleName]], spec: hydra.phantoms.TTerm[Maybe[hydra.haskell.syntax.SpecImport]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.Import"), (hydra.core.Field(hydra.core.Name("qualified"), qualified.value), hydra.core.Field(hydra.core.Name("module"), module.value), hydra.core.Field(hydra.core.Name("as"), as_.value), hydra.core.Field(hydra.core.Name("spec"), spec.value))))))

def import_as(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Import]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Import"), hydra.core.Name("as")))), x.value))))

def import_export_spec(modifier: hydra.phantoms.TTerm[Maybe[hydra.haskell.syntax.ImportModifier]], name: hydra.phantoms.TTerm[hydra.haskell.syntax.Name], subspec: hydra.phantoms.TTerm[Maybe[hydra.haskell.syntax.SubspecImportExportSpec]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), (hydra.core.Field(hydra.core.Name("modifier"), modifier.value), hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("subspec"), subspec.value))))))

def import_export_spec_modifier(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ImportExportSpec]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), hydra.core.Name("modifier")))), x.value))))

def import_export_spec_name(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ImportExportSpec]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), hydra.core.Name("name")))), x.value))))

def import_export_spec_subspec(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ImportExportSpec]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), hydra.core.Name("subspec")))), x.value))))

def import_export_spec_with_modifier(original: hydra.phantoms.TTerm[hydra.haskell.syntax.ImportExportSpec], new_val: hydra.phantoms.TTerm[Maybe[hydra.haskell.syntax.ImportModifier]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), (hydra.core.Field(hydra.core.Name("modifier"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("subspec"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), hydra.core.Name("subspec")))), original.value)))))))))

def import_export_spec_with_name(original: hydra.phantoms.TTerm[hydra.haskell.syntax.ImportExportSpec], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), (hydra.core.Field(hydra.core.Name("modifier"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), hydra.core.Name("modifier")))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("subspec"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), hydra.core.Name("subspec")))), original.value)))))))))

def import_export_spec_with_subspec(original: hydra.phantoms.TTerm[hydra.haskell.syntax.ImportExportSpec], new_val: hydra.phantoms.TTerm[Maybe[hydra.haskell.syntax.SubspecImportExportSpec]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), (hydra.core.Field(hydra.core.Name("modifier"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), hydra.core.Name("modifier")))), original.value)))), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("subspec"), new_val.value))))))

import_modifier_pattern = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.ImportModifier"), hydra.core.Field(hydra.core.Name("pattern"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

import_modifier_type = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.ImportModifier"), hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def import_module(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Import]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Import"), hydra.core.Name("module")))), x.value))))

def import_qualified(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Import]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Import"), hydra.core.Name("qualified")))), x.value))))

def import_spec(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Import]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Import"), hydra.core.Name("spec")))), x.value))))

def import_with_as(original: hydra.phantoms.TTerm[hydra.haskell.syntax.Import], new_val: hydra.phantoms.TTerm[Maybe[hydra.haskell.syntax.ModuleName]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.Import"), (hydra.core.Field(hydra.core.Name("qualified"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Import"), hydra.core.Name("qualified")))), original.value)))), hydra.core.Field(hydra.core.Name("module"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Import"), hydra.core.Name("module")))), original.value)))), hydra.core.Field(hydra.core.Name("as"), new_val.value), hydra.core.Field(hydra.core.Name("spec"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Import"), hydra.core.Name("spec")))), original.value)))))))))

def import_with_module(original: hydra.phantoms.TTerm[hydra.haskell.syntax.Import], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.ModuleName]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.Import"), (hydra.core.Field(hydra.core.Name("qualified"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Import"), hydra.core.Name("qualified")))), original.value)))), hydra.core.Field(hydra.core.Name("module"), new_val.value), hydra.core.Field(hydra.core.Name("as"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Import"), hydra.core.Name("as")))), original.value)))), hydra.core.Field(hydra.core.Name("spec"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Import"), hydra.core.Name("spec")))), original.value)))))))))

def import_with_qualified(original: hydra.phantoms.TTerm[hydra.haskell.syntax.Import], new_val: hydra.phantoms.TTerm[bool]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.Import"), (hydra.core.Field(hydra.core.Name("qualified"), new_val.value), hydra.core.Field(hydra.core.Name("module"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Import"), hydra.core.Name("module")))), original.value)))), hydra.core.Field(hydra.core.Name("as"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Import"), hydra.core.Name("as")))), original.value)))), hydra.core.Field(hydra.core.Name("spec"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Import"), hydra.core.Name("spec")))), original.value)))))))))

def import_with_spec(original: hydra.phantoms.TTerm[hydra.haskell.syntax.Import], new_val: hydra.phantoms.TTerm[Maybe[hydra.haskell.syntax.SpecImport]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.Import"), (hydra.core.Field(hydra.core.Name("qualified"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Import"), hydra.core.Name("qualified")))), original.value)))), hydra.core.Field(hydra.core.Name("module"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Import"), hydra.core.Name("module")))), original.value)))), hydra.core.Field(hydra.core.Name("as"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Import"), hydra.core.Name("as")))), original.value)))), hydra.core.Field(hydra.core.Name("spec"), new_val.value))))))

def infix_application_expression(lhs: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression], operator: hydra.phantoms.TTerm[hydra.haskell.syntax.Operator], rhs: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), (hydra.core.Field(hydra.core.Name("lhs"), lhs.value), hydra.core.Field(hydra.core.Name("operator"), operator.value), hydra.core.Field(hydra.core.Name("rhs"), rhs.value))))))

def infix_application_expression_lhs(x: hydra.phantoms.TTerm[hydra.haskell.syntax.InfixApplicationExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), hydra.core.Name("lhs")))), x.value))))

def infix_application_expression_operator(x: hydra.phantoms.TTerm[hydra.haskell.syntax.InfixApplicationExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), hydra.core.Name("operator")))), x.value))))

def infix_application_expression_rhs(x: hydra.phantoms.TTerm[hydra.haskell.syntax.InfixApplicationExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), hydra.core.Name("rhs")))), x.value))))

def infix_application_expression_with_lhs(original: hydra.phantoms.TTerm[hydra.haskell.syntax.InfixApplicationExpression], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), (hydra.core.Field(hydra.core.Name("lhs"), new_val.value), hydra.core.Field(hydra.core.Name("operator"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), hydra.core.Name("operator")))), original.value)))), hydra.core.Field(hydra.core.Name("rhs"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), hydra.core.Name("rhs")))), original.value)))))))))

def infix_application_expression_with_operator(original: hydra.phantoms.TTerm[hydra.haskell.syntax.InfixApplicationExpression], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Operator]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), (hydra.core.Field(hydra.core.Name("lhs"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), hydra.core.Name("lhs")))), original.value)))), hydra.core.Field(hydra.core.Name("operator"), new_val.value), hydra.core.Field(hydra.core.Name("rhs"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), hydra.core.Name("rhs")))), original.value)))))))))

def infix_application_expression_with_rhs(original: hydra.phantoms.TTerm[hydra.haskell.syntax.InfixApplicationExpression], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), (hydra.core.Field(hydra.core.Name("lhs"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), hydra.core.Name("lhs")))), original.value)))), hydra.core.Field(hydra.core.Name("operator"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), hydra.core.Name("operator")))), original.value)))), hydra.core.Field(hydra.core.Name("rhs"), new_val.value))))))

def infix_type(lhs: hydra.phantoms.TTerm[hydra.haskell.syntax.Type], operator: hydra.phantoms.TTerm[hydra.haskell.syntax.Operator], rhs: hydra.phantoms.TTerm[hydra.haskell.syntax.Operator]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.InfixType"), (hydra.core.Field(hydra.core.Name("lhs"), lhs.value), hydra.core.Field(hydra.core.Name("operator"), operator.value), hydra.core.Field(hydra.core.Name("rhs"), rhs.value))))))

def infix_type_lhs(x: hydra.phantoms.TTerm[hydra.haskell.syntax.InfixType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.InfixType"), hydra.core.Name("lhs")))), x.value))))

def infix_type_operator(x: hydra.phantoms.TTerm[hydra.haskell.syntax.InfixType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.InfixType"), hydra.core.Name("operator")))), x.value))))

def infix_type_rhs(x: hydra.phantoms.TTerm[hydra.haskell.syntax.InfixType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.InfixType"), hydra.core.Name("rhs")))), x.value))))

def infix_type_with_lhs(original: hydra.phantoms.TTerm[hydra.haskell.syntax.InfixType], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.InfixType"), (hydra.core.Field(hydra.core.Name("lhs"), new_val.value), hydra.core.Field(hydra.core.Name("operator"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.InfixType"), hydra.core.Name("operator")))), original.value)))), hydra.core.Field(hydra.core.Name("rhs"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.InfixType"), hydra.core.Name("rhs")))), original.value)))))))))

def infix_type_with_operator(original: hydra.phantoms.TTerm[hydra.haskell.syntax.InfixType], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Operator]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.InfixType"), (hydra.core.Field(hydra.core.Name("lhs"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.InfixType"), hydra.core.Name("lhs")))), original.value)))), hydra.core.Field(hydra.core.Name("operator"), new_val.value), hydra.core.Field(hydra.core.Name("rhs"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.InfixType"), hydra.core.Name("rhs")))), original.value)))))))))

def infix_type_with_rhs(original: hydra.phantoms.TTerm[hydra.haskell.syntax.InfixType], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Operator]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.InfixType"), (hydra.core.Field(hydra.core.Name("lhs"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.InfixType"), hydra.core.Name("lhs")))), original.value)))), hydra.core.Field(hydra.core.Name("operator"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.InfixType"), hydra.core.Name("operator")))), original.value)))), hydra.core.Field(hydra.core.Name("rhs"), new_val.value))))))

def lambda_expression(bindings: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Pattern]], inner: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.LambdaExpression"), (hydra.core.Field(hydra.core.Name("bindings"), bindings.value), hydra.core.Field(hydra.core.Name("inner"), inner.value))))))

def lambda_expression_bindings(x: hydra.phantoms.TTerm[hydra.haskell.syntax.LambdaExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.LambdaExpression"), hydra.core.Name("bindings")))), x.value))))

def lambda_expression_inner(x: hydra.phantoms.TTerm[hydra.haskell.syntax.LambdaExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.LambdaExpression"), hydra.core.Name("inner")))), x.value))))

def lambda_expression_with_bindings(original: hydra.phantoms.TTerm[hydra.haskell.syntax.LambdaExpression], new_val: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Pattern]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.LambdaExpression"), (hydra.core.Field(hydra.core.Name("bindings"), new_val.value), hydra.core.Field(hydra.core.Name("inner"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.LambdaExpression"), hydra.core.Name("inner")))), original.value)))))))))

def lambda_expression_with_inner(original: hydra.phantoms.TTerm[hydra.haskell.syntax.LambdaExpression], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.LambdaExpression"), (hydra.core.Field(hydra.core.Name("bindings"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.LambdaExpression"), hydra.core.Name("bindings")))), original.value)))), hydra.core.Field(hydra.core.Name("inner"), new_val.value))))))

def let_expression(bindings: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.LocalBinding]], inner: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.LetExpression"), (hydra.core.Field(hydra.core.Name("bindings"), bindings.value), hydra.core.Field(hydra.core.Name("inner"), inner.value))))))

def let_expression_bindings(x: hydra.phantoms.TTerm[hydra.haskell.syntax.LetExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.LetExpression"), hydra.core.Name("bindings")))), x.value))))

def let_expression_inner(x: hydra.phantoms.TTerm[hydra.haskell.syntax.LetExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.LetExpression"), hydra.core.Name("inner")))), x.value))))

def let_expression_with_bindings(original: hydra.phantoms.TTerm[hydra.haskell.syntax.LetExpression], new_val: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.LocalBinding]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.LetExpression"), (hydra.core.Field(hydra.core.Name("bindings"), new_val.value), hydra.core.Field(hydra.core.Name("inner"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.LetExpression"), hydra.core.Name("inner")))), original.value)))))))))

def let_expression_with_inner(original: hydra.phantoms.TTerm[hydra.haskell.syntax.LetExpression], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.LetExpression"), (hydra.core.Field(hydra.core.Name("bindings"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.LetExpression"), hydra.core.Name("bindings")))), original.value)))), hydra.core.Field(hydra.core.Name("inner"), new_val.value))))))

def literal_char(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Literal"), hydra.core.Field(hydra.core.Name("char"), x.value)))))

def literal_double(x: hydra.phantoms.TTerm[float]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Literal"), hydra.core.Field(hydra.core.Name("double"), x.value)))))

def literal_float(x: hydra.phantoms.TTerm[float]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Literal"), hydra.core.Field(hydra.core.Name("float"), x.value)))))

def literal_int(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Literal"), hydra.core.Field(hydra.core.Name("int"), x.value)))))

def literal_integer(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Literal"), hydra.core.Field(hydra.core.Name("integer"), x.value)))))

def literal_string(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Literal"), hydra.core.Field(hydra.core.Name("string"), x.value)))))

def local_binding_signature(x: hydra.phantoms.TTerm[hydra.haskell.syntax.TypeSignature]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.LocalBinding"), hydra.core.Field(hydra.core.Name("signature"), x.value)))))

def local_binding_value(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ValueBinding]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.LocalBinding"), hydra.core.Field(hydra.core.Name("value"), x.value)))))

def local_bindings(x: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.LocalBinding]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.haskell.syntax.LocalBindings"), x.value))))

def module(head: hydra.phantoms.TTerm[Maybe[hydra.haskell.syntax.ModuleHead]], imports: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Import]], declarations: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.DeclarationWithComments]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.Module"), (hydra.core.Field(hydra.core.Name("head"), head.value), hydra.core.Field(hydra.core.Name("imports"), imports.value), hydra.core.Field(hydra.core.Name("declarations"), declarations.value))))))

def module_declarations(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Module]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Module"), hydra.core.Name("declarations")))), x.value))))

def module_head(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Module]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Module"), hydra.core.Name("head")))), x.value))))

def module_head_comments(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ModuleHead]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ModuleHead"), hydra.core.Name("comments")))), x.value))))

def module_head_exports(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ModuleHead]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ModuleHead"), hydra.core.Name("exports")))), x.value))))

def module_head_name(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ModuleHead]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ModuleHead"), hydra.core.Name("name")))), x.value))))

def module_head_with_comments(original: hydra.phantoms.TTerm[hydra.haskell.syntax.ModuleHead], new_val: hydra.phantoms.TTerm[Maybe[str]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ModuleHead"), (hydra.core.Field(hydra.core.Name("comments"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ModuleHead"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("exports"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ModuleHead"), hydra.core.Name("exports")))), original.value)))))))))

def module_head_with_exports(original: hydra.phantoms.TTerm[hydra.haskell.syntax.ModuleHead], new_val: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Export]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ModuleHead"), (hydra.core.Field(hydra.core.Name("comments"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ModuleHead"), hydra.core.Name("comments")))), original.value)))), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ModuleHead"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("exports"), new_val.value))))))

def module_head_with_name(original: hydra.phantoms.TTerm[hydra.haskell.syntax.ModuleHead], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.ModuleName]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ModuleHead"), (hydra.core.Field(hydra.core.Name("comments"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ModuleHead"), hydra.core.Name("comments")))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("exports"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.ModuleHead"), hydra.core.Name("exports")))), original.value)))))))))

def module_head_(comments: hydra.phantoms.TTerm[Maybe[str]], name: hydra.phantoms.TTerm[hydra.haskell.syntax.ModuleName], exports: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Export]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.ModuleHead"), (hydra.core.Field(hydra.core.Name("comments"), comments.value), hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("exports"), exports.value))))))

def module_imports(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Module]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Module"), hydra.core.Name("imports")))), x.value))))

def module_name(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.haskell.syntax.ModuleName"), x.value))))

def module_with_declarations(original: hydra.phantoms.TTerm[hydra.haskell.syntax.Module], new_val: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.DeclarationWithComments]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.Module"), (hydra.core.Field(hydra.core.Name("head"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Module"), hydra.core.Name("head")))), original.value)))), hydra.core.Field(hydra.core.Name("imports"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Module"), hydra.core.Name("imports")))), original.value)))), hydra.core.Field(hydra.core.Name("declarations"), new_val.value))))))

def module_with_head(original: hydra.phantoms.TTerm[hydra.haskell.syntax.Module], new_val: hydra.phantoms.TTerm[Maybe[hydra.haskell.syntax.ModuleHead]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.Module"), (hydra.core.Field(hydra.core.Name("head"), new_val.value), hydra.core.Field(hydra.core.Name("imports"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Module"), hydra.core.Name("imports")))), original.value)))), hydra.core.Field(hydra.core.Name("declarations"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Module"), hydra.core.Name("declarations")))), original.value)))))))))

def module_with_imports(original: hydra.phantoms.TTerm[hydra.haskell.syntax.Module], new_val: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Import]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.Module"), (hydra.core.Field(hydra.core.Name("head"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Module"), hydra.core.Name("head")))), original.value)))), hydra.core.Field(hydra.core.Name("imports"), new_val.value), hydra.core.Field(hydra.core.Name("declarations"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.Module"), hydra.core.Name("declarations")))), original.value)))))))))

def name_implicit(x: hydra.phantoms.TTerm[hydra.haskell.syntax.QualifiedName]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Name"), hydra.core.Field(hydra.core.Name("implicit"), x.value)))))

def name_normal(x: hydra.phantoms.TTerm[hydra.haskell.syntax.QualifiedName]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Name"), hydra.core.Field(hydra.core.Name("normal"), x.value)))))

def name_parens(x: hydra.phantoms.TTerm[hydra.haskell.syntax.QualifiedName]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Name"), hydra.core.Field(hydra.core.Name("parens"), x.value)))))

def name_part(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.haskell.syntax.NamePart"), x.value))))

def operator_backtick(x: hydra.phantoms.TTerm[hydra.haskell.syntax.QualifiedName]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Operator"), hydra.core.Field(hydra.core.Name("backtick"), x.value)))))

def operator_normal(x: hydra.phantoms.TTerm[hydra.haskell.syntax.QualifiedName]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Operator"), hydra.core.Field(hydra.core.Name("normal"), x.value)))))

def ordinary_constructor(name: hydra.phantoms.TTerm[hydra.haskell.syntax.Name], fields: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Type]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.OrdinaryConstructor"), (hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("fields"), fields.value))))))

def ordinary_constructor_fields(x: hydra.phantoms.TTerm[hydra.haskell.syntax.OrdinaryConstructor]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.OrdinaryConstructor"), hydra.core.Name("fields")))), x.value))))

def ordinary_constructor_name(x: hydra.phantoms.TTerm[hydra.haskell.syntax.OrdinaryConstructor]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.OrdinaryConstructor"), hydra.core.Name("name")))), x.value))))

def ordinary_constructor_with_fields(original: hydra.phantoms.TTerm[hydra.haskell.syntax.OrdinaryConstructor], new_val: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Type]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.OrdinaryConstructor"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.OrdinaryConstructor"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("fields"), new_val.value))))))

def ordinary_constructor_with_name(original: hydra.phantoms.TTerm[hydra.haskell.syntax.OrdinaryConstructor], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.OrdinaryConstructor"), (hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("fields"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.OrdinaryConstructor"), hydra.core.Name("fields")))), original.value)))))))))

def pattern_application(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ApplicationPattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Pattern"), hydra.core.Field(hydra.core.Name("application"), x.value)))))

def pattern_as(x: hydra.phantoms.TTerm[hydra.haskell.syntax.AsPattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Pattern"), hydra.core.Field(hydra.core.Name("as"), x.value)))))

def pattern_field(name: hydra.phantoms.TTerm[hydra.haskell.syntax.Name], pattern: hydra.phantoms.TTerm[hydra.haskell.syntax.Pattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.PatternField"), (hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("pattern"), pattern.value))))))

def pattern_field_name(x: hydra.phantoms.TTerm[hydra.haskell.syntax.PatternField]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.PatternField"), hydra.core.Name("name")))), x.value))))

def pattern_field_pattern(x: hydra.phantoms.TTerm[hydra.haskell.syntax.PatternField]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.PatternField"), hydra.core.Name("pattern")))), x.value))))

def pattern_field_with_name(original: hydra.phantoms.TTerm[hydra.haskell.syntax.PatternField], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.PatternField"), (hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("pattern"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.PatternField"), hydra.core.Name("pattern")))), original.value)))))))))

def pattern_field_with_pattern(original: hydra.phantoms.TTerm[hydra.haskell.syntax.PatternField], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Pattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.PatternField"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.PatternField"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("pattern"), new_val.value))))))

def pattern_list(x: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Pattern]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Pattern"), hydra.core.Field(hydra.core.Name("list"), x.value)))))

def pattern_literal(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Literal]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Pattern"), hydra.core.Field(hydra.core.Name("literal"), x.value)))))

def pattern_name(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Pattern"), hydra.core.Field(hydra.core.Name("name"), x.value)))))

def pattern_parens(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Pattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Pattern"), hydra.core.Field(hydra.core.Name("parens"), x.value)))))

def pattern_record(x: hydra.phantoms.TTerm[hydra.haskell.syntax.RecordPattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Pattern"), hydra.core.Field(hydra.core.Name("record"), x.value)))))

def pattern_tuple(x: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Pattern]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Pattern"), hydra.core.Field(hydra.core.Name("tuple"), x.value)))))

def pattern_typed(x: hydra.phantoms.TTerm[hydra.haskell.syntax.TypedPattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Pattern"), hydra.core.Field(hydra.core.Name("typed"), x.value)))))

pattern_wildcard = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Pattern"), hydra.core.Field(hydra.core.Name("wildcard"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def prefix_application_expression(operator: hydra.phantoms.TTerm[hydra.haskell.syntax.Operator], rhs: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.PrefixApplicationExpression"), (hydra.core.Field(hydra.core.Name("operator"), operator.value), hydra.core.Field(hydra.core.Name("rhs"), rhs.value))))))

def prefix_application_expression_operator(x: hydra.phantoms.TTerm[hydra.haskell.syntax.PrefixApplicationExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.PrefixApplicationExpression"), hydra.core.Name("operator")))), x.value))))

def prefix_application_expression_rhs(x: hydra.phantoms.TTerm[hydra.haskell.syntax.PrefixApplicationExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.PrefixApplicationExpression"), hydra.core.Name("rhs")))), x.value))))

def prefix_application_expression_with_operator(original: hydra.phantoms.TTerm[hydra.haskell.syntax.PrefixApplicationExpression], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Operator]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.PrefixApplicationExpression"), (hydra.core.Field(hydra.core.Name("operator"), new_val.value), hydra.core.Field(hydra.core.Name("rhs"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.PrefixApplicationExpression"), hydra.core.Name("rhs")))), original.value)))))))))

def prefix_application_expression_with_rhs(original: hydra.phantoms.TTerm[hydra.haskell.syntax.PrefixApplicationExpression], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.PrefixApplicationExpression"), (hydra.core.Field(hydra.core.Name("operator"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.PrefixApplicationExpression"), hydra.core.Name("operator")))), original.value)))), hydra.core.Field(hydra.core.Name("rhs"), new_val.value))))))

def qualified_name(qualifiers: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.NamePart]], unqualified: hydra.phantoms.TTerm[hydra.haskell.syntax.NamePart]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.QualifiedName"), (hydra.core.Field(hydra.core.Name("qualifiers"), qualifiers.value), hydra.core.Field(hydra.core.Name("unqualified"), unqualified.value))))))

def qualified_name_qualifiers(x: hydra.phantoms.TTerm[hydra.haskell.syntax.QualifiedName]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.QualifiedName"), hydra.core.Name("qualifiers")))), x.value))))

def qualified_name_unqualified(x: hydra.phantoms.TTerm[hydra.haskell.syntax.QualifiedName]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.QualifiedName"), hydra.core.Name("unqualified")))), x.value))))

def qualified_name_with_qualifiers(original: hydra.phantoms.TTerm[hydra.haskell.syntax.QualifiedName], new_val: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.NamePart]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.QualifiedName"), (hydra.core.Field(hydra.core.Name("qualifiers"), new_val.value), hydra.core.Field(hydra.core.Name("unqualified"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.QualifiedName"), hydra.core.Name("unqualified")))), original.value)))))))))

def qualified_name_with_unqualified(original: hydra.phantoms.TTerm[hydra.haskell.syntax.QualifiedName], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.NamePart]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.QualifiedName"), (hydra.core.Field(hydra.core.Name("qualifiers"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.QualifiedName"), hydra.core.Name("qualifiers")))), original.value)))), hydra.core.Field(hydra.core.Name("unqualified"), new_val.value))))))

def record_constructor(name: hydra.phantoms.TTerm[hydra.haskell.syntax.Name], fields: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.FieldWithComments]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.RecordConstructor"), (hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("fields"), fields.value))))))

def record_constructor_fields(x: hydra.phantoms.TTerm[hydra.haskell.syntax.RecordConstructor]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.RecordConstructor"), hydra.core.Name("fields")))), x.value))))

def record_constructor_name(x: hydra.phantoms.TTerm[hydra.haskell.syntax.RecordConstructor]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.RecordConstructor"), hydra.core.Name("name")))), x.value))))

def record_constructor_with_fields(original: hydra.phantoms.TTerm[hydra.haskell.syntax.RecordConstructor], new_val: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.FieldWithComments]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.RecordConstructor"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.RecordConstructor"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("fields"), new_val.value))))))

def record_constructor_with_name(original: hydra.phantoms.TTerm[hydra.haskell.syntax.RecordConstructor], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.RecordConstructor"), (hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("fields"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.RecordConstructor"), hydra.core.Name("fields")))), original.value)))))))))

def record_pattern(name: hydra.phantoms.TTerm[hydra.haskell.syntax.Name], fields: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.PatternField]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.RecordPattern"), (hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("fields"), fields.value))))))

def record_pattern_fields(x: hydra.phantoms.TTerm[hydra.haskell.syntax.RecordPattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.RecordPattern"), hydra.core.Name("fields")))), x.value))))

def record_pattern_name(x: hydra.phantoms.TTerm[hydra.haskell.syntax.RecordPattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.RecordPattern"), hydra.core.Name("name")))), x.value))))

def record_pattern_with_fields(original: hydra.phantoms.TTerm[hydra.haskell.syntax.RecordPattern], new_val: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.PatternField]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.RecordPattern"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.RecordPattern"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("fields"), new_val.value))))))

def record_pattern_with_name(original: hydra.phantoms.TTerm[hydra.haskell.syntax.RecordPattern], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.RecordPattern"), (hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("fields"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.RecordPattern"), hydra.core.Name("fields")))), original.value)))))))))

def right_hand_side(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.haskell.syntax.RightHandSide"), x.value))))

def section_expression(operator: hydra.phantoms.TTerm[hydra.haskell.syntax.Operator], expression: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.SectionExpression"), (hydra.core.Field(hydra.core.Name("operator"), operator.value), hydra.core.Field(hydra.core.Name("expression"), expression.value))))))

def section_expression_expression(x: hydra.phantoms.TTerm[hydra.haskell.syntax.SectionExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.SectionExpression"), hydra.core.Name("expression")))), x.value))))

def section_expression_operator(x: hydra.phantoms.TTerm[hydra.haskell.syntax.SectionExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.SectionExpression"), hydra.core.Name("operator")))), x.value))))

def section_expression_with_expression(original: hydra.phantoms.TTerm[hydra.haskell.syntax.SectionExpression], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.SectionExpression"), (hydra.core.Field(hydra.core.Name("operator"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.SectionExpression"), hydra.core.Name("operator")))), original.value)))), hydra.core.Field(hydra.core.Name("expression"), new_val.value))))))

def section_expression_with_operator(original: hydra.phantoms.TTerm[hydra.haskell.syntax.SectionExpression], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Operator]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.SectionExpression"), (hydra.core.Field(hydra.core.Name("operator"), new_val.value), hydra.core.Field(hydra.core.Name("expression"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.SectionExpression"), hydra.core.Name("expression")))), original.value)))))))))

def simple_value_binding(pattern: hydra.phantoms.TTerm[hydra.haskell.syntax.Pattern], rhs: hydra.phantoms.TTerm[hydra.haskell.syntax.RightHandSide], local_bindings: hydra.phantoms.TTerm[Maybe[hydra.haskell.syntax.LocalBindings]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), (hydra.core.Field(hydra.core.Name("pattern"), pattern.value), hydra.core.Field(hydra.core.Name("rhs"), rhs.value), hydra.core.Field(hydra.core.Name("localBindings"), local_bindings.value))))))

def simple_value_binding_local_bindings(x: hydra.phantoms.TTerm[hydra.haskell.syntax.SimpleValueBinding]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), hydra.core.Name("localBindings")))), x.value))))

def simple_value_binding_pattern(x: hydra.phantoms.TTerm[hydra.haskell.syntax.SimpleValueBinding]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), hydra.core.Name("pattern")))), x.value))))

def simple_value_binding_rhs(x: hydra.phantoms.TTerm[hydra.haskell.syntax.SimpleValueBinding]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), hydra.core.Name("rhs")))), x.value))))

def simple_value_binding_with_local_bindings(original: hydra.phantoms.TTerm[hydra.haskell.syntax.SimpleValueBinding], new_val: hydra.phantoms.TTerm[Maybe[hydra.haskell.syntax.LocalBindings]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), (hydra.core.Field(hydra.core.Name("pattern"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), hydra.core.Name("pattern")))), original.value)))), hydra.core.Field(hydra.core.Name("rhs"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), hydra.core.Name("rhs")))), original.value)))), hydra.core.Field(hydra.core.Name("localBindings"), new_val.value))))))

def simple_value_binding_with_pattern(original: hydra.phantoms.TTerm[hydra.haskell.syntax.SimpleValueBinding], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Pattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), (hydra.core.Field(hydra.core.Name("pattern"), new_val.value), hydra.core.Field(hydra.core.Name("rhs"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), hydra.core.Name("rhs")))), original.value)))), hydra.core.Field(hydra.core.Name("localBindings"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), hydra.core.Name("localBindings")))), original.value)))))))))

def simple_value_binding_with_rhs(original: hydra.phantoms.TTerm[hydra.haskell.syntax.SimpleValueBinding], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.RightHandSide]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), (hydra.core.Field(hydra.core.Name("pattern"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), hydra.core.Name("pattern")))), original.value)))), hydra.core.Field(hydra.core.Name("rhs"), new_val.value), hydra.core.Field(hydra.core.Name("localBindings"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), hydra.core.Name("localBindings")))), original.value)))))))))

def spec_import_hiding(x: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.ImportExportSpec]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.SpecImport"), hydra.core.Field(hydra.core.Name("hiding"), x.value)))))

def spec_import_list(x: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.ImportExportSpec]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.SpecImport"), hydra.core.Field(hydra.core.Name("list"), x.value)))))

def statement(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.haskell.syntax.Statement"), x.value))))

subspec_import_export_spec_all = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.SubspecImportExportSpec"), hydra.core.Field(hydra.core.Name("all"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def subspec_import_export_spec_list(x: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Name]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.SubspecImportExportSpec"), hydra.core.Field(hydra.core.Name("list"), x.value)))))

def type_application(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ApplicationType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Type"), hydra.core.Field(hydra.core.Name("application"), x.value)))))

def type_ctx(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ContextType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Type"), hydra.core.Field(hydra.core.Name("ctx"), x.value)))))

def type_declaration(name: hydra.phantoms.TTerm[hydra.haskell.syntax.DeclarationHead], type: hydra.phantoms.TTerm[hydra.haskell.syntax.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.TypeDeclaration"), (hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("type"), type.value))))))

def type_declaration_name(x: hydra.phantoms.TTerm[hydra.haskell.syntax.TypeDeclaration]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.TypeDeclaration"), hydra.core.Name("name")))), x.value))))

def type_declaration_type(x: hydra.phantoms.TTerm[hydra.haskell.syntax.TypeDeclaration]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.TypeDeclaration"), hydra.core.Name("type")))), x.value))))

def type_declaration_with_name(original: hydra.phantoms.TTerm[hydra.haskell.syntax.TypeDeclaration], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.DeclarationHead]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.TypeDeclaration"), (hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.TypeDeclaration"), hydra.core.Name("type")))), original.value)))))))))

def type_declaration_with_type(original: hydra.phantoms.TTerm[hydra.haskell.syntax.TypeDeclaration], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.TypeDeclaration"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.TypeDeclaration"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("type"), new_val.value))))))

def type_function(x: hydra.phantoms.TTerm[hydra.haskell.syntax.FunctionType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Type"), hydra.core.Field(hydra.core.Name("function"), x.value)))))

def type_infix(x: hydra.phantoms.TTerm[hydra.haskell.syntax.InfixType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Type"), hydra.core.Field(hydra.core.Name("infix"), x.value)))))

def type_list(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Type"), hydra.core.Field(hydra.core.Name("list"), x.value)))))

def type_parens(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Type"), hydra.core.Field(hydra.core.Name("parens"), x.value)))))

def type_signature(name: hydra.phantoms.TTerm[hydra.haskell.syntax.Name], type: hydra.phantoms.TTerm[hydra.haskell.syntax.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.TypeSignature"), (hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("type"), type.value))))))

def type_signature_expression(inner: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression], type: hydra.phantoms.TTerm[hydra.haskell.syntax.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.TypeSignatureExpression"), (hydra.core.Field(hydra.core.Name("inner"), inner.value), hydra.core.Field(hydra.core.Name("type"), type.value))))))

def type_signature_expression_inner(x: hydra.phantoms.TTerm[hydra.haskell.syntax.TypeSignatureExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.TypeSignatureExpression"), hydra.core.Name("inner")))), x.value))))

def type_signature_expression_type(x: hydra.phantoms.TTerm[hydra.haskell.syntax.TypeSignatureExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.TypeSignatureExpression"), hydra.core.Name("type")))), x.value))))

def type_signature_expression_with_inner(original: hydra.phantoms.TTerm[hydra.haskell.syntax.TypeSignatureExpression], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.TypeSignatureExpression"), (hydra.core.Field(hydra.core.Name("inner"), new_val.value), hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.TypeSignatureExpression"), hydra.core.Name("type")))), original.value)))))))))

def type_signature_expression_with_type(original: hydra.phantoms.TTerm[hydra.haskell.syntax.TypeSignatureExpression], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.TypeSignatureExpression"), (hydra.core.Field(hydra.core.Name("inner"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.TypeSignatureExpression"), hydra.core.Name("inner")))), original.value)))), hydra.core.Field(hydra.core.Name("type"), new_val.value))))))

def type_signature_name(x: hydra.phantoms.TTerm[hydra.haskell.syntax.TypeSignature]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.TypeSignature"), hydra.core.Name("name")))), x.value))))

def type_signature_type(x: hydra.phantoms.TTerm[hydra.haskell.syntax.TypeSignature]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.TypeSignature"), hydra.core.Name("type")))), x.value))))

def type_signature_with_name(original: hydra.phantoms.TTerm[hydra.haskell.syntax.TypeSignature], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.TypeSignature"), (hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.TypeSignature"), hydra.core.Name("type")))), original.value)))))))))

def type_signature_with_type(original: hydra.phantoms.TTerm[hydra.haskell.syntax.TypeSignature], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.TypeSignature"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.TypeSignature"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("type"), new_val.value))))))

def type_tuple(x: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.Type]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Type"), hydra.core.Field(hydra.core.Name("tuple"), x.value)))))

def type_variable(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.Type"), hydra.core.Field(hydra.core.Name("variable"), x.value)))))

def typed_binding(type_signature: hydra.phantoms.TTerm[hydra.haskell.syntax.TypeSignature], value_binding: hydra.phantoms.TTerm[hydra.haskell.syntax.ValueBinding]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.TypedBinding"), (hydra.core.Field(hydra.core.Name("typeSignature"), type_signature.value), hydra.core.Field(hydra.core.Name("valueBinding"), value_binding.value))))))

def typed_binding_type_signature(x: hydra.phantoms.TTerm[hydra.haskell.syntax.TypedBinding]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.TypedBinding"), hydra.core.Name("typeSignature")))), x.value))))

def typed_binding_value_binding(x: hydra.phantoms.TTerm[hydra.haskell.syntax.TypedBinding]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.TypedBinding"), hydra.core.Name("valueBinding")))), x.value))))

def typed_binding_with_type_signature(original: hydra.phantoms.TTerm[hydra.haskell.syntax.TypedBinding], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.TypeSignature]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.TypedBinding"), (hydra.core.Field(hydra.core.Name("typeSignature"), new_val.value), hydra.core.Field(hydra.core.Name("valueBinding"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.TypedBinding"), hydra.core.Name("valueBinding")))), original.value)))))))))

def typed_binding_with_value_binding(original: hydra.phantoms.TTerm[hydra.haskell.syntax.TypedBinding], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.ValueBinding]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.TypedBinding"), (hydra.core.Field(hydra.core.Name("typeSignature"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.TypedBinding"), hydra.core.Name("typeSignature")))), original.value)))), hydra.core.Field(hydra.core.Name("valueBinding"), new_val.value))))))

def typed_pattern(inner: hydra.phantoms.TTerm[hydra.haskell.syntax.Pattern], type: hydra.phantoms.TTerm[hydra.haskell.syntax.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.TypedPattern"), (hydra.core.Field(hydra.core.Name("inner"), inner.value), hydra.core.Field(hydra.core.Name("type"), type.value))))))

def typed_pattern_inner(x: hydra.phantoms.TTerm[hydra.haskell.syntax.TypedPattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.TypedPattern"), hydra.core.Name("inner")))), x.value))))

def typed_pattern_type(x: hydra.phantoms.TTerm[hydra.haskell.syntax.TypedPattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.TypedPattern"), hydra.core.Name("type")))), x.value))))

def typed_pattern_with_inner(original: hydra.phantoms.TTerm[hydra.haskell.syntax.TypedPattern], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Pattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.TypedPattern"), (hydra.core.Field(hydra.core.Name("inner"), new_val.value), hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.TypedPattern"), hydra.core.Name("type")))), original.value)))))))))

def typed_pattern_with_type(original: hydra.phantoms.TTerm[hydra.haskell.syntax.TypedPattern], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.TypedPattern"), (hydra.core.Field(hydra.core.Name("inner"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.TypedPattern"), hydra.core.Name("inner")))), original.value)))), hydra.core.Field(hydra.core.Name("type"), new_val.value))))))

def un_case_rhs(x: hydra.phantoms.TTerm[hydra.haskell.syntax.CaseRhs]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.haskell.syntax.CaseRhs"))), x.value))))

def un_deriving(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Deriving]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.haskell.syntax.Deriving"))), x.value))))

def un_local_bindings(x: hydra.phantoms.TTerm[hydra.haskell.syntax.LocalBindings]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.haskell.syntax.LocalBindings"))), x.value))))

def un_module_name(x: hydra.phantoms.TTerm[hydra.haskell.syntax.ModuleName]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.haskell.syntax.ModuleName"))), x.value))))

def un_name_part(x: hydra.phantoms.TTerm[hydra.haskell.syntax.NamePart]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.haskell.syntax.NamePart"))), x.value))))

def un_right_hand_side(x: hydra.phantoms.TTerm[hydra.haskell.syntax.RightHandSide]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.haskell.syntax.RightHandSide"))), x.value))))

def un_statement(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Statement]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.haskell.syntax.Statement"))), x.value))))

def un_variable(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Variable]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.haskell.syntax.Variable"))), x.value))))

def update_record_expression(inner: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression], fields: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.FieldUpdate]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.UpdateRecordExpression"), (hydra.core.Field(hydra.core.Name("inner"), inner.value), hydra.core.Field(hydra.core.Name("fields"), fields.value))))))

def update_record_expression_fields(x: hydra.phantoms.TTerm[hydra.haskell.syntax.UpdateRecordExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.UpdateRecordExpression"), hydra.core.Name("fields")))), x.value))))

def update_record_expression_inner(x: hydra.phantoms.TTerm[hydra.haskell.syntax.UpdateRecordExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.UpdateRecordExpression"), hydra.core.Name("inner")))), x.value))))

def update_record_expression_with_fields(original: hydra.phantoms.TTerm[hydra.haskell.syntax.UpdateRecordExpression], new_val: hydra.phantoms.TTerm[frozenlist[hydra.haskell.syntax.FieldUpdate]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.UpdateRecordExpression"), (hydra.core.Field(hydra.core.Name("inner"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.UpdateRecordExpression"), hydra.core.Name("inner")))), original.value)))), hydra.core.Field(hydra.core.Name("fields"), new_val.value))))))

def update_record_expression_with_inner(original: hydra.phantoms.TTerm[hydra.haskell.syntax.UpdateRecordExpression], new_val: hydra.phantoms.TTerm[hydra.haskell.syntax.Expression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.haskell.syntax.UpdateRecordExpression"), (hydra.core.Field(hydra.core.Name("inner"), new_val.value), hydra.core.Field(hydra.core.Name("fields"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.haskell.syntax.UpdateRecordExpression"), hydra.core.Name("fields")))), original.value)))))))))

def value_binding_simple(x: hydra.phantoms.TTerm[hydra.haskell.syntax.SimpleValueBinding]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.haskell.syntax.ValueBinding"), hydra.core.Field(hydra.core.Name("simple"), x.value)))))

def variable(x: hydra.phantoms.TTerm[hydra.haskell.syntax.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.haskell.syntax.Variable"), x.value))))
