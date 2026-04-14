# Note: this is an automatically generated file. Do not edit.

r"""Annotation and type stripping and normalization."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Nothing
from typing import TypeVar, cast
import hydra.core
import hydra.lib.lists
import hydra.rewriting

T0 = TypeVar("T0")

def deannotate_and_detype_term(t: hydra.core.Term) -> hydra.core.Term:
    r"""Strip type annotations from the top levels of a term."""

    while True:
        match t:
            case hydra.core.TermAnnotated(value=at):
                t = at.body
                continue

            case hydra.core.TermTypeApplication(value=tt):
                t = tt.body
                continue

            case hydra.core.TermTypeLambda(value=ta):
                t = ta.body
                continue

            case _:
                return t

def deannotate_term(t: hydra.core.Term) -> hydra.core.Term:
    r"""Strip all annotations (including System F type annotations) from the top levels of a term."""

    while True:
        match t:
            case hydra.core.TermAnnotated(value=at):
                t = at.body
                continue

            case _:
                return t

def deannotate_type(t: hydra.core.Type) -> hydra.core.Type:
    r"""Strip all annotations from a term."""

    while True:
        match t:
            case hydra.core.TypeAnnotated(value=arg_):
                t = arg_.body
                continue

            case _:
                return t

def deannotate_type_parameters(t: hydra.core.Type) -> hydra.core.Type:
    r"""Strip any top-level type lambdas from a type, extracting the (possibly nested) type body."""

    while True:
        match deannotate_type(t):
            case hydra.core.TypeForall(value=lt):
                t = lt.body
                continue

            case _:
                return t

def deannotate_type_recursive(typ: hydra.core.Type) -> hydra.core.Type:
    r"""Recursively strip all annotations from a type."""

    def strip(recurse: Callable[[T0], hydra.core.Type], typ2: T0):
        @lru_cache(1)
        def rewritten() -> hydra.core.Type:
            return recurse(typ2)
        def _hoist_rewritten_body_1(v1):
            match v1:
                case hydra.core.TypeAnnotated(value=at):
                    return at.body

                case _:
                    return rewritten()
        return _hoist_rewritten_body_1(rewritten())
    return hydra.rewriting.rewrite_type((lambda x1, x2: strip(x1, x2)), typ)

def deannotate_type_scheme_recursive(ts: hydra.core.TypeScheme) -> hydra.core.TypeScheme:
    r"""Recursively strip all annotations from a type scheme."""

    vars = ts.variables
    typ = ts.type
    constraints = ts.constraints
    return hydra.core.TypeScheme(vars, deannotate_type_recursive(typ), constraints)

def detype_term(t: hydra.core.Term) -> hydra.core.Term:
    r"""Strip System F type annotations from the top levels of a term, but leave application-specific annotations intact."""

    match t:
        case hydra.core.TermAnnotated(value=at):
            subj = at.body
            ann = at.annotation
            return cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(detype_term(subj), ann)))

        case hydra.core.TermTypeApplication(value=tt):
            return deannotate_and_detype_term(tt.body)

        case hydra.core.TermTypeLambda(value=ta):
            return deannotate_and_detype_term(ta.body)

        case _:
            return t

def remove_term_annotations(term: hydra.core.Term) -> hydra.core.Term:
    r"""Recursively remove term annotations, including within subterms."""

    def remove(recurse: Callable[[hydra.core.Term], hydra.core.Term], term2: hydra.core.Term) -> hydra.core.Term:
        @lru_cache(1)
        def rewritten() -> hydra.core.Term:
            return recurse(term2)
        match term2:
            case hydra.core.TermAnnotated(value=at):
                return at.body

            case _:
                return rewritten()
    return hydra.rewriting.rewrite_term((lambda x1, x2: remove(x1, x2)), term)

def remove_type_annotations(typ: hydra.core.Type) -> hydra.core.Type:
    r"""Recursively remove type annotations, including within subtypes."""

    def remove(recurse: Callable[[T0], hydra.core.Type], typ2: T0):
        @lru_cache(1)
        def rewritten() -> hydra.core.Type:
            return recurse(typ2)
        def _hoist_rewritten_body_1(v1):
            match v1:
                case hydra.core.TypeAnnotated(value=at):
                    return at.body

                case _:
                    return rewritten()
        return _hoist_rewritten_body_1(rewritten())
    return hydra.rewriting.rewrite_type((lambda x1, x2: remove(x1, x2)), typ)

def remove_type_annotations_from_term(term: hydra.core.Term) -> hydra.core.Term:
    r"""Strip type annotations (TypeLambda, TypeApplication, binding type schemes) from terms while preserving lambda domain types and other annotations."""

    def strip(recurse: Callable[[T0], hydra.core.Term], term2: T0):
        @lru_cache(1)
        def rewritten() -> hydra.core.Term:
            return recurse(term2)
        def strip_binding(b: hydra.core.Binding) -> hydra.core.Binding:
            return hydra.core.Binding(b.name, b.term, Nothing())
        def _hoist_strip_binding_body_1(v1):
            match v1:
                case hydra.core.TermLet(value=lt):
                    return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.map((lambda x1: strip_binding(x1)), lt.bindings), lt.body)))

                case hydra.core.TermTypeApplication(value=tt):
                    return tt.body

                case hydra.core.TermTypeLambda(value=ta):
                    return ta.body

                case _:
                    return rewritten()
        return _hoist_strip_binding_body_1(rewritten())
    return hydra.rewriting.rewrite_term((lambda x1, x2: strip(x1, x2)), term)

def remove_types_from_term(term: hydra.core.Term) -> hydra.core.Term:
    r"""Strip type annotations from terms while preserving other annotations."""

    def strip(recurse: Callable[[T0], hydra.core.Term], term2: T0):
        @lru_cache(1)
        def rewritten() -> hydra.core.Term:
            return recurse(term2)
        def strip_binding(b: hydra.core.Binding) -> hydra.core.Binding:
            return hydra.core.Binding(b.name, b.term, Nothing())
        def _hoist_strip_binding_body_1(v1):
            match v1:
                case hydra.core.TermLambda(value=l):
                    return cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(l.parameter, Nothing(), l.body)))

                case hydra.core.TermLet(value=lt):
                    return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.map((lambda x1: strip_binding(x1)), lt.bindings), lt.body)))

                case hydra.core.TermTypeApplication(value=tt):
                    return tt.body

                case hydra.core.TermTypeLambda(value=ta):
                    return ta.body

                case _:
                    return rewritten()
        return _hoist_strip_binding_body_1(rewritten())
    return hydra.rewriting.rewrite_term((lambda x1, x2: strip(x1, x2)), term)

def strip_type_lambdas(t: hydra.core.Term) -> hydra.core.Term:
    r"""Strip outer type lambda wrappers from a term, preserving type application wrappers and annotations."""

    match t:
        case hydra.core.TermAnnotated(value=at):
            subj = at.body
            ann = at.annotation
            return cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(strip_type_lambdas(subj), ann)))

        case hydra.core.TermTypeLambda(value=ta):
            return strip_type_lambdas(ta.body)

        case _:
            return t
