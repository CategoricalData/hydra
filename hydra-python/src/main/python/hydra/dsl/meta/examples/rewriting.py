"""Partial implementation of hydra.rewriting using the new Python phantom-typed DSLs.

This demonstrates how Hydra kernel source code looks in Python, using the meta DSLs
(phantoms, core, and library wrappers). Compare with the Haskell original at:
  hydra-haskell/src/main/haskell/Hydra/Sources/Kernel/Terms/Rewriting.hs

The functions chosen range from simple (deannotate_term) to complex (free_variables_in_term,
replace_free_term_variable) to showcase the DSL's expressiveness.

Note: Recursive self-references use var("hydra.rewriting.functionName") because Python
module-level variables can't reference themselves during construction, unlike Haskell's
lazy TBinding references.
"""

import hydra.module
import hydra.dsl.meta.core as Core
from hydra.dsl.meta.phantoms import (
    cases, compose, constant, definition_in_namespace, false, field, fold,
    inject_unit, lam, lams, list_, make_local, match, project, to_binding,
    true, var,
)
import hydra.dsl.meta.lib.equality as Equality
import hydra.dsl.meta.lib.lists as Lists
import hydra.dsl.meta.lib.logic as Logic
import hydra.dsl.meta.lib.sets as Sets
from hydra.coders import TraversalOrder
from hydra.core import (
    AnnotatedType, Binding, Function, Let, Name, Term, Type, TypeScheme,
)
from hydra.dsl.python import Just, Nothing
from hydra.phantoms import TBinding, TTerm

from typing import Callable, TypeVar

X = TypeVar("X")


# =============================================================================
# Namespace and binding helpers
# =============================================================================

ns = hydra.module.Namespace("hydra.rewriting")


def define(lname: str, term: TTerm | None = None):
    """Create a definition in the hydra.rewriting namespace."""
    return definition_in_namespace(ns, lname, term)


local = make_local(ns)


# =============================================================================
# TraversalOrder constructors
# =============================================================================

def traversal_order_pre() -> TTerm:
    """The pre-order TraversalOrder variant."""
    return inject_unit(TraversalOrder.TYPE_,
                       TraversalOrder.PRE.value)


def traversal_order_post() -> TTerm:
    """The post-order TraversalOrder variant."""
    return inject_unit(TraversalOrder.TYPE_,
                       TraversalOrder.POST.value)


# =============================================================================
# Simple deannotation functions
# =============================================================================

deannotate_term: TBinding[Callable[[Term], Term]] = (
    define("deannotateTerm")
        .doc("Strip all annotations from the top levels of a term")
        .lam("t")
        .to(cases(Term.TYPE_, var("t"),
            Just(var("t")), [
            Term.ANNOTATED >>
                lam("at", local("deannotateTerm")
                    @ Core.annotated_term_body(var("at")))])))

deannotate_and_detype_term: TBinding[Callable[[Term], Term]] = (
    define("deannotateAndDetypeTerm")
        .doc("Strip type annotations from the top levels of a term")
        .lam("t")
        .to(cases(Term.TYPE_, var("t"),
            Just(var("t")), [
            Term.ANNOTATED >>
                lam("at", local("deannotateAndDetypeTerm")
                    @ Core.annotated_term_body(var("at"))),
            Term.TYPE_APPLICATION >>
                lam("tt", local("deannotateAndDetypeTerm")
                    @ Core.type_application_term_body(var("tt"))),
            Term.TYPE_LAMBDA >>
                lam("ta", local("deannotateAndDetypeTerm")
                    @ Core.type_lambda_body(var("ta")))])))

deannotate_type: TBinding[Callable[[Type], Type]] = (
    define("deannotateType")
        .doc("Strip all annotations from a type")
        .lam("t")
        .to(cases(Type.TYPE_, var("t"),
            Just(var("t")), [
            Type.ANNOTATED >>
                compose(local("deannotateType"),
                    project(AnnotatedType.TYPE_,
                        AnnotatedType.BODY))])))


# =============================================================================
# Deannotation with let-bindings and composition
# =============================================================================

deannotate_type_recursive: TBinding[Callable[[Type], Type]] = (
    define("deannotateTypeRecursive")
        .doc("Recursively strip all annotations from a type")
        .lam("typ")
        .let("strip",
            lams("recurse", "typ")
                .let("rewritten", var("recurse") @ var("typ"))
                .to(cases(Type.TYPE_, var("rewritten"),
                    Just(var("rewritten")), [
                    Type.ANNOTATED >>
                        lam("at", Core.annotated_type_body(var("at")))])))
        .to(local("rewriteType") @ var("strip") @ var("typ")))

deannotate_type_scheme_recursive: TBinding[Callable[[TypeScheme], TypeScheme]] = (
    define("deannotateTypeSchemeRecursive")
        .doc("Recursively strip all annotations from a type scheme")
        .lam("ts")
        .let("vars", Core.type_scheme_variables(var("ts")))
        .let("typ", Core.type_scheme_type(var("ts")))
        .let("constraints", Core.type_scheme_constraints(var("ts")))
        .to(Core.type_scheme(var("vars"),
            deannotate_type_recursive @ var("typ"),
            var("constraints"))))


# =============================================================================
# detypeTerm — preserving some annotations while stripping others
# =============================================================================

detype_term: TBinding[Callable[[Term], Term]] = (
    define("detypeTerm")
        .doc("Strip System F type annotations but leave application-specific annotations intact")
        .lam("t")
        .to(cases(Term.TYPE_, var("t"),
            Just(var("t")), [
            Term.ANNOTATED >>
                (lam("at")
                    .let("subj", Core.annotated_term_body(var("at")))
                    .let("ann", Core.annotated_term_annotation(var("at")))
                    .to(Core.term_annotated(Core.annotated_term(
                        local("detypeTerm") @ var("subj"),
                        var("ann"))))),
            Term.TYPE_APPLICATION >>
                lam("tt", deannotate_and_detype_term
                    @ Core.type_application_term_body(var("tt"))),
            Term.TYPE_LAMBDA >>
                lam("ta", deannotate_and_detype_term
                    @ Core.type_lambda_body(var("ta")))])))


# =============================================================================
# mapBeneathTypeAnnotations — recursive transformation
# =============================================================================

map_beneath_type_annotations: TBinding[Callable[[Callable[[Type], Type]], Callable[[Type], Type]]] = (
    define("mapBeneathTypeAnnotations")
        .doc("Apply a transformation to the first type beneath a chain of annotations")
        .lams("f", "t")
        .to(cases(Type.TYPE_, var("t"),
            Just(var("f") @ var("t")), [
            Type.ANNOTATED >>
                lam("at", Core.type_annotated(Core.annotated_type(
                    local("mapBeneathTypeAnnotations") @ var("f")
                        @ Core.annotated_type_body(var("at")),
                    Core.annotated_type_annotation(var("at")))))])))


# =============================================================================
# isLambda — nested pattern matching
# =============================================================================

is_lambda: TBinding[Callable[[Term], bool]] = (
    define("isLambda")
        .doc("Check whether a term is a lambda, possibly nested within let/annotation terms")
        .lam("term")
        .to(cases(Term.TYPE_,
            deannotate_term @ var("term"),
            Just(false()), [
            Term.FUNCTION >>
                match(Function.TYPE_,
                    Just(false()), [
                    Function.LAMBDA >>
                        constant(true())]),
            Term.LET >>
                lam("lt", local("isLambda")
                    @ (project(Let.TYPE_,
                        Let.BODY) @ var("lt")))])))


# =============================================================================
# Free variable analysis
# =============================================================================

free_variables_in_term: TBinding[Callable[[Term], set[Name]]] = (
    define("freeVariablesInTerm")
        .doc("Find the free variables (not bound by a lambda or let) in a term")
        .lam("term")
        .let("dfltVars",
            Lists.foldl(
                lams("s", "t")
                    .to(Sets.union(var("s"),
                        local("freeVariablesInTerm") @ var("t"))),
                Sets.empty(),
                local("subterms") @ var("term")))
        .to(cases(Term.TYPE_, var("term"),
            Just(var("dfltVars")), [
            Term.FUNCTION >>
                match(Function.TYPE_,
                    Just(var("dfltVars")), [
                    Function.LAMBDA >>
                        lam("l", Sets.delete(
                            Core.lambda_parameter(var("l")),
                            local("freeVariablesInTerm")
                                @ Core.lambda_body(var("l"))))]),
            Term.LET >>
                lam("l", Sets.difference(
                    var("dfltVars"),
                    Sets.from_list(Lists.map(
                        project(Binding.TYPE_,
                            Binding.NAME),
                        Core.let_bindings(var("l")))))),
            Term.VARIABLE >>
                lam("v", Sets.singleton(var("v")))])))

free_variables_in_type: TBinding[Callable[[Type], set[Name]]] = (
    define("freeVariablesInType")
        .doc("Find the free variables (not bound by forall) in a type")
        .lam("typ")
        .let("dfltVars",
            fold(lams("s", "t")
                    .to(Sets.union(var("s"),
                        local("freeVariablesInType") @ var("t"))))
                @ Sets.empty()
                @ (local("subtypes") @ var("typ")))
        .to(cases(Type.TYPE_, var("typ"),
            Just(var("dfltVars")), [
            Type.FORALL >>
                lam("lt", Sets.delete(
                    Core.forall_type_parameter(var("lt")),
                    local("freeVariablesInType")
                        @ Core.forall_type_body(var("lt")))),
            Type.VARIABLE >>
                lam("v", Sets.singleton(var("v")))])))

free_variables_in_type_scheme: TBinding[Callable[[TypeScheme], set[Name]]] = (
    define("freeVariablesInTypeScheme")
        .doc("Find free variables in a type scheme")
        .lam("ts")
        .let("vars", Core.type_scheme_variables(var("ts")))
        .let("t", Core.type_scheme_type(var("ts")))
        .to(Sets.difference(
            free_variables_in_type @ var("t"),
            Sets.from_list(var("vars")))))

is_free_variable_in_term: TBinding[Callable[[Name], Callable[[Term], bool]]] = (
    define("isFreeVariableInTerm")
        .doc("Check whether a variable is free (not bound) in a term")
        .lams("v", "term")
        .to(Logic.not_(Sets.member(var("v"),
            free_variables_in_term @ var("term")))))


# =============================================================================
# Recursive rewriting with binding awareness
# =============================================================================

replace_free_term_variable: TBinding[Callable[[Name], Callable[[Term], Callable[[Term], Term]]]] = (
    define("replaceFreeTermVariable")
        .doc("Replace free occurrences of a variable name with a new term")
        .lams("vold", "tnew", "term")
        .let("rewrite",
            lams("recurse", "t")
                .to(cases(Term.TYPE_, var("t"),
                    Just(var("recurse") @ var("t")), [
                    Term.FUNCTION >>
                        lam("f", cases(Function.TYPE_, var("f"),
                            Just(var("recurse") @ var("t")), [
                            Function.LAMBDA >>
                                (lam("l")
                                    .let("v", Core.lambda_parameter(var("l")))
                                    .to(Logic.if_else(
                                        Equality.equal(var("v"), var("vold")),
                                        var("t"),
                                        var("recurse") @ var("t"))))])),
                    Term.VARIABLE >>
                        lam("v", Logic.if_else(
                            Equality.equal(var("v"), var("vold")),
                            var("tnew"),
                            Core.term_variable(var("v"))))])))
        .to(local("rewriteTerm") @ var("rewrite") @ var("term")))

replace_free_type_variable: TBinding[Callable[[Name], Callable[[Type], Callable[[Type], Type]]]] = (
    define("replaceFreeTypeVariable")
        .doc("Replace free occurrences of a name in a type")
        .lams("v", "rep", "typ")
        .let("mapExpr",
            lams("recurse", "t")
                .to(cases(Type.TYPE_, var("t"),
                    Just(var("recurse") @ var("t")), [
                    Type.FORALL >>
                        lam("ft", Logic.if_else(
                            Equality.equal(var("v"), Core.forall_type_parameter(var("ft"))),
                            var("t"),
                            Core.type_forall(Core.forall_type(
                                Core.forall_type_parameter(var("ft")),
                                var("recurse") @ Core.forall_type_body(var("ft")))))),
                    Type.VARIABLE >>
                        lam("v2", Logic.if_else(
                            Equality.equal(var("v"), var("v2")),
                            var("rep"),
                            var("t")))])))
        .to(local("rewriteType") @ var("mapExpr") @ var("typ")))


# =============================================================================
# Recursive annotation removal
# =============================================================================

remove_term_annotations: TBinding[Callable[[Term], Term]] = (
    define("removeTermAnnotations")
        .doc("Recursively remove term annotations, including within subterms")
        .lam("term")
        .let("remove",
            lams("recurse", "term")
                .let("rewritten", var("recurse") @ var("term"))
                .to(cases(Term.TYPE_, var("term"),
                    Just(var("rewritten")), [
                    Term.ANNOTATED >>
                        lam("at", Core.annotated_term_body(var("at")))])))
        .to(local("rewriteTerm") @ var("remove") @ var("term")))

remove_type_annotations: TBinding[Callable[[Type], Type]] = (
    define("removeTypeAnnotations")
        .doc("Recursively remove type annotations, including within subtypes")
        .lam("typ")
        .let("remove",
            lams("recurse", "typ")
                .let("rewritten", var("recurse") @ var("typ"))
                .to(cases(Type.TYPE_, var("rewritten"),
                    Just(var("rewritten")), [
                    Type.ANNOTATED >>
                        lam("at", Core.annotated_type_body(var("at")))])))
        .to(local("rewriteType") @ var("remove") @ var("typ")))


# =============================================================================
# Fold operations
# =============================================================================

fold_over_term: TBinding[Callable[[TraversalOrder], Callable[[Callable[[X, Term], X]], Callable[[X], Callable[[Term], X]]]]] = (
    define("foldOverTerm")
        .doc("Fold over a term, traversing its subterms in the specified order")
        .lams("order", "fld", "b0", "term")
        .to(cases(TraversalOrder.TYPE_, var("order"),
            Nothing(), [
            TraversalOrder.PRE >>
                constant(
                    fold(local("foldOverTerm") @ var("order") @ var("fld"))
                        @ (var("fld") @ var("b0") @ var("term"))
                        @ (local("subterms") @ var("term"))),
            TraversalOrder.POST >>
                constant(var("fld")
                    @ (fold(local("foldOverTerm") @ var("order") @ var("fld"))
                        @ var("b0")
                        @ (local("subterms") @ var("term")))
                    @ var("term"))])))

fold_over_type: TBinding[Callable[[TraversalOrder], Callable[[Callable[[X, Type], X]], Callable[[X], Callable[[Type], X]]]]] = (
    define("foldOverType")
        .doc("Fold over a type, traversing its subtypes in the specified order")
        .lams("order", "fld", "b0", "typ")
        .to(cases(TraversalOrder.TYPE_, var("order"),
            Nothing(), [
            TraversalOrder.PRE >>
                constant(
                    fold(local("foldOverType") @ var("order") @ var("fld"))
                        @ (var("fld") @ var("b0") @ var("typ"))
                        @ (local("subtypes") @ var("typ"))),
            TraversalOrder.POST >>
                constant(var("fld")
                    @ (fold(local("foldOverType") @ var("order") @ var("fld"))
                        @ var("b0")
                        @ (local("subtypes") @ var("typ")))
                    @ var("typ"))])))


# =============================================================================
# Subtypes extraction (partial — showing the pattern)
# =============================================================================

subtypes: TBinding[Callable[[Type], list[Type]]] = (
    define("subtypes")
        .doc("Extract the immediate subtypes of a type")
        .lam("typ")
        .to(cases(Type.TYPE_, var("typ"),
            Just(list_([])), [
            Type.ANNOTATED >>
                lam("at", list_([Core.annotated_type_body(var("at"))])),
            Type.FUNCTION >>
                lam("ft", list_([
                    Core.function_type_domain(var("ft")),
                    Core.function_type_codomain(var("ft"))])),
            Type.FORALL >>
                lam("ft", list_([Core.forall_type_body(var("ft"))])),
            Type.LIST >>
                lam("t", list_([var("t")])),
            Type.SET >>
                lam("t", list_([var("t")])),
            Type.MAYBE >>
                lam("t", list_([var("t")])),
            Type.MAP >>
                lam("mt", list_([
                    Core.map_type_keys(var("mt")),
                    Core.map_type_values(var("mt"))])),
            Type.EITHER >>
                lam("et", list_([
                    Core.either_type_left(var("et")),
                    Core.either_type_right(var("et"))])),
            Type.PAIR >>
                lam("pt", list_([
                    Core.pair_type_first(var("pt")),
                    Core.pair_type_second(var("pt"))]))])))


# =============================================================================
# Module definition
# =============================================================================

elements = [
    to_binding(deannotate_term),
    to_binding(deannotate_and_detype_term),
    to_binding(deannotate_type),
    to_binding(deannotate_type_recursive),
    to_binding(deannotate_type_scheme_recursive),
    to_binding(detype_term),
    to_binding(fold_over_term),
    to_binding(fold_over_type),
    to_binding(free_variables_in_term),
    to_binding(free_variables_in_type),
    to_binding(free_variables_in_type_scheme),
    to_binding(is_free_variable_in_term),
    to_binding(is_lambda),
    to_binding(map_beneath_type_annotations),
    to_binding(remove_term_annotations),
    to_binding(remove_type_annotations),
    to_binding(replace_free_term_variable),
    to_binding(replace_free_type_variable),
    to_binding(subtypes)]

module = hydra.module.Module(
    ns,
    tuple(elements),
    (),
    (),
    Just("Utilities for type and term rewriting and analysis."))
