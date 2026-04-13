# Note: this is an automatically generated file. Do not edit.

r"""Type checking and type reconstruction (type-of) for the results of Hydra unification and inference."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.coders
import hydra.constants
import hydra.context
import hydra.core
import hydra.dependencies
import hydra.error.checking
import hydra.error.core
import hydra.errors
import hydra.extract.core
import hydra.formatting
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.names
import hydra.paths
import hydra.reflect
import hydra.resolution
import hydra.rewriting
import hydra.scoping
import hydra.show.core
import hydra.strip
import hydra.substitution
import hydra.typing
import hydra.variables

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def all_equal(els: frozenlist[T0]) -> bool:
    return hydra.lib.logic.if_else(hydra.lib.lists.null(els), (lambda : True), (lambda : hydra.lib.lists.foldl((lambda b, t: hydra.lib.logic.and_(b, hydra.lib.equality.equal(t, hydra.lib.lists.head(els)))), True, hydra.lib.lists.tail(els))))

def apply_type_arguments_to_type(cx: T0, tx: hydra.graph.Graph, type_args: frozenlist[hydra.core.Type], t: hydra.core.Type) -> Either[hydra.errors.Error, hydra.core.Type]:
    r"""Apply type arguments to a type, substituting forall-bound variables."""

    return hydra.lib.logic.if_else(hydra.lib.lists.null(type_args), (lambda : Right(t)), (lambda : (nonnull := (_hoist_nonnull_1 := (lambda v1: (lambda ft: (v := ft.parameter, tbody := ft.body, apply_type_arguments_to_type(cx, tx, hydra.lib.lists.tail(type_args), hydra.substitution.subst_in_type(hydra.typing.TypeSubst(hydra.lib.maps.singleton(v, hydra.lib.lists.head(type_args))), tbody)))[2])(v1.value) if isinstance(v1, hydra.core.TypeForall) else Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("forall type", hydra.lib.strings.cat((hydra.show.core.type(t), ". Trying to apply ", hydra.lib.literals.show_int32(hydra.lib.lists.length(type_args)), " type args: ", hydra.formatting.show_list((lambda x1: hydra.show.core.type(x1)), type_args), ". Context has vars: {", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map((lambda v1: v1.value), hydra.lib.maps.keys(tx.bound_types))), "}"))))))))), _hoist_nonnull_1(t))[1], nonnull)[1]))

def check_for_unbound_type_variables(cx: T0, tx: hydra.graph.Graph, term0: hydra.core.Term) -> Either[hydra.errors.Error, None]:
    r"""Check that a term has no unbound type variables (Either version)."""

    @lru_cache(1)
    def svars() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(hydra.lib.maps.keys(tx.schema_types))
    def check_recursive(vars: frozenset[hydra.core.Name], trace: frozenlist[str], lbinding: Maybe[hydra.core.Binding], term: hydra.core.Term) -> Either[hydra.errors.Error, None]:
        def recurse(v1: hydra.core.Term) -> Either[hydra.errors.Error, None]:
            return check_recursive(vars, trace, lbinding, v1)
        @lru_cache(1)
        def dflt() -> Either[hydra.errors.Error, None]:
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: recurse(x1)), hydra.rewriting.subterms(term)), (lambda _: Right(None)))
        def check(typ: hydra.core.Type) -> Either[hydra.errors.Error, None]:
            @lru_cache(1)
            def freevars() -> frozenset[hydra.core.Name]:
                return hydra.variables.free_variables_in_type(typ)
            @lru_cache(1)
            def badvars() -> frozenset[hydra.core.Name]:
                return hydra.lib.sets.difference(hydra.lib.sets.difference(freevars(), vars), svars())
            return hydra.lib.logic.if_else(hydra.lib.sets.null(badvars()), (lambda : Right(None)), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorChecking(cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorUnboundTypeVariables(hydra.error.checking.UnboundTypeVariablesError(badvars(), typ))))))))
        def check_optional(m: Maybe[hydra.core.Type]) -> Either[hydra.errors.Error, None]:
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_maybe((lambda x1: check(x1)), m), (lambda _: Right(None)))
        match term:
            case hydra.core.TermLambda(value=l):
                return hydra.lib.eithers.bind(check_optional(l.domain), (lambda _: recurse(l.body)))

            case hydra.core.TermLet(value=l2):
                def for_binding(b: hydra.core.Binding) -> Either[hydra.errors.Error, None]:
                    bterm = b.term
                    @lru_cache(1)
                    def new_vars() -> frozenset[hydra.core.Name]:
                        return hydra.lib.maybes.maybe((lambda : vars), (lambda ts: hydra.lib.sets.union(vars, hydra.lib.sets.from_list(ts.variables))), b.type)
                    @lru_cache(1)
                    def new_trace() -> frozenlist[str]:
                        return hydra.lib.lists.cons(b.name.value, trace)
                    return check_recursive(new_vars(), new_trace(), Just(b), bterm)
                return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: for_binding(x1)), l2.bindings), (lambda _: recurse(l2.body)))

            case hydra.core.TermTypeApplication(value=tt):
                return hydra.lib.eithers.bind(check(tt.type), (lambda _: recurse(tt.body)))

            case hydra.core.TermTypeLambda(value=tl):
                return hydra.lib.eithers.bind(check(cast(hydra.core.Type, hydra.core.TypeVariable(tl.parameter))), (lambda _: recurse(tl.body)))

            case _:
                return dflt()
    return check_recursive(hydra.lib.sets.empty(), ("top level",), Nothing(), term0)

def check_nominal_application(cx: hydra.context.Context, tx: hydra.graph.Graph, tname: hydra.core.Name, type_args: frozenlist[hydra.core.Type]) -> Either[hydra.errors.Error, tuple[None, hydra.context.Context]]:
    r"""Check that a nominal type is applied to the correct number of type arguments (Either version)."""

    return hydra.lib.eithers.bind(hydra.resolution.require_schema_type(cx, tx.schema_types, tname), (lambda result: (schema_type := hydra.lib.pairs.first(result), cx2 := hydra.lib.pairs.second(result), vars := schema_type.variables, varslen := hydra.lib.lists.length(vars), argslen := hydra.lib.lists.length(type_args), hydra.lib.logic.if_else(hydra.lib.equality.equal(varslen, argslen), (lambda : Right((None, cx2))), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorChecking(cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorTypeArityMismatch(hydra.error.checking.TypeArityMismatchError(cast(hydra.core.Type, hydra.core.TypeVariable(tname)), varslen, argslen, type_args)))))))))[5]))

def normalize_type_free_vars(typ: hydra.core.Type) -> hydra.core.Type:
    r"""Normalize free type variables in a type to canonical names based on order of first occurrence. This allows comparing types that differ only in the naming of free type variables."""

    def collect_vars(acc: FrozenDict[hydra.core.Name, hydra.core.Name], t: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        match t:
            case hydra.core.TypeVariable(value=v):
                return hydra.lib.logic.if_else(hydra.lib.maps.member(v, acc), (lambda : acc), (lambda : hydra.lib.maps.insert(v, hydra.core.Name(hydra.lib.strings.cat2("_tv", hydra.lib.literals.show_int32(hydra.lib.maps.size(acc)))), acc)))

            case _:
                return acc
    @lru_cache(1)
    def subst() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        return hydra.rewriting.fold_over_type(hydra.coders.TraversalOrder.PRE, (lambda x1, x2: collect_vars(x1, x2)), hydra.lib.maps.empty(), typ)
    return hydra.variables.substitute_type_variables(subst(), typ)

def types_all_effectively_equal(tx: hydra.graph.Graph, tlist: frozenlist[hydra.core.Type]) -> bool:
    r"""Check whether a list of types are effectively equal, disregarding type aliases and free type variable naming. Also treats free type variables (not in schema) as wildcards, since inference has already verified consistency."""

    types = tx.schema_types
    def contains_free_var(t: hydra.core.Type) -> bool:
        @lru_cache(1)
        def all_vars() -> frozenset[hydra.core.Name]:
            return hydra.variables.free_variables_in_type_simple(t)
        @lru_cache(1)
        def schema_names() -> frozenset[hydra.core.Name]:
            return hydra.lib.sets.from_list(hydra.lib.maps.keys(types))
        return hydra.lib.logic.not_(hydra.lib.sets.null(hydra.lib.sets.difference(all_vars(), schema_names())))
    @lru_cache(1)
    def any_contains_free_var() -> bool:
        return hydra.lib.lists.foldl((lambda acc, t: hydra.lib.logic.or_(acc, contains_free_var(t))), False, tlist)
    return hydra.lib.logic.if_else(any_contains_free_var(), (lambda : True), (lambda : hydra.lib.logic.if_else(all_equal(hydra.lib.lists.map((lambda t: normalize_type_free_vars(t)), tlist)), (lambda : True), (lambda : all_equal(hydra.lib.lists.map((lambda t: normalize_type_free_vars(hydra.strip.deannotate_type_recursive(hydra.dependencies.replace_typedefs(types, t)))), tlist))))))

def check_same_type(cx: T0, tx: hydra.graph.Graph, desc: str, types: frozenlist[hydra.core.Type]) -> Either[hydra.errors.Error, hydra.core.Type]:
    r"""Ensure all types in a list are equal and return the common type."""

    return hydra.lib.logic.if_else(types_all_effectively_equal(tx, types), (lambda : Right(hydra.lib.lists.head(types))), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorChecking(cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorUnequalTypes(hydra.error.checking.UnequalTypesError(types, desc))))))))

def contains_in_scope_type_vars(tx: hydra.graph.Graph, t: hydra.core.Type) -> bool:
    r"""Check if a type contains any type variable from the current scope."""

    vars = tx.type_variables
    @lru_cache(1)
    def free_vars() -> frozenset[hydra.core.Name]:
        return hydra.variables.free_variables_in_type_simple(t)
    return hydra.lib.logic.not_(hydra.lib.sets.null(hydra.lib.sets.intersection(vars, free_vars())))

def types_effectively_equal(tx: hydra.graph.Graph, t1: hydra.core.Type, t2: hydra.core.Type) -> bool:
    r"""Check whether two types are effectively equal, disregarding type aliases, forall quantifiers, and treating in-scope type variables as wildcards."""

    return hydra.lib.logic.or_(contains_in_scope_type_vars(tx, t1), hydra.lib.logic.or_(contains_in_scope_type_vars(tx, t2), types_all_effectively_equal(tx, (hydra.resolution.fully_strip_and_normalize_type(t1), hydra.resolution.fully_strip_and_normalize_type(t2)))))

def type_of_injection(cx: hydra.context.Context, tx: hydra.graph.Graph, type_args: frozenlist[hydra.core.Type], injection: hydra.core.Injection) -> Either[hydra.errors.Error, tuple[hydra.core.Type, hydra.context.Context]]:
    r"""Reconstruct the type of a union injection (Either/Context version)."""

    tname = injection.type_name
    field = injection.field
    fname = field.name
    fterm = field.term
    return hydra.lib.eithers.bind(hydra.resolution.require_schema_type(cx, tx.schema_types, tname), (lambda schema_result: (schema_type := hydra.lib.pairs.first(schema_result), cx2 := hydra.lib.pairs.second(schema_result), svars := schema_type.variables, sbody := schema_type.type, hydra.lib.eithers.bind(hydra.extract.core.union_type(tname, sbody), (lambda sfields: hydra.lib.eithers.bind(hydra.resolution.find_field_type(cx2, fname, sfields), (lambda ftyp: Right((hydra.resolution.nominal_application(tname, type_args), cx2)))))))[4]))

def type_of_literal(cx: T0, tx: hydra.graph.Graph, type_args: frozenlist[hydra.core.Type], lit: hydra.core.Literal) -> Either[hydra.errors.Error, tuple[hydra.core.Type, T0]]:
    r"""Reconstruct the type of a literal (Either/Context version)."""

    @lru_cache(1)
    def t() -> hydra.core.Type:
        return cast(hydra.core.Type, hydra.core.TypeLiteral(hydra.reflect.literal_type(lit)))
    return hydra.lib.eithers.bind(apply_type_arguments_to_type(cx, tx, type_args, t()), (lambda applied: Right((applied, cx))))

def type_of_projection(cx: hydra.context.Context, tx: hydra.graph.Graph, type_args: frozenlist[hydra.core.Type], p: hydra.core.Projection) -> Either[hydra.errors.Error, tuple[hydra.core.Type, hydra.context.Context]]:
    r"""Reconstruct the type of a record projection (Either/Context version)."""

    tname = p.type_name
    fname = p.field
    return hydra.lib.eithers.bind(hydra.resolution.require_schema_type(cx, tx.schema_types, tname), (lambda schema_result: (schema_type := hydra.lib.pairs.first(schema_result), cx2 := hydra.lib.pairs.second(schema_result), svars := schema_type.variables, sbody := schema_type.type, hydra.lib.eithers.bind(hydra.extract.core.record_type(tname, sbody), (lambda sfields: hydra.lib.eithers.bind(hydra.resolution.find_field_type(cx2, fname, sfields), (lambda ftyp: (subst := hydra.typing.TypeSubst(hydra.lib.maps.from_list(hydra.lib.lists.zip(svars, type_args))), sftyp := hydra.substitution.subst_in_type(subst, ftyp), Right((cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(hydra.resolution.nominal_application(tname, type_args), sftyp))), cx2)))[2])))))[4]))

def type_of_unit(cx: T0, tx: hydra.graph.Graph, type_args: frozenlist[hydra.core.Type]) -> Either[hydra.errors.Error, tuple[hydra.core.Type, T0]]:
    r"""Reconstruct the type of the unit term (Either/Context version)."""

    return hydra.lib.eithers.bind(apply_type_arguments_to_type(cx, tx, type_args, cast(hydra.core.Type, hydra.core.TypeUnit())), (lambda applied: Right((applied, cx))))

def type_of_unwrap(cx: hydra.context.Context, tx: hydra.graph.Graph, type_args: frozenlist[hydra.core.Type], tname: hydra.core.Name) -> Either[hydra.errors.Error, tuple[hydra.core.Type, hydra.context.Context]]:
    r"""Reconstruct the type of an unwrap operation (Either/Context version)."""

    return hydra.lib.eithers.bind(hydra.resolution.require_schema_type(cx, tx.schema_types, tname), (lambda schema_result: (schema_type := hydra.lib.pairs.first(schema_result), cx2 := hydra.lib.pairs.second(schema_result), svars := schema_type.variables, sbody := schema_type.type, hydra.lib.eithers.bind(hydra.extract.core.wrapped_type(tname, sbody), (lambda wrapped: (subst := hydra.typing.TypeSubst(hydra.lib.maps.from_list(hydra.lib.lists.zip(svars, type_args))), swrapped := hydra.substitution.subst_in_type(subst, wrapped), Right((cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(hydra.resolution.nominal_application(tname, type_args), swrapped))), cx2)))[2])))[4]))

def type_of_variable(cx: hydra.context.Context, tx: hydra.graph.Graph, type_args: frozenlist[hydra.core.Type], name: hydra.core.Name) -> Either[hydra.errors.Error, tuple[hydra.core.Type, hydra.context.Context]]:
    r"""Reconstruct the type of a variable (Either/Context version)."""

    @lru_cache(1)
    def raw_type_scheme() -> Maybe[hydra.core.TypeScheme]:
        return hydra.lib.maps.lookup(name, tx.bound_types)
    def for_scheme(ts: hydra.core.TypeScheme) -> Either[hydra.errors.Error, tuple[hydra.core.Type, hydra.context.Context]]:
        @lru_cache(1)
        def t_result() -> tuple[hydra.core.Type, hydra.context.Context]:
            return hydra.lib.logic.if_else(hydra.lib.lists.null(type_args), (lambda : hydra.resolution.instantiate_type(cx, hydra.scoping.type_scheme_to_f_type(ts))), (lambda : (hydra.scoping.type_scheme_to_f_type(ts), cx)))
        @lru_cache(1)
        def t() -> hydra.core.Type:
            return hydra.lib.pairs.first(t_result())
        @lru_cache(1)
        def cx2() -> hydra.context.Context:
            return hydra.lib.pairs.second(t_result())
        return hydra.lib.eithers.bind(apply_type_arguments_to_type(cx2(), tx, type_args, t()), (lambda applied: Right((applied, cx2()))))
    return hydra.lib.maybes.maybe((lambda : hydra.lib.maybes.maybe((lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorUntypedTermVariable(hydra.error.core.UntypedTermVariableError(hydra.paths.SubtermPath(()), name))))), (lambda x1: for_scheme(x1)), hydra.lib.maybes.map((lambda _p: _p.type), hydra.lib.maps.lookup(name, tx.primitives)))), (lambda x1: for_scheme(x1)), raw_type_scheme())

def type_of(cx: hydra.context.Context, tx: hydra.graph.Graph, type_args: frozenlist[hydra.core.Type], term: hydra.core.Term) -> Either[hydra.errors.Error, tuple[hydra.core.Type, hydra.context.Context]]:
    r"""Given a type context, reconstruct the type of a System F term."""

    @lru_cache(1)
    def cx1() -> hydra.context.Context:
        return hydra.context.Context(hydra.lib.lists.cons("typeOf", cx.trace), cx.messages, cx.other)
    match term:
        case hydra.core.TermAnnotated(value=v1):
            return type_of_annotated_term(cx1(), tx, type_args, v1)

        case hydra.core.TermApplication(value=v12):
            return type_of_application(cx1(), tx, type_args, v12)

        case hydra.core.TermCases(value=v13):
            return type_of_case_statement(cx1(), tx, type_args, v13)

        case hydra.core.TermEither(value=v14):
            return type_of_either(cx1(), tx, type_args, v14)

        case hydra.core.TermLambda(value=v15):
            return type_of_lambda(cx1(), tx, type_args, v15)

        case hydra.core.TermLet(value=v16):
            return type_of_let(cx1(), tx, type_args, v16)

        case hydra.core.TermList(value=v17):
            return type_of_list(cx1(), tx, type_args, v17)

        case hydra.core.TermLiteral(value=v18):
            return type_of_literal(cx1(), tx, type_args, v18)

        case hydra.core.TermMap(value=v19):
            return type_of_map(cx1(), tx, type_args, v19)

        case hydra.core.TermMaybe(value=v110):
            return type_of_maybe(cx1(), tx, type_args, v110)

        case hydra.core.TermPair(value=v111):
            return type_of_pair(cx1(), tx, type_args, v111)

        case hydra.core.TermProject(value=v112):
            return type_of_projection(cx1(), tx, type_args, v112)

        case hydra.core.TermRecord(value=v113):
            return type_of_record(cx1(), tx, type_args, v113)

        case hydra.core.TermSet(value=v114):
            return type_of_set(cx1(), tx, type_args, v114)

        case hydra.core.TermTypeApplication(value=v115):
            return type_of_type_application(cx1(), tx, type_args, v115)

        case hydra.core.TermTypeLambda(value=v116):
            return type_of_type_lambda(cx1(), tx, type_args, v116)

        case hydra.core.TermUnion(value=v117):
            return type_of_injection(cx1(), tx, type_args, v117)

        case hydra.core.TermUnit():
            return type_of_unit(cx1(), tx, type_args)

        case hydra.core.TermUnwrap(value=v118):
            return type_of_unwrap(cx1(), tx, type_args, v118)

        case hydra.core.TermVariable(value=v119):
            return type_of_variable(cx1(), tx, type_args, v119)

        case hydra.core.TermWrap(value=v120):
            return type_of_wrapped_term(cx1(), tx, type_args, v120)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorChecking(cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorUnsupportedTermVariant(hydra.error.checking.UnsupportedTermVariantError(hydra.reflect.term_variant(term)))))))

def type_of_annotated_term(cx: hydra.context.Context, tx: hydra.graph.Graph, type_args: frozenlist[hydra.core.Type], at: hydra.core.AnnotatedTerm) -> Either[hydra.errors.Error, tuple[hydra.core.Type, hydra.context.Context]]:
    r"""Reconstruct the type of an annotated term (Either/Context version)."""

    return type_of(cx, tx, type_args, at.body)

def type_of_application(cx: hydra.context.Context, tx: hydra.graph.Graph, type_args: frozenlist[hydra.core.Type], app: hydra.core.Application) -> Either[hydra.errors.Error, tuple[hydra.core.Type, hydra.context.Context]]:
    r"""Reconstruct the type of an application term (Either/Context version)."""

    fun = app.function
    arg = app.argument
    def try_type(cx0: hydra.context.Context, tfun: hydra.core.Type, targ: hydra.core.Type) -> Either[hydra.errors.Error, tuple[hydra.core.Type, hydra.context.Context]]:
        while True:
            match tfun:
                case hydra.core.TypeForall(value=ft):
                    cx0 = cx0
                    tfun = ft.body
                    targ = targ
                    continue

                case hydra.core.TypeFunction(value=ft2):
                    return (dom := ft2.domain, (cod := ft2.codomain, hydra.lib.logic.if_else(types_effectively_equal(tx, dom, targ), (lambda : Right((cod, cx0))), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorChecking(cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorTypeMismatch(hydra.error.checking.TypeMismatchError(dom, targ)))))))))[1])[1]

                case hydra.core.TypeVariable():
                    return (name_result := hydra.names.fresh_name(cx0), (fresh_n := hydra.lib.pairs.first(name_result), (cx1 := hydra.lib.pairs.second(name_result), Right((cast(hydra.core.Type, hydra.core.TypeVariable(fresh_n)), cx1)))[1])[1])[1]

                case _:
                    return Left(cast(hydra.errors.Error, hydra.errors.ErrorChecking(cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorNotAFunctionType(hydra.error.checking.NotAFunctionTypeError(tfun))))))
    return hydra.lib.eithers.bind(type_of(cx, tx, (), fun), (lambda result1: (tfun := hydra.lib.pairs.first(result1), cx2 := hydra.lib.pairs.second(result1), hydra.lib.eithers.bind(type_of(cx2, tx, (), arg), (lambda result2: (targ := hydra.lib.pairs.first(result2), cx3 := hydra.lib.pairs.second(result2), hydra.lib.eithers.bind(try_type(cx3, tfun, targ), (lambda result3: (t := hydra.lib.pairs.first(result3), cx4 := hydra.lib.pairs.second(result3), hydra.lib.eithers.bind(apply_type_arguments_to_type(cx4, tx, type_args, t), (lambda applied: Right((applied, cx4)))))[2])))[2])))[2]))

def type_of_case_statement(cx: hydra.context.Context, tx: hydra.graph.Graph, type_args: frozenlist[hydra.core.Type], cs: hydra.core.CaseStatement) -> Either[hydra.errors.Error, tuple[hydra.core.Type, hydra.context.Context]]:
    r"""Reconstruct the type of a case statement (Either/Context version)."""

    tname = cs.type_name
    dflt = cs.default
    cases = cs.cases
    @lru_cache(1)
    def cterms() -> frozenlist[hydra.core.Term]:
        return hydra.lib.lists.map((lambda v1: v1.term), cases)
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_maybe((lambda e: type_of(cx, tx, (), e)), dflt), (lambda dflt_result: (tdflt := hydra.lib.maybes.map((lambda x1: hydra.lib.pairs.first(x1)), dflt_result), cx2 := hydra.lib.maybes.maybe((lambda : cx), (lambda x1: hydra.lib.pairs.second(x1)), dflt_result), fold_result := hydra.lib.lists.foldl((lambda acc, term: hydra.lib.eithers.bind(acc, (lambda acc_r: (types := hydra.lib.pairs.first(acc_r), cx_a := hydra.lib.pairs.second(acc_r), hydra.lib.eithers.bind(type_of(cx_a, tx, (), term), (lambda t_result: (t := hydra.lib.pairs.first(t_result), cx_b := hydra.lib.pairs.second(t_result), Right((hydra.lib.lists.concat2(types, hydra.lib.lists.pure(t)), cx_b)))[2])))[2]))), Right(((), cx2)), cterms()), hydra.lib.eithers.bind(fold_result, (lambda fold_r: (tcterms := hydra.lib.pairs.first(fold_r), cx3 := hydra.lib.pairs.second(fold_r), fcods_result := hydra.lib.lists.foldl((lambda acc, t: hydra.lib.eithers.bind(acc, (lambda acc_r: (cods := hydra.lib.pairs.first(acc_r), hydra.lib.eithers.bind(hydra.extract.core.function_type(t), (lambda ft: Right((hydra.lib.lists.concat2(cods, hydra.lib.lists.pure(ft.codomain)), cx3)))))[1]))), Right(((), cx3)), tcterms), hydra.lib.eithers.bind(fcods_result, (lambda fcods_r: (fcods := hydra.lib.pairs.first(fcods_r), cods := hydra.lib.maybes.cat(hydra.lib.lists.cons(tdflt, hydra.lib.lists.map((lambda x1: hydra.lib.maybes.pure(x1)), fcods))), hydra.lib.eithers.bind(check_same_type(cx3, tx, "case branches", cods), (lambda cod: Right((cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(hydra.resolution.nominal_application(tname, type_args), cod))), cx3)))))[2])))[3])))[3]))

def type_of_either(cx: hydra.context.Context, tx: hydra.graph.Graph, type_args: frozenlist[hydra.core.Type], et: Either[hydra.core.Term, hydra.core.Term]) -> Either[hydra.errors.Error, tuple[hydra.core.Type, hydra.context.Context]]:
    r"""Reconstruct the type of an either value (Either/Context version)."""

    @lru_cache(1)
    def n() -> int:
        return hydra.lib.lists.length(type_args)
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(n(), 2), (lambda : hydra.lib.eithers.either((lambda left_term: hydra.lib.eithers.bind(type_of(cx, tx, (), left_term), (lambda result: (left_type := hydra.lib.pairs.first(result), cx2 := hydra.lib.pairs.second(result), Right((cast(hydra.core.Type, hydra.core.TypeEither(hydra.core.EitherType(left_type, hydra.lib.lists.at(1, type_args)))), cx2)))[2]))), (lambda right_term: hydra.lib.eithers.bind(type_of(cx, tx, (), right_term), (lambda result: (right_type := hydra.lib.pairs.first(result), cx2 := hydra.lib.pairs.second(result), Right((cast(hydra.core.Type, hydra.core.TypeEither(hydra.core.EitherType(hydra.lib.lists.at(0, type_args), right_type))), cx2)))[2]))), et)), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorChecking(cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorTypeArityMismatch(hydra.error.checking.TypeArityMismatchError(cast(hydra.core.Type, hydra.core.TypeEither(hydra.core.EitherType(cast(hydra.core.Type, hydra.core.TypeUnit()), cast(hydra.core.Type, hydra.core.TypeUnit())))), 2, n(), type_args))))))))

def type_of_lambda(cx: hydra.context.Context, tx: hydra.graph.Graph, type_args: frozenlist[hydra.core.Type], l: hydra.core.Lambda) -> Either[hydra.errors.Error, tuple[hydra.core.Type, hydra.context.Context]]:
    r"""Reconstruct the type of a lambda function (Either/Context version)."""

    v = l.parameter
    mdom = l.domain
    body = l.body
    return hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorChecking(cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorUntypedLambda(hydra.error.checking.UntypedLambdaError())))))), (lambda dom: (types2 := hydra.lib.maps.insert(v, hydra.scoping.f_type_to_type_scheme(dom), tx.bound_types), hydra.lib.eithers.bind(type_of(cx, hydra.graph.Graph(tx.bound_terms, types2, tx.class_constraints, tx.lambda_variables, tx.metadata, tx.primitives, tx.schema_types, tx.type_variables), (), body), (lambda cod_result: (cod := hydra.lib.pairs.first(cod_result), cx2 := hydra.lib.pairs.second(cod_result), Right((cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(dom, cod))), cx2)))[2])))[1]), mdom), (lambda tbody_result: (tbody := hydra.lib.pairs.first(tbody_result), cx3 := hydra.lib.pairs.second(tbody_result), hydra.lib.eithers.bind(apply_type_arguments_to_type(cx3, tx, type_args, tbody), (lambda applied: Right((applied, cx3)))))[2]))

def type_of_let(cx: hydra.context.Context, tx: hydra.graph.Graph, type_args: frozenlist[hydra.core.Type], let_term: hydra.core.Let) -> Either[hydra.errors.Error, tuple[hydra.core.Type, hydra.context.Context]]:
    r"""Reconstruct the type of a let binding (Either/Context version)."""

    bs = let_term.bindings
    body = let_term.body
    @lru_cache(1)
    def bnames() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.map((lambda v1: v1.name), bs)
    def binding_type(b: hydra.core.Binding) -> Either[hydra.errors.Error, hydra.core.Type]:
        return hydra.lib.maybes.maybe((lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorChecking(cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorUntypedLetBinding(hydra.error.checking.UntypedLetBindingError(b))))))), (lambda ts: Right(hydra.scoping.type_scheme_to_f_type(ts))), b.type)
    @lru_cache(1)
    def btypes_result() -> Either[hydra.errors.Error, tuple[frozenlist[hydra.core.Type], None]]:
        return hydra.lib.lists.foldl((lambda acc, b: hydra.lib.eithers.bind(acc, (lambda acc_r: (types := hydra.lib.pairs.first(acc_r), hydra.lib.eithers.bind(binding_type(b), (lambda btype: Right((hydra.lib.lists.concat2(types, hydra.lib.lists.pure(btype)), None)))))[1]))), Right(((), None)), bs)
    return hydra.lib.eithers.bind(btypes_result(), (lambda btypes_r: (btypes := hydra.lib.pairs.first(btypes_r), tx2 := hydra.graph.Graph(tx.bound_terms, hydra.lib.maps.union(hydra.lib.maps.from_list(hydra.lib.lists.zip(bnames(), hydra.lib.lists.map((lambda x1: hydra.scoping.f_type_to_type_scheme(x1)), btypes))), tx.bound_types), tx.class_constraints, tx.lambda_variables, tx.metadata, tx.primitives, tx.schema_types, tx.type_variables), hydra.lib.eithers.bind(type_of(cx, tx2, (), body), (lambda t_result: (t := hydra.lib.pairs.first(t_result), cx2 := hydra.lib.pairs.second(t_result), hydra.lib.eithers.bind(apply_type_arguments_to_type(cx2, tx, type_args, t), (lambda applied: Right((applied, cx2)))))[2])))[2]))

def type_of_list(cx: hydra.context.Context, tx: hydra.graph.Graph, type_args: frozenlist[hydra.core.Type], els: frozenlist[hydra.core.Term]) -> Either[hydra.errors.Error, tuple[hydra.core.Type, hydra.context.Context]]:
    r"""Reconstruct the type of a list (Either/Context version)."""

    return hydra.lib.logic.if_else(hydra.lib.lists.null(els), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(type_args), 1), (lambda : Right((cast(hydra.core.Type, hydra.core.TypeList(hydra.lib.lists.head(type_args))), cx))), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorChecking(cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorTypeArityMismatch(hydra.error.checking.TypeArityMismatchError(cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeUnit()))), 1, hydra.lib.lists.length(type_args), type_args))))))))), (lambda : (fold_result := hydra.lib.lists.foldl((lambda acc, term: hydra.lib.eithers.bind(acc, (lambda acc_r: (types := hydra.lib.pairs.first(acc_r), cx_a := hydra.lib.pairs.second(acc_r), hydra.lib.eithers.bind(type_of(cx_a, tx, (), term), (lambda t_result: (t := hydra.lib.pairs.first(t_result), cx_b := hydra.lib.pairs.second(t_result), Right((hydra.lib.lists.concat2(types, hydra.lib.lists.pure(t)), cx_b)))[2])))[2]))), Right(((), cx)), els), hydra.lib.eithers.bind(fold_result, (lambda fold_r: (eltypes := hydra.lib.pairs.first(fold_r), cx2 := hydra.lib.pairs.second(fold_r), hydra.lib.eithers.bind(check_same_type(cx2, tx, "list elements", eltypes), (lambda unified_type: Right((cast(hydra.core.Type, hydra.core.TypeList(unified_type)), cx2)))))[2])))[1]))

def type_of_map(cx: hydra.context.Context, tx: hydra.graph.Graph, type_args: frozenlist[hydra.core.Type], m: FrozenDict[hydra.core.Term, hydra.core.Term]) -> Either[hydra.errors.Error, tuple[hydra.core.Type, hydra.context.Context]]:
    r"""Reconstruct the type of a map (Either/Context version)."""

    return hydra.lib.logic.if_else(hydra.lib.maps.null(m), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(type_args), 2), (lambda : Right((cast(hydra.core.Type, hydra.core.TypeMap(hydra.core.MapType(hydra.lib.lists.at(0, type_args), hydra.lib.lists.at(1, type_args)))), cx))), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorChecking(cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorTypeArityMismatch(hydra.error.checking.TypeArityMismatchError(cast(hydra.core.Type, hydra.core.TypeMap(hydra.core.MapType(cast(hydra.core.Type, hydra.core.TypeUnit()), cast(hydra.core.Type, hydra.core.TypeUnit())))), 2, hydra.lib.lists.length(type_args), type_args))))))))), (lambda : (pairs := hydra.lib.maps.to_list(m), (key_fold_result := hydra.lib.lists.foldl((lambda acc, p: hydra.lib.eithers.bind(acc, (lambda acc_r: (types := hydra.lib.pairs.first(acc_r), cx_a := hydra.lib.pairs.second(acc_r), hydra.lib.eithers.bind(type_of(cx_a, tx, (), hydra.lib.pairs.first(p)), (lambda t_result: (t := hydra.lib.pairs.first(t_result), cx_b := hydra.lib.pairs.second(t_result), Right((hydra.lib.lists.concat2(types, hydra.lib.lists.pure(t)), cx_b)))[2])))[2]))), Right(((), cx)), pairs), hydra.lib.eithers.bind(key_fold_result, (lambda key_fold_r: (key_types := hydra.lib.pairs.first(key_fold_r), cx2 := hydra.lib.pairs.second(key_fold_r), hydra.lib.eithers.bind(check_same_type(cx2, tx, "map keys", key_types), (lambda kt: (val_fold_result := hydra.lib.lists.foldl((lambda acc, p: hydra.lib.eithers.bind(acc, (lambda acc_r: (types := hydra.lib.pairs.first(acc_r), cx_a := hydra.lib.pairs.second(acc_r), hydra.lib.eithers.bind(type_of(cx_a, tx, (), hydra.lib.pairs.second(p)), (lambda t_result: (t := hydra.lib.pairs.first(t_result), cx_b := hydra.lib.pairs.second(t_result), Right((hydra.lib.lists.concat2(types, hydra.lib.lists.pure(t)), cx_b)))[2])))[2]))), Right(((), cx2)), pairs), hydra.lib.eithers.bind(val_fold_result, (lambda val_fold_r: (val_types := hydra.lib.pairs.first(val_fold_r), cx3 := hydra.lib.pairs.second(val_fold_r), hydra.lib.eithers.bind(check_same_type(cx3, tx, "map values", val_types), (lambda vt: hydra.lib.eithers.bind(apply_type_arguments_to_type(cx3, tx, type_args, cast(hydra.core.Type, hydra.core.TypeMap(hydra.core.MapType(kt, vt)))), (lambda applied: Right((applied, cx3)))))))[2])))[1])))[2])))[1])[1]))

def type_of_maybe(cx: hydra.context.Context, tx: hydra.graph.Graph, type_args: frozenlist[hydra.core.Type], mt: Maybe[hydra.core.Term]) -> Either[hydra.errors.Error, tuple[hydra.core.Type, hydra.context.Context]]:
    r"""Reconstruct the type of an optional value (Either/Context version)."""

    @lru_cache(1)
    def for_nothing() -> Either[hydra.errors.Error, tuple[hydra.core.Type, hydra.context.Context]]:
        @lru_cache(1)
        def n() -> int:
            return hydra.lib.lists.length(type_args)
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(n(), 1), (lambda : Right((cast(hydra.core.Type, hydra.core.TypeMaybe(hydra.lib.lists.head(type_args))), cx))), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorChecking(cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorTypeArityMismatch(hydra.error.checking.TypeArityMismatchError(cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeUnit()))), 1, n(), type_args))))))))
    def for_just(term: hydra.core.Term) -> Either[hydra.errors.Error, tuple[hydra.core.Type, hydra.context.Context]]:
        return hydra.lib.eithers.bind(type_of(cx, tx, (), term), (lambda t_result: (term_type := hydra.lib.pairs.first(t_result), cx2 := hydra.lib.pairs.second(t_result), t := cast(hydra.core.Type, hydra.core.TypeMaybe(term_type)), hydra.lib.eithers.bind(apply_type_arguments_to_type(cx2, tx, type_args, t), (lambda applied: Right((applied, cx2)))))[3]))
    return hydra.lib.maybes.maybe((lambda : for_nothing()), (lambda x1: for_just(x1)), mt)

def type_of_pair(cx: hydra.context.Context, tx: hydra.graph.Graph, type_args: frozenlist[hydra.core.Type], p: tuple[hydra.core.Term, hydra.core.Term]) -> Either[hydra.errors.Error, tuple[hydra.core.Type, hydra.context.Context]]:
    r"""Reconstruct the type of a pair (Either/Context version)."""

    @lru_cache(1)
    def n() -> int:
        return hydra.lib.lists.length(type_args)
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(n(), 2), (lambda : (pair_fst := hydra.lib.pairs.first(p), (pair_snd := hydra.lib.pairs.second(p), hydra.lib.eithers.bind(type_of(cx, tx, (), pair_fst), (lambda result1: (first_type := hydra.lib.pairs.first(result1), cx2 := hydra.lib.pairs.second(result1), hydra.lib.eithers.bind(type_of(cx2, tx, (), pair_snd), (lambda result2: (second_type := hydra.lib.pairs.first(result2), cx3 := hydra.lib.pairs.second(result2), Right((cast(hydra.core.Type, hydra.core.TypePair(hydra.core.PairType(first_type, second_type))), cx3)))[2])))[2])))[1])[1]), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorChecking(cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorTypeArityMismatch(hydra.error.checking.TypeArityMismatchError(cast(hydra.core.Type, hydra.core.TypePair(hydra.core.PairType(cast(hydra.core.Type, hydra.core.TypeUnit()), cast(hydra.core.Type, hydra.core.TypeUnit())))), 2, n(), type_args))))))))

def type_of_record(cx: hydra.context.Context, tx: hydra.graph.Graph, type_args: frozenlist[hydra.core.Type], record: hydra.core.Record) -> Either[hydra.errors.Error, tuple[hydra.core.Type, hydra.context.Context]]:
    r"""Reconstruct the type of a record (Either/Context version)."""

    tname = record.type_name
    fields = record.fields
    @lru_cache(1)
    def fold_result() -> Either[hydra.errors.Error, tuple[frozenlist[hydra.core.Type], hydra.context.Context]]:
        return hydra.lib.lists.foldl((lambda acc, term: hydra.lib.eithers.bind(acc, (lambda acc_r: (types := hydra.lib.pairs.first(acc_r), cx_a := hydra.lib.pairs.second(acc_r), hydra.lib.eithers.bind(type_of(cx_a, tx, (), term), (lambda t_result: (t := hydra.lib.pairs.first(t_result), cx_b := hydra.lib.pairs.second(t_result), Right((hydra.lib.lists.concat2(types, hydra.lib.lists.pure(t)), cx_b)))[2])))[2]))), Right(((), cx)), hydra.lib.lists.map((lambda v1: v1.term), fields))
    return hydra.lib.eithers.bind(fold_result(), (lambda fold_r: (cx2 := hydra.lib.pairs.second(fold_r), Right((hydra.resolution.nominal_application(tname, type_args), cx2)))[1]))

def type_of_set(cx: hydra.context.Context, tx: hydra.graph.Graph, type_args: frozenlist[hydra.core.Type], els: frozenset[hydra.core.Term]) -> Either[hydra.errors.Error, tuple[hydra.core.Type, hydra.context.Context]]:
    r"""Reconstruct the type of a set (Either/Context version)."""

    return hydra.lib.logic.if_else(hydra.lib.sets.null(els), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(type_args), 1), (lambda : Right((cast(hydra.core.Type, hydra.core.TypeSet(hydra.lib.lists.head(type_args))), cx))), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorChecking(cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorTypeArityMismatch(hydra.error.checking.TypeArityMismatchError(cast(hydra.core.Type, hydra.core.TypeSet(cast(hydra.core.Type, hydra.core.TypeUnit()))), 1, hydra.lib.lists.length(type_args), type_args))))))))), (lambda : (fold_result := hydra.lib.lists.foldl((lambda acc, term: hydra.lib.eithers.bind(acc, (lambda acc_r: (types := hydra.lib.pairs.first(acc_r), cx_a := hydra.lib.pairs.second(acc_r), hydra.lib.eithers.bind(type_of(cx_a, tx, (), term), (lambda t_result: (t := hydra.lib.pairs.first(t_result), cx_b := hydra.lib.pairs.second(t_result), Right((hydra.lib.lists.concat2(types, hydra.lib.lists.pure(t)), cx_b)))[2])))[2]))), Right(((), cx)), hydra.lib.sets.to_list(els)), hydra.lib.eithers.bind(fold_result, (lambda fold_r: (eltypes := hydra.lib.pairs.first(fold_r), cx2 := hydra.lib.pairs.second(fold_r), hydra.lib.eithers.bind(check_same_type(cx2, tx, "set elements", eltypes), (lambda unified_type: Right((cast(hydra.core.Type, hydra.core.TypeSet(unified_type)), cx2)))))[2])))[1]))

def type_of_type_application(cx: hydra.context.Context, tx: hydra.graph.Graph, type_args: frozenlist[hydra.core.Type], tyapp: hydra.core.TypeApplicationTerm) -> Either[hydra.errors.Error, tuple[hydra.core.Type, hydra.context.Context]]:
    r"""Reconstruct the type of a type application term (Either/Context version)."""

    body = tyapp.body
    t = tyapp.type
    return type_of(cx, tx, hydra.lib.lists.cons(t, type_args), body)

def type_of_type_lambda(cx: hydra.context.Context, tx: hydra.graph.Graph, type_args: frozenlist[hydra.core.Type], tl: hydra.core.TypeLambda) -> Either[hydra.errors.Error, tuple[hydra.core.Type, hydra.context.Context]]:
    r"""Reconstruct the type of a type lambda (type abstraction) term (Either/Context version)."""

    v = tl.parameter
    body = tl.body
    vars = tx.type_variables
    @lru_cache(1)
    def tx2() -> hydra.graph.Graph:
        return hydra.graph.Graph(tx.bound_terms, tx.bound_types, tx.class_constraints, tx.lambda_variables, tx.metadata, tx.primitives, tx.schema_types, hydra.lib.sets.insert(v, vars))
    return hydra.lib.eithers.bind(type_of(cx, tx2(), (), body), (lambda result1: (t1 := hydra.lib.pairs.first(result1), cx2 := hydra.lib.pairs.second(result1), hydra.lib.eithers.bind(apply_type_arguments_to_type(cx2, tx, type_args, cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(v, t1)))), (lambda applied: Right((applied, cx2)))))[2]))

def type_of_wrapped_term(cx: hydra.context.Context, tx: hydra.graph.Graph, type_args: frozenlist[hydra.core.Type], wt: hydra.core.WrappedTerm) -> Either[hydra.errors.Error, tuple[hydra.core.Type, hydra.context.Context]]:
    r"""Reconstruct the type of a wrapped term (Either/Context version)."""

    tname = wt.type_name
    body = wt.body
    return hydra.lib.eithers.bind(type_of(cx, tx, (), body), (lambda result: (cx2 := hydra.lib.pairs.second(result), Right((hydra.resolution.nominal_application(tname, type_args), cx2)))[1]))

def check_type(cx: hydra.context.Context, tx: hydra.graph.Graph, term: hydra.core.Term, typ: hydra.core.Type) -> Either[hydra.errors.Error, None]:
    r"""Check that a term has the expected type."""

    vars = tx.type_variables
    return hydra.lib.logic.if_else(hydra.constants.debug_inference, (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.map((lambda _p: hydra.lib.pairs.first(_p)), type_of(cx, tx, (), term)), (lambda t0: hydra.lib.logic.if_else(types_effectively_equal(tx, t0, typ), (lambda : Right(None)), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorChecking(cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorTypeMismatch(hydra.error.checking.TypeMismatchError(typ, t0))))))))))), (lambda : Right(None)))

def check_type_subst(cx: T0, tx: hydra.graph.Graph, subst: hydra.typing.TypeSubst) -> Either[hydra.errors.Error, hydra.typing.TypeSubst]:
    r"""Sanity-check a type substitution arising from unification. Specifically, check that schema types have not been inappropriately unified with type variables inferred from terms."""

    s = subst.value
    @lru_cache(1)
    def vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(hydra.lib.maps.keys(s))
    @lru_cache(1)
    def suspect_vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.intersection(vars(), hydra.lib.sets.from_list(hydra.lib.maps.keys(tx.schema_types)))
    def is_nominal(ts: hydra.core.TypeScheme) -> bool:
        match hydra.strip.deannotate_type(ts.type):
            case hydra.core.TypeRecord():
                return True

            case hydra.core.TypeUnion():
                return True

            case hydra.core.TypeWrap():
                return True

            case _:
                return False
    @lru_cache(1)
    def bad_vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(hydra.lib.lists.filter((lambda v: hydra.lib.maybes.maybe((lambda : False), (lambda x1: is_nominal(x1)), hydra.lexical.dereference_schema_type(v, tx.schema_types))), hydra.lib.sets.to_list(suspect_vars())))
    @lru_cache(1)
    def bad_pairs() -> frozenlist[tuple[hydra.core.Name, hydra.core.Type]]:
        return hydra.lib.lists.filter((lambda p: hydra.lib.sets.member(hydra.lib.pairs.first(p), bad_vars())), hydra.lib.maps.to_list(s))
    def print_pair(p: tuple[hydra.core.Name, hydra.core.Type]) -> str:
        return hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.pairs.first(p).value, " --> "), hydra.show.core.type(hydra.lib.pairs.second(p)))
    return hydra.lib.logic.if_else(hydra.lib.sets.null(bad_vars()), (lambda : Right(subst)), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorChecking(cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorIncorrectUnification(hydra.error.checking.IncorrectUnificationError(subst))))))))

def check_type_variables(_tx: T0, _typ: T1) -> None:
    r"""Check that all type variables in a type are bound. NOTE: This check is currently disabled to allow phantom type variables from polymorphic instantiation to pass through. The proper fix is to ensure `typeOf` doesn't create fresh variables for post-inference code."""

    return None

def to_f_context(cx: hydra.graph.Graph) -> FrozenDict[hydra.core.Name, hydra.core.Type]:
    r"""Get the bound types from a graph as a type environment."""

    return hydra.lib.maps.map((lambda x1: hydra.scoping.type_scheme_to_f_type(x1)), cx.bound_types)

def type_lists_effectively_equal(tx: hydra.graph.Graph, tlist1: frozenlist[hydra.core.Type], tlist2: frozenlist[hydra.core.Type]) -> bool:
    r"""Check whether two lists of types are effectively equal, disregarding type aliases."""

    return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(tlist1), hydra.lib.lists.length(tlist2)), (lambda : hydra.lib.lists.foldl(hydra.lib.logic.and_, True, hydra.lib.lists.zip_with((lambda v1, v2: types_effectively_equal(tx, v1, v2)), tlist1, tlist2))), (lambda : False))

def type_of_primitive(cx: hydra.context.Context, tx: hydra.graph.Graph, type_args: frozenlist[hydra.core.Type], name: hydra.core.Name) -> Either[hydra.errors.Error, tuple[hydra.core.Type, hydra.context.Context]]:
    r"""Reconstruct the type of a primitive function (Either/Context version)."""

    @lru_cache(1)
    def raw_ts() -> Maybe[hydra.core.TypeScheme]:
        return hydra.lib.maybes.map((lambda _p: _p.type), hydra.lib.maps.lookup(name, tx.primitives))
    return hydra.lib.maybes.maybe((lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorUndefinedTermVariable(hydra.error.core.UndefinedTermVariableError(hydra.paths.SubtermPath(()), name))))), (lambda ts_raw: (inst_result := hydra.resolution.instantiate_type_scheme(cx, ts_raw), ts := hydra.lib.pairs.first(inst_result), cx2 := hydra.lib.pairs.second(inst_result), t := hydra.scoping.type_scheme_to_f_type(ts), hydra.lib.eithers.bind(apply_type_arguments_to_type(cx2, tx, type_args, t), (lambda applied: Right((applied, cx2)))))[4]), raw_ts())

def type_of_term(cx: hydra.context.Context, g: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Type]:
    r"""Check the type of a term."""

    return hydra.lib.eithers.map((lambda x1: hydra.lib.pairs.first(x1)), type_of(cx, g, (), term))
