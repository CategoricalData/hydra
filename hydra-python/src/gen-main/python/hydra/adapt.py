# Note: this is an automatically generated file. Do not edit.

r"""Simple, one-way adapters for types and terms."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.coders
import hydra.context
import hydra.core
import hydra.dependencies
import hydra.environment
import hydra.errors
import hydra.graph
import hydra.hoisting
import hydra.inference
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
import hydra.literals
import hydra.module
import hydra.names
import hydra.reduction
import hydra.reflect
import hydra.resolution
import hydra.rewriting
import hydra.scoping
import hydra.show.core
import hydra.show.errors
import hydra.strip
import hydra.variables

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def literal_type_supported(constraints: hydra.coders.LanguageConstraints, lt: hydra.core.LiteralType) -> bool:
    r"""Check if a literal type is supported by the given language constraints."""

    def for_type(lt2: hydra.core.LiteralType) -> bool:
        match lt2:
            case hydra.core.LiteralTypeFloat(value=ft):
                return hydra.lib.sets.member(ft, constraints.float_types)

            case hydra.core.LiteralTypeInteger(value=it):
                return hydra.lib.sets.member(it, constraints.integer_types)

            case _:
                return True
    return hydra.lib.logic.if_else(hydra.lib.sets.member(hydra.reflect.literal_type_variant(lt), constraints.literal_variants), (lambda : for_type(lt)), (lambda : False))

def type_alternatives(type: hydra.core.Type) -> frozenlist[hydra.core.Type]:
    r"""Find a list of alternatives for a given type, if any."""

    match type:
        case hydra.core.TypeAnnotated(value=at):
            type2 = at.body
            return (type2,)

        case hydra.core.TypeMaybe(value=ot):
            return (cast(hydra.core.Type, hydra.core.TypeList(ot)),)

        case hydra.core.TypeUnion(value=rt):
            def to_opt_field(f: hydra.core.FieldType) -> hydra.core.FieldType:
                return hydra.core.FieldType(f.name, cast(hydra.core.Type, hydra.core.TypeMaybe(f.type)))
            @lru_cache(1)
            def opt_fields() -> frozenlist[hydra.core.FieldType]:
                return hydra.lib.lists.map((lambda x1: to_opt_field(x1)), rt)
            return (cast(hydra.core.Type, hydra.core.TypeRecord(opt_fields())),)

        case hydra.core.TypeUnit():
            return (cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeBoolean()))),)

        case hydra.core.TypeVoid():
            return (cast(hydra.core.Type, hydra.core.TypeUnit()),)

        case _:
            return ()

def adapt_type(constraints: hydra.coders.LanguageConstraints, litmap: FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType], type0: hydra.core.Type) -> Either[str, hydra.core.Type]:
    r"""Adapt a type using the given language constraints."""

    def for_supported(typ: hydra.core.Type) -> Maybe[hydra.core.Type]:
        match typ:
            case hydra.core.TypeLiteral(value=lt):
                return hydra.lib.logic.if_else(literal_type_supported(constraints, lt), (lambda : Just(typ)), (lambda : hydra.lib.maybes.maybe((lambda : Just(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))))), (lambda lt2: Just(cast(hydra.core.Type, hydra.core.TypeLiteral(lt2)))), hydra.lib.maps.lookup(lt, litmap))))

            case _:
                return Just(typ)
    def for_unsupported(typ: hydra.core.Type) -> Maybe[hydra.core.Type]:
        def try_alts(alts: frozenlist[hydra.core.Type]) -> Maybe[hydra.core.Type]:
            return hydra.lib.logic.if_else(hydra.lib.lists.null(alts), (lambda : Nothing()), (lambda : hydra.lib.maybes.maybe((lambda : try_alts(hydra.lib.lists.tail(alts))), (lambda t: Just(t)), try_type(hydra.lib.lists.head(alts)))))
        @lru_cache(1)
        def alts0() -> frozenlist[hydra.core.Type]:
            return type_alternatives(typ)
        return try_alts(alts0())
    def try_type(typ: hydra.core.Type) -> Maybe[hydra.core.Type]:
        @lru_cache(1)
        def supported_variant() -> bool:
            return hydra.lib.sets.member(hydra.reflect.type_variant(typ), constraints.type_variants)
        return hydra.lib.logic.if_else(supported_variant(), (lambda : for_supported(typ)), (lambda : for_unsupported(typ)))
    def rewrite(recurse: Callable[[hydra.core.Type], Either[str, hydra.core.Type]], typ: hydra.core.Type) -> Either[str, hydra.core.Type]:
        return hydra.lib.eithers.bind(recurse(typ), (lambda type1: hydra.lib.maybes.maybe((lambda : Left(hydra.lib.strings.cat2("no alternatives for type: ", hydra.show.core.type(typ)))), (lambda type2: Right(type2)), try_type(type1))))
    return hydra.rewriting.rewrite_type_m((lambda x1, x2: rewrite(x1, x2)), type0)

def adapt_graph_schema(constraints: hydra.coders.LanguageConstraints, litmap: FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType], types0: FrozenDict[T0, hydra.core.Type]) -> Either[str, FrozenDict[T0, hydra.core.Type]]:
    r"""Adapt a schema graph to the given language constraints."""

    def map_pair(pair: tuple[T1, hydra.core.Type]) -> Either[str, tuple[T1, hydra.core.Type]]:
        @lru_cache(1)
        def name() -> hydra.core.Type:
            return hydra.lib.pairs.first(pair)
        @lru_cache(1)
        def typ() -> hydra.core.Type:
            return hydra.lib.pairs.second(pair)
        return hydra.lib.eithers.bind(adapt_type(constraints, litmap, typ()), (lambda typ1: Right((name(), typ1))))
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: map_pair(x1)), hydra.lib.maps.to_list(types0)), (lambda pairs: Right(hydra.lib.maps.from_list(pairs))))

def adapt_lambda_domains(constraints: hydra.coders.LanguageConstraints, litmap: FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType], recurse: Callable[[T0], Either[str, hydra.core.Term]], term: T0):
    def _hoist_hydra_adapt_adapt_lambda_domains_1(constraints, f, litmap, v1):
        match v1:
            case hydra.core.FunctionLambda(value=l):
                return hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda dom: hydra.lib.eithers.bind(adapt_type(constraints, litmap, dom), (lambda dom1: Right(Just(dom1))))), l.domain), (lambda adapted_domain: Right(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, adapted_domain, l.body))))))))

            case _:
                return Right(cast(hydra.core.Term, hydra.core.TermFunction(f)))
    def _hoist_hydra_adapt_adapt_lambda_domains_2(constraints, litmap, rewritten, v1):
        match v1:
            case hydra.core.TermFunction(value=f):
                return _hoist_hydra_adapt_adapt_lambda_domains_1(constraints, f, litmap, f)

            case _:
                return Right(rewritten)
    return hydra.lib.eithers.bind(recurse(term), (lambda rewritten: _hoist_hydra_adapt_adapt_lambda_domains_2(constraints, litmap, rewritten, rewritten)))

def adapt_float_type(constraints: hydra.coders.LanguageConstraints, ft: hydra.core.FloatType) -> Maybe[hydra.core.FloatType]:
    r"""Attempt to adapt a floating-point type using the given language constraints."""

    @lru_cache(1)
    def supported() -> bool:
        return hydra.lib.sets.member(ft, constraints.float_types)
    def alt(v1: hydra.core.FloatType) -> Maybe[hydra.core.FloatType]:
        return adapt_float_type(constraints, v1)
    def for_unsupported(ft2: hydra.core.FloatType) -> Maybe[hydra.core.FloatType]:
        match ft2:
            case hydra.core.FloatType.BIGFLOAT:
                return alt(hydra.core.FloatType.FLOAT64)

            case hydra.core.FloatType.FLOAT32:
                return alt(hydra.core.FloatType.FLOAT64)

            case hydra.core.FloatType.FLOAT64:
                return alt(hydra.core.FloatType.BIGFLOAT)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.lib.logic.if_else(supported(), (lambda : Just(ft)), (lambda : for_unsupported(ft)))

def adapt_integer_type(constraints: hydra.coders.LanguageConstraints, it: hydra.core.IntegerType) -> Maybe[hydra.core.IntegerType]:
    r"""Attempt to adapt an integer type using the given language constraints."""

    @lru_cache(1)
    def supported() -> bool:
        return hydra.lib.sets.member(it, constraints.integer_types)
    def alt(v1: hydra.core.IntegerType) -> Maybe[hydra.core.IntegerType]:
        return adapt_integer_type(constraints, v1)
    def for_unsupported(it2: hydra.core.IntegerType) -> Maybe[hydra.core.IntegerType]:
        match it2:
            case hydra.core.IntegerType.BIGINT:
                return Nothing()

            case hydra.core.IntegerType.INT8:
                return alt(hydra.core.IntegerType.UINT16)

            case hydra.core.IntegerType.INT16:
                return alt(hydra.core.IntegerType.UINT32)

            case hydra.core.IntegerType.INT32:
                return alt(hydra.core.IntegerType.UINT64)

            case hydra.core.IntegerType.INT64:
                return alt(hydra.core.IntegerType.BIGINT)

            case hydra.core.IntegerType.UINT8:
                return alt(hydra.core.IntegerType.INT16)

            case hydra.core.IntegerType.UINT16:
                return alt(hydra.core.IntegerType.INT32)

            case hydra.core.IntegerType.UINT32:
                return alt(hydra.core.IntegerType.INT64)

            case hydra.core.IntegerType.UINT64:
                return alt(hydra.core.IntegerType.BIGINT)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.lib.logic.if_else(supported(), (lambda : Just(it)), (lambda : for_unsupported(it)))

def adapt_literal_type(constraints: hydra.coders.LanguageConstraints, lt: hydra.core.LiteralType) -> Maybe[hydra.core.LiteralType]:
    r"""Attempt to adapt a literal type using the given language constraints."""

    def for_unsupported(lt2: hydra.core.LiteralType) -> Maybe[hydra.core.LiteralType]:
        match lt2:
            case hydra.core.LiteralTypeBinary():
                return Just(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))

            case hydra.core.LiteralTypeBoolean():
                return hydra.lib.maybes.map((lambda x: cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(x))), adapt_integer_type(constraints, hydra.core.IntegerType.INT8))

            case hydra.core.LiteralTypeFloat(value=ft):
                return hydra.lib.maybes.map((lambda x: cast(hydra.core.LiteralType, hydra.core.LiteralTypeFloat(x))), adapt_float_type(constraints, ft))

            case hydra.core.LiteralTypeInteger(value=it):
                return hydra.lib.maybes.map((lambda x: cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(x))), adapt_integer_type(constraints, it))

            case _:
                return Nothing()
    return hydra.lib.logic.if_else(literal_type_supported(constraints, lt), (lambda : Nothing()), (lambda : for_unsupported(lt)))

def adapt_literal_types_map(constraints: hydra.coders.LanguageConstraints) -> FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType]:
    r"""Derive a map of adapted literal types for the given language constraints."""

    def try_type(lt: hydra.core.LiteralType) -> Maybe[tuple[hydra.core.LiteralType, hydra.core.LiteralType]]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda lt2: Just((lt, lt2))), adapt_literal_type(constraints, lt))
    return hydra.lib.maps.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda x1: try_type(x1)), hydra.reflect.literal_types())))

def adapt_type_scheme(constraints: hydra.coders.LanguageConstraints, litmap: FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType], ts0: hydra.core.TypeScheme) -> Either[str, hydra.core.TypeScheme]:
    r"""Adapt a type scheme to the given language constraints, prior to inference."""

    vars0 = ts0.variables
    t0 = ts0.type
    return hydra.lib.eithers.bind(adapt_type(constraints, litmap, t0), (lambda t1: Right(hydra.core.TypeScheme(vars0, t1, ts0.constraints))))

def adapt_nested_types(constraints: hydra.coders.LanguageConstraints, litmap: FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType], recurse: Callable[[T0], Either[str, hydra.core.Term]], term: T0):
    def _hoist_hydra_adapt_adapt_nested_types_1(constraints, litmap, rewritten, v1):
        match v1:
            case hydra.core.TermLet(value=lt):
                def adapt_b(b: hydra.core.Binding) -> Either[str, hydra.core.Binding]:
                    return hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda ts: hydra.lib.eithers.bind(adapt_type_scheme(constraints, litmap, ts), (lambda ts1: Right(Just(ts1))))), b.type), (lambda adapted_b_type: Right(hydra.core.Binding(b.name, b.term, adapted_b_type))))
                return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: adapt_b(x1)), lt.bindings), (lambda adapted_bindings: Right(cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(adapted_bindings, lt.body))))))

            case _:
                return Right(rewritten)
    return hydra.lib.eithers.bind(recurse(term), (lambda rewritten: _hoist_hydra_adapt_adapt_nested_types_1(constraints, litmap, rewritten, rewritten)))

def adapt_primitive(constraints: hydra.coders.LanguageConstraints, litmap: FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType], prim0: hydra.graph.Primitive) -> Either[str, hydra.graph.Primitive]:
    r"""Adapt a primitive to the given language constraints, prior to inference."""

    ts0 = prim0.type
    return hydra.lib.eithers.bind(adapt_type_scheme(constraints, litmap, ts0), (lambda ts1: Right(hydra.graph.Primitive(prim0.name, ts1, prim0.implementation))))

def adapt_literal(lt: hydra.core.LiteralType, l: hydra.core.Literal):
    def _hoist_hydra_adapt_adapt_literal_1(b, v1):
        match v1:
            case hydra.core.LiteralTypeString():
                return cast(hydra.core.Literal, hydra.core.LiteralString(hydra.lib.literals.binary_to_string(b)))

            case _:
                raise TypeError("Unsupported LiteralType")
    def _hoist_hydra_adapt_adapt_literal_2(b, v1):
        match v1:
            case hydra.core.LiteralTypeInteger(value=it):
                return cast(hydra.core.Literal, hydra.core.LiteralInteger(hydra.literals.bigint_to_integer_value(it, hydra.lib.logic.if_else(b, (lambda : 1), (lambda : 0)))))

            case _:
                raise TypeError("Unsupported LiteralType")
    def _hoist_hydra_adapt_adapt_literal_3(f, v1):
        match v1:
            case hydra.core.LiteralTypeFloat(value=ft):
                return cast(hydra.core.Literal, hydra.core.LiteralFloat(hydra.literals.bigfloat_to_float_value(ft, hydra.literals.float_value_to_bigfloat(f))))

            case _:
                raise TypeError("Unsupported LiteralType")
    def _hoist_hydra_adapt_adapt_literal_4(i, v1):
        match v1:
            case hydra.core.LiteralTypeInteger(value=it):
                return cast(hydra.core.Literal, hydra.core.LiteralInteger(hydra.literals.bigint_to_integer_value(it, hydra.literals.integer_value_to_bigint(i))))

            case _:
                raise TypeError("Unsupported LiteralType")
    match l:
        case hydra.core.LiteralBinary(value=b):
            return _hoist_hydra_adapt_adapt_literal_1(b, lt)

        case hydra.core.LiteralBoolean(value=b2):
            return _hoist_hydra_adapt_adapt_literal_2(b2, lt)

        case hydra.core.LiteralFloat(value=f):
            return _hoist_hydra_adapt_adapt_literal_3(f, lt)

        case hydra.core.LiteralInteger(value=i):
            return _hoist_hydra_adapt_adapt_literal_4(i, lt)

        case _:
            raise TypeError("Unsupported Literal")

def adapt_literal_value(litmap: FrozenDict[T0, hydra.core.LiteralType], lt: T0, l: hydra.core.Literal) -> hydra.core.Literal:
    r"""Adapt a literal value using the given language constraints."""

    return hydra.lib.maybes.maybe((lambda : cast(hydra.core.Literal, hydra.core.LiteralString(hydra.show.core.literal(l)))), (lambda lt2: adapt_literal(lt2, l)), hydra.lib.maps.lookup(lt, litmap))

def term_alternatives(cx: hydra.context.Context, graph: hydra.graph.Graph, term: hydra.core.Term) -> Either[str, frozenlist[hydra.core.Term]]:
    r"""Find a list of alternatives for a given term, if any."""

    match term:
        case hydra.core.TermAnnotated(value=at):
            term2 = at.body
            return Right((term2,))

        case hydra.core.TermMaybe(value=ot):
            return Right((cast(hydra.core.Term, hydra.core.TermList(hydra.lib.maybes.maybe((lambda : ()), (lambda term2: (term2,)), ot))),))

        case hydra.core.TermTypeLambda(value=abs):
            term2 = abs.body
            return Right((term2,))

        case hydra.core.TermTypeApplication(value=ta):
            term2 = ta.body
            return Right((term2,))

        case hydra.core.TermUnion(value=inj):
            tname = inj.type_name
            field = inj.field
            fname = field.name
            fterm = field.term
            def for_field_type(ft: hydra.core.FieldType) -> hydra.core.Field:
                ftname = ft.name
                return hydra.core.Field(fname, cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.logic.if_else(hydra.lib.equality.equal(ftname, fname), (lambda : Just(fterm)), (lambda : Nothing())))))
            return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda ic: hydra.show.errors.error(ic.object)), (lambda x: x), hydra.resolution.require_union_type(cx, graph, tname)), (lambda rt: Right((cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(tname, hydra.lib.lists.map((lambda x1: for_field_type(x1)), rt)))),))))

        case hydra.core.TermUnit():
            return Right((cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(True)))),))

        case hydra.core.TermWrap(value=wt):
            term2 = wt.body
            return Right((term2,))

        case _:
            return Right(())

def adapt_term(constraints: hydra.coders.LanguageConstraints, litmap: FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType], cx: hydra.context.Context, graph: hydra.graph.Graph, term0: hydra.core.Term) -> Either[str, hydra.core.Term]:
    r"""Adapt a term using the given language constraints."""

    def rewrite(recurse: Callable[[T0], Either[str, hydra.core.Term]], term02: T0):
        def for_supported(term: hydra.core.Term) -> Either[T1, Maybe[hydra.core.Term]]:
            match term:
                case hydra.core.TermLiteral(value=l):
                    @lru_cache(1)
                    def lt() -> hydra.core.LiteralType:
                        return hydra.reflect.literal_type(l)
                    return Right(Just(hydra.lib.logic.if_else(literal_type_supported(constraints, lt()), (lambda : term), (lambda : cast(hydra.core.Term, hydra.core.TermLiteral(adapt_literal_value(litmap, lt(), l)))))))

                case _:
                    return Right(Just(term))
        def for_unsupported(term: hydra.core.Term) -> Either[str, Maybe[hydra.core.Term]]:
            def for_non_null(alts: frozenlist[hydra.core.Term]) -> Either[str, Maybe[hydra.core.Term]]:
                return hydra.lib.eithers.bind(try_term(hydra.lib.lists.head(alts)), (lambda mterm: hydra.lib.maybes.maybe((lambda : try_alts(hydra.lib.lists.tail(alts))), (lambda t: Right(Just(t))), mterm)))
            def try_alts(alts: frozenlist[hydra.core.Term]) -> Either[str, Maybe[hydra.core.Term]]:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(alts), (lambda : Right(Nothing())), (lambda : for_non_null(alts)))
            return hydra.lib.eithers.bind(term_alternatives(cx, graph, term), (lambda alts0: try_alts(alts0)))
        def try_term(term: hydra.core.Term) -> Either[str, Maybe[hydra.core.Term]]:
            @lru_cache(1)
            def supported_variant() -> bool:
                return hydra.lib.sets.member(hydra.reflect.term_variant(term), constraints.term_variants)
            return hydra.lib.logic.if_else(supported_variant(), (lambda : for_supported(term)), (lambda : for_unsupported(term)))
        def _hoist_for_supported_body_1(term1, v1):
            match v1:
                case hydra.core.TermTypeApplication(value=ta):
                    return hydra.lib.eithers.bind(adapt_type(constraints, litmap, ta.type), (lambda atyp: Right(cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(ta.body, atyp))))))

                case hydra.core.TermTypeLambda():
                    return Right(term1)

                case _:
                    return hydra.lib.eithers.bind(try_term(term1), (lambda mterm: hydra.lib.maybes.maybe((lambda : Left(hydra.lib.strings.cat2("no alternatives for term: ", hydra.show.core.term(term1)))), (lambda term2: Right(term2)), mterm)))
        return hydra.lib.eithers.bind(recurse(term02), (lambda term1: _hoist_for_supported_body_1(term1, term1)))
    return hydra.rewriting.rewrite_term_m((lambda x1, x2: rewrite(x1, x2)), term0)

def push_type_apps_inward(term: hydra.core.Term) -> hydra.core.Term:
    r"""Normalize a term by pushing TermTypeApplication inward past TermApplication and TermFunction (Lambda). This corrects structures produced by poly-let hoisting and eta expansion, where type applications from inference end up wrapping term applications or lambda abstractions instead of being directly on the polymorphic variable."""

    def push(body: hydra.core.Term, typ: hydra.core.Type):
        def _hoist_push_1(f, typ, v1):
            match v1:
                case hydra.core.FunctionLambda(value=l):
                    return go(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, l.domain, cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(l.body, typ)))))))))

                case _:
                    return cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(cast(hydra.core.Term, hydra.core.TermFunction(f)), typ)))
        match body:
            case hydra.core.TermApplication(value=a):
                return go(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(a.function, typ))), a.argument))))

            case hydra.core.TermFunction(value=f):
                return _hoist_push_1(f, typ, f)

            case hydra.core.TermLet(value=lt):
                return go(cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(lt.bindings, cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(lt.body, typ)))))))

            case _:
                return cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(body, typ)))
    def go(t: hydra.core.Term) -> hydra.core.Term:
        def for_field(fld: hydra.core.Field) -> hydra.core.Field:
            return hydra.core.Field(fld.name, go(fld.term))
        def for_elimination(elm: hydra.core.Elimination) -> hydra.core.Elimination:
            match elm:
                case hydra.core.EliminationRecord(value=p):
                    return cast(hydra.core.Elimination, hydra.core.EliminationRecord(p))

                case hydra.core.EliminationUnion(value=cs):
                    return cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(cs.type_name, hydra.lib.maybes.map((lambda x1: go(x1)), cs.default), hydra.lib.lists.map((lambda x1: for_field(x1)), cs.cases))))

                case hydra.core.EliminationWrap(value=name):
                    return cast(hydra.core.Elimination, hydra.core.EliminationWrap(name))

                case _:
                    raise AssertionError("Unreachable: all variants handled")
        def for_function(fun: hydra.core.Function) -> hydra.core.Function:
            match fun:
                case hydra.core.FunctionElimination(value=elm):
                    return cast(hydra.core.Function, hydra.core.FunctionElimination(for_elimination(elm)))

                case hydra.core.FunctionLambda(value=l):
                    return cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, l.domain, go(l.body))))

                case hydra.core.FunctionPrimitive(value=name):
                    return cast(hydra.core.Function, hydra.core.FunctionPrimitive(name))

                case _:
                    raise AssertionError("Unreachable: all variants handled")
        def for_let(lt: hydra.core.Let) -> hydra.core.Let:
            def map_binding(b: hydra.core.Binding) -> hydra.core.Binding:
                return hydra.core.Binding(b.name, go(b.term), b.type)
            return hydra.core.Let(hydra.lib.lists.map((lambda x1: map_binding(x1)), lt.bindings), go(lt.body))
        def for_map(m: FrozenDict[hydra.core.Term, hydra.core.Term]) -> FrozenDict[hydra.core.Term, hydra.core.Term]:
            def for_pair(p: tuple[hydra.core.Term, hydra.core.Term]) -> tuple[hydra.core.Term, hydra.core.Term]:
                return (go(hydra.lib.pairs.first(p)), go(hydra.lib.pairs.second(p)))
            return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda x1: for_pair(x1)), hydra.lib.maps.to_list(m)))
        match t:
            case hydra.core.TermAnnotated(value=at):
                return cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(go(at.body), at.annotation)))

            case hydra.core.TermApplication(value=a):
                return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(go(a.function), go(a.argument))))

            case hydra.core.TermEither(value=e):
                return cast(hydra.core.Term, hydra.core.TermEither(hydra.lib.eithers.either((lambda l: Left(go(l))), (lambda r: Right(go(r))), e)))

            case hydra.core.TermFunction(value=fun):
                return cast(hydra.core.Term, hydra.core.TermFunction(for_function(fun)))

            case hydra.core.TermLet(value=lt):
                return cast(hydra.core.Term, hydra.core.TermLet(for_let(lt)))

            case hydra.core.TermList(value=els):
                return cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: go(x1)), els)))

            case hydra.core.TermLiteral(value=v):
                return cast(hydra.core.Term, hydra.core.TermLiteral(v))

            case hydra.core.TermMap(value=m):
                return cast(hydra.core.Term, hydra.core.TermMap(for_map(m)))

            case hydra.core.TermMaybe(value=m2):
                return cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map((lambda x1: go(x1)), m2)))

            case hydra.core.TermPair(value=p):
                return cast(hydra.core.Term, hydra.core.TermPair((go(hydra.lib.pairs.first(p)), go(hydra.lib.pairs.second(p)))))

            case hydra.core.TermRecord(value=r):
                return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(r.type_name, hydra.lib.lists.map((lambda x1: for_field(x1)), r.fields))))

            case hydra.core.TermSet(value=s):
                return cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(hydra.lib.lists.map((lambda x1: go(x1)), hydra.lib.sets.to_list(s)))))

            case hydra.core.TermTypeApplication(value=tt):
                @lru_cache(1)
                def body1() -> hydra.core.Term:
                    return go(tt.body)
                return push(body1(), tt.type)

            case hydra.core.TermTypeLambda(value=ta):
                return cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(ta.parameter, go(ta.body))))

            case hydra.core.TermUnion(value=i):
                return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(i.type_name, for_field(i.field))))

            case hydra.core.TermUnit():
                return cast(hydra.core.Term, hydra.core.TermUnit())

            case hydra.core.TermVariable(value=v2):
                return cast(hydra.core.Term, hydra.core.TermVariable(v2))

            case hydra.core.TermWrap(value=wt):
                return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(wt.type_name, go(wt.body))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return go(term)

def adapt_data_graph(constraints: hydra.coders.LanguageConstraints, do_expand: bool, els0: frozenlist[hydra.core.Binding], cx: hydra.context.Context, graph0: hydra.graph.Graph) -> Either[str, tuple[hydra.graph.Graph, frozenlist[hydra.core.Binding]]]:
    r"""Adapt a graph and its schema to the given language constraints. The doExpand flag controls eta expansion of partial applications. Adaptation is type-preserving: binding-level TypeSchemes are adapted (not stripped). Note: case statement hoisting is done separately, prior to adaptation. The els0 parameter provides the original ordered bindings. Returns both the adapted graph and the ordered adapted bindings."""

    def transform(g: hydra.graph.Graph, gterm: hydra.core.Term) -> hydra.core.Term:
        tx = g
        @lru_cache(1)
        def gterm1() -> hydra.core.Term:
            return hydra.variables.unshadow_variables(push_type_apps_inward(gterm))
        @lru_cache(1)
        def gterm2() -> hydra.core.Term:
            return hydra.variables.unshadow_variables(hydra.lib.logic.if_else(do_expand, (lambda : push_type_apps_inward(hydra.reduction.eta_expand_term_new(tx, gterm1()))), (lambda : gterm1())))
        return hydra.dependencies.lift_lambda_above_let(gterm2())
    @lru_cache(1)
    def litmap() -> FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType]:
        return adapt_literal_types_map(constraints)
    prims0 = graph0.primitives
    schema_types0 = graph0.schema_types
    @lru_cache(1)
    def schema_bindings() -> frozenlist[hydra.core.Binding]:
        return hydra.environment.types_to_elements(hydra.lib.maps.map((lambda ts: hydra.scoping.type_scheme_to_f_type(ts)), schema_types0))
    return hydra.lib.eithers.bind(hydra.lib.logic.if_else(hydra.lib.maps.null(schema_types0), (lambda : Right(hydra.lib.maps.empty())), (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda ic: ic.object.value), (lambda x: x), hydra.environment.graph_as_types(cx, graph0, schema_bindings())), (lambda tmap0: hydra.lib.eithers.bind(adapt_graph_schema(constraints, litmap(), tmap0), (lambda tmap1: Right(hydra.lib.maps.map((lambda t: hydra.resolution.type_to_type_scheme(t)), tmap1)))))))), (lambda schema_result: (adapted_schema_types := schema_result, gterm0 := cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(els0, cast(hydra.core.Term, hydra.core.TermUnit())))), gterm1 := hydra.lib.logic.if_else(do_expand, (lambda : transform(graph0, gterm0)), (lambda : gterm0)), hydra.lib.eithers.bind(adapt_term(constraints, litmap(), cx, graph0, gterm1), (lambda gterm2: hydra.lib.eithers.bind(hydra.rewriting.rewrite_term_m((lambda v1, v2: adapt_lambda_domains(constraints, litmap(), v1, v2)), gterm2), (lambda gterm3: (els1_raw := hydra.environment.term_as_bindings(gterm3), process_binding := (lambda el: hydra.lib.eithers.bind(hydra.rewriting.rewrite_term_m((lambda v1, v2: adapt_nested_types(constraints, litmap(), v1, v2)), el.term), (lambda new_term: hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda ts: hydra.lib.eithers.bind(adapt_type_scheme(constraints, litmap(), ts), (lambda ts1: Right(Just(ts1))))), el.type), (lambda adapted_type: Right(hydra.core.Binding(el.name, new_term, adapted_type))))))), hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: process_binding(x1)), els1_raw), (lambda els1: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda kv: hydra.lib.eithers.bind(adapt_primitive(constraints, litmap(), hydra.lib.pairs.second(kv)), (lambda prim1: Right((hydra.lib.pairs.first(kv), prim1))))), hydra.lib.maps.to_list(prims0)), (lambda prim_pairs: (prims1 := hydra.lib.maps.from_list(prim_pairs), adapted_graph_raw := hydra.lexical.build_graph(els1, hydra.lib.maps.empty(), prims1), adapted_graph := hydra.graph.Graph(adapted_graph_raw.bound_terms, adapted_graph_raw.bound_types, adapted_graph_raw.class_constraints, adapted_graph_raw.lambda_variables, adapted_graph_raw.metadata, adapted_graph_raw.primitives, adapted_schema_types, adapted_graph_raw.type_variables), Right((adapted_graph, els1)))[3])))))[2])))))[3]))

def adapt_term_for_language(lang: hydra.coders.Language, cx: hydra.context.Context, g: hydra.graph.Graph, term: hydra.core.Term) -> Either[str, hydra.core.Term]:
    r"""Adapt a term using the constraints of a given language."""

    constraints = lang.constraints
    @lru_cache(1)
    def litmap() -> FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType]:
        return adapt_literal_types_map(constraints)
    return adapt_term(constraints, litmap(), cx, g, term)

def adapt_type_for_language(lang: hydra.coders.Language, typ: hydra.core.Type) -> Either[str, hydra.core.Type]:
    r"""Adapt a type using the constraints of a given language."""

    constraints = lang.constraints
    @lru_cache(1)
    def litmap() -> FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType]:
        return adapt_literal_types_map(constraints)
    return adapt_type(constraints, litmap(), typ)

def compose_coders(c1: hydra.coders.Coder[T0, T1], c2: hydra.coders.Coder[T1, T2]) -> hydra.coders.Coder[T0, T2]:
    r"""Compose two coders into a single coder."""

    return hydra.coders.Coder((lambda cx, a: hydra.lib.eithers.bind(c1.encode(cx, a), (lambda b1: c2.encode(cx, b1)))), (lambda cx, c: hydra.lib.eithers.bind(c2.decode(cx, c), (lambda b2: c1.decode(cx, b2)))))

def data_graph_to_definitions(constraints: hydra.coders.LanguageConstraints, do_infer: bool, do_expand: bool, do_hoist_case_statements: bool, do_hoist_polymorphic_let_bindings: bool, original_bindings: frozenlist[hydra.core.Binding], graph0: hydra.graph.Graph, namespaces: frozenlist[hydra.module.Namespace], cx: hydra.context.Context) -> Either[str, tuple[hydra.graph.Graph, frozenlist[frozenlist[hydra.module.TermDefinition]]]]:
    r"""Given a data graph along with language constraints, original ordered bindings, and a designated list of namespaces, adapt the graph to the language constraints, then return the processed graph along with term definitions grouped by namespace (in the order of the input namespaces). Inference is performed before adaptation if bindings lack type annotations. Hoisting must preserve type schemes; if any binding loses its type scheme after hoisting, the pipeline fails. Adaptation preserves type application/lambda wrappers and adapts embedded types. Post-adaptation inference is performed to ensure binding TypeSchemes are fully consistent. The doExpand flag controls eta expansion. The doHoistCaseStatements flag controls case statement hoisting (needed for Python). The doHoistPolymorphicLetBindings flag controls polymorphic let binding hoisting (needed for Java). The originalBindings parameter provides the original ordered bindings (from module elements)."""

    @lru_cache(1)
    def namespaces_set() -> frozenset[hydra.module.Namespace]:
        return hydra.lib.sets.from_list(namespaces)
    def is_parent_binding(b: hydra.core.Binding) -> bool:
        return hydra.lib.maybes.maybe((lambda : False), (lambda ns: hydra.lib.sets.member(ns, namespaces_set())), hydra.names.namespace_of(b.name))
    def hoist_cases(bindings: frozenlist[hydra.core.Binding]) -> frozenlist[hydra.core.Binding]:
        @lru_cache(1)
        def stripped() -> frozenlist[hydra.core.Binding]:
            return hydra.lib.lists.map((lambda b: hydra.core.Binding(b.name, hydra.strip.strip_type_lambdas(b.term), b.type)), bindings)
        @lru_cache(1)
        def term0() -> hydra.core.Term:
            return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(stripped(), cast(hydra.core.Term, hydra.core.TermUnit()))))
        @lru_cache(1)
        def unshadowed0() -> frozenlist[hydra.core.Binding]:
            return hydra.environment.term_as_bindings(hydra.variables.unshadow_variables(term0()))
        @lru_cache(1)
        def hoisted() -> frozenlist[hydra.core.Binding]:
            return hydra.hoisting.hoist_case_statements_in_graph(unshadowed0())
        @lru_cache(1)
        def term1() -> hydra.core.Term:
            return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hoisted(), cast(hydra.core.Term, hydra.core.TermUnit()))))
        return hydra.environment.term_as_bindings(hydra.variables.unshadow_variables(term1()))
    def hoist_poly(bindings: frozenlist[hydra.core.Binding]) -> frozenlist[hydra.core.Binding]:
        @lru_cache(1)
        def let_before() -> hydra.core.Let:
            return hydra.core.Let(bindings, cast(hydra.core.Term, hydra.core.TermUnit()))
        @lru_cache(1)
        def let_after() -> hydra.core.Let:
            return hydra.hoisting.hoist_polymorphic_let_bindings((lambda x1: is_parent_binding(x1)), let_before())
        return let_after().bindings
    def check_bindings_typed(debug_label: str, bindings: frozenlist[hydra.core.Binding]) -> Either[str, frozenlist[hydra.core.Binding]]:
        @lru_cache(1)
        def untyped_bindings() -> frozenlist[str]:
            return hydra.lib.lists.map((lambda b: b.name.value), hydra.lib.lists.filter((lambda b: hydra.lib.logic.not_(hydra.lib.maybes.is_just(b.type))), bindings))
        return hydra.lib.logic.if_else(hydra.lib.lists.null(untyped_bindings()), (lambda : Right(bindings)), (lambda : Left(hydra.lib.strings.cat(("Found untyped bindings (", debug_label, "): ", hydra.lib.strings.intercalate(", ", untyped_bindings()))))))
    def normalize_bindings(bindings: frozenlist[hydra.core.Binding]) -> frozenlist[hydra.core.Binding]:
        return hydra.lib.lists.map((lambda b: hydra.core.Binding(b.name, push_type_apps_inward(b.term), b.type)), bindings)
    def rebuild_graph(bindings: frozenlist[hydra.core.Binding]) -> hydra.graph.Graph:
        @lru_cache(1)
        def g() -> hydra.graph.Graph:
            return hydra.lexical.build_graph(bindings, hydra.lib.maps.empty(), graph0.primitives)
        return hydra.graph.Graph(g().bound_terms, g().bound_types, g().class_constraints, g().lambda_variables, g().metadata, g().primitives, graph0.schema_types, g().type_variables)
    bins0 = original_bindings
    @lru_cache(1)
    def bins1() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.logic.if_else(do_hoist_case_statements, (lambda : hoist_cases(bins0)), (lambda : bins0))
    return hydra.lib.eithers.bind(hydra.lib.logic.if_else(do_infer, (lambda : hydra.lib.eithers.map((lambda result: hydra.lib.pairs.second(hydra.lib.pairs.first(result))), hydra.lib.eithers.bimap((lambda ic: hydra.show.errors.error(ic.object)), (lambda x: x), hydra.inference.infer_graph_types(cx, bins1(), rebuild_graph(bins1()))))), (lambda : check_bindings_typed("after case hoisting", bins1()))), (lambda bins2: hydra.lib.eithers.bind(hydra.lib.logic.if_else(do_hoist_polymorphic_let_bindings, (lambda : check_bindings_typed("after let hoisting", hoist_poly(bins2))), (lambda : Right(bins2))), (lambda bins3: hydra.lib.eithers.bind(adapt_data_graph(constraints, do_expand, bins3, cx, rebuild_graph(bins3)), (lambda adapt_result: (adapted := hydra.lib.pairs.first(adapt_result), adapted_bindings := hydra.lib.pairs.second(adapt_result), hydra.lib.eithers.bind(check_bindings_typed("after adaptation", adapted_bindings), (lambda bins4: (bins5 := normalize_bindings(bins4), to_def := (lambda el: hydra.lib.maybes.map((lambda ts: hydra.module.TermDefinition(el.name, el.term, Just(ts))), el.type)), selected_elements := hydra.lib.lists.filter((lambda el: hydra.lib.maybes.maybe((lambda : False), (lambda ns: hydra.lib.sets.member(ns, namespaces_set())), hydra.names.namespace_of(el.name))), bins5), elements_by_namespace := hydra.lib.lists.foldl((lambda acc, el: hydra.lib.maybes.maybe((lambda : acc), (lambda ns: (existing := hydra.lib.maybes.maybe((lambda : ()), (lambda x1: hydra.lib.equality.identity(x1)), hydra.lib.maps.lookup(ns, acc)), hydra.lib.maps.insert(ns, hydra.lib.lists.concat2(existing, (el,)), acc))[1]), hydra.names.namespace_of(el.name))), hydra.lib.maps.empty(), selected_elements), defs_grouped := hydra.lib.lists.map((lambda ns: (els_for_ns := hydra.lib.maybes.maybe((lambda : ()), (lambda x1: hydra.lib.equality.identity(x1)), hydra.lib.maps.lookup(ns, elements_by_namespace)), hydra.lib.maybes.cat(hydra.lib.lists.map((lambda x1: to_def(x1)), els_for_ns)))[1]), namespaces), g := hydra.lexical.build_graph(bins5, hydra.lib.maps.empty(), adapted.primitives), Right((hydra.graph.Graph(g.bound_terms, g.bound_types, g.class_constraints, g.lambda_variables, g.metadata, g.primitives, adapted.schema_types, g.type_variables), defs_grouped)))[6])))[2]))))))

def prepare_same(x: T0) -> tuple[T0, tuple[Callable[[T1], T1], frozenset[T2]]]:
    r"""Return a value unchanged with identity transform and no messages."""

    return (x, ((lambda y: y), hydra.lib.sets.empty()))

def prepare_float_type(ft: hydra.core.FloatType):
    def _hoist_hydra_adapt_prepare_float_type_1(v, v1):
        match v1:
            case hydra.core.FloatValueBigfloat(value=d):
                return cast(hydra.core.FloatValue, hydra.core.FloatValueFloat64(hydra.lib.literals.bigfloat_to_float64(d)))

            case _:
                return v
    match ft:
        case hydra.core.FloatType.BIGFLOAT:
            return (hydra.core.FloatType.FLOAT64, ((lambda v: _hoist_hydra_adapt_prepare_float_type_1(v, v)), hydra.lib.sets.from_list(("replace arbitrary-precision floating-point numbers with 64-bit floating-point numbers (doubles)",))))

        case _:
            return prepare_same(ft)

def prepare_integer_type(it: hydra.core.IntegerType):
    def _hoist_hydra_adapt_prepare_integer_type_1(v, v1):
        match v1:
            case hydra.core.IntegerValueBigint(value=i):
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt64(hydra.lib.literals.bigint_to_int64(i)))

            case _:
                return v
    def _hoist_hydra_adapt_prepare_integer_type_2(v, v1):
        match v1:
            case hydra.core.IntegerValueUint8(value=i):
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt8(hydra.lib.literals.bigint_to_int8(hydra.lib.literals.uint8_to_bigint(i))))

            case _:
                return v
    def _hoist_hydra_adapt_prepare_integer_type_3(v, v1):
        match v1:
            case hydra.core.IntegerValueUint32(value=i):
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(hydra.lib.literals.bigint_to_int32(hydra.lib.literals.uint32_to_bigint(i))))

            case _:
                return v
    def _hoist_hydra_adapt_prepare_integer_type_4(v, v1):
        match v1:
            case hydra.core.IntegerValueUint64(value=i):
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt64(hydra.lib.literals.bigint_to_int64(hydra.lib.literals.uint64_to_bigint(i))))

            case _:
                return v
    match it:
        case hydra.core.IntegerType.BIGINT:
            return (hydra.core.IntegerType.INT64, ((lambda v: _hoist_hydra_adapt_prepare_integer_type_1(v, v)), hydra.lib.sets.from_list(("replace arbitrary-precision integers with 64-bit integers",))))

        case hydra.core.IntegerType.UINT8:
            return (hydra.core.IntegerType.INT8, ((lambda v: _hoist_hydra_adapt_prepare_integer_type_2(v, v)), hydra.lib.sets.from_list(("replace unsigned 8-bit integers with signed 8-bit integers",))))

        case hydra.core.IntegerType.UINT32:
            return (hydra.core.IntegerType.INT32, ((lambda v: _hoist_hydra_adapt_prepare_integer_type_3(v, v)), hydra.lib.sets.from_list(("replace unsigned 32-bit integers with signed 32-bit integers",))))

        case hydra.core.IntegerType.UINT64:
            return (hydra.core.IntegerType.INT64, ((lambda v: _hoist_hydra_adapt_prepare_integer_type_4(v, v)), hydra.lib.sets.from_list(("replace unsigned 64-bit integers with signed 64-bit integers",))))

        case _:
            return prepare_same(it)

def prepare_literal_type(at: hydra.core.LiteralType):
    def _hoist_hydra_adapt_prepare_literal_type_1(v, v1):
        match v1:
            case hydra.core.LiteralBinary(value=b):
                return cast(hydra.core.Literal, hydra.core.LiteralString(hydra.lib.literals.binary_to_string(b)))

            case _:
                return v
    match at:
        case hydra.core.LiteralTypeBinary():
            return (cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()), ((lambda v: _hoist_hydra_adapt_prepare_literal_type_1(v, v)), hydra.lib.sets.from_list(("replace binary strings with character strings",))))

        case hydra.core.LiteralTypeFloat(value=ft):
            @lru_cache(1)
            def result() -> tuple[hydra.core.FloatType, tuple[Callable[[hydra.core.FloatValue], hydra.core.FloatValue], frozenset[str]]]:
                return prepare_float_type(ft)
            @lru_cache(1)
            def rtyp() -> hydra.core.FloatType:
                return hydra.lib.pairs.first(result())
            @lru_cache(1)
            def rep() -> Callable[[hydra.core.FloatValue], hydra.core.FloatValue]:
                return hydra.lib.pairs.first(hydra.lib.pairs.second(result()))
            @lru_cache(1)
            def msgs() -> frozenset[str]:
                return hydra.lib.pairs.second(hydra.lib.pairs.second(result()))
            def _hoist_result_body_1(v, v1):
                match v1:
                    case hydra.core.LiteralFloat(value=fv):
                        return cast(hydra.core.Literal, hydra.core.LiteralFloat(rep(fv)))

                    case _:
                        return v
            return (cast(hydra.core.LiteralType, hydra.core.LiteralTypeFloat(rtyp())), ((lambda v: _hoist_result_body_1(v, v)), msgs()))

        case hydra.core.LiteralTypeInteger(value=it):
            @lru_cache(1)
            def result() -> tuple[hydra.core.IntegerType, tuple[Callable[[hydra.core.IntegerValue], hydra.core.IntegerValue], frozenset[str]]]:
                return prepare_integer_type(it)
            @lru_cache(1)
            def rtyp() -> hydra.core.IntegerType:
                return hydra.lib.pairs.first(result())
            @lru_cache(1)
            def rep() -> Callable[[hydra.core.IntegerValue], hydra.core.IntegerValue]:
                return hydra.lib.pairs.first(hydra.lib.pairs.second(result()))
            @lru_cache(1)
            def msgs() -> frozenset[str]:
                return hydra.lib.pairs.second(hydra.lib.pairs.second(result()))
            def _hoist_result_body_1(v, v1):
                match v1:
                    case hydra.core.LiteralInteger(value=iv):
                        return cast(hydra.core.Literal, hydra.core.LiteralInteger(rep(iv)))

                    case _:
                        return v
            return (cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(rtyp())), ((lambda v: _hoist_result_body_1(v, v)), msgs()))

        case _:
            return prepare_same(at)

def prepare_type(cx: T0, typ: hydra.core.Type):
    r"""Prepare a type, substituting unsupported literal types."""

    match hydra.strip.deannotate_type(typ):
        case hydra.core.TypeLiteral(value=at):
            @lru_cache(1)
            def result() -> tuple[hydra.core.LiteralType, tuple[Callable[[hydra.core.Literal], hydra.core.Literal], frozenset[str]]]:
                return prepare_literal_type(at)
            @lru_cache(1)
            def rtyp() -> hydra.core.LiteralType:
                return hydra.lib.pairs.first(result())
            @lru_cache(1)
            def rep() -> Callable[[hydra.core.Literal], hydra.core.Literal]:
                return hydra.lib.pairs.first(hydra.lib.pairs.second(result()))
            @lru_cache(1)
            def msgs() -> frozenset[str]:
                return hydra.lib.pairs.second(hydra.lib.pairs.second(result()))
            def _hoist_result_body_1(v, v1):
                match v1:
                    case hydra.core.TermLiteral(value=av):
                        return cast(hydra.core.Term, hydra.core.TermLiteral(rep(av)))

                    case _:
                        return v
            return (cast(hydra.core.Type, hydra.core.TypeLiteral(rtyp())), ((lambda v: _hoist_result_body_1(v, v)), msgs()))

        case _:
            return prepare_same(typ)

def schema_graph_to_definitions(constraints: hydra.coders.LanguageConstraints, graph: hydra.graph.Graph, name_lists: frozenlist[frozenlist[hydra.core.Name]], cx: hydra.context.Context) -> Either[str, tuple[FrozenDict[hydra.core.Name, hydra.core.Type], frozenlist[frozenlist[hydra.module.TypeDefinition]]]]:
    r"""Given a schema graph along with language constraints and a designated list of element names, adapt the graph to the language constraints, then return a corresponding type definition for each element name."""

    @lru_cache(1)
    def litmap() -> FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType]:
        return adapt_literal_types_map(constraints)
    return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda ic: ic.object.value), (lambda x: x), hydra.environment.graph_as_types(cx, graph, hydra.lexical.graph_to_bindings(graph))), (lambda tmap0: hydra.lib.eithers.bind(adapt_graph_schema(constraints, litmap(), tmap0), (lambda tmap1: (to_def := (lambda pair: hydra.module.TypeDefinition(hydra.lib.pairs.first(pair), hydra.lib.pairs.second(pair))), Right((tmap1, hydra.lib.lists.map((lambda names: hydra.lib.lists.map((lambda x1: to_def(x1)), hydra.lib.lists.map((lambda n: (n, hydra.lib.maybes.from_just(hydra.lib.maps.lookup(n, tmap1)))), names))), name_lists))))[1]))))

def simple_language_adapter(lang: hydra.coders.Language, cx: T0, g: hydra.graph.Graph, typ: hydra.core.Type) -> Either[str, hydra.coders.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Given a target language and a source type, produce an adapter which rewrites the type and its terms according to the language's constraints. The encode direction adapts terms; the decode direction is identity."""

    constraints = lang.constraints
    @lru_cache(1)
    def litmap() -> FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType]:
        return adapt_literal_types_map(constraints)
    return hydra.lib.eithers.bind(adapt_type(constraints, litmap(), typ), (lambda adapted_type: Right(hydra.coders.Adapter(False, typ, adapted_type, hydra.coders.Coder((lambda cx2, term: hydra.lib.eithers.bimap((lambda _s: hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_s))), cx2)), (lambda _x: _x), adapt_term(constraints, litmap(), cx2, g, term))), (lambda cx2, term: Right(term)))))))
