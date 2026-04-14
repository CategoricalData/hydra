# Note: this is an automatically generated file. Do not edit.

r"""Java code generator: converts Hydra modules to Java source code."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.analysis
import hydra.annotations
import hydra.arity
import hydra.checking
import hydra.constants
import hydra.core
import hydra.decode.core
import hydra.dependencies
import hydra.encode.core
import hydra.environment
import hydra.errors
import hydra.formatting
import hydra.graph
import hydra.java.environment
import hydra.java.language
import hydra.java.names
import hydra.java.serde
import hydra.java.syntax
import hydra.java.utils
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.names
import hydra.packaging
import hydra.predicates
import hydra.resolution
import hydra.rewriting
import hydra.scoping
import hydra.serialization
import hydra.show.core
import hydra.sorting
import hydra.strip
import hydra.typing
import hydra.util
import hydra.variables

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")
T4 = TypeVar("T4")

def add_comment(decl: hydra.java.syntax.ClassBodyDeclaration, field: hydra.core.FieldType, cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.ClassBodyDeclarationWithComments]:
    return hydra.lib.eithers.map((lambda c: hydra.java.syntax.ClassBodyDeclarationWithComments(decl, c)), hydra.annotations.comments_from_field_type(cx, g, field))

def java_env_get_graph(env: hydra.java.environment.JavaEnvironment) -> hydra.graph.Graph:
    return env.graph

def java_env_set_graph(g: hydra.graph.Graph, env: hydra.java.environment.JavaEnvironment) -> hydra.java.environment.JavaEnvironment:
    return hydra.java.environment.JavaEnvironment(env.aliases, g)

def analyze_java_function(env: hydra.java.environment.JavaEnvironment, term: hydra.core.Term, cx: hydra.context.Context, g: T0) -> Either[T1, hydra.typing.FunctionStructure[hydra.java.environment.JavaEnvironment]]:
    return hydra.analysis.analyze_function_term(cx, (lambda x1: java_env_get_graph(x1)), (lambda x1, x2: java_env_set_graph(x1, x2)), env, term)

def extract_arg_type(_lhs: T0, typ: hydra.core.Type):
    def _hoist_hydra_java_coder_extract_arg_type_1(at1, typ, v1):
        match v1:
            case hydra.core.TypeApplication():
                return at1.argument

            case _:
                return typ
    match typ:
        case hydra.core.TypeApplication(value=at1):
            return _hoist_hydra_java_coder_extract_arg_type_1(at1, typ, at1.function)

        case _:
            return typ

def annotate_body_with_cod(typ: hydra.core.Type, term: hydra.core.Term) -> hydra.core.Term:
    def set_ann(t: hydra.core.Term) -> hydra.core.Term:
        return hydra.annotations.set_term_annotation(hydra.constants.key_type, Just(hydra.encode.core.type(typ)), t)
    match hydra.strip.deannotate_term(term):
        case hydra.core.TermTypeApplication():
            return set_ann(term)

        case hydra.core.TermApplication(value=app):
            lhs = app.function
            rhs = app.argument
            @lru_cache(1)
            def annotated_rhs():
                def _hoist_annotated_rhs_1(v1):
                    match v1:
                        case hydra.core.TermTypeApplication():
                            return annotate_body_with_cod(extract_arg_type(lhs, typ), rhs)

                        case _:
                            return rhs
                return _hoist_annotated_rhs_1(hydra.strip.deannotate_term(rhs))
            return set_ann(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(lhs, annotated_rhs()))))

        case _:
            return set_ann(term)

def collect_type_vars_go(t: hydra.core.Type) -> frozenset[hydra.core.Name]:
    match t:
        case hydra.core.TypeVariable(value=name):
            return hydra.lib.sets.singleton(name)

        case hydra.core.TypeFunction(value=ft):
            return hydra.lib.sets.union(collect_type_vars_go(hydra.strip.deannotate_type(ft.domain)), collect_type_vars_go(hydra.strip.deannotate_type(ft.codomain)))

        case hydra.core.TypeApplication(value=at):
            return hydra.lib.sets.union(collect_type_vars_go(hydra.strip.deannotate_type(at.function)), collect_type_vars_go(hydra.strip.deannotate_type(at.argument)))

        case hydra.core.TypeList(value=inner):
            return collect_type_vars_go(hydra.strip.deannotate_type(inner))

        case hydra.core.TypeSet(value=inner2):
            return collect_type_vars_go(hydra.strip.deannotate_type(inner2))

        case hydra.core.TypeMaybe(value=inner3):
            return collect_type_vars_go(hydra.strip.deannotate_type(inner3))

        case hydra.core.TypeMap(value=mt):
            return hydra.lib.sets.union(collect_type_vars_go(hydra.strip.deannotate_type(mt.keys)), collect_type_vars_go(hydra.strip.deannotate_type(mt.values)))

        case hydra.core.TypePair(value=pt):
            return hydra.lib.sets.union(collect_type_vars_go(hydra.strip.deannotate_type(pt.first)), collect_type_vars_go(hydra.strip.deannotate_type(pt.second)))

        case hydra.core.TypeEither(value=et):
            return hydra.lib.sets.union(collect_type_vars_go(hydra.strip.deannotate_type(et.left)), collect_type_vars_go(hydra.strip.deannotate_type(et.right)))

        case hydra.core.TypeForall(value=ft2):
            return collect_type_vars_go(hydra.strip.deannotate_type(ft2.body))

        case _:
            return hydra.lib.sets.empty()

def collect_type_vars(typ: hydra.core.Type) -> frozenset[hydra.core.Name]:
    return collect_type_vars_go(hydra.strip.deannotate_type(typ))

def apply_subst_full(s: FrozenDict[hydra.core.Name, hydra.core.Type], t: hydra.core.Type) -> hydra.core.Type:
    match hydra.strip.deannotate_type(t):
        case hydra.core.TypeVariable(value=v):
            return hydra.lib.maps.find_with_default(t, v, s)

        case hydra.core.TypeFunction(value=ft):
            return cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(apply_subst_full(s, ft.domain), apply_subst_full(s, ft.codomain))))

        case hydra.core.TypeApplication(value=at):
            return cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(apply_subst_full(s, at.function), apply_subst_full(s, at.argument))))

        case hydra.core.TypeList(value=inner):
            return cast(hydra.core.Type, hydra.core.TypeList(apply_subst_full(s, inner)))

        case hydra.core.TypeSet(value=inner2):
            return cast(hydra.core.Type, hydra.core.TypeSet(apply_subst_full(s, inner2)))

        case hydra.core.TypeMaybe(value=inner3):
            return cast(hydra.core.Type, hydra.core.TypeMaybe(apply_subst_full(s, inner3)))

        case hydra.core.TypeMap(value=mt):
            return cast(hydra.core.Type, hydra.core.TypeMap(hydra.core.MapType(apply_subst_full(s, mt.keys), apply_subst_full(s, mt.values))))

        case hydra.core.TypePair(value=pt):
            return cast(hydra.core.Type, hydra.core.TypePair(hydra.core.PairType(apply_subst_full(s, pt.first), apply_subst_full(s, pt.second))))

        case hydra.core.TypeEither(value=et):
            return cast(hydra.core.Type, hydra.core.TypeEither(hydra.core.EitherType(apply_subst_full(s, et.left), apply_subst_full(s, et.right))))

        case hydra.core.TypeForall(value=ft2):
            return cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(ft2.parameter, apply_subst_full(hydra.lib.maps.delete(ft2.parameter, s), ft2.body))))

        case _:
            return t

def peel_expected_types(subst: FrozenDict[hydra.core.Name, hydra.core.Type], n: int, t: hydra.core.Type):
    def _hoist_hydra_java_coder_peel_expected_types_1(n, subst, v1):
        match v1:
            case hydra.core.TypeFunction(value=ft):
                return hydra.lib.lists.cons(apply_subst_full(subst, ft.domain), peel_expected_types(subst, hydra.lib.math.sub(n, 1), ft.codomain))

            case _:
                return ()
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(n, 0), (lambda : ()), (lambda : _hoist_hydra_java_coder_peel_expected_types_1(n, subst, hydra.strip.deannotate_type(t))))

def propagate_type_rebuild_let(t: hydra.core.Term, bindings: frozenlist[hydra.core.Binding], new_body: hydra.core.Term) -> hydra.core.Term:
    match t:
        case hydra.core.TermAnnotated(value=at):
            return cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(propagate_type_rebuild_let(at.body, bindings, new_body), at.annotation)))

        case hydra.core.TermLet():
            return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(bindings, new_body)))

        case _:
            return t

def propagate_type(typ: hydra.core.Type, term: hydra.core.Term):
    def set_type_ann(t: hydra.core.Term) -> hydra.core.Term:
        return hydra.annotations.set_term_annotation(hydra.constants.key_type, Just(hydra.encode.core.type(typ)), t)
    match hydra.strip.deannotate_term(term):
        case hydra.core.TermLambda():
            @lru_cache(1)
            def annotated() -> hydra.core.Term:
                return set_type_ann(term)
            def _hoist_annotated_body_1(v1):
                match v1:
                    case hydra.core.TypeFunction(value=ft):
                        return propagate_type_propagate_into_lambda(ft.codomain, annotated())

                    case _:
                        return annotated()
            return _hoist_annotated_body_1(hydra.strip.deannotate_type(typ))

        case hydra.core.TermLet(value=lt):
            @lru_cache(1)
            def propagated_bindings() -> frozenlist[hydra.core.Binding]:
                return hydra.lib.lists.map((lambda b: hydra.lib.maybes.maybe((lambda : b), (lambda ts: hydra.core.Binding(b.name, propagate_type(ts.type, b.term), b.type)), b.type)), lt.bindings)
            return set_type_ann(propagate_type_rebuild_let(term, propagated_bindings(), propagate_type(typ, lt.body)))

        case hydra.core.TermApplication(value=app):
            fun = app.function
            arg = app.argument
            @lru_cache(1)
            def annotated_fun():
                def _hoist_annotated_fun_1(v1):
                    match v1:
                        case hydra.core.TermCases(value=cs):
                            @lru_cache(1)
                            def dom() -> hydra.core.Type:
                                return hydra.resolution.nominal_application(cs.type_name, ())
                            @lru_cache(1)
                            def ft() -> hydra.core.Type:
                                return cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(dom(), typ)))
                            return hydra.annotations.set_term_annotation(hydra.constants.key_type, Just(hydra.encode.core.type(ft())), fun)

                        case _:
                            return fun
                return _hoist_annotated_fun_1(hydra.strip.deannotate_term(fun))
            return set_type_ann(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(annotated_fun(), arg))))

        case _:
            return set_type_ann(term)

def propagate_type_propagate_into_lambda(cod: hydra.core.Type, t: hydra.core.Term) -> hydra.core.Term:
    match t:
        case hydra.core.TermAnnotated(value=at):
            return cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(propagate_type_propagate_into_lambda(cod, at.body), at.annotation)))

        case hydra.core.TermLambda(value=lam):
            return cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(lam.parameter, lam.domain, propagate_type(cod, lam.body))))

        case _:
            return t

def annotate_lambda_args(cname: hydra.core.Name, t_apps: frozenlist[hydra.core.Type], arg_terms: frozenlist[hydra.core.Term], cx: T0, g: hydra.graph.Graph) -> Either[T1, frozenlist[hydra.core.Term]]:
    return hydra.lib.logic.if_else(hydra.lib.lists.null(t_apps), (lambda : Right(arg_terms)), (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.bind(Right(hydra.lexical.lookup_binding(g, cname)), (lambda mel: hydra.lib.maybes.cases(mel, (lambda : Right(hydra.lib.maybes.map((lambda prim: prim.type), hydra.lib.maps.lookup(cname, g.primitives)))), (lambda el: Right(el.type))))), (lambda mts: hydra.lib.maybes.cases(mts, (lambda : Right(arg_terms)), (lambda ts: (scheme_type := ts.type, scheme_type_vars := collect_type_vars(scheme_type), scheme_vars := hydra.lib.lists.filter((lambda v: hydra.lib.sets.member(v, scheme_type_vars)), ts.variables), hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.lib.lists.null(scheme_vars), hydra.lib.logic.not_(hydra.lib.equality.equal(hydra.lib.lists.length(scheme_vars), hydra.lib.lists.length(t_apps)))), (lambda : Right(arg_terms)), (lambda : (subst := hydra.lib.maps.from_list(hydra.lib.lists.zip(scheme_vars, t_apps)), (expected_types := peel_expected_types(subst, hydra.lib.lists.length(arg_terms), scheme_type), Right(hydra.lib.lists.zip_with((lambda arg, m_expected: propagate_type(m_expected, arg)), arg_terms, hydra.lib.lists.concat2(expected_types, hydra.lib.lists.replicate(hydra.lib.lists.length(arg_terms), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("unused"))))))))[1])[1])))[3]))))))

def encode_literal_type_simple(n: str, cx: T0, g: T1) -> Either[T2, hydra.java.syntax.Type]:
    return Right(hydra.java.utils.java_ref_type((), Nothing(), n))

def encode_literal_type(lt: hydra.core.LiteralType, cx: T0, g: T1):
    def _hoist_hydra_java_coder_encode_literal_type_1(cx, g, v1):
        match v1:
            case hydra.core.FloatType.BIGFLOAT:
                return Right(hydra.java.utils.java_ref_type((), Just(hydra.java.names.java_package_name(("java", "math"))), "BigDecimal"))

            case hydra.core.FloatType.FLOAT32:
                return encode_literal_type_simple("Float", cx, g)

            case hydra.core.FloatType.FLOAT64:
                return encode_literal_type_simple("Double", cx, g)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_hydra_java_coder_encode_literal_type_2(cx, g, v1):
        match v1:
            case hydra.core.IntegerType.BIGINT:
                return Right(hydra.java.utils.java_ref_type((), Just(hydra.java.names.java_package_name(("java", "math"))), "BigInteger"))

            case hydra.core.IntegerType.INT8:
                return encode_literal_type_simple("Byte", cx, g)

            case hydra.core.IntegerType.INT16:
                return encode_literal_type_simple("Short", cx, g)

            case hydra.core.IntegerType.INT32:
                return encode_literal_type_simple("Integer", cx, g)

            case hydra.core.IntegerType.INT64:
                return encode_literal_type_simple("Long", cx, g)

            case hydra.core.IntegerType.UINT8:
                return encode_literal_type_simple("Short", cx, g)

            case hydra.core.IntegerType.UINT16:
                return encode_literal_type_simple("Character", cx, g)

            case hydra.core.IntegerType.UINT32:
                return encode_literal_type_simple("Long", cx, g)

            case hydra.core.IntegerType.UINT64:
                return Right(hydra.java.utils.java_ref_type((), Just(hydra.java.names.java_package_name(("java", "math"))), "BigInteger"))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    match lt:
        case hydra.core.LiteralTypeBinary():
            return Right(cast(hydra.java.syntax.Type, hydra.java.syntax.TypeReference(cast(hydra.java.syntax.ReferenceType, hydra.java.syntax.ReferenceTypeArray(hydra.java.syntax.ArrayType(hydra.java.syntax.Dims(((),)), cast(hydra.java.syntax.ArrayType_Variant, hydra.java.syntax.ArrayType_VariantPrimitive(hydra.java.syntax.PrimitiveTypeWithAnnotations(cast(hydra.java.syntax.PrimitiveType, hydra.java.syntax.PrimitiveTypeNumeric(cast(hydra.java.syntax.NumericType, hydra.java.syntax.NumericTypeIntegral(hydra.java.syntax.IntegralType.BYTE)))), ())))))))))

        case hydra.core.LiteralTypeBoolean():
            return encode_literal_type_simple("Boolean", cx, g)

        case hydra.core.LiteralTypeFloat(value=ft):
            return _hoist_hydra_java_coder_encode_literal_type_1(cx, g, ft)

        case hydra.core.LiteralTypeInteger(value=it):
            return _hoist_hydra_java_coder_encode_literal_type_2(cx, g, it)

        case hydra.core.LiteralTypeString():
            return encode_literal_type_simple("String", cx, g)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def is_lambda_bound_variable(name: hydra.core.Name) -> bool:
    v = name.value
    return hydra.lib.equality.lte(hydra.lib.strings.length(v), 4)

def encode_type_resolve_if_typedef(aliases: T0, bound_vars: frozenset[hydra.core.Name], in_scope_type_params: frozenset[hydra.core.Name], name: hydra.core.Name, cx: T1, g: hydra.graph.Graph):
    return hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.lib.sets.member(name, bound_vars), hydra.lib.sets.member(name, in_scope_type_params)), (lambda : Right(Nothing())), (lambda : hydra.lib.logic.if_else(is_lambda_bound_variable(name), (lambda : Right(Nothing())), (lambda : (schema_types := g.schema_types, (_hoist_schema_types_body_1 := (lambda ts, v1: (lambda _: Right(Nothing()))(v1.value) if isinstance(v1, hydra.core.TypeRecord) else (lambda _: Right(Nothing()))(v1.value) if isinstance(v1, hydra.core.TypeUnion) else (lambda _: Right(Nothing()))(v1.value) if isinstance(v1, hydra.core.TypeWrap) else Right(Just(ts.type))), hydra.lib.maybes.cases(hydra.lib.maps.lookup(name, schema_types), (lambda : Right(Nothing())), (lambda ts: hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(ts.variables)), (lambda : Right(Nothing())), (lambda : _hoist_schema_types_body_1(ts, hydra.strip.deannotate_type(ts.type)))))))[1])[1]))))

def is_unresolved_inference_var_is_digit(c: int) -> bool:
    return hydra.lib.logic.and_(hydra.lib.equality.gte(c, 48), hydra.lib.equality.lte(c, 57))

def is_unresolved_inference_var(name: hydra.core.Name) -> bool:
    @lru_cache(1)
    def chars() -> frozenlist[int]:
        return hydra.lib.strings.to_list(name.value)
    return hydra.lib.logic.if_else(hydra.lib.lists.null(chars()), (lambda : False), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.equality.equal(hydra.lib.lists.head(chars()), 116)), (lambda : False), (lambda : (rest := hydra.lib.lists.tail(chars()), hydra.lib.logic.and_(hydra.lib.logic.not_(hydra.lib.lists.null(rest)), hydra.lib.lists.null(hydra.lib.lists.filter((lambda c: hydra.lib.logic.not_(is_unresolved_inference_var_is_digit(c))), rest))))[1]))))

def encode_type(aliases: hydra.java.environment.Aliases, bound_vars: frozenset[hydra.core.Name], t: hydra.core.Type, cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.Type]:
    in_scope_type_params = aliases.in_scope_type_params
    type_var_subst = aliases.type_var_subst
    match hydra.strip.deannotate_type(t):
        case hydra.core.TypeApplication(value=at):
            return hydra.lib.eithers.bind(encode_type(aliases, bound_vars, at.function, cx, g), (lambda jlhs: hydra.lib.eithers.bind(hydra.lib.eithers.bind(encode_type(aliases, bound_vars, at.argument, cx, g), (lambda jt_: hydra.java.utils.java_type_to_java_reference_type(jt_, cx))), (lambda jrhs: hydra.java.utils.add_java_type_parameter(jrhs, jlhs, cx)))))

        case hydra.core.TypeFunction(value=ft):
            return hydra.lib.eithers.bind(hydra.lib.eithers.bind(encode_type(aliases, bound_vars, ft.domain, cx, g), (lambda jt_: hydra.java.utils.java_type_to_java_reference_type(jt_, cx))), (lambda jdom: hydra.lib.eithers.bind(hydra.lib.eithers.bind(encode_type(aliases, bound_vars, ft.codomain, cx, g), (lambda jt_: hydra.java.utils.java_type_to_java_reference_type(jt_, cx))), (lambda jcod: Right(hydra.java.utils.java_ref_type((jdom, jcod), hydra.java.names.java_util_function_package_name(), "Function"))))))

        case hydra.core.TypeForall(value=fa):
            return hydra.lib.eithers.bind(encode_type(aliases, hydra.lib.sets.insert(fa.parameter, bound_vars), fa.body, cx, g), (lambda jbody: hydra.java.utils.add_java_type_parameter(hydra.java.utils.java_type_variable(fa.parameter.value), jbody, cx)))

        case hydra.core.TypeList(value=et):
            return hydra.lib.eithers.bind(encode_type(aliases, bound_vars, et, cx, g), (lambda jet: hydra.lib.eithers.bind(hydra.lib.eithers.bind(Right(jet), (lambda jt_: hydra.java.utils.java_type_to_java_reference_type(jt_, cx))), (lambda rt: Right(hydra.java.utils.java_ref_type((rt,), hydra.java.names.java_util_package_name(), "List"))))))

        case hydra.core.TypeLiteral(value=lt):
            return encode_literal_type(lt, cx, g)

        case hydra.core.TypeEither(value=et2):
            return hydra.lib.eithers.bind(hydra.lib.eithers.bind(encode_type(aliases, bound_vars, et2.left, cx, g), (lambda jt_: hydra.java.utils.java_type_to_java_reference_type(jt_, cx))), (lambda jlt: hydra.lib.eithers.bind(hydra.lib.eithers.bind(encode_type(aliases, bound_vars, et2.right, cx, g), (lambda jt_: hydra.java.utils.java_type_to_java_reference_type(jt_, cx))), (lambda jrt: Right(hydra.java.utils.java_ref_type((jlt, jrt), hydra.java.names.hydra_util_package_name(), "Either"))))))

        case hydra.core.TypeMap(value=mt):
            return hydra.lib.eithers.bind(hydra.lib.eithers.bind(encode_type(aliases, bound_vars, mt.keys, cx, g), (lambda jt_: hydra.java.utils.java_type_to_java_reference_type(jt_, cx))), (lambda jkt: hydra.lib.eithers.bind(hydra.lib.eithers.bind(encode_type(aliases, bound_vars, mt.values, cx, g), (lambda jt_: hydra.java.utils.java_type_to_java_reference_type(jt_, cx))), (lambda jvt: Right(hydra.java.utils.java_ref_type((jkt, jvt), hydra.java.names.java_util_package_name(), "Map"))))))

        case hydra.core.TypePair(value=pt):
            return hydra.lib.eithers.bind(hydra.lib.eithers.bind(encode_type(aliases, bound_vars, pt.first, cx, g), (lambda jt_: hydra.java.utils.java_type_to_java_reference_type(jt_, cx))), (lambda jfirst: hydra.lib.eithers.bind(hydra.lib.eithers.bind(encode_type(aliases, bound_vars, pt.second, cx, g), (lambda jt_: hydra.java.utils.java_type_to_java_reference_type(jt_, cx))), (lambda jsecond: Right(hydra.java.utils.java_ref_type((jfirst, jsecond), hydra.java.names.hydra_util_package_name(), "Pair"))))))

        case hydra.core.TypeUnit():
            return Right(hydra.java.utils.java_ref_type((), hydra.java.names.java_lang_package_name(), "Void"))

        case hydra.core.TypeRecord(value=rt):
            return hydra.lib.logic.if_else(hydra.lib.lists.null(rt), (lambda : Right(hydra.java.utils.java_ref_type((), hydra.java.names.java_lang_package_name(), "Void"))), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unexpected anonymous record type"))))))

        case hydra.core.TypeMaybe(value=ot):
            return hydra.lib.eithers.bind(hydra.lib.eithers.bind(encode_type(aliases, bound_vars, ot, cx, g), (lambda jt_: hydra.java.utils.java_type_to_java_reference_type(jt_, cx))), (lambda jot: Right(hydra.java.utils.java_ref_type((jot,), hydra.java.names.hydra_util_package_name(), "Maybe"))))

        case hydra.core.TypeSet(value=st):
            return hydra.lib.eithers.bind(hydra.lib.eithers.bind(encode_type(aliases, bound_vars, st, cx, g), (lambda jt_: hydra.java.utils.java_type_to_java_reference_type(jt_, cx))), (lambda jst: Right(hydra.java.utils.java_ref_type((jst,), hydra.java.names.java_util_package_name(), "Set"))))

        case hydra.core.TypeUnion():
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unexpected anonymous union type"))))

        case hydra.core.TypeVariable(value=name0):
            @lru_cache(1)
            def name() -> hydra.core.Name:
                return hydra.lib.maybes.from_maybe((lambda : name0), hydra.lib.maps.lookup(name0, type_var_subst))
            return hydra.lib.eithers.bind(encode_type_resolve_if_typedef(aliases, bound_vars, in_scope_type_params, name(), cx, g), (lambda resolved: hydra.lib.maybes.cases(resolved, (lambda : Right(hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.lib.sets.member(name(), bound_vars), hydra.lib.sets.member(name(), in_scope_type_params)), (lambda : cast(hydra.java.syntax.Type, hydra.java.syntax.TypeReference(hydra.java.utils.java_type_variable(name().value)))), (lambda : hydra.lib.logic.if_else(is_lambda_bound_variable(name()), (lambda : cast(hydra.java.syntax.Type, hydra.java.syntax.TypeReference(hydra.java.utils.java_type_variable(name().value)))), (lambda : hydra.lib.logic.if_else(is_unresolved_inference_var(name()), (lambda : cast(hydra.java.syntax.Type, hydra.java.syntax.TypeReference(cast(hydra.java.syntax.ReferenceType, hydra.java.syntax.ReferenceTypeClassOrInterface(cast(hydra.java.syntax.ClassOrInterfaceType, hydra.java.syntax.ClassOrInterfaceTypeClass(hydra.java.utils.java_class_type((), hydra.java.names.java_lang_package_name(), "Object")))))))), (lambda : cast(hydra.java.syntax.Type, hydra.java.syntax.TypeReference(hydra.java.utils.name_to_java_reference_type(aliases, True, (), name(), Nothing()))))))))))), (lambda resolved_type: encode_type(aliases, bound_vars, resolved_type, cx, g)))))

        case hydra.core.TypeWrap():
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unexpected anonymous wrap type"))))

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("can't encode unsupported type in Java: ", hydra.show.core.type(t))))))

def apply_cast_if_safe(aliases: hydra.java.environment.Aliases, cast_type: hydra.core.Type, expr: hydra.java.syntax.Expression, cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.Expression]:
    trusted = aliases.trusted_type_vars
    in_scope = aliases.in_scope_type_params
    @lru_cache(1)
    def cast_vars() -> frozenset[hydra.core.Name]:
        return collect_type_vars(cast_type)
    @lru_cache(1)
    def java_type_vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(hydra.lib.lists.filter((lambda v: hydra.lib.logic.or_(hydra.lib.sets.member(v, in_scope), is_lambda_bound_variable(v))), hydra.lib.sets.to_list(cast_vars())))
    @lru_cache(1)
    def is_safe() -> bool:
        return hydra.lib.logic.or_(hydra.lib.sets.null(trusted), hydra.lib.logic.or_(hydra.lib.sets.null(java_type_vars()), hydra.lib.sets.null(hydra.lib.sets.difference(java_type_vars(), trusted))))
    return hydra.lib.logic.if_else(is_safe(), (lambda : hydra.lib.eithers.bind(encode_type(aliases, hydra.lib.sets.empty(), cast_type, cx, g), (lambda jtype: hydra.lib.eithers.bind(hydra.java.utils.java_type_to_java_reference_type(jtype, cx), (lambda rt: Right(hydra.java.utils.java_cast_expression_to_java_expression(hydra.java.utils.java_cast_expression(rt, hydra.java.utils.java_expression_to_java_unary_expression(expr))))))))), (lambda : Right(expr)))

def apply_java_arg(expr: hydra.java.syntax.Expression, jarg: hydra.java.syntax.Expression) -> hydra.java.syntax.Expression:
    return hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation(Just(Right(hydra.java.utils.java_expression_to_java_primary(expr))), hydra.java.syntax.Identifier(hydra.java.names.apply_method_name), (jarg,)))

def substitute_type_vars_with_types_go(subst: FrozenDict[hydra.core.Name, hydra.core.Type], t: hydra.core.Type) -> hydra.core.Type:
    match hydra.strip.deannotate_type(t):
        case hydra.core.TypeVariable(value=v):
            return hydra.lib.maybes.cases(hydra.lib.maps.lookup(v, subst), (lambda : t), (lambda rep: rep))

        case hydra.core.TypeFunction(value=ft):
            return cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(substitute_type_vars_with_types_go(subst, ft.domain), substitute_type_vars_with_types_go(subst, ft.codomain))))

        case hydra.core.TypeApplication(value=at):
            return cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(substitute_type_vars_with_types_go(subst, at.function), substitute_type_vars_with_types_go(subst, at.argument))))

        case hydra.core.TypeList(value=inner):
            return cast(hydra.core.Type, hydra.core.TypeList(substitute_type_vars_with_types_go(subst, inner)))

        case hydra.core.TypeSet(value=inner2):
            return cast(hydra.core.Type, hydra.core.TypeSet(substitute_type_vars_with_types_go(subst, inner2)))

        case hydra.core.TypeMaybe(value=inner3):
            return cast(hydra.core.Type, hydra.core.TypeMaybe(substitute_type_vars_with_types_go(subst, inner3)))

        case hydra.core.TypeMap(value=mt):
            return cast(hydra.core.Type, hydra.core.TypeMap(hydra.core.MapType(substitute_type_vars_with_types_go(subst, mt.keys), substitute_type_vars_with_types_go(subst, mt.values))))

        case hydra.core.TypePair(value=pt):
            return cast(hydra.core.Type, hydra.core.TypePair(hydra.core.PairType(substitute_type_vars_with_types_go(subst, pt.first), substitute_type_vars_with_types_go(subst, pt.second))))

        case hydra.core.TypeEither(value=et):
            return cast(hydra.core.Type, hydra.core.TypeEither(hydra.core.EitherType(substitute_type_vars_with_types_go(subst, et.left), substitute_type_vars_with_types_go(subst, et.right))))

        case hydra.core.TypeForall(value=ft2):
            return cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(ft2.parameter, substitute_type_vars_with_types_go(subst, ft2.body))))

        case _:
            return t

def substitute_type_vars_with_types(subst: FrozenDict[hydra.core.Name, hydra.core.Type], t: hydra.core.Type) -> hydra.core.Type:
    return substitute_type_vars_with_types_go(subst, hydra.strip.deannotate_type(t))

def apply_overgen_subst_to_term_annotations_go(subst: FrozenDict[hydra.core.Name, hydra.core.Type], cx: hydra.graph.Graph, term: hydra.core.Term) -> hydra.core.Term:
    match term:
        case hydra.core.TermAnnotated(value=at):
            inner = at.body
            ann = at.annotation
            @lru_cache(1)
            def ann_() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                return hydra.lib.maybes.cases(hydra.lib.maps.lookup(hydra.constants.key_type, ann), (lambda : ann), (lambda type_term: hydra.lib.eithers.either((lambda _: ann), (lambda t: (t_ := substitute_type_vars_with_types(subst, t), hydra.lib.maps.insert(hydra.constants.key_type, hydra.encode.core.type(t_), ann))[1]), hydra.decode.core.type(cx, type_term))))
            return cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(apply_overgen_subst_to_term_annotations_go(subst, cx, inner), ann_())))

        case hydra.core.TermApplication(value=app):
            return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(apply_overgen_subst_to_term_annotations_go(subst, cx, app.function), apply_overgen_subst_to_term_annotations_go(subst, cx, app.argument))))

        case hydra.core.TermLambda(value=lam):
            return cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(lam.parameter, hydra.lib.maybes.map((lambda d: substitute_type_vars_with_types(subst, d)), lam.domain), apply_overgen_subst_to_term_annotations_go(subst, cx, lam.body))))

        case hydra.core.TermCases(value=cs):
            return cast(hydra.core.Term, hydra.core.TermCases(hydra.core.CaseStatement(cs.type_name, hydra.lib.maybes.map((lambda d: apply_overgen_subst_to_term_annotations_go(subst, cx, d)), cs.default), hydra.lib.lists.map((lambda fld: hydra.core.Field(fld.name, apply_overgen_subst_to_term_annotations_go(subst, cx, fld.term))), cs.cases))))

        case hydra.core.TermLet(value=lt):
            return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.map((lambda b: hydra.core.Binding(b.name, apply_overgen_subst_to_term_annotations_go(subst, cx, b.term), b.type)), lt.bindings), apply_overgen_subst_to_term_annotations_go(subst, cx, lt.body))))

        case hydra.core.TermTypeApplication(value=ta):
            return cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(apply_overgen_subst_to_term_annotations_go(subst, cx, ta.body), substitute_type_vars_with_types(subst, ta.type))))

        case hydra.core.TermTypeLambda(value=tl):
            return cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(tl.parameter, apply_overgen_subst_to_term_annotations_go(subst, cx, tl.body))))

        case _:
            return term

def apply_overgen_subst_to_term_annotations(subst: FrozenDict[hydra.core.Name, hydra.core.Type], term0: hydra.core.Term, cx: T0, g: hydra.graph.Graph) -> Either[T1, hydra.core.Term]:
    return Right(apply_overgen_subst_to_term_annotations_go(subst, g, term0))

def apply_subst_simple(subst: FrozenDict[hydra.core.Name, hydra.core.Type], t: hydra.core.Type) -> hydra.core.Type:
    match hydra.strip.deannotate_type(t):
        case hydra.core.TypeVariable(value=v):
            return hydra.lib.maps.find_with_default(t, v, subst)

        case _:
            return t

def arrays_compare_expr(other_var: str, fname: str) -> hydra.java.syntax.Expression:
    @lru_cache(1)
    def header() -> hydra.java.syntax.MethodInvocation_Header:
        return cast(hydra.java.syntax.MethodInvocation_Header, hydra.java.syntax.MethodInvocation_HeaderComplex(hydra.java.syntax.MethodInvocation_Complex(cast(hydra.java.syntax.MethodInvocation_Variant, hydra.java.syntax.MethodInvocation_VariantType(hydra.java.utils.java_type_name(hydra.java.syntax.Identifier("java.util.Arrays")))), (), hydra.java.syntax.Identifier("compare"))))
    @lru_cache(1)
    def arg1() -> hydra.java.syntax.Expression:
        return hydra.java.utils.java_expression_name_to_java_expression(hydra.java.syntax.ExpressionName(Nothing(), hydra.java.syntax.Identifier(hydra.java.utils.sanitize_java_name(fname))))
    @lru_cache(1)
    def arg2() -> hydra.java.syntax.Expression:
        return hydra.java.utils.java_expression_name_to_java_expression(hydra.java.utils.field_expression(hydra.java.utils.java_identifier(other_var), hydra.java.utils.java_identifier(fname)))
    return hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.syntax.MethodInvocation(header(), (arg1(), arg2())))

def arrays_equals_clause(tmp_name: str, fname: str) -> hydra.java.syntax.InclusiveOrExpression:
    @lru_cache(1)
    def this_arg() -> hydra.java.syntax.Expression:
        return hydra.java.utils.java_expression_name_to_java_expression(hydra.java.utils.field_expression(hydra.java.syntax.Identifier("this"), hydra.java.utils.java_identifier(fname)))
    @lru_cache(1)
    def other_arg() -> hydra.java.syntax.Expression:
        return hydra.java.utils.java_expression_name_to_java_expression(hydra.java.utils.field_expression(hydra.java.utils.java_identifier(tmp_name), hydra.java.utils.java_identifier(fname)))
    @lru_cache(1)
    def header() -> hydra.java.syntax.MethodInvocation_Header:
        return cast(hydra.java.syntax.MethodInvocation_Header, hydra.java.syntax.MethodInvocation_HeaderComplex(hydra.java.syntax.MethodInvocation_Complex(cast(hydra.java.syntax.MethodInvocation_Variant, hydra.java.syntax.MethodInvocation_VariantType(hydra.java.utils.java_type_name(hydra.java.syntax.Identifier("java.util.Arrays")))), (), hydra.java.syntax.Identifier(hydra.java.names.equals_method_name))))
    return hydra.java.utils.java_postfix_expression_to_java_inclusive_or_expression(hydra.java.utils.java_method_invocation_to_java_postfix_expression(hydra.java.syntax.MethodInvocation(header(), (this_arg(), other_arg()))))

def no_comment(decl: hydra.java.syntax.ClassBodyDeclaration) -> hydra.java.syntax.ClassBodyDeclarationWithComments:
    return hydra.java.syntax.ClassBodyDeclarationWithComments(decl, Nothing())

def augment_variant_class(aliases: hydra.java.environment.Aliases, tparams: frozenlist[hydra.java.syntax.TypeParameter], el_name: hydra.core.Name, cd: hydra.java.syntax.ClassDeclaration) -> hydra.java.syntax.ClassDeclaration:
    match cd:
        case hydra.java.syntax.ClassDeclarationNormal(value=ncd):
            @lru_cache(1)
            def args() -> frozenlist[hydra.java.syntax.TypeArgument]:
                return hydra.lib.lists.map((lambda tp: hydra.java.utils.type_parameter_to_type_argument(tp)), tparams)
            @lru_cache(1)
            def extends_part() -> hydra.java.syntax.ClassType:
                return hydra.java.utils.name_to_java_class_type(aliases, True, args(), el_name, Nothing())
            new_mods = (cast(hydra.java.syntax.ClassModifier, hydra.java.syntax.ClassModifierPublic()), cast(hydra.java.syntax.ClassModifier, hydra.java.syntax.ClassModifierStatic()), cast(hydra.java.syntax.ClassModifier, hydra.java.syntax.ClassModifierFinal()))
            old_body = ncd.body
            old_decls = old_body.value
            @lru_cache(1)
            def accept_decl() -> hydra.java.syntax.ClassBodyDeclarationWithComments:
                return no_comment(hydra.java.utils.to_accept_method(False, tparams))
            @lru_cache(1)
            def new_body() -> hydra.java.syntax.ClassBody:
                return hydra.java.syntax.ClassBody(hydra.lib.lists.concat2(old_decls, (accept_decl(),)))
            return cast(hydra.java.syntax.ClassDeclaration, hydra.java.syntax.ClassDeclarationNormal(hydra.java.syntax.NormalClassDeclaration(new_mods, ncd.identifier, tparams, Just(extends_part()), ncd.implements, new_body())))

        case _:
            return cd

def binding_is_function_type(b: hydra.core.Binding):
    def _hoist_hydra_java_coder_binding_is_function_type_1(v1):
        match v1:
            case hydra.core.TermLambda():
                return True

            case hydra.core.TermProject():
                return True

            case hydra.core.TermCases():
                return True

            case hydra.core.TermUnwrap():
                return True

            case _:
                return False
    def _hoist_hydra_java_coder_binding_is_function_type_2(v1):
        match v1:
            case hydra.core.TypeFunction():
                return True

            case _:
                return False
    def _hoist_hydra_java_coder_binding_is_function_type_3(v1):
        match v1:
            case hydra.core.TypeFunction():
                return True

            case hydra.core.TypeForall(value=fa):
                return _hoist_hydra_java_coder_binding_is_function_type_2(hydra.strip.deannotate_type(fa.body))

            case _:
                return False
    return hydra.lib.maybes.maybe((lambda : _hoist_hydra_java_coder_binding_is_function_type_1(hydra.strip.deannotate_term(b.term))), (lambda ts: _hoist_hydra_java_coder_binding_is_function_type_3(hydra.strip.deannotate_type(ts.type))), b.type)

def binding_name_to_file_path(name: hydra.core.Name) -> str:
    @lru_cache(1)
    def qn() -> hydra.packaging.QualifiedName:
        return hydra.names.qualify_name(name)
    ns_ = qn().namespace
    local = qn().local
    @lru_cache(1)
    def sanitized() -> str:
        return hydra.formatting.sanitize_with_underscores(hydra.java.language.reserved_words(), local)
    @lru_cache(1)
    def unq() -> hydra.core.Name:
        return hydra.names.unqualify_name(hydra.packaging.QualifiedName(ns_, sanitized()))
    return hydra.names.name_to_file_path(hydra.util.CaseConvention.CAMEL, hydra.util.CaseConvention.PASCAL, hydra.packaging.FileExtension("java"), unq())

def fresh_java_name_go(base: hydra.core.Name, avoid: frozenset[hydra.core.Name], i: int) -> hydra.core.Name:
    @lru_cache(1)
    def candidate() -> hydra.core.Name:
        return hydra.core.Name(hydra.lib.strings.cat2(base.value, hydra.lib.literals.show_int32(i)))
    return hydra.lib.logic.if_else(hydra.lib.sets.member(candidate(), avoid), (lambda : fresh_java_name_go(base, avoid, hydra.lib.math.add(i, 1))), (lambda : candidate()))

def fresh_java_name(base: hydra.core.Name, avoid: frozenset[hydra.core.Name]) -> hydra.core.Name:
    return fresh_java_name_go(base, avoid, 2)

def dedup_bindings(in_scope: frozenset[hydra.core.Name], bs: frozenlist[hydra.core.Binding]) -> frozenlist[hydra.core.Binding]:
    return hydra.lib.logic.if_else(hydra.lib.lists.null(bs), (lambda : ()), (lambda : (b := hydra.lib.lists.head(bs), (rest := hydra.lib.lists.tail(bs), (name := b.name, hydra.lib.logic.if_else(hydra.lib.sets.member(name, in_scope), (lambda : (new_name := fresh_java_name(name, in_scope), (subst := hydra.lib.maps.singleton(name, new_name), (rest2 := hydra.lib.lists.map((lambda b2: hydra.core.Binding(b2.name, hydra.variables.substitute_variables(subst, b2.term), b2.type)), rest), hydra.lib.lists.cons(hydra.core.Binding(new_name, b.term, b.type), dedup_bindings(hydra.lib.sets.insert(new_name, in_scope), rest2)))[1])[1])[1]), (lambda : hydra.lib.lists.cons(b, dedup_bindings(hydra.lib.sets.insert(name, in_scope), rest)))))[1])[1])[1]))

def flatten_bindings(bindings: frozenlist[hydra.core.Binding]):
    def _hoist_hydra_java_coder_flatten_bindings_1(b, v1):
        match v1:
            case hydra.core.TermLet(value=lt):
                return hydra.lib.lists.concat2(flatten_bindings(lt.bindings), (hydra.core.Binding(b.name, lt.body, b.type),))

            case _:
                return (b,)
    return hydra.lib.lists.bind(bindings, (lambda b: _hoist_hydra_java_coder_flatten_bindings_1(b, hydra.strip.deannotate_term(b.term))))

def needs_thunking(t: hydra.core.Term) -> bool:
    match hydra.strip.deannotate_term(t):
        case hydra.core.TermLet():
            return True

        case hydra.core.TermTypeApplication():
            return True

        case hydra.core.TermTypeLambda():
            return True

        case _:
            return hydra.lib.lists.foldl((lambda b, st: hydra.lib.logic.or_(b, needs_thunking(st))), False, hydra.rewriting.subterms(t))

java11_features = hydra.java.environment.JavaFeatures(True)

java_features = java11_features

def type_args_or_diamond(args: frozenlist[hydra.java.syntax.TypeArgument]) -> hydra.java.syntax.TypeArgumentsOrDiamond:
    return hydra.lib.logic.if_else(java_features.supports_diamond_operator, (lambda : cast(hydra.java.syntax.TypeArgumentsOrDiamond, hydra.java.syntax.TypeArgumentsOrDiamondDiamond())), (lambda : cast(hydra.java.syntax.TypeArgumentsOrDiamond, hydra.java.syntax.TypeArgumentsOrDiamondArguments(args))))

def to_decl_init(aliases_ext: hydra.java.environment.Aliases, g_ext: hydra.graph.Graph, recursive_vars: frozenset[hydra.core.Name], flat_bindings: frozenlist[hydra.core.Binding], name: hydra.core.Name, cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.errors.Error, Maybe[hydra.java.syntax.BlockStatement]]:
    return hydra.lib.logic.if_else(hydra.lib.sets.member(name, recursive_vars), (lambda : (binding := hydra.lib.lists.head(hydra.lib.lists.filter((lambda b: hydra.lib.equality.equal(b.name, name)), flat_bindings)), (value := binding.term, hydra.lib.eithers.bind(hydra.lib.maybes.cases(binding.type, (lambda : hydra.checking.type_of_term(cx, g_ext, value)), (lambda ts: Right(ts.type))), (lambda typ: hydra.lib.eithers.bind(encode_type(aliases_ext, hydra.lib.sets.empty(), typ, cx, g), (lambda jtype: (id := hydra.java.utils.variable_to_java_identifier(name), arid := hydra.java.syntax.Identifier("java.util.concurrent.atomic.AtomicReference"), aid := hydra.java.syntax.AnnotatedIdentifier((), arid), hydra.lib.eithers.bind(hydra.java.utils.java_type_to_java_reference_type(jtype, cx), (lambda rt: (targs := type_args_or_diamond((cast(hydra.java.syntax.TypeArgument, hydra.java.syntax.TypeArgumentReference(rt)),)), ci := hydra.java.syntax.ClassOrInterfaceTypeToInstantiate((aid,), Just(targs)), body := hydra.java.utils.java_constructor_call(ci, (), Nothing()), pkg := hydra.java.names.java_package_name(("java", "util", "concurrent", "atomic")), artype := hydra.java.utils.java_ref_type((rt,), Just(pkg), "AtomicReference"), Right(Just(hydra.java.utils.variable_declaration_statement(aliases_ext, artype, id, body))))[5])))[3])))))[1])[1]), (lambda : Right(Nothing())))

def classify_data_term_count_lambda_params(t: hydra.core.Term) -> int:
    match hydra.strip.deannotate_term(t):
        case hydra.core.TermLambda(value=lam):
            return hydra.lib.math.add(1, classify_data_term_count_lambda_params(lam.body))

        case hydra.core.TermLet(value=lt):
            return classify_data_term_count_lambda_params(lt.body)

        case _:
            return 0

def classify_data_term_strip_type_lambdas(t: hydra.core.Term) -> hydra.core.Term:
    while True:
        match hydra.strip.deannotate_term(t):
            case hydra.core.TermTypeLambda(value=tl):
                t = tl.body
                continue

            case _:
                return t

def classify_data_term(ts: hydra.core.TypeScheme, term: hydra.core.Term) -> hydra.java.environment.JavaSymbolClass:
    return hydra.lib.logic.if_else(hydra.dependencies.is_lambda(term), (lambda : (n := classify_data_term_count_lambda_params(term), hydra.lib.logic.if_else(hydra.lib.equality.gt(n, 1), (lambda : cast(hydra.java.environment.JavaSymbolClass, hydra.java.environment.JavaSymbolClassHoistedLambda(n))), (lambda : cast(hydra.java.environment.JavaSymbolClass, hydra.java.environment.JavaSymbolClassUnaryFunction()))))[1]), (lambda : (has_type_params := hydra.lib.logic.not_(hydra.lib.lists.null(ts.variables)), hydra.lib.logic.if_else(has_type_params, (lambda : (n2 := classify_data_term_count_lambda_params(classify_data_term_strip_type_lambdas(term)), hydra.lib.logic.if_else(hydra.lib.equality.gt(n2, 0), (lambda : cast(hydra.java.environment.JavaSymbolClass, hydra.java.environment.JavaSymbolClassHoistedLambda(n2))), (lambda : cast(hydra.java.environment.JavaSymbolClass, hydra.java.environment.JavaSymbolClassNullaryFunction()))))[1]), (lambda : cast(hydra.java.environment.JavaSymbolClass, hydra.java.environment.JavaSymbolClassNullaryFunction()))))[1]))

def classify_data_reference(name: hydra.core.Name, cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.environment.JavaSymbolClass]:
    return hydra.lib.eithers.bind(Right(hydra.lexical.lookup_binding(g, name)), (lambda mel: hydra.lib.maybes.cases(mel, (lambda : Right(cast(hydra.java.environment.JavaSymbolClass, hydra.java.environment.JavaSymbolClassLocalVariable()))), (lambda el: hydra.lib.maybes.cases(el.type, (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("no type scheme for element ", el.name.value)))))), (lambda ts: Right(classify_data_term(ts, el.term))))))))

def collect_forall_params(t: hydra.core.Type) -> frozenlist[hydra.core.Name]:
    match hydra.strip.deannotate_type(t):
        case hydra.core.TypeForall(value=fa):
            return hydra.lib.lists.cons(fa.parameter, collect_forall_params(fa.body))

        case _:
            return ()

def collect_type_apps(t: hydra.core.Term, acc: frozenlist[hydra.core.Type]) -> tuple[hydra.core.Term, frozenlist[hydra.core.Type]]:
    while True:
        match hydra.strip.deannotate_term(t):
            case hydra.core.TermTypeApplication(value=ta):
                t = ta.body
                acc = hydra.lib.lists.cons(ta.type, acc)
                continue

            case _:
                return (hydra.strip.deannotate_term(t), acc)

def collect_type_apps0(t: hydra.core.Term, acc: frozenlist[hydra.core.Type]) -> tuple[hydra.core.Term, frozenlist[hydra.core.Type]]:
    while True:
        match hydra.strip.deannotate_term(t):
            case hydra.core.TermTypeApplication(value=ta):
                t = ta.body
                acc = hydra.lib.lists.cons(ta.type, acc)
                continue

            case _:
                return (t, acc)

def correct_cast_type(inner_body: hydra.core.Term, type_args: frozenlist[hydra.core.Type], fallback: hydra.core.Type, cx: T0, g: T1) -> Either[T2, hydra.core.Type]:
    match hydra.strip.deannotate_term(inner_body):
        case hydra.core.TermPair():
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(type_args), 2), (lambda : Right(cast(hydra.core.Type, hydra.core.TypePair(hydra.core.PairType(hydra.lib.lists.head(type_args), hydra.lib.lists.head(hydra.lib.lists.tail(type_args))))))), (lambda : Right(fallback)))

        case _:
            return Right(fallback)

def build_arg_subst(scheme_var_set: frozenset[hydra.core.Name], scheme_doms: frozenlist[hydra.core.Type], arg_types: frozenlist[T0]):
    return hydra.lib.maps.from_list(hydra.lib.lists.bind(hydra.lib.lists.zip(scheme_doms, arg_types), (lambda p: (sdom := hydra.lib.pairs.first(p), arg_type := hydra.lib.pairs.second(p), _hoist_arg_type_body_1 := (lambda v1: (lambda v: hydra.lib.logic.if_else(hydra.lib.sets.member(v, scheme_var_set), (lambda : ((v, arg_type),)), (lambda : ())))(v1.value) if isinstance(v1, hydra.core.TypeVariable) else ()), _hoist_arg_type_body_1(hydra.strip.deannotate_type(sdom)))[3])))

def peel_domain_types(n: int, t: hydra.core.Type):
    def _hoist_hydra_java_coder_peel_domain_types_1(n, t, v1):
        match v1:
            case hydra.core.TypeFunction(value=ft):
                @lru_cache(1)
                def rest() -> tuple[frozenlist[hydra.core.Type], hydra.core.Type]:
                    return peel_domain_types(hydra.lib.math.sub(n, 1), ft.codomain)
                return (hydra.lib.lists.cons(ft.domain, hydra.lib.pairs.first(rest())), hydra.lib.pairs.second(rest()))

            case _:
                return ((), t)
    return hydra.lib.logic.if_else(hydra.lib.equality.lte(n, 0), (lambda : ((), t)), (lambda : _hoist_hydra_java_coder_peel_domain_types_1(n, t, hydra.strip.deannotate_type(t))))

def resolve_type_apps(scheme_vars: frozenlist[hydra.core.Name], fallback_type_apps: frozenlist[hydra.core.Type], arg_subst: FrozenDict[hydra.core.Name, hydra.core.Type]) -> frozenlist[hydra.core.Type]:
    @lru_cache(1)
    def resolved_vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(hydra.lib.maps.keys(arg_subst))
    @lru_cache(1)
    def unresolved_vars() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.filter((lambda v: hydra.lib.logic.not_(hydra.lib.sets.member(v, resolved_vars()))), scheme_vars)
    @lru_cache(1)
    def used_types() -> frozenset[hydra.core.Type]:
        return hydra.lib.sets.from_list(hydra.lib.maps.elems(arg_subst))
    @lru_cache(1)
    def unused_ir_types() -> frozenlist[hydra.core.Type]:
        return hydra.lib.lists.filter((lambda t: hydra.lib.logic.not_(hydra.lib.sets.member(t, used_types()))), fallback_type_apps)
    @lru_cache(1)
    def remaining_subst() -> FrozenDict[hydra.core.Name, hydra.core.Type]:
        return hydra.lib.maps.from_list(hydra.lib.lists.zip(unresolved_vars(), unused_ir_types()))
    @lru_cache(1)
    def full_subst() -> FrozenDict[hydra.core.Name, hydra.core.Type]:
        return hydra.lib.maps.union(arg_subst, remaining_subst())
    return hydra.lib.lists.map((lambda v: hydra.lib.maps.find_with_default(cast(hydra.core.Type, hydra.core.TypeVariable(v)), v, full_subst())), scheme_vars)

def types_match(a: hydra.core.Type, b: hydra.core.Type):
    def _hoist_hydra_java_coder_types_match_1(va, v1):
        match v1:
            case hydra.core.TypeVariable(value=vb):
                return hydra.lib.equality.equal(va, vb)

            case _:
                return True
    def _hoist_hydra_java_coder_types_match_2(wa, v1):
        match v1:
            case hydra.core.TypeWrap(value=wb):
                return hydra.lib.equality.equal(wa, wb)

            case _:
                return True
    match a:
        case hydra.core.TypeVariable(value=va):
            return _hoist_hydra_java_coder_types_match_1(va, b)

        case hydra.core.TypeWrap(value=wa):
            return _hoist_hydra_java_coder_types_match_2(wa, b)

        case _:
            return True

def correct_type_apps_with_args(scheme_vars: frozenlist[hydra.core.Name], fallback_type_apps: frozenlist[hydra.core.Type], scheme_type: hydra.core.Type, args: frozenlist[hydra.core.Term], cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, frozenlist[hydra.core.Type]]:
    @lru_cache(1)
    def scheme_var_set() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(scheme_vars)
    @lru_cache(1)
    def ir_subst() -> FrozenDict[hydra.core.Name, hydra.core.Type]:
        return hydra.lib.maps.from_list(hydra.lib.lists.zip(scheme_vars, fallback_type_apps))
    @lru_cache(1)
    def peeled() -> tuple[frozenlist[hydra.core.Type], hydra.core.Type]:
        return peel_domain_types(hydra.lib.lists.length(args), scheme_type)
    @lru_cache(1)
    def scheme_doms() -> frozenlist[hydra.core.Type]:
        return hydra.lib.pairs.first(peeled())
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda arg: hydra.lib.eithers.bimap((lambda _de: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_de.value)))), (lambda _a: _a), hydra.annotations.get_type(g, hydra.annotations.term_annotation_internal(arg)))), args), (lambda m_arg_types: hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(hydra.lib.lists.filter((lambda m: hydra.lib.maybes.is_nothing(m)), m_arg_types))), (lambda : Right(fallback_type_apps)), (lambda : (arg_types := hydra.lib.lists.bind(m_arg_types, (lambda m: hydra.lib.maybes.cases(m, (lambda : ()), (lambda x: hydra.lib.lists.pure(x))))), (ir_doms := hydra.lib.lists.map((lambda d: apply_subst_simple(ir_subst(), d)), scheme_doms()), (doms_match := hydra.lib.lists.null(hydra.lib.lists.filter((lambda p: hydra.lib.logic.not_(types_match(hydra.strip.deannotate_type(hydra.lib.pairs.first(p)), hydra.strip.deannotate_type(hydra.lib.pairs.second(p))))), hydra.lib.lists.zip(ir_doms, arg_types))), hydra.lib.logic.if_else(doms_match, (lambda : Right(fallback_type_apps)), (lambda : Right(resolve_type_apps(scheme_vars, fallback_type_apps, build_arg_subst(scheme_var_set(), scheme_doms(), arg_types))))))[1])[1])[1]))))

def count_function_params(t: hydra.core.Type) -> int:
    match hydra.strip.deannotate_type(t):
        case hydra.core.TypeFunction(value=ft):
            return hydra.lib.math.add(1, count_function_params(ft.codomain))

        case _:
            return 0

def direct_ref_substitution_process_group(direct_input_vars: frozenset[T0], cod_var: Maybe[T0], subst: FrozenDict[T0, T0], in_var: T0, out_vars: frozenlist[T0]) -> FrozenDict[T0, T0]:
    @lru_cache(1)
    def self_ref_count() -> int:
        return hydra.lib.lists.length(hydra.lib.lists.filter((lambda v: hydra.lib.equality.equal(v, in_var)), out_vars))
    @lru_cache(1)
    def non_self_vars() -> frozenlist[T0]:
        return hydra.lib.lists.filter((lambda v: hydra.lib.logic.not_(hydra.lib.equality.equal(v, in_var))), out_vars)
    @lru_cache(1)
    def safe_non_self_vars() -> frozenlist[T0]:
        return hydra.lib.lists.filter((lambda v: hydra.lib.logic.and_(hydra.lib.logic.not_(hydra.lib.sets.member(v, direct_input_vars)), hydra.lib.logic.not_(hydra.lib.equality.equal(Just(v), cod_var)))), non_self_vars())
    return hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.gte(self_ref_count(), 2), hydra.lib.logic.not_(hydra.lib.lists.null(safe_non_self_vars()))), (lambda : hydra.lib.lists.foldl((lambda s, v: hydra.lib.maps.insert(v, in_var, s)), subst, safe_non_self_vars())), (lambda : subst))

def direct_ref_substitution(direct_input_vars: frozenset[T0], cod_var: Maybe[T0], grouped: FrozenDict[T0, frozenlist[T0]]) -> FrozenDict[T0, T0]:
    return hydra.lib.lists.foldl((lambda subst, entry: direct_ref_substitution_process_group(direct_input_vars, cod_var, subst, hydra.lib.pairs.first(entry), hydra.lib.pairs.second(entry))), hydra.lib.maps.empty(), hydra.lib.maps.to_list(grouped))

def extract_direct_return_go(tparam_set: frozenset[hydra.core.Name], t: hydra.core.Type):
    match hydra.strip.deannotate_type(t):
        case hydra.core.TypeFunction(value=ft):
            @lru_cache(1)
            def dom() -> hydra.core.Type:
                return hydra.strip.deannotate_type(ft.domain)
            cod = ft.codomain
            def _hoist_cod_body_1(in_var, v1):
                match v1:
                    case hydra.core.TypeFunction(value=ft2):
                        @lru_cache(1)
                        def mid_arg() -> hydra.core.Type:
                            return hydra.strip.deannotate_type(ft2.domain)
                        @lru_cache(1)
                        def ret_part() -> hydra.core.Type:
                            return hydra.strip.deannotate_type(ft2.codomain)
                        def _hoist_ret_part_body_1(v12):
                            match v12:
                                case hydra.core.TypeVariable(value=out_var):
                                    return hydra.lib.logic.if_else(hydra.lib.sets.member(out_var, tparam_set), (lambda : ((in_var, out_var),)), (lambda : ()))

                                case _:
                                    return ()
                        def _hoist_ret_part_body_2(v12):
                            match v12:
                                case hydra.core.TypeVariable(value=out_var):
                                    return hydra.lib.logic.if_else(hydra.lib.sets.member(out_var, tparam_set), (lambda : ((in_var, out_var),)), (lambda : ()))

                                case _:
                                    return ()
                        def _hoist_ret_part_body_3(v12):
                            match v12:
                                case hydra.core.TypeVariable(value=mid_var):
                                    return hydra.lib.logic.if_else(hydra.lib.sets.member(mid_var, tparam_set), (lambda : ()), (lambda : _hoist_ret_part_body_2(ret_part())))

                                case _:
                                    return _hoist_ret_part_body_1(ret_part())
                        return _hoist_ret_part_body_3(mid_arg())

                    case _:
                        return ()
            def _hoist_cod_body_2(v1):
                match v1:
                    case hydra.core.TypeVariable(value=in_var):
                        return hydra.lib.logic.if_else(hydra.lib.sets.member(in_var, tparam_set), (lambda : _hoist_cod_body_1(in_var, hydra.strip.deannotate_type(cod))), (lambda : extract_direct_return_go(tparam_set, cod)))

                    case _:
                        return extract_direct_return_go(tparam_set, cod)
            return _hoist_cod_body_2(dom())

        case _:
            return ()

def extract_direct_return(tparam_set: frozenset[hydra.core.Name], t: hydra.core.Type) -> frozenlist[tuple[hydra.core.Name, hydra.core.Name]]:
    return extract_direct_return_go(tparam_set, t)

def unwrap_return_type(t: hydra.core.Type) -> hydra.core.Type:
    while True:
        match hydra.strip.deannotate_type(t):
            case hydra.core.TypeFunction(value=ft):
                t = ft.codomain
                continue

            case hydra.core.TypeApplication(value=at):
                t = at.argument
                continue

            case _:
                return t

def extract_in_out_pair(t: hydra.core.Type):
    def _hoist_hydra_java_coder_extract_in_out_pair_1(ft, v1):
        match v1:
            case hydra.core.TypeVariable(value=in_var):
                @lru_cache(1)
                def ret_type() -> hydra.core.Type:
                    return unwrap_return_type(ft.codomain)
                def _hoist_ret_type_body_1(v12):
                    match v12:
                        case hydra.core.TypeVariable(value=out_var):
                            return ((in_var, out_var),)

                        case _:
                            return ()
                def _hoist_ret_type_body_2(v12):
                    match v12:
                        case hydra.core.TypePair(value=pt):
                            return _hoist_ret_type_body_1(hydra.strip.deannotate_type(pt.first))

                        case _:
                            return ()
                return _hoist_ret_type_body_2(hydra.strip.deannotate_type(ret_type()))

            case _:
                return ()
    match hydra.strip.deannotate_type(t):
        case hydra.core.TypeFunction(value=ft):
            return _hoist_hydra_java_coder_extract_in_out_pair_1(ft, hydra.strip.deannotate_type(ft.domain))

        case _:
            return ()

def find_pair_first(t: hydra.core.Type):
    def _hoist_hydra_java_coder_find_pair_first_1(v1):
        match v1:
            case hydra.core.TypeVariable(value=v):
                return Just(v)

            case _:
                return Nothing()
    match hydra.strip.deannotate_type(t):
        case hydra.core.TypePair(value=pt):
            return _hoist_hydra_java_coder_find_pair_first_1(hydra.strip.deannotate_type(pt.first))

        case _:
            return Nothing()

def find_self_ref_var(grouped: FrozenDict[T0, frozenlist[T0]]) -> Maybe[T0]:
    @lru_cache(1)
    def self_refs() -> frozenlist[tuple[T0, frozenlist[T0]]]:
        return hydra.lib.lists.filter((lambda entry: hydra.lib.lists.elem(hydra.lib.pairs.first(entry), hydra.lib.pairs.second(entry))), hydra.lib.maps.to_list(grouped))
    return hydra.lib.logic.if_else(hydra.lib.lists.null(self_refs()), (lambda : Nothing()), (lambda : Just(hydra.lib.pairs.first(hydra.lib.lists.head(self_refs())))))

def group_pairs_by_first(pairs: frozenlist[tuple[T0, T1]]) -> FrozenDict[T0, frozenlist[T1]]:
    return hydra.lib.lists.foldl((lambda m, p: (k := hydra.lib.pairs.first(p), v := hydra.lib.pairs.second(p), hydra.lib.maps.alter((lambda mv: hydra.lib.maybes.maybe((lambda : Just((v,))), (lambda vs: Just(hydra.lib.lists.concat2(vs, (v,)))), mv)), k, m))[2]), hydra.lib.maps.empty(), pairs)

def name_map_to_type_map(m: FrozenDict[T0, hydra.core.Name]) -> FrozenDict[T0, hydra.core.Type]:
    return hydra.lib.maps.map((lambda v: cast(hydra.core.Type, hydra.core.TypeVariable(v))), m)

def self_ref_substitution_process_group(subst: FrozenDict[T0, T0], in_var: T0, out_vars: frozenlist[T0]) -> FrozenDict[T0, T0]:
    return hydra.lib.logic.if_else(hydra.lib.lists.elem(in_var, out_vars), (lambda : hydra.lib.lists.foldl((lambda s, v: hydra.lib.logic.if_else(hydra.lib.equality.equal(v, in_var), (lambda : s), (lambda : hydra.lib.maps.insert(v, in_var, s)))), subst, out_vars)), (lambda : subst))

def self_ref_substitution(grouped: FrozenDict[T0, frozenlist[T0]]) -> FrozenDict[T0, T0]:
    return hydra.lib.lists.foldl((lambda subst, entry: self_ref_substitution_process_group(subst, hydra.lib.pairs.first(entry), hydra.lib.pairs.second(entry))), hydra.lib.maps.empty(), hydra.lib.maps.to_list(grouped))

def detect_accumulator_unification(doms: frozenlist[hydra.core.Type], cod: hydra.core.Type, tparams: frozenlist[hydra.core.Name]) -> FrozenDict[hydra.core.Name, hydra.core.Type]:
    @lru_cache(1)
    def tparam_set() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(tparams)
    @lru_cache(1)
    def all_pairs() -> frozenlist[tuple[hydra.core.Name, hydra.core.Name]]:
        return hydra.lib.lists.bind(doms, (lambda d: extract_in_out_pair(d)))
    @lru_cache(1)
    def grouped_by_input() -> FrozenDict[hydra.core.Name, frozenlist[hydra.core.Name]]:
        return group_pairs_by_first(all_pairs())
    @lru_cache(1)
    def self_ref_subst() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        return self_ref_substitution(grouped_by_input())
    @lru_cache(1)
    def direct_pairs() -> frozenlist[tuple[hydra.core.Name, hydra.core.Name]]:
        return hydra.lib.lists.bind(doms, (lambda d: extract_direct_return(tparam_set(), d)))
    @lru_cache(1)
    def grouped_direct() -> FrozenDict[hydra.core.Name, frozenlist[hydra.core.Name]]:
        return group_pairs_by_first(direct_pairs())
    @lru_cache(1)
    def direct_input_vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(hydra.lib.lists.map((lambda p: hydra.lib.pairs.first(p)), direct_pairs()))
    @lru_cache(1)
    def cod_var() -> Maybe[hydra.core.Name]:
        match hydra.strip.deannotate_type(cod):
            case hydra.core.TypeVariable(value=v):
                return Just(v)

            case _:
                return Nothing()
    @lru_cache(1)
    def direct_ref_subst() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        return direct_ref_substitution(direct_input_vars(), cod_var(), grouped_direct())
    @lru_cache(1)
    def cod_subst() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        return hydra.lib.maybes.maybe((lambda : hydra.lib.maps.empty()), (lambda cv: hydra.lib.logic.if_else(hydra.lib.maps.member(cv, self_ref_subst()), (lambda : hydra.lib.maps.empty()), (lambda : hydra.lib.maybes.maybe((lambda : hydra.lib.maps.empty()), (lambda ref_var: hydra.lib.logic.if_else(hydra.lib.equality.equal(cv, ref_var), (lambda : hydra.lib.maps.empty()), (lambda : hydra.lib.maps.singleton(cv, ref_var)))), find_self_ref_var(grouped_by_input()))))), find_pair_first(cod))
    @lru_cache(1)
    def dom_vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(hydra.lib.lists.bind(doms, (lambda d: hydra.lib.sets.to_list(collect_type_vars(d)))))
    @lru_cache(1)
    def dangling_subst() -> FrozenDict[hydra.core.Name, hydra.core.Type]:
        return hydra.lib.maybes.maybe((lambda : hydra.lib.maps.empty()), (lambda cv: hydra.lib.logic.if_else(hydra.lib.sets.member(cv, dom_vars()), (lambda : hydra.lib.maps.empty()), (lambda : hydra.lib.maybes.maybe((lambda : hydra.lib.maps.empty()), (lambda ref_var: hydra.lib.maps.singleton(cv, cast(hydra.core.Type, hydra.core.TypeVariable(ref_var)))), find_self_ref_var(grouped_by_input()))))), find_pair_first(cod))
    return hydra.lib.maps.union(hydra.lib.maps.union(hydra.lib.maps.union(name_map_to_type_map(self_ref_subst()), name_map_to_type_map(cod_subst())), dangling_subst()), name_map_to_type_map(direct_ref_subst()))

def filter_by_flags(xs: frozenlist[T0], flags: frozenlist[bool]) -> frozenlist[T0]:
    return hydra.lib.lists.map((lambda p: hydra.lib.pairs.first(p)), hydra.lib.lists.filter((lambda p: hydra.lib.pairs.second(p)), hydra.lib.lists.zip(xs, flags)))

def is_simple_name(name: hydra.core.Name) -> bool:
    return hydra.lib.equality.equal(hydra.lib.lists.length(hydra.lib.strings.split_on(".", name.value)), 1)

def correct_type_apps(gr: T0, name: hydra.core.Name, args: frozenlist[hydra.core.Term], fallback_type_apps: frozenlist[hydra.core.Type], cx: T1, g: hydra.graph.Graph) -> Either[hydra.errors.Error, frozenlist[hydra.core.Type]]:
    return hydra.lib.eithers.bind(Right(hydra.lexical.lookup_binding(g, name)), (lambda mel: hydra.lib.maybes.cases(mel, (lambda : Right(fallback_type_apps)), (lambda el: hydra.lib.maybes.cases(el.type, (lambda : Right(fallback_type_apps)), (lambda ts: (scheme_type := ts.type, all_scheme_vars := hydra.lib.lists.filter((lambda v: is_simple_name(v)), ts.variables), scheme_type_vars := collect_type_vars(scheme_type), used_flags := hydra.lib.lists.map((lambda v: hydra.lib.sets.member(v, scheme_type_vars)), all_scheme_vars), used_scheme_vars := filter_by_flags(all_scheme_vars, used_flags), n_params := count_function_params(scheme_type), peeled := peel_domain_types(n_params, scheme_type), callee_doms := hydra.lib.pairs.first(peeled), callee_cod := hydra.lib.pairs.second(peeled), overgen_subst := detect_accumulator_unification(callee_doms, callee_cod, used_scheme_vars), keep_flags := hydra.lib.lists.map((lambda v: hydra.lib.logic.and_(hydra.lib.sets.member(v, scheme_type_vars), hydra.lib.logic.not_(hydra.lib.maps.member(v, overgen_subst)))), all_scheme_vars), scheme_vars := filter_by_flags(all_scheme_vars, keep_flags), filtered_fallback0 := hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(all_scheme_vars), hydra.lib.lists.length(fallback_type_apps)), (lambda : filter_by_flags(fallback_type_apps, keep_flags)), (lambda : fallback_type_apps)), filtered_fallback := hydra.lib.logic.if_else(hydra.lib.maps.null(overgen_subst), (lambda : filtered_fallback0), (lambda : hydra.lib.lists.map((lambda t: substitute_type_vars_with_types(overgen_subst, t)), filtered_fallback0))), hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.lib.lists.null(scheme_vars), hydra.lib.logic.not_(hydra.lib.equality.equal(hydra.lib.lists.length(scheme_vars), hydra.lib.lists.length(filtered_fallback)))), (lambda : Right(filtered_fallback)), (lambda : correct_type_apps_with_args(scheme_vars, filtered_fallback, scheme_type, args, cx, g))))[14]))))))

def extract_type_application_args_go(t: hydra.core.Type) -> frozenlist[hydra.core.Type]:
    match t:
        case hydra.core.TypeApplication(value=at):
            return hydra.lib.lists.cons(at.argument, extract_type_application_args_go(at.function))

        case _:
            return ()

def extract_type_application_args(typ: hydra.core.Type) -> frozenlist[hydra.core.Type]:
    return hydra.lib.lists.reverse(extract_type_application_args_go(typ))

def java_type_parameters_for_type_bvars(t: hydra.core.Type) -> frozenlist[hydra.core.Name]:
    match t:
        case hydra.core.TypeForall(value=ft):
            return hydra.lib.lists.cons(ft.parameter, java_type_parameters_for_type_bvars(ft.body))

        case _:
            return ()

def java_type_parameters_for_type(typ: hydra.core.Type) -> frozenlist[hydra.java.syntax.TypeParameter]:
    def to_param(name: hydra.core.Name) -> hydra.java.syntax.TypeParameter:
        return hydra.java.utils.java_type_parameter(hydra.formatting.capitalize(name.value))
    @lru_cache(1)
    def bound_vars() -> frozenlist[hydra.core.Name]:
        return java_type_parameters_for_type_bvars(typ)
    @lru_cache(1)
    def free_vars() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.filter((lambda v: is_lambda_bound_variable(v)), hydra.lib.sets.to_list(hydra.variables.free_variables_in_type(typ)))
    @lru_cache(1)
    def vars() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.nub(hydra.lib.lists.concat2(bound_vars(), free_vars()))
    return hydra.lib.lists.map((lambda x1: to_param(x1)), vars())

def java_type_arguments_for_type(typ: hydra.core.Type) -> frozenlist[hydra.java.syntax.TypeArgument]:
    return hydra.lib.lists.reverse(hydra.lib.lists.map((lambda x1: hydra.java.utils.type_parameter_to_type_argument(x1)), java_type_parameters_for_type(typ)))

def dom_type_args(aliases: hydra.java.environment.Aliases, d: hydra.core.Type, cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, frozenlist[hydra.java.syntax.TypeArgument]]:
    @lru_cache(1)
    def args() -> frozenlist[hydra.core.Type]:
        return extract_type_application_args(hydra.strip.deannotate_type(d))
    return hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(args())), (lambda : hydra.lib.eithers.map_list((lambda t: hydra.lib.eithers.bind(encode_type(aliases, hydra.lib.sets.empty(), t, cx, g), (lambda jt: hydra.lib.eithers.bind(hydra.java.utils.java_type_to_java_reference_type(jt, cx), (lambda rt: Right(cast(hydra.java.syntax.TypeArgument, hydra.java.syntax.TypeArgumentReference(rt)))))))), args())), (lambda : Right(java_type_arguments_for_type(d))))

def inner_class_ref(aliases: hydra.java.environment.Aliases, name: hydra.core.Name, local: str) -> hydra.java.syntax.Identifier:
    @lru_cache(1)
    def id() -> str:
        return hydra.java.utils.name_to_java_name(aliases, name).value
    return hydra.java.syntax.Identifier(hydra.lib.strings.cat2(hydra.lib.strings.cat2(id(), "."), local))

def insert_branch_var(name: hydra.core.Name, env: hydra.java.environment.JavaEnvironment) -> hydra.java.environment.JavaEnvironment:
    aliases = env.aliases
    return hydra.java.environment.JavaEnvironment(hydra.java.environment.Aliases(aliases.current_namespace, aliases.packages, hydra.lib.sets.insert(name, aliases.branch_vars), aliases.recursive_vars, aliases.in_scope_type_params, aliases.polymorphic_locals, aliases.in_scope_java_vars, aliases.var_renames, aliases.lambda_vars, aliases.type_var_subst, aliases.trusted_type_vars, Nothing(), aliases.thunked_vars), env.graph)

def with_lambda(env: hydra.java.environment.JavaEnvironment, lam: hydra.core.Lambda, k: Callable[[hydra.java.environment.JavaEnvironment], T0]) -> T0:
    return hydra.environment.with_lambda_context((lambda x1: java_env_get_graph(x1)), (lambda x1, x2: java_env_set_graph(x1, x2)), env, lam, (lambda env1: (aliases := env1.aliases, aliases2 := hydra.java.environment.Aliases(aliases.current_namespace, aliases.packages, aliases.branch_vars, aliases.recursive_vars, aliases.in_scope_type_params, aliases.polymorphic_locals, aliases.in_scope_java_vars, aliases.var_renames, hydra.lib.sets.insert(lam.parameter, aliases.lambda_vars), aliases.type_var_subst, aliases.trusted_type_vars, aliases.method_codomain, aliases.thunked_vars), env2 := hydra.java.environment.JavaEnvironment(aliases2, env1.graph), k(env2))[3]))

def filter_phantom_type_args_filter_and_apply(all_type_args: frozenlist[hydra.core.Type], keep_flags: frozenlist[bool], overgen_subst: FrozenDict[hydra.core.Name, hydra.core.Type]) -> frozenlist[hydra.core.Type]:
    @lru_cache(1)
    def filtered() -> frozenlist[hydra.core.Type]:
        return hydra.lib.lists.map((lambda p: hydra.lib.pairs.first(p)), hydra.lib.lists.filter((lambda p: hydra.lib.pairs.second(p)), hydra.lib.lists.zip(all_type_args, keep_flags)))
    return hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.maps.null(overgen_subst)), (lambda : hydra.lib.lists.map((lambda t: substitute_type_vars_with_types(overgen_subst, t)), filtered())), (lambda : filtered()))

def filter_phantom_type_args(callee_name: hydra.core.Name, all_type_args: frozenlist[hydra.core.Type], cx: T0, g: hydra.graph.Graph) -> Either[T1, frozenlist[hydra.core.Type]]:
    return hydra.lib.eithers.bind(Right(hydra.lexical.lookup_binding(g, callee_name)), (lambda mel: hydra.lib.maybes.cases(mel, (lambda : Right(all_type_args)), (lambda el: hydra.lib.maybes.cases(el.type, (lambda : Right(all_type_args)), (lambda ts: (scheme_vars := hydra.lib.lists.filter((lambda v: is_simple_name(v)), ts.variables), scheme_type_vars := collect_type_vars(ts.type), scheme_type := ts.type, n_params := count_function_params(scheme_type), peeled := peel_domain_types(n_params, scheme_type), callee_doms := hydra.lib.pairs.first(peeled), callee_cod := hydra.lib.pairs.second(peeled), overgen_subst := detect_accumulator_unification(callee_doms, callee_cod, scheme_vars), keep_flags := hydra.lib.lists.map((lambda v: hydra.lib.logic.and_(hydra.lib.sets.member(v, scheme_type_vars), hydra.lib.logic.not_(hydra.lib.maps.member(v, overgen_subst)))), scheme_vars), hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.equality.equal(hydra.lib.lists.length(scheme_vars), hydra.lib.lists.length(all_type_args))), (lambda : Right(all_type_args)), (lambda : Right(filter_phantom_type_args_filter_and_apply(all_type_args, keep_flags, overgen_subst)))))[9]))))))

def element_java_identifier_qualify(aliases: hydra.java.environment.Aliases, mns: Maybe[hydra.packaging.Namespace], s: str) -> str:
    return hydra.java.utils.name_to_java_name(aliases, hydra.names.unqualify_name(hydra.packaging.QualifiedName(mns, s))).value

def elements_class_name(ns: hydra.packaging.Namespace) -> str:
    ns_str = ns.value
    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.strings.split_on(".", ns_str)
    return hydra.formatting.sanitize_with_underscores(hydra.java.language.reserved_words(), hydra.formatting.capitalize(hydra.lib.lists.last(parts())))

def namespace_parent(ns: hydra.packaging.Namespace) -> Maybe[hydra.packaging.Namespace]:
    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.strings.split_on(".", ns.value)
    return hydra.lib.logic.if_else(hydra.lib.lists.null(hydra.lib.lists.init(parts())), (lambda : Nothing()), (lambda : Just(hydra.packaging.Namespace(hydra.lib.strings.intercalate(".", hydra.lib.lists.init(parts()))))))

def element_java_identifier(is_prim: bool, is_method: bool, aliases: hydra.java.environment.Aliases, name: hydra.core.Name) -> hydra.java.syntax.Identifier:
    @lru_cache(1)
    def qn() -> hydra.packaging.QualifiedName:
        return hydra.names.qualify_name(name)
    ns_ = qn().namespace
    local = qn().local
    @lru_cache(1)
    def sep() -> str:
        return hydra.lib.logic.if_else(is_method, (lambda : "::"), (lambda : "."))
    return hydra.lib.logic.if_else(is_prim, (lambda : hydra.java.syntax.Identifier(hydra.lib.strings.cat2(hydra.lib.strings.cat2(element_java_identifier_qualify(aliases, ns_, hydra.formatting.capitalize(local)), "."), hydra.java.names.apply_method_name))), (lambda : hydra.lib.maybes.cases(ns_, (lambda : hydra.java.syntax.Identifier(hydra.java.utils.sanitize_java_name(local))), (lambda n: hydra.java.syntax.Identifier(hydra.lib.strings.cat2(hydra.lib.strings.cat2(element_java_identifier_qualify(aliases, namespace_parent(n), elements_class_name(n)), sep()), hydra.java.utils.sanitize_java_name(local)))))))

def elements_qualified_name(ns: hydra.packaging.Namespace) -> hydra.core.Name:
    return hydra.names.unqualify_name(hydra.packaging.QualifiedName(namespace_parent(ns), elements_class_name(ns)))

def encode_variable_build_curried(params: frozenlist[hydra.core.Name], inner: hydra.java.syntax.Expression) -> hydra.java.syntax.Expression:
    return hydra.lib.logic.if_else(hydra.lib.lists.null(params), (lambda : inner), (lambda : hydra.java.utils.java_lambda(hydra.lib.lists.head(params), encode_variable_build_curried(hydra.lib.lists.tail(params), inner))))

def encode_variable_hoisted_lambda_case(aliases: hydra.java.environment.Aliases, name: hydra.core.Name, arity: int, cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.Expression]:
    @lru_cache(1)
    def param_names() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.map((lambda i: hydra.core.Name(hydra.lib.strings.cat2("p", hydra.lib.literals.show_int32(i)))), hydra.lib.math.range_(0, hydra.lib.math.sub(arity, 1)))
    @lru_cache(1)
    def param_exprs() -> frozenlist[hydra.java.syntax.Expression]:
        return hydra.lib.lists.map((lambda pn: hydra.java.utils.java_identifier_to_java_expression(hydra.java.utils.variable_to_java_identifier(pn))), param_names())
    @lru_cache(1)
    def call() -> hydra.java.syntax.Expression:
        return hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation(Nothing(), element_java_identifier(False, False, aliases, name), param_exprs()))
    @lru_cache(1)
    def lam() -> hydra.java.syntax.Expression:
        return encode_variable_build_curried(param_names(), call())
    return hydra.lib.eithers.bind(Right(hydra.lexical.lookup_binding(g, name)), (lambda mel: hydra.lib.maybes.cases(mel, (lambda : Right(lam())), (lambda el: hydra.lib.maybes.cases(el.type, (lambda : Right(lam())), (lambda ts: (typ := ts.type, hydra.lib.eithers.bind(encode_type(aliases, hydra.lib.sets.empty(), typ, cx, g), (lambda jtype: hydra.lib.eithers.bind(hydra.java.utils.java_type_to_java_reference_type(jtype, cx), (lambda rt: Right(hydra.java.utils.java_cast_expression_to_java_expression(hydra.java.utils.java_cast_expression(rt, hydra.java.utils.java_expression_to_java_unary_expression(lam())))))))))[1]))))))

def is_lambda_bound_in_is_qualified(n: hydra.core.Name) -> bool:
    return hydra.lib.maybes.is_just(hydra.names.qualify_name(n).namespace)

def find_matching_lambda_var(name: hydra.core.Name, lambda_vars: frozenset[hydra.core.Name]) -> hydra.core.Name:
    return hydra.lib.logic.if_else(hydra.lib.sets.member(name, lambda_vars), (lambda : name), (lambda : hydra.lib.logic.if_else(is_lambda_bound_in_is_qualified(name), (lambda : hydra.lib.maybes.from_maybe((lambda : name), hydra.lib.lists.find((lambda lv: hydra.lib.logic.and_(is_lambda_bound_in_is_qualified(lv), hydra.lib.equality.equal(hydra.names.local_name_of(lv), hydra.names.local_name_of(name)))), hydra.lib.sets.to_list(lambda_vars)))), (lambda : hydra.lib.logic.if_else(hydra.lib.sets.member(hydra.core.Name(hydra.names.local_name_of(name)), lambda_vars), (lambda : hydra.core.Name(hydra.names.local_name_of(name))), (lambda : name))))))

def is_lambda_bound_in(name: hydra.core.Name, lambda_vars: frozenset[hydra.core.Name]) -> bool:
    return hydra.lib.logic.or_(hydra.lib.sets.member(name, lambda_vars), hydra.lib.logic.or_(hydra.lib.logic.and_(is_lambda_bound_in_is_qualified(name), hydra.lib.maybes.is_just(hydra.lib.lists.find((lambda lv: hydra.lib.logic.and_(is_lambda_bound_in_is_qualified(lv), hydra.lib.equality.equal(hydra.names.local_name_of(lv), hydra.names.local_name_of(name)))), hydra.lib.sets.to_list(lambda_vars)))), hydra.lib.logic.and_(hydra.lib.logic.not_(is_lambda_bound_in_is_qualified(name)), hydra.lib.sets.member(hydra.core.Name(hydra.names.local_name_of(name)), lambda_vars))))

def is_recursive_variable(aliases: hydra.java.environment.Aliases, name: hydra.core.Name) -> bool:
    return hydra.lib.sets.member(name, aliases.recursive_vars)

def encode_variable(env: hydra.java.environment.JavaEnvironment, name: hydra.core.Name, cx: T0, g: hydra.graph.Graph):
    aliases = env.aliases
    @lru_cache(1)
    def resolved_name() -> hydra.core.Name:
        return hydra.java.utils.lookup_java_var_name(aliases, name)
    @lru_cache(1)
    def jid() -> hydra.java.syntax.Identifier:
        return hydra.java.utils.java_identifier(resolved_name().value)
    def _hoist_jid_body_1(v1):
        match v1:
            case hydra.java.environment.JavaSymbolClassHoistedLambda(value=arity):
                return encode_variable_hoisted_lambda_case(aliases, name, arity, cx, g)

            case hydra.java.environment.JavaSymbolClassLocalVariable():
                return Right(hydra.java.utils.java_identifier_to_java_expression(element_java_identifier(False, False, aliases, resolved_name())))

            case hydra.java.environment.JavaSymbolClassConstant():
                return Right(hydra.java.utils.java_identifier_to_java_expression(element_java_identifier(False, False, aliases, name)))

            case hydra.java.environment.JavaSymbolClassNullaryFunction():
                return Right(hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation(Nothing(), element_java_identifier(False, False, aliases, name), ())))

            case hydra.java.environment.JavaSymbolClassUnaryFunction():
                return Right(hydra.java.utils.java_identifier_to_java_expression(element_java_identifier(False, True, aliases, name)))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.lib.logic.if_else(hydra.lib.sets.member(name, aliases.branch_vars), (lambda : Right(hydra.java.utils.java_field_access_to_java_expression(hydra.java.syntax.FieldAccess(cast(hydra.java.syntax.FieldAccess_Qualifier, hydra.java.syntax.FieldAccess_QualifierPrimary(hydra.java.utils.java_expression_to_java_primary(hydra.java.utils.java_identifier_to_java_expression(jid())))), hydra.java.utils.java_identifier(hydra.java.names.value_field_name))))), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.equal(name, hydra.core.Name(hydra.lib.strings.cat((hydra.java.names.instance_name, "_", hydra.java.names.value_field_name)))), is_recursive_variable(aliases, name)), (lambda : (instance_expr := hydra.java.utils.java_identifier_to_java_expression(hydra.java.utils.java_identifier(hydra.java.names.instance_name)), Right(hydra.java.utils.java_field_access_to_java_expression(hydra.java.syntax.FieldAccess(cast(hydra.java.syntax.FieldAccess_Qualifier, hydra.java.syntax.FieldAccess_QualifierPrimary(hydra.java.utils.java_expression_to_java_primary(instance_expr))), hydra.java.utils.java_identifier(hydra.java.names.value_field_name)))))[1]), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.and_(is_recursive_variable(aliases, name), hydra.lib.logic.not_(is_lambda_bound_in(name, aliases.lambda_vars))), (lambda : Right(hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation(Just(Left(hydra.java.syntax.ExpressionName(Nothing(), jid()))), hydra.java.syntax.Identifier(hydra.java.names.get_method_name), ())))), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.sets.member(name, aliases.thunked_vars), hydra.lib.logic.not_(is_lambda_bound_in(name, aliases.lambda_vars))), (lambda : Right(hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation(Just(Left(hydra.java.syntax.ExpressionName(Nothing(), jid()))), hydra.java.syntax.Identifier(hydra.java.names.get_method_name), ())))), (lambda : hydra.lib.logic.if_else(is_lambda_bound_in(name, aliases.lambda_vars), (lambda : (actual_name := find_matching_lambda_var(name, aliases.lambda_vars), (resolved_actual := hydra.java.utils.lookup_java_var_name(aliases, actual_name), Right(hydra.java.utils.java_identifier_to_java_expression(hydra.java.utils.variable_to_java_identifier(resolved_actual))))[1])[1]), (lambda : hydra.lib.logic.if_else(hydra.lib.sets.member(name, aliases.in_scope_java_vars), (lambda : Right(hydra.java.utils.java_identifier_to_java_expression(element_java_identifier(False, False, aliases, resolved_name())))), (lambda : hydra.lib.eithers.bind(classify_data_reference(name, cx, g), (lambda cls: _hoist_jid_body_1(cls)))))))))))))))

def is_local_variable(name: hydra.core.Name) -> bool:
    return hydra.lib.maybes.is_nothing(hydra.names.qualify_name(name).namespace)

def wrap_in_supplier_lambda(expr: hydra.java.syntax.Expression) -> hydra.java.syntax.Expression:
    return cast(hydra.java.syntax.Expression, hydra.java.syntax.ExpressionLambda(hydra.java.syntax.LambdaExpression(cast(hydra.java.syntax.LambdaParameters, hydra.java.syntax.LambdaParametersTuple(())), cast(hydra.java.syntax.LambdaBody, hydra.java.syntax.LambdaBodyExpression(expr)))))

def wrap_lazy_arguments(name: hydra.core.Name, args: frozenlist[hydra.java.syntax.Expression]) -> tuple[frozenlist[hydra.java.syntax.Expression], Maybe[str]]:
    return hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.equal(name, hydra.core.Name("hydra.lib.logic.ifElse")), hydra.lib.equality.equal(hydra.lib.lists.length(args), 3)), (lambda : ((hydra.lib.lists.at(0, args), wrap_in_supplier_lambda(hydra.lib.lists.at(1, args)), wrap_in_supplier_lambda(hydra.lib.lists.at(2, args))), Just("lazy"))), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.equal(name, hydra.core.Name("hydra.lib.maybes.maybe")), hydra.lib.equality.equal(hydra.lib.lists.length(args), 3)), (lambda : ((wrap_in_supplier_lambda(hydra.lib.lists.at(0, args)), hydra.lib.lists.at(1, args), hydra.lib.lists.at(2, args)), Just("applyLazy"))), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.equal(name, hydra.core.Name("hydra.lib.maybes.cases")), hydra.lib.equality.equal(hydra.lib.lists.length(args), 3)), (lambda : ((hydra.lib.lists.at(0, args), wrap_in_supplier_lambda(hydra.lib.lists.at(1, args)), hydra.lib.lists.at(2, args)), Just("applyLazy"))), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.equal(name, hydra.core.Name("hydra.lib.maps.findWithDefault")), hydra.lib.equality.equal(hydra.lib.lists.length(args), 3)), (lambda : ((wrap_in_supplier_lambda(hydra.lib.lists.at(0, args)), hydra.lib.lists.at(1, args), hydra.lib.lists.at(2, args)), Just("applyLazy"))), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.logic.or_(hydra.lib.equality.equal(name, hydra.core.Name("hydra.lib.maybes.fromMaybe")), hydra.lib.logic.or_(hydra.lib.equality.equal(name, hydra.core.Name("hydra.lib.eithers.fromLeft")), hydra.lib.equality.equal(name, hydra.core.Name("hydra.lib.eithers.fromRight")))), hydra.lib.equality.equal(hydra.lib.lists.length(args), 2)), (lambda : ((wrap_in_supplier_lambda(hydra.lib.lists.at(0, args)), hydra.lib.lists.at(1, args)), Just("applyLazy"))), (lambda : (args, Nothing())))))))))))

def encode_literal_java_special_float_expr(class_name: str, field_name: str) -> hydra.java.syntax.Expression:
    return hydra.java.utils.java_expression_name_to_java_expression(hydra.java.syntax.ExpressionName(Just(hydra.java.syntax.AmbiguousName((hydra.java.syntax.Identifier(class_name),))), hydra.java.syntax.Identifier(field_name)))

def encode_literal_lit_exp(l: hydra.java.syntax.Literal) -> hydra.java.syntax.Expression:
    return hydra.java.utils.java_literal_to_java_expression(l)

def encode_literal_prim_cast(pt: hydra.java.syntax.PrimitiveType, expr: hydra.java.syntax.Expression) -> hydra.java.syntax.Expression:
    return hydra.java.utils.java_cast_expression_to_java_expression(hydra.java.utils.java_cast_primitive(pt, hydra.java.utils.java_expression_to_java_unary_expression(expr)))

def encode_literal_encode_float32(v: float) -> hydra.java.syntax.Expression:
    @lru_cache(1)
    def s() -> str:
        return hydra.lib.literals.show_float32(v)
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(s(), "NaN"), (lambda : encode_literal_java_special_float_expr("Float", "NaN")), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(s(), "Infinity"), (lambda : encode_literal_java_special_float_expr("Float", "POSITIVE_INFINITY")), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(s(), "-Infinity"), (lambda : encode_literal_java_special_float_expr("Float", "NEGATIVE_INFINITY")), (lambda : encode_literal_prim_cast(cast(hydra.java.syntax.PrimitiveType, hydra.java.syntax.PrimitiveTypeNumeric(cast(hydra.java.syntax.NumericType, hydra.java.syntax.NumericTypeFloatingPoint(hydra.java.syntax.FloatingPointType.FLOAT)))), encode_literal_lit_exp(cast(hydra.java.syntax.Literal, hydra.java.syntax.LiteralFloatingPoint(hydra.java.syntax.FloatingPointLiteral(hydra.lib.literals.float32_to_bigfloat(v))))))))))))

def encode_literal(lit: hydra.core.Literal) -> hydra.java.syntax.Expression:
    match lit:
        case hydra.core.LiteralBinary(value=bs):
            @lru_cache(1)
            def byte_values() -> frozenlist[int]:
                return hydra.lib.literals.binary_to_bytes(bs)
            return hydra.java.utils.java_array_creation(hydra.java.utils.java_byte_primitive_type(), Just(hydra.java.utils.java_array_initializer(hydra.lib.lists.map((lambda w: hydra.java.utils.java_literal_to_java_expression(cast(hydra.java.syntax.Literal, hydra.java.syntax.LiteralInteger(hydra.java.syntax.IntegerLiteral(hydra.lib.literals.int32_to_bigint(w)))))), byte_values()))))

        case hydra.core.LiteralBoolean(value=b):
            return encode_literal_lit_exp(hydra.java.utils.java_boolean(b))

        case hydra.core.LiteralFloat(value=f):
            return encode_literal_encode_float(f)

        case hydra.core.LiteralInteger(value=i):
            return encode_literal_encode_integer(i)

        case hydra.core.LiteralString(value=s):
            return encode_literal_lit_exp(hydra.java.utils.java_string(s))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_literal_encode_float(f: hydra.core.FloatValue) -> hydra.java.syntax.Expression:
    match f:
        case hydra.core.FloatValueBigfloat(value=v):
            return hydra.java.utils.java_constructor_call(hydra.java.utils.java_constructor_name(hydra.java.syntax.Identifier("java.math.BigDecimal"), Nothing()), (encode_literal(cast(hydra.core.Literal, hydra.core.LiteralString(hydra.lib.literals.show_bigfloat(v)))),), Nothing())

        case hydra.core.FloatValueFloat32(value=v2):
            return encode_literal_encode_float32(v2)

        case hydra.core.FloatValueFloat64(value=v3):
            return encode_literal_encode_float64(v3)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_literal_encode_float64(v: float) -> hydra.java.syntax.Expression:
    @lru_cache(1)
    def s() -> str:
        return hydra.lib.literals.show_float64(v)
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(s(), "NaN"), (lambda : encode_literal_java_special_float_expr("Double", "NaN")), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(s(), "Infinity"), (lambda : encode_literal_java_special_float_expr("Double", "POSITIVE_INFINITY")), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(s(), "-Infinity"), (lambda : encode_literal_java_special_float_expr("Double", "NEGATIVE_INFINITY")), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(s(), "-0.0"), (lambda : encode_literal_java_parse_double("-0.0")), (lambda : encode_literal_lit_exp(cast(hydra.java.syntax.Literal, hydra.java.syntax.LiteralFloatingPoint(hydra.java.syntax.FloatingPointLiteral(hydra.lib.literals.float64_to_bigfloat(v)))))))))))))

def encode_literal_encode_integer(i: hydra.core.IntegerValue) -> hydra.java.syntax.Expression:
    match i:
        case hydra.core.IntegerValueBigint(value=v):
            return hydra.java.utils.java_constructor_call(hydra.java.utils.java_constructor_name(hydra.java.syntax.Identifier("java.math.BigInteger"), Nothing()), (encode_literal(cast(hydra.core.Literal, hydra.core.LiteralString(hydra.lib.literals.show_bigint(v)))),), Nothing())

        case hydra.core.IntegerValueInt8(value=v2):
            return encode_literal_prim_cast(cast(hydra.java.syntax.PrimitiveType, hydra.java.syntax.PrimitiveTypeNumeric(cast(hydra.java.syntax.NumericType, hydra.java.syntax.NumericTypeIntegral(hydra.java.syntax.IntegralType.BYTE)))), encode_literal_lit_exp(cast(hydra.java.syntax.Literal, hydra.java.syntax.LiteralInteger(hydra.java.syntax.IntegerLiteral(hydra.lib.literals.int8_to_bigint(v2))))))

        case hydra.core.IntegerValueInt16(value=v3):
            return encode_literal_prim_cast(cast(hydra.java.syntax.PrimitiveType, hydra.java.syntax.PrimitiveTypeNumeric(cast(hydra.java.syntax.NumericType, hydra.java.syntax.NumericTypeIntegral(hydra.java.syntax.IntegralType.SHORT)))), encode_literal_lit_exp(cast(hydra.java.syntax.Literal, hydra.java.syntax.LiteralInteger(hydra.java.syntax.IntegerLiteral(hydra.lib.literals.int16_to_bigint(v3))))))

        case hydra.core.IntegerValueInt32(value=v4):
            return encode_literal_lit_exp(cast(hydra.java.syntax.Literal, hydra.java.syntax.LiteralInteger(hydra.java.syntax.IntegerLiteral(hydra.lib.literals.int32_to_bigint(v4)))))

        case hydra.core.IntegerValueInt64(value=v5):
            return encode_literal_prim_cast(cast(hydra.java.syntax.PrimitiveType, hydra.java.syntax.PrimitiveTypeNumeric(cast(hydra.java.syntax.NumericType, hydra.java.syntax.NumericTypeIntegral(hydra.java.syntax.IntegralType.LONG)))), encode_literal_lit_exp(cast(hydra.java.syntax.Literal, hydra.java.syntax.LiteralInteger(hydra.java.syntax.IntegerLiteral(hydra.lib.literals.int64_to_bigint(v5))))))

        case hydra.core.IntegerValueUint8(value=v6):
            return encode_literal_prim_cast(cast(hydra.java.syntax.PrimitiveType, hydra.java.syntax.PrimitiveTypeNumeric(cast(hydra.java.syntax.NumericType, hydra.java.syntax.NumericTypeIntegral(hydra.java.syntax.IntegralType.SHORT)))), encode_literal_lit_exp(cast(hydra.java.syntax.Literal, hydra.java.syntax.LiteralInteger(hydra.java.syntax.IntegerLiteral(hydra.lib.literals.uint8_to_bigint(v6))))))

        case hydra.core.IntegerValueUint16(value=v7):
            return encode_literal_lit_exp(cast(hydra.java.syntax.Literal, hydra.java.syntax.LiteralCharacter(v7)))

        case hydra.core.IntegerValueUint32(value=v8):
            return encode_literal_prim_cast(cast(hydra.java.syntax.PrimitiveType, hydra.java.syntax.PrimitiveTypeNumeric(cast(hydra.java.syntax.NumericType, hydra.java.syntax.NumericTypeIntegral(hydra.java.syntax.IntegralType.LONG)))), encode_literal_lit_exp(cast(hydra.java.syntax.Literal, hydra.java.syntax.LiteralInteger(hydra.java.syntax.IntegerLiteral(hydra.lib.literals.uint32_to_bigint(v8))))))

        case hydra.core.IntegerValueUint64(value=v9):
            return hydra.java.utils.java_constructor_call(hydra.java.utils.java_constructor_name(hydra.java.syntax.Identifier("java.math.BigInteger"), Nothing()), (encode_literal(cast(hydra.core.Literal, hydra.core.LiteralString(hydra.lib.literals.show_bigint(hydra.lib.literals.uint64_to_bigint(v9))))),), Nothing())

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_literal_java_parse_double(value: str) -> hydra.java.syntax.Expression:
    return hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation_static(hydra.java.syntax.Identifier("Double"), hydra.java.syntax.Identifier("parseDouble"), (encode_literal(cast(hydra.core.Literal, hydra.core.LiteralString(value))),)))

def encode_nullary_constant(env: T0, typ: T1, fun_term: hydra.core.Term, cx: T2, g: T3) -> Either[hydra.errors.Error, T4]:
    return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("unexpected ", hydra.lib.strings.cat2("nullary function", hydra.lib.strings.cat2(" in ", hydra.show.core.term(fun_term))))))))

def decode_type_from_term(term: hydra.core.Term):
    match hydra.strip.deannotate_term(term):
        case hydra.core.TermUnion(value=inj):
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(inj.type_name, hydra.core.Name("hydra.core.Type")), (lambda : (fname := inj.field.name.value, (fterm := inj.field.term, (_hoist_fterm_body_1 := (lambda v1: (lambda s: Just(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name(s)))))(v1.value) if isinstance(v1, hydra.core.LiteralString) else Nothing()), _hoist_fterm_body_2 := (lambda v1: (lambda lit: _hoist_fterm_body_1(lit))(v1.value) if isinstance(v1, hydra.core.TermLiteral) else Nothing()), _hoist_fterm_body_3 := (lambda v1: (lambda wt: _hoist_fterm_body_2(wt.body))(v1.value) if isinstance(v1, hydra.core.TermWrap) else Nothing()), _hoist_fterm_body_4 := (lambda v1: (lambda rec: hydra.lib.maybes.bind(hydra.lib.lists.safe_head(hydra.lib.lists.filter((lambda f: hydra.lib.equality.equal(f.name, hydra.core.Name("body"))), rec.fields)), (lambda body_field: decode_type_from_term(body_field.term))))(v1.value) if isinstance(v1, hydra.core.TermRecord) else Nothing()), _hoist_fterm_body_5 := (lambda v1: (lambda rec: hydra.lib.maybes.bind(hydra.lib.lists.safe_head(hydra.lib.lists.filter((lambda f: hydra.lib.equality.equal(f.name, hydra.core.Name("function"))), rec.fields)), (lambda func_field: hydra.lib.maybes.bind(decode_type_from_term(func_field.term), (lambda func: hydra.lib.maybes.bind(hydra.lib.lists.safe_head(hydra.lib.lists.filter((lambda f: hydra.lib.equality.equal(f.name, hydra.core.Name("argument"))), rec.fields)), (lambda arg_field: hydra.lib.maybes.map((lambda arg: cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(func, arg)))), decode_type_from_term(arg_field.term)))))))))(v1.value) if isinstance(v1, hydra.core.TermRecord) else Nothing()), _hoist_fterm_body_6 := (lambda v1: (lambda rec: hydra.lib.maybes.bind(hydra.lib.lists.safe_head(hydra.lib.lists.filter((lambda f: hydra.lib.equality.equal(f.name, hydra.core.Name("domain"))), rec.fields)), (lambda dom_field: hydra.lib.maybes.bind(decode_type_from_term(dom_field.term), (lambda dom: hydra.lib.maybes.bind(hydra.lib.lists.safe_head(hydra.lib.lists.filter((lambda f: hydra.lib.equality.equal(f.name, hydra.core.Name("codomain"))), rec.fields)), (lambda cod_field: hydra.lib.maybes.map((lambda cod: cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(dom, cod)))), decode_type_from_term(cod_field.term)))))))))(v1.value) if isinstance(v1, hydra.core.TermRecord) else Nothing()), _hoist_fterm_body_7 := (lambda v1: (lambda lit_inj: hydra.lib.logic.if_else(hydra.lib.equality.equal(lit_inj.field.name.value, "string"), (lambda : Just(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))))), (lambda : Nothing())))(v1.value) if isinstance(v1, hydra.core.TermUnion) else Nothing()), hydra.lib.logic.if_else(hydra.lib.equality.equal(fname, "variable"), (lambda : _hoist_fterm_body_3(fterm)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(fname, "annotated"), (lambda : _hoist_fterm_body_4(fterm)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(fname, "application"), (lambda : _hoist_fterm_body_5(fterm)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(fname, "function"), (lambda : _hoist_fterm_body_6(fterm)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(fname, "literal"), (lambda : _hoist_fterm_body_7(fterm)), (lambda : Nothing())))))))))))[7])[1])[1]), (lambda : Nothing()))

        case _:
            return Nothing()

def try_infer_function_type(fun_term: hydra.core.Term) -> Maybe[hydra.core.Type]:
    match hydra.strip.deannotate_term(fun_term):
        case hydra.core.TermLambda(value=lam):
            return hydra.lib.maybes.bind(lam.domain, (lambda dom: (m_cod := (_hoist_m_cod_1 := (lambda v1: (lambda at: hydra.lib.maybes.bind(hydra.lib.maps.lookup(hydra.constants.key_type, at.annotation), (lambda type_term: decode_type_from_term(type_term))))(v1.value) if isinstance(v1, hydra.core.TermAnnotated) else (lambda _inner_lam: try_infer_function_type(lam.body))(v1.value) if isinstance(v1, hydra.core.TermLambda) else Nothing()), _hoist_m_cod_1(lam.body))[1], hydra.lib.maybes.map((lambda cod: cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(dom, cod)))), m_cod))[1]))

        case _:
            return Nothing()

def build_curried_lambda(params: frozenlist[hydra.core.Name], inner: hydra.java.syntax.Expression) -> hydra.java.syntax.Expression:
    return hydra.lib.lists.foldl((lambda acc, p: hydra.java.utils.java_lambda(p, acc)), inner, hydra.lib.lists.reverse(params))

def encode_function_primitive_by_name(env: hydra.java.environment.JavaEnvironment, dom: hydra.core.Type, cod: hydra.core.Type, name: hydra.core.Name, cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.Expression]:
    aliases = env.aliases
    @lru_cache(1)
    def class_with_apply() -> str:
        return element_java_identifier(True, False, aliases, name).value
    @lru_cache(1)
    def suffix() -> str:
        return hydra.lib.strings.cat2(".", hydra.java.names.apply_method_name)
    @lru_cache(1)
    def class_name() -> str:
        return hydra.lib.strings.from_list(hydra.lib.lists.take(hydra.lib.math.sub(hydra.lib.strings.length(class_with_apply()), hydra.lib.strings.length(suffix())), hydra.lib.strings.to_list(class_with_apply())))
    @lru_cache(1)
    def arity() -> int:
        return hydra.arity.type_arity(cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(dom, cod))))
    return hydra.lib.logic.if_else(hydra.lib.equality.lte(arity(), 1), (lambda : Right(hydra.java.utils.java_identifier_to_java_expression(hydra.java.syntax.Identifier(hydra.lib.strings.cat((class_name(), "::", hydra.java.names.apply_method_name)))))), (lambda : (param_names := hydra.lib.lists.map((lambda i: hydra.core.Name(hydra.lib.strings.cat2("p", hydra.lib.literals.show_int32(i)))), hydra.lib.math.range_(0, hydra.lib.math.sub(arity(), 1))), (param_exprs := hydra.lib.lists.map((lambda p: hydra.java.utils.java_identifier_to_java_expression(hydra.java.utils.variable_to_java_identifier(p))), param_names), (class_id := hydra.java.syntax.Identifier(class_name()), (call := hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation_static(class_id, hydra.java.syntax.Identifier(hydra.java.names.apply_method_name), param_exprs)), (curried := build_curried_lambda(param_names, call), hydra.lib.eithers.bind(encode_type(aliases, hydra.lib.sets.empty(), cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(dom, cod))), cx, g), (lambda jtype: hydra.lib.eithers.bind(hydra.java.utils.java_type_to_java_reference_type(jtype, cx), (lambda rt: Right(hydra.java.utils.java_cast_expression_to_java_expression(hydra.java.utils.java_cast_expression(rt, hydra.java.utils.java_expression_to_java_unary_expression(curried)))))))))[1])[1])[1])[1])[1]))

def encode_nullary_constant_type_args_from_return_type(aliases: hydra.java.environment.Aliases, t: hydra.core.Type, cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, frozenlist[hydra.java.syntax.TypeArgument]]:
    match hydra.strip.deannotate_type(t):
        case hydra.core.TypeSet(value=st):
            return hydra.lib.eithers.bind(encode_type(aliases, hydra.lib.sets.empty(), st, cx, g), (lambda jst: hydra.lib.eithers.bind(hydra.java.utils.java_type_to_java_reference_type(jst, cx), (lambda rt: Right((cast(hydra.java.syntax.TypeArgument, hydra.java.syntax.TypeArgumentReference(rt)),))))))

        case hydra.core.TypeList(value=lt_):
            return hydra.lib.eithers.bind(encode_type(aliases, hydra.lib.sets.empty(), lt_, cx, g), (lambda jlt: hydra.lib.eithers.bind(hydra.java.utils.java_type_to_java_reference_type(jlt, cx), (lambda rt: Right((cast(hydra.java.syntax.TypeArgument, hydra.java.syntax.TypeArgumentReference(rt)),))))))

        case hydra.core.TypeMaybe(value=mt):
            return hydra.lib.eithers.bind(encode_type(aliases, hydra.lib.sets.empty(), mt, cx, g), (lambda jmt: hydra.lib.eithers.bind(hydra.java.utils.java_type_to_java_reference_type(jmt, cx), (lambda rt: Right((cast(hydra.java.syntax.TypeArgument, hydra.java.syntax.TypeArgumentReference(rt)),))))))

        case hydra.core.TypeMap(value=mp):
            return hydra.lib.eithers.bind(encode_type(aliases, hydra.lib.sets.empty(), mp.keys, cx, g), (lambda jkt: hydra.lib.eithers.bind(hydra.java.utils.java_type_to_java_reference_type(jkt, cx), (lambda rk: hydra.lib.eithers.bind(encode_type(aliases, hydra.lib.sets.empty(), mp.values, cx, g), (lambda jvt: hydra.lib.eithers.bind(hydra.java.utils.java_type_to_java_reference_type(jvt, cx), (lambda rv: Right((cast(hydra.java.syntax.TypeArgument, hydra.java.syntax.TypeArgumentReference(rk)), cast(hydra.java.syntax.TypeArgument, hydra.java.syntax.TypeArgumentReference(rv))))))))))))

        case _:
            return Right(())

def encode_nullary_primitive_by_name(env: hydra.java.environment.JavaEnvironment, typ: hydra.core.Type, name: hydra.core.Name, cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.Expression]:
    aliases = env.aliases
    return hydra.lib.eithers.bind(encode_nullary_constant_type_args_from_return_type(aliases, typ, cx, g), (lambda targs: hydra.lib.logic.if_else(hydra.lib.lists.null(targs), (lambda : (header := cast(hydra.java.syntax.MethodInvocation_Header, hydra.java.syntax.MethodInvocation_HeaderSimple(hydra.java.syntax.MethodName(element_java_identifier(True, False, aliases, name)))), Right(hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.syntax.MethodInvocation(header, ()))))[1]), (lambda : (full_name := element_java_identifier(True, False, aliases, name).value, (parts := hydra.lib.strings.split_on(".", full_name), (class_name := hydra.java.syntax.Identifier(hydra.lib.strings.intercalate(".", hydra.lib.lists.init(parts))), (method_name := hydra.java.syntax.Identifier(hydra.lib.lists.last(parts)), Right(hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation_static_with_type_args(class_name, method_name, targs, ()))))[1])[1])[1])[1]))))

def is_field_unit_type(type_name: hydra.core.Name, field_name: hydra.core.Name, cx: T0, g: hydra.graph.Graph):
    schema_types = g.schema_types
    def _hoist_schema_types_body_1(v1):
        match v1:
            case hydra.core.TypeUnion(value=rt):
                return Right(hydra.lib.maybes.cases(hydra.lib.lists.find((lambda ft: hydra.lib.equality.equal(ft.name, field_name)), rt), (lambda : False), (lambda ft: hydra.predicates.is_unit_type(hydra.strip.deannotate_type(ft.type)))))

            case _:
                return Right(False)
    return hydra.lib.maybes.cases(hydra.lib.maps.lookup(type_name, schema_types), (lambda : Right(False)), (lambda ts: _hoist_schema_types_body_1(hydra.strip.deannotate_type(ts.type))))

def strip_foralls(t: hydra.core.Type) -> hydra.core.Type:
    while True:
        match hydra.strip.deannotate_type(t):
            case hydra.core.TypeForall(value=fa):
                t = fa.body
                continue

            case _:
                return t

def take_type_args(label: str, n: int, tyapps: frozenlist[hydra.java.syntax.Type], cx: T0, g: T1) -> Either[hydra.errors.Error, frozenlist[hydra.java.syntax.TypeArgument]]:
    return hydra.lib.logic.if_else(hydra.lib.equality.lt(hydra.lib.lists.length(tyapps), n), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat(("needed type arguments for ", label, ", found too few"))))))), (lambda : hydra.lib.eithers.map_list((lambda jt: hydra.lib.eithers.bind(hydra.java.utils.java_type_to_java_reference_type(jt, cx), (lambda rt: Right(cast(hydra.java.syntax.TypeArgument, hydra.java.syntax.TypeArgumentReference(rt)))))), hydra.lib.lists.take(n, tyapps))))

def with_type_lambda(v1: hydra.java.environment.JavaEnvironment, v2: hydra.core.TypeLambda, v3: Callable[[hydra.java.environment.JavaEnvironment], T0]) -> T0:
    return hydra.environment.with_type_lambda_context((lambda x1: java_env_get_graph(x1)), (lambda x1, x2: java_env_set_graph(x1, x2)), v1, v2, v3)

def bindings_to_statements(env: hydra.java.environment.JavaEnvironment, bindings: frozenlist[hydra.core.Binding], cx: hydra.context.Context, g0: hydra.graph.Graph) -> Either[hydra.errors.Error, tuple[frozenlist[hydra.java.syntax.BlockStatement], hydra.java.environment.JavaEnvironment]]:
    aliases = env.aliases
    g = env.graph
    @lru_cache(1)
    def flat_bindings() -> frozenlist[hydra.core.Binding]:
        return dedup_bindings(aliases.in_scope_java_vars, flatten_bindings(bindings))
    @lru_cache(1)
    def g_extended() -> hydra.graph.Graph:
        return hydra.scoping.extend_graph_for_let((lambda g2, b: hydra.lib.logic.if_else(hydra.predicates.is_complex_binding(g2, b), (lambda : Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(True)))))), (lambda : Nothing()))), g, hydra.core.Let(flat_bindings(), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("dummy")))))
    @lru_cache(1)
    def binding_vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(hydra.lib.lists.map((lambda b: b.name), flat_bindings()))
    @lru_cache(1)
    def all_deps() -> FrozenDict[hydra.core.Name, frozenset[hydra.core.Name]]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda b: (key := b.name, deps := hydra.lib.sets.intersection(binding_vars(), hydra.variables.free_variables_in_term(b.term)), (key, deps))[2]), flat_bindings()))
    @lru_cache(1)
    def sorted() -> frozenlist[frozenlist[hydra.core.Name]]:
        return hydra.sorting.topological_sort_components(hydra.lib.lists.map((lambda entry: (key := hydra.lib.pairs.first(entry), deps := hydra.lib.pairs.second(entry), (key, hydra.lib.sets.to_list(deps)))[2]), hydra.lib.maps.to_list(all_deps())))
    @lru_cache(1)
    def recursive_vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(hydra.lib.lists.concat(hydra.lib.lists.map((lambda names: hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(names), 1), (lambda : (single_name := hydra.lib.lists.head(names), hydra.lib.maybes.cases(hydra.lib.maps.lookup(single_name, all_deps()), (lambda : ()), (lambda deps: hydra.lib.logic.if_else(hydra.lib.sets.member(single_name, deps), (lambda : (single_name,)), (lambda : ())))))[1]), (lambda : names))), sorted())))
    @lru_cache(1)
    def thunked_vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(hydra.lib.lists.concat(hydra.lib.lists.map((lambda b: (bname := b.name, hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.logic.not_(hydra.lib.sets.member(bname, recursive_vars())), hydra.lib.logic.and_(needs_thunking(b.term), hydra.lib.logic.not_(binding_is_function_type(b)))), (lambda : (bname,)), (lambda : ())))[1]), flat_bindings())))
    @lru_cache(1)
    def aliases_extended() -> hydra.java.environment.Aliases:
        return hydra.java.environment.Aliases(aliases.current_namespace, aliases.packages, aliases.branch_vars, hydra.lib.sets.union(aliases.recursive_vars, recursive_vars()), aliases.in_scope_type_params, aliases.polymorphic_locals, hydra.lib.sets.union(aliases.in_scope_java_vars, binding_vars()), aliases.var_renames, aliases.lambda_vars, aliases.type_var_subst, aliases.trusted_type_vars, aliases.method_codomain, hydra.lib.sets.union(aliases.thunked_vars, thunked_vars()))
    env_extended = hydra.java.environment.JavaEnvironment(aliases_extended(), g_extended())
    return hydra.lib.logic.if_else(hydra.lib.lists.null(bindings), (lambda : Right(((), env_extended))), (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda names: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda n: to_decl_init(aliases_extended(), g_extended(), recursive_vars(), flat_bindings(), n, cx, g)), names), (lambda inits: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda n: to_decl_statement(env_extended, aliases_extended(), g_extended(), recursive_vars(), thunked_vars(), flat_bindings(), n, cx, g)), names), (lambda decls: Right(hydra.lib.lists.concat2(hydra.lib.maybes.cat(inits), decls))))))), sorted()), (lambda groups: Right((hydra.lib.lists.concat(groups), env_extended))))))

def encode_application(env: hydra.java.environment.JavaEnvironment, app: hydra.core.Application, cx: hydra.context.Context, g0: hydra.graph.Graph):
    aliases = env.aliases
    g = env.graph
    @lru_cache(1)
    def gathered() -> tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Term], frozenlist[hydra.core.Type]]]:
        return hydra.analysis.gather_args_with_type_apps(cast(hydra.core.Term, hydra.core.TermApplication(app)), (), ())
    @lru_cache(1)
    def fun() -> hydra.core.Term:
        return hydra.lib.pairs.first(gathered())
    @lru_cache(1)
    def args() -> frozenlist[hydra.core.Term]:
        return hydra.lib.pairs.first(hydra.lib.pairs.second(gathered()))
    @lru_cache(1)
    def type_apps() -> frozenlist[hydra.core.Type]:
        return hydra.lib.pairs.second(hydra.lib.pairs.second(gathered()))
    return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _de: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_de.value)))), (lambda _a: _a), hydra.annotations.get_type(g, hydra.annotations.term_annotation_internal(fun()))), (lambda mfun_typ: hydra.lib.eithers.bind(hydra.lib.maybes.cases(mfun_typ, (lambda : hydra.checking.type_of_term(cx, g, fun())), (lambda t: Right(t))), (lambda fun_typ: (arity := hydra.arity.type_arity(fun_typ), deannotated_fun := hydra.strip.deannotate_term(fun()), callee_name := (_hoist_callee_name_1 := (lambda v1: (lambda n: Just(n))(v1.value) if isinstance(v1, hydra.core.TermVariable) else Nothing()), _hoist_callee_name_1(deannotated_fun))[1], _hoist_callee_name_body_1 := (lambda annotated_args, v1: (lambda name: hydra.lib.logic.if_else(hydra.lib.maybes.is_just(hydra.lib.maps.lookup(name, g.primitives)), (lambda : (hargs := hydra.lib.lists.take(arity, annotated_args), (rargs := hydra.lib.lists.drop(arity, annotated_args), hydra.lib.eithers.bind(function_call(env, True, name, hargs, (), cx, g), (lambda initial_call: hydra.lib.eithers.foldl((lambda acc, h: hydra.lib.eithers.bind(encode_term(env, h, cx, g), (lambda jarg: Right(apply_java_arg(acc, jarg))))), initial_call, rargs))))[1])[1]), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.and_(is_recursive_variable(aliases, name), hydra.lib.logic.not_(is_lambda_bound_in(name, aliases.lambda_vars))), (lambda : encode_application_fallback(env, aliases, g, type_apps(), app.function, app.argument, cx, g)), (lambda : hydra.lib.eithers.bind(classify_data_reference(name, cx, g), (lambda sym_class: (method_arity := (_hoist_method_arity_1 := (lambda v12: (lambda n: n)(v12.value) if isinstance(v12, hydra.java.environment.JavaSymbolClassHoistedLambda) else arity), _hoist_method_arity_1(sym_class))[1], hargs := hydra.lib.lists.take(method_arity, annotated_args), rargs := hydra.lib.lists.drop(method_arity, annotated_args), trusted := aliases.trusted_type_vars, in_scope := aliases.in_scope_type_params, filtered_type_apps := hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.lib.sets.null(trusted), hydra.lib.sets.null(in_scope)), (lambda : ()), (lambda : (all_vars := hydra.lib.sets.unions(hydra.lib.lists.map((lambda t: collect_type_vars(t)), type_apps())), hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.sets.null(hydra.lib.sets.difference(all_vars, in_scope))), (lambda : ()), (lambda : hydra.lib.logic.if_else(hydra.lib.sets.null(hydra.lib.sets.difference(all_vars, trusted)), (lambda : type_apps()), (lambda : ())))))[1])), hydra.lib.eithers.bind(hydra.lib.logic.if_else(hydra.lib.lists.null(filtered_type_apps), (lambda : Right(())), (lambda : correct_type_apps(g, name, hargs, filtered_type_apps, cx, g))), (lambda safe_type_apps: hydra.lib.eithers.bind(filter_phantom_type_args(name, safe_type_apps, cx, g), (lambda final_type_apps: hydra.lib.eithers.bind(function_call(env, False, name, hargs, final_type_apps, cx, g), (lambda initial_call: hydra.lib.eithers.foldl((lambda acc, h: hydra.lib.eithers.bind(encode_term(env, h, cx, g), (lambda jarg: Right(apply_java_arg(acc, jarg))))), initial_call, rargs))))))))[6])))))))(v1.value) if isinstance(v1, hydra.core.TermVariable) else encode_application_fallback(env, aliases, g, type_apps(), app.function, app.argument, cx, g)), hydra.lib.eithers.bind(hydra.lib.maybes.cases(callee_name, (lambda : Right(args())), (lambda cname: annotate_lambda_args(cname, type_apps(), args(), cx, g))), (lambda annotated_args: _hoist_callee_name_body_1(annotated_args, deannotated_fun))))[4]))))

def encode_application_fallback(env: hydra.java.environment.JavaEnvironment, aliases: hydra.java.environment.Aliases, gr: hydra.graph.Graph, type_apps: frozenlist[hydra.core.Type], lhs: hydra.core.Term, rhs: hydra.core.Term, cx: hydra.context.Context, g: hydra.graph.Graph):
    def _hoist_hydra_java_coder_encode_application_fallback_1(cx, env, g, lhs, rhs, v1):
        match v1:
            case hydra.core.TypeFunction(value=ft):
                dom = ft.domain
                cod = ft.codomain
                @lru_cache(1)
                def default_expr() -> Either[hydra.errors.Error, hydra.java.syntax.Expression]:
                    return hydra.lib.eithers.bind(encode_term(env, lhs, cx, g), (lambda jfun: hydra.lib.eithers.bind(encode_term(env, rhs, cx, g), (lambda jarg: Right(apply_java_arg(jfun, jarg))))))
                @lru_cache(1)
                def elim_branch() -> Either[hydra.errors.Error, hydra.java.syntax.Expression]:
                    return hydra.lib.eithers.bind(encode_term(env, rhs, cx, g), (lambda jarg: hydra.lib.eithers.bind(hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(java_type_arguments_for_type(dom))), (lambda : Right(dom)), (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _de: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_de.value)))), (lambda _a: _a), hydra.annotations.get_type(g, hydra.annotations.term_annotation_internal(rhs))), (lambda mrt: hydra.lib.maybes.cases(mrt, (lambda : hydra.lib.eithers.bind(hydra.checking.type_of_term(cx, g, rhs), (lambda rt: Right(hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(java_type_arguments_for_type(rt))), (lambda : rt), (lambda : dom)))))), (lambda rt: Right(hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(java_type_arguments_for_type(rt))), (lambda : rt), (lambda : dom))))))))), (lambda enriched_dom: encode_elimination(env, Just(jarg), enriched_dom, cod, hydra.strip.deannotate_term(lhs), cx, g)))))
                def _hoist_elim_branch_body_1(v12):
                    match v12:
                        case hydra.core.TermProject():
                            return elim_branch()

                        case hydra.core.TermCases():
                            return elim_branch()

                        case hydra.core.TermUnwrap():
                            return elim_branch()

                        case _:
                            return default_expr()
                return _hoist_elim_branch_body_1(hydra.strip.deannotate_term(lhs))

            case _:
                return hydra.lib.eithers.bind(encode_term(env, lhs, cx, g), (lambda jfun: hydra.lib.eithers.bind(encode_term(env, rhs, cx, g), (lambda jarg: Right(apply_java_arg(jfun, jarg))))))
    return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _de: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_de.value)))), (lambda _a: _a), hydra.annotations.get_type(g, hydra.annotations.term_annotation_internal(lhs))), (lambda mt: hydra.lib.eithers.bind(hydra.lib.maybes.cases(mt, (lambda : hydra.checking.type_of_term(cx, g, lhs)), (lambda typ: Right(typ))), (lambda t: _hoist_hydra_java_coder_encode_application_fallback_1(cx, env, g, lhs, rhs, hydra.strip.deannotate_type_parameters(hydra.strip.deannotate_type(t)))))))

def encode_elimination(env: hydra.java.environment.JavaEnvironment, marg: Maybe[hydra.java.syntax.Expression], dom: hydra.core.Type, cod: hydra.core.Type, elim_term: hydra.core.Term, cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.Expression]:
    aliases = env.aliases
    match hydra.strip.deannotate_term(elim_term):
        case hydra.core.TermProject(value=proj):
            fname = proj.field
            return hydra.lib.eithers.bind(encode_type(aliases, hydra.lib.sets.empty(), dom, cx, g), (lambda jdom0: hydra.lib.eithers.bind(hydra.java.utils.java_type_to_java_reference_type(jdom0, cx), (lambda jdomr: hydra.lib.maybes.cases(marg, (lambda : (proj_var := hydra.core.Name("projected"), (jbody := hydra.java.utils.java_expression_name_to_java_expression(hydra.java.utils.field_expression(hydra.java.utils.variable_to_java_identifier(proj_var), hydra.java.utils.java_identifier(fname.value))), Right(hydra.java.utils.java_lambda(proj_var, jbody)))[1])[1]), (lambda jarg: (qual := cast(hydra.java.syntax.FieldAccess_Qualifier, hydra.java.syntax.FieldAccess_QualifierPrimary(hydra.java.utils.java_expression_to_java_primary(jarg))), Right(hydra.java.utils.java_field_access_to_java_expression(hydra.java.syntax.FieldAccess(qual, hydra.java.utils.java_identifier(fname.value)))))[1]))))))

        case hydra.core.TermCases(value=cs):
            tname = cs.type_name
            def_ = cs.default
            fields = cs.cases
            return hydra.lib.maybes.cases(marg, (lambda : (u_var := hydra.core.Name("u"), (typed_lambda := cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(u_var, Just(dom), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(elim_term, cast(hydra.core.Term, hydra.core.TermVariable(u_var)))))))), encode_term(env, typed_lambda, cx, g))[1])[1]), (lambda jarg: (prim := hydra.java.utils.java_expression_to_java_primary(jarg), cons_id := inner_class_ref(aliases, tname, hydra.java.names.partial_visitor_name), effective_cod := cod, hydra.lib.eithers.bind(encode_type(aliases, hydra.lib.sets.empty(), effective_cod, cx, g), (lambda jcod: hydra.lib.eithers.bind(hydra.java.utils.java_type_to_java_reference_type(jcod, cx), (lambda rt: hydra.lib.eithers.bind(dom_type_args(aliases, dom, cx, g), (lambda dom_args: (targs := type_args_or_diamond(hydra.lib.lists.concat2(dom_args, (cast(hydra.java.syntax.TypeArgument, hydra.java.syntax.TypeArgumentReference(rt)),))), hydra.lib.eithers.bind(hydra.lib.maybes.cases(def_, (lambda : Right(())), (lambda d: hydra.lib.eithers.bind(otherwise_branch(env, aliases, dom, cod, tname, jcod, dom_args, d, cx, g), (lambda b: Right((b,)))))), (lambda otherwise_branches: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda f: visit_branch(env, aliases, dom, tname, jcod, dom_args, f, cx, g)), fields), (lambda visit_branches: (body := hydra.java.syntax.ClassBody(hydra.lib.lists.concat2(otherwise_branches, visit_branches)), visitor := hydra.java.utils.java_constructor_call(hydra.java.utils.java_constructor_name(cons_id, Just(targs)), (), Just(body)), Right(hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation(Just(Right(prim)), hydra.java.syntax.Identifier(hydra.java.names.accept_method_name), (visitor,)))))[2])))))[1])))))))[3]))

        case hydra.core.TermUnwrap():
            def with_arg(ja: hydra.java.syntax.Expression) -> hydra.java.syntax.Expression:
                return hydra.java.utils.java_field_access_to_java_expression(hydra.java.syntax.FieldAccess(cast(hydra.java.syntax.FieldAccess_Qualifier, hydra.java.syntax.FieldAccess_QualifierPrimary(hydra.java.utils.java_expression_to_java_primary(ja))), hydra.java.utils.java_identifier(hydra.java.names.value_field_name)))
            return Right(hydra.lib.maybes.cases(marg, (lambda : (w_var := hydra.core.Name("wrapped"), (w_arg := hydra.java.utils.java_identifier_to_java_expression(hydra.java.utils.variable_to_java_identifier(w_var)), hydra.java.utils.java_lambda(w_var, with_arg(w_arg)))[1])[1]), (lambda jarg: with_arg(jarg))))

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("unexpected ", hydra.lib.strings.cat2("elimination case", hydra.lib.strings.cat2(" in ", "encodeElimination")))))))

def encode_function(env: hydra.java.environment.JavaEnvironment, dom: hydra.core.Type, cod: hydra.core.Type, fun_term: hydra.core.Term, cx: hydra.context.Context, g: hydra.graph.Graph):
    aliases = env.aliases
    def encode_lambda_fallback(env2: hydra.java.environment.JavaEnvironment, lam: hydra.core.Lambda) -> Either[hydra.errors.Error, hydra.java.syntax.Expression]:
        lambda_var = lam.parameter
        body = lam.body
        return hydra.lib.eithers.bind(analyze_java_function(env2, body, cx, g), (lambda fs: (bindings := fs.bindings, inner_body := fs.body, env3 := fs.environment, hydra.lib.eithers.bind(bindings_to_statements(env3, bindings, cx, g), (lambda bind_result: (binding_stmts := hydra.lib.pairs.first(bind_result), env4 := hydra.lib.pairs.second(bind_result), hydra.lib.eithers.bind(encode_term(env4, inner_body, cx, g), (lambda jbody: (lam1 := hydra.lib.logic.if_else(hydra.lib.lists.null(bindings), (lambda : hydra.java.utils.java_lambda(lambda_var, jbody)), (lambda : (return_st := cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(hydra.java.utils.java_return_statement(Just(jbody)))), hydra.java.utils.java_lambda_from_block(lambda_var, hydra.java.syntax.Block(hydra.lib.lists.concat2(binding_stmts, (return_st,)))))[1])), apply_cast_if_safe(aliases, cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(dom, cod))), lam1, cx, g))[1])))[2])))[3]))
    match hydra.strip.deannotate_term(fun_term):
        case hydra.core.TermProject():
            return encode_elimination(env, Nothing(), dom, cod, hydra.strip.deannotate_term(fun_term), cx, g)

        case hydra.core.TermCases():
            return encode_elimination(env, Nothing(), dom, cod, hydra.strip.deannotate_term(fun_term), cx, g)

        case hydra.core.TermUnwrap():
            return encode_elimination(env, Nothing(), dom, cod, hydra.strip.deannotate_term(fun_term), cx, g)

        case hydra.core.TermLambda(value=lam):
            return with_lambda(env, lam, (lambda env2: (lambda_var := lam.parameter, body := lam.body, _hoist_body_body_1 := (lambda inner_lam, v1: (lambda ft: (dom2 := ft.domain, cod2 := ft.codomain, hydra.lib.eithers.bind(encode_function(env2, dom2, cod2, cast(hydra.core.Term, hydra.core.TermLambda(inner_lam)), cx, g), (lambda inner_java_lambda: (lam1 := hydra.java.utils.java_lambda(lambda_var, inner_java_lambda), apply_cast_if_safe(aliases, cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(dom, cod))), lam1, cx, g))[1])))[2])(v1.value) if isinstance(v1, hydra.core.TypeFunction) else Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("expected function type for lambda body, but got: ", hydra.show.core.type(cod))))))), _hoist_body_body_2 := (lambda v1: (lambda inner_lam: _hoist_body_body_1(inner_lam, hydra.strip.deannotate_type(cod)))(v1.value) if isinstance(v1, hydra.core.TermLambda) else encode_lambda_fallback(env2, lam)), _hoist_body_body_2(hydra.strip.deannotate_term(body)))[4]))

        case _:
            return Right(encode_literal(cast(hydra.core.Literal, hydra.core.LiteralString(hydra.lib.strings.cat2("Unimplemented function variant: ", hydra.show.core.term(fun_term))))))

def encode_function_form_term(env: hydra.java.environment.JavaEnvironment, anns: frozenlist[FrozenDict[hydra.core.Name, hydra.core.Term]], term: hydra.core.Term, cx: hydra.context.Context, g: hydra.graph.Graph):
    @lru_cache(1)
    def combined_anns() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
        return hydra.lib.lists.foldl((lambda acc, m: hydra.lib.maps.union(acc, m)), hydra.lib.maps.empty(), anns)
    def _hoist_combined_anns_body_1(typ, v1):
        match v1:
            case hydra.core.TypeFunction(value=ft):
                return encode_function(env, ft.domain, ft.codomain, term, cx, g)

            case _:
                return encode_nullary_constant(env, typ, term, cx, g)
    return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _de: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_de.value)))), (lambda _a: _a), hydra.annotations.get_type(g, combined_anns())), (lambda mt: hydra.lib.eithers.bind(hydra.lib.maybes.cases(mt, (lambda : hydra.lib.maybes.cases(try_infer_function_type(term), (lambda : hydra.checking.type_of_term(cx, g, term)), (lambda inferred_type: Right(inferred_type)))), (lambda t: Right(t))), (lambda typ: _hoist_combined_anns_body_1(typ, hydra.strip.deannotate_type(typ))))))

def encode_term(env: hydra.java.environment.JavaEnvironment, term: hydra.core.Term, cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.Expression]:
    return encode_term_internal(env, (), (), term, cx, g)

def encode_term_internal(env: hydra.java.environment.JavaEnvironment, anns: frozenlist[FrozenDict[hydra.core.Name, hydra.core.Term]], tyapps: frozenlist[hydra.java.syntax.Type], term: hydra.core.Term, cx: hydra.context.Context, g0: hydra.graph.Graph):
    aliases = env.aliases
    g = env.graph
    def encode(t: hydra.core.Term) -> Either[hydra.errors.Error, hydra.java.syntax.Expression]:
        return encode_term(env, t, cx, g)
    match term:
        case hydra.core.TermAnnotated(value=at):
            return encode_term_internal(env, hydra.lib.lists.cons(at.annotation, anns), tyapps, at.body, cx, g)

        case hydra.core.TermApplication(value=app):
            return encode_application(env, app, cx, g)

        case hydra.core.TermEither(value=et):
            return hydra.lib.eithers.bind(hydra.lib.logic.if_else(hydra.lib.lists.null(tyapps), (lambda : Right(Nothing())), (lambda : hydra.lib.eithers.bind(take_type_args("either", 2, tyapps, cx, g), (lambda ta: Right(Just(ta)))))), (lambda mtargs: (combined_anns := hydra.lib.lists.foldl((lambda acc, m: hydra.lib.maps.union(acc, m)), hydra.lib.maps.empty(), anns), hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _de: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_de.value)))), (lambda _a: _a), hydra.annotations.get_type(g, combined_anns)), (lambda m_either_type: (branch_types := (_hoist_branch_types_1 := (lambda v1: (lambda et2: Just((et2.left, et2.right)))(v1.value) if isinstance(v1, hydra.core.TypeEither) else Nothing()), hydra.lib.maybes.bind(m_either_type, (lambda etyp: _hoist_branch_types_1(hydra.strip.deannotate_type(etyp)))))[1], encode_with_type := (lambda branch_type, t1: (annotated := hydra.annotations.set_term_annotation(hydra.constants.key_type, Just(hydra.encode.core.type(branch_type)), t1), encode_term_internal(env, anns, (), annotated, cx, g))[1]), either_call := (lambda method_name, expr: hydra.lib.maybes.cases(mtargs, (lambda : hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation_static(hydra.java.syntax.Identifier("hydra.util.Either"), hydra.java.syntax.Identifier(method_name), (expr,)))), (lambda targs: hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation_static_with_type_args(hydra.java.syntax.Identifier("hydra.util.Either"), hydra.java.syntax.Identifier(method_name), targs, (expr,)))))), hydra.lib.eithers.either((lambda term1: hydra.lib.eithers.bind(hydra.lib.maybes.cases(branch_types, (lambda : encode(term1)), (lambda bt: encode_with_type(hydra.lib.pairs.first(bt), term1))), (lambda expr: Right(either_call("left", expr))))), (lambda term1: hydra.lib.eithers.bind(hydra.lib.maybes.cases(branch_types, (lambda : encode(term1)), (lambda bt: encode_with_type(hydra.lib.pairs.second(bt), term1))), (lambda expr: Right(either_call("right", expr))))), et))[3])))[1]))

        case hydra.core.TermLambda():
            return encode_function_form_term(env, anns, term, cx, g)

        case hydra.core.TermProject():
            return encode_function_form_term(env, anns, term, cx, g)

        case hydra.core.TermCases():
            return encode_function_form_term(env, anns, term, cx, g)

        case hydra.core.TermUnwrap():
            return encode_function_form_term(env, anns, term, cx, g)

        case hydra.core.TermLet(value=lt):
            bindings = lt.bindings
            body = lt.body
            return hydra.lib.logic.if_else(hydra.lib.lists.null(bindings), (lambda : encode_term_internal(env, anns, (), body, cx, g)), (lambda : hydra.lib.eithers.bind(bindings_to_statements(env, bindings, cx, g), (lambda bind_result: (binding_stmts := hydra.lib.pairs.first(bind_result), env2 := hydra.lib.pairs.second(bind_result), hydra.lib.eithers.bind(encode_term_internal(env2, anns, (), body, cx, g), (lambda jbody: (return_st := cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(hydra.java.utils.java_return_statement(Just(jbody)))), block := hydra.java.syntax.Block(hydra.lib.lists.concat2(binding_stmts, (return_st,))), nullary_lambda := cast(hydra.java.syntax.Expression, hydra.java.syntax.ExpressionLambda(hydra.java.syntax.LambdaExpression(cast(hydra.java.syntax.LambdaParameters, hydra.java.syntax.LambdaParametersTuple(())), cast(hydra.java.syntax.LambdaBody, hydra.java.syntax.LambdaBodyBlock(block))))), combined_anns := hydra.lib.lists.foldl((lambda acc, m: hydra.lib.maps.union(acc, m)), hydra.lib.maps.empty(), anns), g2 := env2.graph, aliases2 := env2.aliases, hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _de: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_de.value)))), (lambda _a: _a), hydra.annotations.get_type(g, combined_anns)), (lambda mt: hydra.lib.eithers.bind(hydra.lib.maybes.cases(mt, (lambda : hydra.checking.type_of_term(cx, g2, body)), (lambda t: Right(t))), (lambda let_type: hydra.lib.eithers.bind(encode_type(aliases2, hydra.lib.sets.empty(), let_type, cx, g), (lambda j_let_type: hydra.lib.eithers.bind(hydra.java.utils.java_type_to_java_reference_type(j_let_type, cx), (lambda rt: (supplier_rt := cast(hydra.java.syntax.ReferenceType, hydra.java.syntax.ReferenceTypeClassOrInterface(cast(hydra.java.syntax.ClassOrInterfaceType, hydra.java.syntax.ClassOrInterfaceTypeClass(hydra.java.utils.java_class_type((rt,), hydra.java.names.java_util_function_package_name(), "Supplier"))))), cast_expr := hydra.java.utils.java_cast_expression_to_java_expression(hydra.java.utils.java_cast_expression(supplier_rt, hydra.java.utils.java_expression_to_java_unary_expression(nullary_lambda))), Right(hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation(Just(Right(hydra.java.utils.java_expression_to_java_primary(cast_expr))), hydra.java.syntax.Identifier("get"), ()))))[2])))))))))[6])))[2]))))

        case hydra.core.TermList(value=els):
            return hydra.lib.logic.if_else(hydra.lib.lists.null(els), (lambda : hydra.lib.logic.if_else(hydra.lib.lists.null(tyapps), (lambda : Right(hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation_static(hydra.java.syntax.Identifier("java.util.Collections"), hydra.java.syntax.Identifier("emptyList"), ())))), (lambda : hydra.lib.eithers.bind(take_type_args("list", 1, tyapps, cx, g), (lambda targs: Right(hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation_static_with_type_args(hydra.java.syntax.Identifier("java.util.Collections"), hydra.java.syntax.Identifier("emptyList"), targs, ())))))))), (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: encode(x1)), els), (lambda jels: Right(hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation_static(hydra.java.syntax.Identifier("java.util.Arrays"), hydra.java.syntax.Identifier("asList"), jels)))))))

        case hydra.core.TermLiteral(value=l):
            return Right(encode_literal(l))

        case hydra.core.TermMap(value=m):
            return hydra.lib.logic.if_else(hydra.lib.maps.null(m), (lambda : hydra.lib.logic.if_else(hydra.lib.lists.null(tyapps), (lambda : Right(hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation_static(hydra.java.syntax.Identifier("java.util.Collections"), hydra.java.syntax.Identifier("emptyMap"), ())))), (lambda : hydra.lib.eithers.bind(take_type_args("map", 2, tyapps, cx, g), (lambda targs: Right(hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation_static_with_type_args(hydra.java.syntax.Identifier("java.util.Collections"), hydra.java.syntax.Identifier("emptyMap"), targs, ())))))))), (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: encode(x1)), hydra.lib.maps.keys(m)), (lambda jkeys: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: encode(x1)), hydra.lib.maps.elems(m)), (lambda jvals: (pair_exprs := hydra.lib.lists.map((lambda kv: hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation_static(hydra.java.syntax.Identifier("java.util.Map"), hydra.java.syntax.Identifier("entry"), (hydra.lib.pairs.first(kv), hydra.lib.pairs.second(kv))))), hydra.lib.lists.zip(jkeys, jvals)), inner_map := hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation_static(hydra.java.syntax.Identifier("java.util.Map"), hydra.java.syntax.Identifier("ofEntries"), pair_exprs)), Right(hydra.java.utils.java_constructor_call(hydra.java.utils.java_constructor_name(hydra.java.syntax.Identifier("java.util.TreeMap"), Nothing()), (inner_map,), Nothing())))[2]))))))

        case hydra.core.TermMaybe(value=mt):
            return hydra.lib.maybes.cases(mt, (lambda : hydra.lib.logic.if_else(hydra.lib.lists.null(tyapps), (lambda : Right(hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation_static(hydra.java.syntax.Identifier("hydra.util.Maybe"), hydra.java.syntax.Identifier("nothing"), ())))), (lambda : hydra.lib.eithers.bind(take_type_args("maybe", 1, tyapps, cx, g), (lambda targs: Right(hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation_static_with_type_args(hydra.java.syntax.Identifier("hydra.util.Maybe"), hydra.java.syntax.Identifier("nothing"), targs, ())))))))), (lambda term1: hydra.lib.eithers.bind(encode(term1), (lambda expr: Right(hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation_static(hydra.java.syntax.Identifier("hydra.util.Maybe"), hydra.java.syntax.Identifier("just"), (expr,))))))))

        case hydra.core.TermPair(value=p):
            return hydra.lib.eithers.bind(encode(hydra.lib.pairs.first(p)), (lambda jterm1: hydra.lib.eithers.bind(encode(hydra.lib.pairs.second(p)), (lambda jterm2: hydra.lib.eithers.bind(hydra.lib.logic.if_else(hydra.lib.lists.null(tyapps), (lambda : Right(Nothing())), (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda jt: hydra.java.utils.java_type_to_java_reference_type(jt, cx)), tyapps), (lambda rts: Right(Just(cast(hydra.java.syntax.TypeArgumentsOrDiamond, hydra.java.syntax.TypeArgumentsOrDiamondArguments(hydra.lib.lists.map((lambda rt: cast(hydra.java.syntax.TypeArgument, hydra.java.syntax.TypeArgumentReference(rt))), rts))))))))), (lambda mtargs: Right(hydra.java.utils.java_constructor_call(hydra.java.utils.java_constructor_name(hydra.java.syntax.Identifier("hydra.util.Pair"), mtargs), (jterm1, jterm2), Nothing()))))))))

        case hydra.core.TermRecord(value=rec):
            rec_name = rec.type_name
            @lru_cache(1)
            def m_record_type() -> Maybe[hydra.core.Type]:
                return hydra.lib.eithers.either((lambda _: Nothing()), (lambda t: Just(t)), hydra.resolution.require_type(cx, g, rec_name))
            @lru_cache(1)
            def stripped_rec_typ() -> Maybe[hydra.core.Type]:
                return hydra.lib.maybes.map((lambda rec_typ: strip_foralls(hydra.strip.deannotate_type(rec_typ))), m_record_type())
            @lru_cache(1)
            def m_field_type_map():
                def _hoist_m_field_type_map_1(v1):
                    match v1:
                        case hydra.core.TypeRecord(value=rt):
                            return Just(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda ft: (ft.name, ft.type)), rt)))

                        case _:
                            return Nothing()
                return hydra.lib.maybes.bind(stripped_rec_typ(), (lambda body_typ: _hoist_m_field_type_map_1(body_typ)))
            @lru_cache(1)
            def combined_anns_rec() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                return hydra.lib.lists.foldl((lambda acc, m: hydra.lib.maps.union(acc, m)), hydra.lib.maps.empty(), anns)
            return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _de: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_de.value)))), (lambda _a: _a), hydra.annotations.get_type(g, combined_anns_rec())), (lambda m_annot_type: (m_type_subst := hydra.lib.maybes.bind(m_annot_type, (lambda ann_typ: hydra.lib.maybes.bind(m_record_type(), (lambda rec_typ: (args := extract_type_application_args(hydra.strip.deannotate_type(ann_typ)), params := collect_forall_params(hydra.strip.deannotate_type(rec_typ)), hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.lib.lists.null(args), hydra.lib.logic.not_(hydra.lib.equality.equal(hydra.lib.lists.length(args), hydra.lib.lists.length(params)))), (lambda : Nothing()), (lambda : Just(hydra.lib.maps.from_list(hydra.lib.lists.zip(params, args))))))[2])))), encode_field := (lambda fld: hydra.lib.maybes.cases(m_field_type_map(), (lambda : encode(fld.term)), (lambda ftmap: (mftyp := hydra.lib.maps.lookup(fld.name, ftmap), hydra.lib.maybes.cases(mftyp, (lambda : encode(fld.term)), (lambda ftyp: (resolved_type := hydra.lib.maybes.cases(m_type_subst, (lambda : ftyp), (lambda subst: apply_subst_full(subst, ftyp))), annotated_field_term := hydra.annotations.set_term_annotation(hydra.constants.key_type, Just(hydra.encode.core.type(resolved_type)), fld.term), encode_term_internal(env, anns, (), annotated_field_term, cx, g))[2])))[1]))), hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: encode_field(x1)), rec.fields), (lambda field_exprs: (cons_id := hydra.java.utils.name_to_java_name(aliases, rec_name), hydra.lib.eithers.bind(hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(tyapps)), (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda jt: hydra.java.utils.java_type_to_java_reference_type(jt, cx)), tyapps), (lambda rts: Right(Just(cast(hydra.java.syntax.TypeArgumentsOrDiamond, hydra.java.syntax.TypeArgumentsOrDiamondArguments(hydra.lib.lists.map((lambda rt: cast(hydra.java.syntax.TypeArgument, hydra.java.syntax.TypeArgumentReference(rt))), rts)))))))), (lambda : (combined_anns := hydra.lib.lists.foldl((lambda acc, m: hydra.lib.maps.union(acc, m)), hydra.lib.maps.empty(), anns), hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _de: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_de.value)))), (lambda _a: _a), hydra.annotations.get_type(g, combined_anns)), (lambda mtyp: hydra.lib.maybes.cases(mtyp, (lambda : Right(Nothing())), (lambda ann_typ: (type_args := extract_type_application_args(hydra.strip.deannotate_type(ann_typ)), hydra.lib.logic.if_else(hydra.lib.lists.null(type_args), (lambda : Right(Nothing())), (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda t: hydra.lib.eithers.bind(encode_type(aliases, hydra.lib.sets.empty(), t, cx, g), (lambda jt: hydra.java.utils.java_type_to_java_reference_type(jt, cx)))), type_args), (lambda j_type_args: Right(Just(cast(hydra.java.syntax.TypeArgumentsOrDiamond, hydra.java.syntax.TypeArgumentsOrDiamondArguments(hydra.lib.lists.map((lambda rt: cast(hydra.java.syntax.TypeArgument, hydra.java.syntax.TypeArgumentReference(rt))), j_type_args))))))))))[1])))))[1])), (lambda mtargs: Right(hydra.java.utils.java_constructor_call(hydra.java.utils.java_constructor_name(cons_id, mtargs), field_exprs, Nothing())))))[1])))[2]))

        case hydra.core.TermSet(value=s):
            return hydra.lib.logic.if_else(hydra.lib.sets.null(s), (lambda : hydra.lib.logic.if_else(hydra.lib.lists.null(tyapps), (lambda : Right(hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation_static(hydra.java.syntax.Identifier("java.util.Collections"), hydra.java.syntax.Identifier("emptySet"), ())))), (lambda : hydra.lib.eithers.bind(take_type_args("set", 1, tyapps, cx, g), (lambda targs: Right(hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation_static_with_type_args(hydra.java.syntax.Identifier("java.util.Collections"), hydra.java.syntax.Identifier("emptySet"), targs, ())))))))), (lambda : (slist := hydra.lib.sets.to_list(s), hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: encode(x1)), slist), (lambda jels: (inner_set := hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation_static(hydra.java.syntax.Identifier("java.util.Set"), hydra.java.syntax.Identifier("of"), jels)), Right(hydra.java.utils.java_constructor_call(hydra.java.utils.java_constructor_name(hydra.java.syntax.Identifier("java.util.TreeSet"), Nothing()), (inner_set,), Nothing())))[1])))[1]))

        case hydra.core.TermTypeLambda(value=tl):
            return with_type_lambda(env, tl, (lambda env2: (combined_anns := hydra.lib.lists.foldl((lambda acc, m: hydra.lib.maps.union(acc, m)), hydra.lib.maps.empty(), anns), hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _de: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_de.value)))), (lambda _a: _a), hydra.annotations.get_type(g, combined_anns)), (lambda mtyp: (annotated_body := (_hoist_annotated_body_1 := (lambda v1: (lambda fa: hydra.annotations.set_term_annotation(hydra.constants.key_type, Just(hydra.encode.core.type(fa.body)), tl.body))(v1.value) if isinstance(v1, hydra.core.TypeForall) else tl.body), hydra.lib.maybes.cases(mtyp, (lambda : tl.body), (lambda t: _hoist_annotated_body_1(t))))[1], encode_term(env2, annotated_body, cx, g))[1])))[1]))

        case hydra.core.TermUnion(value=inj):
            inj_type_name = inj.type_name
            inj_field = inj.field
            inj_field_name = inj_field.name
            inj_field_term = inj_field.term
            @lru_cache(1)
            def type_id() -> str:
                return hydra.java.utils.name_to_java_name(aliases, inj_type_name).value
            @lru_cache(1)
            def cons_id() -> hydra.java.syntax.Identifier:
                return hydra.java.syntax.Identifier(hydra.lib.strings.cat((type_id(), ".", hydra.java.utils.sanitize_java_name(hydra.formatting.capitalize(inj_field_name.value)))))
            return hydra.lib.eithers.bind(is_field_unit_type(inj_type_name, inj_field_name, cx, g), (lambda field_is_unit: hydra.lib.eithers.bind(hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.predicates.is_unit_term(hydra.strip.deannotate_term(inj_field_term)), field_is_unit), (lambda : Right(())), (lambda : hydra.lib.eithers.bind(encode(inj_field_term), (lambda ex: Right((ex,)))))), (lambda args: Right(hydra.java.utils.java_constructor_call(hydra.java.utils.java_constructor_name(cons_id(), Nothing()), args, Nothing()))))))

        case hydra.core.TermVariable(value=name):
            return hydra.lib.maybes.cases(hydra.lib.maps.lookup(name, g.primitives), (lambda : encode_variable(env, name, cx, g)), (lambda _prim: (combined_anns := hydra.lib.lists.foldl((lambda acc, m: hydra.lib.maps.union(acc, m)), hydra.lib.maps.empty(), anns), _hoist_combined_anns_body_1 := (lambda typ, v1: (lambda ft: encode_function_primitive_by_name(env, ft.domain, ft.codomain, name, cx, g))(v1.value) if isinstance(v1, hydra.core.TypeFunction) else encode_nullary_primitive_by_name(env, typ, name, cx, g)), hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _de: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_de.value)))), (lambda _a: _a), hydra.annotations.get_type(g, combined_anns)), (lambda mt: hydra.lib.eithers.bind(hydra.lib.maybes.cases(mt, (lambda : hydra.checking.type_of_term(cx, g, term)), (lambda t: Right(t))), (lambda typ: _hoist_combined_anns_body_1(typ, hydra.strip.deannotate_type(typ)))))))[2]))

        case hydra.core.TermUnit():
            return Right(hydra.java.utils.java_literal_to_java_expression(cast(hydra.java.syntax.Literal, hydra.java.syntax.LiteralNull())))

        case hydra.core.TermWrap(value=wt):
            return hydra.lib.eithers.bind(encode(wt.body), (lambda jarg: Right(hydra.java.utils.java_constructor_call(hydra.java.utils.java_constructor_name(hydra.java.utils.name_to_java_name(aliases, wt.type_name), Nothing()), (jarg,), Nothing()))))

        case hydra.core.TermTypeApplication(value=ta):
            atyp = ta.type
            body = ta.body
            return hydra.lib.eithers.bind(encode_type(aliases, hydra.lib.sets.empty(), atyp, cx, g), (lambda jatyp: (combined_anns := hydra.lib.lists.foldl((lambda acc, m: hydra.lib.maps.union(acc, m)), hydra.lib.maps.empty(), anns), hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _de: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_de.value)))), (lambda _a: _a), hydra.annotations.get_type(g, combined_anns)), (lambda mtyp: hydra.lib.eithers.bind(hydra.lib.maybes.cases(mtyp, (lambda : hydra.checking.type_of_term(cx, g, term)), (lambda t: Right(t))), (lambda typ: (collected0 := collect_type_apps0(body, (atyp,)), innermost_body0 := hydra.lib.pairs.first(collected0), all_type_args0 := hydra.lib.pairs.second(collected0), hydra.lib.eithers.bind(correct_cast_type(innermost_body0, all_type_args0, typ, cx, g), (lambda corrected_typ: (collected := collect_type_apps(body, (atyp,)), innermost_body := hydra.lib.pairs.first(collected), all_type_args := hydra.lib.pairs.second(collected), _hoist_all_type_args_body_1 := (lambda v1: (lambda var_name: hydra.lib.eithers.bind(classify_data_reference(var_name, cx, g), (lambda cls: type_app_nullary_or_hoisted(env, aliases, anns, tyapps, jatyp, body, corrected_typ, var_name, cls, all_type_args, cx, g))))(v1.value) if isinstance(v1, hydra.core.TermVariable) else (lambda either_term: hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(all_type_args), 2), (lambda : (either_branch_types := (hydra.lib.lists.head(all_type_args), hydra.lib.lists.head(hydra.lib.lists.tail(all_type_args))), hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda t: hydra.lib.eithers.bind(encode_type(aliases, hydra.lib.sets.empty(), t, cx, g), (lambda jt: hydra.java.utils.java_type_to_java_reference_type(jt, cx)))), all_type_args), (lambda j_type_args: (either_targs := hydra.lib.lists.map((lambda rt: cast(hydra.java.syntax.TypeArgument, hydra.java.syntax.TypeArgumentReference(rt))), j_type_args), encode_either_branch := (lambda branch_type, t1: (annotated := hydra.annotations.set_term_annotation(hydra.constants.key_type, Just(hydra.encode.core.type(branch_type)), t1), encode_term_internal(env, anns, (), annotated, cx, g))[1]), hydra.lib.eithers.either((lambda term1: hydra.lib.eithers.bind(encode_either_branch(hydra.lib.pairs.first(either_branch_types), term1), (lambda expr: Right(hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation_static_with_type_args(hydra.java.syntax.Identifier("hydra.util.Either"), hydra.java.syntax.Identifier("left"), either_targs, (expr,))))))), (lambda term1: hydra.lib.eithers.bind(encode_either_branch(hydra.lib.pairs.second(either_branch_types), term1), (lambda expr: Right(hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation_static_with_type_args(hydra.java.syntax.Identifier("hydra.util.Either"), hydra.java.syntax.Identifier("right"), either_targs, (expr,))))))), either_term))[2])))[1]), (lambda : type_app_fallback_cast(env, aliases, anns, tyapps, jatyp, body, corrected_typ, cx, g))))(v1.value) if isinstance(v1, hydra.core.TermEither) else type_app_fallback_cast(env, aliases, anns, tyapps, jatyp, body, corrected_typ, cx, g)), _hoist_all_type_args_body_1(innermost_body))[4])))[3])))))[1]))

        case _:
            return Right(encode_literal(cast(hydra.core.Literal, hydra.core.LiteralString("Unimplemented term variant"))))

def function_call(env: hydra.java.environment.JavaEnvironment, is_prim: bool, name: hydra.core.Name, args: frozenlist[hydra.core.Term], type_apps: frozenlist[hydra.core.Type], cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.Expression]:
    aliases = env.aliases
    @lru_cache(1)
    def is_lambda_bound() -> bool:
        return is_lambda_bound_in(name, aliases.lambda_vars)
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda arg: encode_term(env, arg, cx, g)), args), (lambda jargs0: (wrap_result := wrap_lazy_arguments(name, jargs0), jargs := hydra.lib.pairs.first(wrap_result), m_method_override := hydra.lib.pairs.second(wrap_result), hydra.lib.logic.if_else(hydra.lib.logic.or_(is_local_variable(name), is_lambda_bound()), (lambda : hydra.lib.eithers.bind(encode_variable(env, name, cx, g), (lambda base_expr: Right(hydra.lib.lists.foldl((lambda acc, jarg: apply_java_arg(acc, jarg)), base_expr, jargs))))), (lambda : (override_method_name := (lambda jid: hydra.lib.maybes.cases(m_method_override, (lambda : jid), (lambda m: (s := jid.value, hydra.java.syntax.Identifier(hydra.lib.strings.cat2(hydra.lib.strings.from_list(hydra.lib.lists.take(hydra.lib.math.sub(hydra.lib.strings.length(s), hydra.lib.strings.length(hydra.java.names.apply_method_name)), hydra.lib.strings.to_list(s))), m)))[1]))), hydra.lib.logic.if_else(hydra.lib.lists.null(type_apps), (lambda : (header := cast(hydra.java.syntax.MethodInvocation_Header, hydra.java.syntax.MethodInvocation_HeaderSimple(hydra.java.syntax.MethodName(override_method_name(element_java_identifier(is_prim, False, aliases, name))))), Right(hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.syntax.MethodInvocation(header, jargs))))[1]), (lambda : (qn := hydra.names.qualify_name(name), (mns := qn.namespace, (local_name := qn.local, hydra.lib.maybes.cases(mns, (lambda : (header := cast(hydra.java.syntax.MethodInvocation_Header, hydra.java.syntax.MethodInvocation_HeaderSimple(hydra.java.syntax.MethodName(override_method_name(element_java_identifier(is_prim, False, aliases, name))))), Right(hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.syntax.MethodInvocation(header, jargs))))[1]), (lambda ns_: (class_id := hydra.java.utils.name_to_java_name(aliases, elements_qualified_name(ns_)), method_id := hydra.lib.logic.if_else(is_prim, (lambda : override_method_name(hydra.java.syntax.Identifier(hydra.lib.strings.cat2(hydra.java.utils.name_to_java_name(aliases, hydra.names.unqualify_name(hydra.packaging.QualifiedName(Just(ns_), hydra.formatting.capitalize(local_name)))).value, hydra.lib.strings.cat2(".", hydra.java.names.apply_method_name))))), (lambda : hydra.java.syntax.Identifier(hydra.java.utils.sanitize_java_name(local_name)))), hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda t: hydra.lib.eithers.bind(encode_type(aliases, hydra.lib.sets.empty(), t, cx, g), (lambda jt: hydra.lib.eithers.bind(hydra.java.utils.java_type_to_java_reference_type(jt, cx), (lambda rt: Right(cast(hydra.java.syntax.TypeArgument, hydra.java.syntax.TypeArgumentReference(rt)))))))), type_apps), (lambda j_type_args: Right(hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation_static_with_type_args(class_id, method_id, j_type_args, jargs))))))[2])))[1])[1])[1])))[1])))[3]))

def otherwise_branch(env: hydra.java.environment.JavaEnvironment, aliases: hydra.java.environment.Aliases, dom: hydra.core.Type, cod: hydra.core.Type, tname: hydra.core.Name, jcod: hydra.java.syntax.Type, targs: frozenlist[hydra.java.syntax.TypeArgument], d: hydra.core.Term, cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.ClassBodyDeclarationWithComments]:
    @lru_cache(1)
    def jdom() -> hydra.java.syntax.Type:
        return cast(hydra.java.syntax.Type, hydra.java.syntax.TypeReference(hydra.java.utils.name_to_java_reference_type(aliases, True, targs, tname, Nothing())))
    mods = (cast(hydra.java.syntax.MethodModifier, hydra.java.syntax.MethodModifierPublic()),)
    anns = (hydra.java.utils.override_annotation(),)
    @lru_cache(1)
    def param() -> hydra.java.syntax.FormalParameter:
        return hydra.java.utils.java_type_to_java_formal_parameter(jdom(), hydra.core.Name("instance"))
    @lru_cache(1)
    def result() -> hydra.java.syntax.Result:
        return cast(hydra.java.syntax.Result, hydra.java.syntax.ResultType(hydra.java.syntax.UnannType(jcod)))
    return hydra.lib.eithers.bind(analyze_java_function(env, d, cx, g), (lambda fs: (bindings := fs.bindings, raw_body := fs.body, inner_body := annotate_body_with_cod(cod, raw_body), env2 := fs.environment, hydra.lib.eithers.bind(bindings_to_statements(env2, bindings, cx, g), (lambda bind_result: (binding_stmts := hydra.lib.pairs.first(bind_result), env3 := hydra.lib.pairs.second(bind_result), hydra.lib.eithers.bind(encode_term(env3, inner_body, cx, g), (lambda jret: (return_stmt := cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(hydra.java.utils.java_return_statement(Just(jret)))), all_stmts := hydra.lib.lists.concat2(binding_stmts, (return_stmt,)), Right(no_comment(hydra.java.utils.method_declaration(mods, (), anns, hydra.java.names.otherwise_method_name, (param(),), result(), Just(all_stmts)))))[2])))[2])))[4]))

def to_decl_statement(env_ext: hydra.java.environment.JavaEnvironment, aliases_ext: hydra.java.environment.Aliases, g_ext: hydra.graph.Graph, recursive_vars: frozenset[hydra.core.Name], thunked_vars: frozenset[hydra.core.Name], flat_bindings: frozenlist[hydra.core.Binding], name: hydra.core.Name, cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.BlockStatement]:
    @lru_cache(1)
    def binding() -> hydra.core.Binding:
        return hydra.lib.lists.head(hydra.lib.lists.filter((lambda b: hydra.lib.equality.equal(b.name, name)), flat_bindings))
    value = binding().term
    return hydra.lib.eithers.bind(hydra.lib.maybes.cases(binding().type, (lambda : hydra.checking.type_of_term(cx, g_ext, value)), (lambda ts: Right(ts.type))), (lambda typ: hydra.lib.eithers.bind(encode_type(aliases_ext, hydra.lib.sets.empty(), typ, cx, g), (lambda jtype: (id := hydra.java.utils.variable_to_java_identifier(name), annotated_value := hydra.annotations.set_term_annotation(hydra.constants.key_type, Just(hydra.encode.core.type(typ)), value), hydra.lib.eithers.bind(encode_term(env_ext, annotated_value, cx, g), (lambda rhs: hydra.lib.logic.if_else(hydra.lib.sets.member(name, recursive_vars), (lambda : Right(cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(hydra.java.utils.java_method_invocation_to_java_statement(hydra.java.utils.method_invocation(Just(Left(hydra.java.syntax.ExpressionName(Nothing(), id))), hydra.java.syntax.Identifier(hydra.java.names.set_method_name), (rhs,))))))), (lambda : hydra.lib.logic.if_else(hydra.lib.sets.member(name, thunked_vars), (lambda : hydra.lib.eithers.bind(hydra.java.utils.java_type_to_java_reference_type(jtype, cx), (lambda rt: (lazy_type := hydra.java.utils.java_ref_type((rt,), hydra.java.names.hydra_util_package_name(), "Lazy"), lambda_body := cast(hydra.java.syntax.LambdaBody, hydra.java.syntax.LambdaBodyExpression(rhs)), supplier_lambda := cast(hydra.java.syntax.Expression, hydra.java.syntax.ExpressionLambda(hydra.java.syntax.LambdaExpression(cast(hydra.java.syntax.LambdaParameters, hydra.java.syntax.LambdaParametersTuple(())), lambda_body))), targs := type_args_or_diamond((cast(hydra.java.syntax.TypeArgument, hydra.java.syntax.TypeArgumentReference(rt)),)), lazy_expr := hydra.java.utils.java_constructor_call(hydra.java.utils.java_constructor_name(hydra.java.syntax.Identifier("hydra.util.Lazy"), Just(targs)), (supplier_lambda,), Nothing()), Right(hydra.java.utils.variable_declaration_statement(aliases_ext, lazy_type, id, lazy_expr)))[5]))), (lambda : Right(hydra.java.utils.variable_declaration_statement(aliases_ext, jtype, id, rhs)))))))))[2]))))

def type_app_fallback_cast(env: hydra.java.environment.JavaEnvironment, aliases: hydra.java.environment.Aliases, anns: frozenlist[FrozenDict[hydra.core.Name, hydra.core.Term]], tyapps: frozenlist[hydra.java.syntax.Type], jatyp: hydra.java.syntax.Type, body: hydra.core.Term, typ: hydra.core.Type, cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.Expression]:
    @lru_cache(1)
    def annotated_body() -> hydra.core.Term:
        return hydra.annotations.set_term_annotation(hydra.constants.key_type, Just(hydra.encode.core.type(typ)), body)
    return hydra.lib.eithers.bind(encode_term_internal(env, anns, hydra.lib.lists.cons(jatyp, tyapps), annotated_body(), cx, g), (lambda jbody: hydra.lib.eithers.bind(encode_type(aliases, hydra.lib.sets.empty(), typ, cx, g), (lambda jtype: hydra.lib.eithers.bind(hydra.java.utils.java_type_to_java_reference_type(jtype, cx), (lambda rt: Right(hydra.java.utils.java_cast_expression_to_java_expression(hydra.java.utils.java_cast_expression(rt, hydra.java.utils.java_expression_to_java_unary_expression(jbody))))))))))

def type_app_nullary_or_hoisted(env: hydra.java.environment.JavaEnvironment, aliases: hydra.java.environment.Aliases, anns: frozenlist[FrozenDict[hydra.core.Name, hydra.core.Term]], tyapps: frozenlist[hydra.java.syntax.Type], jatyp: hydra.java.syntax.Type, body: hydra.core.Term, corrected_typ: hydra.core.Type, var_name: hydra.core.Name, cls: hydra.java.environment.JavaSymbolClass, all_type_args: frozenlist[hydra.core.Type], cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.Expression]:
    @lru_cache(1)
    def qn() -> hydra.packaging.QualifiedName:
        return hydra.names.qualify_name(var_name)
    mns = qn().namespace
    local_name = qn().local
    match cls:
        case hydra.java.environment.JavaSymbolClassNullaryFunction():
            return hydra.lib.maybes.cases(mns, (lambda : type_app_fallback_cast(env, aliases, anns, tyapps, jatyp, body, corrected_typ, cx, g)), (lambda ns_: (class_id := hydra.java.utils.name_to_java_name(aliases, elements_qualified_name(ns_)), method_id := hydra.java.syntax.Identifier(hydra.java.utils.sanitize_java_name(local_name)), hydra.lib.eithers.bind(filter_phantom_type_args(var_name, all_type_args, cx, g), (lambda filtered_type_args: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda t: hydra.lib.eithers.bind(encode_type(aliases, hydra.lib.sets.empty(), t, cx, g), (lambda jt: hydra.lib.eithers.bind(hydra.java.utils.java_type_to_java_reference_type(jt, cx), (lambda rt: Right(cast(hydra.java.syntax.TypeArgument, hydra.java.syntax.TypeArgumentReference(rt)))))))), filtered_type_args), (lambda j_type_args: Right(hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation_static_with_type_args(class_id, method_id, j_type_args, ()))))))))[2]))

        case hydra.java.environment.JavaSymbolClassHoistedLambda(value=arity):
            return hydra.lib.maybes.cases(mns, (lambda : type_app_fallback_cast(env, aliases, anns, tyapps, jatyp, body, corrected_typ, cx, g)), (lambda ns_: (class_id := hydra.java.utils.name_to_java_name(aliases, elements_qualified_name(ns_)), method_id := hydra.java.syntax.Identifier(hydra.java.utils.sanitize_java_name(local_name)), hydra.lib.eithers.bind(filter_phantom_type_args(var_name, all_type_args, cx, g), (lambda filtered_type_args: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda t: hydra.lib.eithers.bind(encode_type(aliases, hydra.lib.sets.empty(), t, cx, g), (lambda jt: hydra.lib.eithers.bind(hydra.java.utils.java_type_to_java_reference_type(jt, cx), (lambda rt: Right(cast(hydra.java.syntax.TypeArgument, hydra.java.syntax.TypeArgumentReference(rt)))))))), filtered_type_args), (lambda j_type_args: (param_names := hydra.lib.lists.map((lambda i: hydra.core.Name(hydra.lib.strings.cat2("p", hydra.lib.literals.show_int32(i)))), hydra.lib.math.range_(0, hydra.lib.math.sub(arity, 1))), param_exprs := hydra.lib.lists.map((lambda p: hydra.java.utils.java_identifier_to_java_expression(hydra.java.utils.variable_to_java_identifier(p))), param_names), call := hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation_static_with_type_args(class_id, method_id, j_type_args, param_exprs)), Right(build_curried_lambda(param_names, call)))[3])))))[2]))

        case _:
            return type_app_fallback_cast(env, aliases, anns, tyapps, jatyp, body, corrected_typ, cx, g)

def visit_branch(env: hydra.java.environment.JavaEnvironment, aliases: hydra.java.environment.Aliases, dom: hydra.core.Type, tname: hydra.core.Name, jcod: hydra.java.syntax.Type, targs: frozenlist[hydra.java.syntax.TypeArgument], field: hydra.core.Field, cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.ClassBodyDeclarationWithComments]:
    @lru_cache(1)
    def jdom() -> hydra.java.syntax.Type:
        return cast(hydra.java.syntax.Type, hydra.java.syntax.TypeReference(hydra.java.utils.name_to_java_reference_type(aliases, True, targs, tname, Just(hydra.formatting.capitalize(field.name.value)))))
    mods = (cast(hydra.java.syntax.MethodModifier, hydra.java.syntax.MethodModifierPublic()),)
    anns = (hydra.java.utils.override_annotation(),)
    @lru_cache(1)
    def result() -> hydra.java.syntax.Result:
        return cast(hydra.java.syntax.Result, hydra.java.syntax.ResultType(hydra.java.syntax.UnannType(jcod)))
    match hydra.strip.deannotate_term(field.term):
        case hydra.core.TermLambda(value=lam):
            return with_lambda(env, lam, (lambda env2: (lambda_param := lam.parameter, body := lam.body, env3 := insert_branch_var(lambda_param, env2), hydra.lib.eithers.bind(analyze_java_function(env3, body, cx, g), (lambda fs: (bindings := fs.bindings, inner_body := fs.body, env4 := fs.environment, hydra.lib.eithers.bind(bindings_to_statements(env4, bindings, cx, g), (lambda bind_result: (binding_stmts := hydra.lib.pairs.first(bind_result), env5 := hydra.lib.pairs.second(bind_result), hydra.lib.eithers.bind(encode_term(env5, inner_body, cx, g), (lambda jret: (param := hydra.java.utils.java_type_to_java_formal_parameter(jdom(), lambda_param), return_stmt := cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(hydra.java.utils.java_return_statement(Just(jret)))), all_stmts := hydra.lib.lists.concat2(binding_stmts, (return_stmt,)), Right(no_comment(hydra.java.utils.method_declaration(mods, (), anns, hydra.java.names.visit_method_name, (param,), result(), Just(all_stmts)))))[3])))[2])))[3])))[3]))

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("visitBranch: field term is not a lambda: ", hydra.show.core.term(field.term))))))

def bound_type_variables(typ: hydra.core.Type) -> frozenlist[hydra.core.Name]:
    match typ:
        case hydra.core.TypeAnnotated(value=at):
            return bound_type_variables(at.body)

        case hydra.core.TypeForall(value=ft):
            return hydra.lib.lists.cons(ft.parameter, bound_type_variables(ft.body))

        case _:
            return ()

def build_type_var_subst_go(svs: frozenset[hydra.core.Name], ft: hydra.core.Type, ct: hydra.core.Type):
    def go_sub(a: hydra.core.Type, b: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        return build_type_var_subst_go(svs, hydra.strip.deannotate_type(a), hydra.strip.deannotate_type(b))
    def _hoist_go_sub_body_1(v1):
        match v1:
            case hydra.core.TypeForall(value=cfa):
                return build_type_var_subst_go(svs, ft, hydra.strip.deannotate_type(cfa.body))

            case _:
                return hydra.lib.maps.empty()
    def _hoist_go_sub_body_2(fn, v1):
        match v1:
            case hydra.core.TypeVariable(value=cn):
                return hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.logic.not_(hydra.lib.equality.equal(fn, cn)), hydra.lib.sets.member(cn, svs)), (lambda : hydra.lib.maps.singleton(fn, cn)), (lambda : hydra.lib.maps.empty()))

            case _:
                return hydra.lib.maps.empty()
    def _hoist_go_sub_body_3(fft, v1):
        match v1:
            case hydra.core.TypeFunction(value=cft):
                return hydra.lib.maps.union(go_sub(fft.domain, cft.domain), go_sub(fft.codomain, cft.codomain))

            case _:
                return hydra.lib.maps.empty()
    def _hoist_go_sub_body_4(fat, v1):
        match v1:
            case hydra.core.TypeApplication(value=cat):
                return hydra.lib.maps.union(go_sub(fat.function, cat.function), go_sub(fat.argument, cat.argument))

            case _:
                return hydra.lib.maps.empty()
    def _hoist_go_sub_body_5(fl, v1):
        match v1:
            case hydra.core.TypeList(value=cl):
                return go_sub(fl, cl)

            case _:
                return hydra.lib.maps.empty()
    def _hoist_go_sub_body_6(fs, v1):
        match v1:
            case hydra.core.TypeSet(value=cs):
                return go_sub(fs, cs)

            case _:
                return hydra.lib.maps.empty()
    def _hoist_go_sub_body_7(fm, v1):
        match v1:
            case hydra.core.TypeMaybe(value=cm):
                return go_sub(fm, cm)

            case _:
                return hydra.lib.maps.empty()
    def _hoist_go_sub_body_8(fmt, v1):
        match v1:
            case hydra.core.TypeMap(value=cmt):
                return hydra.lib.maps.union(go_sub(fmt.keys, cmt.keys), go_sub(fmt.values, cmt.values))

            case _:
                return hydra.lib.maps.empty()
    def _hoist_go_sub_body_9(fpt, v1):
        match v1:
            case hydra.core.TypePair(value=cpt):
                return hydra.lib.maps.union(go_sub(fpt.first, cpt.first), go_sub(fpt.second, cpt.second))

            case _:
                return hydra.lib.maps.empty()
    def _hoist_go_sub_body_10(fet, v1):
        match v1:
            case hydra.core.TypeEither(value=cet):
                return hydra.lib.maps.union(go_sub(fet.left, cet.left), go_sub(fet.right, cet.right))

            case _:
                return hydra.lib.maps.empty()
    def _hoist_go_sub_body_11(ffa, v1):
        match v1:
            case hydra.core.TypeForall(value=cfa):
                return go_sub(ffa.body, cfa.body)

            case _:
                return build_type_var_subst_go(svs, hydra.strip.deannotate_type(ffa.body), ct)
    match ft:
        case hydra.core.TypeVariable(value=fn):
            return _hoist_go_sub_body_2(fn, ct)

        case hydra.core.TypeFunction(value=fft):
            return _hoist_go_sub_body_3(fft, ct)

        case hydra.core.TypeApplication(value=fat):
            return _hoist_go_sub_body_4(fat, ct)

        case hydra.core.TypeList(value=fl):
            return _hoist_go_sub_body_5(fl, ct)

        case hydra.core.TypeSet(value=fs):
            return _hoist_go_sub_body_6(fs, ct)

        case hydra.core.TypeMaybe(value=fm):
            return _hoist_go_sub_body_7(fm, ct)

        case hydra.core.TypeMap(value=fmt):
            return _hoist_go_sub_body_8(fmt, ct)

        case hydra.core.TypePair(value=fpt):
            return _hoist_go_sub_body_9(fpt, ct)

        case hydra.core.TypeEither(value=fet):
            return _hoist_go_sub_body_10(fet, ct)

        case hydra.core.TypeForall(value=ffa):
            return _hoist_go_sub_body_11(ffa, ct)

        case _:
            return _hoist_go_sub_body_1(ct)

def build_type_var_subst(scheme_var_set: frozenset[hydra.core.Name], fresh_typ: hydra.core.Type, canon_typ: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
    return build_type_var_subst_go(scheme_var_set, hydra.strip.deannotate_type(fresh_typ), hydra.strip.deannotate_type(canon_typ))

def build_subst_from_annotations_go(scheme_var_set: frozenset[hydra.core.Name], g: hydra.graph.Graph, term: hydra.core.Term) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
    match term:
        case hydra.core.TermAnnotated(value=at):
            body = at.body
            anns = at.annotation
            @lru_cache(1)
            def body_subst() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
                return build_subst_from_annotations_go(scheme_var_set, g, body)
            @lru_cache(1)
            def ann_subst():
                def _hoist_ann_subst_1(dom, v1):
                    match v1:
                        case hydra.core.TypeFunction(value=ft):
                            return build_type_var_subst(scheme_var_set, ft.domain, dom)

                        case _:
                            return hydra.lib.maps.empty()
                def _hoist_ann_subst_2(ann_type, v1):
                    match v1:
                        case hydra.core.TermLambda(value=lam):
                            return hydra.lib.maybes.cases(lam.domain, (lambda : hydra.lib.maps.empty()), (lambda dom: _hoist_ann_subst_1(dom, hydra.strip.deannotate_type(ann_type))))

                        case _:
                            return hydra.lib.maps.empty()
                return hydra.lib.maybes.cases(hydra.lib.maps.lookup(hydra.constants.key_type, anns), (lambda : hydra.lib.maps.empty()), (lambda type_term: hydra.lib.eithers.either((lambda _: hydra.lib.maps.empty()), (lambda ann_type: _hoist_ann_subst_2(ann_type, hydra.strip.deannotate_term(body))), hydra.decode.core.type(g, type_term))))
            return hydra.lib.maps.union(ann_subst(), body_subst())

        case hydra.core.TermApplication(value=app):
            return hydra.lib.maps.union(build_subst_from_annotations_go(scheme_var_set, g, app.function), build_subst_from_annotations_go(scheme_var_set, g, app.argument))

        case hydra.core.TermLambda(value=lam):
            return build_subst_from_annotations_go(scheme_var_set, g, lam.body)

        case hydra.core.TermCases(value=cs):
            @lru_cache(1)
            def def_subst() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
                return hydra.lib.maybes.cases(cs.default, (lambda : hydra.lib.maps.empty()), (lambda d: build_subst_from_annotations_go(scheme_var_set, g, d)))
            @lru_cache(1)
            def case_substs() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
                return hydra.lib.lists.foldl((lambda acc, fld: hydra.lib.maps.union(acc, build_subst_from_annotations_go(scheme_var_set, g, fld.term))), hydra.lib.maps.empty(), cs.cases)
            return hydra.lib.maps.union(def_subst(), case_substs())

        case hydra.core.TermLet(value=lt):
            @lru_cache(1)
            def binding_subst() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
                return hydra.lib.lists.foldl((lambda acc, b: hydra.lib.maps.union(acc, build_subst_from_annotations_go(scheme_var_set, g, b.term))), hydra.lib.maps.empty(), lt.bindings)
            return hydra.lib.maps.union(binding_subst(), build_subst_from_annotations_go(scheme_var_set, g, lt.body))

        case hydra.core.TermList(value=terms):
            return hydra.lib.lists.foldl((lambda acc, t: hydra.lib.maps.union(acc, build_subst_from_annotations_go(scheme_var_set, g, t))), hydra.lib.maps.empty(), terms)

        case hydra.core.TermMaybe(value=mt):
            return hydra.lib.maybes.cases(mt, (lambda : hydra.lib.maps.empty()), (lambda t: build_subst_from_annotations_go(scheme_var_set, g, t)))

        case hydra.core.TermPair(value=p):
            return hydra.lib.maps.union(build_subst_from_annotations_go(scheme_var_set, g, hydra.lib.pairs.first(p)), build_subst_from_annotations_go(scheme_var_set, g, hydra.lib.pairs.second(p)))

        case hydra.core.TermRecord(value=r):
            return hydra.lib.lists.foldl((lambda acc, fld: hydra.lib.maps.union(acc, build_subst_from_annotations_go(scheme_var_set, g, fld.term))), hydra.lib.maps.empty(), r.fields)

        case hydra.core.TermSet(value=terms2):
            return hydra.lib.lists.foldl((lambda acc, t: hydra.lib.maps.union(acc, build_subst_from_annotations_go(scheme_var_set, g, t))), hydra.lib.maps.empty(), hydra.lib.sets.to_list(terms2))

        case hydra.core.TermTypeApplication(value=ta):
            return build_subst_from_annotations_go(scheme_var_set, g, ta.body)

        case hydra.core.TermTypeLambda(value=tl):
            return build_subst_from_annotations_go(scheme_var_set, g, tl.body)

        case hydra.core.TermEither(value=e):
            return hydra.lib.eithers.either((lambda t: build_subst_from_annotations_go(scheme_var_set, g, t)), (lambda t: build_subst_from_annotations_go(scheme_var_set, g, t)), e)

        case _:
            return hydra.lib.maps.empty()

def build_subst_from_annotations(scheme_var_set: frozenset[hydra.core.Name], term: hydra.core.Term, cx: T0, g: hydra.graph.Graph) -> Either[T1, FrozenDict[hydra.core.Name, hydra.core.Name]]:
    return Right(build_subst_from_annotations_go(scheme_var_set, g, term))

def build_type_subst_go(svs: frozenset[hydra.core.Name], st: hydra.core.Type, at: hydra.core.Type):
    def go_sub(a: hydra.core.Type, b: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Type]:
        return build_type_subst_go(svs, hydra.strip.deannotate_type(a), hydra.strip.deannotate_type(b))
    def _hoist_go_sub_body_1(sft, v1):
        match v1:
            case hydra.core.TypeFunction(value=aft):
                return hydra.lib.maps.union(go_sub(sft.domain, aft.domain), go_sub(sft.codomain, aft.codomain))

            case _:
                return hydra.lib.maps.empty()
    def _hoist_go_sub_body_2(sat, v1):
        match v1:
            case hydra.core.TypeApplication(value=aat):
                return hydra.lib.maps.union(go_sub(sat.function, aat.function), go_sub(sat.argument, aat.argument))

            case _:
                return hydra.lib.maps.empty()
    def _hoist_go_sub_body_3(sl, v1):
        match v1:
            case hydra.core.TypeList(value=al):
                return go_sub(sl, al)

            case _:
                return hydra.lib.maps.empty()
    def _hoist_go_sub_body_4(ss, v1):
        match v1:
            case hydra.core.TypeSet(value=as_):
                return go_sub(ss, as_)

            case _:
                return hydra.lib.maps.empty()
    def _hoist_go_sub_body_5(sm, v1):
        match v1:
            case hydra.core.TypeMaybe(value=am):
                return go_sub(sm, am)

            case _:
                return hydra.lib.maps.empty()
    def _hoist_go_sub_body_6(smt, v1):
        match v1:
            case hydra.core.TypeMap(value=amt):
                return hydra.lib.maps.union(go_sub(smt.keys, amt.keys), go_sub(smt.values, amt.values))

            case _:
                return hydra.lib.maps.empty()
    def _hoist_go_sub_body_7(spt, v1):
        match v1:
            case hydra.core.TypePair(value=apt):
                return hydra.lib.maps.union(go_sub(spt.first, apt.first), go_sub(spt.second, apt.second))

            case _:
                return hydra.lib.maps.empty()
    def _hoist_go_sub_body_8(set_, v1):
        match v1:
            case hydra.core.TypeEither(value=aet):
                return hydra.lib.maps.union(go_sub(set_.left, aet.left), go_sub(set_.right, aet.right))

            case _:
                return hydra.lib.maps.empty()
    def _hoist_go_sub_body_9(sfa, v1):
        match v1:
            case hydra.core.TypeForall(value=afa):
                return go_sub(sfa.body, afa.body)

            case _:
                return go_sub(sfa.body, at)
    match st:
        case hydra.core.TypeVariable(value=v):
            return hydra.lib.logic.if_else(hydra.lib.sets.member(v, svs), (lambda : hydra.lib.maps.singleton(v, at)), (lambda : hydra.lib.maps.empty()))

        case hydra.core.TypeFunction(value=sft):
            return _hoist_go_sub_body_1(sft, at)

        case hydra.core.TypeApplication(value=sat):
            return _hoist_go_sub_body_2(sat, at)

        case hydra.core.TypeList(value=sl):
            return _hoist_go_sub_body_3(sl, at)

        case hydra.core.TypeSet(value=ss):
            return _hoist_go_sub_body_4(ss, at)

        case hydra.core.TypeMaybe(value=sm):
            return _hoist_go_sub_body_5(sm, at)

        case hydra.core.TypeMap(value=smt):
            return _hoist_go_sub_body_6(smt, at)

        case hydra.core.TypePair(value=spt):
            return _hoist_go_sub_body_7(spt, at)

        case hydra.core.TypeEither(value=set_):
            return _hoist_go_sub_body_8(set_, at)

        case hydra.core.TypeForall(value=sfa):
            return _hoist_go_sub_body_9(sfa, at)

        case _:
            return hydra.lib.maps.empty()

def build_type_subst(scheme_var_set: frozenset[hydra.core.Name], scheme_type: hydra.core.Type, actual_type: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Type]:
    return build_type_subst_go(scheme_var_set, hydra.strip.deannotate_type(scheme_type), hydra.strip.deannotate_type(actual_type))

class_mods_public = (cast(hydra.java.syntax.ClassModifier, hydra.java.syntax.ClassModifierPublic()),)

def cmp_decl_statement(aliases: T0) -> hydra.java.syntax.BlockStatement:
    return hydra.java.utils.variable_declaration_statement(aliases, hydra.java.utils.java_int_type(), hydra.java.utils.java_identifier("cmp"), hydra.java.utils.java_int_expression(0))

@lru_cache(1)
def cmp_not_zero_expr() -> hydra.java.syntax.Expression:
    @lru_cache(1)
    def lhs() -> hydra.java.syntax.EqualityExpression:
        return hydra.java.utils.java_relational_expression_to_java_equality_expression(hydra.java.utils.java_postfix_expression_to_java_relational_expression(cast(hydra.java.syntax.PostfixExpression, hydra.java.syntax.PostfixExpressionName(hydra.java.syntax.ExpressionName(Nothing(), hydra.java.utils.java_identifier("cmp"))))))
    @lru_cache(1)
    def rhs() -> hydra.java.syntax.RelationalExpression:
        return hydra.java.utils.java_postfix_expression_to_java_relational_expression(cast(hydra.java.syntax.PostfixExpression, hydra.java.syntax.PostfixExpressionPrimary(hydra.java.utils.java_literal_to_java_primary(hydra.java.utils.java_int(0)))))
    return hydra.java.utils.java_equality_expression_to_java_expression(cast(hydra.java.syntax.EqualityExpression, hydra.java.syntax.EqualityExpressionNotEqual(hydra.java.syntax.EqualityExpression_Binary(lhs(), rhs()))))

def collect_lambda_domains(t: hydra.core.Term) -> tuple[frozenlist[hydra.core.Type], hydra.core.Term]:
    match hydra.strip.deannotate_term(t):
        case hydra.core.TermLambda(value=lam):
            return hydra.lib.maybes.cases(lam.domain, (lambda : ((), t)), (lambda dom: (rest := collect_lambda_domains(lam.body), (hydra.lib.lists.cons(dom, hydra.lib.pairs.first(rest)), hydra.lib.pairs.second(rest)))[1]))

        case _:
            return ((), t)

def comparable_compare_expr(other_var: str, fname: str) -> hydra.java.syntax.Expression:
    @lru_cache(1)
    def this_field() -> hydra.java.syntax.Expression:
        return hydra.java.utils.java_identifier_to_java_expression(hydra.java.syntax.Identifier(hydra.java.utils.sanitize_java_name(fname)))
    @lru_cache(1)
    def other_field() -> hydra.java.syntax.Expression:
        return hydra.java.utils.java_expression_name_to_java_expression(hydra.java.utils.field_expression(hydra.java.utils.java_identifier(other_var), hydra.java.utils.java_identifier(fname)))
    return hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation_static(hydra.java.syntax.Identifier("hydra.util.Comparing"), hydra.java.syntax.Identifier("compare"), (this_field(), other_field())))

def hash_code_compare_expr(other_var: str, fname: str) -> hydra.java.syntax.Expression:
    @lru_cache(1)
    def header() -> hydra.java.syntax.MethodInvocation_Header:
        return cast(hydra.java.syntax.MethodInvocation_Header, hydra.java.syntax.MethodInvocation_HeaderComplex(hydra.java.syntax.MethodInvocation_Complex(cast(hydra.java.syntax.MethodInvocation_Variant, hydra.java.syntax.MethodInvocation_VariantType(hydra.java.utils.java_type_name(hydra.java.syntax.Identifier("Integer")))), (), hydra.java.syntax.Identifier("compare"))))
    @lru_cache(1)
    def this_hash_code() -> hydra.java.syntax.Expression:
        return hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.syntax.MethodInvocation(cast(hydra.java.syntax.MethodInvocation_Header, hydra.java.syntax.MethodInvocation_HeaderComplex(hydra.java.syntax.MethodInvocation_Complex(cast(hydra.java.syntax.MethodInvocation_Variant, hydra.java.syntax.MethodInvocation_VariantExpression(hydra.java.syntax.ExpressionName(Nothing(), hydra.java.syntax.Identifier(hydra.java.utils.sanitize_java_name(fname))))), (), hydra.java.syntax.Identifier(hydra.java.names.hash_code_method_name)))), ()))
    @lru_cache(1)
    def other_hash_code() -> hydra.java.syntax.Expression:
        return hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.syntax.MethodInvocation(cast(hydra.java.syntax.MethodInvocation_Header, hydra.java.syntax.MethodInvocation_HeaderComplex(hydra.java.syntax.MethodInvocation_Complex(cast(hydra.java.syntax.MethodInvocation_Variant, hydra.java.syntax.MethodInvocation_VariantExpression(hydra.java.utils.field_expression(hydra.java.utils.java_identifier(other_var), hydra.java.utils.java_identifier(fname)))), (), hydra.java.syntax.Identifier(hydra.java.names.hash_code_method_name)))), ()))
    return hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.syntax.MethodInvocation(header(), (this_hash_code(), other_hash_code())))

def is_binary_type(typ: hydra.core.Type):
    def _hoist_hydra_java_coder_is_binary_type_1(v1):
        match v1:
            case hydra.core.LiteralTypeBinary():
                return True

            case _:
                return False
    match hydra.strip.deannotate_type(typ):
        case hydra.core.TypeLiteral(value=lt):
            return _hoist_hydra_java_coder_is_binary_type_1(lt)

        case _:
            return False

def is_non_comparable_type(typ: hydra.core.Type):
    while True:
        def _hoist_hydra_java_coder_is_non_comparable_type_1(v1):
            match v1:
                case hydra.core.LiteralTypeBinary():
                    return True

                case _:
                    return False
        match hydra.strip.deannotate_type(typ):
            case hydra.core.TypeEither():
                return True

            case hydra.core.TypeFunction():
                return True

            case hydra.core.TypeUnit():
                return True

            case hydra.core.TypeLiteral(value=lt):
                return _hoist_hydra_java_coder_is_non_comparable_type_1(lt)

            case hydra.core.TypeForall(value=ft):
                typ = ft.body
                continue

            case _:
                return False

def compare_field_expr(other_var: str, ft: hydra.core.FieldType) -> hydra.java.syntax.Expression:
    fname = ft.name.value
    ftype = ft.type
    return hydra.lib.logic.if_else(is_binary_type(ftype), (lambda : arrays_compare_expr(other_var, fname)), (lambda : hydra.lib.logic.if_else(is_non_comparable_type(ftype), (lambda : hash_code_compare_expr(other_var, fname)), (lambda : comparable_compare_expr(other_var, fname)))))

def compare_and_return_stmts(other_var: str, f: hydra.core.FieldType) -> frozenlist[hydra.java.syntax.BlockStatement]:
    return (cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(hydra.java.utils.java_assignment_statement(cast(hydra.java.syntax.LeftHandSide, hydra.java.syntax.LeftHandSideExpressionName(hydra.java.syntax.ExpressionName(Nothing(), hydra.java.utils.java_identifier("cmp")))), compare_field_expr(other_var, f)))), cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(cast(hydra.java.syntax.Statement, hydra.java.syntax.StatementIfThen(hydra.java.syntax.IfThenStatement(cmp_not_zero_expr(), hydra.java.utils.java_return_statement(Just(hydra.java.utils.java_expression_name_to_java_expression(hydra.java.syntax.ExpressionName(Nothing(), hydra.java.utils.java_identifier("cmp")))))))))))

def compare_to_body(aliases: T0, other_var: str, fields: frozenlist[hydra.core.FieldType]) -> frozenlist[hydra.java.syntax.BlockStatement]:
    return hydra.lib.logic.if_else(hydra.lib.lists.null(fields), (lambda : (cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(hydra.java.utils.java_return_statement(Just(hydra.java.utils.java_int_expression(0))))),)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(fields), 1), (lambda : (cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(hydra.java.utils.java_return_statement(Just(compare_field_expr(other_var, hydra.lib.lists.head(fields)))))),)), (lambda : hydra.lib.lists.concat2((cmp_decl_statement(aliases),), hydra.lib.lists.concat2(hydra.lib.lists.concat(hydra.lib.lists.map((lambda f: compare_and_return_stmts(other_var, f)), hydra.lib.lists.init(fields))), (cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(hydra.java.utils.java_return_statement(Just(compare_field_expr(other_var, hydra.lib.lists.last(fields)))))),)))))))

def compare_to_zero_clause(tmp_name: str, fname: str) -> hydra.java.syntax.InclusiveOrExpression:
    @lru_cache(1)
    def compare_to_arg() -> hydra.java.syntax.Expression:
        return hydra.java.utils.java_expression_name_to_java_expression(hydra.java.utils.field_expression(hydra.java.utils.java_identifier(tmp_name), hydra.java.utils.java_identifier(fname)))
    @lru_cache(1)
    def compare_to_var() -> hydra.java.syntax.MethodInvocation_Variant:
        return cast(hydra.java.syntax.MethodInvocation_Variant, hydra.java.syntax.MethodInvocation_VariantExpression(hydra.java.utils.field_expression(hydra.java.syntax.Identifier("this"), hydra.java.utils.java_identifier(fname))))
    @lru_cache(1)
    def compare_to_header() -> hydra.java.syntax.MethodInvocation_Header:
        return cast(hydra.java.syntax.MethodInvocation_Header, hydra.java.syntax.MethodInvocation_HeaderComplex(hydra.java.syntax.MethodInvocation_Complex(compare_to_var(), (), hydra.java.syntax.Identifier(hydra.java.names.compare_to_method_name))))
    @lru_cache(1)
    def lhs() -> hydra.java.syntax.EqualityExpression:
        return hydra.java.utils.java_relational_expression_to_java_equality_expression(hydra.java.utils.java_postfix_expression_to_java_relational_expression(hydra.java.utils.java_method_invocation_to_java_postfix_expression(hydra.java.syntax.MethodInvocation(compare_to_header(), (compare_to_arg(),)))))
    @lru_cache(1)
    def rhs() -> hydra.java.syntax.RelationalExpression:
        return hydra.java.utils.java_postfix_expression_to_java_relational_expression(cast(hydra.java.syntax.PostfixExpression, hydra.java.syntax.PostfixExpressionPrimary(hydra.java.utils.java_literal_to_java_primary(hydra.java.utils.java_int(0)))))
    return hydra.java.utils.java_equality_expression_to_java_inclusive_or_expression(cast(hydra.java.syntax.EqualityExpression, hydra.java.syntax.EqualityExpressionEqual(hydra.java.syntax.EqualityExpression_Binary(lhs(), rhs()))))

def constant_decl(java_name: str, aliases: hydra.java.environment.Aliases, name: hydra.core.Name, cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.ClassBodyDeclarationWithComments]:
    mods = (cast(hydra.java.syntax.FieldModifier, hydra.java.syntax.FieldModifierPublic()), cast(hydra.java.syntax.FieldModifier, hydra.java.syntax.FieldModifierStatic()), cast(hydra.java.syntax.FieldModifier, hydra.java.syntax.FieldModifierFinal()))
    @lru_cache(1)
    def name_name() -> hydra.java.syntax.Identifier:
        return hydra.java.utils.name_to_java_name(aliases, hydra.core.Name("hydra.core.Name"))
    env = hydra.java.environment.JavaEnvironment(aliases, g)
    return hydra.lib.eithers.bind(encode_type(aliases, hydra.lib.sets.empty(), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Name"))), cx, g), (lambda jt: hydra.lib.eithers.bind(encode_term(env, cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(name.value)))), cx, g), (lambda arg: (init := cast(hydra.java.syntax.VariableInitializer, hydra.java.syntax.VariableInitializerExpression(hydra.java.utils.java_constructor_call(hydra.java.utils.java_constructor_name(name_name(), Nothing()), (arg,), Nothing()))), var := hydra.java.utils.java_variable_declarator(hydra.java.syntax.Identifier(java_name), Just(init)), Right(no_comment(hydra.java.utils.java_member_field(mods, jt, var))))[2]))))

def constant_decl_for_field_type(aliases: hydra.java.environment.Aliases, ftyp: hydra.core.FieldType, cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.ClassBodyDeclarationWithComments]:
    name = ftyp.name
    @lru_cache(1)
    def java_name() -> str:
        return hydra.formatting.non_alnum_to_underscores(hydra.formatting.convert_case(hydra.util.CaseConvention.CAMEL, hydra.util.CaseConvention.UPPER_SNAKE, name.value))
    return constant_decl(java_name(), aliases, name, cx, g)

def constant_decl_for_type_name(aliases: hydra.java.environment.Aliases, name: hydra.core.Name, cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.ClassBodyDeclarationWithComments]:
    return constant_decl("TYPE_", aliases, name, cx, g)

def construct_elements_interface(mod: hydra.packaging.Module, members: frozenlist[hydra.java.syntax.InterfaceMemberDeclaration]) -> tuple[hydra.core.Name, hydra.java.syntax.CompilationUnit]:
    ns = mod.namespace
    @lru_cache(1)
    def parent_ns() -> Maybe[hydra.packaging.Namespace]:
        return namespace_parent(ns)
    @lru_cache(1)
    def pkg() -> hydra.java.syntax.PackageDeclaration:
        return hydra.lib.maybes.cases(parent_ns(), (lambda : hydra.java.utils.java_package_declaration(ns)), (lambda pns: hydra.java.utils.java_package_declaration(pns)))
    mods = (cast(hydra.java.syntax.InterfaceModifier, hydra.java.syntax.InterfaceModifierPublic()),)
    @lru_cache(1)
    def class_name() -> str:
        return elements_class_name(ns)
    @lru_cache(1)
    def el_name() -> hydra.core.Name:
        return elements_qualified_name(ns)
    body = hydra.java.syntax.InterfaceBody(members)
    @lru_cache(1)
    def itf() -> hydra.java.syntax.TypeDeclaration:
        return cast(hydra.java.syntax.TypeDeclaration, hydra.java.syntax.TypeDeclarationInterface(cast(hydra.java.syntax.InterfaceDeclaration, hydra.java.syntax.InterfaceDeclarationNormalInterface(hydra.java.syntax.NormalInterfaceDeclaration(mods, hydra.java.utils.java_type_identifier(class_name()), (), (), body)))))
    decl = hydra.java.syntax.TypeDeclarationWithComments(itf(), mod.description)
    return (el_name(), cast(hydra.java.syntax.CompilationUnit, hydra.java.syntax.CompilationUnitOrdinary(hydra.java.syntax.OrdinaryCompilationUnit(Just(pkg()), (), (decl,)))))

def interface_types(is_ser: bool, aliases: hydra.java.environment.Aliases, tparams: frozenlist[hydra.java.syntax.TypeParameter], el_name: hydra.core.Name) -> frozenlist[hydra.java.syntax.InterfaceType]:
    @lru_cache(1)
    def java_serializable_type() -> hydra.java.syntax.InterfaceType:
        return hydra.java.syntax.InterfaceType(hydra.java.syntax.ClassType((), cast(hydra.java.syntax.ClassTypeQualifier, hydra.java.syntax.ClassTypeQualifierNone()), hydra.java.utils.java_type_identifier("Serializable"), ()))
    @lru_cache(1)
    def self_type_arg() -> hydra.java.syntax.TypeArgument:
        return cast(hydra.java.syntax.TypeArgument, hydra.java.syntax.TypeArgumentReference(hydra.java.utils.name_to_java_reference_type(aliases, False, hydra.lib.lists.map((lambda tp_: hydra.java.utils.type_parameter_to_type_argument(tp_)), tparams), el_name, Nothing())))
    @lru_cache(1)
    def java_comparable_type() -> hydra.java.syntax.InterfaceType:
        return hydra.java.syntax.InterfaceType(hydra.java.syntax.ClassType((), cast(hydra.java.syntax.ClassTypeQualifier, hydra.java.syntax.ClassTypeQualifierNone()), hydra.java.utils.java_type_identifier("Comparable"), (self_type_arg(),)))
    return hydra.lib.logic.if_else(is_ser, (lambda : (java_serializable_type(), java_comparable_type())), (lambda : ()))

def record_compare_to_method(aliases: hydra.java.environment.Aliases, tparams: T0, el_name: hydra.core.Name, fields: frozenlist[hydra.core.FieldType]) -> hydra.java.syntax.ClassBodyDeclaration:
    anns = (hydra.java.utils.override_annotation(), hydra.java.utils.suppress_warnings_unchecked_annotation())
    mods = (cast(hydra.java.syntax.MethodModifier, hydra.java.syntax.MethodModifierPublic()),)
    @lru_cache(1)
    def param() -> hydra.java.syntax.FormalParameter:
        return hydra.java.utils.java_type_to_java_formal_parameter(hydra.java.utils.java_type_from_type_name(aliases, el_name), hydra.core.Name(hydra.java.names.other_instance_name))
    @lru_cache(1)
    def result() -> hydra.java.syntax.Result:
        return hydra.java.utils.java_type_to_java_result(hydra.java.utils.java_int_type())
    return hydra.java.utils.method_declaration(mods, (), anns, hydra.java.names.compare_to_method_name, (param(),), result(), Just(compare_to_body(aliases, hydra.java.names.other_instance_name, fields)))

def field_type_to_formal_param(aliases: hydra.java.environment.Aliases, ft: hydra.core.FieldType, cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.FormalParameter]:
    return hydra.lib.eithers.bind(encode_type(aliases, hydra.lib.sets.empty(), ft.type, cx, g), (lambda jt: Right(hydra.java.utils.java_type_to_java_formal_parameter(jt, ft.name))))

def record_constructor(aliases: hydra.java.environment.Aliases, el_name: hydra.core.Name, fields: frozenlist[hydra.core.FieldType], cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.ClassBodyDeclaration]:
    @lru_cache(1)
    def assign_stmts() -> frozenlist[hydra.java.syntax.BlockStatement]:
        return hydra.lib.lists.map((lambda f: cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(hydra.java.utils.to_assign_stmt(f.name)))), fields)
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda f: field_type_to_formal_param(aliases, f, cx, g)), fields), (lambda params: Right(hydra.java.utils.make_constructor(aliases, el_name, False, params, assign_stmts()))))

def equals_clause(tmp_name: str, fname: str) -> hydra.java.syntax.InclusiveOrExpression:
    @lru_cache(1)
    def this_arg() -> hydra.java.syntax.Expression:
        return hydra.java.utils.java_expression_name_to_java_expression(hydra.java.utils.field_expression(hydra.java.syntax.Identifier("this"), hydra.java.utils.java_identifier(fname)))
    @lru_cache(1)
    def other_arg() -> hydra.java.syntax.Expression:
        return hydra.java.utils.java_expression_name_to_java_expression(hydra.java.utils.field_expression(hydra.java.utils.java_identifier(tmp_name), hydra.java.utils.java_identifier(fname)))
    @lru_cache(1)
    def header() -> hydra.java.syntax.MethodInvocation_Header:
        return cast(hydra.java.syntax.MethodInvocation_Header, hydra.java.syntax.MethodInvocation_HeaderComplex(hydra.java.syntax.MethodInvocation_Complex(cast(hydra.java.syntax.MethodInvocation_Variant, hydra.java.syntax.MethodInvocation_VariantType(hydra.java.utils.java_type_name(hydra.java.syntax.Identifier("java.util.Objects")))), (), hydra.java.syntax.Identifier(hydra.java.names.equals_method_name))))
    return hydra.java.utils.java_postfix_expression_to_java_inclusive_or_expression(hydra.java.utils.java_method_invocation_to_java_postfix_expression(hydra.java.syntax.MethodInvocation(header(), (this_arg(), other_arg()))))

def is_big_numeric_type(typ: hydra.core.Type):
    def _hoist_hydra_java_coder_is_big_numeric_type_1(v1):
        match v1:
            case hydra.core.FloatType.BIGFLOAT:
                return True

            case _:
                return False
    def _hoist_hydra_java_coder_is_big_numeric_type_2(v1):
        match v1:
            case hydra.core.IntegerType.BIGINT:
                return True

            case _:
                return False
    def _hoist_hydra_java_coder_is_big_numeric_type_3(v1):
        match v1:
            case hydra.core.LiteralTypeFloat(value=ft):
                return _hoist_hydra_java_coder_is_big_numeric_type_1(ft)

            case hydra.core.LiteralTypeInteger(value=it):
                return _hoist_hydra_java_coder_is_big_numeric_type_2(it)

            case _:
                return False
    match hydra.strip.deannotate_type(typ):
        case hydra.core.TypeLiteral(value=lt):
            return _hoist_hydra_java_coder_is_big_numeric_type_3(lt)

        case _:
            return False

def eq_clause(tmp_name: str, ft: hydra.core.FieldType) -> hydra.java.syntax.InclusiveOrExpression:
    fname = ft.name.value
    ftype = ft.type
    return hydra.lib.logic.if_else(is_binary_type(ftype), (lambda : arrays_equals_clause(tmp_name, fname)), (lambda : hydra.lib.logic.if_else(is_big_numeric_type(ftype), (lambda : compare_to_zero_clause(tmp_name, fname)), (lambda : equals_clause(tmp_name, fname)))))

def record_equals_method(aliases: hydra.java.environment.Aliases, el_name: hydra.core.Name, fields: frozenlist[hydra.core.FieldType]) -> hydra.java.syntax.ClassBodyDeclaration:
    anns = (hydra.java.utils.override_annotation(),)
    mods = (cast(hydra.java.syntax.MethodModifier, hydra.java.syntax.MethodModifierPublic()),)
    @lru_cache(1)
    def param() -> hydra.java.syntax.FormalParameter:
        return hydra.java.utils.java_type_to_java_formal_parameter(hydra.java.utils.java_ref_type((), Nothing(), "Object"), hydra.core.Name(hydra.java.names.other_instance_name))
    @lru_cache(1)
    def result() -> hydra.java.syntax.Result:
        return hydra.java.utils.java_type_to_java_result(hydra.java.utils.java_boolean_type())
    tmp_name = "o"
    @lru_cache(1)
    def instance_of_stmt() -> hydra.java.syntax.BlockStatement:
        return cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(cast(hydra.java.syntax.Statement, hydra.java.syntax.StatementIfThen(hydra.java.syntax.IfThenStatement(hydra.java.utils.java_unary_expression_to_java_expression(cast(hydra.java.syntax.UnaryExpression, hydra.java.syntax.UnaryExpressionOther(cast(hydra.java.syntax.UnaryExpressionNotPlusMinus, hydra.java.syntax.UnaryExpressionNotPlusMinusNot(hydra.java.utils.java_relational_expression_to_java_unary_expression(hydra.java.utils.java_instance_of(hydra.java.utils.java_identifier_to_java_relational_expression(hydra.java.utils.java_identifier(hydra.java.names.other_instance_name)), hydra.java.utils.name_to_java_reference_type(aliases, False, (), el_name, Nothing())))))))), hydra.java.utils.java_return_statement(Just(hydra.java.utils.java_boolean_expression(False))))))))
    @lru_cache(1)
    def cast_stmt() -> hydra.java.syntax.BlockStatement:
        return hydra.java.utils.variable_declaration_statement(aliases, hydra.java.utils.java_type_from_type_name(aliases, el_name), hydra.java.utils.java_identifier(tmp_name), hydra.java.utils.java_cast_expression_to_java_expression(hydra.java.utils.java_cast_expression(hydra.java.utils.name_to_java_reference_type(aliases, False, (), el_name, Nothing()), hydra.java.utils.java_identifier_to_java_unary_expression(hydra.java.syntax.Identifier(hydra.java.utils.sanitize_java_name(hydra.java.names.other_instance_name))))))
    @lru_cache(1)
    def return_all_fields_equal() -> hydra.java.syntax.BlockStatement:
        return cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(hydra.java.utils.java_return_statement(Just(hydra.lib.logic.if_else(hydra.lib.lists.null(fields), (lambda : hydra.java.utils.java_boolean_expression(True)), (lambda : hydra.java.utils.java_conditional_and_expression_to_java_expression(hydra.java.syntax.ConditionalAndExpression(hydra.lib.lists.map((lambda f: eq_clause(tmp_name, f)), fields)))))))))
    return hydra.java.utils.method_declaration(mods, (), anns, hydra.java.names.equals_method_name, (param(),), result(), Just((instance_of_stmt(), cast_stmt(), return_all_fields_equal())))

first20_primes = (2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71)

def hash_code_mult_pair(i: int, fname: hydra.core.Name) -> hydra.java.syntax.MultiplicativeExpression:
    fname_str = fname.value
    @lru_cache(1)
    def lhs() -> hydra.java.syntax.MultiplicativeExpression:
        return cast(hydra.java.syntax.MultiplicativeExpression, hydra.java.syntax.MultiplicativeExpressionUnary(hydra.java.utils.java_primary_to_java_unary_expression(hydra.java.utils.java_literal_to_java_primary(hydra.java.utils.java_int(i)))))
    @lru_cache(1)
    def rhs() -> hydra.java.syntax.UnaryExpression:
        return hydra.java.utils.java_postfix_expression_to_java_unary_expression(hydra.java.utils.java_method_invocation_to_java_postfix_expression(hydra.java.syntax.MethodInvocation(cast(hydra.java.syntax.MethodInvocation_Header, hydra.java.syntax.MethodInvocation_HeaderComplex(hydra.java.syntax.MethodInvocation_Complex(cast(hydra.java.syntax.MethodInvocation_Variant, hydra.java.syntax.MethodInvocation_VariantType(hydra.java.utils.java_type_name(hydra.java.syntax.Identifier("java.util.Objects")))), (), hydra.java.syntax.Identifier(hydra.java.names.hash_code_method_name)))), (hydra.java.utils.java_expression_name_to_java_expression(hydra.java.syntax.ExpressionName(Nothing(), hydra.java.syntax.Identifier(hydra.java.utils.sanitize_java_name(fname_str)))),))))
    return cast(hydra.java.syntax.MultiplicativeExpression, hydra.java.syntax.MultiplicativeExpressionTimes(hydra.java.syntax.MultiplicativeExpression_Binary(lhs(), rhs())))

def record_hash_code_method(fields: frozenlist[hydra.core.FieldType]) -> hydra.java.syntax.ClassBodyDeclaration:
    anns = (hydra.java.utils.override_annotation(),)
    mods = (cast(hydra.java.syntax.MethodModifier, hydra.java.syntax.MethodModifierPublic()),)
    @lru_cache(1)
    def result() -> hydra.java.syntax.Result:
        return hydra.java.utils.java_type_to_java_result(hydra.java.utils.java_int_type())
    @lru_cache(1)
    def return_sum() -> hydra.java.syntax.BlockStatement:
        return cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(hydra.lib.logic.if_else(hydra.lib.lists.null(fields), (lambda : hydra.java.utils.java_return_statement(Just(hydra.java.utils.java_int_expression(0)))), (lambda : hydra.java.utils.java_return_statement(Just(hydra.java.utils.java_additive_expression_to_java_expression(hydra.java.utils.add_expressions(hydra.lib.lists.zip_with((lambda x1, x2: hash_code_mult_pair(x1, x2)), first20_primes, hydra.lib.lists.map((lambda f: f.name), fields))))))))))
    return hydra.java.utils.method_declaration(mods, (), anns, hydra.java.names.hash_code_method_name, (), result(), Just((return_sum(),)))

def record_member_var(aliases: hydra.java.environment.Aliases, ft: hydra.core.FieldType, cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.ClassBodyDeclaration]:
    mods = (cast(hydra.java.syntax.FieldModifier, hydra.java.syntax.FieldModifierPublic()), cast(hydra.java.syntax.FieldModifier, hydra.java.syntax.FieldModifierFinal()))
    fname = ft.name
    ftype = ft.type
    return hydra.lib.eithers.bind(encode_type(aliases, hydra.lib.sets.empty(), ftype, cx, g), (lambda jt: Right(hydra.java.utils.java_member_field(mods, jt, hydra.java.utils.field_name_to_java_variable_declarator(fname)))))

def record_with_method(aliases: hydra.java.environment.Aliases, el_name: hydra.core.Name, fields: frozenlist[hydra.core.FieldType], field: hydra.core.FieldType, cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.ClassBodyDeclaration]:
    mods = (cast(hydra.java.syntax.MethodModifier, hydra.java.syntax.MethodModifierPublic()),)
    @lru_cache(1)
    def anns() -> frozenlist[T1]:
        return ()
    @lru_cache(1)
    def method_name() -> str:
        return hydra.lib.strings.cat2("with", hydra.formatting.non_alnum_to_underscores(hydra.formatting.capitalize(field.name.value)))
    @lru_cache(1)
    def result() -> hydra.java.syntax.Result:
        return hydra.java.utils.reference_type_to_result(hydra.java.utils.name_to_java_reference_type(aliases, False, (), el_name, Nothing()))
    @lru_cache(1)
    def cons_id() -> hydra.java.syntax.Identifier:
        return hydra.java.syntax.Identifier(hydra.java.utils.sanitize_java_name(hydra.names.local_name_of(el_name)))
    @lru_cache(1)
    def field_args() -> frozenlist[hydra.java.syntax.Expression]:
        return hydra.lib.lists.map((lambda f: hydra.java.utils.field_name_to_java_expression(f.name)), fields)
    @lru_cache(1)
    def return_stmt() -> hydra.java.syntax.BlockStatement:
        return cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(hydra.java.utils.java_return_statement(Just(hydra.java.utils.java_constructor_call(hydra.java.utils.java_constructor_name(cons_id(), Nothing()), field_args(), Nothing())))))
    return hydra.lib.eithers.bind(field_type_to_formal_param(aliases, field, cx, g), (lambda param: Right(hydra.java.utils.method_declaration(mods, (), anns(), method_name(), (param,), result(), Just((return_stmt(),))))))

def serializable_types(is_ser: bool) -> frozenlist[hydra.java.syntax.InterfaceType]:
    @lru_cache(1)
    def java_serializable_type() -> hydra.java.syntax.InterfaceType:
        return hydra.java.syntax.InterfaceType(hydra.java.syntax.ClassType((), cast(hydra.java.syntax.ClassTypeQualifier, hydra.java.syntax.ClassTypeQualifierNone()), hydra.java.utils.java_type_identifier("Serializable"), ()))
    return hydra.lib.logic.if_else(is_ser, (lambda : (java_serializable_type(),)), (lambda : ()))

@lru_cache(1)
def tag_cmp_not_zero_expr() -> hydra.java.syntax.Expression:
    @lru_cache(1)
    def lhs() -> hydra.java.syntax.EqualityExpression:
        return hydra.java.utils.java_relational_expression_to_java_equality_expression(hydra.java.utils.java_postfix_expression_to_java_relational_expression(cast(hydra.java.syntax.PostfixExpression, hydra.java.syntax.PostfixExpressionName(hydra.java.syntax.ExpressionName(Nothing(), hydra.java.utils.java_identifier("tagCmp"))))))
    @lru_cache(1)
    def rhs() -> hydra.java.syntax.RelationalExpression:
        return hydra.java.utils.java_postfix_expression_to_java_relational_expression(cast(hydra.java.syntax.PostfixExpression, hydra.java.syntax.PostfixExpressionPrimary(hydra.java.utils.java_literal_to_java_primary(hydra.java.utils.java_int(0)))))
    return hydra.java.utils.java_equality_expression_to_java_expression(cast(hydra.java.syntax.EqualityExpression, hydra.java.syntax.EqualityExpressionNotEqual(hydra.java.syntax.EqualityExpression_Binary(lhs(), rhs()))))

@lru_cache(1)
def tag_compare_expr() -> hydra.java.syntax.Expression:
    @lru_cache(1)
    def this_get_class() -> hydra.java.syntax.MethodInvocation:
        return hydra.java.syntax.MethodInvocation(cast(hydra.java.syntax.MethodInvocation_Header, hydra.java.syntax.MethodInvocation_HeaderComplex(hydra.java.syntax.MethodInvocation_Complex(cast(hydra.java.syntax.MethodInvocation_Variant, hydra.java.syntax.MethodInvocation_VariantPrimary(hydra.java.utils.java_expression_to_java_primary(hydra.java.utils.java_this))), (), hydra.java.syntax.Identifier("getClass")))), ())
    @lru_cache(1)
    def this_get_name() -> hydra.java.syntax.MethodInvocation:
        return hydra.java.syntax.MethodInvocation(cast(hydra.java.syntax.MethodInvocation_Header, hydra.java.syntax.MethodInvocation_HeaderComplex(hydra.java.syntax.MethodInvocation_Complex(cast(hydra.java.syntax.MethodInvocation_Variant, hydra.java.syntax.MethodInvocation_VariantPrimary(hydra.java.utils.java_method_invocation_to_java_primary(this_get_class()))), (), hydra.java.syntax.Identifier("getName")))), ())
    @lru_cache(1)
    def other_get_class() -> hydra.java.syntax.MethodInvocation:
        return hydra.java.syntax.MethodInvocation(cast(hydra.java.syntax.MethodInvocation_Header, hydra.java.syntax.MethodInvocation_HeaderComplex(hydra.java.syntax.MethodInvocation_Complex(cast(hydra.java.syntax.MethodInvocation_Variant, hydra.java.syntax.MethodInvocation_VariantExpression(hydra.java.syntax.ExpressionName(Nothing(), hydra.java.syntax.Identifier(hydra.java.names.other_instance_name)))), (), hydra.java.syntax.Identifier("getClass")))), ())
    @lru_cache(1)
    def other_get_name() -> hydra.java.syntax.MethodInvocation:
        return hydra.java.syntax.MethodInvocation(cast(hydra.java.syntax.MethodInvocation_Header, hydra.java.syntax.MethodInvocation_HeaderComplex(hydra.java.syntax.MethodInvocation_Complex(cast(hydra.java.syntax.MethodInvocation_Variant, hydra.java.syntax.MethodInvocation_VariantPrimary(hydra.java.utils.java_method_invocation_to_java_primary(other_get_class()))), (), hydra.java.syntax.Identifier("getName")))), ())
    return hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.syntax.MethodInvocation(cast(hydra.java.syntax.MethodInvocation_Header, hydra.java.syntax.MethodInvocation_HeaderComplex(hydra.java.syntax.MethodInvocation_Complex(cast(hydra.java.syntax.MethodInvocation_Variant, hydra.java.syntax.MethodInvocation_VariantPrimary(hydra.java.utils.java_method_invocation_to_java_primary(this_get_name()))), (), hydra.java.syntax.Identifier(hydra.java.names.compare_to_method_name)))), (hydra.java.utils.java_method_invocation_to_java_expression(other_get_name()),)))

def variant_compare_to_method(aliases: hydra.java.environment.Aliases, tparams: T0, parent_name: hydra.core.Name, variant_name: hydra.core.Name, fields: frozenlist[hydra.core.FieldType]) -> hydra.java.syntax.ClassBodyDeclaration:
    anns = (hydra.java.utils.override_annotation(), hydra.java.utils.suppress_warnings_unchecked_annotation())
    mods = (cast(hydra.java.syntax.MethodModifier, hydra.java.syntax.MethodModifierPublic()),)
    @lru_cache(1)
    def param() -> hydra.java.syntax.FormalParameter:
        return hydra.java.utils.java_type_to_java_formal_parameter(hydra.java.utils.java_type_from_type_name(aliases, parent_name), hydra.core.Name(hydra.java.names.other_instance_name))
    @lru_cache(1)
    def result() -> hydra.java.syntax.Result:
        return hydra.java.utils.java_type_to_java_result(hydra.java.utils.java_int_type())
    var_tmp_name = "o"
    @lru_cache(1)
    def tag_decl_stmt() -> hydra.java.syntax.BlockStatement:
        return hydra.java.utils.variable_declaration_statement(aliases, hydra.java.utils.java_int_type(), hydra.java.utils.java_identifier("tagCmp"), tag_compare_expr())
    @lru_cache(1)
    def tag_return_stmt() -> hydra.java.syntax.BlockStatement:
        return cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(cast(hydra.java.syntax.Statement, hydra.java.syntax.StatementIfThen(hydra.java.syntax.IfThenStatement(tag_cmp_not_zero_expr(), hydra.java.utils.java_return_statement(Just(hydra.java.utils.java_expression_name_to_java_expression(hydra.java.syntax.ExpressionName(Nothing(), hydra.java.utils.java_identifier("tagCmp"))))))))))
    @lru_cache(1)
    def variant_java_type() -> hydra.java.syntax.Type:
        return hydra.java.utils.java_type_from_type_name(aliases, variant_name)
    @lru_cache(1)
    def cast_other_expr() -> hydra.java.syntax.Expression:
        return hydra.java.utils.java_cast_expression_to_java_expression(hydra.java.utils.java_cast_expression(hydra.java.utils.name_to_java_reference_type(aliases, False, (), variant_name, Nothing()), hydra.java.utils.java_identifier_to_java_unary_expression(hydra.java.syntax.Identifier(hydra.java.names.other_instance_name))))
    @lru_cache(1)
    def cast_decl_stmt() -> hydra.java.syntax.BlockStatement:
        return hydra.java.utils.variable_declaration_statement(aliases, variant_java_type(), hydra.java.utils.java_identifier(var_tmp_name), cast_other_expr())
    @lru_cache(1)
    def empty_return() -> frozenlist[hydra.java.syntax.BlockStatement]:
        return (cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(hydra.java.utils.java_return_statement(Just(hydra.java.utils.java_int_expression(0))))),)
    @lru_cache(1)
    def value_compare_stmt() -> frozenlist[hydra.java.syntax.BlockStatement]:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(fields), (lambda : empty_return()), (lambda : hydra.lib.lists.concat2((cast_decl_stmt(),), compare_to_body(aliases, var_tmp_name, fields))))
    @lru_cache(1)
    def body() -> frozenlist[hydra.java.syntax.BlockStatement]:
        return hydra.lib.lists.concat2((tag_decl_stmt(), tag_return_stmt()), value_compare_stmt())
    return hydra.java.utils.method_declaration(mods, (), anns, hydra.java.names.compare_to_method_name, (param(),), result(), Just(body()))

def declaration_for_record_type_(is_inner: bool, is_ser: bool, aliases: hydra.java.environment.Aliases, tparams: frozenlist[hydra.java.syntax.TypeParameter], el_name: hydra.core.Name, parent_name: Maybe[hydra.core.Name], fields: frozenlist[hydra.core.FieldType], cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.ClassDeclaration]:
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda f: record_member_var(aliases, f, cx, g)), fields), (lambda member_vars: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda p: add_comment(hydra.lib.pairs.first(p), hydra.lib.pairs.second(p), cx, g)), hydra.lib.lists.zip(member_vars, fields)), (lambda member_vars_: hydra.lib.eithers.bind(hydra.lib.logic.if_else(hydra.lib.equality.gt(hydra.lib.lists.length(fields), 1), (lambda : hydra.lib.eithers.map_list((lambda f: record_with_method(aliases, el_name, fields, f, cx, g)), fields)), (lambda : Right(()))), (lambda with_methods: hydra.lib.eithers.bind(record_constructor(aliases, el_name, fields, cx, g), (lambda cons: hydra.lib.eithers.bind(hydra.lib.logic.if_else(is_inner, (lambda : Right(())), (lambda : hydra.lib.eithers.bind(constant_decl_for_type_name(aliases, el_name, cx, g), (lambda d: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda f: constant_decl_for_field_type(aliases, f, cx, g)), fields), (lambda dfields: Right(hydra.lib.lists.cons(d, dfields)))))))), (lambda tn: (comparable_methods := hydra.lib.maybes.cases(parent_name, (lambda : hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.logic.not_(is_inner), is_ser), (lambda : (record_compare_to_method(aliases, tparams, el_name, fields),)), (lambda : ()))), (lambda pn: hydra.lib.logic.if_else(is_ser, (lambda : (variant_compare_to_method(aliases, tparams, pn, el_name, fields),)), (lambda : ())))), body_decls := hydra.lib.lists.concat2(tn, hydra.lib.lists.concat2(member_vars_, hydra.lib.lists.map((lambda x: no_comment(x)), hydra.lib.lists.concat2((cons, record_equals_method(aliases, el_name, fields), record_hash_code_method(fields)), hydra.lib.lists.concat2(comparable_methods, with_methods))))), ifaces := hydra.lib.logic.if_else(is_inner, (lambda : serializable_types(is_ser)), (lambda : interface_types(is_ser, aliases, tparams, el_name))), Right(hydra.java.utils.java_class_declaration(aliases, tparams, el_name, class_mods_public, Nothing(), ifaces, body_decls)))[3]))))))))))

def declaration_for_record_type(is_inner: bool, is_ser: bool, aliases: hydra.java.environment.Aliases, tparams: frozenlist[hydra.java.syntax.TypeParameter], el_name: hydra.core.Name, fields: frozenlist[hydra.core.FieldType], cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.ClassDeclaration]:
    return declaration_for_record_type_(is_inner, is_ser, aliases, tparams, el_name, Nothing(), fields, cx, g)

def declaration_for_union_type(is_ser: bool, aliases: hydra.java.environment.Aliases, tparams: frozenlist[hydra.java.syntax.TypeParameter], el_name: hydra.core.Name, fields: frozenlist[hydra.core.FieldType], cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.ClassDeclaration]:
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda ft: (fname := ft.name, ftype := ft.type, rfields := hydra.lib.logic.if_else(hydra.predicates.is_unit_type(hydra.strip.deannotate_type(ftype)), (lambda : ()), (lambda : (hydra.core.FieldType(hydra.core.Name("value"), hydra.strip.deannotate_type(ftype)),))), var_name := hydra.java.utils.variant_class_name(False, el_name, fname), hydra.lib.eithers.bind(declaration_for_record_type_(True, is_ser, aliases, (), var_name, hydra.lib.logic.if_else(is_ser, (lambda : Just(el_name)), (lambda : Nothing())), rfields, cx, g), (lambda inner_decl: Right(augment_variant_class(aliases, tparams, el_name, inner_decl)))))[4]), fields), (lambda variant_classes: (variant_decls := hydra.lib.lists.map((lambda vc: cast(hydra.java.syntax.ClassBodyDeclaration, hydra.java.syntax.ClassBodyDeclarationClassMember(cast(hydra.java.syntax.ClassMemberDeclaration, hydra.java.syntax.ClassMemberDeclarationClass(vc))))), variant_classes), hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda pair: add_comment(hydra.lib.pairs.first(pair), hydra.lib.pairs.second(pair), cx, g)), hydra.lib.lists.zip(variant_decls, fields)), (lambda variant_decls_: (private_const := hydra.java.utils.make_constructor(aliases, el_name, True, (), ()), accept_decl := hydra.java.utils.to_accept_method(True, tparams), vtparams := hydra.lib.lists.concat2(tparams, (hydra.java.utils.java_type_parameter(hydra.java.names.visitor_return_parameter),)), visitor_methods := hydra.lib.lists.map((lambda ft: (fname := ft.name, type_args := hydra.lib.lists.map((lambda tp: hydra.java.utils.type_parameter_to_type_argument(tp)), tparams), var_ref := hydra.java.utils.java_class_type_to_java_type(hydra.java.utils.name_to_java_class_type(aliases, False, type_args, hydra.java.utils.variant_class_name(False, el_name, fname), Nothing())), param := hydra.java.utils.java_type_to_java_formal_parameter(var_ref, hydra.core.Name("instance")), result_r := hydra.java.utils.java_type_to_java_result(cast(hydra.java.syntax.Type, hydra.java.syntax.TypeReference(hydra.java.utils.visitor_type_variable()))), hydra.java.utils.interface_method_declaration((), (), hydra.java.names.visit_method_name, (param,), result_r, Nothing()))[5]), fields), visitor_body := hydra.java.syntax.InterfaceBody(visitor_methods), visitor := hydra.java.utils.java_interface_declaration_to_java_class_body_declaration(hydra.java.syntax.NormalInterfaceDeclaration((cast(hydra.java.syntax.InterfaceModifier, hydra.java.syntax.InterfaceModifierPublic()),), hydra.java.syntax.TypeIdentifier(hydra.java.syntax.Identifier(hydra.java.names.visitor_name)), vtparams, (), visitor_body)), type_args := hydra.lib.lists.map((lambda tp: hydra.java.utils.type_parameter_to_type_argument(tp)), tparams), visitor_class_type := hydra.java.utils.java_class_type(hydra.lib.lists.concat2(hydra.lib.lists.map((lambda tp: hydra.java.utils.type_parameter_to_reference_type(tp)), tparams), (hydra.java.utils.visitor_type_variable(),)), Nothing(), hydra.java.names.visitor_name), main_instance_param := hydra.java.utils.java_type_to_java_formal_parameter(hydra.java.utils.java_class_type_to_java_type(hydra.java.utils.name_to_java_class_type(aliases, False, type_args, el_name, Nothing())), hydra.core.Name("instance")), result_r := hydra.java.utils.java_type_to_java_result(cast(hydra.java.syntax.Type, hydra.java.syntax.TypeReference(hydra.java.utils.visitor_type_variable()))), throw_stmt := cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(hydra.java.utils.java_throw_illegal_state_exception((hydra.java.utils.java_additive_expression_to_java_expression(hydra.java.utils.add_expressions((hydra.java.utils.java_string_multiplicative_expression("Non-exhaustive patterns when matching: "), cast(hydra.java.syntax.MultiplicativeExpression, hydra.java.syntax.MultiplicativeExpressionUnary(hydra.java.utils.java_identifier_to_java_unary_expression(hydra.java.syntax.Identifier("instance"))))))),)))), default_mod := (cast(hydra.java.syntax.InterfaceMethodModifier, hydra.java.syntax.InterfaceMethodModifierDefault()),), otherwise_decl := hydra.java.utils.interface_method_declaration(default_mod, (), hydra.java.names.otherwise_method_name, (main_instance_param,), result_r, Just((throw_stmt,))), pv_visit_methods := hydra.lib.lists.map((lambda ft: (fname := ft.name, var_ref := hydra.java.utils.java_class_type_to_java_type(hydra.java.utils.name_to_java_class_type(aliases, False, type_args, hydra.java.utils.variant_class_name(False, el_name, fname), Nothing())), param := hydra.java.utils.java_type_to_java_formal_parameter(var_ref, hydra.core.Name("instance")), mi := hydra.java.utils.method_invocation(Nothing(), hydra.java.syntax.Identifier(hydra.java.names.otherwise_method_name), (hydra.java.utils.java_identifier_to_java_expression(hydra.java.syntax.Identifier("instance")),)), return_otherwise := cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(hydra.java.utils.java_return_statement(Just(hydra.java.utils.java_primary_to_java_expression(hydra.java.utils.java_method_invocation_to_java_primary(mi)))))), hydra.java.utils.interface_method_declaration(default_mod, (), hydra.java.names.visit_method_name, (param,), result_r, Just((return_otherwise,))))[5]), fields), pv_body := hydra.java.syntax.InterfaceBody(hydra.lib.lists.concat2((otherwise_decl,), pv_visit_methods)), partial_visitor := hydra.java.utils.java_interface_declaration_to_java_class_body_declaration(hydra.java.syntax.NormalInterfaceDeclaration((cast(hydra.java.syntax.InterfaceModifier, hydra.java.syntax.InterfaceModifierPublic()),), hydra.java.syntax.TypeIdentifier(hydra.java.syntax.Identifier(hydra.java.names.partial_visitor_name)), vtparams, (hydra.java.syntax.InterfaceType(visitor_class_type),), pv_body)), hydra.lib.eithers.bind(constant_decl_for_type_name(aliases, el_name, cx, g), (lambda tn0: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda ft: constant_decl_for_field_type(aliases, ft, cx, g)), fields), (lambda tn1: (tn := hydra.lib.lists.concat2((tn0,), tn1), other_decls := hydra.lib.lists.map((lambda d: no_comment(d)), (private_const, accept_decl, visitor, partial_visitor)), body_decls := hydra.lib.lists.concat((tn, other_decls, variant_decls_)), mods := hydra.lib.lists.concat2(class_mods_public, (cast(hydra.java.syntax.ClassModifier, hydra.java.syntax.ClassModifierAbstract()),)), Right(hydra.java.utils.java_class_declaration(aliases, tparams, el_name, mods, Nothing(), interface_types(is_ser, aliases, tparams, el_name), body_decls)))[4])))))[16])))[1]))

def encode_term_t_c_o(env0: hydra.java.environment.JavaEnvironment, func_name: hydra.core.Name, param_names: frozenlist[hydra.core.Name], tco_var_renames: FrozenDict[hydra.core.Name, hydra.core.Name], tco_depth: int, term: hydra.core.Term, cx: hydra.context.Context, g: hydra.graph.Graph):
    aliases0 = env0.aliases
    @lru_cache(1)
    def env() -> hydra.java.environment.JavaEnvironment:
        return hydra.java.environment.JavaEnvironment(hydra.java.environment.Aliases(aliases0.current_namespace, aliases0.packages, aliases0.branch_vars, aliases0.recursive_vars, aliases0.in_scope_type_params, aliases0.polymorphic_locals, aliases0.in_scope_java_vars, hydra.lib.maps.union(tco_var_renames, aliases0.var_renames), aliases0.lambda_vars, aliases0.type_var_subst, aliases0.trusted_type_vars, aliases0.method_codomain, aliases0.thunked_vars), env0.graph)
    @lru_cache(1)
    def stripped() -> hydra.core.Term:
        return hydra.strip.deannotate_and_detype_term(term)
    @lru_cache(1)
    def gathered() -> tuple[frozenlist[hydra.core.Term], hydra.core.Term]:
        return hydra.analysis.gather_applications(stripped())
    @lru_cache(1)
    def gather_args() -> frozenlist[hydra.core.Term]:
        return hydra.lib.pairs.first(gathered())
    @lru_cache(1)
    def gather_fun() -> hydra.core.Term:
        return hydra.lib.pairs.second(gathered())
    @lru_cache(1)
    def stripped_fun() -> hydra.core.Term:
        return hydra.strip.deannotate_and_detype_term(gather_fun())
    @lru_cache(1)
    def is_self_call() -> bool:
        match stripped_fun():
            case hydra.core.TermVariable(value=n):
                return hydra.lib.equality.equal(n, func_name)

            case _:
                return False
    def _hoist_is_self_call_body_1(v1):
        match v1:
            case hydra.core.TermLet(value=lt):
                let_bindings = lt.bindings
                let_body = lt.body
                return hydra.lib.eithers.bind(bindings_to_statements(env(), let_bindings, cx, g), (lambda bind_result: (let_stmts := hydra.lib.pairs.first(bind_result), env_after_let := hydra.lib.pairs.second(bind_result), hydra.lib.eithers.bind(encode_term_t_c_o(env_after_let, func_name, param_names, tco_var_renames, tco_depth, let_body, cx, g), (lambda tco_body_stmts: Right(hydra.lib.lists.concat2(let_stmts, tco_body_stmts)))))[2]))

            case _:
                return (gathered2 := hydra.analysis.gather_applications(term), (args2 := hydra.lib.pairs.first(gathered2), (body2 := hydra.lib.pairs.second(gathered2), hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(args2), 1), (lambda : (arg := hydra.lib.lists.head(args2), (_hoist_arg_body_1 := (lambda v12: (lambda cs: (aliases := env().aliases, tname := cs.type_name, dflt := cs.default, cases_ := cs.cases, hydra.lib.eithers.bind(dom_type_args(aliases, hydra.resolution.nominal_application(tname, ()), cx, g), (lambda dom_args: hydra.lib.eithers.bind(encode_term(env(), arg, cx, g), (lambda j_arg_raw: (depth_suffix := hydra.lib.logic.if_else(hydra.lib.equality.equal(tco_depth, 0), (lambda : ""), (lambda : hydra.lib.literals.show_int32(tco_depth))), match_var_id := hydra.java.utils.java_identifier(hydra.lib.strings.cat(("_tco_match_", hydra.formatting.decapitalize(hydra.names.local_name_of(tname)), depth_suffix))), match_decl := hydra.java.utils.var_declaration_statement(match_var_id, j_arg_raw), j_arg := hydra.java.utils.java_identifier_to_java_expression(match_var_id), hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda field: (field_name := field.name, variant_ref_type := hydra.java.utils.name_to_java_reference_type(aliases, True, dom_args, tname, Just(hydra.formatting.capitalize(field_name.value))), _hoist_variant_ref_type_body_1 := (lambda v13: (lambda lam: with_lambda(env(), lam, (lambda env2: (lambda_param := lam.parameter, branch_body := lam.body, env3 := insert_branch_var(lambda_param, env2), var_id := hydra.java.utils.variable_to_java_identifier(lambda_param), cast_expr := hydra.java.utils.java_cast_expression_to_java_expression(hydra.java.utils.java_cast_expression(variant_ref_type, hydra.java.utils.java_expression_to_java_unary_expression(j_arg))), local_decl := hydra.java.utils.var_declaration_statement(var_id, cast_expr), is_branch_tail_call := hydra.analysis.is_tail_recursive_in_tail_position(func_name, branch_body), hydra.lib.eithers.bind(hydra.lib.logic.if_else(is_branch_tail_call, (lambda : encode_term_t_c_o(env3, func_name, param_names, tco_var_renames, hydra.lib.math.add(tco_depth, 1), branch_body, cx, g)), (lambda : hydra.lib.eithers.bind(analyze_java_function(env3, branch_body, cx, g), (lambda fs: (bindings := fs.bindings, inner_body := fs.body, env4 := fs.environment, hydra.lib.eithers.bind(bindings_to_statements(env4, bindings, cx, g), (lambda bind_result: (binding_stmts := hydra.lib.pairs.first(bind_result), env5 := hydra.lib.pairs.second(bind_result), hydra.lib.eithers.bind(encode_term(env5, inner_body, cx, g), (lambda jret: (return_stmt := cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(hydra.java.utils.java_return_statement(Just(jret)))), Right(hydra.lib.lists.concat2(binding_stmts, (return_stmt,))))[1])))[2])))[3])))), (lambda body_stmts: (rel_expr := hydra.java.utils.java_instance_of(hydra.java.utils.java_unary_expression_to_java_relational_expression(hydra.java.utils.java_expression_to_java_unary_expression(j_arg)), variant_ref_type), cond_expr := hydra.java.utils.java_relational_expression_to_java_expression(rel_expr), block_stmts := hydra.lib.lists.cons(local_decl, body_stmts), if_body := cast(hydra.java.syntax.Statement, hydra.java.syntax.StatementWithoutTrailing(cast(hydra.java.syntax.StatementWithoutTrailingSubstatement, hydra.java.syntax.StatementWithoutTrailingSubstatementBlock(hydra.java.syntax.Block(block_stmts))))), Right(cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(cast(hydra.java.syntax.Statement, hydra.java.syntax.StatementIfThen(hydra.java.syntax.IfThenStatement(cond_expr, if_body)))))))[4])))[7])))(v13.value) if isinstance(v13, hydra.core.TermLambda) else Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("TCO: case branch is not a lambda"))))), _hoist_variant_ref_type_body_1(hydra.strip.deannotate_term(field.term)))[3]), cases_), (lambda if_blocks: hydra.lib.eithers.bind(hydra.lib.maybes.cases(dflt, (lambda : Right((cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(hydra.java.utils.java_return_statement(Just(j_arg)))),))), (lambda d: hydra.lib.eithers.bind(encode_term(env(), d, cx, g), (lambda d_expr: Right((cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(hydra.java.utils.java_return_statement(Just(d_expr)))),)))))), (lambda default_stmt: Right(hydra.lib.lists.concat(((match_decl,), if_blocks, default_stmt))))))))[4])))))[4])(v12.value) if isinstance(v12, hydra.core.TermCases) else hydra.lib.eithers.bind(encode_term(env(), term, cx, g), (lambda expr: Right((cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(hydra.java.utils.java_return_statement(Just(expr)))),))))), _hoist_arg_body_1(hydra.strip.deannotate_and_detype_term(body2)))[1])[1]), (lambda : hydra.lib.eithers.bind(encode_term(env(), term, cx, g), (lambda expr: Right((cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(hydra.java.utils.java_return_statement(Just(expr)))),)))))))[1])[1])[1]
    return hydra.lib.logic.if_else(hydra.lib.logic.and_(is_self_call(), hydra.lib.equality.equal(hydra.lib.lists.length(gather_args()), hydra.lib.lists.length(param_names))), (lambda : (change_pairs := (_hoist_change_pairs_1 := (lambda pair, v1: (lambda n: hydra.lib.equality.equal(n, hydra.lib.pairs.first(pair)))(v1.value) if isinstance(v1, hydra.core.TermVariable) else False), hydra.lib.lists.filter((lambda pair: hydra.lib.logic.not_(_hoist_change_pairs_1(pair, hydra.strip.deannotate_and_detype_term(hydra.lib.pairs.second(pair))))), hydra.lib.lists.zip(param_names, gather_args())))[1], (changed_params := hydra.lib.lists.map((lambda x1: hydra.lib.pairs.first(x1)), change_pairs), hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda pair: encode_term(env(), hydra.lib.pairs.second(pair), cx, g)), change_pairs), (lambda j_changed_args: (assignments := hydra.lib.lists.map((lambda pair: (param_name := hydra.lib.pairs.first(pair), j_arg := hydra.lib.pairs.second(pair), cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(hydra.java.utils.java_assignment_statement(cast(hydra.java.syntax.LeftHandSide, hydra.java.syntax.LeftHandSideExpressionName(hydra.java.utils.java_identifier_to_java_expression_name(hydra.java.utils.variable_to_java_identifier(param_name)))), j_arg))))[2]), hydra.lib.lists.zip(changed_params, j_changed_args)), continue_stmt := cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(cast(hydra.java.syntax.Statement, hydra.java.syntax.StatementWithoutTrailing(cast(hydra.java.syntax.StatementWithoutTrailingSubstatement, hydra.java.syntax.StatementWithoutTrailingSubstatementContinue(hydra.java.syntax.ContinueStatement(Nothing()))))))), Right(hydra.lib.lists.concat2(assignments, (continue_stmt,))))[2])))[1])[1]), (lambda : _hoist_is_self_call_body_1(stripped())))

def peel_domains_and_cod(n: int, t: hydra.core.Type):
    def _hoist_hydra_java_coder_peel_domains_and_cod_1(n, t, v1):
        match v1:
            case hydra.core.TypeFunction(value=ft):
                @lru_cache(1)
                def rest() -> tuple[frozenlist[hydra.core.Type], hydra.core.Type]:
                    return peel_domains_and_cod(hydra.lib.math.sub(n, 1), ft.codomain)
                return (hydra.lib.lists.cons(ft.domain, hydra.lib.pairs.first(rest())), hydra.lib.pairs.second(rest()))

            case _:
                return ((), t)
    return hydra.lib.logic.if_else(hydra.lib.equality.lte(n, 0), (lambda : ((), t)), (lambda : _hoist_hydra_java_coder_peel_domains_and_cod_1(n, t, hydra.strip.deannotate_type(t))))

def flatten_apps(t: hydra.core.Term, acc: frozenlist[hydra.core.Term]) -> tuple[frozenlist[hydra.core.Term], hydra.core.Term]:
    while True:
        match hydra.strip.deannotate_term(t):
            case hydra.core.TermApplication(value=app):
                t = app.function
                acc = hydra.lib.lists.cons(app.argument, acc)
                continue

            case _:
                return (acc, t)

def rebuild_apps(f: hydra.core.Term, args: frozenlist[hydra.core.Term], f_type: hydra.core.Type):
    def _hoist_hydra_java_coder_rebuild_apps_1(args, f, v1):
        match v1:
            case hydra.core.TypeFunction(value=ft):
                @lru_cache(1)
                def arg() -> hydra.core.Term:
                    return hydra.lib.lists.head(args)
                @lru_cache(1)
                def rest() -> frozenlist[hydra.core.Term]:
                    return hydra.lib.lists.tail(args)
                remaining_type = ft.codomain
                @lru_cache(1)
                def app() -> hydra.core.Term:
                    return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(f, arg())))
                @lru_cache(1)
                def annotated_app() -> hydra.core.Term:
                    return hydra.annotations.set_term_annotation(hydra.constants.key_type, Just(hydra.encode.core.type(remaining_type)), app())
                return rebuild_apps(annotated_app(), rest(), remaining_type)

            case _:
                return hydra.lib.lists.foldl((lambda acc, a: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(acc, a)))), f, args)
    return hydra.lib.logic.if_else(hydra.lib.lists.null(args), (lambda : f), (lambda : _hoist_hydra_java_coder_rebuild_apps_1(args, f, hydra.strip.deannotate_type(f_type))))

def propagate_types_in_app_chain(fixed_cod: hydra.core.Type, result_type: hydra.core.Type, t: hydra.core.Term):
    @lru_cache(1)
    def flattened() -> tuple[frozenlist[hydra.core.Term], hydra.core.Term]:
        return flatten_apps(t, ())
    @lru_cache(1)
    def args() -> frozenlist[hydra.core.Term]:
        return hydra.lib.pairs.first(flattened())
    @lru_cache(1)
    def fun() -> hydra.core.Term:
        return hydra.lib.pairs.second(flattened())
    @lru_cache(1)
    def lambda_doms_result() -> tuple[frozenlist[hydra.core.Type], hydra.core.Term]:
        return collect_lambda_domains(fun())
    @lru_cache(1)
    def lambda_doms() -> frozenlist[hydra.core.Type]:
        return hydra.lib.pairs.first(lambda_doms_result())
    @lru_cache(1)
    def n_args() -> int:
        return hydra.lib.lists.length(args())
    @lru_cache(1)
    def n_lambda_doms() -> int:
        return hydra.lib.lists.length(lambda_doms())
    def _hoist_n_lambda_doms_body_1(v1):
        match v1:
            case hydra.core.TermApplication(value=app):
                lhs = app.function
                rhs = app.argument
                @lru_cache(1)
                def annotated_lhs():
                    def _hoist_annotated_lhs_1(v12):
                        match v12:
                            case hydra.core.TermCases(value=cs):
                                @lru_cache(1)
                                def dom() -> hydra.core.Type:
                                    return hydra.resolution.nominal_application(cs.type_name, ())
                                @lru_cache(1)
                                def ft() -> hydra.core.Type:
                                    return cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(dom(), fixed_cod)))
                                return hydra.annotations.set_term_annotation(hydra.constants.key_type, Just(hydra.encode.core.type(ft())), lhs)

                            case _:
                                return lhs
                    return _hoist_annotated_lhs_1(hydra.strip.deannotate_term(lhs))
                return hydra.annotations.set_term_annotation(hydra.constants.key_type, Just(hydra.encode.core.type(result_type)), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(annotated_lhs(), rhs))))

            case _:
                return hydra.annotations.set_term_annotation(hydra.constants.key_type, Just(hydra.encode.core.type(result_type)), t)
    return hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.gt(n_lambda_doms(), 0), hydra.lib.equality.gt(n_args(), 0)), (lambda : (body_ret_type := hydra.lib.pairs.second(peel_domains_and_cod(hydra.lib.math.sub(n_lambda_doms(), n_args()), result_type)), (fun_type := hydra.lib.lists.foldl((lambda c, d: cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(d, c)))), body_ret_type, hydra.lib.lists.reverse(lambda_doms())), (annotated_fun := hydra.annotations.set_term_annotation(hydra.constants.key_type, Just(hydra.encode.core.type(fun_type)), fun()), rebuild_apps(annotated_fun, args(), fun_type))[1])[1])[1]), (lambda : _hoist_n_lambda_doms_body_1(hydra.strip.deannotate_term(t))))

def encode_term_definition(env: hydra.java.environment.JavaEnvironment, tdef: hydra.packaging.TermDefinition, cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.InterfaceMemberDeclaration]:
    name = tdef.name
    term0 = tdef.term
    @lru_cache(1)
    def ts() -> hydra.core.TypeScheme:
        return hydra.lib.maybes.maybe((lambda : hydra.core.TypeScheme((), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Unit"))), Nothing())), (lambda x: x), tdef.type)
    @lru_cache(1)
    def term() -> hydra.core.Term:
        return hydra.variables.unshadow_variables(term0)
    return hydra.lib.eithers.bind(analyze_java_function(env, term(), cx, g), (lambda fs: (scheme_vars := hydra.lib.lists.filter((lambda v: is_simple_name(v)), ts().variables), term_vars := fs.type_params, scheme_type_vars := collect_type_vars(ts().type), used_scheme_vars := hydra.lib.lists.filter((lambda v: hydra.lib.sets.member(v, scheme_type_vars)), scheme_vars), tparams := hydra.lib.logic.if_else(hydra.lib.lists.null(used_scheme_vars), (lambda : term_vars), (lambda : used_scheme_vars)), params := fs.params, bindings := fs.bindings, body := fs.body, doms := fs.domains, env2 := fs.environment, scheme_type := ts().type, num_params := hydra.lib.lists.length(params), peel_result := peel_domains_and_cod(num_params, scheme_type), scheme_doms := hydra.lib.pairs.first(peel_result), cod := hydra.lib.pairs.second(peel_result), scheme_var_set := hydra.lib.sets.from_list(tparams), hydra.lib.eithers.bind(hydra.lib.logic.if_else(hydra.lib.lists.null(tparams), (lambda : Right(hydra.lib.maps.empty())), (lambda : build_subst_from_annotations(scheme_var_set, term(), cx, g))), (lambda type_var_subst: (overgen_subst := detect_accumulator_unification(scheme_doms, cod, tparams), overgen_var_subst := hydra.lib.maps.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda entry: (k := hydra.lib.pairs.first(entry), v := hydra.lib.pairs.second(entry), _hoist_v_body_1 := (lambda v1: (lambda n: Just((k, n)))(v1.value) if isinstance(v1, hydra.core.TypeVariable) else Nothing()), _hoist_v_body_1(v))[3]), hydra.lib.maps.to_list(overgen_subst)))), fixed_cod := hydra.lib.logic.if_else(hydra.lib.maps.null(overgen_subst), (lambda : cod), (lambda : substitute_type_vars_with_types(overgen_subst, cod))), fixed_doms := hydra.lib.logic.if_else(hydra.lib.maps.null(overgen_subst), (lambda : scheme_doms), (lambda : hydra.lib.lists.map((lambda d: substitute_type_vars_with_types(overgen_subst, d)), scheme_doms))), fixed_tparams := hydra.lib.logic.if_else(hydra.lib.maps.null(overgen_subst), (lambda : tparams), (lambda : hydra.lib.lists.filter((lambda v: hydra.lib.logic.not_(hydra.lib.maps.member(v, overgen_subst))), tparams))), constraints := hydra.lib.maybes.from_maybe((lambda : hydra.lib.maps.empty()), ts().constraints), jparams := hydra.lib.lists.map((lambda v: hydra.java.utils.java_type_parameter(hydra.formatting.capitalize(v.value))), fixed_tparams), aliases2base := env2.aliases, trusted_vars := hydra.lib.sets.unions(hydra.lib.lists.map((lambda d: collect_type_vars(d)), hydra.lib.lists.concat2(fixed_doms, (fixed_cod,)))), fixed_scheme_var_set := hydra.lib.sets.from_list(fixed_tparams), aliases2 := hydra.java.environment.Aliases(aliases2base.current_namespace, aliases2base.packages, aliases2base.branch_vars, aliases2base.recursive_vars, fixed_scheme_var_set, aliases2base.polymorphic_locals, aliases2base.in_scope_java_vars, aliases2base.var_renames, hydra.lib.sets.union(aliases2base.lambda_vars, hydra.lib.sets.from_list(params)), hydra.lib.maps.union(overgen_var_subst, type_var_subst), hydra.lib.sets.intersection(trusted_vars, fixed_scheme_var_set), Just(fixed_cod), aliases2base.thunked_vars), env2_with_type_params := hydra.java.environment.JavaEnvironment(aliases2, env2.graph), hydra.lib.eithers.bind(bindings_to_statements(env2_with_type_params, bindings, cx, g), (lambda bind_result: (binding_stmts := hydra.lib.pairs.first(bind_result), env3 := hydra.lib.pairs.second(bind_result), hydra.lib.eithers.bind(hydra.lib.logic.if_else(hydra.lib.maps.null(overgen_subst), (lambda : Right(body)), (lambda : apply_overgen_subst_to_term_annotations(overgen_subst, body, cx, g))), (lambda body_: (annotated_body := propagate_types_in_app_chain(fixed_cod, fixed_cod, body_), hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda pair: hydra.lib.eithers.bind(encode_type(aliases2, hydra.lib.sets.empty(), hydra.lib.pairs.first(pair), cx, g), (lambda jdom: Right(hydra.java.utils.java_type_to_java_formal_parameter(jdom, hydra.lib.pairs.second(pair)))))), hydra.lib.lists.zip(fixed_doms, params)), (lambda jformal_params: hydra.lib.eithers.bind(encode_type(aliases2, hydra.lib.sets.empty(), fixed_cod, cx, g), (lambda jcod: (result := hydra.java.utils.java_type_to_java_result(jcod), mods := (cast(hydra.java.syntax.InterfaceMethodModifier, hydra.java.syntax.InterfaceMethodModifierStatic()),), jname := hydra.java.utils.sanitize_java_name(hydra.formatting.decapitalize(hydra.names.local_name_of(name))), is_t_c_o := False, hydra.lib.eithers.bind(hydra.lib.logic.if_else(is_t_c_o, (lambda : (tco_suffix := "_tco", (snapshot_names := hydra.lib.lists.map((lambda p: hydra.core.Name(hydra.lib.strings.cat2(p.value, tco_suffix))), params), (tco_var_renames := hydra.lib.maps.from_list(hydra.lib.lists.zip(params, snapshot_names)), (snapshot_decls := hydra.lib.lists.map((lambda pair: hydra.java.utils.final_var_declaration_statement(hydra.java.utils.variable_to_java_identifier(hydra.lib.pairs.second(pair)), hydra.java.utils.java_identifier_to_java_expression(hydra.java.utils.variable_to_java_identifier(hydra.lib.pairs.first(pair))))), hydra.lib.lists.zip(params, snapshot_names)), (tco_body := hydra.lib.logic.if_else(hydra.lib.lists.null(bindings), (lambda : annotated_body), (lambda : cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(bindings, annotated_body))))), hydra.lib.eithers.bind(encode_term_t_c_o(env2_with_type_params, name, params, tco_var_renames, 0, tco_body, cx, g), (lambda tco_stmts: (while_body_stmts := hydra.lib.lists.concat2(snapshot_decls, tco_stmts), while_body_block := cast(hydra.java.syntax.Statement, hydra.java.syntax.StatementWithoutTrailing(cast(hydra.java.syntax.StatementWithoutTrailingSubstatement, hydra.java.syntax.StatementWithoutTrailingSubstatementBlock(hydra.java.syntax.Block(while_body_stmts))))), no_cond := Nothing(), while_stmt := cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(cast(hydra.java.syntax.Statement, hydra.java.syntax.StatementWhile(hydra.java.syntax.WhileStatement(no_cond, while_body_block))))), Right((while_stmt,)))[4])))[1])[1])[1])[1])[1]), (lambda : hydra.lib.eithers.bind(encode_term(env3, annotated_body, cx, g), (lambda jbody: (return_st := cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(hydra.java.utils.java_return_statement(Just(jbody)))), Right(hydra.lib.lists.concat2(binding_stmts, (return_st,))))[1])))), (lambda method_body: Right(hydra.java.utils.interface_method_declaration(mods, jparams, jname, jformal_params, result, Just(method_body))))))[4])))))[1])))[2])))[12])))[16]))

def is_serializable_java_type(typ: hydra.core.Type) -> bool:
    return hydra.predicates.is_nominal_type(typ)

def to_class_decl(is_inner: bool, is_ser: bool, aliases: hydra.java.environment.Aliases, tparams: frozenlist[hydra.java.syntax.TypeParameter], el_name: hydra.core.Name, t: hydra.core.Type, cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.java.syntax.ClassDeclaration]:
    while True:
        def wrap(t_: hydra.core.Type) -> Either[hydra.errors.Error, hydra.java.syntax.ClassDeclaration]:
            return declaration_for_record_type(is_inner, is_ser, aliases, tparams, el_name, (hydra.core.FieldType(hydra.core.Name("value"), hydra.strip.deannotate_type(t_)),), cx, g)
        match hydra.strip.deannotate_type(t):
            case hydra.core.TypeRecord(value=rt):
                return declaration_for_record_type(is_inner, is_ser, aliases, tparams, el_name, rt, cx, g)

            case hydra.core.TypeUnion(value=rt2):
                return declaration_for_union_type(is_ser, aliases, tparams, el_name, rt2, cx, g)

            case hydra.core.TypeForall(value=fa):
                return (v := fa.parameter, (body := fa.body, (param := hydra.java.utils.java_type_parameter(hydra.formatting.capitalize(v.value)), to_class_decl(False, is_ser, aliases, hydra.lib.lists.concat2(tparams, (param,)), el_name, body, cx, g))[1])[1])[1]

            case hydra.core.TypeWrap(value=wt):
                return declaration_for_record_type(is_inner, is_ser, aliases, tparams, el_name, (hydra.core.FieldType(hydra.core.Name("value"), wt),), cx, g)

            case _:
                return wrap(t)

def encode_type_definition(pkg: hydra.java.syntax.PackageDeclaration, aliases: hydra.java.environment.Aliases, tdef: hydra.packaging.TypeDefinition, cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.errors.Error, tuple[hydra.core.Name, hydra.java.syntax.CompilationUnit]]:
    name = tdef.name
    typ = tdef.type.type
    @lru_cache(1)
    def serializable() -> bool:
        return is_serializable_java_type(typ)
    @lru_cache(1)
    def imports() -> frozenlist[hydra.java.syntax.ImportDeclaration]:
        return hydra.lib.logic.if_else(serializable(), (lambda : (cast(hydra.java.syntax.ImportDeclaration, hydra.java.syntax.ImportDeclarationSingleType(hydra.java.syntax.SingleTypeImportDeclaration(hydra.java.utils.java_type_name(hydra.java.syntax.Identifier("java.io.Serializable"))))),)), (lambda : ()))
    return hydra.lib.eithers.bind(to_class_decl(False, serializable(), aliases, (), name, typ, cx, g), (lambda decl: hydra.lib.eithers.bind(hydra.annotations.get_type_description(cx, g, typ), (lambda comment: (tdecl := hydra.java.syntax.TypeDeclarationWithComments(cast(hydra.java.syntax.TypeDeclaration, hydra.java.syntax.TypeDeclarationClass(decl)), comment), Right((name, cast(hydra.java.syntax.CompilationUnit, hydra.java.syntax.CompilationUnitOrdinary(hydra.java.syntax.OrdinaryCompilationUnit(Just(pkg), imports(), (tdecl,)))))))[1]))))

def encode_definitions(mod: hydra.packaging.Module, defs: frozenlist[hydra.packaging.Definition], cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.errors.Error, FrozenDict[hydra.core.Name, hydra.java.syntax.CompilationUnit]]:
    @lru_cache(1)
    def aliases() -> hydra.java.environment.Aliases:
        return hydra.java.utils.import_aliases_for_module(mod)
    env = hydra.java.environment.JavaEnvironment(aliases(), g)
    @lru_cache(1)
    def pkg() -> hydra.java.syntax.PackageDeclaration:
        return hydra.java.utils.java_package_declaration(mod.namespace)
    @lru_cache(1)
    def partitioned() -> tuple[frozenlist[hydra.packaging.TypeDefinition], frozenlist[hydra.packaging.TermDefinition]]:
        return hydra.environment.partition_definitions(defs)
    @lru_cache(1)
    def type_defs() -> frozenlist[hydra.packaging.TypeDefinition]:
        return hydra.lib.pairs.first(partitioned())
    @lru_cache(1)
    def term_defs() -> frozenlist[hydra.packaging.TermDefinition]:
        return hydra.lib.pairs.second(partitioned())
    @lru_cache(1)
    def non_typedef_defs() -> frozenlist[hydra.packaging.TypeDefinition]:
        return hydra.lib.lists.filter((lambda td: (typ := td.type.type, is_serializable_java_type(typ))[1]), type_defs())
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda td: encode_type_definition(pkg(), aliases(), td, cx, g)), non_typedef_defs()), (lambda type_units: hydra.lib.eithers.bind(hydra.lib.logic.if_else(hydra.lib.lists.null(term_defs()), (lambda : Right(())), (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda td: encode_term_definition(env, td, cx, g)), term_defs()), (lambda data_members: Right((construct_elements_interface(mod, data_members),)))))), (lambda term_units: Right(hydra.lib.maps.from_list(hydra.lib.lists.concat2(type_units, term_units)))))))

def get_function_type(ann: FrozenDict[hydra.core.Name, hydra.core.Term], cx: T0, g: hydra.graph.Graph):
    def _hoist_hydra_java_coder_get_function_type_1(t, v1):
        match v1:
            case hydra.core.TypeFunction(value=ft):
                return Right(ft)

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("expected function type, got: ", hydra.show.core.type(t))))))
    return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _de: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_de.value)))), (lambda _a: _a), hydra.annotations.get_type(g, ann)), (lambda mt: hydra.lib.maybes.cases(mt, (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("type annotation is required for function and elimination terms in Java"))))), (lambda t: _hoist_hydra_java_coder_get_function_type_1(t, t)))))

def get_codomain(ann: FrozenDict[hydra.core.Name, hydra.core.Term], cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.core.Type]:
    return hydra.lib.eithers.map((lambda ft: ft.codomain), get_function_type(ann, cx, g))

java8_features = hydra.java.environment.JavaFeatures(False)

@lru_cache(1)
def java_comparable_ref_type() -> hydra.java.syntax.ReferenceType:
    return cast(hydra.java.syntax.ReferenceType, hydra.java.syntax.ReferenceTypeClassOrInterface(cast(hydra.java.syntax.ClassOrInterfaceType, hydra.java.syntax.ClassOrInterfaceTypeClass(hydra.java.syntax.ClassType((), cast(hydra.java.syntax.ClassTypeQualifier, hydra.java.syntax.ClassTypeQualifierNone()), hydra.java.utils.java_type_identifier("Comparable"), ())))))

def java_identifier_to_string(id: hydra.java.syntax.Identifier) -> str:
    return id.value

def java_type_arguments_for_named_type(tname: hydra.core.Name, cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, frozenlist[hydra.java.syntax.TypeArgument]]:
    return hydra.lib.eithers.bind(hydra.resolution.require_type(cx, g, tname), (lambda typ: Right(hydra.lib.lists.map((lambda tp_: hydra.java.utils.type_parameter_to_type_argument(tp_)), java_type_parameters_for_type(typ)))))

def module_to_java(mod: hydra.packaging.Module, defs: frozenlist[hydra.packaging.Definition], cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.errors.Error, FrozenDict[str, str]]:
    return hydra.lib.eithers.bind(encode_definitions(mod, defs, cx, g), (lambda units: Right(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda entry: (name := hydra.lib.pairs.first(entry), unit := hydra.lib.pairs.second(entry), (binding_name_to_file_path(name), hydra.serialization.print_expr(hydra.serialization.parenthesize(hydra.java.serde.write_compilation_unit(unit)))))[2]), hydra.lib.maps.to_list(units))))))

def split_constant_initializer_split_var(mods: frozenlist[hydra.java.syntax.ConstantModifier], utype: hydra.java.syntax.UnannType, vd: hydra.java.syntax.VariableDeclarator):
    vid = vd.id
    m_init = vd.initializer
    def _hoist_vid_body_1(v1):
        match v1:
            case hydra.java.syntax.VariableInitializerExpression(value=expr):
                @lru_cache(1)
                def var_name() -> str:
                    return java_identifier_to_string(vid.identifier)
                @lru_cache(1)
                def helper_name() -> str:
                    return hydra.lib.strings.cat2("_init_", var_name())
                @lru_cache(1)
                def call_expr() -> hydra.java.syntax.Expression:
                    return hydra.java.utils.java_method_invocation_to_java_expression(hydra.java.utils.method_invocation(Nothing(), hydra.java.syntax.Identifier(helper_name()), ()))
                @lru_cache(1)
                def field() -> hydra.java.syntax.InterfaceMemberDeclaration:
                    return cast(hydra.java.syntax.InterfaceMemberDeclaration, hydra.java.syntax.InterfaceMemberDeclarationConstant(hydra.java.syntax.ConstantDeclaration(mods, utype, (hydra.java.syntax.VariableDeclarator(vid, Just(cast(hydra.java.syntax.VariableInitializer, hydra.java.syntax.VariableInitializerExpression(call_expr())))),))))
                @lru_cache(1)
                def return_st() -> hydra.java.syntax.BlockStatement:
                    return cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(hydra.java.utils.java_return_statement(Just(expr))))
                @lru_cache(1)
                def result_type() -> hydra.java.syntax.Result:
                    return cast(hydra.java.syntax.Result, hydra.java.syntax.ResultType(utype))
                @lru_cache(1)
                def helper() -> hydra.java.syntax.InterfaceMemberDeclaration:
                    return hydra.java.utils.interface_method_declaration((cast(hydra.java.syntax.InterfaceMethodModifier, hydra.java.syntax.InterfaceMethodModifierStatic()), cast(hydra.java.syntax.InterfaceMethodModifier, hydra.java.syntax.InterfaceMethodModifierPrivate())), (), helper_name(), (), result_type(), Just((return_st(),)))
                return (field(), helper())

            case _:
                return (cast(hydra.java.syntax.InterfaceMemberDeclaration, hydra.java.syntax.InterfaceMemberDeclarationConstant(hydra.java.syntax.ConstantDeclaration(mods, utype, (vd,)))),)
    return hydra.lib.maybes.cases(m_init, (lambda : (cast(hydra.java.syntax.InterfaceMemberDeclaration, hydra.java.syntax.InterfaceMemberDeclarationConstant(hydra.java.syntax.ConstantDeclaration(mods, utype, (vd,)))),)), (lambda init_: _hoist_vid_body_1(init_)))

def split_constant_initializer(member: hydra.java.syntax.InterfaceMemberDeclaration) -> frozenlist[hydra.java.syntax.InterfaceMemberDeclaration]:
    match member:
        case hydra.java.syntax.InterfaceMemberDeclarationConstant(value=cd):
            return hydra.lib.lists.bind(cd.variables, (lambda v1: split_constant_initializer_split_var(cd.modifiers, cd.type, v1)))

        case _:
            return (member,)
