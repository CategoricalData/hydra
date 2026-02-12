# Note: this is an automatically generated file. Do not edit.

r"""Java code generator: converts Hydra modules to Java source code."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.adapt.utils
import hydra.annotations
import hydra.arity
import hydra.coder_utils
import hydra.constants
import hydra.core
import hydra.decode.core
import hydra.encode.core
import hydra.ext.java.helpers
import hydra.ext.java.language
import hydra.ext.java.names
import hydra.ext.java.serde
import hydra.ext.java.syntax
import hydra.ext.java.utils
import hydra.formatting
import hydra.graph
import hydra.inference
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.module
import hydra.monads
import hydra.names
import hydra.rewriting
import hydra.schemas
import hydra.serialization
import hydra.show.core
import hydra.sorting
import hydra.typing
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def add_comment(decl: hydra.ext.java.syntax.ClassBodyDeclaration, field: hydra.core.FieldType) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments]:
    return hydra.lib.flows.map((lambda c: hydra.ext.java.syntax.ClassBodyDeclarationWithComments(decl, c)), hydra.coder_utils.comments_from_field_type(field))

def java_env_get_t_c(env: hydra.ext.java.helpers.JavaEnvironment) -> hydra.typing.TypeContext:
    return env.type_context

def java_env_set_t_c(tc: hydra.typing.TypeContext, env: hydra.ext.java.helpers.JavaEnvironment) -> hydra.ext.java.helpers.JavaEnvironment:
    return hydra.ext.java.helpers.JavaEnvironment(env.aliases, tc)

def analyze_java_function(v1: hydra.ext.java.helpers.JavaEnvironment, v2: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.typing.FunctionStructure[hydra.ext.java.helpers.JavaEnvironment]]:
    return hydra.coder_utils.analyze_function_term((lambda x1: java_env_get_t_c(x1)), (lambda x1, x2: java_env_set_t_c(x1, x2)), v1, v2)

def analyze_java_function_no_infer(v1: hydra.ext.java.helpers.JavaEnvironment, v2: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.typing.FunctionStructure[hydra.ext.java.helpers.JavaEnvironment]]:
    return hydra.coder_utils.analyze_function_term_no_infer((lambda x1: java_env_get_t_c(x1)), (lambda x1, x2: java_env_set_t_c(x1, x2)), v1, v2)

def extract_arg_type(_lhs: T0, typ: hydra.core.Type) -> hydra.core.Type:
    def _hoist_hydra_ext_java_coder_extract_arg_type_1(at1: hydra.core.ApplicationType, typ: hydra.core.Type, v1: hydra.core.Type) -> hydra.core.Type:
        match v1:
            case hydra.core.TypeApplication():
                return at1.argument
            
            case _:
                return typ
    match typ:
        case hydra.core.TypeApplication(value=at1):
            return _hoist_hydra_ext_java_coder_extract_arg_type_1(at1, typ, at1.function)
        
        case _:
            return typ

def annotate_body_with_cod(typ: hydra.core.Type, term: hydra.core.Term) -> hydra.core.Term:
    def set_ann(t: hydra.core.Term) -> hydra.core.Term:
        return hydra.annotations.set_term_annotation(hydra.constants.key_type, Just(hydra.encode.core.type(typ)), t)
    match hydra.rewriting.deannotate_term(term):
        case hydra.core.TermTypeApplication():
            return set_ann(term)
        
        case hydra.core.TermApplication(value=app):
            @lru_cache(1)
            def lhs() -> hydra.core.Term:
                return app.function
            @lru_cache(1)
            def rhs() -> hydra.core.Term:
                return app.argument
            @lru_cache(1)
            def annotated_rhs() -> hydra.core.Term:
                match hydra.rewriting.deannotate_term(rhs()):
                    case hydra.core.TermTypeApplication():
                        return annotate_body_with_cod(extract_arg_type(lhs(), typ), rhs())
                    
                    case _:
                        return rhs()
            return set_ann(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(lhs(), annotated_rhs()))))
        
        case _:
            return set_ann(term)

def collect_type_vars_go(t: hydra.core.Type) -> frozenset[hydra.core.Name]:
    match t:
        case hydra.core.TypeVariable(value=name):
            return hydra.lib.sets.singleton(name)
        
        case hydra.core.TypeFunction(value=ft):
            return hydra.lib.sets.union(collect_type_vars_go(hydra.rewriting.deannotate_type(ft.domain)), collect_type_vars_go(hydra.rewriting.deannotate_type(ft.codomain)))
        
        case hydra.core.TypeApplication(value=at):
            return hydra.lib.sets.union(collect_type_vars_go(hydra.rewriting.deannotate_type(at.function)), collect_type_vars_go(hydra.rewriting.deannotate_type(at.argument)))
        
        case hydra.core.TypeList(value=inner):
            return collect_type_vars_go(hydra.rewriting.deannotate_type(inner))
        
        case hydra.core.TypeSet(value=inner2):
            return collect_type_vars_go(hydra.rewriting.deannotate_type(inner2))
        
        case hydra.core.TypeMaybe(value=inner3):
            return collect_type_vars_go(hydra.rewriting.deannotate_type(inner3))
        
        case hydra.core.TypeMap(value=mt):
            return hydra.lib.sets.union(collect_type_vars_go(hydra.rewriting.deannotate_type(mt.keys)), collect_type_vars_go(hydra.rewriting.deannotate_type(mt.values)))
        
        case hydra.core.TypePair(value=pt):
            return hydra.lib.sets.union(collect_type_vars_go(hydra.rewriting.deannotate_type(pt.first)), collect_type_vars_go(hydra.rewriting.deannotate_type(pt.second)))
        
        case hydra.core.TypeEither(value=et):
            return hydra.lib.sets.union(collect_type_vars_go(hydra.rewriting.deannotate_type(et.left)), collect_type_vars_go(hydra.rewriting.deannotate_type(et.right)))
        
        case hydra.core.TypeForall(value=ft2):
            return collect_type_vars_go(hydra.rewriting.deannotate_type(ft2.body))
        
        case _:
            return hydra.lib.sets.empty()

def collect_type_vars(typ: hydra.core.Type) -> frozenset[hydra.core.Name]:
    return collect_type_vars_go(hydra.rewriting.deannotate_type(typ))

def apply_subst_full(s: FrozenDict[hydra.core.Name, hydra.core.Type], t: hydra.core.Type) -> hydra.core.Type:
    match hydra.rewriting.deannotate_type(t):
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

def peel_expected_types(subst: FrozenDict[hydra.core.Name, hydra.core.Type], n: int, t: hydra.core.Type) -> frozenlist[hydra.core.Type]:
    def _hoist_hydra_ext_java_coder_peel_expected_types_1(n: int, subst: FrozenDict[hydra.core.Name, hydra.core.Type], v1: hydra.core.Type) -> frozenlist[hydra.core.Type]:
        match v1:
            case hydra.core.TypeFunction(value=ft):
                return hydra.lib.lists.cons(apply_subst_full(subst, ft.domain), peel_expected_types(subst, hydra.lib.math.sub(n, 1), ft.codomain))
            
            case _:
                return ()
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(n, 0), (lambda : ()), (lambda : _hoist_hydra_ext_java_coder_peel_expected_types_1(n, subst, hydra.rewriting.deannotate_type(t))))

def propagate_type_rebuild_let(t: hydra.core.Term, bindings: frozenlist[hydra.core.Binding], new_body: hydra.core.Term) -> hydra.core.Term:
    match t:
        case hydra.core.TermAnnotated(value=at):
            return cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(propagate_type_rebuild_let(at.body, bindings, new_body), at.annotation)))
        
        case hydra.core.TermLet():
            return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(bindings, new_body)))
        
        case _:
            return t

def propagate_type(typ: hydra.core.Type, term: hydra.core.Term) -> hydra.core.Term:
    def set_type_ann(t: hydra.core.Term) -> hydra.core.Term:
        return hydra.annotations.set_term_annotation(hydra.constants.key_type, Just(hydra.encode.core.type(typ)), t)
    def _hoist_body_1(v1: hydra.core.Function) -> hydra.core.Term:
        match v1:
            case hydra.core.FunctionLambda():
                @lru_cache(1)
                def annotated() -> hydra.core.Term:
                    return set_type_ann(term)
                match hydra.rewriting.deannotate_type(typ):
                    case hydra.core.TypeFunction(value=ft):
                        return propagate_type_propagate_into_lambda(ft.codomain, annotated())
                    
                    case _:
                        return annotated()
            
            case _:
                return set_type_ann(term)
    match hydra.rewriting.deannotate_term(term):
        case hydra.core.TermFunction(value=f):
            return _hoist_body_1(f)
        
        case hydra.core.TermLet(value=lt):
            return set_type_ann(propagate_type_rebuild_let(term, lt.bindings, propagate_type(typ, lt.body)))
        
        case _:
            return set_type_ann(term)

def propagate_type_propagate_into_lambda(cod: hydra.core.Type, t: hydra.core.Term) -> hydra.core.Term:
    def _hoist_hydra_ext_java_coder_propagate_type_propagate_into_lambda_1(cod: hydra.core.Type, t: hydra.core.Term, v1: hydra.core.Function) -> hydra.core.Term:
        match v1:
            case hydra.core.FunctionLambda(value=lam):
                return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(lam.parameter, lam.domain, propagate_type(cod, lam.body))))))
            
            case _:
                return t
    match t:
        case hydra.core.TermAnnotated(value=at):
            return cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(propagate_type_propagate_into_lambda(cod, at.body), at.annotation)))
        
        case hydra.core.TermFunction(value=f):
            return _hoist_hydra_ext_java_coder_propagate_type_propagate_into_lambda_1(cod, t, f)
        
        case _:
            return t

def annotate_lambda_args(cname: hydra.core.Name, t_apps: frozenlist[hydra.core.Type], arg_terms: frozenlist[hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, frozenlist[hydra.core.Term]]:
    return hydra.lib.logic.if_else(hydra.lib.lists.null(t_apps), (lambda : hydra.lib.flows.pure(arg_terms)), (lambda : hydra.lib.flows.bind(hydra.lib.flows.bind(hydra.lexical.dereference_element(cname), (lambda mel: hydra.lib.maybes.cases(mel, hydra.lib.flows.bind(hydra.monads.get_state(), (lambda g: hydra.lib.flows.pure(hydra.lib.maybes.map((lambda prim: prim.type), hydra.lib.maps.lookup(cname, g.primitives))))), (lambda el: hydra.lib.flows.pure(el.type))))), (lambda mts: hydra.lib.maybes.cases(mts, hydra.lib.flows.pure(arg_terms), (lambda ts: (scheme_type := ts.type, scheme_type_vars := collect_type_vars(scheme_type), scheme_vars := hydra.lib.lists.filter((lambda v: hydra.lib.sets.member(v, scheme_type_vars)), ts.variables), hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.lib.lists.null(scheme_vars), hydra.lib.logic.not_(hydra.lib.equality.equal(hydra.lib.lists.length(scheme_vars), hydra.lib.lists.length(t_apps)))), (lambda : hydra.lib.flows.pure(arg_terms)), (lambda : (subst := hydra.lib.maps.from_list(hydra.lib.lists.zip(scheme_vars, t_apps)), (expected_types := peel_expected_types(subst, hydra.lib.lists.length(arg_terms), scheme_type), hydra.lib.flows.pure(hydra.lib.lists.zip_with((lambda arg, m_expected: propagate_type(m_expected, arg)), arg_terms, hydra.lib.lists.concat2(expected_types, hydra.lib.lists.replicate(hydra.lib.lists.length(arg_terms), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("unused"))))))))[1])[1])))[3]))))))

def encode_literal_type_simple(n: str) -> hydra.compute.Flow[T0, hydra.ext.java.syntax.Type]:
    return hydra.lib.flows.pure(hydra.ext.java.utils.java_ref_type((), Nothing(), n))

def encode_literal_type(lt: hydra.core.LiteralType) -> hydra.compute.Flow[T0, hydra.ext.java.syntax.Type]:
    def _hoist_hydra_ext_java_coder_encode_literal_type_1(v1: hydra.core.FloatType) -> hydra.compute.Flow[T1, hydra.ext.java.syntax.Type]:
        match v1:
            case hydra.core.FloatType.BIGFLOAT:
                return hydra.lib.flows.pure(hydra.ext.java.utils.java_ref_type((), Just(hydra.ext.java.names.java_package_name(("java", "math"))), "BigDecimal"))
            
            case hydra.core.FloatType.FLOAT32:
                return encode_literal_type_simple("Float")
            
            case hydra.core.FloatType.FLOAT64:
                return encode_literal_type_simple("Double")
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_hydra_ext_java_coder_encode_literal_type_2(v1: hydra.core.IntegerType) -> hydra.compute.Flow[T1, hydra.ext.java.syntax.Type]:
        match v1:
            case hydra.core.IntegerType.BIGINT:
                return hydra.lib.flows.pure(hydra.ext.java.utils.java_ref_type((), Just(hydra.ext.java.names.java_package_name(("java", "math"))), "BigInteger"))
            
            case hydra.core.IntegerType.INT8:
                return encode_literal_type_simple("Byte")
            
            case hydra.core.IntegerType.INT16:
                return encode_literal_type_simple("Short")
            
            case hydra.core.IntegerType.INT32:
                return encode_literal_type_simple("Integer")
            
            case hydra.core.IntegerType.INT64:
                return encode_literal_type_simple("Long")
            
            case hydra.core.IntegerType.UINT8:
                return encode_literal_type_simple("Short")
            
            case hydra.core.IntegerType.UINT16:
                return encode_literal_type_simple("Character")
            
            case hydra.core.IntegerType.UINT32:
                return encode_literal_type_simple("Long")
            
            case hydra.core.IntegerType.UINT64:
                return hydra.lib.flows.pure(hydra.ext.java.utils.java_ref_type((), Just(hydra.ext.java.names.java_package_name(("java", "math"))), "BigInteger"))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    match lt:
        case hydra.core.LiteralTypeBinary():
            return hydra.lib.flows.pure(cast(hydra.ext.java.syntax.Type, hydra.ext.java.syntax.TypeReference(cast(hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.ReferenceTypeArray(hydra.ext.java.syntax.ArrayType(hydra.ext.java.syntax.Dims(((),)), cast(hydra.ext.java.syntax.ArrayType_Variant, hydra.ext.java.syntax.ArrayType_VariantPrimitive(hydra.ext.java.syntax.PrimitiveTypeWithAnnotations(cast(hydra.ext.java.syntax.PrimitiveType, hydra.ext.java.syntax.PrimitiveTypeNumeric(cast(hydra.ext.java.syntax.NumericType, hydra.ext.java.syntax.NumericTypeIntegral(hydra.ext.java.syntax.IntegralType.BYTE)))), ())))))))))
        
        case hydra.core.LiteralTypeBoolean():
            return encode_literal_type_simple("Boolean")
        
        case hydra.core.LiteralTypeFloat(value=ft):
            return _hoist_hydra_ext_java_coder_encode_literal_type_1(ft)
        
        case hydra.core.LiteralTypeInteger(value=it):
            return _hoist_hydra_ext_java_coder_encode_literal_type_2(it)
        
        case hydra.core.LiteralTypeString():
            return encode_literal_type_simple("String")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def is_lambda_bound_variable(name: hydra.core.Name) -> bool:
    @lru_cache(1)
    def v() -> str:
        return name.value
    return hydra.lib.equality.lte(hydra.lib.strings.length(v()), 4)

def encode_type_resolve_if_typedef(aliases: T0, bound_vars: frozenset[hydra.core.Name], in_scope_type_params: frozenset[hydra.core.Name], name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[hydra.core.Type]]:
    return hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.lib.sets.member(name, bound_vars), hydra.lib.sets.member(name, in_scope_type_params)), (lambda : hydra.lib.flows.pure(Nothing())), (lambda : hydra.lib.logic.if_else(is_lambda_bound_variable(name), (lambda : hydra.lib.flows.pure(Nothing())), (lambda : hydra.lib.flows.bind(hydra.monads.get_state(), (lambda g: hydra.lib.flows.bind(hydra.schemas.graph_to_inference_context(g), (lambda ix: (schema_types := ix.schema_types, _hoist_body_1 := (lambda ts, v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lib.maybes.cases(hydra.lib.maps.lookup(name, schema_types), hydra.lib.flows.pure(Nothing()), (lambda ts: hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(ts.variables)), (lambda : hydra.lib.flows.pure(Nothing())), (lambda : _hoist_body_1(ts, hydra.rewriting.deannotate_type(ts.type)))))))[2]))))))))

def is_unresolved_inference_var_is_digit(c: int) -> bool:
    return hydra.lib.logic.and_(hydra.lib.equality.gte(c, 48), hydra.lib.equality.lte(c, 57))

def is_unresolved_inference_var(name: hydra.core.Name) -> bool:
    @lru_cache(1)
    def chars() -> frozenlist[int]:
        return hydra.lib.strings.to_list(name.value)
    return hydra.lib.logic.if_else(hydra.lib.lists.null(chars()), (lambda : False), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.equality.equal(hydra.lib.lists.head(chars()), 116)), (lambda : False), (lambda : (rest := hydra.lib.lists.tail(chars()), hydra.lib.logic.and_(hydra.lib.logic.not_(hydra.lib.lists.null(rest)), hydra.lib.lists.null(hydra.lib.lists.filter((lambda c: hydra.lib.logic.not_(is_unresolved_inference_var_is_digit(c))), rest))))[1]))))

def java_type_parameters_for_type_bvars(t: hydra.core.Type) -> frozenlist[hydra.core.Name]:
    match t:
        case hydra.core.TypeForall(value=ft):
            return hydra.lib.lists.cons(ft.parameter, java_type_parameters_for_type_bvars(ft.body))
        
        case _:
            return ()

def java_type_parameters_for_type(typ: hydra.core.Type) -> frozenlist[hydra.ext.java.syntax.TypeParameter]:
    def to_param(name: hydra.core.Name) -> hydra.ext.java.syntax.TypeParameter:
        return hydra.ext.java.utils.java_type_parameter(hydra.formatting.capitalize(name.value))
    @lru_cache(1)
    def bound_vars() -> frozenlist[hydra.core.Name]:
        return java_type_parameters_for_type_bvars(typ)
    @lru_cache(1)
    def free_vars() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.filter((lambda v: is_lambda_bound_variable(v)), hydra.lib.sets.to_list(hydra.rewriting.free_variables_in_type(typ)))
    @lru_cache(1)
    def vars() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.nub(hydra.lib.lists.concat2(bound_vars(), free_vars()))
    return hydra.lib.lists.map((lambda x1: to_param(x1)), vars())

def java_type_arguments_for_type(typ: hydra.core.Type) -> frozenlist[hydra.ext.java.syntax.TypeArgument]:
    return hydra.lib.lists.reverse(hydra.lib.lists.map((lambda x1: hydra.ext.java.utils.type_parameter_to_type_argument(x1)), java_type_parameters_for_type(typ)))

def encode_type(aliases: hydra.ext.java.helpers.Aliases, bound_vars: frozenset[hydra.core.Name], t: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.Type]:
    @lru_cache(1)
    def in_scope_type_params() -> frozenset[hydra.core.Name]:
        return aliases.in_scope_type_params
    @lru_cache(1)
    def type_var_subst() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        return aliases.type_var_subst
    match hydra.rewriting.deannotate_type(t):
        case hydra.core.TypeApplication(value=at):
            return hydra.lib.flows.bind(encode_type(aliases, bound_vars, at.function), (lambda jlhs: hydra.lib.flows.bind(hydra.lib.flows.bind(encode_type(aliases, bound_vars, at.argument), (lambda jt_: hydra.ext.java.utils.java_type_to_java_reference_type(jt_))), (lambda jrhs: hydra.ext.java.utils.add_java_type_parameter(jrhs, jlhs)))))
        
        case hydra.core.TypeFunction(value=ft):
            return hydra.lib.flows.bind(hydra.lib.flows.bind(encode_type(aliases, bound_vars, ft.domain), (lambda jt_: hydra.ext.java.utils.java_type_to_java_reference_type(jt_))), (lambda jdom: hydra.lib.flows.bind(hydra.lib.flows.bind(encode_type(aliases, bound_vars, ft.codomain), (lambda jt_: hydra.ext.java.utils.java_type_to_java_reference_type(jt_))), (lambda jcod: hydra.lib.flows.pure(hydra.ext.java.utils.java_ref_type((jdom, jcod), hydra.ext.java.names.java_util_function_package_name, "Function"))))))
        
        case hydra.core.TypeForall(value=fa):
            return hydra.lib.flows.bind(encode_type(aliases, hydra.lib.sets.insert(fa.parameter, bound_vars), fa.body), (lambda jbody: hydra.ext.java.utils.add_java_type_parameter(hydra.ext.java.utils.java_type_variable(fa.parameter.value), jbody)))
        
        case hydra.core.TypeList(value=et):
            return hydra.lib.flows.bind(encode_type(aliases, bound_vars, et), (lambda jet: hydra.lib.flows.bind(hydra.lib.flows.bind(hydra.lib.flows.pure(jet), (lambda jt_: hydra.ext.java.utils.java_type_to_java_reference_type(jt_))), (lambda rt: hydra.lib.flows.pure(hydra.ext.java.utils.java_ref_type((rt,), hydra.ext.java.names.java_util_package_name, "List"))))))
        
        case hydra.core.TypeLiteral(value=lt):
            return encode_literal_type(lt)
        
        case hydra.core.TypeEither(value=et2):
            return hydra.lib.flows.bind(hydra.lib.flows.bind(encode_type(aliases, bound_vars, et2.left), (lambda jt_: hydra.ext.java.utils.java_type_to_java_reference_type(jt_))), (lambda jlt: hydra.lib.flows.bind(hydra.lib.flows.bind(encode_type(aliases, bound_vars, et2.right), (lambda jt_: hydra.ext.java.utils.java_type_to_java_reference_type(jt_))), (lambda jrt: hydra.lib.flows.pure(hydra.ext.java.utils.java_ref_type((jlt, jrt), hydra.ext.java.names.hydra_util_package_name, "Either"))))))
        
        case hydra.core.TypeMap(value=mt):
            return hydra.lib.flows.bind(hydra.lib.flows.bind(encode_type(aliases, bound_vars, mt.keys), (lambda jt_: hydra.ext.java.utils.java_type_to_java_reference_type(jt_))), (lambda jkt: hydra.lib.flows.bind(hydra.lib.flows.bind(encode_type(aliases, bound_vars, mt.values), (lambda jt_: hydra.ext.java.utils.java_type_to_java_reference_type(jt_))), (lambda jvt: hydra.lib.flows.pure(hydra.ext.java.utils.java_ref_type((jkt, jvt), hydra.ext.java.names.java_util_package_name, "Map"))))))
        
        case hydra.core.TypePair(value=pt):
            return hydra.lib.flows.bind(hydra.lib.flows.bind(encode_type(aliases, bound_vars, pt.first), (lambda jt_: hydra.ext.java.utils.java_type_to_java_reference_type(jt_))), (lambda jfirst: hydra.lib.flows.bind(hydra.lib.flows.bind(encode_type(aliases, bound_vars, pt.second), (lambda jt_: hydra.ext.java.utils.java_type_to_java_reference_type(jt_))), (lambda jsecond: hydra.lib.flows.pure(hydra.ext.java.utils.java_ref_type((jfirst, jsecond), hydra.ext.java.names.hydra_util_package_name, "Tuple.Tuple2"))))))
        
        case hydra.core.TypeUnit():
            return hydra.lib.flows.pure(hydra.ext.java.utils.java_ref_type((), hydra.ext.java.names.java_lang_package_name, "Void"))
        
        case hydra.core.TypeRecord(value=rt):
            return hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.equal(rt.type_name, hydra.core.Name("hydra.core.Unit")), hydra.lib.lists.null(rt.fields)), (lambda : hydra.lib.flows.pure(hydra.ext.java.utils.java_ref_type((), hydra.ext.java.names.java_lang_package_name, "Void"))), (lambda : hydra.lib.flows.pure(cast(hydra.ext.java.syntax.Type, hydra.ext.java.syntax.TypeReference(hydra.ext.java.utils.name_to_java_reference_type(aliases, True, java_type_arguments_for_type(t), rt.type_name, Nothing()))))))
        
        case hydra.core.TypeMaybe(value=ot):
            return hydra.lib.flows.bind(hydra.lib.flows.bind(encode_type(aliases, bound_vars, ot), (lambda jt_: hydra.ext.java.utils.java_type_to_java_reference_type(jt_))), (lambda jot: hydra.lib.flows.pure(hydra.ext.java.utils.java_ref_type((jot,), hydra.ext.java.names.hydra_util_package_name, "Maybe"))))
        
        case hydra.core.TypeSet(value=st):
            return hydra.lib.flows.bind(hydra.lib.flows.bind(encode_type(aliases, bound_vars, st), (lambda jt_: hydra.ext.java.utils.java_type_to_java_reference_type(jt_))), (lambda jst: hydra.lib.flows.pure(hydra.ext.java.utils.java_ref_type((jst,), hydra.ext.java.names.java_util_package_name, "Set"))))
        
        case hydra.core.TypeUnion(value=rt2):
            return hydra.lib.flows.pure(cast(hydra.ext.java.syntax.Type, hydra.ext.java.syntax.TypeReference(hydra.ext.java.utils.name_to_java_reference_type(aliases, True, java_type_arguments_for_type(t), rt2.type_name, Nothing()))))
        
        case hydra.core.TypeVariable(value=name0):
            @lru_cache(1)
            def name() -> hydra.core.Name:
                return hydra.lib.maybes.from_maybe(name0, hydra.lib.maps.lookup(name0, type_var_subst()))
            return hydra.lib.flows.bind(encode_type_resolve_if_typedef(aliases, bound_vars, in_scope_type_params(), name()), (lambda resolved: hydra.lib.maybes.cases(resolved, hydra.lib.flows.pure(hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.lib.sets.member(name(), bound_vars), hydra.lib.sets.member(name(), in_scope_type_params())), (lambda : cast(hydra.ext.java.syntax.Type, hydra.ext.java.syntax.TypeReference(hydra.ext.java.utils.java_type_variable(name().value)))), (lambda : hydra.lib.logic.if_else(is_lambda_bound_variable(name()), (lambda : cast(hydra.ext.java.syntax.Type, hydra.ext.java.syntax.TypeReference(hydra.ext.java.utils.java_type_variable(name().value)))), (lambda : hydra.lib.logic.if_else(is_unresolved_inference_var(name()), (lambda : cast(hydra.ext.java.syntax.Type, hydra.ext.java.syntax.TypeReference(cast(hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.ReferenceTypeClassOrInterface(cast(hydra.ext.java.syntax.ClassOrInterfaceType, hydra.ext.java.syntax.ClassOrInterfaceTypeClass(hydra.ext.java.utils.java_class_type((), hydra.ext.java.names.java_lang_package_name, "Object")))))))), (lambda : cast(hydra.ext.java.syntax.Type, hydra.ext.java.syntax.TypeReference(hydra.ext.java.utils.name_to_java_reference_type(aliases, True, (), name(), Nothing())))))))))), (lambda resolved_type: encode_type(aliases, bound_vars, resolved_type)))))
        
        case hydra.core.TypeWrap(value=wt):
            return hydra.lib.flows.pure(cast(hydra.ext.java.syntax.Type, hydra.ext.java.syntax.TypeReference(hydra.ext.java.utils.name_to_java_reference_type(aliases, True, (), wt.type_name, Nothing()))))
        
        case _:
            return hydra.lib.flows.fail(hydra.lib.strings.cat2("can't encode unsupported type in Java: ", hydra.show.core.type(t)))

def apply_cast_if_safe(aliases: hydra.ext.java.helpers.Aliases, cast_type: hydra.core.Type, expr: hydra.ext.java.syntax.Expression) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.Expression]:
    @lru_cache(1)
    def trusted() -> frozenset[hydra.core.Name]:
        return aliases.trusted_type_vars
    @lru_cache(1)
    def in_scope() -> frozenset[hydra.core.Name]:
        return aliases.in_scope_type_params
    @lru_cache(1)
    def cast_vars() -> frozenset[hydra.core.Name]:
        return collect_type_vars(cast_type)
    @lru_cache(1)
    def java_type_vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(hydra.lib.lists.filter((lambda v: hydra.lib.logic.or_(hydra.lib.sets.member(v, in_scope()), is_lambda_bound_variable(v))), hydra.lib.sets.to_list(cast_vars())))
    @lru_cache(1)
    def is_safe() -> bool:
        return hydra.lib.logic.or_(hydra.lib.sets.null(trusted()), hydra.lib.logic.or_(hydra.lib.sets.null(java_type_vars()), hydra.lib.sets.null(hydra.lib.sets.difference(java_type_vars(), trusted()))))
    return hydra.lib.logic.if_else(is_safe(), (lambda : hydra.lib.flows.bind(encode_type(aliases, hydra.lib.sets.empty(), cast_type), (lambda jtype: hydra.lib.flows.bind(hydra.ext.java.utils.java_type_to_java_reference_type(jtype), (lambda rt: hydra.lib.flows.pure(hydra.ext.java.utils.java_cast_expression_to_java_expression(hydra.ext.java.utils.java_cast_expression(rt, hydra.ext.java.utils.java_expression_to_java_unary_expression(expr))))))))), (lambda : hydra.lib.flows.pure(expr)))

def apply_java_arg(expr: hydra.ext.java.syntax.Expression, jarg: hydra.ext.java.syntax.Expression) -> hydra.ext.java.syntax.Expression:
    return hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.utils.method_invocation(Just(Right(hydra.ext.java.utils.java_expression_to_java_primary(expr))), hydra.ext.java.syntax.Identifier(hydra.ext.java.names.apply_method_name), (jarg,)))

def substitute_type_vars_with_types_go(subst: FrozenDict[hydra.core.Name, hydra.core.Type], t: hydra.core.Type) -> hydra.core.Type:
    match hydra.rewriting.deannotate_type(t):
        case hydra.core.TypeVariable(value=v):
            return hydra.lib.maybes.cases(hydra.lib.maps.lookup(v, subst), t, (lambda rep: rep))
        
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
    return substitute_type_vars_with_types_go(subst, hydra.rewriting.deannotate_type(t))

def apply_overgen_subst_to_term_annotations_go(subst: FrozenDict[hydra.core.Name, hydra.core.Type], cx: hydra.graph.Graph, term: hydra.core.Term) -> hydra.core.Term:
    def _hoist_hydra_ext_java_coder_apply_overgen_subst_to_term_annotations_go_1(cx: hydra.graph.Graph, subst: FrozenDict[hydra.core.Name, hydra.core.Type], term: hydra.core.Term, v1: hydra.core.Elimination) -> hydra.core.Term:
        match v1:
            case hydra.core.EliminationUnion(value=cs):
                return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(cs.type_name, hydra.lib.maybes.map((lambda d: apply_overgen_subst_to_term_annotations_go(subst, cx, d)), cs.default), hydra.lib.lists.map((lambda fld: hydra.core.Field(fld.name, apply_overgen_subst_to_term_annotations_go(subst, cx, fld.term))), cs.cases))))))))
            
            case _:
                return term
    def _hoist_hydra_ext_java_coder_apply_overgen_subst_to_term_annotations_go_2(cx: hydra.graph.Graph, subst: FrozenDict[hydra.core.Name, hydra.core.Type], term: hydra.core.Term, v1: hydra.core.Function) -> hydra.core.Term:
        match v1:
            case hydra.core.FunctionLambda(value=lam):
                return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(lam.parameter, hydra.lib.maybes.map((lambda d: substitute_type_vars_with_types(subst, d)), lam.domain), apply_overgen_subst_to_term_annotations_go(subst, cx, lam.body))))))
            
            case hydra.core.FunctionElimination(value=elim):
                return _hoist_hydra_ext_java_coder_apply_overgen_subst_to_term_annotations_go_1(cx, subst, term, elim)
            
            case _:
                return term
    match term:
        case hydra.core.TermAnnotated(value=at):
            @lru_cache(1)
            def inner() -> hydra.core.Term:
                return at.body
            @lru_cache(1)
            def ann() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                return at.annotation
            @lru_cache(1)
            def ann_() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                return hydra.lib.maybes.cases(hydra.lib.maps.lookup(hydra.constants.key_type, ann()), ann(), (lambda type_term: hydra.lib.eithers.either((lambda _: ann()), (lambda t: (t_ := substitute_type_vars_with_types(subst, t), hydra.lib.maps.insert(hydra.constants.key_type, hydra.encode.core.type(t_), ann()))[1]), hydra.decode.core.type(cx, type_term))))
            return cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(apply_overgen_subst_to_term_annotations_go(subst, cx, inner()), ann_())))
        
        case hydra.core.TermApplication(value=app):
            return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(apply_overgen_subst_to_term_annotations_go(subst, cx, app.function), apply_overgen_subst_to_term_annotations_go(subst, cx, app.argument))))
        
        case hydra.core.TermFunction(value=f):
            return _hoist_hydra_ext_java_coder_apply_overgen_subst_to_term_annotations_go_2(cx, subst, term, f)
        
        case hydra.core.TermLet(value=lt):
            return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.map((lambda b: hydra.core.Binding(b.name, apply_overgen_subst_to_term_annotations_go(subst, cx, b.term), b.type)), lt.bindings), apply_overgen_subst_to_term_annotations_go(subst, cx, lt.body))))
        
        case hydra.core.TermTypeApplication(value=ta):
            return cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(apply_overgen_subst_to_term_annotations_go(subst, cx, ta.body), substitute_type_vars_with_types(subst, ta.type))))
        
        case hydra.core.TermTypeLambda(value=tl):
            return cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(tl.parameter, apply_overgen_subst_to_term_annotations_go(subst, cx, tl.body))))
        
        case _:
            return term

def apply_overgen_subst_to_term_annotations(subst: FrozenDict[hydra.core.Name, hydra.core.Type], term0: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
    return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda cx: hydra.lib.flows.pure(apply_overgen_subst_to_term_annotations_go(subst, cx, term0))))

def apply_subst_simple(subst: FrozenDict[hydra.core.Name, hydra.core.Type], t: hydra.core.Type) -> hydra.core.Type:
    match hydra.rewriting.deannotate_type(t):
        case hydra.core.TypeVariable(value=v):
            return hydra.lib.maps.find_with_default(t, v, subst)
        
        case _:
            return t

def arrays_compare_expr(other_var: str, fname: str) -> hydra.ext.java.syntax.Expression:
    @lru_cache(1)
    def header() -> hydra.ext.java.syntax.MethodInvocation_Header:
        return cast(hydra.ext.java.syntax.MethodInvocation_Header, hydra.ext.java.syntax.MethodInvocation_HeaderComplex(hydra.ext.java.syntax.MethodInvocation_Complex(cast(hydra.ext.java.syntax.MethodInvocation_Variant, hydra.ext.java.syntax.MethodInvocation_VariantType(hydra.ext.java.utils.java_type_name(hydra.ext.java.syntax.Identifier("java.util.Arrays")))), (), hydra.ext.java.syntax.Identifier("compare"))))
    @lru_cache(1)
    def arg1() -> hydra.ext.java.syntax.Expression:
        return hydra.ext.java.utils.java_expression_name_to_java_expression(hydra.ext.java.syntax.ExpressionName(Nothing(), hydra.ext.java.syntax.Identifier(hydra.ext.java.utils.sanitize_java_name(fname))))
    @lru_cache(1)
    def arg2() -> hydra.ext.java.syntax.Expression:
        return hydra.ext.java.utils.java_expression_name_to_java_expression(hydra.ext.java.utils.field_expression(hydra.ext.java.utils.java_identifier(other_var), hydra.ext.java.utils.java_identifier(fname)))
    return hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.syntax.MethodInvocation(header(), (arg1(), arg2())))

def arrays_equals_clause(tmp_name: str, fname: str) -> hydra.ext.java.syntax.InclusiveOrExpression:
    @lru_cache(1)
    def this_arg() -> hydra.ext.java.syntax.Expression:
        return hydra.ext.java.utils.java_expression_name_to_java_expression(hydra.ext.java.utils.field_expression(hydra.ext.java.syntax.Identifier("this"), hydra.ext.java.utils.java_identifier(fname)))
    @lru_cache(1)
    def other_arg() -> hydra.ext.java.syntax.Expression:
        return hydra.ext.java.utils.java_expression_name_to_java_expression(hydra.ext.java.utils.field_expression(hydra.ext.java.utils.java_identifier(tmp_name), hydra.ext.java.utils.java_identifier(fname)))
    @lru_cache(1)
    def header() -> hydra.ext.java.syntax.MethodInvocation_Header:
        return cast(hydra.ext.java.syntax.MethodInvocation_Header, hydra.ext.java.syntax.MethodInvocation_HeaderComplex(hydra.ext.java.syntax.MethodInvocation_Complex(cast(hydra.ext.java.syntax.MethodInvocation_Variant, hydra.ext.java.syntax.MethodInvocation_VariantType(hydra.ext.java.utils.java_type_name(hydra.ext.java.syntax.Identifier("java.util.Arrays")))), (), hydra.ext.java.syntax.Identifier(hydra.ext.java.names.equals_method_name))))
    return hydra.ext.java.utils.java_postfix_expression_to_java_inclusive_or_expression(hydra.ext.java.utils.java_method_invocation_to_java_postfix_expression(hydra.ext.java.syntax.MethodInvocation(header(), (this_arg(), other_arg()))))

def no_comment(decl: hydra.ext.java.syntax.ClassBodyDeclaration) -> hydra.ext.java.syntax.ClassBodyDeclarationWithComments:
    return hydra.ext.java.syntax.ClassBodyDeclarationWithComments(decl, Nothing())

def augment_variant_class(aliases: hydra.ext.java.helpers.Aliases, tparams: frozenlist[hydra.ext.java.syntax.TypeParameter], el_name: hydra.core.Name, cd: hydra.ext.java.syntax.ClassDeclaration) -> hydra.ext.java.syntax.ClassDeclaration:
    match cd:
        case hydra.ext.java.syntax.ClassDeclarationNormal(value=ncd):
            @lru_cache(1)
            def args() -> frozenlist[hydra.ext.java.syntax.TypeArgument]:
                return hydra.lib.lists.map((lambda tp: hydra.ext.java.utils.type_parameter_to_type_argument(tp)), tparams)
            @lru_cache(1)
            def extends_part() -> hydra.ext.java.syntax.ClassType:
                return hydra.ext.java.utils.name_to_java_class_type(aliases, True, args(), el_name, Nothing())
            new_mods = (cast(hydra.ext.java.syntax.ClassModifier, hydra.ext.java.syntax.ClassModifierPublic()), cast(hydra.ext.java.syntax.ClassModifier, hydra.ext.java.syntax.ClassModifierStatic()), cast(hydra.ext.java.syntax.ClassModifier, hydra.ext.java.syntax.ClassModifierFinal()))
            @lru_cache(1)
            def old_body() -> hydra.ext.java.syntax.ClassBody:
                return ncd.body
            @lru_cache(1)
            def old_decls() -> frozenlist[hydra.ext.java.syntax.ClassBodyDeclarationWithComments]:
                return old_body().value
            @lru_cache(1)
            def accept_decl() -> hydra.ext.java.syntax.ClassBodyDeclarationWithComments:
                return no_comment(hydra.ext.java.utils.to_accept_method(False, tparams))
            @lru_cache(1)
            def new_body() -> hydra.ext.java.syntax.ClassBody:
                return hydra.ext.java.syntax.ClassBody(hydra.lib.lists.concat2(old_decls(), (accept_decl(),)))
            return cast(hydra.ext.java.syntax.ClassDeclaration, hydra.ext.java.syntax.ClassDeclarationNormal(hydra.ext.java.syntax.NormalClassDeclaration(new_mods, ncd.identifier, tparams, Just(extends_part()), ncd.implements, new_body())))
        
        case _:
            return cd

def binding_is_function_type(b: hydra.core.Binding) -> bool:
    def _hoist_hydra_ext_java_coder_binding_is_function_type_1(v1: hydra.core.Term) -> bool:
        match v1:
            case hydra.core.TermFunction():
                return True
            
            case _:
                return False
    def _hoist_hydra_ext_java_coder_binding_is_function_type_2(v1: hydra.core.Type) -> bool:
        match v1:
            case hydra.core.TypeFunction():
                return True
            
            case _:
                return False
    def _hoist_hydra_ext_java_coder_binding_is_function_type_3(v1: hydra.core.Type) -> bool:
        match v1:
            case hydra.core.TypeFunction():
                return True
            
            case hydra.core.TypeForall(value=fa):
                return _hoist_hydra_ext_java_coder_binding_is_function_type_2(hydra.rewriting.deannotate_type(fa.body))
            
            case _:
                return False
    return hydra.lib.maybes.maybe(_hoist_hydra_ext_java_coder_binding_is_function_type_1(hydra.rewriting.deannotate_term(b.term)), (lambda ts: _hoist_hydra_ext_java_coder_binding_is_function_type_3(hydra.rewriting.deannotate_type(ts.type))), b.type)

def binding_name_to_file_path(name: hydra.core.Name) -> str:
    @lru_cache(1)
    def qn() -> hydra.module.QualifiedName:
        return hydra.names.qualify_name(name)
    @lru_cache(1)
    def ns_() -> Maybe[hydra.module.Namespace]:
        return qn().namespace
    @lru_cache(1)
    def local() -> str:
        return qn().local
    @lru_cache(1)
    def sanitized() -> str:
        return hydra.formatting.sanitize_with_underscores(hydra.ext.java.language.reserved_words(), local())
    @lru_cache(1)
    def unq() -> hydra.core.Name:
        return hydra.names.unqualify_name(hydra.module.QualifiedName(ns_(), sanitized()))
    return hydra.adapt.utils.name_to_file_path(hydra.util.CaseConvention.CAMEL, hydra.util.CaseConvention.PASCAL, hydra.module.FileExtension("java"), unq())

def fresh_java_name_go(base: hydra.core.Name, avoid: frozenset[hydra.core.Name], i: int) -> hydra.core.Name:
    @lru_cache(1)
    def candidate() -> hydra.core.Name:
        return hydra.core.Name(hydra.lib.strings.cat2(base.value, hydra.lib.literals.show_int32(i)))
    return hydra.lib.logic.if_else(hydra.lib.sets.member(candidate(), avoid), (lambda : fresh_java_name_go(base, avoid, hydra.lib.math.add(i, 1))), (lambda : candidate()))

def fresh_java_name(base: hydra.core.Name, avoid: frozenset[hydra.core.Name]) -> hydra.core.Name:
    return fresh_java_name_go(base, avoid, 2)

def dedup_bindings(in_scope: frozenset[hydra.core.Name], bs: frozenlist[hydra.core.Binding]) -> frozenlist[hydra.core.Binding]:
    return hydra.lib.logic.if_else(hydra.lib.lists.null(bs), (lambda : ()), (lambda : (b := hydra.lib.lists.head(bs), (rest := hydra.lib.lists.tail(bs), (name := b.name, hydra.lib.logic.if_else(hydra.lib.sets.member(name, in_scope), (lambda : (new_name := fresh_java_name(name, in_scope), (subst := hydra.lib.maps.singleton(name, new_name), (rest2 := hydra.lib.lists.map((lambda b2: hydra.core.Binding(b2.name, hydra.rewriting.substitute_variables(subst, b2.term), b2.type)), rest), hydra.lib.lists.cons(hydra.core.Binding(new_name, b.term, b.type), dedup_bindings(hydra.lib.sets.insert(new_name, in_scope), rest2)))[1])[1])[1]), (lambda : hydra.lib.lists.cons(b, dedup_bindings(hydra.lib.sets.insert(name, in_scope), rest)))))[1])[1])[1]))

def flatten_bindings(bindings: frozenlist[hydra.core.Binding]) -> frozenlist[hydra.core.Binding]:
    def _hoist_hydra_ext_java_coder_flatten_bindings_1(b: hydra.core.Binding, v1: hydra.core.Term) -> frozenlist[hydra.core.Binding]:
        match v1:
            case hydra.core.TermLet(value=lt):
                return hydra.lib.lists.concat2(flatten_bindings(lt.bindings), (hydra.core.Binding(b.name, lt.body, b.type),))
            
            case _:
                return (b,)
    return hydra.lib.lists.bind(bindings, (lambda b: _hoist_hydra_ext_java_coder_flatten_bindings_1(b, hydra.rewriting.deannotate_term(b.term))))

def needs_thunking(t: hydra.core.Term) -> bool:
    match hydra.rewriting.deannotate_term(t):
        case hydra.core.TermLet():
            return True
        
        case hydra.core.TermTypeApplication():
            return True
        
        case hydra.core.TermTypeLambda():
            return True
        
        case _:
            return hydra.lib.lists.foldl((lambda b, st: hydra.lib.logic.or_(b, needs_thunking(st))), False, hydra.rewriting.subterms(t))

java11_features = hydra.ext.java.helpers.JavaFeatures(True)

java_features = java11_features

def type_args_or_diamond(args: frozenlist[hydra.ext.java.syntax.TypeArgument]) -> hydra.ext.java.syntax.TypeArgumentsOrDiamond:
    return hydra.lib.logic.if_else(java_features.supports_diamond_operator, (lambda : cast(hydra.ext.java.syntax.TypeArgumentsOrDiamond, hydra.ext.java.syntax.TypeArgumentsOrDiamondDiamond())), (lambda : cast(hydra.ext.java.syntax.TypeArgumentsOrDiamond, hydra.ext.java.syntax.TypeArgumentsOrDiamondArguments(args))))

def to_decl_init(aliases_ext: hydra.ext.java.helpers.Aliases, tc_ext: hydra.typing.TypeContext, recursive_vars: frozenset[hydra.core.Name], flat_bindings: frozenlist[hydra.core.Binding], name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[hydra.ext.java.syntax.BlockStatement]]:
    return hydra.lib.logic.if_else(hydra.lib.sets.member(name, recursive_vars), (lambda : (binding := hydra.lib.lists.head(hydra.lib.lists.filter((lambda b: hydra.lib.equality.equal(b.name, name)), flat_bindings)), (value := binding.term, hydra.lib.flows.bind(hydra.lib.maybes.cases(binding.type, hydra.coder_utils.try_type_of("6", tc_ext, value), (lambda ts: hydra.lib.flows.pure(ts.type))), (lambda typ: hydra.lib.flows.bind(encode_type(aliases_ext, hydra.lib.sets.empty(), typ), (lambda jtype: (id := hydra.ext.java.utils.variable_to_java_identifier(name), arid := hydra.ext.java.syntax.Identifier("java.util.concurrent.atomic.AtomicReference"), aid := hydra.ext.java.syntax.AnnotatedIdentifier((), arid), hydra.lib.flows.bind(hydra.ext.java.utils.java_type_to_java_reference_type(jtype), (lambda rt: (targs := type_args_or_diamond((cast(hydra.ext.java.syntax.TypeArgument, hydra.ext.java.syntax.TypeArgumentReference(rt)),)), ci := hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate((aid,), Just(targs)), body := hydra.ext.java.utils.java_constructor_call(ci, (), Nothing()), pkg := hydra.ext.java.names.java_package_name(("java", "util", "concurrent", "atomic")), artype := hydra.ext.java.utils.java_ref_type((rt,), Just(pkg), "AtomicReference"), hydra.lib.flows.pure(Just(hydra.ext.java.utils.variable_declaration_statement(aliases_ext, artype, id, body))))[5])))[3])))))[1])[1]), (lambda : hydra.lib.flows.pure(Nothing())))

def classify_data_term_count_lambda_params(t: hydra.core.Term) -> int:
    def _hoist_hydra_ext_java_coder_classify_data_term_count_lambda_params_1(v1: hydra.core.Function) -> int:
        match v1:
            case hydra.core.FunctionLambda(value=lam):
                return hydra.lib.math.add(1, classify_data_term_count_lambda_params(lam.body))
            
            case _:
                return 0
    match hydra.rewriting.deannotate_term(t):
        case hydra.core.TermFunction(value=f):
            return _hoist_hydra_ext_java_coder_classify_data_term_count_lambda_params_1(f)
        
        case hydra.core.TermLet(value=lt):
            return classify_data_term_count_lambda_params(lt.body)
        
        case _:
            return 0

def classify_data_term_strip_type_lambdas(t: hydra.core.Term) -> hydra.core.Term:
    match hydra.rewriting.deannotate_term(t):
        case hydra.core.TermTypeLambda(value=tl):
            return classify_data_term_strip_type_lambdas(tl.body)
        
        case _:
            return t

def classify_data_term(ts: hydra.core.TypeScheme, term: hydra.core.Term) -> hydra.ext.java.helpers.JavaSymbolClass:
    return hydra.lib.logic.if_else(hydra.rewriting.is_lambda(term), (lambda : (n := classify_data_term_count_lambda_params(term), hydra.lib.logic.if_else(hydra.lib.equality.gt(n, 1), (lambda : cast(hydra.ext.java.helpers.JavaSymbolClass, hydra.ext.java.helpers.JavaSymbolClassHoistedLambda(n))), (lambda : cast(hydra.ext.java.helpers.JavaSymbolClass, hydra.ext.java.helpers.JavaSymbolClassUnaryFunction()))))[1]), (lambda : (has_type_params := hydra.lib.logic.not_(hydra.lib.lists.null(ts.variables)), hydra.lib.logic.if_else(has_type_params, (lambda : (n2 := classify_data_term_count_lambda_params(classify_data_term_strip_type_lambdas(term)), hydra.lib.logic.if_else(hydra.lib.equality.gt(n2, 0), (lambda : cast(hydra.ext.java.helpers.JavaSymbolClass, hydra.ext.java.helpers.JavaSymbolClassHoistedLambda(n2))), (lambda : cast(hydra.ext.java.helpers.JavaSymbolClass, hydra.ext.java.helpers.JavaSymbolClassNullaryFunction()))))[1]), (lambda : cast(hydra.ext.java.helpers.JavaSymbolClass, hydra.ext.java.helpers.JavaSymbolClassNullaryFunction()))))[1]))

def classify_data_reference(name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.helpers.JavaSymbolClass]:
    return hydra.lib.flows.bind(hydra.lexical.dereference_element(name), (lambda mel: hydra.lib.maybes.cases(mel, hydra.lib.flows.pure(cast(hydra.ext.java.helpers.JavaSymbolClass, hydra.ext.java.helpers.JavaSymbolClassLocalVariable())), (lambda el: hydra.lib.maybes.cases(el.type, hydra.lib.flows.fail(hydra.lib.strings.cat2("no type scheme for element ", el.name.value)), (lambda ts: hydra.lib.flows.pure(classify_data_term(ts, el.term))))))))

def collect_type_apps(t: hydra.core.Term, acc: frozenlist[hydra.core.Type]) -> tuple[hydra.core.Term, frozenlist[hydra.core.Type]]:
    match hydra.rewriting.deannotate_term(t):
        case hydra.core.TermTypeApplication(value=ta):
            return collect_type_apps(ta.body, hydra.lib.lists.cons(ta.type, acc))
        
        case _:
            return (hydra.rewriting.deannotate_term(t), acc)

def collect_type_apps0(t: hydra.core.Term, acc: frozenlist[hydra.core.Type]) -> tuple[hydra.core.Term, frozenlist[hydra.core.Type]]:
    match hydra.rewriting.deannotate_term(t):
        case hydra.core.TermTypeApplication(value=ta):
            return collect_type_apps0(ta.body, hydra.lib.lists.cons(ta.type, acc))
        
        case _:
            return (t, acc)

def correct_cast_type(inner_body: hydra.core.Term, type_args: frozenlist[hydra.core.Type], fallback: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.core.Type]:
    match hydra.rewriting.deannotate_term(inner_body):
        case hydra.core.TermPair():
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(type_args), 2), (lambda : hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypePair(hydra.core.PairType(hydra.lib.lists.head(type_args), hydra.lib.lists.head(hydra.lib.lists.tail(type_args))))))), (lambda : hydra.lib.flows.pure(fallback)))
        
        case _:
            return hydra.lib.flows.pure(fallback)

def build_arg_subst(scheme_var_set: frozenset[hydra.core.Name], scheme_doms: frozenlist[hydra.core.Type], arg_types: frozenlist[T0]) -> FrozenDict[hydra.core.Name, T0]:
    return hydra.lib.maps.from_list(hydra.lib.lists.bind(hydra.lib.lists.zip(scheme_doms, arg_types), (lambda p: (sdom := hydra.lib.pairs.first(p), arg_type := hydra.lib.pairs.second(p), hydra.dsl.python.unsupported("inline match expressions are not yet supported"))[2])))

def peel_domain_types(n: int, t: hydra.core.Type) -> tuple[frozenlist[hydra.core.Type], hydra.core.Type]:
    def _hoist_hydra_ext_java_coder_peel_domain_types_1(n: int, t: hydra.core.Type, v1: hydra.core.Type) -> tuple[frozenlist[hydra.core.Type], hydra.core.Type]:
        match v1:
            case hydra.core.TypeFunction(value=ft):
                @lru_cache(1)
                def rest() -> tuple[frozenlist[hydra.core.Type], hydra.core.Type]:
                    return peel_domain_types(hydra.lib.math.sub(n, 1), ft.codomain)
                return (hydra.lib.lists.cons(ft.domain, hydra.lib.pairs.first(rest())), hydra.lib.pairs.second(rest()))
            
            case _:
                return ((), t)
    return hydra.lib.logic.if_else(hydra.lib.equality.lte(n, 0), (lambda : ((), t)), (lambda : _hoist_hydra_ext_java_coder_peel_domain_types_1(n, t, hydra.rewriting.deannotate_type(t))))

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

def types_match(a: hydra.core.Type, b: hydra.core.Type) -> bool:
    def _hoist_hydra_ext_java_coder_types_match_1(va: hydra.core.Name, v1: hydra.core.Type) -> bool:
        match v1:
            case hydra.core.TypeVariable(value=vb):
                return hydra.lib.equality.equal(va, vb)
            
            case _:
                return True
    def _hoist_hydra_ext_java_coder_types_match_2(wa: hydra.core.WrappedType, v1: hydra.core.Type) -> bool:
        match v1:
            case hydra.core.TypeWrap(value=wb):
                return hydra.lib.equality.equal(wa.type_name, wb.type_name)
            
            case _:
                return True
    match a:
        case hydra.core.TypeVariable(value=va):
            return _hoist_hydra_ext_java_coder_types_match_1(va, b)
        
        case hydra.core.TypeWrap(value=wa):
            return _hoist_hydra_ext_java_coder_types_match_2(wa, b)
        
        case _:
            return True

def correct_type_apps_with_args(scheme_vars: frozenlist[hydra.core.Name], fallback_type_apps: frozenlist[hydra.core.Type], scheme_type: hydra.core.Type, args: frozenlist[hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, frozenlist[hydra.core.Type]]:
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
    return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda arg: hydra.annotations.get_type(hydra.annotations.term_annotation_internal(arg))), args), (lambda m_arg_types: hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(hydra.lib.lists.filter((lambda m: hydra.lib.maybes.is_nothing(m)), m_arg_types))), (lambda : hydra.lib.flows.pure(fallback_type_apps)), (lambda : (arg_types := hydra.lib.lists.bind(m_arg_types, (lambda m: hydra.lib.maybes.cases(m, (), (lambda x: hydra.lib.lists.pure(x))))), (ir_doms := hydra.lib.lists.map((lambda d: apply_subst_simple(ir_subst(), d)), scheme_doms()), (doms_match := hydra.lib.lists.null(hydra.lib.lists.filter((lambda p: hydra.lib.logic.not_(types_match(hydra.rewriting.deannotate_type(hydra.lib.pairs.first(p)), hydra.rewriting.deannotate_type(hydra.lib.pairs.second(p))))), hydra.lib.lists.zip(ir_doms, arg_types))), hydra.lib.logic.if_else(doms_match, (lambda : hydra.lib.flows.pure(fallback_type_apps)), (lambda : hydra.lib.flows.pure(resolve_type_apps(scheme_vars, fallback_type_apps, build_arg_subst(scheme_var_set(), scheme_doms(), arg_types))))))[1])[1])[1]))))

def count_function_params(t: hydra.core.Type) -> int:
    match hydra.rewriting.deannotate_type(t):
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

def extract_direct_return_go(tparam_set: frozenset[hydra.core.Name], t: hydra.core.Type) -> frozenlist[tuple[hydra.core.Name, hydra.core.Name]]:
    match hydra.rewriting.deannotate_type(t):
        case hydra.core.TypeFunction(value=ft):
            @lru_cache(1)
            def dom() -> hydra.core.Type:
                return hydra.rewriting.deannotate_type(ft.domain)
            @lru_cache(1)
            def cod() -> hydra.core.Type:
                return ft.codomain
            def _hoist_body_1(in_var: T0, v1: hydra.core.Type) -> frozenlist[tuple[T0, hydra.core.Name]]:
                match v1:
                    case hydra.core.TypeFunction(value=ft2):
                        @lru_cache(1)
                        def mid_arg() -> hydra.core.Type:
                            return hydra.rewriting.deannotate_type(ft2.domain)
                        @lru_cache(1)
                        def ret_part() -> hydra.core.Type:
                            return hydra.rewriting.deannotate_type(ft2.codomain)
                        def _hoist_body_1(v12: hydra.core.Type) -> frozenlist[tuple[T0, hydra.core.Name]]:
                            match v12:
                                case hydra.core.TypeVariable(value=out_var):
                                    return hydra.lib.logic.if_else(hydra.lib.sets.member(out_var, tparam_set), (lambda : ((in_var, out_var),)), (lambda : ()))
                                
                                case _:
                                    return ()
                        def _hoist_body_2(v12: hydra.core.Type) -> frozenlist[tuple[T0, hydra.core.Name]]:
                            match v12:
                                case hydra.core.TypeVariable(value=out_var):
                                    return hydra.lib.logic.if_else(hydra.lib.sets.member(out_var, tparam_set), (lambda : ((in_var, out_var),)), (lambda : ()))
                                
                                case _:
                                    return ()
                        match mid_arg():
                            case hydra.core.TypeVariable(value=mid_var):
                                return hydra.lib.logic.if_else(hydra.lib.sets.member(mid_var, tparam_set), (lambda : ()), (lambda : _hoist_body_2(ret_part())))
                            
                            case _:
                                return _hoist_body_1(ret_part())
                    
                    case _:
                        return ()
            match dom():
                case hydra.core.TypeVariable(value=in_var):
                    return hydra.lib.logic.if_else(hydra.lib.sets.member(in_var, tparam_set), (lambda : _hoist_body_1(in_var, hydra.rewriting.deannotate_type(cod()))), (lambda : extract_direct_return_go(tparam_set, cod())))
                
                case _:
                    return extract_direct_return_go(tparam_set, cod())
        
        case _:
            return ()

def extract_direct_return(tparam_set: frozenset[hydra.core.Name], t: hydra.core.Type) -> frozenlist[tuple[hydra.core.Name, hydra.core.Name]]:
    return extract_direct_return_go(tparam_set, t)

def unwrap_return_type(t: hydra.core.Type) -> hydra.core.Type:
    match hydra.rewriting.deannotate_type(t):
        case hydra.core.TypeFunction(value=ft):
            return unwrap_return_type(ft.codomain)
        
        case hydra.core.TypeApplication(value=at):
            return unwrap_return_type(at.argument)
        
        case _:
            return t

def extract_in_out_pair(t: hydra.core.Type) -> frozenlist[tuple[hydra.core.Name, hydra.core.Name]]:
    def _hoist_hydra_ext_java_coder_extract_in_out_pair_1(ft: hydra.core.FunctionType, v1: hydra.core.Type) -> frozenlist[tuple[hydra.core.Name, hydra.core.Name]]:
        match v1:
            case hydra.core.TypeVariable(value=in_var):
                @lru_cache(1)
                def ret_type() -> hydra.core.Type:
                    return unwrap_return_type(ft.codomain)
                def _hoist_body_1(v12: hydra.core.Type) -> frozenlist[tuple[hydra.core.Name, hydra.core.Name]]:
                    match v12:
                        case hydra.core.TypeVariable(value=out_var):
                            return ((in_var, out_var),)
                        
                        case _:
                            return ()
                match hydra.rewriting.deannotate_type(ret_type()):
                    case hydra.core.TypePair(value=pt):
                        return _hoist_body_1(hydra.rewriting.deannotate_type(pt.first))
                    
                    case _:
                        return ()
            
            case _:
                return ()
    match hydra.rewriting.deannotate_type(t):
        case hydra.core.TypeFunction(value=ft):
            return _hoist_hydra_ext_java_coder_extract_in_out_pair_1(ft, hydra.rewriting.deannotate_type(ft.domain))
        
        case _:
            return ()

def find_pair_first(t: hydra.core.Type) -> Maybe[hydra.core.Name]:
    def _hoist_hydra_ext_java_coder_find_pair_first_1(v1: hydra.core.Type) -> Maybe[hydra.core.Name]:
        match v1:
            case hydra.core.TypeVariable(value=v):
                return Just(v)
            
            case _:
                return Nothing()
    match hydra.rewriting.deannotate_type(t):
        case hydra.core.TypePair(value=pt):
            return _hoist_hydra_ext_java_coder_find_pair_first_1(hydra.rewriting.deannotate_type(pt.first))
        
        case _:
            return Nothing()

def find_self_ref_var(grouped: FrozenDict[T0, frozenlist[T0]]) -> Maybe[T0]:
    @lru_cache(1)
    def self_refs() -> frozenlist[tuple[T0, frozenlist[T0]]]:
        return hydra.lib.lists.filter((lambda entry: hydra.lib.lists.elem(hydra.lib.pairs.first(entry), hydra.lib.pairs.second(entry))), hydra.lib.maps.to_list(grouped))
    return hydra.lib.logic.if_else(hydra.lib.lists.null(self_refs()), (lambda : Nothing()), (lambda : Just(hydra.lib.pairs.first(hydra.lib.lists.head(self_refs())))))

def group_pairs_by_first(pairs: frozenlist[tuple[T0, T1]]) -> FrozenDict[T0, frozenlist[T1]]:
    return hydra.lib.lists.foldl((lambda m, p: (k := hydra.lib.pairs.first(p), v := hydra.lib.pairs.second(p), hydra.lib.maps.alter((lambda mv: hydra.lib.maybes.maybe(Just((v,)), (lambda vs: Just(hydra.lib.lists.concat2(vs, (v,)))), mv)), k, m))[2]), hydra.lib.maps.empty(), pairs)

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
        match hydra.rewriting.deannotate_type(cod):
            case hydra.core.TypeVariable(value=v):
                return Just(v)
            
            case _:
                return Nothing()
    @lru_cache(1)
    def direct_ref_subst() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        return direct_ref_substitution(direct_input_vars(), cod_var(), grouped_direct())
    @lru_cache(1)
    def cod_subst() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        return hydra.lib.maybes.maybe(hydra.lib.maps.empty(), (lambda cv: hydra.lib.logic.if_else(hydra.lib.maps.member(cv, self_ref_subst()), (lambda : hydra.lib.maps.empty()), (lambda : hydra.lib.maybes.maybe(hydra.lib.maps.empty(), (lambda ref_var: hydra.lib.logic.if_else(hydra.lib.equality.equal(cv, ref_var), (lambda : hydra.lib.maps.empty()), (lambda : hydra.lib.maps.singleton(cv, ref_var)))), find_self_ref_var(grouped_by_input()))))), find_pair_first(cod))
    @lru_cache(1)
    def dom_vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(hydra.lib.lists.bind(doms, (lambda d: hydra.lib.sets.to_list(collect_type_vars(d)))))
    @lru_cache(1)
    def dangling_subst() -> FrozenDict[hydra.core.Name, hydra.core.Type]:
        return hydra.lib.maybes.maybe(hydra.lib.maps.empty(), (lambda cv: hydra.lib.logic.if_else(hydra.lib.sets.member(cv, dom_vars()), (lambda : hydra.lib.maps.empty()), (lambda : hydra.lib.maybes.maybe(hydra.lib.maps.empty(), (lambda ref_var: hydra.lib.maps.singleton(cv, cast(hydra.core.Type, hydra.core.TypeVariable(ref_var)))), find_self_ref_var(grouped_by_input()))))), find_pair_first(cod))
    return hydra.lib.maps.union(hydra.lib.maps.union(hydra.lib.maps.union(name_map_to_type_map(self_ref_subst()), name_map_to_type_map(cod_subst())), dangling_subst()), name_map_to_type_map(direct_ref_subst()))

def filter_by_flags(xs: frozenlist[T0], flags: frozenlist[bool]) -> frozenlist[T0]:
    return hydra.lib.lists.map((lambda p: hydra.lib.pairs.first(p)), hydra.lib.lists.filter((lambda p: hydra.lib.pairs.second(p)), hydra.lib.lists.zip(xs, flags)))

def is_simple_name(name: hydra.core.Name) -> bool:
    return hydra.lib.equality.equal(hydra.lib.lists.length(hydra.lib.strings.split_on(".", name.value)), 1)

def correct_type_apps(tc: T0, name: hydra.core.Name, args: frozenlist[hydra.core.Term], fallback_type_apps: frozenlist[hydra.core.Type]) -> hydra.compute.Flow[hydra.graph.Graph, frozenlist[hydra.core.Type]]:
    return hydra.lib.flows.bind(hydra.lexical.dereference_element(name), (lambda mel: hydra.lib.maybes.cases(mel, hydra.lib.flows.pure(fallback_type_apps), (lambda el: hydra.lib.maybes.cases(el.type, hydra.lib.flows.pure(fallback_type_apps), (lambda ts: (scheme_type := ts.type, all_scheme_vars := hydra.lib.lists.filter((lambda v: is_simple_name(v)), ts.variables), scheme_type_vars := collect_type_vars(scheme_type), used_flags := hydra.lib.lists.map((lambda v: hydra.lib.sets.member(v, scheme_type_vars)), all_scheme_vars), used_scheme_vars := filter_by_flags(all_scheme_vars, used_flags), n_params := count_function_params(scheme_type), peeled := peel_domain_types(n_params, scheme_type), callee_doms := hydra.lib.pairs.first(peeled), callee_cod := hydra.lib.pairs.second(peeled), overgen_subst := detect_accumulator_unification(callee_doms, callee_cod, used_scheme_vars), keep_flags := hydra.lib.lists.map((lambda v: hydra.lib.logic.and_(hydra.lib.sets.member(v, scheme_type_vars), hydra.lib.logic.not_(hydra.lib.maps.member(v, overgen_subst)))), all_scheme_vars), scheme_vars := filter_by_flags(all_scheme_vars, keep_flags), filtered_fallback0 := hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(all_scheme_vars), hydra.lib.lists.length(fallback_type_apps)), (lambda : filter_by_flags(fallback_type_apps, keep_flags)), (lambda : fallback_type_apps)), filtered_fallback := hydra.lib.logic.if_else(hydra.lib.maps.null(overgen_subst), (lambda : filtered_fallback0), (lambda : hydra.lib.lists.map((lambda t: substitute_type_vars_with_types(overgen_subst, t)), filtered_fallback0))), hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.lib.lists.null(scheme_vars), hydra.lib.logic.not_(hydra.lib.equality.equal(hydra.lib.lists.length(scheme_vars), hydra.lib.lists.length(filtered_fallback)))), (lambda : hydra.lib.flows.pure(filtered_fallback)), (lambda : correct_type_apps_with_args(scheme_vars, filtered_fallback, scheme_type, args))))[14]))))))

def extract_type_application_args_go(t: hydra.core.Type) -> frozenlist[hydra.core.Type]:
    match t:
        case hydra.core.TypeApplication(value=at):
            return hydra.lib.lists.cons(at.argument, extract_type_application_args_go(at.function))
        
        case _:
            return ()

def extract_type_application_args(typ: hydra.core.Type) -> frozenlist[hydra.core.Type]:
    return hydra.lib.lists.reverse(extract_type_application_args_go(typ))

def dom_type_args(aliases: hydra.ext.java.helpers.Aliases, d: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, frozenlist[hydra.ext.java.syntax.TypeArgument]]:
    @lru_cache(1)
    def args() -> frozenlist[hydra.core.Type]:
        return extract_type_application_args(hydra.rewriting.deannotate_type(d))
    return hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(args())), (lambda : hydra.lib.flows.map_list((lambda t: hydra.lib.flows.bind(encode_type(aliases, hydra.lib.sets.empty(), t), (lambda jt: hydra.lib.flows.bind(hydra.ext.java.utils.java_type_to_java_reference_type(jt), (lambda rt: hydra.lib.flows.pure(cast(hydra.ext.java.syntax.TypeArgument, hydra.ext.java.syntax.TypeArgumentReference(rt)))))))), args())), (lambda : hydra.lib.flows.pure(java_type_arguments_for_type(d))))

def inner_class_ref(aliases: hydra.ext.java.helpers.Aliases, name: hydra.core.Name, local: str) -> hydra.ext.java.syntax.Identifier:
    @lru_cache(1)
    def id() -> str:
        return hydra.ext.java.utils.name_to_java_name(aliases, name).value
    return hydra.ext.java.syntax.Identifier(hydra.lib.strings.cat2(hydra.lib.strings.cat2(id(), "."), local))

def insert_branch_var(name: hydra.core.Name, env: hydra.ext.java.helpers.JavaEnvironment) -> hydra.ext.java.helpers.JavaEnvironment:
    @lru_cache(1)
    def aliases() -> hydra.ext.java.helpers.Aliases:
        return env.aliases
    return hydra.ext.java.helpers.JavaEnvironment(hydra.ext.java.helpers.Aliases(aliases().current_namespace, aliases().packages, hydra.lib.sets.insert(name, aliases().branch_vars), aliases().recursive_vars, aliases().in_scope_type_params, aliases().polymorphic_locals, aliases().in_scope_java_vars, aliases().var_renames, aliases().lambda_vars, aliases().type_var_subst, aliases().trusted_type_vars, aliases().method_codomain, aliases().thunked_vars), env.type_context)

def with_lambda(env: hydra.ext.java.helpers.JavaEnvironment, lam: hydra.core.Lambda, k: Callable[[hydra.ext.java.helpers.JavaEnvironment], T0]) -> T0:
    return hydra.schemas.with_lambda_context((lambda x1: java_env_get_t_c(x1)), (lambda x1, x2: java_env_set_t_c(x1, x2)), env, lam, (lambda env1: (aliases := env1.aliases, aliases2 := hydra.ext.java.helpers.Aliases(aliases.current_namespace, aliases.packages, aliases.branch_vars, aliases.recursive_vars, aliases.in_scope_type_params, aliases.polymorphic_locals, aliases.in_scope_java_vars, aliases.var_renames, hydra.lib.sets.insert(lam.parameter, aliases.lambda_vars), aliases.type_var_subst, aliases.trusted_type_vars, aliases.method_codomain, aliases.thunked_vars), env2 := hydra.ext.java.helpers.JavaEnvironment(aliases2, env1.type_context), k(env2))[3]))

def filter_phantom_type_args_filter_and_apply(all_type_args: frozenlist[hydra.core.Type], keep_flags: frozenlist[bool], overgen_subst: FrozenDict[hydra.core.Name, hydra.core.Type]) -> frozenlist[hydra.core.Type]:
    @lru_cache(1)
    def filtered() -> frozenlist[hydra.core.Type]:
        return hydra.lib.lists.map((lambda p: hydra.lib.pairs.first(p)), hydra.lib.lists.filter((lambda p: hydra.lib.pairs.second(p)), hydra.lib.lists.zip(all_type_args, keep_flags)))
    return hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.maps.null(overgen_subst)), (lambda : hydra.lib.lists.map((lambda t: substitute_type_vars_with_types(overgen_subst, t)), filtered())), (lambda : filtered()))

def filter_phantom_type_args(callee_name: hydra.core.Name, all_type_args: frozenlist[hydra.core.Type]) -> hydra.compute.Flow[hydra.graph.Graph, frozenlist[hydra.core.Type]]:
    return hydra.lib.flows.bind(hydra.lexical.dereference_element(callee_name), (lambda mel: hydra.lib.maybes.cases(mel, hydra.lib.flows.pure(all_type_args), (lambda el: hydra.lib.maybes.cases(el.type, hydra.lib.flows.pure(all_type_args), (lambda ts: (scheme_vars := hydra.lib.lists.filter((lambda v: is_simple_name(v)), ts.variables), scheme_type_vars := collect_type_vars(ts.type), scheme_type := ts.type, n_params := count_function_params(scheme_type), peeled := peel_domain_types(n_params, scheme_type), callee_doms := hydra.lib.pairs.first(peeled), callee_cod := hydra.lib.pairs.second(peeled), overgen_subst := detect_accumulator_unification(callee_doms, callee_cod, scheme_vars), keep_flags := hydra.lib.lists.map((lambda v: hydra.lib.logic.and_(hydra.lib.sets.member(v, scheme_type_vars), hydra.lib.logic.not_(hydra.lib.maps.member(v, overgen_subst)))), scheme_vars), hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.equality.equal(hydra.lib.lists.length(scheme_vars), hydra.lib.lists.length(all_type_args))), (lambda : hydra.lib.flows.pure(all_type_args)), (lambda : hydra.lib.flows.pure(filter_phantom_type_args_filter_and_apply(all_type_args, keep_flags, overgen_subst)))))[9]))))))

def element_java_identifier_qualify(aliases: hydra.ext.java.helpers.Aliases, mns: Maybe[hydra.module.Namespace], s: str) -> str:
    return hydra.ext.java.utils.name_to_java_name(aliases, hydra.names.unqualify_name(hydra.module.QualifiedName(mns, s))).value

def elements_class_name(ns: hydra.module.Namespace) -> str:
    @lru_cache(1)
    def ns_str() -> str:
        return ns.value
    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.strings.split_on(".", ns_str())
    return hydra.formatting.sanitize_with_underscores(hydra.ext.java.language.reserved_words(), hydra.formatting.capitalize(hydra.lib.lists.last(parts())))

def element_java_identifier(is_prim: bool, is_method: bool, aliases: hydra.ext.java.helpers.Aliases, name: hydra.core.Name) -> hydra.ext.java.syntax.Identifier:
    @lru_cache(1)
    def qn() -> hydra.module.QualifiedName:
        return hydra.names.qualify_name(name)
    @lru_cache(1)
    def ns_() -> Maybe[hydra.module.Namespace]:
        return qn().namespace
    @lru_cache(1)
    def local() -> str:
        return qn().local
    @lru_cache(1)
    def sep() -> str:
        return hydra.lib.logic.if_else(is_method, (lambda : "::"), (lambda : "."))
    return hydra.lib.logic.if_else(is_prim, (lambda : hydra.ext.java.syntax.Identifier(hydra.lib.strings.cat2(hydra.lib.strings.cat2(element_java_identifier_qualify(aliases, ns_(), hydra.formatting.capitalize(local())), "."), hydra.ext.java.names.apply_method_name))), (lambda : hydra.lib.maybes.cases(ns_(), hydra.ext.java.syntax.Identifier(hydra.ext.java.utils.sanitize_java_name(local())), (lambda n: hydra.ext.java.syntax.Identifier(hydra.lib.strings.cat2(hydra.lib.strings.cat2(element_java_identifier_qualify(aliases, Just(n), elements_class_name(n)), sep()), hydra.ext.java.utils.sanitize_java_name(local())))))))

def encode_variable_build_curried(params: frozenlist[hydra.core.Name], inner: hydra.ext.java.syntax.Expression) -> hydra.ext.java.syntax.Expression:
    return hydra.lib.logic.if_else(hydra.lib.lists.null(params), (lambda : inner), (lambda : hydra.ext.java.utils.java_lambda(hydra.lib.lists.head(params), encode_variable_build_curried(hydra.lib.lists.tail(params), inner))))

def encode_variable_hoisted_lambda_case(aliases: hydra.ext.java.helpers.Aliases, name: hydra.core.Name, arity: int) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.Expression]:
    @lru_cache(1)
    def param_names() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.map((lambda i: hydra.core.Name(hydra.lib.strings.cat2("p", hydra.lib.literals.show_int32(i)))), hydra.lib.math.range_(0, hydra.lib.math.sub(arity, 1)))
    @lru_cache(1)
    def param_exprs() -> frozenlist[hydra.ext.java.syntax.Expression]:
        return hydra.lib.lists.map((lambda pn: hydra.ext.java.utils.java_identifier_to_java_expression(hydra.ext.java.utils.variable_to_java_identifier(pn))), param_names())
    @lru_cache(1)
    def call() -> hydra.ext.java.syntax.Expression:
        return hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.utils.method_invocation(Nothing(), element_java_identifier(False, False, aliases, name), param_exprs()))
    @lru_cache(1)
    def lam() -> hydra.ext.java.syntax.Expression:
        return encode_variable_build_curried(param_names(), call())
    return hydra.lib.flows.bind(hydra.lexical.dereference_element(name), (lambda mel: hydra.lib.maybes.cases(mel, hydra.lib.flows.pure(lam()), (lambda el: hydra.lib.maybes.cases(el.type, hydra.lib.flows.pure(lam()), (lambda ts: (typ := ts.type, hydra.lib.flows.bind(encode_type(aliases, hydra.lib.sets.empty(), typ), (lambda jtype: hydra.lib.flows.bind(hydra.ext.java.utils.java_type_to_java_reference_type(jtype), (lambda rt: hydra.lib.flows.pure(hydra.ext.java.utils.java_cast_expression_to_java_expression(hydra.ext.java.utils.java_cast_expression(rt, hydra.ext.java.utils.java_expression_to_java_unary_expression(lam())))))))))[1]))))))

def is_lambda_bound_in_is_qualified(n: hydra.core.Name) -> bool:
    return hydra.lib.maybes.is_just(hydra.names.qualify_name(n).namespace)

def find_matching_lambda_var(name: hydra.core.Name, lambda_vars: frozenset[hydra.core.Name]) -> hydra.core.Name:
    return hydra.lib.logic.if_else(hydra.lib.sets.member(name, lambda_vars), (lambda : name), (lambda : hydra.lib.logic.if_else(is_lambda_bound_in_is_qualified(name), (lambda : hydra.lib.maybes.from_maybe(name, hydra.lib.lists.find((lambda lv: hydra.lib.logic.and_(is_lambda_bound_in_is_qualified(lv), hydra.lib.equality.equal(hydra.names.local_name_of(lv), hydra.names.local_name_of(name)))), hydra.lib.sets.to_list(lambda_vars)))), (lambda : hydra.lib.logic.if_else(hydra.lib.sets.member(hydra.core.Name(hydra.names.local_name_of(name)), lambda_vars), (lambda : hydra.core.Name(hydra.names.local_name_of(name))), (lambda : name))))))

def is_lambda_bound_in(name: hydra.core.Name, lambda_vars: frozenset[hydra.core.Name]) -> bool:
    return hydra.lib.logic.or_(hydra.lib.sets.member(name, lambda_vars), hydra.lib.logic.or_(hydra.lib.logic.and_(is_lambda_bound_in_is_qualified(name), hydra.lib.maybes.is_just(hydra.lib.lists.find((lambda lv: hydra.lib.logic.and_(is_lambda_bound_in_is_qualified(lv), hydra.lib.equality.equal(hydra.names.local_name_of(lv), hydra.names.local_name_of(name)))), hydra.lib.sets.to_list(lambda_vars)))), hydra.lib.logic.and_(hydra.lib.logic.not_(is_lambda_bound_in_is_qualified(name)), hydra.lib.sets.member(hydra.core.Name(hydra.names.local_name_of(name)), lambda_vars))))

def is_recursive_variable(aliases: hydra.ext.java.helpers.Aliases, name: hydra.core.Name) -> bool:
    return hydra.lib.sets.member(name, aliases.recursive_vars)

def encode_variable(env: hydra.ext.java.helpers.JavaEnvironment, name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.Expression]:
    @lru_cache(1)
    def aliases() -> hydra.ext.java.helpers.Aliases:
        return env.aliases
    @lru_cache(1)
    def jid() -> hydra.ext.java.syntax.Identifier:
        return hydra.ext.java.utils.java_identifier(name.value)
    def _hoist_body_1(v1: hydra.ext.java.helpers.JavaSymbolClass) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.Expression]:
        match v1:
            case hydra.ext.java.helpers.JavaSymbolClassHoistedLambda(value=arity):
                return encode_variable_hoisted_lambda_case(aliases(), name, arity)
            
            case hydra.ext.java.helpers.JavaSymbolClassLocalVariable():
                return hydra.lib.flows.pure(hydra.ext.java.utils.java_identifier_to_java_expression(element_java_identifier(False, False, aliases(), name)))
            
            case hydra.ext.java.helpers.JavaSymbolClassConstant():
                return hydra.lib.flows.pure(hydra.ext.java.utils.java_identifier_to_java_expression(element_java_identifier(False, False, aliases(), name)))
            
            case hydra.ext.java.helpers.JavaSymbolClassNullaryFunction():
                return hydra.lib.flows.pure(hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.utils.method_invocation(Nothing(), element_java_identifier(False, False, aliases(), name), ())))
            
            case hydra.ext.java.helpers.JavaSymbolClassUnaryFunction():
                return hydra.lib.flows.pure(hydra.ext.java.utils.java_identifier_to_java_expression(element_java_identifier(False, True, aliases(), name)))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.lib.logic.if_else(hydra.lib.sets.member(name, aliases().branch_vars), (lambda : hydra.lib.flows.pure(hydra.ext.java.utils.java_field_access_to_java_expression(hydra.ext.java.syntax.FieldAccess(cast(hydra.ext.java.syntax.FieldAccess_Qualifier, hydra.ext.java.syntax.FieldAccess_QualifierPrimary(hydra.ext.java.utils.java_expression_to_java_primary(hydra.ext.java.utils.java_identifier_to_java_expression(jid())))), hydra.ext.java.utils.java_identifier(hydra.ext.java.names.value_field_name))))), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.equal(name, hydra.core.Name(hydra.lib.strings.cat((hydra.ext.java.names.instance_name, "_", hydra.ext.java.names.value_field_name)))), is_recursive_variable(aliases(), name)), (lambda : (instance_expr := hydra.ext.java.utils.java_identifier_to_java_expression(hydra.ext.java.utils.java_identifier(hydra.ext.java.names.instance_name)), hydra.lib.flows.pure(hydra.ext.java.utils.java_field_access_to_java_expression(hydra.ext.java.syntax.FieldAccess(cast(hydra.ext.java.syntax.FieldAccess_Qualifier, hydra.ext.java.syntax.FieldAccess_QualifierPrimary(hydra.ext.java.utils.java_expression_to_java_primary(instance_expr))), hydra.ext.java.utils.java_identifier(hydra.ext.java.names.value_field_name)))))[1]), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.and_(is_recursive_variable(aliases(), name), hydra.lib.logic.not_(is_lambda_bound_in(name, aliases().lambda_vars))), (lambda : hydra.lib.flows.pure(hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.utils.method_invocation(Just(Left(hydra.ext.java.syntax.ExpressionName(Nothing(), jid()))), hydra.ext.java.syntax.Identifier(hydra.ext.java.names.get_method_name), ())))), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.sets.member(name, aliases().thunked_vars), hydra.lib.logic.not_(is_lambda_bound_in(name, aliases().lambda_vars))), (lambda : hydra.lib.flows.pure(hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.utils.method_invocation(Just(Left(hydra.ext.java.syntax.ExpressionName(Nothing(), jid()))), hydra.ext.java.syntax.Identifier(hydra.ext.java.names.get_method_name), ())))), (lambda : hydra.lib.logic.if_else(is_lambda_bound_in(name, aliases().lambda_vars), (lambda : (actual_name := find_matching_lambda_var(name, aliases().lambda_vars), hydra.lib.flows.pure(hydra.ext.java.utils.java_identifier_to_java_expression(hydra.ext.java.utils.variable_to_java_identifier(actual_name))))[1]), (lambda : hydra.lib.flows.bind(classify_data_reference(name), (lambda cls: _hoist_body_1(cls)))))))))))))

def is_local_variable(name: hydra.core.Name) -> bool:
    return hydra.lib.maybes.is_nothing(hydra.names.qualify_name(name).namespace)

def wrap_in_supplier_lambda(expr: hydra.ext.java.syntax.Expression) -> hydra.ext.java.syntax.Expression:
    return cast(hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.ExpressionLambda(hydra.ext.java.syntax.LambdaExpression(cast(hydra.ext.java.syntax.LambdaParameters, hydra.ext.java.syntax.LambdaParametersTuple(())), cast(hydra.ext.java.syntax.LambdaBody, hydra.ext.java.syntax.LambdaBodyExpression(expr)))))

def wrap_lazy_arguments(name: hydra.core.Name, args: frozenlist[hydra.ext.java.syntax.Expression]) -> tuple[frozenlist[hydra.ext.java.syntax.Expression], Maybe[str]]:
    return hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.equal(name, hydra.core.Name("hydra.lib.logic.ifElse")), hydra.lib.equality.equal(hydra.lib.lists.length(args), 3)), (lambda : ((hydra.lib.lists.at(0, args), wrap_in_supplier_lambda(hydra.lib.lists.at(1, args)), wrap_in_supplier_lambda(hydra.lib.lists.at(2, args))), Just("lazy"))), (lambda : (args, Nothing())))

def build_curried_lambda(params: frozenlist[hydra.core.Name], inner: hydra.ext.java.syntax.Expression) -> hydra.ext.java.syntax.Expression:
    return hydra.lib.lists.foldl((lambda acc, p: hydra.ext.java.utils.java_lambda(p, acc)), inner, hydra.lib.lists.reverse(params))

def encode_literal_lit_exp(l: hydra.ext.java.syntax.Literal) -> hydra.ext.java.syntax.Expression:
    return hydra.ext.java.utils.java_literal_to_java_expression(l)

def encode_literal_prim_cast(pt: hydra.ext.java.syntax.PrimitiveType, expr: hydra.ext.java.syntax.Expression) -> hydra.ext.java.syntax.Expression:
    return hydra.ext.java.utils.java_cast_expression_to_java_expression(hydra.ext.java.utils.java_cast_primitive(pt, hydra.ext.java.utils.java_expression_to_java_unary_expression(expr)))

def encode_literal(lit: hydra.core.Literal) -> hydra.ext.java.syntax.Expression:
    match lit:
        case hydra.core.LiteralBinary(value=bs):
            @lru_cache(1)
            def byte_values() -> frozenlist[int]:
                return hydra.lib.literals.binary_to_bytes(bs)
            return hydra.ext.java.utils.java_array_creation(hydra.ext.java.utils.java_byte_primitive_type(), Just(hydra.ext.java.utils.java_array_initializer(hydra.lib.lists.map((lambda w: hydra.ext.java.utils.java_literal_to_java_expression(cast(hydra.ext.java.syntax.Literal, hydra.ext.java.syntax.LiteralInteger(hydra.ext.java.syntax.IntegerLiteral(hydra.lib.literals.int32_to_bigint(w)))))), byte_values()))))
        
        case hydra.core.LiteralBoolean(value=b):
            return encode_literal_lit_exp(hydra.ext.java.utils.java_boolean(b))
        
        case hydra.core.LiteralFloat(value=f):
            return encode_literal_encode_float(f)
        
        case hydra.core.LiteralInteger(value=i):
            return encode_literal_encode_integer(i)
        
        case hydra.core.LiteralString(value=s):
            return encode_literal_lit_exp(hydra.ext.java.utils.java_string(s))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_literal_encode_float(f: hydra.core.FloatValue) -> hydra.ext.java.syntax.Expression:
    match f:
        case hydra.core.FloatValueBigfloat(value=v):
            return hydra.ext.java.utils.java_constructor_call(hydra.ext.java.utils.java_constructor_name(hydra.ext.java.syntax.Identifier("java.math.BigDecimal"), Nothing()), (encode_literal(cast(hydra.core.Literal, hydra.core.LiteralString(hydra.lib.literals.show_bigfloat(v)))),), Nothing())
        
        case hydra.core.FloatValueFloat32(value=v2):
            return encode_literal_prim_cast(cast(hydra.ext.java.syntax.PrimitiveType, hydra.ext.java.syntax.PrimitiveTypeNumeric(cast(hydra.ext.java.syntax.NumericType, hydra.ext.java.syntax.NumericTypeFloatingPoint(hydra.ext.java.syntax.FloatingPointType.FLOAT)))), encode_literal_lit_exp(cast(hydra.ext.java.syntax.Literal, hydra.ext.java.syntax.LiteralFloatingPoint(hydra.ext.java.syntax.FloatingPointLiteral(hydra.lib.literals.float32_to_bigfloat(v2))))))
        
        case hydra.core.FloatValueFloat64(value=v3):
            return encode_literal_lit_exp(cast(hydra.ext.java.syntax.Literal, hydra.ext.java.syntax.LiteralFloatingPoint(hydra.ext.java.syntax.FloatingPointLiteral(hydra.lib.literals.float64_to_bigfloat(v3)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_literal_encode_integer(i: hydra.core.IntegerValue) -> hydra.ext.java.syntax.Expression:
    match i:
        case hydra.core.IntegerValueBigint(value=v):
            return hydra.ext.java.utils.java_constructor_call(hydra.ext.java.utils.java_constructor_name(hydra.ext.java.syntax.Identifier("java.math.BigInteger"), Nothing()), (encode_literal(cast(hydra.core.Literal, hydra.core.LiteralString(hydra.lib.literals.show_bigint(v)))),), Nothing())
        
        case hydra.core.IntegerValueInt8(value=v2):
            return encode_literal_prim_cast(cast(hydra.ext.java.syntax.PrimitiveType, hydra.ext.java.syntax.PrimitiveTypeNumeric(cast(hydra.ext.java.syntax.NumericType, hydra.ext.java.syntax.NumericTypeIntegral(hydra.ext.java.syntax.IntegralType.BYTE)))), encode_literal_lit_exp(cast(hydra.ext.java.syntax.Literal, hydra.ext.java.syntax.LiteralInteger(hydra.ext.java.syntax.IntegerLiteral(hydra.lib.literals.int8_to_bigint(v2))))))
        
        case hydra.core.IntegerValueInt16(value=v3):
            return encode_literal_prim_cast(cast(hydra.ext.java.syntax.PrimitiveType, hydra.ext.java.syntax.PrimitiveTypeNumeric(cast(hydra.ext.java.syntax.NumericType, hydra.ext.java.syntax.NumericTypeIntegral(hydra.ext.java.syntax.IntegralType.SHORT)))), encode_literal_lit_exp(cast(hydra.ext.java.syntax.Literal, hydra.ext.java.syntax.LiteralInteger(hydra.ext.java.syntax.IntegerLiteral(hydra.lib.literals.int16_to_bigint(v3))))))
        
        case hydra.core.IntegerValueInt32(value=v4):
            return encode_literal_lit_exp(cast(hydra.ext.java.syntax.Literal, hydra.ext.java.syntax.LiteralInteger(hydra.ext.java.syntax.IntegerLiteral(hydra.lib.literals.int32_to_bigint(v4)))))
        
        case hydra.core.IntegerValueInt64(value=v5):
            return encode_literal_prim_cast(cast(hydra.ext.java.syntax.PrimitiveType, hydra.ext.java.syntax.PrimitiveTypeNumeric(cast(hydra.ext.java.syntax.NumericType, hydra.ext.java.syntax.NumericTypeIntegral(hydra.ext.java.syntax.IntegralType.LONG)))), encode_literal_lit_exp(cast(hydra.ext.java.syntax.Literal, hydra.ext.java.syntax.LiteralInteger(hydra.ext.java.syntax.IntegerLiteral(hydra.lib.literals.int64_to_bigint(v5))))))
        
        case hydra.core.IntegerValueUint8(value=v6):
            return encode_literal_prim_cast(cast(hydra.ext.java.syntax.PrimitiveType, hydra.ext.java.syntax.PrimitiveTypeNumeric(cast(hydra.ext.java.syntax.NumericType, hydra.ext.java.syntax.NumericTypeIntegral(hydra.ext.java.syntax.IntegralType.SHORT)))), encode_literal_lit_exp(cast(hydra.ext.java.syntax.Literal, hydra.ext.java.syntax.LiteralInteger(hydra.ext.java.syntax.IntegerLiteral(hydra.lib.literals.uint8_to_bigint(v6))))))
        
        case hydra.core.IntegerValueUint16(value=v7):
            return encode_literal_lit_exp(cast(hydra.ext.java.syntax.Literal, hydra.ext.java.syntax.LiteralCharacter(v7)))
        
        case hydra.core.IntegerValueUint32(value=v8):
            return encode_literal_prim_cast(cast(hydra.ext.java.syntax.PrimitiveType, hydra.ext.java.syntax.PrimitiveTypeNumeric(cast(hydra.ext.java.syntax.NumericType, hydra.ext.java.syntax.NumericTypeIntegral(hydra.ext.java.syntax.IntegralType.LONG)))), encode_literal_lit_exp(cast(hydra.ext.java.syntax.Literal, hydra.ext.java.syntax.LiteralInteger(hydra.ext.java.syntax.IntegerLiteral(hydra.lib.literals.uint32_to_bigint(v8))))))
        
        case hydra.core.IntegerValueUint64(value=v9):
            return hydra.ext.java.utils.java_constructor_call(hydra.ext.java.utils.java_constructor_name(hydra.ext.java.syntax.Identifier("java.math.BigInteger"), Nothing()), (encode_literal(cast(hydra.core.Literal, hydra.core.LiteralString(hydra.lib.literals.show_bigint(hydra.lib.literals.uint64_to_bigint(v9))))),), Nothing())
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_nullary_constant_type_args_from_return_type(aliases: hydra.ext.java.helpers.Aliases, t: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, frozenlist[hydra.ext.java.syntax.TypeArgument]]:
    match hydra.rewriting.deannotate_type(t):
        case hydra.core.TypeSet(value=st):
            return hydra.lib.flows.bind(encode_type(aliases, hydra.lib.sets.empty(), st), (lambda jst: hydra.lib.flows.bind(hydra.ext.java.utils.java_type_to_java_reference_type(jst), (lambda rt: hydra.lib.flows.pure((cast(hydra.ext.java.syntax.TypeArgument, hydra.ext.java.syntax.TypeArgumentReference(rt)),))))))
        
        case hydra.core.TypeList(value=lt_):
            return hydra.lib.flows.bind(encode_type(aliases, hydra.lib.sets.empty(), lt_), (lambda jlt: hydra.lib.flows.bind(hydra.ext.java.utils.java_type_to_java_reference_type(jlt), (lambda rt: hydra.lib.flows.pure((cast(hydra.ext.java.syntax.TypeArgument, hydra.ext.java.syntax.TypeArgumentReference(rt)),))))))
        
        case hydra.core.TypeMaybe(value=mt):
            return hydra.lib.flows.bind(encode_type(aliases, hydra.lib.sets.empty(), mt), (lambda jmt: hydra.lib.flows.bind(hydra.ext.java.utils.java_type_to_java_reference_type(jmt), (lambda rt: hydra.lib.flows.pure((cast(hydra.ext.java.syntax.TypeArgument, hydra.ext.java.syntax.TypeArgumentReference(rt)),))))))
        
        case hydra.core.TypeMap(value=mp):
            return hydra.lib.flows.bind(encode_type(aliases, hydra.lib.sets.empty(), mp.keys), (lambda jkt: hydra.lib.flows.bind(hydra.ext.java.utils.java_type_to_java_reference_type(jkt), (lambda rk: hydra.lib.flows.bind(encode_type(aliases, hydra.lib.sets.empty(), mp.values), (lambda jvt: hydra.lib.flows.bind(hydra.ext.java.utils.java_type_to_java_reference_type(jvt), (lambda rv: hydra.lib.flows.pure((cast(hydra.ext.java.syntax.TypeArgument, hydra.ext.java.syntax.TypeArgumentReference(rk)), cast(hydra.ext.java.syntax.TypeArgument, hydra.ext.java.syntax.TypeArgumentReference(rv))))))))))))
        
        case _:
            return hydra.lib.flows.pure(())

def encode_nullary_constant(env: hydra.ext.java.helpers.JavaEnvironment, typ: hydra.core.Type, fun: hydra.core.Function) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.Expression]:
    @lru_cache(1)
    def aliases() -> hydra.ext.java.helpers.Aliases:
        return env.aliases
    match fun:
        case hydra.core.FunctionPrimitive(value=name):
            return hydra.lib.flows.bind(encode_nullary_constant_type_args_from_return_type(aliases(), typ), (lambda targs: hydra.lib.logic.if_else(hydra.lib.lists.null(targs), (lambda : (header := cast(hydra.ext.java.syntax.MethodInvocation_Header, hydra.ext.java.syntax.MethodInvocation_HeaderSimple(hydra.ext.java.syntax.MethodName(element_java_identifier(True, False, aliases(), name)))), hydra.lib.flows.pure(hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.syntax.MethodInvocation(header, ()))))[1]), (lambda : (full_name := element_java_identifier(True, False, aliases(), name).value, (parts := hydra.lib.strings.split_on(".", full_name), (class_name := hydra.ext.java.syntax.Identifier(hydra.lib.strings.intercalate(".", hydra.lib.lists.init(parts))), (method_name := hydra.ext.java.syntax.Identifier(hydra.lib.lists.last(parts)), hydra.lib.flows.pure(hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.utils.method_invocation_static_with_type_args(class_name, method_name, targs, ()))))[1])[1])[1])[1]))))
        
        case _:
            return hydra.monads.unexpected("nullary function", hydra.show.core.function(fun))

def is_field_unit_type(type_name: hydra.core.Name, field_name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, bool]:
    return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda g: hydra.lib.flows.bind(hydra.schemas.graph_to_inference_context(g), (lambda ix: (schema_types := ix.schema_types, _hoist_body_1 := (lambda v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lib.maybes.cases(hydra.lib.maps.lookup(type_name, schema_types), hydra.lib.flows.pure(False), (lambda ts: _hoist_body_1(hydra.rewriting.deannotate_type(ts.type)))))[2]))))

def take_type_args(label: str, n: int, tyapps: frozenlist[hydra.ext.java.syntax.Type]) -> hydra.compute.Flow[T0, frozenlist[hydra.ext.java.syntax.TypeArgument]]:
    return hydra.lib.logic.if_else(hydra.lib.equality.lt(hydra.lib.lists.length(tyapps), n), (lambda : hydra.monads.unexpected(hydra.lib.strings.cat(("needed type arguments for ", label, ", found too few")), "takeTypeArgs")), (lambda : hydra.lib.flows.map_list((lambda jt: hydra.lib.flows.bind(hydra.ext.java.utils.java_type_to_java_reference_type(jt), (lambda rt: hydra.lib.flows.pure(cast(hydra.ext.java.syntax.TypeArgument, hydra.ext.java.syntax.TypeArgumentReference(rt)))))), hydra.lib.lists.take(n, tyapps))))

def decode_type_from_term(term: hydra.core.Term) -> Maybe[hydra.core.Type]:
    match hydra.rewriting.deannotate_term(term):
        case hydra.core.TermUnion(value=inj):
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(inj.type_name, hydra.core.Name("hydra.core.Type")), (lambda : (fname := inj.field.name.value, (fterm := inj.field.term, (_hoist_body_1 := (lambda v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), _hoist_body_2 := (lambda v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), _hoist_body_3 := (lambda v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), _hoist_body_4 := (lambda v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), _hoist_body_5 := (lambda v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), _hoist_body_6 := (lambda v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), _hoist_body_7 := (lambda v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lib.logic.if_else(hydra.lib.equality.equal(fname, "variable"), (lambda : _hoist_body_3(fterm)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(fname, "annotated"), (lambda : _hoist_body_4(fterm)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(fname, "application"), (lambda : _hoist_body_5(fterm)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(fname, "function"), (lambda : _hoist_body_6(fterm)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(fname, "literal"), (lambda : _hoist_body_7(fterm)), (lambda : Nothing())))))))))))[7])[1])[1]), (lambda : Nothing()))
        
        case _:
            return Nothing()

def try_infer_function_type(fun: hydra.core.Function) -> Maybe[hydra.core.Type]:
    match fun:
        case hydra.core.FunctionLambda(value=lam):
            return hydra.lib.maybes.bind(lam.domain, (lambda dom: (m_cod := hydra.dsl.python.unsupported("inline match expressions are not yet supported"), hydra.lib.maybes.map((lambda cod: cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(dom, cod)))), m_cod))[1]))
        
        case _:
            return Nothing()

def with_type_lambda(v1: hydra.ext.java.helpers.JavaEnvironment, v2: hydra.core.TypeLambda, v3: Callable[[hydra.ext.java.helpers.JavaEnvironment], T0]) -> T0:
    return hydra.schemas.with_type_lambda_context((lambda x1: java_env_get_t_c(x1)), (lambda x1, x2: java_env_set_t_c(x1, x2)), v1, v2, v3)

def bindings_to_statements(env: hydra.ext.java.helpers.JavaEnvironment, bindings: frozenlist[hydra.core.Binding]) -> hydra.compute.Flow[hydra.graph.Graph, tuple[frozenlist[hydra.ext.java.syntax.BlockStatement], hydra.ext.java.helpers.JavaEnvironment]]:
    @lru_cache(1)
    def aliases() -> hydra.ext.java.helpers.Aliases:
        return env.aliases
    @lru_cache(1)
    def tc() -> hydra.typing.TypeContext:
        return env.type_context
    @lru_cache(1)
    def flat_bindings() -> frozenlist[hydra.core.Binding]:
        return dedup_bindings(aliases().in_scope_java_vars, flatten_bindings(bindings))
    @lru_cache(1)
    def tc_extended() -> hydra.typing.TypeContext:
        return hydra.schemas.extend_type_context_for_let((lambda x1, x2: hydra.coder_utils.binding_metadata(x1, x2)), tc(), hydra.core.Let(flat_bindings(), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("dummy")))))
    @lru_cache(1)
    def binding_vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(hydra.lib.lists.map((lambda b: b.name), flat_bindings()))
    @lru_cache(1)
    def all_deps() -> FrozenDict[hydra.core.Name, frozenset[hydra.core.Name]]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda b: (key := b.name, deps := hydra.lib.sets.intersection(binding_vars(), hydra.rewriting.free_variables_in_term(b.term)), (key, deps))[2]), flat_bindings()))
    @lru_cache(1)
    def sorted() -> frozenlist[frozenlist[hydra.core.Name]]:
        return hydra.sorting.topological_sort_components(hydra.lib.lists.map((lambda entry: (key := hydra.lib.pairs.first(entry), deps := hydra.lib.pairs.second(entry), (key, hydra.lib.sets.to_list(deps)))[2]), hydra.lib.maps.to_list(all_deps())))
    @lru_cache(1)
    def recursive_vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(hydra.lib.lists.concat(hydra.lib.lists.map((lambda names: hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(names), 1), (lambda : (single_name := hydra.lib.lists.head(names), hydra.lib.maybes.cases(hydra.lib.maps.lookup(single_name, all_deps()), (), (lambda deps: hydra.lib.logic.if_else(hydra.lib.sets.member(single_name, deps), (lambda : (single_name,)), (lambda : ())))))[1]), (lambda : names))), sorted())))
    @lru_cache(1)
    def thunked_vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(hydra.lib.lists.concat(hydra.lib.lists.map((lambda b: (bname := b.name, hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.logic.not_(hydra.lib.sets.member(bname, recursive_vars())), hydra.lib.logic.and_(needs_thunking(b.term), hydra.lib.logic.not_(binding_is_function_type(b)))), (lambda : (bname,)), (lambda : ())))[1]), flat_bindings())))
    @lru_cache(1)
    def aliases_extended() -> hydra.ext.java.helpers.Aliases:
        return hydra.ext.java.helpers.Aliases(aliases().current_namespace, aliases().packages, aliases().branch_vars, hydra.lib.sets.union(aliases().recursive_vars, recursive_vars()), aliases().in_scope_type_params, aliases().polymorphic_locals, hydra.lib.sets.union(aliases().in_scope_java_vars, binding_vars()), aliases().var_renames, aliases().lambda_vars, aliases().type_var_subst, aliases().trusted_type_vars, aliases().method_codomain, hydra.lib.sets.union(aliases().thunked_vars, thunked_vars()))
    @lru_cache(1)
    def env_extended() -> hydra.ext.java.helpers.JavaEnvironment:
        return hydra.ext.java.helpers.JavaEnvironment(aliases_extended(), tc_extended())
    return hydra.lib.logic.if_else(hydra.lib.lists.null(bindings), (lambda : hydra.lib.flows.pure(((), env_extended()))), (lambda : hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda names: hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda n: to_decl_init(aliases_extended(), tc_extended(), recursive_vars(), flat_bindings(), n)), names), (lambda inits: hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda n: to_decl_statement(env_extended(), aliases_extended(), tc_extended(), recursive_vars(), thunked_vars(), flat_bindings(), n)), names), (lambda decls: hydra.lib.flows.pure(hydra.lib.lists.concat2(hydra.lib.maybes.cat(inits), decls))))))), sorted()), (lambda groups: hydra.lib.flows.pure((hydra.lib.lists.concat(groups), env_extended()))))))

def encode_application(env: hydra.ext.java.helpers.JavaEnvironment, app: hydra.core.Application) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.Expression]:
    @lru_cache(1)
    def aliases() -> hydra.ext.java.helpers.Aliases:
        return env.aliases
    @lru_cache(1)
    def tc() -> hydra.typing.TypeContext:
        return env.type_context
    @lru_cache(1)
    def gathered() -> tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Term], frozenlist[hydra.core.Type]]]:
        return hydra.coder_utils.gather_args_with_type_apps(cast(hydra.core.Term, hydra.core.TermApplication(app)), (), ())
    @lru_cache(1)
    def fun() -> hydra.core.Term:
        return hydra.lib.pairs.first(gathered())
    @lru_cache(1)
    def args() -> frozenlist[hydra.core.Term]:
        return hydra.lib.pairs.first(hydra.lib.pairs.second(gathered()))
    @lru_cache(1)
    def type_apps() -> frozenlist[hydra.core.Type]:
        return hydra.lib.pairs.second(hydra.lib.pairs.second(gathered()))
    return hydra.lib.flows.bind(hydra.annotations.get_type(hydra.annotations.term_annotation_internal(fun())), (lambda mfun_typ: hydra.lib.flows.bind(hydra.lib.maybes.cases(mfun_typ, hydra.coder_utils.try_type_of("1", tc(), fun()), (lambda t: hydra.lib.flows.pure(t))), (lambda fun_typ: (arity := hydra.arity.type_arity(fun_typ), deannotated_fun := hydra.rewriting.deannotate_term(fun()), callee_name := (_hoist_callee_name_1 := (lambda v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.dsl.python.unsupported("inline match expressions are not yet supported"))[1], _hoist_body_1 := (lambda annotated_args, v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), _hoist_body_2 := (lambda annotated_args, v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lib.flows.bind(hydra.lib.maybes.cases(callee_name, hydra.lib.flows.pure(args()), (lambda cname: annotate_lambda_args(cname, type_apps(), args()))), (lambda annotated_args: _hoist_body_2(annotated_args, deannotated_fun))))[5]))))

def encode_application_fallback(env: hydra.ext.java.helpers.JavaEnvironment, aliases: hydra.ext.java.helpers.Aliases, tc: hydra.typing.TypeContext, type_apps: frozenlist[hydra.core.Type], lhs2: hydra.core.Term, rhs2: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.Expression]:
    def _hoist_hydra_ext_java_coder_encode_application_fallback_1(env: hydra.ext.java.helpers.JavaEnvironment, lhs2: hydra.core.Term, rhs2: hydra.core.Term, t: hydra.core.Type, tc: hydra.typing.TypeContext, v1: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.Expression]:
        match v1:
            case hydra.core.TypeFunction(value=ft):
                @lru_cache(1)
                def dom() -> hydra.core.Type:
                    return ft.domain
                @lru_cache(1)
                def cod() -> hydra.core.Type:
                    return ft.codomain
                def _hoist_body_1(v12: hydra.core.Function) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.Expression]:
                    match v12:
                        case hydra.core.FunctionElimination(value=e):
                            return hydra.lib.flows.bind(encode_term(env, rhs2), (lambda jarg: hydra.lib.flows.bind(hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(java_type_arguments_for_type(dom()))), (lambda : hydra.lib.flows.pure(dom())), (lambda : hydra.lib.flows.bind(hydra.annotations.get_type(hydra.annotations.term_annotation_internal(rhs2)), (lambda mrt: hydra.lib.maybes.cases(mrt, hydra.lib.flows.bind(hydra.coder_utils.try_type_of("dom-enrich", tc, rhs2), (lambda rt: hydra.lib.flows.pure(hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(java_type_arguments_for_type(rt))), (lambda : rt), (lambda : dom()))))), (lambda rt: hydra.lib.flows.pure(hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(java_type_arguments_for_type(rt))), (lambda : rt), (lambda : dom()))))))))), (lambda enriched_dom: encode_elimination(env, Just(jarg), enriched_dom, cod(), e)))))
                        
                        case _:
                            return hydra.lib.flows.bind(encode_term(env, lhs2), (lambda jfun: hydra.lib.flows.bind(encode_term(env, rhs2), (lambda jarg: hydra.lib.flows.pure(apply_java_arg(jfun, jarg))))))
                match hydra.rewriting.deannotate_term(lhs2):
                    case hydra.core.TermFunction(value=f):
                        return _hoist_body_1(f)
                    
                    case _:
                        return hydra.lib.flows.bind(encode_term(env, lhs2), (lambda jfun: hydra.lib.flows.bind(encode_term(env, rhs2), (lambda jarg: hydra.lib.flows.pure(apply_java_arg(jfun, jarg))))))
            
            case _:
                return hydra.monads.fail(hydra.lib.strings.cat(("Unexpected type: ", hydra.show.core.type(t))))
    return hydra.monads.with_trace("fallback", hydra.lib.flows.bind(hydra.annotations.get_type(hydra.annotations.term_annotation_internal(lhs2)), (lambda mt: hydra.lib.flows.bind(hydra.lib.maybes.cases(mt, hydra.coder_utils.try_type_of("2", tc, lhs2), (lambda typ: hydra.lib.flows.pure(typ))), (lambda t: _hoist_hydra_ext_java_coder_encode_application_fallback_1(env, lhs2, rhs2, t, tc, hydra.rewriting.deannotate_type_parameters(hydra.rewriting.deannotate_type(t))))))))

def encode_elimination(env: hydra.ext.java.helpers.JavaEnvironment, marg: Maybe[hydra.ext.java.syntax.Expression], dom: hydra.core.Type, cod: hydra.core.Type, elm: hydra.core.Elimination) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.Expression]:
    @lru_cache(1)
    def aliases() -> hydra.ext.java.helpers.Aliases:
        return env.aliases
    match elm:
        case hydra.core.EliminationRecord(value=proj):
            @lru_cache(1)
            def fname() -> hydra.core.Name:
                return proj.field
            return hydra.lib.flows.bind(encode_type(aliases(), hydra.lib.sets.empty(), dom), (lambda jdom0: hydra.lib.flows.bind(hydra.ext.java.utils.java_type_to_java_reference_type(jdom0), (lambda jdomr: hydra.lib.maybes.cases(marg, (proj_var := hydra.core.Name("projected"), (jbody := hydra.ext.java.utils.java_expression_name_to_java_expression(hydra.ext.java.utils.field_expression(hydra.ext.java.utils.variable_to_java_identifier(proj_var), hydra.ext.java.utils.java_identifier(fname().value))), hydra.lib.flows.pure(hydra.ext.java.utils.java_lambda(proj_var, jbody)))[1])[1], (lambda jarg: (qual := cast(hydra.ext.java.syntax.FieldAccess_Qualifier, hydra.ext.java.syntax.FieldAccess_QualifierPrimary(hydra.ext.java.utils.java_expression_to_java_primary(jarg))), hydra.lib.flows.pure(hydra.ext.java.utils.java_field_access_to_java_expression(hydra.ext.java.syntax.FieldAccess(qual, hydra.ext.java.utils.java_identifier(fname().value)))))[1]))))))
        
        case hydra.core.EliminationUnion(value=cs):
            @lru_cache(1)
            def tname() -> hydra.core.Name:
                return cs.type_name
            @lru_cache(1)
            def def_() -> Maybe[hydra.core.Term]:
                return cs.default
            @lru_cache(1)
            def fields() -> frozenlist[hydra.core.Field]:
                return cs.cases
            return hydra.lib.maybes.cases(marg, (u_var := hydra.core.Name("u"), (typed_lambda := cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(u_var, Just(dom), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(elm)))), cast(hydra.core.Term, hydra.core.TermVariable(u_var)))))))))), encode_term(env, typed_lambda))[1])[1], (lambda jarg: (prim := hydra.ext.java.utils.java_expression_to_java_primary(jarg), cons_id := inner_class_ref(aliases(), tname(), hydra.ext.java.names.partial_visitor_name), hydra.lib.flows.bind(encode_type(aliases(), hydra.lib.sets.empty(), cod), (lambda jcod: hydra.lib.flows.bind(hydra.ext.java.utils.java_type_to_java_reference_type(jcod), (lambda rt: hydra.lib.flows.bind(dom_type_args(aliases(), dom), (lambda dom_args: (targs := type_args_or_diamond(hydra.lib.lists.concat2(dom_args, (cast(hydra.ext.java.syntax.TypeArgument, hydra.ext.java.syntax.TypeArgumentReference(rt)),))), hydra.lib.flows.bind(hydra.lib.maybes.cases(def_(), hydra.lib.flows.pure(()), (lambda d: hydra.lib.flows.bind(otherwise_branch(env, aliases(), dom, cod, tname(), jcod, dom_args, d), (lambda b: hydra.lib.flows.pure((b,)))))), (lambda otherwise_branches: hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda f: visit_branch(env, aliases(), dom, tname(), jcod, dom_args, f)), fields()), (lambda visit_branches: (body := hydra.ext.java.syntax.ClassBody(hydra.lib.lists.concat2(otherwise_branches, visit_branches)), visitor := hydra.ext.java.utils.java_constructor_call(hydra.ext.java.utils.java_constructor_name(cons_id, Just(targs)), (), Just(body)), hydra.lib.flows.pure(hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.utils.method_invocation(Just(Right(prim)), hydra.ext.java.syntax.Identifier(hydra.ext.java.names.accept_method_name), (visitor,)))))[2])))))[1])))))))[2]))
        
        case hydra.core.EliminationWrap():
            def with_arg(ja: hydra.ext.java.syntax.Expression) -> hydra.ext.java.syntax.Expression:
                return hydra.ext.java.utils.java_field_access_to_java_expression(hydra.ext.java.syntax.FieldAccess(cast(hydra.ext.java.syntax.FieldAccess_Qualifier, hydra.ext.java.syntax.FieldAccess_QualifierPrimary(hydra.ext.java.utils.java_expression_to_java_primary(ja))), hydra.ext.java.utils.java_identifier(hydra.ext.java.names.value_field_name)))
            return hydra.lib.flows.pure(hydra.lib.maybes.cases(marg, (w_var := hydra.core.Name("wrapped"), (w_arg := hydra.ext.java.utils.java_identifier_to_java_expression(hydra.ext.java.utils.variable_to_java_identifier(w_var)), hydra.ext.java.utils.java_lambda(w_var, with_arg(w_arg)))[1])[1], (lambda jarg: with_arg(jarg))))
        
        case _:
            return hydra.monads.unexpected("elimination case", "encodeElimination")

def encode_function(env: hydra.ext.java.helpers.JavaEnvironment, dom: hydra.core.Type, cod: hydra.core.Type, fun: hydra.core.Function) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.Expression]:
    @lru_cache(1)
    def aliases() -> hydra.ext.java.helpers.Aliases:
        return env.aliases
    match fun:
        case hydra.core.FunctionElimination(value=elm):
            return hydra.monads.with_trace(hydra.lib.strings.cat(("elimination (", hydra.show.core.elimination(elm), ")")), encode_elimination(env, Nothing(), dom, cod, elm))
        
        case hydra.core.FunctionLambda(value=lam):
            return hydra.monads.with_trace(hydra.lib.strings.cat2("lambda ", lam.parameter.value), with_lambda(env, lam, (lambda env2: (lambda_var := lam.parameter, body := lam.body, _hoist_body_1 := (lambda inner_lam, v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), _hoist_body_2 := (lambda v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.dsl.python.unsupported("inline match expressions are not yet supported"))[4])))
        
        case hydra.core.FunctionPrimitive(value=name):
            @lru_cache(1)
            def class_with_apply() -> str:
                return element_java_identifier(True, False, aliases(), name).value
            suffix = hydra.lib.strings.cat2(".", hydra.ext.java.names.apply_method_name)
            @lru_cache(1)
            def class_name() -> str:
                return hydra.lib.strings.from_list(hydra.lib.lists.take(hydra.lib.math.sub(hydra.lib.strings.length(class_with_apply()), hydra.lib.strings.length(suffix)), hydra.lib.strings.to_list(class_with_apply())))
            @lru_cache(1)
            def arity() -> int:
                return hydra.arity.type_arity(cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(dom, cod))))
            return hydra.lib.logic.if_else(hydra.lib.equality.lte(arity(), 1), (lambda : hydra.lib.flows.pure(hydra.ext.java.utils.java_identifier_to_java_expression(hydra.ext.java.syntax.Identifier(hydra.lib.strings.cat((class_name(), "::", hydra.ext.java.names.apply_method_name)))))), (lambda : (param_names := hydra.lib.lists.map((lambda i: hydra.core.Name(hydra.lib.strings.cat2("p", hydra.lib.literals.show_int32(i)))), hydra.lib.math.range_(0, hydra.lib.math.sub(arity(), 1))), (param_exprs := hydra.lib.lists.map((lambda p: hydra.ext.java.utils.java_identifier_to_java_expression(hydra.ext.java.utils.variable_to_java_identifier(p))), param_names), (class_id := hydra.ext.java.syntax.Identifier(class_name()), (call := hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.utils.method_invocation_static(class_id, hydra.ext.java.syntax.Identifier(hydra.ext.java.names.apply_method_name), param_exprs)), (curried := build_curried_lambda(param_names, call), hydra.lib.flows.bind(encode_type(aliases(), hydra.lib.sets.empty(), cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(dom, cod)))), (lambda jtype: hydra.lib.flows.bind(hydra.ext.java.utils.java_type_to_java_reference_type(jtype), (lambda rt: hydra.lib.flows.pure(hydra.ext.java.utils.java_cast_expression_to_java_expression(hydra.ext.java.utils.java_cast_expression(rt, hydra.ext.java.utils.java_expression_to_java_unary_expression(curried)))))))))[1])[1])[1])[1])[1]))
        
        case _:
            return hydra.lib.flows.pure(encode_literal(cast(hydra.core.Literal, hydra.core.LiteralString(hydra.lib.strings.cat2("Unimplemented function variant: ", hydra.show.core.function(fun))))))

def encode_term(env: hydra.ext.java.helpers.JavaEnvironment, term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.Expression]:
    return encode_term_internal(env, (), (), term)

def encode_term_internal(env: hydra.ext.java.helpers.JavaEnvironment, anns: frozenlist[FrozenDict[hydra.core.Name, hydra.core.Term]], tyapps: frozenlist[hydra.ext.java.syntax.Type], term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.Expression]:
    @lru_cache(1)
    def aliases() -> hydra.ext.java.helpers.Aliases:
        return env.aliases
    @lru_cache(1)
    def tc() -> hydra.typing.TypeContext:
        return env.type_context
    def encode(t: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.Expression]:
        return encode_term(env, t)
    match term:
        case hydra.core.TermAnnotated(value=at):
            return encode_term_internal(env, hydra.lib.lists.cons(at.annotation, anns), tyapps, at.body)
        
        case hydra.core.TermApplication(value=app):
            return hydra.monads.with_trace("encode application", encode_application(env, app))
        
        case hydra.core.TermEither(value=et):
            return hydra.lib.flows.bind(take_type_args("either", 2, tyapps), (lambda targs: hydra.lib.eithers.either((lambda term1: hydra.lib.flows.bind(encode(term1), (lambda expr: hydra.lib.flows.pure(hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.utils.method_invocation_static_with_type_args(hydra.ext.java.syntax.Identifier("hydra.util.Either"), hydra.ext.java.syntax.Identifier("left"), targs, (expr,))))))), (lambda term1: hydra.lib.flows.bind(encode(term1), (lambda expr: hydra.lib.flows.pure(hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.utils.method_invocation_static_with_type_args(hydra.ext.java.syntax.Identifier("hydra.util.Either"), hydra.ext.java.syntax.Identifier("right"), targs, (expr,))))))), et)))
        
        case hydra.core.TermFunction(value=f):
            return hydra.monads.with_trace(hydra.lib.strings.cat2("encode function (", hydra.lib.strings.cat2(hydra.show.core.function(f), ")")), (combined_anns := hydra.lib.lists.foldl((lambda acc, m: hydra.lib.maps.union(acc, m)), hydra.lib.maps.empty(), anns), (_hoist_body_1 := (lambda typ, v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lib.flows.bind(hydra.annotations.get_type(combined_anns), (lambda mt: hydra.lib.flows.bind(hydra.lib.maybes.cases(mt, hydra.lib.maybes.cases(try_infer_function_type(f), hydra.coder_utils.try_type_of("4", tc(), term), (lambda inferred_type: hydra.lib.flows.pure(inferred_type))), (lambda t: hydra.lib.flows.pure(t))), (lambda typ: _hoist_body_1(typ, hydra.rewriting.deannotate_type(typ)))))))[1])[1])
        
        case hydra.core.TermLet(value=lt):
            return hydra.monads.with_trace("encode let as block", (bindings := lt.bindings, (body := lt.body, hydra.lib.logic.if_else(hydra.lib.lists.null(bindings), (lambda : encode(body)), (lambda : hydra.lib.flows.bind(bindings_to_statements(env, bindings), (lambda bind_result: (binding_stmts := hydra.lib.pairs.first(bind_result), env2 := hydra.lib.pairs.second(bind_result), hydra.lib.flows.bind(encode_term(env2, body), (lambda jbody: (return_st := cast(hydra.ext.java.syntax.BlockStatement, hydra.ext.java.syntax.BlockStatementStatement(hydra.ext.java.utils.java_return_statement(Just(jbody)))), block := hydra.ext.java.syntax.Block(hydra.lib.lists.concat2(binding_stmts, (return_st,))), nullary_lambda := cast(hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.ExpressionLambda(hydra.ext.java.syntax.LambdaExpression(cast(hydra.ext.java.syntax.LambdaParameters, hydra.ext.java.syntax.LambdaParametersTuple(())), cast(hydra.ext.java.syntax.LambdaBody, hydra.ext.java.syntax.LambdaBodyBlock(block))))), combined_anns := hydra.lib.lists.foldl((lambda acc, m: hydra.lib.maps.union(acc, m)), hydra.lib.maps.empty(), anns), tc2 := env2.type_context, aliases2 := env2.aliases, hydra.lib.flows.bind(hydra.annotations.get_type(combined_anns), (lambda mt: hydra.lib.flows.bind(hydra.lib.maybes.cases(mt, hydra.coder_utils.try_type_of("let-body", tc2, body), (lambda t: hydra.lib.flows.pure(t))), (lambda let_type: hydra.lib.flows.bind(encode_type(aliases2, hydra.lib.sets.empty(), let_type), (lambda j_let_type: hydra.lib.flows.bind(hydra.ext.java.utils.java_type_to_java_reference_type(j_let_type), (lambda rt: (supplier_rt := cast(hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.ReferenceTypeClassOrInterface(cast(hydra.ext.java.syntax.ClassOrInterfaceType, hydra.ext.java.syntax.ClassOrInterfaceTypeClass(hydra.ext.java.utils.java_class_type((rt,), hydra.ext.java.names.java_util_function_package_name, "Supplier"))))), cast_expr := hydra.ext.java.utils.java_cast_expression_to_java_expression(hydra.ext.java.utils.java_cast_expression(supplier_rt, hydra.ext.java.utils.java_expression_to_java_unary_expression(nullary_lambda))), hydra.lib.flows.pure(hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.utils.method_invocation(Just(Right(hydra.ext.java.utils.java_expression_to_java_primary(cast_expr))), hydra.ext.java.syntax.Identifier("get"), ()))))[2])))))))))[6])))[2])))))[1])[1])
        
        case hydra.core.TermList(value=els):
            return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda x1: encode(x1)), els), (lambda jels: hydra.lib.flows.bind(hydra.lib.logic.if_else(hydra.lib.lists.null(jels), (lambda : take_type_args("list", 1, tyapps)), (lambda : hydra.lib.flows.pure(()))), (lambda targs: hydra.lib.flows.pure(hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.utils.method_invocation_static_with_type_args(hydra.ext.java.syntax.Identifier("java.util.List"), hydra.ext.java.syntax.Identifier("of"), targs, jels)))))))
        
        case hydra.core.TermLiteral(value=l):
            return hydra.lib.flows.pure(encode_literal(l))
        
        case hydra.core.TermMap(value=m):
            return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda x1: encode(x1)), hydra.lib.maps.keys(m)), (lambda jkeys: hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda x1: encode(x1)), hydra.lib.maps.elems(m)), (lambda jvals: (pair_exprs := hydra.lib.lists.map((lambda kv: hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.utils.method_invocation_static(hydra.ext.java.syntax.Identifier("java.util.Map"), hydra.ext.java.syntax.Identifier("entry"), (hydra.lib.pairs.first(kv), hydra.lib.pairs.second(kv))))), hydra.lib.lists.zip(jkeys, jvals)), hydra.lib.flows.bind(hydra.lib.logic.if_else(hydra.lib.maps.null(m), (lambda : take_type_args("map", 2, tyapps)), (lambda : hydra.lib.flows.pure(()))), (lambda targs: hydra.lib.flows.pure(hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.utils.method_invocation_static_with_type_args(hydra.ext.java.syntax.Identifier("java.util.Map"), hydra.ext.java.syntax.Identifier("ofEntries"), targs, pair_exprs))))))[1]))))
        
        case hydra.core.TermMaybe(value=mt):
            return hydra.lib.maybes.cases(mt, hydra.lib.flows.bind(take_type_args("maybe", 1, tyapps), (lambda targs: hydra.lib.flows.pure(hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.utils.method_invocation_static_with_type_args(hydra.ext.java.syntax.Identifier("hydra.util.Maybe"), hydra.ext.java.syntax.Identifier("nothing"), targs, ()))))), (lambda term1: hydra.lib.flows.bind(encode(term1), (lambda expr: hydra.lib.flows.pure(hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.utils.method_invocation_static(hydra.ext.java.syntax.Identifier("hydra.util.Maybe"), hydra.ext.java.syntax.Identifier("just"), (expr,))))))))
        
        case hydra.core.TermPair(value=p):
            return hydra.lib.flows.bind(encode(hydra.lib.pairs.first(p)), (lambda jterm1: hydra.lib.flows.bind(encode(hydra.lib.pairs.second(p)), (lambda jterm2: hydra.lib.flows.bind(hydra.lib.logic.if_else(hydra.lib.lists.null(tyapps), (lambda : hydra.lib.flows.pure(Nothing())), (lambda : hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda jt: hydra.ext.java.utils.java_type_to_java_reference_type(jt)), tyapps), (lambda rts: hydra.lib.flows.pure(Just(cast(hydra.ext.java.syntax.TypeArgumentsOrDiamond, hydra.ext.java.syntax.TypeArgumentsOrDiamondArguments(hydra.lib.lists.map((lambda rt: cast(hydra.ext.java.syntax.TypeArgument, hydra.ext.java.syntax.TypeArgumentReference(rt))), rts))))))))), (lambda mtargs: hydra.lib.flows.pure(hydra.ext.java.utils.java_constructor_call(hydra.ext.java.utils.java_constructor_name(hydra.ext.java.syntax.Identifier("hydra.util.Tuple.Tuple2"), mtargs), (jterm1, jterm2), Nothing()))))))))
        
        case hydra.core.TermRecord(value=rec):
            @lru_cache(1)
            def rec_name() -> hydra.core.Name:
                return rec.type_name
            return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda fld: encode(fld.term)), rec.fields), (lambda field_exprs: (cons_id := hydra.ext.java.utils.name_to_java_name(aliases(), rec_name()), hydra.lib.flows.bind(hydra.lib.logic.if_else(hydra.lib.lists.null(tyapps), (lambda : hydra.lib.flows.pure(Nothing())), (lambda : hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda jt: hydra.ext.java.utils.java_type_to_java_reference_type(jt)), tyapps), (lambda rts: hydra.lib.flows.pure(Just(cast(hydra.ext.java.syntax.TypeArgumentsOrDiamond, hydra.ext.java.syntax.TypeArgumentsOrDiamondArguments(hydra.lib.lists.map((lambda rt: cast(hydra.ext.java.syntax.TypeArgument, hydra.ext.java.syntax.TypeArgumentReference(rt))), rts))))))))), (lambda mtargs: hydra.lib.flows.pure(hydra.ext.java.utils.java_constructor_call(hydra.ext.java.utils.java_constructor_name(cons_id, mtargs), field_exprs, Nothing())))))[1]))
        
        case hydra.core.TermSet(value=s):
            @lru_cache(1)
            def slist() -> frozenlist[hydra.core.Term]:
                return hydra.lib.sets.to_list(s)
            return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda x1: encode(x1)), slist()), (lambda jels: hydra.lib.logic.if_else(hydra.lib.sets.null(s), (lambda : hydra.lib.flows.bind(take_type_args("set", 1, tyapps), (lambda targs: hydra.lib.flows.pure(hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.utils.method_invocation_static_with_type_args(hydra.ext.java.syntax.Identifier("java.util.Set"), hydra.ext.java.syntax.Identifier("of"), targs, ())))))), (lambda : (prim := hydra.ext.java.utils.java_method_invocation_to_java_primary(hydra.ext.java.utils.method_invocation_static(hydra.ext.java.syntax.Identifier("java.util.stream.Stream"), hydra.ext.java.syntax.Identifier("of"), jels)), (coll := hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.utils.method_invocation_static(hydra.ext.java.syntax.Identifier("java.util.stream.Collectors"), hydra.ext.java.syntax.Identifier("toSet"), ())), hydra.lib.flows.pure(hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.utils.method_invocation(Just(Right(prim)), hydra.ext.java.syntax.Identifier("collect"), (coll,)))))[1])[1]))))
        
        case hydra.core.TermTypeLambda(value=tl):
            return with_type_lambda(env, tl, (lambda env2: (combined_anns := hydra.lib.lists.foldl((lambda acc, m: hydra.lib.maps.union(acc, m)), hydra.lib.maps.empty(), anns), hydra.lib.flows.bind(hydra.annotations.get_type(combined_anns), (lambda mtyp: (annotated_body := (_hoist_annotated_body_1 := (lambda v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lib.maybes.cases(mtyp, tl.body, (lambda t: _hoist_annotated_body_1(t))))[1], encode_term(env2, annotated_body))[1])))[1]))
        
        case hydra.core.TermUnion(value=inj):
            @lru_cache(1)
            def inj_type_name() -> hydra.core.Name:
                return inj.type_name
            @lru_cache(1)
            def inj_field() -> hydra.core.Field:
                return inj.field
            @lru_cache(1)
            def inj_field_name() -> hydra.core.Name:
                return inj_field().name
            @lru_cache(1)
            def inj_field_term() -> hydra.core.Term:
                return inj_field().term
            @lru_cache(1)
            def type_id() -> str:
                return hydra.ext.java.utils.name_to_java_name(aliases(), inj_type_name()).value
            @lru_cache(1)
            def cons_id() -> hydra.ext.java.syntax.Identifier:
                return hydra.ext.java.syntax.Identifier(hydra.lib.strings.cat((type_id(), ".", hydra.ext.java.utils.sanitize_java_name(hydra.formatting.capitalize(inj_field_name().value)))))
            return hydra.lib.flows.bind(is_field_unit_type(inj_type_name(), inj_field_name()), (lambda field_is_unit: hydra.lib.flows.bind(hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.schemas.is_unit_term(hydra.rewriting.deannotate_term(inj_field_term())), field_is_unit), (lambda : hydra.lib.flows.pure(())), (lambda : hydra.lib.flows.bind(encode(inj_field_term()), (lambda ex: hydra.lib.flows.pure((ex,)))))), (lambda args: hydra.lib.flows.pure(hydra.ext.java.utils.java_constructor_call(hydra.ext.java.utils.java_constructor_name(cons_id(), Nothing()), args, Nothing()))))))
        
        case hydra.core.TermVariable(value=name):
            return encode_variable(env, name)
        
        case hydra.core.TermUnit():
            return hydra.lib.flows.pure(hydra.ext.java.utils.java_literal_to_java_expression(cast(hydra.ext.java.syntax.Literal, hydra.ext.java.syntax.LiteralNull())))
        
        case hydra.core.TermWrap(value=wt):
            return hydra.lib.flows.bind(encode(wt.body), (lambda jarg: hydra.lib.flows.pure(hydra.ext.java.utils.java_constructor_call(hydra.ext.java.utils.java_constructor_name(hydra.ext.java.utils.name_to_java_name(aliases(), wt.type_name), Nothing()), (jarg,), Nothing()))))
        
        case hydra.core.TermTypeApplication(value=ta):
            @lru_cache(1)
            def atyp() -> hydra.core.Type:
                return ta.type
            @lru_cache(1)
            def body() -> hydra.core.Term:
                return ta.body
            return hydra.lib.flows.bind(encode_type(aliases(), hydra.lib.sets.empty(), atyp()), (lambda jatyp: (combined_anns := hydra.lib.lists.foldl((lambda acc, m: hydra.lib.maps.union(acc, m)), hydra.lib.maps.empty(), anns), hydra.lib.flows.bind(hydra.annotations.get_type(combined_anns), (lambda mtyp: hydra.lib.flows.bind(hydra.lib.maybes.cases(mtyp, hydra.coder_utils.try_type_of("5", tc(), term), (lambda t: hydra.lib.flows.pure(t))), (lambda typ: (collected0 := collect_type_apps0(body(), (atyp(),)), innermost_body0 := hydra.lib.pairs.first(collected0), all_type_args0 := hydra.lib.pairs.second(collected0), hydra.lib.flows.bind(correct_cast_type(innermost_body0, all_type_args0, typ), (lambda corrected_typ: (collected := collect_type_apps(body(), (atyp(),)), innermost_body := hydra.lib.pairs.first(collected), all_type_args := hydra.lib.pairs.second(collected), hydra.dsl.python.unsupported("inline match expressions are not yet supported"))[3])))[3])))))[1]))
        
        case _:
            return hydra.lib.flows.pure(encode_literal(cast(hydra.core.Literal, hydra.core.LiteralString("Unimplemented term variant"))))

def function_call(env: hydra.ext.java.helpers.JavaEnvironment, is_prim: bool, name: hydra.core.Name, args: frozenlist[hydra.core.Term], type_apps: frozenlist[hydra.core.Type]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.Expression]:
    @lru_cache(1)
    def aliases() -> hydra.ext.java.helpers.Aliases:
        return env.aliases
    @lru_cache(1)
    def is_lambda_bound() -> bool:
        return is_lambda_bound_in(name, aliases().lambda_vars)
    return hydra.lib.logic.if_else(hydra.lib.logic.and_(is_prim, hydra.lib.logic.and_(hydra.lib.lists.null(args), hydra.lib.logic.not_(is_lambda_bound()))), (lambda : (class_with_apply := element_java_identifier(True, False, aliases(), name).value, (suffix := hydra.lib.strings.cat2(".", hydra.ext.java.names.apply_method_name), (class_name := hydra.lib.strings.from_list(hydra.lib.lists.take(hydra.lib.math.sub(hydra.lib.strings.length(class_with_apply), hydra.lib.strings.length(suffix)), hydra.lib.strings.to_list(class_with_apply))), hydra.lib.flows.pure(hydra.ext.java.utils.java_identifier_to_java_expression(hydra.ext.java.syntax.Identifier(hydra.lib.strings.cat((class_name, "::", hydra.ext.java.names.apply_method_name))))))[1])[1])[1]), (lambda : hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda arg: encode_term(env, arg)), args), (lambda jargs0: (wrap_result := wrap_lazy_arguments(name, jargs0), jargs := hydra.lib.pairs.first(wrap_result), m_method_override := hydra.lib.pairs.second(wrap_result), hydra.lib.logic.if_else(hydra.lib.logic.or_(is_local_variable(name), is_lambda_bound()), (lambda : hydra.lib.flows.bind(encode_variable(env, name), (lambda base_expr: hydra.lib.flows.pure(hydra.lib.lists.foldl((lambda acc, jarg: apply_java_arg(acc, jarg)), base_expr, jargs))))), (lambda : (override_method_name := (lambda jid: hydra.lib.maybes.cases(m_method_override, jid, (lambda m: (s := jid.value, hydra.ext.java.syntax.Identifier(hydra.lib.strings.cat2(hydra.lib.strings.from_list(hydra.lib.lists.take(hydra.lib.math.sub(hydra.lib.strings.length(s), hydra.lib.strings.length(hydra.ext.java.names.apply_method_name)), hydra.lib.strings.to_list(s))), m)))[1]))), hydra.lib.logic.if_else(hydra.lib.lists.null(type_apps), (lambda : (header := cast(hydra.ext.java.syntax.MethodInvocation_Header, hydra.ext.java.syntax.MethodInvocation_HeaderSimple(hydra.ext.java.syntax.MethodName(override_method_name(element_java_identifier(is_prim, False, aliases(), name))))), hydra.lib.flows.pure(hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.syntax.MethodInvocation(header, jargs))))[1]), (lambda : (qn := hydra.names.qualify_name(name), (mns := qn.namespace, (local_name := qn.local, hydra.lib.maybes.cases(mns, (header := cast(hydra.ext.java.syntax.MethodInvocation_Header, hydra.ext.java.syntax.MethodInvocation_HeaderSimple(hydra.ext.java.syntax.MethodName(override_method_name(element_java_identifier(is_prim, False, aliases(), name))))), hydra.lib.flows.pure(hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.syntax.MethodInvocation(header, jargs))))[1], (lambda ns_: (class_id := hydra.ext.java.utils.name_to_java_name(aliases(), hydra.names.unqualify_name(hydra.module.QualifiedName(Just(ns_), elements_class_name(ns_)))), method_id := hydra.lib.logic.if_else(is_prim, (lambda : override_method_name(hydra.ext.java.syntax.Identifier(hydra.lib.strings.cat2(hydra.ext.java.utils.name_to_java_name(aliases(), hydra.names.unqualify_name(hydra.module.QualifiedName(Just(ns_), hydra.formatting.capitalize(local_name)))).value, hydra.lib.strings.cat2(".", hydra.ext.java.names.apply_method_name))))), (lambda : hydra.ext.java.syntax.Identifier(hydra.ext.java.utils.sanitize_java_name(local_name)))), hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda t: hydra.lib.flows.bind(encode_type(aliases(), hydra.lib.sets.empty(), t), (lambda jt: hydra.lib.flows.bind(hydra.ext.java.utils.java_type_to_java_reference_type(jt), (lambda rt: hydra.lib.flows.pure(cast(hydra.ext.java.syntax.TypeArgument, hydra.ext.java.syntax.TypeArgumentReference(rt)))))))), type_apps), (lambda j_type_args: hydra.lib.flows.pure(hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.utils.method_invocation_static_with_type_args(class_id, method_id, j_type_args, jargs))))))[2])))[1])[1])[1])))[1])))[3]))))

def otherwise_branch(env: hydra.ext.java.helpers.JavaEnvironment, aliases: hydra.ext.java.helpers.Aliases, dom: hydra.core.Type, cod: hydra.core.Type, tname: hydra.core.Name, jcod: hydra.ext.java.syntax.Type, targs: frozenlist[hydra.ext.java.syntax.TypeArgument], d: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments]:
    @lru_cache(1)
    def jdom() -> hydra.ext.java.syntax.Type:
        return cast(hydra.ext.java.syntax.Type, hydra.ext.java.syntax.TypeReference(hydra.ext.java.utils.name_to_java_reference_type(aliases, True, targs, tname, Nothing())))
    mods = (cast(hydra.ext.java.syntax.MethodModifier, hydra.ext.java.syntax.MethodModifierPublic()),)
    anns = (hydra.ext.java.utils.override_annotation,)
    @lru_cache(1)
    def param() -> hydra.ext.java.syntax.FormalParameter:
        return hydra.ext.java.utils.java_type_to_java_formal_parameter(jdom(), hydra.core.Name("instance"))
    @lru_cache(1)
    def result() -> hydra.ext.java.syntax.Result:
        return cast(hydra.ext.java.syntax.Result, hydra.ext.java.syntax.ResultType(hydra.ext.java.syntax.UnannType(jcod)))
    return hydra.lib.flows.bind(analyze_java_function(env, d), (lambda fs: (bindings := fs.bindings, raw_body := fs.body, inner_body := annotate_body_with_cod(cod, raw_body), env2 := fs.environment, hydra.lib.flows.bind(bindings_to_statements(env2, bindings), (lambda bind_result: (binding_stmts := hydra.lib.pairs.first(bind_result), env3 := hydra.lib.pairs.second(bind_result), hydra.lib.flows.bind(encode_term(env3, inner_body), (lambda jret: (return_stmt := cast(hydra.ext.java.syntax.BlockStatement, hydra.ext.java.syntax.BlockStatementStatement(hydra.ext.java.utils.java_return_statement(Just(jret)))), all_stmts := hydra.lib.lists.concat2(binding_stmts, (return_stmt,)), hydra.lib.flows.pure(no_comment(hydra.ext.java.utils.method_declaration(mods, (), anns, hydra.ext.java.names.otherwise_method_name, (param(),), result(), Just(all_stmts)))))[2])))[2])))[4]))

def to_decl_statement(env_ext: hydra.ext.java.helpers.JavaEnvironment, aliases_ext: hydra.ext.java.helpers.Aliases, tc_ext: hydra.typing.TypeContext, recursive_vars: frozenset[hydra.core.Name], thunked_vars: frozenset[hydra.core.Name], flat_bindings: frozenlist[hydra.core.Binding], name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.BlockStatement]:
    @lru_cache(1)
    def binding() -> hydra.core.Binding:
        return hydra.lib.lists.head(hydra.lib.lists.filter((lambda b: hydra.lib.equality.equal(b.name, name)), flat_bindings))
    @lru_cache(1)
    def value() -> hydra.core.Term:
        return binding().term
    return hydra.lib.flows.bind(hydra.lib.maybes.cases(binding().type, hydra.coder_utils.try_type_of("7", tc_ext, value()), (lambda ts: hydra.lib.flows.pure(ts.type))), (lambda typ: hydra.lib.flows.bind(encode_type(aliases_ext, hydra.lib.sets.empty(), typ), (lambda jtype: (id := hydra.ext.java.utils.variable_to_java_identifier(name), annotated_value := hydra.annotations.set_term_annotation(hydra.constants.key_type, Just(hydra.encode.core.type(typ)), value()), hydra.lib.flows.bind(encode_term(env_ext, annotated_value), (lambda rhs2: hydra.lib.logic.if_else(hydra.lib.sets.member(name, recursive_vars), (lambda : hydra.lib.flows.pure(cast(hydra.ext.java.syntax.BlockStatement, hydra.ext.java.syntax.BlockStatementStatement(hydra.ext.java.utils.java_method_invocation_to_java_statement(hydra.ext.java.utils.method_invocation(Just(Left(hydra.ext.java.syntax.ExpressionName(Nothing(), id))), hydra.ext.java.syntax.Identifier(hydra.ext.java.names.set_method_name), (rhs2,))))))), (lambda : hydra.lib.logic.if_else(hydra.lib.sets.member(name, thunked_vars), (lambda : hydra.lib.flows.bind(hydra.ext.java.utils.java_type_to_java_reference_type(jtype), (lambda rt: (lazy_type := hydra.ext.java.utils.java_ref_type((rt,), hydra.ext.java.names.hydra_util_package_name, "Lazy"), lambda_body := cast(hydra.ext.java.syntax.LambdaBody, hydra.ext.java.syntax.LambdaBodyExpression(rhs2)), supplier_lambda := cast(hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.ExpressionLambda(hydra.ext.java.syntax.LambdaExpression(cast(hydra.ext.java.syntax.LambdaParameters, hydra.ext.java.syntax.LambdaParametersTuple(())), lambda_body))), targs := type_args_or_diamond((cast(hydra.ext.java.syntax.TypeArgument, hydra.ext.java.syntax.TypeArgumentReference(rt)),)), lazy_expr := hydra.ext.java.utils.java_constructor_call(hydra.ext.java.utils.java_constructor_name(hydra.ext.java.syntax.Identifier("hydra.util.Lazy"), Just(targs)), (supplier_lambda,), Nothing()), hydra.lib.flows.pure(hydra.ext.java.utils.variable_declaration_statement(aliases_ext, lazy_type, id, lazy_expr)))[5]))), (lambda : hydra.lib.flows.pure(hydra.ext.java.utils.variable_declaration_statement(aliases_ext, jtype, id, rhs2)))))))))[2]))))

def type_app_fallback_cast(env: hydra.ext.java.helpers.JavaEnvironment, aliases: hydra.ext.java.helpers.Aliases, anns: frozenlist[FrozenDict[hydra.core.Name, hydra.core.Term]], tyapps: frozenlist[hydra.ext.java.syntax.Type], jatyp: hydra.ext.java.syntax.Type, body: hydra.core.Term, typ: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.Expression]:
    @lru_cache(1)
    def annotated_body() -> hydra.core.Term:
        return hydra.annotations.set_term_annotation(hydra.constants.key_type, Just(hydra.encode.core.type(typ)), body)
    return hydra.lib.flows.bind(encode_term_internal(env, anns, hydra.lib.lists.cons(jatyp, tyapps), annotated_body()), (lambda jbody: hydra.lib.flows.bind(encode_type(aliases, hydra.lib.sets.empty(), typ), (lambda jtype: hydra.lib.flows.bind(hydra.ext.java.utils.java_type_to_java_reference_type(jtype), (lambda rt: hydra.lib.flows.pure(hydra.ext.java.utils.java_cast_expression_to_java_expression(hydra.ext.java.utils.java_cast_expression(rt, hydra.ext.java.utils.java_expression_to_java_unary_expression(jbody))))))))))

def type_app_nullary_or_hoisted(env: hydra.ext.java.helpers.JavaEnvironment, aliases: hydra.ext.java.helpers.Aliases, anns: frozenlist[FrozenDict[hydra.core.Name, hydra.core.Term]], tyapps: frozenlist[hydra.ext.java.syntax.Type], jatyp: hydra.ext.java.syntax.Type, body: hydra.core.Term, corrected_typ: hydra.core.Type, var_name: hydra.core.Name, cls: hydra.ext.java.helpers.JavaSymbolClass, all_type_args: frozenlist[hydra.core.Type]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.Expression]:
    @lru_cache(1)
    def qn() -> hydra.module.QualifiedName:
        return hydra.names.qualify_name(var_name)
    @lru_cache(1)
    def mns() -> Maybe[hydra.module.Namespace]:
        return qn().namespace
    @lru_cache(1)
    def local_name() -> str:
        return qn().local
    match cls:
        case hydra.ext.java.helpers.JavaSymbolClassNullaryFunction():
            return hydra.lib.maybes.cases(mns(), type_app_fallback_cast(env, aliases, anns, tyapps, jatyp, body, corrected_typ), (lambda ns_: (class_id := hydra.ext.java.utils.name_to_java_name(aliases, hydra.names.unqualify_name(hydra.module.QualifiedName(Just(ns_), elements_class_name(ns_)))), method_id := hydra.ext.java.syntax.Identifier(hydra.ext.java.utils.sanitize_java_name(local_name())), hydra.lib.flows.bind(filter_phantom_type_args(var_name, all_type_args), (lambda filtered_type_args: hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda t: hydra.lib.flows.bind(encode_type(aliases, hydra.lib.sets.empty(), t), (lambda jt: hydra.lib.flows.bind(hydra.ext.java.utils.java_type_to_java_reference_type(jt), (lambda rt: hydra.lib.flows.pure(cast(hydra.ext.java.syntax.TypeArgument, hydra.ext.java.syntax.TypeArgumentReference(rt)))))))), filtered_type_args), (lambda j_type_args: hydra.lib.flows.pure(hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.utils.method_invocation_static_with_type_args(class_id, method_id, j_type_args, ()))))))))[2]))
        
        case hydra.ext.java.helpers.JavaSymbolClassHoistedLambda(value=arity):
            return hydra.lib.maybes.cases(mns(), type_app_fallback_cast(env, aliases, anns, tyapps, jatyp, body, corrected_typ), (lambda ns_: (class_id := hydra.ext.java.utils.name_to_java_name(aliases, hydra.names.unqualify_name(hydra.module.QualifiedName(Just(ns_), elements_class_name(ns_)))), method_id := hydra.ext.java.syntax.Identifier(hydra.ext.java.utils.sanitize_java_name(local_name())), hydra.lib.flows.bind(filter_phantom_type_args(var_name, all_type_args), (lambda filtered_type_args: hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda t: hydra.lib.flows.bind(encode_type(aliases, hydra.lib.sets.empty(), t), (lambda jt: hydra.lib.flows.bind(hydra.ext.java.utils.java_type_to_java_reference_type(jt), (lambda rt: hydra.lib.flows.pure(cast(hydra.ext.java.syntax.TypeArgument, hydra.ext.java.syntax.TypeArgumentReference(rt)))))))), filtered_type_args), (lambda j_type_args: (param_names := hydra.lib.lists.map((lambda i: hydra.core.Name(hydra.lib.strings.cat2("p", hydra.lib.literals.show_int32(i)))), hydra.lib.math.range_(0, hydra.lib.math.sub(arity, 1))), param_exprs := hydra.lib.lists.map((lambda p: hydra.ext.java.utils.java_identifier_to_java_expression(hydra.ext.java.utils.variable_to_java_identifier(p))), param_names), call := hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.utils.method_invocation_static_with_type_args(class_id, method_id, j_type_args, param_exprs)), hydra.lib.flows.pure(build_curried_lambda(param_names, call)))[3])))))[2]))
        
        case _:
            return type_app_fallback_cast(env, aliases, anns, tyapps, jatyp, body, corrected_typ)

def visit_branch(env: hydra.ext.java.helpers.JavaEnvironment, aliases: hydra.ext.java.helpers.Aliases, dom: hydra.core.Type, tname: hydra.core.Name, jcod: hydra.ext.java.syntax.Type, targs: frozenlist[hydra.ext.java.syntax.TypeArgument], field: hydra.core.Field) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments]:
    @lru_cache(1)
    def jdom() -> hydra.ext.java.syntax.Type:
        return cast(hydra.ext.java.syntax.Type, hydra.ext.java.syntax.TypeReference(hydra.ext.java.utils.name_to_java_reference_type(aliases, True, targs, tname, Just(hydra.formatting.capitalize(field.name.value)))))
    mods = (cast(hydra.ext.java.syntax.MethodModifier, hydra.ext.java.syntax.MethodModifierPublic()),)
    anns = (hydra.ext.java.utils.override_annotation,)
    @lru_cache(1)
    def result() -> hydra.ext.java.syntax.Result:
        return cast(hydra.ext.java.syntax.Result, hydra.ext.java.syntax.ResultType(hydra.ext.java.syntax.UnannType(jcod)))
    def _hoist_body_1(v1: hydra.core.Function) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments]:
        match v1:
            case hydra.core.FunctionLambda(value=lam):
                return with_lambda(env, lam, (lambda env2: (lambda_param := lam.parameter, body := lam.body, env3 := insert_branch_var(lambda_param, env2), hydra.lib.flows.bind(analyze_java_function(env3, body), (lambda fs: (bindings := fs.bindings, inner_body := fs.body, env4 := fs.environment, hydra.lib.flows.bind(bindings_to_statements(env4, bindings), (lambda bind_result: (binding_stmts := hydra.lib.pairs.first(bind_result), env5 := hydra.lib.pairs.second(bind_result), hydra.lib.flows.bind(encode_term(env5, inner_body), (lambda jret: (param := hydra.ext.java.utils.java_type_to_java_formal_parameter(jdom(), lambda_param), return_stmt := cast(hydra.ext.java.syntax.BlockStatement, hydra.ext.java.syntax.BlockStatementStatement(hydra.ext.java.utils.java_return_statement(Just(jret)))), all_stmts := hydra.lib.lists.concat2(binding_stmts, (return_stmt,)), hydra.lib.flows.pure(no_comment(hydra.ext.java.utils.method_declaration(mods, (), anns, hydra.ext.java.names.visit_method_name, (param,), result(), Just(all_stmts)))))[3])))[2])))[3])))[3]))
            
            case _:
                return hydra.monads.fail(hydra.lib.strings.cat2("visitBranch: field term is not a lambda: ", hydra.show.core.term(field.term)))
    match hydra.rewriting.deannotate_term(field.term):
        case hydra.core.TermFunction(value=f):
            return _hoist_body_1(f)
        
        case _:
            return hydra.monads.fail(hydra.lib.strings.cat2("visitBranch: field term is not a lambda: ", hydra.show.core.term(field.term)))

def bound_type_variables(typ: hydra.core.Type) -> frozenlist[hydra.core.Name]:
    match typ:
        case hydra.core.TypeAnnotated(value=at):
            return bound_type_variables(at.body)
        
        case hydra.core.TypeForall(value=ft):
            return hydra.lib.lists.cons(ft.parameter, bound_type_variables(ft.body))
        
        case _:
            return ()

def build_type_var_subst_go(svs: frozenset[hydra.core.Name], ft: hydra.core.Type, ct: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
    def go_sub(a: hydra.core.Type, b: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        return build_type_var_subst_go(svs, hydra.rewriting.deannotate_type(a), hydra.rewriting.deannotate_type(b))
    def _hoist_body_1(v1: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        match v1:
            case hydra.core.TypeForall(value=cfa):
                return build_type_var_subst_go(svs, ft, hydra.rewriting.deannotate_type(cfa.body))
            
            case _:
                return hydra.lib.maps.empty()
    def _hoist_body_2(fn: hydra.core.Name, v1: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        match v1:
            case hydra.core.TypeVariable(value=cn):
                return hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.logic.not_(hydra.lib.equality.equal(fn, cn)), hydra.lib.sets.member(cn, svs)), (lambda : hydra.lib.maps.singleton(fn, cn)), (lambda : hydra.lib.maps.empty()))
            
            case _:
                return hydra.lib.maps.empty()
    def _hoist_body_3(fft: hydra.core.FunctionType, v1: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        match v1:
            case hydra.core.TypeFunction(value=cft):
                return hydra.lib.maps.union(go_sub(fft.domain, cft.domain), go_sub(fft.codomain, cft.codomain))
            
            case _:
                return hydra.lib.maps.empty()
    def _hoist_body_4(fat: hydra.core.ApplicationType, v1: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        match v1:
            case hydra.core.TypeApplication(value=cat):
                return hydra.lib.maps.union(go_sub(fat.function, cat.function), go_sub(fat.argument, cat.argument))
            
            case _:
                return hydra.lib.maps.empty()
    def _hoist_body_5(fl: hydra.core.Type, v1: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        match v1:
            case hydra.core.TypeList(value=cl):
                return go_sub(fl, cl)
            
            case _:
                return hydra.lib.maps.empty()
    def _hoist_body_6(fs: hydra.core.Type, v1: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        match v1:
            case hydra.core.TypeSet(value=cs):
                return go_sub(fs, cs)
            
            case _:
                return hydra.lib.maps.empty()
    def _hoist_body_7(fm: hydra.core.Type, v1: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        match v1:
            case hydra.core.TypeMaybe(value=cm):
                return go_sub(fm, cm)
            
            case _:
                return hydra.lib.maps.empty()
    def _hoist_body_8(fmt: hydra.core.MapType, v1: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        match v1:
            case hydra.core.TypeMap(value=cmt):
                return hydra.lib.maps.union(go_sub(fmt.keys, cmt.keys), go_sub(fmt.values, cmt.values))
            
            case _:
                return hydra.lib.maps.empty()
    def _hoist_body_9(fpt: hydra.core.PairType, v1: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        match v1:
            case hydra.core.TypePair(value=cpt):
                return hydra.lib.maps.union(go_sub(fpt.first, cpt.first), go_sub(fpt.second, cpt.second))
            
            case _:
                return hydra.lib.maps.empty()
    def _hoist_body_10(fet: hydra.core.EitherType, v1: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        match v1:
            case hydra.core.TypeEither(value=cet):
                return hydra.lib.maps.union(go_sub(fet.left, cet.left), go_sub(fet.right, cet.right))
            
            case _:
                return hydra.lib.maps.empty()
    def _hoist_body_11(ffa: hydra.core.ForallType, v1: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        match v1:
            case hydra.core.TypeForall(value=cfa):
                return go_sub(ffa.body, cfa.body)
            
            case _:
                return build_type_var_subst_go(svs, hydra.rewriting.deannotate_type(ffa.body), ct)
    match ft:
        case hydra.core.TypeVariable(value=fn):
            return _hoist_body_2(fn, ct)
        
        case hydra.core.TypeFunction(value=fft):
            return _hoist_body_3(fft, ct)
        
        case hydra.core.TypeApplication(value=fat):
            return _hoist_body_4(fat, ct)
        
        case hydra.core.TypeList(value=fl):
            return _hoist_body_5(fl, ct)
        
        case hydra.core.TypeSet(value=fs):
            return _hoist_body_6(fs, ct)
        
        case hydra.core.TypeMaybe(value=fm):
            return _hoist_body_7(fm, ct)
        
        case hydra.core.TypeMap(value=fmt):
            return _hoist_body_8(fmt, ct)
        
        case hydra.core.TypePair(value=fpt):
            return _hoist_body_9(fpt, ct)
        
        case hydra.core.TypeEither(value=fet):
            return _hoist_body_10(fet, ct)
        
        case hydra.core.TypeForall(value=ffa):
            return _hoist_body_11(ffa, ct)
        
        case _:
            return _hoist_body_1(ct)

def build_type_var_subst(scheme_var_set: frozenset[hydra.core.Name], fresh_typ: hydra.core.Type, canon_typ: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
    return build_type_var_subst_go(scheme_var_set, hydra.rewriting.deannotate_type(fresh_typ), hydra.rewriting.deannotate_type(canon_typ))

def build_subst_from_annotations_go(scheme_var_set: frozenset[hydra.core.Name], g: hydra.graph.Graph, term: hydra.core.Term) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
    def _hoist_hydra_ext_java_coder_build_subst_from_annotations_go_1(g: hydra.graph.Graph, scheme_var_set: frozenset[hydra.core.Name], v1: hydra.core.Elimination) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        match v1:
            case hydra.core.EliminationUnion(value=cs):
                @lru_cache(1)
                def def_subst() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
                    return hydra.lib.maybes.cases(cs.default, hydra.lib.maps.empty(), (lambda d: build_subst_from_annotations_go(scheme_var_set, g, d)))
                @lru_cache(1)
                def case_substs() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
                    return hydra.lib.lists.foldl((lambda acc, fld: hydra.lib.maps.union(acc, build_subst_from_annotations_go(scheme_var_set, g, fld.term))), hydra.lib.maps.empty(), cs.cases)
                return hydra.lib.maps.union(def_subst(), case_substs())
            
            case _:
                return hydra.lib.maps.empty()
    def _hoist_hydra_ext_java_coder_build_subst_from_annotations_go_2(g: hydra.graph.Graph, scheme_var_set: frozenset[hydra.core.Name], v1: hydra.core.Function) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        match v1:
            case hydra.core.FunctionLambda(value=lam):
                return build_subst_from_annotations_go(scheme_var_set, g, lam.body)
            
            case hydra.core.FunctionElimination(value=elim):
                return _hoist_hydra_ext_java_coder_build_subst_from_annotations_go_1(g, scheme_var_set, elim)
            
            case _:
                return hydra.lib.maps.empty()
    match term:
        case hydra.core.TermAnnotated(value=at):
            @lru_cache(1)
            def body() -> hydra.core.Term:
                return at.body
            @lru_cache(1)
            def anns() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                return at.annotation
            @lru_cache(1)
            def body_subst() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
                return build_subst_from_annotations_go(scheme_var_set, g, body())
            @lru_cache(1)
            def ann_subst() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
                def _hoist_ann_subst_1(dom: hydra.core.Type, v1: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
                    match v1:
                        case hydra.core.TypeFunction(value=ft):
                            return build_type_var_subst(scheme_var_set, ft.domain, dom)
                        
                        case _:
                            return hydra.lib.maps.empty()
                def _hoist_ann_subst_2(ann_type: hydra.core.Type, v1: hydra.core.Function) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
                    match v1:
                        case hydra.core.FunctionLambda(value=lam):
                            return hydra.lib.maybes.cases(lam.domain, hydra.lib.maps.empty(), (lambda dom: _hoist_ann_subst_1(dom, hydra.rewriting.deannotate_type(ann_type))))
                        
                        case _:
                            return hydra.lib.maps.empty()
                def _hoist_ann_subst_3(ann_type: hydra.core.Type, v1: hydra.core.Term) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
                    match v1:
                        case hydra.core.TermFunction(value=f):
                            return _hoist_ann_subst_2(ann_type, f)
                        
                        case _:
                            return hydra.lib.maps.empty()
                return hydra.lib.maybes.cases(hydra.lib.maps.lookup(hydra.constants.key_type, anns()), hydra.lib.maps.empty(), (lambda type_term: hydra.lib.eithers.either((lambda _: hydra.lib.maps.empty()), (lambda ann_type: _hoist_ann_subst_3(ann_type, hydra.rewriting.deannotate_term(body()))), hydra.decode.core.type(g, type_term))))
            return hydra.lib.maps.union(ann_subst(), body_subst())
        
        case hydra.core.TermApplication(value=app):
            return hydra.lib.maps.union(build_subst_from_annotations_go(scheme_var_set, g, app.function), build_subst_from_annotations_go(scheme_var_set, g, app.argument))
        
        case hydra.core.TermFunction(value=f):
            return _hoist_hydra_ext_java_coder_build_subst_from_annotations_go_2(g, scheme_var_set, f)
        
        case hydra.core.TermLet(value=lt):
            @lru_cache(1)
            def binding_subst() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
                return hydra.lib.lists.foldl((lambda acc, b: hydra.lib.maps.union(acc, build_subst_from_annotations_go(scheme_var_set, g, b.term))), hydra.lib.maps.empty(), lt.bindings)
            return hydra.lib.maps.union(binding_subst(), build_subst_from_annotations_go(scheme_var_set, g, lt.body))
        
        case hydra.core.TermList(value=terms):
            return hydra.lib.lists.foldl((lambda acc, t: hydra.lib.maps.union(acc, build_subst_from_annotations_go(scheme_var_set, g, t))), hydra.lib.maps.empty(), terms)
        
        case hydra.core.TermMaybe(value=mt):
            return hydra.lib.maybes.cases(mt, hydra.lib.maps.empty(), (lambda t: build_subst_from_annotations_go(scheme_var_set, g, t)))
        
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

def build_subst_from_annotations(scheme_var_set: frozenset[hydra.core.Name], term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, FrozenDict[hydra.core.Name, hydra.core.Name]]:
    return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda g: hydra.lib.flows.pure(build_subst_from_annotations_go(scheme_var_set, g, term))))

def build_type_subst_go(svs: frozenset[hydra.core.Name], st: hydra.core.Type, at: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Type]:
    def go_sub(a: hydra.core.Type, b: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Type]:
        return build_type_subst_go(svs, hydra.rewriting.deannotate_type(a), hydra.rewriting.deannotate_type(b))
    def _hoist_body_1(sft: hydra.core.FunctionType, v1: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Type]:
        match v1:
            case hydra.core.TypeFunction(value=aft):
                return hydra.lib.maps.union(go_sub(sft.domain, aft.domain), go_sub(sft.codomain, aft.codomain))
            
            case _:
                return hydra.lib.maps.empty()
    def _hoist_body_2(sat: hydra.core.ApplicationType, v1: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Type]:
        match v1:
            case hydra.core.TypeApplication(value=aat):
                return hydra.lib.maps.union(go_sub(sat.function, aat.function), go_sub(sat.argument, aat.argument))
            
            case _:
                return hydra.lib.maps.empty()
    def _hoist_body_3(sl: hydra.core.Type, v1: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Type]:
        match v1:
            case hydra.core.TypeList(value=al):
                return go_sub(sl, al)
            
            case _:
                return hydra.lib.maps.empty()
    def _hoist_body_4(ss: hydra.core.Type, v1: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Type]:
        match v1:
            case hydra.core.TypeSet(value=as_):
                return go_sub(ss, as_)
            
            case _:
                return hydra.lib.maps.empty()
    def _hoist_body_5(sm: hydra.core.Type, v1: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Type]:
        match v1:
            case hydra.core.TypeMaybe(value=am):
                return go_sub(sm, am)
            
            case _:
                return hydra.lib.maps.empty()
    def _hoist_body_6(smt: hydra.core.MapType, v1: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Type]:
        match v1:
            case hydra.core.TypeMap(value=amt):
                return hydra.lib.maps.union(go_sub(smt.keys, amt.keys), go_sub(smt.values, amt.values))
            
            case _:
                return hydra.lib.maps.empty()
    def _hoist_body_7(spt: hydra.core.PairType, v1: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Type]:
        match v1:
            case hydra.core.TypePair(value=apt):
                return hydra.lib.maps.union(go_sub(spt.first, apt.first), go_sub(spt.second, apt.second))
            
            case _:
                return hydra.lib.maps.empty()
    def _hoist_body_8(set_: hydra.core.EitherType, v1: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Type]:
        match v1:
            case hydra.core.TypeEither(value=aet):
                return hydra.lib.maps.union(go_sub(set_.left, aet.left), go_sub(set_.right, aet.right))
            
            case _:
                return hydra.lib.maps.empty()
    def _hoist_body_9(sfa: hydra.core.ForallType, v1: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Type]:
        match v1:
            case hydra.core.TypeForall(value=afa):
                return go_sub(sfa.body, afa.body)
            
            case _:
                return go_sub(sfa.body, at)
    match st:
        case hydra.core.TypeVariable(value=v):
            return hydra.lib.logic.if_else(hydra.lib.sets.member(v, svs), (lambda : hydra.lib.maps.singleton(v, at)), (lambda : hydra.lib.maps.empty()))
        
        case hydra.core.TypeFunction(value=sft):
            return _hoist_body_1(sft, at)
        
        case hydra.core.TypeApplication(value=sat):
            return _hoist_body_2(sat, at)
        
        case hydra.core.TypeList(value=sl):
            return _hoist_body_3(sl, at)
        
        case hydra.core.TypeSet(value=ss):
            return _hoist_body_4(ss, at)
        
        case hydra.core.TypeMaybe(value=sm):
            return _hoist_body_5(sm, at)
        
        case hydra.core.TypeMap(value=smt):
            return _hoist_body_6(smt, at)
        
        case hydra.core.TypePair(value=spt):
            return _hoist_body_7(spt, at)
        
        case hydra.core.TypeEither(value=set_):
            return _hoist_body_8(set_, at)
        
        case hydra.core.TypeForall(value=sfa):
            return _hoist_body_9(sfa, at)
        
        case _:
            return hydra.lib.maps.empty()

def build_type_subst(scheme_var_set: frozenset[hydra.core.Name], scheme_type: hydra.core.Type, actual_type: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Type]:
    return build_type_subst_go(scheme_var_set, hydra.rewriting.deannotate_type(scheme_type), hydra.rewriting.deannotate_type(actual_type))

class_mods_public = (cast(hydra.ext.java.syntax.ClassModifier, hydra.ext.java.syntax.ClassModifierPublic()),)

def cmp_decl_statement(aliases: T0) -> hydra.ext.java.syntax.BlockStatement:
    return hydra.ext.java.utils.variable_declaration_statement(aliases, hydra.ext.java.utils.java_int_type, hydra.ext.java.utils.java_identifier("cmp"), hydra.ext.java.utils.java_int_expression(0))

@lru_cache(1)
def cmp_not_zero_expr() -> hydra.ext.java.syntax.Expression:
    @lru_cache(1)
    def lhs() -> hydra.ext.java.syntax.EqualityExpression:
        return hydra.ext.java.utils.java_relational_expression_to_java_equality_expression(hydra.ext.java.utils.java_postfix_expression_to_java_relational_expression(cast(hydra.ext.java.syntax.PostfixExpression, hydra.ext.java.syntax.PostfixExpressionName(hydra.ext.java.syntax.ExpressionName(Nothing(), hydra.ext.java.utils.java_identifier("cmp"))))))
    rhs = hydra.ext.java.utils.java_postfix_expression_to_java_relational_expression(cast(hydra.ext.java.syntax.PostfixExpression, hydra.ext.java.syntax.PostfixExpressionPrimary(hydra.ext.java.utils.java_literal_to_java_primary(hydra.ext.java.utils.java_int(0)))))
    return hydra.ext.java.utils.java_equality_expression_to_java_expression(cast(hydra.ext.java.syntax.EqualityExpression, hydra.ext.java.syntax.EqualityExpressionNotEqual(hydra.ext.java.syntax.EqualityExpression_Binary(lhs(), rhs))))

def collect_lambda_domains(t: hydra.core.Term) -> tuple[frozenlist[hydra.core.Type], hydra.core.Term]:
    def _hoist_hydra_ext_java_coder_collect_lambda_domains_1(t: hydra.core.Term, v1: hydra.core.Function) -> tuple[frozenlist[hydra.core.Type], hydra.core.Term]:
        match v1:
            case hydra.core.FunctionLambda(value=lam):
                return hydra.lib.maybes.cases(lam.domain, ((), t), (lambda dom: (rest := collect_lambda_domains(lam.body), (hydra.lib.lists.cons(dom, hydra.lib.pairs.first(rest)), hydra.lib.pairs.second(rest)))[1]))
            
            case _:
                return ((), t)
    match hydra.rewriting.deannotate_term(t):
        case hydra.core.TermFunction(value=f):
            return _hoist_hydra_ext_java_coder_collect_lambda_domains_1(t, f)
        
        case _:
            return ((), t)

@lru_cache(1)
def java_comparable_ref_type() -> hydra.ext.java.syntax.ReferenceType:
    return cast(hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.ReferenceTypeClassOrInterface(cast(hydra.ext.java.syntax.ClassOrInterfaceType, hydra.ext.java.syntax.ClassOrInterfaceTypeClass(hydra.ext.java.syntax.ClassType((), cast(hydra.ext.java.syntax.ClassTypeQualifier, hydra.ext.java.syntax.ClassTypeQualifierNone()), hydra.ext.java.utils.java_type_identifier("Comparable"), ())))))

def comparable_compare_expr(other_var: str, fname: str) -> hydra.ext.java.syntax.Expression:
    @lru_cache(1)
    def arg() -> hydra.ext.java.syntax.Expression:
        return hydra.ext.java.utils.java_expression_name_to_java_expression(hydra.ext.java.utils.field_expression(hydra.ext.java.utils.java_identifier(other_var), hydra.ext.java.utils.java_identifier(fname)))
    @lru_cache(1)
    def cast_var() -> hydra.ext.java.syntax.MethodInvocation_Variant:
        return cast(hydra.ext.java.syntax.MethodInvocation_Variant, hydra.ext.java.syntax.MethodInvocation_VariantPrimary(hydra.ext.java.utils.java_expression_to_java_primary(hydra.ext.java.utils.java_cast_expression_to_java_expression(hydra.ext.java.utils.java_cast_expression(java_comparable_ref_type(), hydra.ext.java.utils.java_identifier_to_java_unary_expression(hydra.ext.java.syntax.Identifier(hydra.ext.java.utils.sanitize_java_name(fname))))))))
    @lru_cache(1)
    def header() -> hydra.ext.java.syntax.MethodInvocation_Header:
        return cast(hydra.ext.java.syntax.MethodInvocation_Header, hydra.ext.java.syntax.MethodInvocation_HeaderComplex(hydra.ext.java.syntax.MethodInvocation_Complex(cast_var(), (), hydra.ext.java.syntax.Identifier(hydra.ext.java.names.compare_to_method_name))))
    return hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.syntax.MethodInvocation(header(), (arg(),)))

def hash_code_compare_expr(other_var: str, fname: str) -> hydra.ext.java.syntax.Expression:
    @lru_cache(1)
    def header() -> hydra.ext.java.syntax.MethodInvocation_Header:
        return cast(hydra.ext.java.syntax.MethodInvocation_Header, hydra.ext.java.syntax.MethodInvocation_HeaderComplex(hydra.ext.java.syntax.MethodInvocation_Complex(cast(hydra.ext.java.syntax.MethodInvocation_Variant, hydra.ext.java.syntax.MethodInvocation_VariantType(hydra.ext.java.utils.java_type_name(hydra.ext.java.syntax.Identifier("Integer")))), (), hydra.ext.java.syntax.Identifier("compare"))))
    @lru_cache(1)
    def this_hash_code() -> hydra.ext.java.syntax.Expression:
        return hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.syntax.MethodInvocation(cast(hydra.ext.java.syntax.MethodInvocation_Header, hydra.ext.java.syntax.MethodInvocation_HeaderComplex(hydra.ext.java.syntax.MethodInvocation_Complex(cast(hydra.ext.java.syntax.MethodInvocation_Variant, hydra.ext.java.syntax.MethodInvocation_VariantExpression(hydra.ext.java.syntax.ExpressionName(Nothing(), hydra.ext.java.syntax.Identifier(hydra.ext.java.utils.sanitize_java_name(fname))))), (), hydra.ext.java.syntax.Identifier(hydra.ext.java.names.hash_code_method_name)))), ()))
    @lru_cache(1)
    def other_hash_code() -> hydra.ext.java.syntax.Expression:
        return hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.syntax.MethodInvocation(cast(hydra.ext.java.syntax.MethodInvocation_Header, hydra.ext.java.syntax.MethodInvocation_HeaderComplex(hydra.ext.java.syntax.MethodInvocation_Complex(cast(hydra.ext.java.syntax.MethodInvocation_Variant, hydra.ext.java.syntax.MethodInvocation_VariantExpression(hydra.ext.java.utils.field_expression(hydra.ext.java.utils.java_identifier(other_var), hydra.ext.java.utils.java_identifier(fname)))), (), hydra.ext.java.syntax.Identifier(hydra.ext.java.names.hash_code_method_name)))), ()))
    return hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.syntax.MethodInvocation(header(), (this_hash_code(), other_hash_code())))

def is_binary_type(typ: hydra.core.Type) -> bool:
    def _hoist_hydra_ext_java_coder_is_binary_type_1(v1: hydra.core.LiteralType) -> bool:
        match v1:
            case hydra.core.LiteralTypeBinary():
                return True
            
            case _:
                return False
    match hydra.rewriting.deannotate_type(typ):
        case hydra.core.TypeLiteral(value=lt):
            return _hoist_hydra_ext_java_coder_is_binary_type_1(lt)
        
        case _:
            return False

def is_non_comparable_type(typ: hydra.core.Type) -> bool:
    def _hoist_hydra_ext_java_coder_is_non_comparable_type_1(v1: hydra.core.LiteralType) -> bool:
        match v1:
            case hydra.core.LiteralTypeBinary():
                return True
            
            case _:
                return False
    match hydra.rewriting.deannotate_type(typ):
        case hydra.core.TypeList():
            return True
        
        case hydra.core.TypeSet():
            return True
        
        case hydra.core.TypeMap():
            return True
        
        case hydra.core.TypeMaybe():
            return True
        
        case hydra.core.TypePair():
            return True
        
        case hydra.core.TypeEither():
            return True
        
        case hydra.core.TypeFunction():
            return True
        
        case hydra.core.TypeLiteral(value=lt):
            return _hoist_hydra_ext_java_coder_is_non_comparable_type_1(lt)
        
        case hydra.core.TypeForall(value=ft):
            return is_non_comparable_type(ft.body)
        
        case _:
            return False

def compare_field_expr(other_var: str, ft: hydra.core.FieldType) -> hydra.ext.java.syntax.Expression:
    @lru_cache(1)
    def fname() -> str:
        return ft.name.value
    @lru_cache(1)
    def ftype() -> hydra.core.Type:
        return ft.type
    return hydra.lib.logic.if_else(is_binary_type(ftype()), (lambda : arrays_compare_expr(other_var, fname())), (lambda : hydra.lib.logic.if_else(is_non_comparable_type(ftype()), (lambda : hash_code_compare_expr(other_var, fname())), (lambda : comparable_compare_expr(other_var, fname())))))

def compare_and_return_stmts(other_var: str, f: hydra.core.FieldType) -> frozenlist[hydra.ext.java.syntax.BlockStatement]:
    return (cast(hydra.ext.java.syntax.BlockStatement, hydra.ext.java.syntax.BlockStatementStatement(hydra.ext.java.utils.java_assignment_statement(cast(hydra.ext.java.syntax.LeftHandSide, hydra.ext.java.syntax.LeftHandSideExpressionName(hydra.ext.java.syntax.ExpressionName(Nothing(), hydra.ext.java.utils.java_identifier("cmp")))), compare_field_expr(other_var, f)))), cast(hydra.ext.java.syntax.BlockStatement, hydra.ext.java.syntax.BlockStatementStatement(cast(hydra.ext.java.syntax.Statement, hydra.ext.java.syntax.StatementIfThen(hydra.ext.java.syntax.IfThenStatement(cmp_not_zero_expr(), hydra.ext.java.utils.java_return_statement(Just(hydra.ext.java.utils.java_expression_name_to_java_expression(hydra.ext.java.syntax.ExpressionName(Nothing(), hydra.ext.java.utils.java_identifier("cmp")))))))))))

def compare_to_body(aliases: T0, other_var: str, fields: frozenlist[hydra.core.FieldType]) -> frozenlist[hydra.ext.java.syntax.BlockStatement]:
    return hydra.lib.logic.if_else(hydra.lib.lists.null(fields), (lambda : (cast(hydra.ext.java.syntax.BlockStatement, hydra.ext.java.syntax.BlockStatementStatement(hydra.ext.java.utils.java_return_statement(Just(hydra.ext.java.utils.java_int_expression(0))))),)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(fields), 1), (lambda : (cast(hydra.ext.java.syntax.BlockStatement, hydra.ext.java.syntax.BlockStatementStatement(hydra.ext.java.utils.java_return_statement(Just(compare_field_expr(other_var, hydra.lib.lists.head(fields)))))),)), (lambda : hydra.lib.lists.concat2((cmp_decl_statement(aliases),), hydra.lib.lists.concat2(hydra.lib.lists.concat(hydra.lib.lists.map((lambda f: compare_and_return_stmts(other_var, f)), hydra.lib.lists.init(fields))), (cast(hydra.ext.java.syntax.BlockStatement, hydra.ext.java.syntax.BlockStatementStatement(hydra.ext.java.utils.java_return_statement(Just(compare_field_expr(other_var, hydra.lib.lists.last(fields)))))),)))))))

def compare_to_zero_clause(tmp_name: str, fname: str) -> hydra.ext.java.syntax.InclusiveOrExpression:
    @lru_cache(1)
    def compare_to_arg() -> hydra.ext.java.syntax.Expression:
        return hydra.ext.java.utils.java_expression_name_to_java_expression(hydra.ext.java.utils.field_expression(hydra.ext.java.utils.java_identifier(tmp_name), hydra.ext.java.utils.java_identifier(fname)))
    @lru_cache(1)
    def compare_to_var() -> hydra.ext.java.syntax.MethodInvocation_Variant:
        return cast(hydra.ext.java.syntax.MethodInvocation_Variant, hydra.ext.java.syntax.MethodInvocation_VariantExpression(hydra.ext.java.utils.field_expression(hydra.ext.java.syntax.Identifier("this"), hydra.ext.java.utils.java_identifier(fname))))
    @lru_cache(1)
    def compare_to_header() -> hydra.ext.java.syntax.MethodInvocation_Header:
        return cast(hydra.ext.java.syntax.MethodInvocation_Header, hydra.ext.java.syntax.MethodInvocation_HeaderComplex(hydra.ext.java.syntax.MethodInvocation_Complex(compare_to_var(), (), hydra.ext.java.syntax.Identifier(hydra.ext.java.names.compare_to_method_name))))
    @lru_cache(1)
    def lhs() -> hydra.ext.java.syntax.EqualityExpression:
        return hydra.ext.java.utils.java_relational_expression_to_java_equality_expression(hydra.ext.java.utils.java_postfix_expression_to_java_relational_expression(hydra.ext.java.utils.java_method_invocation_to_java_postfix_expression(hydra.ext.java.syntax.MethodInvocation(compare_to_header(), (compare_to_arg(),)))))
    rhs = hydra.ext.java.utils.java_postfix_expression_to_java_relational_expression(cast(hydra.ext.java.syntax.PostfixExpression, hydra.ext.java.syntax.PostfixExpressionPrimary(hydra.ext.java.utils.java_literal_to_java_primary(hydra.ext.java.utils.java_int(0)))))
    return hydra.ext.java.utils.java_equality_expression_to_java_inclusive_or_expression(cast(hydra.ext.java.syntax.EqualityExpression, hydra.ext.java.syntax.EqualityExpressionEqual(hydra.ext.java.syntax.EqualityExpression_Binary(lhs(), rhs))))

def constant_decl(java_name: str, aliases: hydra.ext.java.helpers.Aliases, name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments]:
    mods = (cast(hydra.ext.java.syntax.FieldModifier, hydra.ext.java.syntax.FieldModifierPublic()), cast(hydra.ext.java.syntax.FieldModifier, hydra.ext.java.syntax.FieldModifierStatic()), cast(hydra.ext.java.syntax.FieldModifier, hydra.ext.java.syntax.FieldModifierFinal()))
    @lru_cache(1)
    def name_name() -> hydra.ext.java.syntax.Identifier:
        return hydra.ext.java.utils.name_to_java_name(aliases, hydra.core.Name("hydra.core.Name"))
    return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda g: hydra.lib.flows.bind(hydra.schemas.graph_to_type_context(g), (lambda tc: (env := hydra.ext.java.helpers.JavaEnvironment(aliases, tc), hydra.lib.flows.bind(encode_type(aliases, hydra.lib.sets.empty(), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Name")))), (lambda jt: hydra.lib.flows.bind(encode_term(env, cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(name.value))))), (lambda arg: (init := cast(hydra.ext.java.syntax.VariableInitializer, hydra.ext.java.syntax.VariableInitializerExpression(hydra.ext.java.utils.java_constructor_call(hydra.ext.java.utils.java_constructor_name(name_name(), Nothing()), (arg,), Nothing()))), var := hydra.ext.java.utils.java_variable_declarator(hydra.ext.java.syntax.Identifier(java_name), Just(init)), hydra.lib.flows.pure(no_comment(hydra.ext.java.utils.java_member_field(mods, jt, var))))[2])))))[1]))))

def constant_decl_for_field_type(aliases: hydra.ext.java.helpers.Aliases, ftyp: hydra.core.FieldType) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments]:
    @lru_cache(1)
    def name() -> hydra.core.Name:
        return ftyp.name
    @lru_cache(1)
    def java_name() -> str:
        return hydra.lib.strings.cat2("FIELD_NAME_", hydra.formatting.non_alnum_to_underscores(hydra.formatting.convert_case(hydra.util.CaseConvention.CAMEL, hydra.util.CaseConvention.UPPER_SNAKE, name().value)))
    return constant_decl(java_name(), aliases, name())

def constant_decl_for_type_name(aliases: hydra.ext.java.helpers.Aliases, name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclarationWithComments]:
    return constant_decl("TYPE_NAME", aliases, name)

def construct_elements_interface(mod: hydra.module.Module, members: frozenlist[hydra.ext.java.syntax.InterfaceMemberDeclaration]) -> tuple[hydra.core.Name, hydra.ext.java.syntax.CompilationUnit]:
    @lru_cache(1)
    def pkg() -> hydra.ext.java.syntax.PackageDeclaration:
        return hydra.ext.java.utils.java_package_declaration(mod.namespace)
    mods = (cast(hydra.ext.java.syntax.InterfaceModifier, hydra.ext.java.syntax.InterfaceModifierPublic()),)
    @lru_cache(1)
    def class_name() -> str:
        return elements_class_name(mod.namespace)
    @lru_cache(1)
    def el_name() -> hydra.core.Name:
        return hydra.names.unqualify_name(hydra.module.QualifiedName(Just(mod.namespace), class_name()))
    @lru_cache(1)
    def body() -> hydra.ext.java.syntax.InterfaceBody:
        return hydra.ext.java.syntax.InterfaceBody(members)
    @lru_cache(1)
    def itf() -> hydra.ext.java.syntax.TypeDeclaration:
        return cast(hydra.ext.java.syntax.TypeDeclaration, hydra.ext.java.syntax.TypeDeclarationInterface(cast(hydra.ext.java.syntax.InterfaceDeclaration, hydra.ext.java.syntax.InterfaceDeclarationNormalInterface(hydra.ext.java.syntax.NormalInterfaceDeclaration(mods, hydra.ext.java.utils.java_type_identifier(class_name()), (), (), body())))))
    @lru_cache(1)
    def decl() -> hydra.ext.java.syntax.TypeDeclarationWithComments:
        return hydra.ext.java.syntax.TypeDeclarationWithComments(itf(), mod.description)
    return (el_name(), cast(hydra.ext.java.syntax.CompilationUnit, hydra.ext.java.syntax.CompilationUnitOrdinary(hydra.ext.java.syntax.OrdinaryCompilationUnit(Just(pkg()), (), (decl(),)))))

def interface_types(is_ser: bool, aliases: hydra.ext.java.helpers.Aliases, tparams: frozenlist[hydra.ext.java.syntax.TypeParameter], el_name: hydra.core.Name) -> frozenlist[hydra.ext.java.syntax.InterfaceType]:
    @lru_cache(1)
    def java_serializable_type() -> hydra.ext.java.syntax.InterfaceType:
        return hydra.ext.java.syntax.InterfaceType(hydra.ext.java.syntax.ClassType((), cast(hydra.ext.java.syntax.ClassTypeQualifier, hydra.ext.java.syntax.ClassTypeQualifierNone()), hydra.ext.java.utils.java_type_identifier("Serializable"), ()))
    @lru_cache(1)
    def self_type_arg() -> hydra.ext.java.syntax.TypeArgument:
        return cast(hydra.ext.java.syntax.TypeArgument, hydra.ext.java.syntax.TypeArgumentReference(hydra.ext.java.utils.name_to_java_reference_type(aliases, False, hydra.lib.lists.map((lambda tp_: hydra.ext.java.utils.type_parameter_to_type_argument(tp_)), tparams), el_name, Nothing())))
    @lru_cache(1)
    def java_comparable_type() -> hydra.ext.java.syntax.InterfaceType:
        return hydra.ext.java.syntax.InterfaceType(hydra.ext.java.syntax.ClassType((), cast(hydra.ext.java.syntax.ClassTypeQualifier, hydra.ext.java.syntax.ClassTypeQualifierNone()), hydra.ext.java.utils.java_type_identifier("Comparable"), (self_type_arg(),)))
    return hydra.lib.logic.if_else(is_ser, (lambda : (java_serializable_type(), java_comparable_type())), (lambda : ()))

def record_compare_to_method(aliases: hydra.ext.java.helpers.Aliases, tparams: T0, el_name: hydra.core.Name, fields: frozenlist[hydra.core.FieldType]) -> hydra.ext.java.syntax.ClassBodyDeclaration:
    anns = (hydra.ext.java.utils.override_annotation, hydra.ext.java.utils.suppress_warnings_unchecked_annotation)
    mods = (cast(hydra.ext.java.syntax.MethodModifier, hydra.ext.java.syntax.MethodModifierPublic()),)
    @lru_cache(1)
    def param() -> hydra.ext.java.syntax.FormalParameter:
        return hydra.ext.java.utils.java_type_to_java_formal_parameter(hydra.ext.java.utils.java_type_from_type_name(aliases, el_name), hydra.core.Name(hydra.ext.java.names.other_instance_name))
    result = hydra.ext.java.utils.java_type_to_java_result(hydra.ext.java.utils.java_int_type)
    return hydra.ext.java.utils.method_declaration(mods, (), anns, hydra.ext.java.names.compare_to_method_name, (param(),), result, Just(compare_to_body(aliases, hydra.ext.java.names.other_instance_name, fields)))

def field_type_to_formal_param(aliases: hydra.ext.java.helpers.Aliases, ft: hydra.core.FieldType) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.FormalParameter]:
    return hydra.lib.flows.bind(encode_type(aliases, hydra.lib.sets.empty(), ft.type), (lambda jt: hydra.lib.flows.pure(hydra.ext.java.utils.java_type_to_java_formal_parameter(jt, ft.name))))

def record_constructor(aliases: hydra.ext.java.helpers.Aliases, el_name: hydra.core.Name, fields: frozenlist[hydra.core.FieldType]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclaration]:
    @lru_cache(1)
    def assign_stmts() -> frozenlist[hydra.ext.java.syntax.BlockStatement]:
        return hydra.lib.lists.map((lambda f: cast(hydra.ext.java.syntax.BlockStatement, hydra.ext.java.syntax.BlockStatementStatement(hydra.ext.java.utils.to_assign_stmt(f.name)))), fields)
    return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda f: field_type_to_formal_param(aliases, f)), fields), (lambda params: hydra.lib.flows.pure(hydra.ext.java.utils.make_constructor(aliases, el_name, False, params, assign_stmts()))))

def equals_clause(tmp_name: str, fname: str) -> hydra.ext.java.syntax.InclusiveOrExpression:
    @lru_cache(1)
    def this_arg() -> hydra.ext.java.syntax.Expression:
        return hydra.ext.java.utils.java_expression_name_to_java_expression(hydra.ext.java.utils.field_expression(hydra.ext.java.syntax.Identifier("this"), hydra.ext.java.utils.java_identifier(fname)))
    @lru_cache(1)
    def other_arg() -> hydra.ext.java.syntax.Expression:
        return hydra.ext.java.utils.java_expression_name_to_java_expression(hydra.ext.java.utils.field_expression(hydra.ext.java.utils.java_identifier(tmp_name), hydra.ext.java.utils.java_identifier(fname)))
    @lru_cache(1)
    def header() -> hydra.ext.java.syntax.MethodInvocation_Header:
        return cast(hydra.ext.java.syntax.MethodInvocation_Header, hydra.ext.java.syntax.MethodInvocation_HeaderComplex(hydra.ext.java.syntax.MethodInvocation_Complex(cast(hydra.ext.java.syntax.MethodInvocation_Variant, hydra.ext.java.syntax.MethodInvocation_VariantType(hydra.ext.java.utils.java_type_name(hydra.ext.java.syntax.Identifier("java.util.Objects")))), (), hydra.ext.java.syntax.Identifier(hydra.ext.java.names.equals_method_name))))
    return hydra.ext.java.utils.java_postfix_expression_to_java_inclusive_or_expression(hydra.ext.java.utils.java_method_invocation_to_java_postfix_expression(hydra.ext.java.syntax.MethodInvocation(header(), (this_arg(), other_arg()))))

def is_big_numeric_type(typ: hydra.core.Type) -> bool:
    def _hoist_hydra_ext_java_coder_is_big_numeric_type_1(v1: hydra.core.FloatType) -> bool:
        match v1:
            case hydra.core.FloatType.BIGFLOAT:
                return True
            
            case _:
                return False
    def _hoist_hydra_ext_java_coder_is_big_numeric_type_2(v1: hydra.core.IntegerType) -> bool:
        match v1:
            case hydra.core.IntegerType.BIGINT:
                return True
            
            case _:
                return False
    def _hoist_hydra_ext_java_coder_is_big_numeric_type_3(v1: hydra.core.LiteralType) -> bool:
        match v1:
            case hydra.core.LiteralTypeFloat(value=ft):
                return _hoist_hydra_ext_java_coder_is_big_numeric_type_1(ft)
            
            case hydra.core.LiteralTypeInteger(value=it):
                return _hoist_hydra_ext_java_coder_is_big_numeric_type_2(it)
            
            case _:
                return False
    match hydra.rewriting.deannotate_type(typ):
        case hydra.core.TypeLiteral(value=lt):
            return _hoist_hydra_ext_java_coder_is_big_numeric_type_3(lt)
        
        case _:
            return False

def eq_clause(tmp_name: str, ft: hydra.core.FieldType) -> hydra.ext.java.syntax.InclusiveOrExpression:
    @lru_cache(1)
    def fname() -> str:
        return ft.name.value
    @lru_cache(1)
    def ftype() -> hydra.core.Type:
        return ft.type
    return hydra.lib.logic.if_else(is_binary_type(ftype()), (lambda : arrays_equals_clause(tmp_name, fname())), (lambda : hydra.lib.logic.if_else(is_big_numeric_type(ftype()), (lambda : compare_to_zero_clause(tmp_name, fname())), (lambda : equals_clause(tmp_name, fname())))))

def record_equals_method(aliases: hydra.ext.java.helpers.Aliases, el_name: hydra.core.Name, fields: frozenlist[hydra.core.FieldType]) -> hydra.ext.java.syntax.ClassBodyDeclaration:
    anns = (hydra.ext.java.utils.override_annotation,)
    mods = (cast(hydra.ext.java.syntax.MethodModifier, hydra.ext.java.syntax.MethodModifierPublic()),)
    @lru_cache(1)
    def param() -> hydra.ext.java.syntax.FormalParameter:
        return hydra.ext.java.utils.java_type_to_java_formal_parameter(hydra.ext.java.utils.java_ref_type((), Nothing(), "Object"), hydra.core.Name(hydra.ext.java.names.other_instance_name))
    result = hydra.ext.java.utils.java_type_to_java_result(hydra.ext.java.utils.java_boolean_type)
    tmp_name = "o"
    @lru_cache(1)
    def instance_of_stmt() -> hydra.ext.java.syntax.BlockStatement:
        return cast(hydra.ext.java.syntax.BlockStatement, hydra.ext.java.syntax.BlockStatementStatement(cast(hydra.ext.java.syntax.Statement, hydra.ext.java.syntax.StatementIfThen(hydra.ext.java.syntax.IfThenStatement(hydra.ext.java.utils.java_unary_expression_to_java_expression(cast(hydra.ext.java.syntax.UnaryExpression, hydra.ext.java.syntax.UnaryExpressionOther(cast(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus, hydra.ext.java.syntax.UnaryExpressionNotPlusMinusNot(hydra.ext.java.utils.java_relational_expression_to_java_unary_expression(hydra.ext.java.utils.java_instance_of(hydra.ext.java.utils.java_identifier_to_java_relational_expression(hydra.ext.java.utils.java_identifier(hydra.ext.java.names.other_instance_name)), hydra.ext.java.utils.name_to_java_reference_type(aliases, False, (), el_name, Nothing())))))))), hydra.ext.java.utils.java_return_statement(Just(hydra.ext.java.utils.java_boolean_expression(False))))))))
    @lru_cache(1)
    def cast_stmt() -> hydra.ext.java.syntax.BlockStatement:
        return hydra.ext.java.utils.variable_declaration_statement(aliases, hydra.ext.java.utils.java_type_from_type_name(aliases, el_name), hydra.ext.java.utils.java_identifier(tmp_name), hydra.ext.java.utils.java_cast_expression_to_java_expression(hydra.ext.java.utils.java_cast_expression(hydra.ext.java.utils.name_to_java_reference_type(aliases, False, (), el_name, Nothing()), hydra.ext.java.utils.java_identifier_to_java_unary_expression(hydra.ext.java.syntax.Identifier(hydra.ext.java.utils.sanitize_java_name(hydra.ext.java.names.other_instance_name))))))
    @lru_cache(1)
    def return_all_fields_equal() -> hydra.ext.java.syntax.BlockStatement:
        return cast(hydra.ext.java.syntax.BlockStatement, hydra.ext.java.syntax.BlockStatementStatement(hydra.ext.java.utils.java_return_statement(Just(hydra.lib.logic.if_else(hydra.lib.lists.null(fields), (lambda : hydra.ext.java.utils.java_boolean_expression(True)), (lambda : hydra.ext.java.utils.java_conditional_and_expression_to_java_expression(hydra.ext.java.syntax.ConditionalAndExpression(hydra.lib.lists.map((lambda f: eq_clause(tmp_name, f)), fields)))))))))
    return hydra.ext.java.utils.method_declaration(mods, (), anns, hydra.ext.java.names.equals_method_name, (param(),), result, Just((instance_of_stmt(), cast_stmt(), return_all_fields_equal())))

first20_primes = (2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71)

def hash_code_mult_pair(i: int, fname: hydra.core.Name) -> hydra.ext.java.syntax.MultiplicativeExpression:
    @lru_cache(1)
    def fname_str() -> str:
        return fname.value
    @lru_cache(1)
    def lhs() -> hydra.ext.java.syntax.MultiplicativeExpression:
        return cast(hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.MultiplicativeExpressionUnary(hydra.ext.java.utils.java_primary_to_java_unary_expression(hydra.ext.java.utils.java_literal_to_java_primary(hydra.ext.java.utils.java_int(i)))))
    @lru_cache(1)
    def rhs() -> hydra.ext.java.syntax.UnaryExpression:
        return hydra.ext.java.utils.java_postfix_expression_to_java_unary_expression(hydra.ext.java.utils.java_method_invocation_to_java_postfix_expression(hydra.ext.java.syntax.MethodInvocation(cast(hydra.ext.java.syntax.MethodInvocation_Header, hydra.ext.java.syntax.MethodInvocation_HeaderComplex(hydra.ext.java.syntax.MethodInvocation_Complex(cast(hydra.ext.java.syntax.MethodInvocation_Variant, hydra.ext.java.syntax.MethodInvocation_VariantType(hydra.ext.java.utils.java_type_name(hydra.ext.java.syntax.Identifier("java.util.Objects")))), (), hydra.ext.java.syntax.Identifier(hydra.ext.java.names.hash_code_method_name)))), (hydra.ext.java.utils.java_expression_name_to_java_expression(hydra.ext.java.syntax.ExpressionName(Nothing(), hydra.ext.java.syntax.Identifier(hydra.ext.java.utils.sanitize_java_name(fname_str())))),))))
    return cast(hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.MultiplicativeExpressionTimes(hydra.ext.java.syntax.MultiplicativeExpression_Binary(lhs(), rhs())))

def record_hash_code_method(fields: frozenlist[hydra.core.FieldType]) -> hydra.ext.java.syntax.ClassBodyDeclaration:
    anns = (hydra.ext.java.utils.override_annotation,)
    mods = (cast(hydra.ext.java.syntax.MethodModifier, hydra.ext.java.syntax.MethodModifierPublic()),)
    result = hydra.ext.java.utils.java_type_to_java_result(hydra.ext.java.utils.java_int_type)
    @lru_cache(1)
    def return_sum() -> hydra.ext.java.syntax.BlockStatement:
        return cast(hydra.ext.java.syntax.BlockStatement, hydra.ext.java.syntax.BlockStatementStatement(hydra.lib.logic.if_else(hydra.lib.lists.null(fields), (lambda : hydra.ext.java.utils.java_return_statement(Just(hydra.ext.java.utils.java_int_expression(0)))), (lambda : hydra.ext.java.utils.java_return_statement(Just(hydra.ext.java.utils.java_additive_expression_to_java_expression(hydra.ext.java.utils.add_expressions(hydra.lib.lists.zip_with((lambda x1, x2: hash_code_mult_pair(x1, x2)), first20_primes, hydra.lib.lists.map((lambda f: f.name), fields))))))))))
    return hydra.ext.java.utils.method_declaration(mods, (), anns, hydra.ext.java.names.hash_code_method_name, (), result, Just((return_sum(),)))

def record_member_var(aliases: hydra.ext.java.helpers.Aliases, ft: hydra.core.FieldType) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclaration]:
    mods = (cast(hydra.ext.java.syntax.FieldModifier, hydra.ext.java.syntax.FieldModifierPublic()), cast(hydra.ext.java.syntax.FieldModifier, hydra.ext.java.syntax.FieldModifierFinal()))
    @lru_cache(1)
    def fname() -> hydra.core.Name:
        return ft.name
    @lru_cache(1)
    def ftype() -> hydra.core.Type:
        return ft.type
    return hydra.lib.flows.bind(encode_type(aliases, hydra.lib.sets.empty(), ftype()), (lambda jt: hydra.lib.flows.pure(hydra.ext.java.utils.java_member_field(mods, jt, hydra.ext.java.utils.field_name_to_java_variable_declarator(fname())))))

def record_with_method(aliases: hydra.ext.java.helpers.Aliases, el_name: hydra.core.Name, fields: frozenlist[hydra.core.FieldType], field: hydra.core.FieldType) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.ClassBodyDeclaration]:
    mods = (cast(hydra.ext.java.syntax.MethodModifier, hydra.ext.java.syntax.MethodModifierPublic()),)
    @lru_cache(1)
    def anns() -> frozenlist[T0]:
        return ()
    @lru_cache(1)
    def method_name() -> str:
        return hydra.lib.strings.cat2("with", hydra.formatting.non_alnum_to_underscores(hydra.formatting.capitalize(field.name.value)))
    @lru_cache(1)
    def result() -> hydra.ext.java.syntax.Result:
        return hydra.ext.java.utils.reference_type_to_result(hydra.ext.java.utils.name_to_java_reference_type(aliases, False, (), el_name, Nothing()))
    @lru_cache(1)
    def cons_id() -> hydra.ext.java.syntax.Identifier:
        return hydra.ext.java.syntax.Identifier(hydra.ext.java.utils.sanitize_java_name(hydra.names.local_name_of(el_name)))
    @lru_cache(1)
    def field_args() -> frozenlist[hydra.ext.java.syntax.Expression]:
        return hydra.lib.lists.map((lambda f: hydra.ext.java.utils.field_name_to_java_expression(f.name)), fields)
    @lru_cache(1)
    def return_stmt() -> hydra.ext.java.syntax.BlockStatement:
        return cast(hydra.ext.java.syntax.BlockStatement, hydra.ext.java.syntax.BlockStatementStatement(hydra.ext.java.utils.java_return_statement(Just(hydra.ext.java.utils.java_constructor_call(hydra.ext.java.utils.java_constructor_name(cons_id(), Nothing()), field_args(), Nothing())))))
    return hydra.lib.flows.bind(field_type_to_formal_param(aliases, field), (lambda param: hydra.lib.flows.pure(hydra.ext.java.utils.method_declaration(mods, (), anns(), method_name(), (param,), result(), Just((return_stmt(),))))))

def serializable_types(is_ser: bool) -> frozenlist[hydra.ext.java.syntax.InterfaceType]:
    @lru_cache(1)
    def java_serializable_type() -> hydra.ext.java.syntax.InterfaceType:
        return hydra.ext.java.syntax.InterfaceType(hydra.ext.java.syntax.ClassType((), cast(hydra.ext.java.syntax.ClassTypeQualifier, hydra.ext.java.syntax.ClassTypeQualifierNone()), hydra.ext.java.utils.java_type_identifier("Serializable"), ()))
    return hydra.lib.logic.if_else(is_ser, (lambda : (java_serializable_type(),)), (lambda : ()))

@lru_cache(1)
def tag_cmp_not_zero_expr() -> hydra.ext.java.syntax.Expression:
    @lru_cache(1)
    def lhs() -> hydra.ext.java.syntax.EqualityExpression:
        return hydra.ext.java.utils.java_relational_expression_to_java_equality_expression(hydra.ext.java.utils.java_postfix_expression_to_java_relational_expression(cast(hydra.ext.java.syntax.PostfixExpression, hydra.ext.java.syntax.PostfixExpressionName(hydra.ext.java.syntax.ExpressionName(Nothing(), hydra.ext.java.utils.java_identifier("tagCmp"))))))
    rhs = hydra.ext.java.utils.java_postfix_expression_to_java_relational_expression(cast(hydra.ext.java.syntax.PostfixExpression, hydra.ext.java.syntax.PostfixExpressionPrimary(hydra.ext.java.utils.java_literal_to_java_primary(hydra.ext.java.utils.java_int(0)))))
    return hydra.ext.java.utils.java_equality_expression_to_java_expression(cast(hydra.ext.java.syntax.EqualityExpression, hydra.ext.java.syntax.EqualityExpressionNotEqual(hydra.ext.java.syntax.EqualityExpression_Binary(lhs(), rhs))))

@lru_cache(1)
def tag_compare_expr() -> hydra.ext.java.syntax.Expression:
    @lru_cache(1)
    def this_get_class() -> hydra.ext.java.syntax.MethodInvocation:
        return hydra.ext.java.syntax.MethodInvocation(cast(hydra.ext.java.syntax.MethodInvocation_Header, hydra.ext.java.syntax.MethodInvocation_HeaderComplex(hydra.ext.java.syntax.MethodInvocation_Complex(cast(hydra.ext.java.syntax.MethodInvocation_Variant, hydra.ext.java.syntax.MethodInvocation_VariantPrimary(hydra.ext.java.utils.java_expression_to_java_primary(hydra.ext.java.utils.java_this))), (), hydra.ext.java.syntax.Identifier("getClass")))), ())
    @lru_cache(1)
    def this_get_name() -> hydra.ext.java.syntax.MethodInvocation:
        return hydra.ext.java.syntax.MethodInvocation(cast(hydra.ext.java.syntax.MethodInvocation_Header, hydra.ext.java.syntax.MethodInvocation_HeaderComplex(hydra.ext.java.syntax.MethodInvocation_Complex(cast(hydra.ext.java.syntax.MethodInvocation_Variant, hydra.ext.java.syntax.MethodInvocation_VariantPrimary(hydra.ext.java.utils.java_method_invocation_to_java_primary(this_get_class()))), (), hydra.ext.java.syntax.Identifier("getName")))), ())
    @lru_cache(1)
    def other_get_class() -> hydra.ext.java.syntax.MethodInvocation:
        return hydra.ext.java.syntax.MethodInvocation(cast(hydra.ext.java.syntax.MethodInvocation_Header, hydra.ext.java.syntax.MethodInvocation_HeaderComplex(hydra.ext.java.syntax.MethodInvocation_Complex(cast(hydra.ext.java.syntax.MethodInvocation_Variant, hydra.ext.java.syntax.MethodInvocation_VariantExpression(hydra.ext.java.syntax.ExpressionName(Nothing(), hydra.ext.java.syntax.Identifier(hydra.ext.java.names.other_instance_name)))), (), hydra.ext.java.syntax.Identifier("getClass")))), ())
    @lru_cache(1)
    def other_get_name() -> hydra.ext.java.syntax.MethodInvocation:
        return hydra.ext.java.syntax.MethodInvocation(cast(hydra.ext.java.syntax.MethodInvocation_Header, hydra.ext.java.syntax.MethodInvocation_HeaderComplex(hydra.ext.java.syntax.MethodInvocation_Complex(cast(hydra.ext.java.syntax.MethodInvocation_Variant, hydra.ext.java.syntax.MethodInvocation_VariantPrimary(hydra.ext.java.utils.java_method_invocation_to_java_primary(other_get_class()))), (), hydra.ext.java.syntax.Identifier("getName")))), ())
    return hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.syntax.MethodInvocation(cast(hydra.ext.java.syntax.MethodInvocation_Header, hydra.ext.java.syntax.MethodInvocation_HeaderComplex(hydra.ext.java.syntax.MethodInvocation_Complex(cast(hydra.ext.java.syntax.MethodInvocation_Variant, hydra.ext.java.syntax.MethodInvocation_VariantPrimary(hydra.ext.java.utils.java_method_invocation_to_java_primary(this_get_name()))), (), hydra.ext.java.syntax.Identifier(hydra.ext.java.names.compare_to_method_name)))), (hydra.ext.java.utils.java_method_invocation_to_java_expression(other_get_name()),)))

def variant_compare_to_method(aliases: hydra.ext.java.helpers.Aliases, tparams: T0, parent_name: hydra.core.Name, variant_name: hydra.core.Name, fields: frozenlist[hydra.core.FieldType]) -> hydra.ext.java.syntax.ClassBodyDeclaration:
    anns = (hydra.ext.java.utils.override_annotation, hydra.ext.java.utils.suppress_warnings_unchecked_annotation)
    mods = (cast(hydra.ext.java.syntax.MethodModifier, hydra.ext.java.syntax.MethodModifierPublic()),)
    @lru_cache(1)
    def param() -> hydra.ext.java.syntax.FormalParameter:
        return hydra.ext.java.utils.java_type_to_java_formal_parameter(hydra.ext.java.utils.java_type_from_type_name(aliases, parent_name), hydra.core.Name(hydra.ext.java.names.other_instance_name))
    result = hydra.ext.java.utils.java_type_to_java_result(hydra.ext.java.utils.java_int_type)
    var_tmp_name = "o"
    @lru_cache(1)
    def tag_decl_stmt() -> hydra.ext.java.syntax.BlockStatement:
        return hydra.ext.java.utils.variable_declaration_statement(aliases, hydra.ext.java.utils.java_int_type, hydra.ext.java.utils.java_identifier("tagCmp"), tag_compare_expr())
    @lru_cache(1)
    def tag_return_stmt() -> hydra.ext.java.syntax.BlockStatement:
        return cast(hydra.ext.java.syntax.BlockStatement, hydra.ext.java.syntax.BlockStatementStatement(cast(hydra.ext.java.syntax.Statement, hydra.ext.java.syntax.StatementIfThen(hydra.ext.java.syntax.IfThenStatement(tag_cmp_not_zero_expr(), hydra.ext.java.utils.java_return_statement(Just(hydra.ext.java.utils.java_expression_name_to_java_expression(hydra.ext.java.syntax.ExpressionName(Nothing(), hydra.ext.java.utils.java_identifier("tagCmp"))))))))))
    @lru_cache(1)
    def variant_java_type() -> hydra.ext.java.syntax.Type:
        return hydra.ext.java.utils.java_type_from_type_name(aliases, variant_name)
    @lru_cache(1)
    def cast_other_expr() -> hydra.ext.java.syntax.Expression:
        return hydra.ext.java.utils.java_cast_expression_to_java_expression(hydra.ext.java.utils.java_cast_expression(hydra.ext.java.utils.name_to_java_reference_type(aliases, False, (), variant_name, Nothing()), hydra.ext.java.utils.java_identifier_to_java_unary_expression(hydra.ext.java.syntax.Identifier(hydra.ext.java.names.other_instance_name))))
    @lru_cache(1)
    def cast_decl_stmt() -> hydra.ext.java.syntax.BlockStatement:
        return hydra.ext.java.utils.variable_declaration_statement(aliases, variant_java_type(), hydra.ext.java.utils.java_identifier(var_tmp_name), cast_other_expr())
    empty_return = (cast(hydra.ext.java.syntax.BlockStatement, hydra.ext.java.syntax.BlockStatementStatement(hydra.ext.java.utils.java_return_statement(Just(hydra.ext.java.utils.java_int_expression(0))))),)
    @lru_cache(1)
    def value_compare_stmt() -> frozenlist[hydra.ext.java.syntax.BlockStatement]:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(fields), (lambda : empty_return), (lambda : hydra.lib.lists.concat2((cast_decl_stmt(),), compare_to_body(aliases, var_tmp_name, fields))))
    @lru_cache(1)
    def body() -> frozenlist[hydra.ext.java.syntax.BlockStatement]:
        return hydra.lib.lists.concat2((tag_decl_stmt(), tag_return_stmt()), value_compare_stmt())
    return hydra.ext.java.utils.method_declaration(mods, (), anns, hydra.ext.java.names.compare_to_method_name, (param(),), result, Just(body()))

def declaration_for_record_type_(is_inner: bool, is_ser: bool, aliases: hydra.ext.java.helpers.Aliases, tparams: frozenlist[hydra.ext.java.syntax.TypeParameter], el_name: hydra.core.Name, parent_name: Maybe[hydra.core.Name], fields: frozenlist[hydra.core.FieldType]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration]:
    return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda f: record_member_var(aliases, f)), fields), (lambda member_vars: hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda p: add_comment(hydra.lib.pairs.first(p), hydra.lib.pairs.second(p))), hydra.lib.lists.zip(member_vars, fields)), (lambda member_vars_: hydra.lib.flows.bind(hydra.lib.logic.if_else(hydra.lib.equality.gt(hydra.lib.lists.length(fields), 1), (lambda : hydra.lib.flows.map_list((lambda f: record_with_method(aliases, el_name, fields, f)), fields)), (lambda : hydra.lib.flows.pure(()))), (lambda with_methods: hydra.lib.flows.bind(record_constructor(aliases, el_name, fields), (lambda cons: hydra.lib.flows.bind(hydra.lib.logic.if_else(is_inner, (lambda : hydra.lib.flows.pure(())), (lambda : hydra.lib.flows.bind(constant_decl_for_type_name(aliases, el_name), (lambda d: hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda f: constant_decl_for_field_type(aliases, f)), fields), (lambda dfields: hydra.lib.flows.pure(hydra.lib.lists.cons(d, dfields)))))))), (lambda tn: (comparable_methods := hydra.lib.maybes.cases(parent_name, hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.logic.not_(is_inner), is_ser), (lambda : (record_compare_to_method(aliases, tparams, el_name, fields),)), (lambda : ())), (lambda pn: hydra.lib.logic.if_else(is_ser, (lambda : (variant_compare_to_method(aliases, tparams, pn, el_name, fields),)), (lambda : ())))), body_decls := hydra.lib.lists.concat2(tn, hydra.lib.lists.concat2(member_vars_, hydra.lib.lists.map((lambda x: no_comment(x)), hydra.lib.lists.concat2((cons, record_equals_method(aliases, el_name, fields), record_hash_code_method(fields)), hydra.lib.lists.concat2(comparable_methods, with_methods))))), ifaces := hydra.lib.logic.if_else(is_inner, (lambda : serializable_types(is_ser)), (lambda : interface_types(is_ser, aliases, tparams, el_name))), hydra.lib.flows.pure(hydra.ext.java.utils.java_class_declaration(aliases, tparams, el_name, class_mods_public, Nothing(), ifaces, body_decls)))[3]))))))))))

def declaration_for_record_type(is_inner: bool, is_ser: bool, aliases: hydra.ext.java.helpers.Aliases, tparams: frozenlist[hydra.ext.java.syntax.TypeParameter], el_name: hydra.core.Name, fields: frozenlist[hydra.core.FieldType]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration]:
    return declaration_for_record_type_(is_inner, is_ser, aliases, tparams, el_name, Nothing(), fields)

def declaration_for_union_type(is_ser: bool, aliases: hydra.ext.java.helpers.Aliases, tparams: frozenlist[hydra.ext.java.syntax.TypeParameter], el_name: hydra.core.Name, fields: frozenlist[hydra.core.FieldType]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration]:
    return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda ft: (fname := ft.name, ftype := ft.type, rfields := hydra.lib.logic.if_else(hydra.schemas.is_unit_type(hydra.rewriting.deannotate_type(ftype)), (lambda : ()), (lambda : (hydra.core.FieldType(hydra.core.Name("value"), hydra.rewriting.deannotate_type(ftype)),))), var_name := hydra.ext.java.utils.variant_class_name(False, el_name, fname), hydra.lib.flows.bind(declaration_for_record_type_(True, is_ser, aliases, (), var_name, hydra.lib.logic.if_else(is_ser, (lambda : Just(el_name)), (lambda : Nothing())), rfields), (lambda inner_decl: hydra.lib.flows.pure(augment_variant_class(aliases, tparams, el_name, inner_decl)))))[4]), fields), (lambda variant_classes: (variant_decls := hydra.lib.lists.map((lambda vc: cast(hydra.ext.java.syntax.ClassBodyDeclaration, hydra.ext.java.syntax.ClassBodyDeclarationClassMember(cast(hydra.ext.java.syntax.ClassMemberDeclaration, hydra.ext.java.syntax.ClassMemberDeclarationClass(vc))))), variant_classes), hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda pair: add_comment(hydra.lib.pairs.first(pair), hydra.lib.pairs.second(pair))), hydra.lib.lists.zip(variant_decls, fields)), (lambda variant_decls_: (private_const := hydra.ext.java.utils.make_constructor(aliases, el_name, True, (), ()), accept_decl := hydra.ext.java.utils.to_accept_method(True, tparams), vtparams := hydra.lib.lists.concat2(tparams, (hydra.ext.java.utils.java_type_parameter(hydra.ext.java.names.visitor_return_parameter),)), visitor_methods := hydra.lib.lists.map((lambda ft: (fname := ft.name, type_args := hydra.lib.lists.map((lambda tp: hydra.ext.java.utils.type_parameter_to_type_argument(tp)), tparams), var_ref := hydra.ext.java.utils.java_class_type_to_java_type(hydra.ext.java.utils.name_to_java_class_type(aliases, False, type_args, hydra.ext.java.utils.variant_class_name(False, el_name, fname), Nothing())), param := hydra.ext.java.utils.java_type_to_java_formal_parameter(var_ref, hydra.core.Name("instance")), result_r := hydra.ext.java.utils.java_type_to_java_result(cast(hydra.ext.java.syntax.Type, hydra.ext.java.syntax.TypeReference(hydra.ext.java.utils.visitor_type_variable))), hydra.ext.java.utils.interface_method_declaration((), (), hydra.ext.java.names.visit_method_name, (param,), result_r, Nothing()))[5]), fields), visitor_body := hydra.ext.java.syntax.InterfaceBody(visitor_methods), visitor := hydra.ext.java.utils.java_interface_declaration_to_java_class_body_declaration(hydra.ext.java.syntax.NormalInterfaceDeclaration((cast(hydra.ext.java.syntax.InterfaceModifier, hydra.ext.java.syntax.InterfaceModifierPublic()),), hydra.ext.java.syntax.TypeIdentifier(hydra.ext.java.syntax.Identifier(hydra.ext.java.names.visitor_name)), vtparams, (), visitor_body)), type_args := hydra.lib.lists.map((lambda tp: hydra.ext.java.utils.type_parameter_to_type_argument(tp)), tparams), visitor_class_type := hydra.ext.java.utils.java_class_type(hydra.lib.lists.concat2(hydra.lib.lists.map((lambda tp: hydra.ext.java.utils.type_parameter_to_reference_type(tp)), tparams), (hydra.ext.java.utils.visitor_type_variable,)), Nothing(), hydra.ext.java.names.visitor_name), main_instance_param := hydra.ext.java.utils.java_type_to_java_formal_parameter(hydra.ext.java.utils.java_class_type_to_java_type(hydra.ext.java.utils.name_to_java_class_type(aliases, False, type_args, el_name, Nothing())), hydra.core.Name("instance")), result_r := hydra.ext.java.utils.java_type_to_java_result(cast(hydra.ext.java.syntax.Type, hydra.ext.java.syntax.TypeReference(hydra.ext.java.utils.visitor_type_variable))), throw_stmt := cast(hydra.ext.java.syntax.BlockStatement, hydra.ext.java.syntax.BlockStatementStatement(hydra.ext.java.utils.java_throw_illegal_state_exception((hydra.ext.java.utils.java_additive_expression_to_java_expression(hydra.ext.java.utils.add_expressions((hydra.ext.java.utils.java_string_multiplicative_expression("Non-exhaustive patterns when matching: "), cast(hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.MultiplicativeExpressionUnary(hydra.ext.java.utils.java_identifier_to_java_unary_expression(hydra.ext.java.syntax.Identifier("instance"))))))),)))), default_mod := (cast(hydra.ext.java.syntax.InterfaceMethodModifier, hydra.ext.java.syntax.InterfaceMethodModifierDefault()),), otherwise_decl := hydra.ext.java.utils.interface_method_declaration(default_mod, (), hydra.ext.java.names.otherwise_method_name, (main_instance_param,), result_r, Just((throw_stmt,))), pv_visit_methods := hydra.lib.lists.map((lambda ft: (fname := ft.name, var_ref := hydra.ext.java.utils.java_class_type_to_java_type(hydra.ext.java.utils.name_to_java_class_type(aliases, False, type_args, hydra.ext.java.utils.variant_class_name(False, el_name, fname), Nothing())), param := hydra.ext.java.utils.java_type_to_java_formal_parameter(var_ref, hydra.core.Name("instance")), mi := hydra.ext.java.utils.method_invocation(Nothing(), hydra.ext.java.syntax.Identifier(hydra.ext.java.names.otherwise_method_name), (hydra.ext.java.utils.java_identifier_to_java_expression(hydra.ext.java.syntax.Identifier("instance")),)), return_otherwise := cast(hydra.ext.java.syntax.BlockStatement, hydra.ext.java.syntax.BlockStatementStatement(hydra.ext.java.utils.java_return_statement(Just(hydra.ext.java.utils.java_primary_to_java_expression(hydra.ext.java.utils.java_method_invocation_to_java_primary(mi)))))), hydra.ext.java.utils.interface_method_declaration(default_mod, (), hydra.ext.java.names.visit_method_name, (param,), result_r, Just((return_otherwise,))))[5]), fields), pv_body := hydra.ext.java.syntax.InterfaceBody(hydra.lib.lists.concat2((otherwise_decl,), pv_visit_methods)), partial_visitor := hydra.ext.java.utils.java_interface_declaration_to_java_class_body_declaration(hydra.ext.java.syntax.NormalInterfaceDeclaration((cast(hydra.ext.java.syntax.InterfaceModifier, hydra.ext.java.syntax.InterfaceModifierPublic()),), hydra.ext.java.syntax.TypeIdentifier(hydra.ext.java.syntax.Identifier(hydra.ext.java.names.partial_visitor_name)), vtparams, (hydra.ext.java.syntax.InterfaceType(visitor_class_type),), pv_body)), hydra.lib.flows.bind(constant_decl_for_type_name(aliases, el_name), (lambda tn0: hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda ft: constant_decl_for_field_type(aliases, ft)), fields), (lambda tn1: (tn := hydra.lib.lists.concat2((tn0,), tn1), other_decls := hydra.lib.lists.map((lambda d: no_comment(d)), (private_const, accept_decl, visitor, partial_visitor)), body_decls := hydra.lib.lists.concat((tn, other_decls, variant_decls_)), mods := hydra.lib.lists.concat2(class_mods_public, (cast(hydra.ext.java.syntax.ClassModifier, hydra.ext.java.syntax.ClassModifierAbstract()),)), hydra.lib.flows.pure(hydra.ext.java.utils.java_class_declaration(aliases, tparams, el_name, mods, Nothing(), interface_types(is_ser, aliases, tparams, el_name), body_decls)))[4])))))[16])))[1]))

def peel_domains_and_cod(n: int, t: hydra.core.Type) -> tuple[frozenlist[hydra.core.Type], hydra.core.Type]:
    def _hoist_hydra_ext_java_coder_peel_domains_and_cod_1(n: int, t: hydra.core.Type, v1: hydra.core.Type) -> tuple[frozenlist[hydra.core.Type], hydra.core.Type]:
        match v1:
            case hydra.core.TypeFunction(value=ft):
                @lru_cache(1)
                def rest() -> tuple[frozenlist[hydra.core.Type], hydra.core.Type]:
                    return peel_domains_and_cod(hydra.lib.math.sub(n, 1), ft.codomain)
                return (hydra.lib.lists.cons(ft.domain, hydra.lib.pairs.first(rest())), hydra.lib.pairs.second(rest()))
            
            case _:
                return ((), t)
    return hydra.lib.logic.if_else(hydra.lib.equality.lte(n, 0), (lambda : ((), t)), (lambda : _hoist_hydra_ext_java_coder_peel_domains_and_cod_1(n, t, hydra.rewriting.deannotate_type(t))))

def flatten_apps(t: hydra.core.Term, acc: frozenlist[hydra.core.Term]) -> tuple[frozenlist[hydra.core.Term], hydra.core.Term]:
    match hydra.rewriting.deannotate_term(t):
        case hydra.core.TermApplication(value=app):
            return flatten_apps(app.function, hydra.lib.lists.cons(app.argument, acc))
        
        case _:
            return (acc, t)

def rebuild_apps(f: hydra.core.Term, args: frozenlist[hydra.core.Term], f_type: hydra.core.Type) -> hydra.core.Term:
    def _hoist_hydra_ext_java_coder_rebuild_apps_1(args: frozenlist[hydra.core.Term], f: hydra.core.Term, v1: hydra.core.Type) -> hydra.core.Term:
        match v1:
            case hydra.core.TypeFunction(value=ft):
                @lru_cache(1)
                def arg() -> hydra.core.Term:
                    return hydra.lib.lists.head(args)
                @lru_cache(1)
                def rest() -> frozenlist[hydra.core.Term]:
                    return hydra.lib.lists.tail(args)
                @lru_cache(1)
                def remaining_type() -> hydra.core.Type:
                    return ft.codomain
                @lru_cache(1)
                def app() -> hydra.core.Term:
                    return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(f, arg())))
                @lru_cache(1)
                def annotated_app() -> hydra.core.Term:
                    return hydra.annotations.set_term_annotation(hydra.constants.key_type, Just(hydra.encode.core.type(remaining_type())), app())
                return rebuild_apps(annotated_app(), rest(), remaining_type())
            
            case _:
                return hydra.lib.lists.foldl((lambda acc, a: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(acc, a)))), f, args)
    return hydra.lib.logic.if_else(hydra.lib.lists.null(args), (lambda : f), (lambda : _hoist_hydra_ext_java_coder_rebuild_apps_1(args, f, hydra.rewriting.deannotate_type(f_type))))

def propagate_types_in_app_chain(fixed_cod: hydra.core.Type, result_type: hydra.core.Type, t: hydra.core.Term) -> hydra.core.Term:
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
    def _hoist_body_1(v1: hydra.core.Term) -> hydra.core.Term:
        match v1:
            case hydra.core.TermApplication(value=app):
                @lru_cache(1)
                def lhs() -> hydra.core.Term:
                    return app.function
                @lru_cache(1)
                def rhs() -> hydra.core.Term:
                    return app.argument
                @lru_cache(1)
                def annotated_lhs() -> hydra.core.Term:
                    def _hoist_annotated_lhs_1(v12: hydra.core.Elimination) -> hydra.core.Term:
                        match v12:
                            case hydra.core.EliminationUnion(value=cs):
                                @lru_cache(1)
                                def dom() -> hydra.core.Type:
                                    return hydra.schemas.nominal_application(cs.type_name, ())
                                @lru_cache(1)
                                def ft() -> hydra.core.Type:
                                    return cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(dom(), fixed_cod)))
                                return hydra.annotations.set_term_annotation(hydra.constants.key_type, Just(hydra.encode.core.type(ft())), lhs())
                            
                            case _:
                                return lhs()
                    def _hoist_annotated_lhs_2(v12: hydra.core.Function) -> hydra.core.Term:
                        match v12:
                            case hydra.core.FunctionElimination(value=elim):
                                return _hoist_annotated_lhs_1(elim)
                            
                            case _:
                                return lhs()
                    match hydra.rewriting.deannotate_term(lhs()):
                        case hydra.core.TermFunction(value=fn):
                            return _hoist_annotated_lhs_2(fn)
                        
                        case _:
                            return lhs()
                return hydra.annotations.set_term_annotation(hydra.constants.key_type, Just(hydra.encode.core.type(result_type)), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(annotated_lhs(), rhs()))))
            
            case _:
                return hydra.annotations.set_term_annotation(hydra.constants.key_type, Just(hydra.encode.core.type(result_type)), t)
    return hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.gt(n_lambda_doms(), 0), hydra.lib.equality.gt(n_args(), 0)), (lambda : (body_ret_type := hydra.lib.pairs.second(peel_domains_and_cod(hydra.lib.math.sub(n_lambda_doms(), n_args()), result_type)), (fun_type := hydra.lib.lists.foldl((lambda c, d: cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(d, c)))), body_ret_type, hydra.lib.lists.reverse(lambda_doms())), (annotated_fun := hydra.annotations.set_term_annotation(hydra.constants.key_type, Just(hydra.encode.core.type(fun_type)), fun()), rebuild_apps(annotated_fun, args(), fun_type))[1])[1])[1]), (lambda : _hoist_body_1(hydra.rewriting.deannotate_term(t))))

def encode_term_definition(env: hydra.ext.java.helpers.JavaEnvironment, tdef: hydra.module.TermDefinition) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.InterfaceMemberDeclaration]:
    @lru_cache(1)
    def name() -> hydra.core.Name:
        return tdef.name
    @lru_cache(1)
    def term0() -> hydra.core.Term:
        return tdef.term
    @lru_cache(1)
    def ts() -> hydra.core.TypeScheme:
        return tdef.type
    return hydra.monads.with_trace(hydra.lib.strings.cat2("encode term definition \"", hydra.lib.strings.cat2(name().value, "\"")), (term := hydra.rewriting.unshadow_variables(term0()), hydra.lib.flows.bind(hydra.monads.with_trace("analyze function term for term assignment", analyze_java_function(env, term)), (lambda fs: (scheme_vars := hydra.lib.lists.filter((lambda v: is_simple_name(v)), ts().variables), term_vars := fs.type_params, scheme_type_vars := collect_type_vars(ts().type), used_scheme_vars := hydra.lib.lists.filter((lambda v: hydra.lib.sets.member(v, scheme_type_vars)), scheme_vars), tparams := hydra.lib.logic.if_else(hydra.lib.lists.null(used_scheme_vars), (lambda : term_vars), (lambda : used_scheme_vars)), params := fs.params, bindings := fs.bindings, body := fs.body, doms := fs.domains, env2 := fs.environment, scheme_type := ts().type, num_params := hydra.lib.lists.length(params), peel_result := peel_domains_and_cod(num_params, scheme_type), scheme_doms := hydra.lib.pairs.first(peel_result), cod := hydra.lib.pairs.second(peel_result), scheme_var_set := hydra.lib.sets.from_list(tparams), hydra.lib.flows.bind(hydra.lib.logic.if_else(hydra.lib.lists.null(tparams), (lambda : hydra.lib.flows.pure(hydra.lib.maps.empty())), (lambda : build_subst_from_annotations(scheme_var_set, term))), (lambda type_var_subst: (overgen_subst := detect_accumulator_unification(scheme_doms, cod, tparams), overgen_var_subst := hydra.lib.maps.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda entry: (k := hydra.lib.pairs.first(entry), v := hydra.lib.pairs.second(entry), hydra.dsl.python.unsupported("inline match expressions are not yet supported"))[2]), hydra.lib.maps.to_list(overgen_subst)))), fixed_cod := hydra.lib.logic.if_else(hydra.lib.maps.null(overgen_subst), (lambda : cod), (lambda : substitute_type_vars_with_types(overgen_subst, cod))), fixed_doms := hydra.lib.logic.if_else(hydra.lib.maps.null(overgen_subst), (lambda : scheme_doms), (lambda : hydra.lib.lists.map((lambda d: substitute_type_vars_with_types(overgen_subst, d)), scheme_doms))), fixed_tparams := hydra.lib.logic.if_else(hydra.lib.maps.null(overgen_subst), (lambda : tparams), (lambda : hydra.lib.lists.filter((lambda v: hydra.lib.logic.not_(hydra.lib.maps.member(v, overgen_subst))), tparams))), constraints := hydra.lib.maybes.from_maybe(hydra.lib.maps.empty(), ts().constraints), jparams := hydra.lib.lists.map((lambda v: hydra.ext.java.utils.java_type_parameter(hydra.formatting.capitalize(v.value))), fixed_tparams), aliases2base := env2.aliases, trusted_vars := hydra.lib.sets.unions(hydra.lib.lists.map((lambda d: collect_type_vars(d)), hydra.lib.lists.concat2(fixed_doms, (fixed_cod,)))), fixed_scheme_var_set := hydra.lib.sets.from_list(fixed_tparams), aliases2 := hydra.ext.java.helpers.Aliases(aliases2base.current_namespace, aliases2base.packages, aliases2base.branch_vars, aliases2base.recursive_vars, fixed_scheme_var_set, aliases2base.polymorphic_locals, aliases2base.in_scope_java_vars, aliases2base.var_renames, hydra.lib.sets.union(aliases2base.lambda_vars, hydra.lib.sets.from_list(params)), hydra.lib.maps.union(overgen_var_subst, type_var_subst), hydra.lib.sets.intersection(trusted_vars, fixed_scheme_var_set), Just(fixed_cod), aliases2base.thunked_vars), env2_with_type_params := hydra.ext.java.helpers.JavaEnvironment(aliases2, env2.type_context), hydra.lib.flows.bind(bindings_to_statements(env2_with_type_params, bindings), (lambda bind_result: (binding_stmts := hydra.lib.pairs.first(bind_result), env3 := hydra.lib.pairs.second(bind_result), hydra.lib.flows.bind(hydra.lib.logic.if_else(hydra.lib.maps.null(overgen_subst), (lambda : hydra.lib.flows.pure(body)), (lambda : apply_overgen_subst_to_term_annotations(overgen_subst, body))), (lambda body_: (annotated_body := propagate_types_in_app_chain(fixed_cod, fixed_cod, body_), hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda pair: hydra.lib.flows.bind(encode_type(aliases2, hydra.lib.sets.empty(), hydra.lib.pairs.first(pair)), (lambda jdom: hydra.lib.flows.pure(hydra.ext.java.utils.java_type_to_java_formal_parameter(jdom, hydra.lib.pairs.second(pair)))))), hydra.lib.lists.zip(fixed_doms, params)), (lambda jformal_params: hydra.lib.flows.bind(encode_type(aliases2, hydra.lib.sets.empty(), fixed_cod), (lambda jcod: (result := hydra.ext.java.utils.java_type_to_java_result(jcod), hydra.lib.flows.bind(encode_term(env3, annotated_body), (lambda jbody: (mods := (cast(hydra.ext.java.syntax.InterfaceMethodModifier, hydra.ext.java.syntax.InterfaceMethodModifierStatic()),), jname := hydra.ext.java.utils.sanitize_java_name(hydra.formatting.decapitalize(hydra.names.local_name_of(name()))), return_st := cast(hydra.ext.java.syntax.BlockStatement, hydra.ext.java.syntax.BlockStatementStatement(hydra.ext.java.utils.java_return_statement(Just(jbody)))), hydra.lib.flows.pure(hydra.ext.java.utils.interface_method_declaration(mods, jparams, jname, jformal_params, result, Just(hydra.lib.lists.concat2(binding_stmts, (return_st,))))))[3])))[1])))))[1])))[2])))[12])))[16])))[1])

def is_serializable_java_type(typ: hydra.core.Type) -> bool:
    match hydra.rewriting.deannotate_type(typ):
        case hydra.core.TypeRecord():
            return True
        
        case hydra.core.TypeUnion():
            return True
        
        case hydra.core.TypeWrap():
            return True
        
        case hydra.core.TypeForall(value=fa):
            return is_serializable_java_type(fa.body)
        
        case _:
            return False

def to_class_decl(is_inner: bool, is_ser: bool, aliases: hydra.ext.java.helpers.Aliases, tparams: frozenlist[hydra.ext.java.syntax.TypeParameter], el_name: hydra.core.Name, t: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration]:
    def wrap(t_: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.java.syntax.ClassDeclaration]:
        return declaration_for_record_type(is_inner, is_ser, aliases, tparams, el_name, (hydra.core.FieldType(hydra.core.Name("value"), hydra.rewriting.deannotate_type(t_)),))
    match hydra.rewriting.deannotate_type(t):
        case hydra.core.TypeRecord(value=rt):
            return declaration_for_record_type(is_inner, is_ser, aliases, tparams, el_name, rt.fields)
        
        case hydra.core.TypeUnion(value=rt2):
            return declaration_for_union_type(is_ser, aliases, tparams, el_name, rt2.fields)
        
        case hydra.core.TypeForall(value=fa):
            @lru_cache(1)
            def v() -> hydra.core.Name:
                return fa.parameter
            @lru_cache(1)
            def body() -> hydra.core.Type:
                return fa.body
            @lru_cache(1)
            def param() -> hydra.ext.java.syntax.TypeParameter:
                return hydra.ext.java.utils.java_type_parameter(hydra.formatting.capitalize(v().value))
            return to_class_decl(False, is_ser, aliases, hydra.lib.lists.concat2(tparams, (param(),)), el_name, body())
        
        case hydra.core.TypeWrap(value=wt):
            @lru_cache(1)
            def wtype() -> hydra.core.Type:
                return wt.body
            return declaration_for_record_type(is_inner, is_ser, aliases, tparams, el_name, (hydra.core.FieldType(hydra.core.Name("value"), wtype()),))
        
        case _:
            return wrap(t)

def encode_type_definition(pkg: hydra.ext.java.syntax.PackageDeclaration, aliases: hydra.ext.java.helpers.Aliases, tdef: hydra.module.TypeDefinition) -> hydra.compute.Flow[hydra.graph.Graph, tuple[hydra.core.Name, hydra.ext.java.syntax.CompilationUnit]]:
    @lru_cache(1)
    def name() -> hydra.core.Name:
        return tdef.name
    @lru_cache(1)
    def typ() -> hydra.core.Type:
        return tdef.type
    @lru_cache(1)
    def serializable() -> bool:
        return is_serializable_java_type(typ())
    @lru_cache(1)
    def imports() -> frozenlist[hydra.ext.java.syntax.ImportDeclaration]:
        return hydra.lib.logic.if_else(serializable(), (lambda : (cast(hydra.ext.java.syntax.ImportDeclaration, hydra.ext.java.syntax.ImportDeclarationSingleType(hydra.ext.java.syntax.SingleTypeImportDeclaration(hydra.ext.java.utils.java_type_name(hydra.ext.java.syntax.Identifier("java.io.Serializable"))))),)), (lambda : ()))
    return hydra.lib.flows.bind(to_class_decl(False, serializable(), aliases, (), name(), typ()), (lambda decl: hydra.lib.flows.bind(hydra.annotations.get_type_description(typ()), (lambda comment: (tdecl := hydra.ext.java.syntax.TypeDeclarationWithComments(cast(hydra.ext.java.syntax.TypeDeclaration, hydra.ext.java.syntax.TypeDeclarationClass(decl)), comment), hydra.lib.flows.pure((name(), cast(hydra.ext.java.syntax.CompilationUnit, hydra.ext.java.syntax.CompilationUnitOrdinary(hydra.ext.java.syntax.OrdinaryCompilationUnit(Just(pkg), imports(), (tdecl,)))))))[1]))))

def encode_definitions(mod: hydra.module.Module, defs: frozenlist[hydra.module.Definition]) -> hydra.compute.Flow[hydra.graph.Graph, FrozenDict[hydra.core.Name, hydra.ext.java.syntax.CompilationUnit]]:
    return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda g: hydra.lib.flows.bind(hydra.inference.initial_type_context(g), (lambda tc: (aliases := hydra.ext.java.utils.import_aliases_for_module(mod), env := hydra.ext.java.helpers.JavaEnvironment(aliases, tc), pkg := hydra.ext.java.utils.java_package_declaration(mod.namespace), partitioned := hydra.schemas.partition_definitions(defs), type_defs := hydra.lib.pairs.first(partitioned), term_defs := hydra.lib.pairs.second(partitioned), non_typedef_defs := hydra.lib.lists.filter((lambda td: (typ := td.type, is_serializable_java_type(typ))[1]), type_defs), hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda td: encode_type_definition(pkg, aliases, td)), non_typedef_defs), (lambda type_units: hydra.lib.flows.bind(hydra.lib.logic.if_else(hydra.lib.lists.null(term_defs), (lambda : hydra.lib.flows.pure(())), (lambda : hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda td: encode_term_definition(env, td)), term_defs), (lambda data_members: hydra.lib.flows.pure((construct_elements_interface(mod, data_members),)))))), (lambda term_units: hydra.lib.flows.pure(hydra.lib.maps.from_list(hydra.lib.lists.concat2(type_units, term_units))))))))[7]))))

def get_function_type(ann: FrozenDict[hydra.core.Name, hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.FunctionType]:
    def _hoist_hydra_ext_java_coder_get_function_type_1(t: hydra.core.Type, v1: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.core.FunctionType]:
        match v1:
            case hydra.core.TypeFunction(value=ft):
                return hydra.lib.flows.pure(ft)
            
            case _:
                return hydra.lib.flows.fail(hydra.lib.strings.cat2("expected function type, got: ", hydra.show.core.type(t)))
    return hydra.lib.flows.bind(hydra.annotations.get_type(ann), (lambda mt: hydra.lib.maybes.cases(mt, hydra.lib.flows.fail("type annotation is required for function and elimination terms in Java"), (lambda t: _hoist_hydra_ext_java_coder_get_function_type_1(t, t)))))

def get_codomain(ann: FrozenDict[hydra.core.Name, hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]:
    return hydra.lib.flows.map((lambda ft: ft.codomain), get_function_type(ann))

java8_features = hydra.ext.java.helpers.JavaFeatures(False)

def java_identifier_to_string(id: hydra.ext.java.syntax.Identifier) -> str:
    return id.value

def java_type_arguments_for_named_type(tname: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, frozenlist[hydra.ext.java.syntax.TypeArgument]]:
    return hydra.lib.flows.map((lambda typ: hydra.lib.lists.map((lambda tp_: hydra.ext.java.utils.type_parameter_to_type_argument(tp_)), java_type_parameters_for_type(typ))), hydra.schemas.require_type(tname))

def module_to_java(mod: hydra.module.Module, defs: frozenlist[hydra.module.Definition]) -> hydra.compute.Flow[hydra.graph.Graph, FrozenDict[str, str]]:
    return hydra.monads.with_trace(hydra.lib.strings.cat2("encode module: ", mod.namespace.value), hydra.lib.flows.bind(encode_definitions(mod, defs), (lambda units: hydra.lib.flows.pure(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda entry: (name := hydra.lib.pairs.first(entry), unit := hydra.lib.pairs.second(entry), (binding_name_to_file_path(name), hydra.serialization.print_expr(hydra.serialization.parenthesize(hydra.ext.java.serde.write_compilation_unit(unit)))))[2]), hydra.lib.maps.to_list(units)))))))

def split_constant_initializer_split_var(mods: frozenlist[hydra.ext.java.syntax.ConstantModifier], utype: hydra.ext.java.syntax.UnannType, vd: hydra.ext.java.syntax.VariableDeclarator) -> frozenlist[hydra.ext.java.syntax.InterfaceMemberDeclaration]:
    @lru_cache(1)
    def vid() -> hydra.ext.java.syntax.VariableDeclaratorId:
        return vd.id
    @lru_cache(1)
    def m_init() -> Maybe[hydra.ext.java.syntax.VariableInitializer]:
        return vd.initializer
    def _hoist_body_1(v1: hydra.ext.java.syntax.VariableInitializer) -> frozenlist[hydra.ext.java.syntax.InterfaceMemberDeclaration]:
        match v1:
            case hydra.ext.java.syntax.VariableInitializerExpression(value=expr):
                @lru_cache(1)
                def var_name() -> str:
                    return java_identifier_to_string(vid().identifier)
                @lru_cache(1)
                def helper_name() -> str:
                    return hydra.lib.strings.cat2("_init_", var_name())
                @lru_cache(1)
                def call_expr() -> hydra.ext.java.syntax.Expression:
                    return hydra.ext.java.utils.java_method_invocation_to_java_expression(hydra.ext.java.utils.method_invocation(Nothing(), hydra.ext.java.syntax.Identifier(helper_name()), ()))
                @lru_cache(1)
                def field() -> hydra.ext.java.syntax.InterfaceMemberDeclaration:
                    return cast(hydra.ext.java.syntax.InterfaceMemberDeclaration, hydra.ext.java.syntax.InterfaceMemberDeclarationConstant(hydra.ext.java.syntax.ConstantDeclaration(mods, utype, (hydra.ext.java.syntax.VariableDeclarator(vid(), Just(cast(hydra.ext.java.syntax.VariableInitializer, hydra.ext.java.syntax.VariableInitializerExpression(call_expr())))),))))
                @lru_cache(1)
                def return_st() -> hydra.ext.java.syntax.BlockStatement:
                    return cast(hydra.ext.java.syntax.BlockStatement, hydra.ext.java.syntax.BlockStatementStatement(hydra.ext.java.utils.java_return_statement(Just(expr))))
                @lru_cache(1)
                def result_type() -> hydra.ext.java.syntax.Result:
                    return cast(hydra.ext.java.syntax.Result, hydra.ext.java.syntax.ResultType(utype))
                @lru_cache(1)
                def helper() -> hydra.ext.java.syntax.InterfaceMemberDeclaration:
                    return hydra.ext.java.utils.interface_method_declaration((cast(hydra.ext.java.syntax.InterfaceMethodModifier, hydra.ext.java.syntax.InterfaceMethodModifierStatic()), cast(hydra.ext.java.syntax.InterfaceMethodModifier, hydra.ext.java.syntax.InterfaceMethodModifierPrivate())), (), helper_name(), (), result_type(), Just((return_st(),)))
                return (field(), helper())
            
            case _:
                return (cast(hydra.ext.java.syntax.InterfaceMemberDeclaration, hydra.ext.java.syntax.InterfaceMemberDeclarationConstant(hydra.ext.java.syntax.ConstantDeclaration(mods, utype, (vd,)))),)
    return hydra.lib.maybes.cases(m_init(), (cast(hydra.ext.java.syntax.InterfaceMemberDeclaration, hydra.ext.java.syntax.InterfaceMemberDeclarationConstant(hydra.ext.java.syntax.ConstantDeclaration(mods, utype, (vd,)))),), (lambda init_: _hoist_body_1(init_)))

def split_constant_initializer(member: hydra.ext.java.syntax.InterfaceMemberDeclaration) -> frozenlist[hydra.ext.java.syntax.InterfaceMemberDeclaration]:
    match member:
        case hydra.ext.java.syntax.InterfaceMemberDeclarationConstant(value=cd):
            return hydra.lib.lists.bind(cd.variables, (lambda v1: split_constant_initializer_split_var(cd.modifiers, cd.type, v1)))
        
        case _:
            return (member,)
