# Note: this is an automatically generated file. Do not edit.

r"""Functions for reducing terms and types, i.e. performing computations."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Maybe, Nothing, frozenlist
from typing import cast
import hydra.arity
import hydra.checking
import hydra.compute
import hydra.core
import hydra.extract.core
import hydra.graph
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
import hydra.lib.sets
import hydra.lib.strings
import hydra.rewriting
import hydra.schemas
import hydra.typing

def alpha_convert(vold: hydra.core.Name, vnew: hydra.core.Name, term: hydra.core.Term) -> hydra.core.Term:
    r"""Alpha convert a variable in a term."""
    
    return hydra.rewriting.replace_free_term_variable(vold, cast(hydra.core.Term, hydra.core.TermVariable(vnew)), term)

def beta_reduce_type(typ: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]:
    r"""Eagerly beta-reduce a type by substituting type arguments into type lambdas."""
    
    def reduce_app(app: hydra.core.ApplicationType) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]:
        lhs = app.function
        rhs = app.argument
        match lhs:
            case hydra.core.TypeAnnotated(value=at):
                return hydra.lib.flows.bind(reduce_app(hydra.core.ApplicationType(at.body, rhs)), (lambda a: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeAnnotated(hydra.core.AnnotatedType(a, at.annotation))))))
            
            case hydra.core.TypeForall(value=ft):
                return beta_reduce_type(hydra.rewriting.replace_free_type_variable(ft.parameter, rhs, ft.body))
            
            case hydra.core.TypeVariable(value=name):
                return hydra.lib.flows.bind(hydra.schemas.require_type(name), (lambda t_: beta_reduce_type(cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(t_, rhs))))))
            
            case _:
                raise TypeError("Unsupported Type")
    def map_expr[T0](recurse: Callable[[T0], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]], t: T0) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]:
        def find_app(r: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]:
            match r:
                case hydra.core.TypeApplication(value=a):
                    return reduce_app(a)
                
                case _:
                    return hydra.lib.flows.pure(r)
        return hydra.lib.flows.bind(recurse(t), (lambda r: find_app(r)))
    return hydra.rewriting.rewrite_type_m(cast(Callable[[
      Callable[[hydra.core.Type], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]],
      hydra.core.Type], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]], map_expr), typ)

def contract_term(term: hydra.core.Term) -> hydra.core.Term:
    r"""Apply the special rules:
        ((\x.e1) e2) == e1, where x does not appear free in e1
      and
         ((\x.e1) e2) = e1[x/e2]
    These are both limited forms of beta reduction which help to "clean up" a term without fully evaluating it."""
    
    def rewrite[T0](recurse: Callable[[T0], hydra.core.Term], t: T0) -> hydra.core.Term:
        rec = recurse(t)
        match rec:
            case hydra.core.TermApplication(value=app):
                lhs = app.function
                rhs = app.argument
                match hydra.rewriting.deannotate_term(lhs):
                    case hydra.core.TermFunction(value=f):
                        match f:
                            case hydra.core.FunctionLambda(value=l):
                                v = l.parameter
                                body = l.body
                                return hydra.lib.logic.if_else(hydra.rewriting.is_free_variable_in_term(v, body), body, hydra.rewriting.replace_free_term_variable(v, rhs, body))
                            
                            case _:
                                return rec
                    
                    case _:
                        return rec
            
            case _:
                return rec
    return hydra.rewriting.rewrite_term(cast(Callable[[Callable[[hydra.core.Term], hydra.core.Term], hydra.core.Term], hydra.core.Term], rewrite), term)

count_primitive_invocations = True

def eta_expansion_arity(graph: hydra.graph.Graph, term: hydra.core.Term) -> int:
    r"""Calculate the arity for eta expansion Note: this is a "trusty" function which assumes the graph is well-formed, i.e. no dangling references."""
    
    match term:
        case hydra.core.TermAnnotated(value=at):
            return eta_expansion_arity(graph, at.body)
        
        case hydra.core.TermApplication(value=app):
            return hydra.lib.math.sub(eta_expansion_arity(graph, app.function), 1)
        
        case hydra.core.TermFunction(value=f):
            match f:
                case hydra.core.FunctionElimination():
                    return 1
                
                case hydra.core.FunctionLambda():
                    return 0
                
                case hydra.core.FunctionPrimitive(value=name):
                    return hydra.arity.primitive_arity(hydra.lib.maybes.from_just(hydra.lexical.lookup_primitive(graph, name)))
        
        case hydra.core.TermTypeLambda(value=ta):
            return eta_expansion_arity(graph, ta.body)
        
        case hydra.core.TermTypeApplication(value=tt):
            return eta_expansion_arity(graph, tt.body)
        
        case hydra.core.TermVariable(value=name):
            return hydra.lib.maybes.maybe(0, (lambda ts: hydra.arity.type_arity(ts.type)), hydra.lib.maybes.bind(hydra.lexical.lookup_element(graph, name), (lambda b: b.type)))
        
        case _:
            return 0

def eta_expand_term(graph: hydra.graph.Graph, term: hydra.core.Term) -> hydra.core.Term:
    r"""Recursively transform arbitrary terms like 'add 42' into terms like '\x.add 42 x', in which the implicit parameters of primitive functions and eliminations are made into explicit lambda parameters. Variable references are not expanded. This is useful for targets like Python with weaker support for currying than Hydra or Haskell. Note: this is a "trusty" function which assumes the graph is well-formed, i.e. no dangling references."""
    
    def expand(args: frozenlist[hydra.core.Term], arity: int, t: hydra.core.Term) -> hydra.core.Term:
        apps = hydra.lib.lists.foldl((lambda lhs, arg: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(lhs, arg)))), t, args)
        is_ = hydra.lib.logic.if_else(hydra.lib.equality.lte(arity, hydra.lib.lists.length(args)), cast(frozenlist[int], ()), hydra.lib.math.range_(1, hydra.lib.math.sub(arity, hydra.lib.lists.length(args))))
        def pad(indices: frozenlist[int], t2: hydra.core.Term) -> hydra.core.Term:
            return hydra.lib.logic.if_else(hydra.lib.lists.null(indices), t2, cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name(hydra.lib.strings.cat2("v", hydra.lib.literals.show_int32(hydra.lib.lists.head(indices)))), cast(Maybe[hydra.core.Type], Nothing()), pad(hydra.lib.lists.tail(indices), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(t2, cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name(hydra.lib.strings.cat2("v", hydra.lib.literals.show_int32(hydra.lib.lists.head(indices))))))))))))))))
        return pad(is_, apps)
    def rewrite(args: frozenlist[hydra.core.Term], recurse: Callable[[hydra.core.Term], hydra.core.Term], t: hydra.core.Term) -> hydra.core.Term:
        def after_recursion(term2: hydra.core.Term) -> hydra.core.Term:
            return expand(args, eta_expansion_arity(graph, term2), term2)
        t2 = hydra.rewriting.detype_term(t)
        match t2:
            case hydra.core.TermApplication(value=app):
                lhs = app.function
                rhs = app.argument
                erhs = rewrite(cast(frozenlist[hydra.core.Term], ()), recurse, rhs)
                return rewrite(hydra.lib.lists.cons(erhs, args), recurse, lhs)
            
            case _:
                return after_recursion(recurse(t2))
    return contract_term(hydra.rewriting.rewrite_term((lambda v1, v2: rewrite(cast(frozenlist[hydra.core.Term], ()), v1, v2)), term))

def eta_expand_typed_term[T0](tx0: hydra.typing.TypeContext, term0: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
    def rewrite[T1](top_level: bool, forced: bool, type_args: frozenlist[hydra.core.Type], recurse: Callable[[hydra.typing.TypeContext, hydra.core.Term], hydra.compute.Flow[T1, hydra.core.Term]], tx: hydra.typing.TypeContext, term: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.core.Term]:
        def rewrite_spine(term2: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.core.Term]:
            match term2:
                case hydra.core.TermAnnotated(value=at):
                    return hydra.lib.flows.bind(rewrite_spine(at.body), (lambda body: (ann := at.annotation, hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(body, ann)))))[1]))
                
                case hydra.core.TermApplication(value=a):
                    l = hydra.lib.logic.if_else(False, (cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))),), cast(frozenlist[hydra.core.Type], ()))
                    return hydra.lib.flows.bind(rewrite_spine(a.function), (lambda lhs: hydra.lib.flows.bind(rewrite(True, False, l, recurse, tx, a.argument), (lambda rhs: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(lhs, rhs))))))))
                
                case hydra.core.TermTypeApplication(value=tat):
                    return hydra.lib.flows.bind(rewrite_spine(tat.body), (lambda body: (typ := tat.type, hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(body, typ)))))[1]))
                
                case _:
                    return rewrite(False, False, cast(frozenlist[hydra.core.Type], ()), recurse, tx, term2)
        def arity_of[T2](tx2: hydra.typing.TypeContext, term2: hydra.core.Term) -> hydra.compute.Flow[T2, int]:
            def dflt[T3]() -> hydra.compute.Flow[T3, int]:
                return hydra.lib.flows.map(hydra.arity.type_arity, hydra.checking.type_of(tx2, cast(frozenlist[hydra.core.Type], ()), term2))
            def for_function(tx3: hydra.typing.TypeContext, f: hydra.core.Function) -> hydra.compute.Flow[T2, int]:
                match f:
                    case hydra.core.FunctionElimination():
                        return hydra.lib.flows.pure(1)
                    
                    case hydra.core.FunctionLambda(value=l):
                        tx2 = hydra.schemas.extend_type_context_for_lambda(tx3, l)
                        return arity_of(tx2, l.body)
                    
                    case hydra.core.FunctionPrimitive(value=name):
                        return hydra.lib.flows.map(hydra.arity.type_scheme_arity, hydra.lexical.require_primitive_type(tx3, name))
            match term2:
                case hydra.core.TermAnnotated(value=at):
                    return arity_of(tx2, at.body)
                
                case hydra.core.TermFunction(value=f):
                    return for_function(tx2, f)
                
                case hydra.core.TermLet(value=l):
                    tx2 = hydra.schemas.extend_type_context_for_let(tx2, l)
                    return arity_of(tx2, l.body)
                
                case hydra.core.TermTypeApplication(value=tat):
                    return arity_of(tx2, tat.body)
                
                case hydra.core.TermTypeLambda(value=tl):
                    tx2 = hydra.schemas.extend_type_context_for_type_lambda(tx2, tl)
                    return arity_of(tx2, tl.body)
                
                case hydra.core.TermVariable(value=name):
                    return hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat(("unbound variable: ", name.value))), (lambda t: hydra.lib.flows.pure(hydra.arity.type_arity(t))), hydra.lib.maps.lookup(name, tx2.types))
                
                case _:
                    return cast(hydra.compute.Flow[T2, int], dflt)
        def extra_variables(n: int) -> frozenlist[hydra.core.Name]:
            return hydra.lib.lists.map((lambda i: hydra.core.Name(hydra.lib.strings.cat2("v", hydra.lib.literals.show_int32(i)))), hydra.lib.math.range_(1, n))
        def pad(vars: frozenlist[hydra.core.Name], body: hydra.core.Term) -> hydra.core.Term:
            return hydra.lib.logic.if_else(hydra.lib.lists.null(vars), body, cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.lib.lists.head(vars), cast(Maybe[hydra.core.Type], Nothing()), pad(hydra.lib.lists.tail(vars), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(body, cast(hydra.core.Term, hydra.core.TermVariable(hydra.lib.lists.head(vars)))))))))))))
        def padn(n: int, body: hydra.core.Term) -> hydra.core.Term:
            return pad(extra_variables(n), body)
        def unwind(term2: hydra.core.Term) -> hydra.core.Term:
            return hydra.lib.lists.foldl((lambda e, t: cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(e, t)))), term2, type_args)
        def force_expansion[T2](t: hydra.core.Term) -> hydra.compute.Flow[T2, hydra.core.Term]:
            return hydra.lib.flows.bind(hydra.checking.type_of(tx, cast(frozenlist[hydra.core.Type], ()), t), (lambda typ: (arity := hydra.arity.type_arity(typ), hydra.lib.flows.pure(padn(arity, unwind(t))))[1]))
        def recurse_or_force(term2: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.core.Term]:
            return hydra.lib.logic.if_else(forced, force_expansion(term2), recurse(tx, unwind(term2)))
        def for_case(f: hydra.core.Field) -> hydra.compute.Flow[T1, hydra.core.Field]:
            return hydra.lib.flows.bind(rewrite(False, True, cast(frozenlist[hydra.core.Type], ()), recurse, tx, f.term), (lambda r: hydra.lib.flows.pure(hydra.core.Field(f.name, r))))
        def for_case_statement(cs: hydra.core.CaseStatement) -> hydra.compute.Flow[T1, hydra.core.Term]:
            tname = cs.type_name
            dflt = cs.default
            cases = cs.cases
            return hydra.lib.flows.bind(hydra.lib.flows.map_maybe((lambda v1: rewrite(False, False, cast(frozenlist[hydra.core.Type], ()), recurse, tx, v1)), dflt), (lambda rdflt: hydra.lib.flows.bind(hydra.lib.flows.map_list(for_case, cases), (lambda rcases: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(tname, rdflt, rcases))))))))))))
        def for_elimination(elm: hydra.core.Elimination) -> hydra.compute.Flow[T1, hydra.core.Term]:
            def check_base(elm2: hydra.core.Elimination) -> hydra.compute.Flow[T1, hydra.core.Term]:
                match elm2:
                    case hydra.core.EliminationUnion(value=cs):
                        return for_case_statement(cs)
                    
                    case _:
                        return recurse(tx, term)
            return hydra.lib.flows.bind(hydra.lib.flows.map(unwind, check_base(elm)), (lambda base: hydra.lib.flows.pure(hydra.lib.logic.if_else(hydra.lib.logic.or_(top_level, forced), padn(1, base), base))))
        match term:
            case hydra.core.TermApplication(value=a):
                lhs = a.function
                rhs = a.argument
                return hydra.lib.flows.bind(rewrite(True, False, cast(frozenlist[hydra.core.Type], ()), recurse, tx, rhs), (lambda rhs2: hydra.lib.flows.bind(arity_of(tx, lhs), (lambda lhsarity: hydra.lib.flows.bind(rewrite_spine(lhs), (lambda lhs2: (a2 := cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(lhs2, rhs2))), hydra.lib.flows.pure(hydra.lib.logic.if_else(hydra.lib.equality.gt(lhsarity, 1), padn(hydra.lib.math.sub(lhsarity, 1), a2), a2)))[1]))))))
            
            case hydra.core.TermFunction(value=f):
                match f:
                    case hydra.core.FunctionElimination(value=elm):
                        return for_elimination(elm)
                    
                    case hydra.core.FunctionLambda(value=l):
                        tx2 = hydra.schemas.extend_type_context_for_lambda(tx, l)
                        return hydra.lib.flows.map(unwind, recurse(tx2, term))
                    
                    case _:
                        return recurse_or_force(term)
            
            case hydra.core.TermLet(value=l):
                tx2 = hydra.schemas.extend_type_context_for_let(tx, l)
                return recurse(tx2, term)
            
            case hydra.core.TermTypeApplication(value=tat):
                return rewrite(top_level, forced, hydra.lib.lists.cons(tat.type, type_args), recurse, tx, tat.body)
            
            case hydra.core.TermTypeLambda(value=tl):
                tx2 = hydra.schemas.extend_type_context_for_type_lambda(tx, tl)
                return recurse(tx2, term)
            
            case _:
                return recurse_or_force(term)
    return hydra.rewriting.rewrite_term_with_context_m((lambda v1, v2, v3: rewrite(True, False, cast(frozenlist[hydra.core.Type], ()), v1, v2, v3)), tx0, term0)

def eta_reduce_term(term: hydra.core.Term) -> hydra.core.Term:
    r"""Eta-reduce a term by removing redundant lambda abstractions."""
    
    no_change = term
    def reduce_lambda(l: hydra.core.Lambda) -> hydra.core.Term:
        v = l.parameter
        d = l.domain
        body = l.body
        match eta_reduce_term(body):
            case hydra.core.TermAnnotated(value=at):
                return reduce_lambda(hydra.core.Lambda(v, d, at.body))
            
            case hydra.core.TermApplication(value=app):
                lhs = app.function
                rhs = app.argument
                match eta_reduce_term(rhs):
                    case hydra.core.TermAnnotated(value=at):
                        return reduce_lambda(hydra.core.Lambda(v, d, cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(lhs, at.body)))))
                    
                    case hydra.core.TermVariable(value=v1):
                        return hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.equal(v.value, v1.value), hydra.lib.logic.not_(hydra.rewriting.is_free_variable_in_term(v, lhs))), eta_reduce_term(lhs), no_change)
                    
                    case _:
                        return no_change
            
            case _:
                return no_change
    match term:
        case hydra.core.TermAnnotated(value=at):
            return cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(eta_reduce_term(at.body), at.annotation)))
        
        case hydra.core.TermFunction(value=f):
            match f:
                case hydra.core.FunctionLambda(value=l):
                    return reduce_lambda(l)
                
                case _:
                    return no_change
        
        case _:
            return no_change

def reduce_term(eager: bool, term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
    r"""A term evaluation function which is alternatively lazy or eager."""
    
    def reduce(eager2: bool, v1: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
        return reduce_term(eager2, v1)
    def do_recurse(eager2: bool, term2: hydra.core.Term) -> bool:
        def is_non_lambda(f: hydra.core.Function) -> bool:
            match f:
                case hydra.core.FunctionLambda():
                    return False
                
                case _:
                    return True
        def is_non_lambda_term() -> bool:
            match term2:
                case hydra.core.TermFunction(value=f):
                    return is_non_lambda(f)
                
                case _:
                    return True
        return hydra.lib.logic.and_(eager2, is_non_lambda_term())
    def reduce_arg(eager2: bool, arg: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
        return hydra.lib.logic.if_else(eager2, hydra.lib.flows.pure(arg), reduce(False, arg))
    def apply_to_arguments(fun: hydra.core.Term, args: frozenlist[hydra.core.Term]) -> hydra.core.Term:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(args), fun, apply_to_arguments(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun, hydra.lib.lists.head(args)))), hydra.lib.lists.tail(args)))
    def apply_elimination(elm: hydra.core.Elimination, reduced_arg: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
        match elm:
            case hydra.core.EliminationRecord(value=proj):
                return hydra.lib.flows.bind(hydra.extract.core.record(proj.type_name, hydra.rewriting.deannotate_term(reduced_arg)), (lambda fields: (matching_fields := hydra.lib.lists.filter((lambda f: hydra.lib.equality.equal(f.name, proj.field)), fields), hydra.lib.logic.if_else(hydra.lib.lists.null(matching_fields), hydra.lib.flows.fail(hydra.lib.strings.cat(("no such field: ", proj.field.value, " in ", proj.type_name.value, " record"))), hydra.lib.flows.pure(hydra.lib.lists.head(matching_fields).term)))[1]))
            
            case hydra.core.EliminationUnion(value=cs):
                return hydra.lib.flows.bind(hydra.extract.core.injection(cs.type_name, reduced_arg), (lambda field: (matching_fields := hydra.lib.lists.filter((lambda f: hydra.lib.equality.equal(f.name, field.name)), cs.cases), hydra.lib.logic.if_else(hydra.lib.lists.null(matching_fields), hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat(("no such field ", field.name.value, " in ", cs.type_name.value, " case statement"))), cast(Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]], hydra.lib.flows.pure), cs.default), hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(hydra.lib.lists.head(matching_fields).term, field.term))))))[1]))
            
            case hydra.core.EliminationWrap(value=name):
                return hydra.extract.core.wrap(name, reduced_arg)
            
            case _:
                raise TypeError("Unsupported Elimination")
    def apply_if_nullary(eager2: bool, original: hydra.core.Term, args: frozenlist[hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
        stripped = hydra.rewriting.deannotate_term(original)
        def for_elimination(elm: hydra.core.Elimination, args2: frozenlist[hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
            arg = hydra.lib.lists.head(args2)
            remaining_args = hydra.lib.lists.tail(args2)
            return hydra.lib.flows.bind(reduce_arg(eager2, hydra.rewriting.deannotate_term(arg)), (lambda reduced_arg: hydra.lib.flows.bind(hydra.lib.flows.bind(apply_elimination(elm, reduced_arg), (lambda v1: reduce(eager2, v1))), (lambda reduced_result: apply_if_nullary(eager2, reduced_result, remaining_args)))))
        def for_lambda(l: hydra.core.Lambda, args2: frozenlist[hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
            param = l.parameter
            body = l.body
            arg = hydra.lib.lists.head(args2)
            remaining_args = hydra.lib.lists.tail(args2)
            return hydra.lib.flows.bind(reduce(eager2, hydra.rewriting.deannotate_term(arg)), (lambda reduced_arg: hydra.lib.flows.bind(reduce(eager2, hydra.rewriting.replace_free_term_variable(param, reduced_arg, body)), (lambda reduced_result: apply_if_nullary(eager2, reduced_result, remaining_args)))))
        def for_primitive(prim: hydra.graph.Primitive, arity: int, args2: frozenlist[hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
            arg_list = hydra.lib.lists.take(arity, args2)
            remaining_args = hydra.lib.lists.drop(arity, args2)
            return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: reduce_arg(eager2, v1)), arg_list), (lambda reduced_args: hydra.lib.flows.bind(hydra.lib.flows.bind(prim.implementation(reduced_args), (lambda v1: reduce(eager2, v1))), (lambda reduced_result: apply_if_nullary(eager2, reduced_result, remaining_args)))))
        match stripped:
            case hydra.core.TermApplication(value=app):
                return apply_if_nullary(eager2, app.function, hydra.lib.lists.cons(app.argument, args))
            
            case hydra.core.TermFunction(value=v1):
                match v1:
                    case hydra.core.FunctionElimination(value=elm):
                        return hydra.lib.logic.if_else(hydra.lib.lists.null(args), hydra.lib.flows.pure(original), for_elimination(elm, args))
                    
                    case hydra.core.FunctionLambda(value=l):
                        return hydra.lib.logic.if_else(hydra.lib.lists.null(args), hydra.lib.flows.pure(original), for_lambda(l, args))
                    
                    case hydra.core.FunctionPrimitive(value=name):
                        return hydra.lib.flows.bind(hydra.lexical.require_primitive(name), (lambda prim: (arity := hydra.arity.primitive_arity(prim), hydra.lib.logic.if_else(hydra.lib.equality.gt(arity, hydra.lib.lists.length(args)), hydra.lib.flows.pure(apply_to_arguments(original, args)), for_primitive(prim, arity, args)))[1]))
            
            case hydra.core.TermVariable():
                return hydra.lib.flows.pure(apply_to_arguments(original, args))
            
            case _:
                return hydra.lib.flows.pure(apply_to_arguments(original, args))
    def mapping(recurse: Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]], mid: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
        return hydra.lib.flows.bind(hydra.lib.logic.if_else(do_recurse(eager, mid), recurse(mid), hydra.lib.flows.pure(mid)), (lambda inner: apply_if_nullary(eager, inner, cast(frozenlist[hydra.core.Term], ()))))
    return hydra.rewriting.rewrite_term_m(mapping, term)

def term_is_closed(term: hydra.core.Term) -> bool:
    r"""Whether a term is closed, i.e. represents a complete program."""
    
    return hydra.lib.sets.null(hydra.rewriting.free_variables_in_term(term))

def term_is_value[T0](g: T0, term: hydra.core.Term) -> bool:
    def for_list(els: frozenlist[hydra.core.Term]) -> bool:
        return hydra.lib.lists.foldl((lambda b, t: hydra.lib.logic.and_(b, term_is_value(g, t))), True, els)
    def check_field(f: hydra.core.Field) -> bool:
        return term_is_value(g, f.term)
    def check_fields(fields: frozenlist[hydra.core.Field]) -> bool:
        return hydra.lib.lists.foldl((lambda b, f: hydra.lib.logic.and_(b, check_field(f))), True, fields)
    def function_is_value(f: hydra.core.Function) -> bool:
        match f:
            case hydra.core.FunctionElimination(value=e):
                match e:
                    case hydra.core.EliminationWrap():
                        return True
                    
                    case hydra.core.EliminationRecord():
                        return True
                    
                    case hydra.core.EliminationUnion(value=cs):
                        return hydra.lib.logic.and_(check_fields(cs.cases), hydra.lib.maybes.maybe(True, (lambda v1: term_is_value(g, v1)), cs.default))
                    
                    case _:
                        raise TypeError("Unsupported Elimination")
            
            case hydra.core.FunctionLambda(value=l):
                return term_is_value(g, l.body)
            
            case hydra.core.FunctionPrimitive():
                return True
    match hydra.rewriting.deannotate_term(term):
        case hydra.core.TermApplication():
            return False
        
        case hydra.core.TermEither(value=e):
            return hydra.lib.eithers.either((lambda l: term_is_value(g, l)), (lambda r: term_is_value(g, r)), e)
        
        case hydra.core.TermLiteral():
            return True
        
        case hydra.core.TermFunction(value=f):
            return function_is_value(f)
        
        case hydra.core.TermList(value=els):
            return for_list(els)
        
        case hydra.core.TermMap(value=m):
            return hydra.lib.lists.foldl((lambda b, kv: hydra.lib.logic.and_(b, hydra.lib.logic.and_(term_is_value(g, kv[0]), term_is_value(g, kv[1])))), True, hydra.lib.maps.to_list(m))
        
        case hydra.core.TermMaybe(value=m2):
            return hydra.lib.maybes.maybe(True, (lambda v1: term_is_value(g, v1)), m2)
        
        case hydra.core.TermRecord(value=r):
            return check_fields(r.fields)
        
        case hydra.core.TermSet(value=s):
            return for_list(hydra.lib.sets.to_list(s))
        
        case hydra.core.TermUnion(value=i):
            return check_field(i.field)
        
        case hydra.core.TermUnit():
            return True
        
        case hydra.core.TermVariable():
            return False
        
        case _:
            return False
