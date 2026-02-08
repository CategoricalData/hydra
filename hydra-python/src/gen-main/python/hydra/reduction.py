# Note: this is an automatically generated file. Do not edit.

r"""Functions for reducing terms and types, i.e. performing computations."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.arity
import hydra.checking
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
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.rewriting
import hydra.schemas
import hydra.typing

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")

def alpha_convert(vold: hydra.core.Name, vnew: hydra.core.Name, term: hydra.core.Term) -> hydra.core.Term:
    r"""Alpha convert a variable in a term."""
    
    return hydra.rewriting.replace_free_term_variable(vold, cast(hydra.core.Term, hydra.core.TermVariable(vnew)), term)

def beta_reduce_type(typ: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]:
    r"""Eagerly beta-reduce a type by substituting type arguments into type lambdas."""
    
    def reduce_app(app: hydra.core.ApplicationType) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]:
        @lru_cache(1)
        def lhs() -> hydra.core.Type:
            return app.function
        @lru_cache(1)
        def rhs() -> hydra.core.Type:
            return app.argument
        match lhs():
            case hydra.core.TypeAnnotated(value=at):
                return hydra.lib.flows.bind(reduce_app(hydra.core.ApplicationType(at.body, rhs())), (lambda a: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeAnnotated(hydra.core.AnnotatedType(a, at.annotation))))))
            
            case hydra.core.TypeForall(value=ft):
                return beta_reduce_type(hydra.rewriting.replace_free_type_variable(ft.parameter, rhs(), ft.body))
            
            case hydra.core.TypeVariable(value=name):
                return hydra.lib.flows.bind(hydra.schemas.require_type(name), (lambda t_: beta_reduce_type(cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(t_, rhs()))))))
            
            case _:
                raise TypeError("Unsupported Type")
    def map_expr(recurse: Callable[[T0], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]], t: T0) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]:
        def find_app(r: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]:
            match r:
                case hydra.core.TypeApplication(value=a):
                    return reduce_app(a)
                
                case _:
                    return hydra.lib.flows.pure(r)
        return hydra.lib.flows.bind(recurse(t), (lambda r: find_app(r)))
    return hydra.rewriting.rewrite_type_m((lambda x1, x2: map_expr(x1, x2)), typ)

def contract_term(term: hydra.core.Term) -> hydra.core.Term:
    r"""Apply the special rules:
        ((\x.e1) e2) == e1, where x does not appear free in e1
      and
         ((\x.e1) e2) = e1[x/e2]
    These are both limited forms of beta reduction which help to "clean up" a term without fully evaluating it."""
    
    def rewrite(recurse: Callable[[T0], hydra.core.Term], t: T0) -> hydra.core.Term:
        @lru_cache(1)
        def rec() -> hydra.core.Term:
            return recurse(t)
        match rec():
            case hydra.core.TermApplication(value=app):
                @lru_cache(1)
                def lhs() -> hydra.core.Term:
                    return app.function
                @lru_cache(1)
                def rhs() -> hydra.core.Term:
                    return app.argument
                def _hoist_body_1(v1: hydra.core.Function) -> hydra.core.Term:
                    match v1:
                        case hydra.core.FunctionLambda(value=l):
                            @lru_cache(1)
                            def v() -> hydra.core.Name:
                                return l.parameter
                            @lru_cache(1)
                            def body() -> hydra.core.Term:
                                return l.body
                            return hydra.lib.logic.if_else(hydra.rewriting.is_free_variable_in_term(v(), body()), (lambda : body()), (lambda : hydra.rewriting.replace_free_term_variable(v(), rhs(), body())))
                        
                        case _:
                            return rec()
                match hydra.rewriting.deannotate_term(lhs()):
                    case hydra.core.TermFunction(value=f):
                        return _hoist_body_1(f)
                    
                    case _:
                        return rec()
            
            case _:
                return rec()
    return hydra.rewriting.rewrite_term((lambda x1, x2: rewrite(x1, x2)), term)

count_primitive_invocations = True

def eta_expansion_arity(graph: hydra.graph.Graph, term: hydra.core.Term) -> int:
    def _hoist_hydra_reduction_eta_expansion_arity_1(graph: hydra.graph.Graph, v1: hydra.core.Function) -> int:
        match v1:
            case hydra.core.FunctionElimination():
                return 1
            
            case hydra.core.FunctionLambda():
                return 0
            
            case hydra.core.FunctionPrimitive(value=name):
                return hydra.arity.primitive_arity(hydra.lib.maybes.from_just(hydra.lexical.lookup_primitive(graph, name)))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    match term:
        case hydra.core.TermAnnotated(value=at):
            return eta_expansion_arity(graph, at.body)
        
        case hydra.core.TermApplication(value=app):
            return hydra.lib.math.sub(eta_expansion_arity(graph, app.function), 1)
        
        case hydra.core.TermFunction(value=f):
            return _hoist_hydra_reduction_eta_expansion_arity_1(graph, f)
        
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
        @lru_cache(1)
        def apps() -> hydra.core.Term:
            return hydra.lib.lists.foldl((lambda lhs, arg: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(lhs, arg)))), t, args)
        @lru_cache(1)
        def is_() -> frozenlist[int]:
            return hydra.lib.logic.if_else(hydra.lib.equality.lte(arity, hydra.lib.lists.length(args)), (lambda : ()), (lambda : hydra.lib.math.range_(1, hydra.lib.math.sub(arity, hydra.lib.lists.length(args)))))
        def pad(indices: frozenlist[int], t2: hydra.core.Term) -> hydra.core.Term:
            return hydra.lib.logic.if_else(hydra.lib.lists.null(indices), (lambda : t2), (lambda : cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name(hydra.lib.strings.cat2("v", hydra.lib.literals.show_int32(hydra.lib.lists.head(indices)))), Nothing(), pad(hydra.lib.lists.tail(indices), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(t2, cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name(hydra.lib.strings.cat2("v", hydra.lib.literals.show_int32(hydra.lib.lists.head(indices)))))))))))))))))
        return pad(is_(), apps())
    def rewrite(args: frozenlist[hydra.core.Term], recurse: Callable[[hydra.core.Term], hydra.core.Term], t: hydra.core.Term) -> hydra.core.Term:
        def after_recursion(term2: hydra.core.Term) -> hydra.core.Term:
            return expand(args, eta_expansion_arity(graph, term2), term2)
        @lru_cache(1)
        def t2() -> hydra.core.Term:
            return hydra.rewriting.detype_term(t)
        match t2():
            case hydra.core.TermApplication(value=app):
                @lru_cache(1)
                def lhs() -> hydra.core.Term:
                    return app.function
                @lru_cache(1)
                def rhs() -> hydra.core.Term:
                    return app.argument
                @lru_cache(1)
                def erhs() -> hydra.core.Term:
                    return rewrite((), recurse, rhs())
                return rewrite(hydra.lib.lists.cons(erhs(), args), recurse, lhs())
            
            case _:
                return after_recursion(recurse(t2()))
    return contract_term(hydra.rewriting.rewrite_term((lambda v1, v2: rewrite((), v1, v2)), term))

def eta_expand_term_new(tx0: hydra.typing.TypeContext, term0: hydra.core.Term) -> hydra.core.Term:
    r"""Recursively transform terms to eliminate partial application, e.g. 'add 42' becomes '\x.add 42 x'. Uses the TypeContext to look up types for arity calculation. Bare primitives and variables are NOT expanded; eliminations and partial applications are. This version properly tracks the TypeContext through nested scopes."""
    
    def term_arity_with_context(tx: hydra.typing.TypeContext, term: hydra.core.Term) -> int:
        def _hoist_term_arity_with_context_1(tx: hydra.typing.TypeContext, v1: hydra.core.Function) -> int:
            match v1:
                case hydra.core.FunctionElimination():
                    return 1
                
                case hydra.core.FunctionLambda():
                    return 0
                
                case hydra.core.FunctionPrimitive(value=name):
                    return hydra.lib.maybes.maybe(0, (lambda x1: hydra.arity.type_scheme_arity(x1)), hydra.lib.maps.lookup(name, tx.inference_context.primitive_types))
                
                case _:
                    raise AssertionError("Unreachable: all variants handled")
        match term:
            case hydra.core.TermAnnotated(value=at):
                return term_arity_with_context(tx, at.body)
            
            case hydra.core.TermApplication(value=app):
                return hydra.lib.math.sub(term_arity_with_context(tx, app.function), 1)
            
            case hydra.core.TermFunction(value=f):
                return _hoist_term_arity_with_context_1(tx, f)
            
            case hydra.core.TermLet(value=l):
                return term_arity_with_context(hydra.schemas.extend_type_context_for_let((lambda _, _2: Nothing()), tx, l), l.body)
            
            case hydra.core.TermTypeLambda(value=tl):
                return term_arity_with_context(hydra.schemas.extend_type_context_for_type_lambda(tx, tl), tl.body)
            
            case hydra.core.TermTypeApplication(value=tat):
                return term_arity_with_context(tx, tat.body)
            
            case hydra.core.TermVariable(value=name):
                return hydra.lib.maybes.maybe(0, (lambda x1: hydra.arity.type_arity(x1)), hydra.lib.maps.lookup(name, tx.types))
            
            case _:
                return 0
    def expand(always_pad: bool, args: frozenlist[hydra.core.Term], arity: int, head: hydra.core.Term) -> hydra.core.Term:
        @lru_cache(1)
        def applied() -> hydra.core.Term:
            return hydra.lib.lists.foldl((lambda lhs, arg: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(lhs, arg)))), head, args)
        @lru_cache(1)
        def num_args() -> int:
            return hydra.lib.lists.length(args)
        @lru_cache(1)
        def needed() -> int:
            return hydra.lib.math.sub(arity, num_args())
        return hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.gt(needed(), 0), hydra.lib.logic.or_(always_pad, hydra.lib.equality.gt(num_args(), 0))), (lambda : (indices := hydra.lib.math.range_(1, needed()), (fully_applied := hydra.lib.lists.foldl((lambda body, i: (vn := hydra.core.Name(hydra.lib.strings.cat2("v", hydra.lib.literals.show_int32(i))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(body, cast(hydra.core.Term, hydra.core.TermVariable(vn))))))[1]), applied(), indices), hydra.lib.lists.foldl((lambda body, i: (vn := hydra.core.Name(hydra.lib.strings.cat2("v", hydra.lib.literals.show_int32(i))), cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(vn, Nothing(), body))))))[1]), fully_applied, hydra.lib.lists.reverse(indices)))[1])[1]), (lambda : applied()))
    def rewrite_with_args(args: frozenlist[hydra.core.Term], tx: hydra.typing.TypeContext, term: hydra.core.Term) -> hydra.core.Term:
        def recurse(tx1: hydra.typing.TypeContext, term1: hydra.core.Term) -> hydra.core.Term:
            return rewrite_with_args((), tx1, term1)
        def after_recursion(trm: hydra.core.Term) -> hydra.core.Term:
            @lru_cache(1)
            def arity() -> int:
                return term_arity_with_context(tx, trm)
            return expand(False, args, arity(), trm)
        def for_field(f: hydra.core.Field) -> hydra.core.Field:
            return hydra.core.Field(f.name, recurse(tx, f.term))
        def for_case_branch(f: hydra.core.Field) -> hydra.core.Field:
            @lru_cache(1)
            def branch_body() -> hydra.core.Term:
                return recurse(tx, f.term)
            @lru_cache(1)
            def arty() -> int:
                return term_arity_with_context(tx, branch_body())
            return hydra.core.Field(f.name, expand(True, (), arty(), branch_body()))
        def for_elimination(elm: hydra.core.Elimination) -> hydra.core.Elimination:
            match elm:
                case hydra.core.EliminationRecord(value=p):
                    return cast(hydra.core.Elimination, hydra.core.EliminationRecord(p))
                
                case hydra.core.EliminationUnion(value=cs):
                    return cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(cs.type_name, hydra.lib.maybes.map((lambda t1: recurse(tx, t1)), cs.default), hydra.lib.lists.map((lambda x1: for_case_branch(x1)), cs.cases))))
                
                case hydra.core.EliminationWrap(value=nm):
                    return cast(hydra.core.Elimination, hydra.core.EliminationWrap(nm))
                
                case _:
                    raise AssertionError("Unreachable: all variants handled")
        def for_map(mp: FrozenDict[hydra.core.Term, hydra.core.Term]) -> FrozenDict[hydra.core.Term, hydra.core.Term]:
            def for_pair(pr: tuple[hydra.core.Term, hydra.core.Term]) -> tuple[hydra.core.Term, hydra.core.Term]:
                return (recurse(tx, hydra.lib.pairs.first(pr)), recurse(tx, hydra.lib.pairs.second(pr)))
            return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda x1: for_pair(x1)), hydra.lib.maps.to_list(mp)))
        def _hoist_body_1(v1: hydra.core.Function) -> hydra.core.Term:
            match v1:
                case hydra.core.FunctionElimination(value=elm):
                    @lru_cache(1)
                    def pad_elim() -> bool:
                        match elm:
                            case hydra.core.EliminationRecord():
                                return False
                            
                            case hydra.core.EliminationUnion():
                                return True
                            
                            case hydra.core.EliminationWrap():
                                return False
                            
                            case _:
                                raise AssertionError("Unreachable: all variants handled")
                    return expand(pad_elim(), args, 1, cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(for_elimination(elm))))))
                
                case hydra.core.FunctionLambda(value=lm):
                    @lru_cache(1)
                    def tx1() -> hydra.typing.TypeContext:
                        return hydra.schemas.extend_type_context_for_lambda(tx, lm)
                    @lru_cache(1)
                    def body() -> hydra.core.Term:
                        return rewrite_with_args((), tx1(), lm.body)
                    @lru_cache(1)
                    def result() -> hydra.core.Term:
                        return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(lm.parameter, lm.domain, body())))))
                    @lru_cache(1)
                    def arty() -> int:
                        return term_arity_with_context(tx, result())
                    return expand(False, args, arty(), result())
                
                case hydra.core.FunctionPrimitive():
                    @lru_cache(1)
                    def arty() -> int:
                        return term_arity_with_context(tx, term)
                    return expand(False, args, arty(), term)
                
                case _:
                    raise AssertionError("Unreachable: all variants handled")
        match term:
            case hydra.core.TermAnnotated(value=at):
                return after_recursion(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(recurse(tx, at.body), at.annotation))))
            
            case hydra.core.TermApplication(value=app):
                @lru_cache(1)
                def rhs() -> hydra.core.Term:
                    return rewrite_with_args((), tx, app.argument)
                return rewrite_with_args(hydra.lib.lists.cons(rhs(), args), tx, app.function)
            
            case hydra.core.TermEither(value=e):
                return after_recursion(cast(hydra.core.Term, hydra.core.TermEither(hydra.lib.eithers.either((lambda l: Left(recurse(tx, l))), (lambda r: Right(recurse(tx, r))), e))))
            
            case hydra.core.TermFunction(value=fn):
                return _hoist_body_1(fn)
            
            case hydra.core.TermLet(value=lt):
                @lru_cache(1)
                def tx1() -> hydra.typing.TypeContext:
                    return hydra.schemas.extend_type_context_for_let((lambda _, _2: Nothing()), tx, lt)
                def map_binding(b: hydra.core.Binding) -> hydra.core.Binding:
                    return hydra.core.Binding(b.name, rewrite_with_args((), tx1(), b.term), b.type)
                @lru_cache(1)
                def result() -> hydra.core.Term:
                    return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.map((lambda x1: map_binding(x1)), lt.bindings), rewrite_with_args((), tx1(), lt.body))))
                return after_recursion(result())
            
            case hydra.core.TermList(value=els):
                return after_recursion(cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda el: recurse(tx, el)), els))))
            
            case hydra.core.TermLiteral(value=v):
                return cast(hydra.core.Term, hydra.core.TermLiteral(v))
            
            case hydra.core.TermMap(value=mp):
                return after_recursion(cast(hydra.core.Term, hydra.core.TermMap(for_map(mp))))
            
            case hydra.core.TermMaybe(value=mb):
                return after_recursion(cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map((lambda v: recurse(tx, v)), mb))))
            
            case hydra.core.TermPair(value=pr):
                return after_recursion(cast(hydra.core.Term, hydra.core.TermPair((recurse(tx, hydra.lib.pairs.first(pr)), recurse(tx, hydra.lib.pairs.second(pr))))))
            
            case hydra.core.TermRecord(value=rc):
                return after_recursion(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(rc.type_name, hydra.lib.lists.map((lambda x1: for_field(x1)), rc.fields)))))
            
            case hydra.core.TermSet(value=st):
                return after_recursion(cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(hydra.lib.lists.map((lambda el: recurse(tx, el)), hydra.lib.sets.to_list(st))))))
            
            case hydra.core.TermTypeApplication(value=tt):
                return after_recursion(cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(recurse(tx, tt.body), tt.type))))
            
            case hydra.core.TermTypeLambda(value=tl):
                @lru_cache(1)
                def tx1() -> hydra.typing.TypeContext:
                    return hydra.schemas.extend_type_context_for_type_lambda(tx, tl)
                @lru_cache(1)
                def result() -> hydra.core.Term:
                    return cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(tl.parameter, rewrite_with_args((), tx1(), tl.body))))
                return after_recursion(result())
            
            case hydra.core.TermUnion(value=inj):
                return after_recursion(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(inj.type_name, for_field(inj.field)))))
            
            case hydra.core.TermUnit():
                return cast(hydra.core.Term, hydra.core.TermUnit())
            
            case hydra.core.TermVariable():
                @lru_cache(1)
                def arty() -> int:
                    return term_arity_with_context(tx, term)
                return expand(False, args, arty(), term)
            
            case hydra.core.TermWrap(value=wt):
                return after_recursion(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(wt.type_name, recurse(tx, wt.body)))))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    return contract_term(rewrite_with_args((), tx0, term0))

def eta_expand_typed_term(tx0: hydra.typing.TypeContext, term0: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
    def rewrite(top_level: bool, forced: bool, type_args: frozenlist[hydra.core.Type], recurse: Callable[[hydra.typing.TypeContext, hydra.core.Term], hydra.compute.Flow[T1, hydra.core.Term]], tx: hydra.typing.TypeContext, term: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.core.Term]:
        def rewrite_spine(term2: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.core.Term]:
            match term2:
                case hydra.core.TermAnnotated(value=at):
                    return hydra.lib.flows.bind(rewrite_spine(at.body), (lambda body: (ann := at.annotation, hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(body, ann)))))[1]))
                
                case hydra.core.TermApplication(value=a):
                    @lru_cache(1)
                    def l() -> frozenlist[hydra.core.Type]:
                        return hydra.lib.logic.if_else(False, (lambda : (cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))),)), (lambda : ()))
                    return hydra.lib.flows.bind(rewrite_spine(a.function), (lambda lhs: hydra.lib.flows.bind(rewrite(True, False, l(), recurse, tx, a.argument), (lambda rhs: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(lhs, rhs))))))))
                
                case hydra.core.TermTypeApplication(value=tat):
                    return hydra.lib.flows.bind(rewrite_spine(tat.body), (lambda body: (typ := tat.type, hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(body, typ)))))[1]))
                
                case _:
                    return rewrite(False, False, (), recurse, tx, term2)
        def arity_of(tx2: hydra.typing.TypeContext, term2: hydra.core.Term) -> hydra.compute.Flow[T2, int]:
            @lru_cache(1)
            def dflt() -> hydra.compute.Flow[T3, int]:
                return hydra.lib.flows.map((lambda x1: hydra.arity.type_arity(x1)), hydra.checking.type_of(tx2, (), term2))
            def for_function(tx3: hydra.typing.TypeContext, f: hydra.core.Function) -> hydra.compute.Flow[T2, int]:
                match f:
                    case hydra.core.FunctionElimination():
                        return hydra.lib.flows.pure(1)
                    
                    case hydra.core.FunctionLambda(value=l):
                        @lru_cache(1)
                        def txl() -> hydra.typing.TypeContext:
                            return hydra.schemas.extend_type_context_for_lambda(tx3, l)
                        return arity_of(txl(), l.body)
                    
                    case hydra.core.FunctionPrimitive(value=name):
                        return hydra.lib.flows.map((lambda x1: hydra.arity.type_scheme_arity(x1)), hydra.lexical.require_primitive_type(tx3, name))
                    
                    case _:
                        raise AssertionError("Unreachable: all variants handled")
            match term2:
                case hydra.core.TermAnnotated(value=at):
                    return arity_of(tx2, at.body)
                
                case hydra.core.TermFunction(value=f):
                    return for_function(tx2, f)
                
                case hydra.core.TermLet(value=l):
                    @lru_cache(1)
                    def txl() -> hydra.typing.TypeContext:
                        return hydra.schemas.extend_type_context_for_let((lambda _, _2: Nothing()), tx2, l)
                    return arity_of(txl(), l.body)
                
                case hydra.core.TermTypeApplication(value=tat):
                    return arity_of(tx2, tat.body)
                
                case hydra.core.TermTypeLambda(value=tl):
                    @lru_cache(1)
                    def txt() -> hydra.typing.TypeContext:
                        return hydra.schemas.extend_type_context_for_type_lambda(tx2, tl)
                    return arity_of(txt(), tl.body)
                
                case hydra.core.TermVariable(value=name):
                    return hydra.lib.maybes.maybe(hydra.lib.flows.map((lambda x1: hydra.arity.type_arity(x1)), hydra.checking.type_of(tx2, (), cast(hydra.core.Term, hydra.core.TermVariable(name)))), (lambda t: hydra.lib.flows.pure(hydra.arity.type_arity(t))), hydra.lib.maps.lookup(name, tx2.types))
                
                case _:
                    return dflt()
        def extra_variables(n: int) -> frozenlist[hydra.core.Name]:
            return hydra.lib.lists.map((lambda i: hydra.core.Name(hydra.lib.strings.cat2("v", hydra.lib.literals.show_int32(i)))), hydra.lib.math.range_(1, n))
        def pad(vars: frozenlist[hydra.core.Name], body: hydra.core.Term) -> hydra.core.Term:
            return hydra.lib.logic.if_else(hydra.lib.lists.null(vars), (lambda : body), (lambda : cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.lib.lists.head(vars), Nothing(), pad(hydra.lib.lists.tail(vars), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(body, cast(hydra.core.Term, hydra.core.TermVariable(hydra.lib.lists.head(vars))))))))))))))
        def padn(n: int, body: hydra.core.Term) -> hydra.core.Term:
            return pad(extra_variables(n), body)
        def unwind(term2: hydra.core.Term) -> hydra.core.Term:
            return hydra.lib.lists.foldl((lambda e, t: cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(e, t)))), term2, type_args)
        def force_expansion(t: hydra.core.Term) -> hydra.compute.Flow[T2, hydra.core.Term]:
            return hydra.lib.flows.bind(hydra.checking.type_of(tx, (), t), (lambda typ: (arity := hydra.arity.type_arity(typ), hydra.lib.flows.pure(padn(arity, unwind(t))))[1]))
        def recurse_or_force(term2: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.core.Term]:
            return hydra.lib.logic.if_else(forced, (lambda : force_expansion(term2)), (lambda : recurse(tx, unwind(term2))))
        def for_case(f: hydra.core.Field) -> hydra.compute.Flow[T1, hydra.core.Field]:
            return hydra.lib.flows.bind(rewrite(False, True, (), recurse, tx, f.term), (lambda r: hydra.lib.flows.pure(hydra.core.Field(f.name, r))))
        def for_case_statement(cs: hydra.core.CaseStatement) -> hydra.compute.Flow[T1, hydra.core.Term]:
            @lru_cache(1)
            def tname() -> hydra.core.Name:
                return cs.type_name
            @lru_cache(1)
            def dflt() -> Maybe[hydra.core.Term]:
                return cs.default
            @lru_cache(1)
            def cases() -> frozenlist[hydra.core.Field]:
                return cs.cases
            return hydra.lib.flows.bind(hydra.lib.flows.map_maybe((lambda v1: rewrite(False, False, (), recurse, tx, v1)), dflt()), (lambda rdflt: hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda x1: for_case(x1)), cases()), (lambda rcases: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(tname(), rdflt, rcases))))))))))))
        def for_elimination(elm: hydra.core.Elimination) -> hydra.compute.Flow[T1, hydra.core.Term]:
            def check_base(elm2: hydra.core.Elimination) -> hydra.compute.Flow[T1, hydra.core.Term]:
                match elm2:
                    case hydra.core.EliminationUnion(value=cs):
                        return for_case_statement(cs)
                    
                    case _:
                        return recurse(tx, term)
            return hydra.lib.flows.bind(hydra.lib.flows.map((lambda x1: unwind(x1)), check_base(elm)), (lambda base: hydra.lib.flows.pure(hydra.lib.logic.if_else(hydra.lib.logic.or_(top_level, forced), (lambda : padn(1, base)), (lambda : base)))))
        def _hoist_body_1(v1: hydra.core.Function) -> hydra.compute.Flow[T1, hydra.core.Term]:
            match v1:
                case hydra.core.FunctionElimination(value=elm):
                    return for_elimination(elm)
                
                case hydra.core.FunctionLambda(value=l):
                    @lru_cache(1)
                    def txl() -> hydra.typing.TypeContext:
                        return hydra.schemas.extend_type_context_for_lambda(tx, l)
                    return hydra.lib.flows.map((lambda x1: unwind(x1)), recurse(txl(), term))
                
                case _:
                    return recurse_or_force(term)
        match term:
            case hydra.core.TermApplication(value=a):
                @lru_cache(1)
                def lhs() -> hydra.core.Term:
                    return a.function
                @lru_cache(1)
                def rhs() -> hydra.core.Term:
                    return a.argument
                return hydra.lib.flows.bind(rewrite(True, False, (), recurse, tx, rhs()), (lambda rhs2: hydra.lib.flows.bind(arity_of(tx, lhs()), (lambda lhsarity: hydra.lib.flows.bind(rewrite_spine(lhs()), (lambda lhs2: (a2 := cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(lhs2, rhs2))), hydra.lib.flows.pure(hydra.lib.logic.if_else(hydra.lib.equality.gt(lhsarity, 1), (lambda : padn(hydra.lib.math.sub(lhsarity, 1), a2)), (lambda : a2))))[1]))))))
            
            case hydra.core.TermFunction(value=f):
                return _hoist_body_1(f)
            
            case hydra.core.TermLet(value=l):
                @lru_cache(1)
                def txlt() -> hydra.typing.TypeContext:
                    return hydra.schemas.extend_type_context_for_let((lambda _, _2: Nothing()), tx, l)
                return recurse(txlt(), term)
            
            case hydra.core.TermTypeApplication(value=tat):
                return rewrite(top_level, forced, hydra.lib.lists.cons(tat.type, type_args), recurse, tx, tat.body)
            
            case hydra.core.TermTypeLambda(value=tl):
                @lru_cache(1)
                def txt() -> hydra.typing.TypeContext:
                    return hydra.schemas.extend_type_context_for_type_lambda(tx, tl)
                return recurse(txt(), term)
            
            case _:
                return recurse_or_force(term)
    return hydra.rewriting.rewrite_term_with_context_m((lambda v1, v2, v3: rewrite(True, False, (), v1, v2, v3)), tx0, term0)

def eta_reduce_term(term: hydra.core.Term) -> hydra.core.Term:
    r"""Eta-reduce a term by removing redundant lambda abstractions."""
    
    @lru_cache(1)
    def no_change() -> hydra.core.Term:
        return term
    def reduce_lambda(l: hydra.core.Lambda) -> hydra.core.Term:
        @lru_cache(1)
        def v() -> hydra.core.Name:
            return l.parameter
        @lru_cache(1)
        def d() -> Maybe[hydra.core.Type]:
            return l.domain
        @lru_cache(1)
        def body() -> hydra.core.Term:
            return l.body
        match eta_reduce_term(body()):
            case hydra.core.TermAnnotated(value=at):
                return reduce_lambda(hydra.core.Lambda(v(), d(), at.body))
            
            case hydra.core.TermApplication(value=app):
                @lru_cache(1)
                def lhs() -> hydra.core.Term:
                    return app.function
                @lru_cache(1)
                def rhs() -> hydra.core.Term:
                    return app.argument
                match eta_reduce_term(rhs()):
                    case hydra.core.TermAnnotated(value=at):
                        return reduce_lambda(hydra.core.Lambda(v(), d(), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(lhs(), at.body)))))
                    
                    case hydra.core.TermVariable(value=v1):
                        return hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.equal(v().value, v1.value), hydra.lib.logic.not_(hydra.rewriting.is_free_variable_in_term(v(), lhs()))), (lambda : eta_reduce_term(lhs())), (lambda : no_change()))
                    
                    case _:
                        return no_change()
            
            case _:
                return no_change()
    def _hoist_body_1(v1: hydra.core.Function) -> hydra.core.Term:
        match v1:
            case hydra.core.FunctionLambda(value=l):
                return reduce_lambda(l)
            
            case _:
                return no_change()
    match term:
        case hydra.core.TermAnnotated(value=at):
            return cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(eta_reduce_term(at.body), at.annotation)))
        
        case hydra.core.TermFunction(value=f):
            return _hoist_body_1(f)
        
        case _:
            return no_change()

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
        @lru_cache(1)
        def is_non_lambda_term() -> bool:
            match term2:
                case hydra.core.TermFunction(value=f):
                    return is_non_lambda(f)
                
                case hydra.core.TermLet():
                    return False
                
                case _:
                    return True
        return hydra.lib.logic.and_(eager2, is_non_lambda_term())
    def reduce_arg(eager2: bool, arg: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
        return hydra.lib.logic.if_else(eager2, (lambda : hydra.lib.flows.pure(arg)), (lambda : reduce(False, arg)))
    def apply_to_arguments(fun: hydra.core.Term, args: frozenlist[hydra.core.Term]) -> hydra.core.Term:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(args), (lambda : fun), (lambda : apply_to_arguments(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun, hydra.lib.lists.head(args)))), hydra.lib.lists.tail(args))))
    def apply_elimination(elm: hydra.core.Elimination, reduced_arg: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
        match elm:
            case hydra.core.EliminationRecord(value=proj):
                return hydra.lib.flows.bind(hydra.extract.core.record(proj.type_name, hydra.rewriting.deannotate_term(reduced_arg)), (lambda fields: (matching_fields := hydra.lib.lists.filter((lambda f: hydra.lib.equality.equal(f.name, proj.field)), fields), hydra.lib.logic.if_else(hydra.lib.lists.null(matching_fields), (lambda : hydra.lib.flows.fail(hydra.lib.strings.cat(("no such field: ", proj.field.value, " in ", proj.type_name.value, " record")))), (lambda : hydra.lib.flows.pure(hydra.lib.lists.head(matching_fields).term))))[1]))
            
            case hydra.core.EliminationUnion(value=cs):
                return hydra.lib.flows.bind(hydra.extract.core.injection(cs.type_name, reduced_arg), (lambda field: (matching_fields := hydra.lib.lists.filter((lambda f: hydra.lib.equality.equal(f.name, field.name)), cs.cases), hydra.lib.logic.if_else(hydra.lib.lists.null(matching_fields), (lambda : hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat(("no such field ", field.name.value, " in ", cs.type_name.value, " case statement"))), (lambda x1: hydra.lib.flows.pure(x1)), cs.default)), (lambda : hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(hydra.lib.lists.head(matching_fields).term, field.term)))))))[1]))
            
            case hydra.core.EliminationWrap(value=name):
                return hydra.extract.core.wrap(name, reduced_arg)
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def apply_if_nullary(eager2: bool, original: hydra.core.Term, args: frozenlist[hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
        @lru_cache(1)
        def stripped() -> hydra.core.Term:
            return hydra.rewriting.deannotate_term(original)
        def for_elimination(elm: hydra.core.Elimination, args2: frozenlist[hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
            @lru_cache(1)
            def arg() -> hydra.core.Term:
                return hydra.lib.lists.head(args2)
            @lru_cache(1)
            def remaining_args() -> frozenlist[hydra.core.Term]:
                return hydra.lib.lists.tail(args2)
            return hydra.lib.flows.bind(reduce_arg(eager2, hydra.rewriting.deannotate_term(arg())), (lambda reduced_arg: hydra.lib.flows.bind(hydra.lib.flows.bind(apply_elimination(elm, reduced_arg), (lambda v1: reduce(eager2, v1))), (lambda reduced_result: apply_if_nullary(eager2, reduced_result, remaining_args())))))
        def for_lambda(l: hydra.core.Lambda, args2: frozenlist[hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
            @lru_cache(1)
            def param() -> hydra.core.Name:
                return l.parameter
            @lru_cache(1)
            def body() -> hydra.core.Term:
                return l.body
            @lru_cache(1)
            def arg() -> hydra.core.Term:
                return hydra.lib.lists.head(args2)
            @lru_cache(1)
            def remaining_args() -> frozenlist[hydra.core.Term]:
                return hydra.lib.lists.tail(args2)
            return hydra.lib.flows.bind(reduce(eager2, hydra.rewriting.deannotate_term(arg())), (lambda reduced_arg: hydra.lib.flows.bind(reduce(eager2, hydra.rewriting.replace_free_term_variable(param(), reduced_arg, body())), (lambda reduced_result: apply_if_nullary(eager2, reduced_result, remaining_args())))))
        def for_primitive(prim: hydra.graph.Primitive, arity: int, args2: frozenlist[hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
            @lru_cache(1)
            def arg_list() -> frozenlist[hydra.core.Term]:
                return hydra.lib.lists.take(arity, args2)
            @lru_cache(1)
            def remaining_args() -> frozenlist[hydra.core.Term]:
                return hydra.lib.lists.drop(arity, args2)
            return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: reduce_arg(eager2, v1)), arg_list()), (lambda reduced_args: (stripped_args := hydra.lib.lists.map((lambda x1: hydra.rewriting.deannotate_term(x1)), reduced_args), hydra.lib.flows.bind(hydra.lib.flows.bind(prim.implementation(stripped_args), (lambda v1: reduce(eager2, v1))), (lambda reduced_result: apply_if_nullary(eager2, reduced_result, remaining_args()))))[1]))
        def _hoist_body_1(v1: hydra.core.Function) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
            match v1:
                case hydra.core.FunctionElimination(value=elm):
                    return hydra.lib.logic.if_else(hydra.lib.lists.null(args), (lambda : hydra.lib.flows.pure(original)), (lambda : for_elimination(elm, args)))
                
                case hydra.core.FunctionLambda(value=l):
                    return hydra.lib.logic.if_else(hydra.lib.lists.null(args), (lambda : hydra.lib.flows.pure(original)), (lambda : for_lambda(l, args)))
                
                case hydra.core.FunctionPrimitive(value=name):
                    return hydra.lib.flows.bind(hydra.lexical.require_primitive(name), (lambda prim: (arity := hydra.arity.primitive_arity(prim), hydra.lib.logic.if_else(hydra.lib.equality.gt(arity, hydra.lib.lists.length(args)), (lambda : hydra.lib.flows.pure(apply_to_arguments(original, args))), (lambda : for_primitive(prim, arity, args))))[1]))
                
                case _:
                    raise AssertionError("Unreachable: all variants handled")
        match stripped():
            case hydra.core.TermApplication(value=app):
                return apply_if_nullary(eager2, app.function, hydra.lib.lists.cons(app.argument, args))
            
            case hydra.core.TermFunction(value=v1):
                return _hoist_body_1(v1)
            
            case hydra.core.TermVariable(value=v):
                return hydra.lib.flows.bind(hydra.lexical.dereference_element(v), (lambda m_binding: hydra.lib.maybes.maybe(hydra.lib.flows.pure(apply_to_arguments(original, args)), (lambda binding: apply_if_nullary(eager2, binding.term, args)), m_binding)))
            
            case hydra.core.TermLet(value=lt):
                @lru_cache(1)
                def bindings() -> frozenlist[hydra.core.Binding]:
                    return lt.bindings
                @lru_cache(1)
                def body() -> hydra.core.Term:
                    return lt.body
                def let_expr(b: hydra.core.Binding) -> hydra.core.Term:
                    return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let((b,), cast(hydra.core.Term, hydra.core.TermVariable(b.name)))))
                def expand_binding(b: hydra.core.Binding) -> hydra.core.Binding:
                    return hydra.core.Binding(b.name, hydra.rewriting.replace_free_term_variable(b.name, let_expr(b), b.term), b.type)
                @lru_cache(1)
                def expanded_bindings() -> frozenlist[hydra.core.Binding]:
                    return hydra.lib.lists.map((lambda x1: expand_binding(x1)), bindings())
                def substitute_binding(term2: hydra.core.Term, b: hydra.core.Binding) -> hydra.core.Term:
                    return hydra.rewriting.replace_free_term_variable(b.name, b.term, term2)
                def substitute_all(bs: frozenlist[hydra.core.Binding], term2: hydra.core.Term) -> hydra.core.Term:
                    return hydra.lib.lists.foldl((lambda x1, x2: substitute_binding(x1, x2)), term2, bs)
                @lru_cache(1)
                def expanded_body() -> hydra.core.Term:
                    return substitute_all(expanded_bindings(), body())
                return hydra.lib.flows.bind(reduce(eager2, expanded_body()), (lambda reduced_body: apply_if_nullary(eager2, reduced_body, args)))
            
            case _:
                return hydra.lib.flows.pure(apply_to_arguments(original, args))
    def mapping(recurse: Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]], mid: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
        return hydra.lib.flows.bind(hydra.lib.logic.if_else(do_recurse(eager, mid), (lambda : recurse(mid)), (lambda : hydra.lib.flows.pure(mid))), (lambda inner: apply_if_nullary(eager, inner, ())))
    return hydra.rewriting.rewrite_term_m((lambda x1, x2: mapping(x1, x2)), term)

def term_is_closed(term: hydra.core.Term) -> bool:
    r"""Whether a term is closed, i.e. represents a complete program."""
    
    return hydra.lib.sets.null(hydra.rewriting.free_variables_in_term(term))

def term_is_value(g: T0, term: hydra.core.Term) -> bool:
    def for_list(els: frozenlist[hydra.core.Term]) -> bool:
        return hydra.lib.lists.foldl((lambda b, t: hydra.lib.logic.and_(b, term_is_value(g, t))), True, els)
    def check_field(f: hydra.core.Field) -> bool:
        return term_is_value(g, f.term)
    def check_fields(fields: frozenlist[hydra.core.Field]) -> bool:
        return hydra.lib.lists.foldl((lambda b, f: hydra.lib.logic.and_(b, check_field(f))), True, fields)
    def function_is_value(f: hydra.core.Function) -> bool:
        def _hoist_function_is_value_1(v1: hydra.core.Elimination) -> bool:
            match v1:
                case hydra.core.EliminationWrap():
                    return True
                
                case hydra.core.EliminationRecord():
                    return True
                
                case hydra.core.EliminationUnion(value=cs):
                    return hydra.lib.logic.and_(check_fields(cs.cases), hydra.lib.maybes.maybe(True, (lambda v12: term_is_value(g, v12)), cs.default))
                
                case _:
                    raise AssertionError("Unreachable: all variants handled")
        match f:
            case hydra.core.FunctionElimination(value=e):
                return _hoist_function_is_value_1(e)
            
            case hydra.core.FunctionLambda(value=l):
                return term_is_value(g, l.body)
            
            case hydra.core.FunctionPrimitive():
                return True
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
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
            return hydra.lib.lists.foldl((lambda b, kv: hydra.lib.logic.and_(b, hydra.lib.logic.and_(term_is_value(g, hydra.lib.pairs.first(kv)), term_is_value(g, hydra.lib.pairs.second(kv))))), True, hydra.lib.maps.to_list(m))
        
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
