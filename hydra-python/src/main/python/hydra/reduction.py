# Note: this is an automatically generated file. Do not edit.

r"""Functions for reducing terms and types, i.e. performing computations."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Either, FrozenDict, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.accessors
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

def alpha_convert(vold: hydra.core.Name, vnew: hydra.core.Name, term: hydra.core.Term) -> hydra.core.Type:
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
    def map_expr(recurse: Callable[[T0], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]], t: T0) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]:
        def find_app(r: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]:
            match r:
                case hydra.core.TypeApplication(value=a):
                    return reduce_app(a)
                
                case _:
                    return hydra.lib.flows.pure(r)
        return hydra.lib.flows.bind(recurse(t), (lambda r: find_app(r)))
    return hydra.rewriting.rewrite_type_m(cast(Callable[[
      Callable[[hydra.core.Type], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]],
      hydra.core.Type], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]], (lambda x1, x2: map_expr(x1, x2))), typ)

def contract_term(term: hydra.core.Term) -> hydra.core.Type:
    r"""Apply the special rules:
        ((\x.e1) e2) == e1, where x does not appear free in e1
      and
         ((\x.e1) e2) = e1[x/e2]
    These are both limited forms of beta reduction which help to "clean up" a term without fully evaluating it."""
    
    def rewrite(recurse: Callable[[T0], hydra.core.Term], t: T0) -> hydra.core.Type:
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
                                return hydra.lib.logic.if_else(hydra.rewriting.is_free_variable_in_term(v, body), (lambda : body), (lambda : hydra.rewriting.replace_free_term_variable(v, rhs, body)))
                            
                            case _:
                                return rec
                    
                    case _:
                        return rec
            
            case _:
                return rec
    return hydra.rewriting.rewrite_term(cast(Callable[[Callable[[hydra.core.Term], hydra.core.Term], hydra.core.Term], hydra.core.Term], (lambda x1, x2: rewrite(x1, x2))), term)

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
                
                case _:
                    raise AssertionError("Unreachable: all variants handled")
        
        case hydra.core.TermTypeLambda(value=ta):
            return eta_expansion_arity(graph, ta.body)
        
        case hydra.core.TermTypeApplication(value=tt):
            return eta_expansion_arity(graph, tt.body)
        
        case hydra.core.TermVariable(value=name):
            return hydra.lib.maybes.maybe(0, (lambda ts: hydra.arity.type_arity(ts.type)), hydra.lib.maybes.bind(hydra.lexical.lookup_element(graph, name), (lambda b: b.type)))
        
        case _:
            return 0

def eta_expand_term(graph: hydra.graph.Graph, term: hydra.core.Term) -> hydra.core.Type:
    r"""Recursively transform arbitrary terms like 'add 42' into terms like '\x.add 42 x', in which the implicit parameters of primitive functions and eliminations are made into explicit lambda parameters. Variable references are not expanded. This is useful for targets like Python with weaker support for currying than Hydra or Haskell. Note: this is a "trusty" function which assumes the graph is well-formed, i.e. no dangling references."""
    
    def expand(args: frozenlist[hydra.core.Term], arity: int, t: hydra.core.Term) -> hydra.core.Type:
        def apps() -> hydra.core.Type:
            return hydra.lib.lists.foldl((lambda lhs, arg: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(lhs, arg)))), t, args)
        def is_() -> frozenlist[int]:
            return hydra.lib.logic.if_else(hydra.lib.equality.lte(arity, hydra.lib.lists.length(args)), (lambda : cast(frozenlist[int], ())), (lambda : hydra.lib.math.range_(1, hydra.lib.math.sub(arity, hydra.lib.lists.length(args)))))
        def pad(indices: frozenlist[int], t2: hydra.core.Term) -> hydra.core.Type:
            return hydra.lib.logic.if_else(hydra.lib.lists.null(indices), (lambda : t2), (lambda : cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name(hydra.lib.strings.cat2("v", hydra.lib.literals.show_int32(hydra.lib.lists.head(indices)))), cast(Maybe[hydra.core.Type], Nothing()), pad(hydra.lib.lists.tail(indices), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(t2, cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name(hydra.lib.strings.cat2("v", hydra.lib.literals.show_int32(hydra.lib.lists.head(indices)))))))))))))))))
        return pad(is_(), apps())
    def rewrite(args: frozenlist[hydra.core.Term], recurse: Callable[[hydra.core.Term], hydra.core.Term], t: hydra.core.Term) -> hydra.core.Type:
        def after_recursion(term2: hydra.core.Term) -> hydra.core.Type:
            return expand(args, eta_expansion_arity(graph, term2), term2)
        t2 = hydra.rewriting.detype_term(t)
        match t2:
            case hydra.core.TermApplication(value=app):
                lhs = app.function
                rhs = app.argument
                def erhs() -> hydra.core.Type:
                    return rewrite(cast(frozenlist[hydra.core.Term], ()), recurse, rhs)
                return rewrite(hydra.lib.lists.cons(erhs(), args), recurse, lhs)
            
            case _:
                return after_recursion(recurse(t2))
    return contract_term(hydra.rewriting.rewrite_term((lambda v1, v2: rewrite(cast(frozenlist[hydra.core.Term], ()), v1, v2)), term))

def eta_expand_typed_term(tx0: hydra.typing.TypeContext, term0: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
    def rewrite(top_level: bool, forced: bool, type_args: frozenlist[hydra.core.Type], recurse: Callable[[hydra.typing.TypeContext, hydra.core.Term], hydra.compute.Flow[T1, hydra.core.Term]], tx: hydra.typing.TypeContext, term: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.core.Term]:
        def rewrite_spine(term2: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.core.Term]:
            match term2:
                case hydra.core.TermAnnotated(value=at):
                    return hydra.lib.flows.bind(rewrite_spine(at.body), (lambda body: (ann := at.annotation, hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(body, ann)))))[1]))
                
                case hydra.core.TermApplication(value=a):
                    def l() -> frozenlist[hydra.core.Type]:
                        return hydra.lib.logic.if_else(False, (lambda : (cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))),)), (lambda : cast(frozenlist[hydra.core.Type], ())))
                    return hydra.lib.flows.bind(rewrite_spine(a.function), (lambda lhs: hydra.lib.flows.bind(rewrite(True, False, l(), recurse, tx, a.argument), (lambda rhs: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(lhs, rhs))))))))
                
                case hydra.core.TermTypeApplication(value=tat):
                    return hydra.lib.flows.bind(rewrite_spine(tat.body), (lambda body: (typ := tat.type, hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(body, typ)))))[1]))
                
                case _:
                    return rewrite(False, False, cast(frozenlist[hydra.core.Type], ()), recurse, tx, term2)
        def arity_of(tx2: hydra.typing.TypeContext, term2: hydra.core.Term) -> hydra.compute.Flow[T2, int]:
            def dflt() -> hydra.compute.Flow[T3, int]:
                return hydra.lib.flows.map(hydra.arity.type_arity, hydra.checking.type_of(tx2, cast(frozenlist[hydra.core.Type], ()), term2))
            def for_function(tx3: hydra.typing.TypeContext, f: hydra.core.Function) -> hydra.compute.Flow[T2, int]:
                match f:
                    case hydra.core.FunctionElimination():
                        return hydra.lib.flows.pure(1)
                    
                    case hydra.core.FunctionLambda(value=l):
                        txl = hydra.schemas.extend_type_context_for_lambda(tx3, l)
                        return arity_of(txl, l.body)
                    
                    case hydra.core.FunctionPrimitive(value=name):
                        return hydra.lib.flows.map(hydra.arity.type_scheme_arity, hydra.lexical.require_primitive_type(tx3, name))
                    
                    case _:
                        raise AssertionError("Unreachable: all variants handled")
            match term2:
                case hydra.core.TermAnnotated(value=at):
                    return arity_of(tx2, at.body)
                
                case hydra.core.TermFunction(value=f):
                    return for_function(tx2, f)
                
                case hydra.core.TermLet(value=l):
                    def txl() -> hydra.core.Type:
                        return hydra.schemas.extend_type_context_for_let((lambda _, _2: cast(Maybe[hydra.core.Term], Nothing())), tx2, l)
                    return arity_of(txl(), l.body)
                
                case hydra.core.TermTypeApplication(value=tat):
                    return arity_of(tx2, tat.body)
                
                case hydra.core.TermTypeLambda(value=tl):
                    txt = hydra.schemas.extend_type_context_for_type_lambda(tx2, tl)
                    return arity_of(txt, tl.body)
                
                case hydra.core.TermVariable(value=name):
                    return hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat(("unbound variable: ", name.value))), (lambda t: hydra.lib.flows.pure(hydra.arity.type_arity(t))), hydra.lib.maps.lookup(name, tx2.types))
                
                case _:
                    return cast(hydra.compute.Flow[T2, int], dflt())
        def extra_variables(n: int) -> frozenlist[hydra.core.Name]:
            return hydra.lib.lists.map((lambda i: hydra.core.Name(hydra.lib.strings.cat2("v", hydra.lib.literals.show_int32(i)))), hydra.lib.math.range_(1, n))
        def pad(vars: frozenlist[hydra.core.Name], body: hydra.core.Term) -> hydra.core.Type:
            return hydra.lib.logic.if_else(hydra.lib.lists.null(vars), (lambda : body), (lambda : cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.lib.lists.head(vars), cast(Maybe[hydra.core.Type], Nothing()), pad(hydra.lib.lists.tail(vars), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(body, cast(hydra.core.Term, hydra.core.TermVariable(hydra.lib.lists.head(vars))))))))))))))
        def padn(n: int, body: hydra.core.Term) -> hydra.core.Type:
            return pad(extra_variables(n), body)
        def unwind(term2: hydra.core.Term) -> hydra.core.Type:
            return hydra.lib.lists.foldl((lambda e, t: cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(e, t)))), term2, type_args)
        def force_expansion(t: hydra.core.Term) -> hydra.compute.Flow[T2, hydra.core.Term]:
            return hydra.lib.flows.bind(hydra.checking.type_of(tx, cast(frozenlist[hydra.core.Type], ()), t), (lambda typ: (arity := hydra.arity.type_arity(typ), hydra.lib.flows.pure(padn(arity, unwind(t))))[1]))
        def recurse_or_force(term2: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.core.Term]:
            return hydra.lib.logic.if_else(forced, (lambda : force_expansion(term2)), (lambda : recurse(tx, unwind(term2))))
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
            return hydra.lib.flows.bind(hydra.lib.flows.map(unwind, check_base(elm)), (lambda base: hydra.lib.flows.pure(hydra.lib.logic.if_else(hydra.lib.logic.or_(top_level, forced), (lambda : padn(1, base)), (lambda : base)))))
        match term:
            case hydra.core.TermApplication(value=a):
                lhs = a.function
                rhs = a.argument
                return hydra.lib.flows.bind(rewrite(True, False, cast(frozenlist[hydra.core.Type], ()), recurse, tx, rhs), (lambda rhs2: hydra.lib.flows.bind(arity_of(tx, lhs), (lambda lhsarity: hydra.lib.flows.bind(rewrite_spine(lhs), (lambda lhs2: (a2 := cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(lhs2, rhs2))), hydra.lib.flows.pure(hydra.lib.logic.if_else(hydra.lib.equality.gt(lhsarity, 1), (lambda : padn(hydra.lib.math.sub(lhsarity, 1), a2)), (lambda : a2))))[1]))))))
            
            case hydra.core.TermFunction(value=f):
                match f:
                    case hydra.core.FunctionElimination(value=elm):
                        return for_elimination(elm)
                    
                    case hydra.core.FunctionLambda(value=l):
                        txl = hydra.schemas.extend_type_context_for_lambda(tx, l)
                        return hydra.lib.flows.map(unwind, recurse(txl, term))
                    
                    case _:
                        return recurse_or_force(term)
            
            case hydra.core.TermLet(value=l):
                def txlt() -> hydra.core.Type:
                    return hydra.schemas.extend_type_context_for_let((lambda _, _2: cast(Maybe[hydra.core.Term], Nothing())), tx, l)
                return recurse(txlt(), term)
            
            case hydra.core.TermTypeApplication(value=tat):
                return rewrite(top_level, forced, hydra.lib.lists.cons(tat.type, type_args), recurse, tx, tat.body)
            
            case hydra.core.TermTypeLambda(value=tl):
                txt = hydra.schemas.extend_type_context_for_type_lambda(tx, tl)
                return recurse(txt, term)
            
            case _:
                return recurse_or_force(term)
    return hydra.rewriting.rewrite_term_with_context_m((lambda v1, v2, v3: rewrite(True, False, cast(frozenlist[hydra.core.Type], ()), v1, v2, v3)), tx0, term0)

def eta_reduce_term(term: hydra.core.Term) -> hydra.core.Type:
    r"""Eta-reduce a term by removing redundant lambda abstractions."""
    
    no_change = term
    def reduce_lambda(l: hydra.core.Lambda) -> hydra.core.Type:
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
                        return hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.equal(v.value, v1.value), hydra.lib.logic.not_(hydra.rewriting.is_free_variable_in_term(v, lhs))), (lambda : eta_reduce_term(lhs)), (lambda : no_change))
                    
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

def rewrite_and_fold_term_with_type_context(f: Callable[[
  Callable[[T0, hydra.core.Term], tuple[T0, hydra.core.Term]],
  hydra.typing.TypeContext,
  T0,
  hydra.core.Term], tuple[T0, hydra.core.Term]], cx0: hydra.typing.TypeContext, val0: T0, term0: hydra.core.Term) -> tuple[T0, hydra.core.Term]:
    def wrapper(low_level_recurse: Callable[[tuple[T0, hydra.typing.TypeContext], hydra.core.Term], tuple[tuple[T0, T1], hydra.core.Term]], val_and_cx: tuple[T0, hydra.typing.TypeContext], term: hydra.core.Term) -> tuple[tuple[T0, hydra.typing.TypeContext], hydra.core.Term]:
        def val() -> T0:
            return hydra.lib.pairs.first(val_and_cx)
        def cx() -> hydra.core.Type:
            return hydra.lib.pairs.second(val_and_cx)
        def cx1() -> hydra.core.Type:
            match term:
                case hydra.core.TermFunction(value=fun):
                    match fun:
                        case hydra.core.FunctionLambda(value=l):
                            return hydra.schemas.extend_type_context_for_lambda(cx(), l)
                        
                        case _:
                            return cx()
                
                case hydra.core.TermLet(value=l):
                    return hydra.schemas.extend_type_context_for_let((lambda _, _2: cast(Maybe[hydra.core.Term], Nothing())), cx(), l)
                
                case hydra.core.TermTypeLambda(value=tl):
                    return hydra.schemas.extend_type_context_for_type_lambda(cx(), tl)
                
                case _:
                    return cx()
        def recurse_for_user(new_val: T0, subterm: hydra.core.Term) -> tuple[T0, hydra.core.Term]:
            def result() -> tuple[tuple[T0, T1], hydra.core.Term]:
                return low_level_recurse(cast(tuple[T0, hydra.typing.TypeContext], (new_val, cx1())), subterm)
            return cast(tuple[T0, hydra.core.Term], (hydra.lib.pairs.first(hydra.lib.pairs.first(result())), hydra.lib.pairs.second(result())))
        def f_result() -> tuple[T0, hydra.core.Term]:
            return f(recurse_for_user, cx1(), val(), term)
        return cast(tuple[tuple[T0, hydra.typing.TypeContext], hydra.core.Term], (cast(tuple[T0, hydra.typing.TypeContext], (hydra.lib.pairs.first(f_result()), cx1())), hydra.lib.pairs.second(f_result())))
    def result() -> tuple[tuple[T0, hydra.typing.TypeContext], hydra.core.Term]:
        return hydra.rewriting.rewrite_and_fold_term(cast(Callable[[
          Callable[[tuple[T0, hydra.typing.TypeContext], hydra.core.Term], tuple[tuple[T0, hydra.typing.TypeContext], hydra.core.Term]],
          tuple[T0, hydra.typing.TypeContext],
          hydra.core.Term], tuple[tuple[T0, hydra.typing.TypeContext], hydra.core.Term]], (lambda x1, x2, x3: wrapper(x1, x2, x3))), cast(tuple[T0, hydra.typing.TypeContext], (val0, cx0)), term0)
    return cast(tuple[T0, hydra.core.Term], (hydra.lib.pairs.first(hydra.lib.pairs.first(result())), hydra.lib.pairs.second(result())))

def hoist_subterms(should_hoist: Callable[[frozenlist[hydra.accessors.TermAccessor], hydra.core.Term], bool], cx0: hydra.typing.TypeContext, term0: hydra.core.Term) -> hydra.core.Type:
    r"""Hoist subterms into bindings in the nearest enclosing let term, based on a predicate. The predicate receives the path (from the let body/binding root) and the term, and returns True if the term should be hoisted. When hoisting a term that contains free variables which are lambda-bound between the enclosing let and the current position, those variables are captured: the hoisted binding is wrapped in lambdas for those variables, and the reference is replaced with an application of those variables. Useful for targets that require certain constructs (e.g., case statements) at the top level."""
    
    def outer_rewrite(recurse: Callable[[int, hydra.core.Term], tuple[int, hydra.core.Term]], cx: hydra.typing.TypeContext, counter: int, term: hydra.core.Term) -> tuple[int, hydra.core.Term]:
        dflt = recurse(counter, term)
        def for_let(l: hydra.core.Let) -> tuple[int, hydra.core.Term]:
            body = l.body
            bindings = l.bindings
            parent_lambda_vars = cx.lambda_variables
            def process_subterm(counter_and_extra: tuple[int, frozenlist[hydra.core.Binding]], subterm: hydra.core.Term) -> tuple[tuple[int, frozenlist[hydra.core.Binding]], hydra.core.Term]:
                def counter0() -> int:
                    return hydra.lib.pairs.first(counter_and_extra)
                def extra_bindings0() -> frozenlist[hydra.core.Binding]:
                    return hydra.lib.pairs.second(counter_and_extra)
                def inner_rewrite(recurse2: Callable[[
                  frozenlist[hydra.accessors.TermAccessor],
                  tuple[tuple[T0, frozenlist[hydra.core.Binding]], hydra.typing.TypeContext],
                  hydra.core.Term], tuple[tuple[tuple[int, frozenlist[hydra.core.Binding]], T1], hydra.core.Term]], path: frozenlist[hydra.accessors.TermAccessor], acc: tuple[tuple[T0, frozenlist[hydra.core.Binding]], hydra.typing.TypeContext], term2: hydra.core.Term) -> tuple[tuple[tuple[int, frozenlist[hydra.core.Binding]], hydra.typing.TypeContext], hydra.core.Term]:
                    def counter_extra() -> tuple[T0, frozenlist[hydra.core.Binding]]:
                        return hydra.lib.pairs.first(acc)
                    def inner_cx() -> hydra.core.Type:
                        return hydra.lib.pairs.second(acc)
                    match term2:
                        case hydra.core.TermLet(value=nested_l):
                            def nested_result() -> tuple[int, hydra.core.Term]:
                                return for_let(nested_l)
                            return cast(tuple[tuple[tuple[int, frozenlist[hydra.core.Binding]], hydra.typing.TypeContext], hydra.core.Term], (cast(tuple[tuple[int, frozenlist[hydra.core.Binding]], hydra.typing.TypeContext], (cast(tuple[int, frozenlist[hydra.core.Binding]], (hydra.lib.pairs.first(nested_result()), hydra.lib.pairs.second(counter_extra()))), inner_cx())), hydra.lib.pairs.second(nested_result())))
                        
                        case _:
                            return "let terms are not supported here"
                def result() -> tuple[tuple[tuple[int, frozenlist[hydra.core.Binding]], hydra.typing.TypeContext], hydra.core.Term]:
                    return hydra.rewriting.rewrite_and_fold_term_with_path(cast(Callable[[
                      Callable[[
                        frozenlist[hydra.accessors.TermAccessor],
                        tuple[tuple[int, frozenlist[hydra.core.Binding]], hydra.typing.TypeContext],
                        hydra.core.Term], tuple[tuple[tuple[int, frozenlist[hydra.core.Binding]], hydra.typing.TypeContext], hydra.core.Term]],
                      frozenlist[hydra.accessors.TermAccessor],
                      tuple[tuple[int, frozenlist[hydra.core.Binding]], hydra.typing.TypeContext],
                      hydra.core.Term], tuple[tuple[tuple[int, frozenlist[hydra.core.Binding]], hydra.typing.TypeContext], hydra.core.Term]], (lambda x1, x2, x3, x4: inner_rewrite(x1, x2, x3, x4))), cast(tuple[tuple[int, frozenlist[hydra.core.Binding]], hydra.typing.TypeContext], (cast(tuple[int, frozenlist[hydra.core.Binding]], (counter0(), extra_bindings0())), cx)), subterm)
                return cast(tuple[tuple[int, frozenlist[hydra.core.Binding]], hydra.core.Term], (hydra.lib.pairs.first(hydra.lib.pairs.first(result())), hydra.lib.pairs.second(result())))
            def p_body() -> tuple[tuple[int, frozenlist[hydra.core.Binding]], hydra.core.Term]:
                return process_subterm(cast(tuple[int, frozenlist[hydra.core.Binding]], (counter, cast(frozenlist[hydra.core.Binding], ()))), body)
            def counter_extra1() -> tuple[int, frozenlist[hydra.core.Binding]]:
                return hydra.lib.pairs.first(p_body())
            def new_body() -> hydra.core.Type:
                return hydra.lib.pairs.second(p_body())
            def fold_binding(acc: tuple[tuple[int, frozenlist[hydra.core.Binding]], frozenlist[hydra.core.Binding]], binding: hydra.core.Binding) -> tuple[tuple[int, frozenlist[hydra.core.Binding]], frozenlist[hydra.core.Binding]]:
                def counter_extra() -> tuple[int, frozenlist[hydra.core.Binding]]:
                    return hydra.lib.pairs.first(acc)
                def new_bindings_so_far() -> frozenlist[hydra.core.Binding]:
                    return hydra.lib.pairs.second(acc)
                def p() -> tuple[tuple[int, frozenlist[hydra.core.Binding]], hydra.core.Term]:
                    return process_subterm(counter_extra(), binding.term)
                def new_counter_extra() -> tuple[int, frozenlist[hydra.core.Binding]]:
                    return hydra.lib.pairs.first(p())
                def new_term() -> hydra.core.Type:
                    return hydra.lib.pairs.second(p())
                def new_binding() -> hydra.core.Type:
                    return hydra.core.Binding(binding.name, new_term(), binding.type)
                return cast(tuple[tuple[int, frozenlist[hydra.core.Binding]], frozenlist[hydra.core.Binding]], (new_counter_extra(), hydra.lib.lists.concat2(new_bindings_so_far(), (new_binding(),))))
            def p_bindings() -> tuple[tuple[int, frozenlist[hydra.core.Binding]], frozenlist[hydra.core.Binding]]:
                return hydra.lib.lists.foldl(fold_binding, cast(tuple[tuple[int, frozenlist[hydra.core.Binding]], frozenlist[hydra.core.Binding]], (counter_extra1(), cast(frozenlist[hydra.core.Binding], ()))), bindings)
            def counter_extra2() -> tuple[int, frozenlist[hydra.core.Binding]]:
                return hydra.lib.pairs.first(p_bindings())
            def counter2() -> int:
                return hydra.lib.pairs.first(counter_extra2())
            def extra_bindings() -> frozenlist[hydra.core.Binding]:
                return hydra.lib.pairs.second(counter_extra2())
            def new_bindings() -> frozenlist[hydra.core.Binding]:
                return hydra.lib.pairs.second(p_bindings())
            def all_bindings() -> frozenlist[hydra.core.Binding]:
                return hydra.lib.lists.concat2(new_bindings(), extra_bindings())
            return cast(tuple[int, hydra.core.Term], (counter2(), cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(all_bindings(), new_body())))))
        match term:
            case hydra.core.TermLet(value=l):
                return for_let(l)
            
            case _:
                return dflt
    return hydra.lib.pairs.second(rewrite_and_fold_term_with_type_context(outer_rewrite, cx0, 1, term0))

def hoist_case_statements(v1: hydra.typing.TypeContext, v2: hydra.core.Term) -> hydra.core.Type:
    r"""Hoist case statements into bindings in the nearest enclosing let term. This is useful for targets such as Python which only support case statements (match) at the top level."""
    
    return hoist_subterms((lambda path, term: (is_union_elim := (lambda : hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lib.logic.if_else(hydra.lib.logic.not_(is_union_elim()), (lambda : False), (lambda : "let terms are not supported here")))[1]), v1, v2)

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
                
                case hydra.core.TermLet():
                    return False
                
                case _:
                    return True
        return hydra.lib.logic.and_(eager2, is_non_lambda_term())
    def reduce_arg(eager2: bool, arg: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
        return hydra.lib.logic.if_else(eager2, (lambda : hydra.lib.flows.pure(arg)), (lambda : reduce(False, arg)))
    def apply_to_arguments(fun: hydra.core.Term, args: frozenlist[hydra.core.Term]) -> hydra.core.Type:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(args), (lambda : fun), (lambda : apply_to_arguments(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun, hydra.lib.lists.head(args)))), hydra.lib.lists.tail(args))))
    def apply_elimination(elm: hydra.core.Elimination, reduced_arg: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
        match elm:
            case hydra.core.EliminationRecord(value=proj):
                return hydra.lib.flows.bind(hydra.extract.core.record(proj.type_name, hydra.rewriting.deannotate_term(reduced_arg)), (lambda fields: (matching_fields := (lambda : hydra.lib.lists.filter((lambda f: hydra.lib.equality.equal(f.name, proj.field)), fields)), hydra.lib.logic.if_else(hydra.lib.lists.null(matching_fields()), (lambda : hydra.lib.flows.fail(hydra.lib.strings.cat(("no such field: ", proj.field.value, " in ", proj.type_name.value, " record")))), (lambda : hydra.lib.flows.pure(hydra.lib.lists.head(matching_fields()).term))))[1]))
            
            case hydra.core.EliminationUnion(value=cs):
                return hydra.lib.flows.bind(hydra.extract.core.injection(cs.type_name, reduced_arg), (lambda field: (matching_fields := (lambda : hydra.lib.lists.filter((lambda f: hydra.lib.equality.equal(f.name, field.name)), cs.cases)), hydra.lib.logic.if_else(hydra.lib.lists.null(matching_fields()), (lambda : hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat(("no such field ", field.name.value, " in ", cs.type_name.value, " case statement"))), cast(Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]], (lambda x1: hydra.lib.flows.pure(x1))), cs.default)), (lambda : hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(hydra.lib.lists.head(matching_fields()).term, field.term)))))))[1]))
            
            case hydra.core.EliminationWrap(value=name):
                return hydra.extract.core.wrap(name, reduced_arg)
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def apply_if_nullary(eager2: bool, original: hydra.core.Term, args: frozenlist[hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
        stripped = hydra.rewriting.deannotate_term(original)
        def for_elimination(elm: hydra.core.Elimination, args2: frozenlist[hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
            def arg() -> hydra.core.Type:
                return hydra.lib.lists.head(args2)
            def remaining_args() -> frozenlist[hydra.core.Term]:
                return hydra.lib.lists.tail(args2)
            return hydra.lib.flows.bind(reduce_arg(eager2, hydra.rewriting.deannotate_term(arg())), (lambda reduced_arg: hydra.lib.flows.bind(hydra.lib.flows.bind(apply_elimination(elm, reduced_arg), (lambda v1: reduce(eager2, v1))), (lambda reduced_result: apply_if_nullary(eager2, reduced_result, remaining_args())))))
        def for_lambda(l: hydra.core.Lambda, args2: frozenlist[hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
            param = l.parameter
            body = l.body
            def arg() -> hydra.core.Type:
                return hydra.lib.lists.head(args2)
            def remaining_args() -> frozenlist[hydra.core.Term]:
                return hydra.lib.lists.tail(args2)
            return hydra.lib.flows.bind(reduce(eager2, hydra.rewriting.deannotate_term(arg())), (lambda reduced_arg: hydra.lib.flows.bind(reduce(eager2, hydra.rewriting.replace_free_term_variable(param, reduced_arg, body)), (lambda reduced_result: apply_if_nullary(eager2, reduced_result, remaining_args())))))
        def for_primitive(prim: hydra.graph.Primitive, arity: int, args2: frozenlist[hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
            def arg_list() -> frozenlist[hydra.core.Term]:
                return hydra.lib.lists.take(arity, args2)
            def remaining_args() -> frozenlist[hydra.core.Term]:
                return hydra.lib.lists.drop(arity, args2)
            return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: reduce_arg(eager2, v1)), arg_list()), (lambda reduced_args: (stripped_args := (lambda : hydra.lib.lists.map(hydra.rewriting.deannotate_term, reduced_args)), hydra.lib.flows.bind(hydra.lib.flows.bind(prim.implementation(stripped_args()), (lambda v1: reduce(eager2, v1))), (lambda reduced_result: apply_if_nullary(eager2, reduced_result, remaining_args()))))[1]))
        match stripped:
            case hydra.core.TermApplication(value=app):
                return apply_if_nullary(eager2, app.function, hydra.lib.lists.cons(app.argument, args))
            
            case hydra.core.TermFunction(value=v1):
                match v1:
                    case hydra.core.FunctionElimination(value=elm):
                        return hydra.lib.logic.if_else(hydra.lib.lists.null(args), (lambda : hydra.lib.flows.pure(original)), (lambda : for_elimination(elm, args)))
                    
                    case hydra.core.FunctionLambda(value=l):
                        return hydra.lib.logic.if_else(hydra.lib.lists.null(args), (lambda : hydra.lib.flows.pure(original)), (lambda : for_lambda(l, args)))
                    
                    case hydra.core.FunctionPrimitive(value=name):
                        return hydra.lib.flows.bind(hydra.lexical.require_primitive(name), (lambda prim: (arity := hydra.arity.primitive_arity(prim), hydra.lib.logic.if_else(hydra.lib.equality.gt(arity, hydra.lib.lists.length(args)), (lambda : hydra.lib.flows.pure(apply_to_arguments(original, args))), (lambda : for_primitive(prim, arity, args))))[1]))
                    
                    case _:
                        raise AssertionError("Unreachable: all variants handled")
            
            case hydra.core.TermVariable(value=v):
                return hydra.lib.flows.bind(hydra.lexical.dereference_element(v), (lambda m_binding: hydra.lib.maybes.maybe(hydra.lib.flows.pure(apply_to_arguments(original, args)), (lambda binding: apply_if_nullary(eager2, binding.term, args)), m_binding)))
            
            case hydra.core.TermLet(value=lt):
                bindings = lt.bindings
                body = lt.body
                def let_expr(b: hydra.core.Binding) -> hydra.core.Type:
                    return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let((b,), cast(hydra.core.Term, hydra.core.TermVariable(b.name)))))
                def expand_binding(b: hydra.core.Binding) -> hydra.core.Type:
                    return hydra.core.Binding(b.name, hydra.rewriting.replace_free_term_variable(b.name, let_expr(b), b.term), b.type)
                def expanded_bindings() -> frozenlist[hydra.core.Binding]:
                    return hydra.lib.lists.map(expand_binding, bindings)
                def substitute_binding(term2: hydra.core.Term, b: hydra.core.Binding) -> hydra.core.Type:
                    return hydra.rewriting.replace_free_term_variable(b.name, b.term, term2)
                def substitute_all(bs: frozenlist[hydra.core.Binding], term2: hydra.core.Term) -> hydra.core.Type:
                    return hydra.lib.lists.foldl(substitute_binding, term2, bs)
                def expanded_body() -> hydra.core.Type:
                    return substitute_all(expanded_bindings(), body)
                return hydra.lib.flows.bind(reduce(eager2, expanded_body()), (lambda reduced_body: apply_if_nullary(eager2, reduced_body, args)))
            
            case _:
                return hydra.lib.flows.pure(apply_to_arguments(original, args))
    def mapping(recurse: Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]], mid: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
        return hydra.lib.flows.bind(hydra.lib.logic.if_else(do_recurse(eager, mid), (lambda : recurse(mid)), (lambda : hydra.lib.flows.pure(mid))), (lambda inner: apply_if_nullary(eager, inner, cast(frozenlist[hydra.core.Term], ()))))
    return hydra.rewriting.rewrite_term_m(mapping, term)

def rewrite_and_fold_term_with_type_context_and_path(f: Callable[[
  Callable[[tuple[T0, tuple[T1, hydra.core.Term]]], T2],
  frozenlist[T0],
  hydra.typing.TypeContext,
  T1,
  hydra.core.Term], T2], cx0: hydra.typing.TypeContext, val0: T1, term0: hydra.core.Term) -> T2:
    def f2(recurse: Callable[[frozenlist[T0], hydra.typing.TypeContext, T1, hydra.core.Term], T2], path: frozenlist[T0], cx: hydra.typing.TypeContext, val: T1, term: hydra.core.Term) -> T2:
        def recurse1(accessor_val_term: tuple[T0, tuple[T1, hydra.core.Term]]) -> T2:
            def accessor() -> T0:
                return hydra.lib.pairs.first(accessor_val_term)
            def v() -> T1:
                return hydra.lib.pairs.first(hydra.lib.pairs.second(accessor_val_term))
            def t() -> hydra.core.Type:
                return hydra.lib.pairs.second(hydra.lib.pairs.second(accessor_val_term))
            def new_path() -> frozenlist[T0]:
                return hydra.lib.lists.concat2(path, (accessor(),))
            return recurse(new_path(), cx, v(), t())
        match term:
            case hydra.core.TermFunction(value=fun):
                match fun:
                    case hydra.core.FunctionLambda(value=l):
                        cx1 = hydra.schemas.extend_type_context_for_lambda(cx, l)
                        def recurse2(accessor_val_term: tuple[T0, tuple[T1, hydra.core.Term]]) -> T2:
                            def accessor() -> T0:
                                return hydra.lib.pairs.first(accessor_val_term)
                            def v() -> T1:
                                return hydra.lib.pairs.first(hydra.lib.pairs.second(accessor_val_term))
                            def t() -> hydra.core.Type:
                                return hydra.lib.pairs.second(hydra.lib.pairs.second(accessor_val_term))
                            def new_path() -> frozenlist[T0]:
                                return hydra.lib.lists.concat2(path, (accessor(),))
                            return recurse(new_path(), cx1, v(), t())
                        return f(recurse2, path, cx1, val, term)
                    
                    case _:
                        return f(recurse1, path, cx, val, term)
            
            case hydra.core.TermLet(value=l):
                def cx1() -> hydra.core.Type:
                    return hydra.schemas.extend_type_context_for_let((lambda _, _2: cast(Maybe[hydra.core.Term], Nothing())), cx, l)
                def recurse2(accessor_val_term: tuple[T0, tuple[T1, hydra.core.Term]]) -> T2:
                    def accessor() -> T0:
                        return hydra.lib.pairs.first(accessor_val_term)
                    def v() -> T1:
                        return hydra.lib.pairs.first(hydra.lib.pairs.second(accessor_val_term))
                    def t() -> hydra.core.Type:
                        return hydra.lib.pairs.second(hydra.lib.pairs.second(accessor_val_term))
                    def new_path() -> frozenlist[T0]:
                        return hydra.lib.lists.concat2(path, (accessor(),))
                    return recurse(new_path(), cx1(), v(), t())
                return f(recurse2, path, cx1(), val, term)
            
            case hydra.core.TermTypeLambda(value=tl):
                cx1 = hydra.schemas.extend_type_context_for_type_lambda(cx, tl)
                def recurse2(accessor_val_term: tuple[T0, tuple[T1, hydra.core.Term]]) -> T2:
                    def accessor() -> T0:
                        return hydra.lib.pairs.first(accessor_val_term)
                    def v() -> T1:
                        return hydra.lib.pairs.first(hydra.lib.pairs.second(accessor_val_term))
                    def t() -> hydra.core.Type:
                        return hydra.lib.pairs.second(hydra.lib.pairs.second(accessor_val_term))
                    def new_path() -> frozenlist[T0]:
                        return hydra.lib.lists.concat2(path, (accessor(),))
                    return recurse(new_path(), cx1, v(), t())
                return f(recurse2, path, cx1, val, term)
            
            case _:
                return f(recurse1, path, cx, val, term)
    def rewrite(path: frozenlist[T0], cx: hydra.typing.TypeContext, val: T1, term: hydra.core.Term) -> T2:
        return f2(rewrite, path, cx, val, term)
    return rewrite(cast(frozenlist[T0], ()), cx0, val0, term0)

def rewrite_term_with_type_context(f: Callable[[
  Callable[[hydra.core.Term], T0],
  hydra.typing.TypeContext,
  hydra.core.Term], T0], cx0: hydra.typing.TypeContext, term0: hydra.core.Term) -> T0:
    def f2(recurse: Callable[[hydra.typing.TypeContext, hydra.core.Term], T0], cx: hydra.typing.TypeContext, term: hydra.core.Term) -> T0:
        def recurse1(term2: hydra.core.Term) -> T0:
            return recurse(cx, term2)
        match term:
            case hydra.core.TermFunction(value=fun):
                match fun:
                    case hydra.core.FunctionLambda(value=l):
                        cx1 = hydra.schemas.extend_type_context_for_lambda(cx, l)
                        def recurse2(term2: hydra.core.Term) -> T0:
                            return recurse(cx1, term2)
                        return f(recurse2, cx1, term)
                    
                    case _:
                        return f(recurse1, cx, term)
            
            case hydra.core.TermLet(value=l):
                def cx1() -> hydra.core.Type:
                    return hydra.schemas.extend_type_context_for_let((lambda _, _2: cast(Maybe[hydra.core.Term], Nothing())), cx, l)
                def recurse2(term2: hydra.core.Term) -> T0:
                    return recurse(cx1(), term2)
                return f(recurse2, cx1(), term)
            
            case hydra.core.TermTypeLambda(value=tl):
                cx1 = hydra.schemas.extend_type_context_for_type_lambda(cx, tl)
                def recurse2(term2: hydra.core.Term) -> T0:
                    return recurse(cx1, term2)
                return f(recurse2, cx1, term)
            
            case _:
                return f(recurse1, cx, term)
    def rewrite(cx: hydra.typing.TypeContext, term: hydra.core.Term) -> T0:
        return f2(rewrite, cx, term)
    return rewrite(cx0, term0)

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
                        raise AssertionError("Unreachable: all variants handled")
            
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
