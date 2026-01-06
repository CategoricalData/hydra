# Note: this is an automatically generated file. Do not edit.

r"""Utilities for type and term rewriting and analysis."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.accessors
import hydra.coders
import hydra.compute
import hydra.core
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
import hydra.names
import hydra.sorting

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")
T4 = TypeVar("T4")
T5 = TypeVar("T5")
T6 = TypeVar("T6")
T7 = TypeVar("T7")
T8 = TypeVar("T8")
T9 = TypeVar("T9")

def deannotate_and_detype_term(t: hydra.core.Term) -> hydra.core.Type:
    r"""Strip type annotations from the top levels of a term."""
    
    match t:
        case hydra.core.TermAnnotated(value=at):
            return deannotate_and_detype_term(at.body)
        
        case hydra.core.TermTypeApplication(value=tt):
            return deannotate_and_detype_term(tt.body)
        
        case hydra.core.TermTypeLambda(value=ta):
            return deannotate_and_detype_term(ta.body)
        
        case _:
            return t

def deannotate_term(t: hydra.core.Term) -> hydra.core.Type:
    r"""Strip all annotations (including System F type annotations) from the top levels of a term."""
    
    match t:
        case hydra.core.TermAnnotated(value=at):
            return deannotate_term(at.body)
        
        case _:
            return t

def deannotate_type(t: hydra.core.Type) -> hydra.core.Type:
    r"""Strip all annotations from a term."""
    
    match t:
        case hydra.core.TypeAnnotated(value=arg_):
            return deannotate_type(arg_.body)
        
        case _:
            return t

def deannotate_type_parameters(t: hydra.core.Type) -> hydra.core.Type:
    r"""Strip any top-level type lambdas from a type, extracting the (possibly nested) type body."""
    
    match deannotate_type(t):
        case hydra.core.TypeForall(value=lt):
            return deannotate_type_parameters(lt.body)
        
        case _:
            return t

def rewrite_type(f: Callable[[Callable[[hydra.core.Type], hydra.core.Type], hydra.core.Type], hydra.core.Type], typ0: hydra.core.Type) -> hydra.core.Type:
    def fsub(recurse: Callable[[hydra.core.Type], hydra.core.Type], typ: hydra.core.Type) -> hydra.core.Type:
        def for_field(field: hydra.core.FieldType) -> hydra.core.Type:
            return hydra.core.FieldType(field.name, recurse(field.type))
        match typ:
            case hydra.core.TypeAnnotated(value=at):
                return cast(hydra.core.Type, hydra.core.TypeAnnotated(hydra.core.AnnotatedType(recurse(at.body), at.annotation)))
            
            case hydra.core.TypeApplication(value=app):
                return cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(recurse(app.function), recurse(app.argument))))
            
            case hydra.core.TypeEither(value=et):
                return cast(hydra.core.Type, hydra.core.TypeEither(hydra.core.EitherType(recurse(et.left), recurse(et.right))))
            
            case hydra.core.TypePair(value=pt):
                return cast(hydra.core.Type, hydra.core.TypePair(hydra.core.PairType(recurse(pt.first), recurse(pt.second))))
            
            case hydra.core.TypeFunction(value=fun):
                return cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(recurse(fun.domain), recurse(fun.codomain))))
            
            case hydra.core.TypeForall(value=lt):
                return cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(lt.parameter, recurse(lt.body))))
            
            case hydra.core.TypeList(value=t):
                return cast(hydra.core.Type, hydra.core.TypeList(recurse(t)))
            
            case hydra.core.TypeLiteral(value=lt2):
                return cast(hydra.core.Type, hydra.core.TypeLiteral(lt2))
            
            case hydra.core.TypeMap(value=mt):
                return cast(hydra.core.Type, hydra.core.TypeMap(hydra.core.MapType(recurse(mt.keys), recurse(mt.values))))
            
            case hydra.core.TypeMaybe(value=t2):
                return cast(hydra.core.Type, hydra.core.TypeMaybe(recurse(t2)))
            
            case hydra.core.TypeRecord(value=rt):
                return cast(hydra.core.Type, hydra.core.TypeRecord(hydra.core.RowType(rt.type_name, hydra.lib.lists.map(for_field, rt.fields))))
            
            case hydra.core.TypeSet(value=t3):
                return cast(hydra.core.Type, hydra.core.TypeSet(recurse(t3)))
            
            case hydra.core.TypeUnion(value=rt2):
                return cast(hydra.core.Type, hydra.core.TypeUnion(hydra.core.RowType(rt2.type_name, hydra.lib.lists.map(for_field, rt2.fields))))
            
            case hydra.core.TypeUnit():
                return cast(hydra.core.Type, hydra.core.TypeUnit())
            
            case hydra.core.TypeVariable(value=v):
                return cast(hydra.core.Type, hydra.core.TypeVariable(v))
            
            case hydra.core.TypeWrap(value=wt):
                return cast(hydra.core.Type, hydra.core.TypeWrap(hydra.core.WrappedType(wt.type_name, recurse(wt.body))))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def recurse(v1: hydra.core.Type) -> hydra.core.Type:
        return f((lambda v1: fsub(recurse, v1)), v1)
    return recurse(typ0)

def deannotate_type_recursive(typ: hydra.core.Type) -> hydra.core.Type:
    r"""Recursively strip all annotations from a type."""
    
    def strip(recurse: Callable[[T0], hydra.core.Type], typ2: T0) -> hydra.core.Type:
        rewritten = recurse(typ2)
        match rewritten:
            case hydra.core.TypeAnnotated(value=at):
                return at.body
            
            case _:
                return rewritten
    return rewrite_type(cast(Callable[[Callable[[hydra.core.Type], hydra.core.Type], hydra.core.Type], hydra.core.Type], (lambda x1, x2: strip(x1, x2))), typ)

def deannotate_type_scheme_recursive(ts: hydra.core.TypeScheme) -> hydra.core.Type:
    r"""Recursively strip all annotations from a type scheme."""
    
    vars = ts.variables
    typ = ts.type
    constraints = ts.constraints
    return hydra.core.TypeScheme(vars, deannotate_type_recursive(typ), constraints)

def detype_term(t: hydra.core.Term) -> hydra.core.Type:
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

def rewrite_term(f: Callable[[Callable[[hydra.core.Term], hydra.core.Term], hydra.core.Term], hydra.core.Term], term0: hydra.core.Term) -> hydra.core.Type:
    def fsub(recurse: Callable[[hydra.core.Term], hydra.core.Term], term: hydra.core.Term) -> hydra.core.Type:
        def for_field(f2: hydra.core.Field) -> hydra.core.Type:
            return hydra.core.Field(f2.name, recurse(f2.term))
        def for_elimination(elm: hydra.core.Elimination) -> hydra.core.Type:
            match elm:
                case hydra.core.EliminationRecord(value=p):
                    return cast(hydra.core.Elimination, hydra.core.EliminationRecord(p))
                
                case hydra.core.EliminationUnion(value=cs):
                    return cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(cs.type_name, hydra.lib.maybes.map(recurse, cs.default), hydra.lib.lists.map(for_field, cs.cases))))
                
                case hydra.core.EliminationWrap(value=name):
                    return cast(hydra.core.Elimination, hydra.core.EliminationWrap(name))
                
                case _:
                    raise AssertionError("Unreachable: all variants handled")
        def for_function(fun: hydra.core.Function) -> hydra.core.Type:
            match fun:
                case hydra.core.FunctionElimination(value=elm):
                    return cast(hydra.core.Function, hydra.core.FunctionElimination(for_elimination(elm)))
                
                case hydra.core.FunctionLambda(value=l):
                    return cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, l.domain, recurse(l.body))))
                
                case hydra.core.FunctionPrimitive(value=name):
                    return cast(hydra.core.Function, hydra.core.FunctionPrimitive(name))
                
                case _:
                    raise AssertionError("Unreachable: all variants handled")
        def for_let(lt: hydra.core.Let) -> hydra.core.Type:
            def map_binding(b: hydra.core.Binding) -> hydra.core.Type:
                return hydra.core.Binding(b.name, recurse(b.term), b.type)
            return hydra.core.Let(hydra.lib.lists.map(map_binding, lt.bindings), recurse(lt.body))
        def for_map(m: FrozenDict[hydra.core.Term, hydra.core.Term]) -> FrozenDict[hydra.core.Term, hydra.core.Term]:
            def for_pair(p: tuple[hydra.core.Term, hydra.core.Term]) -> tuple[hydra.core.Term, hydra.core.Term]:
                return cast(tuple[hydra.core.Term, hydra.core.Term], (recurse(hydra.lib.pairs.first(p)), recurse(hydra.lib.pairs.second(p))))
            return cast(FrozenDict[hydra.core.Term, hydra.core.Term], hydra.lib.maps.from_list(hydra.lib.lists.map(for_pair, hydra.lib.maps.to_list(m))))
        match term:
            case hydra.core.TermAnnotated(value=at):
                return cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(recurse(at.body), at.annotation)))
            
            case hydra.core.TermApplication(value=a):
                return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(recurse(a.function), recurse(a.argument))))
            
            case hydra.core.TermEither(value=e):
                return cast(hydra.core.Term, hydra.core.TermEither(hydra.lib.eithers.either((lambda l: cast(Either[hydra.core.Term, hydra.core.Term], Left(recurse(l)))), (lambda r: cast(Either[hydra.core.Term, hydra.core.Term], Right(recurse(r)))), e)))
            
            case hydra.core.TermFunction(value=fun):
                return cast(hydra.core.Term, hydra.core.TermFunction(for_function(fun)))
            
            case hydra.core.TermLet(value=lt):
                return cast(hydra.core.Term, hydra.core.TermLet(for_let(lt)))
            
            case hydra.core.TermList(value=els):
                return cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(recurse, els)))
            
            case hydra.core.TermLiteral(value=v):
                return cast(hydra.core.Term, hydra.core.TermLiteral(v))
            
            case hydra.core.TermMap(value=m):
                return cast(hydra.core.Term, hydra.core.TermMap(for_map(m)))
            
            case hydra.core.TermMaybe(value=m2):
                return cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(recurse, m2)))
            
            case hydra.core.TermPair(value=p):
                return cast(hydra.core.Term, hydra.core.TermPair(cast(tuple[hydra.core.Term, hydra.core.Term], (recurse(hydra.lib.pairs.first(p)), recurse(hydra.lib.pairs.second(p))))))
            
            case hydra.core.TermRecord(value=r):
                return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(r.type_name, hydra.lib.lists.map(for_field, r.fields))))
            
            case hydra.core.TermSet(value=s):
                return cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(hydra.lib.lists.map(recurse, hydra.lib.sets.to_list(s)))))
            
            case hydra.core.TermTypeApplication(value=tt):
                return cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(recurse(tt.body), tt.type)))
            
            case hydra.core.TermTypeLambda(value=ta):
                return cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(ta.parameter, recurse(ta.body))))
            
            case hydra.core.TermUnion(value=i):
                return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(i.type_name, for_field(i.field))))
            
            case hydra.core.TermUnit():
                return cast(hydra.core.Term, hydra.core.TermUnit())
            
            case hydra.core.TermVariable(value=v2):
                return cast(hydra.core.Term, hydra.core.TermVariable(v2))
            
            case hydra.core.TermWrap(value=wt):
                return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(wt.type_name, recurse(wt.body))))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def recurse(v1: hydra.core.Term) -> hydra.core.Type:
        return f((lambda v1: fsub(recurse, v1)), v1)
    return recurse(term0)

def substitute_variables(subst: FrozenDict[hydra.core.Name, hydra.core.Name], term: hydra.core.Term) -> hydra.core.Type:
    r"""Substitute multiple variables in a term."""
    
    def replace(recurse: Callable[[hydra.core.Term], hydra.core.Term], term2: hydra.core.Term) -> hydra.core.Type:
        def _hoist_replace_1(recurse: Callable[[T0], T0], term2: T0, v1: hydra.core.Function) -> T0:
            match v1:
                case hydra.core.FunctionLambda(value=l):
                    return hydra.lib.maybes.maybe(recurse(term2), (lambda _: term2), hydra.lib.maps.lookup(l.parameter, subst))
                
                case _:
                    return recurse(term2)
        match term2:
            case hydra.core.TermVariable(value=n):
                return cast(hydra.core.Term, hydra.core.TermVariable(hydra.lib.maybes.from_maybe(n, hydra.lib.maps.lookup(n, subst))))
            
            case hydra.core.TermFunction(value=v1):
                return _hoist_replace_1(recurse, term2, v1)
            
            case _:
                return recurse(term2)
    return rewrite_term(replace, term)

def flatten_let_terms(term: hydra.core.Term) -> hydra.core.Type:
    r"""Flatten nested let expressions."""
    
    def rewrite_binding(binding: hydra.core.Binding) -> tuple[hydra.core.Binding, frozenlist[hydra.core.Binding]]:
        key0 = binding.name
        val0 = binding.term
        t = binding.type
        match val0:
            case hydra.core.TermAnnotated(value=at):
                val1 = at.body
                ann = at.annotation
                def recursive() -> tuple[hydra.core.Binding, frozenlist[hydra.core.Binding]]:
                    return rewrite_binding(hydra.core.Binding(key0, val1, t))
                def inner_binding() -> hydra.core.Type:
                    return hydra.lib.pairs.first(recursive())
                def deps() -> frozenlist[hydra.core.Binding]:
                    return hydra.lib.pairs.second(recursive())
                def val2() -> hydra.core.Type:
                    return inner_binding().term
                return cast(tuple[hydra.core.Binding, frozenlist[hydra.core.Binding]], (hydra.core.Binding(key0, cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(val2(), ann))), t), deps()))
            
            case hydra.core.TermLet(value=inner_let):
                bindings1 = inner_let.bindings
                body1 = inner_let.body
                prefix = hydra.lib.strings.cat2(key0.value, "_")
                def qualify(n: hydra.core.Name) -> hydra.core.Type:
                    return hydra.core.Name(hydra.lib.strings.cat2(prefix, n.value))
                def to_subst_pair(b: hydra.core.Binding) -> tuple[hydra.core.Name, hydra.core.Name]:
                    return cast(tuple[hydra.core.Name, hydra.core.Name], (b.name, qualify(b.name)))
                def subst() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
                    return cast(FrozenDict[hydra.core.Name, hydra.core.Name], hydra.lib.maps.from_list(hydra.lib.lists.map(to_subst_pair, bindings1)))
                def replace_vars(v1: hydra.core.Term) -> hydra.core.Type:
                    return substitute_variables(subst(), v1)
                def new_body() -> hydra.core.Type:
                    return replace_vars(body1)
                def new_binding(b: hydra.core.Binding) -> hydra.core.Type:
                    return hydra.core.Binding(qualify(b.name), replace_vars(b.term), b.type)
                return cast(tuple[hydra.core.Binding, frozenlist[hydra.core.Binding]], (hydra.core.Binding(key0, new_body(), t), hydra.lib.lists.map(new_binding, bindings1)))
            
            case _:
                return cast(tuple[hydra.core.Binding, frozenlist[hydra.core.Binding]], (hydra.core.Binding(key0, val0, t), cast(frozenlist[hydra.core.Binding], ())))
    def flatten(recurse: Callable[[T0], hydra.core.Term], term2: T0) -> hydra.core.Type:
        rewritten = recurse(term2)
        match rewritten:
            case hydra.core.TermLet(value=lt):
                bindings = lt.bindings
                body = lt.body
                def for_result(hr: tuple[T1, frozenlist[T1]]) -> frozenlist[T1]:
                    return hydra.lib.lists.cons(hydra.lib.pairs.first(hr), hydra.lib.pairs.second(hr))
                def new_bindings() -> frozenlist[hydra.core.Binding]:
                    return hydra.lib.lists.concat(hydra.lib.lists.map((lambda arg_: for_result(rewrite_binding(arg_))), bindings))
                return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(new_bindings(), body)))
            
            case _:
                return rewritten
    return rewrite_term(cast(Callable[[Callable[[hydra.core.Term], hydra.core.Term], hydra.core.Term], hydra.core.Term], (lambda x1, x2: flatten(x1, x2))), term)

def subterms(v1: hydra.core.Term) -> frozenlist[hydra.core.Term]:
    r"""Find the children of a given term."""
    
    match v1:
        case hydra.core.TermAnnotated(value=at):
            return (at.body,)
        
        case hydra.core.TermApplication(value=p):
            return (p.function, p.argument)
        
        case hydra.core.TermEither(value=e):
            return hydra.lib.eithers.either((lambda l: (l,)), (lambda r: (r,)), e)
        
        case hydra.core.TermFunction(value=v1):
            match v1:
                case hydra.core.FunctionElimination(value=v1):
                    match v1:
                        case hydra.core.EliminationUnion(value=cs):
                            return hydra.lib.lists.concat2(hydra.lib.maybes.maybe(cast(frozenlist[hydra.core.Term], ()), (lambda t: (t,)), cs.default), hydra.lib.lists.map((lambda v1: v1.term), cs.cases))
                        
                        case _:
                            return cast(frozenlist[hydra.core.Term], ())
                
                case hydra.core.FunctionLambda(value=l):
                    return (l.body,)
                
                case _:
                    return cast(frozenlist[hydra.core.Term], ())
        
        case hydra.core.TermLet(value=lt):
            return hydra.lib.lists.cons(lt.body, hydra.lib.lists.map((lambda v1: v1.term), lt.bindings))
        
        case hydra.core.TermList(value=l):
            return l
        
        case hydra.core.TermLiteral():
            return cast(frozenlist[hydra.core.Term], ())
        
        case hydra.core.TermMap(value=m):
            return hydra.lib.lists.concat(hydra.lib.lists.map((lambda p: (hydra.lib.pairs.first(p), hydra.lib.pairs.second(p))), hydra.lib.maps.to_list(m)))
        
        case hydra.core.TermMaybe(value=m2):
            return hydra.lib.maybes.maybe(cast(frozenlist[hydra.core.Term], ()), (lambda t: (t,)), m2)
        
        case hydra.core.TermPair(value=p2):
            return (hydra.lib.pairs.first(p2), hydra.lib.pairs.second(p2))
        
        case hydra.core.TermRecord(value=rt):
            return hydra.lib.lists.map((lambda v1: v1.term), rt.fields)
        
        case hydra.core.TermSet(value=l2):
            return hydra.lib.sets.to_list(l2)
        
        case hydra.core.TermTypeApplication(value=ta):
            return (ta.body,)
        
        case hydra.core.TermTypeLambda(value=ta2):
            return (ta2.body,)
        
        case hydra.core.TermUnion(value=ut):
            return (ut.field.term,)
        
        case hydra.core.TermUnit():
            return cast(frozenlist[hydra.core.Term], ())
        
        case hydra.core.TermVariable():
            return cast(frozenlist[hydra.core.Term], ())
        
        case hydra.core.TermWrap(value=n):
            return (n.body,)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def fold_over_term(order: hydra.coders.TraversalOrder, fld: Callable[[T0, hydra.core.Term], T0], b0: T0, term: hydra.core.Term) -> T0:
    match order:
        case hydra.coders.TraversalOrder.PRE:
            return hydra.lib.lists.foldl((lambda v1, v2: fold_over_term(order, fld, v1, v2)), fld(b0, term), subterms(term))
        
        case hydra.coders.TraversalOrder.POST:
            return fld(hydra.lib.lists.foldl((lambda v1, v2: fold_over_term(order, fld, v1, v2)), b0, subterms(term)), term)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def subtypes(v1: hydra.core.Type) -> frozenlist[hydra.core.Type]:
    r"""Find the children of a given type expression."""
    
    match v1:
        case hydra.core.TypeAnnotated(value=at):
            return (at.body,)
        
        case hydra.core.TypeApplication(value=at2):
            return (at2.function, at2.argument)
        
        case hydra.core.TypeEither(value=et):
            return (et.left, et.right)
        
        case hydra.core.TypePair(value=pt):
            return (pt.first, pt.second)
        
        case hydra.core.TypeFunction(value=ft):
            return (ft.domain, ft.codomain)
        
        case hydra.core.TypeForall(value=lt):
            return (lt.body,)
        
        case hydra.core.TypeList(value=lt2):
            return (lt2,)
        
        case hydra.core.TypeLiteral():
            return cast(frozenlist[hydra.core.Type], ())
        
        case hydra.core.TypeMap(value=mt):
            return (mt.keys, mt.values)
        
        case hydra.core.TypeMaybe(value=ot):
            return (ot,)
        
        case hydra.core.TypeRecord(value=rt):
            return hydra.lib.lists.map((lambda v1: v1.type), rt.fields)
        
        case hydra.core.TypeSet(value=st):
            return (st,)
        
        case hydra.core.TypeUnion(value=rt2):
            return hydra.lib.lists.map((lambda v1: v1.type), rt2.fields)
        
        case hydra.core.TypeUnit():
            return cast(frozenlist[hydra.core.Type], ())
        
        case hydra.core.TypeVariable():
            return cast(frozenlist[hydra.core.Type], ())
        
        case hydra.core.TypeWrap(value=nt):
            return (nt.body,)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def fold_over_type(order: hydra.coders.TraversalOrder, fld: Callable[[T0, hydra.core.Type], T0], b0: T0, typ: hydra.core.Type) -> T0:
    match order:
        case hydra.coders.TraversalOrder.PRE:
            return hydra.lib.lists.foldl((lambda v1, v2: fold_over_type(order, fld, v1, v2)), fld(b0, typ), subtypes(typ))
        
        case hydra.coders.TraversalOrder.POST:
            return fld(hydra.lib.lists.foldl((lambda v1, v2: fold_over_type(order, fld, v1, v2)), b0, subtypes(typ)), typ)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def free_variables_in_type(typ: hydra.core.Type) -> frozenset[hydra.core.Name]:
    r"""Find the free variables (i.e. variables not bound by a lambda or let) in a type."""
    
    def dflt_vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.lists.foldl((lambda s, t: hydra.lib.sets.union(s, free_variables_in_type(t))), cast(frozenset[hydra.core.Name], hydra.lib.sets.empty()), subtypes(typ))
    match typ:
        case hydra.core.TypeForall(value=lt):
            return hydra.lib.sets.delete(lt.parameter, free_variables_in_type(lt.body))
        
        case hydra.core.TypeVariable(value=v):
            return hydra.lib.sets.singleton(v)
        
        case _:
            return dflt_vars()

def free_type_variables_in_term(term0: hydra.core.Term) -> frozenset[hydra.core.Name]:
    r"""Get the set of free type variables in a term (including schema names, where they appear in type annotations). In this context, only the type schemes of let bindings can bind type variables; type lambdas do not."""
    
    def all_of(sets: frozenlist[frozenset[T0]]) -> frozenset[T0]:
        return hydra.lib.lists.foldl(cast(Callable[[frozenset[T0], frozenset[T0]], frozenset[T0]], (lambda x1, x2: hydra.lib.sets.union(x1, x2))), cast(frozenset[T0], hydra.lib.sets.empty()), sets)
    def try_type(tvars: frozenset[hydra.core.Name], typ: hydra.core.Type) -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.difference(free_variables_in_type(typ), tvars)
    def get_all(vars: frozenset[hydra.core.Name], term: hydra.core.Term) -> frozenset[hydra.core.Name]:
        def recurse(v1: hydra.core.Term) -> frozenset[hydra.core.Name]:
            return get_all(vars, v1)
        def dflt() -> frozenset[hydra.core.Name]:
            return all_of(hydra.lib.lists.map(recurse, subterms(term)))
        def _hoist_body_1(v1: hydra.core.Function) -> frozenset[hydra.core.Name]:
            match v1:
                case hydra.core.FunctionElimination():
                    return dflt()
                
                case hydra.core.FunctionLambda(value=l):
                    def domt() -> frozenset[hydra.core.Name]:
                        return hydra.lib.maybes.maybe(cast(frozenset[hydra.core.Name], hydra.lib.sets.empty()), (lambda v1: try_type(vars, v1)), l.domain)
                    return hydra.lib.sets.union(domt(), recurse(l.body))
                
                case _:
                    return dflt()
        match term:
            case hydra.core.TermFunction(value=f):
                return _hoist_body_1(f)
            
            case hydra.core.TermLet(value=l):
                def for_binding(b: hydra.core.Binding) -> frozenset[hydra.core.Name]:
                    def new_vars() -> frozenset[hydra.core.Name]:
                        return hydra.lib.maybes.maybe(vars, (lambda ts: hydra.lib.sets.union(vars, hydra.lib.sets.from_list(ts.variables))), b.type)
                    return hydra.lib.sets.union(get_all(new_vars(), b.term), hydra.lib.maybes.maybe(cast(frozenset[hydra.core.Name], hydra.lib.sets.empty()), (lambda ts: try_type(new_vars(), ts.type)), b.type))
                return hydra.lib.sets.union(all_of(hydra.lib.lists.map(for_binding, l.bindings)), recurse(l.body))
            
            case hydra.core.TermTypeApplication(value=tt):
                return hydra.lib.sets.union(try_type(vars, tt.type), recurse(tt.body))
            
            case hydra.core.TermTypeLambda(value=tl):
                return hydra.lib.sets.union(try_type(vars, cast(hydra.core.Type, hydra.core.TypeVariable(tl.parameter))), recurse(tl.body))
            
            case _:
                return dflt()
    return get_all(cast(frozenset[hydra.core.Name], hydra.lib.sets.empty()), term0)

def free_variables_in_term(term: hydra.core.Term) -> frozenset[hydra.core.Name]:
    r"""Find the free variables (i.e. variables not bound by a lambda or let) in a term."""
    
    def dflt_vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.lists.foldl((lambda s, t: hydra.lib.sets.union(s, free_variables_in_term(t))), cast(frozenset[hydra.core.Name], hydra.lib.sets.empty()), subterms(term))
    def _hoist_body_1(v1: hydra.core.Function) -> frozenset[hydra.core.Name]:
        match v1:
            case hydra.core.FunctionLambda(value=l):
                return hydra.lib.sets.delete(l.parameter, free_variables_in_term(l.body))
            
            case _:
                return dflt_vars()
    match term:
        case hydra.core.TermFunction(value=v1):
            return _hoist_body_1(v1)
        
        case hydra.core.TermLet(value=l):
            return hydra.lib.sets.difference(dflt_vars(), hydra.lib.sets.from_list(hydra.lib.lists.map((lambda v1: v1.name), l.bindings)))
        
        case hydra.core.TermVariable(value=v):
            return hydra.lib.sets.singleton(v)
        
        case _:
            return dflt_vars()

def free_variables_in_type_ordered(typ: hydra.core.Type) -> frozenlist[hydra.core.Name]:
    r"""Find the free variables in a type in deterministic left-to-right order."""
    
    def collect_vars(bound_vars: frozenset[hydra.core.Name], t: hydra.core.Type) -> frozenlist[hydra.core.Name]:
        match t:
            case hydra.core.TypeVariable(value=v):
                return hydra.lib.logic.if_else(hydra.lib.sets.member(v, bound_vars), (lambda : cast(frozenlist[hydra.core.Name], ())), (lambda : (v,)))
            
            case hydra.core.TypeForall(value=ft):
                return collect_vars(hydra.lib.sets.insert(ft.parameter, bound_vars), ft.body)
            
            case _:
                return hydra.lib.lists.concat(hydra.lib.lists.map((lambda v1: collect_vars(bound_vars, v1)), subtypes(t)))
    return hydra.lib.lists.nub(collect_vars(cast(frozenset[hydra.core.Name], hydra.lib.sets.empty()), typ))

def free_variables_in_type_scheme(ts: hydra.core.TypeScheme) -> frozenset[hydra.core.Name]:
    r"""Find free variables in a type scheme."""
    
    vars = ts.variables
    t = ts.type
    return hydra.lib.sets.difference(free_variables_in_type(t), hydra.lib.sets.from_list(vars))

def free_variables_in_type_simple(typ: hydra.core.Type) -> frozenset[hydra.core.Name]:
    r"""Same as freeVariablesInType, but ignores the binding action of lambda types."""
    
    def helper(types: frozenset[hydra.core.Name], typ2: hydra.core.Type) -> frozenset[hydra.core.Name]:
        match typ2:
            case hydra.core.TypeVariable(value=v):
                return hydra.lib.sets.insert(v, types)
            
            case _:
                return types
    return fold_over_type(hydra.coders.TraversalOrder.PRE, helper, cast(frozenset[hydra.core.Name], hydra.lib.sets.empty()), typ)

def free_variables_in_type_scheme_simple(ts: hydra.core.TypeScheme) -> frozenset[hydra.core.Name]:
    r"""Find free variables in a type scheme (simple version)."""
    
    vars = ts.variables
    t = ts.type
    return hydra.lib.sets.difference(free_variables_in_type_simple(t), hydra.lib.sets.from_list(vars))

def rewrite_type_m(f: Callable[[
  Callable[[hydra.core.Type], hydra.compute.Flow[T0, hydra.core.Type]],
  hydra.core.Type], hydra.compute.Flow[T0, hydra.core.Type]], typ0: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.core.Type]:
    def fsub(recurse: Callable[[hydra.core.Type], hydra.compute.Flow[T1, hydra.core.Type]], typ: hydra.core.Type) -> hydra.compute.Flow[T1, hydra.core.Type]:
        match typ:
            case hydra.core.TypeAnnotated(value=at):
                return hydra.lib.flows.bind(recurse(at.body), (lambda t: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeAnnotated(hydra.core.AnnotatedType(t, at.annotation))))))
            
            case hydra.core.TypeApplication(value=at2):
                return hydra.lib.flows.bind(recurse(at2.function), (lambda lhs: hydra.lib.flows.bind(recurse(at2.argument), (lambda rhs: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(lhs, rhs))))))))
            
            case hydra.core.TypeEither(value=et):
                return hydra.lib.flows.bind(recurse(et.left), (lambda left: hydra.lib.flows.bind(recurse(et.right), (lambda right: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeEither(hydra.core.EitherType(left, right))))))))
            
            case hydra.core.TypePair(value=pt):
                return hydra.lib.flows.bind(recurse(pt.first), (lambda pair_first: hydra.lib.flows.bind(recurse(pt.second), (lambda pair_second: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypePair(hydra.core.PairType(pair_first, pair_second))))))))
            
            case hydra.core.TypeFunction(value=ft):
                return hydra.lib.flows.bind(recurse(ft.domain), (lambda dom: hydra.lib.flows.bind(recurse(ft.codomain), (lambda cod: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(dom, cod))))))))
            
            case hydra.core.TypeForall(value=ft2):
                return hydra.lib.flows.bind(recurse(ft2.body), (lambda b: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(ft2.parameter, b))))))
            
            case hydra.core.TypeList(value=t):
                return hydra.lib.flows.bind(recurse(t), (lambda rt: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeList(rt)))))
            
            case hydra.core.TypeLiteral(value=lt):
                return hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeLiteral(lt)))
            
            case hydra.core.TypeMap(value=mt):
                return hydra.lib.flows.bind(recurse(mt.keys), (lambda kt: hydra.lib.flows.bind(recurse(mt.values), (lambda vt: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeMap(hydra.core.MapType(kt, vt))))))))
            
            case hydra.core.TypeMaybe(value=t2):
                return hydra.lib.flows.bind(recurse(t2), (lambda rt: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeMaybe(rt)))))
            
            case hydra.core.TypeRecord(value=rt):
                name = rt.type_name
                fields = rt.fields
                def for_field(f2: hydra.core.FieldType) -> hydra.compute.Flow[T1, hydra.core.FieldType]:
                    return hydra.lib.flows.bind(recurse(f2.type), (lambda t: hydra.lib.flows.pure(hydra.core.FieldType(f2.name, t))))
                return hydra.lib.flows.bind(hydra.lib.flows.map_list(for_field, fields), (lambda rfields: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeRecord(hydra.core.RowType(name, rfields))))))
            
            case hydra.core.TypeSet(value=t3):
                return hydra.lib.flows.bind(recurse(t3), (lambda rt: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeSet(rt)))))
            
            case hydra.core.TypeUnion(value=rt2):
                name = rt2.type_name
                fields = rt2.fields
                def for_field(f2: hydra.core.FieldType) -> hydra.compute.Flow[T1, hydra.core.FieldType]:
                    return hydra.lib.flows.bind(recurse(f2.type), (lambda t: hydra.lib.flows.pure(hydra.core.FieldType(f2.name, t))))
                return hydra.lib.flows.bind(hydra.lib.flows.map_list(for_field, fields), (lambda rfields: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeUnion(hydra.core.RowType(name, rfields))))))
            
            case hydra.core.TypeUnit():
                return hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeUnit()))
            
            case hydra.core.TypeVariable(value=v):
                return hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeVariable(v)))
            
            case hydra.core.TypeWrap(value=wt):
                return hydra.lib.flows.bind(recurse(wt.body), (lambda t: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeWrap(hydra.core.WrappedType(wt.type_name, t))))))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def recurse(v1: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.core.Type]:
        return f((lambda v1: fsub(recurse, v1)), v1)
    return recurse(typ0)

def inline_type(schema: FrozenDict[hydra.core.Name, hydra.core.Type], typ: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.core.Type]:
    def f(recurse: Callable[[T1], hydra.compute.Flow[T0, hydra.core.Type]], typ2: T1) -> hydra.compute.Flow[T0, hydra.core.Type]:
        def after_recurse(tr: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.core.Type]:
            match tr:
                case hydra.core.TypeVariable(value=v):
                    return hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat2("No such type in schema: ", v.value)), (lambda v1: inline_type(schema, v1)), hydra.lib.maps.lookup(v, schema))
                
                case _:
                    return hydra.lib.flows.pure(tr)
        return hydra.lib.flows.bind(recurse(typ2), (lambda tr: after_recurse(tr)))
    return rewrite_type_m(cast(Callable[[
      Callable[[hydra.core.Type], hydra.compute.Flow[T0, hydra.core.Type]],
      hydra.core.Type], hydra.compute.Flow[T0, hydra.core.Type]], (lambda x1, x2: f(x1, x2))), typ)

def is_free_variable_in_term(v: hydra.core.Name, term: hydra.core.Term) -> bool:
    r"""Check whether a variable is free (not bound) in a term."""
    
    return hydra.lib.logic.not_(hydra.lib.sets.member(v, free_variables_in_term(term)))

def is_lambda(term: hydra.core.Term) -> bool:
    def _hoist_hydra_rewriting_is_lambda_1(v1: hydra.core.Function) -> bool:
        match v1:
            case hydra.core.FunctionLambda():
                return True
            
            case _:
                return False
    match deannotate_term(term):
        case hydra.core.TermFunction(value=v1):
            return _hoist_hydra_rewriting_is_lambda_1(v1)
        
        case hydra.core.TermLet(value=lt):
            return is_lambda(lt.body)
        
        case _:
            return False

def lift_lambda_above_let(term0: hydra.core.Term) -> hydra.core.Type:
    r"""Rewrite terms like `let foo = bar in λx.baz` to `λx.let foo = bar in baz`, lifting lambda-bound variables above let-bound variables, recursively. This is helpful for targets such as Python."""
    
    def rewrite(recurse: Callable[[hydra.core.Term], hydra.core.Term], term: hydra.core.Term) -> hydra.core.Type:
        def rewrite_binding(b: hydra.core.Binding) -> hydra.core.Type:
            return hydra.core.Binding(b.name, rewrite(recurse, b.term), b.type)
        def rewrite_bindings(bs: frozenlist[hydra.core.Binding]) -> frozenlist[hydra.core.Binding]:
            return hydra.lib.lists.map(rewrite_binding, bs)
        def dig_for_lambdas(original: hydra.core.Term, cons: Callable[[hydra.core.Term], hydra.core.Term], term2: hydra.core.Term) -> hydra.core.Type:
            def _hoist_dig_for_lambdas_1(cons: Callable[[hydra.core.Term], hydra.core.Term], original: hydra.core.Term, v1: hydra.core.Function) -> hydra.core.Type:
                match v1:
                    case hydra.core.FunctionLambda(value=l):
                        return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, l.domain, dig_for_lambdas(cons(l.body), (lambda t: cons(t)), l.body))))))
                    
                    case _:
                        return recurse(original)
            match term2:
                case hydra.core.TermAnnotated(value=at):
                    return dig_for_lambdas(original, (lambda t: cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cons(t), at.annotation)))), at.body)
                
                case hydra.core.TermFunction(value=f):
                    return _hoist_dig_for_lambdas_1(cons, original, f)
                
                case hydra.core.TermLet(value=l):
                    return dig_for_lambdas(original, (lambda t: cons(cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(rewrite_bindings(l.bindings), t))))), l.body)
                
                case _:
                    return recurse(original)
        match term:
            case hydra.core.TermLet(value=l):
                return dig_for_lambdas(term, (lambda t: cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(rewrite_bindings(l.bindings), t)))), l.body)
            
            case _:
                return recurse(term)
    return rewrite_term(rewrite, term0)

def map_beneath_type_annotations(f: Callable[[hydra.core.Type], hydra.core.Type], t: hydra.core.Type) -> hydra.core.Type:
    r"""Apply a transformation to the first type beneath a chain of annotations."""
    
    match t:
        case hydra.core.TypeAnnotated(value=at):
            return cast(hydra.core.Type, hydra.core.TypeAnnotated(hydra.core.AnnotatedType(map_beneath_type_annotations(f, at.body), at.annotation)))
        
        case _:
            return f(t)

def normalize_type_variables_in_term(term: hydra.core.Term) -> hydra.core.Type:
    r"""Recursively replace the type variables of let bindings with the systematic type variables t0, t1, t2, ..."""
    
    def replace_name(subst: FrozenDict[T0, T0], v: T0) -> T0:
        return hydra.lib.maybes.from_maybe(v, hydra.lib.maps.lookup(v, subst))
    def subst_type(subst: FrozenDict[hydra.core.Name, hydra.core.Name], typ: hydra.core.Type) -> hydra.core.Type:
        def rewrite(recurse: Callable[[hydra.core.Type], hydra.core.Type], typ2: hydra.core.Type) -> hydra.core.Type:
            match typ2:
                case hydra.core.TypeVariable(value=v):
                    return cast(hydra.core.Type, hydra.core.TypeVariable(replace_name(subst, v)))
                
                case _:
                    return recurse(typ2)
        return rewrite_type(rewrite, typ)
    def rewrite_with_subst(state: tuple[tuple[FrozenDict[hydra.core.Name, hydra.core.Name], frozenset[hydra.core.Name]], int], term0: hydra.core.Term) -> hydra.core.Type:
        def sb() -> tuple[FrozenDict[hydra.core.Name, hydra.core.Name], frozenset[hydra.core.Name]]:
            return hydra.lib.pairs.first(state)
        def next() -> int:
            return hydra.lib.pairs.second(state)
        def subst() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
            return hydra.lib.pairs.first(sb())
        def bound_vars() -> frozenset[hydra.core.Name]:
            return hydra.lib.pairs.second(sb())
        def rewrite(recurse: Callable[[hydra.core.Term], hydra.core.Term], term2: hydra.core.Term) -> hydra.core.Type:
            def _hoist_rewrite_1(recurse: Callable[[T0], hydra.core.Term], term2: T0, v1: hydra.core.Function) -> hydra.core.Type:
                match v1:
                    case hydra.core.FunctionElimination():
                        return recurse(term2)
                    
                    case hydra.core.FunctionLambda(value=l):
                        domain = l.domain
                        return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, hydra.lib.maybes.map((lambda v1: subst_type(subst(), v1)), domain), rewrite_with_subst(cast(tuple[tuple[FrozenDict[hydra.core.Name, hydra.core.Name], frozenset[hydra.core.Name]], int], (cast(tuple[FrozenDict[hydra.core.Name, hydra.core.Name], frozenset[hydra.core.Name]], (subst(), bound_vars())), next())), l.body))))))
                    
                    case _:
                        return recurse(term2)
            match term2:
                case hydra.core.TermFunction(value=v1):
                    return _hoist_rewrite_1(recurse, term2, v1)
                
                case hydra.core.TermLet(value=lt):
                    bindings0 = lt.bindings
                    body0 = lt.body
                    def step(acc: frozenlist[hydra.core.Binding], bs: frozenlist[hydra.core.Binding]) -> frozenlist[hydra.core.Binding]:
                        def b() -> hydra.core.Type:
                            return hydra.lib.lists.head(bs)
                        def tl() -> frozenlist[hydra.core.Binding]:
                            return hydra.lib.lists.tail(bs)
                        def no_type() -> frozenlist[hydra.core.Binding]:
                            def new_val() -> hydra.core.Type:
                                return rewrite_with_subst(cast(tuple[tuple[FrozenDict[hydra.core.Name, hydra.core.Name], frozenset[hydra.core.Name]], int], (cast(tuple[FrozenDict[hydra.core.Name, hydra.core.Name], frozenset[hydra.core.Name]], (subst(), bound_vars())), next())), b().term)
                            def b1() -> hydra.core.Type:
                                return hydra.core.Binding(b().name, new_val(), cast(Maybe[hydra.core.TypeScheme], Nothing()))
                            return step(hydra.lib.lists.cons(b1(), acc), tl())
                        def with_type(ts: hydra.core.TypeScheme) -> frozenlist[hydra.core.Binding]:
                            vars = ts.variables
                            typ = ts.type
                            def k() -> int:
                                return hydra.lib.lists.length(vars)
                            def gen(i: int, rem: int, acc2: frozenlist[hydra.core.Name]) -> frozenlist[hydra.core.Name]:
                                def ti() -> hydra.core.Type:
                                    return hydra.core.Name(hydra.lib.strings.cat2("t", hydra.lib.literals.show_int32(hydra.lib.math.add(next(), i))))
                                return hydra.lib.logic.if_else(hydra.lib.equality.equal(rem, 0), (lambda : hydra.lib.lists.reverse(acc2)), (lambda : gen(hydra.lib.math.add(i, 1), hydra.lib.math.sub(rem, 1), hydra.lib.lists.cons(ti(), acc2))))
                            def new_vars() -> frozenlist[hydra.core.Name]:
                                return gen(0, k(), cast(frozenlist[hydra.core.Name], ()))
                            def new_subst() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
                                return hydra.lib.maps.union(cast(FrozenDict[hydra.core.Name, hydra.core.Name], hydra.lib.maps.from_list(hydra.lib.lists.zip(vars, new_vars()))), subst())
                            def new_bound() -> frozenset[hydra.core.Name]:
                                return hydra.lib.sets.union(bound_vars(), hydra.lib.sets.from_list(new_vars()))
                            def new_val() -> hydra.core.Type:
                                return rewrite_with_subst(cast(tuple[tuple[FrozenDict[hydra.core.Name, hydra.core.Name], frozenset[hydra.core.Name]], int], (cast(tuple[FrozenDict[hydra.core.Name, hydra.core.Name], frozenset[hydra.core.Name]], (new_subst(), new_bound())), hydra.lib.math.add(next(), k()))), b().term)
                            def rename_constraint_keys(constraint_map: FrozenDict[hydra.core.Name, T0]) -> FrozenDict[hydra.core.Name, T0]:
                                return cast(FrozenDict[hydra.core.Name, T0], hydra.lib.maps.from_list(hydra.lib.lists.map((lambda p: (old_name := (lambda : hydra.lib.pairs.first(p)), meta := (lambda : hydra.lib.pairs.second(p)), new_name := (lambda : hydra.lib.maybes.from_maybe(old_name(), hydra.lib.maps.lookup(old_name(), new_subst()))), cast(tuple[hydra.core.Name, T0], (new_name(), meta())))[3]), hydra.lib.maps.to_list(constraint_map))))
                            old_constraints = ts.constraints
                            def new_constraints() -> Maybe[FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]]:
                                return hydra.lib.maybes.map(cast(Callable[[FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]], FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]], (lambda x1: rename_constraint_keys(x1))), old_constraints)
                            def b1() -> hydra.core.Type:
                                return hydra.core.Binding(b().name, new_val(), cast(Maybe[hydra.core.TypeScheme], Just(hydra.core.TypeScheme(new_vars(), subst_type(new_subst(), typ), new_constraints()))))
                            return step(hydra.lib.lists.cons(b1(), acc), tl())
                        return hydra.lib.logic.if_else(hydra.lib.lists.null(bs), (lambda : hydra.lib.lists.reverse(acc)), (lambda : hydra.lib.maybes.maybe(no_type(), (lambda ts: with_type(ts)), b().type)))
                    def bindings1() -> frozenlist[hydra.core.Binding]:
                        return step(cast(frozenlist[hydra.core.Binding], ()), bindings0)
                    return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(bindings1(), rewrite_with_subst(cast(tuple[tuple[FrozenDict[hydra.core.Name, hydra.core.Name], frozenset[hydra.core.Name]], int], (cast(tuple[FrozenDict[hydra.core.Name, hydra.core.Name], frozenset[hydra.core.Name]], (subst(), bound_vars())), next())), body0))))
                
                case hydra.core.TermTypeApplication(value=tt):
                    return cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(rewrite_with_subst(cast(tuple[tuple[FrozenDict[hydra.core.Name, hydra.core.Name], frozenset[hydra.core.Name]], int], (cast(tuple[FrozenDict[hydra.core.Name, hydra.core.Name], frozenset[hydra.core.Name]], (subst(), bound_vars())), next())), tt.body), subst_type(subst(), tt.type))))
                
                case hydra.core.TermTypeLambda(value=ta):
                    return cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(replace_name(subst(), ta.parameter), rewrite_with_subst(cast(tuple[tuple[FrozenDict[hydra.core.Name, hydra.core.Name], frozenset[hydra.core.Name]], int], (cast(tuple[FrozenDict[hydra.core.Name, hydra.core.Name], frozenset[hydra.core.Name]], (subst(), bound_vars())), next())), ta.body))))
                
                case _:
                    return recurse(term2)
        return rewrite_term(rewrite, term0)
    return rewrite_with_subst(cast(tuple[tuple[FrozenDict[hydra.core.Name, hydra.core.Name], frozenset[hydra.core.Name]], int], (cast(tuple[FrozenDict[hydra.core.Name, hydra.core.Name], frozenset[hydra.core.Name]], (cast(FrozenDict[hydra.core.Name, hydra.core.Name], hydra.lib.maps.empty()), cast(frozenset[hydra.core.Name], hydra.lib.sets.empty()))), 0)), term)

def prune_let(l: hydra.core.Let) -> hydra.core.Type:
    r"""Given a let expression, remove any unused bindings. The resulting expression is still a let, even if has no remaining bindings."""
    
    def binding_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
        return cast(FrozenDict[hydra.core.Name, hydra.core.Term], hydra.lib.maps.from_list(hydra.lib.lists.map((lambda b: cast(tuple[hydra.core.Name, hydra.core.Term], (b.name, b.term))), l.bindings)))
    root_name = hydra.core.Name("[[[root]]]")
    def adj(n: hydra.core.Name) -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.intersection(hydra.lib.sets.from_list(hydra.lib.maps.keys(binding_map())), free_variables_in_term(hydra.lib.logic.if_else(hydra.lib.equality.equal(n, root_name), (lambda : l.body), (lambda : hydra.lib.maybes.from_just(hydra.lib.maps.lookup(n, binding_map()))))))
    def reachable() -> frozenset[hydra.core.Name]:
        return hydra.sorting.find_reachable_nodes(adj, root_name)
    def pruned_bindings() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.lists.filter((lambda b: hydra.lib.sets.member(b.name, reachable())), l.bindings)
    return hydra.core.Let(pruned_bindings(), l.body)

def remove_term_annotations(term: hydra.core.Term) -> hydra.core.Type:
    r"""Recursively remove term annotations, including within subterms."""
    
    def remove(recurse: Callable[[hydra.core.Term], hydra.core.Term], term2: hydra.core.Term) -> hydra.core.Type:
        rewritten = recurse(term2)
        match term2:
            case hydra.core.TermAnnotated(value=at):
                return at.body
            
            case _:
                return rewritten
    return rewrite_term(remove, term)

def remove_type_annotations(typ: hydra.core.Type) -> hydra.core.Type:
    r"""Recursively remove type annotations, including within subtypes."""
    
    def remove(recurse: Callable[[T0], hydra.core.Type], typ2: T0) -> hydra.core.Type:
        rewritten = recurse(typ2)
        match rewritten:
            case hydra.core.TypeAnnotated(value=at):
                return at.body
            
            case _:
                return rewritten
    return rewrite_type(cast(Callable[[Callable[[hydra.core.Type], hydra.core.Type], hydra.core.Type], hydra.core.Type], (lambda x1, x2: remove(x1, x2))), typ)

def remove_types_from_term(term: hydra.core.Term) -> hydra.core.Type:
    r"""Strip type annotations from terms while preserving other annotations."""
    
    def strip(recurse: Callable[[T0], hydra.core.Term], term2: T0) -> hydra.core.Type:
        rewritten = recurse(term2)
        def strip_binding(b: hydra.core.Binding) -> hydra.core.Type:
            return hydra.core.Binding(b.name, b.term, cast(Maybe[hydra.core.TypeScheme], Nothing()))
        def _hoist_body_1(f: hydra.core.Function, v1: hydra.core.Function) -> hydra.core.Type:
            match v1:
                case hydra.core.FunctionElimination(value=e):
                    return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(e))))
                
                case hydra.core.FunctionLambda(value=l):
                    return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, cast(Maybe[hydra.core.Type], Nothing()), l.body)))))
                
                case _:
                    return cast(hydra.core.Term, hydra.core.TermFunction(f))
        match rewritten:
            case hydra.core.TermFunction(value=f):
                return _hoist_body_1(f, f)
            
            case hydra.core.TermLet(value=lt):
                return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.map(strip_binding, lt.bindings), lt.body)))
            
            case hydra.core.TermTypeApplication(value=tt):
                return tt.body
            
            case hydra.core.TermTypeLambda(value=ta):
                return ta.body
            
            case _:
                return rewritten
    return rewrite_term(cast(Callable[[Callable[[hydra.core.Term], hydra.core.Term], hydra.core.Term], hydra.core.Term], (lambda x1, x2: strip(x1, x2))), term)

def replace_free_term_variable(vold: hydra.core.Name, tnew: hydra.core.Term, term: hydra.core.Term) -> hydra.core.Type:
    r"""Replace a free variable in a term."""
    
    def rewrite(recurse: Callable[[hydra.core.Term], hydra.core.Term], t: hydra.core.Term) -> hydra.core.Type:
        def _hoist_rewrite_1(recurse: Callable[[T0], T0], t: T0, v1: hydra.core.Function) -> T0:
            match v1:
                case hydra.core.FunctionLambda(value=l):
                    v = l.parameter
                    return hydra.lib.logic.if_else(hydra.lib.equality.equal(v, vold), (lambda : t), (lambda : recurse(t)))
                
                case _:
                    return recurse(t)
        match t:
            case hydra.core.TermFunction(value=f):
                return _hoist_rewrite_1(recurse, t, f)
            
            case hydra.core.TermVariable(value=v):
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(v, vold), (lambda : tnew), (lambda : cast(hydra.core.Term, hydra.core.TermVariable(v))))
            
            case _:
                return recurse(t)
    return rewrite_term(rewrite, term)

def replace_free_type_variable(v: hydra.core.Name, rep: hydra.core.Type, typ: hydra.core.Type) -> hydra.core.Type:
    r"""Replace free occurrences of a name in a type."""
    
    def map_expr(recurse: Callable[[hydra.core.Type], hydra.core.Type], t: hydra.core.Type) -> hydra.core.Type:
        match t:
            case hydra.core.TypeForall(value=ft):
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(v, ft.parameter), (lambda : t), (lambda : cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(ft.parameter, recurse(ft.body))))))
            
            case hydra.core.TypeVariable(value=v_):
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(v, v_), (lambda : rep), (lambda : t))
            
            case _:
                return recurse(t)
    return rewrite_type(map_expr, typ)

def replace_typedefs(types: FrozenDict[hydra.core.Name, hydra.core.TypeScheme], typ0: hydra.core.Type) -> hydra.core.Type:
    r"""Replace all occurrences of simple typedefs (type aliases) with the aliased types, recursively."""
    
    def rewrite(recurse: Callable[[hydra.core.Type], hydra.core.Type], typ: hydra.core.Type) -> hydra.core.Type:
        dflt = recurse(typ)
        match typ:
            case hydra.core.TypeAnnotated(value=at):
                return rewrite(recurse, at.body)
            
            case hydra.core.TypeRecord():
                return typ
            
            case hydra.core.TypeUnion():
                return typ
            
            case hydra.core.TypeVariable(value=v):
                def for_mono(t: hydra.core.Type) -> hydra.core.Type:
                    match t:
                        case hydra.core.TypeRecord():
                            return dflt
                        
                        case hydra.core.TypeUnion():
                            return dflt
                        
                        case hydra.core.TypeWrap():
                            return dflt
                        
                        case _:
                            return rewrite(recurse, t)
                def for_type_scheme(ts: hydra.core.TypeScheme) -> hydra.core.Type:
                    t = ts.type
                    return hydra.lib.logic.if_else(hydra.lib.lists.null(ts.variables), (lambda : for_mono(t)), (lambda : dflt))
                return hydra.lib.maybes.maybe(dflt, (lambda ts: for_type_scheme(ts)), hydra.lib.maps.lookup(v, types))
            
            case hydra.core.TypeWrap():
                return typ
            
            case _:
                return dflt
    return rewrite_type(rewrite, typ0)

def rewrite_and_fold_term(f: Callable[[
  Callable[[T0, hydra.core.Term], tuple[T0, hydra.core.Term]],
  T0,
  hydra.core.Term], tuple[T0, hydra.core.Term]], term0: T0, v1: hydra.core.Term) -> tuple[T0, hydra.core.Term]:
    def fsub(recurse: Callable[[T1, hydra.core.Term], tuple[T1, hydra.core.Term]], val0: T1, term02: hydra.core.Term) -> tuple[T1, hydra.core.Term]:
        def for_single(rec: Callable[[T2, T3], tuple[T4, T5]], cons: Callable[[T5], T6], val: T2, term: T3) -> tuple[T4, T6]:
            r = rec(val, term)
            return cast(tuple[T4, T6], (hydra.lib.pairs.first(r), cons(hydra.lib.pairs.second(r))))
        def for_many(rec: Callable[[T2, T3], tuple[T2, T4]], cons: Callable[[frozenlist[T4]], T5], val: T2, els: frozenlist[T3]) -> tuple[T2, T5]:
            def rr() -> tuple[T2, frozenlist[T4]]:
                return hydra.lib.lists.foldl((lambda r, el: (r2 := (lambda : rec(hydra.lib.pairs.first(r), el)), cast(tuple[T2, frozenlist[T4]], (hydra.lib.pairs.first(r2()), hydra.lib.lists.cons(hydra.lib.pairs.second(r2()), hydra.lib.pairs.second(r)))))[1]), cast(tuple[T2, frozenlist[T4]], (val, cast(frozenlist[T4], ()))), els)
            return cast(tuple[T2, T5], (hydra.lib.pairs.first(rr()), cons(hydra.lib.lists.reverse(hydra.lib.pairs.second(rr())))))
        def for_field(val: T1, field: hydra.core.Field) -> tuple[T1, hydra.core.Field]:
            r = recurse(val, field.term)
            return cast(tuple[T1, hydra.core.Field], (hydra.lib.pairs.first(r), hydra.core.Field(field.name, hydra.lib.pairs.second(r))))
        def for_fields(v1: T1, v2: frozenlist[hydra.core.Field]) -> tuple[T1, frozenlist[hydra.core.Field]]:
            return for_many(for_field, (lambda x: x), v1, v2)
        def for_pair(val: T1, kv: tuple[hydra.core.Term, hydra.core.Term]) -> tuple[T1, tuple[hydra.core.Term, hydra.core.Term]]:
            def rk() -> tuple[T1, hydra.core.Term]:
                return recurse(val, hydra.lib.pairs.first(kv))
            def rv() -> tuple[T1, hydra.core.Term]:
                return recurse(hydra.lib.pairs.first(rk()), hydra.lib.pairs.second(kv))
            return cast(tuple[T1, tuple[hydra.core.Term, hydra.core.Term]], (hydra.lib.pairs.first(rv()), cast(tuple[hydra.core.Term, hydra.core.Term], (hydra.lib.pairs.second(rk()), hydra.lib.pairs.second(rv())))))
        def for_binding(val: T1, binding: hydra.core.Binding) -> tuple[T1, hydra.core.Binding]:
            r = recurse(val, binding.term)
            return cast(tuple[T1, hydra.core.Binding], (hydra.lib.pairs.first(r), hydra.core.Binding(binding.name, hydra.lib.pairs.second(r), binding.type)))
        def for_elimination(val: T1, elm: hydra.core.Elimination) -> tuple[T1, hydra.core.Elimination]:
            def r() -> tuple[T1, hydra.core.Elimination]:
                match elm:
                    case hydra.core.EliminationUnion(value=cs):
                        def rmd() -> Maybe[tuple[T1, hydra.core.Term]]:
                            return hydra.lib.maybes.map((lambda v1: recurse(val, v1)), cs.default)
                        def val1() -> T1:
                            return hydra.lib.maybes.maybe(val, cast(Callable[[tuple[T1, hydra.core.Term]], T1], (lambda x1: hydra.lib.pairs.first(x1))), rmd())
                        def rcases() -> tuple[T1, frozenlist[hydra.core.Field]]:
                            return for_fields(val1(), cs.cases)
                        return cast(tuple[T1, hydra.core.Elimination], (hydra.lib.pairs.first(rcases()), cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(cs.type_name, hydra.lib.maybes.map(cast(Callable[[tuple[T1, hydra.core.Term]], hydra.core.Term], (lambda x1: hydra.lib.pairs.second(x1))), rmd()), hydra.lib.pairs.second(rcases()))))))
                    
                    case _:
                        return cast(tuple[T1, hydra.core.Elimination], (val, elm))
            return cast(tuple[T1, hydra.core.Elimination], (hydra.lib.pairs.first(r()), hydra.lib.pairs.second(r())))
        def for_function(val: T1, fun: hydra.core.Function) -> tuple[T1, hydra.core.Function]:
            match fun:
                case hydra.core.FunctionElimination(value=elm):
                    def re() -> tuple[T1, hydra.core.Elimination]:
                        return for_elimination(val, elm)
                    return cast(tuple[T1, hydra.core.Function], (hydra.lib.pairs.first(re()), cast(hydra.core.Function, hydra.core.FunctionElimination(hydra.lib.pairs.second(re())))))
                
                case hydra.core.FunctionLambda(value=l):
                    rl = recurse(val, l.body)
                    return cast(tuple[T1, hydra.core.Function], (hydra.lib.pairs.first(rl), cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, l.domain, hydra.lib.pairs.second(rl))))))
                
                case _:
                    return cast(tuple[T1, hydra.core.Function], (val, fun))
        def dflt() -> tuple[T1, hydra.core.Term]:
            return cast(tuple[T1, hydra.core.Term], (val0, term02))
        match term02:
            case hydra.core.TermAnnotated(value=at):
                return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(t, at.annotation)))), val0, at.body)
            
            case hydra.core.TermApplication(value=a):
                rlhs = recurse(val0, a.function)
                def rrhs() -> tuple[T1, hydra.core.Term]:
                    return recurse(hydra.lib.pairs.first(rlhs), a.argument)
                return cast(tuple[T1, hydra.core.Term], (hydra.lib.pairs.first(rrhs()), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(hydra.lib.pairs.second(rlhs), hydra.lib.pairs.second(rrhs()))))))
            
            case hydra.core.TermEither(value=e):
                return hydra.lib.eithers.either((lambda l: (rl := recurse(val0, l), cast(tuple[T1, hydra.core.Term], (hydra.lib.pairs.first(rl), cast(hydra.core.Term, hydra.core.TermEither(cast(Either[hydra.core.Term, hydra.core.Term], Left(hydra.lib.pairs.second(rl))))))))[1]), (lambda r: (rr := recurse(val0, r), cast(tuple[T1, hydra.core.Term], (hydra.lib.pairs.first(rr), cast(hydra.core.Term, hydra.core.TermEither(cast(Either[hydra.core.Term, hydra.core.Term], Right(hydra.lib.pairs.second(rr))))))))[1]), e)
            
            case hydra.core.TermFunction(value=f2):
                return for_single(for_function, (lambda f3: cast(hydra.core.Term, hydra.core.TermFunction(f3))), val0, f2)
            
            case hydra.core.TermLet(value=l):
                renv = recurse(val0, l.body)
                return for_many(for_binding, (lambda bins: cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(bins, hydra.lib.pairs.second(renv))))), hydra.lib.pairs.first(renv), l.bindings)
            
            case hydra.core.TermList(value=els):
                return for_many(recurse, (lambda x: cast(hydra.core.Term, hydra.core.TermList(x))), val0, els)
            
            case hydra.core.TermMap(value=m):
                return for_many(for_pair, (lambda pairs: cast(hydra.core.Term, hydra.core.TermMap(cast(FrozenDict[hydra.core.Term, hydra.core.Term], hydra.lib.maps.from_list(pairs))))), val0, hydra.lib.maps.to_list(m))
            
            case hydra.core.TermMaybe(value=mt):
                return hydra.lib.maybes.maybe(dflt(), (lambda t: for_single(recurse, (lambda t1: cast(hydra.core.Term, hydra.core.TermMaybe(cast(Maybe[hydra.core.Term], Just(t1))))), val0, t)), mt)
            
            case hydra.core.TermPair(value=p):
                def rf() -> tuple[T1, hydra.core.Term]:
                    return recurse(val0, hydra.lib.pairs.first(p))
                def rs() -> tuple[T1, hydra.core.Term]:
                    return recurse(hydra.lib.pairs.first(rf()), hydra.lib.pairs.second(p))
                return cast(tuple[T1, hydra.core.Term], (hydra.lib.pairs.first(rs()), cast(hydra.core.Term, hydra.core.TermPair(cast(tuple[hydra.core.Term, hydra.core.Term], (hydra.lib.pairs.second(rf()), hydra.lib.pairs.second(rs())))))))
            
            case hydra.core.TermRecord(value=r):
                return for_many(for_field, (lambda fields: cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(r.type_name, fields)))), val0, r.fields)
            
            case hydra.core.TermSet(value=els2):
                return for_many(recurse, (lambda e: cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(e)))), val0, hydra.lib.sets.to_list(els2))
            
            case hydra.core.TermTypeApplication(value=ta):
                return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(t, ta.type)))), val0, ta.body)
            
            case hydra.core.TermTypeLambda(value=tl):
                return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(tl.parameter, t)))), val0, tl.body)
            
            case hydra.core.TermUnion(value=inj):
                return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(inj.type_name, hydra.core.Field(inj.field.name, t))))), val0, inj.field.term)
            
            case hydra.core.TermWrap(value=wt):
                return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(wt.type_name, t)))), val0, wt.body)
            
            case _:
                return dflt()
    def recurse(v1: T0, v2: hydra.core.Term) -> tuple[T0, hydra.core.Term]:
        return f((lambda v1, v2: fsub(recurse, v1, v2)), v1, v2)
    return recurse(term0, v1)

def rewrite_and_fold_term_m(f: Callable[[
  Callable[[T0, hydra.core.Term], hydra.compute.Flow[T1, tuple[T0, hydra.core.Term]]],
  T0,
  hydra.core.Term], hydra.compute.Flow[T1, tuple[T0, hydra.core.Term]]], term0: T0, v1: hydra.core.Term) -> hydra.compute.Flow[T1, tuple[T0, hydra.core.Term]]:
    def fsub(recurse: Callable[[T2, hydra.core.Term], hydra.compute.Flow[T3, tuple[T2, hydra.core.Term]]], val0: T2, term02: hydra.core.Term) -> hydra.compute.Flow[T3, tuple[T2, hydra.core.Term]]:
        def for_single(rec: Callable[[T4, T5], hydra.compute.Flow[T6, tuple[T7, T8]]], cons: Callable[[T8], T9], val: T4, term: T5) -> hydra.compute.Flow[T6, tuple[T7, T9]]:
            return hydra.lib.flows.bind(rec(val, term), (lambda r: hydra.lib.flows.pure(cast(tuple[T7, T9], (hydra.lib.pairs.first(r), cons(hydra.lib.pairs.second(r)))))))
        def for_many(rec: Callable[[T4, T5], hydra.compute.Flow[T6, tuple[T4, T7]]], cons: Callable[[frozenlist[T7]], T8], val: T4, els: frozenlist[T5]) -> hydra.compute.Flow[T6, tuple[T4, T8]]:
            return hydra.lib.flows.bind(hydra.lib.flows.foldl((lambda r, el: hydra.lib.flows.bind(rec(hydra.lib.pairs.first(r), el), (lambda r2: hydra.lib.flows.pure(cast(tuple[T4, frozenlist[T7]], (hydra.lib.pairs.first(r2), hydra.lib.lists.cons(hydra.lib.pairs.second(r2), hydra.lib.pairs.second(r)))))))), cast(tuple[T4, frozenlist[T7]], (val, cast(frozenlist[T7], ()))), els), (lambda rr: hydra.lib.flows.pure(cast(tuple[T4, T8], (hydra.lib.pairs.first(rr), cons(hydra.lib.lists.reverse(hydra.lib.pairs.second(rr))))))))
        def for_field(val: T2, field: hydra.core.Field) -> hydra.compute.Flow[T3, tuple[T2, hydra.core.Field]]:
            return hydra.lib.flows.bind(recurse(val, field.term), (lambda r: hydra.lib.flows.pure(cast(tuple[T2, hydra.core.Field], (hydra.lib.pairs.first(r), hydra.core.Field(field.name, hydra.lib.pairs.second(r)))))))
        def for_fields(v1: T2, v2: frozenlist[hydra.core.Field]) -> hydra.compute.Flow[T3, tuple[T2, frozenlist[hydra.core.Field]]]:
            return for_many(for_field, (lambda x: x), v1, v2)
        def for_pair(val: T2, kv: tuple[hydra.core.Term, hydra.core.Term]) -> hydra.compute.Flow[T3, tuple[T2, tuple[hydra.core.Term, hydra.core.Term]]]:
            return hydra.lib.flows.bind(recurse(val, hydra.lib.pairs.first(kv)), (lambda rk: hydra.lib.flows.bind(recurse(hydra.lib.pairs.first(rk), hydra.lib.pairs.second(kv)), (lambda rv: hydra.lib.flows.pure(cast(tuple[T2, tuple[hydra.core.Term, hydra.core.Term]], (hydra.lib.pairs.first(rv), cast(tuple[hydra.core.Term, hydra.core.Term], (hydra.lib.pairs.second(rk), hydra.lib.pairs.second(rv))))))))))
        def for_binding(val: T2, binding: hydra.core.Binding) -> hydra.compute.Flow[T3, tuple[T2, hydra.core.Binding]]:
            return hydra.lib.flows.bind(recurse(val, binding.term), (lambda r: hydra.lib.flows.pure(cast(tuple[T2, hydra.core.Binding], (hydra.lib.pairs.first(r), hydra.core.Binding(binding.name, hydra.lib.pairs.second(r), binding.type))))))
        def for_elimination(val: T2, elm: hydra.core.Elimination) -> hydra.compute.Flow[T3, tuple[T2, hydra.core.Elimination]]:
            def rw(elm2: hydra.core.Elimination) -> hydra.compute.Flow[T3, tuple[T2, hydra.core.Elimination]]:
                match elm2:
                    case hydra.core.EliminationUnion(value=cs):
                        return hydra.lib.flows.bind(hydra.lib.maybes.maybe(hydra.lib.flows.pure(cast(Maybe[tuple[T2, hydra.core.Term]], Nothing())), (lambda def_: hydra.lib.flows.map(cast(Callable[[tuple[T2, hydra.core.Term]], Maybe[tuple[T2, hydra.core.Term]]], (lambda x1: hydra.lib.maybes.pure(x1))), recurse(val, def_))), cs.default), (lambda rmd: (val1 := (lambda : hydra.lib.maybes.maybe(val, cast(Callable[[tuple[T2, hydra.core.Term]], T2], (lambda x1: hydra.lib.pairs.first(x1))), rmd)), hydra.lib.flows.bind(for_fields(val1(), cs.cases), (lambda rcases: hydra.lib.flows.pure(cast(tuple[T2, hydra.core.Elimination], (hydra.lib.pairs.first(rcases), cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(cs.type_name, hydra.lib.maybes.map(cast(Callable[[tuple[T2, hydra.core.Term]], hydra.core.Term], (lambda x1: hydra.lib.pairs.second(x1))), rmd), hydra.lib.pairs.second(rcases))))))))))[1]))
                    
                    case _:
                        return hydra.lib.flows.pure(cast(tuple[T2, hydra.core.Elimination], (val, elm2)))
            return hydra.lib.flows.bind(rw(elm), (lambda r: hydra.lib.flows.pure(cast(tuple[T2, hydra.core.Elimination], (hydra.lib.pairs.first(r), hydra.lib.pairs.second(r))))))
        def for_function(val: T2, fun: hydra.core.Function) -> hydra.compute.Flow[T3, tuple[T2, hydra.core.Function]]:
            match fun:
                case hydra.core.FunctionElimination(value=elm):
                    return hydra.lib.flows.bind(for_elimination(val, elm), (lambda r: hydra.lib.flows.pure(cast(tuple[T2, hydra.core.Function], (hydra.lib.pairs.first(r), cast(hydra.core.Function, hydra.core.FunctionElimination(hydra.lib.pairs.second(r))))))))
                
                case hydra.core.FunctionLambda(value=l):
                    return hydra.lib.flows.bind(recurse(val, l.body), (lambda r: hydra.lib.flows.pure(cast(tuple[T2, hydra.core.Function], (hydra.lib.pairs.first(r), cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, l.domain, hydra.lib.pairs.second(r)))))))))
                
                case _:
                    return hydra.lib.flows.pure(cast(tuple[T2, hydra.core.Function], (val, fun)))
        def dflt() -> hydra.compute.Flow[T4, tuple[T2, hydra.core.Term]]:
            return hydra.lib.flows.pure(cast(tuple[T2, hydra.core.Term], (val0, term02)))
        match term02:
            case hydra.core.TermAnnotated(value=at):
                return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(t, at.annotation)))), val0, at.body)
            
            case hydra.core.TermApplication(value=a):
                return hydra.lib.flows.bind(recurse(val0, a.function), (lambda rlhs: hydra.lib.flows.bind(recurse(hydra.lib.pairs.first(rlhs), a.argument), (lambda rrhs: hydra.lib.flows.pure(cast(tuple[T2, hydra.core.Term], (hydra.lib.pairs.first(rrhs), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(hydra.lib.pairs.second(rlhs), hydra.lib.pairs.second(rrhs)))))))))))
            
            case hydra.core.TermEither(value=e):
                return hydra.lib.eithers.either((lambda l: hydra.lib.flows.bind(recurse(val0, l), (lambda rl: hydra.lib.flows.pure(cast(tuple[T2, hydra.core.Term], (hydra.lib.pairs.first(rl), cast(hydra.core.Term, hydra.core.TermEither(cast(Either[hydra.core.Term, hydra.core.Term], Left(hydra.lib.pairs.second(rl))))))))))), (lambda r: hydra.lib.flows.bind(recurse(val0, r), (lambda rr: hydra.lib.flows.pure(cast(tuple[T2, hydra.core.Term], (hydra.lib.pairs.first(rr), cast(hydra.core.Term, hydra.core.TermEither(cast(Either[hydra.core.Term, hydra.core.Term], Right(hydra.lib.pairs.second(rr))))))))))), e)
            
            case hydra.core.TermFunction(value=f2):
                return for_single(for_function, (lambda f3: cast(hydra.core.Term, hydra.core.TermFunction(f3))), val0, f2)
            
            case hydra.core.TermLet(value=l):
                return hydra.lib.flows.bind(recurse(val0, l.body), (lambda renv: for_many(for_binding, (lambda bins: cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(bins, hydra.lib.pairs.second(renv))))), hydra.lib.pairs.first(renv), l.bindings)))
            
            case hydra.core.TermList(value=els):
                return for_many(recurse, (lambda x: cast(hydra.core.Term, hydra.core.TermList(x))), val0, els)
            
            case hydra.core.TermMap(value=m):
                return for_many(for_pair, (lambda pairs: cast(hydra.core.Term, hydra.core.TermMap(cast(FrozenDict[hydra.core.Term, hydra.core.Term], hydra.lib.maps.from_list(pairs))))), val0, hydra.lib.maps.to_list(m))
            
            case hydra.core.TermMaybe(value=mt):
                return hydra.lib.maybes.maybe(cast(hydra.compute.Flow[T3, tuple[T2, hydra.core.Term]], dflt()), (lambda t: for_single(recurse, (lambda t1: cast(hydra.core.Term, hydra.core.TermMaybe(cast(Maybe[hydra.core.Term], Just(t1))))), val0, t)), mt)
            
            case hydra.core.TermPair(value=p):
                return hydra.lib.flows.bind(recurse(val0, hydra.lib.pairs.first(p)), (lambda rf: hydra.lib.flows.bind(recurse(hydra.lib.pairs.first(rf), hydra.lib.pairs.second(p)), (lambda rs: hydra.lib.flows.pure(cast(tuple[T2, hydra.core.Term], (hydra.lib.pairs.first(rs), cast(hydra.core.Term, hydra.core.TermPair(cast(tuple[hydra.core.Term, hydra.core.Term], (hydra.lib.pairs.second(rf), hydra.lib.pairs.second(rs))))))))))))
            
            case hydra.core.TermRecord(value=r):
                return for_many(for_field, (lambda fields: cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(r.type_name, fields)))), val0, r.fields)
            
            case hydra.core.TermSet(value=els2):
                return for_many(recurse, (lambda e: cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(e)))), val0, hydra.lib.sets.to_list(els2))
            
            case hydra.core.TermTypeApplication(value=ta):
                return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(t, ta.type)))), val0, ta.body)
            
            case hydra.core.TermTypeLambda(value=tl):
                return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(tl.parameter, t)))), val0, tl.body)
            
            case hydra.core.TermUnion(value=inj):
                return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(inj.type_name, hydra.core.Field(inj.field.name, t))))), val0, inj.field.term)
            
            case hydra.core.TermWrap(value=wt):
                return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(wt.type_name, t)))), val0, wt.body)
            
            case _:
                return cast(hydra.compute.Flow[T3, tuple[T2, hydra.core.Term]], dflt())
    def recurse(v1: T0, v2: hydra.core.Term) -> hydra.compute.Flow[T1, tuple[T0, hydra.core.Term]]:
        return f((lambda v1, v2: fsub(recurse, v1, v2)), v1, v2)
    return recurse(term0, v1)

def rewrite_and_fold_term_with_path(f: Callable[[
  Callable[[frozenlist[hydra.accessors.TermAccessor], T0, hydra.core.Term], tuple[T0, hydra.core.Term]],
  frozenlist[hydra.accessors.TermAccessor],
  T0,
  hydra.core.Term], tuple[T0, hydra.core.Term]], term0: T0, v1: hydra.core.Term) -> tuple[T0, hydra.core.Term]:
    def fsub(recurse: Callable[[frozenlist[hydra.accessors.TermAccessor], T1, hydra.core.Term], tuple[T1, hydra.core.Term]], path: frozenlist[hydra.accessors.TermAccessor], val0: T1, term02: hydra.core.Term) -> tuple[T1, hydra.core.Term]:
        def for_single_with_accessor(rec: Callable[[frozenlist[hydra.accessors.TermAccessor], T2, T3], tuple[T4, T5]], cons: Callable[[T5], T6], accessor: hydra.accessors.TermAccessor, val: T2, term: T3) -> tuple[T4, T6]:
            def r() -> tuple[T4, T5]:
                return rec(hydra.lib.lists.concat2(path, (accessor,)), val, term)
            return cast(tuple[T4, T6], (hydra.lib.pairs.first(r()), cons(hydra.lib.pairs.second(r()))))
        def for_many_with_accessors(rec: Callable[[frozenlist[hydra.accessors.TermAccessor], T2, T3], tuple[T2, T4]], cons: Callable[[frozenlist[T4]], T5], val: T2, accessor_term_pairs: frozenlist[tuple[hydra.accessors.TermAccessor, T3]]) -> tuple[T2, T5]:
            def rr() -> tuple[T2, frozenlist[T4]]:
                return hydra.lib.lists.foldl((lambda r, atp: (r2 := (lambda : rec(hydra.lib.lists.concat2(path, (hydra.lib.pairs.first(atp),)), hydra.lib.pairs.first(r), hydra.lib.pairs.second(atp))), cast(tuple[T2, frozenlist[T4]], (hydra.lib.pairs.first(r2()), hydra.lib.lists.cons(hydra.lib.pairs.second(r2()), hydra.lib.pairs.second(r)))))[1]), cast(tuple[T2, frozenlist[T4]], (val, cast(frozenlist[T4], ()))), accessor_term_pairs)
            return cast(tuple[T2, T5], (hydra.lib.pairs.first(rr()), cons(hydra.lib.lists.reverse(hydra.lib.pairs.second(rr())))))
        def for_field_with_accessor(mk_accessor: Callable[[hydra.core.Name], hydra.accessors.TermAccessor], val: T1, field: hydra.core.Field) -> tuple[T1, hydra.core.Field]:
            def r() -> tuple[T1, hydra.core.Term]:
                return recurse(hydra.lib.lists.concat2(path, (mk_accessor(field.name),)), val, field.term)
            return cast(tuple[T1, hydra.core.Field], (hydra.lib.pairs.first(r()), hydra.core.Field(field.name, hydra.lib.pairs.second(r()))))
        def for_fields_with_accessor(mk_accessor: Callable[[hydra.core.Name], hydra.accessors.TermAccessor], v1: T1, v2: frozenlist[tuple[hydra.accessors.TermAccessor, hydra.core.Field]]) -> tuple[T1, frozenlist[hydra.core.Field]]:
            return for_many_with_accessors((lambda path1, val1, field1: for_field_with_accessor(mk_accessor, val1, field1)), (lambda x: x), v1, v2)
        def for_pair_with_accessors(key_accessor: hydra.accessors.TermAccessor, val_accessor: hydra.accessors.TermAccessor, val: T1, kv: tuple[hydra.core.Term, hydra.core.Term]) -> tuple[T1, tuple[hydra.core.Term, hydra.core.Term]]:
            def rk() -> tuple[T1, hydra.core.Term]:
                return recurse(hydra.lib.lists.concat2(path, (key_accessor,)), val, hydra.lib.pairs.first(kv))
            def rv() -> tuple[T1, hydra.core.Term]:
                return recurse(hydra.lib.lists.concat2(path, (val_accessor,)), hydra.lib.pairs.first(rk()), hydra.lib.pairs.second(kv))
            return cast(tuple[T1, tuple[hydra.core.Term, hydra.core.Term]], (hydra.lib.pairs.first(rv()), cast(tuple[hydra.core.Term, hydra.core.Term], (hydra.lib.pairs.second(rk()), hydra.lib.pairs.second(rv())))))
        def for_binding_with_accessor(val: T1, binding: hydra.core.Binding) -> tuple[T1, hydra.core.Binding]:
            def r() -> tuple[T1, hydra.core.Term]:
                return recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorLetBinding(binding.name)),)), val, binding.term)
            return cast(tuple[T1, hydra.core.Binding], (hydra.lib.pairs.first(r()), hydra.core.Binding(binding.name, hydra.lib.pairs.second(r()), binding.type)))
        def for_elimination(val: T1, elm: hydra.core.Elimination) -> tuple[T1, hydra.core.Elimination]:
            def r() -> tuple[T1, hydra.core.Elimination]:
                match elm:
                    case hydra.core.EliminationUnion(value=cs):
                        def rmd() -> Maybe[tuple[T1, hydra.core.Term]]:
                            return hydra.lib.maybes.map((lambda def_: recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorUnionCasesDefault()),)), val, def_)), cs.default)
                        def val1() -> T1:
                            return hydra.lib.maybes.maybe(val, cast(Callable[[tuple[T1, hydra.core.Term]], T1], (lambda x1: hydra.lib.pairs.first(x1))), rmd())
                        def rcases() -> tuple[T1, frozenlist[hydra.core.Term]]:
                            return for_many_with_accessors(recurse, (lambda x: x), val1(), hydra.lib.lists.map((lambda f2: cast(tuple[hydra.accessors.TermAccessor, hydra.core.Term], (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorUnionCasesBranch(f2.name)), f2.term))), cs.cases))
                        return cast(tuple[T1, hydra.core.Elimination], (hydra.lib.pairs.first(rcases()), cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(cs.type_name, hydra.lib.maybes.map(cast(Callable[[tuple[T1, hydra.core.Term]], hydra.core.Term], (lambda x1: hydra.lib.pairs.second(x1))), rmd()), hydra.lib.lists.map((lambda ft: hydra.core.Field(hydra.lib.pairs.first(ft), hydra.lib.pairs.second(ft))), hydra.lib.lists.zip(hydra.lib.lists.map((lambda v1: v1.name), cs.cases), hydra.lib.pairs.second(rcases()))))))))
                    
                    case _:
                        return cast(tuple[T1, hydra.core.Elimination], (val, elm))
            return cast(tuple[T1, hydra.core.Elimination], (hydra.lib.pairs.first(r()), hydra.lib.pairs.second(r())))
        def for_function(val: T1, fun: hydra.core.Function) -> tuple[T1, hydra.core.Function]:
            match fun:
                case hydra.core.FunctionElimination(value=elm):
                    def re() -> tuple[T1, hydra.core.Elimination]:
                        return for_elimination(val, elm)
                    return cast(tuple[T1, hydra.core.Function], (hydra.lib.pairs.first(re()), cast(hydra.core.Function, hydra.core.FunctionElimination(hydra.lib.pairs.second(re())))))
                
                case hydra.core.FunctionLambda(value=l):
                    def rl() -> tuple[T1, hydra.core.Term]:
                        return recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorLambdaBody()),)), val, l.body)
                    return cast(tuple[T1, hydra.core.Function], (hydra.lib.pairs.first(rl()), cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, l.domain, hydra.lib.pairs.second(rl()))))))
                
                case _:
                    return cast(tuple[T1, hydra.core.Function], (val, fun))
        def dflt() -> tuple[T1, hydra.core.Term]:
            return cast(tuple[T1, hydra.core.Term], (val0, term02))
        match term02:
            case hydra.core.TermAnnotated(value=at):
                return for_single_with_accessor(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(t, at.annotation)))), cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorAnnotatedBody()), val0, at.body)
            
            case hydra.core.TermApplication(value=a):
                def rlhs() -> tuple[T1, hydra.core.Term]:
                    return recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorApplicationFunction()),)), val0, a.function)
                def rrhs() -> tuple[T1, hydra.core.Term]:
                    return recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorApplicationArgument()),)), hydra.lib.pairs.first(rlhs()), a.argument)
                return cast(tuple[T1, hydra.core.Term], (hydra.lib.pairs.first(rrhs()), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(hydra.lib.pairs.second(rlhs()), hydra.lib.pairs.second(rrhs()))))))
            
            case hydra.core.TermEither(value=e):
                return hydra.lib.eithers.either((lambda l: (rl := (lambda : recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorSumTerm()),)), val0, l)), cast(tuple[T1, hydra.core.Term], (hydra.lib.pairs.first(rl()), cast(hydra.core.Term, hydra.core.TermEither(cast(Either[hydra.core.Term, hydra.core.Term], Left(hydra.lib.pairs.second(rl()))))))))[1]), (lambda r: (rr := (lambda : recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorSumTerm()),)), val0, r)), cast(tuple[T1, hydra.core.Term], (hydra.lib.pairs.first(rr()), cast(hydra.core.Term, hydra.core.TermEither(cast(Either[hydra.core.Term, hydra.core.Term], Right(hydra.lib.pairs.second(rr()))))))))[1]), e)
            
            case hydra.core.TermFunction(value=f2):
                def rf() -> tuple[T1, hydra.core.Function]:
                    return for_function(val0, f2)
                return cast(tuple[T1, hydra.core.Term], (hydra.lib.pairs.first(rf()), cast(hydra.core.Term, hydra.core.TermFunction(hydra.lib.pairs.second(rf())))))
            
            case hydra.core.TermLet(value=l):
                def renv() -> tuple[T1, hydra.core.Term]:
                    return recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorLetBody()),)), val0, l.body)
                def rbindings() -> tuple[T1, frozenlist[hydra.core.Binding]]:
                    return hydra.lib.lists.foldl((lambda r, binding: (rb := (lambda : for_binding_with_accessor(hydra.lib.pairs.first(r), binding)), cast(tuple[T1, frozenlist[hydra.core.Binding]], (hydra.lib.pairs.first(rb()), hydra.lib.lists.cons(hydra.lib.pairs.second(rb()), hydra.lib.pairs.second(r)))))[1]), cast(tuple[T1, frozenlist[hydra.core.Binding]], (hydra.lib.pairs.first(renv()), cast(frozenlist[hydra.core.Binding], ()))), l.bindings)
                return cast(tuple[T1, hydra.core.Term], (hydra.lib.pairs.first(rbindings()), cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.reverse(hydra.lib.pairs.second(rbindings())), hydra.lib.pairs.second(renv()))))))
            
            case hydra.core.TermList(value=els):
                idx = 0
                def rr() -> tuple[int, tuple[T1, frozenlist[hydra.core.Term]]]:
                    return hydra.lib.lists.foldl((lambda r, el: (r2 := (lambda : recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorListElement(hydra.lib.pairs.first(r))),)), hydra.lib.pairs.first(hydra.lib.pairs.second(r)), el)), cast(tuple[int, tuple[T1, frozenlist[hydra.core.Term]]], (hydra.lib.math.add(hydra.lib.pairs.first(r), 1), cast(tuple[T1, frozenlist[hydra.core.Term]], (hydra.lib.pairs.first(r2()), hydra.lib.lists.cons(hydra.lib.pairs.second(r2()), hydra.lib.pairs.second(hydra.lib.pairs.second(r))))))))[1]), cast(tuple[int, tuple[T1, frozenlist[hydra.core.Term]]], (idx, cast(tuple[T1, frozenlist[hydra.core.Term]], (val0, cast(frozenlist[hydra.core.Term], ()))))), els)
                return cast(tuple[T1, hydra.core.Term], (hydra.lib.pairs.first(hydra.lib.pairs.second(rr())), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.reverse(hydra.lib.pairs.second(hydra.lib.pairs.second(rr())))))))
            
            case hydra.core.TermMap(value=m):
                idx = 0
                def rr() -> tuple[int, tuple[T1, frozenlist[tuple[hydra.core.Term, hydra.core.Term]]]]:
                    return hydra.lib.lists.foldl((lambda r, kv: (rk := (lambda : recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorMapKey(hydra.lib.pairs.first(r))),)), hydra.lib.pairs.first(hydra.lib.pairs.second(r)), hydra.lib.pairs.first(kv))), rv := (lambda : recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorMapValue(hydra.lib.pairs.first(r))),)), hydra.lib.pairs.first(rk()), hydra.lib.pairs.second(kv))), cast(tuple[int, tuple[T1, frozenlist[tuple[hydra.core.Term, hydra.core.Term]]]], (hydra.lib.math.add(hydra.lib.pairs.first(r), 1), cast(tuple[T1, frozenlist[tuple[hydra.core.Term, hydra.core.Term]]], (hydra.lib.pairs.first(rv()), hydra.lib.lists.cons(cast(tuple[hydra.core.Term, hydra.core.Term], (hydra.lib.pairs.second(rk()), hydra.lib.pairs.second(rv()))), hydra.lib.pairs.second(hydra.lib.pairs.second(r))))))))[2]), cast(tuple[int, tuple[T1, frozenlist[tuple[hydra.core.Term, hydra.core.Term]]]], (idx, cast(tuple[T1, frozenlist[tuple[hydra.core.Term, hydra.core.Term]]], (val0, cast(frozenlist[tuple[hydra.core.Term, hydra.core.Term]], ()))))), hydra.lib.maps.to_list(m))
                return cast(tuple[T1, hydra.core.Term], (hydra.lib.pairs.first(hydra.lib.pairs.second(rr())), cast(hydra.core.Term, hydra.core.TermMap(cast(FrozenDict[hydra.core.Term, hydra.core.Term], hydra.lib.maps.from_list(hydra.lib.lists.reverse(hydra.lib.pairs.second(hydra.lib.pairs.second(rr())))))))))
            
            case hydra.core.TermMaybe(value=mt):
                return hydra.lib.maybes.maybe(dflt(), (lambda t: for_single_with_accessor(recurse, (lambda t1: cast(hydra.core.Term, hydra.core.TermMaybe(cast(Maybe[hydra.core.Term], Just(t1))))), cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorMaybeTerm()), val0, t)), mt)
            
            case hydra.core.TermPair(value=p):
                def rf() -> tuple[T1, hydra.core.Term]:
                    return recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorProductTerm(0)),)), val0, hydra.lib.pairs.first(p))
                def rs() -> tuple[T1, hydra.core.Term]:
                    return recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorProductTerm(1)),)), hydra.lib.pairs.first(rf()), hydra.lib.pairs.second(p))
                return cast(tuple[T1, hydra.core.Term], (hydra.lib.pairs.first(rs()), cast(hydra.core.Term, hydra.core.TermPair(cast(tuple[hydra.core.Term, hydra.core.Term], (hydra.lib.pairs.second(rf()), hydra.lib.pairs.second(rs())))))))
            
            case hydra.core.TermRecord(value=r):
                def rfields() -> tuple[T1, frozenlist[hydra.core.Term]]:
                    return for_many_with_accessors(recurse, (lambda x: x), val0, hydra.lib.lists.map((lambda f2: cast(tuple[hydra.accessors.TermAccessor, hydra.core.Term], (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorRecordField(f2.name)), f2.term))), r.fields))
                return cast(tuple[T1, hydra.core.Term], (hydra.lib.pairs.first(rfields()), cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(r.type_name, hydra.lib.lists.map((lambda ft: hydra.core.Field(hydra.lib.pairs.first(ft), hydra.lib.pairs.second(ft))), hydra.lib.lists.zip(hydra.lib.lists.map((lambda v1: v1.name), r.fields), hydra.lib.pairs.second(rfields()))))))))
            
            case hydra.core.TermSet(value=els2):
                idx = 0
                def rr() -> tuple[int, tuple[T1, frozenlist[hydra.core.Term]]]:
                    return hydra.lib.lists.foldl((lambda r, el: (r2 := (lambda : recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorSetElement(hydra.lib.pairs.first(r))),)), hydra.lib.pairs.first(hydra.lib.pairs.second(r)), el)), cast(tuple[int, tuple[T1, frozenlist[hydra.core.Term]]], (hydra.lib.math.add(hydra.lib.pairs.first(r), 1), cast(tuple[T1, frozenlist[hydra.core.Term]], (hydra.lib.pairs.first(r2()), hydra.lib.lists.cons(hydra.lib.pairs.second(r2()), hydra.lib.pairs.second(hydra.lib.pairs.second(r))))))))[1]), cast(tuple[int, tuple[T1, frozenlist[hydra.core.Term]]], (idx, cast(tuple[T1, frozenlist[hydra.core.Term]], (val0, cast(frozenlist[hydra.core.Term], ()))))), hydra.lib.sets.to_list(els2))
                return cast(tuple[T1, hydra.core.Term], (hydra.lib.pairs.first(hydra.lib.pairs.second(rr())), cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(hydra.lib.lists.reverse(hydra.lib.pairs.second(hydra.lib.pairs.second(rr()))))))))
            
            case hydra.core.TermTypeApplication(value=ta):
                return for_single_with_accessor(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(t, ta.type)))), cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorTypeApplicationTerm()), val0, ta.body)
            
            case hydra.core.TermTypeLambda(value=tl):
                return for_single_with_accessor(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(tl.parameter, t)))), cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorTypeLambdaBody()), val0, tl.body)
            
            case hydra.core.TermUnion(value=inj):
                return for_single_with_accessor(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(inj.type_name, hydra.core.Field(inj.field.name, t))))), cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorInjectionTerm()), val0, inj.field.term)
            
            case hydra.core.TermWrap(value=wt):
                return for_single_with_accessor(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(wt.type_name, t)))), cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorWrappedTerm()), val0, wt.body)
            
            case _:
                return dflt()
    def recurse(v1: frozenlist[hydra.accessors.TermAccessor], v2: T0, v3: hydra.core.Term) -> tuple[T0, hydra.core.Term]:
        return f((lambda v1, v2, v3: fsub(recurse, v1, v2, v3)), v1, v2, v3)
    return recurse(cast(frozenlist[hydra.accessors.TermAccessor], ()), term0, v1)

def rewrite_term_m(f: Callable[[
  Callable[[hydra.core.Term], hydra.compute.Flow[T0, hydra.core.Term]],
  hydra.core.Term], hydra.compute.Flow[T0, hydra.core.Term]], term0: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
    def fsub(recurse: Callable[[hydra.core.Term], hydra.compute.Flow[T1, hydra.core.Term]], term: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.core.Term]:
        def for_field(field: hydra.core.Field) -> hydra.compute.Flow[T1, hydra.core.Field]:
            return hydra.lib.flows.bind(recurse(field.term), (lambda t: hydra.lib.flows.pure(hydra.core.Field(field.name, t))))
        def for_pair(kv: tuple[hydra.core.Term, hydra.core.Term]) -> hydra.compute.Flow[T1, tuple[hydra.core.Term, hydra.core.Term]]:
            return hydra.lib.flows.bind(recurse(hydra.lib.pairs.first(kv)), (lambda k: hydra.lib.flows.bind(recurse(hydra.lib.pairs.second(kv)), (lambda v: hydra.lib.flows.pure(cast(tuple[hydra.core.Term, hydra.core.Term], (k, v)))))))
        def map_binding(b: hydra.core.Binding) -> hydra.compute.Flow[T1, hydra.core.Binding]:
            return hydra.lib.flows.bind(recurse(b.term), (lambda v: hydra.lib.flows.pure(hydra.core.Binding(b.name, v, b.type))))
        match term:
            case hydra.core.TermAnnotated(value=at):
                return hydra.lib.flows.bind(recurse(at.body), (lambda ex: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(ex, at.annotation))))))
            
            case hydra.core.TermApplication(value=app):
                return hydra.lib.flows.bind(recurse(app.function), (lambda lhs: hydra.lib.flows.bind(recurse(app.argument), (lambda rhs: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(lhs, rhs))))))))
            
            case hydra.core.TermEither(value=e):
                return hydra.lib.flows.bind(hydra.lib.eithers.either((lambda l: hydra.lib.flows.map((lambda x: cast(Either[hydra.core.Term, hydra.core.Term], Left(x))), recurse(l))), (lambda r: hydra.lib.flows.map((lambda x: cast(Either[hydra.core.Term, hydra.core.Term], Right(x))), recurse(r))), e), (lambda re: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermEither(re)))))
            
            case hydra.core.TermFunction(value=fun):
                def for_elm(e: hydra.core.Elimination) -> hydra.compute.Flow[T1, hydra.core.Function]:
                    match e:
                        case hydra.core.EliminationRecord(value=p):
                            return hydra.lib.flows.pure(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(p)))))
                        
                        case hydra.core.EliminationUnion(value=cs):
                            n = cs.type_name
                            def_ = cs.default
                            cases = cs.cases
                            return hydra.lib.flows.bind(hydra.lib.maybes.maybe(hydra.lib.flows.pure(cast(Maybe[hydra.core.Term], Nothing())), (lambda t: hydra.lib.flows.map(cast(Callable[[hydra.core.Term], Maybe[hydra.core.Term]], (lambda x1: hydra.lib.maybes.pure(x1))), recurse(t))), def_), (lambda rdef: hydra.lib.flows.map((lambda rcases: cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(n, rdef, rcases)))))), hydra.lib.flows.map_list(for_field, cases))))
                        
                        case hydra.core.EliminationWrap(value=name):
                            return hydra.lib.flows.pure(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(name)))))
                        
                        case _:
                            raise AssertionError("Unreachable: all variants handled")
                def for_fun(fun2: hydra.core.Function) -> hydra.compute.Flow[T1, hydra.core.Function]:
                    match fun2:
                        case hydra.core.FunctionElimination(value=e):
                            return for_elm(e)
                        
                        case hydra.core.FunctionLambda(value=l):
                            v = l.parameter
                            d = l.domain
                            body = l.body
                            return hydra.lib.flows.bind(recurse(body), (lambda rbody: hydra.lib.flows.pure(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(v, d, rbody))))))
                        
                        case hydra.core.FunctionPrimitive(value=name):
                            return hydra.lib.flows.pure(cast(hydra.core.Function, hydra.core.FunctionPrimitive(name)))
                        
                        case _:
                            raise AssertionError("Unreachable: all variants handled")
                return hydra.lib.flows.bind(for_fun(fun), (lambda rfun: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermFunction(rfun)))))
            
            case hydra.core.TermLet(value=lt):
                bindings = lt.bindings
                env = lt.body
                return hydra.lib.flows.bind(hydra.lib.flows.map_list(map_binding, bindings), (lambda rbindings: hydra.lib.flows.bind(recurse(env), (lambda renv: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(rbindings, renv))))))))
            
            case hydra.core.TermList(value=els):
                return hydra.lib.flows.bind(hydra.lib.flows.map_list(recurse, els), (lambda rels: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermList(rels)))))
            
            case hydra.core.TermLiteral(value=v):
                return hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermLiteral(v)))
            
            case hydra.core.TermMap(value=m):
                return hydra.lib.flows.bind(hydra.lib.flows.map_list(for_pair, hydra.lib.maps.to_list(m)), (lambda pairs: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermMap(cast(FrozenDict[hydra.core.Term, hydra.core.Term], hydra.lib.maps.from_list(pairs)))))))
            
            case hydra.core.TermMaybe(value=m2):
                return hydra.lib.flows.bind(hydra.lib.flows.map_maybe(recurse, m2), (lambda rm: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermMaybe(rm)))))
            
            case hydra.core.TermPair(value=p):
                return hydra.lib.flows.bind(recurse(hydra.lib.pairs.first(p)), (lambda rf: hydra.lib.flows.bind(recurse(hydra.lib.pairs.second(p)), (lambda rs: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermPair(cast(tuple[hydra.core.Term, hydra.core.Term], (rf, rs)))))))))
            
            case hydra.core.TermRecord(value=r):
                n = r.type_name
                fields = r.fields
                return hydra.lib.flows.map((lambda rfields: cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(n, rfields)))), hydra.lib.flows.map_list(for_field, fields))
            
            case hydra.core.TermSet(value=s):
                return hydra.lib.flows.bind(hydra.lib.flows.map_list(recurse, hydra.lib.sets.to_list(s)), (lambda rlist: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(rlist))))))
            
            case hydra.core.TermTypeApplication(value=tt):
                return hydra.lib.flows.bind(recurse(tt.body), (lambda t: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(t, tt.type))))))
            
            case hydra.core.TermTypeLambda(value=tl):
                v = tl.parameter
                body = tl.body
                return hydra.lib.flows.bind(recurse(body), (lambda rbody: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(v, rbody))))))
            
            case hydra.core.TermUnion(value=i):
                n = i.type_name
                field = i.field
                return hydra.lib.flows.map((lambda rfield: cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(n, rfield)))), for_field(field))
            
            case hydra.core.TermUnit():
                return hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermUnit()))
            
            case hydra.core.TermVariable(value=v2):
                return hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermVariable(v2)))
            
            case hydra.core.TermWrap(value=wt):
                name = wt.type_name
                t = wt.body
                return hydra.lib.flows.bind(recurse(t), (lambda rt: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(name, rt))))))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def recurse(v1: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
        return f((lambda v1: fsub(recurse, v1)), v1)
    return recurse(term0)

def rewrite_term_with_context(f: Callable[[
  Callable[[T0, hydra.core.Term], hydra.core.Term],
  T0,
  hydra.core.Term], hydra.core.Term], cx0: T0, term0: hydra.core.Term) -> hydra.core.Type:
    def for_subterms(recurse0: Callable[[T1, hydra.core.Term], hydra.core.Term], cx: T1, term: hydra.core.Term) -> hydra.core.Type:
        def recurse(v1: hydra.core.Term) -> hydra.core.Type:
            return recurse0(cx, v1)
        def for_field(field: hydra.core.Field) -> hydra.core.Type:
            return hydra.core.Field(field.name, recurse(field.term))
        def for_elimination(elm: hydra.core.Elimination) -> hydra.core.Type:
            match elm:
                case hydra.core.EliminationRecord(value=p):
                    return cast(hydra.core.Elimination, hydra.core.EliminationRecord(p))
                
                case hydra.core.EliminationUnion(value=cs):
                    return cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(cs.type_name, hydra.lib.maybes.map(recurse, cs.default), hydra.lib.lists.map(for_field, cs.cases))))
                
                case hydra.core.EliminationWrap(value=name):
                    return cast(hydra.core.Elimination, hydra.core.EliminationWrap(name))
                
                case _:
                    raise AssertionError("Unreachable: all variants handled")
        def for_function(fun: hydra.core.Function) -> hydra.core.Type:
            match fun:
                case hydra.core.FunctionElimination(value=elm):
                    return cast(hydra.core.Function, hydra.core.FunctionElimination(for_elimination(elm)))
                
                case hydra.core.FunctionLambda(value=l):
                    return cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, l.domain, recurse(l.body))))
                
                case hydra.core.FunctionPrimitive(value=name):
                    return cast(hydra.core.Function, hydra.core.FunctionPrimitive(name))
                
                case _:
                    raise AssertionError("Unreachable: all variants handled")
        def for_let(lt: hydra.core.Let) -> hydra.core.Type:
            def map_binding(b: hydra.core.Binding) -> hydra.core.Type:
                return hydra.core.Binding(b.name, recurse(b.term), b.type)
            return hydra.core.Let(hydra.lib.lists.map(map_binding, lt.bindings), recurse(lt.body))
        def for_map(m: FrozenDict[hydra.core.Term, hydra.core.Term]) -> FrozenDict[hydra.core.Term, hydra.core.Term]:
            def for_pair(p: tuple[hydra.core.Term, hydra.core.Term]) -> tuple[hydra.core.Term, hydra.core.Term]:
                return cast(tuple[hydra.core.Term, hydra.core.Term], (recurse(hydra.lib.pairs.first(p)), recurse(hydra.lib.pairs.second(p))))
            return cast(FrozenDict[hydra.core.Term, hydra.core.Term], hydra.lib.maps.from_list(hydra.lib.lists.map(for_pair, hydra.lib.maps.to_list(m))))
        match term:
            case hydra.core.TermAnnotated(value=at):
                return cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(recurse(at.body), at.annotation)))
            
            case hydra.core.TermApplication(value=a):
                return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(recurse(a.function), recurse(a.argument))))
            
            case hydra.core.TermEither(value=e):
                return cast(hydra.core.Term, hydra.core.TermEither(hydra.lib.eithers.either((lambda l: cast(Either[hydra.core.Term, hydra.core.Term], Left(recurse(l)))), (lambda r: cast(Either[hydra.core.Term, hydra.core.Term], Right(recurse(r)))), e)))
            
            case hydra.core.TermFunction(value=fun):
                return cast(hydra.core.Term, hydra.core.TermFunction(for_function(fun)))
            
            case hydra.core.TermLet(value=lt):
                return cast(hydra.core.Term, hydra.core.TermLet(for_let(lt)))
            
            case hydra.core.TermList(value=els):
                return cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(recurse, els)))
            
            case hydra.core.TermLiteral(value=v):
                return cast(hydra.core.Term, hydra.core.TermLiteral(v))
            
            case hydra.core.TermMap(value=m):
                return cast(hydra.core.Term, hydra.core.TermMap(for_map(m)))
            
            case hydra.core.TermMaybe(value=m2):
                return cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(recurse, m2)))
            
            case hydra.core.TermPair(value=p):
                return cast(hydra.core.Term, hydra.core.TermPair(cast(tuple[hydra.core.Term, hydra.core.Term], (recurse(hydra.lib.pairs.first(p)), recurse(hydra.lib.pairs.second(p))))))
            
            case hydra.core.TermRecord(value=r):
                return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(r.type_name, hydra.lib.lists.map(for_field, r.fields))))
            
            case hydra.core.TermSet(value=s):
                return cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(hydra.lib.lists.map(recurse, hydra.lib.sets.to_list(s)))))
            
            case hydra.core.TermTypeApplication(value=tt):
                return cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(recurse(tt.body), tt.type)))
            
            case hydra.core.TermTypeLambda(value=ta):
                return cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(ta.parameter, recurse(ta.body))))
            
            case hydra.core.TermUnion(value=i):
                return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(i.type_name, for_field(i.field))))
            
            case hydra.core.TermUnit():
                return cast(hydra.core.Term, hydra.core.TermUnit())
            
            case hydra.core.TermVariable(value=v2):
                return cast(hydra.core.Term, hydra.core.TermVariable(v2))
            
            case hydra.core.TermWrap(value=wt):
                return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(wt.type_name, recurse(wt.body))))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def rewrite(cx: T0, term: hydra.core.Term) -> hydra.core.Type:
        return f((lambda v1, v2: for_subterms(rewrite, v1, v2)), cx, term)
    return rewrite(cx0, term0)

def rewrite_term_with_context_m(f: Callable[[
  Callable[[T0, hydra.core.Term], hydra.compute.Flow[T1, hydra.core.Term]],
  T0,
  hydra.core.Term], hydra.compute.Flow[T1, hydra.core.Term]], cx0: T0, term0: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.core.Term]:
    def for_subterms(recurse0: Callable[[T2, hydra.core.Term], hydra.compute.Flow[T3, hydra.core.Term]], cx: T2, term: hydra.core.Term) -> hydra.compute.Flow[T3, hydra.core.Term]:
        def recurse(v1: hydra.core.Term) -> hydra.compute.Flow[T3, hydra.core.Term]:
            return recurse0(cx, v1)
        def for_field(field: hydra.core.Field) -> hydra.compute.Flow[T3, hydra.core.Field]:
            return hydra.lib.flows.bind(recurse(field.term), (lambda t: hydra.lib.flows.pure(hydra.core.Field(field.name, t))))
        def for_pair(kv: tuple[hydra.core.Term, hydra.core.Term]) -> hydra.compute.Flow[T3, tuple[hydra.core.Term, hydra.core.Term]]:
            return hydra.lib.flows.bind(recurse(hydra.lib.pairs.first(kv)), (lambda k: hydra.lib.flows.bind(recurse(hydra.lib.pairs.second(kv)), (lambda v: hydra.lib.flows.pure(cast(tuple[hydra.core.Term, hydra.core.Term], (k, v)))))))
        def for_elimination(e: hydra.core.Elimination) -> hydra.compute.Flow[T3, hydra.core.Function]:
            match e:
                case hydra.core.EliminationRecord(value=p):
                    return hydra.lib.flows.pure(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(p)))))
                
                case hydra.core.EliminationUnion(value=cs):
                    n = cs.type_name
                    def_ = cs.default
                    cases = cs.cases
                    return hydra.lib.flows.bind(hydra.lib.maybes.maybe(hydra.lib.flows.pure(cast(Maybe[hydra.core.Term], Nothing())), (lambda t: hydra.lib.flows.map(cast(Callable[[hydra.core.Term], Maybe[hydra.core.Term]], (lambda x1: hydra.lib.maybes.pure(x1))), recurse(t))), def_), (lambda rdef: hydra.lib.flows.map((lambda rcases: cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(n, rdef, rcases)))))), hydra.lib.flows.map_list(for_field, cases))))
                
                case hydra.core.EliminationWrap(value=name):
                    return hydra.lib.flows.pure(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(name)))))
                
                case _:
                    raise AssertionError("Unreachable: all variants handled")
        def for_function(fun: hydra.core.Function) -> hydra.compute.Flow[T3, hydra.core.Function]:
            match fun:
                case hydra.core.FunctionElimination(value=e):
                    return for_elimination(e)
                
                case hydra.core.FunctionLambda(value=l):
                    v = l.parameter
                    d = l.domain
                    body = l.body
                    return hydra.lib.flows.bind(recurse(body), (lambda rbody: hydra.lib.flows.pure(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(v, d, rbody))))))
                
                case hydra.core.FunctionPrimitive(value=name):
                    return hydra.lib.flows.pure(cast(hydra.core.Function, hydra.core.FunctionPrimitive(name)))
                
                case _:
                    raise AssertionError("Unreachable: all variants handled")
        def map_binding(b: hydra.core.Binding) -> hydra.compute.Flow[T3, hydra.core.Binding]:
            return hydra.lib.flows.bind(recurse(b.term), (lambda v: hydra.lib.flows.pure(hydra.core.Binding(b.name, v, b.type))))
        match term:
            case hydra.core.TermAnnotated(value=at):
                return hydra.lib.flows.bind(recurse(at.body), (lambda ex: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(ex, at.annotation))))))
            
            case hydra.core.TermApplication(value=app):
                return hydra.lib.flows.bind(recurse(app.function), (lambda lhs: hydra.lib.flows.bind(recurse(app.argument), (lambda rhs: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(lhs, rhs))))))))
            
            case hydra.core.TermEither(value=e):
                return hydra.lib.flows.bind(hydra.lib.eithers.either((lambda l: hydra.lib.flows.map((lambda x: cast(Either[hydra.core.Term, hydra.core.Term], Left(x))), recurse(l))), (lambda r: hydra.lib.flows.map((lambda x: cast(Either[hydra.core.Term, hydra.core.Term], Right(x))), recurse(r))), e), (lambda re: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermEither(re)))))
            
            case hydra.core.TermFunction(value=fun):
                return hydra.lib.flows.bind(for_function(fun), (lambda rfun: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermFunction(rfun)))))
            
            case hydra.core.TermLet(value=lt):
                bindings = lt.bindings
                body = lt.body
                return hydra.lib.flows.bind(hydra.lib.flows.map_list(map_binding, bindings), (lambda rbindings: hydra.lib.flows.bind(recurse(body), (lambda rbody: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(rbindings, rbody))))))))
            
            case hydra.core.TermList(value=els):
                return hydra.lib.flows.bind(hydra.lib.flows.map_list(recurse, els), (lambda rels: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermList(rels)))))
            
            case hydra.core.TermLiteral(value=v):
                return hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermLiteral(v)))
            
            case hydra.core.TermMap(value=m):
                return hydra.lib.flows.bind(hydra.lib.flows.map_list(for_pair, hydra.lib.maps.to_list(m)), (lambda pairs: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermMap(cast(FrozenDict[hydra.core.Term, hydra.core.Term], hydra.lib.maps.from_list(pairs)))))))
            
            case hydra.core.TermMaybe(value=m2):
                return hydra.lib.flows.bind(hydra.lib.flows.map_maybe(recurse, m2), (lambda rm: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermMaybe(rm)))))
            
            case hydra.core.TermPair(value=p):
                return hydra.lib.flows.bind(recurse(hydra.lib.pairs.first(p)), (lambda rfirst: hydra.lib.flows.bind(recurse(hydra.lib.pairs.second(p)), (lambda rsecond: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermPair(cast(tuple[hydra.core.Term, hydra.core.Term], (rfirst, rsecond)))))))))
            
            case hydra.core.TermRecord(value=r):
                n = r.type_name
                fields = r.fields
                return hydra.lib.flows.map((lambda rfields: cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(n, rfields)))), hydra.lib.flows.map_list(for_field, fields))
            
            case hydra.core.TermSet(value=s):
                return hydra.lib.flows.bind(hydra.lib.flows.map_list(recurse, hydra.lib.sets.to_list(s)), (lambda rlist: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(rlist))))))
            
            case hydra.core.TermTypeApplication(value=tt):
                return hydra.lib.flows.bind(recurse(tt.body), (lambda t: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(t, tt.type))))))
            
            case hydra.core.TermTypeLambda(value=tl):
                v = tl.parameter
                body = tl.body
                return hydra.lib.flows.bind(recurse(body), (lambda rbody: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(v, rbody))))))
            
            case hydra.core.TermUnion(value=i):
                n = i.type_name
                field = i.field
                return hydra.lib.flows.map((lambda rfield: cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(n, rfield)))), for_field(field))
            
            case hydra.core.TermUnit():
                return hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermUnit()))
            
            case hydra.core.TermVariable(value=v2):
                return hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermVariable(v2)))
            
            case hydra.core.TermWrap(value=wt):
                name = wt.type_name
                t = wt.body
                return hydra.lib.flows.bind(recurse(t), (lambda rt: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(name, rt))))))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def rewrite(cx: T0, term: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.core.Term]:
        return f((lambda v1, v2: for_subterms(rewrite, v1, v2)), cx, term)
    return rewrite(cx0, term0)

def substitute_variable(from_: hydra.core.Name, to: hydra.core.Name, term: hydra.core.Term) -> hydra.core.Type:
    r"""Substitute one variable for another in a term."""
    
    def replace(recurse: Callable[[hydra.core.Term], hydra.core.Term], term2: hydra.core.Term) -> hydra.core.Type:
        def _hoist_replace_1(recurse: Callable[[T0], T0], term2: T0, v1: hydra.core.Function) -> T0:
            match v1:
                case hydra.core.FunctionLambda(value=l):
                    return hydra.lib.logic.if_else(hydra.lib.equality.equal(l.parameter, from_), (lambda : term2), (lambda : recurse(term2)))
                
                case _:
                    return recurse(term2)
        match term2:
            case hydra.core.TermVariable(value=x):
                return cast(hydra.core.Term, hydra.core.TermVariable(hydra.lib.logic.if_else(hydra.lib.equality.equal(x, from_), (lambda : to), (lambda : x))))
            
            case hydra.core.TermFunction(value=v1):
                return _hoist_replace_1(recurse, term2, v1)
            
            case _:
                return recurse(term2)
    return rewrite_term(replace, term)

def simplify_term(term: hydra.core.Term) -> hydra.core.Type:
    r"""Simplify terms by applying beta reduction where possible."""
    
    def simplify(recurse: Callable[[hydra.core.Term], T0], term2: hydra.core.Term) -> T0:
        def for_rhs(rhs: hydra.core.Term, var: hydra.core.Name, body: hydra.core.Term) -> hydra.core.Type:
            match deannotate_term(rhs):
                case hydra.core.TermVariable(value=v):
                    return simplify_term(substitute_variable(var, v, body))
                
                case _:
                    return term2
        def for_lhs(lhs: hydra.core.Term, rhs: hydra.core.Term) -> hydra.core.Type:
            def for_fun(fun: hydra.core.Function) -> hydra.core.Type:
                match fun:
                    case hydra.core.FunctionLambda(value=l):
                        var = l.parameter
                        body = l.body
                        return hydra.lib.logic.if_else(hydra.lib.sets.member(var, free_variables_in_term(body)), (lambda : for_rhs(rhs, var, body)), (lambda : simplify_term(body)))
                    
                    case _:
                        return term2
            match deannotate_term(lhs):
                case hydra.core.TermFunction(value=fun):
                    return for_fun(fun)
                
                case _:
                    return term2
        def for_term(stripped: hydra.core.Term) -> hydra.core.Type:
            match stripped:
                case hydra.core.TermApplication(value=app):
                    lhs = app.function
                    rhs = app.argument
                    return for_lhs(lhs, rhs)
                
                case _:
                    return term2
        def stripped() -> hydra.core.Type:
            return deannotate_term(term2)
        return recurse(for_term(stripped()))
    return rewrite_term(cast(Callable[[Callable[[hydra.core.Term], hydra.core.Term], hydra.core.Term], hydra.core.Term], (lambda x1, x2: simplify(x1, x2))), term)

def substitute_type_variables(subst: FrozenDict[hydra.core.Name, hydra.core.Name], typ: hydra.core.Type) -> hydra.core.Type:
    r"""Substitute type variables in a type."""
    
    def replace(recurse: Callable[[hydra.core.Type], hydra.core.Type], typ2: hydra.core.Type) -> hydra.core.Type:
        match typ2:
            case hydra.core.TypeVariable(value=n):
                return cast(hydra.core.Type, hydra.core.TypeVariable(hydra.lib.maybes.from_maybe(n, hydra.lib.maps.lookup(n, subst))))
            
            case _:
                return recurse(typ2)
    return rewrite_type(replace, typ)

def subterms_with_accessors(v1: hydra.core.Term) -> frozenlist[tuple[hydra.accessors.TermAccessor, hydra.core.Term]]:
    r"""Find the children of a given term."""
    
    match v1:
        case hydra.core.TermAnnotated(value=at):
            return (cast(tuple[hydra.accessors.TermAccessor, hydra.core.Term], (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorAnnotatedBody()), at.body)),)
        
        case hydra.core.TermApplication(value=p):
            return (cast(tuple[hydra.accessors.TermAccessor, hydra.core.Term], (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorApplicationFunction()), p.function)), cast(tuple[hydra.accessors.TermAccessor, hydra.core.Term], (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorApplicationArgument()), p.argument)))
        
        case hydra.core.TermEither():
            return cast(frozenlist[tuple[hydra.accessors.TermAccessor, hydra.core.Term]], ())
        
        case hydra.core.TermFunction(value=v1):
            match v1:
                case hydra.core.FunctionElimination(value=v1):
                    match v1:
                        case hydra.core.EliminationUnion(value=cs):
                            return hydra.lib.lists.concat2(hydra.lib.maybes.maybe(cast(frozenlist[tuple[hydra.accessors.TermAccessor, hydra.core.Term]], ()), (lambda t: (cast(tuple[hydra.accessors.TermAccessor, hydra.core.Term], (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorUnionCasesDefault()), t)),)), cs.default), hydra.lib.lists.map((lambda f: cast(tuple[hydra.accessors.TermAccessor, hydra.core.Term], (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorUnionCasesBranch(f.name)), f.term))), cs.cases))
                        
                        case _:
                            return cast(frozenlist[tuple[hydra.accessors.TermAccessor, hydra.core.Term]], ())
                
                case hydra.core.FunctionLambda(value=l):
                    return (cast(tuple[hydra.accessors.TermAccessor, hydra.core.Term], (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorLambdaBody()), l.body)),)
                
                case _:
                    return cast(frozenlist[tuple[hydra.accessors.TermAccessor, hydra.core.Term]], ())
        
        case hydra.core.TermLet(value=lt):
            return hydra.lib.lists.cons(cast(tuple[hydra.accessors.TermAccessor, hydra.core.Term], (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorLetBody()), lt.body)), hydra.lib.lists.map((lambda b: cast(tuple[hydra.accessors.TermAccessor, hydra.core.Term], (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorLetBinding(b.name)), b.term))), lt.bindings))
        
        case hydra.core.TermList(value=l):
            return hydra.lib.lists.map((lambda e: cast(tuple[hydra.accessors.TermAccessor, hydra.core.Term], (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorListElement(0)), e))), l)
        
        case hydra.core.TermLiteral():
            return cast(frozenlist[tuple[hydra.accessors.TermAccessor, hydra.core.Term]], ())
        
        case hydra.core.TermMap(value=m):
            return hydra.lib.lists.concat(hydra.lib.lists.map((lambda p: (cast(tuple[hydra.accessors.TermAccessor, hydra.core.Term], (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorMapKey(0)), hydra.lib.pairs.first(p))), cast(tuple[hydra.accessors.TermAccessor, hydra.core.Term], (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorMapValue(0)), hydra.lib.pairs.second(p))))), hydra.lib.maps.to_list(m)))
        
        case hydra.core.TermMaybe(value=m2):
            return hydra.lib.maybes.maybe(cast(frozenlist[tuple[hydra.accessors.TermAccessor, hydra.core.Term]], ()), (lambda t: (cast(tuple[hydra.accessors.TermAccessor, hydra.core.Term], (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorMaybeTerm()), t)),)), m2)
        
        case hydra.core.TermPair():
            return cast(frozenlist[tuple[hydra.accessors.TermAccessor, hydra.core.Term]], ())
        
        case hydra.core.TermRecord(value=rt):
            return hydra.lib.lists.map((lambda f: cast(tuple[hydra.accessors.TermAccessor, hydra.core.Term], (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorRecordField(f.name)), f.term))), rt.fields)
        
        case hydra.core.TermSet(value=s):
            return hydra.lib.lists.map((lambda e: cast(tuple[hydra.accessors.TermAccessor, hydra.core.Term], (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorListElement(0)), e))), hydra.lib.sets.to_list(s))
        
        case hydra.core.TermTypeApplication(value=ta):
            return (cast(tuple[hydra.accessors.TermAccessor, hydra.core.Term], (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorTypeApplicationTerm()), ta.body)),)
        
        case hydra.core.TermTypeLambda(value=ta2):
            return (cast(tuple[hydra.accessors.TermAccessor, hydra.core.Term], (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorTypeLambdaBody()), ta2.body)),)
        
        case hydra.core.TermUnion(value=ut):
            return (cast(tuple[hydra.accessors.TermAccessor, hydra.core.Term], (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorInjectionTerm()), ut.field.term)),)
        
        case hydra.core.TermUnit():
            return cast(frozenlist[tuple[hydra.accessors.TermAccessor, hydra.core.Term]], ())
        
        case hydra.core.TermVariable():
            return cast(frozenlist[tuple[hydra.accessors.TermAccessor, hydra.core.Term]], ())
        
        case hydra.core.TermWrap(value=n):
            return (cast(tuple[hydra.accessors.TermAccessor, hydra.core.Term], (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorWrappedTerm()), n.body)),)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def term_dependency_names(binds: bool, with_prims: bool, with_noms: bool, term0: hydra.core.Term) -> frozenset[hydra.core.Name]:
    r"""Note: does not distinguish between bound and free variables; use freeVariablesInTerm for that."""
    
    def add_names(names: frozenset[hydra.core.Name], term: hydra.core.Term) -> frozenset[hydra.core.Name]:
        def nominal(name: hydra.core.Name) -> frozenset[hydra.core.Name]:
            return hydra.lib.logic.if_else(with_noms, (lambda : hydra.lib.sets.insert(name, names)), (lambda : names))
        def prim(name: hydra.core.Name) -> frozenset[hydra.core.Name]:
            return hydra.lib.logic.if_else(with_prims, (lambda : hydra.lib.sets.insert(name, names)), (lambda : names))
        def var(name: hydra.core.Name) -> frozenset[hydra.core.Name]:
            return hydra.lib.logic.if_else(binds, (lambda : hydra.lib.sets.insert(name, names)), (lambda : names))
        def _hoist_body_1(v1: hydra.core.Elimination) -> frozenset[hydra.core.Name]:
            match v1:
                case hydra.core.EliminationRecord(value=proj):
                    return nominal(proj.type_name)
                
                case hydra.core.EliminationUnion(value=case_stmt):
                    return nominal(case_stmt.type_name)
                
                case hydra.core.EliminationWrap(value=name):
                    return nominal(name)
                
                case _:
                    raise AssertionError("Unreachable: all variants handled")
        def _hoist_body_2(v1: hydra.core.Function) -> frozenset[hydra.core.Name]:
            match v1:
                case hydra.core.FunctionPrimitive(value=name):
                    return prim(name)
                
                case hydra.core.FunctionElimination(value=e):
                    return _hoist_body_1(e)
                
                case _:
                    return names
        match term:
            case hydra.core.TermFunction(value=f):
                return _hoist_body_2(f)
            
            case hydra.core.TermRecord(value=record):
                return nominal(record.type_name)
            
            case hydra.core.TermUnion(value=injection):
                return nominal(injection.type_name)
            
            case hydra.core.TermVariable(value=name):
                return var(name)
            
            case hydra.core.TermWrap(value=wrapped_term):
                return nominal(wrapped_term.type_name)
            
            case _:
                return names
    return fold_over_term(hydra.coders.TraversalOrder.PRE, add_names, cast(frozenset[hydra.core.Name], hydra.lib.sets.empty()), term0)

def to_short_names(original: frozenlist[hydra.core.Name]) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
    r"""Generate short names from a list of fully qualified names."""
    
    def add_name(acc: FrozenDict[str, frozenset[hydra.core.Name]], name: hydra.core.Name) -> FrozenDict[str, frozenset[hydra.core.Name]]:
        local = hydra.names.local_name_of(name)
        def group() -> frozenset[hydra.core.Name]:
            return hydra.lib.maybes.from_maybe(cast(frozenset[hydra.core.Name], hydra.lib.sets.empty()), hydra.lib.maps.lookup(local, acc))
        return hydra.lib.maps.insert(local, hydra.lib.sets.insert(name, group()), acc)
    def group_names_by_local(names: frozenlist[hydra.core.Name]) -> FrozenDict[str, frozenset[hydra.core.Name]]:
        return hydra.lib.lists.foldl(add_name, cast(FrozenDict[str, frozenset[hydra.core.Name]], hydra.lib.maps.empty()), names)
    def groups() -> FrozenDict[str, frozenset[hydra.core.Name]]:
        return group_names_by_local(original)
    def rename_group(local_names: tuple[str, frozenset[T0]]) -> frozenlist[tuple[T0, hydra.core.Name]]:
        def local() -> str:
            return hydra.lib.pairs.first(local_names)
        def names() -> frozenset[T0]:
            return hydra.lib.pairs.second(local_names)
        def range_from(start: int) -> frozenlist[int]:
            return hydra.lib.lists.cons(start, range_from(hydra.lib.math.add(start, 1)))
        def rename(name: T1, i: int) -> tuple[T1, hydra.core.Name]:
            return cast(tuple[T1, hydra.core.Name], (name, hydra.core.Name(hydra.lib.logic.if_else(hydra.lib.equality.gt(i, 1), (lambda : hydra.lib.strings.cat2(local(), hydra.lib.literals.show_int32(i))), (lambda : local())))))
        return hydra.lib.lists.zip_with(cast(Callable[[T0, int], tuple[T0, hydra.core.Name]], (lambda x1, x2: rename(x1, x2))), hydra.lib.sets.to_list(names()), range_from(1))
    return cast(FrozenDict[hydra.core.Name, hydra.core.Name], hydra.lib.maps.from_list(hydra.lib.lists.concat(hydra.lib.lists.map(cast(Callable[[tuple[str, frozenset[hydra.core.Name]]], frozenlist[tuple[hydra.core.Name, hydra.core.Name]]], (lambda x1: rename_group(x1))), hydra.lib.maps.to_list(groups())))))

def topological_sort_binding_map(binding_map: FrozenDict[hydra.core.Name, hydra.core.Term]) -> frozenlist[frozenlist[tuple[hydra.core.Name, hydra.core.Term]]]:
    r"""Topological sort of connected components, in terms of dependencies between variable/term binding pairs."""
    
    def bindings() -> frozenlist[tuple[hydra.core.Name, hydra.core.Term]]:
        return hydra.lib.maps.to_list(binding_map)
    def keys() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(hydra.lib.lists.map(cast(Callable[[tuple[hydra.core.Name, hydra.core.Term]], hydra.core.Name], (lambda x1: hydra.lib.pairs.first(x1))), bindings()))
    def has_type_annotation(term: hydra.core.Term) -> bool:
        match term:
            case hydra.core.TermAnnotated(value=at):
                return has_type_annotation(at.body)
            
            case _:
                return False
    def deps_of(name_and_term: tuple[T0, hydra.core.Term]) -> tuple[T0, frozenlist[hydra.core.Name]]:
        def name() -> T0:
            return hydra.lib.pairs.first(name_and_term)
        def term() -> hydra.core.Type:
            return hydra.lib.pairs.second(name_and_term)
        return cast(tuple[T0, frozenlist[hydra.core.Name]], (name(), hydra.lib.logic.if_else(has_type_annotation(term()), (lambda : cast(frozenlist[hydra.core.Name], ())), (lambda : hydra.lib.sets.to_list(hydra.lib.sets.intersection(keys(), free_variables_in_term(term())))))))
    def to_pair(name: hydra.core.Name) -> tuple[hydra.core.Name, hydra.core.Term]:
        return cast(tuple[hydra.core.Name, hydra.core.Term], (name, hydra.lib.maybes.from_maybe(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("Impossible!")))), hydra.lib.maps.lookup(name, binding_map))))
    return hydra.lib.lists.map((lambda v1: hydra.lib.lists.map(to_pair, v1)), hydra.sorting.topological_sort_components(hydra.lib.lists.map(cast(Callable[[tuple[hydra.core.Name, hydra.core.Term]], tuple[hydra.core.Name, frozenlist[hydra.core.Name]]], (lambda x1: deps_of(x1))), bindings())))

def topological_sort_bindings(els: frozenlist[hydra.core.Binding]) -> Either[frozenlist[frozenlist[hydra.core.Name]], frozenlist[hydra.core.Name]]:
    r"""Topological sort of elements based on their dependencies."""
    
    def adjlist(e: hydra.core.Binding) -> tuple[hydra.core.Name, frozenlist[hydra.core.Name]]:
        return cast(tuple[hydra.core.Name, frozenlist[hydra.core.Name]], (e.name, hydra.lib.sets.to_list(term_dependency_names(False, True, True, e.term))))
    return hydra.sorting.topological_sort(hydra.lib.lists.map(adjlist, els))

def type_names_in_type(typ0: hydra.core.Type) -> frozenset[hydra.core.Name]:
    def add_names(names: frozenset[hydra.core.Name], typ: hydra.core.Type) -> frozenset[hydra.core.Name]:
        match typ:
            case hydra.core.TypeRecord(value=row_type):
                tname = row_type.type_name
                return hydra.lib.sets.insert(tname, names)
            
            case hydra.core.TypeUnion(value=row_type2):
                tname = row_type2.type_name
                return hydra.lib.sets.insert(tname, names)
            
            case hydra.core.TypeWrap(value=wrapped_type):
                tname = wrapped_type.type_name
                return hydra.lib.sets.insert(tname, names)
            
            case _:
                return names
    return fold_over_type(hydra.coders.TraversalOrder.PRE, add_names, cast(frozenset[hydra.core.Name], hydra.lib.sets.empty()), typ0)

def type_dependency_names(with_schema: bool, typ: hydra.core.Type) -> frozenset[hydra.core.Name]:
    return hydra.lib.logic.if_else(with_schema, (lambda : hydra.lib.sets.union(free_variables_in_type(typ), type_names_in_type(typ))), (lambda : free_variables_in_type(typ)))

def unshadow_variables(term0: hydra.core.Term) -> hydra.core.Type:
    r"""Unshadow lambda-bound variables in a term."""
    
    def rewrite(recurse: Callable[[FrozenDict[hydra.core.Name, int], hydra.core.Term], tuple[FrozenDict[hydra.core.Name, int], hydra.core.Term]], m: FrozenDict[hydra.core.Name, int], term: hydra.core.Term) -> tuple[FrozenDict[hydra.core.Name, int], hydra.core.Term]:
        dflt = recurse(m, term)
        def _hoist_body_1(v1: hydra.core.Function) -> tuple[FrozenDict[hydra.core.Name, int], hydra.core.Term]:
            match v1:
                case hydra.core.FunctionLambda(value=l):
                    v = l.parameter
                    domain = l.domain
                    body = l.body
                    return cast(tuple[FrozenDict[hydra.core.Name, int], hydra.core.Term], (m, hydra.lib.maybes.maybe(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(v, domain, hydra.lib.pairs.second(rewrite(recurse, hydra.lib.maps.insert(v, 1, m), body))))))), (lambda i: (i2 := hydra.lib.math.add(i, 1), v2 := hydra.core.Name(hydra.lib.strings.cat2(v.value, hydra.lib.literals.show_int32(i2))), m2 := (lambda : hydra.lib.maps.insert(v, i2, m)), cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(v2, domain, hydra.lib.pairs.second(rewrite(recurse, m2(), body))))))))[3]), hydra.lib.maps.lookup(v, m))))
                
                case _:
                    return dflt
        match term:
            case hydra.core.TermFunction(value=f):
                return _hoist_body_1(f)
            
            case hydra.core.TermVariable(value=v):
                return cast(tuple[FrozenDict[hydra.core.Name, int], hydra.core.Term], (m, cast(hydra.core.Term, hydra.core.TermVariable(hydra.lib.maybes.maybe(v, (lambda i: hydra.lib.logic.if_else(hydra.lib.equality.equal(i, 1), (lambda : v), (lambda : hydra.core.Name(hydra.lib.strings.cat2(v.value, hydra.lib.literals.show_int32(i)))))), hydra.lib.maps.lookup(v, m))))))
            
            case _:
                return dflt
    return hydra.lib.pairs.second(rewrite_and_fold_term(rewrite, cast(FrozenDict[hydra.core.Name, int], hydra.lib.maps.empty()), term0))
