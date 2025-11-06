# Note: this is an automatically generated file. Do not edit.

r"""Utilities for type and term rewriting and analysis."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import Tuple, cast
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
import hydra.lib.sets
import hydra.lib.strings
import hydra.names
import hydra.sorting

def deannotate_and_detype_term(t: hydra.core.Term) -> hydra.core.Term:
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

def deannotate_term(t: hydra.core.Term) -> hydra.core.Term:
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
        def for_field(field: hydra.core.FieldType) -> hydra.core.FieldType:
            return hydra.core.FieldType(field.name, recurse(field.type))
        match typ:
            case hydra.core.TypeAnnotated(value=at):
                return cast(hydra.core.Type, hydra.core.TypeAnnotated(hydra.core.AnnotatedType(recurse(at.body), at.annotation)))
            
            case hydra.core.TypeApplication(value=app):
                return cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(recurse(app.function), recurse(app.argument))))
            
            case hydra.core.TypeEither(value=et):
                return cast(hydra.core.Type, hydra.core.TypeEither(hydra.core.EitherType(recurse(et.left), recurse(et.right))))
            
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
            
            case hydra.core.TypeProduct(value=ts):
                return cast(hydra.core.Type, hydra.core.TypeProduct(hydra.lib.lists.map(recurse, ts)))
            
            case hydra.core.TypeRecord(value=rt):
                return cast(hydra.core.Type, hydra.core.TypeRecord(hydra.core.RowType(rt.type_name, hydra.lib.lists.map(for_field, rt.fields))))
            
            case hydra.core.TypeSet(value=t3):
                return cast(hydra.core.Type, hydra.core.TypeSet(recurse(t3)))
            
            case hydra.core.TypeSum(value=ts2):
                return cast(hydra.core.Type, hydra.core.TypeSum(hydra.lib.lists.map(recurse, ts2)))
            
            case hydra.core.TypeUnion(value=rt2):
                return cast(hydra.core.Type, hydra.core.TypeUnion(hydra.core.RowType(rt2.type_name, hydra.lib.lists.map(for_field, rt2.fields))))
            
            case hydra.core.TypeUnit():
                return cast(hydra.core.Type, hydra.core.TypeUnit(None))
            
            case hydra.core.TypeVariable(value=v):
                return cast(hydra.core.Type, hydra.core.TypeVariable(v))
            
            case hydra.core.TypeWrap(value=wt):
                return cast(hydra.core.Type, hydra.core.TypeWrap(hydra.core.WrappedType(wt.type_name, recurse(wt.body))))
            
            case _:
                raise TypeError("Unsupported Type")
    def recurse(v1: hydra.core.Type) -> hydra.core.Type:
        return f((lambda v12: fsub(recurse, v12)), v1)
    return recurse(typ0)

def deannotate_type_recursive(typ: hydra.core.Type) -> hydra.core.Type:
    r"""Recursively strip all annotations from a type."""
    
    def strip[T0](recurse: Callable[[T0], hydra.core.Type], typ2: T0) -> hydra.core.Type:
        rewritten = recurse(typ2)
        match rewritten:
            case hydra.core.TypeAnnotated(value=at):
                return at.body
            
            case _:
                return rewritten
    return rewrite_type(cast(Callable[[Callable[[hydra.core.Type], hydra.core.Type], hydra.core.Type], hydra.core.Type], strip), typ)

def deannotate_type_scheme_recursive(ts: hydra.core.TypeScheme) -> hydra.core.TypeScheme:
    r"""Recursively strip all annotations from a type scheme."""
    
    vars = ts.variables
    typ = ts.type
    return hydra.core.TypeScheme(vars, deannotate_type_recursive(typ))

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

def rewrite_term(f: Callable[[Callable[[hydra.core.Term], hydra.core.Term], hydra.core.Term], hydra.core.Term], term0: hydra.core.Term) -> hydra.core.Term:
    def fsub(recurse: Callable[[hydra.core.Term], hydra.core.Term], term: hydra.core.Term) -> hydra.core.Term:
        def for_field(f2: hydra.core.Field) -> hydra.core.Field:
            return hydra.core.Field(f2.name, recurse(f2.term))
        def for_elimination(elm: hydra.core.Elimination) -> hydra.core.Elimination:
            match elm:
                case hydra.core.EliminationProduct(value=tp):
                    return cast(hydra.core.Elimination, hydra.core.EliminationProduct(tp))
                
                case hydra.core.EliminationRecord(value=p):
                    return cast(hydra.core.Elimination, hydra.core.EliminationRecord(p))
                
                case hydra.core.EliminationUnion(value=cs):
                    return cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(cs.type_name, hydra.lib.maybes.map(recurse, cs.default), hydra.lib.lists.map(for_field, cs.cases))))
                
                case hydra.core.EliminationWrap(value=name):
                    return cast(hydra.core.Elimination, hydra.core.EliminationWrap(name))
        def for_function(fun: hydra.core.Function) -> hydra.core.Function:
            match fun:
                case hydra.core.FunctionElimination(value=elm):
                    return cast(hydra.core.Function, hydra.core.FunctionElimination(for_elimination(elm)))
                
                case hydra.core.FunctionLambda(value=l):
                    return cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, l.domain, recurse(l.body))))
                
                case hydra.core.FunctionPrimitive(value=name):
                    return cast(hydra.core.Function, hydra.core.FunctionPrimitive(name))
        def for_let(lt: hydra.core.Let) -> hydra.core.Let:
            def map_binding(b: hydra.core.Binding) -> hydra.core.Binding:
                return hydra.core.Binding(b.name, recurse(b.term), b.type)
            return hydra.core.Let(hydra.lib.lists.map(map_binding, lt.bindings), recurse(lt.body))
        def for_map(m: FrozenDict[hydra.core.Term, hydra.core.Term]) -> FrozenDict[hydra.core.Term, hydra.core.Term]:
            def for_pair(p: Tuple[hydra.core.Term, hydra.core.Term]) -> Tuple[hydra.core.Term, hydra.core.Term]:
                return (recurse(p[0]), recurse(p[1]))
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
                return cast(hydra.core.Term, hydra.core.TermPair((recurse(p[0]), recurse(p[1]))))
            
            case hydra.core.TermProduct(value=tuple):
                return cast(hydra.core.Term, hydra.core.TermProduct(hydra.lib.lists.map(recurse, tuple)))
            
            case hydra.core.TermRecord(value=r):
                return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(r.type_name, hydra.lib.lists.map(for_field, r.fields))))
            
            case hydra.core.TermSet(value=s):
                return cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(hydra.lib.lists.map(recurse, hydra.lib.sets.to_list(s)))))
            
            case hydra.core.TermSum(value=s2):
                return cast(hydra.core.Term, hydra.core.TermSum(hydra.core.Sum(s2.index, s2.size, recurse(s2.term))))
            
            case hydra.core.TermTypeApplication(value=tt):
                return cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(recurse(tt.body), tt.type)))
            
            case hydra.core.TermTypeLambda(value=ta):
                return cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(ta.parameter, recurse(ta.body))))
            
            case hydra.core.TermUnion(value=i):
                return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(i.type_name, for_field(i.field))))
            
            case hydra.core.TermUnit():
                return cast(hydra.core.Term, hydra.core.TermUnit(None))
            
            case hydra.core.TermVariable(value=v2):
                return cast(hydra.core.Term, hydra.core.TermVariable(v2))
            
            case hydra.core.TermWrap(value=wt):
                return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(wt.type_name, recurse(wt.body))))
    def recurse(v1: hydra.core.Term) -> hydra.core.Term:
        return f((lambda v12: fsub(recurse, v12)), v1)
    return recurse(term0)

def substitute_variables(subst: FrozenDict[hydra.core.Name, hydra.core.Name], term: hydra.core.Term) -> hydra.core.Term:
    r"""Substitute multiple variables in a term."""
    
    def replace(recurse: Callable[[hydra.core.Term], hydra.core.Term], term2: hydra.core.Term) -> hydra.core.Term:
        match term2:
            case hydra.core.TermVariable(value=n):
                return cast(hydra.core.Term, hydra.core.TermVariable(hydra.lib.maybes.from_maybe(n, hydra.lib.maps.lookup(n, subst))))
            
            case hydra.core.TermFunction(value=v1):
                match v1:
                    case hydra.core.FunctionLambda(value=l):
                        return hydra.lib.maybes.maybe(recurse(term2), (lambda _: term2), hydra.lib.maps.lookup(l.parameter, subst))
                    
                    case _:
                        return recurse(term2)
            
            case _:
                return recurse(term2)
    return rewrite_term(replace, term)

def flatten_let_terms(term: hydra.core.Term) -> hydra.core.Term:
    r"""Flatten nested let expressions."""
    
    def rewrite_binding(binding: hydra.core.Binding) -> Tuple[hydra.core.Binding, frozenlist[hydra.core.Binding]]:
        key0 = binding.name
        val0 = binding.term
        t = binding.type
        match val0:
            case hydra.core.TermAnnotated(value=at):
                val1 = at.body
                ann = at.annotation
                recursive = rewrite_binding(hydra.core.Binding(key0, val1, t))
                inner_binding = recursive[0]
                deps = recursive[1]
                val2 = inner_binding.term
                return (hydra.core.Binding(key0, cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(val2, ann))), t), deps)
            
            case hydra.core.TermLet(value=inner_let):
                bindings1 = inner_let.bindings
                body1 = inner_let.body
                prefix = hydra.lib.strings.cat2(key0.value, "_")
                def qualify(n: hydra.core.Name) -> hydra.core.Name:
                    return hydra.core.Name(hydra.lib.strings.cat2(prefix, n.value))
                def to_subst_pair(b: hydra.core.Binding) -> Tuple[hydra.core.Name, hydra.core.Name]:
                    return (b.name, qualify(b.name))
                subst = cast(FrozenDict[hydra.core.Name, hydra.core.Name], hydra.lib.maps.from_list(hydra.lib.lists.map(to_subst_pair, bindings1)))
                def replace_vars(v1: hydra.core.Term) -> hydra.core.Term:
                    return substitute_variables(subst, v1)
                new_body = replace_vars(body1)
                def new_binding(b: hydra.core.Binding) -> hydra.core.Binding:
                    return hydra.core.Binding(qualify(b.name), replace_vars(b.term), b.type)
                return (hydra.core.Binding(key0, new_body, t), hydra.lib.lists.map(new_binding, bindings1))
            
            case _:
                return (hydra.core.Binding(key0, val0, t), cast(frozenlist[hydra.core.Binding], ()))
    def flatten[T0](recurse: Callable[[T0], hydra.core.Term], term2: T0) -> hydra.core.Term:
        rewritten = recurse(term2)
        match rewritten:
            case hydra.core.TermLet(value=lt):
                bindings = lt.bindings
                body = lt.body
                def for_result[T1](hr: Tuple[T1, frozenlist[T1]]) -> frozenlist[T1]:
                    return hydra.lib.lists.cons(hr[0], hr[1])
                new_bindings = hydra.lib.lists.concat(hydra.lib.lists.map((lambda arg_: for_result(rewrite_binding(arg_))), bindings))
                return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(new_bindings, body)))
            
            case _:
                return rewritten
    return rewrite_term(cast(Callable[[Callable[[hydra.core.Term], hydra.core.Term], hydra.core.Term], hydra.core.Term], flatten), term)

def subterms(v1: hydra.core.Term) -> frozenlist[hydra.core.Term]:
    r"""Find the children of a given term."""
    
    match v1:
        case hydra.core.TermAnnotated(value=at):
            return (at.body,)
        
        case hydra.core.TermApplication(value=p):
            return (p.function, p.argument)
        
        case hydra.core.TermEither(value=e):
            return hydra.lib.eithers.either((lambda l: (l,)), (lambda r: (r,)), e)
        
        case hydra.core.TermFunction(value=v12):
            match v12:
                case hydra.core.FunctionElimination(value=v13):
                    match v13:
                        case hydra.core.EliminationUnion(value=cs):
                            return hydra.lib.lists.concat2(hydra.lib.maybes.maybe(cast(frozenlist[hydra.core.Term], ()), (lambda t: (t,)), cs.default), hydra.lib.lists.map((lambda v14: v14.term), cs.cases))
                        
                        case _:
                            return cast(frozenlist[hydra.core.Term], ())
                
                case hydra.core.FunctionLambda(value=l):
                    return (l.body,)
                
                case _:
                    return cast(frozenlist[hydra.core.Term], ())
        
        case hydra.core.TermLet(value=lt):
            return hydra.lib.lists.cons(lt.body, hydra.lib.lists.map((lambda v12: v12.term), lt.bindings))
        
        case hydra.core.TermList(value=l):
            return l
        
        case hydra.core.TermLiteral():
            return cast(frozenlist[hydra.core.Term], ())
        
        case hydra.core.TermMap(value=m):
            return hydra.lib.lists.concat(hydra.lib.lists.map((lambda p: (p[0], p[1])), hydra.lib.maps.to_list(m)))
        
        case hydra.core.TermMaybe(value=m2):
            return hydra.lib.maybes.maybe(cast(frozenlist[hydra.core.Term], ()), (lambda t: (t,)), m2)
        
        case hydra.core.TermPair(value=p2):
            return (p2[0], p2[1])
        
        case hydra.core.TermProduct(value=tuple):
            return tuple
        
        case hydra.core.TermRecord(value=rt):
            return hydra.lib.lists.map((lambda v12: v12.term), rt.fields)
        
        case hydra.core.TermSet(value=l2):
            return hydra.lib.sets.to_list(l2)
        
        case hydra.core.TermSum(value=st):
            return (st.term,)
        
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

def fold_over_term[T0](order: hydra.coders.TraversalOrder, fld: Callable[[T0, hydra.core.Term], T0], b0: T0, term: hydra.core.Term) -> T0:
    match order:
        case hydra.coders.TraversalOrderPre():
            return hydra.lib.lists.foldl((lambda v1, v2: fold_over_term(order, fld, v1, v2)), fld(b0, term), subterms(term))
        
        case hydra.coders.TraversalOrderPost():
            return fld(hydra.lib.lists.foldl((lambda v1, v2: fold_over_term(order, fld, v1, v2)), b0, subterms(term)), term)

def subtypes(v1: hydra.core.Type) -> frozenlist[hydra.core.Type]:
    r"""Find the children of a given type expression."""
    
    match v1:
        case hydra.core.TypeAnnotated(value=at):
            return (at.body,)
        
        case hydra.core.TypeApplication(value=at2):
            return (at2.function, at2.argument)
        
        case hydra.core.TypeEither(value=et):
            return (et.left, et.right)
        
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
        
        case hydra.core.TypeProduct(value=pt):
            return pt
        
        case hydra.core.TypeRecord(value=rt):
            return hydra.lib.lists.map((lambda v12: v12.type), rt.fields)
        
        case hydra.core.TypeSet(value=st):
            return (st,)
        
        case hydra.core.TypeSum(value=st2):
            return st2
        
        case hydra.core.TypeUnion(value=rt2):
            return hydra.lib.lists.map((lambda v12: v12.type), rt2.fields)
        
        case hydra.core.TypeUnit():
            return cast(frozenlist[hydra.core.Type], ())
        
        case hydra.core.TypeVariable():
            return cast(frozenlist[hydra.core.Type], ())
        
        case hydra.core.TypeWrap(value=nt):
            return (nt.body,)
        
        case _:
            raise TypeError("Unsupported Type")

def fold_over_type[T0](order: hydra.coders.TraversalOrder, fld: Callable[[T0, hydra.core.Type], T0], b0: T0, typ: hydra.core.Type) -> T0:
    match order:
        case hydra.coders.TraversalOrderPre():
            return hydra.lib.lists.foldl((lambda v1, v2: fold_over_type(order, fld, v1, v2)), fld(b0, typ), subtypes(typ))
        
        case hydra.coders.TraversalOrderPost():
            return fld(hydra.lib.lists.foldl((lambda v1, v2: fold_over_type(order, fld, v1, v2)), b0, subtypes(typ)), typ)

def free_variables_in_type(typ: hydra.core.Type) -> frozenset[hydra.core.Name]:
    r"""Find the free variables (i.e. variables not bound by a lambda or let) in a type."""
    
    dflt_vars = hydra.lib.lists.foldl((lambda s, t: hydra.lib.sets.union(s, free_variables_in_type(t))), cast(frozenset[hydra.core.Name], hydra.lib.sets.empty()), subtypes(typ))
    match typ:
        case hydra.core.TypeForall(value=lt):
            return hydra.lib.sets.delete(lt.parameter, free_variables_in_type(lt.body))
        
        case hydra.core.TypeVariable(value=v):
            return hydra.lib.sets.singleton(v)
        
        case _:
            return dflt_vars

def free_type_variables_in_term(term0: hydra.core.Term) -> frozenset[hydra.core.Name]:
    r"""Get the set of free type variables in a term (including schema names, where they appear in type annotations). In this context, only the type schemes of let bindings can bind type variables; type lambdas do not."""
    
    def all_of[T0](sets: frozenlist[frozenset[T0]]) -> frozenset[T0]:
        return hydra.lib.lists.foldl(cast(Callable[[frozenset[T0], frozenset[T0]], frozenset[T0]], hydra.lib.sets.union), cast(frozenset[T0], hydra.lib.sets.empty()), sets)
    def try_type(tvars: frozenset[hydra.core.Name], typ: hydra.core.Type) -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.difference(free_variables_in_type(typ), tvars)
    def get_all(vars: frozenset[hydra.core.Name], term: hydra.core.Term) -> frozenset[hydra.core.Name]:
        def recurse(v1: hydra.core.Term) -> frozenset[hydra.core.Name]:
            return get_all(vars, v1)
        dflt = all_of(hydra.lib.lists.map(recurse, subterms(term)))
        match term:
            case hydra.core.TermFunction(value=f):
                match f:
                    case hydra.core.FunctionElimination(value=e):
                        match e:
                            case hydra.core.EliminationProduct(value=tp):
                                return hydra.lib.maybes.maybe(cast(frozenset[hydra.core.Name], hydra.lib.sets.empty()), (lambda typs: all_of(hydra.lib.lists.map((lambda v1: try_type(vars, v1)), typs))), tp.domain)
                            
                            case _:
                                return dflt
                    
                    case hydra.core.FunctionLambda(value=l):
                        domt = hydra.lib.maybes.maybe(cast(frozenset[hydra.core.Name], hydra.lib.sets.empty()), (lambda v1: try_type(vars, v1)), l.domain)
                        return hydra.lib.sets.union(domt, recurse(l.body))
                    
                    case _:
                        return dflt
            
            case hydra.core.TermLet(value=l):
                def for_binding(b: hydra.core.Binding) -> frozenset[hydra.core.Name]:
                    new_vars = hydra.lib.maybes.maybe(vars, (lambda ts: hydra.lib.sets.union(vars, hydra.lib.sets.from_list(ts.variables))), b.type)
                    return hydra.lib.sets.union(get_all(new_vars, b.term), hydra.lib.maybes.maybe(cast(frozenset[hydra.core.Name], hydra.lib.sets.empty()), (lambda ts: try_type(new_vars, ts.type)), b.type))
                return hydra.lib.sets.union(all_of(hydra.lib.lists.map(for_binding, l.bindings)), recurse(l.body))
            
            case hydra.core.TermTypeApplication(value=tt):
                return hydra.lib.sets.union(try_type(vars, tt.type), recurse(tt.body))
            
            case hydra.core.TermTypeLambda(value=tl):
                return hydra.lib.sets.union(try_type(vars, cast(hydra.core.Type, hydra.core.TypeVariable(tl.parameter))), recurse(tl.body))
            
            case _:
                return dflt
    return get_all(cast(frozenset[hydra.core.Name], hydra.lib.sets.empty()), term0)

def free_variables_in_term(term: hydra.core.Term) -> frozenset[hydra.core.Name]:
    r"""Find the free variables (i.e. variables not bound by a lambda or let) in a term."""
    
    dflt_vars = hydra.lib.lists.foldl((lambda s, t: hydra.lib.sets.union(s, free_variables_in_term(t))), cast(frozenset[hydra.core.Name], hydra.lib.sets.empty()), subterms(term))
    match term:
        case hydra.core.TermFunction(value=v1):
            match v1:
                case hydra.core.FunctionLambda(value=l):
                    return hydra.lib.sets.delete(l.parameter, free_variables_in_term(l.body))
                
                case _:
                    return dflt_vars
        
        case hydra.core.TermLet(value=l):
            return hydra.lib.sets.difference(dflt_vars, hydra.lib.sets.from_list(hydra.lib.lists.map((lambda v1: v1.name), l.bindings)))
        
        case hydra.core.TermVariable(value=v):
            return hydra.lib.sets.singleton(v)
        
        case _:
            return dflt_vars

def free_variables_in_type_ordered(typ: hydra.core.Type) -> frozenlist[hydra.core.Name]:
    r"""Find the free variables in a type in deterministic left-to-right order."""
    
    def collect_vars(bound_vars: frozenset[hydra.core.Name], t: hydra.core.Type) -> frozenlist[hydra.core.Name]:
        match t:
            case hydra.core.TypeVariable(value=v):
                return hydra.lib.logic.if_else(hydra.lib.sets.member(v, bound_vars), cast(frozenlist[hydra.core.Name], ()), (v,))
            
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
    return fold_over_type(cast(hydra.coders.TraversalOrder, hydra.coders.TraversalOrderPre(None)), helper, cast(frozenset[hydra.core.Name], hydra.lib.sets.empty()), typ)

def free_variables_in_type_scheme_simple(ts: hydra.core.TypeScheme) -> frozenset[hydra.core.Name]:
    r"""Find free variables in a type scheme (simple version)."""
    
    vars = ts.variables
    t = ts.type
    return hydra.lib.sets.difference(free_variables_in_type_simple(t), hydra.lib.sets.from_list(vars))

def rewrite_type_m[T0](f: Callable[[
  Callable[[hydra.core.Type], hydra.compute.Flow[T0, hydra.core.Type]],
  hydra.core.Type], hydra.compute.Flow[T0, hydra.core.Type]], typ0: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.core.Type]:
    def fsub[T1](recurse: Callable[[hydra.core.Type], hydra.compute.Flow[T1, hydra.core.Type]], typ: hydra.core.Type) -> hydra.compute.Flow[T1, hydra.core.Type]:
        match typ:
            case hydra.core.TypeAnnotated(value=at):
                return hydra.lib.flows.bind(recurse(at.body), (lambda t: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeAnnotated(hydra.core.AnnotatedType(t, at.annotation))))))
            
            case hydra.core.TypeApplication(value=at2):
                return hydra.lib.flows.bind(recurse(at2.function), (lambda lhs: hydra.lib.flows.bind(recurse(at2.argument), (lambda rhs: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(lhs, rhs))))))))
            
            case hydra.core.TypeEither(value=et):
                return hydra.lib.flows.bind(recurse(et.left), (lambda left: hydra.lib.flows.bind(recurse(et.right), (lambda right: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeEither(hydra.core.EitherType(left, right))))))))
            
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
            
            case hydra.core.TypeProduct(value=types):
                return hydra.lib.flows.bind(hydra.lib.flows.map_list(recurse, types), (lambda rtypes: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeProduct(rtypes)))))
            
            case hydra.core.TypeRecord(value=rt):
                name = rt.type_name
                fields = rt.fields
                def for_field(f2: hydra.core.FieldType) -> hydra.compute.Flow[T1, hydra.core.FieldType]:
                    return hydra.lib.flows.bind(recurse(f2.type), (lambda t: hydra.lib.flows.pure(hydra.core.FieldType(f2.name, t))))
                return hydra.lib.flows.bind(hydra.lib.flows.map_list(for_field, fields), (lambda rfields: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeRecord(hydra.core.RowType(name, rfields))))))
            
            case hydra.core.TypeSet(value=t3):
                return hydra.lib.flows.bind(recurse(t3), (lambda rt: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeSet(rt)))))
            
            case hydra.core.TypeSum(value=types2):
                return hydra.lib.flows.bind(hydra.lib.flows.map_list(recurse, types2), (lambda rtypes: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeSum(rtypes)))))
            
            case hydra.core.TypeUnion(value=rt2):
                name = rt2.type_name
                fields = rt2.fields
                def for_field(f2: hydra.core.FieldType) -> hydra.compute.Flow[T1, hydra.core.FieldType]:
                    return hydra.lib.flows.bind(recurse(f2.type), (lambda t: hydra.lib.flows.pure(hydra.core.FieldType(f2.name, t))))
                return hydra.lib.flows.bind(hydra.lib.flows.map_list(for_field, fields), (lambda rfields: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeUnion(hydra.core.RowType(name, rfields))))))
            
            case hydra.core.TypeUnit():
                return hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeUnit(None)))
            
            case hydra.core.TypeVariable(value=v):
                return hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeVariable(v)))
            
            case hydra.core.TypeWrap(value=wt):
                return hydra.lib.flows.bind(recurse(wt.body), (lambda t: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeWrap(hydra.core.WrappedType(wt.type_name, t))))))
            
            case _:
                raise TypeError("Unsupported Type")
    def recurse(v1: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.core.Type]:
        return f((lambda v12: fsub(recurse, v12)), v1)
    return recurse(typ0)

def inline_type[T0](schema: FrozenDict[hydra.core.Name, hydra.core.Type], typ: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.core.Type]:
    def f[T1](recurse: Callable[[T1], hydra.compute.Flow[T0, hydra.core.Type]], typ2: T1) -> hydra.compute.Flow[T0, hydra.core.Type]:
        def after_recurse(tr: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.core.Type]:
            match tr:
                case hydra.core.TypeVariable(value=v):
                    return hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat2("No such type in schema: ", v.value)), (lambda v1: inline_type(schema, v1)), hydra.lib.maps.lookup(v, schema))
                
                case _:
                    return hydra.lib.flows.pure(tr)
        return hydra.lib.flows.bind(recurse(typ2), (lambda tr: after_recurse(tr)))
    return rewrite_type_m(cast(Callable[[
      Callable[[hydra.core.Type], hydra.compute.Flow[T0, hydra.core.Type]],
      hydra.core.Type], hydra.compute.Flow[T0, hydra.core.Type]], f), typ)

def is_free_variable_in_term(v: hydra.core.Name, term: hydra.core.Term) -> bool:
    r"""Check whether a variable is free (not bound) in a term."""
    
    return hydra.lib.logic.not_(hydra.lib.sets.member(v, free_variables_in_term(term)))

def is_lambda(term: hydra.core.Term) -> bool:
    r"""Check whether a term is a lambda, possibly nested within let and/or annotation terms."""
    
    match deannotate_term(term):
        case hydra.core.TermFunction(value=v1):
            match v1:
                case hydra.core.FunctionLambda():
                    return True
                
                case _:
                    return False
        
        case hydra.core.TermLet(value=lt):
            return is_lambda(lt.body)
        
        case _:
            return False

def lift_lambda_above_let(term0: hydra.core.Term) -> hydra.core.Term:
    r"""Rewrite terms like `let foo = bar in λx.baz` to `λx.let foo = bar in baz`, lifting lambda-bound variables above let-bound variables, recursively. This is helpful for targets such as Python."""
    
    def rewrite(recurse: Callable[[hydra.core.Term], hydra.core.Term], term: hydra.core.Term) -> hydra.core.Term:
        def rewrite_binding(b: hydra.core.Binding) -> hydra.core.Binding:
            return hydra.core.Binding(b.name, rewrite(recurse, b.term), b.type)
        def rewrite_bindings(bs: frozenlist[hydra.core.Binding]) -> frozenlist[hydra.core.Binding]:
            return hydra.lib.lists.map(rewrite_binding, bs)
        def dig_for_lambdas(original: hydra.core.Term, cons: Callable[[hydra.core.Term], hydra.core.Term], term2: hydra.core.Term) -> hydra.core.Term:
            match term2:
                case hydra.core.TermAnnotated(value=at):
                    return dig_for_lambdas(original, (lambda t: cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cons(t), at.annotation)))), at.body)
                
                case hydra.core.TermFunction(value=f):
                    match f:
                        case hydra.core.FunctionLambda(value=l):
                            return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, l.domain, dig_for_lambdas(cons(l.body), (lambda t: cons(t)), l.body))))))
                        
                        case _:
                            return recurse(original)
                
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

def normalize_type_variables_in_term(term: hydra.core.Term) -> hydra.core.Term:
    r"""Recursively replace the type variables of let bindings with the systematic type variables t0, t1, t2, ..."""
    
    def replace_name[T0](subst: FrozenDict[T0, T0], v: T0) -> T0:
        return hydra.lib.maybes.from_maybe(v, hydra.lib.maps.lookup(v, subst))
    def subst_type(subst: FrozenDict[hydra.core.Name, hydra.core.Name], typ: hydra.core.Type) -> hydra.core.Type:
        def rewrite(recurse: Callable[[hydra.core.Type], hydra.core.Type], typ2: hydra.core.Type) -> hydra.core.Type:
            match typ2:
                case hydra.core.TypeVariable(value=v):
                    return cast(hydra.core.Type, hydra.core.TypeVariable(replace_name(subst, v)))
                
                case _:
                    return recurse(typ2)
        return rewrite_type(rewrite, typ)
    def rewrite_with_subst(state: Tuple[Tuple[FrozenDict[hydra.core.Name, hydra.core.Name], frozenset[hydra.core.Name]], int], term0: hydra.core.Term) -> hydra.core.Term:
        sb = state[0]
        next = state[1]
        subst = sb[0]
        bound_vars = sb[1]
        def rewrite(recurse: Callable[[hydra.core.Term], hydra.core.Term], term2: hydra.core.Term) -> hydra.core.Term:
            match term2:
                case hydra.core.TermFunction(value=v1):
                    match v1:
                        case hydra.core.FunctionElimination(value=v12):
                            match v12:
                                case hydra.core.EliminationProduct(value=tproj):
                                    domain = tproj.domain
                                    return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationProduct(hydra.core.TupleProjection(tproj.arity, tproj.index, hydra.lib.maybes.map((lambda types: hydra.lib.lists.map((lambda v13: subst_type(subst, v13)), types)), domain))))))))
                                
                                case _:
                                    return recurse(term2)
                        
                        case hydra.core.FunctionLambda(value=l):
                            domain = l.domain
                            return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, hydra.lib.maybes.map((lambda v12: subst_type(subst, v12)), domain), rewrite_with_subst(((subst, bound_vars), next), l.body))))))
                        
                        case _:
                            return recurse(term2)
                
                case hydra.core.TermLet(value=lt):
                    bindings0 = lt.bindings
                    body0 = lt.body
                    def step(acc: frozenlist[hydra.core.Binding], bs: frozenlist[hydra.core.Binding]) -> frozenlist[hydra.core.Binding]:
                        b = hydra.lib.lists.head(bs)
                        tl = hydra.lib.lists.tail(bs)
                        def no_type() -> frozenlist[hydra.core.Binding]:
                            new_val = rewrite_with_subst(((subst, bound_vars), next), b.term)
                            b1 = hydra.core.Binding(b.name, new_val, cast(Maybe[hydra.core.TypeScheme], Nothing()))
                            return step(hydra.lib.lists.cons(b1, acc), tl)
                        def with_type(ts: hydra.core.TypeScheme) -> frozenlist[hydra.core.Binding]:
                            vars = ts.variables
                            typ = ts.type
                            k = hydra.lib.lists.length(vars)
                            def gen(i: int, rem: int, acc2: frozenlist[hydra.core.Name]) -> frozenlist[hydra.core.Name]:
                                ti = hydra.core.Name(hydra.lib.strings.cat2("t", hydra.lib.literals.show_int32(hydra.lib.math.add(next, i))))
                                return hydra.lib.logic.if_else(hydra.lib.equality.equal(rem, 0), hydra.lib.lists.reverse(acc2), gen(hydra.lib.math.add(i, 1), hydra.lib.math.sub(rem, 1), hydra.lib.lists.cons(ti, acc2)))
                            new_vars = gen(0, k, cast(frozenlist[hydra.core.Name], ()))
                            new_subst = hydra.lib.maps.union(cast(FrozenDict[hydra.core.Name, hydra.core.Name], hydra.lib.maps.from_list(hydra.lib.lists.zip(vars, new_vars))), subst)
                            new_bound = hydra.lib.sets.union(bound_vars, hydra.lib.sets.from_list(new_vars))
                            new_val = rewrite_with_subst(((new_subst, new_bound), hydra.lib.math.add(next, k)), b.term)
                            b1 = hydra.core.Binding(b.name, new_val, cast(Maybe[hydra.core.TypeScheme], Just(hydra.core.TypeScheme(new_vars, subst_type(new_subst, typ)))))
                            return step(hydra.lib.lists.cons(b1, acc), tl)
                        return hydra.lib.logic.if_else(hydra.lib.lists.null(bs), hydra.lib.lists.reverse(acc), hydra.lib.maybes.maybe(no_type(), (lambda ts: with_type(ts)), b.type))
                    bindings1 = step(cast(frozenlist[hydra.core.Binding], ()), bindings0)
                    return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(bindings1, rewrite_with_subst(((subst, bound_vars), next), body0))))
                
                case hydra.core.TermTypeApplication(value=tt):
                    return cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(rewrite_with_subst(((subst, bound_vars), next), tt.body), subst_type(subst, tt.type))))
                
                case hydra.core.TermTypeLambda(value=ta):
                    return cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(replace_name(subst, ta.parameter), rewrite_with_subst(((subst, bound_vars), next), ta.body))))
                
                case _:
                    return recurse(term2)
        return rewrite_term(rewrite, term0)
    return rewrite_with_subst(((cast(FrozenDict[hydra.core.Name, hydra.core.Name], hydra.lib.maps.empty()), cast(frozenset[hydra.core.Name], hydra.lib.sets.empty())), 0), term)

def remove_term_annotations(term: hydra.core.Term) -> hydra.core.Term:
    r"""Recursively remove term annotations, including within subterms."""
    
    def remove(recurse: Callable[[hydra.core.Term], hydra.core.Term], term2: hydra.core.Term) -> hydra.core.Term:
        rewritten = recurse(term2)
        match term2:
            case hydra.core.TermAnnotated(value=at):
                return at.body
            
            case _:
                return rewritten
    return rewrite_term(remove, term)

def remove_type_annotations(typ: hydra.core.Type) -> hydra.core.Type:
    r"""Recursively remove type annotations, including within subtypes."""
    
    def remove[T0](recurse: Callable[[T0], hydra.core.Type], typ2: T0) -> hydra.core.Type:
        rewritten = recurse(typ2)
        match rewritten:
            case hydra.core.TypeAnnotated(value=at):
                return at.body
            
            case _:
                return rewritten
    return rewrite_type(cast(Callable[[Callable[[hydra.core.Type], hydra.core.Type], hydra.core.Type], hydra.core.Type], remove), typ)

def remove_types_from_term(term: hydra.core.Term) -> hydra.core.Term:
    r"""Strip type annotations from terms while preserving other annotations."""
    
    def strip[T0](recurse: Callable[[T0], hydra.core.Term], term2: T0) -> hydra.core.Term:
        rewritten = recurse(term2)
        def strip_binding(b: hydra.core.Binding) -> hydra.core.Binding:
            return hydra.core.Binding(b.name, b.term, cast(Maybe[hydra.core.TypeScheme], Nothing()))
        match rewritten:
            case hydra.core.TermFunction(value=f):
                match f:
                    case hydra.core.FunctionElimination(value=e):
                        match e:
                            case hydra.core.EliminationProduct(value=tp):
                                return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationProduct(hydra.core.TupleProjection(tp.arity, tp.index, cast(Maybe[frozenlist[hydra.core.Type]], Nothing()))))))))
                            
                            case _:
                                return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(e))))
                    
                    case hydra.core.FunctionLambda(value=l):
                        return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, cast(Maybe[hydra.core.Type], Nothing()), l.body)))))
                    
                    case _:
                        return cast(hydra.core.Term, hydra.core.TermFunction(f))
            
            case hydra.core.TermLet(value=lt):
                return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.map(strip_binding, lt.bindings), lt.body)))
            
            case hydra.core.TermTypeApplication(value=tt):
                return tt.body
            
            case hydra.core.TermTypeLambda(value=ta):
                return ta.body
            
            case _:
                return rewritten
    return rewrite_term(cast(Callable[[Callable[[hydra.core.Term], hydra.core.Term], hydra.core.Term], hydra.core.Term], strip), term)

def replace_free_term_variable(vold: hydra.core.Name, tnew: hydra.core.Term, term: hydra.core.Term) -> hydra.core.Term:
    r"""Replace a free variable in a term."""
    
    def rewrite(recurse: Callable[[hydra.core.Term], hydra.core.Term], t: hydra.core.Term) -> hydra.core.Term:
        match t:
            case hydra.core.TermFunction(value=f):
                match f:
                    case hydra.core.FunctionLambda(value=l):
                        v = l.parameter
                        return hydra.lib.logic.if_else(hydra.lib.equality.equal(v, vold), t, recurse(t))
                    
                    case _:
                        return recurse(t)
            
            case hydra.core.TermVariable(value=v):
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(v, vold), tnew, cast(hydra.core.Term, hydra.core.TermVariable(v)))
            
            case _:
                return recurse(t)
    return rewrite_term(rewrite, term)

def replace_free_type_variable(v: hydra.core.Name, rep: hydra.core.Type, typ: hydra.core.Type) -> hydra.core.Type:
    r"""Replace free occurrences of a name in a type."""
    
    def map_expr(recurse: Callable[[hydra.core.Type], hydra.core.Type], t: hydra.core.Type) -> hydra.core.Type:
        match t:
            case hydra.core.TypeForall(value=ft):
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(v, ft.parameter), t, cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(ft.parameter, recurse(ft.body)))))
            
            case hydra.core.TypeVariable(value=v_):
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(v, v_), rep, t)
            
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
                    return hydra.lib.logic.if_else(hydra.lib.lists.null(ts.variables), for_mono(t), dflt)
                return hydra.lib.maybes.maybe(dflt, (lambda ts: for_type_scheme(ts)), hydra.lib.maps.lookup(v, types))
            
            case hydra.core.TypeWrap():
                return typ
            
            case _:
                return dflt
    return rewrite_type(rewrite, typ0)

def rewrite_and_fold_term[T0](f: Callable[[
  Callable[[T0, hydra.core.Term], Tuple[T0, hydra.core.Term]],
  T0,
  hydra.core.Term], Tuple[T0, hydra.core.Term]], term0: T0, v1: hydra.core.Term) -> Tuple[T0, hydra.core.Term]:
    def fsub[T1](recurse: Callable[[T1, hydra.core.Term], Tuple[T1, hydra.core.Term]], val0: T1, term02: hydra.core.Term) -> Tuple[T1, hydra.core.Term]:
        def for_single[T2, T3, T4, T5, T6](rec: Callable[[T2, T3], Tuple[T4, T5]], cons: Callable[[T5], T6], val: T2, term: T3) -> Tuple[T4, T6]:
            r = rec(val, term)
            return (r[0], cons(r[1]))
        def for_many[T2, T3, T4, T5](rec: Callable[[T2, T3], Tuple[T2, T4]], cons: Callable[[frozenlist[T4]], T5], val: T2, els: frozenlist[T3]) -> Tuple[T2, T5]:
            rr = hydra.lib.lists.foldl((lambda r, el: (r2 := rec(r[0], el), (r2[0], hydra.lib.lists.cons(r2[1], r[1])))[1]), (val, cast(frozenlist[T4], ())), els)
            return (rr[0], cons(hydra.lib.lists.reverse(rr[1])))
        def for_field(val: T1, field: hydra.core.Field) -> Tuple[T1, hydra.core.Field]:
            r = recurse(val, field.term)
            return (r[0], hydra.core.Field(field.name, r[1]))
        def for_fields(v1: T1, v2: frozenlist[hydra.core.Field]) -> Tuple[T1, frozenlist[hydra.core.Field]]:
            return for_many(for_field, (lambda x: x), v1, v2)
        def for_pair(val: T1, kv: Tuple[hydra.core.Term, hydra.core.Term]) -> Tuple[T1, Tuple[hydra.core.Term, hydra.core.Term]]:
            rk = recurse(val, kv[0])
            rv = recurse(rk[0], kv[1])
            return (rv[0], (rk[1], rv[1]))
        def for_binding(val: T1, binding: hydra.core.Binding) -> Tuple[T1, hydra.core.Binding]:
            r = recurse(val, binding.term)
            return (r[0], hydra.core.Binding(binding.name, r[1], binding.type))
        def for_elimination(val: T1, elm: hydra.core.Elimination) -> Tuple[T1, hydra.core.Elimination]:
            def r() -> Tuple[T1, hydra.core.Elimination]:
                match elm:
                    case hydra.core.EliminationUnion(value=cs):
                        rmd = hydra.lib.maybes.map((lambda v1: recurse(val, v1)), cs.default)
                        val1 = hydra.lib.maybes.maybe(val, (lambda v1: v1[0]), rmd)
                        rcases = for_fields(val1, cs.cases)
                        return (rcases[0], cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(cs.type_name, hydra.lib.maybes.map((lambda v1: v1[1]), rmd), rcases[1]))))
                    
                    case _:
                        return (val, elm)
            return (r()[0], r()[1])
        def for_function(val: T1, fun: hydra.core.Function) -> Tuple[T1, hydra.core.Function]:
            match fun:
                case hydra.core.FunctionElimination(value=elm):
                    r = for_elimination(val, elm)
                    return (r[0], cast(hydra.core.Function, hydra.core.FunctionElimination(r[1])))
                
                case hydra.core.FunctionLambda(value=l):
                    r = recurse(val, l.body)
                    return (r[0], cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, l.domain, r[1]))))
                
                case _:
                    return (val, fun)
        dflt = (val0, term02)
        match term02:
            case hydra.core.TermAnnotated(value=at):
                return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(t, at.annotation)))), val0, at.body)
            
            case hydra.core.TermApplication(value=a):
                rlhs = recurse(val0, a.function)
                rrhs = recurse(rlhs[0], a.argument)
                return (rrhs[0], cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(rlhs[1], rrhs[1]))))
            
            case hydra.core.TermEither(value=e):
                return hydra.lib.eithers.either((lambda l: (rl := recurse(val0, l), (rl[0], cast(hydra.core.Term, hydra.core.TermEither(cast(Either[hydra.core.Term, hydra.core.Term], Left(rl[1]))))))[1]), (lambda r: (rr := recurse(val0, r), (rr[0], cast(hydra.core.Term, hydra.core.TermEither(cast(Either[hydra.core.Term, hydra.core.Term], Right(rr[1]))))))[1]), e)
            
            case hydra.core.TermFunction(value=f2):
                return for_single(for_function, (lambda f3: cast(hydra.core.Term, hydra.core.TermFunction(f3))), val0, f2)
            
            case hydra.core.TermLet(value=l):
                renv = recurse(val0, l.body)
                return for_many(for_binding, (lambda bins: cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(bins, renv[1])))), renv[0], l.bindings)
            
            case hydra.core.TermList(value=els):
                return for_many(recurse, (lambda x: cast(hydra.core.Term, hydra.core.TermList(x))), val0, els)
            
            case hydra.core.TermMap(value=m):
                return for_many(for_pair, (lambda pairs: cast(hydra.core.Term, hydra.core.TermMap(cast(FrozenDict[hydra.core.Term, hydra.core.Term], hydra.lib.maps.from_list(pairs))))), val0, hydra.lib.maps.to_list(m))
            
            case hydra.core.TermMaybe(value=mt):
                return hydra.lib.maybes.maybe(dflt, (lambda t: for_single(recurse, (lambda t1: cast(hydra.core.Term, hydra.core.TermMaybe(cast(Maybe[hydra.core.Term], Just(t1))))), val0, t)), mt)
            
            case hydra.core.TermPair(value=p):
                rf = recurse(val0, p[0])
                rs = recurse(rf[0], p[1])
                return (rs[0], cast(hydra.core.Term, hydra.core.TermPair((rf[1], rs[1]))))
            
            case hydra.core.TermProduct(value=terms):
                return for_many(recurse, (lambda x: cast(hydra.core.Term, hydra.core.TermProduct(x))), val0, terms)
            
            case hydra.core.TermRecord(value=r):
                return for_many(for_field, (lambda fields: cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(r.type_name, fields)))), val0, r.fields)
            
            case hydra.core.TermSet(value=els2):
                return for_many(recurse, (lambda e: cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(e)))), val0, hydra.lib.sets.to_list(els2))
            
            case hydra.core.TermSum(value=s):
                return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermSum(hydra.core.Sum(s.index, s.size, t)))), val0, s.term)
            
            case hydra.core.TermTypeApplication(value=ta):
                return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(t, ta.type)))), val0, ta.body)
            
            case hydra.core.TermTypeLambda(value=tl):
                return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(tl.parameter, t)))), val0, tl.body)
            
            case hydra.core.TermUnion(value=inj):
                return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(inj.type_name, hydra.core.Field(inj.field.name, t))))), val0, inj.field.term)
            
            case hydra.core.TermWrap(value=wt):
                return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(wt.type_name, t)))), val0, wt.body)
            
            case _:
                return dflt
    def recurse(v1: T0, v2: hydra.core.Term) -> Tuple[T0, hydra.core.Term]:
        return f((lambda v12, v22: fsub(recurse, v12, v22)), v1, v2)
    return recurse(term0, v1)

def rewrite_and_fold_term_m[T0, T1](f: Callable[[
  Callable[[T0, hydra.core.Term], hydra.compute.Flow[T1, Tuple[T0, hydra.core.Term]]],
  T0,
  hydra.core.Term], hydra.compute.Flow[T1, Tuple[T0, hydra.core.Term]]], term0: T0, v1: hydra.core.Term) -> hydra.compute.Flow[T1, Tuple[T0, hydra.core.Term]]:
    def fsub[T2, T3](recurse: Callable[[T2, hydra.core.Term], hydra.compute.Flow[T3, Tuple[T2, hydra.core.Term]]], val0: T2, term02: hydra.core.Term) -> hydra.compute.Flow[T3, Tuple[T2, hydra.core.Term]]:
        def for_single[T4, T5, T6, T7, T8, T9](rec: Callable[[T4, T5], hydra.compute.Flow[T6, Tuple[T7, T8]]], cons: Callable[[T8], T9], val: T4, term: T5) -> hydra.compute.Flow[T6, Tuple[T7, T9]]:
            return hydra.lib.flows.bind(rec(val, term), (lambda r: hydra.lib.flows.pure((r[0], cons(r[1])))))
        def for_many[T4, T5, T6, T7, T8](rec: Callable[[T4, T5], hydra.compute.Flow[T6, Tuple[T4, T7]]], cons: Callable[[frozenlist[T7]], T8], val: T4, els: frozenlist[T5]) -> hydra.compute.Flow[T6, Tuple[T4, T8]]:
            return hydra.lib.flows.bind(hydra.lib.flows.foldl((lambda r, el: hydra.lib.flows.bind(rec(r[0], el), (lambda r2: hydra.lib.flows.pure((r2[0], hydra.lib.lists.cons(r2[1], r[1])))))), (val, cast(frozenlist[T7], ())), els), (lambda rr: hydra.lib.flows.pure((rr[0], cons(hydra.lib.lists.reverse(rr[1]))))))
        def for_field(val: T2, field: hydra.core.Field) -> hydra.compute.Flow[T3, Tuple[T2, hydra.core.Field]]:
            return hydra.lib.flows.bind(recurse(val, field.term), (lambda r: hydra.lib.flows.pure((r[0], hydra.core.Field(field.name, r[1])))))
        def for_fields(v1: T2, v2: frozenlist[hydra.core.Field]) -> hydra.compute.Flow[T3, Tuple[T2, frozenlist[hydra.core.Field]]]:
            return for_many(for_field, (lambda x: x), v1, v2)
        def for_pair(val: T2, kv: Tuple[hydra.core.Term, hydra.core.Term]) -> hydra.compute.Flow[T3, Tuple[T2, Tuple[hydra.core.Term, hydra.core.Term]]]:
            return hydra.lib.flows.bind(recurse(val, kv[0]), (lambda rk: hydra.lib.flows.bind(recurse(rk[0], kv[1]), (lambda rv: hydra.lib.flows.pure((rv[0], (rk[1], rv[1])))))))
        def for_binding(val: T2, binding: hydra.core.Binding) -> hydra.compute.Flow[T3, Tuple[T2, hydra.core.Binding]]:
            return hydra.lib.flows.bind(recurse(val, binding.term), (lambda r: hydra.lib.flows.pure((r[0], hydra.core.Binding(binding.name, r[1], binding.type)))))
        def for_elimination(val: T2, elm: hydra.core.Elimination) -> hydra.compute.Flow[T3, Tuple[T2, hydra.core.Elimination]]:
            def rw(elm2: hydra.core.Elimination) -> hydra.compute.Flow[T3, Tuple[T2, hydra.core.Elimination]]:
                match elm2:
                    case hydra.core.EliminationUnion(value=cs):
                        return hydra.lib.flows.bind(hydra.lib.maybes.maybe(hydra.lib.flows.pure(cast(Maybe[Tuple[T2, hydra.core.Term]], Nothing())), (lambda def_: hydra.lib.flows.map(cast(Callable[[Tuple[T2, hydra.core.Term]], Maybe[Tuple[T2, hydra.core.Term]]], hydra.lib.maybes.pure), recurse(val, def_))), cs.default), (lambda rmd: (val1 := hydra.lib.maybes.maybe(val, (lambda v1: v1[0]), rmd), hydra.lib.flows.bind(for_fields(val1, cs.cases), (lambda rcases: hydra.lib.flows.pure((rcases[0], cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(cs.type_name, hydra.lib.maybes.map((lambda v1: v1[1]), rmd), rcases[1]))))))))[1]))
                    
                    case _:
                        return hydra.lib.flows.pure((val, elm2))
            return hydra.lib.flows.bind(rw(elm), (lambda r: hydra.lib.flows.pure((r[0], r[1]))))
        def for_function(val: T2, fun: hydra.core.Function) -> hydra.compute.Flow[T3, Tuple[T2, hydra.core.Function]]:
            match fun:
                case hydra.core.FunctionElimination(value=elm):
                    return hydra.lib.flows.bind(for_elimination(val, elm), (lambda r: hydra.lib.flows.pure((r[0], cast(hydra.core.Function, hydra.core.FunctionElimination(r[1]))))))
                
                case hydra.core.FunctionLambda(value=l):
                    return hydra.lib.flows.bind(recurse(val, l.body), (lambda r: hydra.lib.flows.pure((r[0], cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, l.domain, r[1])))))))
                
                case _:
                    return hydra.lib.flows.pure((val, fun))
        def dflt[T4]() -> hydra.compute.Flow[T4, Tuple[T2, hydra.core.Term]]:
            return hydra.lib.flows.pure((val0, term02))
        match term02:
            case hydra.core.TermAnnotated(value=at):
                return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(t, at.annotation)))), val0, at.body)
            
            case hydra.core.TermApplication(value=a):
                return hydra.lib.flows.bind(recurse(val0, a.function), (lambda rlhs: hydra.lib.flows.bind(recurse(rlhs[0], a.argument), (lambda rrhs: hydra.lib.flows.pure((rrhs[0], cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(rlhs[1], rrhs[1])))))))))
            
            case hydra.core.TermEither(value=e):
                return hydra.lib.eithers.either((lambda l: hydra.lib.flows.bind(recurse(val0, l), (lambda rl: hydra.lib.flows.pure((rl[0], cast(hydra.core.Term, hydra.core.TermEither(cast(Either[hydra.core.Term, hydra.core.Term], Left(rl[1]))))))))), (lambda r: hydra.lib.flows.bind(recurse(val0, r), (lambda rr: hydra.lib.flows.pure((rr[0], cast(hydra.core.Term, hydra.core.TermEither(cast(Either[hydra.core.Term, hydra.core.Term], Right(rr[1]))))))))), e)
            
            case hydra.core.TermFunction(value=f2):
                return for_single(for_function, (lambda f3: cast(hydra.core.Term, hydra.core.TermFunction(f3))), val0, f2)
            
            case hydra.core.TermLet(value=l):
                return hydra.lib.flows.bind(recurse(val0, l.body), (lambda renv: for_many(for_binding, (lambda bins: cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(bins, renv[1])))), renv[0], l.bindings)))
            
            case hydra.core.TermList(value=els):
                return for_many(recurse, (lambda x: cast(hydra.core.Term, hydra.core.TermList(x))), val0, els)
            
            case hydra.core.TermMap(value=m):
                return for_many(for_pair, (lambda pairs: cast(hydra.core.Term, hydra.core.TermMap(cast(FrozenDict[hydra.core.Term, hydra.core.Term], hydra.lib.maps.from_list(pairs))))), val0, hydra.lib.maps.to_list(m))
            
            case hydra.core.TermMaybe(value=mt):
                return hydra.lib.maybes.maybe(cast(hydra.compute.Flow[T3, Tuple[T2, hydra.core.Term]], dflt), (lambda t: for_single(recurse, (lambda t1: cast(hydra.core.Term, hydra.core.TermMaybe(cast(Maybe[hydra.core.Term], Just(t1))))), val0, t)), mt)
            
            case hydra.core.TermPair(value=p):
                return hydra.lib.flows.bind(recurse(val0, p[0]), (lambda rf: hydra.lib.flows.bind(recurse(rf[0], p[1]), (lambda rs: hydra.lib.flows.pure((rs[0], cast(hydra.core.Term, hydra.core.TermPair((rf[1], rs[1])))))))))
            
            case hydra.core.TermProduct(value=terms):
                return for_many(recurse, (lambda x: cast(hydra.core.Term, hydra.core.TermProduct(x))), val0, terms)
            
            case hydra.core.TermRecord(value=r):
                return for_many(for_field, (lambda fields: cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(r.type_name, fields)))), val0, r.fields)
            
            case hydra.core.TermSet(value=els2):
                return for_many(recurse, (lambda e: cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(e)))), val0, hydra.lib.sets.to_list(els2))
            
            case hydra.core.TermSum(value=s):
                return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermSum(hydra.core.Sum(s.index, s.size, t)))), val0, s.term)
            
            case hydra.core.TermTypeApplication(value=ta):
                return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(t, ta.type)))), val0, ta.body)
            
            case hydra.core.TermTypeLambda(value=tl):
                return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(tl.parameter, t)))), val0, tl.body)
            
            case hydra.core.TermUnion(value=inj):
                return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(inj.type_name, hydra.core.Field(inj.field.name, t))))), val0, inj.field.term)
            
            case hydra.core.TermWrap(value=wt):
                return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(wt.type_name, t)))), val0, wt.body)
            
            case _:
                return cast(hydra.compute.Flow[T3, Tuple[T2, hydra.core.Term]], dflt)
    def recurse(v1: T0, v2: hydra.core.Term) -> hydra.compute.Flow[T1, Tuple[T0, hydra.core.Term]]:
        return f((lambda v12, v22: fsub(recurse, v12, v22)), v1, v2)
    return recurse(term0, v1)

def rewrite_term_m[T0](f: Callable[[
  Callable[[hydra.core.Term], hydra.compute.Flow[T0, hydra.core.Term]],
  hydra.core.Term], hydra.compute.Flow[T0, hydra.core.Term]], term0: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
    def fsub[T1](recurse: Callable[[hydra.core.Term], hydra.compute.Flow[T1, hydra.core.Term]], term: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.core.Term]:
        def for_field(field: hydra.core.Field) -> hydra.compute.Flow[T1, hydra.core.Field]:
            return hydra.lib.flows.bind(recurse(field.term), (lambda t: hydra.lib.flows.pure(hydra.core.Field(field.name, t))))
        def for_pair(kv: Tuple[hydra.core.Term, hydra.core.Term]) -> hydra.compute.Flow[T1, Tuple[hydra.core.Term, hydra.core.Term]]:
            return hydra.lib.flows.bind(recurse(kv[0]), (lambda k: hydra.lib.flows.bind(recurse(kv[1]), (lambda v: hydra.lib.flows.pure((k, v))))))
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
                        case hydra.core.EliminationProduct(value=tp):
                            return hydra.lib.flows.pure(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationProduct(tp)))))
                        
                        case hydra.core.EliminationRecord(value=p):
                            return hydra.lib.flows.pure(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(p)))))
                        
                        case hydra.core.EliminationUnion(value=cs):
                            n = cs.type_name
                            def_ = cs.default
                            cases = cs.cases
                            return hydra.lib.flows.bind(hydra.lib.maybes.maybe(hydra.lib.flows.pure(cast(Maybe[hydra.core.Term], Nothing())), (lambda t: hydra.lib.flows.map(cast(Callable[[hydra.core.Term], Maybe[hydra.core.Term]], hydra.lib.maybes.pure), recurse(t))), def_), (lambda rdef: hydra.lib.flows.map((lambda rcases: cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(n, rdef, rcases)))))), hydra.lib.flows.map_list(for_field, cases))))
                        
                        case hydra.core.EliminationWrap(value=name):
                            return hydra.lib.flows.pure(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(name)))))
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
                return hydra.lib.flows.bind(recurse(p[0]), (lambda rf: hydra.lib.flows.bind(recurse(p[1]), (lambda rs: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermPair((rf, rs))))))))
            
            case hydra.core.TermProduct(value=tuple):
                return hydra.lib.flows.map((lambda rtuple: cast(hydra.core.Term, hydra.core.TermProduct(rtuple))), hydra.lib.flows.map_list(recurse, tuple))
            
            case hydra.core.TermRecord(value=r):
                n = r.type_name
                fields = r.fields
                return hydra.lib.flows.map((lambda rfields: cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(n, rfields)))), hydra.lib.flows.map_list(for_field, fields))
            
            case hydra.core.TermSet(value=s):
                return hydra.lib.flows.bind(hydra.lib.flows.map_list(recurse, hydra.lib.sets.to_list(s)), (lambda rlist: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(rlist))))))
            
            case hydra.core.TermSum(value=sum):
                i = sum.index
                s = sum.size
                trm = sum.term
                return hydra.lib.flows.bind(recurse(trm), (lambda rtrm: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermSum(hydra.core.Sum(i, s, rtrm))))))
            
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
                return hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermUnit(None)))
            
            case hydra.core.TermVariable(value=v2):
                return hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermVariable(v2)))
            
            case hydra.core.TermWrap(value=wt):
                name = wt.type_name
                t = wt.body
                return hydra.lib.flows.bind(recurse(t), (lambda rt: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(name, rt))))))
    def recurse(v1: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
        return f((lambda v12: fsub(recurse, v12)), v1)
    return recurse(term0)

def rewrite_term_with_context[T0](f: Callable[[
  Callable[[T0, hydra.core.Term], hydra.core.Term],
  T0,
  hydra.core.Term], hydra.core.Term], cx0: T0, term0: hydra.core.Term) -> hydra.core.Term:
    def for_subterms[T1](recurse0: Callable[[T1, hydra.core.Term], hydra.core.Term], cx: T1, term: hydra.core.Term) -> hydra.core.Term:
        def recurse(v1: hydra.core.Term) -> hydra.core.Term:
            return recurse0(cx, v1)
        def for_field(field: hydra.core.Field) -> hydra.core.Field:
            return hydra.core.Field(field.name, recurse(field.term))
        def for_elimination(elm: hydra.core.Elimination) -> hydra.core.Elimination:
            match elm:
                case hydra.core.EliminationProduct(value=tp):
                    return cast(hydra.core.Elimination, hydra.core.EliminationProduct(tp))
                
                case hydra.core.EliminationRecord(value=p):
                    return cast(hydra.core.Elimination, hydra.core.EliminationRecord(p))
                
                case hydra.core.EliminationUnion(value=cs):
                    return cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(cs.type_name, hydra.lib.maybes.map(recurse, cs.default), hydra.lib.lists.map(for_field, cs.cases))))
                
                case hydra.core.EliminationWrap(value=name):
                    return cast(hydra.core.Elimination, hydra.core.EliminationWrap(name))
        def for_function(fun: hydra.core.Function) -> hydra.core.Function:
            match fun:
                case hydra.core.FunctionElimination(value=elm):
                    return cast(hydra.core.Function, hydra.core.FunctionElimination(for_elimination(elm)))
                
                case hydra.core.FunctionLambda(value=l):
                    return cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, l.domain, recurse(l.body))))
                
                case hydra.core.FunctionPrimitive(value=name):
                    return cast(hydra.core.Function, hydra.core.FunctionPrimitive(name))
        def for_let(lt: hydra.core.Let) -> hydra.core.Let:
            def map_binding(b: hydra.core.Binding) -> hydra.core.Binding:
                return hydra.core.Binding(b.name, recurse(b.term), b.type)
            return hydra.core.Let(hydra.lib.lists.map(map_binding, lt.bindings), recurse(lt.body))
        def for_map(m: FrozenDict[hydra.core.Term, hydra.core.Term]) -> FrozenDict[hydra.core.Term, hydra.core.Term]:
            def for_pair(p: Tuple[hydra.core.Term, hydra.core.Term]) -> Tuple[hydra.core.Term, hydra.core.Term]:
                return (recurse(p[0]), recurse(p[1]))
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
                return cast(hydra.core.Term, hydra.core.TermPair((recurse(p[0]), recurse(p[1]))))
            
            case hydra.core.TermProduct(value=tuple):
                return cast(hydra.core.Term, hydra.core.TermProduct(hydra.lib.lists.map(recurse, tuple)))
            
            case hydra.core.TermRecord(value=r):
                return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(r.type_name, hydra.lib.lists.map(for_field, r.fields))))
            
            case hydra.core.TermSet(value=s):
                return cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(hydra.lib.lists.map(recurse, hydra.lib.sets.to_list(s)))))
            
            case hydra.core.TermSum(value=s2):
                return cast(hydra.core.Term, hydra.core.TermSum(hydra.core.Sum(s2.index, s2.size, recurse(s2.term))))
            
            case hydra.core.TermTypeApplication(value=tt):
                return cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(recurse(tt.body), tt.type)))
            
            case hydra.core.TermTypeLambda(value=ta):
                return cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(ta.parameter, recurse(ta.body))))
            
            case hydra.core.TermUnion(value=i):
                return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(i.type_name, for_field(i.field))))
            
            case hydra.core.TermUnit():
                return cast(hydra.core.Term, hydra.core.TermUnit(None))
            
            case hydra.core.TermVariable(value=v2):
                return cast(hydra.core.Term, hydra.core.TermVariable(v2))
            
            case hydra.core.TermWrap(value=wt):
                return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(wt.type_name, recurse(wt.body))))
    def rewrite(cx: T0, term: hydra.core.Term) -> hydra.core.Term:
        return f((lambda v1, v2: for_subterms(rewrite, v1, v2)), cx, term)
    return rewrite(cx0, term0)

def rewrite_term_with_context_m[T0, T1](f: Callable[[
  Callable[[T0, hydra.core.Term], hydra.compute.Flow[T1, hydra.core.Term]],
  T0,
  hydra.core.Term], hydra.compute.Flow[T1, hydra.core.Term]], cx0: T0, term0: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.core.Term]:
    def for_subterms[T2, T3](recurse0: Callable[[T2, hydra.core.Term], hydra.compute.Flow[T3, hydra.core.Term]], cx: T2, term: hydra.core.Term) -> hydra.compute.Flow[T3, hydra.core.Term]:
        def recurse(v1: hydra.core.Term) -> hydra.compute.Flow[T3, hydra.core.Term]:
            return recurse0(cx, v1)
        def for_field(field: hydra.core.Field) -> hydra.compute.Flow[T3, hydra.core.Field]:
            return hydra.lib.flows.bind(recurse(field.term), (lambda t: hydra.lib.flows.pure(hydra.core.Field(field.name, t))))
        def for_pair(kv: Tuple[hydra.core.Term, hydra.core.Term]) -> hydra.compute.Flow[T3, Tuple[hydra.core.Term, hydra.core.Term]]:
            return hydra.lib.flows.bind(recurse(kv[0]), (lambda k: hydra.lib.flows.bind(recurse(kv[1]), (lambda v: hydra.lib.flows.pure((k, v))))))
        def for_elimination(e: hydra.core.Elimination) -> hydra.compute.Flow[T3, hydra.core.Function]:
            match e:
                case hydra.core.EliminationProduct(value=tp):
                    return hydra.lib.flows.pure(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationProduct(tp)))))
                
                case hydra.core.EliminationRecord(value=p):
                    return hydra.lib.flows.pure(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(p)))))
                
                case hydra.core.EliminationUnion(value=cs):
                    n = cs.type_name
                    def_ = cs.default
                    cases = cs.cases
                    return hydra.lib.flows.bind(hydra.lib.maybes.maybe(hydra.lib.flows.pure(cast(Maybe[hydra.core.Term], Nothing())), (lambda t: hydra.lib.flows.map(cast(Callable[[hydra.core.Term], Maybe[hydra.core.Term]], hydra.lib.maybes.pure), recurse(t))), def_), (lambda rdef: hydra.lib.flows.map((lambda rcases: cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(n, rdef, rcases)))))), hydra.lib.flows.map_list(for_field, cases))))
                
                case hydra.core.EliminationWrap(value=name):
                    return hydra.lib.flows.pure(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(name)))))
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
            
            case hydra.core.TermProduct(value=tuple):
                return hydra.lib.flows.map((lambda rtuple: cast(hydra.core.Term, hydra.core.TermProduct(rtuple))), hydra.lib.flows.map_list(recurse, tuple))
            
            case hydra.core.TermRecord(value=r):
                n = r.type_name
                fields = r.fields
                return hydra.lib.flows.map((lambda rfields: cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(n, rfields)))), hydra.lib.flows.map_list(for_field, fields))
            
            case hydra.core.TermSet(value=s):
                return hydra.lib.flows.bind(hydra.lib.flows.map_list(recurse, hydra.lib.sets.to_list(s)), (lambda rlist: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(rlist))))))
            
            case hydra.core.TermSum(value=sum):
                i = sum.index
                s = sum.size
                trm = sum.term
                return hydra.lib.flows.bind(recurse(trm), (lambda rtrm: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermSum(hydra.core.Sum(i, s, rtrm))))))
            
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
                return hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermUnit(None)))
            
            case hydra.core.TermVariable(value=v2):
                return hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermVariable(v2)))
            
            case hydra.core.TermWrap(value=wt):
                name = wt.type_name
                t = wt.body
                return hydra.lib.flows.bind(recurse(t), (lambda rt: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(name, rt))))))
            
            case _:
                raise TypeError("Unsupported Term")
    def rewrite(cx: T0, term: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.core.Term]:
        return f((lambda v1, v2: for_subterms(rewrite, v1, v2)), cx, term)
    return rewrite(cx0, term0)

def substitute_variable(from_: hydra.core.Name, to: hydra.core.Name, term: hydra.core.Term) -> hydra.core.Term:
    r"""Substitute one variable for another in a term."""
    
    def replace(recurse: Callable[[hydra.core.Term], hydra.core.Term], term2: hydra.core.Term) -> hydra.core.Term:
        match term2:
            case hydra.core.TermVariable(value=x):
                return cast(hydra.core.Term, hydra.core.TermVariable(hydra.lib.logic.if_else(hydra.lib.equality.equal(x, from_), to, x)))
            
            case hydra.core.TermFunction(value=v1):
                match v1:
                    case hydra.core.FunctionLambda(value=l):
                        return hydra.lib.logic.if_else(hydra.lib.equality.equal(l.parameter, from_), term2, recurse(term2))
                    
                    case _:
                        return recurse(term2)
            
            case _:
                return recurse(term2)
    return rewrite_term(replace, term)

def simplify_term(term: hydra.core.Term) -> hydra.core.Term:
    r"""Simplify terms by applying beta reduction where possible."""
    
    def simplify[T0](recurse: Callable[[hydra.core.Term], T0], term2: hydra.core.Term) -> T0:
        def for_rhs(rhs: hydra.core.Term, var: hydra.core.Name, body: hydra.core.Term) -> hydra.core.Term:
            match deannotate_term(rhs):
                case hydra.core.TermVariable(value=v):
                    return simplify_term(substitute_variable(var, v, body))
                
                case _:
                    return term2
        def for_lhs(lhs: hydra.core.Term, rhs: hydra.core.Term) -> hydra.core.Term:
            def for_fun(fun: hydra.core.Function) -> hydra.core.Term:
                match fun:
                    case hydra.core.FunctionLambda(value=l):
                        var = l.parameter
                        body = l.body
                        return hydra.lib.logic.if_else(hydra.lib.sets.member(var, free_variables_in_term(body)), for_rhs(rhs, var, body), simplify_term(body))
                    
                    case _:
                        return term2
            match deannotate_term(lhs):
                case hydra.core.TermFunction(value=fun):
                    return for_fun(fun)
                
                case _:
                    return term2
        def for_term(stripped: hydra.core.Term) -> hydra.core.Term:
            match stripped:
                case hydra.core.TermApplication(value=app):
                    lhs = app.function
                    rhs = app.argument
                    return for_lhs(lhs, rhs)
                
                case _:
                    return term2
        stripped = deannotate_term(term2)
        return recurse(for_term(stripped))
    return rewrite_term(cast(Callable[[Callable[[hydra.core.Term], hydra.core.Term], hydra.core.Term], hydra.core.Term], simplify), term)

def substitute_type_variables(subst: FrozenDict[hydra.core.Name, hydra.core.Name], typ: hydra.core.Type) -> hydra.core.Type:
    r"""Substitute type variables in a type."""
    
    def replace(recurse: Callable[[hydra.core.Type], hydra.core.Type], typ2: hydra.core.Type) -> hydra.core.Type:
        match typ2:
            case hydra.core.TypeVariable(value=n):
                return cast(hydra.core.Type, hydra.core.TypeVariable(hydra.lib.maybes.from_maybe(n, hydra.lib.maps.lookup(n, subst))))
            
            case _:
                return recurse(typ2)
    return rewrite_type(replace, typ)

def subterms_with_accessors(v1: hydra.core.Term) -> frozenlist[Tuple[hydra.accessors.TermAccessor, hydra.core.Term]]:
    r"""Find the children of a given term."""
    
    match v1:
        case hydra.core.TermAnnotated(value=at):
            return ((cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorAnnotatedBody(None)), at.body),)
        
        case hydra.core.TermApplication(value=p):
            return ((cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorApplicationFunction(None)), p.function), (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorApplicationArgument(None)), p.argument))
        
        case hydra.core.TermEither():
            return cast(frozenlist[Tuple[hydra.accessors.TermAccessor, hydra.core.Term]], ())
        
        case hydra.core.TermFunction(value=v12):
            match v12:
                case hydra.core.FunctionElimination(value=v13):
                    match v13:
                        case hydra.core.EliminationUnion(value=cs):
                            return hydra.lib.lists.concat2(hydra.lib.maybes.maybe(cast(frozenlist[Tuple[hydra.accessors.TermAccessor, hydra.core.Term]], ()), (lambda t: ((cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorUnionCasesDefault(None)), t),)), cs.default), hydra.lib.lists.map((lambda f: (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorUnionCasesBranch(f.name)), f.term)), cs.cases))
                        
                        case _:
                            return cast(frozenlist[Tuple[hydra.accessors.TermAccessor, hydra.core.Term]], ())
                
                case hydra.core.FunctionLambda(value=l):
                    return ((cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorLambdaBody(None)), l.body),)
                
                case _:
                    return cast(frozenlist[Tuple[hydra.accessors.TermAccessor, hydra.core.Term]], ())
        
        case hydra.core.TermLet(value=lt):
            return hydra.lib.lists.cons((cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorLetBody(None)), lt.body), hydra.lib.lists.map((lambda b: (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorLetBinding(b.name)), b.term)), lt.bindings))
        
        case hydra.core.TermList(value=l):
            return hydra.lib.lists.map((lambda e: (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorListElement(0)), e)), l)
        
        case hydra.core.TermLiteral():
            return cast(frozenlist[Tuple[hydra.accessors.TermAccessor, hydra.core.Term]], ())
        
        case hydra.core.TermMap(value=m):
            return hydra.lib.lists.concat(hydra.lib.lists.map((lambda p: ((cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorMapKey(0)), p[0]), (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorMapValue(0)), p[1]))), hydra.lib.maps.to_list(m)))
        
        case hydra.core.TermMaybe(value=m2):
            return hydra.lib.maybes.maybe(cast(frozenlist[Tuple[hydra.accessors.TermAccessor, hydra.core.Term]], ()), (lambda t: ((cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorMaybeTerm(None)), t),)), m2)
        
        case hydra.core.TermPair():
            return cast(frozenlist[Tuple[hydra.accessors.TermAccessor, hydra.core.Term]], ())
        
        case hydra.core.TermProduct(value=p3):
            return hydra.lib.lists.map((lambda e: (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorProductTerm(0)), e)), p3)
        
        case hydra.core.TermRecord(value=rt):
            return hydra.lib.lists.map((lambda f: (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorRecordField(f.name)), f.term)), rt.fields)
        
        case hydra.core.TermSet(value=s):
            return hydra.lib.lists.map((lambda e: (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorListElement(0)), e)), hydra.lib.sets.to_list(s))
        
        case hydra.core.TermSum(value=st):
            return ((cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorSumTerm(None)), st.term),)
        
        case hydra.core.TermTypeApplication(value=ta):
            return ((cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorTypeApplicationTerm(None)), ta.body),)
        
        case hydra.core.TermTypeLambda(value=ta2):
            return ((cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorTypeLambdaBody(None)), ta2.body),)
        
        case hydra.core.TermUnion(value=ut):
            return ((cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorInjectionTerm(None)), ut.field.term),)
        
        case hydra.core.TermUnit():
            return cast(frozenlist[Tuple[hydra.accessors.TermAccessor, hydra.core.Term]], ())
        
        case hydra.core.TermVariable():
            return cast(frozenlist[Tuple[hydra.accessors.TermAccessor, hydra.core.Term]], ())
        
        case hydra.core.TermWrap(value=n):
            return ((cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorWrappedTerm(None)), n.body),)

def term_dependency_names(binds: bool, with_prims: bool, with_noms: bool, term0: hydra.core.Term) -> frozenset[hydra.core.Name]:
    r"""Note: does not distinguish between bound and free variables; use freeVariablesInTerm for that."""
    
    def add_names(names: frozenset[hydra.core.Name], term: hydra.core.Term) -> frozenset[hydra.core.Name]:
        def nominal(name: hydra.core.Name) -> frozenset[hydra.core.Name]:
            return hydra.lib.logic.if_else(with_noms, hydra.lib.sets.insert(name, names), names)
        def prim(name: hydra.core.Name) -> frozenset[hydra.core.Name]:
            return hydra.lib.logic.if_else(with_prims, hydra.lib.sets.insert(name, names), names)
        def var(name: hydra.core.Name) -> frozenset[hydra.core.Name]:
            return hydra.lib.logic.if_else(binds, hydra.lib.sets.insert(name, names), names)
        match term:
            case hydra.core.TermFunction(value=f):
                match f:
                    case hydra.core.FunctionPrimitive(value=name):
                        return prim(name)
                    
                    case hydra.core.FunctionElimination(value=e):
                        match e:
                            case hydra.core.EliminationRecord(value=proj):
                                return nominal(proj.type_name)
                            
                            case hydra.core.EliminationUnion(value=case_stmt):
                                return nominal(case_stmt.type_name)
                            
                            case hydra.core.EliminationWrap(value=name):
                                return nominal(name)
                            
                            case _:
                                return names
                    
                    case _:
                        return names
            
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
    return fold_over_term(cast(hydra.coders.TraversalOrder, hydra.coders.TraversalOrderPre(None)), add_names, cast(frozenset[hydra.core.Name], hydra.lib.sets.empty()), term0)

def to_short_names(original: frozenlist[hydra.core.Name]) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
    r"""Generate short names from a list of fully qualified names."""
    
    def add_name(acc: FrozenDict[str, frozenset[hydra.core.Name]], name: hydra.core.Name) -> FrozenDict[str, frozenset[hydra.core.Name]]:
        local = hydra.names.local_name_of(name)
        group = hydra.lib.maybes.from_maybe(cast(frozenset[hydra.core.Name], hydra.lib.sets.empty()), hydra.lib.maps.lookup(local, acc))
        return hydra.lib.maps.insert(local, hydra.lib.sets.insert(name, group), acc)
    def group_names_by_local(names: frozenlist[hydra.core.Name]) -> FrozenDict[str, frozenset[hydra.core.Name]]:
        return hydra.lib.lists.foldl(add_name, cast(FrozenDict[str, frozenset[hydra.core.Name]], hydra.lib.maps.empty()), names)
    groups = group_names_by_local(original)
    def rename_group[T0](local_names: Tuple[str, frozenset[T0]]) -> frozenlist[Tuple[T0, hydra.core.Name]]:
        local = local_names[0]
        names = local_names[1]
        def range_from(start: int) -> frozenlist[int]:
            return hydra.lib.lists.cons(start, range_from(hydra.lib.math.add(start, 1)))
        def rename[T1](name: T1, i: int) -> Tuple[T1, hydra.core.Name]:
            return (name, hydra.core.Name(hydra.lib.logic.if_else(hydra.lib.equality.gt(i, 1), hydra.lib.strings.cat2(local, hydra.lib.literals.show_int32(i)), local)))
        return hydra.lib.lists.zip_with(cast(Callable[[T0, int], Tuple[T0, hydra.core.Name]], rename), hydra.lib.sets.to_list(names), range_from(1))
    return cast(FrozenDict[hydra.core.Name, hydra.core.Name], hydra.lib.maps.from_list(hydra.lib.lists.concat(hydra.lib.lists.map(cast(Callable[[Tuple[str, frozenset[hydra.core.Name]]], frozenlist[Tuple[hydra.core.Name, hydra.core.Name]]], rename_group), hydra.lib.maps.to_list(groups)))))

def topological_sort_binding_map(binding_map: FrozenDict[hydra.core.Name, hydra.core.Term]) -> frozenlist[frozenlist[Tuple[hydra.core.Name, hydra.core.Term]]]:
    r"""Topological sort of connected components, in terms of dependencies between variable/term binding pairs."""
    
    bindings = hydra.lib.maps.to_list(binding_map)
    keys = hydra.lib.sets.from_list(hydra.lib.lists.map((lambda v1: v1[0]), bindings))
    def has_type_annotation(term: hydra.core.Term) -> bool:
        match term:
            case hydra.core.TermAnnotated(value=at):
                return has_type_annotation(at.body)
            
            case _:
                return False
    def deps_of[T0](name_and_term: Tuple[T0, hydra.core.Term]) -> Tuple[T0, frozenlist[hydra.core.Name]]:
        name = name_and_term[0]
        term = name_and_term[1]
        return (name, hydra.lib.logic.if_else(has_type_annotation(term), cast(frozenlist[hydra.core.Name], ()), hydra.lib.sets.to_list(hydra.lib.sets.intersection(keys, free_variables_in_term(term)))))
    def to_pair(name: hydra.core.Name) -> Tuple[hydra.core.Name, hydra.core.Term]:
        return (name, hydra.lib.maybes.from_maybe(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("Impossible!")))), hydra.lib.maps.lookup(name, binding_map)))
    return hydra.lib.lists.map((lambda v1: hydra.lib.lists.map(to_pair, v1)), hydra.sorting.topological_sort_components(hydra.lib.lists.map(cast(Callable[[Tuple[hydra.core.Name, hydra.core.Term]], Tuple[hydra.core.Name, frozenlist[hydra.core.Name]]], deps_of), bindings)))

def topological_sort_bindings(els: frozenlist[hydra.core.Binding]) -> Either[frozenlist[frozenlist[hydra.core.Name]], frozenlist[hydra.core.Name]]:
    r"""Topological sort of elements based on their dependencies."""
    
    def adjlist(e: hydra.core.Binding) -> Tuple[hydra.core.Name, frozenlist[hydra.core.Name]]:
        return (e.name, hydra.lib.sets.to_list(term_dependency_names(False, True, True, e.term)))
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
    return fold_over_type(cast(hydra.coders.TraversalOrder, hydra.coders.TraversalOrderPre(None)), add_names, cast(frozenset[hydra.core.Name], hydra.lib.sets.empty()), typ0)

def type_dependency_names(with_schema: bool, typ: hydra.core.Type) -> frozenset[hydra.core.Name]:
    return hydra.lib.logic.if_else(with_schema, hydra.lib.sets.union(free_variables_in_type(typ), type_names_in_type(typ)), free_variables_in_type(typ))

def unshadow_variables(term: hydra.core.Term) -> hydra.core.Term:
    r"""Unshadow lambda-bound variables in a term."""
    
    def rewrite(recurse: Callable[[FrozenDict[hydra.core.Name, int], hydra.core.Term], Tuple[FrozenDict[hydra.core.Name, int], hydra.core.Term]], m: FrozenDict[hydra.core.Name, int], term2: hydra.core.Term) -> Tuple[FrozenDict[hydra.core.Name, int], hydra.core.Term]:
        dflt = recurse(m, term2)
        match term2:
            case hydra.core.TermFunction(value=f):
                match f:
                    case hydra.core.FunctionLambda(value=l):
                        v = l.parameter
                        domain = l.domain
                        body = l.body
                        return (m, hydra.lib.maybes.maybe(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(v, domain, rewrite(recurse, hydra.lib.maps.insert(v, 1, m), body)[1]))))), (lambda i: (i2 := hydra.lib.math.add(i, 1), v2 := hydra.core.Name(hydra.lib.strings.cat2(v.value, hydra.lib.literals.show_int32(i2))), m2 := hydra.lib.maps.insert(v, i2, m), cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(v2, domain, rewrite(recurse, m2, body)[1]))))))[3]), hydra.lib.maps.lookup(v, m)))
                    
                    case _:
                        return dflt
            
            case hydra.core.TermVariable(value=v):
                return (m, cast(hydra.core.Term, hydra.core.TermVariable(hydra.lib.maybes.maybe(v, (lambda i: hydra.lib.logic.if_else(hydra.lib.equality.equal(i, 1), v, hydra.core.Name(hydra.lib.strings.cat2(v.value, hydra.lib.literals.show_int32(i))))), hydra.lib.maps.lookup(v, m)))))
            
            case _:
                return dflt
    return rewrite_and_fold_term(rewrite, cast(FrozenDict[hydra.core.Name, int], hydra.lib.maps.empty()), term)[1]
