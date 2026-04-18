# Note: this is an automatically generated file. Do not edit.

r"""Pure helpers for the Coq code generator."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.coq.language
import hydra.core
import hydra.formatting
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.packaging
import hydra.rewriting
import hydra.sorting
import hydra.variables

T0 = TypeVar("T0")

def extract_type_params(ty: hydra.core.Type) -> tuple[frozenlist[str], hydra.core.Type]:
    r"""Peel off leading forall binders, returning the list of parameter names and the inner body type."""

    match ty:
        case hydra.core.TypeForall(value=ft):
            param = ft.parameter.value
            @lru_cache(1)
            def rest() -> tuple[frozenlist[str], hydra.core.Type]:
                return extract_type_params(ft.body)
            return (hydra.lib.lists.cons(param, hydra.lib.pairs.first(rest())), hydra.lib.pairs.second(rest()))

        case hydra.core.TypeAnnotated(value=at):
            return extract_type_params(at.body)

        case _:
            return ((), ty)

def build_constructor_counts(defs: frozenlist[tuple[T0, hydra.core.Type]]):
    r"""Build a map from each union-type definition's name to its constructor count."""

    return hydra.lib.maps.from_list(hydra.lib.lists.concat(hydra.lib.lists.map((lambda nt: (name := hydra.lib.pairs.first(nt), ty := hydra.lib.pairs.second(nt), extracted := extract_type_params(ty), body_ty := hydra.lib.pairs.second(extracted), _hoist_name_body_1 := (lambda v1: (lambda fields: ((name, hydra.lib.lists.length(fields)),))(v1.value) if isinstance(v1, hydra.core.TypeUnion) else ()), _hoist_name_body_1(body_ty))[5]), defs)))

def sanitize(s: str) -> str:
    r"""Escape a stripped local name against Coq's stripped reserved-words set."""

    return hydra.formatting.escape_with_underscore(hydra.coq.language.coq_stripped_reserved_words(), s)

def local_name(s: str) -> str:
    r"""Return the last dot-separated segment of a qualified Hydra name, sanitised via `sanitize`."""

    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.strings.split_on(".", s)
    @lru_cache(1)
    def raw() -> str:
        return hydra.lib.maybes.from_maybe((lambda : s), hydra.lib.lists.maybe_last(parts()))
    return sanitize(raw())

def local_name_raw(s: str) -> str:
    r"""Return the last dot-separated segment of a qualified Hydra name, unsanitized."""

    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.strings.split_on(".", s)
    return hydra.lib.maybes.from_maybe((lambda : s), hydra.lib.lists.maybe_last(parts()))

def build_field_mapping(modules: frozenlist[hydra.packaging.Module]):
    def _hoist_hydra_coq_utils_build_field_mapping_1(v1):
        match v1:
            case hydra.packaging.DefinitionType(value=td):
                qname = td.name.value
                @lru_cache(1)
                def tname() -> str:
                    return local_name(qname)
                ty = td.type.type
                @lru_cache(1)
                def extracted() -> tuple[frozenlist[str], hydra.core.Type]:
                    return extract_type_params(ty)
                @lru_cache(1)
                def body_ty() -> hydra.core.Type:
                    return hydra.lib.pairs.second(extracted())
                def _hoist_qname_body_1(v12):
                    match v12:
                        case hydra.core.TypeRecord(value=fields):
                            return hydra.lib.lists.map((lambda ft: (raw_fn := local_name_raw(ft.name.value), fn := sanitize(raw_fn), prefixed := hydra.lib.strings.cat((hydra.formatting.decapitalize(tname()), "_", fn)), ((qname, raw_fn), prefixed))[3]), fields)

                        case _:
                            return ()
                return _hoist_qname_body_1(body_ty())

            case _:
                return ()
    return hydra.lib.maps.from_list(hydra.lib.lists.concat(hydra.lib.lists.map((lambda m: hydra.lib.lists.concat(hydra.lib.lists.map((lambda def_: _hoist_hydra_coq_utils_build_field_mapping_1(def_)), m.definitions))), modules)))

def is_type_var_like(s: str) -> bool:
    r"""Return True if the string is of the form `t<digits>` with at least one digit."""

    @lru_cache(1)
    def chars() -> frozenlist[int]:
        return hydra.lib.strings.to_list(s)
    return hydra.lib.maybes.from_maybe((lambda : False), hydra.lib.maybes.map((lambda p: (first_ch := hydra.lib.pairs.first(p), rest := hydra.lib.pairs.second(p), hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.equality.equal(first_ch, 116)), (lambda : False), (lambda : hydra.lib.logic.and_(hydra.lib.logic.not_(hydra.lib.lists.null(rest)), hydra.lib.lists.foldl((lambda acc, c: hydra.lib.logic.and_(acc, hydra.lib.logic.and_(hydra.lib.equality.gte(c, 48), hydra.lib.equality.lte(c, 57)))), True, rest)))))[2]), hydra.lib.lists.uncons(chars())))

def collect_free_type_vars_in_type(ty: hydra.core.Type) -> frozenset[str]:
    r"""Collect names of type-variable-like references (t0, t1, ...) inside a Type."""

    match ty:
        case hydra.core.TypeAnnotated(value=at):
            return collect_free_type_vars_in_type(at.body)

        case hydra.core.TypeApplication(value=app):
            return hydra.lib.sets.union(collect_free_type_vars_in_type(app.function), collect_free_type_vars_in_type(app.argument))

        case hydra.core.TypeForall(value=ft):
            return collect_free_type_vars_in_type(ft.body)

        case hydra.core.TypeFunction(value=ft2):
            return hydra.lib.sets.union(collect_free_type_vars_in_type(ft2.domain), collect_free_type_vars_in_type(ft2.codomain))

        case hydra.core.TypeList(value=t):
            return collect_free_type_vars_in_type(t)

        case hydra.core.TypeMap(value=mt):
            return hydra.lib.sets.union(collect_free_type_vars_in_type(mt.keys), collect_free_type_vars_in_type(mt.values))

        case hydra.core.TypeMaybe(value=t2):
            return collect_free_type_vars_in_type(t2)

        case hydra.core.TypePair(value=pt):
            return hydra.lib.sets.union(collect_free_type_vars_in_type(pt.first), collect_free_type_vars_in_type(pt.second))

        case hydra.core.TypeRecord(value=fields):
            return hydra.lib.sets.unions(hydra.lib.lists.map((lambda f: collect_free_type_vars_in_type(f.type)), fields))

        case hydra.core.TypeSet(value=t3):
            return collect_free_type_vars_in_type(t3)

        case hydra.core.TypeUnion(value=fields2):
            return hydra.lib.sets.unions(hydra.lib.lists.map((lambda f: collect_free_type_vars_in_type(f.type)), fields2))

        case hydra.core.TypeVariable(value=n):
            nm = n.value
            return hydra.lib.logic.if_else(is_type_var_like(nm), (lambda : hydra.lib.sets.singleton(nm)), (lambda : hydra.lib.sets.empty()))

        case hydra.core.TypeWrap(value=wt):
            return collect_free_type_vars_in_type(wt)

        case _:
            return hydra.lib.sets.empty()

def collect_free_type_vars_in_type_scheme(ts: hydra.core.TypeScheme) -> frozenset[str]:
    r"""Collect type-variable-like names declared or referenced by a TypeScheme."""

    @lru_cache(1)
    def explicit() -> frozenset[str]:
        return hydra.lib.sets.from_list(hydra.lib.lists.map((lambda n: n.value), hydra.lib.lists.filter((lambda n: is_type_var_like(n.value)), ts.variables)))
    return hydra.lib.sets.union(explicit(), collect_free_type_vars_in_type(ts.type))

def collect_free_type_vars(tm: hydra.core.Term) -> frozenset[str]:
    r"""Collect the set of free type-variable-like names (t0, t1, ...) referenced anywhere inside a Term."""

    match tm:
        case hydra.core.TermAnnotated(value=at):
            return collect_free_type_vars(at.body)

        case hydra.core.TermApplication(value=app):
            return hydra.lib.sets.union(collect_free_type_vars(app.function), collect_free_type_vars(app.argument))

        case hydra.core.TermCases(value=cs):
            return hydra.lib.sets.union(hydra.lib.maybes.maybe((lambda : hydra.lib.sets.empty()), (lambda d: collect_free_type_vars(d)), cs.default), hydra.lib.sets.unions(hydra.lib.lists.map((lambda f: collect_free_type_vars(f.term)), cs.cases)))

        case hydra.core.TermEither(value=e):
            return hydra.lib.eithers.either((lambda l: collect_free_type_vars(l)), (lambda r: collect_free_type_vars(r)), e)

        case hydra.core.TermInject(value=inj):
            return collect_free_type_vars(inj.field.term)

        case hydra.core.TermLambda(value=lam):
            param_name = lam.parameter.value
            @lru_cache(1)
            def dom_vars() -> frozenset[str]:
                return hydra.lib.maybes.maybe((lambda : hydra.lib.sets.empty()), (lambda dty: collect_free_type_vars_in_type(dty)), lam.domain)
            @lru_cache(1)
            def body_vars() -> frozenset[str]:
                return collect_free_type_vars(lam.body)
            @lru_cache(1)
            def all_vars() -> frozenset[str]:
                return hydra.lib.sets.union(dom_vars(), body_vars())
            return hydra.lib.logic.if_else(is_type_var_like(param_name), (lambda : hydra.lib.sets.delete(param_name, all_vars())), (lambda : all_vars()))

        case hydra.core.TermLet(value=lt):
            @lru_cache(1)
            def bind_vars() -> frozenset[str]:
                return hydra.lib.sets.unions(hydra.lib.lists.map((lambda b: hydra.lib.sets.union(collect_free_type_vars(b.term), hydra.lib.maybes.maybe((lambda : hydra.lib.sets.empty()), (lambda sch: collect_free_type_vars_in_type_scheme(sch)), b.type))), lt.bindings))
            return hydra.lib.sets.union(bind_vars(), collect_free_type_vars(lt.body))

        case hydra.core.TermList(value=xs):
            return hydra.lib.sets.unions(hydra.lib.lists.map((lambda el: collect_free_type_vars(el)), xs))

        case hydra.core.TermMaybe(value=mt):
            return hydra.lib.maybes.maybe((lambda : hydra.lib.sets.empty()), (lambda el: collect_free_type_vars(el)), mt)

        case hydra.core.TermPair(value=p):
            return hydra.lib.sets.union(collect_free_type_vars(hydra.lib.pairs.first(p)), collect_free_type_vars(hydra.lib.pairs.second(p)))

        case hydra.core.TermRecord(value=r):
            return hydra.lib.sets.unions(hydra.lib.lists.map((lambda f: collect_free_type_vars(f.term)), r.fields))

        case hydra.core.TermSet(value=s):
            return hydra.lib.sets.unions(hydra.lib.lists.map((lambda el: collect_free_type_vars(el)), hydra.lib.sets.to_list(s)))

        case hydra.core.TermTypeApplication(value=ta):
            return collect_free_type_vars(ta.body)

        case hydra.core.TermTypeLambda(value=tl):
            return hydra.lib.sets.delete(tl.parameter.value, collect_free_type_vars(tl.body))

        case hydra.core.TermWrap(value=wt):
            return collect_free_type_vars(wt.body)

        case _:
            return hydra.lib.sets.empty()

def collect_let_bindings(tm: hydra.core.Term) -> tuple[frozenlist[hydra.core.Binding], hydra.core.Term]:
    r"""Flatten consecutive TermLet wrappers into (bindings, innermostBody)."""

    match tm:
        case hydra.core.TermLet(value=lt):
            @lru_cache(1)
            def rest() -> tuple[frozenlist[hydra.core.Binding], hydra.core.Term]:
                return collect_let_bindings(lt.body)
            return (hydra.lib.lists.concat2(lt.bindings, hydra.lib.pairs.first(rest())), hydra.lib.pairs.second(rest()))

        case _:
            return ((), tm)

def qualified_from_name(n: hydra.core.Name) -> frozenset[str]:
    r"""Wrap a Hydra Name as a singleton set of its raw string, iff it is a qualified (hydra.*) reference."""

    raw = n.value
    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.strings.split_on(".", raw)
    return hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.gte(hydra.lib.lists.length(parts()), 2), hydra.lib.equality.equal(hydra.lib.maybes.from_maybe((lambda : ""), hydra.lib.lists.maybe_head(parts())), "hydra")), (lambda : hydra.lib.sets.singleton(raw)), (lambda : hydra.lib.sets.empty()))

def collect_qualified_names_in_type(ty: hydra.core.Type) -> frozenset[str]:
    r"""Collect the set of qualified (hydra.*) Name strings that a Hydra Type references."""

    match ty:
        case hydra.core.TypeAnnotated(value=at):
            return collect_qualified_names_in_type(at.body)

        case hydra.core.TypeApplication(value=app):
            return hydra.lib.sets.union(collect_qualified_names_in_type(app.function), collect_qualified_names_in_type(app.argument))

        case hydra.core.TypeEither(value=et):
            return hydra.lib.sets.union(collect_qualified_names_in_type(et.left), collect_qualified_names_in_type(et.right))

        case hydra.core.TypeForall(value=ft):
            return collect_qualified_names_in_type(ft.body)

        case hydra.core.TypeFunction(value=ft2):
            return hydra.lib.sets.union(collect_qualified_names_in_type(ft2.domain), collect_qualified_names_in_type(ft2.codomain))

        case hydra.core.TypeList(value=t):
            return collect_qualified_names_in_type(t)

        case hydra.core.TypeMap(value=mt):
            return hydra.lib.sets.union(collect_qualified_names_in_type(mt.keys), collect_qualified_names_in_type(mt.values))

        case hydra.core.TypeMaybe(value=t2):
            return collect_qualified_names_in_type(t2)

        case hydra.core.TypePair(value=pt):
            return hydra.lib.sets.union(collect_qualified_names_in_type(pt.first), collect_qualified_names_in_type(pt.second))

        case hydra.core.TypeRecord(value=fields):
            return hydra.lib.sets.unions(hydra.lib.lists.map((lambda f: collect_qualified_names_in_type(f.type)), fields))

        case hydra.core.TypeSet(value=t3):
            return collect_qualified_names_in_type(t3)

        case hydra.core.TypeUnion(value=fields2):
            return hydra.lib.sets.unions(hydra.lib.lists.map((lambda f: collect_qualified_names_in_type(f.type)), fields2))

        case hydra.core.TypeVariable(value=n):
            return qualified_from_name(n)

        case hydra.core.TypeWrap(value=wt):
            return collect_qualified_names_in_type(wt)

        case _:
            return hydra.lib.sets.empty()

def collect_qualified_names_in_term(tm: hydra.core.Term) -> frozenset[str]:
    r"""Collect the set of qualified (hydra.*) Name strings that a Hydra Term references."""

    match tm:
        case hydra.core.TermAnnotated(value=at):
            return collect_qualified_names_in_term(at.body)

        case hydra.core.TermApplication(value=app):
            return hydra.lib.sets.union(collect_qualified_names_in_term(app.function), collect_qualified_names_in_term(app.argument))

        case hydra.core.TermCases(value=cs):
            return hydra.lib.sets.union(qualified_from_name(cs.type_name), hydra.lib.sets.union(hydra.lib.sets.unions(hydra.lib.lists.map((lambda f: collect_qualified_names_in_term(f.term)), cs.cases)), hydra.lib.maybes.maybe((lambda : hydra.lib.sets.empty()), (lambda d: collect_qualified_names_in_term(d)), cs.default)))

        case hydra.core.TermEither(value=e):
            return hydra.lib.eithers.either((lambda l: collect_qualified_names_in_term(l)), (lambda r: collect_qualified_names_in_term(r)), e)

        case hydra.core.TermInject(value=inj):
            return hydra.lib.sets.union(qualified_from_name(inj.type_name), collect_qualified_names_in_term(inj.field.term))

        case hydra.core.TermLambda(value=lam):
            return hydra.lib.sets.union(hydra.lib.maybes.maybe((lambda : hydra.lib.sets.empty()), (lambda dom_ty: collect_qualified_names_in_type(dom_ty)), lam.domain), collect_qualified_names_in_term(lam.body))

        case hydra.core.TermLet(value=lt):
            return hydra.lib.sets.union(hydra.lib.sets.unions(hydra.lib.lists.map((lambda b: collect_qualified_names_in_term(b.term)), lt.bindings)), collect_qualified_names_in_term(lt.body))

        case hydra.core.TermList(value=xs):
            return hydra.lib.sets.unions(hydra.lib.lists.map((lambda el: collect_qualified_names_in_term(el)), xs))

        case hydra.core.TermMaybe(value=mt):
            return hydra.lib.maybes.maybe((lambda : hydra.lib.sets.empty()), (lambda el: collect_qualified_names_in_term(el)), mt)

        case hydra.core.TermPair(value=p):
            return hydra.lib.sets.union(collect_qualified_names_in_term(hydra.lib.pairs.first(p)), collect_qualified_names_in_term(hydra.lib.pairs.second(p)))

        case hydra.core.TermRecord(value=r):
            return hydra.lib.sets.union(qualified_from_name(r.type_name), hydra.lib.sets.unions(hydra.lib.lists.map((lambda f: collect_qualified_names_in_term(f.term)), r.fields)))

        case hydra.core.TermTypeApplication(value=ta):
            return hydra.lib.sets.union(collect_qualified_names_in_term(ta.body), collect_qualified_names_in_type(ta.type))

        case hydra.core.TermTypeLambda(value=tl):
            return collect_qualified_names_in_term(tl.body)

        case hydra.core.TermVariable(value=n):
            return qualified_from_name(n)

        case hydra.core.TermWrap(value=wt):
            return collect_qualified_names_in_term(wt.body)

        case _:
            return hydra.lib.sets.empty()

def collect_qualified_names_in_type_scheme(ts: hydra.core.TypeScheme) -> frozenset[str]:
    r"""Collect qualified (hydra.*) Name strings from a TypeScheme's body, after stripping forall binders."""

    @lru_cache(1)
    def extracted() -> tuple[frozenlist[str], hydra.core.Type]:
        return extract_type_params(ts.type)
    @lru_cache(1)
    def body() -> hydra.core.Type:
        return hydra.lib.pairs.second(extracted())
    return collect_qualified_names_in_type(body())

def type_contains_group_ref(group_names: frozenset[str], ty: hydra.core.Type) -> bool:
    r"""Return True if the Type mentions any type variable whose local name is in the given set."""

    match ty:
        case hydra.core.TypeAnnotated(value=at):
            return type_contains_group_ref(group_names, at.body)

        case hydra.core.TypeApplication(value=app):
            return hydra.lib.logic.or_(type_contains_group_ref(group_names, app.function), type_contains_group_ref(group_names, app.argument))

        case hydra.core.TypeEither(value=et):
            return hydra.lib.logic.or_(type_contains_group_ref(group_names, et.left), type_contains_group_ref(group_names, et.right))

        case hydra.core.TypeForall(value=ft):
            return type_contains_group_ref(group_names, ft.body)

        case hydra.core.TypeFunction(value=ft2):
            return hydra.lib.logic.or_(type_contains_group_ref(group_names, ft2.domain), type_contains_group_ref(group_names, ft2.codomain))

        case hydra.core.TypeList(value=t):
            return type_contains_group_ref(group_names, t)

        case hydra.core.TypeMap(value=mt):
            return hydra.lib.logic.or_(type_contains_group_ref(group_names, mt.keys), type_contains_group_ref(group_names, mt.values))

        case hydra.core.TypeMaybe(value=t2):
            return type_contains_group_ref(group_names, t2)

        case hydra.core.TypePair(value=pt):
            return hydra.lib.logic.or_(type_contains_group_ref(group_names, pt.first), type_contains_group_ref(group_names, pt.second))

        case hydra.core.TypeSet(value=t3):
            return type_contains_group_ref(group_names, t3)

        case hydra.core.TypeVariable(value=n):
            return hydra.lib.sets.member(local_name(n.value), group_names)

        case hydra.core.TypeWrap(value=wt):
            return type_contains_group_ref(group_names, wt)

        case _:
            return False

def field_causes_positivity_issue(group_names: frozenset[str], fty: hydra.core.Type) -> bool:
    r"""Return True if the field type contains a function whose domain mentions a group member."""

    match fty:
        case hydra.core.TypeFunction(value=ft):
            return hydra.lib.logic.or_(type_contains_group_ref(group_names, ft.domain), field_causes_positivity_issue(group_names, ft.codomain))

        case hydra.core.TypeAnnotated(value=at):
            return field_causes_positivity_issue(group_names, at.body)

        case hydra.core.TypeForall(value=ft2):
            return field_causes_positivity_issue(group_names, ft2.body)

        case hydra.core.TypeWrap(value=wt):
            return field_causes_positivity_issue(group_names, wt)

        case _:
            return False

def has_positivity_issue(group_names: frozenset[str], defs: frozenlist[tuple[T0, hydra.core.Type]]):
    r"""Return True if any definition in the group has a record/union field whose type causes a positivity violation."""

    return hydra.lib.lists.foldl((lambda acc, nt: hydra.lib.logic.or_(acc, (ty := hydra.lib.pairs.second(nt), extracted := extract_type_params(ty), body_ty := hydra.lib.pairs.second(extracted), (_hoist_ty_body_1 := (lambda v1: (lambda fields: hydra.lib.lists.foldl((lambda acc2, f: hydra.lib.logic.or_(acc2, field_causes_positivity_issue(group_names, f.type))), False, fields))(v1.value) if isinstance(v1, hydra.core.TypeRecord) else (lambda fields: hydra.lib.lists.foldl((lambda acc2, f: hydra.lib.logic.or_(acc2, field_causes_positivity_issue(group_names, f.type))), False, fields))(v1.value) if isinstance(v1, hydra.core.TypeUnion) else False), _hoist_ty_body_1(body_ty))[1])[3])), False, defs)

def collect_sanitized_accessors(type_groups: frozenlist[tuple[T0, frozenlist[tuple[str, hydra.core.Type]]]]):
    r"""Return the set of decapitalized, sanitized accessor names whose fields were replaced with unit due to positivity issues."""

    return hydra.lib.sets.from_list(hydra.lib.lists.concat(hydra.lib.lists.map((lambda group: (defs := hydra.lib.pairs.second(group), group_names := hydra.lib.sets.from_list(hydra.lib.lists.map((lambda nt: hydra.lib.pairs.first(nt)), defs)), hydra.lib.logic.if_else(has_positivity_issue(group_names, defs), (lambda : hydra.lib.lists.concat(hydra.lib.lists.map((lambda nt: (type_name := hydra.lib.pairs.first(nt), ty := hydra.lib.pairs.second(nt), extracted := extract_type_params(ty), body_ty := hydra.lib.pairs.second(extracted), _hoist_type_name_body_1 := (lambda v1: (lambda fields: hydra.lib.maybes.cat(hydra.lib.lists.map((lambda f: hydra.lib.logic.if_else(field_causes_positivity_issue(group_names, f.type), (lambda : Just(hydra.lib.strings.cat((hydra.formatting.decapitalize(type_name), "_", sanitize(local_name(f.name.value)))))), (lambda : Nothing()))), fields)))(v1.value) if isinstance(v1, hydra.core.TypeRecord) else ()), _hoist_type_name_body_1(body_ty))[5]), defs))), (lambda : ())))[2]), type_groups)))

def rebuild_lets(bindings: frozenlist[hydra.core.Binding], body: hydra.core.Term) -> hydra.core.Term:
    r"""Build a chain of single-binding TermLet wrappers around the given body."""

    return hydra.lib.lists.foldr((lambda b, acc: cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let((b,), acc)))), body, bindings)

def strip_hydra_fix(b_name: hydra.core.Name, tm: hydra.core.Term):
    r"""Strip an outer hydra_fix lambda wrapper, substituting the inner self-reference for the binding name."""

    match tm:
        case hydra.core.TermApplication(value=app):
            fn = app.function
            arg = app.argument
            def _hoist_fn_body_1(v1):
                match v1:
                    case hydra.core.TermLambda(value=lam):
                        return hydra.variables.substitute_variable(lam.parameter, b_name, lam.body)

                    case _:
                        return tm
            def _hoist_fn_body_2(v1):
                match v1:
                    case hydra.core.TermVariable(value=v):
                        return hydra.lib.logic.if_else(hydra.lib.equality.equal(v.value, "hydra_fix"), (lambda : _hoist_fn_body_1(arg)), (lambda : tm))

                    case _:
                        return tm
            return _hoist_fn_body_2(fn)

        case _:
            return tm

def encode_mutual_let_group(grp: frozenlist[hydra.core.Binding], body: hydra.core.Term) -> hydra.core.Term:
    r"""Wrap a mutually recursive binding group in a hydra_fix nested-pair bundle with per-name projection lets."""

    @lru_cache(1)
    def n() -> int:
        return hydra.lib.lists.length(grp)
    bundle_name = hydra.core.Name("hydra_mutual_bundle_")
    bundle_inner = hydra.core.Name("hydra_mutual_b_")
    inner_bundle_var = cast(hydra.core.Term, hydra.core.TermVariable(bundle_inner))
    outer_bundle_var = cast(hydra.core.Term, hydra.core.TermVariable(bundle_name))
    def app_var(fname: str, v: hydra.core.Term) -> hydra.core.Term:
        return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name(fname))), v)))
    def nested_second(k: int, v: hydra.core.Term) -> hydra.core.Term:
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(k, 0), (lambda : v), (lambda : app_var("pairs.second", nested_second(hydra.lib.math.sub(k, 1), v))))
    def mk_proj(bvar: hydra.core.Term, i: int) -> hydra.core.Term:
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(i, hydra.lib.math.sub(n(), 1)), (lambda : nested_second(i, bvar)), (lambda : app_var("pairs.first", nested_second(i, bvar))))
    def mk_proj_bindings(bvar: hydra.core.Term) -> frozenlist[hydra.core.Binding]:
        return hydra.lib.lists.map((lambda ib: (i := hydra.lib.pairs.first(ib), b := hydra.lib.pairs.second(ib), hydra.core.Binding(b.name, mk_proj(bvar, i), b.type))[2]), hydra.lib.lists.zip(hydra.lib.math.range_(0, hydra.lib.math.sub(n(), 1)), grp))
    @lru_cache(1)
    def inner_proj_bindings() -> frozenlist[hydra.core.Binding]:
        return mk_proj_bindings(inner_bundle_var)
    @lru_cache(1)
    def outer_proj_bindings() -> frozenlist[hydra.core.Binding]:
        return mk_proj_bindings(outer_bundle_var)
    @lru_cache(1)
    def stripped_bindings() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.lists.map((lambda b: hydra.core.Binding(b.name, strip_hydra_fix(b.name, b.term), b.type)), grp)
    def mk_pair(ts: frozenlist[hydra.core.Term]) -> hydra.core.Term:
        return hydra.lib.maybes.from_maybe((lambda : cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("tt")))), hydra.lib.maybes.map((lambda p: hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(ts), 1), (lambda : hydra.lib.pairs.first(p)), (lambda : cast(hydra.core.Term, hydra.core.TermPair((hydra.lib.pairs.first(p), mk_pair(hydra.lib.pairs.second(p)))))))), hydra.lib.lists.uncons(ts)))
    @lru_cache(1)
    def pair_expr() -> hydra.core.Term:
        return mk_pair(hydra.lib.lists.map((lambda b: b.term), stripped_bindings()))
    @lru_cache(1)
    def fix_body() -> hydra.core.Term:
        return rebuild_lets(inner_proj_bindings(), pair_expr())
    @lru_cache(1)
    def fix_term() -> hydra.core.Term:
        return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra_fix"))), cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(bundle_inner, Nothing(), fix_body()))))))
    bundle_binding = hydra.core.Binding(bundle_name, fix_term(), Nothing())
    return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let((bundle_binding,), rebuild_lets(outer_proj_bindings(), body))))

def has_unbound_type_var(bound: frozenset[str], ty: hydra.core.Type) -> bool:
    r"""Return True if the type mentions a t<digits> variable not present in the given set."""

    match ty:
        case hydra.core.TypeAnnotated(value=at):
            return has_unbound_type_var(bound, at.body)

        case hydra.core.TypeApplication(value=app):
            return hydra.lib.logic.or_(has_unbound_type_var(bound, app.function), has_unbound_type_var(bound, app.argument))

        case hydra.core.TypeFunction(value=ft):
            return hydra.lib.logic.or_(has_unbound_type_var(bound, ft.domain), has_unbound_type_var(bound, ft.codomain))

        case hydra.core.TypeList(value=t):
            return has_unbound_type_var(bound, t)

        case hydra.core.TypeMap(value=mt):
            return hydra.lib.logic.or_(has_unbound_type_var(bound, mt.keys), has_unbound_type_var(bound, mt.values))

        case hydra.core.TypeMaybe(value=t2):
            return has_unbound_type_var(bound, t2)

        case hydra.core.TypePair(value=pt):
            return hydra.lib.logic.or_(has_unbound_type_var(bound, pt.first), has_unbound_type_var(bound, pt.second))

        case hydra.core.TypeSet(value=t3):
            return has_unbound_type_var(bound, t3)

        case hydra.core.TypeVariable(value=n):
            nm = n.value
            return hydra.lib.logic.and_(is_type_var_like(nm), hydra.lib.logic.not_(hydra.lib.sets.member(nm, bound)))

        case _:
            return False

def erase_unbound_type_var_domains(initial_bound: frozenset[str], term0: hydra.core.Term) -> hydra.core.Term:
    r"""Erase lambda domain annotations referencing unbound type variables; recurse under new type binders."""

    def erase_if_unbound(bound: frozenset[str], mdom: Maybe[hydra.core.Type]) -> Maybe[hydra.core.Type]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda ty: hydra.lib.logic.if_else(has_unbound_type_var(bound, ty), (lambda : Nothing()), (lambda : Just(ty)))), mdom)
    def f(recurse: Callable[[frozenset[str], hydra.core.Term], hydra.core.Term], bound: frozenset[str], term: hydra.core.Term) -> hydra.core.Term:
        match term:
            case hydra.core.TermLambda(value=lam):
                param_name = lam.parameter.value
                dom = lam.domain
                @lru_cache(1)
                def is_type_param():
                    def _hoist_is_type_param_1(v1):
                        match v1:
                            case hydra.core.TypeVariable(value=v):
                                return hydra.lib.equality.equal(v.value, "Type")

                            case _:
                                return False
                    return hydra.lib.maybes.maybe((lambda : False), (lambda d: _hoist_is_type_param_1(d)), dom)
                @lru_cache(1)
                def bound2() -> frozenset[str]:
                    return hydra.lib.logic.if_else(hydra.lib.logic.and_(is_type_param(), is_type_var_like(param_name)), (lambda : hydra.lib.sets.insert(param_name, bound)), (lambda : bound))
                return cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(lam.parameter, erase_if_unbound(bound, dom), f(recurse, bound2(), lam.body))))

            case _:
                return recurse(bound, term)
    return hydra.rewriting.rewrite_term_with_context((lambda x1, x2, x3: f(x1, x2, x3)), initial_bound, term0)

def extract_qualified_namespace(s: str) -> str:
    r"""Extract the namespace (everything except the last dot-separated component) from a qualified Hydra name."""

    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.strings.split_on(".", s)
    return hydra.lib.logic.if_else(hydra.lib.equality.gte(hydra.lib.lists.length(parts()), 2), (lambda : hydra.lib.strings.intercalate(".", hydra.lib.maybes.from_maybe((lambda : ()), hydra.lib.lists.maybe_init(parts())))), (lambda : s))

def is_type_lambda_term(tm: hydra.core.Term) -> bool:
    r"""Return True if a Term (possibly under TermAnnotated wrappers) is a TermTypeLambda."""

    while True:
        match tm:
            case hydra.core.TermAnnotated(value=at):
                tm = at.body
                continue

            case hydra.core.TermTypeLambda():
                return True

            case _:
                return False

def module_dependencies(m: hydra.packaging.Module) -> frozenlist[str]:
    r"""Return the deduplicated list of dependency namespace strings for a Module, excluding its own namespace."""

    @lru_cache(1)
    def type_deps() -> frozenlist[str]:
        return hydra.lib.lists.map((lambda ns: ns.value), m.type_dependencies)
    @lru_cache(1)
    def term_deps() -> frozenlist[str]:
        return hydra.lib.lists.map((lambda ns: ns.value), m.term_dependencies)
    own_ns = m.namespace.value
    @lru_cache(1)
    def all_deps() -> frozenlist[str]:
        return hydra.lib.lists.concat2(type_deps(), term_deps())
    @lru_cache(1)
    def filtered() -> frozenlist[str]:
        return hydra.lib.lists.filter((lambda s: hydra.lib.logic.not_(hydra.lib.equality.equal(s, own_ns))), all_deps())
    return hydra.lib.lists.nub(filtered())

def targets_poly_name(poly_names: frozenset[str], tm: hydra.core.Term) -> bool:
    r"""Return True if the innermost target of a (possibly nested) type application is a poly-converted local name."""

    while True:
        match tm:
            case hydra.core.TermVariable(value=v):
                return hydra.lib.sets.member(v.value, poly_names)

            case hydra.core.TermTypeApplication(value=ta):
                poly_names = poly_names
                tm = ta.body
                continue

            case hydra.core.TermAnnotated(value=at):
                poly_names = poly_names
                tm = at.body
                continue

            case _:
                return False

def type_to_term(ty: hydra.core.Type) -> hydra.core.Term:
    r"""Convert a Hydra Type to a placeholder Term for use as an explicit Coq type argument. Coq-builtin type constructors are marked with a `Coq.` prefix so the encoder can emit them raw without going through sanitizeVar, which would clash with user-level lambda parameters of the same name (e.g. `list` -> `list_`)."""

    match ty:
        case hydra.core.TypeVariable(value=v):
            return cast(hydra.core.Term, hydra.core.TermVariable(v))

        case hydra.core.TypeList(value=t):
            return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("Coq.list"))), type_to_term(t))))

        case hydra.core.TypeMaybe(value=t2):
            return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("Coq.option"))), type_to_term(t2))))

        case hydra.core.TypeSet(value=t3):
            return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("Coq.list"))), type_to_term(t3))))

        case hydra.core.TypeApplication(value=at):
            return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(type_to_term(at.function), type_to_term(at.argument))))

        case hydra.core.TypeFunction():
            return cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("Coq.unit")))

        case hydra.core.TypePair(value=pt):
            return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("Coq.prod"))), type_to_term(pt.first)))), type_to_term(pt.second))))

        case hydra.core.TypeMap(value=mt):
            return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("Coq.list"))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("Coq.prod"))), type_to_term(mt.keys)))))))

        case hydra.core.TypeUnit():
            return cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("Coq.unit")))

        case hydra.core.TypeLiteral():
            return cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("Coq.unit")))

        case hydra.core.TypeEither(value=et):
            return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("Coq.sum"))), type_to_term(et.left)))), type_to_term(et.right))))

        case hydra.core.TypeRecord():
            return cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("Coq.unit")))

        case hydra.core.TypeUnion():
            return cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("Coq.unit")))

        case hydra.core.TypeWrap(value=wt):
            return type_to_term(wt)

        case hydra.core.TypeAnnotated(value=at2):
            return type_to_term(at2.body)

        case hydra.core.TypeForall(value=ft):
            return type_to_term(ft.body)

        case hydra.core.TypeVoid():
            return cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("Coq.Empty_set")))

        case _:
            return cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("Coq.unit")))

def normalize_inner_type_lambdas(term: hydra.core.Term) -> hydra.core.Term:
    r"""Rewrite inner TermTypeLambda nodes and type applications so that polymorphic helpers work under Coq's erasure-based encoding."""

    def strip_type_lambdas(tm: hydra.core.Term) -> tuple[frozenlist[str], hydra.core.Term]:
        match tm:
            case hydra.core.TermTypeLambda(value=tl):
                @lru_cache(1)
                def rest() -> tuple[frozenlist[str], hydra.core.Term]:
                    return strip_type_lambdas(tl.body)
                return (hydra.lib.lists.cons(tl.parameter.value, hydra.lib.pairs.first(rest())), hydra.lib.pairs.second(rest()))

            case _:
                return ((), tm)
    def rebuild_type_lambdas(params: frozenlist[str], body: hydra.core.Term) -> hydra.core.Term:
        return hydra.lib.lists.foldr((lambda p, acc: cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(hydra.core.Name(p), acc)))), body, params)
    def f(recurse: Callable[[frozenset[str], hydra.core.Term], hydra.core.Term], poly_names: frozenset[str], tm: hydra.core.Term) -> hydra.core.Term:
        match tm:
            case hydra.core.TermLet(value=lt):
                @lru_cache(1)
                def new_poly() -> frozenset[str]:
                    return hydra.lib.sets.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda b: hydra.lib.logic.if_else(is_type_lambda_term(b.term), (lambda : Just(b.name.value)), (lambda : Nothing()))), lt.bindings)))
                @lru_cache(1)
                def poly_names2() -> frozenset[str]:
                    return hydra.lib.sets.union(poly_names, new_poly())
                return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.map((lambda b: hydra.core.Binding(b.name, f(recurse, poly_names2(), b.term), hydra.lib.logic.if_else(is_type_lambda_term(b.term), (lambda : Nothing()), (lambda : b.type)))), lt.bindings), f(recurse, poly_names2(), lt.body))))

            case hydra.core.TermTypeLambda(value=tl):
                return f(recurse, poly_names, cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(tl.parameter, Just(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("Type")))), tl.body))))

            case hydra.core.TermTypeApplication(value=ta):
                body = ta.body
                ttype = ta.type
                return hydra.lib.logic.if_else(targets_poly_name(poly_names, body), (lambda : f(recurse, poly_names, cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(body, type_to_term(ttype)))))), (lambda : f(recurse, poly_names, body)))

            case _:
                return recurse(poly_names, tm)
    @lru_cache(1)
    def stripped() -> tuple[frozenlist[str], hydra.core.Term]:
        return strip_type_lambdas(term)
    @lru_cache(1)
    def outer_params() -> frozenlist[str]:
        return hydra.lib.pairs.first(stripped())
    @lru_cache(1)
    def body0() -> hydra.core.Term:
        return hydra.lib.pairs.second(stripped())
    return hydra.lib.logic.if_else(hydra.lib.lists.null(outer_params()), (lambda : term), (lambda : rebuild_type_lambdas(outer_params(), hydra.rewriting.rewrite_term_with_context((lambda x1, x2, x3: f(x1, x2, x3)), hydra.lib.sets.empty(), body0()))))

def process_let_s_c_cs(bindings: frozenlist[hydra.core.Binding]) -> frozenlist[frozenlist[hydra.core.Binding]]:
    r"""Sort bindings into SCC groups using free-var analysis and local-name matching."""

    def get_name(b: hydra.core.Binding) -> str:
        return b.name.value
    @lru_cache(1)
    def names() -> frozenset[str]:
        return hydra.lib.sets.from_list(hydra.lib.lists.map((lambda x1: get_name(x1)), bindings))
    @lru_cache(1)
    def local_names() -> frozenset[str]:
        return hydra.lib.sets.from_list(hydra.lib.lists.map((lambda b: local_name_raw(get_name(b))), bindings))
    @lru_cache(1)
    def all_names() -> frozenset[str]:
        return hydra.lib.sets.union(names(), local_names())
    def dep_vars(b: hydra.core.Binding) -> frozenlist[str]:
        @lru_cache(1)
        def vars_name() -> frozenset[hydra.core.Name]:
            return hydra.variables.free_variables_in_term(b.term)
        @lru_cache(1)
        def vars() -> frozenset[str]:
            return hydra.lib.sets.from_list(hydra.lib.lists.map((lambda n: n.value), hydra.lib.sets.to_list(vars_name())))
        @lru_cache(1)
        def local_vars() -> frozenset[str]:
            return hydra.lib.sets.from_list(hydra.lib.lists.map((lambda v: local_name_raw(v)), hydra.lib.sets.to_list(vars())))
        return hydra.lib.sets.to_list(hydra.lib.sets.intersection(all_names(), hydra.lib.sets.union(vars(), local_vars())))
    return hydra.sorting.topological_sort_nodes((lambda x1: get_name(x1)), (lambda x1: dep_vars(x1)), bindings)

def rebuild_mutual_lets(groups: frozenlist[frozenlist[hydra.core.Binding]], body: hydra.core.Term) -> hydra.core.Term:
    r"""Rebuild a chain of TermLet/hydra_fix wrappers from SCC-sorted binding groups."""

    return hydra.lib.lists.foldr((lambda grp, acc: hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(grp), 1), (lambda : cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(grp, acc)))), (lambda : encode_mutual_let_group(grp, acc)))), body, groups)

def reorder_let_bindings(term0: hydra.core.Term) -> hydra.core.Term:
    r"""Topologically reorder let bindings and pair-encode mutually recursive groups."""

    def f(recurse: Callable[[hydra.core.Term], hydra.core.Term], tm: hydra.core.Term) -> hydra.core.Term:
        match tm:
            case hydra.core.TermLet():
                @lru_cache(1)
                def flat() -> tuple[frozenlist[hydra.core.Binding], hydra.core.Term]:
                    return collect_let_bindings(tm)
                @lru_cache(1)
                def all_bindings() -> frozenlist[hydra.core.Binding]:
                    return hydra.lib.pairs.first(flat())
                @lru_cache(1)
                def inner_body() -> hydra.core.Term:
                    return hydra.lib.pairs.second(flat())
                @lru_cache(1)
                def groups() -> frozenlist[frozenlist[hydra.core.Binding]]:
                    return process_let_s_c_cs(all_bindings())
                @lru_cache(1)
                def groups2() -> frozenlist[frozenlist[hydra.core.Binding]]:
                    return hydra.lib.lists.map((lambda grp: hydra.lib.lists.map((lambda b: hydra.core.Binding(b.name, reorder_let_bindings(b.term), b.type)), grp)), groups())
                return rebuild_mutual_lets(groups2(), reorder_let_bindings(inner_body()))

            case _:
                return recurse(tm)
    return hydra.rewriting.rewrite_term((lambda x1, x2: f(x1, x2)), term0)

def rewrite_term_fields(fm: FrozenDict[tuple[str, str], str], term0: hydra.core.Term) -> hydra.core.Term:
    r"""Replace field names in TermProject nodes using the given (typeName, rawFieldName) -> prefixedName map."""

    def rewrite(recurse: Callable[[hydra.core.Term], hydra.core.Term], term: hydra.core.Term) -> hydra.core.Term:
        match term:
            case hydra.core.TermProject(value=p):
                tname = p.type_name.value
                @lru_cache(1)
                def raw_fn() -> str:
                    return local_name_raw(p.field.value)
                @lru_cache(1)
                def key() -> tuple[str, str]:
                    return (tname, raw_fn())
                @lru_cache(1)
                def new_fname() -> hydra.core.Name:
                    return hydra.lib.maybes.from_maybe((lambda : p.field), hydra.lib.maybes.map((lambda s: hydra.core.Name(s)), hydra.lib.maps.lookup(key(), fm)))
                return cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(p.type_name, new_fname())))

            case _:
                return recurse(term)
    return hydra.rewriting.rewrite_term((lambda x1, x2: rewrite(x1, x2)), term0)

def sanitize_positivity(group_names: frozenset[str], ty: hydra.core.Type) -> hydra.core.Type:
    r"""Rewrite a Type, replacing offending record/union fields with TypeUnit and restoring forall binders."""

    @lru_cache(1)
    def extracted() -> tuple[frozenlist[str], hydra.core.Type]:
        return extract_type_params(ty)
    @lru_cache(1)
    def params() -> frozenlist[str]:
        return hydra.lib.pairs.first(extracted())
    @lru_cache(1)
    def body_ty() -> hydra.core.Type:
        return hydra.lib.pairs.second(extracted())
    def sanitize_field(f: hydra.core.FieldType) -> hydra.core.FieldType:
        return hydra.lib.logic.if_else(field_causes_positivity_issue(group_names, f.type), (lambda : hydra.core.FieldType(f.name, cast(hydra.core.Type, hydra.core.TypeUnit()))), (lambda : f))
    @lru_cache(1)
    def sanitized() -> hydra.core.Type:
        match body_ty():
            case hydra.core.TypeRecord(value=fields):
                return cast(hydra.core.Type, hydra.core.TypeRecord(hydra.lib.lists.map((lambda x1: sanitize_field(x1)), fields)))

            case hydra.core.TypeUnion(value=fields2):
                return cast(hydra.core.Type, hydra.core.TypeUnion(hydra.lib.lists.map((lambda x1: sanitize_field(x1)), fields2)))

            case _:
                return body_ty()
    return hydra.lib.lists.foldr((lambda p, t: cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name(p), t)))), sanitized(), params())

def term_refs(locals: frozenset[str], tm: hydra.core.Term) -> frozenset[str]:
    r"""Walk a Term and collect the local names it references, intersected with the given locally-defined names."""

    match tm:
        case hydra.core.TermAnnotated(value=at):
            return term_refs(locals, at.body)

        case hydra.core.TermApplication(value=app):
            return hydra.lib.sets.union(term_refs(locals, app.function), term_refs(locals, app.argument))

        case hydra.core.TermCases(value=cs):
            return hydra.lib.sets.union(hydra.lib.sets.unions(hydra.lib.lists.map((lambda f: term_refs(locals, f.term)), cs.cases)), hydra.lib.maybes.maybe((lambda : hydra.lib.sets.empty()), (lambda d: term_refs(locals, d)), cs.default))

        case hydra.core.TermEither(value=e):
            return hydra.lib.eithers.either((lambda l: term_refs(locals, l)), (lambda r: term_refs(locals, r)), e)

        case hydra.core.TermInject(value=inj):
            return term_refs(locals, inj.field.term)

        case hydra.core.TermLambda(value=lam):
            return term_refs(locals, lam.body)

        case hydra.core.TermLet(value=lt):
            return hydra.lib.sets.union(hydra.lib.sets.unions(hydra.lib.lists.map((lambda b: term_refs(locals, b.term)), lt.bindings)), term_refs(locals, lt.body))

        case hydra.core.TermList(value=xs):
            return hydra.lib.sets.unions(hydra.lib.lists.map((lambda el: term_refs(locals, el)), xs))

        case hydra.core.TermMaybe(value=mt):
            return hydra.lib.maybes.maybe((lambda : hydra.lib.sets.empty()), (lambda el: term_refs(locals, el)), mt)

        case hydra.core.TermPair(value=p):
            return hydra.lib.sets.union(term_refs(locals, hydra.lib.pairs.first(p)), term_refs(locals, hydra.lib.pairs.second(p)))

        case hydra.core.TermRecord(value=r):
            return hydra.lib.sets.unions(hydra.lib.lists.map((lambda f: term_refs(locals, f.term)), r.fields))

        case hydra.core.TermTypeApplication(value=ta):
            return term_refs(locals, ta.body)

        case hydra.core.TermTypeLambda(value=tl):
            return term_refs(locals, tl.body)

        case hydra.core.TermVariable(value=n):
            @lru_cache(1)
            def local() -> str:
                return local_name(n.value)
            return hydra.lib.logic.if_else(hydra.lib.sets.member(local(), locals), (lambda : hydra.lib.sets.singleton(local())), (lambda : hydra.lib.sets.empty()))

        case hydra.core.TermWrap(value=wt):
            return term_refs(locals, wt.body)

        case _:
            return hydra.lib.sets.empty()

def sort_term_defs_s_c_c(defs: frozenlist[tuple[str, hydra.core.Term]]) -> frozenlist[tuple[bool, frozenlist[tuple[str, hydra.core.Term]]]]:
    r"""Group term definitions into SCC components with a cyclic/acyclic flag."""

    @lru_cache(1)
    def local_names() -> frozenset[str]:
        return hydra.lib.sets.from_list(hydra.lib.lists.map((lambda d: hydra.lib.pairs.first(d)), defs))
    def deps_of(d: tuple[T0, hydra.core.Term]) -> frozenlist[str]:
        return hydra.lib.sets.to_list(term_refs(local_names(), hydra.lib.pairs.second(d)))
    @lru_cache(1)
    def comps() -> frozenlist[frozenlist[tuple[str, hydra.core.Term]]]:
        return hydra.sorting.topological_sort_nodes((lambda d: hydra.lib.pairs.first(d)), (lambda x1: deps_of(x1)), defs)
    return hydra.lib.lists.map((lambda grp: hydra.lib.logic.if_else(hydra.lib.equality.gte(hydra.lib.lists.length(grp), 2), (lambda : (True, grp)), (lambda : hydra.lib.maybes.from_maybe((lambda : (False, grp)), hydra.lib.maybes.map((lambda d: (name := hydra.lib.pairs.first(d), deps := term_refs(local_names(), hydra.lib.pairs.second(d)), (hydra.lib.sets.member(name, deps), grp))[2]), hydra.lib.lists.maybe_head(grp)))))), comps())

def type_refs(locals: frozenset[str], ty: hydra.core.Type) -> frozenset[str]:
    r"""Walk a Type and collect the local names it references, intersected with the given locally-defined names."""

    match ty:
        case hydra.core.TypeAnnotated(value=at):
            return type_refs(locals, at.body)

        case hydra.core.TypeApplication(value=app):
            return hydra.lib.sets.union(type_refs(locals, app.function), type_refs(locals, app.argument))

        case hydra.core.TypeEither(value=et):
            return hydra.lib.sets.union(type_refs(locals, et.left), type_refs(locals, et.right))

        case hydra.core.TypeForall(value=ft):
            return type_refs(locals, ft.body)

        case hydra.core.TypeFunction(value=ft2):
            return hydra.lib.sets.union(type_refs(locals, ft2.domain), type_refs(locals, ft2.codomain))

        case hydra.core.TypeList(value=t):
            return type_refs(locals, t)

        case hydra.core.TypeMap(value=mt):
            return hydra.lib.sets.union(type_refs(locals, mt.keys), type_refs(locals, mt.values))

        case hydra.core.TypeMaybe(value=t2):
            return type_refs(locals, t2)

        case hydra.core.TypePair(value=pt):
            return hydra.lib.sets.union(type_refs(locals, pt.first), type_refs(locals, pt.second))

        case hydra.core.TypeRecord(value=fields):
            return hydra.lib.sets.unions(hydra.lib.lists.map((lambda f: type_refs(locals, f.type)), fields))

        case hydra.core.TypeSet(value=t3):
            return type_refs(locals, t3)

        case hydra.core.TypeUnion(value=fields2):
            return hydra.lib.sets.unions(hydra.lib.lists.map((lambda f: type_refs(locals, f.type)), fields2))

        case hydra.core.TypeVariable(value=n):
            @lru_cache(1)
            def local() -> str:
                return local_name(n.value)
            return hydra.lib.logic.if_else(hydra.lib.sets.member(local(), locals), (lambda : hydra.lib.sets.singleton(local())), (lambda : hydra.lib.sets.empty()))

        case hydra.core.TypeWrap(value=wt):
            return type_refs(locals, wt)

        case _:
            return hydra.lib.sets.empty()

def sort_type_defs_s_c_c(defs: frozenlist[tuple[str, hydra.core.Type]]) -> frozenlist[tuple[bool, frozenlist[tuple[str, hydra.core.Type]]]]:
    r"""Group type definitions into SCC components with a cyclic/acyclic flag."""

    @lru_cache(1)
    def local_names() -> frozenset[str]:
        return hydra.lib.sets.from_list(hydra.lib.lists.map((lambda d: hydra.lib.pairs.first(d)), defs))
    def deps_of(d: tuple[T0, hydra.core.Type]) -> frozenlist[str]:
        return hydra.lib.sets.to_list(type_refs(local_names(), hydra.lib.pairs.second(d)))
    @lru_cache(1)
    def comps() -> frozenlist[frozenlist[tuple[str, hydra.core.Type]]]:
        return hydra.sorting.topological_sort_nodes((lambda d: hydra.lib.pairs.first(d)), (lambda x1: deps_of(x1)), defs)
    return hydra.lib.lists.map((lambda grp: hydra.lib.logic.if_else(hydra.lib.equality.gte(hydra.lib.lists.length(grp), 2), (lambda : (True, grp)), (lambda : hydra.lib.maybes.from_maybe((lambda : (False, grp)), hydra.lib.maybes.map((lambda d: (name := hydra.lib.pairs.first(d), deps := type_refs(local_names(), hydra.lib.pairs.second(d)), (hydra.lib.sets.member(name, deps), grp))[2]), hydra.lib.lists.maybe_head(grp)))))), comps())
