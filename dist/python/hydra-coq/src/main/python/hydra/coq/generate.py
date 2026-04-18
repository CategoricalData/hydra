# Note: this is an automatically generated file. Do not edit.

r"""Coq code generation driver — pre-passes, sentence producers, and per-module pipeline."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.coq.coder
import hydra.coq.environment
import hydra.coq.serde
import hydra.coq.syntax
import hydra.coq.utils
import hydra.core
import hydra.formatting
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
import hydra.packaging
import hydra.serialization

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def dependency_imports(deps: frozenlist[str]) -> frozenlist[hydra.coq.syntax.Sentence]:
    r"""Emit a Require Import sentence for the given dependency namespaces; empty list yields no sentence."""

    return hydra.lib.logic.if_else(hydra.lib.lists.null(deps), (lambda : ()), (lambda : (hydra.coq.syntax.Sentence(Just(hydra.coq.syntax.Comment("Module dependencies")), cast(hydra.coq.syntax.SentenceContent, hydra.coq.syntax.SentenceContentRequireImport(hydra.coq.syntax.RequireImport(Nothing(), True, Just(hydra.coq.syntax.ImportQualification.IMPORT), hydra.lib.lists.map((lambda d: hydra.coq.coder.coq_qualid(d)), deps))))),)))

def build_axiom_only_content(env: hydra.coq.environment.CoqEnvironment, desc: str, ns_str: T0, type_defs: frozenlist[tuple[str, T1]], term_defs: frozenlist[tuple[str, tuple[T2, tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.Type]]]]], mod_: hydra.packaging.Module) -> str:
    r"""Render an axiom-only Coq module: imports + dependency imports + Axiom declarations."""

    type_of_type = cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("Type")))
    @lru_cache(1)
    def type_axioms() -> frozenlist[hydra.coq.syntax.Sentence]:
        return hydra.lib.lists.map((lambda nt: hydra.coq.coder.encode_axiom_definition_pair(env, (hydra.lib.pairs.first(nt), type_of_type))), type_defs)
    @lru_cache(1)
    def term_axioms() -> frozenlist[hydra.coq.syntax.Sentence]:
        return hydra.lib.maybes.cat(hydra.lib.lists.map((lambda td: (name := hydra.lib.pairs.first(td), tvars := hydra.lib.pairs.first(hydra.lib.pairs.second(hydra.lib.pairs.second(td))), mty := hydra.lib.pairs.second(hydra.lib.pairs.second(hydra.lib.pairs.second(td))), hydra.lib.maybes.maybe((lambda : Nothing()), (lambda scheme_ty: (wrapped := hydra.lib.lists.foldr((lambda v, t: cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(v, t)))), scheme_ty, tvars), Just(hydra.coq.coder.encode_axiom_definition_pair(env, (name, wrapped))))[1]), mty))[3]), term_defs))
    @lru_cache(1)
    def deps() -> frozenlist[str]:
        return hydra.coq.utils.module_dependencies(mod_)
    @lru_cache(1)
    def dep_sentences() -> frozenlist[hydra.coq.syntax.Sentence]:
        return dependency_imports(deps())
    @lru_cache(1)
    def all_sentences() -> frozenlist[hydra.coq.syntax.Sentence]:
        return hydra.lib.lists.cons(hydra.coq.coder.standard_imports(), hydra.lib.lists.concat2(dep_sentences(), hydra.lib.lists.concat2(type_axioms(), term_axioms())))
    doc_ = hydra.coq.syntax.Document(all_sentences())
    @lru_cache(1)
    def body() -> str:
        return hydra.serialization.print_expr(hydra.serialization.parenthesize(hydra.coq.serde.document_to_expr(doc_)))
    return hydra.lib.strings.cat((desc, body(), "\n"))

def implicit_args_line(name: str, type_var_names: frozenlist[str]) -> str:
    r"""Emit an Arguments line marking every type parameter of a definition as implicit."""

    return hydra.lib.logic.if_else(hydra.lib.lists.null(type_var_names), (lambda : ""), (lambda : hydra.lib.strings.cat(("Arguments ", name, " ", hydra.lib.strings.intercalate(" ", hydra.lib.lists.map((lambda v: hydra.lib.strings.cat(("{", v, "}"))), type_var_names)), ".\n"))))

def make_prod_type(ts: frozenlist[str]) -> str:
    r"""Emit nested `prod (T1) (prod ...)` textual type expression."""

    return hydra.lib.maybes.from_maybe((lambda : "unit"), hydra.lib.maybes.map((lambda p: hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(ts), 1), (lambda : hydra.lib.pairs.first(p)), (lambda : hydra.lib.strings.cat(("prod (", hydra.lib.pairs.first(p), ") (", make_prod_type(hydra.lib.pairs.second(p)), ")"))))), hydra.lib.lists.uncons(ts)))

def make_prod_val(bs: frozenlist[str]) -> str:
    r"""Emit a nested `(pair (b1) (...))` textual value expression."""

    return hydra.lib.maybes.from_maybe((lambda : "tt"), hydra.lib.maybes.map((lambda p: hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(bs), 1), (lambda : hydra.lib.pairs.first(p)), (lambda : hydra.lib.strings.cat(("(pair (", hydra.lib.pairs.first(p), ") (", make_prod_val(hydra.lib.pairs.second(p)), "))"))))), hydra.lib.lists.uncons(bs)))

def make_projection_exprs(n: int, bvar: str) -> frozenlist[str]:
    r"""Emit the n projection expressions extracting each member of a nested pair bundle."""

    def snds(k: int, v: str) -> str:
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(k, 0), (lambda : v), (lambda : snds(hydra.lib.math.sub(k, 1), hydra.lib.strings.cat(("(snd ", v, ")")))))
    def mk_proj(i: int, total: int, v: str) -> str:
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(i, 0), (lambda : hydra.lib.strings.cat(("(fst ", v, ")"))), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(i, hydra.lib.math.sub(total, 1)), (lambda : snds(i, v)), (lambda : hydra.lib.strings.cat(("(fst ", snds(i, v), ")"))))))
    return hydra.lib.logic.if_else(hydra.lib.equality.lte(n, 0), (lambda : ()), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(n, 1), (lambda : (bvar,)), (lambda : hydra.lib.lists.map((lambda i: mk_proj(i, n, bvar)), hydra.lib.math.range_(0, hydra.lib.math.sub(n, 1)))))))

def make_type_binder(p: str) -> hydra.coq.syntax.Binder:
    r"""Build a Coq `(p : Type)` binder for a type parameter."""

    return cast(hydra.coq.syntax.Binder, hydra.coq.syntax.BinderType(hydra.coq.syntax.TypeBinders((hydra.coq.coder.coq_name(p),), hydra.coq.syntax.Type(hydra.coq.coder.coq_term_qualid("Type")))))

def mk_type_binders(body: hydra.core.Term, type_vars: frozenlist[hydra.core.Name]) -> tuple[frozenlist[str], frozenlist[hydra.coq.syntax.Binder]]:
    r"""Collect type-variable names and the Coq binders needed for a term definition."""

    @lru_cache(1)
    def scheme_var_names() -> frozenset[str]:
        return hydra.lib.sets.from_list(hydra.lib.lists.map((lambda n: n.value), type_vars))
    @lru_cache(1)
    def inner_type_vars() -> frozenset[str]:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(type_vars), (lambda : hydra.lib.sets.empty()), (lambda : hydra.coq.utils.collect_free_type_vars(body)))
    @lru_cache(1)
    def explicit() -> frozenlist[str]:
        return hydra.lib.lists.map((lambda n: n.value), type_vars)
    @lru_cache(1)
    def extras() -> frozenlist[str]:
        return hydra.lib.lists.filter((lambda nm: hydra.lib.logic.not_(hydra.lib.sets.member(nm, scheme_var_names()))), hydra.lib.sets.to_list(inner_type_vars()))
    @lru_cache(1)
    def all_type_var_names() -> frozenlist[str]:
        return hydra.lib.lists.nub(hydra.lib.lists.concat2(explicit(), extras()))
    @lru_cache(1)
    def binders() -> frozenlist[hydra.coq.syntax.Binder]:
        return hydra.lib.lists.map((lambda v: make_type_binder(v)), all_type_var_names())
    return (all_type_var_names(), binders())

def replace_bundle(s: str, bname: str) -> str:
    r"""Replace literal `bundle_` with the given replacement string."""

    return hydra.lib.strings.intercalate(bname, hydra.lib.strings.split_on("bundle_", s))

def encode_mutual_group_text(env: hydra.coq.environment.CoqEnvironment, group: frozenlist[tuple[str, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.Type]]]]]) -> str:
    r"""Render a mutually recursive term group as a hydra_fix bundle plus projection Definitions."""

    @lru_cache(1)
    def group_scheme_vars() -> frozenset[str]:
        return hydra.lib.sets.from_list(hydra.lib.lists.concat(hydra.lib.lists.map((lambda td: (rest1 := hydra.lib.pairs.second(td), rest2 := hydra.lib.pairs.second(rest1), tv := hydra.lib.pairs.first(rest2), hydra.lib.lists.map((lambda n: n.value), tv))[3]), group)))
    @lru_cache(1)
    def fun_infos() -> frozenlist[tuple[str, tuple[str, str]]]:
        return hydra.lib.lists.map((lambda td: (name := hydra.lib.pairs.first(td), rest1 := hydra.lib.pairs.second(td), body := hydra.lib.pairs.first(rest1), rest2 := hydra.lib.pairs.second(rest1), m_type := hydra.lib.pairs.second(rest2), body2 := hydra.coq.utils.reorder_let_bindings(hydra.coq.utils.erase_unbound_type_var_domains(group_scheme_vars(), body)), coq_body := hydra.coq.coder.encode_term(env, body2), body_text := hydra.serialization.print_expr(hydra.serialization.parenthesize(hydra.coq.serde.term_to_expr(coq_body))), type_text := hydra.lib.maybes.maybe((lambda : "_"), (lambda ty: (ep := hydra.coq.utils.extract_type_params(ty), body_ty := hydra.lib.pairs.second(ep), hydra.serialization.print_expr(hydra.serialization.parenthesize(hydra.coq.serde.type_to_expr(hydra.coq.syntax.Type(hydra.coq.coder.encode_type(env, body_ty))))))[2]), m_type), (name, (type_text, body_text)))[9]), group)
    @lru_cache(1)
    def all_type_var_names() -> frozenlist[str]:
        return hydra.lib.lists.nub(hydra.lib.lists.concat(hydra.lib.lists.map((lambda td: (rest1 := hydra.lib.pairs.second(td), b := hydra.lib.pairs.first(rest1), rest2 := hydra.lib.pairs.second(rest1), tv := hydra.lib.pairs.first(rest2), binders := mk_type_binders(b, tv), hydra.lib.pairs.first(binders))[5]), group)))
    @lru_cache(1)
    def names() -> frozenlist[str]:
        return hydra.lib.lists.map((lambda fi: hydra.lib.pairs.first(fi)), fun_infos())
    @lru_cache(1)
    def bundle_name() -> str:
        return hydra.lib.strings.cat((hydra.lib.strings.intercalate("_", hydra.lib.lists.take(2, names())), "_bundle"))
    @lru_cache(1)
    def n() -> int:
        return hydra.lib.lists.length(fun_infos())
    @lru_cache(1)
    def types() -> frozenlist[str]:
        return hydra.lib.lists.map((lambda fi: hydra.lib.pairs.first(hydra.lib.pairs.second(fi))), fun_infos())
    @lru_cache(1)
    def product_type() -> str:
        return make_prod_type(types())
    @lru_cache(1)
    def proj_exprs() -> frozenlist[str]:
        return make_projection_exprs(n(), "bundle_")
    @lru_cache(1)
    def let_parts() -> frozenlist[str]:
        return hydra.lib.lists.map((lambda np: (nm := hydra.lib.pairs.first(hydra.lib.pairs.first(np)), proj := hydra.lib.pairs.second(np), hydra.lib.strings.cat((nm, " := ", proj)))[2]), hydra.lib.lists.zip(fun_infos(), proj_exprs()))
    @lru_cache(1)
    def let_block() -> str:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(let_parts()), (lambda : ""), (lambda : hydra.lib.strings.cat(("let ", hydra.lib.strings.intercalate(" in\n    let ", let_parts()), " in\n    "))))
    @lru_cache(1)
    def bodies() -> frozenlist[str]:
        return hydra.lib.lists.map((lambda fi: hydra.lib.pairs.second(hydra.lib.pairs.second(fi))), fun_infos())
    @lru_cache(1)
    def prod_val() -> str:
        return make_prod_val(bodies())
    @lru_cache(1)
    def typ_bind_text() -> str:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(all_type_var_names()), (lambda : ""), (lambda : hydra.lib.strings.cat((" ", hydra.lib.strings.intercalate(" ", hydra.lib.lists.map((lambda v: hydra.lib.strings.cat(("(", v, " : Type)"))), all_type_var_names()))))))
    @lru_cache(1)
    def bundle_args_line() -> str:
        return implicit_args_line(bundle_name(), all_type_var_names())
    @lru_cache(1)
    def bundle_def() -> str:
        return hydra.lib.strings.cat(("Definition ", bundle_name(), typ_bind_text(), " :=\n  hydra_fix (fun (bundle_ : ", product_type(), ") =>\n    ", let_block(), prod_val(), ").\n", bundle_args_line()))
    @lru_cache(1)
    def indexed() -> frozenlist[tuple[int, tuple[str, tuple[str, str]]]]:
        return hydra.lib.lists.zip(hydra.lib.math.range_(0, hydra.lib.math.sub(n(), 1)), fun_infos())
    @lru_cache(1)
    def proj_defs() -> str:
        return hydra.lib.strings.cat(hydra.lib.lists.map((lambda i_fi: (i := hydra.lib.pairs.first(i_fi), fi := hydra.lib.pairs.second(i_fi), nm := hydra.lib.pairs.first(fi), t := hydra.lib.pairs.first(hydra.lib.pairs.second(fi)), proj_text0 := hydra.lib.maybes.from_maybe((lambda : ""), hydra.lib.maps.lookup(i, hydra.lib.maps.from_list(hydra.lib.lists.zip(hydra.lib.math.range_(0, hydra.lib.math.sub(n(), 1)), proj_exprs())))), proj_text := replace_bundle(proj_text0, bundle_name()), args_def := implicit_args_line(nm, all_type_var_names()), hydra.lib.strings.cat(("Definition ", nm, typ_bind_text(), " : ", t, " :=\n  ", proj_text, ".\n", args_def)))[7]), indexed()))
    return hydra.lib.strings.cat((bundle_def(), "\n", proj_defs()))

def encode_term_group_singleton(env: hydra.coq.environment.CoqEnvironment, td: tuple[str, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.Type]]]]) -> frozenlist[hydra.coq.syntax.Sentence]:
    r"""Encode a non-cyclic term definition as a Coq Definition sentence."""

    @lru_cache(1)
    def name() -> str:
        return hydra.lib.pairs.first(td)
    @lru_cache(1)
    def rest1() -> tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.Type]]]:
        return hydra.lib.pairs.second(td)
    @lru_cache(1)
    def body() -> hydra.core.Term:
        return hydra.lib.pairs.first(rest1())
    @lru_cache(1)
    def rest2() -> tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.Type]]:
        return hydra.lib.pairs.second(rest1())
    @lru_cache(1)
    def type_vars() -> frozenlist[hydra.core.Name]:
        return hydra.lib.pairs.first(rest2())
    @lru_cache(1)
    def m_type() -> Maybe[hydra.core.Type]:
        return hydra.lib.pairs.second(rest2())
    @lru_cache(1)
    def scheme_var_names() -> frozenset[str]:
        return hydra.lib.sets.from_list(hydra.lib.lists.map((lambda n: n.value), type_vars()))
    @lru_cache(1)
    def body2() -> hydra.core.Term:
        return hydra.coq.utils.reorder_let_bindings(hydra.coq.utils.erase_unbound_type_var_domains(scheme_var_names(), body()))
    @lru_cache(1)
    def coq_body() -> hydra.coq.syntax.Term:
        return hydra.coq.coder.encode_term(env, body2())
    @lru_cache(1)
    def binders() -> tuple[frozenlist[str], frozenlist[hydra.coq.syntax.Binder]]:
        return mk_type_binders(body2(), type_vars())
    @lru_cache(1)
    def type_binders() -> frozenlist[hydra.coq.syntax.Binder]:
        return hydra.lib.pairs.second(binders())
    @lru_cache(1)
    def return_type() -> Maybe[hydra.coq.syntax.Type]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda ty: (ep := hydra.coq.utils.extract_type_params(ty), body_ty := hydra.lib.pairs.second(ep), Just(hydra.coq.syntax.Type(hydra.coq.coder.encode_type(env, body_ty))))[2]), m_type())
    return (hydra.coq.syntax.Sentence(Nothing(), cast(hydra.coq.syntax.SentenceContent, hydra.coq.syntax.SentenceContentDefinition(hydra.coq.syntax.Definition(Nothing(), hydra.coq.coder.coq_ident(name()), type_binders(), return_type(), coq_body())))),)

def generate_arguments_decls(type_defs: frozenlist[tuple[str, hydra.core.Type]]) -> str:
    r"""Produce Arguments {p} declarations for every parameterized type's constructor and field accessors."""

    def implicit_all(params: frozenlist[str]) -> str:
        return hydra.lib.strings.intercalate(" ", hydra.lib.lists.map((lambda p: hydra.lib.strings.cat(("{", p, "}"))), params))
    def lines_for(triple: tuple[str, tuple[frozenlist[str], hydra.core.Type]]) -> frozenlist[str]:
        @lru_cache(1)
        def name() -> str:
            return hydra.lib.pairs.first(triple)
        @lru_cache(1)
        def params() -> frozenlist[str]:
            return hydra.lib.pairs.first(hydra.lib.pairs.second(triple))
        @lru_cache(1)
        def body_ty() -> hydra.core.Type:
            return hydra.lib.pairs.second(hydra.lib.pairs.second(triple))
        @lru_cache(1)
        def imp_all() -> str:
            return implicit_all(params())
        match body_ty():
            case hydra.core.TypeUnion(value=fields):
                return hydra.lib.lists.map((lambda ft: hydra.lib.strings.cat(("Arguments ", name(), "_", hydra.formatting.capitalize(ft.name.value), " ", imp_all(), "."))), fields)

            case hydra.core.TypeRecord(value=fields2):
                return hydra.lib.logic.if_else(hydra.lib.lists.null(fields2), (lambda : ()), (lambda : (constr_line := hydra.lib.strings.cat(("Arguments Build_", name(), " ", imp_all(), ".")), field_lines := hydra.lib.lists.map((lambda ft: hydra.lib.strings.cat(("Arguments ", hydra.formatting.decapitalize(name()), "_", hydra.coq.utils.sanitize(hydra.coq.utils.local_name(ft.name.value)), " ", imp_all(), "."))), fields2), hydra.lib.lists.cons(constr_line, field_lines))[2]))

            case _:
                return ()
    @lru_cache(1)
    def triples() -> frozenlist[tuple[str, tuple[frozenlist[str], hydra.core.Type]]]:
        return hydra.lib.maybes.cat(hydra.lib.lists.map((lambda nt: (name := hydra.lib.pairs.first(nt), ty := hydra.lib.pairs.second(nt), ep := hydra.coq.utils.extract_type_params(ty), params := hydra.lib.pairs.first(ep), body_ty := hydra.lib.pairs.second(ep), hydra.lib.logic.if_else(hydra.lib.lists.null(params), (lambda : Nothing()), (lambda : Just((name, (params, body_ty))))))[5]), type_defs))
    @lru_cache(1)
    def all_lines() -> frozenlist[str]:
        return hydra.lib.lists.concat(hydra.lib.lists.map((lambda x1: lines_for(x1)), triples()))
    return hydra.lib.logic.if_else(hydra.lib.lists.null(all_lines()), (lambda : ""), (lambda : hydra.lib.strings.cat(("\n", hydra.lib.strings.intercalate("\n", all_lines()), "\n"))))

def make_return_type(type_name: str, params: frozenlist[str]) -> hydra.coq.syntax.Term:
    r"""Return-type Coq term: `TypeName` or `TypeName p1 p2 ...`."""

    return hydra.lib.logic.if_else(hydra.lib.lists.null(params), (lambda : hydra.coq.coder.coq_term_qualid(type_name)), (lambda : hydra.coq.coder.coq_term_app(hydra.coq.coder.coq_term_qualid(type_name), hydra.lib.lists.map((lambda p: hydra.coq.coder.coq_term_qualid(p)), params))))

def make_constructor(env: hydra.coq.environment.CoqEnvironment, type_name: str, params: frozenlist[str], ft: hydra.core.FieldType) -> hydra.coq.syntax.Constructor:
    r"""Build a Coq Constructor from a union field (prepended with the type name and capitalized field name)."""

    fn = ft.name.value
    @lru_cache(1)
    def constr_name() -> str:
        return hydra.lib.strings.cat((type_name, "_", hydra.formatting.capitalize(fn)))
    field_ty = ft.type
    @lru_cache(1)
    def arg_type() -> hydra.coq.syntax.Term:
        return hydra.coq.coder.encode_type(env, field_ty)
    @lru_cache(1)
    def return_type() -> hydra.coq.syntax.Term:
        return make_return_type(type_name, params)
    return hydra.coq.syntax.Constructor(hydra.coq.coder.coq_ident(constr_name()), (), Just(hydra.coq.syntax.Type(hydra.coq.coder.coq_arrow(arg_type(), return_type()))))

def generate_type_sentence(env: hydra.coq.environment.CoqEnvironment, name: str, ty: hydra.core.Type) -> frozenlist[hydra.coq.syntax.Sentence]:
    r"""Generate the Coq sentence(s) for a non-cyclic type definition."""

    @lru_cache(1)
    def extracted() -> tuple[frozenlist[str], hydra.core.Type]:
        return hydra.coq.utils.extract_type_params(ty)
    @lru_cache(1)
    def params() -> frozenlist[str]:
        return hydra.lib.pairs.first(extracted())
    @lru_cache(1)
    def body_ty() -> hydra.core.Type:
        return hydra.lib.pairs.second(extracted())
    @lru_cache(1)
    def param_binders() -> frozenlist[hydra.coq.syntax.Binder]:
        return hydra.lib.lists.map((lambda p: cast(hydra.coq.syntax.Binder, hydra.coq.syntax.BinderType(hydra.coq.syntax.TypeBinders((hydra.coq.coder.coq_name(p),), hydra.coq.syntax.Type(hydra.coq.coder.coq_term_qualid("Type")))))), params())
    def mk_def(n: str, binders: frozenlist[hydra.coq.syntax.Binder], body: hydra.coq.syntax.Term) -> hydra.coq.syntax.Sentence:
        return hydra.coq.syntax.Sentence(Nothing(), cast(hydra.coq.syntax.SentenceContent, hydra.coq.syntax.SentenceContentDefinition(hydra.coq.syntax.Definition(Nothing(), hydra.coq.coder.coq_ident(n), binders, Just(hydra.coq.syntax.Type(hydra.coq.coder.coq_term_qualid("Type"))), body))))
    match body_ty():
        case hydra.core.TypeUnion(value=fields):
            @lru_cache(1)
            def body() -> hydra.coq.syntax.InductiveBody:
                return hydra.coq.syntax.InductiveBody(hydra.coq.coder.coq_ident(name), param_binders(), Just(hydra.coq.syntax.Type(hydra.coq.coder.coq_term_qualid("Type"))), hydra.lib.lists.map((lambda ft: make_constructor(env, name, params(), ft)), fields))
            @lru_cache(1)
            def ind_def() -> hydra.coq.syntax.InductiveDefinition:
                return hydra.coq.syntax.InductiveDefinition(Nothing(), False, (body(),))
            return (hydra.coq.syntax.Sentence(Nothing(), cast(hydra.coq.syntax.SentenceContent, hydra.coq.syntax.SentenceContentInductive(ind_def()))),)

        case hydra.core.TypeRecord(value=fields2):
            return hydra.lib.logic.if_else(hydra.lib.lists.null(fields2), (lambda : (mk_def(name, param_binders(), hydra.coq.coder.coq_term_qualid("unit")),)), (lambda : (hydra.coq.syntax.Sentence(Nothing(), cast(hydra.coq.syntax.SentenceContent, hydra.coq.syntax.SentenceContentRecord(hydra.coq.syntax.RecordDefinition(Nothing(), hydra.coq.coder.coq_ident(name), param_binders(), Just(cast(hydra.coq.syntax.Sort, hydra.coq.syntax.SortType())), hydra.coq.syntax.RecordBody(Just(hydra.coq.coder.coq_ident(hydra.lib.strings.cat(("Build_", name)))), hydra.lib.lists.map((lambda ft: (fn := hydra.coq.utils.sanitize(hydra.coq.utils.local_name(ft.name.value)), prefixed_fn := hydra.lib.strings.cat((hydra.formatting.decapitalize(name), "_", fn)), ft_coq := hydra.coq.coder.encode_type(env, ft.type), hydra.coq.syntax.RecordField(hydra.coq.coder.coq_ident(prefixed_fn), hydra.coq.syntax.Type(ft_coq)))[3]), fields2)))))),)))

        case _:
            return (mk_def(name, param_binders(), hydra.coq.coder.encode_type(env, body_ty())),)

def make_one_accessor(type_name: str, constr_pat: hydra.coq.syntax.Pattern10_Qualid, field_vars: frozenlist[str], idx: int, ft: hydra.core.FieldType) -> hydra.coq.syntax.Sentence:
    r"""Emit a Definition for a record field accessor, keyed by the Build_T pattern."""

    @lru_cache(1)
    def fn() -> str:
        return hydra.coq.utils.sanitize(hydra.coq.utils.local_name(ft.name.value))
    @lru_cache(1)
    def prefixed_fn() -> str:
        return hydra.lib.strings.cat((hydra.formatting.decapitalize(type_name), "_", fn()))
    @lru_cache(1)
    def return_expr() -> hydra.coq.syntax.Term:
        return hydra.coq.coder.coq_term_qualid(hydra.lib.maybes.from_maybe((lambda : ""), hydra.lib.maps.lookup(idx, hydra.lib.maps.from_list(hydra.lib.lists.zip(hydra.lib.math.range_(0, hydra.lib.math.sub(hydra.lib.lists.length(field_vars), 1)), field_vars)))))
    @lru_cache(1)
    def match_expr() -> hydra.coq.syntax.Term:
        return cast(hydra.coq.syntax.Term, hydra.coq.syntax.TermTerm100(cast(hydra.coq.syntax.Term100, hydra.coq.syntax.Term100Term10(cast(hydra.coq.syntax.Term10, hydra.coq.syntax.Term10OneTerm(cast(hydra.coq.syntax.OneTerm, hydra.coq.syntax.OneTermTerm1(cast(hydra.coq.syntax.Term1, hydra.coq.syntax.Term1Term0(cast(hydra.coq.syntax.Term0, hydra.coq.syntax.Term0Match(hydra.coq.syntax.Match((hydra.coq.syntax.CaseItem(cast(hydra.coq.syntax.Term100, hydra.coq.syntax.Term100Term10(cast(hydra.coq.syntax.Term10, hydra.coq.syntax.Term10OneTerm(cast(hydra.coq.syntax.OneTerm, hydra.coq.syntax.OneTermExplicit(hydra.coq.syntax.QualidAnnotated(hydra.coq.coder.coq_qualid("r_"), Nothing()))))))), Nothing(), Nothing()),), Nothing(), False, (hydra.coq.syntax.Equation(((cast(hydra.coq.syntax.Pattern, hydra.coq.syntax.PatternPattern(cast(hydra.coq.syntax.Pattern10, hydra.coq.syntax.Pattern10Qualiid(constr_pat)))),),), return_expr()),))))))))))))))
    return hydra.coq.syntax.Sentence(Nothing(), cast(hydra.coq.syntax.SentenceContent, hydra.coq.syntax.SentenceContentDefinition(hydra.coq.syntax.Definition(Nothing(), hydra.coq.coder.coq_ident(prefixed_fn()), (cast(hydra.coq.syntax.Binder, hydra.coq.syntax.BinderType(hydra.coq.syntax.TypeBinders((hydra.coq.coder.coq_name("r_"),), hydra.coq.syntax.Type(hydra.coq.coder.coq_term_qualid(type_name))))),), Nothing(), match_expr()))))

def make_accessor_defs(nt: tuple[str, hydra.core.Type]) -> frozenlist[hydra.coq.syntax.Sentence]:
    r"""Build one Definition per record field, pattern-matching on Build_T."""

    @lru_cache(1)
    def name() -> str:
        return hydra.lib.pairs.first(nt)
    @lru_cache(1)
    def ty() -> hydra.core.Type:
        return hydra.lib.pairs.second(nt)
    @lru_cache(1)
    def extracted() -> tuple[frozenlist[str], hydra.core.Type]:
        return hydra.coq.utils.extract_type_params(ty())
    @lru_cache(1)
    def body_ty() -> hydra.core.Type:
        return hydra.lib.pairs.second(extracted())
    match body_ty():
        case hydra.core.TypeRecord(value=fields):
            return hydra.lib.logic.if_else(hydra.lib.lists.null(fields), (lambda : ()), (lambda : (n_fields := hydra.lib.lists.length(fields), field_vars := hydra.lib.lists.map((lambda i: hydra.lib.strings.cat(("f", hydra.lib.literals.show_int32(i)))), hydra.lib.math.range_(0, hydra.lib.math.sub(n_fields, 1))), constr_pat := hydra.coq.syntax.Pattern10_Qualid(hydra.coq.coder.coq_qualid(hydra.lib.strings.cat(("Build_", name()))), hydra.lib.lists.map((lambda v: hydra.coq.syntax.Pattern1(cast(hydra.coq.syntax.Pattern0, hydra.coq.syntax.Pattern0Qualid(hydra.coq.coder.coq_qualid(v))), Nothing())), field_vars)), indexed := hydra.lib.lists.zip(hydra.lib.math.range_(0, hydra.lib.math.sub(n_fields, 1)), fields), hydra.lib.lists.map((lambda ift: make_one_accessor(name(), constr_pat, field_vars, hydra.lib.pairs.first(ift), hydra.lib.pairs.second(ift))), indexed))[4]))

        case _:
            return ()

def make_inductive_body(env: hydra.coq.environment.CoqEnvironment, name: str, ty: hydra.core.Type) -> frozenlist[hydra.coq.syntax.InductiveBody]:
    r"""Build an Inductive body for a union or record type in a mutual group."""

    @lru_cache(1)
    def extracted() -> tuple[frozenlist[str], hydra.core.Type]:
        return hydra.coq.utils.extract_type_params(ty)
    @lru_cache(1)
    def params() -> frozenlist[str]:
        return hydra.lib.pairs.first(extracted())
    @lru_cache(1)
    def body_ty() -> hydra.core.Type:
        return hydra.lib.pairs.second(extracted())
    @lru_cache(1)
    def param_binders() -> frozenlist[hydra.coq.syntax.Binder]:
        return hydra.lib.lists.map((lambda p: cast(hydra.coq.syntax.Binder, hydra.coq.syntax.BinderType(hydra.coq.syntax.TypeBinders((hydra.coq.coder.coq_name(p),), hydra.coq.syntax.Type(hydra.coq.coder.coq_term_qualid("Type")))))), params())
    match body_ty():
        case hydra.core.TypeUnion(value=fields):
            return (hydra.coq.syntax.InductiveBody(hydra.coq.coder.coq_ident(name), param_binders(), Just(hydra.coq.syntax.Type(hydra.coq.coder.coq_term_qualid("Type"))), hydra.lib.lists.map((lambda ft: make_constructor(env, name, params(), ft)), fields)),)

        case hydra.core.TypeRecord(value=fields2):
            return hydra.lib.logic.if_else(hydra.lib.lists.null(fields2), (lambda : (hydra.coq.syntax.InductiveBody(hydra.coq.coder.coq_ident(name), param_binders(), Just(hydra.coq.syntax.Type(hydra.coq.coder.coq_term_qualid("Type"))), (hydra.coq.syntax.Constructor(hydra.coq.coder.coq_ident(hydra.lib.strings.cat(("Build_", name))), (), Just(hydra.coq.syntax.Type(make_return_type(name, params())))),)),)), (lambda : (constr_type := hydra.lib.lists.foldr((lambda ft, acc: hydra.coq.coder.coq_arrow(hydra.coq.coder.encode_type(env, ft.type), acc)), make_return_type(name, params()), fields2), (hydra.coq.syntax.InductiveBody(hydra.coq.coder.coq_ident(name), param_binders(), Just(hydra.coq.syntax.Type(hydra.coq.coder.coq_term_qualid("Type"))), (hydra.coq.syntax.Constructor(hydra.coq.coder.coq_ident(hydra.lib.strings.cat(("Build_", name))), (), Just(hydra.coq.syntax.Type(constr_type))),)),))[1]))

        case _:
            return ()

def generate_type_group(env: hydra.coq.environment.CoqEnvironment, group: tuple[bool, frozenlist[tuple[str, hydra.core.Type]]]) -> frozenlist[hydra.coq.syntax.Sentence]:
    r"""Emit Coq sentences for a type-definition SCC group, handling mutual recursion and positivity."""

    @lru_cache(1)
    def cyclic() -> bool:
        return hydra.lib.pairs.first(group)
    @lru_cache(1)
    def defs() -> frozenlist[tuple[str, hydra.core.Type]]:
        return hydra.lib.pairs.second(group)
    return hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.logic.not_(cyclic()), hydra.lib.equality.equal(hydra.lib.lists.length(defs()), 1)), (lambda : hydra.lib.maybes.from_maybe((lambda : ()), hydra.lib.maybes.map((lambda d: generate_type_sentence(env, hydra.lib.pairs.first(d), hydra.lib.pairs.second(d))), hydra.lib.lists.maybe_head(defs())))), (lambda : (group_names := hydra.lib.sets.from_list(hydra.lib.lists.map((lambda d: hydra.lib.pairs.first(d)), defs())), has_positivity := hydra.coq.utils.has_positivity_issue(group_names, defs()), sanitized_group := hydra.lib.logic.if_else(has_positivity, (lambda : hydra.lib.lists.map((lambda d: (hydra.lib.pairs.first(d), hydra.coq.utils.sanitize_positivity(group_names, hydra.lib.pairs.second(d)))), defs())), (lambda : defs())), bodies := hydra.lib.lists.concat(hydra.lib.lists.map((lambda d: make_inductive_body(env, hydra.lib.pairs.first(d), hydra.lib.pairs.second(d))), sanitized_group)), accessors := hydra.lib.lists.concat(hydra.lib.lists.map((lambda d: make_accessor_defs(d)), sanitized_group)), inductive_sent := hydra.lib.logic.if_else(hydra.lib.lists.null(bodies), (lambda : ()), (lambda : (hydra.coq.syntax.Sentence(Nothing(), cast(hydra.coq.syntax.SentenceContent, hydra.coq.syntax.SentenceContentInductive(hydra.coq.syntax.InductiveDefinition(Nothing(), False, bodies)))),))), hydra.lib.lists.concat2(inductive_sent, accessors))[6]))

def render_sentences(sentences: frozenlist[hydra.coq.syntax.Sentence]) -> str:
    r"""Pretty-print a Document containing the given Coq sentences."""

    return hydra.serialization.print_expr(hydra.serialization.parenthesize(hydra.coq.serde.document_to_expr(hydra.coq.syntax.Document(sentences))))

def render_require_imports(dep_sentences: frozenlist[hydra.coq.syntax.Sentence]) -> str:
    r"""Pretty-print the standard-imports sentence followed by additional dependency imports."""

    return render_sentences(hydra.lib.lists.cons(hydra.coq.coder.standard_imports(), dep_sentences))

def build_full_module(env: hydra.coq.environment.CoqEnvironment, field_map: FrozenDict[tuple[str, str], str], mod_: T0, ns_str: str, path: T1, desc: str, type_defs: frozenlist[tuple[str, hydra.core.Type]], term_defs: frozenlist[tuple[str, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.Type]]]]]) -> FrozenDict[T1, str]:
    r"""Assemble the full (non-axiom) Coq source for a module."""

    @lru_cache(1)
    def term_defs_for_sort() -> frozenlist[tuple[str, hydra.core.Term]]:
        return hydra.lib.lists.map((lambda td: (hydra.lib.pairs.first(td), hydra.lib.pairs.first(hydra.lib.pairs.second(td)))), term_defs)
    @lru_cache(1)
    def term_def_map() -> FrozenDict[str, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.Type]]]]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda td: (hydra.lib.pairs.first(td), hydra.lib.pairs.second(td))), term_defs))
    @lru_cache(1)
    def term_groups() -> frozenlist[tuple[bool, frozenlist[tuple[str, hydra.core.Term]]]]:
        return hydra.coq.utils.sort_term_defs_s_c_c(term_defs_for_sort())
    @lru_cache(1)
    def term_groups2() -> frozenlist[tuple[bool, frozenlist[tuple[str, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.Type]]]]]]]:
        return hydra.lib.lists.map((lambda cg: (cyc := hydra.lib.pairs.first(cg), grp := hydra.lib.pairs.second(cg), enriched := hydra.lib.maybes.cat(hydra.lib.lists.map((lambda nt: (nm := hydra.lib.pairs.first(nt), t := hydra.lib.pairs.second(nt), hydra.lib.maybes.map((lambda rec: (body2 := hydra.coq.utils.normalize_inner_type_lambdas(hydra.coq.utils.rewrite_term_fields(field_map, t)), rest := hydra.lib.pairs.second(rec), vs := hydra.lib.pairs.first(rest), mty := hydra.lib.pairs.second(rest), (nm, (body2, (vs, mty))))[4]), hydra.lib.maps.lookup(nm, term_def_map())))[2]), grp)), (cyc, enriched))[3]), term_groups())
    @lru_cache(1)
    def type_groups() -> frozenlist[tuple[bool, frozenlist[tuple[str, hydra.core.Type]]]]:
        return hydra.coq.utils.sort_type_defs_s_c_c(type_defs)
    @lru_cache(1)
    def type_sentences() -> frozenlist[hydra.coq.syntax.Sentence]:
        return hydra.lib.lists.concat(hydra.lib.lists.map((lambda g: generate_type_group(env, g)), type_groups()))
    @lru_cache(1)
    def term_rendered_parts() -> frozenlist[str]:
        return hydra.lib.lists.concat(hydra.lib.lists.map((lambda cg: (cyc := hydra.lib.pairs.first(cg), grp := hydra.lib.pairs.second(cg), hydra.lib.logic.if_else(cyc, (lambda : (encode_mutual_group_text(env, grp),)), (lambda : hydra.lib.lists.map((lambda td: (sentences := encode_term_group_singleton(env, td), nm := hydra.lib.pairs.first(td), body := hydra.lib.pairs.first(hydra.lib.pairs.second(td)), tv := hydra.lib.pairs.first(hydra.lib.pairs.second(hydra.lib.pairs.second(td))), scheme_var_names := hydra.lib.sets.from_list(hydra.lib.lists.map((lambda n: n.value), tv)), body2 := hydra.coq.utils.reorder_let_bindings(hydra.coq.utils.erase_unbound_type_var_domains(scheme_var_names, body)), binders := mk_type_binders(body2, tv), all_type_var_names := hydra.lib.pairs.first(binders), rendered := render_sentences(sentences), args_line := hydra.lib.logic.if_else(hydra.lib.lists.null(all_type_var_names), (lambda : ""), (lambda : hydra.lib.strings.cat(("\nArguments ", nm, " ", hydra.lib.strings.intercalate(" ", hydra.lib.lists.map((lambda v: hydra.lib.strings.cat(("{", v, "}"))), all_type_var_names)), ".")))), hydra.lib.strings.cat((rendered, args_line, "\n")))[10]), grp))))[2]), term_groups2()))
    @lru_cache(1)
    def all_qualified_names_from_types() -> frozenset[str]:
        return hydra.lib.sets.unions(hydra.lib.lists.map((lambda nt: hydra.coq.utils.collect_qualified_names_in_type(hydra.lib.pairs.second(nt))), type_defs))
    @lru_cache(1)
    def all_qualified_names_from_terms() -> frozenset[str]:
        return hydra.lib.sets.unions(hydra.lib.lists.map((lambda td: hydra.coq.utils.collect_qualified_names_in_term(hydra.lib.pairs.first(hydra.lib.pairs.second(td)))), term_defs))
    @lru_cache(1)
    def all_qualified_names_from_term_types() -> frozenset[str]:
        return hydra.lib.sets.unions(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda td: (mty := hydra.lib.pairs.second(hydra.lib.pairs.second(hydra.lib.pairs.second(td))), hydra.lib.maybes.map((lambda ty: (ep := hydra.coq.utils.extract_type_params(ty), body_ty := hydra.lib.pairs.second(ep), hydra.coq.utils.collect_qualified_names_in_type(body_ty))[2]), mty))[1]), term_defs)))
    @lru_cache(1)
    def all_qualified_names() -> frozenset[str]:
        return hydra.lib.sets.union(all_qualified_names_from_types(), hydra.lib.sets.union(all_qualified_names_from_terms(), all_qualified_names_from_term_types()))
    @lru_cache(1)
    def ns_set() -> frozenset[str]:
        return hydra.lib.sets.from_list(hydra.lib.lists.map((lambda q: hydra.coq.utils.extract_qualified_namespace(q)), hydra.lib.sets.to_list(all_qualified_names())))
    def str_starts_with(pref: str, s: str) -> bool:
        return hydra.lib.logic.and_(hydra.lib.equality.gte(hydra.lib.strings.length(s), hydra.lib.strings.length(pref)), hydra.lib.equality.equal(hydra.lib.strings.from_list(hydra.lib.lists.take(hydra.lib.strings.length(pref), hydra.lib.strings.to_list(s))), pref))
    def has_strict_suffix(ns_c: str, other_list: frozenlist[str]) -> bool:
        return hydra.lib.maybes.is_just(hydra.lib.lists.find((lambda other: hydra.lib.logic.and_(hydra.lib.logic.not_(hydra.lib.equality.equal(other, ns_c)), str_starts_with(hydra.lib.strings.cat((ns_c, ".")), other))), other_list))
    @lru_cache(1)
    def referenced_ns() -> frozenlist[str]:
        return hydra.lib.lists.nub(hydra.lib.lists.filter((lambda ns_c: hydra.lib.logic.and_(hydra.lib.logic.not_(hydra.lib.equality.equal(ns_c, ns_str)), hydra.lib.logic.not_(has_strict_suffix(ns_c, hydra.lib.sets.to_list(ns_set()))))), hydra.lib.sets.to_list(ns_set())))
    @lru_cache(1)
    def dep_sentences() -> frozenlist[hydra.coq.syntax.Sentence]:
        return dependency_imports(referenced_ns())
    @lru_cache(1)
    def import_text() -> str:
        return render_require_imports(dep_sentences())
    @lru_cache(1)
    def type_sentences_text() -> str:
        return render_sentences(type_sentences())
    @lru_cache(1)
    def all_term_text() -> str:
        return hydra.lib.strings.cat(term_rendered_parts())
    @lru_cache(1)
    def type_args_decls() -> str:
        return generate_arguments_decls(type_defs)
    @lru_cache(1)
    def content() -> str:
        return hydra.lib.strings.cat((desc, import_text(), "\n", type_sentences_text(), "\n", all_term_text(), type_args_decls(), "\n"))
    return hydra.lib.maps.from_list(((path, content()),))

def global_ambiguous_names(modules: frozenlist[hydra.packaging.Module]) -> frozenset[str]:
    r"""Collect local names that occur in more than one module's type or term definitions."""

    @lru_cache(1)
    def all_names() -> frozenlist[tuple[str, str]]:
        return hydra.lib.lists.concat(hydra.lib.lists.map((lambda m: (ns_str := m.namespace.value, from_def := (lambda def_: (_hoist_from_def_1 := (lambda v1: (lambda td: Just((hydra.coq.utils.local_name(td.name.value), ns_str)))(v1.value) if isinstance(v1, hydra.packaging.DefinitionType) else (lambda td: Just((hydra.coq.utils.local_name(td.name.value), ns_str)))(v1.value) if isinstance(v1, hydra.packaging.DefinitionTerm) else Nothing()), _hoist_from_def_1(def_))[1]), hydra.lib.maybes.cat(hydra.lib.lists.map((lambda x1: from_def(x1)), m.definitions)))[2]), modules))
    @lru_cache(1)
    def name_to_ns() -> FrozenDict[str, frozenset[str]]:
        return hydra.lib.lists.foldl((lambda acc, np: (n := hydra.lib.pairs.first(np), ns_val := hydra.lib.pairs.second(np), existing := hydra.lib.maybes.from_maybe((lambda : hydra.lib.sets.empty()), hydra.lib.maps.lookup(n, acc)), hydra.lib.maps.insert(n, hydra.lib.sets.insert(ns_val, existing), acc))[3]), hydra.lib.maps.empty(), all_names())
    return hydra.lib.sets.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda entry: hydra.lib.logic.if_else(hydra.lib.equality.gte(hydra.lib.lists.length(hydra.lib.sets.to_list(hydra.lib.pairs.second(entry))), 2), (lambda : Just(hydra.lib.pairs.first(entry))), (lambda : Nothing()))), hydra.lib.maps.to_list(name_to_ns()))))

def global_constructor_counts(modules: frozenlist[hydra.packaging.Module]) -> FrozenDict[str, int]:
    r"""Collect all type definitions from every module and run buildConstructorCounts over them."""

    @lru_cache(1)
    def all_type_defs():
        def _hoist_all_type_defs_1(v1):
            match v1:
                case hydra.packaging.DefinitionType(value=td):
                    return Just((hydra.coq.utils.local_name(td.name.value), td.type.type))

                case _:
                    return Nothing()
        return hydra.lib.lists.concat(hydra.lib.lists.map((lambda m: hydra.lib.maybes.cat(hydra.lib.lists.map((lambda def_: _hoist_all_type_defs_1(def_)), m.definitions))), modules))
    return hydra.coq.utils.build_constructor_counts(all_type_defs())

def global_field_mapping(modules: frozenlist[hydra.packaging.Module]) -> FrozenDict[tuple[str, str], str]:
    r"""Delegate to CoqUtils.buildFieldMapping across all supplied modules."""

    return hydra.coq.utils.build_field_mapping(modules)

def global_sanitized_accessors(modules: frozenlist[hydra.packaging.Module]) -> frozenset[str]:
    r"""Collect sanitized accessor names by SCC-sorting every module's type defs and folding collectSanitizedAccessors."""

    @lru_cache(1)
    def all_type_groups() -> frozenlist[tuple[bool, frozenlist[tuple[str, hydra.core.Type]]]]:
        return hydra.lib.lists.concat(hydra.lib.lists.map((lambda m: (type_defs := (_hoist_type_defs_1 := (lambda v1: (lambda td: Just((hydra.coq.utils.local_name(td.name.value), td.type.type)))(v1.value) if isinstance(v1, hydra.packaging.DefinitionType) else Nothing()), hydra.lib.maybes.cat(hydra.lib.lists.map((lambda def_: _hoist_type_defs_1(def_)), m.definitions)))[1], hydra.coq.utils.sort_type_defs_s_c_c(type_defs))[1]), modules))
    return hydra.coq.utils.collect_sanitized_accessors(all_type_groups())

def namespace_to_path(ns: str) -> str:
    r"""Convert a Hydra namespace string (e.g. hydra.show.core) into a relative .v file path."""

    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.strings.split_on(".", ns)
    @lru_cache(1)
    def dir_parts() -> frozenlist[str]:
        return hydra.lib.maybes.from_maybe((lambda : ()), hydra.lib.lists.maybe_init(parts()))
    @lru_cache(1)
    def file_name() -> str:
        return hydra.lib.strings.cat((hydra.lib.maybes.from_maybe((lambda : ns), hydra.lib.lists.maybe_last(parts())), ".v"))
    return hydra.lib.logic.if_else(hydra.lib.lists.null(dir_parts()), (lambda : file_name()), (lambda : hydra.lib.strings.cat((hydra.lib.strings.intercalate("/", dir_parts()), "/", file_name()))))

def module_to_coq(field_map: FrozenDict[tuple[str, str], str], constr_counts: FrozenDict[str, int], ambiguous_names: frozenset[str], global_sanitized_acc: frozenset[str], mod_: hydra.packaging.Module, defs: frozenlist[hydra.packaging.Definition]) -> FrozenDict[str, str]:
    r"""Top-level driver: dispatch a module to either full-emission or axiom-only emission, producing (path, content) pairs."""

    ns_str = mod_.namespace.value
    @lru_cache(1)
    def path() -> str:
        return namespace_to_path(ns_str)
    @lru_cache(1)
    def desc() -> str:
        return hydra.lib.maybes.maybe((lambda : ""), (lambda d: hydra.lib.strings.cat(("(* ", d, " *)\n\n"))), mod_.description)
    axiom_only_modules = ("hydra.hoisting", "hydra.inference")
    @lru_cache(1)
    def is_axiom_only() -> bool:
        return hydra.lib.lists.elem(ns_str, axiom_only_modules)
    @lru_cache(1)
    def type_defs():
        def _hoist_type_defs_1(v1):
            match v1:
                case hydra.packaging.DefinitionType(value=td):
                    return Just((hydra.coq.utils.local_name(td.name.value), td.type.type))

                case _:
                    return Nothing()
        return hydra.lib.maybes.cat(hydra.lib.lists.map((lambda def_: _hoist_type_defs_1(def_)), defs))
    @lru_cache(1)
    def term_defs():
        def _hoist_term_defs_1(v1):
            match v1:
                case hydra.packaging.DefinitionTerm(value=td):
                    mts = td.type
                    @lru_cache(1)
                    def vs() -> frozenlist[hydra.core.Name]:
                        return hydra.lib.maybes.maybe((lambda : ()), (lambda ts: ts.variables), mts)
                    @lru_cache(1)
                    def mty() -> Maybe[hydra.core.Type]:
                        return hydra.lib.maybes.map((lambda ts: ts.type), mts)
                    return Just((hydra.coq.utils.local_name(td.name.value), (td.term, (vs(), mty()))))

                case _:
                    return Nothing()
        return hydra.lib.maybes.cat(hydra.lib.lists.map((lambda def_: _hoist_term_defs_1(def_)), defs))
    @lru_cache(1)
    def local_def_names() -> frozenset[str]:
        return hydra.lib.sets.from_list(hydra.lib.lists.concat2(hydra.lib.lists.map((lambda nt: hydra.lib.pairs.first(nt)), type_defs()), hydra.lib.lists.map((lambda td: hydra.lib.pairs.first(td)), term_defs())))
    @lru_cache(1)
    def module_ambig() -> frozenset[str]:
        return hydra.lib.sets.union(ambiguous_names, local_def_names())
    env = hydra.coq.environment.CoqEnvironment(ns_str, constr_counts, module_ambig(), global_sanitized_acc)
    return hydra.lib.logic.if_else(is_axiom_only(), (lambda : hydra.lib.maps.from_list(((path(), build_axiom_only_content(env, desc(), ns_str, type_defs(), term_defs(), mod_)),))), (lambda : build_full_module(env, field_map, mod_, ns_str, path(), desc(), type_defs(), term_defs())))
