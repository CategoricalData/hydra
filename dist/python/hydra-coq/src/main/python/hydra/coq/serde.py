# Note: this is an automatically generated file. Do not edit.

r"""Coq serializer: converts Coq AST to concrete Coq source code."""

from __future__ import annotations
from functools import lru_cache
from hydra.dsl.python import Maybe, frozenlist
from typing import cast
import hydra.coq.syntax
import hydra.core
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maybes
import hydra.lib.strings
import hydra.serialization

def ident_to_expr(ident: hydra.coq.syntax.Ident) -> hydra.ast.Expr:
    return hydra.serialization.cst(ident.value.value)

def qualid_to_expr(q: hydra.coq.syntax.Qualid) -> hydra.ast.Expr:
    @lru_cache(1)
    def id_expr() -> hydra.ast.Expr:
        return ident_to_expr(q.id)
    field_ids = q.field_ids
    @lru_cache(1)
    def field_exprs() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda f: ident_to_expr(f.value)), field_ids)
    return hydra.lib.logic.if_else(hydra.lib.lists.null(field_exprs()), (lambda : id_expr()), (lambda : hydra.serialization.dot_sep(hydra.lib.lists.concat2((id_expr(),), field_exprs()))))

def pattern0_to_expr(p: hydra.coq.syntax.Pattern0) -> hydra.ast.Expr:
    match p:
        case hydra.coq.syntax.Pattern0Qualid(value=q):
            return qualid_to_expr(q)

        case hydra.coq.syntax.Pattern0QualIdAndPattern():
            return hydra.serialization.cst("...")

        case hydra.coq.syntax.Pattern0Placeholder():
            return hydra.serialization.cst("_")

        case hydra.coq.syntax.Pattern0Parens(value=ps):
            return hydra.serialization.parens(hydra.serialization.infix_ws_list(", ", hydra.lib.lists.map((lambda p2: pattern_to_expr(p2)), ps)))

        case hydra.coq.syntax.Pattern0Number(value=n):
            v = n.value
            return hydra.serialization.cst(hydra.lib.literals.show_bigfloat(v))

        case hydra.coq.syntax.Pattern0String(value=s):
            return hydra.serialization.space_sep((hydra.serialization.cst("\""), hydra.serialization.cst(s.value), hydra.serialization.cst("\"")))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def pattern10_to_expr(p: hydra.coq.syntax.Pattern10) -> hydra.ast.Expr:
    match p:
        case hydra.coq.syntax.Pattern10As(value=pa):
            return hydra.serialization.space_sep((pattern1_to_expr(pa.pattern), hydra.serialization.cst("as"), hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("_")), (lambda i: ident_to_expr(i)), pa.as_.value)))

        case hydra.coq.syntax.Pattern10Patterns(value=pps):
            @lru_cache(1)
            def first() -> hydra.ast.Expr:
                return pattern1_to_expr(pps.pattern)
            @lru_cache(1)
            def rest() -> frozenlist[hydra.ast.Expr]:
                return hydra.lib.lists.map((lambda p2: pattern1_to_expr(p2)), pps.patterns)
            return hydra.serialization.space_sep(hydra.lib.lists.cons(first(), rest()))

        case hydra.coq.syntax.Pattern10Qualiid(value=pq):
            @lru_cache(1)
            def q() -> hydra.ast.Expr:
                return qualid_to_expr(pq.qualid)
            @lru_cache(1)
            def args() -> frozenlist[hydra.ast.Expr]:
                return hydra.lib.lists.map((lambda p2: pattern1_to_expr(p2)), pq.patterns)
            return hydra.lib.logic.if_else(hydra.lib.lists.null(args()), (lambda : q()), (lambda : hydra.serialization.space_sep(hydra.lib.lists.cons(q(), args()))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def pattern1_to_expr(p: hydra.coq.syntax.Pattern1) -> hydra.ast.Expr:
    return pattern0_to_expr(p.pattern)

def pattern_to_expr(p: hydra.coq.syntax.Pattern) -> hydra.ast.Expr:
    match p:
        case hydra.coq.syntax.PatternPattern(value=p10):
            return pattern10_to_expr(p10)

        case hydra.coq.syntax.PatternTerm():
            return hydra.serialization.cst("_")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def sort_to_expr(s: hydra.coq.syntax.Sort) -> hydra.ast.Expr:
    match s:
        case hydra.coq.syntax.SortSet():
            return hydra.serialization.cst("Set")

        case hydra.coq.syntax.SortProp():
            return hydra.serialization.cst("Prop")

        case hydra.coq.syntax.SortSProp():
            return hydra.serialization.cst("SProp")

        case hydra.coq.syntax.SortType():
            return hydra.serialization.cst("Type")

        case hydra.coq.syntax.SortTypeWithAnyUniverse():
            return hydra.serialization.cst("Type")

        case hydra.coq.syntax.SortTypeWithUniverse():
            return hydra.serialization.no_sep((hydra.serialization.cst("Type"), hydra.serialization.cst("@{"), hydra.serialization.cst("}")))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def application_to_expr(app: hydra.coq.syntax.Application):
    def _hoist_hydra_coq_serde_application_to_expr_1(v1):
        match v1:
            case hydra.coq.syntax.ArgIdent(value=ia):
                return hydra.serialization.space_sep((hydra.serialization.parens(hydra.serialization.space_sep((ident_to_expr(ia.ident), hydra.serialization.cst(":="), term_to_expr(ia.term)))),))

            case hydra.coq.syntax.ArgNatural(value=na2):
                v = na2.natural.value
                return hydra.serialization.cst(hydra.lib.literals.show_bigint(v))

            case hydra.coq.syntax.ArgTerm(value=t1):
                return term1_to_expr(t1)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    match app:
        case hydra.coq.syntax.ApplicationNormal(value=na):
            return hydra.serialization.space_sep((term1_to_expr(na.lhs), hydra.serialization.space_sep(hydra.lib.lists.map((lambda a: _hoist_hydra_coq_serde_application_to_expr_1(a)), na.rhs))))

        case hydra.coq.syntax.ApplicationAnnotated(value=aa):
            return hydra.serialization.space_sep((hydra.serialization.cst("@"), qualid_to_expr(aa.annot.qualid)))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def binder_to_expr(b: hydra.coq.syntax.Binder):
    def _hoist_hydra_coq_serde_binder_to_expr_1(v1):
        match v1:
            case hydra.coq.syntax.ImplicitBindersMaximallyInserted(value=tb):
                @lru_cache(1)
                def names() -> frozenlist[hydra.ast.Expr]:
                    return hydra.lib.lists.map((lambda n: hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("_")), (lambda i: ident_to_expr(i)), n.value)), tb.names)
                @lru_cache(1)
                def ty() -> hydra.ast.Expr:
                    return type_to_expr(tb.type)
                return hydra.serialization.brackets(hydra.serialization.curly_braces, hydra.serialization.inline_style, hydra.serialization.space_sep((hydra.serialization.space_sep(names()), hydra.serialization.cst(":"), ty())))

            case hydra.coq.syntax.ImplicitBindersNonMaximallyInserted(value=tb):
                @lru_cache(1)
                def names() -> frozenlist[hydra.ast.Expr]:
                    return hydra.lib.lists.map((lambda n: hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("_")), (lambda i: ident_to_expr(i)), n.value)), tb.names)
                @lru_cache(1)
                def ty() -> hydra.ast.Expr:
                    return type_to_expr(tb.type)
                return hydra.serialization.brackets(hydra.serialization.square_brackets, hydra.serialization.inline_style, hydra.serialization.space_sep((hydra.serialization.space_sep(names()), hydra.serialization.cst(":"), ty())))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_hydra_coq_serde_binder_to_expr_2(v1):
        match v1:
            case hydra.coq.syntax.GeneralizingBinderExplicit(value=tc):
                return hydra.serialization.parens(term_to_expr(tc.term))

            case hydra.coq.syntax.GeneralizingBinderImplicitMaximallyInserted(value=tc):
                return hydra.serialization.brackets(hydra.serialization.curly_braces, hydra.serialization.inline_style, term_to_expr(tc.term))

            case hydra.coq.syntax.GeneralizingBinderImplicitNonMaximallyInserted(value=tc):
                return hydra.serialization.brackets(hydra.serialization.square_brackets, hydra.serialization.inline_style, term_to_expr(tc.term))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    match b:
        case hydra.coq.syntax.BinderName(value=n):
            return hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("_")), (lambda i: ident_to_expr(i)), n.value)

        case hydra.coq.syntax.BinderType(value=tb):
            @lru_cache(1)
            def names() -> frozenlist[hydra.ast.Expr]:
                return hydra.lib.lists.map((lambda n: hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("_")), (lambda i: ident_to_expr(i)), n.value)), tb.names)
            @lru_cache(1)
            def ty() -> hydra.ast.Expr:
                return type_to_expr(tb.type)
            return hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.space_sep(names()), hydra.serialization.cst(":"), ty())))

        case hydra.coq.syntax.BinderTerm(value=lb):
            @lru_cache(1)
            def name() -> hydra.ast.Expr:
                return hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("_")), (lambda i: ident_to_expr(i)), lb.name.value)
            @lru_cache(1)
            def ty() -> frozenlist[hydra.ast.Expr]:
                return hydra.lib.maybes.maybe((lambda : ()), (lambda t: (hydra.serialization.cst(":"), type_to_expr(t))), lb.type)
            @lru_cache(1)
            def body() -> hydra.ast.Expr:
                return term_to_expr(lb.term)
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((name(),), ty(), (hydra.serialization.cst(":="), body())))))

        case hydra.coq.syntax.BinderImplicit(value=ib):
            return _hoist_hydra_coq_serde_binder_to_expr_1(ib)

        case hydra.coq.syntax.BinderGeneralizing(value=gb):
            return _hoist_hydra_coq_serde_binder_to_expr_2(gb)

        case hydra.coq.syntax.BinderPattern():
            return hydra.serialization.cst("_")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def match_to_expr(m: hydra.coq.syntax.Match) -> hydra.ast.Expr:
    @lru_cache(1)
    def items() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda ci: (t := term100_to_expr(ci.term), as_p := hydra.lib.maybes.maybe((lambda : ()), (lambda n: (hydra.serialization.cst("as"), hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("_")), (lambda i: ident_to_expr(i)), n.value))), ci.as_), hydra.serialization.space_sep(hydra.lib.lists.concat(((t,), as_p))))[2]), m.case_items)
    @lru_cache(1)
    def ret() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda r: (hydra.serialization.cst("return"), term100_to_expr(r))), m.return_)
    @lru_cache(1)
    def eqs() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda eq: (pat_groups := hydra.lib.lists.map((lambda grp: hydra.serialization.space_sep(hydra.lib.lists.map((lambda p: pattern_to_expr(p)), grp))), eq.pattern), pats := hydra.serialization.infix_ws_list(" | ", pat_groups), body := term_to_expr(eq.term), hydra.serialization.space_sep((hydra.serialization.cst("|"), pats, hydra.serialization.cst("=>"), body)))[3]), m.equations)
    return hydra.serialization.newline_sep(hydra.lib.lists.concat(((hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("match"),), items(), ret(), (hydra.serialization.cst("with"),)))),), eqs(), (hydra.serialization.cst("end"),))))

def term0_to_expr(t: hydra.coq.syntax.Term0):
    def _hoist_hydra_coq_serde_term0_to_expr_1(v1):
        match v1:
            case hydra.coq.syntax.PrimitiveNotationsNumber(value=n):
                v = n.value
                return hydra.serialization.cst(hydra.lib.literals.show_bigfloat(v))

            case hydra.coq.syntax.PrimitiveNotationsString(value=s2):
                return hydra.serialization.space_sep((hydra.serialization.cst("\""), hydra.serialization.cst(s2.value), hydra.serialization.cst("\"")))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    match t:
        case hydra.coq.syntax.Term0QualidAnnotated(value=qa):
            return qualid_to_expr(qa.qualid)

        case hydra.coq.syntax.Term0Sort(value=s):
            return sort_to_expr(s)

        case hydra.coq.syntax.Term0PrimitiveNotations(value=pn):
            return _hoist_hydra_coq_serde_term0_to_expr_1(pn)

        case hydra.coq.syntax.Term0Evar():
            return hydra.serialization.cst("?evar")

        case hydra.coq.syntax.Term0Match(value=m):
            return match_to_expr(m)

        case hydra.coq.syntax.Term0Record():
            return hydra.serialization.cst("{| |}")

        case hydra.coq.syntax.Term0Generalizing():
            return hydra.serialization.cst("`( )")

        case hydra.coq.syntax.Term0Ltac():
            return hydra.serialization.cst("ltac:( )")

        case hydra.coq.syntax.Term0Parens(value=inner):
            return hydra.serialization.parens(term_to_expr(inner))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def term100_to_expr(t: hydra.coq.syntax.Term100) -> hydra.ast.Expr:
    match t:
        case hydra.coq.syntax.Term100Cast(value=tc):
            return hydra.serialization.space_sep((term10_to_expr(tc.term), hydra.serialization.cst(":"), type_to_expr(tc.type)))

        case hydra.coq.syntax.Term100Term10(value=t10):
            return term10_to_expr(t10)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def term10_to_expr(t: hydra.coq.syntax.Term10):
    def _hoist_hydra_coq_serde_term10_to_expr_1(v1):
        match v1:
            case hydra.coq.syntax.OneTermExplicit(value=qa):
                return qualid_to_expr(qa.qualid)

            case hydra.coq.syntax.OneTermTerm1(value=t1):
                return term1_to_expr(t1)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    match t:
        case hydra.coq.syntax.Term10Application(value=app):
            return application_to_expr(app)

        case hydra.coq.syntax.Term10OneTerm(value=ot):
            return _hoist_hydra_coq_serde_term10_to_expr_1(ot)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def term1_to_expr(t: hydra.coq.syntax.Term1) -> hydra.ast.Expr:
    match t:
        case hydra.coq.syntax.Term1Projection():
            return hydra.serialization.cst("?projection")

        case hydra.coq.syntax.Term1Scope():
            return hydra.serialization.cst("?scope")

        case hydra.coq.syntax.Term1Term0(value=t0):
            return term0_to_expr(t0)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def term_to_expr(t: hydra.coq.syntax.Term):
    def _hoist_hydra_coq_serde_term_to_expr_1(v1):
        match v1:
            case hydra.coq.syntax.OpenBindersType(value=tb):
                @lru_cache(1)
                def names() -> frozenlist[hydra.ast.Expr]:
                    return hydra.lib.lists.map((lambda n: hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("_")), (lambda i: ident_to_expr(i)), n.value)), tb.names)
                @lru_cache(1)
                def ty() -> hydra.ast.Expr:
                    return type_to_expr(tb.type)
                return hydra.serialization.space_sep((hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.space_sep(names()), hydra.serialization.cst(":"), ty()))),))

            case hydra.coq.syntax.OpenBindersBinders(value=bs):
                return hydra.serialization.space_sep(hydra.lib.lists.map((lambda b: binder_to_expr(b)), bs))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_hydra_coq_serde_term_to_expr_2(v1):
        match v1:
            case hydra.coq.syntax.OpenBindersType(value=tb):
                @lru_cache(1)
                def names() -> frozenlist[hydra.ast.Expr]:
                    return hydra.lib.lists.map((lambda n: hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("_")), (lambda i: ident_to_expr(i)), n.value)), tb.names)
                @lru_cache(1)
                def ty() -> hydra.ast.Expr:
                    return type_to_expr(tb.type)
                return hydra.serialization.space_sep((hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.space_sep(names()), hydra.serialization.cst(":"), ty()))),))

            case hydra.coq.syntax.OpenBindersBinders(value=bs):
                return hydra.serialization.space_sep(hydra.lib.lists.map((lambda b: binder_to_expr(b)), bs))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_hydra_coq_serde_term_to_expr_3(v1):
        match v1:
            case hydra.coq.syntax.ForallOrFunForall(value=fa):
                return hydra.serialization.space_sep((hydra.serialization.cst("forall"), _hoist_hydra_coq_serde_term_to_expr_1(fa.binders), hydra.serialization.cst(","), type_to_expr(fa.type)))

            case hydra.coq.syntax.ForallOrFunFun(value=fn):
                return hydra.serialization.space_sep((hydra.serialization.cst("fun"), _hoist_hydra_coq_serde_term_to_expr_2(fn.binders), hydra.serialization.cst("=>"), term_to_expr(fn.body)))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_hydra_coq_serde_term_to_expr_4(v1):
        match v1:
            case hydra.coq.syntax.FixDecl(value=d):
                return hydra.serialization.space_sep((hydra.serialization.cst("fix"), (name := ident_to_expr(d.ident), binders := hydra.lib.lists.map((lambda b: binder_to_expr(b)), d.binders), ty := hydra.lib.maybes.maybe((lambda : ()), (lambda t2: (hydra.serialization.cst(":"), type_to_expr(t2))), d.type), body := term_to_expr(d.term), hydra.serialization.space_sep(hydra.lib.lists.concat(((name,), binders, ty, (hydra.serialization.cst(":="), body)))))[4]))

            case hydra.coq.syntax.FixQual():
                return hydra.serialization.cst("fix")

            case _:
                raise AssertionError("Unreachable: all variants handled")
    match t:
        case hydra.coq.syntax.TermForallOrFun(value=fof):
            return _hoist_hydra_coq_serde_term_to_expr_3(fof)

        case hydra.coq.syntax.TermLet(value=lt):
            bindings = lt.bindings
            @lru_cache(1)
            def body() -> hydra.ast.Expr:
                return term_to_expr(lt.in_)
            def _hoist_bindings_body_1(v1):
                match v1:
                    case hydra.coq.syntax.LetBindingsNamed(value=ln):
                        binder = ln.binder
                        @lru_cache(1)
                        def name() -> hydra.ast.Expr:
                            return hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("_")), (lambda i: ident_to_expr(i)), binder.name.value)
                        @lru_cache(1)
                        def binders() -> frozenlist[hydra.ast.Expr]:
                            return hydra.lib.lists.map((lambda b: binder_to_expr(b)), ln.binders)
                        @lru_cache(1)
                        def ty() -> frozenlist[hydra.ast.Expr]:
                            return hydra.lib.maybes.maybe((lambda : ()), (lambda t2: (hydra.serialization.cst(":"), type_to_expr(t2))), binder.type)
                        @lru_cache(1)
                        def val() -> hydra.ast.Expr:
                            return term_to_expr(binder.term)
                        return hydra.serialization.space_sep((hydra.serialization.cst("let"), hydra.serialization.space_sep(hydra.lib.lists.concat(((name(),), binders(), ty(), (hydra.serialization.cst(":="), val()), (hydra.serialization.cst("in"),)))), body()))

                    case hydra.coq.syntax.LetBindingsDestructuring():
                        return hydra.serialization.space_sep((hydra.serialization.cst("let"), hydra.serialization.cst("..."), hydra.serialization.cst("in"), body()))

                    case _:
                        raise AssertionError("Unreachable: all variants handled")
            return _hoist_bindings_body_1(bindings)

        case hydra.coq.syntax.TermIf(value=if_e):
            @lru_cache(1)
            def cond() -> hydra.ast.Expr:
                return term_to_expr(if_e.condition)
            @lru_cache(1)
            def thn() -> hydra.ast.Expr:
                return term_to_expr(if_e.then)
            @lru_cache(1)
            def els() -> hydra.ast.Expr:
                return term_to_expr(if_e.else_)
            return hydra.serialization.space_sep((hydra.serialization.cst("if"), cond(), hydra.serialization.cst("then"), thn(), hydra.serialization.cst("else"), els()))

        case hydra.coq.syntax.TermFix(value=fx):
            return _hoist_hydra_coq_serde_term_to_expr_4(fx)

        case hydra.coq.syntax.TermCofix():
            return hydra.serialization.space_sep((hydra.serialization.cst("cofix"), hydra.serialization.cst("...")))

        case hydra.coq.syntax.TermTerm100(value=t100):
            return term100_to_expr(t100)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def type_to_expr(t: hydra.coq.syntax.Type) -> hydra.ast.Expr:
    return term_to_expr(t.value)

def axiom_declaration_to_expr(a: hydra.coq.syntax.AxiomDeclaration) -> hydra.ast.Expr:
    return hydra.serialization.suffix(".", hydra.serialization.space_sep((hydra.serialization.cst("Axiom"), ident_to_expr(a.name), hydra.serialization.cst(":"), type_to_expr(a.type))))

def comment_to_expr(c: hydra.coq.syntax.Comment) -> hydra.ast.Expr:
    return hydra.serialization.cst(hydra.lib.strings.cat(("(* ", c.value, " *)")))

def constructor_to_expr(c: hydra.coq.syntax.Constructor) -> hydra.ast.Expr:
    @lru_cache(1)
    def name() -> hydra.ast.Expr:
        return ident_to_expr(c.name)
    @lru_cache(1)
    def binders() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda b: binder_to_expr(b)), c.binders)
    @lru_cache(1)
    def ty() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda t: (hydra.serialization.cst(":"), type_to_expr(t))), c.type)
    return hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("|"), name()), binders(), ty())))

def locality_to_expr(loc: hydra.coq.syntax.Locality) -> hydra.ast.Expr:
    match loc:
        case hydra.coq.syntax.Locality.LOCAL:
            return hydra.serialization.cst("Local")

        case hydra.coq.syntax.Locality.GLOBAL:
            return hydra.serialization.cst("Global")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def definition_to_expr(d: hydra.coq.syntax.Definition) -> hydra.ast.Expr:
    @lru_cache(1)
    def loc_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda l: (locality_to_expr(l),)), d.locality)
    @lru_cache(1)
    def name() -> hydra.ast.Expr:
        return ident_to_expr(d.name)
    @lru_cache(1)
    def binders() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda b: binder_to_expr(b)), d.binders)
    @lru_cache(1)
    def ty() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda t: (hydra.serialization.cst(":"), type_to_expr(t))), d.type)
    @lru_cache(1)
    def body() -> hydra.ast.Expr:
        return term_to_expr(d.body)
    return hydra.serialization.suffix(".", hydra.serialization.space_sep(hydra.lib.lists.concat((loc_part(), (hydra.serialization.cst("Definition"), name()), binders(), ty(), (hydra.serialization.cst(":="), body())))))

def fixpoint_definition_to_expr(fd: hydra.coq.syntax.FixpointDefinition) -> hydra.ast.Expr:
    @lru_cache(1)
    def loc_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda l: (locality_to_expr(l),)), fd.locality)
    @lru_cache(1)
    def name() -> hydra.ast.Expr:
        return ident_to_expr(fd.name)
    @lru_cache(1)
    def binders() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda b: binder_to_expr(b)), fd.binders)
    @lru_cache(1)
    def ty() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda t: (hydra.serialization.cst(":"), type_to_expr(t))), fd.type)
    @lru_cache(1)
    def body() -> hydra.ast.Expr:
        return term_to_expr(fd.body)
    return hydra.serialization.suffix(".", hydra.serialization.space_sep(hydra.lib.lists.concat((loc_part(), (hydra.serialization.cst("Fixpoint"), name()), binders(), ty(), (hydra.serialization.cst(":="), body())))))

def inductive_body_to_expr(ib: hydra.coq.syntax.InductiveBody) -> hydra.ast.Expr:
    @lru_cache(1)
    def name() -> hydra.ast.Expr:
        return ident_to_expr(ib.name)
    @lru_cache(1)
    def binders() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda b: binder_to_expr(b)), ib.binders)
    @lru_cache(1)
    def ty() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda t: (hydra.serialization.cst(":"), type_to_expr(t))), ib.type)
    @lru_cache(1)
    def constrs() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda c: constructor_to_expr(c)), ib.constructors)
    return hydra.serialization.newline_sep(hydra.lib.lists.concat(((hydra.serialization.space_sep(hydra.lib.lists.concat(((name(),), binders(), ty(), (hydra.serialization.cst(":="),)))),), constrs())))

def inductive_definition_to_expr(id: hydra.coq.syntax.InductiveDefinition) -> hydra.ast.Expr:
    @lru_cache(1)
    def loc_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda l: (locality_to_expr(l),)), id.locality)
    @lru_cache(1)
    def kw_part() -> hydra.ast.Expr:
        return hydra.lib.logic.if_else(id.coinductive, (lambda : hydra.serialization.cst("CoInductive")), (lambda : hydra.serialization.cst("Inductive")))
    @lru_cache(1)
    def body_exprs() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda b: inductive_body_to_expr(b)), id.bodies)
    @lru_cache(1)
    def first_body() -> hydra.ast.Expr:
        return hydra.lib.maybes.from_maybe((lambda : hydra.serialization.cst("")), hydra.lib.lists.maybe_head(body_exprs()))
    @lru_cache(1)
    def rest_bodies() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda b: hydra.serialization.space_sep((hydra.serialization.cst("with"), b))), hydra.lib.lists.drop(1, body_exprs()))
    @lru_cache(1)
    def first_line() -> hydra.ast.Expr:
        return hydra.serialization.space_sep(hydra.lib.lists.concat((loc_part(), (kw_part(), first_body()))))
    return hydra.serialization.suffix(".", hydra.serialization.newline_sep(hydra.lib.lists.cons(first_line(), rest_bodies())))

def record_field_to_expr(rf: hydra.coq.syntax.RecordField) -> hydra.ast.Expr:
    return hydra.serialization.space_sep((ident_to_expr(rf.name), hydra.serialization.cst(":"), type_to_expr(rf.type)))

def record_definition_to_expr(rd: hydra.coq.syntax.RecordDefinition) -> hydra.ast.Expr:
    @lru_cache(1)
    def loc_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda l: (locality_to_expr(l),)), rd.locality)
    @lru_cache(1)
    def name() -> hydra.ast.Expr:
        return ident_to_expr(rd.name)
    @lru_cache(1)
    def binders() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda b: binder_to_expr(b)), rd.binders)
    @lru_cache(1)
    def sort_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda s: (hydra.serialization.cst(":"), sort_to_expr(s))), rd.sort)
    body = rd.body
    @lru_cache(1)
    def constr_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda c: (ident_to_expr(c),)), body.constructor)
    @lru_cache(1)
    def fields() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda f: hydra.serialization.suffix(" ;", record_field_to_expr(f))), body.fields)
    return hydra.serialization.suffix(".", hydra.serialization.newline_sep(hydra.lib.lists.concat(((hydra.serialization.space_sep(hydra.lib.lists.concat((loc_part(), (hydra.serialization.cst("Record"),), (name(),), binders(), sort_part(), (hydra.serialization.cst(":="),), constr_part(), (hydra.serialization.cst("{"),)))),), fields(), (hydra.serialization.cst("}"),)))))

def require_import_to_expr(ri: hydra.coq.syntax.RequireImport) -> hydra.ast.Expr:
    @lru_cache(1)
    def from_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda q: (hydra.serialization.cst("From"), qualid_to_expr(q))), ri.from_)
    @lru_cache(1)
    def require_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(ri.require, (lambda : (hydra.serialization.cst("Require"),)), (lambda : ()))
    @lru_cache(1)
    def qual_part():
        def _hoist_qual_part_1(v1):
            match v1:
                case hydra.coq.syntax.ImportQualification.IMPORT:
                    return (hydra.serialization.cst("Import"),)

                case hydra.coq.syntax.ImportQualification.EXPORT:
                    return (hydra.serialization.cst("Export"),)

                case _:
                    raise AssertionError("Unreachable: all variants handled")
        return hydra.lib.maybes.maybe((lambda : ()), (lambda q: _hoist_qual_part_1(q)), ri.qualification)
    @lru_cache(1)
    def mods() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda m: qualid_to_expr(m)), ri.modules)
    return hydra.serialization.suffix(".", hydra.serialization.space_sep(hydra.lib.lists.concat((from_part(), require_part(), qual_part(), mods()))))

def theorem_body_to_expr(tb: hydra.coq.syntax.TheoremBody) -> hydra.ast.Expr:
    @lru_cache(1)
    def kind_kw() -> hydra.ast.Expr:
        match tb.kind:
            case hydra.coq.syntax.TheoremKind.THEOREM:
                return hydra.serialization.cst("Theorem")

            case hydra.coq.syntax.TheoremKind.LEMMA:
                return hydra.serialization.cst("Lemma")

            case hydra.coq.syntax.TheoremKind.PROPOSITION:
                return hydra.serialization.cst("Proposition")

            case hydra.coq.syntax.TheoremKind.COROLLARY:
                return hydra.serialization.cst("Corollary")

            case hydra.coq.syntax.TheoremKind.EXAMPLE:
                return hydra.serialization.cst("Example")

            case _:
                raise AssertionError("Unreachable: all variants handled")
    @lru_cache(1)
    def name() -> hydra.ast.Expr:
        return ident_to_expr(tb.name)
    @lru_cache(1)
    def binders() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda b: binder_to_expr(b)), tb.binders)
    @lru_cache(1)
    def ty() -> hydra.ast.Expr:
        return type_to_expr(tb.type)
    @lru_cache(1)
    def proof() -> hydra.ast.Expr:
        return term_to_expr(tb.proof)
    return hydra.serialization.newline_sep((hydra.serialization.suffix(".", hydra.serialization.space_sep(hydra.lib.lists.concat(((kind_kw(), name()), binders(), (hydra.serialization.cst(":"), ty()))))), hydra.serialization.cst("Proof."), hydra.serialization.space_sep((hydra.serialization.cst("exact"), hydra.serialization.parens(proof()))), hydra.serialization.cst("Qed.")))

def module_definition_to_expr(md: hydra.coq.syntax.ModuleDefinition) -> hydra.ast.Expr:
    @lru_cache(1)
    def name() -> hydra.ast.Expr:
        return ident_to_expr(md.name)
    @lru_cache(1)
    def sentences() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda s: sentence_to_expr(s)), md.sentences)
    return hydra.serialization.double_newline_sep(hydra.lib.lists.concat(((hydra.serialization.suffix(".", hydra.serialization.space_sep((hydra.serialization.cst("Module"), name()))),), sentences(), (hydra.serialization.suffix(".", hydra.serialization.space_sep((hydra.serialization.cst("End"), name()))),))))

def section_definition_to_expr(sd: hydra.coq.syntax.SectionDefinition) -> hydra.ast.Expr:
    @lru_cache(1)
    def name() -> hydra.ast.Expr:
        return ident_to_expr(sd.name)
    @lru_cache(1)
    def sentences() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda s: sentence_to_expr(s)), sd.sentences)
    return hydra.serialization.double_newline_sep(hydra.lib.lists.concat(((hydra.serialization.suffix(".", hydra.serialization.space_sep((hydra.serialization.cst("Section"), name()))),), sentences(), (hydra.serialization.suffix(".", hydra.serialization.space_sep((hydra.serialization.cst("End"), name()))),))))

def sentence_content_to_expr(sc: hydra.coq.syntax.SentenceContent) -> hydra.ast.Expr:
    match sc:
        case hydra.coq.syntax.SentenceContentAxiom(value=a):
            return axiom_declaration_to_expr(a)

        case hydra.coq.syntax.SentenceContentDefinition(value=d):
            return definition_to_expr(d)

        case hydra.coq.syntax.SentenceContentFixpoint(value=f):
            return fixpoint_definition_to_expr(f)

        case hydra.coq.syntax.SentenceContentInductive(value=i):
            return inductive_definition_to_expr(i)

        case hydra.coq.syntax.SentenceContentModule(value=m):
            return module_definition_to_expr(m)

        case hydra.coq.syntax.SentenceContentNotation():
            return hydra.serialization.cst("(* notation *)")

        case hydra.coq.syntax.SentenceContentRecord(value=r):
            return record_definition_to_expr(r)

        case hydra.coq.syntax.SentenceContentRequireImport(value=ri):
            return require_import_to_expr(ri)

        case hydra.coq.syntax.SentenceContentSection(value=s):
            return section_definition_to_expr(s)

        case hydra.coq.syntax.SentenceContentTheorem(value=t):
            return theorem_body_to_expr(t)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def sentence_to_expr(s: hydra.coq.syntax.Sentence) -> hydra.ast.Expr:
    @lru_cache(1)
    def cmt_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda c: (comment_to_expr(c),)), s.comment)
    @lru_cache(1)
    def content() -> hydra.ast.Expr:
        return sentence_content_to_expr(s.content)
    return hydra.serialization.newline_sep(hydra.lib.lists.concat((cmt_part(), (content(),))))

def document_to_expr(doc: hydra.coq.syntax.Document) -> hydra.ast.Expr:
    return hydra.serialization.double_newline_sep(hydra.lib.lists.map((lambda s: sentence_to_expr(s)), doc.sentences))
