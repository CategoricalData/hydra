# Note: this is an automatically generated file. Do not edit.

r"""Coq code generator: converts Hydra modules to Coq source."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.coq.environment
import hydra.coq.language
import hydra.coq.syntax
import hydra.coq.utils
import hydra.core
import hydra.formatting
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings

T0 = TypeVar("T0")

def coq_type_term(t: hydra.coq.syntax.Term) -> hydra.coq.syntax.Type:
    return hydra.coq.syntax.Type(t)

def coq_arrow(dom: hydra.coq.syntax.Term, cod: hydra.coq.syntax.Term) -> hydra.coq.syntax.Term:
    r"""Build the Coq dependent-function term `forall (_ : dom), cod` used as the arrow type."""

    return cast(hydra.coq.syntax.Term, hydra.coq.syntax.TermForallOrFun(cast(hydra.coq.syntax.ForallOrFun, hydra.coq.syntax.ForallOrFunForall(hydra.coq.syntax.Forall(cast(hydra.coq.syntax.OpenBinders, hydra.coq.syntax.OpenBindersBinders((cast(hydra.coq.syntax.Binder, hydra.coq.syntax.BinderType(hydra.coq.syntax.TypeBinders((hydra.coq.syntax.Name(Nothing()),), coq_type_term(dom)))),))), coq_type_term(cod))))))

def coq_ident(s: str) -> hydra.coq.syntax.Ident:
    return hydra.coq.syntax.Ident(hydra.coq.syntax.String(s))

def coq_name(s: str) -> hydra.coq.syntax.Name:
    return hydra.coq.syntax.Name(Just(coq_ident(s)))

def coq_qualid(s: str) -> hydra.coq.syntax.Qualid:
    return hydra.coq.syntax.Qualid(coq_ident(s), ())

def coq_term_app(f: hydra.coq.syntax.Term, args: frozenlist[hydra.coq.syntax.Term]) -> hydra.coq.syntax.Term:
    r"""Apply a Coq term to a list of argument terms, parenthesising each."""

    return hydra.lib.logic.if_else(hydra.lib.lists.null(args), (lambda : f), (lambda : cast(hydra.coq.syntax.Term, hydra.coq.syntax.TermTerm100(cast(hydra.coq.syntax.Term100, hydra.coq.syntax.Term100Term10(cast(hydra.coq.syntax.Term10, hydra.coq.syntax.Term10Application(cast(hydra.coq.syntax.Application, hydra.coq.syntax.ApplicationNormal(hydra.coq.syntax.NormalApplication(cast(hydra.coq.syntax.Term1, hydra.coq.syntax.Term1Term0(cast(hydra.coq.syntax.Term0, hydra.coq.syntax.Term0Parens(f)))), hydra.lib.lists.map((lambda a: cast(hydra.coq.syntax.Arg, hydra.coq.syntax.ArgTerm(cast(hydra.coq.syntax.Term1, hydra.coq.syntax.Term1Term0(cast(hydra.coq.syntax.Term0, hydra.coq.syntax.Term0Parens(a))))))), args))))))))))))

def coq_term_cast(t: hydra.coq.syntax.Term, ty: hydra.coq.syntax.Type) -> hydra.coq.syntax.Term:
    r"""Build a Coq Term expressing `(t : T)` with the normal cast operator."""

    return cast(hydra.coq.syntax.Term, hydra.coq.syntax.TermTerm100(cast(hydra.coq.syntax.Term100, hydra.coq.syntax.Term100Cast(hydra.coq.syntax.TypeCast(cast(hydra.coq.syntax.Term10, hydra.coq.syntax.Term10OneTerm(cast(hydra.coq.syntax.OneTerm, hydra.coq.syntax.OneTermTerm1(cast(hydra.coq.syntax.Term1, hydra.coq.syntax.Term1Term0(cast(hydra.coq.syntax.Term0, hydra.coq.syntax.Term0Parens(t)))))))), ty, hydra.coq.syntax.TypeCastOperator.NORMAL)))))

def coq_term_qualid(s: str) -> hydra.coq.syntax.Term:
    r"""Build a Coq Term that references a (possibly qualified) identifier."""

    return cast(hydra.coq.syntax.Term, hydra.coq.syntax.TermTerm100(cast(hydra.coq.syntax.Term100, hydra.coq.syntax.Term100Term10(cast(hydra.coq.syntax.Term10, hydra.coq.syntax.Term10OneTerm(cast(hydra.coq.syntax.OneTerm, hydra.coq.syntax.OneTermExplicit(hydra.coq.syntax.QualidAnnotated(coq_qualid(s), Nothing())))))))))

def encode_literal_type(lt: hydra.core.LiteralType):
    def _hoist_hydra_coq_coder_encode_literal_type_1(v1):
        match v1:
            case hydra.core.IntegerType.BIGINT:
                return coq_term_qualid("Z")

            case hydra.core.IntegerType.INT8:
                return coq_term_qualid("Z")

            case hydra.core.IntegerType.INT16:
                return coq_term_qualid("Z")

            case hydra.core.IntegerType.INT32:
                return coq_term_qualid("Z")

            case hydra.core.IntegerType.INT64:
                return coq_term_qualid("Z")

            case hydra.core.IntegerType.UINT8:
                return coq_term_qualid("nat")

            case hydra.core.IntegerType.UINT16:
                return coq_term_qualid("nat")

            case hydra.core.IntegerType.UINT32:
                return coq_term_qualid("nat")

            case hydra.core.IntegerType.UINT64:
                return coq_term_qualid("nat")

            case _:
                raise AssertionError("Unreachable: all variants handled")
    match lt:
        case hydra.core.LiteralTypeBoolean():
            return coq_term_qualid("bool")

        case hydra.core.LiteralTypeDecimal():
            return coq_term_qualid("Q")

        case hydra.core.LiteralTypeFloat():
            return coq_term_qualid("Q")

        case hydra.core.LiteralTypeInteger(value=it):
            return _hoist_hydra_coq_coder_encode_literal_type_1(it)

        case hydra.core.LiteralTypeString():
            return coq_term_qualid("string")

        case hydra.core.LiteralTypeBinary():
            return coq_term_qualid("string")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def rename_lib_keyword(s: str) -> str:
    r"""Rewrite a stripped hydra.lib.<mod>.<func> name to avoid Coq keyword collisions."""

    return hydra.lib.logic.if_else(hydra.lib.equality.equal(s, "lists.at"), (lambda : "lists.at_"), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(s, "math.mod"), (lambda : "math.mod_"), (lambda : s))))

def sanitize_stripped(s: str) -> str:
    r"""Append an underscore if a stripped-local-name reference collides with a Coq reserved word."""

    return hydra.formatting.escape_with_underscore(hydra.coq.language.coq_stripped_reserved_words(), s)

def sanitize_var(s: str) -> str:
    r"""Append an underscore if the name collides with a Coq reserved word."""

    return hydra.formatting.escape_with_underscore(hydra.coq.language.coq_reserved_words(), s)

def resolve_qualified_name(env: hydra.coq.environment.CoqEnvironment, s: str) -> str:
    r"""Resolve a (possibly qualified) Hydra identifier to the form that should appear in Coq source."""

    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.strings.split_on(".", s)
    @lru_cache(1)
    def head1() -> str:
        return hydra.lib.maybes.from_maybe((lambda : s), hydra.lib.lists.maybe_head(parts()))
    current_ns = env.current_namespace
    ambig = env.ambiguous_names
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(head1(), "Coq"), (lambda : hydra.lib.maybes.from_maybe((lambda : s), hydra.lib.lists.maybe_last(parts()))), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(head1(), "Build_hydra"), (lambda : hydra.lib.strings.cat2("Build_", sanitize_stripped(hydra.lib.maybes.from_maybe((lambda : s), hydra.lib.lists.maybe_last(parts()))))), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(head1(), "hydra"), (lambda : (rest := hydra.lib.lists.drop(1, parts()), head2 := hydra.lib.maybes.from_maybe((lambda : ""), hydra.lib.lists.maybe_head(rest)), hydra.lib.logic.if_else(hydra.lib.equality.equal(head2, "lib"), (lambda : rename_lib_keyword(hydra.lib.strings.intercalate(".", hydra.lib.lists.drop(1, rest)))), (lambda : (local_raw := hydra.lib.maybes.from_maybe((lambda : s), hydra.lib.lists.maybe_last(parts())), local_n := sanitize_stripped(local_raw), source_ns := hydra.lib.strings.intercalate(".", hydra.lib.maybes.from_maybe((lambda : ()), hydra.lib.lists.maybe_init(parts()))), is_current := hydra.lib.equality.equal(source_ns, current_ns), is_ambig := hydra.lib.sets.member(local_raw, ambig), is_collision_prone := hydra.lib.logic.and_(hydra.lib.equality.equal(hydra.lib.lists.length(parts()), 3), hydra.lib.logic.and_(hydra.lib.equality.equal(head2, "parsers"), hydra.lib.logic.not_(is_current))), hydra.lib.logic.if_else(hydra.lib.logic.and_(is_ambig, hydra.lib.logic.not_(is_current)), (lambda : hydra.lib.strings.cat((source_ns, ".", local_n))), (lambda : hydra.lib.logic.if_else(is_collision_prone, (lambda : hydra.lib.strings.cat((sanitize_stripped(head2), ".", sanitize_stripped(local_raw)))), (lambda : local_n)))))[6])))[2]), (lambda : sanitize_var(s)))))))

def encode_type(env: hydra.coq.environment.CoqEnvironment, ty: hydra.core.Type) -> hydra.coq.syntax.Term:
    r"""Translate a Hydra Type into a Coq Term representing that type. The environment is consulted to resolve qualified type variable references."""

    match ty:
        case hydra.core.TypeAnnotated(value=at):
            return encode_type(env, at.body)

        case hydra.core.TypeApplication(value=app):
            return coq_term_app(encode_type(env, app.function), (encode_type(env, app.argument),))

        case hydra.core.TypeEither(value=et):
            return coq_term_app(coq_term_qualid("sum"), (encode_type(env, et.left), encode_type(env, et.right)))

        case hydra.core.TypeForall(value=ft):
            return cast(hydra.coq.syntax.Term, hydra.coq.syntax.TermForallOrFun(cast(hydra.coq.syntax.ForallOrFun, hydra.coq.syntax.ForallOrFunForall(hydra.coq.syntax.Forall(cast(hydra.coq.syntax.OpenBinders, hydra.coq.syntax.OpenBindersBinders((cast(hydra.coq.syntax.Binder, hydra.coq.syntax.BinderType(hydra.coq.syntax.TypeBinders((coq_name(ft.parameter.value),), coq_type_term(coq_term_qualid("Type"))))),))), coq_type_term(encode_type(env, ft.body)))))))

        case hydra.core.TypeFunction(value=ft2):
            return coq_arrow(encode_type(env, ft2.domain), encode_type(env, ft2.codomain))

        case hydra.core.TypeList(value=t):
            return coq_term_app(coq_term_qualid("list"), (encode_type(env, t),))

        case hydra.core.TypeLiteral(value=lt):
            return encode_literal_type(lt)

        case hydra.core.TypeMap(value=mt):
            return coq_term_app(coq_term_qualid("list"), (coq_term_app(coq_term_qualid("prod"), (encode_type(env, mt.keys), encode_type(env, mt.values))),))

        case hydra.core.TypeMaybe(value=t2):
            return coq_term_app(coq_term_qualid("option"), (encode_type(env, t2),))

        case hydra.core.TypePair(value=pt):
            return coq_term_app(coq_term_qualid("prod"), (encode_type(env, pt.first), encode_type(env, pt.second)))

        case hydra.core.TypeRecord():
            return coq_term_qualid("unit")

        case hydra.core.TypeSet(value=t3):
            return coq_term_app(coq_term_qualid("list"), (encode_type(env, t3),))

        case hydra.core.TypeUnion():
            return coq_term_qualid("unit")

        case hydra.core.TypeUnit():
            return coq_term_qualid("unit")

        case hydra.core.TypeVariable(value=n):
            raw = n.value
            @lru_cache(1)
            def head_seg() -> str:
                return hydra.lib.maybes.from_maybe((lambda : raw), hydra.lib.lists.maybe_head(hydra.lib.strings.split_on(".", raw)))
            return hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.lib.equality.equal(head_seg(), "hydra"), hydra.lib.equality.equal(head_seg(), "Build_hydra")), (lambda : coq_term_qualid(resolve_qualified_name(env, raw))), (lambda : coq_term_qualid(raw)))

        case hydra.core.TypeVoid():
            return coq_term_qualid("Empty_set")

        case hydra.core.TypeWrap(value=wt):
            return encode_type(env, wt)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_axiom_definition_pair(env: hydra.coq.environment.CoqEnvironment, nt: tuple[str, hydra.core.Type]) -> hydra.coq.syntax.Sentence:
    r"""Produce `Axiom name : type.` from a (name, Hydra type) pair."""

    return hydra.coq.syntax.Sentence(Nothing(), cast(hydra.coq.syntax.SentenceContent, hydra.coq.syntax.SentenceContentAxiom(hydra.coq.syntax.AxiomDeclaration(coq_ident(hydra.lib.pairs.first(nt)), coq_type_term(encode_type(env, hydra.lib.pairs.second(nt)))))))

def encode_float_literal(s: str) -> hydra.coq.syntax.Term:
    r"""Map a Haskell-`show`n Double/Scientific to a Coq term, routing NaN/Inf to base-lib axioms."""

    return hydra.lib.logic.if_else(hydra.lib.equality.equal(s, "Infinity"), (lambda : coq_term_qualid("hydra_posInf")), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(s, "-Infinity"), (lambda : coq_term_qualid("hydra_negInf")), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(s, "NaN"), (lambda : coq_term_qualid("hydra_nan")), (lambda : coq_term_qualid(hydra.lib.strings.cat(("(", s, ")")))))))))

def escape_coq_string(s: str) -> str:
    r"""Escape a string for Coq string literals: double any embedded quotes."""

    return hydra.lib.strings.intercalate("\"\"", hydra.lib.strings.split_on("\"", s))

def encode_literal(lit: hydra.core.Literal):
    def _hoist_hydra_coq_coder_encode_literal_1(v1):
        match v1:
            case hydra.core.FloatValueBigfloat(value=v):
                return encode_float_literal(hydra.lib.literals.show_bigfloat(v))

            case hydra.core.FloatValueFloat32(value=v):
                return encode_float_literal(hydra.lib.literals.show_bigfloat(hydra.lib.literals.float32_to_bigfloat(v)))

            case hydra.core.FloatValueFloat64(value=v):
                return encode_float_literal(hydra.lib.literals.show_bigfloat(hydra.lib.literals.float64_to_bigfloat(v)))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_hydra_coq_coder_encode_literal_2(v1):
        match v1:
            case hydra.core.IntegerValueBigint(value=v):
                return coq_term_qualid(hydra.lib.strings.cat(("(", hydra.lib.literals.show_bigint(v), ")%Z")))

            case hydra.core.IntegerValueInt8(value=v):
                return coq_term_qualid(hydra.lib.strings.cat(("(", hydra.lib.literals.show_int8(v), ")%Z")))

            case hydra.core.IntegerValueInt16(value=v):
                return coq_term_qualid(hydra.lib.strings.cat(("(", hydra.lib.literals.show_int16(v), ")%Z")))

            case hydra.core.IntegerValueInt32(value=v):
                return coq_term_qualid(hydra.lib.strings.cat(("(", hydra.lib.literals.show_int32(v), ")%Z")))

            case hydra.core.IntegerValueInt64(value=v):
                return coq_term_qualid(hydra.lib.strings.cat(("(", hydra.lib.literals.show_int64(v), ")%Z")))

            case hydra.core.IntegerValueUint8(value=v):
                return coq_term_qualid(hydra.lib.strings.cat(("(", hydra.lib.literals.show_uint8(v), ")")))

            case hydra.core.IntegerValueUint16(value=v):
                return coq_term_qualid(hydra.lib.strings.cat(("(", hydra.lib.literals.show_uint16(v), ")")))

            case hydra.core.IntegerValueUint32(value=v):
                return coq_term_qualid(hydra.lib.strings.cat(("(", hydra.lib.literals.show_uint32(v), ")")))

            case hydra.core.IntegerValueUint64(value=v):
                return coq_term_qualid(hydra.lib.strings.cat(("(", hydra.lib.literals.show_uint64(v), ")")))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    match lit:
        case hydra.core.LiteralBoolean(value=b):
            return hydra.lib.logic.if_else(b, (lambda : coq_term_qualid("true")), (lambda : coq_term_qualid("false")))

        case hydra.core.LiteralDecimal(value=d):
            return coq_term_qualid(hydra.lib.strings.cat(("(", hydra.lib.literals.show_decimal(d), ")")))

        case hydra.core.LiteralFloat(value=fv):
            return _hoist_hydra_coq_coder_encode_literal_1(fv)

        case hydra.core.LiteralInteger(value=iv):
            return _hoist_hydra_coq_coder_encode_literal_2(iv)

        case hydra.core.LiteralString(value=s):
            return coq_term_qualid(hydra.lib.strings.cat(("\"", escape_coq_string(s), "\"%string")))

        case hydra.core.LiteralBinary():
            return coq_term_qualid("\"\"")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_projection_elim(env: hydra.coq.environment.CoqEnvironment, p: hydra.core.Projection) -> hydra.coq.syntax.Term:
    r"""Translate a Hydra record projection into a Coq lambda that pulls out the field."""

    fname = p.field
    raw_fname = fname.value
    sanitized_set = env.sanitized_accessors
    return hydra.lib.logic.if_else(hydra.lib.sets.member(raw_fname, sanitized_set), (lambda : cast(hydra.coq.syntax.Term, hydra.coq.syntax.TermForallOrFun(cast(hydra.coq.syntax.ForallOrFun, hydra.coq.syntax.ForallOrFunFun(hydra.coq.syntax.Fun(cast(hydra.coq.syntax.OpenBinders, hydra.coq.syntax.OpenBindersBinders((cast(hydra.coq.syntax.Binder, hydra.coq.syntax.BinderName(coq_name("_"))),))), coq_term_qualid("hydra_unreachable"))))))), (lambda : cast(hydra.coq.syntax.Term, hydra.coq.syntax.TermForallOrFun(cast(hydra.coq.syntax.ForallOrFun, hydra.coq.syntax.ForallOrFunFun(hydra.coq.syntax.Fun(cast(hydra.coq.syntax.OpenBinders, hydra.coq.syntax.OpenBindersBinders((cast(hydra.coq.syntax.Binder, hydra.coq.syntax.BinderName(coq_name("r_"))),))), coq_term_app(coq_term_qualid(raw_fname), (coq_term_qualid("r_"),)))))))))

def is_unit_domain(mty: Maybe[hydra.core.Type]):
    def _hoist_hydra_coq_coder_is_unit_domain_1(v1):
        match v1:
            case hydra.core.TypeUnit():
                return True

            case hydra.core.TypeRecord(value=fs):
                return hydra.lib.lists.null(fs)

            case hydra.core.TypeAnnotated(value=at):
                return is_unit_domain(Just(at.body))

            case _:
                return False
    return hydra.lib.maybes.maybe((lambda : False), (lambda ty: _hoist_hydra_coq_coder_is_unit_domain_1(ty)), mty)

def term_references_var(name: hydra.core.Name, tm: hydra.core.Term) -> bool:
    r"""Syntactic free-variable check over the shapes encodeTerm walks through."""

    match tm:
        case hydra.core.TermVariable(value=v):
            return hydra.lib.equality.equal(v, name)

        case hydra.core.TermAnnotated(value=at):
            return term_references_var(name, at.body)

        case hydra.core.TermApplication(value=app):
            return hydra.lib.logic.or_(term_references_var(name, app.function), term_references_var(name, app.argument))

        case hydra.core.TermLambda(value=lam):
            return term_references_var(name, lam.body)

        case hydra.core.TermCases(value=cs):
            return hydra.lib.logic.or_(hydra.lib.maybes.is_just(hydra.lib.lists.find((lambda f: term_references_var(name, f.term)), cs.cases)), hydra.lib.maybes.maybe((lambda : False), (lambda d: term_references_var(name, d)), cs.default))

        case hydra.core.TermLet(value=lt):
            return hydra.lib.logic.or_(hydra.lib.maybes.is_just(hydra.lib.lists.find((lambda b: term_references_var(name, b.term)), lt.bindings)), term_references_var(name, lt.body))

        case hydra.core.TermList(value=xs):
            return hydra.lib.maybes.is_just(hydra.lib.lists.find((lambda el: term_references_var(name, el)), xs))

        case hydra.core.TermMaybe(value=mt):
            return hydra.lib.maybes.maybe((lambda : False), (lambda el: term_references_var(name, el)), mt)

        case hydra.core.TermPair(value=p):
            return hydra.lib.logic.or_(term_references_var(name, hydra.lib.pairs.first(p)), term_references_var(name, hydra.lib.pairs.second(p)))

        case hydra.core.TermRecord(value=r):
            return hydra.lib.maybes.is_just(hydra.lib.lists.find((lambda f: term_references_var(name, f.term)), r.fields))

        case hydra.core.TermInject(value=inj):
            return term_references_var(name, inj.field.term)

        case hydra.core.TermEither(value=e):
            return hydra.lib.eithers.either((lambda l: term_references_var(name, l)), (lambda r: term_references_var(name, r)), e)

        case hydra.core.TermTypeApplication(value=ta):
            return term_references_var(name, ta.body)

        case hydra.core.TermTypeLambda(value=tl):
            return term_references_var(name, tl.body)

        case hydra.core.TermWrap(value=wt):
            return term_references_var(name, wt.body)

        case _:
            return False

def is_unit_lambda(tm: hydra.core.Term) -> bool:
    r"""Detect a lambda over the unit type whose parameter is not referenced in the body."""

    while True:
        match tm:
            case hydra.core.TermAnnotated(value=at):
                tm = at.body
                continue

            case hydra.core.TermLambda(value=lam):
                return (unused := hydra.lib.logic.not_(term_references_var(lam.parameter, lam.body)), hydra.lib.logic.and_(is_unit_domain(lam.domain), unused))[1]

            case _:
                return False

def local_type_name(s: str) -> str:
    r"""Take the last dot-separated segment of a (possibly) qualified Hydra name and sanitize it."""

    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.strings.split_on(".", s)
    @lru_cache(1)
    def local_part() -> str:
        return hydra.lib.maybes.from_maybe((lambda : s), hydra.lib.lists.maybe_last(parts()))
    return sanitize_var(local_part())

def union_constructor_name(type_name: str, field_name: str) -> str:
    r"""Combine a type name and field name into a constructor identifier, preserving the namespace prefix."""

    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.strings.split_on(".", type_name)
    @lru_cache(1)
    def local_part() -> str:
        return hydra.lib.maybes.from_maybe((lambda : type_name), hydra.lib.lists.maybe_last(parts()))
    @lru_cache(1)
    def prefix_parts() -> frozenlist[str]:
        return hydra.lib.maybes.from_maybe((lambda : ()), hydra.lib.lists.maybe_init(parts()))
    @lru_cache(1)
    def prefix() -> str:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(prefix_parts()), (lambda : ""), (lambda : hydra.lib.strings.cat2(hydra.lib.strings.intercalate(".", prefix_parts()), ".")))
    @lru_cache(1)
    def sanitized() -> str:
        return sanitize_var(local_part())
    return hydra.lib.strings.cat((prefix(), sanitized(), "_", hydra.formatting.capitalize(field_name)))

def unit_lambda_body(tm: hydra.core.Term) -> hydra.core.Term:
    r"""Peel the outer unit lambda off a term, returning the body."""

    while True:
        match tm:
            case hydra.core.TermAnnotated(value=at):
                tm = at.body
                continue

            case hydra.core.TermLambda(value=lam):
                return lam.body

            case _:
                return tm

def encode_wrap_elim(_n: T0) -> hydra.coq.syntax.Term:
    r"""A Hydra wrap eliminator is just the identity on the wrapped object in Coq."""

    return cast(hydra.coq.syntax.Term, hydra.coq.syntax.TermForallOrFun(cast(hydra.coq.syntax.ForallOrFun, hydra.coq.syntax.ForallOrFunFun(hydra.coq.syntax.Fun(cast(hydra.coq.syntax.OpenBinders, hydra.coq.syntax.OpenBindersBinders((cast(hydra.coq.syntax.Binder, hydra.coq.syntax.BinderName(coq_name("w_"))),))), coq_term_qualid("w_"))))))

def encode_lambda_term(env: hydra.coq.environment.CoqEnvironment, lam: hydra.core.Lambda) -> hydra.coq.syntax.Term:
    r"""Encode a Lambda into a Coq `fun` expression, sanitising the parameter name."""

    @lru_cache(1)
    def param_name() -> str:
        return sanitize_var(lam.parameter.value)
    @lru_cache(1)
    def binder() -> hydra.coq.syntax.Binder:
        return hydra.lib.maybes.maybe((lambda : cast(hydra.coq.syntax.Binder, hydra.coq.syntax.BinderName(coq_name(param_name())))), (lambda dom_ty: cast(hydra.coq.syntax.Binder, hydra.coq.syntax.BinderType(hydra.coq.syntax.TypeBinders((coq_name(param_name()),), coq_type_term(encode_type(env, dom_ty)))))), lam.domain)
    return cast(hydra.coq.syntax.Term, hydra.coq.syntax.TermForallOrFun(cast(hydra.coq.syntax.ForallOrFun, hydra.coq.syntax.ForallOrFunFun(hydra.coq.syntax.Fun(cast(hydra.coq.syntax.OpenBinders, hydra.coq.syntax.OpenBindersBinders((binder(),))), encode_term(env, lam.body))))))

def encode_term(env: hydra.coq.environment.CoqEnvironment, tm: hydra.core.Term):
    r"""Translate a Hydra Term into its Coq Term counterpart. The environment provides the constructor-count map used by encodeUnionElim (to decide whether a match is exhaustive) and the ambiguous-name set used by resolveQualifiedName (to decide whether cross-module references need to stay fully qualified)."""

    match tm:
        case hydra.core.TermAnnotated(value=at):
            return encode_term(env, at.body)

        case hydra.core.TermApplication(value=app):
            return coq_term_app(encode_term(env, app.function), (encode_term(env, app.argument),))

        case hydra.core.TermCases(value=cs):
            return encode_union_elim(env, cs)

        case hydra.core.TermEither(value=e):
            return hydra.lib.eithers.either((lambda l: coq_term_app(coq_term_qualid("inl"), (encode_term(env, l),))), (lambda r: coq_term_app(coq_term_qualid("inr"), (encode_term(env, r),))), e)

        case hydra.core.TermInject(value=inj):
            uname = inj.type_name
            ufield = inj.field
            fname = ufield.name
            fterm = ufield.term
            @lru_cache(1)
            def constr_name() -> str:
                return union_constructor_name(uname.value, fname.value)
            return coq_term_app(coq_term_qualid(resolve_qualified_name(env, constr_name())), (encode_term(env, fterm),))

        case hydra.core.TermLambda(value=lam):
            return encode_lambda_term(env, lam)

        case hydra.core.TermLet(value=lt):
            bindings = lt.bindings
            body = lt.body
            return hydra.lib.lists.foldr((lambda binding, acc: (bname := binding.name, bterm := binding.term, safe_name := sanitize_var(bname.value), recursive := term_references_var(bname, bterm), rec_body := coq_term_app(coq_term_qualid("hydra_fix"), (cast(hydra.coq.syntax.Term, hydra.coq.syntax.TermForallOrFun(cast(hydra.coq.syntax.ForallOrFun, hydra.coq.syntax.ForallOrFunFun(hydra.coq.syntax.Fun(cast(hydra.coq.syntax.OpenBinders, hydra.coq.syntax.OpenBindersBinders((cast(hydra.coq.syntax.Binder, hydra.coq.syntax.BinderName(coq_name(safe_name))),))), encode_term(env, bterm)))))),)), bound_term := hydra.lib.logic.if_else(recursive, (lambda : rec_body), (lambda : encode_term(env, bterm))), cast(hydra.coq.syntax.Term, hydra.coq.syntax.TermLet(hydra.coq.syntax.Let(cast(hydra.coq.syntax.LetBindings, hydra.coq.syntax.LetBindingsNamed(hydra.coq.syntax.LetNamed(hydra.coq.syntax.LetBinder(coq_name(safe_name), Nothing(), bound_term), ()))), acc))))[6]), encode_term(env, body), bindings)

        case hydra.core.TermList(value=xs):
            return hydra.lib.lists.foldr((lambda el, acc: coq_term_app(coq_term_qualid("cons"), (encode_term(env, el), acc))), coq_term_qualid("nil"), xs)

        case hydra.core.TermLiteral(value=l):
            return encode_literal(l)

        case hydra.core.TermMap(value=mt):
            return hydra.lib.lists.foldr((lambda kv, acc: coq_term_app(coq_term_qualid("cons"), (coq_term_app(coq_term_qualid("pair"), (encode_term(env, hydra.lib.pairs.first(kv)), encode_term(env, hydra.lib.pairs.second(kv)))), acc))), coq_term_qualid("nil"), hydra.lib.maps.to_list(mt))

        case hydra.core.TermMaybe(value=mt2):
            return hydra.lib.maybes.maybe((lambda : coq_term_qualid("None")), (lambda v: coq_term_app(coq_term_qualid("Some"), (encode_term(env, v),))), mt2)

        case hydra.core.TermPair(value=p):
            return coq_term_app(coq_term_qualid("pair"), (encode_term(env, hydra.lib.pairs.first(p)), encode_term(env, hydra.lib.pairs.second(p))))

        case hydra.core.TermProject(value=pr):
            return encode_projection_elim(env, pr)

        case hydra.core.TermRecord(value=r):
            rname = r.type_name
            rfields = r.fields
            return hydra.lib.logic.if_else(hydra.lib.lists.null(rfields), (lambda : coq_term_qualid("tt")), (lambda : coq_term_app(coq_term_qualid(resolve_qualified_name(env, hydra.lib.strings.cat2("Build_", rname.value))), hydra.lib.lists.map((lambda f: encode_term(env, f.term)), rfields))))

        case hydra.core.TermSet(value=st):
            return hydra.lib.lists.foldr((lambda el, acc: coq_term_app(coq_term_qualid("cons"), (encode_term(env, el), acc))), coq_term_qualid("nil"), hydra.lib.sets.to_list(st))

        case hydra.core.TermTypeApplication(value=ta):
            body = ta.body
            ty_arg = ta.type
            @lru_cache(1)
            def encoded() -> hydra.coq.syntax.Term:
                return encode_term(env, body)
            @lru_cache(1)
            def is_ground() -> bool:
                return hydra.lib.sets.null(hydra.coq.utils.collect_free_type_vars_in_type(ty_arg))
            def _hoist_body_body_1(v1):
                match v1:
                    case hydra.core.TypeEither():
                        return True

                    case hydra.core.TypePair():
                        return True

                    case hydra.core.TypeMap():
                        return True

                    case _:
                        return False
            def _hoist_body_body_2(v1):
                match v1:
                    case hydra.core.TypeEither(value=et):
                        @lru_cache(1)
                        def sum_ty() -> hydra.coq.syntax.Type:
                            return coq_type_term(coq_term_app(coq_term_qualid("sum"), (encode_type(env, et.left), encode_type(env, et.right))))
                        return coq_term_cast(encoded(), sum_ty())

                    case _:
                        return encoded()
            def _hoist_body_body_3(v1):
                match v1:
                    case hydra.core.TermMaybe(value=mt):
                        return hydra.lib.maybes.maybe((lambda : coq_term_cast(coq_term_qualid("None"), coq_type_term(coq_term_app(coq_term_qualid("option"), (encode_type(env, ty_arg),))))), (lambda _: encoded()), mt)

                    case hydra.core.TermList(value=xs):
                        return hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.lists.null(xs), _hoist_body_body_1(ty_arg)), (lambda : coq_term_cast(coq_term_qualid("nil"), coq_type_term(coq_term_app(coq_term_qualid("list"), (encode_type(env, ty_arg),))))), (lambda : encoded()))

                    case hydra.core.TermEither():
                        return _hoist_body_body_2(ty_arg)

                    case hydra.core.TermTypeApplication(value=inner_ta):
                        inner_body = inner_ta.body
                        inner_ty_arg = inner_ta.type
                        @lru_cache(1)
                        def inner_encoded() -> hydra.coq.syntax.Term:
                            return encode_term(env, inner_body)
                        def _hoist_inner_body_body_1(v12):
                            match v12:
                                case hydra.core.TermEither():
                                    @lru_cache(1)
                                    def sum_ty() -> hydra.coq.syntax.Type:
                                        return coq_type_term(coq_term_app(coq_term_qualid("sum"), (encode_type(env, inner_ty_arg), encode_type(env, ty_arg))))
                                    return coq_term_cast(inner_encoded(), sum_ty())

                                case _:
                                    return encoded()
                        return _hoist_inner_body_body_1(inner_body)

                    case _:
                        return encoded()
            return hydra.lib.logic.if_else(hydra.lib.logic.not_(is_ground()), (lambda : encoded()), (lambda : _hoist_body_body_3(body)))

        case hydra.core.TermTypeLambda(value=tl):
            return encode_term(env, tl.body)

        case hydra.core.TermUnit():
            return coq_term_qualid("tt")

        case hydra.core.TermUnwrap(value=n):
            return encode_wrap_elim(n)

        case hydra.core.TermVariable(value=n2):
            return coq_term_qualid(resolve_qualified_name(env, n2.value))

        case hydra.core.TermWrap(value=wt):
            return encode_term(env, wt.body)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_union_elim(env: hydra.coq.environment.CoqEnvironment, cs: hydra.core.CaseStatement) -> hydra.coq.syntax.Term:
    r"""Build a Coq match expression from a Hydra union eliminator. Uses the constructor-count map in the environment to decide whether the match is exhaustive: if so, an explicit default is suppressed; if not and the kernel didn't provide one, inserts `| _ => hydra_unreachable`."""

    cs_name = cs.type_name
    cs_cases = cs.cases
    cs_default = cs.default
    @lru_cache(1)
    def cs_local_name() -> str:
        return local_type_name(cs_name.value)
    @lru_cache(1)
    def expected_count() -> Maybe[int]:
        return hydra.lib.maps.lookup(cs_local_name(), env.constructor_counts)
    @lru_cache(1)
    def case_count() -> int:
        return hydra.lib.lists.length(cs_cases)
    @lru_cache(1)
    def base_eqs() -> frozenlist[hydra.coq.syntax.Equation]:
        return hydra.lib.lists.map((lambda c: (cfn := c.name, cft := c.term, constr := resolve_qualified_name(env, union_constructor_name(cs_name.value, cfn.value)), hydra.lib.logic.if_else(is_unit_lambda(cft), (lambda : hydra.coq.syntax.Equation(((cast(hydra.coq.syntax.Pattern, hydra.coq.syntax.PatternPattern(cast(hydra.coq.syntax.Pattern10, hydra.coq.syntax.Pattern10Qualiid(hydra.coq.syntax.Pattern10_Qualid(coq_qualid(constr), (hydra.coq.syntax.Pattern1(cast(hydra.coq.syntax.Pattern0, hydra.coq.syntax.Pattern0Qualid(coq_qualid("_"))), Nothing()),)))))),),), encode_term(env, unit_lambda_body(cft)))), (lambda : hydra.coq.syntax.Equation(((cast(hydra.coq.syntax.Pattern, hydra.coq.syntax.PatternPattern(cast(hydra.coq.syntax.Pattern10, hydra.coq.syntax.Pattern10Qualiid(hydra.coq.syntax.Pattern10_Qualid(coq_qualid(constr), (hydra.coq.syntax.Pattern1(cast(hydra.coq.syntax.Pattern0, hydra.coq.syntax.Pattern0Qualid(coq_qualid("v_"))), Nothing()),)))))),),), coq_term_app(encode_term(env, cft), (coq_term_qualid("v_"),))))))[3]), cs_cases)
    def wildcard_eq(body: hydra.coq.syntax.Term) -> hydra.coq.syntax.Equation:
        return hydra.coq.syntax.Equation(((cast(hydra.coq.syntax.Pattern, hydra.coq.syntax.PatternPattern(cast(hydra.coq.syntax.Pattern10, hydra.coq.syntax.Pattern10Qualiid(hydra.coq.syntax.Pattern10_Qualid(coq_qualid("_"), ()))))),),), body)
    @lru_cache(1)
    def default_eqs() -> frozenlist[hydra.coq.syntax.Equation]:
        return hydra.lib.maybes.maybe((lambda : hydra.lib.logic.if_else(hydra.lib.maybes.maybe((lambda : False), (lambda n: hydra.lib.logic.not_(hydra.lib.equality.gte(case_count(), n))), expected_count()), (lambda : (wildcard_eq(coq_term_qualid("hydra_unreachable")),)), (lambda : ()))), (lambda def_t: hydra.lib.logic.if_else(hydra.lib.maybes.maybe((lambda : False), (lambda n: hydra.lib.equality.gte(case_count(), n)), expected_count()), (lambda : ()), (lambda : (wildcard_eq(encode_term(env, def_t)),)))), cs_default)
    @lru_cache(1)
    def all_eqs() -> frozenlist[hydra.coq.syntax.Equation]:
        return hydra.lib.lists.concat2(base_eqs(), default_eqs())
    return cast(hydra.coq.syntax.Term, hydra.coq.syntax.TermForallOrFun(cast(hydra.coq.syntax.ForallOrFun, hydra.coq.syntax.ForallOrFunFun(hydra.coq.syntax.Fun(cast(hydra.coq.syntax.OpenBinders, hydra.coq.syntax.OpenBindersBinders((cast(hydra.coq.syntax.Binder, hydra.coq.syntax.BinderName(coq_name("x_"))),))), cast(hydra.coq.syntax.Term, hydra.coq.syntax.TermTerm100(cast(hydra.coq.syntax.Term100, hydra.coq.syntax.Term100Term10(cast(hydra.coq.syntax.Term10, hydra.coq.syntax.Term10OneTerm(cast(hydra.coq.syntax.OneTerm, hydra.coq.syntax.OneTermTerm1(cast(hydra.coq.syntax.Term1, hydra.coq.syntax.Term1Term0(cast(hydra.coq.syntax.Term0, hydra.coq.syntax.Term0Match(hydra.coq.syntax.Match((hydra.coq.syntax.CaseItem(cast(hydra.coq.syntax.Term100, hydra.coq.syntax.Term100Term10(cast(hydra.coq.syntax.Term10, hydra.coq.syntax.Term10OneTerm(cast(hydra.coq.syntax.OneTerm, hydra.coq.syntax.OneTermExplicit(hydra.coq.syntax.QualidAnnotated(coq_qualid("x_"), Nothing()))))))), Nothing(), Nothing()),), Nothing(), True, all_eqs()))))))))))))))))))

def encode_term_definition(env: hydra.coq.environment.CoqEnvironment, name: str, body: hydra.core.Term) -> hydra.coq.syntax.SentenceContent:
    r"""Build a Coq `Definition name := body.` sentence from a Hydra term."""

    return cast(hydra.coq.syntax.SentenceContent, hydra.coq.syntax.SentenceContentDefinition(hydra.coq.syntax.Definition(Nothing(), coq_ident(name), (), Nothing(), encode_term(env, body))))

def encode_term_definition_pair(env: hydra.coq.environment.CoqEnvironment, ed: tuple[str, hydra.core.Term]) -> hydra.coq.syntax.Sentence:
    r"""Wrap encodeTermDefinition in a Coq Sentence with no leading comment."""

    return hydra.coq.syntax.Sentence(Nothing(), encode_term_definition(env, hydra.lib.pairs.first(ed), hydra.lib.pairs.second(ed)))

def encode_type_definition(env: hydra.coq.environment.CoqEnvironment, name: str, ty: hydra.core.Type) -> hydra.coq.syntax.SentenceContent:
    r"""Build a Coq `Definition name : Type := body.` sentence from a Hydra type."""

    return cast(hydra.coq.syntax.SentenceContent, hydra.coq.syntax.SentenceContentDefinition(hydra.coq.syntax.Definition(Nothing(), coq_ident(name), (), Just(coq_type_term(coq_term_qualid("Type"))), encode_type(env, ty))))

def encode_type_definition_pair(env: hydra.coq.environment.CoqEnvironment, td: tuple[str, hydra.core.Type]) -> hydra.coq.syntax.Sentence:
    r"""Wrap encodeTypeDefinition in a Coq Sentence with no leading comment."""

    return hydra.coq.syntax.Sentence(Nothing(), encode_type_definition(env, hydra.lib.pairs.first(td), hydra.lib.pairs.second(td)))

def encode_union_constructor(env: hydra.coq.environment.CoqEnvironment, type_name: str, f: hydra.core.FieldType) -> hydra.coq.syntax.Constructor:
    r"""Construct a Coq Inductive Constructor line `Name_Tag : body -> Name` for a union variant."""

    ufn = f.name
    uft = f.type
    @lru_cache(1)
    def constr_name() -> str:
        return hydra.lib.strings.cat((type_name, "_", hydra.formatting.capitalize(ufn.value)))
    return hydra.coq.syntax.Constructor(coq_ident(constr_name()), (), Just(coq_type_term(coq_arrow(encode_type(env, uft), coq_term_qualid(type_name)))))

def extract_lambda_binders(env: hydra.coq.environment.CoqEnvironment, tm: hydra.core.Term) -> frozenlist[hydra.coq.syntax.Binder]:
    r"""Collect a chain of leading lambdas as Coq binders, converting type annotations as well."""

    match tm:
        case hydra.core.TermAnnotated(value=at):
            return extract_lambda_binders(env, at.body)

        case hydra.core.TermLambda(value=lam):
            param = lam.parameter
            m_domain = lam.domain
            @lru_cache(1)
            def binder() -> hydra.coq.syntax.Binder:
                return hydra.lib.maybes.maybe((lambda : cast(hydra.coq.syntax.Binder, hydra.coq.syntax.BinderName(coq_name(param.value)))), (lambda dom_ty: cast(hydra.coq.syntax.Binder, hydra.coq.syntax.BinderType(hydra.coq.syntax.TypeBinders((coq_name(param.value),), coq_type_term(encode_type(env, dom_ty)))))), m_domain)
            return hydra.lib.lists.cons(binder(), extract_lambda_binders(env, lam.body))

        case _:
            return ()

def require_import_sentence(mods: frozenlist[str]) -> hydra.coq.syntax.Sentence:
    r"""Emit a Coq `Require Import m1 m2 ...` sentence with a `Standard library imports` comment."""

    return hydra.coq.syntax.Sentence(Just(hydra.coq.syntax.Comment("Standard library imports")), cast(hydra.coq.syntax.SentenceContent, hydra.coq.syntax.SentenceContentRequireImport(hydra.coq.syntax.RequireImport(Nothing(), True, Just(hydra.coq.syntax.ImportQualification.IMPORT), hydra.lib.lists.map((lambda m: coq_qualid(m)), mods)))))

@lru_cache(1)
def standard_imports() -> hydra.coq.syntax.Sentence:
    r"""The Coq stdlib modules plus the hand-written hydra.lib.base axioms."""

    return require_import_sentence(("Stdlib.Strings.String", "Stdlib.Lists.List", "Stdlib.ZArith.ZArith", "Stdlib.QArith.QArith", "hydra.lib.base"))

def module_to_coq(env: hydra.coq.environment.CoqEnvironment, type_defs: frozenlist[tuple[str, hydra.core.Type]], term_defs: frozenlist[tuple[str, hydra.core.Term]]) -> hydra.coq.syntax.Document:
    r"""Build a Coq Document from lists of type definitions and term definitions."""

    @lru_cache(1)
    def types_sentences() -> frozenlist[hydra.coq.syntax.Sentence]:
        return hydra.lib.lists.map((lambda td: encode_type_definition_pair(env, td)), type_defs)
    @lru_cache(1)
    def terms_sentences() -> frozenlist[hydra.coq.syntax.Sentence]:
        return hydra.lib.lists.map((lambda ed: encode_term_definition_pair(env, ed)), term_defs)
    return hydra.coq.syntax.Document(hydra.lib.lists.concat(((standard_imports(),), types_sentences(), terms_sentences())))

def strip_lambdas(tm: hydra.core.Term) -> hydra.core.Term:
    r"""Peel off leading lambdas and annotations, returning the first non-lambda body."""

    while True:
        match tm:
            case hydra.core.TermAnnotated(value=at):
                tm = at.body
                continue

            case hydra.core.TermLambda(value=lam):
                tm = lam.body
                continue

            case _:
                return tm
