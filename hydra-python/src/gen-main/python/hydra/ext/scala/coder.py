# Note: this is an automatically generated file. Do not edit.

r"""Scala code generator: converts Hydra modules to Scala source code."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.annotations
import hydra.coder_utils
import hydra.context
import hydra.core
import hydra.errors
import hydra.ext.scala.serde
import hydra.ext.scala.syntax
import hydra.ext.scala.utils
import hydra.formatting
import hydra.graph
import hydra.inference
import hydra.lib.eithers
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
import hydra.module
import hydra.names
import hydra.rewriting
import hydra.schemas
import hydra.serialization
import hydra.typing
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def apply_var(fterm: hydra.core.Term, avar: hydra.core.Name):
    r"""Apply a variable to a term, performing substitution for lambdas."""

    v = avar.value
    def _hoist_v_body_1(v1):
        match v1:
            case hydra.core.FunctionLambda(value=lam):
                lam_param = lam.parameter
                lam_body = lam.body
                return hydra.lib.logic.if_else(hydra.rewriting.is_free_variable_in_term(lam_param, lam_body), (lambda : lam_body), (lambda : hydra.rewriting.substitute_variable(lam_param, avar, lam_body)))

            case _:
                return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fterm, cast(hydra.core.Term, hydra.core.TermVariable(avar)))))
    match hydra.rewriting.deannotate_and_detype_term(fterm):
        case hydra.core.TermFunction(value=f):
            return _hoist_v_body_1(f)

        case _:
            return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fterm, cast(hydra.core.Term, hydra.core.TermVariable(avar)))))

def drop_domains(n: int, t: hydra.core.Type):
    def _hoist_hydra_ext_scala_coder_drop_domains_1(n, t, v1):
        match v1:
            case hydra.core.TypeFunction(value=ft):
                return drop_domains(hydra.lib.math.sub(n, 1), ft.codomain)

            case hydra.core.TypeForall(value=fa):
                return drop_domains(n, fa.body)

            case _:
                return t
    return hydra.lib.logic.if_else(hydra.lib.equality.lte(n, 0), (lambda : t), (lambda : _hoist_hydra_ext_scala_coder_drop_domains_1(n, t, hydra.rewriting.deannotate_type(t))))

def encode_type(cx: hydra.context.Context, g: T0, t: hydra.core.Type):
    def _hoist_hydra_ext_scala_coder_encode_type_1(cx, v1):
        match v1:
            case hydra.core.FloatType.BIGFLOAT:
                return Right(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeRef(cast(hydra.ext.scala.syntax.Type_Ref, hydra.ext.scala.syntax.Type_RefName(hydra.ext.scala.syntax.Type_Name("BigDecimal"))))))

            case hydra.core.FloatType.FLOAT32:
                return Right(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeRef(cast(hydra.ext.scala.syntax.Type_Ref, hydra.ext.scala.syntax.Type_RefName(hydra.ext.scala.syntax.Type_Name("Float"))))))

            case hydra.core.FloatType.FLOAT64:
                return Right(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeRef(cast(hydra.ext.scala.syntax.Type_Ref, hydra.ext.scala.syntax.Type_RefName(hydra.ext.scala.syntax.Type_Name("Double"))))))

            case _:
                return Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unsupported float type"))), cx))
    def _hoist_hydra_ext_scala_coder_encode_type_2(cx, v1):
        match v1:
            case hydra.core.IntegerType.BIGINT:
                return Right(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeRef(cast(hydra.ext.scala.syntax.Type_Ref, hydra.ext.scala.syntax.Type_RefName(hydra.ext.scala.syntax.Type_Name("BigInt"))))))

            case hydra.core.IntegerType.INT8:
                return Right(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeRef(cast(hydra.ext.scala.syntax.Type_Ref, hydra.ext.scala.syntax.Type_RefName(hydra.ext.scala.syntax.Type_Name("Byte"))))))

            case hydra.core.IntegerType.INT16:
                return Right(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeRef(cast(hydra.ext.scala.syntax.Type_Ref, hydra.ext.scala.syntax.Type_RefName(hydra.ext.scala.syntax.Type_Name("Short"))))))

            case hydra.core.IntegerType.INT32:
                return Right(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeRef(cast(hydra.ext.scala.syntax.Type_Ref, hydra.ext.scala.syntax.Type_RefName(hydra.ext.scala.syntax.Type_Name("Int"))))))

            case hydra.core.IntegerType.INT64:
                return Right(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeRef(cast(hydra.ext.scala.syntax.Type_Ref, hydra.ext.scala.syntax.Type_RefName(hydra.ext.scala.syntax.Type_Name("Long"))))))

            case hydra.core.IntegerType.UINT8:
                return Right(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeRef(cast(hydra.ext.scala.syntax.Type_Ref, hydra.ext.scala.syntax.Type_RefName(hydra.ext.scala.syntax.Type_Name("Byte"))))))

            case hydra.core.IntegerType.UINT16:
                return Right(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeRef(cast(hydra.ext.scala.syntax.Type_Ref, hydra.ext.scala.syntax.Type_RefName(hydra.ext.scala.syntax.Type_Name("Int"))))))

            case hydra.core.IntegerType.UINT32:
                return Right(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeRef(cast(hydra.ext.scala.syntax.Type_Ref, hydra.ext.scala.syntax.Type_RefName(hydra.ext.scala.syntax.Type_Name("Long"))))))

            case hydra.core.IntegerType.UINT64:
                return Right(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeRef(cast(hydra.ext.scala.syntax.Type_Ref, hydra.ext.scala.syntax.Type_RefName(hydra.ext.scala.syntax.Type_Name("BigInt"))))))

            case _:
                return Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unsupported integer type"))), cx))
    def _hoist_hydra_ext_scala_coder_encode_type_3(cx, v1):
        match v1:
            case hydra.core.LiteralTypeBinary():
                return Right(hydra.ext.scala.utils.stapply(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeRef(cast(hydra.ext.scala.syntax.Type_Ref, hydra.ext.scala.syntax.Type_RefName(hydra.ext.scala.syntax.Type_Name("Array"))))), (cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeRef(cast(hydra.ext.scala.syntax.Type_Ref, hydra.ext.scala.syntax.Type_RefName(hydra.ext.scala.syntax.Type_Name("Byte"))))),)))

            case hydra.core.LiteralTypeBoolean():
                return Right(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeRef(cast(hydra.ext.scala.syntax.Type_Ref, hydra.ext.scala.syntax.Type_RefName(hydra.ext.scala.syntax.Type_Name("Boolean"))))))

            case hydra.core.LiteralTypeFloat(value=ft):
                return _hoist_hydra_ext_scala_coder_encode_type_1(cx, ft)

            case hydra.core.LiteralTypeInteger(value=it):
                return _hoist_hydra_ext_scala_coder_encode_type_2(cx, it)

            case hydra.core.LiteralTypeString():
                return Right(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeRef(cast(hydra.ext.scala.syntax.Type_Ref, hydra.ext.scala.syntax.Type_RefName(hydra.ext.scala.syntax.Type_Name("scala.Predef.String"))))))

            case _:
                return Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unsupported literal type"))), cx))
    match hydra.rewriting.deannotate_type(t):
        case hydra.core.TypeApplication(value=at):
            def collect_type_args(t2: hydra.core.Type, acc: frozenlist[hydra.core.Type]):
                def _hoist_collect_type_args_1(acc, t2, v1):
                    match v1:
                        case hydra.core.TypeApplication(value=at2):
                            f2 = at2.function
                            a2 = at2.argument
                            return collect_type_args(f2, hydra.lib.lists.cons(a2, acc))

                        case _:
                            return (t2, acc)
                return _hoist_collect_type_args_1(acc, t2, hydra.rewriting.deannotate_type(t2))
            @lru_cache(1)
            def collected() -> tuple[hydra.core.Type, frozenlist[hydra.core.Type]]:
                return collect_type_args(cast(hydra.core.Type, hydra.core.TypeApplication(at)), ())
            @lru_cache(1)
            def base_fun() -> hydra.core.Type:
                return hydra.lib.pairs.first(collected())
            @lru_cache(1)
            def all_args() -> frozenlist[hydra.core.Type]:
                return hydra.lib.pairs.second(collected())
            return hydra.lib.eithers.bind(encode_type(cx, g, base_fun()), (lambda sfun: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda a: encode_type(cx, g, a)), all_args()), (lambda sargs: Right(hydra.ext.scala.utils.stapply(sfun, sargs))))))

        case hydra.core.TypeUnit():
            return Right(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeRef(cast(hydra.ext.scala.syntax.Type_Ref, hydra.ext.scala.syntax.Type_RefName(hydra.ext.scala.syntax.Type_Name("Unit"))))))

        case hydra.core.TypeEither(value=et):
            lt = et.left
            rt = et.right
            return hydra.lib.eithers.bind(encode_type(cx, g, lt), (lambda slt: hydra.lib.eithers.bind(encode_type(cx, g, rt), (lambda srt: Right(hydra.ext.scala.utils.stapply2(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeRef(cast(hydra.ext.scala.syntax.Type_Ref, hydra.ext.scala.syntax.Type_RefName(hydra.ext.scala.syntax.Type_Name("Either"))))), slt, srt))))))

        case hydra.core.TypeFunction(value=ft):
            dom = ft.domain
            cod = ft.codomain
            return hydra.lib.eithers.bind(encode_type(cx, g, dom), (lambda sdom: hydra.lib.eithers.bind(encode_type(cx, g, cod), (lambda scod: Right(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeFunctionType(cast(hydra.ext.scala.syntax.Type_FunctionType, hydra.ext.scala.syntax.Type_FunctionTypeFunction(hydra.ext.scala.syntax.Type_Function((sdom,), scod))))))))))

        case hydra.core.TypeList(value=lt):
            return hydra.lib.eithers.bind(encode_type(cx, g, lt), (lambda slt: Right(hydra.ext.scala.utils.stapply1(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeRef(cast(hydra.ext.scala.syntax.Type_Ref, hydra.ext.scala.syntax.Type_RefName(hydra.ext.scala.syntax.Type_Name("Seq"))))), slt))))

        case hydra.core.TypeLiteral(value=lt2):
            return _hoist_hydra_ext_scala_coder_encode_type_3(cx, lt2)

        case hydra.core.TypeMap(value=mt):
            kt = mt.keys
            vt = mt.values
            return hydra.lib.eithers.bind(encode_type(cx, g, kt), (lambda skt: hydra.lib.eithers.bind(encode_type(cx, g, vt), (lambda svt: Right(hydra.ext.scala.utils.stapply2(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeRef(cast(hydra.ext.scala.syntax.Type_Ref, hydra.ext.scala.syntax.Type_RefName(hydra.ext.scala.syntax.Type_Name("Map"))))), skt, svt))))))

        case hydra.core.TypeMaybe(value=ot):
            return hydra.lib.eithers.bind(encode_type(cx, g, ot), (lambda sot: Right(hydra.ext.scala.utils.stapply1(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeRef(cast(hydra.ext.scala.syntax.Type_Ref, hydra.ext.scala.syntax.Type_RefName(hydra.ext.scala.syntax.Type_Name("Option"))))), sot))))

        case hydra.core.TypePair(value=pt):
            ft = pt.first
            st = pt.second
            return hydra.lib.eithers.bind(encode_type(cx, g, ft), (lambda sft: hydra.lib.eithers.bind(encode_type(cx, g, st), (lambda sst: Right(hydra.ext.scala.utils.stapply2(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeRef(cast(hydra.ext.scala.syntax.Type_Ref, hydra.ext.scala.syntax.Type_RefName(hydra.ext.scala.syntax.Type_Name("Tuple2"))))), sft, sst))))))

        case hydra.core.TypeRecord():
            return Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unexpected anonymous record type"))), cx))

        case hydra.core.TypeSet(value=st):
            return hydra.lib.eithers.bind(encode_type(cx, g, st), (lambda sst: Right(hydra.ext.scala.utils.stapply1(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeRef(cast(hydra.ext.scala.syntax.Type_Ref, hydra.ext.scala.syntax.Type_RefName(hydra.ext.scala.syntax.Type_Name("scala.collection.immutable.Set"))))), sst))))

        case hydra.core.TypeUnion():
            return Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unexpected anonymous union type"))), cx))

        case hydra.core.TypeWrap():
            return Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unexpected anonymous wrap type"))), cx))

        case hydra.core.TypeForall(value=ft2):
            v = ft2.parameter
            body = ft2.body
            return hydra.lib.eithers.bind(encode_type(cx, g, body), (lambda sbody: Right(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeLambda(hydra.ext.scala.syntax.Type_Lambda((hydra.ext.scala.utils.stparam(v),), sbody))))))

        case hydra.core.TypeVariable(value=v):
            raw_name = v.value
            @lru_cache(1)
            def type_name() -> str:
                return hydra.lib.logic.if_else(hydra.lib.lists.elem(46, hydra.lib.strings.to_list(raw_name)), (lambda : raw_name), (lambda : hydra.formatting.capitalize(raw_name)))
            return Right(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeVar(hydra.ext.scala.syntax.Type_Var(hydra.ext.scala.syntax.Type_Name(type_name())))))

        case _:
            return Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unsupported type"))), cx))

def find_sdom(cx: hydra.context.Context, g: hydra.graph.Graph, meta: FrozenDict[hydra.core.Name, hydra.core.Term]):
    def _hoist_hydra_ext_scala_coder_find_sdom_1(cx, g, v1):
        match v1:
            case hydra.core.TypeFunction(value=ft2):
                dom2 = ft2.domain
                return hydra.lib.eithers.bind(encode_type(cx, g, dom2), (lambda sdom2: Right(Just(sdom2))))

            case _:
                return Right(Nothing())
    def _hoist_hydra_ext_scala_coder_find_sdom_2(cx, g, t, v1):
        match v1:
            case hydra.core.TypeFunction(value=ft):
                dom = ft.domain
                return hydra.lib.eithers.bind(encode_type(cx, g, dom), (lambda sdom: Right(Just(sdom))))

            case hydra.core.TypeForall(value=fa):
                return _hoist_hydra_ext_scala_coder_find_sdom_1(cx, g, hydra.rewriting.deannotate_type(fa.body))

            case _:
                return hydra.lib.eithers.bind(encode_type(cx, g, t), (lambda st: Right(Just(st))))
    return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _de: hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_de.value))), cx)), (lambda _a: _a), hydra.annotations.get_type(g, meta)), (lambda mtyp: hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda t: _hoist_hydra_ext_scala_coder_find_sdom_2(cx, g, t, hydra.rewriting.deannotate_type(t))), mtyp)))

def encode_literal(cx: hydra.context.Context, g: T0, av: hydra.core.Literal):
    def _hoist_hydra_ext_scala_coder_encode_literal_1(cx, v1):
        match v1:
            case hydra.core.FloatValueBigfloat(value=bf):
                return Right(cast(hydra.ext.scala.syntax.Lit, hydra.ext.scala.syntax.LitDouble(hydra.lib.literals.bigfloat_to_float64(bf))))

            case hydra.core.FloatValueFloat32(value=f):
                return Right(cast(hydra.ext.scala.syntax.Lit, hydra.ext.scala.syntax.LitFloat(f)))

            case hydra.core.FloatValueFloat64(value=f):
                return Right(cast(hydra.ext.scala.syntax.Lit, hydra.ext.scala.syntax.LitDouble(f)))

            case _:
                return Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unexpected float value"))), cx))
    def _hoist_hydra_ext_scala_coder_encode_literal_2(cx, v1):
        match v1:
            case hydra.core.IntegerValueBigint(value=i):
                return Right(cast(hydra.ext.scala.syntax.Lit, hydra.ext.scala.syntax.LitLong(hydra.lib.literals.bigint_to_int64(i))))

            case hydra.core.IntegerValueInt8(value=i):
                return Right(cast(hydra.ext.scala.syntax.Lit, hydra.ext.scala.syntax.LitByte(i)))

            case hydra.core.IntegerValueInt16(value=i):
                return Right(cast(hydra.ext.scala.syntax.Lit, hydra.ext.scala.syntax.LitShort(i)))

            case hydra.core.IntegerValueInt32(value=i):
                return Right(cast(hydra.ext.scala.syntax.Lit, hydra.ext.scala.syntax.LitInt(i)))

            case hydra.core.IntegerValueInt64(value=i):
                return Right(cast(hydra.ext.scala.syntax.Lit, hydra.ext.scala.syntax.LitLong(i)))

            case hydra.core.IntegerValueUint8(value=i):
                return Right(cast(hydra.ext.scala.syntax.Lit, hydra.ext.scala.syntax.LitByte(hydra.lib.literals.bigint_to_int8(hydra.lib.literals.uint8_to_bigint(i)))))

            case hydra.core.IntegerValueUint16(value=i):
                return Right(cast(hydra.ext.scala.syntax.Lit, hydra.ext.scala.syntax.LitInt(hydra.lib.literals.bigint_to_int32(hydra.lib.literals.uint16_to_bigint(i)))))

            case hydra.core.IntegerValueUint32(value=i):
                return Right(cast(hydra.ext.scala.syntax.Lit, hydra.ext.scala.syntax.LitLong(hydra.lib.literals.bigint_to_int64(hydra.lib.literals.uint32_to_bigint(i)))))

            case hydra.core.IntegerValueUint64(value=i):
                return Right(cast(hydra.ext.scala.syntax.Lit, hydra.ext.scala.syntax.LitLong(hydra.lib.literals.bigint_to_int64(hydra.lib.literals.uint64_to_bigint(i)))))

            case _:
                return Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unexpected integer value"))), cx))
    match av:
        case hydra.core.LiteralBinary():
            return Right(cast(hydra.ext.scala.syntax.Lit, hydra.ext.scala.syntax.LitString("<binary>")))

        case hydra.core.LiteralBoolean(value=b):
            return Right(cast(hydra.ext.scala.syntax.Lit, hydra.ext.scala.syntax.LitBoolean(b)))

        case hydra.core.LiteralFloat(value=fv):
            return _hoist_hydra_ext_scala_coder_encode_literal_1(cx, fv)

        case hydra.core.LiteralInteger(value=iv):
            return _hoist_hydra_ext_scala_coder_encode_literal_2(cx, iv)

        case hydra.core.LiteralString(value=s):
            return Right(cast(hydra.ext.scala.syntax.Lit, hydra.ext.scala.syntax.LitString(s)))

        case _:
            return Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unexpected literal"))), cx))

def strip_wrap_eliminations(t: hydra.core.Term):
    r"""Strip wrap eliminations from terms (newtypes are erased in Scala)."""

    match hydra.rewriting.deannotate_and_detype_term(t):
        case hydra.core.TermApplication(value=app):
            app_fun = app.function
            app_arg = app.argument
            def _hoist_app_fun_body_1(v1):
                match v1:
                    case hydra.core.EliminationWrap():
                        return strip_wrap_eliminations(app_arg)

                    case _:
                        return t
            def _hoist_app_fun_body_2(v1):
                match v1:
                    case hydra.core.FunctionElimination(value=e):
                        return _hoist_app_fun_body_1(e)

                    case _:
                        return t
            def _hoist_app_fun_body_3(v1):
                match v1:
                    case hydra.core.TermFunction(value=f):
                        return _hoist_app_fun_body_2(f)

                    case hydra.core.TermApplication(value=inner_app):
                        inner_fun = inner_app.function
                        inner_arg = inner_app.argument
                        def _hoist_inner_fun_body_1(v12):
                            match v12:
                                case hydra.core.EliminationWrap():
                                    return strip_wrap_eliminations(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(inner_arg, app_arg))))

                                case _:
                                    return t
                        def _hoist_inner_fun_body_2(v12):
                            match v12:
                                case hydra.core.FunctionElimination(value=inner_e):
                                    return _hoist_inner_fun_body_1(inner_e)

                                case _:
                                    return t
                        def _hoist_inner_fun_body_3(v12):
                            match v12:
                                case hydra.core.TermFunction(value=inner_f):
                                    return _hoist_inner_fun_body_2(inner_f)

                                case _:
                                    return t
                        return _hoist_inner_fun_body_3(hydra.rewriting.deannotate_and_detype_term(inner_fun))

                    case _:
                        return t
            return _hoist_app_fun_body_3(hydra.rewriting.deannotate_and_detype_term(app_fun))

        case _:
            return t

def encode_typed_param(cx: hydra.context.Context, g: T0, pair: tuple[hydra.core.Name, hydra.core.Type]) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data_Param]:
    r"""Encode a parameter with its type annotation."""

    @lru_cache(1)
    def pname() -> str:
        return hydra.ext.scala.utils.scala_escape_name(hydra.names.local_name_of(hydra.lib.pairs.first(pair)))
    @lru_cache(1)
    def pdom() -> hydra.core.Type:
        return hydra.lib.pairs.second(pair)
    return hydra.lib.eithers.bind(encode_type(cx, g, pdom()), (lambda sdom: Right(hydra.ext.scala.syntax.Data_Param((), cast(hydra.ext.scala.syntax.Name, hydra.ext.scala.syntax.NameValue(pname())), Just(sdom), Nothing()))))

def extract_body(t: hydra.core.Term):
    while True:
        def _hoist_hydra_ext_scala_coder_extract_body_1(t, v1):
            match v1:
                case hydra.core.FunctionLambda(value=lam):
                    return extract_body(lam.body)

                case _:
                    return t
        match hydra.rewriting.deannotate_and_detype_term(t):
            case hydra.core.TermFunction(value=f):
                return _hoist_hydra_ext_scala_coder_extract_body_1(t, f)

            case hydra.core.TermTypeLambda(value=tl):
                t = tl.body
                continue

            case hydra.core.TermTypeApplication(value=ta):
                t = ta.body
                continue

            case hydra.core.TermLet(value=lt):
                t = lt.body
                continue

            case _:
                return t

def extract_domains(t: hydra.core.Type) -> frozenlist[hydra.core.Type]:
    r"""Extract domain types from a function type."""

    match hydra.rewriting.deannotate_type(t):
        case hydra.core.TypeFunction(value=ft):
            return hydra.lib.lists.cons(ft.domain, extract_domains(ft.codomain))

        case hydra.core.TypeForall(value=fa):
            return extract_domains(fa.body)

        case _:
            return ()

def extract_let_bindings(t: hydra.core.Term):
    def _hoist_hydra_ext_scala_coder_extract_let_bindings_1(v1):
        match v1:
            case hydra.core.FunctionLambda(value=lam):
                return extract_let_bindings(lam.body)

            case _:
                return ()
    match hydra.rewriting.deannotate_and_detype_term(t):
        case hydra.core.TermFunction(value=f):
            return _hoist_hydra_ext_scala_coder_extract_let_bindings_1(f)

        case hydra.core.TermTypeLambda(value=tl):
            return extract_let_bindings(tl.body)

        case hydra.core.TermTypeApplication(value=ta):
            return extract_let_bindings(ta.body)

        case hydra.core.TermLet(value=lt):
            return hydra.lib.lists.concat2(lt.bindings, extract_let_bindings(lt.body))

        case _:
            return ()

def extract_params(t: hydra.core.Term):
    while True:
        def _hoist_hydra_ext_scala_coder_extract_params_1(v1):
            match v1:
                case hydra.core.FunctionLambda(value=lam):
                    return hydra.lib.lists.cons(lam.parameter, extract_params(lam.body))

                case _:
                    return ()
        match hydra.rewriting.deannotate_and_detype_term(t):
            case hydra.core.TermFunction(value=f):
                return _hoist_hydra_ext_scala_coder_extract_params_1(f)

            case hydra.core.TermTypeLambda(value=tl):
                t = tl.body
                continue

            case hydra.core.TermTypeApplication(value=ta):
                t = ta.body
                continue

            case hydra.core.TermLet(value=lt):
                t = lt.body
                continue

            case _:
                return ()

def encode_case(cx: hydra.context.Context, g: hydra.graph.Graph, ftypes: FrozenDict[hydra.core.Name, hydra.core.Type], sn: Maybe[hydra.core.Name], f: hydra.core.Field) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Case]:
    r"""Encode a case branch."""

    fname = f.name
    fterm = f.term
    @lru_cache(1)
    def is_unit():
        def _hoist_is_unit_1(v1):
            match v1:
                case hydra.core.FunctionLambda(value=lam):
                    lam_param = lam.parameter
                    lam_body = lam.body
                    @lru_cache(1)
                    def dom_is_unit() -> bool:
                        return hydra.lib.maybes.maybe((lambda : False), (lambda dom: hydra.lib.equality.equal(dom, cast(hydra.core.Type, hydra.core.TypeUnit()))), lam.domain)
                    @lru_cache(1)
                    def body_ignores_param() -> bool:
                        return hydra.rewriting.is_free_variable_in_term(lam_param, lam_body)
                    return hydra.lib.logic.or_(dom_is_unit(), body_ignores_param())

                case _:
                    return False
        def _hoist_is_unit_2(v1):
            match v1:
                case hydra.core.TermFunction(value=fn):
                    return _hoist_is_unit_1(fn)

                case hydra.core.TermRecord(value=r):
                    return hydra.lib.equality.equal(hydra.lib.lists.length(r.fields), 0)

                case hydra.core.TermUnit():
                    return True

                case _:
                    return False
        def _hoist_is_unit_3(v1):
            match v1:
                case hydra.core.TypeUnit():
                    return True

                case hydra.core.TypeRecord(value=rt):
                    return hydra.lib.equality.equal(hydra.lib.lists.length(rt), 0)

                case _:
                    return False
        return hydra.lib.maybes.maybe((lambda : _hoist_is_unit_2(hydra.rewriting.deannotate_and_detype_term(fterm))), (lambda dom: _hoist_is_unit_3(hydra.rewriting.deannotate_type(dom))), hydra.lib.maps.lookup(fname, ftypes))
    @lru_cache(1)
    def short_type_name() -> str:
        return hydra.lib.lists.last(hydra.lib.strings.split_on(".", hydra.lib.maybes.maybe((lambda : "x"), (lambda n: n.value), sn)))
    @lru_cache(1)
    def lam_param_suffix():
        def _hoist_lam_param_suffix_1(v1):
            match v1:
                case hydra.core.FunctionLambda(value=lam):
                    raw_name = lam.parameter.value
                    @lru_cache(1)
                    def safe_name() -> str:
                        return hydra.lib.strings.from_list(hydra.lib.lists.map((lambda c: hydra.lib.logic.if_else(hydra.lib.equality.equal(c, 39), (lambda : 95), (lambda : c))), hydra.lib.strings.to_list(raw_name)))
                    return hydra.lib.strings.cat2("_", safe_name())

                case _:
                    return ""
        match hydra.rewriting.deannotate_and_detype_term(fterm):
            case hydra.core.TermFunction(value=fn):
                return _hoist_lam_param_suffix_1(fn)

            case _:
                return ""
    @lru_cache(1)
    def v() -> hydra.core.Name:
        return hydra.core.Name(hydra.lib.strings.cat(("v_", short_type_name(), "_", fname.value, lam_param_suffix())))
    @lru_cache(1)
    def pat_args() -> frozenlist[hydra.ext.scala.syntax.Pat]:
        return hydra.lib.logic.if_else(is_unit(), (lambda : ()), (lambda : (hydra.ext.scala.utils.svar(v()),)))
    @lru_cache(1)
    def pat() -> hydra.ext.scala.syntax.Pat:
        return cast(hydra.ext.scala.syntax.Pat, hydra.ext.scala.syntax.PatExtract(hydra.ext.scala.syntax.Pat_Extract(hydra.ext.scala.utils.sname(hydra.ext.scala.utils.qualify_union_field_name("MATCHED.", sn, fname)), pat_args())))
    @lru_cache(1)
    def applied() -> hydra.core.Term:
        return apply_var(fterm, v())
    return hydra.lib.eithers.bind(encode_term(cx, g, applied()), (lambda body: Right(hydra.ext.scala.syntax.Case(pat(), Nothing(), body))))

def encode_function(cx: hydra.context.Context, g: hydra.graph.Graph, meta: FrozenDict[hydra.core.Name, hydra.core.Term], fun: hydra.core.Function, arg: Maybe[hydra.core.Term]):
    def _hoist_hydra_ext_scala_coder_encode_function_1(arg, cx, g, meta, v1):
        match v1:
            case hydra.core.EliminationWrap():
                return hydra.lib.maybes.maybe((lambda : hydra.lib.eithers.bind(find_sdom(cx, g, meta), (lambda sdom: Right(hydra.ext.scala.utils.slambda("x", hydra.ext.scala.utils.sname("x"), sdom))))), (lambda a: encode_term(cx, g, a)), arg)

            case hydra.core.EliminationRecord(value=proj):
                @lru_cache(1)
                def fname() -> str:
                    return hydra.ext.scala.utils.scala_escape_name(proj.field.value)
                type_name = proj.type_name
                pv = "x"
                return hydra.lib.maybes.maybe((lambda : hydra.lib.eithers.bind(hydra.lib.eithers.either((lambda _: hydra.lib.eithers.bind(encode_type(cx, g, cast(hydra.core.Type, hydra.core.TypeVariable(type_name))), (lambda st: Right(Just(st))))), (lambda msdom: hydra.lib.maybes.maybe((lambda : hydra.lib.eithers.bind(encode_type(cx, g, cast(hydra.core.Type, hydra.core.TypeVariable(type_name))), (lambda st: Right(Just(st))))), (lambda sdom: Right(Just(sdom))), msdom)), find_sdom(cx, g, meta)), (lambda msdom: Right(hydra.ext.scala.utils.slambda(pv, cast(hydra.ext.scala.syntax.Data, hydra.ext.scala.syntax.DataRef(cast(hydra.ext.scala.syntax.Data_Ref, hydra.ext.scala.syntax.Data_RefSelect(hydra.ext.scala.syntax.Data_Select(hydra.ext.scala.utils.sname(pv), hydra.ext.scala.syntax.Data_Name(hydra.ext.scala.syntax.PredefString(fname()))))))), msdom))))), (lambda a: hydra.lib.eithers.bind(encode_term(cx, g, a), (lambda sa: Right(cast(hydra.ext.scala.syntax.Data, hydra.ext.scala.syntax.DataRef(cast(hydra.ext.scala.syntax.Data_Ref, hydra.ext.scala.syntax.Data_RefSelect(hydra.ext.scala.syntax.Data_Select(sa, hydra.ext.scala.syntax.Data_Name(hydra.ext.scala.syntax.PredefString(fname()))))))))))), arg)

            case hydra.core.EliminationUnion(value=cs):
                v = "v"
                tname = cs.type_name
                dom = cast(hydra.core.Type, hydra.core.TypeVariable(tname))
                @lru_cache(1)
                def sn() -> Maybe[hydra.core.Name]:
                    return hydra.ext.scala.utils.name_of_type(g, dom)
                cases = cs.cases
                dflt = cs.default
                @lru_cache(1)
                def ftypes() -> FrozenDict[hydra.core.Name, hydra.core.Type]:
                    return hydra.lib.eithers.either((lambda _: hydra.lib.maps.empty()), (lambda x_: x_), hydra.schemas.field_types(cx, g, dom))
                return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda f: encode_case(cx, g, ftypes(), sn(), f)), cases), (lambda field_cases: hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : Right(field_cases)), (lambda dflt_term: hydra.lib.eithers.bind(encode_term(cx, g, dflt_term), (lambda sdflt: Right(hydra.lib.lists.concat2(field_cases, (hydra.ext.scala.syntax.Case(cast(hydra.ext.scala.syntax.Pat, hydra.ext.scala.syntax.PatWildcard()), Nothing(), sdflt),)))))), dflt), (lambda scases: hydra.lib.maybes.maybe((lambda : hydra.lib.eithers.bind(find_sdom(cx, g, meta), (lambda sdom: Right(hydra.ext.scala.utils.slambda(v, cast(hydra.ext.scala.syntax.Data, hydra.ext.scala.syntax.DataMatch(hydra.ext.scala.syntax.Data_Match(hydra.ext.scala.utils.sname(v), scases))), sdom))))), (lambda a: hydra.lib.eithers.bind(encode_term(cx, g, a), (lambda sa: Right(cast(hydra.ext.scala.syntax.Data, hydra.ext.scala.syntax.DataMatch(hydra.ext.scala.syntax.Data_Match(sa, scases))))))), arg)))))

            case _:
                return Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unsupported elimination"))), cx))
    match fun:
        case hydra.core.FunctionLambda(value=lam):
            param = lam.parameter
            @lru_cache(1)
            def v() -> str:
                return hydra.ext.scala.utils.scala_escape_name(param.value)
            body = lam.body
            raw_mdom = lam.domain
            @lru_cache(1)
            def mdom() -> Maybe[hydra.core.Type]:
                return hydra.lib.maybes.bind(raw_mdom, (lambda dom: (free_vars := hydra.rewriting.free_variables_in_type(dom), unqualified_free_vars := hydra.lib.sets.from_list(hydra.lib.lists.filter((lambda n: hydra.lib.logic.not_(hydra.lib.lists.elem(46, hydra.lib.strings.to_list(n.value)))), hydra.lib.sets.to_list(free_vars))), unresolved_vars := hydra.lib.sets.difference(unqualified_free_vars, g.type_variables), hydra.lib.logic.if_else(hydra.lib.sets.null(unresolved_vars), (lambda : Just(dom)), (lambda : Nothing())))[3]))
            return hydra.lib.eithers.bind(encode_term(cx, g, body), (lambda sbody: hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : find_sdom(cx, g, meta)), (lambda dom: hydra.lib.eithers.bind(encode_type(cx, g, dom), (lambda sdom: Right(Just(sdom))))), mdom()), (lambda sdom: Right(hydra.ext.scala.utils.slambda(v(), sbody, sdom))))))

        case hydra.core.FunctionPrimitive(value=name):
            return Right(hydra.ext.scala.utils.sprim(name))

        case hydra.core.FunctionElimination(value=e):
            return _hoist_hydra_ext_scala_coder_encode_function_1(arg, cx, g, meta, e)

        case _:
            return Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unsupported function"))), cx))

def encode_let_binding(cx: hydra.context.Context, g: hydra.graph.Graph, outer_type_vars: frozenset[hydra.core.Name], b: hydra.core.Binding) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Stat]:
    r"""Encode a let binding as a val or def declaration. outerTypeVars are type params from the enclosing scope."""

    @lru_cache(1)
    def bname() -> str:
        return hydra.ext.scala.utils.scala_escape_name(b.name.value)
    bterm = b.term
    @lru_cache(1)
    def mts() -> Maybe[hydra.core.TypeScheme]:
        return hydra.lib.maybes.maybe((lambda : hydra.lib.maps.lookup(b.name, g.bound_types)), (lambda ts: Just(ts)), b.type)
    @lru_cache(1)
    def is_fn():
        def _hoist_is_fn_1(v1):
            match v1:
                case hydra.core.TypeFunction():
                    return True

                case _:
                    return False
        def _hoist_is_fn_2(v1):
            match v1:
                case hydra.core.TypeFunction():
                    return True

                case hydra.core.TypeForall(value=fa):
                    return _hoist_is_fn_1(hydra.rewriting.deannotate_type(fa.body))

                case _:
                    return False
        return hydra.lib.maybes.maybe((lambda : False), (lambda ts: _hoist_is_fn_2(hydra.rewriting.deannotate_type(ts.type))), mts())
    return hydra.lib.maybes.maybe((lambda : hydra.lib.eithers.bind(encode_term(cx, g, bterm), (lambda srhs: Right(cast(hydra.ext.scala.syntax.Stat, hydra.ext.scala.syntax.StatDefn(cast(hydra.ext.scala.syntax.Defn, hydra.ext.scala.syntax.DefnVal(hydra.ext.scala.syntax.Defn_Val((cast(hydra.ext.scala.syntax.Mod, hydra.ext.scala.syntax.ModLazy()),), (cast(hydra.ext.scala.syntax.Pat, hydra.ext.scala.syntax.PatVar(hydra.ext.scala.syntax.Pat_Var(hydra.ext.scala.syntax.Data_Name(hydra.ext.scala.syntax.PredefString(bname()))))),), Nothing(), srhs))))))))), (lambda ts: (new_vars := hydra.lib.lists.filter((lambda v: hydra.lib.logic.not_(hydra.lib.sets.member(v, outer_type_vars))), ts.variables), use_def := hydra.lib.logic.or_(is_fn(), hydra.lib.logic.not_(hydra.lib.lists.null(new_vars))), hydra.lib.logic.if_else(use_def, (lambda : encode_local_def(cx, g, outer_type_vars, bname(), bterm, ts.type)), (lambda : hydra.lib.eithers.bind(encode_term(cx, g, bterm), (lambda srhs: hydra.lib.eithers.bind(encode_type(cx, g, ts.type), (lambda styp: Right(cast(hydra.ext.scala.syntax.Stat, hydra.ext.scala.syntax.StatDefn(cast(hydra.ext.scala.syntax.Defn, hydra.ext.scala.syntax.DefnVal(hydra.ext.scala.syntax.Defn_Val((cast(hydra.ext.scala.syntax.Mod, hydra.ext.scala.syntax.ModLazy()),), (cast(hydra.ext.scala.syntax.Pat, hydra.ext.scala.syntax.PatVar(hydra.ext.scala.syntax.Pat_Var(hydra.ext.scala.syntax.Data_Name(hydra.ext.scala.syntax.PredefString(bname()))))),), Just(styp), srhs)))))))))))))[2]), mts())

def encode_local_def(cx: hydra.context.Context, g: hydra.graph.Graph, outer_type_vars: frozenset[hydra.core.Name], lname: str, term: hydra.core.Term, typ: hydra.core.Type) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Stat]:
    r"""Encode a local def. outerTypeVars are type params already in scope (don't redeclare them)."""

    @lru_cache(1)
    def free_type_vars() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.filter((lambda v: hydra.lib.logic.and_(hydra.lib.logic.not_(hydra.lib.lists.elem(46, hydra.lib.strings.to_list(v.value))), hydra.lib.logic.not_(hydra.lib.sets.member(v, outer_type_vars)))), hydra.lib.sets.to_list(hydra.rewriting.free_variables_in_type(typ)))
    @lru_cache(1)
    def doms() -> frozenlist[hydra.core.Type]:
        return extract_domains(typ)
    @lru_cache(1)
    def param_names() -> frozenlist[hydra.core.Name]:
        return extract_params(term)
    @lru_cache(1)
    def param_count() -> int:
        return hydra.lib.math.min(hydra.lib.lists.length(param_names()), hydra.lib.lists.length(doms()))
    @lru_cache(1)
    def cod() -> hydra.core.Type:
        return drop_domains(param_count(), typ)
    @lru_cache(1)
    def zipped_params() -> frozenlist[tuple[hydra.core.Name, hydra.core.Type]]:
        return hydra.lib.lists.zip(hydra.lib.lists.take(param_count(), param_names()), hydra.lib.lists.take(param_count(), doms()))
    @lru_cache(1)
    def let_bindings() -> frozenlist[hydra.core.Binding]:
        return extract_let_bindings(term)
    @lru_cache(1)
    def tparams() -> frozenlist[hydra.ext.scala.syntax.Type_Param]:
        return hydra.lib.lists.map((lambda tv: hydra.ext.scala.utils.stparam(tv)), free_type_vars())
    @lru_cache(1)
    def all_type_vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.union(outer_type_vars, hydra.lib.sets.from_list(free_type_vars()))
    @lru_cache(1)
    def g_with_type_vars() -> hydra.graph.Graph:
        return hydra.graph.Graph(g.bound_terms, g.bound_types, g.class_constraints, g.lambda_variables, g.metadata, g.primitives, g.schema_types, hydra.lib.sets.union(all_type_vars(), g.type_variables))
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_typed_param(cx, g_with_type_vars(), v1)), zipped_params()), (lambda sparams: hydra.lib.eithers.bind(encode_term(cx, g_with_type_vars(), extract_body(term)), (lambda sbody: hydra.lib.eithers.bind(encode_type(cx, g_with_type_vars(), cod()), (lambda scod: (g_for_lets := hydra.lib.logic.if_else(hydra.lib.lists.null(let_bindings()), (lambda : g_with_type_vars()), (lambda : hydra.rewriting.extend_graph_for_let((lambda x1, x2: hydra.coder_utils.binding_metadata(x1, x2)), g_with_type_vars(), hydra.core.Let(let_bindings(), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("dummy"))))))), hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_let_binding(cx, g_for_lets, all_type_vars(), v1)), let_bindings()), (lambda sbindings: (def_body := hydra.lib.logic.if_else(hydra.lib.lists.null(sbindings), (lambda : sbody), (lambda : cast(hydra.ext.scala.syntax.Data, hydra.ext.scala.syntax.DataBlock(hydra.ext.scala.syntax.Data_Block(hydra.lib.lists.concat2(sbindings, (cast(hydra.ext.scala.syntax.Stat, hydra.ext.scala.syntax.StatTerm(sbody)),))))))), Right(cast(hydra.ext.scala.syntax.Stat, hydra.ext.scala.syntax.StatDefn(cast(hydra.ext.scala.syntax.Defn, hydra.ext.scala.syntax.DefnDef(hydra.ext.scala.syntax.Defn_Def((), hydra.ext.scala.syntax.Data_Name(hydra.ext.scala.syntax.PredefString(lname)), tparams(), hydra.lib.lists.map((lambda p: (p,)), sparams), Just(scod), def_body)))))))[1])))[1]))))))

def encode_term(cx: hydra.context.Context, g: hydra.graph.Graph, term0: hydra.core.Term):
    r"""Encode a Hydra term as a Scala expression."""

    @lru_cache(1)
    def term() -> hydra.core.Term:
        return strip_wrap_eliminations(term0)
    match hydra.rewriting.deannotate_term(term()):
        case hydra.core.TermTypeApplication(value=ta):
            def collect_type_args(t: hydra.core.Term, acc: frozenlist[hydra.core.Type]):
                def _hoist_collect_type_args_1(acc, t, v1):
                    match v1:
                        case hydra.core.TermTypeApplication(value=ta2):
                            return collect_type_args(ta2.body, hydra.lib.lists.cons(ta2.type, acc))

                        case _:
                            return (acc, t)
                return _hoist_collect_type_args_1(acc, t, hydra.rewriting.deannotate_term(t))
            @lru_cache(1)
            def collected() -> tuple[frozenlist[hydra.core.Type], hydra.core.Term]:
                return collect_type_args(ta.body, (ta.type,))
            @lru_cache(1)
            def type_args() -> frozenlist[hydra.core.Type]:
                return hydra.lib.pairs.first(collected())
            @lru_cache(1)
            def inner_term() -> hydra.core.Term:
                return hydra.lib.pairs.second(collected())
            def collect_type_lambdas(t: hydra.core.Term, acc: frozenlist[hydra.core.Name]):
                def _hoist_collect_type_lambdas_1(acc, t, v1):
                    match v1:
                        case hydra.core.TermTypeLambda(value=tl):
                            return collect_type_lambdas(tl.body, hydra.lib.lists.cons(tl.parameter, acc))

                        case _:
                            return (acc, t)
                return _hoist_collect_type_lambdas_1(acc, t, hydra.rewriting.deannotate_term(t))
            @lru_cache(1)
            def tl_collected() -> tuple[frozenlist[hydra.core.Name], hydra.core.Term]:
                return collect_type_lambdas(inner_term(), ())
            @lru_cache(1)
            def type_params() -> frozenlist[hydra.core.Name]:
                return hydra.lib.pairs.first(tl_collected())
            @lru_cache(1)
            def body_after_type_lambdas() -> hydra.core.Term:
                return hydra.lib.pairs.second(tl_collected())
            substituted_body = body_after_type_lambdas()
            def _hoist_collect_type_args_body_1(v1):
                match v1:
                    case hydra.core.FunctionPrimitive(value=pname):
                        return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda targ: encode_type(cx, g, targ)), type_args()), (lambda stype_args: (in_scope_type_var_names := hydra.lib.sets.from_list(hydra.lib.lists.map((lambda n: hydra.formatting.capitalize(n.value)), hydra.lib.sets.to_list(g.type_variables))), has_forall_residual := (_hoist_has_forall_residual_1 := (lambda v12: (lambda tv: (tv_name := tv.name.value, hydra.lib.logic.and_(hydra.lib.logic.not_(hydra.lib.lists.elem(46, hydra.lib.strings.to_list(tv_name))), hydra.lib.logic.not_(hydra.lib.sets.member(tv_name, in_scope_type_var_names))))[1])(v12.value) if isinstance(v12, hydra.ext.scala.syntax.TypeVar) else False), hydra.lib.logic.not_(hydra.lib.lists.null(hydra.lib.lists.filter((lambda st: _hoist_has_forall_residual_1(st)), stype_args))))[1], hydra.lib.logic.if_else(has_forall_residual, (lambda : Right(hydra.ext.scala.utils.sprim(pname))), (lambda : Right(hydra.ext.scala.utils.sapply_types(hydra.ext.scala.utils.sprim(pname), stype_args)))))[2]))

                    case hydra.core.FunctionElimination():
                        return encode_term(cx, g, substituted_body)

                    case _:
                        return encode_term(cx, g, substituted_body)
            def _hoist_collect_type_args_body_2(v1):
                match v1:
                    case hydra.core.TermFunction(value=f):
                        return _hoist_collect_type_args_body_1(f)

                    case _:
                        return encode_term(cx, g, substituted_body)
            return _hoist_collect_type_args_body_2(hydra.rewriting.deannotate_term(substituted_body))

        case hydra.core.TermTypeLambda(value=tl):
            return encode_term(cx, hydra.rewriting.extend_graph_for_type_lambda(g, tl), tl.body)

        case hydra.core.TermApplication(value=app):
            fun = app.function
            arg = app.argument
            def _hoist_fun_body_1(f, v1):
                match v1:
                    case hydra.core.EliminationRecord(value=proj):
                        @lru_cache(1)
                        def fname() -> str:
                            return hydra.ext.scala.utils.scala_escape_name(proj.field.value)
                        return hydra.lib.eithers.bind(encode_term(cx, g, arg), (lambda sarg: Right(cast(hydra.ext.scala.syntax.Data, hydra.ext.scala.syntax.DataRef(cast(hydra.ext.scala.syntax.Data_Ref, hydra.ext.scala.syntax.Data_RefSelect(hydra.ext.scala.syntax.Data_Select(sarg, hydra.ext.scala.syntax.Data_Name(hydra.ext.scala.syntax.PredefString(fname()))))))))))

                    case hydra.core.EliminationUnion():
                        return encode_function(cx, g, hydra.annotations.term_annotation_internal(fun), f, Just(arg))

                    case _:
                        return hydra.lib.eithers.bind(encode_term(cx, g, fun), (lambda sfun: hydra.lib.eithers.bind(encode_term(cx, g, arg), (lambda sarg: Right(hydra.ext.scala.utils.sapply(sfun, (sarg,)))))))
            def _hoist_fun_body_2(f, v1):
                match v1:
                    case hydra.core.FunctionLambda(value=lam):
                        lam_body = lam.body
                        def _hoist_lam_body_body_1(v12):
                            match v12:
                                case hydra.core.TermApplication(value=inner_app):
                                    inner_fun = inner_app.function
                                    def _hoist_inner_fun_body_1(inner_f, v13):
                                        match v13:
                                            case hydra.core.EliminationUnion():
                                                return encode_function(cx, g, hydra.annotations.term_annotation_internal(inner_fun), inner_f, Just(arg))

                                            case _:
                                                return hydra.lib.eithers.bind(encode_term(cx, g, fun), (lambda sfun: hydra.lib.eithers.bind(encode_term(cx, g, arg), (lambda sarg: Right(hydra.ext.scala.utils.sapply(sfun, (sarg,)))))))
                                    def _hoist_inner_fun_body_2(inner_f, v13):
                                        match v13:
                                            case hydra.core.FunctionElimination(value=inner_e):
                                                return _hoist_inner_fun_body_1(inner_f, inner_e)

                                            case _:
                                                return hydra.lib.eithers.bind(encode_term(cx, g, fun), (lambda sfun: hydra.lib.eithers.bind(encode_term(cx, g, arg), (lambda sarg: Right(hydra.ext.scala.utils.sapply(sfun, (sarg,)))))))
                                    def _hoist_inner_fun_body_3(v13):
                                        match v13:
                                            case hydra.core.TermFunction(value=inner_f):
                                                return _hoist_inner_fun_body_2(inner_f, inner_f)

                                            case _:
                                                return hydra.lib.eithers.bind(encode_term(cx, g, fun), (lambda sfun: hydra.lib.eithers.bind(encode_term(cx, g, arg), (lambda sarg: Right(hydra.ext.scala.utils.sapply(sfun, (sarg,)))))))
                                    return _hoist_inner_fun_body_3(hydra.rewriting.deannotate_and_detype_term(inner_fun))

                                case _:
                                    return hydra.lib.eithers.bind(encode_term(cx, g, fun), (lambda sfun: hydra.lib.eithers.bind(encode_term(cx, g, arg), (lambda sarg: Right(hydra.ext.scala.utils.sapply(sfun, (sarg,)))))))
                        return _hoist_lam_body_body_1(hydra.rewriting.deannotate_and_detype_term(lam_body))

                    case hydra.core.FunctionElimination(value=e):
                        return _hoist_fun_body_1(f, e)

                    case _:
                        return hydra.lib.eithers.bind(encode_term(cx, g, fun), (lambda sfun: hydra.lib.eithers.bind(encode_term(cx, g, arg), (lambda sarg: Right(hydra.ext.scala.utils.sapply(sfun, (sarg,)))))))
            def _hoist_fun_body_3(v1):
                match v1:
                    case hydra.core.TermFunction(value=f):
                        return _hoist_fun_body_2(f, f)

                    case _:
                        return hydra.lib.eithers.bind(encode_term(cx, g, fun), (lambda sfun: hydra.lib.eithers.bind(encode_term(cx, g, arg), (lambda sarg: Right(hydra.ext.scala.utils.sapply(sfun, (sarg,)))))))
            return _hoist_fun_body_3(hydra.rewriting.deannotate_and_detype_term(fun))

        case hydra.core.TermFunction(value=f):
            return encode_function(cx, g, hydra.annotations.term_annotation_internal(term()), f, Nothing())

        case hydra.core.TermList(value=els):
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda e: encode_term(cx, g, e)), els), (lambda sels: Right(hydra.ext.scala.utils.sapply(hydra.ext.scala.utils.sname("Seq"), sels))))

        case hydra.core.TermLiteral(value=v):
            return hydra.lib.eithers.bind(encode_literal(cx, g, v), (lambda slit: (lit_data := cast(hydra.ext.scala.syntax.Data, hydra.ext.scala.syntax.DataLit(slit)), _hoist_lit_data_body_1 := (lambda v1: (lambda _: Right(hydra.ext.scala.utils.sapply(hydra.ext.scala.utils.sname("BigInt"), (lit_data,))))(v1.value) if isinstance(v1, hydra.core.IntegerValueBigint) else (lambda _: Right(hydra.ext.scala.utils.sapply(hydra.ext.scala.utils.sname("BigInt"), (lit_data,))))(v1.value) if isinstance(v1, hydra.core.IntegerValueUint64) else Right(lit_data)), _hoist_lit_data_body_2 := (lambda v1: (lambda _: Right(hydra.ext.scala.utils.sapply(hydra.ext.scala.utils.sname("BigDecimal"), (lit_data,))))(v1.value) if isinstance(v1, hydra.core.FloatValueBigfloat) else Right(lit_data)), _hoist_lit_data_body_3 := (lambda v1: (lambda iv: _hoist_lit_data_body_1(iv))(v1.value) if isinstance(v1, hydra.core.LiteralInteger) else (lambda fv: _hoist_lit_data_body_2(fv))(v1.value) if isinstance(v1, hydra.core.LiteralFloat) else Right(lit_data)), _hoist_lit_data_body_3(v))[4]))

        case hydra.core.TermMap(value=m):
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda kv: hydra.lib.eithers.bind(encode_term(cx, g, hydra.lib.pairs.first(kv)), (lambda sk: hydra.lib.eithers.bind(encode_term(cx, g, hydra.lib.pairs.second(kv)), (lambda sv: Right(hydra.ext.scala.utils.sassign(sk, sv))))))), hydra.lib.maps.to_list(m)), (lambda spairs: Right(hydra.ext.scala.utils.sapply(hydra.ext.scala.utils.sname("Map"), spairs))))

        case hydra.core.TermWrap(value=wt):
            return encode_term(cx, g, wt.body)

        case hydra.core.TermMaybe(value=m2):
            return hydra.lib.maybes.maybe((lambda : Right(hydra.ext.scala.utils.sname("None"))), (lambda t: hydra.lib.eithers.bind(encode_term(cx, g, t), (lambda s: Right(hydra.ext.scala.utils.sapply(hydra.ext.scala.utils.sname("Some"), (s,)))))), m2)

        case hydra.core.TermRecord(value=rec):
            rname = rec.type_name
            fields = rec.fields
            @lru_cache(1)
            def n() -> str:
                return hydra.ext.scala.utils.scala_type_name(True, rname)
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda f: encode_term(cx, g, f.term)), fields), (lambda args: Right(hydra.ext.scala.utils.sapply(hydra.ext.scala.utils.sname(n()), args))))

        case hydra.core.TermSet(value=s):
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda e: encode_term(cx, g, e)), hydra.lib.sets.to_list(s)), (lambda sels: Right(hydra.ext.scala.utils.sapply(hydra.ext.scala.utils.sname("scala.collection.immutable.Set"), sels))))

        case hydra.core.TermUnion(value=inj):
            sn = inj.type_name
            fn = inj.field.name
            ft = inj.field.term
            @lru_cache(1)
            def lhs() -> hydra.ext.scala.syntax.Data:
                return hydra.ext.scala.utils.sname(hydra.ext.scala.utils.qualify_union_field_name("UNION.", Just(sn), fn))
            @lru_cache(1)
            def union_ftypes() -> FrozenDict[hydra.core.Name, hydra.core.Type]:
                return hydra.lib.eithers.either((lambda _: hydra.lib.maps.empty()), (lambda x_: x_), hydra.schemas.field_types(cx, g, cast(hydra.core.Type, hydra.core.TypeVariable(sn))))
            def _hoist_sn_body_1(v1):
                match v1:
                    case hydra.core.TermUnit():
                        return True

                    case hydra.core.TermRecord(value=rec):
                        return hydra.lib.equality.equal(hydra.lib.lists.length(rec.fields), 0)

                    case _:
                        return False
            def _hoist_sn_body_2(v1):
                match v1:
                    case hydra.core.TypeUnit():
                        return True

                    case hydra.core.TypeRecord(value=rt):
                        return hydra.lib.equality.equal(hydra.lib.lists.length(rt), 0)

                    case _:
                        return False
            return hydra.lib.logic.if_else(hydra.lib.maybes.maybe((lambda : _hoist_sn_body_1(hydra.rewriting.deannotate_and_detype_term(ft))), (lambda dom: _hoist_sn_body_2(hydra.rewriting.deannotate_type(dom))), hydra.lib.maps.lookup(fn, union_ftypes())), (lambda : Right(lhs())), (lambda : hydra.lib.eithers.bind(encode_term(cx, g, ft), (lambda sarg: Right(hydra.ext.scala.utils.sapply(lhs(), (sarg,)))))))

        case hydra.core.TermVariable(value=v2):
            full_name = v2.value
            @lru_cache(1)
            def local_name() -> str:
                return hydra.names.local_name_of(v2)
            parts = hydra.lib.strings.split_on(".", full_name)
            @lru_cache(1)
            def num_parts() -> int:
                return hydra.lib.lists.length(parts)
            @lru_cache(1)
            def escaped() -> str:
                return hydra.lib.logic.if_else(hydra.lib.equality.lte(num_parts(), 1), (lambda : hydra.ext.scala.utils.scala_escape_name(full_name)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(num_parts(), 2), (lambda : hydra.lib.strings.cat2(hydra.lib.lists.head(parts), hydra.lib.strings.cat2(".", hydra.ext.scala.utils.scala_escape_name(local_name())))), (lambda : hydra.lib.strings.intercalate(".", hydra.lib.lists.concat2(hydra.lib.lists.take(hydra.lib.math.sub(num_parts(), 1), parts), (hydra.ext.scala.utils.scala_escape_name(local_name()),)))))))
            return Right(hydra.ext.scala.utils.sname(escaped()))

        case hydra.core.TermAnnotated(value=at):
            return encode_term(cx, g, at.body)

        case hydra.core.TermEither(value=e):
            return hydra.lib.eithers.either((lambda l: hydra.lib.eithers.bind(encode_term(cx, g, l), (lambda sl: Right(hydra.ext.scala.utils.sapply(hydra.ext.scala.utils.sname("Left"), (sl,)))))), (lambda r: hydra.lib.eithers.bind(encode_term(cx, g, r), (lambda sr: Right(hydra.ext.scala.utils.sapply(hydra.ext.scala.utils.sname("Right"), (sr,)))))), e)

        case hydra.core.TermPair(value=p):
            return hydra.lib.eithers.bind(encode_term(cx, g, hydra.lib.pairs.first(p)), (lambda sf: hydra.lib.eithers.bind(encode_term(cx, g, hydra.lib.pairs.second(p)), (lambda ss: Right(hydra.ext.scala.utils.sapply(hydra.ext.scala.utils.sname("Tuple2"), (sf, ss)))))))

        case hydra.core.TermUnit():
            return Right(cast(hydra.ext.scala.syntax.Data, hydra.ext.scala.syntax.DataLit(cast(hydra.ext.scala.syntax.Lit, hydra.ext.scala.syntax.LitUnit()))))

        case hydra.core.TermTypeApplication(value=ta2):
            return encode_term(cx, g, ta2.body)

        case hydra.core.TermTypeLambda(value=tl2):
            return encode_term(cx, hydra.rewriting.extend_graph_for_type_lambda(g, tl2), tl2.body)

        case hydra.core.TermLet(value=lt):
            bindings = lt.bindings
            body = lt.body
            @lru_cache(1)
            def g_let() -> hydra.graph.Graph:
                return hydra.rewriting.extend_graph_for_let((lambda x1, x2: hydra.coder_utils.binding_metadata(x1, x2)), g, lt)
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_let_binding(cx, g_let(), g_let().type_variables, v1)), bindings), (lambda sbindings: hydra.lib.eithers.bind(encode_term(cx, g_let(), body), (lambda sbody: Right(cast(hydra.ext.scala.syntax.Data, hydra.ext.scala.syntax.DataBlock(hydra.ext.scala.syntax.Data_Block(hydra.lib.lists.concat2(sbindings, (cast(hydra.ext.scala.syntax.Stat, hydra.ext.scala.syntax.StatTerm(sbody)),))))))))))

        case _:
            return Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unexpected term"))), cx))

def encode_complex_term_def(cx: hydra.context.Context, g: hydra.graph.Graph, lname: str, term: hydra.core.Term, typ: hydra.core.Type) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Stat]:
    r"""Encode a complex term definition with proper parameter types from the type signature."""

    @lru_cache(1)
    def doms() -> frozenlist[hydra.core.Type]:
        return extract_domains(typ)
    @lru_cache(1)
    def param_names() -> frozenlist[hydra.core.Name]:
        return extract_params(term)
    @lru_cache(1)
    def param_count() -> int:
        return hydra.lib.math.min(hydra.lib.lists.length(param_names()), hydra.lib.lists.length(doms()))
    @lru_cache(1)
    def cod() -> hydra.core.Type:
        return drop_domains(param_count(), typ)
    @lru_cache(1)
    def zipped_params() -> frozenlist[tuple[hydra.core.Name, hydra.core.Type]]:
        return hydra.lib.lists.zip(hydra.lib.lists.take(param_count(), param_names()), hydra.lib.lists.take(param_count(), doms()))
    @lru_cache(1)
    def free_type_vars() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.filter((lambda v: hydra.lib.logic.not_(hydra.lib.lists.elem(46, hydra.lib.strings.to_list(v.value)))), hydra.lib.sets.to_list(hydra.rewriting.free_variables_in_type(typ)))
    @lru_cache(1)
    def tparams() -> frozenlist[hydra.ext.scala.syntax.Type_Param]:
        return hydra.lib.lists.map((lambda tv: hydra.ext.scala.utils.stparam(tv)), free_type_vars())
    @lru_cache(1)
    def let_bindings() -> frozenlist[hydra.core.Binding]:
        return extract_let_bindings(term)
    @lru_cache(1)
    def g_with_type_vars() -> hydra.graph.Graph:
        return hydra.graph.Graph(g.bound_terms, g.bound_types, g.class_constraints, g.lambda_variables, g.metadata, g.primitives, g.schema_types, hydra.lib.sets.union(hydra.lib.sets.from_list(free_type_vars()), g.type_variables))
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_typed_param(cx, g_with_type_vars(), v1)), zipped_params()), (lambda sparams: hydra.lib.eithers.bind(encode_term(cx, g_with_type_vars(), extract_body(term)), (lambda sbody: hydra.lib.eithers.bind(encode_type(cx, g, cod()), (lambda scod: (g_for_lets := hydra.lib.logic.if_else(hydra.lib.lists.null(let_bindings()), (lambda : g_with_type_vars()), (lambda : hydra.rewriting.extend_graph_for_let((lambda x1, x2: hydra.coder_utils.binding_metadata(x1, x2)), g_with_type_vars(), hydra.core.Let(let_bindings(), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("dummy"))))))), hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_let_binding(cx, g_for_lets, hydra.lib.sets.from_list(free_type_vars()), v1)), let_bindings()), (lambda sbindings: (def_body := hydra.lib.logic.if_else(hydra.lib.lists.null(sbindings), (lambda : sbody), (lambda : cast(hydra.ext.scala.syntax.Data, hydra.ext.scala.syntax.DataBlock(hydra.ext.scala.syntax.Data_Block(hydra.lib.lists.concat2(sbindings, (cast(hydra.ext.scala.syntax.Stat, hydra.ext.scala.syntax.StatTerm(sbody)),))))))), Right(cast(hydra.ext.scala.syntax.Stat, hydra.ext.scala.syntax.StatDefn(cast(hydra.ext.scala.syntax.Defn, hydra.ext.scala.syntax.DefnDef(hydra.ext.scala.syntax.Defn_Def((), hydra.ext.scala.syntax.Data_Name(hydra.ext.scala.syntax.PredefString(lname)), tparams(), hydra.lib.lists.map((lambda p: (p,)), sparams), Just(scod), def_body)))))))[1])))[1]))))))

def encode_term_definition(cx: hydra.context.Context, g: hydra.graph.Graph, td: hydra.module.TermDefinition) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Stat]:
    r"""Encode a term definition as a Scala statement."""

    name = td.name
    term = td.term
    @lru_cache(1)
    def lname() -> str:
        return hydra.ext.scala.utils.scala_escape_name(hydra.names.local_name_of(name))
    @lru_cache(1)
    def typ_() -> hydra.core.Type:
        return hydra.lib.maybes.maybe((lambda : cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Unit")))), (lambda v1: v1.type), td.type)
    @lru_cache(1)
    def is_function_type():
        def _hoist_is_function_type_1(v1):
            match v1:
                case hydra.core.TypeFunction():
                    return True

                case _:
                    return False
        match hydra.rewriting.deannotate_type(typ_()):
            case hydra.core.TypeFunction():
                return True

            case hydra.core.TypeForall(value=fa):
                return _hoist_is_function_type_1(hydra.rewriting.deannotate_type(fa.body))

            case _:
                return False
    return hydra.lib.logic.if_else(is_function_type(), (lambda : encode_complex_term_def(cx, g, lname(), term, typ_())), (lambda : hydra.lib.eithers.bind(encode_type(cx, g, typ_()), (lambda stype: hydra.lib.eithers.bind(encode_term(cx, g, term), (lambda rhs: Right(cast(hydra.ext.scala.syntax.Stat, hydra.ext.scala.syntax.StatDefn(cast(hydra.ext.scala.syntax.Defn, hydra.ext.scala.syntax.DefnVal(hydra.ext.scala.syntax.Defn_Val((cast(hydra.ext.scala.syntax.Mod, hydra.ext.scala.syntax.ModLazy()),), (cast(hydra.ext.scala.syntax.Pat, hydra.ext.scala.syntax.PatVar(hydra.ext.scala.syntax.Pat_Var(hydra.ext.scala.syntax.Data_Name(hydra.ext.scala.syntax.PredefString(lname()))))),), Just(stype), rhs))))))))))))

def type_param_to_type_var(tp: hydra.ext.scala.syntax.Type_Param) -> hydra.ext.scala.syntax.Type:
    r"""Convert a type parameter to a type variable reference."""

    n = tp.name
    @lru_cache(1)
    def s() -> str:
        match n:
            case hydra.ext.scala.syntax.NameValue(value=v):
                return v

            case _:
                return ""
    return cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeVar(hydra.ext.scala.syntax.Type_Var(hydra.ext.scala.syntax.Type_Name(s()))))

def field_to_enum_case(cx: hydra.context.Context, g: T0, parent_name: str, tparams: frozenlist[hydra.ext.scala.syntax.Type_Param], ft: hydra.core.FieldType) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Stat]:
    r"""Convert a field type to a Scala enum case."""

    @lru_cache(1)
    def fname() -> str:
        return hydra.ext.scala.utils.scala_escape_name(ft.name.value)
    ftyp = ft.type
    case_name = hydra.ext.scala.syntax.Data_Name(hydra.ext.scala.syntax.PredefString(fname()))
    @lru_cache(1)
    def is_unit() -> bool:
        match hydra.rewriting.deannotate_type(ftyp):
            case hydra.core.TypeUnit():
                return True

            case hydra.core.TypeRecord(value=rt):
                return hydra.lib.equality.equal(hydra.lib.lists.length(rt), 0)

            case _:
                return False
    @lru_cache(1)
    def parent_type() -> hydra.ext.scala.syntax.Type:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(tparams), (lambda : cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeRef(cast(hydra.ext.scala.syntax.Type_Ref, hydra.ext.scala.syntax.Type_RefName(hydra.ext.scala.syntax.Type_Name(parent_name)))))), (lambda : cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeApply(hydra.ext.scala.syntax.Type_Apply(cast(hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.TypeRef(cast(hydra.ext.scala.syntax.Type_Ref, hydra.ext.scala.syntax.Type_RefName(hydra.ext.scala.syntax.Type_Name(parent_name))))), hydra.lib.lists.map((lambda x1: type_param_to_type_var(x1)), tparams))))))
    return hydra.lib.eithers.bind(encode_type(cx, g, ftyp), (lambda sftyp: Right(cast(hydra.ext.scala.syntax.Stat, hydra.ext.scala.syntax.StatDefn(cast(hydra.ext.scala.syntax.Defn, hydra.ext.scala.syntax.DefnEnumCase(hydra.ext.scala.syntax.Defn_EnumCase((), case_name, (), hydra.ext.scala.syntax.Ctor_Primary((), cast(hydra.ext.scala.syntax.Name, hydra.ext.scala.syntax.NameValue("")), (hydra.lib.logic.if_else(is_unit(), (lambda : ()), (lambda : (hydra.ext.scala.syntax.Data_Param((), cast(hydra.ext.scala.syntax.Name, hydra.ext.scala.syntax.NameValue("value")), Just(sftyp), Nothing()),))),)), (hydra.ext.scala.syntax.Init(parent_type(), cast(hydra.ext.scala.syntax.Name, hydra.ext.scala.syntax.NameValue("")), ()),)))))))))

def field_to_param(cx: hydra.context.Context, g: T0, ft: hydra.core.FieldType) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data_Param]:
    r"""Convert a field type to a Scala parameter."""

    @lru_cache(1)
    def fname() -> str:
        return hydra.ext.scala.utils.scala_escape_name(ft.name.value)
    ftyp = ft.type
    return hydra.lib.eithers.bind(encode_type(cx, g, ftyp), (lambda sftyp: Right(hydra.ext.scala.syntax.Data_Param((), cast(hydra.ext.scala.syntax.Name, hydra.ext.scala.syntax.NameValue(fname())), Just(sftyp), Nothing()))))

def encode_type_definition(cx: hydra.context.Context, g: T0, td: hydra.module.TypeDefinition):
    r"""Encode a type definition as a Scala statement."""

    name = td.name
    typ = td.type
    @lru_cache(1)
    def lname() -> str:
        return hydra.names.local_name_of(name)
    tname = hydra.ext.scala.syntax.Type_Name(lname())
    dname = hydra.ext.scala.syntax.Data_Name(hydra.ext.scala.syntax.PredefString(lname()))
    @lru_cache(1)
    def free_vars() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.filter((lambda v: hydra.lib.logic.not_(hydra.lib.lists.elem(46, hydra.lib.strings.to_list(v.value)))), hydra.lib.sets.to_list(hydra.rewriting.free_variables_in_type(typ)))
    @lru_cache(1)
    def tparams() -> frozenlist[hydra.ext.scala.syntax.Type_Param]:
        return hydra.lib.lists.map((lambda _v: (vn := hydra.formatting.capitalize(_v.value), hydra.ext.scala.syntax.Type_Param((), cast(hydra.ext.scala.syntax.Name, hydra.ext.scala.syntax.NameValue(vn)), (), (), (), ()))[1]), free_vars())
    match hydra.rewriting.deannotate_type(typ):
        case hydra.core.TypeForall(value=ft):
            forall_body = ft.body
            forall_param = ft.parameter
            def collect_forall_params(t: hydra.core.Type, acc: frozenlist[hydra.core.Name]):
                def _hoist_collect_forall_params_1(acc, t, v1):
                    match v1:
                        case hydra.core.TypeForall(value=ft2):
                            return collect_forall_params(ft2.body, hydra.lib.lists.cons(ft2.parameter, acc))

                        case _:
                            return (acc, t)
                return _hoist_collect_forall_params_1(acc, t, hydra.rewriting.deannotate_type(t))
            @lru_cache(1)
            def collected() -> tuple[frozenlist[hydra.core.Name], hydra.core.Type]:
                return collect_forall_params(forall_body, (forall_param,))
            @lru_cache(1)
            def all_forall_params() -> frozenlist[hydra.core.Name]:
                return hydra.lib.lists.reverse(hydra.lib.pairs.first(collected()))
            @lru_cache(1)
            def inner_body() -> hydra.core.Type:
                return hydra.lib.pairs.second(collected())
            @lru_cache(1)
            def all_tparams() -> frozenlist[hydra.ext.scala.syntax.Type_Param]:
                return hydra.lib.lists.map((lambda _v: (vn := hydra.formatting.capitalize(_v.value), hydra.ext.scala.syntax.Type_Param((), cast(hydra.ext.scala.syntax.Name, hydra.ext.scala.syntax.NameValue(vn)), (), (), (), ()))[1]), all_forall_params())
            def _hoist_forall_body_body_1(v1):
                match v1:
                    case hydra.core.TypeRecord(value=rt2):
                        return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda f: field_to_param(cx, g, f)), rt2), (lambda params: Right(cast(hydra.ext.scala.syntax.Stat, hydra.ext.scala.syntax.StatDefn(cast(hydra.ext.scala.syntax.Defn, hydra.ext.scala.syntax.DefnClass(hydra.ext.scala.syntax.Defn_Class((cast(hydra.ext.scala.syntax.Mod, hydra.ext.scala.syntax.ModCase()),), tname, all_tparams(), hydra.ext.scala.syntax.Ctor_Primary((), cast(hydra.ext.scala.syntax.Name, hydra.ext.scala.syntax.NameValue("")), (params,)), hydra.ext.scala.syntax.Template((), (), hydra.ext.scala.syntax.Self(None), ())))))))))

                    case hydra.core.TypeUnion(value=rt2):
                        return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda f: field_to_enum_case(cx, g, lname(), all_tparams(), f)), rt2), (lambda cases: Right(cast(hydra.ext.scala.syntax.Stat, hydra.ext.scala.syntax.StatDefn(cast(hydra.ext.scala.syntax.Defn, hydra.ext.scala.syntax.DefnEnum(hydra.ext.scala.syntax.Defn_Enum((), tname, all_tparams(), hydra.ext.scala.syntax.Ctor_Primary((), cast(hydra.ext.scala.syntax.Name, hydra.ext.scala.syntax.NameValue("")), ()), hydra.ext.scala.syntax.Template((), (), hydra.ext.scala.syntax.Self(None), cases)))))))))

                    case hydra.core.TypeWrap(value=wt2):
                        return hydra.lib.eithers.bind(encode_type(cx, g, wt2), (lambda styp: Right(cast(hydra.ext.scala.syntax.Stat, hydra.ext.scala.syntax.StatDefn(cast(hydra.ext.scala.syntax.Defn, hydra.ext.scala.syntax.DefnType(hydra.ext.scala.syntax.Defn_Type((), tname, all_tparams(), styp))))))))

                    case _:
                        return (mk_alias := (lambda styp: Right(cast(hydra.ext.scala.syntax.Stat, hydra.ext.scala.syntax.StatDefn(cast(hydra.ext.scala.syntax.Defn, hydra.ext.scala.syntax.DefnType(hydra.ext.scala.syntax.Defn_Type((), hydra.ext.scala.syntax.Type_Name(lname()), all_tparams(), styp))))))), hydra.lib.eithers.either((lambda _: mk_alias(hydra.ext.scala.utils.stref("Any"))), (lambda x1: mk_alias(x1)), encode_type(cx, g, inner_body())))[1]
            return _hoist_forall_body_body_1(hydra.rewriting.deannotate_type(inner_body()))

        case hydra.core.TypeRecord(value=rt):
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda f: field_to_param(cx, g, f)), rt), (lambda params: Right(cast(hydra.ext.scala.syntax.Stat, hydra.ext.scala.syntax.StatDefn(cast(hydra.ext.scala.syntax.Defn, hydra.ext.scala.syntax.DefnClass(hydra.ext.scala.syntax.Defn_Class((cast(hydra.ext.scala.syntax.Mod, hydra.ext.scala.syntax.ModCase()),), tname, tparams(), hydra.ext.scala.syntax.Ctor_Primary((), cast(hydra.ext.scala.syntax.Name, hydra.ext.scala.syntax.NameValue("")), (params,)), hydra.ext.scala.syntax.Template((), (), hydra.ext.scala.syntax.Self(None), ())))))))))

        case hydra.core.TypeUnion(value=rt2):
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda f: field_to_enum_case(cx, g, lname(), tparams(), f)), rt2), (lambda cases: Right(cast(hydra.ext.scala.syntax.Stat, hydra.ext.scala.syntax.StatDefn(cast(hydra.ext.scala.syntax.Defn, hydra.ext.scala.syntax.DefnEnum(hydra.ext.scala.syntax.Defn_Enum((), tname, tparams(), hydra.ext.scala.syntax.Ctor_Primary((), cast(hydra.ext.scala.syntax.Name, hydra.ext.scala.syntax.NameValue("")), ()), hydra.ext.scala.syntax.Template((), (), hydra.ext.scala.syntax.Self(None), cases)))))))))

        case hydra.core.TypeWrap(value=wt):
            return hydra.lib.eithers.bind(encode_type(cx, g, wt), (lambda styp: Right(cast(hydra.ext.scala.syntax.Stat, hydra.ext.scala.syntax.StatDefn(cast(hydra.ext.scala.syntax.Defn, hydra.ext.scala.syntax.DefnType(hydra.ext.scala.syntax.Defn_Type((), tname, tparams(), styp))))))))

        case _:
            return (mk_alias := (lambda styp: Right(cast(hydra.ext.scala.syntax.Stat, hydra.ext.scala.syntax.StatDefn(cast(hydra.ext.scala.syntax.Defn, hydra.ext.scala.syntax.DefnType(hydra.ext.scala.syntax.Defn_Type((), hydra.ext.scala.syntax.Type_Name(lname()), tparams(), styp))))))), hydra.lib.eithers.either((lambda _: mk_alias(hydra.ext.scala.utils.stref("Any"))), (lambda x1: mk_alias(x1)), encode_type(cx, g, typ)))[1]

def to_el_import(ns: hydra.module.Namespace) -> hydra.ext.scala.syntax.Stat:
    r"""Create an element import statement."""

    return cast(hydra.ext.scala.syntax.Stat, hydra.ext.scala.syntax.StatImportExport(cast(hydra.ext.scala.syntax.ImportExportStat, hydra.ext.scala.syntax.ImportExportStatImport(hydra.ext.scala.syntax.Import((hydra.ext.scala.syntax.Importer(cast(hydra.ext.scala.syntax.Data_Ref, hydra.ext.scala.syntax.Data_RefName(hydra.ext.scala.syntax.Data_Name(hydra.ext.scala.syntax.PredefString(hydra.lib.strings.intercalate(".", hydra.lib.strings.split_on(".", ns.value)))))), (cast(hydra.ext.scala.syntax.Importee, hydra.ext.scala.syntax.ImporteeWildcard()),)),))))))

def to_prim_import(ns: hydra.module.Namespace) -> hydra.ext.scala.syntax.Stat:
    r"""Create a primitive import statement."""

    return cast(hydra.ext.scala.syntax.Stat, hydra.ext.scala.syntax.StatImportExport(cast(hydra.ext.scala.syntax.ImportExportStat, hydra.ext.scala.syntax.ImportExportStatImport(hydra.ext.scala.syntax.Import((hydra.ext.scala.syntax.Importer(cast(hydra.ext.scala.syntax.Data_Ref, hydra.ext.scala.syntax.Data_RefName(hydra.ext.scala.syntax.Data_Name(hydra.ext.scala.syntax.PredefString(hydra.lib.strings.intercalate(".", hydra.lib.strings.split_on(".", ns.value)))))), ()),))))))

def find_imports(cx: hydra.context.Context, g: hydra.graph.Graph, mod: hydra.module.Module) -> Either[hydra.context.InContext[hydra.errors.Error], frozenlist[hydra.ext.scala.syntax.Stat]]:
    r"""Find import statements for the module."""

    return hydra.lib.eithers.bind(hydra.schemas.module_dependency_namespaces(cx, g, False, False, True, False, mod), (lambda el_imps: hydra.lib.eithers.bind(hydra.schemas.module_dependency_namespaces(cx, g, False, True, False, False, mod), (lambda prim_imps: Right(hydra.lib.lists.concat((hydra.lib.lists.map((lambda x1: to_el_import(x1)), hydra.lib.sets.to_list(el_imps)), hydra.lib.lists.map((lambda x1: to_prim_import(x1)), hydra.lib.sets.to_list(prim_imps)))))))))

def construct_module(cx: hydra.context.Context, g: hydra.graph.Graph, mod: hydra.module.Module, defs: frozenlist[hydra.module.Definition]) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Pkg]:
    r"""Construct a Scala package from a Hydra module and its definitions."""

    @lru_cache(1)
    def partitioned() -> tuple[frozenlist[hydra.module.TypeDefinition], frozenlist[hydra.module.TermDefinition]]:
        return hydra.schemas.partition_definitions(defs)
    @lru_cache(1)
    def type_defs() -> frozenlist[hydra.module.TypeDefinition]:
        return hydra.lib.pairs.first(partitioned())
    @lru_cache(1)
    def term_defs() -> frozenlist[hydra.module.TermDefinition]:
        return hydra.lib.pairs.second(partitioned())
    ns_name = mod.namespace.value
    pname = hydra.ext.scala.syntax.Data_Name(hydra.ext.scala.syntax.PredefString(hydra.lib.strings.intercalate(".", hydra.lib.strings.split_on(".", ns_name))))
    pref = cast(hydra.ext.scala.syntax.Data_Ref, hydra.ext.scala.syntax.Data_RefName(pname))
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda td: encode_type_definition(cx, g, td)), type_defs()), (lambda type_decl_stats: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda td: encode_term_definition(cx, g, td)), term_defs()), (lambda term_decl_stats: hydra.lib.eithers.bind(find_imports(cx, g, mod), (lambda imports: Right(hydra.ext.scala.syntax.Pkg(pname, pref, hydra.lib.lists.concat((imports, type_decl_stats, term_decl_stats))))))))))

def encode_untype_application_term(cx: hydra.context.Context, g: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data]:
    r"""Encode an untyped application term by first inferring types."""

    return hydra.lib.eithers.bind(hydra.inference.infer_in_graph_context(cx, g, term), (lambda result: encode_term(cx, g, result.term)))

def extract_codomain(t: hydra.core.Type) -> hydra.core.Type:
    r"""Extract the final return type from a function type."""

    while True:
        match hydra.rewriting.deannotate_type(t):
            case hydra.core.TypeFunction(value=ft):
                t = ft.codomain
                continue

            case hydra.core.TypeForall(value=fa):
                t = fa.body
                continue

            case _:
                return t

def find_domain(cx: hydra.context.Context, g: hydra.graph.Graph, meta: FrozenDict[hydra.core.Name, hydra.core.Term]):
    def _hoist_hydra_ext_scala_coder_find_domain_1(cx, v1):
        match v1:
            case hydra.core.TypeFunction(value=ft):
                return Right(ft.domain)

            case _:
                return Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("expected a function type"))), cx))
    return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _de: hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_de.value))), cx)), (lambda _a: _a), hydra.annotations.get_type(g, meta)), (lambda r: hydra.lib.maybes.maybe((lambda : Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("expected a typed term"))), cx))), (lambda t: _hoist_hydra_ext_scala_coder_find_domain_1(cx, hydra.rewriting.deannotate_type(t))), r)))

def module_to_scala(mod: hydra.module.Module, defs: frozenlist[hydra.module.Definition], cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.context.InContext[hydra.errors.Error], FrozenDict[str, str]]:
    r"""Convert a Hydra module to Scala source code."""

    return hydra.lib.eithers.bind(construct_module(cx, g, mod, defs), (lambda pkg: (s := hydra.serialization.print_expr(hydra.serialization.parenthesize(hydra.ext.scala.serde.write_pkg(pkg))), Right(hydra.lib.maps.singleton(hydra.names.namespace_to_file_path(hydra.util.CaseConvention.CAMEL, hydra.module.FileExtension("scala"), mod.namespace), s)))[1]))
