# Note: this is an automatically generated file. Do not edit.

r"""Rust code generator: converts Hydra type and term modules to Rust source code."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.environment
import hydra.errors
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
import hydra.names
import hydra.packaging
import hydra.rust.language
import hydra.rust.serde
import hydra.rust.syntax
import hydra.serialization
import hydra.strip
import hydra.util
import hydra.variables

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def rust_apply1(name: str, arg: hydra.rust.syntax.Type) -> hydra.rust.syntax.Type:
    return cast(hydra.rust.syntax.Type, hydra.rust.syntax.TypePath_(hydra.rust.syntax.TypePath(False, (hydra.rust.syntax.PathSegment(name, cast(hydra.rust.syntax.GenericArguments, hydra.rust.syntax.GenericArgumentsAngleBracketed(hydra.rust.syntax.AngleBracketedArgs((cast(hydra.rust.syntax.GenericArg, hydra.rust.syntax.GenericArgType(arg)),))))),))))

def rust_path(name: str) -> hydra.rust.syntax.Type:
    return cast(hydra.rust.syntax.Type, hydra.rust.syntax.TypePath_(hydra.rust.syntax.TypePath(False, (hydra.rust.syntax.PathSegment(name, cast(hydra.rust.syntax.GenericArguments, hydra.rust.syntax.GenericArgumentsNone())),))))

def rust_path_segmented(segs: frozenlist[str]) -> hydra.rust.syntax.Type:
    return cast(hydra.rust.syntax.Type, hydra.rust.syntax.TypePath_(hydra.rust.syntax.TypePath(False, hydra.lib.lists.map((lambda s: hydra.rust.syntax.PathSegment(s, cast(hydra.rust.syntax.GenericArguments, hydra.rust.syntax.GenericArgumentsNone()))), segs))))

def encode_literal_type(lt: hydra.core.LiteralType):
    def _hoist_hydra_rust_coder_encode_literal_type_1(v1):
        match v1:
            case hydra.core.FloatType.BIGFLOAT:
                return rust_path("f64")

            case hydra.core.FloatType.FLOAT32:
                return rust_path("f32")

            case hydra.core.FloatType.FLOAT64:
                return rust_path("f64")

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_hydra_rust_coder_encode_literal_type_2(v1):
        match v1:
            case hydra.core.IntegerType.BIGINT:
                return rust_path_segmented(("num", "BigInt"))

            case hydra.core.IntegerType.INT8:
                return rust_path("i8")

            case hydra.core.IntegerType.INT16:
                return rust_path("i16")

            case hydra.core.IntegerType.INT32:
                return rust_path("i32")

            case hydra.core.IntegerType.INT64:
                return rust_path("i64")

            case hydra.core.IntegerType.UINT8:
                return rust_path("u8")

            case hydra.core.IntegerType.UINT16:
                return rust_path("u16")

            case hydra.core.IntegerType.UINT32:
                return rust_path("u32")

            case hydra.core.IntegerType.UINT64:
                return rust_path("u64")

            case _:
                raise AssertionError("Unreachable: all variants handled")
    match lt:
        case hydra.core.LiteralTypeBinary():
            return rust_apply1("Vec", rust_path("u8"))

        case hydra.core.LiteralTypeBoolean():
            return rust_path("bool")

        case hydra.core.LiteralTypeFloat(value=ft):
            return _hoist_hydra_rust_coder_encode_literal_type_1(ft)

        case hydra.core.LiteralTypeInteger(value=it):
            return _hoist_hydra_rust_coder_encode_literal_type_2(it)

        case hydra.core.LiteralTypeString():
            return rust_path("String")

        case _:
            raise TypeError("Unsupported LiteralType")

def rust_apply2(name: str, arg1: hydra.rust.syntax.Type, arg2: hydra.rust.syntax.Type) -> hydra.rust.syntax.Type:
    return cast(hydra.rust.syntax.Type, hydra.rust.syntax.TypePath_(hydra.rust.syntax.TypePath(False, (hydra.rust.syntax.PathSegment(name, cast(hydra.rust.syntax.GenericArguments, hydra.rust.syntax.GenericArgumentsAngleBracketed(hydra.rust.syntax.AngleBracketedArgs((cast(hydra.rust.syntax.GenericArg, hydra.rust.syntax.GenericArgType(arg1)), cast(hydra.rust.syntax.GenericArg, hydra.rust.syntax.GenericArgType(arg2))))))),))))

rust_unit = cast(hydra.rust.syntax.Type, hydra.rust.syntax.TypeUnit())

def encode_type(cx: T0, g: T1, t: hydra.core.Type) -> Either[hydra.errors.Error, hydra.rust.syntax.Type]:
    @lru_cache(1)
    def typ() -> hydra.core.Type:
        return hydra.strip.deannotate_type(t)
    match typ():
        case hydra.core.TypeAnnotated(value=at):
            return encode_type(cx, g, at.body)

        case hydra.core.TypeApplication(value=at2):
            return encode_type(cx, g, at2.function)

        case hydra.core.TypeUnit():
            return Right(rust_unit)

        case hydra.core.TypeVoid():
            return Right(rust_unit)

        case hydra.core.TypeLiteral(value=lt):
            return Right(encode_literal_type(lt))

        case hydra.core.TypeList(value=inner):
            return hydra.lib.eithers.map((lambda enc: rust_apply1("Vec", enc)), encode_type(cx, g, inner))

        case hydra.core.TypeSet(value=inner2):
            return hydra.lib.eithers.map((lambda enc: rust_apply1("BTreeSet", enc)), encode_type(cx, g, inner2))

        case hydra.core.TypeMap(value=mt):
            return hydra.lib.eithers.bind(encode_type(cx, g, mt.keys), (lambda kt: hydra.lib.eithers.bind(encode_type(cx, g, mt.values), (lambda vt: Right(rust_apply2("BTreeMap", kt, vt))))))

        case hydra.core.TypeMaybe(value=inner3):
            return hydra.lib.eithers.map((lambda enc: rust_apply1("Option", enc)), encode_type(cx, g, inner3))

        case hydra.core.TypeEither(value=et):
            return hydra.lib.eithers.bind(encode_type(cx, g, et.left), (lambda lt: hydra.lib.eithers.bind(encode_type(cx, g, et.right), (lambda rt: Right(rust_apply2("Either", lt, rt))))))

        case hydra.core.TypePair(value=pt):
            return hydra.lib.eithers.bind(encode_type(cx, g, pt.first), (lambda ft: hydra.lib.eithers.bind(encode_type(cx, g, pt.second), (lambda st: Right(cast(hydra.rust.syntax.Type, hydra.rust.syntax.TypeTuple((ft, st))))))))

        case hydra.core.TypeFunction(value=ft):
            return hydra.lib.eithers.bind(encode_type(cx, g, ft.domain), (lambda dom: hydra.lib.eithers.bind(encode_type(cx, g, ft.codomain), (lambda cod: Right(rust_apply1("Box", cast(hydra.rust.syntax.Type, hydra.rust.syntax.TypeDynTrait((cast(hydra.rust.syntax.TypeParamBound, hydra.rust.syntax.TypeParamBoundTrait(hydra.rust.syntax.TypePath(False, (hydra.rust.syntax.PathSegment("Fn", cast(hydra.rust.syntax.GenericArguments, hydra.rust.syntax.GenericArgumentsParenthesized(hydra.rust.syntax.ParenthesizedArgs((dom,), Just(cod))))),)))),)))))))))

        case hydra.core.TypeRecord():
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unexpected anonymous record type"))))

        case hydra.core.TypeUnion():
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unexpected anonymous union type"))))

        case hydra.core.TypeWrap():
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unexpected anonymous wrap type"))))

        case hydra.core.TypeVariable(value=name):
            return Right(rust_path(hydra.formatting.capitalize(name.value)))

        case hydra.core.TypeForall(value=fa):
            return encode_type(cx, g, fa.body)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_struct_field(cx: T0, g: T1, ft: hydra.core.FieldType) -> Either[hydra.errors.Error, hydra.rust.syntax.StructField]:
    fname = ft.name.value
    ftyp = ft.type
    return hydra.lib.eithers.bind(encode_type(cx, g, ftyp), (lambda sftyp: Right(hydra.rust.syntax.StructField(hydra.formatting.convert_case_camel_to_lower_snake(hydra.formatting.sanitize_with_underscores(hydra.rust.language.rust_reserved_words(), fname)), sftyp, True, Nothing()))))

def encode_enum_variant(cx: T0, g: T1, ft: hydra.core.FieldType):
    fname = ft.name.value
    ftyp = ft.type
    @lru_cache(1)
    def dtyp() -> hydra.core.Type:
        return hydra.strip.deannotate_type(ftyp)
    @lru_cache(1)
    def is_unit() -> bool:
        match dtyp():
            case hydra.core.TypeUnit():
                return True

            case hydra.core.TypeRecord(value=rt):
                return hydra.lib.lists.null(rt)

            case _:
                return False
    def _hoist_is_unit_body_1(v1):
        match v1:
            case hydra.core.TypeRecord(value=rt):
                return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v12: encode_struct_field(cx, g, v12)), rt), (lambda sfields: Right(hydra.rust.syntax.EnumVariant(hydra.formatting.capitalize(fname), cast(hydra.rust.syntax.EnumVariantBody, hydra.rust.syntax.EnumVariantBodyStruct(sfields)), Nothing()))))

            case _:
                return hydra.lib.eithers.bind(encode_type(cx, g, ftyp), (lambda sftyp: Right(hydra.rust.syntax.EnumVariant(hydra.formatting.capitalize(fname), cast(hydra.rust.syntax.EnumVariantBody, hydra.rust.syntax.EnumVariantBodyTuple((sftyp,))), Nothing()))))
    return hydra.lib.logic.if_else(is_unit(), (lambda : Right(hydra.rust.syntax.EnumVariant(hydra.formatting.capitalize(fname), cast(hydra.rust.syntax.EnumVariantBody, hydra.rust.syntax.EnumVariantBodyUnit()), Nothing()))), (lambda : _hoist_is_unit_body_1(dtyp())))

def encode_literal(lit: hydra.core.Literal):
    def _hoist_hydra_rust_coder_encode_literal_1(v1):
        match v1:
            case hydra.core.FloatValueFloat32(value=f):
                return cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionLiteral(cast(hydra.rust.syntax.Literal, hydra.rust.syntax.LiteralFloat(hydra.rust.syntax.FloatLiteral(hydra.lib.literals.bigfloat_to_float64(hydra.lib.literals.float32_to_bigfloat(f)), Just("f32"))))))

            case hydra.core.FloatValueFloat64(value=f):
                return cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionLiteral(cast(hydra.rust.syntax.Literal, hydra.rust.syntax.LiteralFloat(hydra.rust.syntax.FloatLiteral(f, Nothing())))))

            case hydra.core.FloatValueBigfloat(value=f):
                return cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionLiteral(cast(hydra.rust.syntax.Literal, hydra.rust.syntax.LiteralFloat(hydra.rust.syntax.FloatLiteral(hydra.lib.literals.bigfloat_to_float64(f), Nothing())))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_hydra_rust_coder_encode_literal_2(v1):
        match v1:
            case hydra.core.IntegerValueInt8(value=i):
                return cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionLiteral(cast(hydra.rust.syntax.Literal, hydra.rust.syntax.LiteralInteger(hydra.rust.syntax.IntegerLiteral(hydra.lib.literals.int8_to_bigint(i), Just("i8"))))))

            case hydra.core.IntegerValueInt16(value=i):
                return cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionLiteral(cast(hydra.rust.syntax.Literal, hydra.rust.syntax.LiteralInteger(hydra.rust.syntax.IntegerLiteral(hydra.lib.literals.int16_to_bigint(i), Just("i16"))))))

            case hydra.core.IntegerValueInt32(value=i):
                return cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionLiteral(cast(hydra.rust.syntax.Literal, hydra.rust.syntax.LiteralInteger(hydra.rust.syntax.IntegerLiteral(hydra.lib.literals.int32_to_bigint(i), Just("i32"))))))

            case hydra.core.IntegerValueInt64(value=i):
                return cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionLiteral(cast(hydra.rust.syntax.Literal, hydra.rust.syntax.LiteralInteger(hydra.rust.syntax.IntegerLiteral(hydra.lib.literals.int64_to_bigint(i), Just("i64"))))))

            case hydra.core.IntegerValueUint8(value=i):
                return cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionLiteral(cast(hydra.rust.syntax.Literal, hydra.rust.syntax.LiteralInteger(hydra.rust.syntax.IntegerLiteral(hydra.lib.literals.uint8_to_bigint(i), Just("u8"))))))

            case hydra.core.IntegerValueUint16(value=i):
                return cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionLiteral(cast(hydra.rust.syntax.Literal, hydra.rust.syntax.LiteralInteger(hydra.rust.syntax.IntegerLiteral(hydra.lib.literals.uint16_to_bigint(i), Just("u16"))))))

            case hydra.core.IntegerValueUint32(value=i):
                return cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionLiteral(cast(hydra.rust.syntax.Literal, hydra.rust.syntax.LiteralInteger(hydra.rust.syntax.IntegerLiteral(hydra.lib.literals.uint32_to_bigint(i), Just("u32"))))))

            case hydra.core.IntegerValueUint64(value=i):
                return cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionLiteral(cast(hydra.rust.syntax.Literal, hydra.rust.syntax.LiteralInteger(hydra.rust.syntax.IntegerLiteral(hydra.lib.literals.uint64_to_bigint(i), Just("u64"))))))

            case hydra.core.IntegerValueBigint(value=i):
                return cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionLiteral(cast(hydra.rust.syntax.Literal, hydra.rust.syntax.LiteralInteger(hydra.rust.syntax.IntegerLiteral(i, Nothing())))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    match lit:
        case hydra.core.LiteralBoolean(value=b):
            return cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionLiteral(cast(hydra.rust.syntax.Literal, hydra.rust.syntax.LiteralBool(b))))

        case hydra.core.LiteralString(value=s):
            return cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionLiteral(cast(hydra.rust.syntax.Literal, hydra.rust.syntax.LiteralString(s))))

        case hydra.core.LiteralFloat(value=fv):
            return _hoist_hydra_rust_coder_encode_literal_1(fv)

        case hydra.core.LiteralInteger(value=iv):
            return _hoist_hydra_rust_coder_encode_literal_2(iv)

        case _:
            raise TypeError("Unsupported Literal")

def rust_closure(params: frozenlist[str], body: hydra.rust.syntax.Expression) -> hydra.rust.syntax.Expression:
    return cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionClosure(hydra.rust.syntax.ClosureExpr(False, hydra.lib.lists.map((lambda p: hydra.rust.syntax.ClosureParam(cast(hydra.rust.syntax.Pattern, hydra.rust.syntax.PatternIdentifier(hydra.rust.syntax.IdentifierPattern(p, False, Nothing()))), Nothing())), params), Nothing(), body)))

def rust_expr_path(name: str) -> hydra.rust.syntax.Expression:
    return cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionPath(hydra.rust.syntax.ExprPath(False, (hydra.rust.syntax.PathSegment(name, cast(hydra.rust.syntax.GenericArguments, hydra.rust.syntax.GenericArgumentsNone())),))))

def rust_block(stmts: frozenlist[hydra.rust.syntax.Statement], expr: hydra.rust.syntax.Expression) -> hydra.rust.syntax.Expression:
    return cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionBlock(hydra.rust.syntax.Block(stmts, Just(expr))))

def rust_call(fun: hydra.rust.syntax.Expression, args: frozenlist[hydra.rust.syntax.Expression]) -> hydra.rust.syntax.Expression:
    return cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionCall(hydra.rust.syntax.CallExpr(fun, args)))

def rust_let_stmt(name: str, expr: hydra.rust.syntax.Expression) -> hydra.rust.syntax.Statement:
    return cast(hydra.rust.syntax.Statement, hydra.rust.syntax.StatementLet(hydra.rust.syntax.LetStatement(cast(hydra.rust.syntax.Pattern, hydra.rust.syntax.PatternIdentifier(hydra.rust.syntax.IdentifierPattern(name, False, Nothing()))), False, Nothing(), Just(expr))))

def encode_projection_elim(cx: T0, g: T1, proj: hydra.core.Projection, marg: Maybe[hydra.core.Term]) -> Either[hydra.errors.Error, hydra.rust.syntax.Expression]:
    @lru_cache(1)
    def fname() -> str:
        return hydra.formatting.convert_case_camel_to_lower_snake(proj.field.value)
    return hydra.lib.maybes.cases(marg, (lambda : Right(rust_closure(("v",), cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionFieldAccess(hydra.rust.syntax.FieldAccessExpr(rust_expr_path("v"), fname())))))), (lambda arg: hydra.lib.eithers.bind(encode_term(cx, g, arg), (lambda sarg: Right(cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionFieldAccess(hydra.rust.syntax.FieldAccessExpr(sarg, fname()))))))))

def encode_term(cx: T0, g: T1, term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.rust.syntax.Expression]:
    match term:
        case hydra.core.TermAnnotated(value=at):
            return encode_term(cx, g, at.body)

        case hydra.core.TermApplication(value=app):
            return hydra.lib.eithers.bind(encode_term(cx, g, app.function), (lambda fun: hydra.lib.eithers.bind(encode_term(cx, g, app.argument), (lambda arg: Right(rust_call(fun, (arg,)))))))

        case hydra.core.TermEither(value=e):
            return hydra.lib.eithers.either((lambda l: hydra.lib.eithers.bind(encode_term(cx, g, l), (lambda sl: Right(rust_call(rust_expr_path("Left"), (sl,)))))), (lambda r: hydra.lib.eithers.bind(encode_term(cx, g, r), (lambda sr: Right(rust_call(rust_expr_path("Right"), (sr,)))))), e)

        case hydra.core.TermLambda(value=lam):
            @lru_cache(1)
            def param() -> str:
                return hydra.formatting.convert_case_camel_to_lower_snake(lam.parameter.value)
            return hydra.lib.eithers.bind(encode_term(cx, g, lam.body), (lambda body: Right(rust_closure((param(),), body))))

        case hydra.core.TermProject(value=proj):
            return encode_projection_elim(cx, g, proj, Nothing())

        case hydra.core.TermCases(value=cs):
            return encode_union_elim(cx, g, cs, Nothing())

        case hydra.core.TermUnwrap(value=name):
            return encode_unwrap_elim(cx, g, name, Nothing())

        case hydra.core.TermLet(value=lt):
            bindings = lt.bindings
            body = lt.body
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda b: (bname := hydra.formatting.convert_case_camel_to_lower_snake(b.name.value), hydra.lib.eithers.bind(encode_term(cx, g, b.term), (lambda bval: Right(rust_let_stmt(bname, bval)))))[1]), bindings), (lambda stmts: hydra.lib.eithers.bind(encode_term(cx, g, body), (lambda body_expr: Right(rust_block(stmts, body_expr))))))

        case hydra.core.TermList(value=els):
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_term(cx, g, v1)), els), (lambda sels: Right(rust_call(rust_expr_path("Vec::from"), (cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionArray(cast(hydra.rust.syntax.ArrayExpr, hydra.rust.syntax.ArrayExprElements(sels)))),)))))

        case hydra.core.TermLiteral(value=lit):
            return Right(encode_literal(lit))

        case hydra.core.TermMap(value=m):
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda entry: hydra.lib.eithers.bind(encode_term(cx, g, hydra.lib.pairs.first(entry)), (lambda k: hydra.lib.eithers.bind(encode_term(cx, g, hydra.lib.pairs.second(entry)), (lambda v: Right(cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionTuple((k, v))))))))), hydra.lib.maps.to_list(m)), (lambda pairs: Right(rust_call(rust_expr_path("BTreeMap::from"), (cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionArray(cast(hydra.rust.syntax.ArrayExpr, hydra.rust.syntax.ArrayExprElements(pairs)))),)))))

        case hydra.core.TermMaybe(value=mt):
            return hydra.lib.maybes.cases(mt, (lambda : Right(rust_expr_path("None"))), (lambda val: hydra.lib.eithers.bind(encode_term(cx, g, val), (lambda sval: Right(rust_call(rust_expr_path("Some"), (sval,)))))))

        case hydra.core.TermPair(value=p):
            return hydra.lib.eithers.bind(encode_term(cx, g, hydra.lib.pairs.first(p)), (lambda f: hydra.lib.eithers.bind(encode_term(cx, g, hydra.lib.pairs.second(p)), (lambda s: Right(cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionTuple((f, s))))))))

        case hydra.core.TermRecord(value=rec):
            rname = rec.type_name
            fields = rec.fields
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda f: (fname := hydra.formatting.convert_case_camel_to_lower_snake(f.name.value), hydra.lib.eithers.bind(encode_term(cx, g, f.term), (lambda fval: Right(hydra.rust.syntax.FieldValue(fname, Just(fval))))))[1]), fields), (lambda sfields: Right(cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionStruct(hydra.rust.syntax.StructExpr(hydra.rust.syntax.ExprPath(False, (hydra.rust.syntax.PathSegment(hydra.formatting.capitalize(hydra.names.local_name_of(rname)), cast(hydra.rust.syntax.GenericArguments, hydra.rust.syntax.GenericArgumentsNone())),)), sfields, Nothing()))))))

        case hydra.core.TermSet(value=s):
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_term(cx, g, v1)), hydra.lib.sets.to_list(s)), (lambda sels: Right(rust_call(rust_expr_path("BTreeSet::from"), (cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionArray(cast(hydra.rust.syntax.ArrayExpr, hydra.rust.syntax.ArrayExprElements(sels)))),)))))

        case hydra.core.TermInject(value=inj):
            @lru_cache(1)
            def tname() -> str:
                return hydra.formatting.capitalize(hydra.names.local_name_of(inj.type_name))
            field = inj.field
            @lru_cache(1)
            def fname() -> str:
                return hydra.formatting.capitalize(field.name.value)
            fterm = field.term
            @lru_cache(1)
            def dterm() -> hydra.core.Term:
                return hydra.strip.deannotate_term(fterm)
            @lru_cache(1)
            def is_unit():
                def _hoist_is_unit_1(v1):
                    match v1:
                        case hydra.core.TermUnit():
                            return True

                        case hydra.core.TermRecord(value=rt):
                            return hydra.lib.lists.null(rt.fields)

                        case _:
                            return False
                return _hoist_is_unit_1(dterm())
            return hydra.lib.logic.if_else(is_unit(), (lambda : Right(rust_expr_path(hydra.lib.strings.cat2(hydra.lib.strings.cat2(tname(), "::"), fname())))), (lambda : hydra.lib.eithers.bind(encode_term(cx, g, fterm), (lambda sval: Right(rust_call(rust_expr_path(hydra.lib.strings.cat2(hydra.lib.strings.cat2(tname(), "::"), fname())), (sval,)))))))

        case hydra.core.TermUnit():
            return Right(cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionTuple(())))

        case hydra.core.TermVariable(value=name2):
            return Right(rust_expr_path(hydra.formatting.convert_case_camel_to_lower_snake(hydra.formatting.sanitize_with_underscores(hydra.rust.language.rust_reserved_words(), name2.value))))

        case hydra.core.TermWrap(value=wt):
            @lru_cache(1)
            def tname() -> str:
                return hydra.formatting.capitalize(hydra.names.local_name_of(wt.type_name))
            return hydra.lib.eithers.bind(encode_term(cx, g, wt.body), (lambda inner: Right(rust_call(rust_expr_path(tname()), (inner,)))))

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unexpected term variant"))))

def encode_union_elim(cx: T0, g: T1, cs: hydra.core.CaseStatement, marg: Maybe[hydra.core.Term]) -> Either[hydra.errors.Error, hydra.rust.syntax.Expression]:
    @lru_cache(1)
    def tname() -> str:
        return hydra.formatting.capitalize(hydra.names.local_name_of(cs.type_name))
    case_fields = cs.cases
    def_case = cs.default
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda cf: (cfname := hydra.formatting.capitalize(cf.name.value), cfterm := cf.term, hydra.lib.eithers.bind(encode_term(cx, g, cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cfterm, cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("v"))))))), (lambda arm_body: Right(hydra.rust.syntax.MatchArm(cast(hydra.rust.syntax.Pattern, hydra.rust.syntax.PatternTupleStruct(hydra.rust.syntax.TupleStructPattern(hydra.rust.syntax.ExprPath(False, (hydra.rust.syntax.PathSegment(hydra.lib.strings.cat2(hydra.lib.strings.cat2(tname(), "::"), cfname), cast(hydra.rust.syntax.GenericArguments, hydra.rust.syntax.GenericArgumentsNone())),)), (cast(hydra.rust.syntax.Pattern, hydra.rust.syntax.PatternIdentifier(hydra.rust.syntax.IdentifierPattern("v", False, Nothing()))),)))), Nothing(), arm_body)))))[2]), case_fields), (lambda arms: hydra.lib.eithers.bind(hydra.lib.maybes.cases(def_case, (lambda : Right(arms)), (lambda dt: hydra.lib.eithers.bind(encode_term(cx, g, cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(dt, cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("v"))))))), (lambda def_body: Right(hydra.lib.lists.concat2(arms, (hydra.rust.syntax.MatchArm(cast(hydra.rust.syntax.Pattern, hydra.rust.syntax.PatternWildcard()), Nothing(), def_body),))))))), (lambda all_arms: hydra.lib.maybes.cases(marg, (lambda : Right(rust_closure(("v",), cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionMatch(hydra.rust.syntax.MatchExpr(rust_expr_path("v"), all_arms)))))), (lambda arg: hydra.lib.eithers.bind(encode_term(cx, g, arg), (lambda sarg: Right(cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionMatch(hydra.rust.syntax.MatchExpr(sarg, all_arms))))))))))))

def encode_unwrap_elim(cx: T0, g: T1, name: hydra.core.Name, marg: Maybe[hydra.core.Term]) -> Either[hydra.errors.Error, hydra.rust.syntax.Expression]:
    return hydra.lib.maybes.cases(marg, (lambda : Right(rust_closure(("v",), cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionTupleIndex(hydra.rust.syntax.TupleIndexExpr(rust_expr_path("v"), 0)))))), (lambda arg: hydra.lib.eithers.bind(encode_term(cx, g, arg), (lambda sarg: Right(cast(hydra.rust.syntax.Expression, hydra.rust.syntax.ExpressionTupleIndex(hydra.rust.syntax.TupleIndexExpr(sarg, 0))))))))

def encode_term_definition(cx: T0, g: T1, tdef: hydra.packaging.TermDefinition) -> Either[hydra.errors.Error, hydra.rust.syntax.ItemWithComments]:
    name = tdef.name
    term = tdef.term
    @lru_cache(1)
    def lname() -> str:
        return hydra.formatting.convert_case_camel_to_lower_snake(hydra.names.local_name_of(name))
    @lru_cache(1)
    def typ() -> hydra.core.Type:
        return hydra.lib.maybes.maybe((lambda : cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Unit")))), (lambda v1: v1.type), tdef.type)
    return hydra.lib.eithers.bind(encode_term(cx, g, term), (lambda body: hydra.lib.eithers.bind(encode_type(cx, g, typ()), (lambda ret_type: Right(hydra.rust.syntax.ItemWithComments(Nothing(), cast(hydra.rust.syntax.Visibility, hydra.rust.syntax.VisibilityPublic()), cast(hydra.rust.syntax.Item, hydra.rust.syntax.ItemFn(hydra.rust.syntax.FnDef(lname(), (), Nothing(), (), Just(ret_type), hydra.rust.syntax.Block((), Just(body)), True, False, False, False, Nothing())))))))))

standard_derives = ("Clone", "Debug", "PartialEq", "Eq", "PartialOrd", "Ord")

def encode_type_definition(cx: T0, g: T1, tdef: hydra.packaging.TypeDefinition):
    name = tdef.name
    typ = tdef.type.type
    @lru_cache(1)
    def lname() -> str:
        return hydra.formatting.capitalize(hydra.names.local_name_of(name))
    @lru_cache(1)
    def free_vars() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.filter((lambda v: hydra.lib.equality.equal(hydra.lib.lists.length(hydra.lib.strings.split_on(".", v.value)), 1)), hydra.lib.sets.to_list(hydra.variables.free_variables_in_type(typ)))
    @lru_cache(1)
    def generics() -> frozenlist[hydra.rust.syntax.GenericParam]:
        return hydra.lib.lists.map((lambda v: hydra.rust.syntax.GenericParam(hydra.formatting.capitalize(v.value), ())), free_vars())
    @lru_cache(1)
    def dtyp() -> hydra.core.Type:
        return hydra.strip.deannotate_type(typ)
    def _hoist_dtyp_body_1(v1):
        match v1:
            case hydra.core.TypeRecord(value=rt):
                return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v12: encode_struct_field(cx, g, v12)), rt), (lambda sfields: Right(cast(hydra.rust.syntax.Item, hydra.rust.syntax.ItemStruct(hydra.rust.syntax.StructDef(lname(), generics(), Nothing(), cast(hydra.rust.syntax.StructBody, hydra.rust.syntax.StructBodyNamed(sfields)), standard_derives, True, Nothing()))))))

            case hydra.core.TypeUnion(value=rt):
                return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v12: encode_enum_variant(cx, g, v12)), rt), (lambda variants: Right(cast(hydra.rust.syntax.Item, hydra.rust.syntax.ItemEnum(hydra.rust.syntax.EnumDef(lname(), generics(), Nothing(), variants, standard_derives, True, Nothing()))))))

            case hydra.core.TypeWrap(value=wt):
                return hydra.lib.eithers.bind(encode_type(cx, g, wt), (lambda styp: Right(cast(hydra.rust.syntax.Item, hydra.rust.syntax.ItemStruct(hydra.rust.syntax.StructDef(lname(), generics(), Nothing(), cast(hydra.rust.syntax.StructBody, hydra.rust.syntax.StructBodyTuple((hydra.rust.syntax.TupleField(styp, True),))), standard_derives, True, Nothing()))))))

            case _:
                return hydra.lib.eithers.bind(encode_type(cx, g, typ), (lambda styp: Right(cast(hydra.rust.syntax.Item, hydra.rust.syntax.ItemTypeAlias(hydra.rust.syntax.TypeAlias(lname(), generics(), styp, True, Nothing()))))))
    return hydra.lib.eithers.bind(_hoist_dtyp_body_1(dtyp()), (lambda item: Right(hydra.rust.syntax.ItemWithComments(Nothing(), cast(hydra.rust.syntax.Visibility, hydra.rust.syntax.VisibilityPublic()), item))))

def module_to_rust(mod: hydra.packaging.Module, defs: frozenlist[hydra.packaging.Definition], cx: T0, g: T1) -> Either[hydra.errors.Error, FrozenDict[str, str]]:
    @lru_cache(1)
    def partitioned() -> tuple[frozenlist[hydra.packaging.TypeDefinition], frozenlist[hydra.packaging.TermDefinition]]:
        return hydra.environment.partition_definitions(defs)
    @lru_cache(1)
    def type_defs() -> frozenlist[hydra.packaging.TypeDefinition]:
        return hydra.lib.pairs.first(partitioned())
    @lru_cache(1)
    def term_defs() -> frozenlist[hydra.packaging.TermDefinition]:
        return hydra.lib.pairs.second(partitioned())
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_type_definition(cx, g, v1)), type_defs()), (lambda type_items: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_term_definition(cx, g, v1)), term_defs()), (lambda term_items: (all_items := hydra.lib.lists.concat2(type_items, term_items), crate := hydra.rust.syntax.Crate(all_items), code := hydra.serialization.print_expr(hydra.serialization.parenthesize(hydra.rust.serde.crate_to_expr(crate))), file_path := hydra.names.namespace_to_file_path(hydra.util.CaseConvention.LOWER_SNAKE, hydra.packaging.FileExtension("rs"), mod.namespace), Right(hydra.lib.maps.singleton(file_path, code)))[4]))))
