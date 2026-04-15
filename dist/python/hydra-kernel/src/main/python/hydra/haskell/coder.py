# Note: this is an automatically generated file. Do not edit.

r"""Functions for encoding Hydra modules as Haskell modules."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.adapt
import hydra.analysis
import hydra.annotations
import hydra.classes
import hydra.coders
import hydra.constants
import hydra.core
import hydra.dependencies
import hydra.encode.core
import hydra.errors
import hydra.formatting
import hydra.graph
import hydra.haskell.environment
import hydra.haskell.language
import hydra.haskell.serde
import hydra.haskell.syntax
import hydra.haskell.utils
import hydra.lexical
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
import hydra.names
import hydra.packaging
import hydra.predicates
import hydra.resolution
import hydra.rewriting
import hydra.serialization
import hydra.show.core
import hydra.strip
import hydra.util
import hydra.variables

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")
T4 = TypeVar("T4")

def encode_type(namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName], typ: hydra.core.Type, cx: T0, g: T1):
    r"""Encode a Hydra type as a Haskell type."""

    def encode(t: hydra.core.Type) -> Either[hydra.errors.Error, hydra.haskell.syntax.Type]:
        return encode_type(namespaces, t, cx, g)
    def ref(name: hydra.core.Name) -> Either[T2, hydra.haskell.syntax.Type]:
        return Right(cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeVariable(hydra.haskell.utils.element_reference(namespaces, name))))
    @lru_cache(1)
    def unit_tuple() -> hydra.haskell.syntax.Type:
        return cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeTuple(()))
    def _hoist_encode_body_1(v1):
        match v1:
            case hydra.core.FloatType.FLOAT32:
                return Right(cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeVariable(hydra.haskell.utils.raw_name("Float"))))

            case hydra.core.FloatType.FLOAT64:
                return Right(cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeVariable(hydra.haskell.utils.raw_name("Double"))))

            case hydra.core.FloatType.BIGFLOAT:
                return Right(cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeVariable(hydra.haskell.utils.raw_name("Double"))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_encode_body_2(it, v1):
        match v1:
            case hydra.core.IntegerType.BIGINT:
                return Right(cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeVariable(hydra.haskell.utils.raw_name("Integer"))))

            case hydra.core.IntegerType.INT8:
                return Right(cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeVariable(hydra.haskell.utils.raw_name("I.Int8"))))

            case hydra.core.IntegerType.INT16:
                return Right(cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeVariable(hydra.haskell.utils.raw_name("I.Int16"))))

            case hydra.core.IntegerType.INT32:
                return Right(cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeVariable(hydra.haskell.utils.raw_name("Int"))))

            case hydra.core.IntegerType.INT64:
                return Right(cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeVariable(hydra.haskell.utils.raw_name("I.Int64"))))

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("supported integer type", hydra.show.core.integer_type(it)))))))
    def _hoist_encode_body_3(lt, v1):
        match v1:
            case hydra.core.LiteralTypeBinary():
                return Right(cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeVariable(hydra.haskell.utils.raw_name("B.ByteString"))))

            case hydra.core.LiteralTypeBoolean():
                return Right(cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeVariable(hydra.haskell.utils.raw_name("Bool"))))

            case hydra.core.LiteralTypeDecimal():
                return Right(cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeVariable(hydra.haskell.utils.raw_name("Sci.Scientific"))))

            case hydra.core.LiteralTypeFloat(value=ft):
                return _hoist_encode_body_1(ft)

            case hydra.core.LiteralTypeInteger(value=it):
                return _hoist_encode_body_2(it, it)

            case hydra.core.LiteralTypeString():
                return Right(cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeVariable(hydra.haskell.utils.raw_name("String"))))

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("supported literal type", hydra.show.core.literal_type(lt)))))))
    match hydra.strip.deannotate_type(typ):
        case hydra.core.TypeApplication(value=app):
            lhs = app.function
            rhs = app.argument
            return hydra.lib.eithers.bind(encode(lhs), (lambda hlhs: hydra.lib.eithers.bind(encode(rhs), (lambda hrhs: Right(hydra.haskell.utils.to_type_application((hlhs, hrhs)))))))

        case hydra.core.TypeEither(value=either_type):
            left_ = either_type.left
            right_ = either_type.right
            return hydra.lib.eithers.bind(encode(left_), (lambda hleft: hydra.lib.eithers.bind(encode(right_), (lambda hright: Right(hydra.haskell.utils.to_type_application((cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeVariable(hydra.haskell.utils.raw_name("Either"))), hleft, hright)))))))

        case hydra.core.TypeFunction(value=fun_type):
            dom = fun_type.domain
            cod = fun_type.codomain
            return hydra.lib.eithers.bind(encode(dom), (lambda hdom: hydra.lib.eithers.bind(encode(cod), (lambda hcod: Right(cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeFunction(hydra.haskell.syntax.FunctionType(hdom, hcod))))))))

        case hydra.core.TypeForall(value=forall_type):
            v = forall_type.parameter
            body = forall_type.body
            return encode(body)

        case hydra.core.TypeList(value=lt):
            return hydra.lib.eithers.bind(encode(lt), (lambda hlt: Right(cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeList(hlt)))))

        case hydra.core.TypeLiteral(value=lt2):
            return _hoist_encode_body_3(lt2, lt2)

        case hydra.core.TypeMap(value=map_type):
            kt = map_type.keys
            vt = map_type.values
            return hydra.lib.eithers.bind(encode(kt), (lambda hkt: hydra.lib.eithers.bind(encode(vt), (lambda hvt: Right(hydra.haskell.utils.to_type_application((cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeVariable(hydra.haskell.utils.raw_name("M.Map"))), hkt, hvt)))))))

        case hydra.core.TypeMaybe(value=ot):
            return hydra.lib.eithers.bind(encode(ot), (lambda hot: Right(hydra.haskell.utils.to_type_application((cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeVariable(hydra.haskell.utils.raw_name("Maybe"))), hot)))))

        case hydra.core.TypePair(value=pt):
            return hydra.lib.eithers.bind(encode(pt.first), (lambda f: hydra.lib.eithers.bind(encode(pt.second), (lambda s: Right(cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeTuple((f, s))))))))

        case hydra.core.TypeRecord():
            return ref(hydra.core.Name("placeholder"))

        case hydra.core.TypeSet(value=st):
            return hydra.lib.eithers.bind(encode(st), (lambda hst: Right(hydra.haskell.utils.to_type_application((cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeVariable(hydra.haskell.utils.raw_name("S.Set"))), hst)))))

        case hydra.core.TypeUnion():
            return ref(hydra.core.Name("placeholder"))

        case hydra.core.TypeUnit():
            return Right(unit_tuple())

        case hydra.core.TypeVariable(value=v1):
            return ref(v1)

        case hydra.core.TypeVoid():
            return Right(cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeVariable(hydra.haskell.utils.raw_name("Void"))))

        case hydra.core.TypeWrap():
            return ref(hydra.core.Name("placeholder"))

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("supported type", hydra.show.core.type(typ)))))))

def adapt_type_to_haskell_and_encode(namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName], typ: hydra.core.Type, cx: T0, g: T1) -> Either[hydra.errors.Error, hydra.haskell.syntax.Type]:
    r"""Adapt a Hydra type to Haskell's type system and encode it."""

    def enc(t: hydra.core.Type) -> Either[hydra.errors.Error, hydra.haskell.syntax.Type]:
        return encode_type(namespaces, t, cx, g)
    match hydra.strip.deannotate_type(typ):
        case hydra.core.TypeVariable():
            return enc(typ)

        case _:
            return hydra.lib.eithers.bind(hydra.adapt.adapt_type_for_language(hydra.haskell.language.haskell_language(), typ), (lambda adapted_type: enc(adapted_type)))

def constant_for_field_name(tname: hydra.core.Name, fname: hydra.core.Name) -> str:
    r"""Generate a constant name for a field (e.g., '_TypeName_fieldName')."""

    return hydra.lib.strings.cat(("_", hydra.names.local_name_of(tname), "_", fname.value))

def constant_for_type_name(tname: hydra.core.Name) -> str:
    r"""Generate a constant name for a type (e.g., '_TypeName')."""

    return hydra.lib.strings.cat2("_", hydra.names.local_name_of(tname))

# Create an initial empty metadata record with all flags set to false.
empty_metadata = hydra.haskell.environment.HaskellModuleMetadata(False, False, False, False)

def set_meta_uses_map(b: bool, m: hydra.haskell.environment.HaskellModuleMetadata) -> hydra.haskell.environment.HaskellModuleMetadata:
    return hydra.haskell.environment.HaskellModuleMetadata(m.uses_byte_string, m.uses_int, b, m.uses_set)

def set_meta_uses_set(b: bool, m: hydra.haskell.environment.HaskellModuleMetadata) -> hydra.haskell.environment.HaskellModuleMetadata:
    return hydra.haskell.environment.HaskellModuleMetadata(m.uses_byte_string, m.uses_int, m.uses_map, b)

def extend_meta_for_term(meta: hydra.haskell.environment.HaskellModuleMetadata, term: hydra.core.Term) -> hydra.haskell.environment.HaskellModuleMetadata:
    r"""Extend metadata by analyzing a term for standard import usage (bottom-up step function)."""

    match term:
        case hydra.core.TermMap():
            return set_meta_uses_map(True, meta)

        case hydra.core.TermSet():
            return set_meta_uses_set(True, meta)

        case _:
            return meta

def set_meta_uses_byte_string(b: bool, m: hydra.haskell.environment.HaskellModuleMetadata) -> hydra.haskell.environment.HaskellModuleMetadata:
    return hydra.haskell.environment.HaskellModuleMetadata(b, m.uses_int, m.uses_map, m.uses_set)

def set_meta_uses_int(b: bool, m: hydra.haskell.environment.HaskellModuleMetadata) -> hydra.haskell.environment.HaskellModuleMetadata:
    return hydra.haskell.environment.HaskellModuleMetadata(m.uses_byte_string, b, m.uses_map, m.uses_set)

def extend_meta_for_type(meta: hydra.haskell.environment.HaskellModuleMetadata, typ: hydra.core.Type):
    def _hoist_hydra_haskell_coder_extend_meta_for_type_1(meta, v1):
        match v1:
            case hydra.core.IntegerType.INT8:
                return set_meta_uses_int(True, meta)

            case hydra.core.IntegerType.INT16:
                return set_meta_uses_int(True, meta)

            case hydra.core.IntegerType.INT64:
                return set_meta_uses_int(True, meta)

            case _:
                return meta
    def _hoist_hydra_haskell_coder_extend_meta_for_type_2(meta, v1):
        match v1:
            case hydra.core.LiteralTypeBinary():
                return set_meta_uses_byte_string(True, meta)

            case hydra.core.LiteralTypeInteger(value=it):
                return _hoist_hydra_haskell_coder_extend_meta_for_type_1(meta, it)

            case _:
                return meta
    match hydra.strip.deannotate_type(typ):
        case hydra.core.TypeLiteral(value=lt):
            return _hoist_hydra_haskell_coder_extend_meta_for_type_2(meta, lt)

        case hydra.core.TypeMap():
            return set_meta_uses_map(True, meta)

        case hydra.core.TypeSet():
            return set_meta_uses_set(True, meta)

        case _:
            return meta

def gather_metadata(defs: frozenlist[hydra.packaging.Definition]) -> hydra.haskell.environment.HaskellModuleMetadata:
    r"""Gather metadata from definitions by bottom-up traversal of all terms and types."""

    def add_def(meta: hydra.haskell.environment.HaskellModuleMetadata, def_: hydra.packaging.Definition) -> hydra.haskell.environment.HaskellModuleMetadata:
        match def_:
            case hydra.packaging.DefinitionTerm(value=term_def):
                term = term_def.term
                @lru_cache(1)
                def meta_with_term() -> hydra.haskell.environment.HaskellModuleMetadata:
                    return hydra.rewriting.fold_over_term(hydra.coders.TraversalOrder.PRE, (lambda m, t: extend_meta_for_term(m, t)), meta, term)
                return hydra.lib.maybes.maybe((lambda : meta_with_term()), (lambda ts: hydra.rewriting.fold_over_type(hydra.coders.TraversalOrder.PRE, (lambda m, t: extend_meta_for_type(m, t)), meta_with_term(), ts.type)), term_def.type)

            case hydra.packaging.DefinitionType(value=type_def):
                typ = type_def.type.type
                return hydra.rewriting.fold_over_type(hydra.coders.TraversalOrder.PRE, (lambda m, t: extend_meta_for_type(m, t)), meta, typ)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.lib.lists.foldl((lambda x1, x2: add_def(x1, x2)), empty_metadata, defs)

def encode_literal(l: hydra.core.Literal, cx: T0):
    def _hoist_hydra_haskell_coder_encode_literal_1(v1):
        match v1:
            case hydra.core.FloatValueFloat32(value=f):
                return Right(hydra.haskell.utils.hslit(cast(hydra.haskell.syntax.Literal, hydra.haskell.syntax.LiteralFloat(f))))

            case hydra.core.FloatValueFloat64(value=f):
                return Right(hydra.haskell.utils.hslit(cast(hydra.haskell.syntax.Literal, hydra.haskell.syntax.LiteralDouble(f))))

            case hydra.core.FloatValueBigfloat(value=f):
                return Right(hydra.haskell.utils.hslit(cast(hydra.haskell.syntax.Literal, hydra.haskell.syntax.LiteralDouble(hydra.lib.literals.bigfloat_to_float64(f)))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_hydra_haskell_coder_encode_literal_2(v1):
        match v1:
            case hydra.core.IntegerValueBigint(value=i):
                return Right(hydra.haskell.utils.hslit(cast(hydra.haskell.syntax.Literal, hydra.haskell.syntax.LiteralInteger(i))))

            case hydra.core.IntegerValueInt8(value=i):
                return Right(hydra.haskell.utils.hslit(cast(hydra.haskell.syntax.Literal, hydra.haskell.syntax.LiteralInteger(hydra.lib.literals.int8_to_bigint(i)))))

            case hydra.core.IntegerValueInt16(value=i):
                return Right(hydra.haskell.utils.hslit(cast(hydra.haskell.syntax.Literal, hydra.haskell.syntax.LiteralInteger(hydra.lib.literals.int16_to_bigint(i)))))

            case hydra.core.IntegerValueInt32(value=i):
                return Right(hydra.haskell.utils.hslit(cast(hydra.haskell.syntax.Literal, hydra.haskell.syntax.LiteralInt(i))))

            case hydra.core.IntegerValueInt64(value=i):
                return Right(hydra.haskell.utils.hslit(cast(hydra.haskell.syntax.Literal, hydra.haskell.syntax.LiteralInteger(hydra.lib.literals.int64_to_bigint(i)))))

            case hydra.core.IntegerValueUint8(value=i):
                return Right(hydra.haskell.utils.hslit(cast(hydra.haskell.syntax.Literal, hydra.haskell.syntax.LiteralInteger(hydra.lib.literals.uint8_to_bigint(i)))))

            case hydra.core.IntegerValueUint16(value=i):
                return Right(hydra.haskell.utils.hslit(cast(hydra.haskell.syntax.Literal, hydra.haskell.syntax.LiteralInteger(hydra.lib.literals.uint16_to_bigint(i)))))

            case hydra.core.IntegerValueUint32(value=i):
                return Right(hydra.haskell.utils.hslit(cast(hydra.haskell.syntax.Literal, hydra.haskell.syntax.LiteralInteger(hydra.lib.literals.uint32_to_bigint(i)))))

            case hydra.core.IntegerValueUint64(value=i):
                return Right(hydra.haskell.utils.hslit(cast(hydra.haskell.syntax.Literal, hydra.haskell.syntax.LiteralInteger(hydra.lib.literals.uint64_to_bigint(i)))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    match l:
        case hydra.core.LiteralBinary(value=bs):
            return Right(hydra.haskell.utils.hsapp(hydra.haskell.utils.hsvar("Literals.stringToBinary"), hydra.haskell.utils.hslit(cast(hydra.haskell.syntax.Literal, hydra.haskell.syntax.LiteralString(hydra.lib.literals.binary_to_string(bs))))))

        case hydra.core.LiteralBoolean(value=b):
            return Right(hydra.haskell.utils.hsvar(hydra.lib.logic.if_else(b, (lambda : "True"), (lambda : "False"))))

        case hydra.core.LiteralDecimal(value=d):
            return Right(hydra.haskell.utils.hsapp(hydra.haskell.utils.hsvar("Literals.stringToDecimal"), hydra.haskell.utils.hslit(cast(hydra.haskell.syntax.Literal, hydra.haskell.syntax.LiteralString(hydra.lib.literals.show_decimal(d))))))

        case hydra.core.LiteralFloat(value=fv):
            return _hoist_hydra_haskell_coder_encode_literal_1(fv)

        case hydra.core.LiteralInteger(value=iv):
            return _hoist_hydra_haskell_coder_encode_literal_2(iv)

        case hydra.core.LiteralString(value=s):
            return Right(hydra.haskell.utils.hslit(cast(hydra.haskell.syntax.Literal, hydra.haskell.syntax.LiteralString(s))))

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("supported literal", hydra.show.core.literal(l)))))))

def encode_projection(namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName], proj: hydra.core.Projection) -> Either[T0, hydra.haskell.syntax.Expression]:
    r"""Encode a record projection as a Haskell expression."""

    dn = proj.type_name
    fname = proj.field
    return Right(cast(hydra.haskell.syntax.Expression, hydra.haskell.syntax.ExpressionVariable(hydra.haskell.utils.record_field_reference(namespaces, dn, fname))))

def encode_unwrap(namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName], name: hydra.core.Name) -> Either[T0, hydra.haskell.syntax.Expression]:
    r"""Encode an unwrap term as a Haskell expression."""

    return Right(cast(hydra.haskell.syntax.Expression, hydra.haskell.syntax.ExpressionVariable(hydra.haskell.utils.element_reference(namespaces, hydra.names.qname(hydra.lib.maybes.from_just(hydra.names.namespace_of(name)), hydra.haskell.utils.newtype_accessor_name(name))))))

def encode_case_expression(depth: int, namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName], stmt: hydra.core.CaseStatement, scrutinee: hydra.haskell.syntax.Expression, cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.haskell.syntax.Expression]:
    r"""Encode a Hydra case statement as a Haskell case expression with a given scrutinee."""

    dn = stmt.type_name
    def_ = stmt.default
    fields = stmt.cases
    def to_alt(field_map: FrozenDict[hydra.core.Name, hydra.core.FieldType], field: hydra.core.Field):
        fn = field.name
        fun_ = field.term
        @lru_cache(1)
        def v0() -> str:
            return hydra.lib.strings.cat2("v", hydra.lib.literals.show_int32(depth))
        @lru_cache(1)
        def raw() -> hydra.core.Term:
            return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_, cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name(v0()))))))
        @lru_cache(1)
        def rhs_term() -> hydra.core.Term:
            return hydra.dependencies.simplify_term(raw())
        @lru_cache(1)
        def v1() -> str:
            return hydra.lib.logic.if_else(hydra.variables.is_free_variable_in_term(hydra.core.Name(v0()), rhs_term()), (lambda : hydra.constants.ignored_variable), (lambda : v0()))
        @lru_cache(1)
        def hname() -> hydra.haskell.syntax.Name:
            return hydra.haskell.utils.union_field_reference(hydra.lib.sets.union(hydra.lib.sets.from_list(hydra.lib.maps.keys(g.bound_terms)), hydra.lib.sets.from_list(hydra.lib.maps.keys(g.schema_types))), namespaces, dn, fn)
        return hydra.lib.eithers.bind(hydra.lib.maybes.cases(hydra.lib.maps.lookup(fn, field_map), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorResolution(cast(hydra.errors.ResolutionError, hydra.errors.ResolutionErrorNoMatchingField(hydra.errors.NoMatchingFieldError(fn))))))), (lambda field_type: (ft := field_type.type, no_args := (), single_arg := (cast(hydra.haskell.syntax.Pattern, hydra.haskell.syntax.PatternName(hydra.haskell.utils.raw_name(v1()))),), _hoist_ft_body_1 := (lambda v12: (lambda _: Right(no_args))(v12) if isinstance(v12, hydra.core.TypeUnit) else Right(single_arg)), _hoist_ft_body_1(hydra.strip.deannotate_type(ft)))[4])), (lambda args: (lhs := hydra.haskell.utils.application_pattern(hname(), args), hydra.lib.eithers.bind(hydra.lib.eithers.map((lambda x: hydra.haskell.syntax.CaseRhs(x)), encode_term(hydra.lib.math.add(depth, 1), namespaces, rhs_term(), cx, g)), (lambda rhs: Right(hydra.haskell.syntax.Alternative(lhs, rhs, Nothing())))))[1]))
    return hydra.lib.eithers.bind(hydra.resolution.require_union_type(cx, g, dn), (lambda rt: (to_field_map_entry := (lambda f: (f.name, f)), field_map := hydra.lib.maps.from_list(hydra.lib.lists.map((lambda x1: to_field_map_entry(x1)), rt)), hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: to_alt(field_map, v1)), fields), (lambda ecases: hydra.lib.eithers.bind(hydra.lib.maybes.cases(def_, (lambda : Right(())), (lambda d: hydra.lib.eithers.bind(hydra.lib.eithers.map((lambda x: hydra.haskell.syntax.CaseRhs(x)), encode_term(depth, namespaces, d, cx, g)), (lambda cs: (lhs := cast(hydra.haskell.syntax.Pattern, hydra.haskell.syntax.PatternName(hydra.haskell.utils.raw_name(hydra.constants.ignored_variable))), alt := hydra.haskell.syntax.Alternative(lhs, cs, Nothing()), Right((alt,)))[2])))), (lambda dcases: Right(cast(hydra.haskell.syntax.Expression, hydra.haskell.syntax.ExpressionCase(hydra.haskell.syntax.CaseExpression(scrutinee, hydra.lib.lists.concat2(ecases, dcases))))))))))[2]))

def encode_lambda_term(depth: int, namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName], lam: hydra.core.Lambda, cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.haskell.syntax.Expression]:
    r"""Encode a Hydra lambda as a Haskell expression."""

    v = lam.parameter
    body = lam.body
    return hydra.lib.eithers.bind(encode_term(depth, namespaces, body, cx, g), (lambda hbody: Right(hydra.haskell.utils.hslambda(hydra.haskell.utils.element_reference(namespaces, v), hbody))))

def encode_standalone_cases(depth: int, namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName], stmt: hydra.core.CaseStatement, cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.haskell.syntax.Expression]:
    r"""Encode a standalone (un-applied) case statement as a Haskell lambda over a case expression."""

    return hydra.lib.eithers.map((lambda v1: hydra.haskell.utils.hslambda(hydra.haskell.utils.raw_name("x"), v1)), encode_case_expression(depth, namespaces, stmt, hydra.haskell.utils.hsvar("x"), cx, g))

def encode_term(depth: int, namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName], term: hydra.core.Term, cx: T0, g: hydra.graph.Graph):
    r"""Encode a Hydra term as a Haskell expression."""

    def encode(t: hydra.core.Term) -> Either[hydra.errors.Error, hydra.haskell.syntax.Expression]:
        return encode_term(depth, namespaces, t, cx, g)
    def nonempty_map(m: FrozenDict[hydra.core.Term, hydra.core.Term]) -> Either[hydra.errors.Error, hydra.haskell.syntax.Expression]:
        @lru_cache(1)
        def lhs() -> hydra.haskell.syntax.Expression:
            return hydra.haskell.utils.hsvar("M.fromList")
        def encode_pair(pair: tuple[hydra.core.Term, hydra.core.Term]) -> Either[hydra.errors.Error, hydra.haskell.syntax.Expression]:
            @lru_cache(1)
            def k() -> hydra.core.Term:
                return hydra.lib.pairs.first(pair)
            @lru_cache(1)
            def v() -> hydra.core.Term:
                return hydra.lib.pairs.second(pair)
            return hydra.lib.eithers.bind(encode(k()), (lambda hk: hydra.lib.eithers.bind(encode(v()), (lambda hv: Right(cast(hydra.haskell.syntax.Expression, hydra.haskell.syntax.ExpressionTuple((hk, hv))))))))
        return hydra.lib.eithers.bind(hydra.lib.eithers.map((lambda x: cast(hydra.haskell.syntax.Expression, hydra.haskell.syntax.ExpressionList(x))), hydra.lib.eithers.map_list((lambda x1: encode_pair(x1)), hydra.lib.maps.to_list(m))), (lambda rhs: Right(hydra.haskell.utils.hsapp(lhs(), rhs))))
    def nonempty_set(s: frozenset[hydra.core.Term]) -> Either[hydra.errors.Error, hydra.haskell.syntax.Expression]:
        @lru_cache(1)
        def lhs() -> hydra.haskell.syntax.Expression:
            return hydra.haskell.utils.hsvar("S.fromList")
        return hydra.lib.eithers.bind(encode_term(depth, namespaces, cast(hydra.core.Term, hydra.core.TermList(hydra.lib.sets.to_list(s))), cx, g), (lambda rhs: Right(hydra.haskell.utils.hsapp(lhs(), rhs))))
    match hydra.strip.deannotate_term(term):
        case hydra.core.TermApplication(value=app):
            fun = app.function
            arg = app.argument
            @lru_cache(1)
            def deannotated_fun() -> hydra.core.Term:
                return hydra.strip.deannotate_term(fun)
            def _hoist_fun_body_1(v1):
                match v1:
                    case hydra.core.TermCases(value=stmt):
                        return hydra.lib.eithers.bind(encode(arg), (lambda harg: encode_case_expression(depth, namespaces, stmt, harg, cx, g)))

                    case _:
                        return hydra.lib.eithers.bind(encode(fun), (lambda hfun: hydra.lib.eithers.bind(encode(arg), (lambda harg: Right(hydra.haskell.utils.hsapp(hfun, harg))))))
            return _hoist_fun_body_1(deannotated_fun())

        case hydra.core.TermCases(value=stmt):
            return encode_standalone_cases(depth, namespaces, stmt, cx, g)

        case hydra.core.TermEither(value=e):
            return hydra.lib.eithers.either((lambda l: hydra.lib.eithers.bind(encode(l), (lambda hl: Right(hydra.haskell.utils.hsapp(hydra.haskell.utils.hsvar("Left"), hl))))), (lambda r: hydra.lib.eithers.bind(encode(r), (lambda hr: Right(hydra.haskell.utils.hsapp(hydra.haskell.utils.hsvar("Right"), hr))))), e)

        case hydra.core.TermLambda(value=lam):
            return encode_lambda_term(depth, namespaces, lam, cx, g)

        case hydra.core.TermProject(value=proj):
            return encode_projection(namespaces, proj)

        case hydra.core.TermUnwrap(value=name):
            return encode_unwrap(namespaces, name)

        case hydra.core.TermLet(value=let_term):
            def collect_bindings(lt: hydra.core.Let):
                bs = lt.bindings
                body = lt.body
                def _hoist_body_body_1(v1):
                    match v1:
                        case hydra.core.TermLet(value=inner_lt):
                            @lru_cache(1)
                            def inner_result() -> tuple[frozenlist[hydra.core.Binding], hydra.core.Term]:
                                return collect_bindings(inner_lt)
                            return (hydra.lib.lists.concat2(bs, hydra.lib.pairs.first(inner_result())), hydra.lib.pairs.second(inner_result()))

                        case _:
                            return (bs, body)
                return _hoist_body_body_1(hydra.strip.deannotate_term(body))
            @lru_cache(1)
            def collected() -> tuple[frozenlist[hydra.core.Binding], hydra.core.Term]:
                return collect_bindings(let_term)
            @lru_cache(1)
            def all_bindings() -> frozenlist[hydra.core.Binding]:
                return hydra.lib.pairs.first(collected())
            @lru_cache(1)
            def final_body() -> hydra.core.Term:
                return hydra.lib.pairs.second(collected())
            def encode_binding(binding: hydra.core.Binding) -> Either[hydra.errors.Error, hydra.haskell.syntax.LocalBinding]:
                name = binding.name
                term_ = binding.term
                @lru_cache(1)
                def hname() -> hydra.haskell.syntax.Name:
                    return hydra.haskell.utils.simple_name(name.value)
                return hydra.lib.eithers.bind(encode(term_), (lambda hexpr: Right(cast(hydra.haskell.syntax.LocalBinding, hydra.haskell.syntax.LocalBindingValue(hydra.haskell.utils.simple_value_binding(hname(), hexpr, Nothing()))))))
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: encode_binding(x1)), all_bindings()), (lambda hbindings: hydra.lib.eithers.bind(encode(final_body()), (lambda hinner: Right(cast(hydra.haskell.syntax.Expression, hydra.haskell.syntax.ExpressionLet(hydra.haskell.syntax.LetExpression(hbindings, hinner))))))))

        case hydra.core.TermList(value=els):
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: encode(x1)), els), (lambda helems: Right(cast(hydra.haskell.syntax.Expression, hydra.haskell.syntax.ExpressionList(helems)))))

        case hydra.core.TermLiteral(value=v):
            return encode_literal(v, cx)

        case hydra.core.TermMap(value=m):
            return hydra.lib.logic.if_else(hydra.lib.maps.null(m), (lambda : Right(hydra.haskell.utils.hsvar("M.empty"))), (lambda : nonempty_map(m)))

        case hydra.core.TermMaybe(value=m2):
            return hydra.lib.maybes.cases(m2, (lambda : Right(hydra.haskell.utils.hsvar("Nothing"))), (lambda t: hydra.lib.eithers.bind(encode(t), (lambda ht: Right(hydra.haskell.utils.hsapp(hydra.haskell.utils.hsvar("Just"), ht))))))

        case hydra.core.TermPair(value=p):
            return hydra.lib.eithers.bind(encode(hydra.lib.pairs.first(p)), (lambda f: hydra.lib.eithers.bind(encode(hydra.lib.pairs.second(p)), (lambda s: Right(cast(hydra.haskell.syntax.Expression, hydra.haskell.syntax.ExpressionTuple((f, s))))))))

        case hydra.core.TermRecord(value=record):
            sname = record.type_name
            fields = record.fields
            def to_field_update(field: hydra.core.Field) -> Either[hydra.errors.Error, hydra.haskell.syntax.FieldUpdate]:
                fn = field.name
                ft = field.term
                @lru_cache(1)
                def field_ref() -> hydra.haskell.syntax.Name:
                    return hydra.haskell.utils.record_field_reference(namespaces, sname, fn)
                return hydra.lib.eithers.bind(encode(ft), (lambda hft: Right(hydra.haskell.syntax.FieldUpdate(field_ref(), hft))))
            @lru_cache(1)
            def type_name() -> hydra.haskell.syntax.Name:
                return hydra.haskell.utils.element_reference(namespaces, sname)
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: to_field_update(x1)), fields), (lambda updates: Right(cast(hydra.haskell.syntax.Expression, hydra.haskell.syntax.ExpressionConstructRecord(hydra.haskell.syntax.ConstructRecordExpression(type_name(), updates))))))

        case hydra.core.TermSet(value=s):
            return hydra.lib.logic.if_else(hydra.lib.sets.null(s), (lambda : Right(hydra.haskell.utils.hsvar("S.empty"))), (lambda : nonempty_set(s)))

        case hydra.core.TermTypeLambda(value=abs):
            term1 = abs.body
            return encode(term1)

        case hydra.core.TermTypeApplication(value=typed):
            term1 = typed.body
            return encode(term1)

        case hydra.core.TermInject(value=injection):
            sname = injection.type_name
            field = injection.field
            fn = field.name
            ft = field.term
            @lru_cache(1)
            def lhs() -> hydra.haskell.syntax.Expression:
                return cast(hydra.haskell.syntax.Expression, hydra.haskell.syntax.ExpressionVariable(hydra.haskell.utils.union_field_reference(hydra.lib.sets.union(hydra.lib.sets.from_list(hydra.lib.maps.keys(g.bound_terms)), hydra.lib.sets.from_list(hydra.lib.maps.keys(g.schema_types))), namespaces, sname, fn)))
            @lru_cache(1)
            def dflt() -> Either[hydra.errors.Error, hydra.haskell.syntax.Expression]:
                return hydra.lib.eithers.map((lambda v1: hydra.haskell.utils.hsapp(lhs(), v1)), encode(ft))
            def _hoist_sname_body_1(v1):
                match v1:
                    case hydra.core.TypeUnit():
                        return Right(lhs())

                    case _:
                        return dflt()
            return hydra.lib.eithers.bind(hydra.resolution.require_union_field(cx, g, sname, fn), (lambda ftyp: _hoist_sname_body_1(hydra.strip.deannotate_type(ftyp))))

        case hydra.core.TermUnit():
            return Right(cast(hydra.haskell.syntax.Expression, hydra.haskell.syntax.ExpressionTuple(())))

        case hydra.core.TermVariable(value=name2):
            return Right(cast(hydra.haskell.syntax.Expression, hydra.haskell.syntax.ExpressionVariable(hydra.haskell.utils.element_reference(namespaces, name2))))

        case hydra.core.TermWrap(value=wrapped):
            tname = wrapped.type_name
            term_ = wrapped.body
            @lru_cache(1)
            def lhs() -> hydra.haskell.syntax.Expression:
                return cast(hydra.haskell.syntax.Expression, hydra.haskell.syntax.ExpressionVariable(hydra.haskell.utils.element_reference(namespaces, tname)))
            return hydra.lib.eithers.bind(encode(term_), (lambda rhs: Right(hydra.haskell.utils.hsapp(lhs(), rhs))))

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("supported term", hydra.show.core.term(term)))))))

def find_ord_variables(typ: hydra.core.Type) -> frozenset[hydra.core.Name]:
    r"""Find type variables that require an Ord constraint (used in maps or sets)."""

    def fold(names: frozenset[hydra.core.Name], typ_: hydra.core.Type) -> frozenset[hydra.core.Name]:
        match typ_:
            case hydra.core.TypeMap(value=map_type):
                kt = map_type.keys
                return try_type(names, kt)

            case hydra.core.TypeSet(value=et):
                return try_type(names, et)

            case _:
                return names
    def is_type_variable(v: hydra.core.Name) -> bool:
        return hydra.lib.maybes.is_nothing(hydra.names.namespace_of(v))
    def try_type(names: frozenset[hydra.core.Name], t: hydra.core.Type) -> frozenset[hydra.core.Name]:
        match hydra.strip.deannotate_type(t):
            case hydra.core.TypeVariable(value=v):
                return hydra.lib.logic.if_else(is_type_variable(v), (lambda : hydra.lib.sets.insert(v, names)), (lambda : names))

            case _:
                return names
    return hydra.rewriting.fold_over_type(hydra.coders.TraversalOrder.PRE, (lambda x1, x2: fold(x1, x2)), hydra.lib.sets.empty(), typ)

def get_implicit_type_classes(typ: hydra.core.Type) -> FrozenDict[hydra.core.Name, frozenset[hydra.classes.TypeClass]]:
    r"""Get implicit typeclass constraints for type variables that need Ord."""

    def to_pair(name: T0) -> tuple[T0, frozenset[hydra.classes.TypeClass]]:
        return (name, hydra.lib.sets.from_list((hydra.classes.TypeClass.ORDERING,)))
    return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda x1: to_pair(x1)), hydra.lib.sets.to_list(find_ord_variables(typ))))

def encode_type_with_class_assertions(namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName], explicit_classes: FrozenDict[hydra.core.Name, frozenset[hydra.classes.TypeClass]], typ: hydra.core.Type, cx: T0, g: T1) -> Either[hydra.errors.Error, hydra.haskell.syntax.Type]:
    r"""Encode a Hydra type as a Haskell type with typeclass assertions."""

    @lru_cache(1)
    def classes() -> FrozenDict[hydra.core.Name, frozenset[hydra.classes.TypeClass]]:
        return hydra.lib.maps.union(explicit_classes, get_implicit_type_classes(typ))
    @lru_cache(1)
    def implicit_classes() -> FrozenDict[hydra.core.Name, frozenset[hydra.classes.TypeClass]]:
        return get_implicit_type_classes(typ)
    def encode_assertion(pair: tuple[hydra.core.Name, hydra.classes.TypeClass]) -> hydra.haskell.syntax.Assertion:
        @lru_cache(1)
        def name() -> hydra.core.Name:
            return hydra.lib.pairs.first(pair)
        @lru_cache(1)
        def cls() -> hydra.classes.TypeClass:
            return hydra.lib.pairs.second(pair)
        @lru_cache(1)
        def hname():
            def _hoist_hname_1(v1):
                match v1:
                    case hydra.classes.TypeClass.EQUALITY:
                        return "Eq"

                    case hydra.classes.TypeClass.ORDERING:
                        return "Ord"

                    case _:
                        raise AssertionError("Unreachable: all variants handled")
            return hydra.haskell.utils.raw_name(_hoist_hname_1(cls()))
        @lru_cache(1)
        def htype() -> hydra.haskell.syntax.Type:
            return cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeVariable(hydra.haskell.utils.raw_name(name().value)))
        return cast(hydra.haskell.syntax.Assertion, hydra.haskell.syntax.AssertionClass(hydra.haskell.syntax.ClassAssertion(hname(), (htype(),))))
    @lru_cache(1)
    def assert_pairs() -> frozenlist[tuple[hydra.core.Name, hydra.classes.TypeClass]]:
        return hydra.lib.lists.concat(hydra.lib.lists.map((lambda x1: to_pairs(x1)), hydra.lib.maps.to_list(classes())))
    def to_pairs(map_entry: tuple[T2, frozenset[T3]]) -> frozenlist[tuple[T2, T3]]:
        @lru_cache(1)
        def name() -> T2:
            return hydra.lib.pairs.first(map_entry)
        @lru_cache(1)
        def cls_set() -> frozenset[T3]:
            return hydra.lib.pairs.second(map_entry)
        def to_pair(c: T4) -> tuple[T2, T4]:
            return (name(), c)
        return hydra.lib.lists.map((lambda x1: to_pair(x1)), hydra.lib.sets.to_list(cls_set()))
    return hydra.lib.eithers.bind(adapt_type_to_haskell_and_encode(namespaces, typ, cx, g), (lambda htyp: hydra.lib.logic.if_else(hydra.lib.lists.null(assert_pairs()), (lambda : Right(htyp)), (lambda : (encoded := hydra.lib.lists.map((lambda x1: encode_assertion(x1)), assert_pairs()), hassert := hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(encoded), 1), (lambda : hydra.lib.lists.head(encoded)), (lambda : cast(hydra.haskell.syntax.Assertion, hydra.haskell.syntax.AssertionTuple(encoded)))), Right(cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeCtx(hydra.haskell.syntax.ContextType(hassert, htyp)))))[2]))))

def type_scheme_constraints_to_class_map(maybe_constraints: Maybe[FrozenDict[T0, hydra.core.TypeVariableMetadata]]) -> FrozenDict[T0, frozenset[hydra.classes.TypeClass]]:
    r"""Convert type scheme constraints to a map of type variables to typeclasses."""

    def name_to_type_class(class_name: hydra.core.Name) -> Maybe[hydra.classes.TypeClass]:
        class_name_str = class_name.value
        @lru_cache(1)
        def is_eq() -> bool:
            return hydra.lib.equality.equal(class_name_str, hydra.core.Name("equality").value)
        @lru_cache(1)
        def is_ord() -> bool:
            return hydra.lib.equality.equal(class_name_str, hydra.core.Name("ordering").value)
        return hydra.lib.logic.if_else(is_eq(), (lambda : Just(hydra.classes.TypeClass.EQUALITY)), (lambda : hydra.lib.logic.if_else(is_ord(), (lambda : Just(hydra.classes.TypeClass.ORDERING)), (lambda : Nothing()))))
    return hydra.lib.maybes.maybe((lambda : hydra.lib.maps.empty()), (lambda constraints: hydra.lib.maps.map((lambda meta: hydra.lib.sets.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda x1: name_to_type_class(x1)), hydra.lib.sets.to_list(meta.classes))))), constraints)), maybe_constraints)

def to_data_declaration(namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName], def_: hydra.packaging.TermDefinition, cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.haskell.syntax.DeclarationWithComments]:
    r"""Convert a Hydra term definition to a Haskell declaration with comments."""

    name = def_.name
    term = def_.term
    typ = def_.type
    @lru_cache(1)
    def hname() -> hydra.haskell.syntax.Name:
        return hydra.haskell.utils.simple_name(hydra.names.local_name_of(name))
    def rewrite_value_binding(vb: hydra.haskell.syntax.ValueBinding):
        match vb:
            case hydra.haskell.syntax.ValueBindingSimple(value=simple):
                pattern_ = simple.pattern
                rhs = simple.rhs
                bindings = simple.local_bindings
                def _hoist_pattern_body_1(v1):
                    match v1:
                        case hydra.haskell.syntax.PatternApplication(value=app_pat):
                            name_ = app_pat.name
                            args = app_pat.args
                            rhs_expr = rhs.value
                            def _hoist_name_body_1(v12):
                                match v12:
                                    case hydra.haskell.syntax.ExpressionLambda(value=lambda_):
                                        vars = lambda_.bindings
                                        body = lambda_.inner
                                        @lru_cache(1)
                                        def new_pattern() -> hydra.haskell.syntax.Pattern:
                                            return hydra.haskell.utils.application_pattern(name_, hydra.lib.lists.concat2(args, vars))
                                        new_rhs = hydra.haskell.syntax.RightHandSide(body)
                                        return rewrite_value_binding(cast(hydra.haskell.syntax.ValueBinding, hydra.haskell.syntax.ValueBindingSimple(hydra.haskell.syntax.SimpleValueBinding(new_pattern(), new_rhs, bindings))))

                                    case _:
                                        return vb
                            return _hoist_name_body_1(rhs_expr)

                        case _:
                            return vb
                return _hoist_pattern_body_1(pattern_)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def to_decl(comments: Maybe[str], hname_: hydra.haskell.syntax.Name, term_: hydra.core.Term, bindings: Maybe[hydra.haskell.syntax.LocalBindings]) -> Either[hydra.errors.Error, hydra.haskell.syntax.DeclarationWithComments]:
        match hydra.strip.deannotate_term(term_):
            case hydra.core.TermLet(value=let_term):
                lbindings = let_term.bindings
                env = let_term.body
                def to_term_definition(hname_: hydra.haskell.syntax.Name, hterm_: hydra.haskell.syntax.Expression) -> hydra.haskell.syntax.LocalBinding:
                    return cast(hydra.haskell.syntax.LocalBinding, hydra.haskell.syntax.LocalBindingValue(hydra.haskell.utils.simple_value_binding(hname_, hterm_, Nothing())))
                @lru_cache(1)
                def hnames() -> frozenlist[hydra.haskell.syntax.Name]:
                    return hydra.lib.lists.map((lambda binding: hydra.haskell.utils.simple_name(binding.name.value)), lbindings)
                @lru_cache(1)
                def terms() -> frozenlist[hydra.core.Term]:
                    return hydra.lib.lists.map((lambda v1: v1.term), lbindings)
                return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda t: encode_term(0, namespaces, t, cx, g)), terms()), (lambda hterms: (hbindings := hydra.lib.lists.zip_with((lambda x1, x2: to_term_definition(x1, x2)), hnames(), hterms), prev_bindings := hydra.lib.maybes.maybe((lambda : ()), (lambda lb: lb.value), bindings), all_bindings := hydra.lib.lists.concat2(prev_bindings, hbindings), to_decl(comments, hname_, env, Just(hydra.haskell.syntax.LocalBindings(all_bindings))))[3]))

            case _:
                return hydra.lib.eithers.bind(encode_term(0, namespaces, term_, cx, g), (lambda hterm: (vb := hydra.haskell.utils.simple_value_binding(hname_, hterm, bindings), scheme_constraints := hydra.lib.maybes.maybe((lambda : Nothing()), (lambda ts: ts.constraints), typ), scheme_classes := type_scheme_constraints_to_class_map(scheme_constraints), hydra.lib.eithers.bind(hydra.annotations.get_type_classes(cx, g, hydra.strip.remove_types_from_term(term)), (lambda explicit_classes: (combined_classes := hydra.lib.maps.union(scheme_classes, explicit_classes), scheme_type := hydra.lib.maybes.maybe((lambda : cast(hydra.core.Type, hydra.core.TypeUnit())), (lambda ts: ts.type), typ), hydra.lib.eithers.bind(encode_type_with_class_assertions(namespaces, combined_classes, scheme_type, cx, g), (lambda htype: (decl := cast(hydra.haskell.syntax.Declaration, hydra.haskell.syntax.DeclarationTypedBinding(hydra.haskell.syntax.TypedBinding(hydra.haskell.syntax.TypeSignature(hname_, htype), rewrite_value_binding(vb)))), Right(hydra.haskell.syntax.DeclarationWithComments(decl, comments)))[1])))[2])))[3]))
    return hydra.lib.eithers.bind(hydra.annotations.get_term_description(cx, g, term), (lambda comments: to_decl(comments, hname(), term, Nothing())))

# Whether to include type definitions in generated Haskell modules.
include_type_definitions = False

# Whether to use the Hydra core import in generated modules.
use_core_import = True

def name_decls(namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName], name: hydra.core.Name, typ: hydra.core.Type) -> frozenlist[hydra.haskell.syntax.DeclarationWithComments]:
    r"""Generate Haskell declarations for type and field name constants."""

    nm = name.value
    def to_decl(n: hydra.core.Name, pair: tuple[str, str]) -> hydra.haskell.syntax.DeclarationWithComments:
        @lru_cache(1)
        def k() -> str:
            return hydra.lib.pairs.first(pair)
        @lru_cache(1)
        def v() -> str:
            return hydra.lib.pairs.second(pair)
        @lru_cache(1)
        def decl() -> hydra.haskell.syntax.Declaration:
            return cast(hydra.haskell.syntax.Declaration, hydra.haskell.syntax.DeclarationValueBinding(cast(hydra.haskell.syntax.ValueBinding, hydra.haskell.syntax.ValueBindingSimple(hydra.haskell.syntax.SimpleValueBinding(hydra.haskell.utils.application_pattern(hydra.haskell.utils.simple_name(k()), ()), hydra.haskell.syntax.RightHandSide(cast(hydra.haskell.syntax.Expression, hydra.haskell.syntax.ExpressionApplication(hydra.haskell.syntax.ApplicationExpression(cast(hydra.haskell.syntax.Expression, hydra.haskell.syntax.ExpressionVariable(hydra.haskell.utils.element_reference(namespaces, n))), cast(hydra.haskell.syntax.Expression, hydra.haskell.syntax.ExpressionLiteral(cast(hydra.haskell.syntax.Literal, hydra.haskell.syntax.LiteralString(v())))))))), Nothing())))))
        return hydra.haskell.syntax.DeclarationWithComments(decl(), Nothing())
    @lru_cache(1)
    def name_decl() -> tuple[str, str]:
        return (constant_for_type_name(name), nm)
    @lru_cache(1)
    def field_decls() -> frozenlist[tuple[str, str]]:
        return hydra.lib.lists.map((lambda x1: to_constant(x1)), hydra.lexical.fields_of(typ))
    def to_constant(field_type: hydra.core.FieldType) -> tuple[str, str]:
        fname = field_type.name
        return (constant_for_field_name(name, fname), fname.value)
    return hydra.lib.logic.if_else(use_core_import, (lambda : hydra.lib.lists.cons(to_decl(hydra.core.Name("hydra.core.Name"), name_decl()), hydra.lib.lists.map((lambda v1: to_decl(hydra.core.Name("hydra.core.Name"), v1)), field_decls()))), (lambda : ()))

def type_decl(namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName], name: hydra.core.Name, typ: hydra.core.Type, cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.haskell.syntax.DeclarationWithComments]:
    r"""Generate a Haskell declaration for a type definition constant."""

    def type_name(ns: hydra.packaging.Namespace, name_: hydra.core.Name) -> hydra.core.Name:
        return hydra.names.qname(ns, type_name_local(name_))
    def type_name_local(name_: hydra.core.Name) -> str:
        return hydra.lib.strings.cat(("_", hydra.names.local_name_of(name_), "_type_"))
    @lru_cache(1)
    def raw_term() -> hydra.core.Term:
        return hydra.encode.core.type(typ)
    def rewrite(recurse: Callable[[hydra.core.Term], hydra.core.Term], term: hydra.core.Term) -> hydra.core.Term:
        @lru_cache(1)
        def variant_result() -> Maybe[hydra.core.Field]:
            match hydra.strip.deannotate_term(term):
                case hydra.core.TermInject(value=inj):
                    return hydra.lib.logic.if_else(hydra.lib.equality.equal(inj.type_name, hydra.core.Name("hydra.core.Type")), (lambda : Just(inj.field)), (lambda : Nothing()))

                case _:
                    return Nothing()
        def decode_string(term2: hydra.core.Term):
            def _hoist_decode_string_1(v1):
                match v1:
                    case hydra.core.LiteralString(value=s):
                        return Just(s)

                    case _:
                        return Nothing()
            match hydra.strip.deannotate_term(term2):
                case hydra.core.TermLiteral(value=lit):
                    return _hoist_decode_string_1(lit)

                case _:
                    return Nothing()
        def decode_name(term2: hydra.core.Term) -> Maybe[hydra.core.Name]:
            match hydra.strip.deannotate_term(term2):
                case hydra.core.TermWrap(value=wt):
                    return hydra.lib.logic.if_else(hydra.lib.equality.equal(wt.type_name, hydra.core.Name("hydra.core.Name")), (lambda : hydra.lib.maybes.map((lambda x: hydra.core.Name(x)), decode_string(wt.body))), (lambda : Nothing()))

                case _:
                    return Nothing()
        def for_type(field: hydra.core.Field) -> Maybe[hydra.core.Term]:
            fname = field.name
            fterm = field.term
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(fname, hydra.core.Name("record")), (lambda : Nothing()), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(fname, hydra.core.Name("variable")), (lambda : hydra.lib.maybes.bind(decode_name(fterm), (lambda x1: for_variable_type(x1)))), (lambda : Nothing()))))
        def for_variable_type(vname: hydra.core.Name) -> Maybe[hydra.core.Term]:
            @lru_cache(1)
            def qname() -> hydra.packaging.QualifiedName:
                return hydra.names.qualify_name(vname)
            mns = qname().namespace
            local = qname().local
            return hydra.lib.maybes.map((lambda ns: cast(hydra.core.Term, hydra.core.TermVariable(hydra.names.qname(ns, hydra.lib.strings.cat(("_", local, "_type_")))))), mns)
        return hydra.lib.maybes.from_maybe((lambda : recurse(term)), hydra.lib.maybes.bind(variant_result(), (lambda x1: for_type(x1))))
    @lru_cache(1)
    def final_term() -> hydra.core.Term:
        return hydra.rewriting.rewrite_term((lambda x1, x2: rewrite(x1, x2)), raw_term())
    return hydra.lib.eithers.bind(encode_term(0, namespaces, final_term(), cx, g), (lambda expr: (rhs := hydra.haskell.syntax.RightHandSide(expr), hname := hydra.haskell.utils.simple_name(type_name_local(name)), pat := hydra.haskell.utils.application_pattern(hname, ()), decl := cast(hydra.haskell.syntax.Declaration, hydra.haskell.syntax.DeclarationValueBinding(cast(hydra.haskell.syntax.ValueBinding, hydra.haskell.syntax.ValueBindingSimple(hydra.haskell.syntax.SimpleValueBinding(pat, rhs, Nothing()))))), Right(hydra.haskell.syntax.DeclarationWithComments(decl, Nothing())))[4]))

def to_type_declarations_from(namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName], element_name: hydra.core.Name, typ: hydra.core.Type, cx: hydra.context.Context, g: hydra.graph.Graph):
    r"""Convert a Hydra type definition to Haskell declarations."""

    @lru_cache(1)
    def lname() -> str:
        return hydra.names.local_name_of(element_name)
    @lru_cache(1)
    def hname() -> hydra.haskell.syntax.Name:
        return hydra.haskell.utils.simple_name(lname())
    def decl_head(name: hydra.haskell.syntax.Name, vars_: frozenlist[hydra.core.Name]) -> hydra.haskell.syntax.DeclarationHead:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(vars_), (lambda : cast(hydra.haskell.syntax.DeclarationHead, hydra.haskell.syntax.DeclarationHeadSimple(name))), (lambda : (h := hydra.lib.lists.head(vars_), rest := hydra.lib.lists.tail(vars_), hvar := hydra.haskell.syntax.Variable(hydra.haskell.utils.simple_name(h.value)), cast(hydra.haskell.syntax.DeclarationHead, hydra.haskell.syntax.DeclarationHeadApplication(hydra.haskell.syntax.ApplicationDeclarationHead(decl_head(name, rest), hvar))))[3]))
    def newtype_cons(tname: hydra.core.Name, typ_: hydra.core.Type) -> Either[hydra.errors.Error, hydra.haskell.syntax.ConstructorWithComments]:
        @lru_cache(1)
        def hname0() -> hydra.haskell.syntax.Name:
            return hydra.haskell.utils.simple_name(hydra.haskell.utils.newtype_accessor_name(tname))
        return hydra.lib.eithers.bind(adapt_type_to_haskell_and_encode(namespaces, typ_, cx, g), (lambda htype: (hfield := hydra.haskell.syntax.FieldWithComments(hydra.haskell.syntax.Field(hname0(), htype), Nothing()), constructor_name := hydra.haskell.utils.simple_name(hydra.names.local_name_of(tname)), Right(hydra.haskell.syntax.ConstructorWithComments(cast(hydra.haskell.syntax.Constructor, hydra.haskell.syntax.ConstructorRecord(hydra.haskell.syntax.RecordConstructor(constructor_name, (hfield,)))), Nothing())))[2]))
    def record_cons(lname_: str, fields: frozenlist[hydra.core.FieldType]) -> Either[hydra.errors.Error, hydra.haskell.syntax.ConstructorWithComments]:
        def to_field(field_type: hydra.core.FieldType) -> Either[hydra.errors.Error, hydra.haskell.syntax.FieldWithComments]:
            fname = field_type.name
            ftype = field_type.type
            @lru_cache(1)
            def hname_() -> hydra.haskell.syntax.Name:
                return hydra.haskell.utils.simple_name(hydra.lib.strings.cat2(hydra.formatting.decapitalize(lname_), hydra.formatting.capitalize(fname.value)))
            return hydra.lib.eithers.bind(adapt_type_to_haskell_and_encode(namespaces, ftype, cx, g), (lambda htype: hydra.lib.eithers.bind(hydra.annotations.get_type_description(cx, g, ftype), (lambda comments: Right(hydra.haskell.syntax.FieldWithComments(hydra.haskell.syntax.Field(hname_(), htype), comments))))))
        return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: to_field(x1)), fields), (lambda h_fields: Right(hydra.haskell.syntax.ConstructorWithComments(cast(hydra.haskell.syntax.Constructor, hydra.haskell.syntax.ConstructorRecord(hydra.haskell.syntax.RecordConstructor(hydra.haskell.utils.simple_name(lname_), h_fields))), Nothing()))))
    def union_cons(bound_names_: frozenset[hydra.core.Name], lname_: str, field_type: hydra.core.FieldType) -> Either[hydra.errors.Error, hydra.haskell.syntax.ConstructorWithComments]:
        fname = field_type.name
        ftype = field_type.type
        def deconflict(name: str) -> str:
            @lru_cache(1)
            def tname() -> hydra.core.Name:
                return hydra.names.unqualify_name(hydra.packaging.QualifiedName(Just(hydra.lib.pairs.first(namespaces.focus)), name))
            return hydra.lib.logic.if_else(hydra.lib.sets.member(tname(), bound_names_), (lambda : deconflict(hydra.lib.strings.cat2(name, "_"))), (lambda : name))
        return hydra.lib.eithers.bind(hydra.annotations.get_type_description(cx, g, ftype), (lambda comments: (nm := deconflict(hydra.lib.strings.cat2(hydra.formatting.capitalize(lname_), hydra.formatting.capitalize(fname.value))), hydra.lib.eithers.bind(hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.strip.deannotate_type(ftype), cast(hydra.core.Type, hydra.core.TypeUnit())), (lambda : Right(())), (lambda : hydra.lib.eithers.bind(adapt_type_to_haskell_and_encode(namespaces, ftype, cx, g), (lambda htype: Right((htype,)))))), (lambda type_list: Right(hydra.haskell.syntax.ConstructorWithComments(cast(hydra.haskell.syntax.Constructor, hydra.haskell.syntax.ConstructorOrdinary(hydra.haskell.syntax.OrdinaryConstructor(hydra.haskell.utils.simple_name(nm), type_list))), comments)))))[1]))
    return hydra.lib.eithers.bind(hydra.predicates.is_serializable_by_name(cx, g, element_name), (lambda is_ser: (deriv := hydra.haskell.syntax.Deriving(hydra.lib.logic.if_else(is_ser, (lambda : hydra.lib.lists.map((lambda x1: hydra.haskell.utils.raw_name(x1)), ("Eq", "Ord", "Read", "Show"))), (lambda : ()))), unpack_result := hydra.haskell.utils.unpack_forall_type(typ), vars := hydra.lib.pairs.first(unpack_result), t_ := hydra.lib.pairs.second(unpack_result), hd := decl_head(hname(), hydra.lib.lists.reverse(vars)), _hoist_deriv_body_1 := (lambda v1: (lambda rt: hydra.lib.eithers.bind(record_cons(lname(), rt), (lambda cons: Right(cast(hydra.haskell.syntax.Declaration, hydra.haskell.syntax.DeclarationData(hydra.haskell.syntax.DataDeclaration(hydra.haskell.syntax.DataOrNewtype.DATA, (), hd, (cons,), (deriv,))))))))(v1.value) if isinstance(v1, hydra.core.TypeRecord) else (lambda rt: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v12: union_cons(hydra.lib.sets.from_list(hydra.lib.maps.keys(g.bound_terms)), lname(), v12)), rt), (lambda cons: Right(cast(hydra.haskell.syntax.Declaration, hydra.haskell.syntax.DeclarationData(hydra.haskell.syntax.DataDeclaration(hydra.haskell.syntax.DataOrNewtype.DATA, (), hd, cons, (deriv,))))))))(v1.value) if isinstance(v1, hydra.core.TypeUnion) else (lambda wrapped: hydra.lib.eithers.bind(newtype_cons(element_name, wrapped), (lambda cons: Right(cast(hydra.haskell.syntax.Declaration, hydra.haskell.syntax.DeclarationData(hydra.haskell.syntax.DataDeclaration(hydra.haskell.syntax.DataOrNewtype.NEWTYPE, (), hd, (cons,), (deriv,))))))))(v1.value) if isinstance(v1, hydra.core.TypeWrap) else hydra.lib.eithers.bind(adapt_type_to_haskell_and_encode(namespaces, typ, cx, g), (lambda htype: Right(cast(hydra.haskell.syntax.Declaration, hydra.haskell.syntax.DeclarationType(hydra.haskell.syntax.TypeDeclaration(hd, htype))))))), hydra.lib.eithers.bind(_hoist_deriv_body_1(hydra.strip.deannotate_type(t_)), (lambda decl: hydra.lib.eithers.bind(hydra.annotations.get_type_description(cx, g, typ), (lambda comments: hydra.lib.eithers.bind(hydra.lib.logic.if_else(include_type_definitions, (lambda : hydra.lib.eithers.bind(type_decl(namespaces, element_name, typ, cx, g), (lambda decl_: Right((decl_,))))), (lambda : Right(()))), (lambda tdecls: (main_decl := hydra.haskell.syntax.DeclarationWithComments(decl, comments), name_decls_ := name_decls(namespaces, element_name, typ), Right(hydra.lib.lists.concat(((main_decl,), name_decls_, tdecls))))[2])))))))[6]))

def construct_module(namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName], mod: hydra.packaging.Module, defs: frozenlist[hydra.packaging.Definition], cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.haskell.syntax.Module]:
    r"""Construct a Haskell module from a Hydra module and its definitions."""

    def h(namespace: hydra.packaging.Namespace) -> str:
        return namespace.value
    def create_declarations(def_: hydra.packaging.Definition) -> Either[hydra.errors.Error, frozenlist[hydra.haskell.syntax.DeclarationWithComments]]:
        match def_:
            case hydra.packaging.DefinitionType(value=type):
                name = type.name
                typ = type.type.type
                return to_type_declarations_from(namespaces, name, typ, cx, g)

            case hydra.packaging.DefinitionTerm(value=term):
                return hydra.lib.eithers.bind(to_data_declaration(namespaces, term, cx, g), (lambda d: Right((d,))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def import_name(name: str) -> hydra.haskell.syntax.ModuleName:
        return hydra.haskell.syntax.ModuleName(hydra.lib.strings.intercalate(".", hydra.lib.lists.map(hydra.formatting.capitalize, hydra.lib.strings.split_on(".", name))))
    @lru_cache(1)
    def imports() -> frozenlist[hydra.haskell.syntax.Import]:
        return hydra.lib.lists.concat2(domain_imports(), standard_imports())
    @lru_cache(1)
    def domain_imports() -> frozenlist[hydra.haskell.syntax.Import]:
        def to_import(pair: tuple[hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName]) -> hydra.haskell.syntax.Import:
            @lru_cache(1)
            def namespace() -> hydra.packaging.Namespace:
                return hydra.lib.pairs.first(pair)
            @lru_cache(1)
            def alias() -> hydra.haskell.syntax.ModuleName:
                return hydra.lib.pairs.second(pair)
            @lru_cache(1)
            def name() -> str:
                return h(namespace())
            return hydra.haskell.syntax.Import(True, import_name(name()), Just(alias()), Nothing())
        return hydra.lib.lists.map((lambda x1: to_import(x1)), hydra.lib.maps.to_list(namespaces.mapping))
    @lru_cache(1)
    def meta() -> hydra.haskell.environment.HaskellModuleMetadata:
        return gather_metadata(defs)
    def cond_import(flag: bool, triple: T0) -> frozenlist[T0]:
        return hydra.lib.logic.if_else(flag, (lambda : (triple,)), (lambda : ()))
    @lru_cache(1)
    def standard_imports() -> frozenlist[hydra.haskell.syntax.Import]:
        def to_import(triple: tuple[tuple[str, Maybe[str]], frozenlist[str]]) -> hydra.haskell.syntax.Import:
            @lru_cache(1)
            def name() -> str:
                return hydra.lib.pairs.first(hydra.lib.pairs.first(triple))
            @lru_cache(1)
            def malias() -> Maybe[str]:
                return hydra.lib.pairs.second(hydra.lib.pairs.first(triple))
            @lru_cache(1)
            def hidden() -> frozenlist[str]:
                return hydra.lib.pairs.second(triple)
            @lru_cache(1)
            def spec() -> Maybe[hydra.haskell.syntax.SpecImport]:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(hidden()), (lambda : Nothing()), (lambda : Just(cast(hydra.haskell.syntax.SpecImport, hydra.haskell.syntax.SpecImportHiding(hydra.lib.lists.map((lambda n: hydra.haskell.syntax.ImportExportSpec(Nothing(), hydra.haskell.utils.simple_name(n), Nothing())), hidden()))))))
            return hydra.haskell.syntax.Import(hydra.lib.maybes.is_just(malias()), hydra.haskell.syntax.ModuleName(name()), hydra.lib.maybes.map((lambda x: hydra.haskell.syntax.ModuleName(x)), malias()), spec())
        return hydra.lib.lists.map((lambda x1: to_import(x1)), hydra.lib.lists.concat((((("Prelude", Nothing()), ("Enum", "Ordering", "decodeFloat", "encodeFloat", "fail", "map", "pure", "sum")),), ((("Data.Scientific", Just("Sci")), ()),), cond_import(meta().uses_byte_string, (("Data.ByteString", Just("B")), ())), cond_import(meta().uses_int, (("Data.Int", Just("I")), ())), cond_import(meta().uses_map, (("Data.Map", Just("M")), ())), cond_import(meta().uses_set, (("Data.Set", Just("S")), ())), hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.analysis.module_contains_binary_literals(mod), hydra.analysis.module_contains_decimal_literals(mod)), (lambda : ((("Hydra.Lib.Literals", Just("Literals")), ()),)), (lambda : ())))))
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: create_declarations(x1)), defs), (lambda decl_lists: (decls := hydra.lib.lists.concat(decl_lists), mc := mod.description, Right(hydra.haskell.syntax.Module(Just(hydra.haskell.syntax.ModuleHead(mc, import_name(h(mod.namespace)), ())), imports(), decls)))[2]))

# The key used to track Haskell variable depth in annotations.
key_haskell_var = hydra.core.Name("haskellVar")

def module_to_haskell_module(mod: hydra.packaging.Module, defs: frozenlist[hydra.packaging.Definition], cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.haskell.syntax.Module]:
    r"""Convert a Hydra module and definitions to a Haskell module AST."""

    return hydra.lib.eithers.bind(hydra.haskell.utils.namespaces_for_module(mod, cx, g), (lambda namespaces: construct_module(namespaces, mod, defs, cx, g)))

def module_to_haskell(mod: hydra.packaging.Module, defs: frozenlist[hydra.packaging.Definition], cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.errors.Error, FrozenDict[str, str]]:
    r"""Convert a Hydra module to Haskell source code as a filepath-to-content map."""

    return hydra.lib.eithers.bind(module_to_haskell_module(mod, defs, cx, g), (lambda hsmod: (s := hydra.serialization.print_expr(hydra.serialization.parenthesize(hydra.haskell.serde.module_to_expr(hsmod))), filepath := hydra.names.namespace_to_file_path(hydra.util.CaseConvention.PASCAL, hydra.packaging.FileExtension("hs"), mod.namespace), Right(hydra.lib.maps.singleton(filepath, s)))[2]))
