"""Meta-DSL for constructing Hydra core terms and types as first-class values.

Mirrors the Haskell module Hydra.Dsl.Meta.Core, providing phantom-typed constructors,
accessors, and modifiers for all core Hydra types.
"""

from typing import TypeVar

import hydra.core as C
import hydra.dsl.meta.phantoms as Phantoms
from hydra.core import Name
from hydra.phantoms import TTerm

A = TypeVar("A")
B = TypeVar("B")


# ============================================================
# AnnotatedTerm
# ============================================================

def annotated_term(body: TTerm, annotation: TTerm) -> TTerm:
    """Construct an AnnotatedTerm."""
    return Phantoms.record(C.ANNOTATED_TERM__NAME, [
        Phantoms.field(C.ANNOTATED_TERM__BODY__NAME, body),
        Phantoms.field(C.ANNOTATED_TERM__ANNOTATION__NAME, annotation)])


def annotated_term_body(at: TTerm) -> TTerm:
    """Get the body of an AnnotatedTerm."""
    return Phantoms.apply(Phantoms.project(C.ANNOTATED_TERM__NAME, C.ANNOTATED_TERM__BODY__NAME), at)


def annotated_term_annotation(at: TTerm) -> TTerm:
    """Get the annotation of an AnnotatedTerm."""
    return Phantoms.apply(Phantoms.project(C.ANNOTATED_TERM__NAME, C.ANNOTATED_TERM__ANNOTATION__NAME), at)


def annotated_term_with_body(at: TTerm, body: TTerm) -> TTerm:
    """Return an AnnotatedTerm with a new body."""
    return annotated_term(body, annotated_term_annotation(at))


# ============================================================
# AnnotatedType
# ============================================================

def annotated_type(body: TTerm, annotation: TTerm) -> TTerm:
    """Construct an AnnotatedType."""
    return Phantoms.record(C.ANNOTATED_TYPE__NAME, [
        Phantoms.field(C.ANNOTATED_TYPE__BODY__NAME, body),
        Phantoms.field(C.ANNOTATED_TYPE__ANNOTATION__NAME, annotation)])


def annotated_type_body(at: TTerm) -> TTerm:
    """Get the body of an AnnotatedType."""
    return Phantoms.apply(Phantoms.project(C.ANNOTATED_TYPE__NAME, C.ANNOTATED_TYPE__BODY__NAME), at)


def annotated_type_annotation(at: TTerm) -> TTerm:
    """Get the annotation of an AnnotatedType."""
    return Phantoms.apply(Phantoms.project(C.ANNOTATED_TYPE__NAME, C.ANNOTATED_TYPE__ANNOTATION__NAME), at)


# ============================================================
# Application
# ============================================================

def application(function: TTerm, argument: TTerm) -> TTerm:
    """Construct an Application."""
    return Phantoms.record(C.APPLICATION__NAME, [
        Phantoms.field(C.APPLICATION__FUNCTION__NAME, function),
        Phantoms.field(C.APPLICATION__ARGUMENT__NAME, argument)])


def application_function(app: TTerm) -> TTerm:
    """Get the function of an Application."""
    return Phantoms.apply(Phantoms.project(C.APPLICATION__NAME, C.APPLICATION__FUNCTION__NAME), app)


def application_argument(app: TTerm) -> TTerm:
    """Get the argument of an Application."""
    return Phantoms.apply(Phantoms.project(C.APPLICATION__NAME, C.APPLICATION__ARGUMENT__NAME), app)


# ============================================================
# ApplicationType
# ============================================================

def application_type(function: TTerm, argument: TTerm) -> TTerm:
    """Construct an ApplicationType."""
    return Phantoms.record(C.APPLICATION_TYPE__NAME, [
        Phantoms.field(C.APPLICATION_TYPE__FUNCTION__NAME, function),
        Phantoms.field(C.APPLICATION_TYPE__ARGUMENT__NAME, argument)])


def application_type_function(app: TTerm) -> TTerm:
    """Get the function of an ApplicationType."""
    return Phantoms.apply(Phantoms.project(C.APPLICATION_TYPE__NAME, C.APPLICATION_TYPE__FUNCTION__NAME), app)


def application_type_argument(app: TTerm) -> TTerm:
    """Get the argument of an ApplicationType."""
    return Phantoms.apply(Phantoms.project(C.APPLICATION_TYPE__NAME, C.APPLICATION_TYPE__ARGUMENT__NAME), app)


# ============================================================
# Binding
# ============================================================

def binding(name_term: TTerm, term: TTerm, mtype: TTerm) -> TTerm:
    """Construct a Binding."""
    return Phantoms.record(C.BINDING__NAME, [
        Phantoms.field(C.BINDING__NAME__NAME, name_term),
        Phantoms.field(C.BINDING__TERM__NAME, term),
        Phantoms.field(C.BINDING__TYPE__NAME, mtype)])


def binding_name(b: TTerm) -> TTerm:
    """Get the name of a Binding."""
    return Phantoms.apply(Phantoms.project(C.BINDING__NAME, C.BINDING__NAME__NAME), b)


def binding_term(b: TTerm) -> TTerm:
    """Get the term of a Binding."""
    return Phantoms.apply(Phantoms.project(C.BINDING__NAME, C.BINDING__TERM__NAME), b)


def binding_type(b: TTerm) -> TTerm:
    """Get the type of a Binding."""
    return Phantoms.apply(Phantoms.project(C.BINDING__NAME, C.BINDING__TYPE__NAME), b)


def binding_with_term(b: TTerm, term: TTerm) -> TTerm:
    """Return a Binding with a new term."""
    return binding(binding_name(b), term, binding_type(b))


# ============================================================
# CaseStatement
# ============================================================

def case_statement(type_name: TTerm, default_term: TTerm, cases: TTerm) -> TTerm:
    """Construct a CaseStatement."""
    return Phantoms.record(C.CASE_STATEMENT__NAME, [
        Phantoms.field(C.CASE_STATEMENT__TYPE_NAME__NAME, type_name),
        Phantoms.field(C.CASE_STATEMENT__DEFAULT__NAME, default_term),
        Phantoms.field(C.CASE_STATEMENT__CASES__NAME, cases)])


def case_statement_type_name(cs: TTerm) -> TTerm:
    """Get the typeName of a CaseStatement."""
    return Phantoms.apply(Phantoms.project(C.CASE_STATEMENT__NAME, C.CASE_STATEMENT__TYPE_NAME__NAME), cs)


def case_statement_default(cs: TTerm) -> TTerm:
    """Get the default of a CaseStatement."""
    return Phantoms.apply(Phantoms.project(C.CASE_STATEMENT__NAME, C.CASE_STATEMENT__DEFAULT__NAME), cs)


def case_statement_cases(cs: TTerm) -> TTerm:
    """Get the cases of a CaseStatement."""
    return Phantoms.apply(Phantoms.project(C.CASE_STATEMENT__NAME, C.CASE_STATEMENT__CASES__NAME), cs)


# ============================================================
# EitherType
# ============================================================

def either_type(left: TTerm, right: TTerm) -> TTerm:
    """Construct an EitherType."""
    return Phantoms.record(C.EITHER_TYPE__NAME, [
        Phantoms.field(C.EITHER_TYPE__LEFT__NAME, left),
        Phantoms.field(C.EITHER_TYPE__RIGHT__NAME, right)])


def either_type_left(et: TTerm) -> TTerm:
    """Get the left type of an EitherType."""
    return Phantoms.apply(Phantoms.project(C.EITHER_TYPE__NAME, C.EITHER_TYPE__LEFT__NAME), et)


def either_type_right(et: TTerm) -> TTerm:
    """Get the right type of an EitherType."""
    return Phantoms.apply(Phantoms.project(C.EITHER_TYPE__NAME, C.EITHER_TYPE__RIGHT__NAME), et)


# ============================================================
# PairType
# ============================================================

def pair_type(first: TTerm, second: TTerm) -> TTerm:
    """Construct a PairType."""
    return Phantoms.record(C.PAIR_TYPE__NAME, [
        Phantoms.field(C.PAIR_TYPE__FIRST__NAME, first),
        Phantoms.field(C.PAIR_TYPE__SECOND__NAME, second)])


def pair_type_first(pt: TTerm) -> TTerm:
    """Get the first type of a PairType."""
    return Phantoms.apply(Phantoms.project(C.PAIR_TYPE__NAME, C.PAIR_TYPE__FIRST__NAME), pt)


def pair_type_second(pt: TTerm) -> TTerm:
    """Get the second type of a PairType."""
    return Phantoms.apply(Phantoms.project(C.PAIR_TYPE__NAME, C.PAIR_TYPE__SECOND__NAME), pt)


# ============================================================
# Elimination
# ============================================================

def elimination_record(proj: TTerm) -> TTerm:
    """Inject a Projection into an Elimination."""
    return Phantoms.inject(C.ELIMINATION__NAME, C.ELIMINATION__RECORD__NAME, proj)


def elimination_union(cs: TTerm) -> TTerm:
    """Inject a CaseStatement into an Elimination."""
    return Phantoms.inject(C.ELIMINATION__NAME, C.ELIMINATION__UNION__NAME, cs)


def elimination_wrap(name: TTerm) -> TTerm:
    """Inject a Name into an Elimination (wrap variant)."""
    return Phantoms.inject(C.ELIMINATION__NAME, C.ELIMINATION__WRAP__NAME, name)


# ============================================================
# Field
# ============================================================

def field(name: TTerm, term: TTerm) -> TTerm:
    """Construct a Field."""
    return Phantoms.record(C.FIELD__NAME, [
        Phantoms.field(C.FIELD__NAME__NAME, name),
        Phantoms.field(C.FIELD__TERM__NAME, term)])


def field_name(f: TTerm) -> TTerm:
    """Get the name of a Field."""
    return Phantoms.apply(Phantoms.project(C.FIELD__NAME, C.FIELD__NAME__NAME), f)


def field_term(f: TTerm) -> TTerm:
    """Get the term of a Field."""
    return Phantoms.apply(Phantoms.project(C.FIELD__NAME, C.FIELD__TERM__NAME), f)


def field_with_term(t: TTerm, ft: TTerm) -> TTerm:
    """Return a Field with a new term."""
    return field(field_name(ft), t)


# ============================================================
# FieldType
# ============================================================

def field_type(name: TTerm, typ: TTerm) -> TTerm:
    """Construct a FieldType."""
    return Phantoms.record(C.FIELD_TYPE__NAME, [
        Phantoms.field(C.FIELD_TYPE__NAME__NAME, name),
        Phantoms.field(C.FIELD_TYPE__TYPE__NAME, typ)])


def field_type_name(ft: TTerm) -> TTerm:
    """Get the name of a FieldType."""
    return Phantoms.apply(Phantoms.project(C.FIELD_TYPE__NAME, C.FIELD_TYPE__NAME__NAME), ft)


def field_type_type(ft: TTerm) -> TTerm:
    """Get the type of a FieldType."""
    return Phantoms.apply(Phantoms.project(C.FIELD_TYPE__NAME, C.FIELD_TYPE__TYPE__NAME), ft)


def field_type_with_type(ft: TTerm, t: TTerm) -> TTerm:
    """Return a FieldType with a new type."""
    return field_type(field_type_name(ft), t)


# ============================================================
# FloatType
# ============================================================

def float_type_bigfloat() -> TTerm:
    """The bigfloat FloatType variant."""
    return Phantoms.inject_unit(C.FLOAT_TYPE__NAME, C.FLOAT_TYPE__BIGFLOAT__NAME)


def float_type_float32() -> TTerm:
    """The float32 FloatType variant."""
    return Phantoms.inject_unit(C.FLOAT_TYPE__NAME, C.FLOAT_TYPE__FLOAT32__NAME)


def float_type_float64() -> TTerm:
    """The float64 FloatType variant."""
    return Phantoms.inject_unit(C.FLOAT_TYPE__NAME, C.FLOAT_TYPE__FLOAT64__NAME)


# ============================================================
# FloatValue
# ============================================================

def float_value_bigfloat(v: TTerm) -> TTerm:
    """Inject a bigfloat into a FloatValue."""
    return Phantoms.inject(C.FLOAT_VALUE__NAME, C.FLOAT_VALUE__BIGFLOAT__NAME, v)


def float_value_float32(v: TTerm) -> TTerm:
    """Inject a float32 into a FloatValue."""
    return Phantoms.inject(C.FLOAT_VALUE__NAME, C.FLOAT_VALUE__FLOAT32__NAME, v)


def float_value_float64(v: TTerm) -> TTerm:
    """Inject a float64 into a FloatValue."""
    return Phantoms.inject(C.FLOAT_VALUE__NAME, C.FLOAT_VALUE__FLOAT64__NAME, v)


# ============================================================
# ForallType
# ============================================================

def forall_type(parameter: TTerm, body: TTerm) -> TTerm:
    """Construct a ForallType."""
    return Phantoms.record(C.FORALL_TYPE__NAME, [
        Phantoms.field(C.FORALL_TYPE__PARAMETER__NAME, parameter),
        Phantoms.field(C.FORALL_TYPE__BODY__NAME, body)])


def forall_type_parameter(ft: TTerm) -> TTerm:
    """Get the parameter of a ForallType."""
    return Phantoms.apply(Phantoms.project(C.FORALL_TYPE__NAME, C.FORALL_TYPE__PARAMETER__NAME), ft)


def forall_type_body(ft: TTerm) -> TTerm:
    """Get the body of a ForallType."""
    return Phantoms.apply(Phantoms.project(C.FORALL_TYPE__NAME, C.FORALL_TYPE__BODY__NAME), ft)


# ============================================================
# Function
# ============================================================

def function_elimination(elim: TTerm) -> TTerm:
    """Inject an Elimination into a Function."""
    return Phantoms.inject(C.FUNCTION__NAME, C.FUNCTION__ELIMINATION__NAME, elim)


def function_lambda(lam: TTerm) -> TTerm:
    """Inject a Lambda into a Function."""
    return Phantoms.inject(C.FUNCTION__NAME, C.FUNCTION__LAMBDA__NAME, lam)


def function_primitive(name: TTerm) -> TTerm:
    """Inject a Name into a Function (primitive variant)."""
    return Phantoms.inject(C.FUNCTION__NAME, C.FUNCTION__PRIMITIVE__NAME, name)


# ============================================================
# FunctionType
# ============================================================

def function_type(domain: TTerm, codomain: TTerm) -> TTerm:
    """Construct a FunctionType."""
    return Phantoms.record(C.FUNCTION_TYPE__NAME, [
        Phantoms.field(C.FUNCTION_TYPE__DOMAIN__NAME, domain),
        Phantoms.field(C.FUNCTION_TYPE__CODOMAIN__NAME, codomain)])


def function_type_domain(ft: TTerm) -> TTerm:
    """Get the domain of a FunctionType."""
    return Phantoms.apply(Phantoms.project(C.FUNCTION_TYPE__NAME, C.FUNCTION_TYPE__DOMAIN__NAME), ft)


def function_type_codomain(ft: TTerm) -> TTerm:
    """Get the codomain of a FunctionType."""
    return Phantoms.apply(Phantoms.project(C.FUNCTION_TYPE__NAME, C.FUNCTION_TYPE__CODOMAIN__NAME), ft)


# ============================================================
# Injection
# ============================================================

def injection(type_name: TTerm, fld: TTerm) -> TTerm:
    """Construct an Injection."""
    return Phantoms.record(C.INJECTION__NAME, [
        Phantoms.field(C.INJECTION__TYPE_NAME__NAME, type_name),
        Phantoms.field(C.INJECTION__FIELD__NAME, fld)])


def injection_type_name(inj: TTerm) -> TTerm:
    """Get the typeName of an Injection."""
    return Phantoms.apply(Phantoms.project(C.INJECTION__NAME, C.INJECTION__TYPE_NAME__NAME), inj)


def injection_field(inj: TTerm) -> TTerm:
    """Get the field of an Injection."""
    return Phantoms.apply(Phantoms.project(C.INJECTION__NAME, C.INJECTION__FIELD__NAME), inj)


# ============================================================
# IntegerType
# ============================================================

def integer_type_bigint() -> TTerm:
    """The bigint IntegerType variant."""
    return Phantoms.inject_unit(C.INTEGER_TYPE__NAME, C.INTEGER_TYPE__BIGINT__NAME)


def integer_type_int8() -> TTerm:
    """The int8 IntegerType variant."""
    return Phantoms.inject_unit(C.INTEGER_TYPE__NAME, C.INTEGER_TYPE__INT8__NAME)


def integer_type_int16() -> TTerm:
    """The int16 IntegerType variant."""
    return Phantoms.inject_unit(C.INTEGER_TYPE__NAME, C.INTEGER_TYPE__INT16__NAME)


def integer_type_int32() -> TTerm:
    """The int32 IntegerType variant."""
    return Phantoms.inject_unit(C.INTEGER_TYPE__NAME, C.INTEGER_TYPE__INT32__NAME)


def integer_type_int64() -> TTerm:
    """The int64 IntegerType variant."""
    return Phantoms.inject_unit(C.INTEGER_TYPE__NAME, C.INTEGER_TYPE__INT64__NAME)


def integer_type_uint8() -> TTerm:
    """The uint8 IntegerType variant."""
    return Phantoms.inject_unit(C.INTEGER_TYPE__NAME, C.INTEGER_TYPE__UINT8__NAME)


def integer_type_uint16() -> TTerm:
    """The uint16 IntegerType variant."""
    return Phantoms.inject_unit(C.INTEGER_TYPE__NAME, C.INTEGER_TYPE__UINT16__NAME)


def integer_type_uint32() -> TTerm:
    """The uint32 IntegerType variant."""
    return Phantoms.inject_unit(C.INTEGER_TYPE__NAME, C.INTEGER_TYPE__UINT32__NAME)


def integer_type_uint64() -> TTerm:
    """The uint64 IntegerType variant."""
    return Phantoms.inject_unit(C.INTEGER_TYPE__NAME, C.INTEGER_TYPE__UINT64__NAME)


# ============================================================
# IntegerValue
# ============================================================

def integer_value_bigint(v: TTerm) -> TTerm:
    """Inject a bigint into an IntegerValue."""
    return Phantoms.inject(C.INTEGER_VALUE__NAME, C.INTEGER_VALUE__BIGINT__NAME, v)


def integer_value_int8(v: TTerm) -> TTerm:
    """Inject an int8 into an IntegerValue."""
    return Phantoms.inject(C.INTEGER_VALUE__NAME, C.INTEGER_VALUE__INT8__NAME, v)


def integer_value_int16(v: TTerm) -> TTerm:
    """Inject an int16 into an IntegerValue."""
    return Phantoms.inject(C.INTEGER_VALUE__NAME, C.INTEGER_VALUE__INT16__NAME, v)


def integer_value_int32(v: TTerm) -> TTerm:
    """Inject an int32 into an IntegerValue."""
    return Phantoms.inject(C.INTEGER_VALUE__NAME, C.INTEGER_VALUE__INT32__NAME, v)


def integer_value_int64(v: TTerm) -> TTerm:
    """Inject an int64 into an IntegerValue."""
    return Phantoms.inject(C.INTEGER_VALUE__NAME, C.INTEGER_VALUE__INT64__NAME, v)


def integer_value_uint8(v: TTerm) -> TTerm:
    """Inject a uint8 into an IntegerValue."""
    return Phantoms.inject(C.INTEGER_VALUE__NAME, C.INTEGER_VALUE__UINT8__NAME, v)


def integer_value_uint16(v: TTerm) -> TTerm:
    """Inject a uint16 into an IntegerValue."""
    return Phantoms.inject(C.INTEGER_VALUE__NAME, C.INTEGER_VALUE__UINT16__NAME, v)


def integer_value_uint32(v: TTerm) -> TTerm:
    """Inject a uint32 into an IntegerValue."""
    return Phantoms.inject(C.INTEGER_VALUE__NAME, C.INTEGER_VALUE__UINT32__NAME, v)


def integer_value_uint64(v: TTerm) -> TTerm:
    """Inject a uint64 into an IntegerValue."""
    return Phantoms.inject(C.INTEGER_VALUE__NAME, C.INTEGER_VALUE__UINT64__NAME, v)


# ============================================================
# Lambda
# ============================================================

def lambda_(parameter: TTerm, mdom: TTerm, body: TTerm) -> TTerm:
    """Construct a Lambda."""
    return Phantoms.record(C.LAMBDA__NAME, [
        Phantoms.field(C.LAMBDA__PARAMETER__NAME, parameter),
        Phantoms.field(C.LAMBDA__DOMAIN__NAME, mdom),
        Phantoms.field(C.LAMBDA__BODY__NAME, body)])


def lambda_parameter(l: TTerm) -> TTerm:
    """Get the parameter of a Lambda."""
    return Phantoms.apply(Phantoms.project(C.LAMBDA__NAME, C.LAMBDA__PARAMETER__NAME), l)


def lambda_body(l: TTerm) -> TTerm:
    """Get the body of a Lambda."""
    return Phantoms.apply(Phantoms.project(C.LAMBDA__NAME, C.LAMBDA__BODY__NAME), l)


def lambda_domain(l: TTerm) -> TTerm:
    """Get the domain of a Lambda."""
    return Phantoms.apply(Phantoms.project(C.LAMBDA__NAME, C.LAMBDA__DOMAIN__NAME), l)


def lambda_with_body(l: TTerm, body: TTerm) -> TTerm:
    """Return a Lambda with a new body."""
    return lambda_(lambda_parameter(l), lambda_domain(l), body)


# ============================================================
# Let
# ============================================================

def let_(bindings: TTerm, body: TTerm) -> TTerm:
    """Construct a Let."""
    return Phantoms.record(C.LET__NAME, [
        Phantoms.field(C.LET__BINDINGS__NAME, bindings),
        Phantoms.field(C.LET__BODY__NAME, body)])


def let_bindings(l: TTerm) -> TTerm:
    """Get the bindings of a Let."""
    return Phantoms.apply(Phantoms.project(C.LET__NAME, C.LET__BINDINGS__NAME), l)


def let_body(l: TTerm) -> TTerm:
    """Get the body of a Let."""
    return Phantoms.apply(Phantoms.project(C.LET__NAME, C.LET__BODY__NAME), l)


def let_with_body(l: TTerm, body: TTerm) -> TTerm:
    """Return a Let with a new body."""
    return let_(let_bindings(l), body)


# ============================================================
# Literal
# ============================================================

def literal_binary(v: TTerm) -> TTerm:
    """Inject binary data into a Literal."""
    return Phantoms.inject(C.LITERAL__NAME, C.LITERAL__BINARY__NAME, v)


def literal_boolean(v: TTerm) -> TTerm:
    """Inject a boolean into a Literal."""
    return Phantoms.inject(C.LITERAL__NAME, C.LITERAL__BOOLEAN__NAME, v)


def literal_float(v: TTerm) -> TTerm:
    """Inject a FloatValue into a Literal."""
    return Phantoms.inject(C.LITERAL__NAME, C.LITERAL__FLOAT__NAME, v)


def literal_integer(v: TTerm) -> TTerm:
    """Inject an IntegerValue into a Literal."""
    return Phantoms.inject(C.LITERAL__NAME, C.LITERAL__INTEGER__NAME, v)


def literal_string(v: TTerm) -> TTerm:
    """Inject a string into a Literal."""
    return Phantoms.inject(C.LITERAL__NAME, C.LITERAL__STRING__NAME, v)


# ============================================================
# LiteralType
# ============================================================

def literal_type_binary() -> TTerm:
    """The binary LiteralType variant."""
    return Phantoms.inject_unit(C.LITERAL_TYPE__NAME, C.LITERAL_TYPE__BINARY__NAME)


def literal_type_boolean() -> TTerm:
    """The boolean LiteralType variant."""
    return Phantoms.inject_unit(C.LITERAL_TYPE__NAME, C.LITERAL_TYPE__BOOLEAN__NAME)


def literal_type_float(ft: TTerm) -> TTerm:
    """Inject a FloatType into a LiteralType."""
    return Phantoms.inject(C.LITERAL_TYPE__NAME, C.LITERAL_TYPE__FLOAT__NAME, ft)


def literal_type_integer(it: TTerm) -> TTerm:
    """Inject an IntegerType into a LiteralType."""
    return Phantoms.inject(C.LITERAL_TYPE__NAME, C.LITERAL_TYPE__INTEGER__NAME, it)


def literal_type_string() -> TTerm:
    """The string LiteralType variant."""
    return Phantoms.inject_unit(C.LITERAL_TYPE__NAME, C.LITERAL_TYPE__STRING__NAME)


# ============================================================
# MapType
# ============================================================

def map_type(keys: TTerm, values: TTerm) -> TTerm:
    """Construct a MapType."""
    return Phantoms.record(C.MAP_TYPE__NAME, [
        Phantoms.field(C.MAP_TYPE__KEYS__NAME, keys),
        Phantoms.field(C.MAP_TYPE__VALUES__NAME, values)])


def map_type_keys(mt: TTerm) -> TTerm:
    """Get the keys type of a MapType."""
    return Phantoms.apply(Phantoms.project(C.MAP_TYPE__NAME, C.MAP_TYPE__KEYS__NAME), mt)


def map_type_values(mt: TTerm) -> TTerm:
    """Get the values type of a MapType."""
    return Phantoms.apply(Phantoms.project(C.MAP_TYPE__NAME, C.MAP_TYPE__VALUES__NAME), mt)


# ============================================================
# Projection
# ============================================================

def projection(tname: TTerm, fname: TTerm) -> TTerm:
    """Construct a Projection."""
    return Phantoms.record(C.PROJECTION__NAME, [
        Phantoms.field(C.PROJECTION__TYPE_NAME__NAME, tname),
        Phantoms.field(C.PROJECTION__FIELD__NAME, fname)])


def projection_type_name(p: TTerm) -> TTerm:
    """Get the typeName of a Projection."""
    return Phantoms.apply(Phantoms.project(C.PROJECTION__NAME, C.PROJECTION__TYPE_NAME__NAME), p)


def projection_field(p: TTerm) -> TTerm:
    """Get the field name of a Projection."""
    return Phantoms.apply(Phantoms.project(C.PROJECTION__NAME, C.PROJECTION__FIELD__NAME), p)


# ============================================================
# Record
# ============================================================

def record(type_name: TTerm, fields: TTerm) -> TTerm:
    """Construct a Record."""
    return Phantoms.record(C.RECORD__NAME, [
        Phantoms.field(C.RECORD__TYPE_NAME__NAME, type_name),
        Phantoms.field(C.RECORD__FIELDS__NAME, fields)])


def record_type_name(r: TTerm) -> TTerm:
    """Get the typeName of a Record."""
    return Phantoms.apply(Phantoms.project(C.RECORD__NAME, C.RECORD__TYPE_NAME__NAME), r)


def record_fields(r: TTerm) -> TTerm:
    """Get the fields of a Record."""
    return Phantoms.apply(Phantoms.project(C.RECORD__NAME, C.RECORD__FIELDS__NAME), r)


# ============================================================
# RowType
# ============================================================

def row_type(type_name: TTerm, fields: TTerm) -> TTerm:
    """Construct a RowType."""
    return Phantoms.record(C.ROW_TYPE__NAME, [
        Phantoms.field(C.ROW_TYPE__TYPE_NAME__NAME, type_name),
        Phantoms.field(C.ROW_TYPE__FIELDS__NAME, fields)])


def row_type_type_name(rt: TTerm) -> TTerm:
    """Get the typeName of a RowType."""
    return Phantoms.apply(Phantoms.project(C.ROW_TYPE__NAME, C.ROW_TYPE__TYPE_NAME__NAME), rt)


def row_type_fields(rt: TTerm) -> TTerm:
    """Get the fields of a RowType."""
    return Phantoms.apply(Phantoms.project(C.ROW_TYPE__NAME, C.ROW_TYPE__FIELDS__NAME), rt)


# ============================================================
# Term variants (injection functions)
# ============================================================

def term_annotated(at: TTerm) -> TTerm:
    """Inject an AnnotatedTerm into a Term."""
    return Phantoms.inject(C.TERM__NAME, C.TERM__ANNOTATED__NAME, at)


def term_application(app: TTerm) -> TTerm:
    """Inject an Application into a Term."""
    return Phantoms.inject(C.TERM__NAME, C.TERM__APPLICATION__NAME, app)


def term_either(e: TTerm) -> TTerm:
    """Inject an Either into a Term."""
    return Phantoms.inject(C.TERM__NAME, C.TERM__EITHER__NAME, e)


def term_function(f: TTerm) -> TTerm:
    """Inject a Function into a Term."""
    return Phantoms.inject(C.TERM__NAME, C.TERM__FUNCTION__NAME, f)


def term_let(l: TTerm) -> TTerm:
    """Inject a Let into a Term."""
    return Phantoms.inject(C.TERM__NAME, C.TERM__LET__NAME, l)


def term_list(l: TTerm) -> TTerm:
    """Inject a list into a Term."""
    return Phantoms.inject(C.TERM__NAME, C.TERM__LIST__NAME, l)


def term_literal(lit: TTerm) -> TTerm:
    """Inject a Literal into a Term."""
    return Phantoms.inject(C.TERM__NAME, C.TERM__LITERAL__NAME, lit)


def term_map(m: TTerm) -> TTerm:
    """Inject a map into a Term."""
    return Phantoms.inject(C.TERM__NAME, C.TERM__MAP__NAME, m)


def term_maybe(m: TTerm) -> TTerm:
    """Inject a Maybe into a Term."""
    return Phantoms.inject(C.TERM__NAME, C.TERM__MAYBE__NAME, m)


def term_pair(p: TTerm) -> TTerm:
    """Inject a pair into a Term."""
    return Phantoms.inject(C.TERM__NAME, C.TERM__PAIR__NAME, p)


def term_record(r: TTerm) -> TTerm:
    """Inject a Record into a Term."""
    return Phantoms.inject(C.TERM__NAME, C.TERM__RECORD__NAME, r)


def term_set(s: TTerm) -> TTerm:
    """Inject a set into a Term."""
    return Phantoms.inject(C.TERM__NAME, C.TERM__SET__NAME, s)


def term_type_application(ta: TTerm) -> TTerm:
    """Inject a TypeApplicationTerm into a Term."""
    return Phantoms.inject(C.TERM__NAME, C.TERM__TYPE_APPLICATION__NAME, ta)


def term_type_lambda(tl: TTerm) -> TTerm:
    """Inject a TypeLambda into a Term."""
    return Phantoms.inject(C.TERM__NAME, C.TERM__TYPE_LAMBDA__NAME, tl)


def term_union(inj: TTerm) -> TTerm:
    """Inject an Injection into a Term (union variant)."""
    return Phantoms.inject(C.TERM__NAME, C.TERM__UNION__NAME, inj)


def term_unit() -> TTerm:
    """The unit Term variant."""
    return Phantoms.inject_unit(C.TERM__NAME, C.TERM__UNIT__NAME)


def term_variable(name: TTerm) -> TTerm:
    """Inject a Name into a Term (variable variant)."""
    return Phantoms.inject(C.TERM__NAME, C.TERM__VARIABLE__NAME, name)


def term_wrap(wt: TTerm) -> TTerm:
    """Inject a WrappedTerm into a Term."""
    return Phantoms.inject(C.TERM__NAME, C.TERM__WRAP__NAME, wt)


# ============================================================
# Type variants (injection functions)
# ============================================================

def type_annotated(at: TTerm) -> TTerm:
    """Inject an AnnotatedType into a Type."""
    return Phantoms.inject(C.TYPE__NAME, C.TYPE__ANNOTATED__NAME, at)


def type_application(app: TTerm) -> TTerm:
    """Inject an ApplicationType into a Type."""
    return Phantoms.inject(C.TYPE__NAME, C.TYPE__APPLICATION__NAME, app)


def type_either(et: TTerm) -> TTerm:
    """Inject an EitherType into a Type."""
    return Phantoms.inject(C.TYPE__NAME, C.TYPE__EITHER__NAME, et)


def type_forall(ft: TTerm) -> TTerm:
    """Inject a ForallType into a Type."""
    return Phantoms.inject(C.TYPE__NAME, C.TYPE__FORALL__NAME, ft)


def type_function(ft: TTerm) -> TTerm:
    """Inject a FunctionType into a Type."""
    return Phantoms.inject(C.TYPE__NAME, C.TYPE__FUNCTION__NAME, ft)


def type_list(t: TTerm) -> TTerm:
    """Inject a Type into a Type (list variant)."""
    return Phantoms.inject(C.TYPE__NAME, C.TYPE__LIST__NAME, t)


def type_literal(lt: TTerm) -> TTerm:
    """Inject a LiteralType into a Type."""
    return Phantoms.inject(C.TYPE__NAME, C.TYPE__LITERAL__NAME, lt)


def type_map(mt: TTerm) -> TTerm:
    """Inject a MapType into a Type."""
    return Phantoms.inject(C.TYPE__NAME, C.TYPE__MAP__NAME, mt)


def type_maybe(t: TTerm) -> TTerm:
    """Inject a Type into a Type (maybe variant)."""
    return Phantoms.inject(C.TYPE__NAME, C.TYPE__MAYBE__NAME, t)


def type_pair(pt: TTerm) -> TTerm:
    """Inject a PairType into a Type."""
    return Phantoms.inject(C.TYPE__NAME, C.TYPE__PAIR__NAME, pt)


def type_record(rt: TTerm) -> TTerm:
    """Inject a RowType into a Type (record variant)."""
    return Phantoms.inject(C.TYPE__NAME, C.TYPE__RECORD__NAME, rt)


def type_set(t: TTerm) -> TTerm:
    """Inject a Type into a Type (set variant)."""
    return Phantoms.inject(C.TYPE__NAME, C.TYPE__SET__NAME, t)


def type_union(rt: TTerm) -> TTerm:
    """Inject a RowType into a Type (union variant)."""
    return Phantoms.inject(C.TYPE__NAME, C.TYPE__UNION__NAME, rt)


def type_unit() -> TTerm:
    """The unit Type variant."""
    return Phantoms.inject_unit(C.TYPE__NAME, C.TYPE__UNIT__NAME)


def type_variable(name: TTerm) -> TTerm:
    """Inject a Name into a Type (variable variant)."""
    return Phantoms.inject(C.TYPE__NAME, C.TYPE__VARIABLE__NAME, name)


def type_wrap(wt: TTerm) -> TTerm:
    """Inject a WrappedType into a Type."""
    return Phantoms.inject(C.TYPE__NAME, C.TYPE__WRAP__NAME, wt)


# ============================================================
# TypeApplicationTerm
# ============================================================

def type_application_term(body: TTerm, type_: TTerm) -> TTerm:
    """Construct a TypeApplicationTerm."""
    return Phantoms.record(C.TYPE_APPLICATION_TERM__NAME, [
        Phantoms.field(C.TYPE_APPLICATION_TERM__BODY__NAME, body),
        Phantoms.field(C.TYPE_APPLICATION_TERM__TYPE__NAME, type_)])


def type_application_term_body(tt: TTerm) -> TTerm:
    """Get the body of a TypeApplicationTerm."""
    return Phantoms.apply(Phantoms.project(C.TYPE_APPLICATION_TERM__NAME, C.TYPE_APPLICATION_TERM__BODY__NAME), tt)


def type_application_term_type(tt: TTerm) -> TTerm:
    """Get the type of a TypeApplicationTerm."""
    return Phantoms.apply(Phantoms.project(C.TYPE_APPLICATION_TERM__NAME, C.TYPE_APPLICATION_TERM__TYPE__NAME), tt)


# ============================================================
# TypeLambda
# ============================================================

def type_lambda(parameter: TTerm, body: TTerm) -> TTerm:
    """Construct a TypeLambda."""
    return Phantoms.record(C.TYPE_LAMBDA__NAME, [
        Phantoms.field(C.TYPE_LAMBDA__PARAMETER__NAME, parameter),
        Phantoms.field(C.TYPE_LAMBDA__BODY__NAME, body)])


def type_lambda_parameter(tl: TTerm) -> TTerm:
    """Get the parameter of a TypeLambda."""
    return Phantoms.apply(Phantoms.project(C.TYPE_LAMBDA__NAME, C.TYPE_LAMBDA__PARAMETER__NAME), tl)


def type_lambda_body(tl: TTerm) -> TTerm:
    """Get the body of a TypeLambda."""
    return Phantoms.apply(Phantoms.project(C.TYPE_LAMBDA__NAME, C.TYPE_LAMBDA__BODY__NAME), tl)


def type_lambda_with_body(tl: TTerm, body: TTerm) -> TTerm:
    """Return a TypeLambda with a new body."""
    return type_lambda(type_lambda_parameter(tl), body)


# ============================================================
# TypeScheme
# ============================================================

def type_scheme(variables: TTerm, body: TTerm, constraints: TTerm) -> TTerm:
    """Construct a TypeScheme."""
    return Phantoms.record(C.TYPE_SCHEME__NAME, [
        Phantoms.field(C.TYPE_SCHEME__VARIABLES__NAME, variables),
        Phantoms.field(C.TYPE_SCHEME__TYPE__NAME, body),
        Phantoms.field(C.TYPE_SCHEME__CONSTRAINTS__NAME, constraints)])


def type_scheme_variables(ts: TTerm) -> TTerm:
    """Get the variables of a TypeScheme."""
    return Phantoms.apply(Phantoms.project(C.TYPE_SCHEME__NAME, C.TYPE_SCHEME__VARIABLES__NAME), ts)


def type_scheme_type(ts: TTerm) -> TTerm:
    """Get the type of a TypeScheme."""
    return Phantoms.apply(Phantoms.project(C.TYPE_SCHEME__NAME, C.TYPE_SCHEME__TYPE__NAME), ts)


def type_scheme_constraints(ts: TTerm) -> TTerm:
    """Get the constraints of a TypeScheme."""
    return Phantoms.apply(Phantoms.project(C.TYPE_SCHEME__NAME, C.TYPE_SCHEME__CONSTRAINTS__NAME), ts)


# ============================================================
# TypeVariableMetadata
# ============================================================

def type_variable_metadata(classes: TTerm) -> TTerm:
    """Construct TypeVariableMetadata."""
    return Phantoms.record(C.TYPE_VARIABLE_METADATA__NAME, [
        Phantoms.field(C.TYPE_VARIABLE_METADATA__CLASSES__NAME, classes)])


def type_variable_metadata_classes(meta: TTerm) -> TTerm:
    """Get the classes of TypeVariableMetadata."""
    return Phantoms.apply(
        Phantoms.project(C.TYPE_VARIABLE_METADATA__NAME, C.TYPE_VARIABLE_METADATA__CLASSES__NAME), meta)


# ============================================================
# WrappedTerm
# ============================================================

def wrapped_term(type_name: TTerm, body: TTerm) -> TTerm:
    """Construct a WrappedTerm."""
    return Phantoms.record(C.WRAPPED_TERM__NAME, [
        Phantoms.field(C.WRAPPED_TERM__TYPE_NAME__NAME, type_name),
        Phantoms.field(C.WRAPPED_TERM__BODY__NAME, body)])


def wrapped_term_type_name(wt: TTerm) -> TTerm:
    """Get the typeName of a WrappedTerm."""
    return Phantoms.apply(Phantoms.project(C.WRAPPED_TERM__NAME, C.WRAPPED_TERM__TYPE_NAME__NAME), wt)


def wrapped_term_body(wt: TTerm) -> TTerm:
    """Get the body of a WrappedTerm."""
    return Phantoms.apply(Phantoms.project(C.WRAPPED_TERM__NAME, C.WRAPPED_TERM__BODY__NAME), wt)


# ============================================================
# WrappedType
# ============================================================

def wrapped_type(type_name: TTerm, body: TTerm) -> TTerm:
    """Construct a WrappedType."""
    return Phantoms.record(C.WRAPPED_TYPE__NAME, [
        Phantoms.field(C.WRAPPED_TYPE__TYPE_NAME__NAME, type_name),
        Phantoms.field(C.WRAPPED_TYPE__BODY__NAME, body)])


def wrapped_type_type_name(wt: TTerm) -> TTerm:
    """Get the typeName of a WrappedType."""
    return Phantoms.apply(Phantoms.project(C.WRAPPED_TYPE__NAME, C.WRAPPED_TYPE__TYPE_NAME__NAME), wt)


def wrapped_type_body(wt: TTerm) -> TTerm:
    """Get the body of a WrappedType."""
    return Phantoms.apply(Phantoms.project(C.WRAPPED_TYPE__NAME, C.WRAPPED_TYPE__BODY__NAME), wt)


# ============================================================
# Name helpers
# ============================================================

def name(s: TTerm) -> TTerm:
    """Wrap a string as a Name."""
    return Phantoms.wrap(C.NAME__NAME, s)


def name_lift(n: Name) -> TTerm:
    """Lift a Python Name value into a TTerm Name."""
    return Phantoms.wrap(C.NAME__NAME, Phantoms.string(n.value))


def un_name(n: TTerm) -> TTerm:
    """Unwrap a Name to its underlying string."""
    return Phantoms.apply(Phantoms.unwrap(C.NAME__NAME), n)


def un_namespace(ns: TTerm) -> TTerm:
    """Unwrap a Namespace to its underlying string."""
    _NAMESPACE_NAME = Name("hydra.module.Namespace")
    return Phantoms.apply(Phantoms.unwrap(_NAMESPACE_NAME), ns)


# ============================================================
# Non-schema helpers
# ============================================================

def equal_name(left: TTerm, right: TTerm) -> TTerm:
    """Check equality of two Names by comparing their string values."""
    import hydra.dsl.meta.lib.equality as Equality
    return Equality.equal(un_name(left), un_name(right))


def equal_name_list(lefts: TTerm, rights: TTerm) -> TTerm:
    """Check equality of two Name lists."""
    import hydra.dsl.meta.lib.equality as Equality
    import hydra.dsl.meta.lib.lists as Lists
    import hydra.dsl.meta.lib.logic as Logic
    equal_name_fn = Phantoms.lambdas(
        ["left", "right"],
        Equality.equal(un_name(Phantoms.var("left")), un_name(Phantoms.var("right"))))
    return Logic.and_(
        Equality.equal(Lists.length(lefts), Lists.length(rights)),
        Logic.ands(Lists.zip_with(equal_name_fn, lefts, rights)))


def field_with_term_value(t: TTerm, ft: TTerm) -> TTerm:
    """Return a Field with a replacement term (same name)."""
    return field(field_name(ft), t)


def field_type_with_type_value(ft: TTerm, t: TTerm) -> TTerm:
    """Return a FieldType with a replacement type (same name)."""
    return field_type(field_type_name(ft), t)


# ============================================================
# Convenience: encoded Term constructors
# (Used in meta-level encoding, e.g. for building terms that represent terms)
# ============================================================

def int32_term(v: int) -> TTerm:
    """Create an encoded int32 Term value."""
    return term_literal(literal_integer(integer_value_int32(Phantoms.int32(v))))


def string_term(s: str) -> TTerm:
    """Create an encoded string Term value."""
    return term_literal(literal_string(Phantoms.string(s)))


def false_term() -> TTerm:
    """Create an encoded boolean false Term value."""
    return term_literal(literal_boolean(Phantoms.false))
