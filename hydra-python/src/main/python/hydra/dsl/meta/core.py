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
    return Phantoms.record(C.AnnotatedTerm.TYPE_, [
        Phantoms.field(C.AnnotatedTerm.BODY, body),
        Phantoms.field(C.AnnotatedTerm.ANNOTATION, annotation)])


def annotated_term_body(at: TTerm) -> TTerm:
    """Get the body of an AnnotatedTerm."""
    return Phantoms.apply(Phantoms.project(C.AnnotatedTerm.TYPE_, C.AnnotatedTerm.BODY), at)


def annotated_term_annotation(at: TTerm) -> TTerm:
    """Get the annotation of an AnnotatedTerm."""
    return Phantoms.apply(Phantoms.project(C.AnnotatedTerm.TYPE_, C.AnnotatedTerm.ANNOTATION), at)


def annotated_term_with_body(at: TTerm, body: TTerm) -> TTerm:
    """Return an AnnotatedTerm with a new body."""
    return annotated_term(body, annotated_term_annotation(at))


# ============================================================
# AnnotatedType
# ============================================================

def annotated_type(body: TTerm, annotation: TTerm) -> TTerm:
    """Construct an AnnotatedType."""
    return Phantoms.record(C.AnnotatedType.TYPE_, [
        Phantoms.field(C.AnnotatedType.BODY, body),
        Phantoms.field(C.AnnotatedType.ANNOTATION, annotation)])


def annotated_type_body(at: TTerm) -> TTerm:
    """Get the body of an AnnotatedType."""
    return Phantoms.apply(Phantoms.project(C.AnnotatedType.TYPE_, C.AnnotatedType.BODY), at)


def annotated_type_annotation(at: TTerm) -> TTerm:
    """Get the annotation of an AnnotatedType."""
    return Phantoms.apply(Phantoms.project(C.AnnotatedType.TYPE_, C.AnnotatedType.ANNOTATION), at)


# ============================================================
# Application
# ============================================================

def application(function: TTerm, argument: TTerm) -> TTerm:
    """Construct an Application."""
    return Phantoms.record(C.Application.TYPE_, [
        Phantoms.field(C.Application.FUNCTION, function),
        Phantoms.field(C.Application.ARGUMENT, argument)])


def application_function(app: TTerm) -> TTerm:
    """Get the function of an Application."""
    return Phantoms.apply(Phantoms.project(C.Application.TYPE_, C.Application.FUNCTION), app)


def application_argument(app: TTerm) -> TTerm:
    """Get the argument of an Application."""
    return Phantoms.apply(Phantoms.project(C.Application.TYPE_, C.Application.ARGUMENT), app)


# ============================================================
# ApplicationType
# ============================================================

def application_type(function: TTerm, argument: TTerm) -> TTerm:
    """Construct an ApplicationType."""
    return Phantoms.record(C.ApplicationType.TYPE_, [
        Phantoms.field(C.ApplicationType.FUNCTION, function),
        Phantoms.field(C.ApplicationType.ARGUMENT, argument)])


def application_type_function(app: TTerm) -> TTerm:
    """Get the function of an ApplicationType."""
    return Phantoms.apply(Phantoms.project(C.ApplicationType.TYPE_, C.ApplicationType.FUNCTION), app)


def application_type_argument(app: TTerm) -> TTerm:
    """Get the argument of an ApplicationType."""
    return Phantoms.apply(Phantoms.project(C.ApplicationType.TYPE_, C.ApplicationType.ARGUMENT), app)


# ============================================================
# Binding
# ============================================================

def binding(name_term: TTerm, term: TTerm, mtype: TTerm) -> TTerm:
    """Construct a Binding."""
    return Phantoms.record(C.Binding.TYPE_, [
        Phantoms.field(C.Binding.NAME, name_term),
        Phantoms.field(C.Binding.TERM, term),
        Phantoms.field(C.Binding.TYPE, mtype)])


def binding_name(b: TTerm) -> TTerm:
    """Get the name of a Binding."""
    return Phantoms.apply(Phantoms.project(C.Binding.TYPE_, C.Binding.NAME), b)


def binding_term(b: TTerm) -> TTerm:
    """Get the term of a Binding."""
    return Phantoms.apply(Phantoms.project(C.Binding.TYPE_, C.Binding.TERM), b)


def binding_type(b: TTerm) -> TTerm:
    """Get the type of a Binding."""
    return Phantoms.apply(Phantoms.project(C.Binding.TYPE_, C.Binding.TYPE), b)


def binding_with_term(b: TTerm, term: TTerm) -> TTerm:
    """Return a Binding with a new term."""
    return binding(binding_name(b), term, binding_type(b))


# ============================================================
# CaseStatement
# ============================================================

def case_statement(type_name: TTerm, default_term: TTerm, cases: TTerm) -> TTerm:
    """Construct a CaseStatement."""
    return Phantoms.record(C.CaseStatement.TYPE_, [
        Phantoms.field(C.CaseStatement.TYPE_NAME, type_name),
        Phantoms.field(C.CaseStatement.DEFAULT, default_term),
        Phantoms.field(C.CaseStatement.CASES, cases)])


def case_statement_type_name(cs: TTerm) -> TTerm:
    """Get the typeName of a CaseStatement."""
    return Phantoms.apply(Phantoms.project(C.CaseStatement.TYPE_, C.CaseStatement.TYPE_NAME), cs)


def case_statement_default(cs: TTerm) -> TTerm:
    """Get the default of a CaseStatement."""
    return Phantoms.apply(Phantoms.project(C.CaseStatement.TYPE_, C.CaseStatement.DEFAULT), cs)


def case_statement_cases(cs: TTerm) -> TTerm:
    """Get the cases of a CaseStatement."""
    return Phantoms.apply(Phantoms.project(C.CaseStatement.TYPE_, C.CaseStatement.CASES), cs)


# ============================================================
# EitherType
# ============================================================

def either_type(left: TTerm, right: TTerm) -> TTerm:
    """Construct an EitherType."""
    return Phantoms.record(C.EitherType.TYPE_, [
        Phantoms.field(C.EitherType.LEFT, left),
        Phantoms.field(C.EitherType.RIGHT, right)])


def either_type_left(et: TTerm) -> TTerm:
    """Get the left type of an EitherType."""
    return Phantoms.apply(Phantoms.project(C.EitherType.TYPE_, C.EitherType.LEFT), et)


def either_type_right(et: TTerm) -> TTerm:
    """Get the right type of an EitherType."""
    return Phantoms.apply(Phantoms.project(C.EitherType.TYPE_, C.EitherType.RIGHT), et)


# ============================================================
# PairType
# ============================================================

def pair_type(first: TTerm, second: TTerm) -> TTerm:
    """Construct a PairType."""
    return Phantoms.record(C.PairType.TYPE_, [
        Phantoms.field(C.PairType.FIRST, first),
        Phantoms.field(C.PairType.SECOND, second)])


def pair_type_first(pt: TTerm) -> TTerm:
    """Get the first type of a PairType."""
    return Phantoms.apply(Phantoms.project(C.PairType.TYPE_, C.PairType.FIRST), pt)


def pair_type_second(pt: TTerm) -> TTerm:
    """Get the second type of a PairType."""
    return Phantoms.apply(Phantoms.project(C.PairType.TYPE_, C.PairType.SECOND), pt)


# ============================================================
# Elimination
# ============================================================

def elimination_record(proj: TTerm) -> TTerm:
    """Inject a Projection into an Elimination."""
    return Phantoms.inject(C.Elimination.TYPE_, C.Elimination.RECORD, proj)


def elimination_union(cs: TTerm) -> TTerm:
    """Inject a CaseStatement into an Elimination."""
    return Phantoms.inject(C.Elimination.TYPE_, C.Elimination.UNION, cs)


def elimination_wrap(name: TTerm) -> TTerm:
    """Inject a Name into an Elimination (wrap variant)."""
    return Phantoms.inject(C.Elimination.TYPE_, C.Elimination.WRAP, name)


# ============================================================
# Field
# ============================================================

def field(name: TTerm, term: TTerm) -> TTerm:
    """Construct a Field."""
    return Phantoms.record(C.Field.TYPE_, [
        Phantoms.field(C.Field.NAME, name),
        Phantoms.field(C.Field.TERM, term)])


def field_name(f: TTerm) -> TTerm:
    """Get the name of a Field."""
    return Phantoms.apply(Phantoms.project(C.Field.TYPE_, C.Field.NAME), f)


def field_term(f: TTerm) -> TTerm:
    """Get the term of a Field."""
    return Phantoms.apply(Phantoms.project(C.Field.TYPE_, C.Field.TERM), f)


def field_with_term(t: TTerm, ft: TTerm) -> TTerm:
    """Return a Field with a new term."""
    return field(field_name(ft), t)


# ============================================================
# FieldType
# ============================================================

def field_type(name: TTerm, typ: TTerm) -> TTerm:
    """Construct a FieldType."""
    return Phantoms.record(C.FieldType.TYPE_, [
        Phantoms.field(C.FieldType.NAME, name),
        Phantoms.field(C.FieldType.TYPE, typ)])


def field_type_name(ft: TTerm) -> TTerm:
    """Get the name of a FieldType."""
    return Phantoms.apply(Phantoms.project(C.FieldType.TYPE_, C.FieldType.NAME), ft)


def field_type_type(ft: TTerm) -> TTerm:
    """Get the type of a FieldType."""
    return Phantoms.apply(Phantoms.project(C.FieldType.TYPE_, C.FieldType.TYPE), ft)


def field_type_with_type(ft: TTerm, t: TTerm) -> TTerm:
    """Return a FieldType with a new type."""
    return field_type(field_type_name(ft), t)


# ============================================================
# FloatType
# ============================================================

def float_type_bigfloat() -> TTerm:
    """The bigfloat FloatType variant."""
    return Phantoms.inject_unit(C.FloatType.TYPE_, C.FloatType.BIGFLOAT.value)


def float_type_float32() -> TTerm:
    """The float32 FloatType variant."""
    return Phantoms.inject_unit(C.FloatType.TYPE_, C.FloatType.FLOAT32.value)


def float_type_float64() -> TTerm:
    """The float64 FloatType variant."""
    return Phantoms.inject_unit(C.FloatType.TYPE_, C.FloatType.FLOAT64.value)


# ============================================================
# FloatValue
# ============================================================

def float_value_bigfloat(v: TTerm) -> TTerm:
    """Inject a bigfloat into a FloatValue."""
    return Phantoms.inject(C.FloatValue.TYPE_, C.FloatValue.BIGFLOAT, v)


def float_value_float32(v: TTerm) -> TTerm:
    """Inject a float32 into a FloatValue."""
    return Phantoms.inject(C.FloatValue.TYPE_, C.FloatValue.FLOAT32, v)


def float_value_float64(v: TTerm) -> TTerm:
    """Inject a float64 into a FloatValue."""
    return Phantoms.inject(C.FloatValue.TYPE_, C.FloatValue.FLOAT64, v)


# ============================================================
# ForallType
# ============================================================

def forall_type(parameter: TTerm, body: TTerm) -> TTerm:
    """Construct a ForallType."""
    return Phantoms.record(C.ForallType.TYPE_, [
        Phantoms.field(C.ForallType.PARAMETER, parameter),
        Phantoms.field(C.ForallType.BODY, body)])


def forall_type_parameter(ft: TTerm) -> TTerm:
    """Get the parameter of a ForallType."""
    return Phantoms.apply(Phantoms.project(C.ForallType.TYPE_, C.ForallType.PARAMETER), ft)


def forall_type_body(ft: TTerm) -> TTerm:
    """Get the body of a ForallType."""
    return Phantoms.apply(Phantoms.project(C.ForallType.TYPE_, C.ForallType.BODY), ft)


# ============================================================
# Function
# ============================================================

def function_elimination(elim: TTerm) -> TTerm:
    """Inject an Elimination into a Function."""
    return Phantoms.inject(C.Function.TYPE_, C.Function.ELIMINATION, elim)


def function_lambda(lam: TTerm) -> TTerm:
    """Inject a Lambda into a Function."""
    return Phantoms.inject(C.Function.TYPE_, C.Function.LAMBDA, lam)


def function_primitive(name: TTerm) -> TTerm:
    """Inject a Name into a Function (primitive variant)."""
    return Phantoms.inject(C.Function.TYPE_, C.Function.PRIMITIVE, name)


# ============================================================
# FunctionType
# ============================================================

def function_type(domain: TTerm, codomain: TTerm) -> TTerm:
    """Construct a FunctionType."""
    return Phantoms.record(C.FunctionType.TYPE_, [
        Phantoms.field(C.FunctionType.DOMAIN, domain),
        Phantoms.field(C.FunctionType.CODOMAIN, codomain)])


def function_type_domain(ft: TTerm) -> TTerm:
    """Get the domain of a FunctionType."""
    return Phantoms.apply(Phantoms.project(C.FunctionType.TYPE_, C.FunctionType.DOMAIN), ft)


def function_type_codomain(ft: TTerm) -> TTerm:
    """Get the codomain of a FunctionType."""
    return Phantoms.apply(Phantoms.project(C.FunctionType.TYPE_, C.FunctionType.CODOMAIN), ft)


# ============================================================
# Injection
# ============================================================

def injection(type_name: TTerm, fld: TTerm) -> TTerm:
    """Construct an Injection."""
    return Phantoms.record(C.Injection.TYPE_, [
        Phantoms.field(C.Injection.TYPE_NAME, type_name),
        Phantoms.field(C.Injection.FIELD, fld)])


def injection_type_name(inj: TTerm) -> TTerm:
    """Get the typeName of an Injection."""
    return Phantoms.apply(Phantoms.project(C.Injection.TYPE_, C.Injection.TYPE_NAME), inj)


def injection_field(inj: TTerm) -> TTerm:
    """Get the field of an Injection."""
    return Phantoms.apply(Phantoms.project(C.Injection.TYPE_, C.Injection.FIELD), inj)


# ============================================================
# IntegerType
# ============================================================

def integer_type_bigint() -> TTerm:
    """The bigint IntegerType variant."""
    return Phantoms.inject_unit(C.IntegerType.TYPE_, C.IntegerType.BIGINT.value)


def integer_type_int8() -> TTerm:
    """The int8 IntegerType variant."""
    return Phantoms.inject_unit(C.IntegerType.TYPE_, C.IntegerType.INT8.value)


def integer_type_int16() -> TTerm:
    """The int16 IntegerType variant."""
    return Phantoms.inject_unit(C.IntegerType.TYPE_, C.IntegerType.INT16.value)


def integer_type_int32() -> TTerm:
    """The int32 IntegerType variant."""
    return Phantoms.inject_unit(C.IntegerType.TYPE_, C.IntegerType.INT32.value)


def integer_type_int64() -> TTerm:
    """The int64 IntegerType variant."""
    return Phantoms.inject_unit(C.IntegerType.TYPE_, C.IntegerType.INT64.value)


def integer_type_uint8() -> TTerm:
    """The uint8 IntegerType variant."""
    return Phantoms.inject_unit(C.IntegerType.TYPE_, C.IntegerType.UINT8.value)


def integer_type_uint16() -> TTerm:
    """The uint16 IntegerType variant."""
    return Phantoms.inject_unit(C.IntegerType.TYPE_, C.IntegerType.UINT16.value)


def integer_type_uint32() -> TTerm:
    """The uint32 IntegerType variant."""
    return Phantoms.inject_unit(C.IntegerType.TYPE_, C.IntegerType.UINT32.value)


def integer_type_uint64() -> TTerm:
    """The uint64 IntegerType variant."""
    return Phantoms.inject_unit(C.IntegerType.TYPE_, C.IntegerType.UINT64.value)


# ============================================================
# IntegerValue
# ============================================================

def integer_value_bigint(v: TTerm) -> TTerm:
    """Inject a bigint into an IntegerValue."""
    return Phantoms.inject(C.IntegerValue.TYPE_, C.IntegerValue.BIGINT, v)


def integer_value_int8(v: TTerm) -> TTerm:
    """Inject an int8 into an IntegerValue."""
    return Phantoms.inject(C.IntegerValue.TYPE_, C.IntegerValue.INT8, v)


def integer_value_int16(v: TTerm) -> TTerm:
    """Inject an int16 into an IntegerValue."""
    return Phantoms.inject(C.IntegerValue.TYPE_, C.IntegerValue.INT16, v)


def integer_value_int32(v: TTerm) -> TTerm:
    """Inject an int32 into an IntegerValue."""
    return Phantoms.inject(C.IntegerValue.TYPE_, C.IntegerValue.INT32, v)


def integer_value_int64(v: TTerm) -> TTerm:
    """Inject an int64 into an IntegerValue."""
    return Phantoms.inject(C.IntegerValue.TYPE_, C.IntegerValue.INT64, v)


def integer_value_uint8(v: TTerm) -> TTerm:
    """Inject a uint8 into an IntegerValue."""
    return Phantoms.inject(C.IntegerValue.TYPE_, C.IntegerValue.UINT8, v)


def integer_value_uint16(v: TTerm) -> TTerm:
    """Inject a uint16 into an IntegerValue."""
    return Phantoms.inject(C.IntegerValue.TYPE_, C.IntegerValue.UINT16, v)


def integer_value_uint32(v: TTerm) -> TTerm:
    """Inject a uint32 into an IntegerValue."""
    return Phantoms.inject(C.IntegerValue.TYPE_, C.IntegerValue.UINT32, v)


def integer_value_uint64(v: TTerm) -> TTerm:
    """Inject a uint64 into an IntegerValue."""
    return Phantoms.inject(C.IntegerValue.TYPE_, C.IntegerValue.UINT64, v)


# ============================================================
# Lambda
# ============================================================

def lambda_(parameter: TTerm, mdom: TTerm, body: TTerm) -> TTerm:
    """Construct a Lambda."""
    return Phantoms.record(C.Lambda.TYPE_, [
        Phantoms.field(C.Lambda.PARAMETER, parameter),
        Phantoms.field(C.Lambda.DOMAIN, mdom),
        Phantoms.field(C.Lambda.BODY, body)])


def lambda_parameter(l: TTerm) -> TTerm:
    """Get the parameter of a Lambda."""
    return Phantoms.apply(Phantoms.project(C.Lambda.TYPE_, C.Lambda.PARAMETER), l)


def lambda_body(l: TTerm) -> TTerm:
    """Get the body of a Lambda."""
    return Phantoms.apply(Phantoms.project(C.Lambda.TYPE_, C.Lambda.BODY), l)


def lambda_domain(l: TTerm) -> TTerm:
    """Get the domain of a Lambda."""
    return Phantoms.apply(Phantoms.project(C.Lambda.TYPE_, C.Lambda.DOMAIN), l)


def lambda_with_body(l: TTerm, body: TTerm) -> TTerm:
    """Return a Lambda with a new body."""
    return lambda_(lambda_parameter(l), lambda_domain(l), body)


# ============================================================
# Let
# ============================================================

def let_(bindings: TTerm, body: TTerm) -> TTerm:
    """Construct a Let."""
    return Phantoms.record(C.Let.TYPE_, [
        Phantoms.field(C.Let.BINDINGS, bindings),
        Phantoms.field(C.Let.BODY, body)])


def let_bindings(l: TTerm) -> TTerm:
    """Get the bindings of a Let."""
    return Phantoms.apply(Phantoms.project(C.Let.TYPE_, C.Let.BINDINGS), l)


def let_body(l: TTerm) -> TTerm:
    """Get the body of a Let."""
    return Phantoms.apply(Phantoms.project(C.Let.TYPE_, C.Let.BODY), l)


def let_with_body(l: TTerm, body: TTerm) -> TTerm:
    """Return a Let with a new body."""
    return let_(let_bindings(l), body)


# ============================================================
# Literal
# ============================================================

def literal_binary(v: TTerm) -> TTerm:
    """Inject binary data into a Literal."""
    return Phantoms.inject(C.Literal.TYPE_, C.Literal.BINARY, v)


def literal_boolean(v: TTerm) -> TTerm:
    """Inject a boolean into a Literal."""
    return Phantoms.inject(C.Literal.TYPE_, C.Literal.BOOLEAN, v)


def literal_float(v: TTerm) -> TTerm:
    """Inject a FloatValue into a Literal."""
    return Phantoms.inject(C.Literal.TYPE_, C.Literal.FLOAT, v)


def literal_integer(v: TTerm) -> TTerm:
    """Inject an IntegerValue into a Literal."""
    return Phantoms.inject(C.Literal.TYPE_, C.Literal.INTEGER, v)


def literal_string(v: TTerm) -> TTerm:
    """Inject a string into a Literal."""
    return Phantoms.inject(C.Literal.TYPE_, C.Literal.STRING, v)


# ============================================================
# LiteralType
# ============================================================

def literal_type_binary() -> TTerm:
    """The binary LiteralType variant."""
    return Phantoms.inject_unit(C.LiteralType.TYPE_, C.LiteralType.BINARY)


def literal_type_boolean() -> TTerm:
    """The boolean LiteralType variant."""
    return Phantoms.inject_unit(C.LiteralType.TYPE_, C.LiteralType.BOOLEAN)


def literal_type_float(ft: TTerm) -> TTerm:
    """Inject a FloatType into a LiteralType."""
    return Phantoms.inject(C.LiteralType.TYPE_, C.LiteralType.FLOAT, ft)


def literal_type_integer(it: TTerm) -> TTerm:
    """Inject an IntegerType into a LiteralType."""
    return Phantoms.inject(C.LiteralType.TYPE_, C.LiteralType.INTEGER, it)


def literal_type_string() -> TTerm:
    """The string LiteralType variant."""
    return Phantoms.inject_unit(C.LiteralType.TYPE_, C.LiteralType.STRING)


# ============================================================
# MapType
# ============================================================

def map_type(keys: TTerm, values: TTerm) -> TTerm:
    """Construct a MapType."""
    return Phantoms.record(C.MapType.TYPE_, [
        Phantoms.field(C.MapType.KEYS, keys),
        Phantoms.field(C.MapType.VALUES, values)])


def map_type_keys(mt: TTerm) -> TTerm:
    """Get the keys type of a MapType."""
    return Phantoms.apply(Phantoms.project(C.MapType.TYPE_, C.MapType.KEYS), mt)


def map_type_values(mt: TTerm) -> TTerm:
    """Get the values type of a MapType."""
    return Phantoms.apply(Phantoms.project(C.MapType.TYPE_, C.MapType.VALUES), mt)


# ============================================================
# Projection
# ============================================================

def projection(tname: TTerm, fname: TTerm) -> TTerm:
    """Construct a Projection."""
    return Phantoms.record(C.Projection.TYPE_, [
        Phantoms.field(C.Projection.TYPE_NAME, tname),
        Phantoms.field(C.Projection.FIELD, fname)])


def projection_type_name(p: TTerm) -> TTerm:
    """Get the typeName of a Projection."""
    return Phantoms.apply(Phantoms.project(C.Projection.TYPE_, C.Projection.TYPE_NAME), p)


def projection_field(p: TTerm) -> TTerm:
    """Get the field name of a Projection."""
    return Phantoms.apply(Phantoms.project(C.Projection.TYPE_, C.Projection.FIELD), p)


# ============================================================
# Record
# ============================================================

def record(type_name: TTerm, fields: TTerm) -> TTerm:
    """Construct a Record."""
    return Phantoms.record(C.Record.TYPE_, [
        Phantoms.field(C.Record.TYPE_NAME, type_name),
        Phantoms.field(C.Record.FIELDS, fields)])


def record_type_name(r: TTerm) -> TTerm:
    """Get the typeName of a Record."""
    return Phantoms.apply(Phantoms.project(C.Record.TYPE_, C.Record.TYPE_NAME), r)


def record_fields(r: TTerm) -> TTerm:
    """Get the fields of a Record."""
    return Phantoms.apply(Phantoms.project(C.Record.TYPE_, C.Record.FIELDS), r)


# ============================================================
# RowType
# ============================================================

def row_type(type_name: TTerm, fields: TTerm) -> TTerm:
    """Construct a RowType."""
    return Phantoms.record(C.RowType.TYPE_, [
        Phantoms.field(C.RowType.TYPE_NAME, type_name),
        Phantoms.field(C.RowType.FIELDS, fields)])


def row_type_type_name(rt: TTerm) -> TTerm:
    """Get the typeName of a RowType."""
    return Phantoms.apply(Phantoms.project(C.RowType.TYPE_, C.RowType.TYPE_NAME), rt)


def row_type_fields(rt: TTerm) -> TTerm:
    """Get the fields of a RowType."""
    return Phantoms.apply(Phantoms.project(C.RowType.TYPE_, C.RowType.FIELDS), rt)


# ============================================================
# Term variants (injection functions)
# ============================================================

def term_annotated(at: TTerm) -> TTerm:
    """Inject an AnnotatedTerm into a Term."""
    return Phantoms.inject(C.Term.TYPE_, C.Term.ANNOTATED, at)


def term_application(app: TTerm) -> TTerm:
    """Inject an Application into a Term."""
    return Phantoms.inject(C.Term.TYPE_, C.Term.APPLICATION, app)


def term_either(e: TTerm) -> TTerm:
    """Inject an Either into a Term."""
    return Phantoms.inject(C.Term.TYPE_, C.Term.EITHER, e)


def term_function(f: TTerm) -> TTerm:
    """Inject a Function into a Term."""
    return Phantoms.inject(C.Term.TYPE_, C.Term.FUNCTION, f)


def term_let(l: TTerm) -> TTerm:
    """Inject a Let into a Term."""
    return Phantoms.inject(C.Term.TYPE_, C.Term.LET, l)


def term_list(l: TTerm) -> TTerm:
    """Inject a list into a Term."""
    return Phantoms.inject(C.Term.TYPE_, C.Term.LIST, l)


def term_literal(lit: TTerm) -> TTerm:
    """Inject a Literal into a Term."""
    return Phantoms.inject(C.Term.TYPE_, C.Term.LITERAL, lit)


def term_map(m: TTerm) -> TTerm:
    """Inject a map into a Term."""
    return Phantoms.inject(C.Term.TYPE_, C.Term.MAP, m)


def term_maybe(m: TTerm) -> TTerm:
    """Inject a Maybe into a Term."""
    return Phantoms.inject(C.Term.TYPE_, C.Term.MAYBE, m)


def term_pair(p: TTerm) -> TTerm:
    """Inject a pair into a Term."""
    return Phantoms.inject(C.Term.TYPE_, C.Term.PAIR, p)


def term_record(r: TTerm) -> TTerm:
    """Inject a Record into a Term."""
    return Phantoms.inject(C.Term.TYPE_, C.Term.RECORD, r)


def term_set(s: TTerm) -> TTerm:
    """Inject a set into a Term."""
    return Phantoms.inject(C.Term.TYPE_, C.Term.SET, s)


def term_type_application(ta: TTerm) -> TTerm:
    """Inject a TypeApplicationTerm into a Term."""
    return Phantoms.inject(C.Term.TYPE_, C.Term.TYPE_APPLICATION, ta)


def term_type_lambda(tl: TTerm) -> TTerm:
    """Inject a TypeLambda into a Term."""
    return Phantoms.inject(C.Term.TYPE_, C.Term.TYPE_LAMBDA, tl)


def term_union(inj: TTerm) -> TTerm:
    """Inject an Injection into a Term (union variant)."""
    return Phantoms.inject(C.Term.TYPE_, C.Term.UNION, inj)


def term_unit() -> TTerm:
    """The unit Term variant."""
    return Phantoms.inject_unit(C.Term.TYPE_, C.Term.UNIT)


def term_variable(name: TTerm) -> TTerm:
    """Inject a Name into a Term (variable variant)."""
    return Phantoms.inject(C.Term.TYPE_, C.Term.VARIABLE, name)


def term_wrap(wt: TTerm) -> TTerm:
    """Inject a WrappedTerm into a Term."""
    return Phantoms.inject(C.Term.TYPE_, C.Term.WRAP, wt)


# ============================================================
# Type variants (injection functions)
# ============================================================

def type_annotated(at: TTerm) -> TTerm:
    """Inject an AnnotatedType into a Type."""
    return Phantoms.inject(C.Type.TYPE_, C.Type.ANNOTATED, at)


def type_application(app: TTerm) -> TTerm:
    """Inject an ApplicationType into a Type."""
    return Phantoms.inject(C.Type.TYPE_, C.Type.APPLICATION, app)


def type_either(et: TTerm) -> TTerm:
    """Inject an EitherType into a Type."""
    return Phantoms.inject(C.Type.TYPE_, C.Type.EITHER, et)


def type_forall(ft: TTerm) -> TTerm:
    """Inject a ForallType into a Type."""
    return Phantoms.inject(C.Type.TYPE_, C.Type.FORALL, ft)


def type_function(ft: TTerm) -> TTerm:
    """Inject a FunctionType into a Type."""
    return Phantoms.inject(C.Type.TYPE_, C.Type.FUNCTION, ft)


def type_list(t: TTerm) -> TTerm:
    """Inject a Type into a Type (list variant)."""
    return Phantoms.inject(C.Type.TYPE_, C.Type.LIST, t)


def type_literal(lt: TTerm) -> TTerm:
    """Inject a LiteralType into a Type."""
    return Phantoms.inject(C.Type.TYPE_, C.Type.LITERAL, lt)


def type_map(mt: TTerm) -> TTerm:
    """Inject a MapType into a Type."""
    return Phantoms.inject(C.Type.TYPE_, C.Type.MAP, mt)


def type_maybe(t: TTerm) -> TTerm:
    """Inject a Type into a Type (maybe variant)."""
    return Phantoms.inject(C.Type.TYPE_, C.Type.MAYBE, t)


def type_pair(pt: TTerm) -> TTerm:
    """Inject a PairType into a Type."""
    return Phantoms.inject(C.Type.TYPE_, C.Type.PAIR, pt)


def type_record(rt: TTerm) -> TTerm:
    """Inject a RowType into a Type (record variant)."""
    return Phantoms.inject(C.Type.TYPE_, C.Type.RECORD, rt)


def type_set(t: TTerm) -> TTerm:
    """Inject a Type into a Type (set variant)."""
    return Phantoms.inject(C.Type.TYPE_, C.Type.SET, t)


def type_union(rt: TTerm) -> TTerm:
    """Inject a RowType into a Type (union variant)."""
    return Phantoms.inject(C.Type.TYPE_, C.Type.UNION, rt)


def type_unit() -> TTerm:
    """The unit Type variant."""
    return Phantoms.inject_unit(C.Type.TYPE_, C.Type.UNIT)


def type_variable(name: TTerm) -> TTerm:
    """Inject a Name into a Type (variable variant)."""
    return Phantoms.inject(C.Type.TYPE_, C.Type.VARIABLE, name)


def type_wrap(wt: TTerm) -> TTerm:
    """Inject a WrappedType into a Type."""
    return Phantoms.inject(C.Type.TYPE_, C.Type.WRAP, wt)


# ============================================================
# TypeApplicationTerm
# ============================================================

def type_application_term(body: TTerm, type_: TTerm) -> TTerm:
    """Construct a TypeApplicationTerm."""
    return Phantoms.record(C.TypeApplicationTerm.TYPE_, [
        Phantoms.field(C.TypeApplicationTerm.BODY, body),
        Phantoms.field(C.TypeApplicationTerm.TYPE, type_)])


def type_application_term_body(tt: TTerm) -> TTerm:
    """Get the body of a TypeApplicationTerm."""
    return Phantoms.apply(Phantoms.project(C.TypeApplicationTerm.TYPE_, C.TypeApplicationTerm.BODY), tt)


def type_application_term_type(tt: TTerm) -> TTerm:
    """Get the type of a TypeApplicationTerm."""
    return Phantoms.apply(Phantoms.project(C.TypeApplicationTerm.TYPE_, C.TypeApplicationTerm.TYPE), tt)


# ============================================================
# TypeLambda
# ============================================================

def type_lambda(parameter: TTerm, body: TTerm) -> TTerm:
    """Construct a TypeLambda."""
    return Phantoms.record(C.TypeLambda.TYPE_, [
        Phantoms.field(C.TypeLambda.PARAMETER, parameter),
        Phantoms.field(C.TypeLambda.BODY, body)])


def type_lambda_parameter(tl: TTerm) -> TTerm:
    """Get the parameter of a TypeLambda."""
    return Phantoms.apply(Phantoms.project(C.TypeLambda.TYPE_, C.TypeLambda.PARAMETER), tl)


def type_lambda_body(tl: TTerm) -> TTerm:
    """Get the body of a TypeLambda."""
    return Phantoms.apply(Phantoms.project(C.TypeLambda.TYPE_, C.TypeLambda.BODY), tl)


def type_lambda_with_body(tl: TTerm, body: TTerm) -> TTerm:
    """Return a TypeLambda with a new body."""
    return type_lambda(type_lambda_parameter(tl), body)


# ============================================================
# TypeScheme
# ============================================================

def type_scheme(variables: TTerm, body: TTerm, constraints: TTerm) -> TTerm:
    """Construct a TypeScheme."""
    return Phantoms.record(C.TypeScheme.TYPE_, [
        Phantoms.field(C.TypeScheme.VARIABLES, variables),
        Phantoms.field(C.TypeScheme.TYPE, body),
        Phantoms.field(C.TypeScheme.CONSTRAINTS, constraints)])


def type_scheme_variables(ts: TTerm) -> TTerm:
    """Get the variables of a TypeScheme."""
    return Phantoms.apply(Phantoms.project(C.TypeScheme.TYPE_, C.TypeScheme.VARIABLES), ts)


def type_scheme_type(ts: TTerm) -> TTerm:
    """Get the type of a TypeScheme."""
    return Phantoms.apply(Phantoms.project(C.TypeScheme.TYPE_, C.TypeScheme.TYPE), ts)


def type_scheme_constraints(ts: TTerm) -> TTerm:
    """Get the constraints of a TypeScheme."""
    return Phantoms.apply(Phantoms.project(C.TypeScheme.TYPE_, C.TypeScheme.CONSTRAINTS), ts)


# ============================================================
# TypeVariableMetadata
# ============================================================

def type_variable_metadata(classes: TTerm) -> TTerm:
    """Construct TypeVariableMetadata."""
    return Phantoms.record(C.TypeVariableMetadata.TYPE_, [
        Phantoms.field(C.TypeVariableMetadata.CLASSES, classes)])


def type_variable_metadata_classes(meta: TTerm) -> TTerm:
    """Get the classes of TypeVariableMetadata."""
    return Phantoms.apply(
        Phantoms.project(C.TypeVariableMetadata.TYPE_, C.TypeVariableMetadata.CLASSES), meta)


# ============================================================
# WrappedTerm
# ============================================================

def wrapped_term(type_name: TTerm, body: TTerm) -> TTerm:
    """Construct a WrappedTerm."""
    return Phantoms.record(C.WrappedTerm.TYPE_, [
        Phantoms.field(C.WrappedTerm.TYPE_NAME, type_name),
        Phantoms.field(C.WrappedTerm.BODY, body)])


def wrapped_term_type_name(wt: TTerm) -> TTerm:
    """Get the typeName of a WrappedTerm."""
    return Phantoms.apply(Phantoms.project(C.WrappedTerm.TYPE_, C.WrappedTerm.TYPE_NAME), wt)


def wrapped_term_body(wt: TTerm) -> TTerm:
    """Get the body of a WrappedTerm."""
    return Phantoms.apply(Phantoms.project(C.WrappedTerm.TYPE_, C.WrappedTerm.BODY), wt)


# ============================================================
# WrappedType
# ============================================================

def wrapped_type(type_name: TTerm, body: TTerm) -> TTerm:
    """Construct a WrappedType."""
    return Phantoms.record(C.WrappedType.TYPE_, [
        Phantoms.field(C.WrappedType.TYPE_NAME, type_name),
        Phantoms.field(C.WrappedType.BODY, body)])


def wrapped_type_type_name(wt: TTerm) -> TTerm:
    """Get the typeName of a WrappedType."""
    return Phantoms.apply(Phantoms.project(C.WrappedType.TYPE_, C.WrappedType.TYPE_NAME), wt)


def wrapped_type_body(wt: TTerm) -> TTerm:
    """Get the body of a WrappedType."""
    return Phantoms.apply(Phantoms.project(C.WrappedType.TYPE_, C.WrappedType.BODY), wt)


# ============================================================
# Name helpers
# ============================================================

def name(s: TTerm) -> TTerm:
    """Wrap a string as a Name."""
    return Phantoms.wrap(C.Name.TYPE_, s)


def name_lift(n: Name) -> TTerm:
    """Lift a Python Name value into a TTerm Name."""
    return Phantoms.wrap(C.Name.TYPE_, Phantoms.string(n.value))


def un_name(n: TTerm) -> TTerm:
    """Unwrap a Name to its underlying string."""
    return Phantoms.apply(Phantoms.unwrap(C.Name.TYPE_), n)


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
