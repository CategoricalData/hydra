# Note: this is an automatically generated file. Do not edit.

r"""Python utilities for constructing Python syntax trees."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.analysis
import hydra.core
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.strings
import hydra.packaging
import hydra.python.environment
import hydra.python.names
import hydra.python.serde
import hydra.python.syntax
import hydra.serialization

T0 = TypeVar("T0")

def py_conjunction_to_py_expression(conj: hydra.python.syntax.Conjunction) -> hydra.python.syntax.Expression:
    r"""Convert a Conjunction to an Expression."""

    return cast(hydra.python.syntax.Expression, hydra.python.syntax.ExpressionSimple(hydra.python.syntax.Disjunction((conj,))))

def py_bitwise_or_to_py_conjunction(bor: hydra.python.syntax.BitwiseOr) -> hydra.python.syntax.Conjunction:
    r"""Convert a BitwiseOr to a Conjunction."""

    return hydra.python.syntax.Conjunction((cast(hydra.python.syntax.Inversion, hydra.python.syntax.InversionSimple(hydra.python.syntax.Comparison(bor, ()))),))

def py_primary_to_py_bitwise_or(prim: hydra.python.syntax.Primary) -> hydra.python.syntax.BitwiseOr:
    r"""Convert a Primary to a BitwiseOr."""

    return hydra.python.syntax.BitwiseOr(Nothing(), hydra.python.syntax.BitwiseXor(Nothing(), hydra.python.syntax.BitwiseAnd(Nothing(), hydra.python.syntax.ShiftExpression(Nothing(), hydra.python.syntax.Sum(Nothing(), hydra.python.syntax.Term(Nothing(), cast(hydra.python.syntax.Factor, hydra.python.syntax.FactorSimple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(False, prim), Nothing())))))))))

def py_primary_to_py_conjunction(prim: hydra.python.syntax.Primary) -> hydra.python.syntax.Conjunction:
    r"""Convert a Primary to a Conjunction."""

    return py_bitwise_or_to_py_conjunction(py_primary_to_py_bitwise_or(prim))

def py_primary_to_py_expression(prim: hydra.python.syntax.Primary) -> hydra.python.syntax.Expression:
    r"""Convert a Primary to an Expression."""

    return py_conjunction_to_py_expression(py_primary_to_py_conjunction(prim))

def py_atom_to_py_expression(atom: hydra.python.syntax.Atom) -> hydra.python.syntax.Expression:
    r"""Convert an Atom to an Expression."""

    return py_primary_to_py_expression(cast(hydra.python.syntax.Primary, hydra.python.syntax.PrimarySimple(atom)))

def string_to_py_expression(style: hydra.python.syntax.QuoteStyle, s: str) -> hydra.python.syntax.Expression:
    r"""Create a string expression with a given quote style."""

    return py_atom_to_py_expression(cast(hydra.python.syntax.Atom, hydra.python.syntax.AtomString(hydra.python.syntax.String(s, style))))

def double_quoted_string(s: str) -> hydra.python.syntax.Expression:
    r"""Create a double-quoted string expression."""

    return string_to_py_expression(hydra.python.syntax.QuoteStyle.DOUBLE, s)

def primary_with_rhs(prim: hydra.python.syntax.Primary, rhs: hydra.python.syntax.PrimaryRhs) -> hydra.python.syntax.Primary:
    r"""Combine a Primary with a PrimaryRhs."""

    return cast(hydra.python.syntax.Primary, hydra.python.syntax.PrimaryCompound(hydra.python.syntax.PrimaryWithRhs(prim, rhs)))

def primary_with_slices(prim: hydra.python.syntax.Primary, first: hydra.python.syntax.Slice, rest: frozenlist[hydra.python.syntax.SliceOrStarredExpression]) -> hydra.python.syntax.Primary:
    r"""Create a Primary with slices."""

    return primary_with_rhs(prim, cast(hydra.python.syntax.PrimaryRhs, hydra.python.syntax.PrimaryRhsSlices(hydra.python.syntax.Slices(first, rest))))

def py_expression_to_py_slice(expr: hydra.python.syntax.Expression) -> hydra.python.syntax.Slice:
    r"""Convert an Expression to a Slice."""

    return cast(hydra.python.syntax.Slice, hydra.python.syntax.SliceNamed(cast(hydra.python.syntax.NamedExpression, hydra.python.syntax.NamedExpressionSimple(expr))))

def primary_with_expression_slices(prim: hydra.python.syntax.Primary, exprs: frozenlist[hydra.python.syntax.Expression]) -> hydra.python.syntax.Primary:
    r"""Create a Primary with expression slices."""

    return primary_with_slices(prim, py_expression_to_py_slice(hydra.lib.lists.head(exprs)), hydra.lib.lists.map((lambda e: cast(hydra.python.syntax.SliceOrStarredExpression, hydra.python.syntax.SliceOrStarredExpressionSlice(py_expression_to_py_slice(e)))), hydra.lib.lists.tail(exprs)))

def py_name_to_py_primary(name: hydra.python.syntax.Name) -> hydra.python.syntax.Primary:
    r"""Convert a Name to a Primary (simple atom)."""

    return cast(hydra.python.syntax.Primary, hydra.python.syntax.PrimarySimple(cast(hydra.python.syntax.Atom, hydra.python.syntax.AtomName(name))))

def annotated_expression(mcomment: Maybe[str], expr: hydra.python.syntax.Expression) -> hydra.python.syntax.Expression:
    r"""Annotate an expression with an optional comment using Annotated[]."""

    return hydra.lib.maybes.maybe((lambda : expr), (lambda c: py_primary_to_py_expression(primary_with_expression_slices(py_name_to_py_primary(hydra.python.syntax.Name("Annotated")), (expr, double_quoted_string(c))))), mcomment)

def annotated_statement(mcomment: Maybe[str], stmt: hydra.python.syntax.Statement) -> hydra.python.syntax.Statement:
    r"""Annotate a statement with an optional comment."""

    return hydra.lib.maybes.maybe((lambda : stmt), (lambda c: cast(hydra.python.syntax.Statement, hydra.python.syntax.StatementAnnotated(hydra.python.syntax.AnnotatedStatement(c, stmt)))), mcomment)

def py_simple_statement_to_py_statement(s: hydra.python.syntax.SimpleStatement) -> hydra.python.syntax.Statement:
    r"""Convert a SimpleStatement to a Statement."""

    return cast(hydra.python.syntax.Statement, hydra.python.syntax.StatementSimple((s,)))

def py_assignment_to_py_statement(a: hydra.python.syntax.Assignment) -> hydra.python.syntax.Statement:
    r"""Convert an Assignment to a Statement."""

    return py_simple_statement_to_py_statement(cast(hydra.python.syntax.SimpleStatement, hydra.python.syntax.SimpleStatementAssignment(a)))

def py_name_to_py_star_target(name: hydra.python.syntax.Name) -> hydra.python.syntax.StarTarget:
    r"""Convert a Name to a StarTarget."""

    return cast(hydra.python.syntax.StarTarget, hydra.python.syntax.StarTargetUnstarred(cast(hydra.python.syntax.TargetWithStarAtom, hydra.python.syntax.TargetWithStarAtomAtom(cast(hydra.python.syntax.StarAtom, hydra.python.syntax.StarAtomName(name))))))

def assignment(name: hydra.python.syntax.Name, rhs: hydra.python.syntax.AnnotatedRhs) -> hydra.python.syntax.Statement:
    r"""Create an assignment statement from name and annotated rhs."""

    return py_assignment_to_py_statement(cast(hydra.python.syntax.Assignment, hydra.python.syntax.AssignmentUntyped(hydra.python.syntax.UntypedAssignment((py_name_to_py_star_target(name),), rhs, Nothing()))))

def py_expression_to_py_annotated_rhs(expr: hydra.python.syntax.Expression) -> hydra.python.syntax.AnnotatedRhs:
    r"""Convert an Expression to an AnnotatedRhs."""

    return cast(hydra.python.syntax.AnnotatedRhs, hydra.python.syntax.AnnotatedRhsStar((cast(hydra.python.syntax.StarExpression, hydra.python.syntax.StarExpressionSimple(expr)),)))

def assignment_statement(name: hydra.python.syntax.Name, expr: hydra.python.syntax.Expression) -> hydra.python.syntax.Statement:
    r"""Create an assignment statement from name and expression."""

    return assignment(name, py_expression_to_py_annotated_rhs(expr))

def py_expressions_to_py_args(exprs: frozenlist[hydra.python.syntax.Expression]) -> hydra.python.syntax.Args:
    r"""Convert a list of Expressions to Args."""

    return hydra.python.syntax.Args(hydra.lib.lists.map((lambda e: cast(hydra.python.syntax.PosArg, hydra.python.syntax.PosArgExpression(e))), exprs), (), ())

def function_call(func: hydra.python.syntax.Primary, args: frozenlist[hydra.python.syntax.Expression]) -> hydra.python.syntax.Expression:
    r"""Create a function call expression."""

    return py_primary_to_py_expression(primary_with_rhs(func, cast(hydra.python.syntax.PrimaryRhs, hydra.python.syntax.PrimaryRhsCall(py_expressions_to_py_args(args)))))

def cast_to(pytype: hydra.python.syntax.Expression, pyexpr: hydra.python.syntax.Expression) -> hydra.python.syntax.Expression:
    r"""Create a cast expression: cast(type, expr)."""

    return function_call(py_name_to_py_primary(hydra.python.syntax.Name("cast")), (pytype, pyexpr))

def py_expression_to_py_simple_statement(expr: hydra.python.syntax.Expression) -> hydra.python.syntax.SimpleStatement:
    r"""Convert an Expression to a SimpleStatement (as star expressions)."""

    return cast(hydra.python.syntax.SimpleStatement, hydra.python.syntax.SimpleStatementStarExpressions((cast(hydra.python.syntax.StarExpression, hydra.python.syntax.StarExpressionSimple(expr)),)))

def py_expression_to_py_statement(expr: hydra.python.syntax.Expression) -> hydra.python.syntax.Statement:
    r"""Convert an Expression to a Statement."""

    return py_simple_statement_to_py_statement(py_expression_to_py_simple_statement(expr))

def triple_quoted_string(s: str) -> hydra.python.syntax.Expression:
    r"""Create a triple-quoted string expression."""

    return string_to_py_expression(hydra.python.syntax.QuoteStyle.TRIPLE, s)

def comment_statement(s: str) -> hydra.python.syntax.Statement:
    r"""Create a comment statement (triple-quoted string)."""

    return py_expression_to_py_statement(triple_quoted_string(s))

def decode_py_power_to_py_primary(p: hydra.python.syntax.Power) -> Maybe[hydra.python.syntax.Primary]:
    r"""Decode a Power to a Primary if possible."""

    lhs = p.lhs
    await_ = lhs.await_
    prim = lhs.primary
    return hydra.lib.logic.if_else(await_, (lambda : Nothing()), (lambda : Just(prim)))

def decode_py_comparison_to_py_await_primary(c: hydra.python.syntax.Comparison):
    r"""Decode a Comparison to a Primary if possible."""

    rhs = c.rhs
    lhs = c.lhs
    or_lhs = lhs.lhs
    or_rhs = lhs.rhs
    xor_lhs = or_rhs.lhs
    xor_rhs = or_rhs.rhs
    and_lhs = xor_rhs.lhs
    and_rhs = xor_rhs.rhs
    shift_lhs = and_rhs.lhs
    shift_rhs = and_rhs.rhs
    sum_lhs = shift_rhs.lhs
    sum_rhs = shift_rhs.rhs
    term_lhs = sum_rhs.lhs
    term_rhs = sum_rhs.rhs
    def _hoist_rhs_body_1(v1):
        match v1:
            case hydra.python.syntax.FactorSimple(value=power):
                return decode_py_power_to_py_primary(power)

            case _:
                return Nothing()
    return hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(rhs)), (lambda : Nothing()), (lambda : hydra.lib.logic.if_else(hydra.lib.maybes.is_just(or_lhs), (lambda : Nothing()), (lambda : hydra.lib.logic.if_else(hydra.lib.maybes.is_just(xor_lhs), (lambda : Nothing()), (lambda : hydra.lib.logic.if_else(hydra.lib.maybes.is_just(and_lhs), (lambda : Nothing()), (lambda : hydra.lib.logic.if_else(hydra.lib.maybes.is_just(shift_lhs), (lambda : Nothing()), (lambda : hydra.lib.logic.if_else(hydra.lib.maybes.is_just(sum_lhs), (lambda : Nothing()), (lambda : hydra.lib.logic.if_else(hydra.lib.maybes.is_just(term_lhs), (lambda : Nothing()), (lambda : _hoist_rhs_body_1(term_rhs)))))))))))))))

def decode_py_inversion_to_py_primary(i: hydra.python.syntax.Inversion) -> Maybe[hydra.python.syntax.Primary]:
    r"""Decode an Inversion to a Primary if possible."""

    match i:
        case hydra.python.syntax.InversionSimple(value=comparison):
            return decode_py_comparison_to_py_await_primary(comparison)

        case _:
            return Nothing()

def decode_py_conjunction_to_py_primary(c: hydra.python.syntax.Conjunction) -> Maybe[hydra.python.syntax.Primary]:
    r"""Decode a Conjunction to a Primary if possible."""

    inversions = c.value
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(inversions), 1), (lambda : decode_py_inversion_to_py_primary(hydra.lib.lists.head(inversions))), (lambda : Nothing()))

def decode_py_expression_to_py_primary(e: hydra.python.syntax.Expression) -> Maybe[hydra.python.syntax.Primary]:
    r"""Decode an Expression to a Primary if possible."""

    match e:
        case hydra.python.syntax.ExpressionSimple(value=disj):
            conjunctions = disj.value
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(conjunctions), 1), (lambda : decode_py_conjunction_to_py_primary(hydra.lib.lists.head(conjunctions))), (lambda : Nothing()))

        case _:
            return Nothing()

def dotted_assignment_statement(obj: hydra.python.syntax.Name, attr: hydra.python.syntax.Name, expr: hydra.python.syntax.Expression) -> hydra.python.syntax.Statement:
    r"""Create a dotted assignment statement: obj.attr = expr."""

    @lru_cache(1)
    def target() -> hydra.python.syntax.StarTarget:
        return cast(hydra.python.syntax.StarTarget, hydra.python.syntax.StarTargetUnstarred(cast(hydra.python.syntax.TargetWithStarAtom, hydra.python.syntax.TargetWithStarAtomProject(hydra.python.syntax.TPrimaryAndName(cast(hydra.python.syntax.TPrimary, hydra.python.syntax.TPrimaryAtom(cast(hydra.python.syntax.Atom, hydra.python.syntax.AtomName(obj)))), attr)))))
    return py_assignment_to_py_statement(cast(hydra.python.syntax.Assignment, hydra.python.syntax.AssignmentUntyped(hydra.python.syntax.UntypedAssignment((target(),), py_expression_to_py_annotated_rhs(expr), Nothing()))))

def find_namespaces(focus_ns: hydra.packaging.Namespace, defs: frozenlist[hydra.packaging.Definition]) -> hydra.packaging.Namespaces[hydra.python.syntax.DottedName]:
    r"""Find all namespaces referenced by a list of definitions, plus the core namespace."""

    core_ns = hydra.packaging.Namespace("hydra.core")
    @lru_cache(1)
    def namespaces() -> hydra.packaging.Namespaces[hydra.python.syntax.DottedName]:
        return hydra.analysis.namespaces_for_definitions((lambda x1: hydra.python.names.encode_namespace(x1)), focus_ns, defs)
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.pairs.first(namespaces().focus).value, core_ns.value), (lambda : namespaces()), (lambda : hydra.packaging.Namespaces(namespaces().focus, hydra.lib.maps.insert(core_ns, hydra.python.names.encode_namespace(core_ns), namespaces().mapping))))

@lru_cache(1)
def get_item_params() -> hydra.python.syntax.Parameters:
    return cast(hydra.python.syntax.Parameters, hydra.python.syntax.ParametersParamNoDefault(hydra.python.syntax.ParamNoDefaultParameters((hydra.python.syntax.ParamNoDefault(hydra.python.syntax.Param(hydra.python.syntax.Name("cls"), Nothing()), Nothing()), hydra.python.syntax.ParamNoDefault(hydra.python.syntax.Param(hydra.python.syntax.Name("item"), Nothing()), Nothing())), (), Nothing())))

def indented_block(mcomment: Maybe[str], stmts: frozenlist[frozenlist[hydra.python.syntax.Statement]]) -> hydra.python.syntax.Block:
    r"""Create an indented block with optional comment."""

    @lru_cache(1)
    def comment_group() -> frozenlist[hydra.python.syntax.Statement]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda s: (comment_statement(s),)), mcomment)
    @lru_cache(1)
    def groups() -> frozenlist[frozenlist[hydra.python.syntax.Statement]]:
        return hydra.lib.lists.filter((lambda g: hydra.lib.logic.not_(hydra.lib.lists.null(g))), hydra.lib.lists.cons(comment_group(), stmts))
    return hydra.lib.logic.if_else(hydra.lib.lists.null(groups()), (lambda : cast(hydra.python.syntax.Block, hydra.python.syntax.BlockIndented(((cast(hydra.python.syntax.Statement, hydra.python.syntax.StatementSimple((py_expression_to_py_simple_statement(py_atom_to_py_expression(cast(hydra.python.syntax.Atom, hydra.python.syntax.AtomEllipsis()))),))),),)))), (lambda : cast(hydra.python.syntax.Block, hydra.python.syntax.BlockIndented(groups()))))

def primary_and_params(prim: hydra.python.syntax.Primary, params: frozenlist[hydra.python.syntax.Expression]) -> hydra.python.syntax.Expression:
    r"""Create a primary with parameters (subscript)."""

    return py_primary_to_py_expression(primary_with_expression_slices(prim, params))

def name_and_params(py_name: hydra.python.syntax.Name, params: frozenlist[hydra.python.syntax.Expression]) -> hydra.python.syntax.Expression:
    r"""Create a name with parameters."""

    return primary_and_params(py_name_to_py_primary(py_name), params)

def newtype_statement(name: hydra.python.syntax.Name, mcomment: Maybe[str], expr: hydra.python.syntax.Expression) -> hydra.python.syntax.Statement:
    r"""Create a NewType statement."""

    return annotated_statement(mcomment, assignment_statement(name, function_call(cast(hydra.python.syntax.Primary, hydra.python.syntax.PrimarySimple(cast(hydra.python.syntax.Atom, hydra.python.syntax.AtomName(hydra.python.syntax.Name("NewType"))))), (double_quoted_string(name.value), expr))))

def py_bitwise_or_to_py_expression(bor: hydra.python.syntax.BitwiseOr) -> hydra.python.syntax.Expression:
    r"""Convert a BitwiseOr to an Expression."""

    return py_conjunction_to_py_expression(py_bitwise_or_to_py_conjunction(bor))

def py_primary_to_py_bitwise_xor(prim: hydra.python.syntax.Primary) -> hydra.python.syntax.BitwiseXor:
    r"""Convert a Primary to a BitwiseXor."""

    return hydra.python.syntax.BitwiseXor(Nothing(), hydra.python.syntax.BitwiseAnd(Nothing(), hydra.python.syntax.ShiftExpression(Nothing(), hydra.python.syntax.Sum(Nothing(), hydra.python.syntax.Term(Nothing(), cast(hydra.python.syntax.Factor, hydra.python.syntax.FactorSimple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(False, prim), Nothing()))))))))

def or_expression(prims: frozenlist[hydra.python.syntax.Primary]) -> hydra.python.syntax.Expression:
    r"""Build an or-expression from multiple primaries."""

    def build(prev: Maybe[hydra.python.syntax.BitwiseOr], ps: frozenlist[hydra.python.syntax.Primary]) -> hydra.python.syntax.BitwiseOr:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(hydra.lib.lists.tail(ps)), (lambda : hydra.python.syntax.BitwiseOr(prev, py_primary_to_py_bitwise_xor(hydra.lib.lists.head(ps)))), (lambda : build(Just(hydra.python.syntax.BitwiseOr(prev, py_primary_to_py_bitwise_xor(hydra.lib.lists.head(ps)))), hydra.lib.lists.tail(ps))))
    return py_bitwise_or_to_py_expression(build(Nothing(), prims))

def project_from_expression(exp: hydra.python.syntax.Expression, name: hydra.python.syntax.Name) -> hydra.python.syntax.Expression:
    r"""Project a field from an expression."""

    @lru_cache(1)
    def prim() -> hydra.python.syntax.Primary:
        return cast(hydra.python.syntax.Primary, hydra.python.syntax.PrimarySimple(cast(hydra.python.syntax.Atom, hydra.python.syntax.AtomGroup(cast(hydra.python.syntax.Group, hydra.python.syntax.GroupExpression(cast(hydra.python.syntax.NamedExpression, hydra.python.syntax.NamedExpressionSimple(exp))))))))
    return py_primary_to_py_expression(cast(hydra.python.syntax.Primary, hydra.python.syntax.PrimaryCompound(hydra.python.syntax.PrimaryWithRhs(prim(), cast(hydra.python.syntax.PrimaryRhs, hydra.python.syntax.PrimaryRhsProject(name))))))

def py_class_definition_to_py_statement(cd: hydra.python.syntax.ClassDefinition) -> hydra.python.syntax.Statement:
    r"""Convert a ClassDefinition to a Statement."""

    return cast(hydra.python.syntax.Statement, hydra.python.syntax.StatementCompound(cast(hydra.python.syntax.CompoundStatement, hydra.python.syntax.CompoundStatementClassDef(cd))))

def py_closed_pattern_to_py_patterns(p: hydra.python.syntax.ClosedPattern) -> hydra.python.syntax.Patterns:
    r"""Convert a ClosedPattern to Patterns."""

    return cast(hydra.python.syntax.Patterns, hydra.python.syntax.PatternsPattern(cast(hydra.python.syntax.Pattern, hydra.python.syntax.PatternOr(hydra.python.syntax.OrPattern((p,))))))

def py_expression_to_bitwise_or(e: hydra.python.syntax.Expression) -> hydra.python.syntax.BitwiseOr:
    r"""Convert an Expression to a BitwiseOr, wrapping in parens if needed."""

    return hydra.python.syntax.BitwiseOr(Nothing(), hydra.python.syntax.BitwiseXor(Nothing(), hydra.python.syntax.BitwiseAnd(Nothing(), hydra.python.syntax.ShiftExpression(Nothing(), hydra.python.syntax.Sum(Nothing(), hydra.python.syntax.Term(Nothing(), cast(hydra.python.syntax.Factor, hydra.python.syntax.FactorSimple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(False, cast(hydra.python.syntax.Primary, hydra.python.syntax.PrimarySimple(cast(hydra.python.syntax.Atom, hydra.python.syntax.AtomGroup(cast(hydra.python.syntax.Group, hydra.python.syntax.GroupExpression(cast(hydra.python.syntax.NamedExpression, hydra.python.syntax.NamedExpressionSimple(e))))))))), Nothing())))))))))

def py_expression_to_disjunction(e: hydra.python.syntax.Expression) -> hydra.python.syntax.Disjunction:
    r"""Convert an Expression to a Disjunction, wrapping in parens if needed."""

    match e:
        case hydra.python.syntax.ExpressionSimple(value=disj):
            return disj

        case _:
            return hydra.python.syntax.Disjunction((py_primary_to_py_conjunction(cast(hydra.python.syntax.Primary, hydra.python.syntax.PrimarySimple(cast(hydra.python.syntax.Atom, hydra.python.syntax.AtomGroup(cast(hydra.python.syntax.Group, hydra.python.syntax.GroupExpression(cast(hydra.python.syntax.NamedExpression, hydra.python.syntax.NamedExpressionSimple(e))))))))),))

def py_expression_to_py_primary(e: hydra.python.syntax.Expression) -> hydra.python.syntax.Primary:
    r"""Extracts the primary from an expression, or wraps it in parentheses if the expression does not contain a primary."""

    return hydra.lib.maybes.maybe((lambda : cast(hydra.python.syntax.Primary, hydra.python.syntax.PrimarySimple(cast(hydra.python.syntax.Atom, hydra.python.syntax.AtomGroup(cast(hydra.python.syntax.Group, hydra.python.syntax.GroupExpression(cast(hydra.python.syntax.NamedExpression, hydra.python.syntax.NamedExpressionSimple(e))))))))), (lambda prim: prim), decode_py_expression_to_py_primary(e))

def py_expression_to_py_star_named_expression(expr: hydra.python.syntax.Expression) -> hydra.python.syntax.StarNamedExpression:
    r"""Convert an Expression to a StarNamedExpression."""

    return cast(hydra.python.syntax.StarNamedExpression, hydra.python.syntax.StarNamedExpressionSimple(cast(hydra.python.syntax.NamedExpression, hydra.python.syntax.NamedExpressionSimple(expr))))

def py_list(exprs: frozenlist[hydra.python.syntax.Expression]) -> hydra.python.syntax.List:
    r"""Create a Python list from expressions."""

    return hydra.python.syntax.List(hydra.lib.lists.map((lambda x1: py_expression_to_py_star_named_expression(x1)), exprs))

def py_name_to_py_expression(name: hydra.python.syntax.Name) -> hydra.python.syntax.Expression:
    r"""Convert a Name to an Expression."""

    return py_primary_to_py_expression(py_name_to_py_primary(name))

def py_name_to_py_named_expression(name: hydra.python.syntax.Name) -> hydra.python.syntax.NamedExpression:
    r"""Convert a Name to a NamedExpression."""

    return cast(hydra.python.syntax.NamedExpression, hydra.python.syntax.NamedExpressionSimple(py_name_to_py_expression(name)))

def py_name_to_py_type_parameter(name: hydra.python.syntax.Name) -> hydra.python.syntax.TypeParameter:
    r"""Convert a Name to a TypeParameter."""

    return cast(hydra.python.syntax.TypeParameter, hydra.python.syntax.TypeParameterSimple(hydra.python.syntax.SimpleTypeParameter(name, Nothing(), Nothing())))

# The Python None value as a Name.
py_none = hydra.python.syntax.Name("None")

def py_primary_to_py_slice(prim: hydra.python.syntax.Primary) -> hydra.python.syntax.Slice:
    r"""Convert a Primary to a Slice."""

    return py_expression_to_py_slice(py_primary_to_py_expression(prim))

def raise_assertion_error(msg: str) -> hydra.python.syntax.Statement:
    r"""Create a raise AssertionError statement."""

    return py_simple_statement_to_py_statement(cast(hydra.python.syntax.SimpleStatement, hydra.python.syntax.SimpleStatementRaise(hydra.python.syntax.RaiseStatement(Just(hydra.python.syntax.RaiseExpression(function_call(cast(hydra.python.syntax.Primary, hydra.python.syntax.PrimarySimple(cast(hydra.python.syntax.Atom, hydra.python.syntax.AtomName(hydra.python.syntax.Name("AssertionError"))))), (double_quoted_string(msg),)), Nothing()))))))

def raise_type_error(msg: str) -> hydra.python.syntax.Statement:
    r"""Create a raise TypeError statement."""

    return py_simple_statement_to_py_statement(cast(hydra.python.syntax.SimpleStatement, hydra.python.syntax.SimpleStatementRaise(hydra.python.syntax.RaiseStatement(Just(hydra.python.syntax.RaiseExpression(function_call(cast(hydra.python.syntax.Primary, hydra.python.syntax.PrimarySimple(cast(hydra.python.syntax.Atom, hydra.python.syntax.AtomName(hydra.python.syntax.Name("TypeError"))))), (double_quoted_string(msg),)), Nothing()))))))

def return_single(expr: hydra.python.syntax.Expression) -> hydra.python.syntax.Statement:
    r"""Create a return statement with a single expression."""

    return py_simple_statement_to_py_statement(cast(hydra.python.syntax.SimpleStatement, hydra.python.syntax.SimpleStatementReturn(hydra.python.syntax.ReturnStatement((cast(hydra.python.syntax.StarExpression, hydra.python.syntax.StarExpressionSimple(expr)),)))))

@lru_cache(1)
def self_only_params() -> hydra.python.syntax.Parameters:
    return cast(hydra.python.syntax.Parameters, hydra.python.syntax.ParametersParamNoDefault(hydra.python.syntax.ParamNoDefaultParameters((hydra.python.syntax.ParamNoDefault(hydra.python.syntax.Param(hydra.python.syntax.Name("self"), Nothing()), Nothing()),), (), Nothing())))

@lru_cache(1)
def self_other_params() -> hydra.python.syntax.Parameters:
    return cast(hydra.python.syntax.Parameters, hydra.python.syntax.ParametersParamNoDefault(hydra.python.syntax.ParamNoDefaultParameters((hydra.python.syntax.ParamNoDefault(hydra.python.syntax.Param(hydra.python.syntax.Name("self"), Nothing()), Nothing()), hydra.python.syntax.ParamNoDefault(hydra.python.syntax.Param(hydra.python.syntax.Name("other"), Nothing()), Nothing())), (), Nothing())))

def single_quoted_string(s: str) -> hydra.python.syntax.Expression:
    r"""Create a single-quoted string expression."""

    return string_to_py_expression(hydra.python.syntax.QuoteStyle.SINGLE, s)

# Current target Python version for code generation.
target_python_version = hydra.python.environment.PythonVersion.PYTHON310

def type_alias_statement(name: hydra.python.syntax.Name, tparams: frozenlist[hydra.python.syntax.TypeParameter], mcomment: Maybe[str], tyexpr: hydra.python.syntax.Expression) -> hydra.python.syntax.Statement:
    r"""Generate a type alias statement using PEP 695 syntax (Python 3.12+)."""

    return annotated_statement(mcomment, py_simple_statement_to_py_statement(cast(hydra.python.syntax.SimpleStatement, hydra.python.syntax.SimpleStatementTypeAlias(hydra.python.syntax.TypeAlias(name, tparams, tyexpr)))))

def type_alias_statement310(name: hydra.python.syntax.Name, _tparams: T0, mcomment: Maybe[str], tyexpr: hydra.python.syntax.Expression) -> hydra.python.syntax.Statement:
    r"""Generate a type alias statement using Python 3.10-compatible syntax: Name: TypeAlias = "TypeExpression"."""

    @lru_cache(1)
    def quoted_expr() -> hydra.python.syntax.Expression:
        return double_quoted_string(hydra.serialization.print_expr(hydra.python.serde.encode_expression(tyexpr)))
    return annotated_statement(mcomment, py_assignment_to_py_statement(cast(hydra.python.syntax.Assignment, hydra.python.syntax.AssignmentTyped(hydra.python.syntax.TypedAssignment(cast(hydra.python.syntax.SingleTarget, hydra.python.syntax.SingleTargetName(name)), cast(hydra.python.syntax.Expression, hydra.python.syntax.ExpressionSimple(hydra.python.syntax.Disjunction((hydra.python.syntax.Conjunction((cast(hydra.python.syntax.Inversion, hydra.python.syntax.InversionSimple(hydra.python.syntax.Comparison(hydra.python.syntax.BitwiseOr(Nothing(), hydra.python.syntax.BitwiseXor(Nothing(), hydra.python.syntax.BitwiseAnd(Nothing(), hydra.python.syntax.ShiftExpression(Nothing(), hydra.python.syntax.Sum(Nothing(), hydra.python.syntax.Term(Nothing(), cast(hydra.python.syntax.Factor, hydra.python.syntax.FactorSimple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(False, cast(hydra.python.syntax.Primary, hydra.python.syntax.PrimarySimple(cast(hydra.python.syntax.Atom, hydra.python.syntax.AtomName(hydra.python.syntax.Name("TypeAlias")))))), Nothing()))))))))), ()))),)),)))), Just(py_expression_to_py_annotated_rhs(quoted_expr())))))))

def union_type_class_statements310(name: hydra.python.syntax.Name, mcomment: Maybe[str], tyexpr: hydra.python.syntax.Expression, extra_stmts: frozenlist[hydra.python.syntax.Statement]) -> frozenlist[hydra.python.syntax.Statement]:
    r"""Generate a subscriptable union class for Python 3.10."""

    name_str = name.value
    @lru_cache(1)
    def meta_name() -> hydra.python.syntax.Name:
        return hydra.python.syntax.Name(hydra.lib.strings.cat2(hydra.lib.strings.cat2("_", name_str), "Meta"))
    @lru_cache(1)
    def doc_string() -> str:
        return hydra.serialization.print_expr(hydra.python.serde.encode_expression(tyexpr))
    @lru_cache(1)
    def return_object() -> hydra.python.syntax.Statement:
        return py_simple_statement_to_py_statement(cast(hydra.python.syntax.SimpleStatement, hydra.python.syntax.SimpleStatementReturn(hydra.python.syntax.ReturnStatement((cast(hydra.python.syntax.StarExpression, hydra.python.syntax.StarExpressionSimple(cast(hydra.python.syntax.Expression, hydra.python.syntax.ExpressionSimple(hydra.python.syntax.Disjunction((hydra.python.syntax.Conjunction((cast(hydra.python.syntax.Inversion, hydra.python.syntax.InversionSimple(hydra.python.syntax.Comparison(hydra.python.syntax.BitwiseOr(Nothing(), hydra.python.syntax.BitwiseXor(Nothing(), hydra.python.syntax.BitwiseAnd(Nothing(), hydra.python.syntax.ShiftExpression(Nothing(), hydra.python.syntax.Sum(Nothing(), hydra.python.syntax.Term(Nothing(), cast(hydra.python.syntax.Factor, hydra.python.syntax.FactorSimple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(False, cast(hydra.python.syntax.Primary, hydra.python.syntax.PrimarySimple(cast(hydra.python.syntax.Atom, hydra.python.syntax.AtomName(hydra.python.syntax.Name("object")))))), Nothing()))))))))), ()))),)),)))))),)))))
    @lru_cache(1)
    def get_item_method() -> hydra.python.syntax.Statement:
        return cast(hydra.python.syntax.Statement, hydra.python.syntax.StatementCompound(cast(hydra.python.syntax.CompoundStatement, hydra.python.syntax.CompoundStatementFunction(hydra.python.syntax.FunctionDefinition(Nothing(), hydra.python.syntax.FunctionDefRaw(False, hydra.python.syntax.Name("__getitem__"), (), Just(get_item_params()), Nothing(), Nothing(), indented_block(Nothing(), ((return_object(),),))))))))
    @lru_cache(1)
    def meta_class() -> hydra.python.syntax.Statement:
        return py_class_definition_to_py_statement(hydra.python.syntax.ClassDefinition(Nothing(), meta_name(), (), Just(py_expressions_to_py_args((cast(hydra.python.syntax.Expression, hydra.python.syntax.ExpressionSimple(hydra.python.syntax.Disjunction((hydra.python.syntax.Conjunction((cast(hydra.python.syntax.Inversion, hydra.python.syntax.InversionSimple(hydra.python.syntax.Comparison(hydra.python.syntax.BitwiseOr(Nothing(), hydra.python.syntax.BitwiseXor(Nothing(), hydra.python.syntax.BitwiseAnd(Nothing(), hydra.python.syntax.ShiftExpression(Nothing(), hydra.python.syntax.Sum(Nothing(), hydra.python.syntax.Term(Nothing(), cast(hydra.python.syntax.Factor, hydra.python.syntax.FactorSimple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(False, cast(hydra.python.syntax.Primary, hydra.python.syntax.PrimarySimple(cast(hydra.python.syntax.Atom, hydra.python.syntax.AtomName(hydra.python.syntax.Name("type")))))), Nothing()))))))))), ()))),)),)))),))), indented_block(Nothing(), ((get_item_method(),),))))
    @lru_cache(1)
    def doc_stmt() -> hydra.python.syntax.Statement:
        return py_expression_to_py_statement(triple_quoted_string(doc_string()))
    @lru_cache(1)
    def body_groups() -> frozenlist[frozenlist[hydra.python.syntax.Statement]]:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(extra_stmts), (lambda : (pass_stmt := py_simple_statement_to_py_statement(cast(hydra.python.syntax.SimpleStatement, hydra.python.syntax.SimpleStatementPass())), ((doc_stmt(),), (pass_stmt,)))[1]), (lambda : ((doc_stmt(),), extra_stmts)))
    @lru_cache(1)
    def metaclass_arg() -> hydra.python.syntax.Kwarg:
        return hydra.python.syntax.Kwarg(hydra.python.syntax.Name("metaclass"), cast(hydra.python.syntax.Expression, hydra.python.syntax.ExpressionSimple(hydra.python.syntax.Disjunction((hydra.python.syntax.Conjunction((cast(hydra.python.syntax.Inversion, hydra.python.syntax.InversionSimple(hydra.python.syntax.Comparison(hydra.python.syntax.BitwiseOr(Nothing(), hydra.python.syntax.BitwiseXor(Nothing(), hydra.python.syntax.BitwiseAnd(Nothing(), hydra.python.syntax.ShiftExpression(Nothing(), hydra.python.syntax.Sum(Nothing(), hydra.python.syntax.Term(Nothing(), cast(hydra.python.syntax.Factor, hydra.python.syntax.FactorSimple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(False, cast(hydra.python.syntax.Primary, hydra.python.syntax.PrimarySimple(cast(hydra.python.syntax.Atom, hydra.python.syntax.AtomName(meta_name()))))), Nothing()))))))))), ()))),)),)))))
    @lru_cache(1)
    def union_class() -> hydra.python.syntax.Statement:
        return annotated_statement(mcomment, py_class_definition_to_py_statement(hydra.python.syntax.ClassDefinition(Nothing(), name, (), Just(hydra.python.syntax.Args((), (cast(hydra.python.syntax.KwargOrStarred, hydra.python.syntax.KwargOrStarredKwarg(metaclass_arg())),), ())), indented_block(Nothing(), body_groups()))))
    return (meta_class(), union_class())

def unit_variant_methods(class_name: hydra.python.syntax.Name) -> frozenlist[hydra.python.syntax.Statement]:
    r"""Generate __slots__, __eq__, and __hash__ methods for unit-typed union variants."""

    class_name_str = class_name.value
    @lru_cache(1)
    def slots_stmt() -> hydra.python.syntax.Statement:
        return assignment_statement(hydra.python.syntax.Name("__slots__"), py_primary_to_py_expression(cast(hydra.python.syntax.Primary, hydra.python.syntax.PrimarySimple(cast(hydra.python.syntax.Atom, hydra.python.syntax.AtomTuple(hydra.python.syntax.Tuple(())))))))
    @lru_cache(1)
    def return_isinstance() -> hydra.python.syntax.Statement:
        return py_simple_statement_to_py_statement(cast(hydra.python.syntax.SimpleStatement, hydra.python.syntax.SimpleStatementReturn(hydra.python.syntax.ReturnStatement((cast(hydra.python.syntax.StarExpression, hydra.python.syntax.StarExpressionSimple(function_call(cast(hydra.python.syntax.Primary, hydra.python.syntax.PrimarySimple(cast(hydra.python.syntax.Atom, hydra.python.syntax.AtomName(hydra.python.syntax.Name("isinstance"))))), (cast(hydra.python.syntax.Expression, hydra.python.syntax.ExpressionSimple(hydra.python.syntax.Disjunction((hydra.python.syntax.Conjunction((cast(hydra.python.syntax.Inversion, hydra.python.syntax.InversionSimple(hydra.python.syntax.Comparison(hydra.python.syntax.BitwiseOr(Nothing(), hydra.python.syntax.BitwiseXor(Nothing(), hydra.python.syntax.BitwiseAnd(Nothing(), hydra.python.syntax.ShiftExpression(Nothing(), hydra.python.syntax.Sum(Nothing(), hydra.python.syntax.Term(Nothing(), cast(hydra.python.syntax.Factor, hydra.python.syntax.FactorSimple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(False, cast(hydra.python.syntax.Primary, hydra.python.syntax.PrimarySimple(cast(hydra.python.syntax.Atom, hydra.python.syntax.AtomName(hydra.python.syntax.Name("other")))))), Nothing()))))))))), ()))),)),)))), cast(hydra.python.syntax.Expression, hydra.python.syntax.ExpressionSimple(hydra.python.syntax.Disjunction((hydra.python.syntax.Conjunction((cast(hydra.python.syntax.Inversion, hydra.python.syntax.InversionSimple(hydra.python.syntax.Comparison(hydra.python.syntax.BitwiseOr(Nothing(), hydra.python.syntax.BitwiseXor(Nothing(), hydra.python.syntax.BitwiseAnd(Nothing(), hydra.python.syntax.ShiftExpression(Nothing(), hydra.python.syntax.Sum(Nothing(), hydra.python.syntax.Term(Nothing(), cast(hydra.python.syntax.Factor, hydra.python.syntax.FactorSimple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(False, cast(hydra.python.syntax.Primary, hydra.python.syntax.PrimarySimple(cast(hydra.python.syntax.Atom, hydra.python.syntax.AtomName(class_name))))), Nothing()))))))))), ()))),)),)))))))),)))))
    @lru_cache(1)
    def eq_method() -> hydra.python.syntax.Statement:
        return cast(hydra.python.syntax.Statement, hydra.python.syntax.StatementCompound(cast(hydra.python.syntax.CompoundStatement, hydra.python.syntax.CompoundStatementFunction(hydra.python.syntax.FunctionDefinition(Nothing(), hydra.python.syntax.FunctionDefRaw(False, hydra.python.syntax.Name("__eq__"), (), Just(self_other_params()), Nothing(), Nothing(), indented_block(Nothing(), ((return_isinstance(),),))))))))
    @lru_cache(1)
    def return_hash() -> hydra.python.syntax.Statement:
        return py_simple_statement_to_py_statement(cast(hydra.python.syntax.SimpleStatement, hydra.python.syntax.SimpleStatementReturn(hydra.python.syntax.ReturnStatement((cast(hydra.python.syntax.StarExpression, hydra.python.syntax.StarExpressionSimple(function_call(cast(hydra.python.syntax.Primary, hydra.python.syntax.PrimarySimple(cast(hydra.python.syntax.Atom, hydra.python.syntax.AtomName(hydra.python.syntax.Name("hash"))))), (double_quoted_string(class_name_str),)))),)))))
    @lru_cache(1)
    def hash_method() -> hydra.python.syntax.Statement:
        return cast(hydra.python.syntax.Statement, hydra.python.syntax.StatementCompound(cast(hydra.python.syntax.CompoundStatement, hydra.python.syntax.CompoundStatementFunction(hydra.python.syntax.FunctionDefinition(Nothing(), hydra.python.syntax.FunctionDefRaw(False, hydra.python.syntax.Name("__hash__"), (), Just(self_only_params()), Nothing(), Nothing(), indented_block(Nothing(), ((return_hash(),),))))))))
    return (slots_stmt(), eq_method(), hash_method())
