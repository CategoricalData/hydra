# Note: this is an automatically generated file. Do not edit.

r"""Python utilities for constructing Python syntax trees."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.ext.python.helpers
import hydra.ext.python.names
import hydra.ext.python.serde
import hydra.ext.python.syntax
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.strings
import hydra.module
import hydra.schemas
import hydra.serialization

T0 = TypeVar("T0")

def py_conjunction_to_py_expression(conj: hydra.ext.python.syntax.Conjunction) -> hydra.ext.python.syntax.Expression:
    r"""Convert a Conjunction to an Expression."""
    
    return cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionSimple(hydra.ext.python.syntax.Disjunction((conj,))))

def py_bitwise_or_to_py_conjunction(bor: hydra.ext.python.syntax.BitwiseOr) -> hydra.ext.python.syntax.Conjunction:
    r"""Convert a BitwiseOr to a Conjunction."""
    
    return hydra.ext.python.syntax.Conjunction((cast(hydra.ext.python.syntax.Inversion, hydra.ext.python.syntax.InversionSimple(hydra.ext.python.syntax.Comparison(bor, ()))),))

def py_primary_to_py_bitwise_or(prim: hydra.ext.python.syntax.Primary) -> hydra.ext.python.syntax.BitwiseOr:
    r"""Convert a Primary to a BitwiseOr."""
    
    return hydra.ext.python.syntax.BitwiseOr(Nothing(), hydra.ext.python.syntax.BitwiseXor(Nothing(), hydra.ext.python.syntax.BitwiseAnd(Nothing(), hydra.ext.python.syntax.ShiftExpression(Nothing(), hydra.ext.python.syntax.Sum(Nothing(), hydra.ext.python.syntax.Term(Nothing(), cast(hydra.ext.python.syntax.Factor, hydra.ext.python.syntax.FactorSimple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(False, prim), Nothing())))))))))

def py_primary_to_py_conjunction(prim: hydra.ext.python.syntax.Primary) -> hydra.ext.python.syntax.Conjunction:
    r"""Convert a Primary to a Conjunction."""
    
    return py_bitwise_or_to_py_conjunction(py_primary_to_py_bitwise_or(prim))

def py_primary_to_py_expression(prim: hydra.ext.python.syntax.Primary) -> hydra.ext.python.syntax.Expression:
    r"""Convert a Primary to an Expression."""
    
    return py_conjunction_to_py_expression(py_primary_to_py_conjunction(prim))

def py_atom_to_py_expression(atom: hydra.ext.python.syntax.Atom) -> hydra.ext.python.syntax.Expression:
    r"""Convert an Atom to an Expression."""
    
    return py_primary_to_py_expression(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(atom)))

def string_to_py_expression(style: hydra.ext.python.syntax.QuoteStyle, s: str) -> hydra.ext.python.syntax.Expression:
    r"""Create a string expression with a given quote style."""
    
    return py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomString(hydra.ext.python.syntax.String(s, style))))

def double_quoted_string(s: str) -> hydra.ext.python.syntax.Expression:
    r"""Create a double-quoted string expression."""
    
    return string_to_py_expression(hydra.ext.python.syntax.QuoteStyle.DOUBLE, s)

def primary_with_rhs(prim: hydra.ext.python.syntax.Primary, rhs: hydra.ext.python.syntax.PrimaryRhs) -> hydra.ext.python.syntax.Primary:
    r"""Combine a Primary with a PrimaryRhs."""
    
    return cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimaryCompound(hydra.ext.python.syntax.PrimaryWithRhs(prim, rhs)))

def primary_with_slices(prim: hydra.ext.python.syntax.Primary, first: hydra.ext.python.syntax.Slice, rest: frozenlist[hydra.ext.python.syntax.SliceOrStarredExpression]) -> hydra.ext.python.syntax.Primary:
    r"""Create a Primary with slices."""
    
    return primary_with_rhs(prim, cast(hydra.ext.python.syntax.PrimaryRhs, hydra.ext.python.syntax.PrimaryRhsSlices(hydra.ext.python.syntax.Slices(first, rest))))

def py_expression_to_py_slice(expr: hydra.ext.python.syntax.Expression) -> hydra.ext.python.syntax.Slice:
    r"""Convert an Expression to a Slice."""
    
    return cast(hydra.ext.python.syntax.Slice, hydra.ext.python.syntax.SliceNamed(cast(hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.NamedExpressionSimple(expr))))

def primary_with_expression_slices(prim: hydra.ext.python.syntax.Primary, exprs: frozenlist[hydra.ext.python.syntax.Expression]) -> hydra.ext.python.syntax.Primary:
    r"""Create a Primary with expression slices."""
    
    return primary_with_slices(prim, py_expression_to_py_slice(hydra.lib.lists.head(exprs)), hydra.lib.lists.map((lambda e: cast(hydra.ext.python.syntax.SliceOrStarredExpression, hydra.ext.python.syntax.SliceOrStarredExpressionSlice(py_expression_to_py_slice(e)))), hydra.lib.lists.tail(exprs)))

def py_name_to_py_primary(name: hydra.ext.python.syntax.Name) -> hydra.ext.python.syntax.Primary:
    r"""Convert a Name to a Primary (simple atom)."""
    
    return cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(name))))

def annotated_expression(mcomment: Maybe[str], expr: hydra.ext.python.syntax.Expression) -> hydra.ext.python.syntax.Expression:
    r"""Annotate an expression with an optional comment using Annotated[]."""
    
    return hydra.lib.maybes.maybe(expr, (lambda c: py_primary_to_py_expression(primary_with_expression_slices(py_name_to_py_primary(hydra.ext.python.syntax.Name("Annotated")), (expr, double_quoted_string(c))))), mcomment)

def annotated_statement(mcomment: Maybe[str], stmt: hydra.ext.python.syntax.Statement) -> hydra.ext.python.syntax.Statement:
    r"""Annotate a statement with an optional comment."""
    
    return hydra.lib.maybes.maybe(stmt, (lambda c: cast(hydra.ext.python.syntax.Statement, hydra.ext.python.syntax.StatementAnnotated(hydra.ext.python.syntax.AnnotatedStatement(c, stmt)))), mcomment)

def py_simple_statement_to_py_statement(s: hydra.ext.python.syntax.SimpleStatement) -> hydra.ext.python.syntax.Statement:
    r"""Convert a SimpleStatement to a Statement."""
    
    return cast(hydra.ext.python.syntax.Statement, hydra.ext.python.syntax.StatementSimple((s,)))

def py_assignment_to_py_statement(a: hydra.ext.python.syntax.Assignment) -> hydra.ext.python.syntax.Statement:
    r"""Convert an Assignment to a Statement."""
    
    return py_simple_statement_to_py_statement(cast(hydra.ext.python.syntax.SimpleStatement, hydra.ext.python.syntax.SimpleStatementAssignment(a)))

def py_name_to_py_star_target(name: hydra.ext.python.syntax.Name) -> hydra.ext.python.syntax.StarTarget:
    r"""Convert a Name to a StarTarget."""
    
    return cast(hydra.ext.python.syntax.StarTarget, hydra.ext.python.syntax.StarTargetUnstarred(cast(hydra.ext.python.syntax.TargetWithStarAtom, hydra.ext.python.syntax.TargetWithStarAtomAtom(cast(hydra.ext.python.syntax.StarAtom, hydra.ext.python.syntax.StarAtomName(name))))))

def assignment(name: hydra.ext.python.syntax.Name, rhs: hydra.ext.python.syntax.AnnotatedRhs) -> hydra.ext.python.syntax.Statement:
    r"""Create an assignment statement from name and annotated rhs."""
    
    return py_assignment_to_py_statement(cast(hydra.ext.python.syntax.Assignment, hydra.ext.python.syntax.AssignmentUntyped(hydra.ext.python.syntax.UntypedAssignment((py_name_to_py_star_target(name),), rhs, Nothing()))))

def py_expression_to_py_annotated_rhs(expr: hydra.ext.python.syntax.Expression) -> hydra.ext.python.syntax.AnnotatedRhs:
    r"""Convert an Expression to an AnnotatedRhs."""
    
    return cast(hydra.ext.python.syntax.AnnotatedRhs, hydra.ext.python.syntax.AnnotatedRhsStar((cast(hydra.ext.python.syntax.StarExpression, hydra.ext.python.syntax.StarExpressionSimple(expr)),)))

def assignment_statement(name: hydra.ext.python.syntax.Name, expr: hydra.ext.python.syntax.Expression) -> hydra.ext.python.syntax.Statement:
    r"""Create an assignment statement from name and expression."""
    
    return assignment(name, py_expression_to_py_annotated_rhs(expr))

def py_expressions_to_py_args(exprs: frozenlist[hydra.ext.python.syntax.Expression]) -> hydra.ext.python.syntax.Args:
    r"""Convert a list of Expressions to Args."""
    
    return hydra.ext.python.syntax.Args(hydra.lib.lists.map((lambda e: cast(hydra.ext.python.syntax.PosArg, hydra.ext.python.syntax.PosArgExpression(e))), exprs), (), ())

def function_call(func: hydra.ext.python.syntax.Primary, args: frozenlist[hydra.ext.python.syntax.Expression]) -> hydra.ext.python.syntax.Expression:
    r"""Create a function call expression."""
    
    return py_primary_to_py_expression(primary_with_rhs(func, cast(hydra.ext.python.syntax.PrimaryRhs, hydra.ext.python.syntax.PrimaryRhsCall(py_expressions_to_py_args(args)))))

def cast_to(pytype: hydra.ext.python.syntax.Expression, pyexpr: hydra.ext.python.syntax.Expression) -> hydra.ext.python.syntax.Expression:
    r"""Create a cast expression: cast(type, expr)."""
    
    return function_call(py_name_to_py_primary(hydra.ext.python.syntax.Name("cast")), (pytype, pyexpr))

def py_expression_to_py_simple_statement(expr: hydra.ext.python.syntax.Expression) -> hydra.ext.python.syntax.SimpleStatement:
    r"""Convert an Expression to a SimpleStatement (as star expressions)."""
    
    return cast(hydra.ext.python.syntax.SimpleStatement, hydra.ext.python.syntax.SimpleStatementStarExpressions((cast(hydra.ext.python.syntax.StarExpression, hydra.ext.python.syntax.StarExpressionSimple(expr)),)))

def py_expression_to_py_statement(expr: hydra.ext.python.syntax.Expression) -> hydra.ext.python.syntax.Statement:
    r"""Convert an Expression to a Statement."""
    
    return py_simple_statement_to_py_statement(py_expression_to_py_simple_statement(expr))

def triple_quoted_string(s: str) -> hydra.ext.python.syntax.Expression:
    r"""Create a triple-quoted string expression."""
    
    return string_to_py_expression(hydra.ext.python.syntax.QuoteStyle.TRIPLE, s)

def comment_statement(s: str) -> hydra.ext.python.syntax.Statement:
    r"""Create a comment statement (triple-quoted string)."""
    
    return py_expression_to_py_statement(triple_quoted_string(s))

def decode_py_power_to_py_primary(p: hydra.ext.python.syntax.Power) -> Maybe[hydra.ext.python.syntax.Primary]:
    r"""Decode a Power to a Primary if possible."""
    
    @lru_cache(1)
    def lhs() -> hydra.ext.python.syntax.AwaitPrimary:
        return p.lhs
    @lru_cache(1)
    def await_() -> bool:
        return lhs().await_
    @lru_cache(1)
    def prim() -> hydra.ext.python.syntax.Primary:
        return lhs().primary
    return hydra.lib.logic.if_else(await_(), (lambda : Nothing()), (lambda : Just(prim())))

def decode_py_comparison_to_py_await_primary(c: hydra.ext.python.syntax.Comparison) -> Maybe[hydra.ext.python.syntax.Primary]:
    r"""Decode a Comparison to a Primary if possible."""
    
    @lru_cache(1)
    def rhs() -> frozenlist[hydra.ext.python.syntax.CompareOpBitwiseOrPair]:
        return c.rhs
    @lru_cache(1)
    def lhs() -> hydra.ext.python.syntax.BitwiseOr:
        return c.lhs
    @lru_cache(1)
    def or_lhs() -> Maybe[hydra.ext.python.syntax.BitwiseOr]:
        return lhs().lhs
    @lru_cache(1)
    def or_rhs() -> hydra.ext.python.syntax.BitwiseXor:
        return lhs().rhs
    @lru_cache(1)
    def xor_lhs() -> Maybe[hydra.ext.python.syntax.BitwiseXor]:
        return or_rhs().lhs
    @lru_cache(1)
    def xor_rhs() -> hydra.ext.python.syntax.BitwiseAnd:
        return or_rhs().rhs
    @lru_cache(1)
    def and_lhs() -> Maybe[hydra.ext.python.syntax.BitwiseAnd]:
        return xor_rhs().lhs
    @lru_cache(1)
    def and_rhs() -> hydra.ext.python.syntax.ShiftExpression:
        return xor_rhs().rhs
    @lru_cache(1)
    def shift_lhs() -> Maybe[hydra.ext.python.syntax.ShiftLhs]:
        return and_rhs().lhs
    @lru_cache(1)
    def shift_rhs() -> hydra.ext.python.syntax.Sum:
        return and_rhs().rhs
    @lru_cache(1)
    def sum_lhs() -> Maybe[hydra.ext.python.syntax.SumLhs]:
        return shift_rhs().lhs
    @lru_cache(1)
    def sum_rhs() -> hydra.ext.python.syntax.Term:
        return shift_rhs().rhs
    @lru_cache(1)
    def term_lhs() -> Maybe[hydra.ext.python.syntax.TermLhs]:
        return sum_rhs().lhs
    @lru_cache(1)
    def term_rhs() -> hydra.ext.python.syntax.Factor:
        return sum_rhs().rhs
    def _hoist_body_1(v1: hydra.ext.python.syntax.Factor) -> Maybe[hydra.ext.python.syntax.Primary]:
        match v1:
            case hydra.ext.python.syntax.FactorSimple(value=power):
                return decode_py_power_to_py_primary(power)
            
            case _:
                return Nothing()
    return hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(rhs())), (lambda : Nothing()), (lambda : hydra.lib.logic.if_else(hydra.lib.maybes.is_just(or_lhs()), (lambda : Nothing()), (lambda : hydra.lib.logic.if_else(hydra.lib.maybes.is_just(xor_lhs()), (lambda : Nothing()), (lambda : hydra.lib.logic.if_else(hydra.lib.maybes.is_just(and_lhs()), (lambda : Nothing()), (lambda : hydra.lib.logic.if_else(hydra.lib.maybes.is_just(shift_lhs()), (lambda : Nothing()), (lambda : hydra.lib.logic.if_else(hydra.lib.maybes.is_just(sum_lhs()), (lambda : Nothing()), (lambda : hydra.lib.logic.if_else(hydra.lib.maybes.is_just(term_lhs()), (lambda : Nothing()), (lambda : _hoist_body_1(term_rhs())))))))))))))))

def decode_py_inversion_to_py_primary(i: hydra.ext.python.syntax.Inversion) -> Maybe[hydra.ext.python.syntax.Primary]:
    r"""Decode an Inversion to a Primary if possible."""
    
    match i:
        case hydra.ext.python.syntax.InversionSimple(value=comparison):
            return decode_py_comparison_to_py_await_primary(comparison)
        
        case _:
            return Nothing()

def decode_py_conjunction_to_py_primary(c: hydra.ext.python.syntax.Conjunction) -> Maybe[hydra.ext.python.syntax.Primary]:
    r"""Decode a Conjunction to a Primary if possible."""
    
    @lru_cache(1)
    def inversions() -> frozenlist[hydra.ext.python.syntax.Inversion]:
        return c.value
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(inversions()), 1), (lambda : decode_py_inversion_to_py_primary(hydra.lib.lists.head(inversions()))), (lambda : Nothing()))

def decode_py_expression_to_py_primary(e: hydra.ext.python.syntax.Expression) -> Maybe[hydra.ext.python.syntax.Primary]:
    r"""Decode an Expression to a Primary if possible."""
    
    match e:
        case hydra.ext.python.syntax.ExpressionSimple(value=disj):
            @lru_cache(1)
            def conjunctions() -> frozenlist[hydra.ext.python.syntax.Conjunction]:
                return disj.value
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(conjunctions()), 1), (lambda : decode_py_conjunction_to_py_primary(hydra.lib.lists.head(conjunctions()))), (lambda : Nothing()))
        
        case _:
            return Nothing()

def find_namespaces(focus_ns: hydra.module.Namespace, defs: frozenlist[hydra.module.Definition]) -> hydra.module.Namespaces[hydra.ext.python.syntax.DottedName]:
    r"""Find all namespaces referenced by a list of definitions, plus the core namespace."""
    
    core_ns = hydra.module.Namespace("hydra.core")
    @lru_cache(1)
    def namespaces() -> hydra.module.Namespaces[hydra.ext.python.syntax.DottedName]:
        return hydra.schemas.namespaces_for_definitions((lambda x1: hydra.ext.python.names.encode_namespace(x1)), focus_ns, defs)
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.pairs.first(namespaces().focus).value, core_ns.value), (lambda : namespaces()), (lambda : hydra.module.Namespaces(namespaces().focus, hydra.lib.maps.insert(core_ns, hydra.ext.python.names.encode_namespace(core_ns), namespaces().mapping))))

@lru_cache(1)
def get_item_params() -> hydra.ext.python.syntax.Parameters:
    return cast(hydra.ext.python.syntax.Parameters, hydra.ext.python.syntax.ParametersParamNoDefault(hydra.ext.python.syntax.ParamNoDefaultParameters((hydra.ext.python.syntax.ParamNoDefault(hydra.ext.python.syntax.Param(hydra.ext.python.syntax.Name("cls"), Nothing()), Nothing()), hydra.ext.python.syntax.ParamNoDefault(hydra.ext.python.syntax.Param(hydra.ext.python.syntax.Name("item"), Nothing()), Nothing())), (), Nothing())))

def indented_block(mcomment: Maybe[str], stmts: frozenlist[frozenlist[hydra.ext.python.syntax.Statement]]) -> hydra.ext.python.syntax.Block:
    r"""Create an indented block with optional comment."""
    
    @lru_cache(1)
    def comment_group() -> frozenlist[hydra.ext.python.syntax.Statement]:
        return hydra.lib.maybes.maybe((), (lambda s: (comment_statement(s),)), mcomment)
    @lru_cache(1)
    def groups() -> frozenlist[frozenlist[hydra.ext.python.syntax.Statement]]:
        return hydra.lib.lists.filter((lambda g: hydra.lib.logic.not_(hydra.lib.lists.null(g))), hydra.lib.lists.cons(comment_group(), stmts))
    return hydra.lib.logic.if_else(hydra.lib.lists.null(groups()), (lambda : cast(hydra.ext.python.syntax.Block, hydra.ext.python.syntax.BlockSimple((py_expression_to_py_simple_statement(py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomEllipsis()))),)))), (lambda : cast(hydra.ext.python.syntax.Block, hydra.ext.python.syntax.BlockIndented(groups()))))

def primary_and_params(prim: hydra.ext.python.syntax.Primary, params: frozenlist[hydra.ext.python.syntax.Expression]) -> hydra.ext.python.syntax.Expression:
    r"""Create a primary with parameters (subscript)."""
    
    return py_primary_to_py_expression(primary_with_expression_slices(prim, params))

def name_and_params(py_name: hydra.ext.python.syntax.Name, params: frozenlist[hydra.ext.python.syntax.Expression]) -> hydra.ext.python.syntax.Expression:
    r"""Create a name with parameters."""
    
    return primary_and_params(py_name_to_py_primary(py_name), params)

def newtype_statement(name: hydra.ext.python.syntax.Name, mcomment: Maybe[str], expr: hydra.ext.python.syntax.Expression) -> hydra.ext.python.syntax.Statement:
    r"""Create a NewType statement."""
    
    return annotated_statement(mcomment, assignment_statement(name, function_call(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("NewType"))))), (double_quoted_string(name.value), expr))))

def py_bitwise_or_to_py_expression(bor: hydra.ext.python.syntax.BitwiseOr) -> hydra.ext.python.syntax.Expression:
    r"""Convert a BitwiseOr to an Expression."""
    
    return py_conjunction_to_py_expression(py_bitwise_or_to_py_conjunction(bor))

def py_primary_to_py_bitwise_xor(prim: hydra.ext.python.syntax.Primary) -> hydra.ext.python.syntax.BitwiseXor:
    r"""Convert a Primary to a BitwiseXor."""
    
    return hydra.ext.python.syntax.BitwiseXor(Nothing(), hydra.ext.python.syntax.BitwiseAnd(Nothing(), hydra.ext.python.syntax.ShiftExpression(Nothing(), hydra.ext.python.syntax.Sum(Nothing(), hydra.ext.python.syntax.Term(Nothing(), cast(hydra.ext.python.syntax.Factor, hydra.ext.python.syntax.FactorSimple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(False, prim), Nothing()))))))))

def or_expression(prims: frozenlist[hydra.ext.python.syntax.Primary]) -> hydra.ext.python.syntax.Expression:
    r"""Build an or-expression from multiple primaries."""
    
    def build(prev: Maybe[hydra.ext.python.syntax.BitwiseOr], ps: frozenlist[hydra.ext.python.syntax.Primary]) -> hydra.ext.python.syntax.BitwiseOr:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(hydra.lib.lists.tail(ps)), (lambda : hydra.ext.python.syntax.BitwiseOr(prev, py_primary_to_py_bitwise_xor(hydra.lib.lists.head(ps)))), (lambda : build(Just(hydra.ext.python.syntax.BitwiseOr(prev, py_primary_to_py_bitwise_xor(hydra.lib.lists.head(ps)))), hydra.lib.lists.tail(ps))))
    return py_bitwise_or_to_py_expression(build(Nothing(), prims))

def project_from_expression(exp: hydra.ext.python.syntax.Expression, name: hydra.ext.python.syntax.Name) -> hydra.ext.python.syntax.Expression:
    r"""Project a field from an expression."""
    
    @lru_cache(1)
    def prim() -> hydra.ext.python.syntax.Primary:
        return cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomGroup(cast(hydra.ext.python.syntax.Group, hydra.ext.python.syntax.GroupExpression(cast(hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.NamedExpressionSimple(exp))))))))
    return py_primary_to_py_expression(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimaryCompound(hydra.ext.python.syntax.PrimaryWithRhs(prim(), cast(hydra.ext.python.syntax.PrimaryRhs, hydra.ext.python.syntax.PrimaryRhsProject(name))))))

def py_class_definition_to_py_statement(cd: hydra.ext.python.syntax.ClassDefinition) -> hydra.ext.python.syntax.Statement:
    r"""Convert a ClassDefinition to a Statement."""
    
    return cast(hydra.ext.python.syntax.Statement, hydra.ext.python.syntax.StatementCompound(cast(hydra.ext.python.syntax.CompoundStatement, hydra.ext.python.syntax.CompoundStatementClassDef(cd))))

def py_closed_pattern_to_py_patterns(p: hydra.ext.python.syntax.ClosedPattern) -> hydra.ext.python.syntax.Patterns:
    r"""Convert a ClosedPattern to Patterns."""
    
    return cast(hydra.ext.python.syntax.Patterns, hydra.ext.python.syntax.PatternsPattern(cast(hydra.ext.python.syntax.Pattern, hydra.ext.python.syntax.PatternOr(hydra.ext.python.syntax.OrPattern((p,))))))

def py_expression_to_py_primary(e: hydra.ext.python.syntax.Expression) -> hydra.ext.python.syntax.Primary:
    r"""Extracts the primary from an expression, or wraps it in parentheses if the expression does not contain a primary."""
    
    return hydra.lib.maybes.maybe(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomGroup(cast(hydra.ext.python.syntax.Group, hydra.ext.python.syntax.GroupExpression(cast(hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.NamedExpressionSimple(e)))))))), (lambda prim: prim), decode_py_expression_to_py_primary(e))

def py_expression_to_py_star_named_expression(expr: hydra.ext.python.syntax.Expression) -> hydra.ext.python.syntax.StarNamedExpression:
    r"""Convert an Expression to a StarNamedExpression."""
    
    return cast(hydra.ext.python.syntax.StarNamedExpression, hydra.ext.python.syntax.StarNamedExpressionSimple(cast(hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.NamedExpressionSimple(expr))))

def py_list(exprs: frozenlist[hydra.ext.python.syntax.Expression]) -> hydra.ext.python.syntax.List:
    r"""Create a Python list from expressions."""
    
    return hydra.ext.python.syntax.List(hydra.lib.lists.map((lambda x1: py_expression_to_py_star_named_expression(x1)), exprs))

def py_name_to_py_expression(name: hydra.ext.python.syntax.Name) -> hydra.ext.python.syntax.Expression:
    r"""Convert a Name to an Expression."""
    
    return py_primary_to_py_expression(py_name_to_py_primary(name))

def py_name_to_py_named_expression(name: hydra.ext.python.syntax.Name) -> hydra.ext.python.syntax.NamedExpression:
    r"""Convert a Name to a NamedExpression."""
    
    return cast(hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.NamedExpressionSimple(py_name_to_py_expression(name)))

def py_name_to_py_type_parameter(name: hydra.ext.python.syntax.Name) -> hydra.ext.python.syntax.TypeParameter:
    r"""Convert a Name to a TypeParameter."""
    
    return cast(hydra.ext.python.syntax.TypeParameter, hydra.ext.python.syntax.TypeParameterSimple(hydra.ext.python.syntax.SimpleTypeParameter(name, Nothing(), Nothing())))

# The Python None value as a Name.
py_none = hydra.ext.python.syntax.Name("None")

def py_primary_to_py_slice(prim: hydra.ext.python.syntax.Primary) -> hydra.ext.python.syntax.Slice:
    r"""Convert a Primary to a Slice."""
    
    return py_expression_to_py_slice(py_primary_to_py_expression(prim))

def raise_assertion_error(msg: str) -> hydra.ext.python.syntax.Statement:
    r"""Create a raise AssertionError statement."""
    
    return py_simple_statement_to_py_statement(cast(hydra.ext.python.syntax.SimpleStatement, hydra.ext.python.syntax.SimpleStatementRaise(hydra.ext.python.syntax.RaiseStatement(Just(hydra.ext.python.syntax.RaiseExpression(function_call(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("AssertionError"))))), (double_quoted_string(msg),)), Nothing()))))))

def raise_type_error(msg: str) -> hydra.ext.python.syntax.Statement:
    r"""Create a raise TypeError statement."""
    
    return py_simple_statement_to_py_statement(cast(hydra.ext.python.syntax.SimpleStatement, hydra.ext.python.syntax.SimpleStatementRaise(hydra.ext.python.syntax.RaiseStatement(Just(hydra.ext.python.syntax.RaiseExpression(function_call(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("TypeError"))))), (double_quoted_string(msg),)), Nothing()))))))

def return_single(expr: hydra.ext.python.syntax.Expression) -> hydra.ext.python.syntax.Statement:
    r"""Create a return statement with a single expression."""
    
    return py_simple_statement_to_py_statement(cast(hydra.ext.python.syntax.SimpleStatement, hydra.ext.python.syntax.SimpleStatementReturn(hydra.ext.python.syntax.ReturnStatement((cast(hydra.ext.python.syntax.StarExpression, hydra.ext.python.syntax.StarExpressionSimple(expr)),)))))

@lru_cache(1)
def self_only_params() -> hydra.ext.python.syntax.Parameters:
    return cast(hydra.ext.python.syntax.Parameters, hydra.ext.python.syntax.ParametersParamNoDefault(hydra.ext.python.syntax.ParamNoDefaultParameters((hydra.ext.python.syntax.ParamNoDefault(hydra.ext.python.syntax.Param(hydra.ext.python.syntax.Name("self"), Nothing()), Nothing()),), (), Nothing())))

@lru_cache(1)
def self_other_params() -> hydra.ext.python.syntax.Parameters:
    return cast(hydra.ext.python.syntax.Parameters, hydra.ext.python.syntax.ParametersParamNoDefault(hydra.ext.python.syntax.ParamNoDefaultParameters((hydra.ext.python.syntax.ParamNoDefault(hydra.ext.python.syntax.Param(hydra.ext.python.syntax.Name("self"), Nothing()), Nothing()), hydra.ext.python.syntax.ParamNoDefault(hydra.ext.python.syntax.Param(hydra.ext.python.syntax.Name("other"), Nothing()), Nothing())), (), Nothing())))

def single_quoted_string(s: str) -> hydra.ext.python.syntax.Expression:
    r"""Create a single-quoted string expression."""
    
    return string_to_py_expression(hydra.ext.python.syntax.QuoteStyle.SINGLE, s)

# Current target Python version for code generation.
target_python_version = hydra.ext.python.helpers.PythonVersion.PYTHON310

def type_alias_statement(name: hydra.ext.python.syntax.Name, tparams: frozenlist[hydra.ext.python.syntax.TypeParameter], mcomment: Maybe[str], tyexpr: hydra.ext.python.syntax.Expression) -> hydra.ext.python.syntax.Statement:
    r"""Generate a type alias statement using PEP 695 syntax (Python 3.12+)."""
    
    return annotated_statement(mcomment, py_simple_statement_to_py_statement(cast(hydra.ext.python.syntax.SimpleStatement, hydra.ext.python.syntax.SimpleStatementTypeAlias(hydra.ext.python.syntax.TypeAlias(name, tparams, tyexpr)))))

def type_alias_statement310(name: hydra.ext.python.syntax.Name, _tparams: T0, mcomment: Maybe[str], tyexpr: hydra.ext.python.syntax.Expression) -> hydra.ext.python.syntax.Statement:
    @lru_cache(1)
    def quoted_expr() -> hydra.ext.python.syntax.Expression:
        return double_quoted_string(hydra.serialization.print_expr(hydra.ext.python.serde.encode_expression(tyexpr)))
    return annotated_statement(mcomment, py_assignment_to_py_statement(cast(hydra.ext.python.syntax.Assignment, hydra.ext.python.syntax.AssignmentTyped(hydra.ext.python.syntax.TypedAssignment(cast(hydra.ext.python.syntax.SingleTarget, hydra.ext.python.syntax.SingleTargetName(name)), cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionSimple(hydra.ext.python.syntax.Disjunction((hydra.ext.python.syntax.Conjunction((cast(hydra.ext.python.syntax.Inversion, hydra.ext.python.syntax.InversionSimple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(Nothing(), hydra.ext.python.syntax.BitwiseXor(Nothing(), hydra.ext.python.syntax.BitwiseAnd(Nothing(), hydra.ext.python.syntax.ShiftExpression(Nothing(), hydra.ext.python.syntax.Sum(Nothing(), hydra.ext.python.syntax.Term(Nothing(), cast(hydra.ext.python.syntax.Factor, hydra.ext.python.syntax.FactorSimple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(False, cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("TypeAlias")))))), Nothing()))))))))), ()))),)),)))), Just(py_expression_to_py_annotated_rhs(quoted_expr())))))))

def union_type_class_statements310(name: hydra.ext.python.syntax.Name, mcomment: Maybe[str], tyexpr: hydra.ext.python.syntax.Expression) -> frozenlist[hydra.ext.python.syntax.Statement]:
    r"""Generate a subscriptable union class for Python 3.10."""
    
    @lru_cache(1)
    def name_str() -> str:
        return name.value
    @lru_cache(1)
    def meta_name() -> hydra.ext.python.syntax.Name:
        return hydra.ext.python.syntax.Name(hydra.lib.strings.cat2(hydra.lib.strings.cat2("_", name_str()), "Meta"))
    @lru_cache(1)
    def doc_string() -> str:
        return hydra.serialization.print_expr(hydra.ext.python.serde.encode_expression(tyexpr))
    @lru_cache(1)
    def return_object() -> hydra.ext.python.syntax.Statement:
        return py_simple_statement_to_py_statement(cast(hydra.ext.python.syntax.SimpleStatement, hydra.ext.python.syntax.SimpleStatementReturn(hydra.ext.python.syntax.ReturnStatement((cast(hydra.ext.python.syntax.StarExpression, hydra.ext.python.syntax.StarExpressionSimple(cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionSimple(hydra.ext.python.syntax.Disjunction((hydra.ext.python.syntax.Conjunction((cast(hydra.ext.python.syntax.Inversion, hydra.ext.python.syntax.InversionSimple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(Nothing(), hydra.ext.python.syntax.BitwiseXor(Nothing(), hydra.ext.python.syntax.BitwiseAnd(Nothing(), hydra.ext.python.syntax.ShiftExpression(Nothing(), hydra.ext.python.syntax.Sum(Nothing(), hydra.ext.python.syntax.Term(Nothing(), cast(hydra.ext.python.syntax.Factor, hydra.ext.python.syntax.FactorSimple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(False, cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("object")))))), Nothing()))))))))), ()))),)),)))))),)))))
    @lru_cache(1)
    def get_item_method() -> hydra.ext.python.syntax.Statement:
        return cast(hydra.ext.python.syntax.Statement, hydra.ext.python.syntax.StatementCompound(cast(hydra.ext.python.syntax.CompoundStatement, hydra.ext.python.syntax.CompoundStatementFunction(hydra.ext.python.syntax.FunctionDefinition(Nothing(), hydra.ext.python.syntax.FunctionDefRaw(False, hydra.ext.python.syntax.Name("__getitem__"), (), Just(get_item_params()), Nothing(), Nothing(), indented_block(Nothing(), ((return_object(),),))))))))
    @lru_cache(1)
    def meta_class() -> hydra.ext.python.syntax.Statement:
        return py_class_definition_to_py_statement(hydra.ext.python.syntax.ClassDefinition(Nothing(), meta_name(), (), Just(py_expressions_to_py_args((cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionSimple(hydra.ext.python.syntax.Disjunction((hydra.ext.python.syntax.Conjunction((cast(hydra.ext.python.syntax.Inversion, hydra.ext.python.syntax.InversionSimple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(Nothing(), hydra.ext.python.syntax.BitwiseXor(Nothing(), hydra.ext.python.syntax.BitwiseAnd(Nothing(), hydra.ext.python.syntax.ShiftExpression(Nothing(), hydra.ext.python.syntax.Sum(Nothing(), hydra.ext.python.syntax.Term(Nothing(), cast(hydra.ext.python.syntax.Factor, hydra.ext.python.syntax.FactorSimple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(False, cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("type")))))), Nothing()))))))))), ()))),)),)))),))), indented_block(Nothing(), ((get_item_method(),),))))
    @lru_cache(1)
    def pass_stmt() -> hydra.ext.python.syntax.Statement:
        return py_simple_statement_to_py_statement(cast(hydra.ext.python.syntax.SimpleStatement, hydra.ext.python.syntax.SimpleStatementPass()))
    @lru_cache(1)
    def doc_stmt() -> hydra.ext.python.syntax.Statement:
        return py_expression_to_py_statement(triple_quoted_string(doc_string()))
    @lru_cache(1)
    def metaclass_arg() -> hydra.ext.python.syntax.Kwarg:
        return hydra.ext.python.syntax.Kwarg(hydra.ext.python.syntax.Name("metaclass"), cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionSimple(hydra.ext.python.syntax.Disjunction((hydra.ext.python.syntax.Conjunction((cast(hydra.ext.python.syntax.Inversion, hydra.ext.python.syntax.InversionSimple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(Nothing(), hydra.ext.python.syntax.BitwiseXor(Nothing(), hydra.ext.python.syntax.BitwiseAnd(Nothing(), hydra.ext.python.syntax.ShiftExpression(Nothing(), hydra.ext.python.syntax.Sum(Nothing(), hydra.ext.python.syntax.Term(Nothing(), cast(hydra.ext.python.syntax.Factor, hydra.ext.python.syntax.FactorSimple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(False, cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(meta_name()))))), Nothing()))))))))), ()))),)),)))))
    @lru_cache(1)
    def union_class() -> hydra.ext.python.syntax.Statement:
        return annotated_statement(mcomment, py_class_definition_to_py_statement(hydra.ext.python.syntax.ClassDefinition(Nothing(), name, (), Just(hydra.ext.python.syntax.Args((), (cast(hydra.ext.python.syntax.KwargOrStarred, hydra.ext.python.syntax.KwargOrStarredKwarg(metaclass_arg())),), ())), indented_block(Nothing(), ((doc_stmt(),), (pass_stmt(),))))))
    return (meta_class(), union_class())

def unit_variant_methods(class_name: hydra.ext.python.syntax.Name) -> frozenlist[hydra.ext.python.syntax.Statement]:
    r"""Generate __slots__, __eq__, and __hash__ methods for unit-typed union variants."""
    
    @lru_cache(1)
    def class_name_str() -> str:
        return class_name.value
    @lru_cache(1)
    def slots_stmt() -> hydra.ext.python.syntax.Statement:
        return assignment_statement(hydra.ext.python.syntax.Name("__slots__"), py_primary_to_py_expression(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomTuple(hydra.ext.python.syntax.Tuple(())))))))
    @lru_cache(1)
    def return_isinstance() -> hydra.ext.python.syntax.Statement:
        return py_simple_statement_to_py_statement(cast(hydra.ext.python.syntax.SimpleStatement, hydra.ext.python.syntax.SimpleStatementReturn(hydra.ext.python.syntax.ReturnStatement((cast(hydra.ext.python.syntax.StarExpression, hydra.ext.python.syntax.StarExpressionSimple(function_call(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("isinstance"))))), (cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionSimple(hydra.ext.python.syntax.Disjunction((hydra.ext.python.syntax.Conjunction((cast(hydra.ext.python.syntax.Inversion, hydra.ext.python.syntax.InversionSimple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(Nothing(), hydra.ext.python.syntax.BitwiseXor(Nothing(), hydra.ext.python.syntax.BitwiseAnd(Nothing(), hydra.ext.python.syntax.ShiftExpression(Nothing(), hydra.ext.python.syntax.Sum(Nothing(), hydra.ext.python.syntax.Term(Nothing(), cast(hydra.ext.python.syntax.Factor, hydra.ext.python.syntax.FactorSimple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(False, cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("other")))))), Nothing()))))))))), ()))),)),)))), cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionSimple(hydra.ext.python.syntax.Disjunction((hydra.ext.python.syntax.Conjunction((cast(hydra.ext.python.syntax.Inversion, hydra.ext.python.syntax.InversionSimple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(Nothing(), hydra.ext.python.syntax.BitwiseXor(Nothing(), hydra.ext.python.syntax.BitwiseAnd(Nothing(), hydra.ext.python.syntax.ShiftExpression(Nothing(), hydra.ext.python.syntax.Sum(Nothing(), hydra.ext.python.syntax.Term(Nothing(), cast(hydra.ext.python.syntax.Factor, hydra.ext.python.syntax.FactorSimple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(False, cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(class_name))))), Nothing()))))))))), ()))),)),)))))))),)))))
    @lru_cache(1)
    def eq_method() -> hydra.ext.python.syntax.Statement:
        return cast(hydra.ext.python.syntax.Statement, hydra.ext.python.syntax.StatementCompound(cast(hydra.ext.python.syntax.CompoundStatement, hydra.ext.python.syntax.CompoundStatementFunction(hydra.ext.python.syntax.FunctionDefinition(Nothing(), hydra.ext.python.syntax.FunctionDefRaw(False, hydra.ext.python.syntax.Name("__eq__"), (), Just(self_other_params()), Nothing(), Nothing(), indented_block(Nothing(), ((return_isinstance(),),))))))))
    @lru_cache(1)
    def return_hash() -> hydra.ext.python.syntax.Statement:
        return py_simple_statement_to_py_statement(cast(hydra.ext.python.syntax.SimpleStatement, hydra.ext.python.syntax.SimpleStatementReturn(hydra.ext.python.syntax.ReturnStatement((cast(hydra.ext.python.syntax.StarExpression, hydra.ext.python.syntax.StarExpressionSimple(function_call(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("hash"))))), (double_quoted_string(class_name_str()),)))),)))))
    @lru_cache(1)
    def hash_method() -> hydra.ext.python.syntax.Statement:
        return cast(hydra.ext.python.syntax.Statement, hydra.ext.python.syntax.StatementCompound(cast(hydra.ext.python.syntax.CompoundStatement, hydra.ext.python.syntax.CompoundStatementFunction(hydra.ext.python.syntax.FunctionDefinition(Nothing(), hydra.ext.python.syntax.FunctionDefRaw(False, hydra.ext.python.syntax.Name("__hash__"), (), Just(self_only_params()), Nothing(), Nothing(), indented_block(Nothing(), ((return_hash(),),))))))))
    return (slots_stmt(), eq_method(), hash_method())
