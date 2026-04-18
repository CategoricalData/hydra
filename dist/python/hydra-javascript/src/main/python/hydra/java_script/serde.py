# Note: this is an automatically generated file. Do not edit.

r"""Serialization functions for converting JavaScript AST to abstract expressions."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.constants
import hydra.core
import hydra.java_script.operators
import hydra.java_script.syntax
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maybes
import hydra.lib.strings
import hydra.serialization

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def identifier_to_expr(id: hydra.java_script.syntax.Identifier) -> hydra.ast.Expr:
    r"""Convert an identifier to an AST expression."""

    return hydra.serialization.cst(id.value)

def break_statement_to_expr(b: Maybe[hydra.java_script.syntax.Identifier]) -> hydra.ast.Expr:
    r"""Convert a break statement to an AST expression."""

    return hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("break;")), (lambda label: hydra.serialization.suffix(";", hydra.serialization.space_sep((hydra.serialization.cst("break"), identifier_to_expr(label))))), b)

def continue_statement_to_expr(c: Maybe[hydra.java_script.syntax.Identifier]) -> hydra.ast.Expr:
    r"""Convert a continue statement to an AST expression."""

    return hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("continue;")), (lambda label: hydra.serialization.suffix(";", hydra.serialization.space_sep((hydra.serialization.cst("continue"), identifier_to_expr(label))))), c)

def variable_kind_to_expr(kind: hydra.java_script.syntax.VariableKind) -> hydra.ast.Expr:
    r"""Convert a variable kind to an AST expression."""

    match kind:
        case hydra.java_script.syntax.VariableKind.VAR:
            return hydra.serialization.cst("var")

        case hydra.java_script.syntax.VariableKind.LET:
            return hydra.serialization.cst("let")

        case hydra.java_script.syntax.VariableKind.CONST:
            return hydra.serialization.cst("const")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def assignment_operator_to_string(op: hydra.java_script.syntax.AssignmentOperator) -> str:
    r"""Convert an assignment operator to a string."""

    match op:
        case hydra.java_script.syntax.AssignmentOperator.ASSIGN:
            return "="

        case hydra.java_script.syntax.AssignmentOperator.ADD_ASSIGN:
            return "+="

        case hydra.java_script.syntax.AssignmentOperator.SUBTRACT_ASSIGN:
            return "-="

        case hydra.java_script.syntax.AssignmentOperator.MULTIPLY_ASSIGN:
            return "*="

        case hydra.java_script.syntax.AssignmentOperator.DIVIDE_ASSIGN:
            return "/="

        case hydra.java_script.syntax.AssignmentOperator.MODULO_ASSIGN:
            return "%="

        case hydra.java_script.syntax.AssignmentOperator.EXPONENTIATE_ASSIGN:
            return "**="

        case hydra.java_script.syntax.AssignmentOperator.LEFT_SHIFT_ASSIGN:
            return "<<="

        case hydra.java_script.syntax.AssignmentOperator.RIGHT_SHIFT_ASSIGN:
            return ">>="

        case hydra.java_script.syntax.AssignmentOperator.UNSIGNED_RIGHT_SHIFT_ASSIGN:
            return ">>>="

        case hydra.java_script.syntax.AssignmentOperator.BITWISE_AND_ASSIGN:
            return "&="

        case hydra.java_script.syntax.AssignmentOperator.BITWISE_OR_ASSIGN:
            return "|="

        case hydra.java_script.syntax.AssignmentOperator.BITWISE_XOR_ASSIGN:
            return "^="

        case hydra.java_script.syntax.AssignmentOperator.AND_ASSIGN:
            return "&&="

        case hydra.java_script.syntax.AssignmentOperator.OR_ASSIGN:
            return "||="

        case hydra.java_script.syntax.AssignmentOperator.NULLISH_ASSIGN:
            return "??="

        case _:
            raise AssertionError("Unreachable: all variants handled")

def binary_operator_to_expr(op: hydra.java_script.syntax.BinaryOperator) -> hydra.ast.Op:
    r"""Convert a binary operator to an Op."""

    match op:
        case hydra.java_script.syntax.BinaryOperator.ADD:
            return hydra.java_script.operators.add_op()

        case hydra.java_script.syntax.BinaryOperator.SUBTRACT:
            return hydra.java_script.operators.subtract_op()

        case hydra.java_script.syntax.BinaryOperator.MULTIPLY:
            return hydra.java_script.operators.multiply_op()

        case hydra.java_script.syntax.BinaryOperator.DIVIDE:
            return hydra.java_script.operators.divide_op()

        case hydra.java_script.syntax.BinaryOperator.MODULO:
            return hydra.java_script.operators.modulo_op()

        case hydra.java_script.syntax.BinaryOperator.EXPONENTIATE:
            return hydra.java_script.operators.exponentiate_op()

        case hydra.java_script.syntax.BinaryOperator.EQUAL:
            return hydra.java_script.operators.equal_op()

        case hydra.java_script.syntax.BinaryOperator.NOT_EQUAL:
            return hydra.java_script.operators.not_equal_op()

        case hydra.java_script.syntax.BinaryOperator.STRICT_EQUAL:
            return hydra.java_script.operators.strict_equal_op()

        case hydra.java_script.syntax.BinaryOperator.STRICT_NOT_EQUAL:
            return hydra.java_script.operators.strict_not_equal_op()

        case hydra.java_script.syntax.BinaryOperator.LESS_THAN:
            return hydra.java_script.operators.less_than_op()

        case hydra.java_script.syntax.BinaryOperator.LESS_THAN_OR_EQUAL:
            return hydra.java_script.operators.less_than_or_equal_op()

        case hydra.java_script.syntax.BinaryOperator.GREATER_THAN:
            return hydra.java_script.operators.greater_than_op()

        case hydra.java_script.syntax.BinaryOperator.GREATER_THAN_OR_EQUAL:
            return hydra.java_script.operators.greater_than_or_equal_op()

        case hydra.java_script.syntax.BinaryOperator.AND:
            return hydra.java_script.operators.logical_and_op()

        case hydra.java_script.syntax.BinaryOperator.OR:
            return hydra.java_script.operators.logical_or_op()

        case hydra.java_script.syntax.BinaryOperator.NULLISH_COALESCING:
            return hydra.java_script.operators.nullish_coalescing_op()

        case hydra.java_script.syntax.BinaryOperator.BITWISE_AND:
            return hydra.java_script.operators.bitwise_and_op()

        case hydra.java_script.syntax.BinaryOperator.BITWISE_OR:
            return hydra.java_script.operators.bitwise_or_op()

        case hydra.java_script.syntax.BinaryOperator.BITWISE_XOR:
            return hydra.java_script.operators.bitwise_xor_op()

        case hydra.java_script.syntax.BinaryOperator.LEFT_SHIFT:
            return hydra.java_script.operators.left_shift_op()

        case hydra.java_script.syntax.BinaryOperator.RIGHT_SHIFT:
            return hydra.java_script.operators.right_shift_op()

        case hydra.java_script.syntax.BinaryOperator.UNSIGNED_RIGHT_SHIFT:
            return hydra.java_script.operators.unsigned_right_shift_op()

        case hydra.java_script.syntax.BinaryOperator.IN:
            return hydra.java_script.operators.in_op()

        case hydra.java_script.syntax.BinaryOperator.INSTANCEOF:
            return hydra.java_script.operators.instance_of_op()

        case _:
            raise AssertionError("Unreachable: all variants handled")

def numeric_literal_to_expr(n: hydra.java_script.syntax.NumericLiteral) -> hydra.ast.Expr:
    r"""Convert a numeric literal to an AST expression."""

    match n:
        case hydra.java_script.syntax.NumericLiteralInteger(value=i):
            return hydra.serialization.cst(hydra.lib.literals.show_int64(i))

        case hydra.java_script.syntax.NumericLiteralFloat(value=f):
            return hydra.serialization.cst(hydra.lib.literals.show_float64(f))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def escape_string(s: T0, single_quote: T1) -> T0:
    r"""Escape special characters in a string for JavaScript."""

    return s

def string_literal_to_expr(s: hydra.java_script.syntax.StringLiteral) -> hydra.ast.Expr:
    r"""Convert a string literal to an AST expression."""

    value = s.value
    single_quote = s.single_quote
    @lru_cache(1)
    def quote() -> str:
        return hydra.lib.logic.if_else(single_quote, (lambda : "'"), (lambda : "\""))
    @lru_cache(1)
    def escaped() -> str:
        return escape_string(value, single_quote)
    return hydra.serialization.cst(hydra.lib.strings.cat((quote(), escaped(), quote())))

def template_literal_to_expr(t: hydra.java_script.syntax.TemplateLiteral) -> hydra.ast.Expr:
    r"""Convert a template literal to an AST expression."""

    quasis = t.quasis
    exprs = t.expressions
    return hydra.serialization.cst(hydra.lib.strings.cat(("`", hydra.lib.strings.intercalate("", hydra.lib.lists.map((lambda q: q.value), quasis)), "`")))

def literal_to_expr(lit: hydra.java_script.syntax.Literal) -> hydra.ast.Expr:
    r"""Convert a literal to an AST expression."""

    match lit:
        case hydra.java_script.syntax.LiteralString(value=s):
            return string_literal_to_expr(s)

        case hydra.java_script.syntax.LiteralNumber(value=n):
            return numeric_literal_to_expr(n)

        case hydra.java_script.syntax.LiteralBoolean(value=b):
            return hydra.serialization.cst(hydra.lib.logic.if_else(b, (lambda : "true"), (lambda : "false")))

        case hydra.java_script.syntax.LiteralNull():
            return hydra.serialization.cst("null")

        case hydra.java_script.syntax.LiteralUndefined():
            return hydra.serialization.cst("undefined")

        case hydra.java_script.syntax.LiteralBigInt(value=n2):
            return hydra.serialization.cst(hydra.lib.strings.cat2(hydra.lib.literals.show_bigint(n2), "n"))

        case hydra.java_script.syntax.LiteralTemplate(value=t):
            return template_literal_to_expr(t)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def unary_operator_to_string(op: hydra.java_script.syntax.UnaryOperator) -> str:
    r"""Convert a unary operator to a string."""

    match op:
        case hydra.java_script.syntax.UnaryOperator.NEGATE:
            return "-"

        case hydra.java_script.syntax.UnaryOperator.PLUS:
            return "+"

        case hydra.java_script.syntax.UnaryOperator.NOT:
            return "!"

        case hydra.java_script.syntax.UnaryOperator.BITWISE_NOT:
            return "~"

        case hydra.java_script.syntax.UnaryOperator.TYPEOF:
            return "typeof "

        case hydra.java_script.syntax.UnaryOperator.VOID:
            return "void "

        case hydra.java_script.syntax.UnaryOperator.DELETE:
            return "delete "

        case hydra.java_script.syntax.UnaryOperator.INCREMENT:
            return "++"

        case hydra.java_script.syntax.UnaryOperator.DECREMENT:
            return "--"

        case _:
            raise AssertionError("Unreachable: all variants handled")

def array_element_to_expr(elem: hydra.java_script.syntax.ArrayElement) -> hydra.ast.Expr:
    r"""Convert an array element to an AST expression."""

    match elem:
        case hydra.java_script.syntax.ArrayElementExpression(value=e):
            return expression_to_expr(e)

        case hydra.java_script.syntax.ArrayElementSpread(value=s):
            return hydra.serialization.prefix("...", expression_to_expr(s.value))

        case hydra.java_script.syntax.ArrayElementHole():
            return hydra.serialization.cst("")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def array_expression_to_expr(arr: frozenlist[hydra.java_script.syntax.ArrayElement]) -> hydra.ast.Expr:
    r"""Convert an array expression to an AST expression."""

    return hydra.serialization.bracket_list(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: array_element_to_expr(x1)), arr))

def array_pattern_to_expr(arr: frozenlist[Maybe[hydra.java_script.syntax.Pattern]]) -> hydra.ast.Expr:
    r"""Convert an array pattern to an AST expression."""

    return hydra.serialization.bracket_list(hydra.serialization.inline_style, hydra.lib.lists.map((lambda maybe_p: hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("")), (lambda x1: pattern_to_expr(x1)), maybe_p)), arr))

def arrow_function_expression_to_expr(arrow: hydra.java_script.syntax.ArrowFunctionExpression) -> hydra.ast.Expr:
    r"""Convert an arrow function expression to an AST expression."""

    params = arrow.params
    body = arrow.body
    async_ = arrow.async_
    @lru_cache(1)
    def async_kw() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(async_, (lambda : (hydra.serialization.cst("async"),)), (lambda : ()))
    @lru_cache(1)
    def params_expr() -> hydra.ast.Expr:
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(params), 1), (lambda : hydra.lib.maybes.from_maybe((lambda : hydra.serialization.cst("")), hydra.lib.maybes.map((lambda x1: pattern_to_expr(x1)), hydra.lib.lists.maybe_head(params)))), (lambda : hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: pattern_to_expr(x1)), params))))
    @lru_cache(1)
    def body_expr() -> hydra.ast.Expr:
        match body:
            case hydra.java_script.syntax.ArrowFunctionBodyExpression(value=e):
                return expression_to_expr(e)

            case hydra.java_script.syntax.ArrowFunctionBodyBlock(value=b):
                return block_statement_to_expr(b)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.serialization.space_sep(hydra.lib.lists.concat((async_kw(), (hydra.serialization.ifx(hydra.java_script.operators.arrow_op(), params_expr(), body_expr()),))))

def assignment_expression_to_expr(assign: hydra.java_script.syntax.AssignmentExpression) -> hydra.ast.Expr:
    r"""Convert an assignment expression to an AST expression."""

    op = assign.operator
    left = assign.left
    right = assign.right
    @lru_cache(1)
    def op_str() -> str:
        return assignment_operator_to_string(op)
    return hydra.serialization.space_sep((pattern_to_expr(left), hydra.serialization.cst(op_str()), expression_to_expr(right)))

def assignment_pattern_to_expr(assign: hydra.java_script.syntax.AssignmentPattern) -> hydra.ast.Expr:
    r"""Convert an assignment pattern to an AST expression."""

    left = assign.left
    right = assign.right
    return hydra.serialization.ifx(hydra.java_script.operators.define_op(), pattern_to_expr(left), expression_to_expr(right))

def binary_expression_to_expr(bin: hydra.java_script.syntax.BinaryExpression) -> hydra.ast.Expr:
    r"""Convert a binary expression to an AST expression."""

    op = bin.operator
    left = bin.left
    right = bin.right
    return hydra.serialization.ifx(binary_operator_to_expr(op), expression_to_expr(left), expression_to_expr(right))

def block_statement_to_expr(block: frozenlist[hydra.java_script.syntax.Statement]) -> hydra.ast.Expr:
    r"""Convert a block statement to an AST expression."""

    return hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.full_block_style, hydra.lib.lists.map((lambda x1: statement_to_expr(x1)), block))

def call_expression_to_expr(call: hydra.java_script.syntax.CallExpression) -> hydra.ast.Expr:
    r"""Convert a call expression to an AST expression."""

    callee = call.callee
    args = call.arguments
    optional = call.optional
    @lru_cache(1)
    def callee_expr() -> hydra.ast.Expr:
        return expression_to_expr(callee)
    @lru_cache(1)
    def args_expr() -> hydra.ast.Expr:
        return hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: expression_to_expr(x1)), args))
    @lru_cache(1)
    def optional_dot() -> str:
        return hydra.lib.logic.if_else(optional, (lambda : "?."), (lambda : ""))
    return hydra.serialization.space_sep((callee_expr(), hydra.serialization.cst(optional_dot()), args_expr()))

def catch_clause_to_expr(c: hydra.java_script.syntax.CatchClause) -> hydra.ast.Expr:
    r"""Convert a catch clause to an AST expression."""

    param = c.param
    body = c.body
    @lru_cache(1)
    def catch_kw() -> hydra.ast.Expr:
        return hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("catch")), (lambda p: hydra.serialization.space_sep((hydra.serialization.cst("catch"), hydra.serialization.parenthesize(pattern_to_expr(p))))), param)
    return hydra.serialization.space_sep((catch_kw(), block_statement_to_expr(body)))

def class_declaration_to_expr(cls: hydra.java_script.syntax.ClassDeclaration) -> hydra.ast.Expr:
    r"""Convert a class declaration to an AST expression."""

    id = cls.id
    super_class = cls.super_class
    body = cls.body
    @lru_cache(1)
    def extends_clause() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda s: (hydra.serialization.cst("extends"), expression_to_expr(s))), super_class)
    @lru_cache(1)
    def body_expr() -> hydra.ast.Expr:
        return hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.full_block_style, hydra.lib.lists.map((lambda x1: method_definition_to_expr(x1)), body))
    return hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("class"), identifier_to_expr(id)), extends_clause(), (body_expr(),))))

def conditional_expression_to_expr(cond: hydra.java_script.syntax.ConditionalExpression) -> hydra.ast.Expr:
    r"""Convert a conditional expression to an AST expression."""

    test = cond.test
    consequent = cond.consequent
    alternate = cond.alternate
    return hydra.serialization.space_sep((expression_to_expr(test), hydra.serialization.cst("?"), expression_to_expr(consequent), hydra.serialization.cst(":"), expression_to_expr(alternate)))

def do_while_statement_to_expr(d: hydra.java_script.syntax.DoWhileStatement) -> hydra.ast.Expr:
    r"""Convert a do-while statement to an AST expression."""

    body = d.body
    test = d.test
    return hydra.serialization.suffix(";", hydra.serialization.space_sep((hydra.serialization.cst("do"), statement_to_expr(body), hydra.serialization.cst("while"), hydra.serialization.parenthesize(expression_to_expr(test)))))

def expression_to_expr(expr: hydra.java_script.syntax.Expression) -> hydra.ast.Expr:
    r"""Convert a JavaScript expression to an AST expression."""

    match expr:
        case hydra.java_script.syntax.ExpressionIdentifier(value=id):
            return identifier_to_expr(id)

        case hydra.java_script.syntax.ExpressionLiteral(value=lit):
            return literal_to_expr(lit)

        case hydra.java_script.syntax.ExpressionArray(value=arr):
            return array_expression_to_expr(arr)

        case hydra.java_script.syntax.ExpressionObject(value=obj):
            return object_expression_to_expr(obj)

        case hydra.java_script.syntax.ExpressionFunction(value=fn):
            return function_expression_to_expr(fn)

        case hydra.java_script.syntax.ExpressionArrow(value=arrow):
            return arrow_function_expression_to_expr(arrow)

        case hydra.java_script.syntax.ExpressionCall(value=call):
            return call_expression_to_expr(call)

        case hydra.java_script.syntax.ExpressionMember(value=mem):
            return member_expression_to_expr(mem)

        case hydra.java_script.syntax.ExpressionConditional(value=cond):
            return conditional_expression_to_expr(cond)

        case hydra.java_script.syntax.ExpressionBinary(value=bin):
            return binary_expression_to_expr(bin)

        case hydra.java_script.syntax.ExpressionUnary(value=un):
            return unary_expression_to_expr(un)

        case hydra.java_script.syntax.ExpressionAssignment(value=assign):
            return assignment_expression_to_expr(assign)

        case hydra.java_script.syntax.ExpressionSequence(value=exprs):
            return hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: expression_to_expr(x1)), exprs))

        case hydra.java_script.syntax.ExpressionThis():
            return hydra.serialization.cst("this")

        case hydra.java_script.syntax.ExpressionNew(value=call2):
            return hydra.serialization.space_sep((hydra.serialization.cst("new"), call_expression_to_expr(call2)))

        case hydra.java_script.syntax.ExpressionYield(value=maybe_expr):
            return hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("yield")), (lambda e: hydra.serialization.space_sep((hydra.serialization.cst("yield"), expression_to_expr(e)))), maybe_expr)

        case hydra.java_script.syntax.ExpressionAwait(value=e):
            return hydra.serialization.space_sep((hydra.serialization.cst("await"), expression_to_expr(e)))

        case hydra.java_script.syntax.ExpressionSpread(value=spread):
            return hydra.serialization.prefix("...", expression_to_expr(spread.value))

        case hydra.java_script.syntax.ExpressionParenthesized(value=e2):
            return hydra.serialization.parenthesize(expression_to_expr(e2))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def for_in_statement_to_expr(f: hydra.java_script.syntax.ForInStatement) -> hydra.ast.Expr:
    r"""Convert a for-in statement to an AST expression."""

    left = f.left
    right = f.right
    body = f.body
    @lru_cache(1)
    def left_expr() -> hydra.ast.Expr:
        match left:
            case hydra.java_script.syntax.ForInLeftVariable(value=v):
                return variable_declaration_to_expr(v)

            case hydra.java_script.syntax.ForInLeftPattern(value=p):
                return pattern_to_expr(p)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.serialization.space_sep((hydra.serialization.cst("for"), hydra.serialization.parenthesize(hydra.serialization.space_sep((left_expr(), hydra.serialization.cst("in"), expression_to_expr(right)))), statement_to_expr(body)))

def for_of_statement_to_expr(f: hydra.java_script.syntax.ForOfStatement) -> hydra.ast.Expr:
    r"""Convert a for-of statement to an AST expression."""

    await_ = f.await_
    left = f.left
    right = f.right
    body = f.body
    @lru_cache(1)
    def for_kw() -> hydra.ast.Expr:
        return hydra.lib.logic.if_else(await_, (lambda : hydra.serialization.cst("for await")), (lambda : hydra.serialization.cst("for")))
    @lru_cache(1)
    def left_expr() -> hydra.ast.Expr:
        match left:
            case hydra.java_script.syntax.ForInLeftVariable(value=v):
                return variable_declaration_to_expr(v)

            case hydra.java_script.syntax.ForInLeftPattern(value=p):
                return pattern_to_expr(p)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.serialization.space_sep((for_kw(), hydra.serialization.parenthesize(hydra.serialization.space_sep((left_expr(), hydra.serialization.cst("of"), expression_to_expr(right)))), statement_to_expr(body)))

def for_statement_to_expr(f: hydra.java_script.syntax.ForStatement) -> hydra.ast.Expr:
    r"""Convert a for statement to an AST expression."""

    init = f.init
    test = f.test
    update = f.update
    body = f.body
    @lru_cache(1)
    def init_expr():
        def _hoist_init_expr_1(v1):
            match v1:
                case hydra.java_script.syntax.ForInitVariable(value=v):
                    return variable_declaration_to_expr(v)

                case hydra.java_script.syntax.ForInitExpression(value=e):
                    return expression_to_expr(e)

                case _:
                    raise AssertionError("Unreachable: all variants handled")
        return hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("")), (lambda i: _hoist_init_expr_1(i)), init)
    @lru_cache(1)
    def test_expr() -> hydra.ast.Expr:
        return hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("")), (lambda x1: expression_to_expr(x1)), test)
    @lru_cache(1)
    def update_expr() -> hydra.ast.Expr:
        return hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("")), (lambda x1: expression_to_expr(x1)), update)
    return hydra.serialization.space_sep((hydra.serialization.cst("for"), hydra.serialization.paren_list(False, (init_expr(), test_expr(), update_expr())), statement_to_expr(body)))

def function_declaration_to_expr(fn: hydra.java_script.syntax.FunctionDeclaration) -> hydra.ast.Expr:
    r"""Convert a function declaration to an AST expression."""

    id = fn.id
    params = fn.params
    body = fn.body
    async_ = fn.async_
    generator = fn.generator
    @lru_cache(1)
    def async_kw() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(async_, (lambda : (hydra.serialization.cst("async"),)), (lambda : ()))
    @lru_cache(1)
    def func_kw() -> hydra.ast.Expr:
        return hydra.lib.logic.if_else(generator, (lambda : hydra.serialization.cst("function*")), (lambda : hydra.serialization.cst("function")))
    @lru_cache(1)
    def params_expr() -> hydra.ast.Expr:
        return hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: pattern_to_expr(x1)), params))
    return hydra.serialization.space_sep(hydra.lib.lists.concat((async_kw(), (func_kw(), identifier_to_expr(id), params_expr(), block_statement_to_expr(body)))))

def function_expression_to_expr(fn: hydra.java_script.syntax.FunctionExpression) -> hydra.ast.Expr:
    r"""Convert a function expression to an AST expression."""

    mid = fn.id
    params = fn.params
    body = fn.body
    async_ = fn.async_
    generator = fn.generator
    @lru_cache(1)
    def async_kw() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(async_, (lambda : (hydra.serialization.cst("async"),)), (lambda : ()))
    @lru_cache(1)
    def func_kw() -> hydra.ast.Expr:
        return hydra.lib.logic.if_else(generator, (lambda : hydra.serialization.cst("function*")), (lambda : hydra.serialization.cst("function")))
    @lru_cache(1)
    def name_expr() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda id: (identifier_to_expr(id),)), mid)
    @lru_cache(1)
    def params_expr() -> hydra.ast.Expr:
        return hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: pattern_to_expr(x1)), params))
    return hydra.serialization.space_sep(hydra.lib.lists.concat((async_kw(), (func_kw(),), name_expr(), (params_expr(), block_statement_to_expr(body)))))

def if_statement_to_expr(if_stmt: hydra.java_script.syntax.IfStatement) -> hydra.ast.Expr:
    r"""Convert an if statement to an AST expression."""

    test = if_stmt.test
    consequent = if_stmt.consequent
    alternate = if_stmt.alternate
    @lru_cache(1)
    def if_part() -> hydra.ast.Expr:
        return hydra.serialization.space_sep((hydra.serialization.cst("if"), hydra.serialization.parenthesize(expression_to_expr(test)), statement_to_expr(consequent)))
    return hydra.lib.maybes.maybe((lambda : if_part()), (lambda alt: hydra.serialization.space_sep((if_part(), hydra.serialization.cst("else"), statement_to_expr(alt)))), alternate)

def labeled_statement_to_expr(l: hydra.java_script.syntax.LabeledStatement) -> hydra.ast.Expr:
    r"""Convert a labeled statement to an AST expression."""

    label = l.label
    body = l.body
    return hydra.serialization.space_sep((hydra.serialization.suffix(":", identifier_to_expr(label)), statement_to_expr(body)))

def member_expression_to_expr(mem: hydra.java_script.syntax.MemberExpression) -> hydra.ast.Expr:
    r"""Convert a member expression to an AST expression."""

    obj = mem.object
    prop = mem.property
    computed = mem.computed
    optional = mem.optional
    @lru_cache(1)
    def obj_expr() -> hydra.ast.Expr:
        return expression_to_expr(obj)
    @lru_cache(1)
    def prop_expr() -> hydra.ast.Expr:
        return expression_to_expr(prop)
    return hydra.lib.logic.if_else(computed, (lambda : hydra.serialization.space_sep((obj_expr(), hydra.lib.logic.if_else(optional, (lambda : hydra.serialization.cst("?.")), (lambda : hydra.serialization.cst(""))), hydra.serialization.brackets(hydra.serialization.square_brackets, hydra.serialization.inline_style, prop_expr())))), (lambda : hydra.serialization.ifx(hydra.lib.logic.if_else(optional, (lambda : hydra.java_script.operators.optional_chain_op), (lambda : hydra.java_script.operators.member_op)), obj_expr(), prop_expr())))

def method_definition_to_expr(method: hydra.java_script.syntax.MethodDefinition) -> hydra.ast.Expr:
    r"""Convert a method definition to an AST expression."""

    key = method.key
    value = method.value
    kind = method.kind
    computed = method.computed
    static = method.static
    @lru_cache(1)
    def static_kw() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(static, (lambda : (hydra.serialization.cst("static"),)), (lambda : ()))
    @lru_cache(1)
    def kind_kw() -> frozenlist[hydra.ast.Expr]:
        match kind:
            case hydra.java_script.syntax.MethodKind.CONSTRUCTOR:
                return ()

            case hydra.java_script.syntax.MethodKind.METHOD:
                return ()

            case hydra.java_script.syntax.MethodKind.GET:
                return (hydra.serialization.cst("get"),)

            case hydra.java_script.syntax.MethodKind.SET:
                return (hydra.serialization.cst("set"),)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    @lru_cache(1)
    def key_expr() -> hydra.ast.Expr:
        return hydra.lib.logic.if_else(computed, (lambda : hydra.serialization.brackets(hydra.serialization.square_brackets, hydra.serialization.inline_style, expression_to_expr(key))), (lambda : expression_to_expr(key)))
    params = value.params
    body = value.body
    @lru_cache(1)
    def params_expr() -> hydra.ast.Expr:
        return hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: pattern_to_expr(x1)), params))
    return hydra.serialization.space_sep(hydra.lib.lists.concat((static_kw(), kind_kw(), (key_expr(), params_expr(), block_statement_to_expr(body)))))

def object_expression_to_expr(obj: frozenlist[hydra.java_script.syntax.Property]) -> hydra.ast.Expr:
    r"""Convert an object expression to an AST expression."""

    return hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.half_block_style, hydra.lib.lists.map((lambda x1: property_to_expr(x1)), obj))

def object_pattern_property_to_expr(prop: hydra.java_script.syntax.ObjectPatternProperty) -> hydra.ast.Expr:
    r"""Convert an object pattern property to an AST expression."""

    match prop:
        case hydra.java_script.syntax.ObjectPatternPropertyProperty(value=p):
            return property_to_expr(p)

        case hydra.java_script.syntax.ObjectPatternPropertyRest(value=r):
            return hydra.serialization.prefix("...", pattern_to_expr(r.value))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def object_pattern_to_expr(obj: hydra.java_script.syntax.ObjectPattern) -> hydra.ast.Expr:
    r"""Convert an object pattern to an AST expression."""

    props = obj.properties
    return hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: object_pattern_property_to_expr(x1)), props))

def pattern_to_expr(pat: hydra.java_script.syntax.Pattern) -> hydra.ast.Expr:
    r"""Convert a pattern to an AST expression."""

    match pat:
        case hydra.java_script.syntax.PatternIdentifier(value=id):
            return identifier_to_expr(id)

        case hydra.java_script.syntax.PatternObject(value=obj):
            return object_pattern_to_expr(obj)

        case hydra.java_script.syntax.PatternArray(value=arr):
            return array_pattern_to_expr(arr)

        case hydra.java_script.syntax.PatternAssignment(value=assign):
            return assignment_pattern_to_expr(assign)

        case hydra.java_script.syntax.PatternRest(value=rest):
            return hydra.serialization.prefix("...", pattern_to_expr(rest.value))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def property_to_expr(prop: hydra.java_script.syntax.Property) -> hydra.ast.Expr:
    r"""Convert an object property to an AST expression."""

    key = prop.key
    value = prop.value
    shorthand = prop.shorthand
    computed = prop.computed
    @lru_cache(1)
    def key_expr() -> hydra.ast.Expr:
        return hydra.lib.logic.if_else(computed, (lambda : hydra.serialization.brackets(hydra.serialization.square_brackets, hydra.serialization.inline_style, expression_to_expr(key))), (lambda : expression_to_expr(key)))
    return hydra.lib.logic.if_else(shorthand, (lambda : key_expr()), (lambda : hydra.serialization.ifx(hydra.java_script.operators.colon_op(), key_expr(), expression_to_expr(value))))

def return_statement_to_expr(r: Maybe[hydra.java_script.syntax.Expression]) -> hydra.ast.Expr:
    r"""Convert a return statement to an AST expression."""

    return hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("return;")), (lambda e: hydra.serialization.suffix(";", hydra.serialization.space_sep((hydra.serialization.cst("return"), expression_to_expr(e))))), r)

def statement_to_expr(stmt: hydra.java_script.syntax.Statement) -> hydra.ast.Expr:
    r"""Convert a statement to an AST expression."""

    match stmt:
        case hydra.java_script.syntax.StatementExpression(value=e):
            return hydra.serialization.suffix(";", expression_to_expr(e))

        case hydra.java_script.syntax.StatementBlock(value=b):
            return block_statement_to_expr(b)

        case hydra.java_script.syntax.StatementEmpty():
            return hydra.serialization.cst(";")

        case hydra.java_script.syntax.StatementDebugger():
            return hydra.serialization.cst("debugger;")

        case hydra.java_script.syntax.StatementReturn(value=r):
            return return_statement_to_expr(r)

        case hydra.java_script.syntax.StatementBreak(value=b2):
            return break_statement_to_expr(b2)

        case hydra.java_script.syntax.StatementContinue(value=c):
            return continue_statement_to_expr(c)

        case hydra.java_script.syntax.StatementIf(value=i):
            return if_statement_to_expr(i)

        case hydra.java_script.syntax.StatementSwitch(value=s):
            return switch_statement_to_expr(s)

        case hydra.java_script.syntax.StatementThrow(value=t):
            return throw_statement_to_expr(t)

        case hydra.java_script.syntax.StatementTry(value=t2):
            return try_statement_to_expr(t2)

        case hydra.java_script.syntax.StatementWhile(value=w):
            return while_statement_to_expr(w)

        case hydra.java_script.syntax.StatementDoWhile(value=d):
            return do_while_statement_to_expr(d)

        case hydra.java_script.syntax.StatementFor(value=f):
            return for_statement_to_expr(f)

        case hydra.java_script.syntax.StatementForIn(value=f2):
            return for_in_statement_to_expr(f2)

        case hydra.java_script.syntax.StatementForOf(value=f3):
            return for_of_statement_to_expr(f3)

        case hydra.java_script.syntax.StatementVariableDeclaration(value=v):
            return variable_declaration_to_expr(v)

        case hydra.java_script.syntax.StatementFunctionDeclaration(value=f4):
            return function_declaration_to_expr(f4)

        case hydra.java_script.syntax.StatementClassDeclaration(value=c2):
            return class_declaration_to_expr(c2)

        case hydra.java_script.syntax.StatementLabeled(value=l):
            return labeled_statement_to_expr(l)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def switch_case_to_expr(c: hydra.java_script.syntax.SwitchCase) -> hydra.ast.Expr:
    r"""Convert a switch case to an AST expression."""

    test = c.test
    consequent = c.consequent
    @lru_cache(1)
    def case_label() -> hydra.ast.Expr:
        return hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("default:")), (lambda t: hydra.serialization.space_sep((hydra.serialization.cst("case"), expression_to_expr(t), hydra.serialization.cst(":")))), test)
    return hydra.serialization.newline_sep(hydra.lib.lists.cons(case_label(), hydra.lib.lists.map((lambda x1: statement_to_expr(x1)), consequent)))

def switch_statement_to_expr(switch_stmt: hydra.java_script.syntax.SwitchStatement) -> hydra.ast.Expr:
    r"""Convert a switch statement to an AST expression."""

    discriminant = switch_stmt.discriminant
    cases = switch_stmt.cases
    return hydra.serialization.space_sep((hydra.serialization.cst("switch"), hydra.serialization.parenthesize(expression_to_expr(discriminant)), hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.full_block_style, hydra.lib.lists.map((lambda x1: switch_case_to_expr(x1)), cases))))

def throw_statement_to_expr(t: hydra.java_script.syntax.ThrowStatement) -> hydra.ast.Expr:
    r"""Convert a throw statement to an AST expression."""

    return hydra.serialization.suffix(";", hydra.serialization.space_sep((hydra.serialization.cst("throw"), expression_to_expr(t.value))))

def try_statement_to_expr(t: hydra.java_script.syntax.TryStatement) -> hydra.ast.Expr:
    r"""Convert a try statement to an AST expression."""

    block = t.block
    handler = t.handler
    finalizer = t.finalizer
    @lru_cache(1)
    def try_part() -> hydra.ast.Expr:
        return hydra.serialization.space_sep((hydra.serialization.cst("try"), block_statement_to_expr(block)))
    @lru_cache(1)
    def catch_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda c: (catch_clause_to_expr(c),)), handler)
    @lru_cache(1)
    def finally_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda f: (hydra.serialization.space_sep((hydra.serialization.cst("finally"), block_statement_to_expr(f))),)), finalizer)
    return hydra.serialization.space_sep(hydra.lib.lists.concat(((try_part(),), catch_part(), finally_part())))

def unary_expression_to_expr(un: hydra.java_script.syntax.UnaryExpression) -> hydra.ast.Expr:
    r"""Convert a unary expression to an AST expression."""

    op = un.operator
    arg = un.argument
    prefix = un.prefix
    @lru_cache(1)
    def op_str() -> str:
        return unary_operator_to_string(op)
    @lru_cache(1)
    def arg_expr() -> hydra.ast.Expr:
        return expression_to_expr(arg)
    return hydra.lib.logic.if_else(prefix, (lambda : hydra.serialization.prefix(op_str(), arg_expr())), (lambda : hydra.serialization.suffix(op_str(), arg_expr())))

def variable_declaration_to_expr(decl: hydra.java_script.syntax.VariableDeclaration) -> hydra.ast.Expr:
    r"""Convert a variable declaration to an AST expression."""

    kind = decl.kind
    declarations = decl.declarations
    return hydra.serialization.suffix(";", hydra.serialization.space_sep((variable_kind_to_expr(kind), hydra.serialization.comma_sep(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: variable_declarator_to_expr(x1)), declarations)))))

def variable_declarator_to_expr(decl: hydra.java_script.syntax.VariableDeclarator) -> hydra.ast.Expr:
    r"""Convert a variable declarator to an AST expression."""

    id = decl.id
    init = decl.init
    return hydra.lib.maybes.maybe((lambda : pattern_to_expr(id)), (lambda e: hydra.serialization.ifx(hydra.java_script.operators.define_op(), pattern_to_expr(id), expression_to_expr(e))), init)

def while_statement_to_expr(w: hydra.java_script.syntax.WhileStatement) -> hydra.ast.Expr:
    r"""Convert a while statement to an AST expression."""

    test = w.test
    body = w.body
    return hydra.serialization.space_sep((hydra.serialization.cst("while"), hydra.serialization.parenthesize(expression_to_expr(test)), statement_to_expr(body)))

def type_expression_to_string(typ: hydra.java_script.syntax.TypeExpression) -> str:
    r"""Convert a type expression to a string for JSDoc."""

    match typ:
        case hydra.java_script.syntax.TypeExpressionIdentifier(value=id):
            return id.value

        case hydra.java_script.syntax.TypeExpressionAny():
            return "*"

        case hydra.java_script.syntax.TypeExpressionVoid():
            return "void"

        case hydra.java_script.syntax.TypeExpressionNever():
            return "never"

        case hydra.java_script.syntax.TypeExpressionLiteral():
            return "literal"

        case hydra.java_script.syntax.TypeExpressionArray(value=a):
            return hydra.lib.strings.cat2(type_expression_to_string(a.value), "[]")

        case hydra.java_script.syntax.TypeExpressionFunction():
            return "Function"

        case hydra.java_script.syntax.TypeExpressionObject():
            return "Object"

        case hydra.java_script.syntax.TypeExpressionUnion(value=u):
            return hydra.lib.strings.intercalate("|", hydra.lib.lists.map((lambda x1: type_expression_to_string(x1)), u))

        case hydra.java_script.syntax.TypeExpressionParameterized(value=p):
            base = p.base
            args = p.arguments
            return hydra.lib.strings.cat((type_expression_to_string(base), "<", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map((lambda x1: type_expression_to_string(x1)), args)), ">"))

        case hydra.java_script.syntax.TypeExpressionOptional(value=o):
            return hydra.lib.strings.cat2("?", type_expression_to_string(o))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def documentation_tag_to_line(tag: hydra.java_script.syntax.DocumentationTag) -> str:
    r"""Convert a documentation tag to a JSDoc line."""

    name = tag.name
    mtype = tag.type
    mparam_name = tag.param_name
    description = tag.description
    @lru_cache(1)
    def type_part() -> str:
        return hydra.lib.maybes.maybe((lambda : ""), (lambda t: hydra.lib.strings.cat(("{", type_expression_to_string(t), "} "))), mtype)
    @lru_cache(1)
    def param_part() -> str:
        return hydra.lib.maybes.maybe((lambda : ""), (lambda p: hydra.lib.strings.cat2(p.value, " ")), mparam_name)
    return hydra.lib.strings.cat((" * @", name, " ", type_part(), param_part(), description))

def to_java_script_comments(desc: str, tags: frozenlist[hydra.java_script.syntax.DocumentationTag]) -> str:
    r"""Format a description and tags as a JSDoc comment."""

    @lru_cache(1)
    def desc_lines() -> frozenlist[str]:
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(desc, ""), (lambda : ()), (lambda : hydra.lib.lists.map((lambda line: hydra.lib.strings.cat2(" * ", line)), hydra.lib.strings.lines(desc))))
    @lru_cache(1)
    def tag_lines() -> frozenlist[str]:
        return hydra.lib.lists.map((lambda x1: documentation_tag_to_line(x1)), tags)
    @lru_cache(1)
    def all_lines() -> frozenlist[str]:
        return hydra.lib.lists.concat((desc_lines(), tag_lines()))
    return hydra.lib.logic.if_else(hydra.lib.lists.null(all_lines()), (lambda : ""), (lambda : hydra.lib.strings.intercalate("\n", hydra.lib.lists.concat((("/**",), all_lines(), (" */",))))))

def documentation_comment_to_expr(doc: hydra.java_script.syntax.DocumentationComment) -> hydra.ast.Expr:
    r"""Convert a documentation comment to an AST expression."""

    description = doc.description
    tags = doc.tags
    return hydra.serialization.cst(to_java_script_comments(description, tags))

def class_declaration_with_comments_to_expr(cdwc: hydra.java_script.syntax.ClassDeclarationWithComments) -> hydra.ast.Expr:
    r"""Convert a class declaration with comments to an AST expression."""

    body = cdwc.body
    mc = cdwc.comments
    return hydra.lib.maybes.maybe((lambda : class_declaration_to_expr(body)), (lambda c: hydra.serialization.newline_sep((documentation_comment_to_expr(c), class_declaration_to_expr(body)))), mc)

def export_all_to_expr(a: hydra.java_script.syntax.ExportAllDeclaration) -> hydra.ast.Expr:
    r"""Convert an export all declaration to an AST expression."""

    exported = a.exported
    source = a.source
    @lru_cache(1)
    def exported_clause() -> hydra.ast.Expr:
        return hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("*")), (lambda e: hydra.serialization.space_sep((hydra.serialization.cst("*"), hydra.serialization.cst("as"), identifier_to_expr(e)))), exported)
    return hydra.serialization.suffix(";", hydra.serialization.space_sep((hydra.serialization.cst("export"), exported_clause(), hydra.serialization.cst("from"), string_literal_to_expr(source))))

def export_specifier_to_expr(spec: hydra.java_script.syntax.ExportSpecifier) -> hydra.ast.Expr:
    r"""Convert an export specifier to an AST expression."""

    local = spec.local
    exported = spec.exported
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(local.value, exported.value), (lambda : identifier_to_expr(local)), (lambda : hydra.serialization.space_sep((identifier_to_expr(local), hydra.serialization.cst("as"), identifier_to_expr(exported)))))

def named_export_to_expr(n: hydra.java_script.syntax.NamedExport) -> hydra.ast.Expr:
    r"""Convert a named export to an AST expression."""

    specifiers = n.specifiers
    source = n.source
    @lru_cache(1)
    def spec_exprs() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda x1: export_specifier_to_expr(x1)), specifiers)
    @lru_cache(1)
    def from_clause() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda s: (hydra.serialization.cst("from"), string_literal_to_expr(s))), source)
    return hydra.serialization.suffix(";", hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("export"),), (hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.inline_style, spec_exprs()),), from_clause()))))

def export_declaration_to_expr(exp: hydra.java_script.syntax.ExportDeclaration) -> hydra.ast.Expr:
    r"""Convert an export declaration to an AST expression."""

    match exp:
        case hydra.java_script.syntax.ExportDeclarationNamed(value=n):
            return named_export_to_expr(n)

        case hydra.java_script.syntax.ExportDeclarationDefault(value=e):
            return hydra.serialization.suffix(";", hydra.serialization.space_sep((hydra.serialization.cst("export"), hydra.serialization.cst("default"), expression_to_expr(e))))

        case hydra.java_script.syntax.ExportDeclarationDeclaration(value=d):
            return hydra.serialization.space_sep((hydra.serialization.cst("export"), statement_to_expr(d)))

        case hydra.java_script.syntax.ExportDeclarationAll(value=a):
            return export_all_to_expr(a)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def format_import_specifiers(specs: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    r"""Format import specifiers, handling default vs named imports."""

    return hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.inline_style, specs)

def function_declaration_with_comments_to_expr(fdwc: hydra.java_script.syntax.FunctionDeclarationWithComments) -> hydra.ast.Expr:
    r"""Convert a function declaration with comments to an AST expression."""

    body = fdwc.body
    mc = fdwc.comments
    return hydra.lib.maybes.maybe((lambda : function_declaration_to_expr(body)), (lambda c: hydra.serialization.newline_sep((documentation_comment_to_expr(c), function_declaration_to_expr(body)))), mc)

def import_specifier_to_expr(spec: hydra.java_script.syntax.ImportClause) -> hydra.ast.Expr:
    r"""Convert an import specifier to an AST expression."""

    match spec:
        case hydra.java_script.syntax.ImportClauseNamed(value=n):
            imported = n.imported
            local = n.local
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(imported.value, local.value), (lambda : identifier_to_expr(local)), (lambda : hydra.serialization.space_sep((identifier_to_expr(imported), hydra.serialization.cst("as"), identifier_to_expr(local)))))

        case hydra.java_script.syntax.ImportClauseDefault(value=d):
            return identifier_to_expr(d.value)

        case hydra.java_script.syntax.ImportClauseNamespace(value=n2):
            return hydra.serialization.space_sep((hydra.serialization.cst("*"), hydra.serialization.cst("as"), identifier_to_expr(n2.value)))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def import_declaration_to_expr(imp: hydra.java_script.syntax.ImportDeclaration) -> hydra.ast.Expr:
    r"""Convert an import declaration to an AST expression."""

    specifiers = imp.specifiers
    source = imp.source
    @lru_cache(1)
    def source_expr() -> hydra.ast.Expr:
        return string_literal_to_expr(source)
    @lru_cache(1)
    def spec_exprs() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda x1: import_specifier_to_expr(x1)), specifiers)
    return hydra.lib.logic.if_else(hydra.lib.lists.null(specifiers), (lambda : hydra.serialization.suffix(";", hydra.serialization.space_sep((hydra.serialization.cst("import"), source_expr())))), (lambda : hydra.serialization.suffix(";", hydra.serialization.space_sep((hydra.serialization.cst("import"), format_import_specifiers(spec_exprs()), hydra.serialization.cst("from"), source_expr())))))

def module_item_to_expr(item: hydra.java_script.syntax.ModuleItem) -> hydra.ast.Expr:
    r"""Convert a module item to an AST expression."""

    match item:
        case hydra.java_script.syntax.ModuleItemStatement(value=s):
            return statement_to_expr(s)

        case hydra.java_script.syntax.ModuleItemImport(value=i):
            return import_declaration_to_expr(i)

        case hydra.java_script.syntax.ModuleItemExport(value=e):
            return export_declaration_to_expr(e)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def module_item_with_comments_to_expr(miwc: hydra.java_script.syntax.ModuleItemWithComments) -> hydra.ast.Expr:
    r"""Convert a module item with comments to an AST expression."""

    body = miwc.body
    mc = miwc.comments
    return hydra.lib.maybes.maybe((lambda : module_item_to_expr(body)), (lambda c: hydra.serialization.newline_sep((documentation_comment_to_expr(c), module_item_to_expr(body)))), mc)

def to_line_comment(s: str) -> str:
    r"""Convert a string to a JavaScript line comment."""

    return hydra.lib.strings.intercalate("\n", hydra.lib.lists.map((lambda line: hydra.lib.strings.cat2("// ", line)), hydra.lib.strings.lines(s)))

def program_to_expr(prog: hydra.java_script.syntax.Program) -> hydra.ast.Expr:
    r"""Convert a JavaScript program to an AST expression."""

    body = prog.body
    @lru_cache(1)
    def warning() -> frozenlist[hydra.ast.Expr]:
        return (hydra.serialization.cst(to_line_comment(hydra.constants.warning_auto_generated_file)),)
    @lru_cache(1)
    def items() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda x1: module_item_to_expr(x1)), body)
    return hydra.serialization.double_newline_sep(hydra.lib.lists.concat((warning(), items())))
