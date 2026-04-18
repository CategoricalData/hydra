# Note: this is an automatically generated file. Do not edit.

r"""Serialization functions for converting C++ AST to abstract expressions."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.cpp.syntax
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maybes
import hydra.lib.strings
import hydra.serialization

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def encode_access_specifier(a: hydra.cpp.syntax.AccessSpecifier) -> hydra.ast.Expr:
    r"""Convert an access specifier to an expression."""

    match a:
        case hydra.cpp.syntax.AccessSpecifier.PUBLIC:
            return hydra.serialization.cst("public")

        case hydra.cpp.syntax.AccessSpecifier.PROTECTED:
            return hydra.serialization.cst("protected")

        case hydra.cpp.syntax.AccessSpecifier.PRIVATE:
            return hydra.serialization.cst("private")

        case hydra.cpp.syntax.AccessSpecifier.NONE:
            return hydra.serialization.cst("")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_assignment_operator(op: hydra.cpp.syntax.AssignmentOperator) -> hydra.ast.Expr:
    r"""Convert an assignment operator to an expression."""

    match op:
        case hydra.cpp.syntax.AssignmentOperator.ASSIGN:
            return hydra.serialization.cst("=")

        case hydra.cpp.syntax.AssignmentOperator.PLUS_ASSIGN:
            return hydra.serialization.cst("+=")

        case hydra.cpp.syntax.AssignmentOperator.MINUS_ASSIGN:
            return hydra.serialization.cst("-=")

        case hydra.cpp.syntax.AssignmentOperator.MULTIPLY_ASSIGN:
            return hydra.serialization.cst("*=")

        case hydra.cpp.syntax.AssignmentOperator.DIVIDE_ASSIGN:
            return hydra.serialization.cst("/=")

        case hydra.cpp.syntax.AssignmentOperator.MODULO_ASSIGN:
            return hydra.serialization.cst("%=")

        case hydra.cpp.syntax.AssignmentOperator.LEFT_SHIFT_ASSIGN:
            return hydra.serialization.cst("<<=")

        case hydra.cpp.syntax.AssignmentOperator.RIGHT_SHIFT_ASSIGN:
            return hydra.serialization.cst(">>=")

        case hydra.cpp.syntax.AssignmentOperator.BITWISE_AND_ASSIGN:
            return hydra.serialization.cst("&=")

        case hydra.cpp.syntax.AssignmentOperator.BITWISE_XOR_ASSIGN:
            return hydra.serialization.cst("^=")

        case hydra.cpp.syntax.AssignmentOperator.BITWISE_OR_ASSIGN:
            return hydra.serialization.cst("|=")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_capture(cap: hydra.cpp.syntax.Capture) -> hydra.ast.Expr:
    r"""Convert a capture to an expression."""

    name = cap.name
    by_ref = cap.by_reference
    return hydra.lib.logic.if_else(by_ref, (lambda : hydra.serialization.cst(hydra.lib.strings.cat2("&", name))), (lambda : hydra.serialization.cst(name)))

def encode_capture_list(cl: hydra.cpp.syntax.CaptureList) -> hydra.ast.Expr:
    r"""Convert a capture list to an expression."""

    match cl:
        case hydra.cpp.syntax.CaptureListCaptureByValue():
            return hydra.serialization.cst("[=]")

        case hydra.cpp.syntax.CaptureListCaptures(value=cs):
            return hydra.serialization.bracket_list(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: encode_capture(x1)), cs))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_basic_type(t: hydra.cpp.syntax.BasicType) -> hydra.ast.Expr:
    r"""Convert a basic type to an expression."""

    match t:
        case hydra.cpp.syntax.BasicTypeVoid():
            return hydra.serialization.cst("void")

        case hydra.cpp.syntax.BasicTypeBool():
            return hydra.serialization.cst("bool")

        case hydra.cpp.syntax.BasicTypeChar():
            return hydra.serialization.cst("char")

        case hydra.cpp.syntax.BasicTypeInt():
            return hydra.serialization.cst("int")

        case hydra.cpp.syntax.BasicTypeFloat():
            return hydra.serialization.cst("float")

        case hydra.cpp.syntax.BasicTypeDouble():
            return hydra.serialization.cst("double")

        case hydra.cpp.syntax.BasicTypeString():
            return hydra.serialization.cst("std::string")

        case hydra.cpp.syntax.BasicTypeAuto():
            return hydra.serialization.cst("auto")

        case hydra.cpp.syntax.BasicTypeNamed(value=name):
            return hydra.serialization.cst(name)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_boolean_literal(bl: hydra.cpp.syntax.BooleanLiteral) -> hydra.ast.Expr:
    r"""Convert a boolean literal to an expression."""

    return hydra.lib.logic.if_else(bl.value, (lambda : hydra.serialization.cst("true")), (lambda : hydra.serialization.cst("false")))

def encode_integer_literal(i: hydra.cpp.syntax.IntegerLiteral) -> hydra.ast.Expr:
    r"""Convert an integer literal to an expression."""

    match i:
        case hydra.cpp.syntax.IntegerLiteralDecimal(value=n):
            return hydra.serialization.cst(hydra.lib.literals.show_bigint(n))

        case hydra.cpp.syntax.IntegerLiteralHexadecimal(value=h):
            return hydra.serialization.cst(hydra.lib.strings.cat2("0x", h))

        case hydra.cpp.syntax.IntegerLiteralOctal(value=o):
            return hydra.serialization.cst(hydra.lib.strings.cat2("0", o))

        case hydra.cpp.syntax.IntegerLiteralBinary(value=b):
            return hydra.serialization.cst(hydra.lib.strings.cat2("0b", b))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_literal(l: hydra.cpp.syntax.Literal) -> hydra.ast.Expr:
    r"""Convert a literal to an expression."""

    match l:
        case hydra.cpp.syntax.LiteralInteger(value=i):
            return encode_integer_literal(i)

        case hydra.cpp.syntax.LiteralFloating(value=f):
            return hydra.serialization.cst(hydra.lib.literals.show_bigfloat(f.value))

        case hydra.cpp.syntax.LiteralCharacter(value=c):
            return hydra.serialization.cst(hydra.lib.strings.cat(("'", c.value, "'")))

        case hydra.cpp.syntax.LiteralString(value=s):
            return hydra.serialization.cst(hydra.lib.strings.cat(("\"", s.value, "\"")))

        case hydra.cpp.syntax.LiteralBoolean(value=b):
            return encode_boolean_literal(b)

        case hydra.cpp.syntax.LiteralNull():
            return hydra.serialization.cst("nullptr")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_unary_operator(op: hydra.cpp.syntax.UnaryOperator) -> hydra.ast.Expr:
    r"""Convert a unary operator to an expression."""

    match op:
        case hydra.cpp.syntax.UnaryOperator.PLUS:
            return hydra.serialization.cst("+")

        case hydra.cpp.syntax.UnaryOperator.MINUS:
            return hydra.serialization.cst("-")

        case hydra.cpp.syntax.UnaryOperator.LOGICAL_NOT:
            return hydra.serialization.cst("!")

        case hydra.cpp.syntax.UnaryOperator.BITWISE_NOT:
            return hydra.serialization.cst("~")

        case hydra.cpp.syntax.UnaryOperator.DEREFERENCE:
            return hydra.serialization.cst("*")

        case hydra.cpp.syntax.UnaryOperator.ADDRESS_OF:
            return hydra.serialization.cst("&")

        case hydra.cpp.syntax.UnaryOperator.PRE_INCREMENT:
            return hydra.serialization.cst("++")

        case hydra.cpp.syntax.UnaryOperator.PRE_DECREMENT:
            return hydra.serialization.cst("--")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_add_operation(op: hydra.cpp.syntax.AddOperation) -> hydra.ast.Expr:
    r"""Convert an add operation to an expression."""

    left = op.left
    right = op.right
    return hydra.serialization.space_sep((encode_additive_expression(left), hydra.serialization.cst("+"), encode_multiplicative_expression(right)))

def encode_additive_expression(e: hydra.cpp.syntax.AdditiveExpression) -> hydra.ast.Expr:
    r"""Convert an additive expression to an expression."""

    match e:
        case hydra.cpp.syntax.AdditiveExpressionMultiplicative(value=m):
            return encode_multiplicative_expression(m)

        case hydra.cpp.syntax.AdditiveExpressionAdd(value=a):
            return encode_add_operation(a)

        case hydra.cpp.syntax.AdditiveExpressionSubtract(value=s):
            return encode_subtract_operation(s)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_and_expression(e: hydra.cpp.syntax.AndExpression) -> hydra.ast.Expr:
    r"""Convert an and expression to an expression."""

    match e:
        case hydra.cpp.syntax.AndExpressionEquality(value=eq):
            return encode_equality_expression(eq)

        case hydra.cpp.syntax.AndExpressionBitwiseAnd(value=a):
            return encode_bitwise_and_operation(a)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_assignment_expression(a: hydra.cpp.syntax.AssignmentExpression) -> hydra.ast.Expr:
    r"""Convert an assignment expression to an expression."""

    match a:
        case hydra.cpp.syntax.AssignmentExpressionConditional(value=c):
            return encode_conditional_expression(c)

        case hydra.cpp.syntax.AssignmentExpressionAssignment(value=e):
            return encode_explicit_assignment(e)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_bitwise_and_operation(op: hydra.cpp.syntax.BitwiseAndOperation) -> hydra.ast.Expr:
    r"""Convert a bitwise and operation to an expression."""

    left = op.left
    right = op.right
    return hydra.serialization.space_sep((encode_and_expression(left), hydra.serialization.cst("&"), encode_equality_expression(right)))

def encode_bitwise_or_operation(op: hydra.cpp.syntax.BitwiseOrOperation) -> hydra.ast.Expr:
    r"""Convert a bitwise or operation to an expression."""

    left = op.left
    right = op.right
    return hydra.serialization.space_sep((encode_inclusive_or_expression(left), hydra.serialization.cst("|"), encode_exclusive_or_expression(right)))

def encode_bitwise_xor_operation(op: hydra.cpp.syntax.BitwiseXorOperation) -> hydra.ast.Expr:
    r"""Convert a bitwise xor operation to an expression."""

    left = op.left
    right = op.right
    return hydra.serialization.space_sep((encode_exclusive_or_expression(left), hydra.serialization.cst("^"), encode_and_expression(right)))

def encode_case_statement(stmt: hydra.cpp.syntax.CaseStatement) -> hydra.ast.Expr:
    r"""Convert a case statement to an expression."""

    match stmt:
        case hydra.cpp.syntax.CaseStatementCase(value=cv):
            return encode_case_value(cv)

        case hydra.cpp.syntax.CaseStatementDefault(value=s):
            return hydra.serialization.space_sep((hydra.serialization.cst("default:"), encode_statement(s)))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_case_value(cv: hydra.cpp.syntax.CaseValue) -> hydra.ast.Expr:
    r"""Convert a case value to an expression."""

    value = cv.value
    statement = cv.statement
    return hydra.serialization.space_sep((hydra.serialization.cst("case"), hydra.serialization.no_sep((encode_expression(value), hydra.serialization.cst(":"))), encode_statement(statement)))

def encode_comma_expression(ce: hydra.cpp.syntax.CommaExpression) -> hydra.ast.Expr:
    r"""Convert a comma expression to an expression."""

    left = ce.left
    right = ce.right
    return hydra.serialization.space_sep((encode_expression(left), hydra.serialization.cst(","), encode_assignment_expression(right)))

def encode_compound_statement(cs: hydra.cpp.syntax.CompoundStatement) -> hydra.ast.Expr:
    r"""Convert a compound statement to an expression."""

    return hydra.serialization.curly_braces_list(Just(""), hydra.serialization.full_block_style, hydra.lib.lists.map((lambda x1: encode_statement(x1)), cs.value))

def encode_conditional_expression(c: hydra.cpp.syntax.ConditionalExpression) -> hydra.ast.Expr:
    r"""Convert a conditional expression to an expression."""

    match c:
        case hydra.cpp.syntax.ConditionalExpressionLogicalOr(value=l):
            return encode_logical_or_expression(l)

        case hydra.cpp.syntax.ConditionalExpressionTernary(value=t):
            return encode_ternary_expression(t)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_divide_operation(op: hydra.cpp.syntax.DivideOperation) -> hydra.ast.Expr:
    r"""Convert a divide operation to an expression."""

    left = op.left
    right = op.right
    return hydra.serialization.space_sep((encode_multiplicative_expression(left), hydra.serialization.cst("/"), encode_unary_expression(right)))

def encode_do_statement(ds: hydra.cpp.syntax.DoStatement) -> hydra.ast.Expr:
    r"""Convert a do statement to an expression."""

    body = ds.body
    cond = ds.condition
    return hydra.serialization.newline_sep((hydra.serialization.cst("do"), encode_statement(body), hydra.serialization.with_semi(hydra.serialization.space_sep((hydra.serialization.cst("while"), hydra.serialization.parens(encode_expression(cond)))))))

def encode_equal_operation(op: hydra.cpp.syntax.EqualOperation) -> hydra.ast.Expr:
    r"""Convert an equal operation to an expression."""

    left = op.left
    right = op.right
    return hydra.serialization.space_sep((encode_equality_expression(left), hydra.serialization.cst("=="), encode_relational_expression(right)))

def encode_equality_expression(e: hydra.cpp.syntax.EqualityExpression) -> hydra.ast.Expr:
    r"""Convert an equality expression to an expression."""

    match e:
        case hydra.cpp.syntax.EqualityExpressionRelational(value=r):
            return encode_relational_expression(r)

        case hydra.cpp.syntax.EqualityExpressionEqual(value=eq):
            return encode_equal_operation(eq)

        case hydra.cpp.syntax.EqualityExpressionNotEqual(value=ne):
            return encode_not_equal_operation(ne)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_exclusive_or_expression(e: hydra.cpp.syntax.ExclusiveOrExpression) -> hydra.ast.Expr:
    r"""Convert an exclusive or expression to an expression."""

    match e:
        case hydra.cpp.syntax.ExclusiveOrExpressionAnd(value=a):
            return encode_and_expression(a)

        case hydra.cpp.syntax.ExclusiveOrExpressionBitwiseXor(value=x):
            return encode_bitwise_xor_operation(x)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_explicit_assignment(ea: hydra.cpp.syntax.ExplicitAssignment) -> hydra.ast.Expr:
    r"""Convert an explicit assignment to an expression."""

    left = ea.left
    op = ea.op
    right = ea.right
    return hydra.serialization.space_sep((encode_logical_or_expression(left), encode_assignment_operator(op), encode_assignment_expression(right)))

def encode_expression(e: hydra.cpp.syntax.Expression) -> hydra.ast.Expr:
    r"""Convert an expression to an expression."""

    match e:
        case hydra.cpp.syntax.ExpressionAssignment(value=a):
            return encode_assignment_expression(a)

        case hydra.cpp.syntax.ExpressionComma(value=c):
            return encode_comma_expression(c)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_for_init(i: hydra.cpp.syntax.ForInit) -> hydra.ast.Expr:
    r"""Convert a for-init to an expression."""

    match i:
        case hydra.cpp.syntax.ForInitExpression(value=e):
            return encode_expression(e)

        case hydra.cpp.syntax.ForInitDeclaration(value=d):
            return encode_variable_declaration(False, d)

        case hydra.cpp.syntax.ForInitEmpty():
            return hydra.serialization.cst("")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_for_statement(fs: hydra.cpp.syntax.ForStatement) -> hydra.ast.Expr:
    r"""Convert a for statement to an expression."""

    init = fs.init
    cond = fs.condition
    inc = fs.increment
    body = fs.body
    return hydra.serialization.newline_sep((hydra.serialization.space_sep((hydra.serialization.cst("for"), hydra.serialization.parens(hydra.serialization.no_sep((encode_for_init(init), hydra.serialization.cst(";"), encode_expression(cond), hydra.serialization.cst(";"), encode_expression(inc)))))), encode_statement(body)))

def encode_function_call_operation(fco: hydra.cpp.syntax.FunctionCallOperation) -> hydra.ast.Expr:
    r"""Convert a function call operation to an expression."""

    func = fco.function
    args = fco.arguments
    return hydra.serialization.no_sep((encode_postfix_expression(func), hydra.serialization.parens(hydra.serialization.comma_sep(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: encode_expression(x1)), args)))))

def encode_function_type(ft: hydra.cpp.syntax.FunctionType) -> hydra.ast.Expr:
    r"""Convert a function type to an expression."""

    ret_type = ft.return_type
    params = ft.parameters
    return hydra.serialization.space_sep((encode_type_expression(ret_type), hydra.serialization.parens(hydra.serialization.comma_sep(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: encode_parameter(x1)), params)))))

def encode_greater_equal_operation(op: hydra.cpp.syntax.GreaterEqualOperation) -> hydra.ast.Expr:
    r"""Convert a greater-than-or-equal operation to an expression."""

    left = op.left
    right = op.right
    return hydra.serialization.space_sep((encode_relational_expression(left), hydra.serialization.cst(">="), encode_shift_expression(right)))

def encode_greater_operation(op: hydra.cpp.syntax.GreaterOperation) -> hydra.ast.Expr:
    r"""Convert a greater-than operation to an expression."""

    left = op.left
    right = op.right
    return hydra.serialization.space_sep((encode_relational_expression(left), hydra.serialization.cst(">"), encode_shift_expression(right)))

def encode_inclusive_or_expression(e: hydra.cpp.syntax.InclusiveOrExpression) -> hydra.ast.Expr:
    r"""Convert an inclusive or expression to an expression."""

    match e:
        case hydra.cpp.syntax.InclusiveOrExpressionExclusiveOr(value=x):
            return encode_exclusive_or_expression(x)

        case hydra.cpp.syntax.InclusiveOrExpressionBitwiseOr(value=o):
            return encode_bitwise_or_operation(o)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_iteration_statement(i: hydra.cpp.syntax.IterationStatement) -> hydra.ast.Expr:
    r"""Convert an iteration statement to an expression."""

    match i:
        case hydra.cpp.syntax.IterationStatementWhile(value=w):
            return encode_while_statement(w)

        case hydra.cpp.syntax.IterationStatementDo(value=d):
            return encode_do_statement(d)

        case hydra.cpp.syntax.IterationStatementFor(value=f):
            return encode_for_statement(f)

        case hydra.cpp.syntax.IterationStatementRangeFor(value=r):
            return encode_range_for_statement(r)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_jump_statement(j: hydra.cpp.syntax.JumpStatement) -> hydra.ast.Expr:
    r"""Convert a jump statement to an expression."""

    match j:
        case hydra.cpp.syntax.JumpStatementBreak():
            return hydra.serialization.with_semi(hydra.serialization.cst("break"))

        case hydra.cpp.syntax.JumpStatementContinue():
            return hydra.serialization.with_semi(hydra.serialization.cst("continue"))

        case hydra.cpp.syntax.JumpStatementReturnValue(value=e):
            return hydra.serialization.with_semi(hydra.serialization.space_sep((hydra.serialization.cst("return"), encode_expression(e))))

        case hydra.cpp.syntax.JumpStatementReturnVoid():
            return hydra.serialization.with_semi(hydra.serialization.cst("return"))

        case hydra.cpp.syntax.JumpStatementThrow(value=e2):
            return hydra.serialization.with_semi(hydra.serialization.space_sep((hydra.serialization.cst("throw"), encode_expression(e2))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_labeled_statement(ls: hydra.cpp.syntax.LabeledStatement) -> hydra.ast.Expr:
    r"""Convert a labeled statement to an expression."""

    label = ls.label
    stmt = ls.statement
    return hydra.serialization.newline_sep((hydra.serialization.cst(hydra.lib.strings.cat2(label, ":")), encode_statement(stmt)))

def encode_lambda_expression(le: hydra.cpp.syntax.LambdaExpression) -> hydra.ast.Expr:
    r"""Convert a lambda expression to an expression."""

    captures = le.captures
    params = le.parameters
    ret_type = le.return_type
    body = le.body
    return hydra.serialization.space_sep((encode_capture_list(captures), hydra.lib.logic.if_else(hydra.lib.lists.null(params), (lambda : hydra.serialization.parens(hydra.serialization.cst(""))), (lambda : hydra.serialization.parens(hydra.serialization.comma_sep(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: encode_parameter(x1)), params))))), hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("")), (lambda t: hydra.serialization.space_sep((hydra.serialization.cst("->"), encode_type_expression(t)))), ret_type), encode_compound_statement(body)))

def encode_left_shift_operation(op: hydra.cpp.syntax.LeftShiftOperation) -> hydra.ast.Expr:
    r"""Convert a left shift operation to an expression."""

    left = op.left
    right = op.right
    return hydra.serialization.space_sep((encode_shift_expression(left), hydra.serialization.cst("<<"), encode_additive_expression(right)))

def encode_less_equal_operation(op: hydra.cpp.syntax.LessEqualOperation) -> hydra.ast.Expr:
    r"""Convert a less-than-or-equal operation to an expression."""

    left = op.left
    right = op.right
    return hydra.serialization.space_sep((encode_relational_expression(left), hydra.serialization.cst("<="), encode_shift_expression(right)))

def encode_less_operation(op: hydra.cpp.syntax.LessOperation) -> hydra.ast.Expr:
    r"""Convert a less-than operation to an expression."""

    left = op.left
    right = op.right
    return hydra.serialization.space_sep((encode_relational_expression(left), hydra.serialization.cst("<"), encode_shift_expression(right)))

def encode_logical_and_expression(e: hydra.cpp.syntax.LogicalAndExpression) -> hydra.ast.Expr:
    r"""Convert a logical and expression to an expression."""

    match e:
        case hydra.cpp.syntax.LogicalAndExpressionInclusiveOr(value=i):
            return encode_inclusive_or_expression(i)

        case hydra.cpp.syntax.LogicalAndExpressionLogicalAnd(value=a):
            return encode_logical_and_operation(a)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_logical_and_operation(op: hydra.cpp.syntax.LogicalAndOperation) -> hydra.ast.Expr:
    r"""Convert a logical and operation to an expression."""

    left = op.left
    right = op.right
    return hydra.serialization.space_sep((encode_logical_and_expression(left), hydra.serialization.cst("&&"), encode_inclusive_or_expression(right)))

def encode_logical_or_expression(e: hydra.cpp.syntax.LogicalOrExpression) -> hydra.ast.Expr:
    r"""Convert a logical or expression to an expression."""

    match e:
        case hydra.cpp.syntax.LogicalOrExpressionLogicalAnd(value=l):
            return encode_logical_and_expression(l)

        case hydra.cpp.syntax.LogicalOrExpressionLogicalOr(value=o):
            return encode_logical_or_operation(o)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_logical_or_operation(op: hydra.cpp.syntax.LogicalOrOperation) -> hydra.ast.Expr:
    r"""Convert a logical or operation to an expression."""

    left = op.left
    right = op.right
    return hydra.serialization.space_sep((encode_logical_or_expression(left), hydra.serialization.cst("||"), encode_logical_and_expression(right)))

def encode_member_access_operation(mao: hydra.cpp.syntax.MemberAccessOperation) -> hydra.ast.Expr:
    r"""Convert a member access operation to an expression."""

    obj = mao.object
    member = mao.member
    return hydra.serialization.no_sep((encode_postfix_expression(obj), hydra.serialization.cst("."), hydra.serialization.cst(member)))

def encode_modulo_operation(op: hydra.cpp.syntax.ModuloOperation) -> hydra.ast.Expr:
    r"""Convert a modulo operation to an expression."""

    left = op.left
    right = op.right
    return hydra.serialization.space_sep((encode_multiplicative_expression(left), hydra.serialization.cst("%"), encode_unary_expression(right)))

def encode_multiplicative_expression(e: hydra.cpp.syntax.MultiplicativeExpression) -> hydra.ast.Expr:
    r"""Convert a multiplicative expression to an expression."""

    match e:
        case hydra.cpp.syntax.MultiplicativeExpressionUnary(value=u):
            return encode_unary_expression(u)

        case hydra.cpp.syntax.MultiplicativeExpressionMultiply(value=m):
            return encode_multiply_operation(m)

        case hydra.cpp.syntax.MultiplicativeExpressionDivide(value=d):
            return encode_divide_operation(d)

        case hydra.cpp.syntax.MultiplicativeExpressionModulo(value=m2):
            return encode_modulo_operation(m2)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_multiply_operation(op: hydra.cpp.syntax.MultiplyOperation) -> hydra.ast.Expr:
    r"""Convert a multiply operation to an expression."""

    left = op.left
    right = op.right
    return hydra.serialization.space_sep((encode_multiplicative_expression(left), hydra.serialization.cst("*"), encode_unary_expression(right)))

def encode_not_equal_operation(op: hydra.cpp.syntax.NotEqualOperation) -> hydra.ast.Expr:
    r"""Convert a not-equal operation to an expression."""

    left = op.left
    right = op.right
    return hydra.serialization.space_sep((encode_equality_expression(left), hydra.serialization.cst("!="), encode_relational_expression(right)))

def encode_parameter(p: hydra.cpp.syntax.Parameter) -> hydra.ast.Expr:
    r"""Convert a parameter to an expression."""

    typ = p.type
    name = p.name
    unnamed = p.unnamed
    default_val = p.default_value
    @lru_cache(1)
    def name_expr() -> hydra.ast.Expr:
        return hydra.serialization.cst(hydra.lib.logic.if_else(unnamed, (lambda : hydra.lib.strings.cat(("/*", name, "*/"))), (lambda : name)))
    return hydra.serialization.space_sep(hydra.lib.lists.concat(((encode_type_expression(typ), name_expr()), hydra.lib.maybes.maybe((lambda : ()), (lambda expr: (hydra.serialization.cst("="), encode_expression(expr))), default_val))))

def encode_pointer_member_access_operation(pmao: hydra.cpp.syntax.PointerMemberAccessOperation) -> hydra.ast.Expr:
    r"""Convert a pointer member access operation to an expression."""

    ptr = pmao.pointer
    member = pmao.member
    return hydra.serialization.no_sep((encode_postfix_expression(ptr), hydra.serialization.cst("->"), hydra.serialization.cst(member)))

def encode_postfix_expression(e: hydra.cpp.syntax.PostfixExpression) -> hydra.ast.Expr:
    r"""Convert a postfix expression to an expression."""

    match e:
        case hydra.cpp.syntax.PostfixExpressionPrimary(value=p):
            return encode_primary_expression(p)

        case hydra.cpp.syntax.PostfixExpressionSubscript(value=s):
            return encode_subscript_operation(s)

        case hydra.cpp.syntax.PostfixExpressionFunctionCall(value=f):
            return encode_function_call_operation(f)

        case hydra.cpp.syntax.PostfixExpressionTemplateFunctionCall(value=t):
            return encode_template_function_call_operation(t)

        case hydra.cpp.syntax.PostfixExpressionMemberAccess(value=m):
            return encode_member_access_operation(m)

        case hydra.cpp.syntax.PostfixExpressionPointerMemberAccess(value=p2):
            return encode_pointer_member_access_operation(p2)

        case hydra.cpp.syntax.PostfixExpressionPostIncrement(value=p3):
            return hydra.serialization.no_sep((encode_postfix_expression(p3), hydra.serialization.cst("++")))

        case hydra.cpp.syntax.PostfixExpressionPostDecrement(value=p4):
            return hydra.serialization.no_sep((encode_postfix_expression(p4), hydra.serialization.cst("--")))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_primary_expression(e: hydra.cpp.syntax.PrimaryExpression) -> hydra.ast.Expr:
    r"""Convert a primary expression to an expression."""

    match e:
        case hydra.cpp.syntax.PrimaryExpressionIdentifier(value=id):
            return hydra.serialization.cst(id)

        case hydra.cpp.syntax.PrimaryExpressionLiteral(value=l):
            return encode_literal(l)

        case hydra.cpp.syntax.PrimaryExpressionParenthesized(value=p):
            return hydra.serialization.parens(encode_expression(p))

        case hydra.cpp.syntax.PrimaryExpressionLambda(value=l2):
            return encode_lambda_expression(l2)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_qualified_type(qt: hydra.cpp.syntax.QualifiedType) -> hydra.ast.Expr:
    r"""Convert a qualified type to an expression."""

    base_type = qt.base_type
    qualifier = qt.qualifier
    match qualifier:
        case hydra.cpp.syntax.TypeQualifier.CONST:
            return hydra.serialization.space_sep((hydra.serialization.cst("const"), encode_type_expression(base_type)))

        case hydra.cpp.syntax.TypeQualifier.LVALUE_REF:
            return hydra.serialization.no_sep((encode_type_expression(base_type), hydra.serialization.cst("&")))

        case hydra.cpp.syntax.TypeQualifier.RVALUE_REF:
            return hydra.serialization.no_sep((encode_type_expression(base_type), hydra.serialization.cst("&&")))

        case hydra.cpp.syntax.TypeQualifier.POINTER:
            return hydra.serialization.no_sep((encode_type_expression(base_type), hydra.serialization.cst("*")))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_range_for_statement(rfs: hydra.cpp.syntax.RangeForStatement) -> hydra.ast.Expr:
    r"""Convert a range-for statement to an expression."""

    typ = rfs.type
    var = rfs.variable
    range_ = rfs.range_
    body = rfs.body
    return hydra.serialization.newline_sep((hydra.serialization.space_sep((hydra.serialization.cst("for"), hydra.serialization.parens(hydra.serialization.space_sep((encode_type_expression(typ), hydra.serialization.cst(var), hydra.serialization.cst(":"), encode_expression(range_)))))), encode_statement(body)))

def encode_relational_expression(e: hydra.cpp.syntax.RelationalExpression) -> hydra.ast.Expr:
    r"""Convert a relational expression to an expression."""

    match e:
        case hydra.cpp.syntax.RelationalExpressionShift(value=s):
            return encode_shift_expression(s)

        case hydra.cpp.syntax.RelationalExpressionLess(value=l):
            return encode_less_operation(l)

        case hydra.cpp.syntax.RelationalExpressionGreater(value=g):
            return encode_greater_operation(g)

        case hydra.cpp.syntax.RelationalExpressionLessEqual(value=le):
            return encode_less_equal_operation(le)

        case hydra.cpp.syntax.RelationalExpressionGreaterEqual(value=ge):
            return encode_greater_equal_operation(ge)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_right_shift_operation(op: hydra.cpp.syntax.RightShiftOperation) -> hydra.ast.Expr:
    r"""Convert a right shift operation to an expression."""

    left = op.left
    right = op.right
    return hydra.serialization.space_sep((encode_shift_expression(left), hydra.serialization.cst(">>"), encode_additive_expression(right)))

def encode_selection_statement(ss: hydra.cpp.syntax.SelectionStatement) -> hydra.ast.Expr:
    r"""Convert a selection statement to an expression."""

    cond = ss.condition
    then_branch = ss.then_branch
    else_branch = ss.else_branch
    return hydra.serialization.newline_sep((hydra.serialization.space_sep((hydra.serialization.cst("if"), hydra.serialization.parens(encode_expression(cond)))), encode_statement(then_branch), hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("")), (lambda stmt: hydra.serialization.newline_sep((hydra.serialization.cst("else"), encode_statement(stmt)))), else_branch)))

def encode_shift_expression(e: hydra.cpp.syntax.ShiftExpression) -> hydra.ast.Expr:
    r"""Convert a shift expression to an expression."""

    match e:
        case hydra.cpp.syntax.ShiftExpressionAdditive(value=a):
            return encode_additive_expression(a)

        case hydra.cpp.syntax.ShiftExpressionLeftShift(value=ls):
            return encode_left_shift_operation(ls)

        case hydra.cpp.syntax.ShiftExpressionRightShift(value=rs):
            return encode_right_shift_operation(rs)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_sizeof_expression(se: hydra.cpp.syntax.SizeofExpression) -> hydra.ast.Expr:
    r"""Convert a sizeof expression to an expression."""

    return hydra.serialization.space_sep((hydra.serialization.cst("sizeof"), hydra.serialization.parens(encode_type_expression(se.value))))

def encode_statement(s: hydra.cpp.syntax.Statement) -> hydra.ast.Expr:
    r"""Convert a statement to an expression."""

    match s:
        case hydra.cpp.syntax.StatementLabeled(value=l):
            return encode_labeled_statement(l)

        case hydra.cpp.syntax.StatementCompound(value=c):
            return encode_compound_statement(c)

        case hydra.cpp.syntax.StatementSelection(value=s2):
            return encode_selection_statement(s2)

        case hydra.cpp.syntax.StatementSwitch(value=s22):
            return encode_switch_statement(s22)

        case hydra.cpp.syntax.StatementIteration(value=i):
            return encode_iteration_statement(i)

        case hydra.cpp.syntax.StatementJump(value=j):
            return encode_jump_statement(j)

        case hydra.cpp.syntax.StatementDeclaration(value=v):
            return hydra.serialization.with_semi(encode_variable_declaration(False, v))

        case hydra.cpp.syntax.StatementExpression(value=e):
            return hydra.serialization.with_semi(encode_expression(e))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_subscript_operation(so: hydra.cpp.syntax.SubscriptOperation) -> hydra.ast.Expr:
    r"""Convert a subscript operation to an expression."""

    array = so.array
    index = so.index
    return hydra.serialization.no_sep((encode_postfix_expression(array), hydra.serialization.cst("["), encode_expression(index), hydra.serialization.cst("]")))

def encode_subtract_operation(op: hydra.cpp.syntax.SubtractOperation) -> hydra.ast.Expr:
    r"""Convert a subtract operation to an expression."""

    left = op.left
    right = op.right
    return hydra.serialization.space_sep((encode_additive_expression(left), hydra.serialization.cst("-"), encode_multiplicative_expression(right)))

def encode_switch_statement(ss: hydra.cpp.syntax.SwitchStatement) -> hydra.ast.Expr:
    r"""Convert a switch statement to an expression."""

    value = ss.value
    cases = ss.cases
    return hydra.serialization.space_sep((hydra.serialization.cst("switch"), hydra.serialization.parens(encode_expression(value)), hydra.serialization.curly_block(hydra.serialization.full_block_style, hydra.serialization.newline_sep(hydra.lib.lists.map((lambda x1: encode_case_statement(x1)), cases)))))

def encode_template_argument(a: hydra.cpp.syntax.TemplateArgument) -> hydra.ast.Expr:
    r"""Convert a template argument to an expression."""

    match a:
        case hydra.cpp.syntax.TemplateArgumentType(value=t):
            return encode_type_expression(t)

        case hydra.cpp.syntax.TemplateArgumentValue(value=e):
            return encode_expression(e)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_template_function_call_operation(tfco: hydra.cpp.syntax.TemplateFunctionCallOperation) -> hydra.ast.Expr:
    r"""Convert a template function call operation to an expression."""

    func = tfco.function
    template_args = tfco.template_arguments
    args = tfco.arguments
    return hydra.serialization.no_sep((encode_postfix_expression(func), hydra.serialization.angle_braces_list(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: encode_template_argument(x1)), template_args)), hydra.serialization.parens(hydra.serialization.comma_sep(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: encode_expression(x1)), args)))))

def encode_template_type(tt: hydra.cpp.syntax.TemplateType) -> hydra.ast.Expr:
    r"""Convert a template type to an expression."""

    name = tt.name
    args = tt.arguments
    return hydra.serialization.no_sep((hydra.serialization.cst(name), hydra.serialization.angle_braces_list(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: encode_template_argument(x1)), args))))

def encode_ternary_expression(te: hydra.cpp.syntax.TernaryExpression) -> hydra.ast.Expr:
    r"""Convert a ternary expression to an expression."""

    cond = te.condition
    true_expr = te.true_expr
    false_expr = te.false_expr
    return hydra.serialization.space_sep((encode_logical_or_expression(cond), hydra.serialization.cst("?"), encode_expression(true_expr), hydra.serialization.cst(":"), encode_conditional_expression(false_expr)))

def encode_type_expression(t: hydra.cpp.syntax.TypeExpression) -> hydra.ast.Expr:
    r"""Convert a type expression to an expression."""

    match t:
        case hydra.cpp.syntax.TypeExpressionBasic(value=b):
            return encode_basic_type(b)

        case hydra.cpp.syntax.TypeExpressionQualified(value=q):
            return encode_qualified_type(q)

        case hydra.cpp.syntax.TypeExpressionTemplate(value=t2):
            return encode_template_type(t2)

        case hydra.cpp.syntax.TypeExpressionFunction(value=f):
            return encode_function_type(f)

        case hydra.cpp.syntax.TypeExpressionAuto():
            return hydra.serialization.cst("auto")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_unary_expression(e: hydra.cpp.syntax.UnaryExpression) -> hydra.ast.Expr:
    r"""Convert a unary expression to an expression."""

    match e:
        case hydra.cpp.syntax.UnaryExpressionPostfix(value=p):
            return encode_postfix_expression(p)

        case hydra.cpp.syntax.UnaryExpressionUnaryOp(value=o):
            return encode_unary_operation(o)

        case hydra.cpp.syntax.UnaryExpressionSizeof(value=s):
            return encode_sizeof_expression(s)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_unary_operation(uo: hydra.cpp.syntax.UnaryOperation) -> hydra.ast.Expr:
    r"""Convert a unary operation to an expression."""

    op = uo.operator
    operand = uo.operand
    return hydra.serialization.space_sep((encode_unary_operator(op), encode_unary_expression(operand)))

def encode_variable_declaration(commas: bool, vd: hydra.cpp.syntax.VariableDeclaration) -> hydra.ast.Expr:
    r"""Convert a variable declaration to an expression."""

    typ = vd.type
    name = vd.name
    init = vd.initializer
    is_auto = vd.is_auto
    @lru_cache(1)
    def terminator() -> Callable[[hydra.ast.Expr], hydra.ast.Expr]:
        return hydra.lib.logic.if_else(commas, (lambda : (lambda x1: hydra.serialization.with_comma(x1))), (lambda : (lambda x1: hydra.serialization.with_semi(x1))))
    return terminator(hydra.serialization.space_sep(hydra.lib.lists.concat((hydra.lib.logic.if_else(is_auto, (lambda : (hydra.serialization.cst("auto"),)), (lambda : hydra.lib.maybes.maybe((lambda : ()), (lambda t: (encode_type_expression(t),)), typ))), (hydra.serialization.cst(name),), hydra.lib.maybes.maybe((lambda : ()), (lambda expr: (hydra.serialization.cst("="), encode_expression(expr))), init)))))

def encode_while_statement(ws: hydra.cpp.syntax.WhileStatement) -> hydra.ast.Expr:
    r"""Convert a while statement to an expression."""

    cond = ws.condition
    body = ws.body
    return hydra.serialization.newline_sep((hydra.serialization.space_sep((hydra.serialization.cst("while"), hydra.serialization.parens(encode_expression(cond)))), encode_statement(body)))

def encode_base_specifier(bs: hydra.cpp.syntax.BaseSpecifier) -> hydra.ast.Expr:
    r"""Convert a base specifier to an expression."""

    access = bs.access
    name = bs.name
    return hydra.serialization.space_sep((encode_access_specifier(access), hydra.serialization.cst(name)))

def encode_class_key(k: hydra.cpp.syntax.ClassKey) -> hydra.ast.Expr:
    r"""Convert a class key to an expression."""

    match k:
        case hydra.cpp.syntax.ClassKey.CLASS:
            return hydra.serialization.cst("class")

        case hydra.cpp.syntax.ClassKey.ENUM:
            return hydra.serialization.cst("enum")

        case hydra.cpp.syntax.ClassKey.ENUM_CLASS:
            return hydra.serialization.cst("enum class")

        case hydra.cpp.syntax.ClassKey.STRUCT:
            return hydra.serialization.cst("struct")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_class_specifier(cs: hydra.cpp.syntax.ClassSpecifier) -> hydra.ast.Expr:
    r"""Convert a class specifier to an expression."""

    key = cs.key
    name = cs.name
    inheritance = cs.inheritance
    return hydra.serialization.space_sep(hydra.lib.lists.concat(((encode_class_key(key), hydra.serialization.cst(name)), hydra.lib.logic.if_else(hydra.lib.lists.null(inheritance), (lambda : ()), (lambda : (hydra.serialization.cst(":"), hydra.serialization.comma_sep(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: encode_base_specifier(x1)), inheritance))))))))

def encode_function_body(b: hydra.cpp.syntax.FunctionBody) -> hydra.ast.Expr:
    r"""Convert a function body to an expression."""

    match b:
        case hydra.cpp.syntax.FunctionBodyCompound(value=c):
            return encode_compound_statement(c)

        case hydra.cpp.syntax.FunctionBodyDeclaration():
            return hydra.serialization.cst(";")

        case hydra.cpp.syntax.FunctionBodyPure():
            return hydra.serialization.with_semi(hydra.serialization.cst("= 0"))

        case hydra.cpp.syntax.FunctionBodyDefault():
            return hydra.serialization.with_semi(hydra.serialization.cst("= default"))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_mem_initializer(mi: hydra.cpp.syntax.MemInitializer) -> hydra.ast.Expr:
    r"""Convert a member initializer to an expression."""

    name = mi.name
    args = mi.arguments
    return hydra.serialization.no_sep((hydra.serialization.cst(name), hydra.serialization.parens(hydra.serialization.comma_sep(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: encode_expression(x1)), args)))))

def encode_constructor_declaration(cd: hydra.cpp.syntax.ConstructorDeclaration) -> hydra.ast.Expr:
    r"""Convert a constructor declaration to an expression."""

    name = cd.name
    params = cd.parameters
    inits = cd.initializers
    body = cd.body
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(hydra.serialization.no_sep((hydra.serialization.cst(name), hydra.serialization.parens(hydra.serialization.comma_sep(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: encode_parameter(x1)), params)))))), hydra.lib.logic.if_else(hydra.lib.lists.null(inits), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep((hydra.serialization.cst(":"), hydra.serialization.comma_sep(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: encode_mem_initializer(x1)), inits))))))), Just(encode_function_body(body)))))

def encode_function_specifier_prefix(s: hydra.cpp.syntax.FunctionSpecifierPrefix) -> hydra.ast.Expr:
    r"""Convert a function specifier prefix to an expression."""

    match s:
        case hydra.cpp.syntax.FunctionSpecifierPrefix.INLINE:
            return hydra.serialization.cst("inline")

        case hydra.cpp.syntax.FunctionSpecifierPrefix.VIRTUAL:
            return hydra.serialization.cst("virtual")

        case hydra.cpp.syntax.FunctionSpecifierPrefix.STATIC:
            return hydra.serialization.cst("static")

        case hydra.cpp.syntax.FunctionSpecifierPrefix.EXPLICIT:
            return hydra.serialization.cst("explicit")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_function_specifier_suffix(s: hydra.cpp.syntax.FunctionSpecifierSuffix) -> hydra.ast.Expr:
    r"""Convert a function specifier suffix to an expression."""

    match s:
        case hydra.cpp.syntax.FunctionSpecifierSuffix.CONST:
            return hydra.serialization.cst("const")

        case hydra.cpp.syntax.FunctionSpecifierSuffix.NOEXCEPT:
            return hydra.serialization.cst("noexcept")

        case hydra.cpp.syntax.FunctionSpecifierSuffix.OVERRIDE:
            return hydra.serialization.cst("override")

        case hydra.cpp.syntax.FunctionSpecifierSuffix.FINAL:
            return hydra.serialization.cst("final")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_destructor_declaration(dd: hydra.cpp.syntax.DestructorDeclaration) -> hydra.ast.Expr:
    r"""Convert a destructor declaration to an expression."""

    prefix_specs = dd.prefix_specifiers
    name = dd.name
    suffix_specs = dd.suffix_specifiers
    body = dd.body
    return hydra.serialization.space_sep(hydra.lib.lists.concat((hydra.lib.lists.map((lambda x1: encode_function_specifier_prefix(x1)), prefix_specs), (hydra.serialization.no_sep((hydra.serialization.cst(hydra.lib.strings.cat2("~", name)), hydra.serialization.parens(hydra.serialization.cst("")))),), hydra.lib.lists.map((lambda x1: encode_function_specifier_suffix(x1)), suffix_specs), (encode_function_body(body),))))

def encode_function_declaration(fd: hydra.cpp.syntax.FunctionDeclaration) -> hydra.ast.Expr:
    r"""Convert a function declaration to an expression."""

    prefix_specs = fd.prefix_specifiers
    ret_type = fd.return_type
    name = fd.name
    params = fd.parameters
    suffix_specs = fd.suffix_specifiers
    body = fd.body
    return hydra.serialization.space_sep(hydra.lib.lists.concat((hydra.lib.lists.map((lambda x1: encode_function_specifier_prefix(x1)), prefix_specs), (encode_type_expression(ret_type), hydra.serialization.no_sep((hydra.serialization.cst(name), hydra.serialization.parens(hydra.serialization.comma_sep(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: encode_parameter(x1)), params)))))), hydra.lib.lists.map((lambda x1: encode_function_specifier_suffix(x1)), suffix_specs), (encode_function_body(body),))))

def encode_define_directive(dd: hydra.cpp.syntax.DefineDirective) -> hydra.ast.Expr:
    r"""Convert a define directive to an expression."""

    name = dd.name
    params = dd.parameters
    replacement = dd.replacement
    return hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("#define"), hydra.serialization.cst(name)), hydra.lib.maybes.maybe((lambda : ()), (lambda ps: (hydra.serialization.parens(hydra.serialization.comma_sep(hydra.serialization.inline_style, hydra.lib.lists.map((lambda p: hydra.serialization.cst(p)), ps))),)), params), hydra.lib.maybes.maybe((lambda : ()), (lambda r: (hydra.serialization.cst(r),)), replacement))))

def encode_elif_directive(ed: hydra.cpp.syntax.ElifDirective) -> hydra.ast.Expr:
    r"""Convert an elif directive to an expression."""

    return hydra.serialization.space_sep((hydra.serialization.cst("#elif"), hydra.serialization.cst(ed.condition)))

def encode_else_directive(ed: T0) -> hydra.ast.Expr:
    r"""Convert an else directive to an expression."""

    return hydra.serialization.cst("#else")

def encode_endif_directive(ed: T0) -> hydra.ast.Expr:
    r"""Convert an endif directive to an expression."""

    return hydra.serialization.cst("#endif")

def encode_error_directive(ed: hydra.cpp.syntax.ErrorDirective) -> hydra.ast.Expr:
    r"""Convert an error directive to an expression."""

    return hydra.serialization.space_sep((hydra.serialization.cst("#error"), hydra.serialization.cst(ed.message)))

def encode_if_directive(ifd: hydra.cpp.syntax.IfDirective) -> hydra.ast.Expr:
    r"""Convert an if directive to an expression."""

    return hydra.serialization.space_sep((hydra.serialization.cst("#if"), hydra.serialization.cst(ifd.condition)))

def encode_ifdef_directive(id: hydra.cpp.syntax.IfdefDirective) -> hydra.ast.Expr:
    r"""Convert an ifdef directive to an expression."""

    return hydra.serialization.space_sep((hydra.serialization.cst("#ifdef"), hydra.serialization.cst(id.identifier)))

def encode_ifndef_directive(ind: hydra.cpp.syntax.IfndefDirective) -> hydra.ast.Expr:
    r"""Convert an ifndef directive to an expression."""

    return hydra.serialization.space_sep((hydra.serialization.cst("#ifndef"), hydra.serialization.cst(ind.identifier)))

def encode_include_directive(incl: hydra.cpp.syntax.IncludeDirective) -> hydra.ast.Expr:
    r"""Convert an include directive to an expression."""

    name = incl.name
    is_system = incl.is_system
    return hydra.lib.logic.if_else(is_system, (lambda : hydra.serialization.cst(hydra.lib.strings.cat(("#include <", name, ">")))), (lambda : hydra.serialization.cst(hydra.lib.strings.cat(("#include \"", name, "\"")))))

def encode_line_directive(ld: hydra.cpp.syntax.LineDirective) -> hydra.ast.Expr:
    r"""Convert a line directive to an expression."""

    line_number = ld.line_number
    filename = ld.filename
    return hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("#line"), hydra.serialization.cst(hydra.lib.literals.show_int32(line_number))), hydra.lib.maybes.maybe((lambda : ()), (lambda f: (hydra.serialization.cst(hydra.lib.strings.cat(("\"", f, "\""))),)), filename))))

def encode_pragma_directive(pd: hydra.cpp.syntax.PragmaDirective) -> hydra.ast.Expr:
    r"""Convert a pragma directive to an expression."""

    return hydra.serialization.cst(hydra.lib.strings.cat2("#pragma ", pd.content))

def encode_undef_directive(ud: hydra.cpp.syntax.UndefDirective) -> hydra.ast.Expr:
    r"""Convert an undef directive to an expression."""

    return hydra.serialization.space_sep((hydra.serialization.cst("#undef"), hydra.serialization.cst(ud.name)))

def encode_warning_directive(wd: hydra.cpp.syntax.WarningDirective) -> hydra.ast.Expr:
    r"""Convert a warning directive to an expression."""

    return hydra.serialization.space_sep((hydra.serialization.cst("#warning"), hydra.serialization.cst(wd.message)))

def encode_preprocessor_directive(d: hydra.cpp.syntax.PreprocessorDirective) -> hydra.ast.Expr:
    r"""Convert a preprocessor directive to an expression."""

    match d:
        case hydra.cpp.syntax.PreprocessorDirectiveInclude(value=i):
            return encode_include_directive(i)

        case hydra.cpp.syntax.PreprocessorDirectivePragma(value=p):
            return encode_pragma_directive(p)

        case hydra.cpp.syntax.PreprocessorDirectiveDefine(value=d2):
            return encode_define_directive(d2)

        case hydra.cpp.syntax.PreprocessorDirectiveUndef(value=u):
            return encode_undef_directive(u)

        case hydra.cpp.syntax.PreprocessorDirectiveIfdef(value=i2):
            return encode_ifdef_directive(i2)

        case hydra.cpp.syntax.PreprocessorDirectiveIfndef(value=i3):
            return encode_ifndef_directive(i3)

        case hydra.cpp.syntax.PreprocessorDirectiveIf(value=i4):
            return encode_if_directive(i4)

        case hydra.cpp.syntax.PreprocessorDirectiveElif(value=e):
            return encode_elif_directive(e)

        case hydra.cpp.syntax.PreprocessorDirectiveElse(value=e2):
            return encode_else_directive(e2)

        case hydra.cpp.syntax.PreprocessorDirectiveEndif(value=e3):
            return encode_endif_directive(e3)

        case hydra.cpp.syntax.PreprocessorDirectiveLine(value=l):
            return encode_line_directive(l)

        case hydra.cpp.syntax.PreprocessorDirectiveError(value=e4):
            return encode_error_directive(e4)

        case hydra.cpp.syntax.PreprocessorDirectiveWarning(value=w):
            return encode_warning_directive(w)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_typedef_declaration(td: hydra.cpp.syntax.TypedefDeclaration) -> hydra.ast.Expr:
    r"""Convert a typedef declaration to an expression."""

    name = td.name
    typ = td.type
    is_using = td.is_using
    return hydra.lib.logic.if_else(is_using, (lambda : hydra.serialization.with_semi(hydra.serialization.space_sep((hydra.serialization.cst(hydra.lib.strings.cat2("using ", name)), hydra.serialization.cst("="), encode_type_expression(typ))))), (lambda : hydra.serialization.with_semi(hydra.serialization.space_sep((hydra.serialization.cst("typedef"), encode_type_expression(typ), hydra.serialization.cst(name))))))

def encode_class_body(commas: bool, cb: hydra.cpp.syntax.ClassBody) -> hydra.ast.Expr:
    r"""Convert a class body to an expression."""

    return hydra.serialization.curly_block(hydra.serialization.full_block_style, hydra.serialization.double_newline_sep(hydra.lib.lists.map((lambda v1: encode_member_specification(commas, v1)), cb.value)))

def encode_class_declaration(cd: hydra.cpp.syntax.ClassDeclaration) -> hydra.ast.Expr:
    r"""Convert a class declaration to an expression."""

    spec = cd.specifier
    mbody = cd.body
    key = spec.key
    @lru_cache(1)
    def is_enum() -> bool:
        return hydra.lib.logic.or_(hydra.lib.equality.equal(key, hydra.cpp.syntax.ClassKey.ENUM), hydra.lib.equality.equal(key, hydra.cpp.syntax.ClassKey.ENUM_CLASS))
    return hydra.serialization.with_semi(hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(encode_class_specifier(spec)), hydra.lib.maybes.map((lambda body: encode_class_body(is_enum(), body)), mbody)))))

def encode_declaration(d: hydra.cpp.syntax.Declaration) -> hydra.ast.Expr:
    r"""Convert a declaration to an expression."""

    match d:
        case hydra.cpp.syntax.DeclarationPreprocessor(value=p):
            return encode_preprocessor_directive(p)

        case hydra.cpp.syntax.DeclarationClass(value=c):
            return encode_class_declaration(c)

        case hydra.cpp.syntax.DeclarationFunction(value=f):
            return encode_function_declaration(f)

        case hydra.cpp.syntax.DeclarationVariable(value=v):
            return encode_variable_declaration(False, v)

        case hydra.cpp.syntax.DeclarationTypedef(value=t):
            return encode_typedef_declaration(t)

        case hydra.cpp.syntax.DeclarationNamespace(value=n):
            return encode_namespace_declaration(n)

        case hydra.cpp.syntax.DeclarationTemplate(value=t2):
            return encode_template_declaration(t2)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_member_declaration(commas: bool, m: hydra.cpp.syntax.MemberDeclaration) -> hydra.ast.Expr:
    r"""Convert a member declaration to an expression."""

    match m:
        case hydra.cpp.syntax.MemberDeclarationFunction(value=f):
            return encode_function_declaration(f)

        case hydra.cpp.syntax.MemberDeclarationVariable(value=v):
            return encode_variable_declaration(commas, v)

        case hydra.cpp.syntax.MemberDeclarationConstructor(value=c):
            return encode_constructor_declaration(c)

        case hydra.cpp.syntax.MemberDeclarationDestructor(value=d):
            return encode_destructor_declaration(d)

        case hydra.cpp.syntax.MemberDeclarationNestedClass(value=c2):
            return encode_class_declaration(c2)

        case hydra.cpp.syntax.MemberDeclarationTemplate(value=t):
            return encode_template_declaration(t)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_member_specification(commas: bool, m: hydra.cpp.syntax.MemberSpecification) -> hydra.ast.Expr:
    r"""Convert a member specification to an expression."""

    match m:
        case hydra.cpp.syntax.MemberSpecificationAccessLabel(value=a):
            return hydra.serialization.no_sep((encode_access_specifier(a), hydra.serialization.cst(":")))

        case hydra.cpp.syntax.MemberSpecificationMember(value=d):
            return encode_member_declaration(commas, d)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_namespace_declaration(nd: hydra.cpp.syntax.NamespaceDeclaration) -> hydra.ast.Expr:
    r"""Convert a namespace declaration to an expression."""

    name = nd.name
    decls = nd.declarations
    return hydra.serialization.space_sep((hydra.serialization.cst(hydra.lib.strings.cat2("namespace ", name)), hydra.serialization.curly_block(hydra.serialization.full_block_style, hydra.serialization.double_newline_sep(hydra.lib.lists.map((lambda x1: encode_declaration(x1)), decls)))))

def encode_template_declaration(td: hydra.cpp.syntax.TemplateDeclaration) -> hydra.ast.Expr:
    r"""Convert a template declaration to an expression."""

    inline = td.inline
    params = td.parameters
    declaration = td.declaration
    @lru_cache(1)
    def sep() -> Callable[[frozenlist[hydra.ast.Expr]], hydra.ast.Expr]:
        return hydra.lib.logic.if_else(inline, (lambda : (lambda x1: hydra.serialization.space_sep(x1))), (lambda : (lambda x1: hydra.serialization.newline_sep(x1))))
    return sep((hydra.serialization.no_sep((hydra.serialization.cst("template"), hydra.serialization.angle_braces_list(hydra.serialization.inline_style, hydra.lib.lists.map((lambda p: hydra.serialization.cst(p)), params)))), encode_declaration(declaration)))

def to_cpp_comments(s: str, is_multiline: bool) -> str:
    r"""Convert a string to a C++ comment."""

    return hydra.lib.logic.if_else(is_multiline, (lambda : hydra.lib.strings.cat(("/* ", s, " */"))), (lambda : hydra.lib.strings.cat2("// ", s)))

def encode_comment(c: hydra.cpp.syntax.Comment) -> hydra.ast.Expr:
    r"""Convert a comment to an expression."""

    text = c.text
    is_multiline = c.is_multiline
    return hydra.serialization.cst(to_cpp_comments(text, is_multiline))

def encode_qualified_identifier(qi: hydra.cpp.syntax.QualifiedIdentifier) -> hydra.ast.Expr:
    r"""Convert a qualified identifier to an expression."""

    ns = qi.namespace
    name = qi.name
    return hydra.serialization.cst(hydra.lib.strings.cat((ns, "::", name)))

def encode_function_identifier(f: hydra.cpp.syntax.FunctionIdentifier) -> hydra.ast.Expr:
    r"""Convert a function identifier to an expression."""

    match f:
        case hydra.cpp.syntax.FunctionIdentifierSimple(value=name):
            return hydra.serialization.cst(name)

        case hydra.cpp.syntax.FunctionIdentifierQualified(value=q):
            return encode_qualified_identifier(q)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_function_application(fa: hydra.cpp.syntax.FunctionApplication) -> hydra.ast.Expr:
    r"""Convert a function application to an expression."""

    func = fa.function
    args = fa.arguments
    return hydra.serialization.space_sep((encode_function_identifier(func), hydra.serialization.parens(hydra.serialization.comma_sep(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: encode_expression(x1)), args)))))

def encode_map_entry(me: hydra.cpp.syntax.MapEntry) -> hydra.ast.Expr:
    r"""Convert a map entry to an expression."""

    key = me.key
    val = me.value
    return hydra.serialization.space_sep((hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.inline_style, (encode_expression(key),)), hydra.serialization.cst("->"), encode_expression(val)))

def encode_map(m: hydra.cpp.syntax.Map) -> hydra.ast.Expr:
    r"""Convert a map to an expression."""

    key_type = m.key_type
    val_type = m.value_type
    entries = m.entries
    return hydra.serialization.space_sep((hydra.serialization.cst("std::map<"), hydra.serialization.comma_sep(hydra.serialization.inline_style, (encode_type_expression(key_type), encode_type_expression(val_type))), hydra.serialization.cst(">"), hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: encode_map_entry(x1)), entries))))

def encode_optional(opt: hydra.cpp.syntax.Optional) -> hydra.ast.Expr:
    r"""Convert an optional to an expression."""

    val_type = opt.value_type
    val = opt.value
    return hydra.serialization.space_sep((hydra.serialization.cst("std::optional<"), encode_type_expression(val_type), hydra.serialization.cst(">"), hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("{}")), (lambda v: hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.inline_style, (encode_expression(v),))), val)))

def encode_overloaded_lambdas(ol: hydra.cpp.syntax.OverloadedLambdas) -> hydra.ast.Expr:
    r"""Convert overloaded lambdas to an expression."""

    return hydra.serialization.space_sep((hydra.serialization.cst("overloaded"), hydra.serialization.curly_block(hydra.serialization.full_block_style, hydra.serialization.newline_sep(hydra.lib.lists.map((lambda x1: encode_lambda_expression(x1)), ol.value)))))

def encode_visitor(v: hydra.cpp.syntax.Visitor) -> hydra.ast.Expr:
    r"""Convert a visitor to an expression."""

    match v:
        case hydra.cpp.syntax.VisitorLambda(value=l):
            return encode_lambda_expression(l)

        case hydra.cpp.syntax.VisitorOverloaded(value=o):
            return encode_overloaded_lambdas(o)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_pattern_match(pm: hydra.cpp.syntax.PatternMatch) -> hydra.ast.Expr:
    r"""Convert a pattern match to an expression."""

    visitor = pm.visitor
    variant = pm.variant
    return hydra.serialization.space_sep((hydra.serialization.cst("std::visit"), hydra.serialization.parens(hydra.serialization.comma_sep(hydra.serialization.inline_style, (encode_visitor(visitor), encode_expression(variant))))))

def encode_program(prog: hydra.cpp.syntax.Program) -> hydra.ast.Expr:
    r"""Convert a program to an expression."""

    preps = prog.preprocessor_directives
    includes = prog.includes
    decls = prog.declarations
    def separate(sep: Callable[[frozenlist[T0]], T1], defs: frozenlist[T0]) -> Maybe[T1]:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(defs), (lambda : Nothing()), (lambda : Just(sep(defs))))
    return hydra.serialization.double_newline_sep(hydra.lib.maybes.cat((separate((lambda x1: hydra.serialization.newline_sep(x1)), hydra.lib.lists.map((lambda x1: encode_preprocessor_directive(x1)), preps)), separate((lambda x1: hydra.serialization.newline_sep(x1)), hydra.lib.lists.map((lambda x1: encode_include_directive(x1)), includes)), separate((lambda x1: hydra.serialization.double_newline_sep(x1)), hydra.lib.lists.map((lambda x1: encode_declaration(x1)), decls)))))

def encode_set(s: hydra.cpp.syntax.Set) -> hydra.ast.Expr:
    r"""Convert a set to an expression."""

    elem_type = s.element_type
    elems = s.elements
    return hydra.serialization.space_sep((hydra.serialization.cst("std::set<"), encode_type_expression(elem_type), hydra.serialization.cst(">"), hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: encode_expression(x1)), elems))))

def encode_vector(v: hydra.cpp.syntax.Vector) -> hydra.ast.Expr:
    r"""Convert a vector to an expression."""

    elem_type = v.element_type
    elems = v.elements
    return hydra.serialization.space_sep((hydra.serialization.cst("std::vector<"), encode_type_expression(elem_type), hydra.serialization.cst(">"), hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: encode_expression(x1)), elems))))
