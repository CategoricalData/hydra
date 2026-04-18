# Note: this is an automatically generated file. Do not edit.

r"""C++ utilities for constructing C++ syntax trees."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.cpp.syntax
import hydra.lib.lists
import hydra.lib.logic

T0 = TypeVar("T0")

def const_parameter(name: str, typ: hydra.cpp.syntax.TypeExpression) -> hydra.cpp.syntax.Parameter:
    r"""Create a const reference parameter."""

    return hydra.cpp.syntax.Parameter(cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionQualified(hydra.cpp.syntax.QualifiedType(cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionQualified(hydra.cpp.syntax.QualifiedType(typ, hydra.cpp.syntax.TypeQualifier.CONST))), hydra.cpp.syntax.TypeQualifier.LVALUE_REF))), name, False, Nothing())

def cpp_class_declaration(name: str, base_specs: frozenlist[hydra.cpp.syntax.BaseSpecifier], mbody: Maybe[hydra.cpp.syntax.ClassBody]) -> hydra.cpp.syntax.Declaration:
    r"""Create a C++ class declaration."""

    return cast(hydra.cpp.syntax.Declaration, hydra.cpp.syntax.DeclarationClass(hydra.cpp.syntax.ClassDeclaration(hydra.cpp.syntax.ClassSpecifier(hydra.cpp.syntax.ClassKey.CLASS, name, base_specs), mbody)))

def cpp_enum_declaration(name: str, mbody: Maybe[hydra.cpp.syntax.ClassBody]) -> hydra.cpp.syntax.Declaration:
    r"""Create a C++ enum class declaration."""

    return cast(hydra.cpp.syntax.Declaration, hydra.cpp.syntax.DeclarationClass(hydra.cpp.syntax.ClassDeclaration(hydra.cpp.syntax.ClassSpecifier(hydra.cpp.syntax.ClassKey.ENUM_CLASS, name, ()), mbody)))

def cpp_enum_forward_declaration(name: str) -> hydra.cpp.syntax.Declaration:
    r"""Create a C++ enum class forward declaration."""

    return cast(hydra.cpp.syntax.Declaration, hydra.cpp.syntax.DeclarationClass(hydra.cpp.syntax.ClassDeclaration(hydra.cpp.syntax.ClassSpecifier(hydra.cpp.syntax.ClassKey.ENUM_CLASS, name, ()), Nothing())))

def cpp_unary_expression_to_cpp_logical_or_expression(ue: hydra.cpp.syntax.UnaryExpression) -> hydra.cpp.syntax.LogicalOrExpression:
    r"""Convert a UnaryExpression to a LogicalOrExpression."""

    return cast(hydra.cpp.syntax.LogicalOrExpression, hydra.cpp.syntax.LogicalOrExpressionLogicalAnd(cast(hydra.cpp.syntax.LogicalAndExpression, hydra.cpp.syntax.LogicalAndExpressionInclusiveOr(cast(hydra.cpp.syntax.InclusiveOrExpression, hydra.cpp.syntax.InclusiveOrExpressionExclusiveOr(cast(hydra.cpp.syntax.ExclusiveOrExpression, hydra.cpp.syntax.ExclusiveOrExpressionAnd(cast(hydra.cpp.syntax.AndExpression, hydra.cpp.syntax.AndExpressionEquality(cast(hydra.cpp.syntax.EqualityExpression, hydra.cpp.syntax.EqualityExpressionRelational(cast(hydra.cpp.syntax.RelationalExpression, hydra.cpp.syntax.RelationalExpressionShift(cast(hydra.cpp.syntax.ShiftExpression, hydra.cpp.syntax.ShiftExpressionAdditive(cast(hydra.cpp.syntax.AdditiveExpression, hydra.cpp.syntax.AdditiveExpressionMultiplicative(cast(hydra.cpp.syntax.MultiplicativeExpression, hydra.cpp.syntax.MultiplicativeExpressionUnary(ue))))))))))))))))))))

def cpp_unary_expression_to_cpp_expression(ue: hydra.cpp.syntax.UnaryExpression) -> hydra.cpp.syntax.Expression:
    r"""Convert a UnaryExpression to an Expression."""

    return cast(hydra.cpp.syntax.Expression, hydra.cpp.syntax.ExpressionAssignment(cast(hydra.cpp.syntax.AssignmentExpression, hydra.cpp.syntax.AssignmentExpressionConditional(cast(hydra.cpp.syntax.ConditionalExpression, hydra.cpp.syntax.ConditionalExpressionLogicalOr(cpp_unary_expression_to_cpp_logical_or_expression(ue)))))))

def cpp_postfix_expression_to_cpp_expression(pf: hydra.cpp.syntax.PostfixExpression) -> hydra.cpp.syntax.Expression:
    r"""Convert a PostfixExpression to an Expression."""

    return cpp_unary_expression_to_cpp_expression(cast(hydra.cpp.syntax.UnaryExpression, hydra.cpp.syntax.UnaryExpressionPostfix(pf)))

def cpp_primary_expression_to_cpp_expression(prim: hydra.cpp.syntax.PrimaryExpression) -> hydra.cpp.syntax.Expression:
    r"""Convert a PrimaryExpression to an Expression."""

    return cpp_postfix_expression_to_cpp_expression(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(prim)))

def create_cast_expr(target_type: T0, expr: hydra.cpp.syntax.Expression) -> hydra.cpp.syntax.Expression:
    r"""Create a cast expression."""

    return cpp_primary_expression_to_cpp_expression(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionParenthesized(cpp_primary_expression_to_cpp_expression(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionParenthesized(expr))))))

def create_compound_stmt(stmts: frozenlist[hydra.cpp.syntax.Statement]) -> hydra.cpp.syntax.CompoundStatement:
    r"""Create a compound statement."""

    return hydra.cpp.syntax.CompoundStatement(stmts)

def create_qualified_type(base_type: hydra.cpp.syntax.TypeExpression, qualifier: hydra.cpp.syntax.TypeQualifier) -> hydra.cpp.syntax.TypeExpression:
    r"""Create a qualified type with a qualifier."""

    return cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionQualified(hydra.cpp.syntax.QualifiedType(base_type, qualifier)))

def create_const_type(base_type: hydra.cpp.syntax.TypeExpression) -> hydra.cpp.syntax.TypeExpression:
    r"""Create a const-qualified type."""

    return create_qualified_type(base_type, hydra.cpp.syntax.TypeQualifier.CONST)

def create_reference_type(base_type: hydra.cpp.syntax.TypeExpression) -> hydra.cpp.syntax.TypeExpression:
    r"""Create a reference type."""

    return create_qualified_type(base_type, hydra.cpp.syntax.TypeQualifier.LVALUE_REF)

def create_const_ref_type(base_type: hydra.cpp.syntax.TypeExpression) -> hydra.cpp.syntax.TypeExpression:
    r"""Create a const reference type."""

    return create_reference_type(create_const_type(base_type))

@lru_cache(1)
def empty_function_body() -> hydra.cpp.syntax.FunctionBody:
    r"""An empty function body."""

    return cast(hydra.cpp.syntax.FunctionBody, hydra.cpp.syntax.FunctionBodyCompound(hydra.cpp.syntax.CompoundStatement(())))

def create_constructor_body(params: frozenlist[T0]) -> hydra.cpp.syntax.FunctionBody:
    r"""Create a constructor body (default if no params, empty otherwise)."""

    return hydra.lib.logic.if_else(hydra.lib.lists.null(params), (lambda : cast(hydra.cpp.syntax.FunctionBody, hydra.cpp.syntax.FunctionBodyDefault())), (lambda : empty_function_body()))

def create_enum_access_expr(enum_name: str, value_name: str) -> hydra.cpp.syntax.Expression:
    r"""Create an enum access expression."""

    return cpp_postfix_expression_to_cpp_expression(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionMemberAccess(hydra.cpp.syntax.MemberAccessOperation(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier(enum_name)))), value_name))))

def create_function_call_expr(func_name: str, args: frozenlist[hydra.cpp.syntax.Expression]) -> hydra.cpp.syntax.Expression:
    r"""Create a function call expression."""

    return cpp_postfix_expression_to_cpp_expression(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionFunctionCall(hydra.cpp.syntax.FunctionCallOperation(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier(func_name)))), args))))

def create_header_file(includes: frozenlist[hydra.cpp.syntax.IncludeDirective], decls: frozenlist[hydra.cpp.syntax.Declaration]) -> hydra.cpp.syntax.Program:
    r"""Create a header file with pragma once."""

    return hydra.cpp.syntax.Program((cast(hydra.cpp.syntax.PreprocessorDirective, hydra.cpp.syntax.PreprocessorDirectivePragma(hydra.cpp.syntax.PragmaDirective("once"))),), includes, decls)

def create_identifier_expr(name: str) -> hydra.cpp.syntax.Expression:
    r"""Create an identifier expression."""

    return cpp_primary_expression_to_cpp_expression(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier(name)))

def create_lambda_expr(params: frozenlist[hydra.cpp.syntax.Parameter], body: hydra.cpp.syntax.Expression) -> hydra.cpp.syntax.Expression:
    r"""Create a lambda expression."""

    return cpp_postfix_expression_to_cpp_expression(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionLambda(hydra.cpp.syntax.LambdaExpression(cast(hydra.cpp.syntax.CaptureList, hydra.cpp.syntax.CaptureListCaptures(())), params, Nothing(), hydra.cpp.syntax.CompoundStatement((cast(hydra.cpp.syntax.Statement, hydra.cpp.syntax.StatementExpression(body)),))))))))

def create_literal_bool_expr(val: bool) -> hydra.cpp.syntax.Expression:
    r"""Create a boolean literal expression."""

    return cpp_primary_expression_to_cpp_expression(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionLiteral(cast(hydra.cpp.syntax.Literal, hydra.cpp.syntax.LiteralBoolean(hydra.cpp.syntax.BooleanLiteral(val))))))

def create_literal_int_expr(val: int) -> hydra.cpp.syntax.Expression:
    r"""Create an integer literal expression."""

    return cpp_primary_expression_to_cpp_expression(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionLiteral(cast(hydra.cpp.syntax.Literal, hydra.cpp.syntax.LiteralInteger(cast(hydra.cpp.syntax.IntegerLiteral, hydra.cpp.syntax.IntegerLiteralDecimal(val)))))))

def create_literal_string_expr(val: str) -> hydra.cpp.syntax.Expression:
    r"""Create a string literal expression."""

    return cpp_primary_expression_to_cpp_expression(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionLiteral(cast(hydra.cpp.syntax.Literal, hydra.cpp.syntax.LiteralString(hydra.cpp.syntax.StringLiteral(val))))))

def extract_postfix_expression(expr: hydra.cpp.syntax.Expression):
    def _hoist_hydra_cpp_utils_extract_postfix_expression_1(v1):
        match v1:
            case hydra.cpp.syntax.UnaryExpressionPostfix(value=pf):
                return pf

            case _:
                return cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier("error"))))
    def _hoist_hydra_cpp_utils_extract_postfix_expression_2(v1):
        match v1:
            case hydra.cpp.syntax.MultiplicativeExpressionUnary(value=ue):
                return _hoist_hydra_cpp_utils_extract_postfix_expression_1(ue)

            case _:
                return cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier("error"))))
    def _hoist_hydra_cpp_utils_extract_postfix_expression_3(v1):
        match v1:
            case hydra.cpp.syntax.AdditiveExpressionMultiplicative(value=mu):
                return _hoist_hydra_cpp_utils_extract_postfix_expression_2(mu)

            case _:
                return cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier("error"))))
    def _hoist_hydra_cpp_utils_extract_postfix_expression_4(v1):
        match v1:
            case hydra.cpp.syntax.ShiftExpressionAdditive(value=ad):
                return _hoist_hydra_cpp_utils_extract_postfix_expression_3(ad)

            case _:
                return cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier("error"))))
    def _hoist_hydra_cpp_utils_extract_postfix_expression_5(v1):
        match v1:
            case hydra.cpp.syntax.RelationalExpressionShift(value=sh):
                return _hoist_hydra_cpp_utils_extract_postfix_expression_4(sh)

            case _:
                return cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier("error"))))
    def _hoist_hydra_cpp_utils_extract_postfix_expression_6(v1):
        match v1:
            case hydra.cpp.syntax.EqualityExpressionRelational(value=re):
                return _hoist_hydra_cpp_utils_extract_postfix_expression_5(re)

            case _:
                return cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier("error"))))
    def _hoist_hydra_cpp_utils_extract_postfix_expression_7(v1):
        match v1:
            case hydra.cpp.syntax.AndExpressionEquality(value=eq):
                return _hoist_hydra_cpp_utils_extract_postfix_expression_6(eq)

            case _:
                return cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier("error"))))
    def _hoist_hydra_cpp_utils_extract_postfix_expression_8(v1):
        match v1:
            case hydra.cpp.syntax.ExclusiveOrExpressionAnd(value=ae):
                return _hoist_hydra_cpp_utils_extract_postfix_expression_7(ae)

            case _:
                return cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier("error"))))
    def _hoist_hydra_cpp_utils_extract_postfix_expression_9(v1):
        match v1:
            case hydra.cpp.syntax.InclusiveOrExpressionExclusiveOr(value=xo):
                return _hoist_hydra_cpp_utils_extract_postfix_expression_8(xo)

            case _:
                return cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier("error"))))
    def _hoist_hydra_cpp_utils_extract_postfix_expression_10(v1):
        match v1:
            case hydra.cpp.syntax.LogicalAndExpressionInclusiveOr(value=io):
                return _hoist_hydra_cpp_utils_extract_postfix_expression_9(io)

            case _:
                return cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier("error"))))
    def _hoist_hydra_cpp_utils_extract_postfix_expression_11(v1):
        match v1:
            case hydra.cpp.syntax.LogicalOrExpressionLogicalAnd(value=la):
                return _hoist_hydra_cpp_utils_extract_postfix_expression_10(la)

            case _:
                return cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier("error"))))
    def _hoist_hydra_cpp_utils_extract_postfix_expression_12(v1):
        match v1:
            case hydra.cpp.syntax.ConditionalExpressionLogicalOr(value=lo):
                return _hoist_hydra_cpp_utils_extract_postfix_expression_11(lo)

            case _:
                return cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier("error"))))
    def _hoist_hydra_cpp_utils_extract_postfix_expression_13(v1):
        match v1:
            case hydra.cpp.syntax.AssignmentExpressionConditional(value=c):
                return _hoist_hydra_cpp_utils_extract_postfix_expression_12(c)

            case _:
                return cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier("error"))))
    match expr:
        case hydra.cpp.syntax.ExpressionAssignment(value=a):
            return _hoist_hydra_cpp_utils_extract_postfix_expression_13(a)

        case _:
            return cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier("error"))))

def create_member_access_expr(obj_expr: hydra.cpp.syntax.Expression, member: str) -> hydra.cpp.syntax.Expression:
    r"""Create a member access expression."""

    return cpp_postfix_expression_to_cpp_expression(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionMemberAccess(hydra.cpp.syntax.MemberAccessOperation(extract_postfix_expression(obj_expr), member))))

def create_return_stmt(expr: hydra.cpp.syntax.Expression) -> hydra.cpp.syntax.Statement:
    r"""Create a return statement with a value."""

    return cast(hydra.cpp.syntax.Statement, hydra.cpp.syntax.StatementJump(cast(hydra.cpp.syntax.JumpStatement, hydra.cpp.syntax.JumpStatementReturnValue(expr))))

# Create a void return statement.
create_return_void_stmt = cast(hydra.cpp.syntax.Statement, hydra.cpp.syntax.StatementJump(cast(hydra.cpp.syntax.JumpStatement, hydra.cpp.syntax.JumpStatementReturnVoid())))

def create_template_type(name: str, args: frozenlist[hydra.cpp.syntax.TypeExpression]) -> hydra.cpp.syntax.TypeExpression:
    r"""Create a template type."""

    return cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionTemplate(hydra.cpp.syntax.TemplateType(name, hydra.lib.lists.map((lambda a: cast(hydra.cpp.syntax.TemplateArgument, hydra.cpp.syntax.TemplateArgumentType(a))), args))))

@lru_cache(1)
def create_this_expr() -> hydra.cpp.syntax.Expression:
    r"""Create a *this expression."""

    return cpp_postfix_expression_to_cpp_expression(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier("*this")))))

def create_throw_stmt(exception_type: str, arg: hydra.cpp.syntax.Expression) -> hydra.cpp.syntax.Statement:
    r"""Create a throw statement."""

    return cast(hydra.cpp.syntax.Statement, hydra.cpp.syntax.StatementJump(cast(hydra.cpp.syntax.JumpStatement, hydra.cpp.syntax.JumpStatementThrow(create_function_call_expr(exception_type, (arg,))))))

@lru_cache(1)
def create_type_id_name_call() -> hydra.cpp.syntax.Expression:
    r"""Create a typeid(...).name() call expression."""

    return cpp_postfix_expression_to_cpp_expression(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionFunctionCall(hydra.cpp.syntax.FunctionCallOperation(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionMemberAccess(hydra.cpp.syntax.MemberAccessOperation(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionFunctionCall(hydra.cpp.syntax.FunctionCallOperation(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier("typeid")))), (cpp_primary_expression_to_cpp_expression(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionParenthesized(create_this_expr()))),)))), "name"))), ()))))

def create_type_name_expr(type_name: str) -> hydra.cpp.syntax.Expression:
    r"""Create a type name expression."""

    return cpp_postfix_expression_to_cpp_expression(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier(type_name)))))

@lru_cache(1)
def create_variant_expr() -> hydra.cpp.syntax.Expression:
    r"""Create a variant expression."""

    return cpp_postfix_expression_to_cpp_expression(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier("variant")))))

# Protected access specifier member specification.
member_specification_protected = cast(hydra.cpp.syntax.MemberSpecification, hydra.cpp.syntax.MemberSpecificationAccessLabel(hydra.cpp.syntax.AccessSpecifier.PROTECTED))

# Public access specifier member specification.
member_specification_public = cast(hydra.cpp.syntax.MemberSpecification, hydra.cpp.syntax.MemberSpecificationAccessLabel(hydra.cpp.syntax.AccessSpecifier.PUBLIC))

def string_expression(s: str) -> hydra.cpp.syntax.Expression:
    r"""Create a string expression."""

    return cpp_primary_expression_to_cpp_expression(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionLiteral(cast(hydra.cpp.syntax.Literal, hydra.cpp.syntax.LiteralString(hydra.cpp.syntax.StringLiteral(s))))))

def to_const_type(base_type: hydra.cpp.syntax.TypeExpression) -> hydra.cpp.syntax.TypeExpression:
    r"""Add const qualifier to a type."""

    return cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionQualified(hydra.cpp.syntax.QualifiedType(base_type, hydra.cpp.syntax.TypeQualifier.CONST)))

def unnamed_parameter(name: str, typ: hydra.cpp.syntax.TypeExpression) -> hydra.cpp.syntax.Parameter:
    r"""Create an unnamed parameter."""

    return hydra.cpp.syntax.Parameter(typ, name, True, Nothing())
