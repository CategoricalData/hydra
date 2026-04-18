# Note: this is an automatically generated file. Do not edit.

r"""C++ code generator: converts Hydra modules to C++ header files."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.cpp.language
import hydra.cpp.serde
import hydra.cpp.syntax
import hydra.dependencies
import hydra.environment
import hydra.errors
import hydra.formatting
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.names
import hydra.packaging
import hydra.predicates
import hydra.resolution
import hydra.serialization
import hydra.show.core
import hydra.strip
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")

def binding_name_to_file_path(name: hydra.core.Name) -> str:
    return hydra.names.name_to_file_path(hydra.util.CaseConvention.LOWER_SNAKE, hydra.util.CaseConvention.LOWER_SNAKE, hydra.packaging.FileExtension("h"), name)

def sanitize_cpp_name(name: str) -> str:
    return hydra.formatting.sanitize_with_underscores(hydra.cpp.language.cpp_reserved_words(), name)

def class_name(name: hydra.core.Name) -> str:
    return sanitize_cpp_name(hydra.names.local_name_of(name))

def const_parameter(name: str, typ: hydra.cpp.syntax.TypeExpression) -> hydra.cpp.syntax.Parameter:
    return hydra.cpp.syntax.Parameter(cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionQualified(hydra.cpp.syntax.QualifiedType(cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionQualified(hydra.cpp.syntax.QualifiedType(typ, hydra.cpp.syntax.TypeQualifier.CONST))), hydra.cpp.syntax.TypeQualifier.LVALUE_REF))), name, False, Nothing())

def cpp_class_declaration(name: str, base_specs: frozenlist[hydra.cpp.syntax.BaseSpecifier], mbody: Maybe[hydra.cpp.syntax.ClassBody]) -> hydra.cpp.syntax.Declaration:
    return cast(hydra.cpp.syntax.Declaration, hydra.cpp.syntax.DeclarationClass(hydra.cpp.syntax.ClassDeclaration(hydra.cpp.syntax.ClassSpecifier(hydra.cpp.syntax.ClassKey.CLASS, name, base_specs), mbody)))

def cpp_enum_declaration(name: str, mbody: Maybe[hydra.cpp.syntax.ClassBody]) -> hydra.cpp.syntax.Declaration:
    return cast(hydra.cpp.syntax.Declaration, hydra.cpp.syntax.DeclarationClass(hydra.cpp.syntax.ClassDeclaration(hydra.cpp.syntax.ClassSpecifier(hydra.cpp.syntax.ClassKey.ENUM_CLASS, name, ()), mbody)))

def cpp_enum_forward_declaration(name: str) -> hydra.cpp.syntax.Declaration:
    return cpp_enum_declaration(name, Nothing())

def cpp_unary_expression_to_cpp_logical_or_expression(ue: hydra.cpp.syntax.UnaryExpression) -> hydra.cpp.syntax.LogicalOrExpression:
    return cast(hydra.cpp.syntax.LogicalOrExpression, hydra.cpp.syntax.LogicalOrExpressionLogicalAnd(cast(hydra.cpp.syntax.LogicalAndExpression, hydra.cpp.syntax.LogicalAndExpressionInclusiveOr(cast(hydra.cpp.syntax.InclusiveOrExpression, hydra.cpp.syntax.InclusiveOrExpressionExclusiveOr(cast(hydra.cpp.syntax.ExclusiveOrExpression, hydra.cpp.syntax.ExclusiveOrExpressionAnd(cast(hydra.cpp.syntax.AndExpression, hydra.cpp.syntax.AndExpressionEquality(cast(hydra.cpp.syntax.EqualityExpression, hydra.cpp.syntax.EqualityExpressionRelational(cast(hydra.cpp.syntax.RelationalExpression, hydra.cpp.syntax.RelationalExpressionShift(cast(hydra.cpp.syntax.ShiftExpression, hydra.cpp.syntax.ShiftExpressionAdditive(cast(hydra.cpp.syntax.AdditiveExpression, hydra.cpp.syntax.AdditiveExpressionMultiplicative(cast(hydra.cpp.syntax.MultiplicativeExpression, hydra.cpp.syntax.MultiplicativeExpressionUnary(ue))))))))))))))))))))

def cpp_unary_expression_to_cpp_expression(ue: hydra.cpp.syntax.UnaryExpression) -> hydra.cpp.syntax.Expression:
    return cast(hydra.cpp.syntax.Expression, hydra.cpp.syntax.ExpressionAssignment(cast(hydra.cpp.syntax.AssignmentExpression, hydra.cpp.syntax.AssignmentExpressionConditional(cast(hydra.cpp.syntax.ConditionalExpression, hydra.cpp.syntax.ConditionalExpressionLogicalOr(cpp_unary_expression_to_cpp_logical_or_expression(ue)))))))

def cpp_postfix_expression_to_cpp_expression(pe: hydra.cpp.syntax.PostfixExpression) -> hydra.cpp.syntax.Expression:
    return cpp_unary_expression_to_cpp_expression(cast(hydra.cpp.syntax.UnaryExpression, hydra.cpp.syntax.UnaryExpressionPostfix(pe)))

def cpp_primary_expression_to_cpp_expression(prim: hydra.cpp.syntax.PrimaryExpression) -> hydra.cpp.syntax.Expression:
    return cpp_postfix_expression_to_cpp_expression(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(prim)))

def create_function_call_expr(func_name: str, args: frozenlist[hydra.cpp.syntax.Expression]) -> hydra.cpp.syntax.Expression:
    return cpp_postfix_expression_to_cpp_expression(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionFunctionCall(hydra.cpp.syntax.FunctionCallOperation(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier(func_name)))), args))))

def create_throw_stmt(exception_type: str, arg: hydra.cpp.syntax.Expression) -> hydra.cpp.syntax.Statement:
    return cast(hydra.cpp.syntax.Statement, hydra.cpp.syntax.StatementJump(cast(hydra.cpp.syntax.JumpStatement, hydra.cpp.syntax.JumpStatementThrow(create_function_call_expr(exception_type, (arg,))))))

@lru_cache(1)
def create_type_id_name_call() -> hydra.cpp.syntax.Expression:
    return cpp_postfix_expression_to_cpp_expression(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionFunctionCall(hydra.cpp.syntax.FunctionCallOperation(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionMemberAccess(hydra.cpp.syntax.MemberAccessOperation(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionFunctionCall(hydra.cpp.syntax.FunctionCallOperation(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier("typeid")))), (cpp_primary_expression_to_cpp_expression(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionParenthesized(cpp_postfix_expression_to_cpp_expression(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier("*this")))))))),)))), "name"))), ()))))

def variant_name(tname: hydra.core.Name, fname: hydra.core.Name) -> str:
    return sanitize_cpp_name(hydra.lib.strings.cat2(hydra.names.local_name_of(tname), hydra.formatting.capitalize(fname.value)))

def visitor_name(name: hydra.core.Name) -> str:
    return sanitize_cpp_name(hydra.lib.strings.cat2(hydra.names.local_name_of(name), "Visitor"))

def create_accept_implementation(tname: hydra.core.Name, variants: frozenlist[hydra.core.FieldType]) -> hydra.cpp.syntax.Declaration:
    return cast(hydra.cpp.syntax.Declaration, hydra.cpp.syntax.DeclarationTemplate(hydra.cpp.syntax.TemplateDeclaration(False, ("typename R",), cast(hydra.cpp.syntax.Declaration, hydra.cpp.syntax.DeclarationFunction(hydra.cpp.syntax.FunctionDeclaration((), cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionBasic(cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeNamed("R")))), hydra.lib.strings.cat2(class_name(tname), "::accept"), (hydra.cpp.syntax.Parameter(cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionQualified(hydra.cpp.syntax.QualifiedType(cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionBasic(cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeNamed(hydra.lib.strings.cat2(visitor_name(tname), "<R>"))))), hydra.cpp.syntax.TypeQualifier.LVALUE_REF))), "visitor", False, Nothing()),), (hydra.cpp.syntax.FunctionSpecifierSuffix.CONST,), cast(hydra.cpp.syntax.FunctionBody, hydra.cpp.syntax.FunctionBodyCompound(hydra.cpp.syntax.CompoundStatement(hydra.lib.lists.map((lambda ft: (fname := ft.name, cast(hydra.cpp.syntax.Statement, hydra.cpp.syntax.StatementSelection(hydra.cpp.syntax.SelectionStatement(cast(hydra.cpp.syntax.Expression, hydra.cpp.syntax.ExpressionAssignment(cast(hydra.cpp.syntax.AssignmentExpression, hydra.cpp.syntax.AssignmentExpressionAssignment(hydra.cpp.syntax.ExplicitAssignment(cpp_unary_expression_to_cpp_logical_or_expression(cast(hydra.cpp.syntax.UnaryExpression, hydra.cpp.syntax.UnaryExpressionPostfix(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier("auto ptr"))))))), hydra.cpp.syntax.AssignmentOperator.ASSIGN, cast(hydra.cpp.syntax.AssignmentExpression, hydra.cpp.syntax.AssignmentExpressionConditional(cast(hydra.cpp.syntax.ConditionalExpression, hydra.cpp.syntax.ConditionalExpressionLogicalOr(cpp_unary_expression_to_cpp_logical_or_expression(cast(hydra.cpp.syntax.UnaryExpression, hydra.cpp.syntax.UnaryExpressionPostfix(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionFunctionCall(hydra.cpp.syntax.FunctionCallOperation(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier(hydra.lib.strings.cat2(hydra.lib.strings.cat2("dynamic_cast<const ", variant_name(tname, fname)), "*>"))))), (cpp_primary_expression_to_cpp_expression(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier("this"))),)))))))))))))))), cast(hydra.cpp.syntax.Statement, hydra.cpp.syntax.StatementCompound(hydra.cpp.syntax.CompoundStatement((cast(hydra.cpp.syntax.Statement, hydra.cpp.syntax.StatementJump(cast(hydra.cpp.syntax.JumpStatement, hydra.cpp.syntax.JumpStatementReturnValue(cpp_postfix_expression_to_cpp_expression(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionFunctionCall(hydra.cpp.syntax.FunctionCallOperation(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionMemberAccess(hydra.cpp.syntax.MemberAccessOperation(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier("visitor")))), "visit"))), (cpp_unary_expression_to_cpp_expression(cast(hydra.cpp.syntax.UnaryExpression, hydra.cpp.syntax.UnaryExpressionUnaryOp(hydra.cpp.syntax.UnaryOperation(hydra.cpp.syntax.UnaryOperator.DEREFERENCE, cast(hydra.cpp.syntax.UnaryExpression, hydra.cpp.syntax.UnaryExpressionPostfix(cast(hydra.cpp.syntax.PostfixExpression, hydra.cpp.syntax.PostfixExpressionPrimary(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier("ptr")))))))))),))))))))),)))), Just(create_throw_stmt("std::runtime_error", create_type_id_name_call()))))))[1]), variants))))))))))

def create_constructor_body(params: frozenlist[T0]) -> hydra.cpp.syntax.FunctionBody:
    return hydra.lib.logic.if_else(hydra.lib.lists.null(params), (lambda : cast(hydra.cpp.syntax.FunctionBody, hydra.cpp.syntax.FunctionBodyDefault())), (lambda : cast(hydra.cpp.syntax.FunctionBody, hydra.cpp.syntax.FunctionBodyCompound(hydra.cpp.syntax.CompoundStatement(())))))

def create_header_file(includes: frozenlist[hydra.cpp.syntax.IncludeDirective], decls: frozenlist[hydra.cpp.syntax.Declaration]) -> hydra.cpp.syntax.Program:
    return hydra.cpp.syntax.Program((cast(hydra.cpp.syntax.PreprocessorDirective, hydra.cpp.syntax.PreprocessorDirectivePragma(hydra.cpp.syntax.PragmaDirective("once"))),), includes, decls)

def create_identifier_expr(name: str) -> hydra.cpp.syntax.Expression:
    return cpp_primary_expression_to_cpp_expression(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionIdentifier(name)))

def create_literal_bool_expr(val: bool) -> hydra.cpp.syntax.Expression:
    return cpp_primary_expression_to_cpp_expression(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionLiteral(cast(hydra.cpp.syntax.Literal, hydra.cpp.syntax.LiteralBoolean(hydra.cpp.syntax.BooleanLiteral(val))))))

def unnamed_parameter(name: str, typ: hydra.cpp.syntax.TypeExpression) -> hydra.cpp.syntax.Parameter:
    return hydra.cpp.syntax.Parameter(typ, name, True, Nothing())

def create_less_than_operator(type_name: hydra.core.Name, fields: T0) -> hydra.cpp.syntax.Declaration:
    return cast(hydra.cpp.syntax.Declaration, hydra.cpp.syntax.DeclarationFunction(hydra.cpp.syntax.FunctionDeclaration((hydra.cpp.syntax.FunctionSpecifierPrefix.INLINE,), cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionBasic(cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeBool()))), "operator<", (unnamed_parameter("lhs", cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionBasic(cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeNamed(class_name(type_name)))))), unnamed_parameter("rhs", cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionBasic(cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeNamed(class_name(type_name))))))), (), cast(hydra.cpp.syntax.FunctionBody, hydra.cpp.syntax.FunctionBodyCompound(hydra.cpp.syntax.CompoundStatement((cast(hydra.cpp.syntax.Statement, hydra.cpp.syntax.StatementJump(cast(hydra.cpp.syntax.JumpStatement, hydra.cpp.syntax.JumpStatementReturnValue(create_literal_bool_expr(False))))),)))))))

def create_literal_int_expr(val: int) -> hydra.cpp.syntax.Expression:
    return cpp_primary_expression_to_cpp_expression(cast(hydra.cpp.syntax.PrimaryExpression, hydra.cpp.syntax.PrimaryExpressionLiteral(cast(hydra.cpp.syntax.Literal, hydra.cpp.syntax.LiteralInteger(cast(hydra.cpp.syntax.IntegerLiteral, hydra.cpp.syntax.IntegerLiteralDecimal(val)))))))

member_specification_public = cast(hydra.cpp.syntax.MemberSpecification, hydra.cpp.syntax.MemberSpecificationAccessLabel(hydra.cpp.syntax.AccessSpecifier.PUBLIC))

def partial_visitor_name(name: hydra.core.Name) -> str:
    return sanitize_cpp_name(hydra.lib.strings.cat2(hydra.names.local_name_of(name), "PartialVisitor"))

def create_partial_visitor_interface(tname: hydra.core.Name, variants: frozenlist[hydra.core.FieldType]) -> hydra.cpp.syntax.Declaration:
    return cast(hydra.cpp.syntax.Declaration, hydra.cpp.syntax.DeclarationTemplate(hydra.cpp.syntax.TemplateDeclaration(False, ("typename R",), cpp_class_declaration(partial_visitor_name(tname), (hydra.cpp.syntax.BaseSpecifier(hydra.cpp.syntax.AccessSpecifier.PUBLIC, hydra.lib.strings.cat2(visitor_name(tname), "<R>")),), Just(hydra.cpp.syntax.ClassBody(hydra.lib.lists.concat(((member_specification_public,), (cast(hydra.cpp.syntax.MemberSpecification, hydra.cpp.syntax.MemberSpecificationMember(cast(hydra.cpp.syntax.MemberDeclaration, hydra.cpp.syntax.MemberDeclarationFunction(hydra.cpp.syntax.FunctionDeclaration((hydra.cpp.syntax.FunctionSpecifierPrefix.VIRTUAL,), cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionBasic(cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeNamed("R")))), "otherwise", (const_parameter("value", cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionBasic(cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeNamed(class_name(tname)))))),), (hydra.cpp.syntax.FunctionSpecifierSuffix.CONST,), cast(hydra.cpp.syntax.FunctionBody, hydra.cpp.syntax.FunctionBodyPure())))))),), hydra.lib.lists.map((lambda ft: (fname := ft.name, cast(hydra.cpp.syntax.MemberSpecification, hydra.cpp.syntax.MemberSpecificationMember(cast(hydra.cpp.syntax.MemberDeclaration, hydra.cpp.syntax.MemberDeclarationFunction(hydra.cpp.syntax.FunctionDeclaration((), cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionBasic(cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeNamed("R")))), "visit", (const_parameter("value", cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionBasic(cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeNamed(variant_name(tname, fname)))))),), (hydra.cpp.syntax.FunctionSpecifierSuffix.OVERRIDE,), cast(hydra.cpp.syntax.FunctionBody, hydra.cpp.syntax.FunctionBodyCompound(hydra.cpp.syntax.CompoundStatement((cast(hydra.cpp.syntax.Statement, hydra.cpp.syntax.StatementJump(cast(hydra.cpp.syntax.JumpStatement, hydra.cpp.syntax.JumpStatementReturnValue(create_function_call_expr("otherwise", (create_identifier_expr("value"),)))))),))))))))))[1]), variants)))))))))

def create_template_type(name: str, args: frozenlist[hydra.cpp.syntax.TypeExpression]) -> hydra.cpp.syntax.TypeExpression:
    return cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionTemplate(hydra.cpp.syntax.TemplateType(name, hydra.lib.lists.map((lambda a: cast(hydra.cpp.syntax.TemplateArgument, hydra.cpp.syntax.TemplateArgumentType(a))), args))))

def to_const_type(base_type: hydra.cpp.syntax.TypeExpression) -> hydra.cpp.syntax.TypeExpression:
    return cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionQualified(hydra.cpp.syntax.QualifiedType(base_type, hydra.cpp.syntax.TypeQualifier.CONST)))

def create_type_reference(is_pointer: bool, name: hydra.core.Name) -> hydra.cpp.syntax.TypeExpression:
    return hydra.lib.logic.if_else(is_pointer, (lambda : create_template_type("std::shared_ptr", (to_const_type(cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionBasic(cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeNamed(sanitize_cpp_name(hydra.names.local_name_of(name))))))),))), (lambda : cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionBasic(cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeNamed(sanitize_cpp_name(hydra.names.local_name_of(name))))))))

member_specification_protected = cast(hydra.cpp.syntax.MemberSpecification, hydra.cpp.syntax.MemberSpecificationAccessLabel(hydra.cpp.syntax.AccessSpecifier.PROTECTED))

def create_union_base_class(name: hydra.core.Name, variants: T0) -> hydra.cpp.syntax.Declaration:
    return cpp_class_declaration(class_name(name), (), Just(hydra.cpp.syntax.ClassBody((member_specification_protected, cast(hydra.cpp.syntax.MemberSpecification, hydra.cpp.syntax.MemberSpecificationMember(cast(hydra.cpp.syntax.MemberDeclaration, hydra.cpp.syntax.MemberDeclarationConstructor(hydra.cpp.syntax.ConstructorDeclaration(class_name(name), (), (), cast(hydra.cpp.syntax.FunctionBody, hydra.cpp.syntax.FunctionBodyDefault())))))), member_specification_public, cast(hydra.cpp.syntax.MemberSpecification, hydra.cpp.syntax.MemberSpecificationMember(cast(hydra.cpp.syntax.MemberDeclaration, hydra.cpp.syntax.MemberDeclarationDestructor(hydra.cpp.syntax.DestructorDeclaration((hydra.cpp.syntax.FunctionSpecifierPrefix.VIRTUAL,), class_name(name), (), cast(hydra.cpp.syntax.FunctionBody, hydra.cpp.syntax.FunctionBodyDefault())))))), cast(hydra.cpp.syntax.MemberSpecification, hydra.cpp.syntax.MemberSpecificationMember(cast(hydra.cpp.syntax.MemberDeclaration, hydra.cpp.syntax.MemberDeclarationTemplate(hydra.cpp.syntax.TemplateDeclaration(False, ("typename R",), cast(hydra.cpp.syntax.Declaration, hydra.cpp.syntax.DeclarationFunction(hydra.cpp.syntax.FunctionDeclaration((), cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionBasic(cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeNamed("R")))), "accept", (hydra.cpp.syntax.Parameter(cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionQualified(hydra.cpp.syntax.QualifiedType(cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionBasic(cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeNamed(hydra.lib.strings.cat2(visitor_name(name), "<R>"))))), hydra.cpp.syntax.TypeQualifier.LVALUE_REF))), "visitor", False, Nothing()),), (hydra.cpp.syntax.FunctionSpecifierSuffix.CONST,), cast(hydra.cpp.syntax.FunctionBody, hydra.cpp.syntax.FunctionBodyDeclaration())))))))))))))

def encode_literal_type(lt: hydra.core.LiteralType) -> hydra.cpp.syntax.TypeExpression:
    def _hoist_hydra_cpp_coder_encode_literal_type_1(v1):
        match v1:
            case hydra.core.FloatType.FLOAT32:
                return cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeFloat())

            case hydra.core.FloatType.FLOAT64:
                return cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeDouble())

            case _:
                return cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeDouble())
    def _hoist_hydra_cpp_coder_encode_literal_type_2(v1):
        match v1:
            case hydra.core.IntegerType.BIGINT:
                return cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeInt())

            case hydra.core.IntegerType.INT8:
                return cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeChar())

            case hydra.core.IntegerType.INT16:
                return cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeNamed("int16_t"))

            case hydra.core.IntegerType.INT32:
                return cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeInt())

            case hydra.core.IntegerType.INT64:
                return cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeNamed("int64_t"))

            case _:
                return cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeInt())
    def _hoist_hydra_cpp_coder_encode_literal_type_3(v1):
        match v1:
            case hydra.core.LiteralTypeBinary():
                return cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeChar())

            case hydra.core.LiteralTypeBoolean():
                return cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeBool())

            case hydra.core.LiteralTypeFloat(value=ft):
                return _hoist_hydra_cpp_coder_encode_literal_type_1(ft)

            case hydra.core.LiteralTypeInteger(value=it):
                return _hoist_hydra_cpp_coder_encode_literal_type_2(it)

            case hydra.core.LiteralTypeString():
                return cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeString())

            case _:
                raise TypeError("Unsupported LiteralType")
    return cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionBasic(_hoist_hydra_cpp_coder_encode_literal_type_3(lt)))

def encode_application_type(cx: T0, g: T1, at: hydra.core.ApplicationType) -> Either[hydra.errors.Error, hydra.cpp.syntax.TypeExpression]:
    return hydra.lib.eithers.bind(encode_type(cx, g, at.function), (lambda body: hydra.lib.eithers.bind(encode_type(cx, g, at.argument), (lambda arg: Right(create_template_type("TODO_template", (body, arg)))))))

def encode_forall_type(cx: T0, g: T1, lt: hydra.core.ForallType) -> Either[hydra.errors.Error, hydra.cpp.syntax.TypeExpression]:
    return encode_type(cx, g, lt.body)

def encode_function_type(cx: T0, g: T1, ft: hydra.core.FunctionType) -> Either[hydra.errors.Error, hydra.cpp.syntax.TypeExpression]:
    return hydra.lib.eithers.bind(encode_type(cx, g, ft.domain), (lambda dom: hydra.lib.eithers.bind(encode_type(cx, g, ft.codomain), (lambda cod: Right(cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionFunction(hydra.cpp.syntax.FunctionType(cod, (hydra.cpp.syntax.Parameter(dom, "", False, Nothing()),)))))))))

def encode_type(cx: T0, g: T1, typ: hydra.core.Type) -> Either[hydra.errors.Error, hydra.cpp.syntax.TypeExpression]:
    @lru_cache(1)
    def t() -> hydra.core.Type:
        return hydra.strip.deannotate_type(typ)
    match t():
        case hydra.core.TypeApplication(value=at):
            return encode_application_type(cx, g, at)

        case hydra.core.TypeEither(value=et):
            return hydra.lib.eithers.bind(encode_type(cx, g, et.left), (lambda lt: hydra.lib.eithers.bind(encode_type(cx, g, et.right), (lambda rt: Right(to_const_type(create_template_type("std::variant", (lt, rt))))))))

        case hydra.core.TypeFunction(value=ft):
            return encode_function_type(cx, g, ft)

        case hydra.core.TypeForall(value=lt):
            return encode_forall_type(cx, g, lt)

        case hydra.core.TypeList(value=et2):
            return hydra.lib.eithers.map((lambda enc: to_const_type(create_template_type("std::vector", (enc,)))), encode_type(cx, g, et2))

        case hydra.core.TypeMap(value=mt):
            return hydra.lib.eithers.bind(encode_type(cx, g, mt.keys), (lambda kt: hydra.lib.eithers.bind(encode_type(cx, g, mt.values), (lambda vt: Right(to_const_type(create_template_type("std::map", (kt, vt))))))))

        case hydra.core.TypeLiteral(value=lt2):
            return Right(encode_literal_type(lt2))

        case hydra.core.TypeMaybe(value=et3):
            return hydra.lib.eithers.map((lambda enc: to_const_type(create_template_type("std::optional", (enc,)))), encode_type(cx, g, et3))

        case hydra.core.TypePair(value=pt):
            return hydra.lib.eithers.bind(encode_type(cx, g, pt.first), (lambda ft: hydra.lib.eithers.bind(encode_type(cx, g, pt.second), (lambda st: Right(to_const_type(create_template_type("std::pair", (ft, st))))))))

        case hydra.core.TypeRecord():
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unexpected anonymous record type"))))

        case hydra.core.TypeSet(value=et4):
            return hydra.lib.eithers.map((lambda enc: to_const_type(create_template_type("std::set", (enc,)))), encode_type(cx, g, et4))

        case hydra.core.TypeUnion():
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unexpected anonymous union type"))))

        case hydra.core.TypeVariable(value=name):
            return Right(cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionBasic(cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeNamed(sanitize_cpp_name(name.value))))))

        case hydra.core.TypeWrap():
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unexpected anonymous wrapped type"))))

        case hydra.core.TypeUnit():
            return Right(create_template_type("std::tuple", ()))

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("Unsupported type"))))

def create_variant_class(cx: T0, g: T1, tname: hydra.core.Name, parent_class: hydra.core.Name, ft: hydra.core.FieldType) -> Either[hydra.errors.Error, hydra.cpp.syntax.Declaration]:
    fname = ft.name
    variant_type = ft.type
    @lru_cache(1)
    def has_value() -> bool:
        return hydra.lib.logic.not_(hydra.predicates.is_unit_type(variant_type))
    @lru_cache(1)
    def value_field() -> Either[hydra.errors.Error, frozenlist[hydra.cpp.syntax.MemberSpecification]]:
        return hydra.lib.logic.if_else(has_value(), (lambda : hydra.lib.eithers.map((lambda cpp_type: (cast(hydra.cpp.syntax.MemberSpecification, hydra.cpp.syntax.MemberSpecificationMember(cast(hydra.cpp.syntax.MemberDeclaration, hydra.cpp.syntax.MemberDeclarationVariable(hydra.cpp.syntax.VariableDeclaration(Just(cpp_type), "value", Nothing(), False))))),)), encode_type(cx, g, hydra.strip.deannotate_type(variant_type)))), (lambda : Right(())))
    @lru_cache(1)
    def constructor_params() -> Either[hydra.errors.Error, frozenlist[hydra.cpp.syntax.Parameter]]:
        return hydra.lib.logic.if_else(has_value(), (lambda : hydra.lib.eithers.map((lambda param_type: (hydra.cpp.syntax.Parameter(param_type, "value", False, Nothing()),)), encode_type(cx, g, hydra.strip.deannotate_type(variant_type)))), (lambda : Right(())))
    return hydra.lib.eithers.bind(value_field(), (lambda v_fields: hydra.lib.eithers.bind(constructor_params(), (lambda v_params: (init_list := hydra.lib.logic.if_else(has_value(), (lambda : (hydra.cpp.syntax.MemInitializer("value", (create_identifier_expr("value"),)),)), (lambda : ())), Right(cpp_class_declaration(variant_name(tname, fname), (hydra.cpp.syntax.BaseSpecifier(hydra.cpp.syntax.AccessSpecifier.PUBLIC, class_name(parent_class)),), Just(hydra.cpp.syntax.ClassBody(hydra.lib.lists.concat(((member_specification_public,), v_fields, (cast(hydra.cpp.syntax.MemberSpecification, hydra.cpp.syntax.MemberSpecificationMember(cast(hydra.cpp.syntax.MemberDeclaration, hydra.cpp.syntax.MemberDeclarationConstructor(hydra.cpp.syntax.ConstructorDeclaration(variant_name(tname, fname), v_params, init_list, create_constructor_body(v_params)))))),))))))))[1]))))

def create_visitor_interface(tname: hydra.core.Name, variants: frozenlist[hydra.core.FieldType]) -> hydra.cpp.syntax.Declaration:
    return cast(hydra.cpp.syntax.Declaration, hydra.cpp.syntax.DeclarationTemplate(hydra.cpp.syntax.TemplateDeclaration(False, ("typename R",), cpp_class_declaration(visitor_name(tname), (), Just(hydra.cpp.syntax.ClassBody(hydra.lib.lists.concat(((member_specification_public,), hydra.lib.lists.map((lambda ft: (fname := ft.name, cast(hydra.cpp.syntax.MemberSpecification, hydra.cpp.syntax.MemberSpecificationMember(cast(hydra.cpp.syntax.MemberDeclaration, hydra.cpp.syntax.MemberDeclarationFunction(hydra.cpp.syntax.FunctionDeclaration((hydra.cpp.syntax.FunctionSpecifierPrefix.VIRTUAL,), cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionBasic(cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeNamed("R")))), "visit", (hydra.cpp.syntax.Parameter(cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionQualified(hydra.cpp.syntax.QualifiedType(cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionQualified(hydra.cpp.syntax.QualifiedType(cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionBasic(cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeNamed(variant_name(tname, fname))))), hydra.cpp.syntax.TypeQualifier.CONST))), hydra.cpp.syntax.TypeQualifier.LVALUE_REF))), "value", False, Nothing()),), (), cast(hydra.cpp.syntax.FunctionBody, hydra.cpp.syntax.FunctionBodyPure())))))))[1]), variants), (cast(hydra.cpp.syntax.MemberSpecification, hydra.cpp.syntax.MemberSpecificationMember(cast(hydra.cpp.syntax.MemberDeclaration, hydra.cpp.syntax.MemberDeclarationDestructor(hydra.cpp.syntax.DestructorDeclaration((hydra.cpp.syntax.FunctionSpecifierPrefix.VIRTUAL,), visitor_name(tname), (), cast(hydra.cpp.syntax.FunctionBody, hydra.cpp.syntax.FunctionBodyDefault())))))),)))))))))

def encode_enum_value(fname: hydra.core.Name) -> str:
    return sanitize_cpp_name(hydra.formatting.convert_case_camel_to_upper_snake(fname.value))

def encode_enum_type(cx: T0, g: T1, name: hydra.core.Name, tfields: frozenlist[hydra.core.FieldType], comment: T2) -> Either[T3, frozenlist[hydra.cpp.syntax.Declaration]]:
    return Right((cpp_enum_declaration(class_name(name), Just(hydra.cpp.syntax.ClassBody(hydra.lib.lists.map((lambda ft: cast(hydra.cpp.syntax.MemberSpecification, hydra.cpp.syntax.MemberSpecificationMember(cast(hydra.cpp.syntax.MemberDeclaration, hydra.cpp.syntax.MemberDeclarationVariable(hydra.cpp.syntax.VariableDeclaration(Nothing(), encode_enum_value(ft.name), Nothing(), False)))))), tfields)))),))

def encode_field_name(fname: hydra.core.Name) -> str:
    return sanitize_cpp_name(hydra.formatting.convert_case_camel_to_lower_snake(fname.value))

def encode_field_type(is_parameter: T0, ft: hydra.core.FieldType, cx: T1, g: T2) -> Either[hydra.errors.Error, hydra.cpp.syntax.VariableDeclaration]:
    fname = ft.name
    ftype = ft.type
    return hydra.lib.eithers.bind(encode_type(cx, g, ftype), (lambda cpp_type: Right(hydra.cpp.syntax.VariableDeclaration(Just(cpp_type), encode_field_name(fname), Nothing(), False))))

def encode_name(is_qualified: T0, conv: T1, env: T2, name: hydra.core.Name) -> str:
    r"""Encode a name with a specified case convention, optionally qualified."""

    return sanitize_cpp_name(hydra.names.local_name_of(name))

def encode_namespace(ns: hydra.packaging.Namespace) -> str:
    return hydra.lib.strings.intercalate("::", hydra.lib.lists.map((lambda seg: hydra.formatting.convert_case_camel_to_lower_snake(seg)), hydra.lib.strings.split_on(".", ns.value)))

def encode_record_type(cx: T0, g: T1, name: hydra.core.Name, rt: frozenlist[hydra.core.FieldType], comment: T2) -> Either[hydra.errors.Error, frozenlist[hydra.cpp.syntax.Declaration]]:
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda f: encode_field_type(False, f, cx, g)), rt), (lambda cpp_fields: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda f: encode_field_type(True, f, cx, g)), rt), (lambda constructor_params: Right((cpp_class_declaration(class_name(name), (), Just(hydra.cpp.syntax.ClassBody(hydra.lib.lists.concat(((member_specification_public,), hydra.lib.lists.map((lambda field: cast(hydra.cpp.syntax.MemberSpecification, hydra.cpp.syntax.MemberSpecificationMember(cast(hydra.cpp.syntax.MemberDeclaration, hydra.cpp.syntax.MemberDeclarationVariable(field))))), cpp_fields), (cast(hydra.cpp.syntax.MemberSpecification, hydra.cpp.syntax.MemberSpecificationMember(cast(hydra.cpp.syntax.MemberDeclaration, hydra.cpp.syntax.MemberDeclarationConstructor(hydra.cpp.syntax.ConstructorDeclaration(class_name(name), hydra.lib.lists.map((lambda p: hydra.cpp.syntax.Parameter(hydra.lib.maybes.from_maybe((lambda : cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionBasic(cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeInt())))), p.type), p.name, False, Nothing())), constructor_params), hydra.lib.lists.map((lambda field: hydra.cpp.syntax.MemInitializer(field.name, (create_identifier_expr(field.name),))), cpp_fields), create_constructor_body(constructor_params)))))),)))))), create_less_than_operator(name, rt)))))))

def encode_type_alias(cx: T0, g: T1, name: hydra.core.Name, typ: hydra.core.Type, comment: T2) -> Either[hydra.errors.Error, hydra.cpp.syntax.Declaration]:
    return hydra.lib.eithers.bind(encode_type(cx, g, typ), (lambda cpp_type: Right(cast(hydra.cpp.syntax.Declaration, hydra.cpp.syntax.DeclarationTypedef(hydra.cpp.syntax.TypedefDeclaration(class_name(name), cpp_type, True))))))

def generate_forward_declarations(tname: hydra.core.Name, fields: frozenlist[hydra.core.FieldType]) -> frozenlist[hydra.cpp.syntax.Declaration]:
    return hydra.lib.lists.map((lambda ft: cpp_class_declaration(variant_name(tname, ft.name), (), Nothing())), fields)

def encode_variant_type(cx: T0, g: T1, name: hydra.core.Name, variants: frozenlist[hydra.core.FieldType], comment: T2) -> Either[hydra.errors.Error, frozenlist[hydra.cpp.syntax.Declaration]]:
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v: create_variant_class(cx, g, name, name, v)), variants), (lambda variant_classes: Right(hydra.lib.lists.concat((generate_forward_declarations(name, variants), (create_visitor_interface(name, variants),), (create_union_base_class(name, variants),), variant_classes, (create_partial_visitor_interface(name, variants),), (create_accept_implementation(name, variants),))))))

def encode_union_type(cx: T0, g: T1, name: hydra.core.Name, rt: frozenlist[hydra.core.FieldType], comment: T2) -> Either[hydra.errors.Error, frozenlist[hydra.cpp.syntax.Declaration]]:
    return hydra.lib.logic.if_else(hydra.predicates.is_enum_row_type(rt), (lambda : encode_enum_type(cx, g, name, rt, comment)), (lambda : encode_variant_type(cx, g, name, rt, comment)))

def encode_wrapped_type(cx: T0, g: T1, name: hydra.core.Name, typ: hydra.core.Type, comment: T2) -> Either[hydra.errors.Error, frozenlist[hydra.cpp.syntax.Declaration]]:
    return encode_record_type(cx, g, name, (hydra.core.FieldType(hydra.core.Name("value"), typ),), comment)

def encode_type_definition(cx: T0, g: T1, name: hydra.core.Name, typ: hydra.core.Type) -> Either[hydra.errors.Error, frozenlist[hydra.cpp.syntax.Declaration]]:
    while True:
        @lru_cache(1)
        def t() -> hydra.core.Type:
            return hydra.strip.deannotate_type(typ)
        match t():
            case hydra.core.TypeForall(value=fa):
                cx = cx
                g = g
                name = name
                typ = fa.body
                continue

            case hydra.core.TypeRecord(value=rt):
                return encode_record_type(cx, g, name, rt, Nothing())

            case hydra.core.TypeUnion(value=rt2):
                return encode_union_type(cx, g, name, rt2, Nothing())

            case hydra.core.TypeWrap(value=wt):
                return encode_wrapped_type(cx, g, name, wt, Nothing())

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("unexpected type in definition: ", hydra.show.core.type(typ))))))

def find_type_dependencies(ns: hydra.packaging.Namespace, defs: frozenlist[hydra.packaging.TypeDefinition]) -> frozenlist[hydra.core.Name]:
    return hydra.lib.lists.filter((lambda n: hydra.lib.logic.not_(hydra.lib.equality.equal(hydra.lib.maybes.map((lambda v1: v1.value), hydra.names.namespace_of(n)), Just(ns.value)))), hydra.lib.sets.to_list(hydra.lib.lists.foldl((lambda acc, d: hydra.lib.sets.union(acc, hydra.dependencies.type_dependency_names(True, d.type.type))), hydra.lib.sets.empty(), defs)))

def fwd_header_name(ns: hydra.packaging.Namespace) -> hydra.core.Name:
    return hydra.names.unqualify_name(hydra.packaging.QualifiedName(Just(ns), "Fwd"))

def find_includes(with_fwd: bool, ns: hydra.packaging.Namespace, defs: frozenlist[hydra.packaging.TypeDefinition]) -> frozenlist[hydra.cpp.syntax.IncludeDirective]:
    return hydra.lib.lists.concat(((hydra.cpp.syntax.IncludeDirective("memory", True), hydra.cpp.syntax.IncludeDirective("stdexcept", True)), hydra.lib.lists.map((lambda dep_name: hydra.cpp.syntax.IncludeDirective(binding_name_to_file_path(dep_name), False)), find_type_dependencies(ns, defs)), hydra.lib.logic.if_else(with_fwd, (lambda : (hydra.cpp.syntax.IncludeDirective(binding_name_to_file_path(fwd_header_name(ns)), False),)), (lambda : ()))))

def gather_metadata(defs: T0) -> bool:
    return True

def namespace_decl(ns: hydra.packaging.Namespace, decls: frozenlist[hydra.cpp.syntax.Declaration]) -> hydra.cpp.syntax.Declaration:
    return cast(hydra.cpp.syntax.Declaration, hydra.cpp.syntax.DeclarationNamespace(hydra.cpp.syntax.NamespaceDeclaration(encode_namespace(ns), decls)))

def serialize_header_file(name: hydra.core.Name, includes: frozenlist[hydra.cpp.syntax.IncludeDirective], decls: frozenlist[hydra.cpp.syntax.Declaration]) -> tuple[str, str]:
    return (binding_name_to_file_path(name), hydra.serialization.print_expr(hydra.serialization.parenthesize(hydra.cpp.serde.encode_program(create_header_file(includes, decls)))))

def generate_type_file(ns: hydra.packaging.Namespace, def_: hydra.packaging.TypeDefinition, cx: T0, g: T1) -> Either[hydra.errors.Error, tuple[str, str]]:
    name = def_.name
    typ = def_.type.type
    return hydra.lib.eithers.bind(encode_type_definition(cx, g, name, typ), (lambda decls: (includes := find_includes(True, ns, (def_,)), Right(serialize_header_file(name, includes, (namespace_decl(ns, decls),))))[1]))

def generate_type_files(ns: hydra.packaging.Namespace, defs: frozenlist[hydra.packaging.TypeDefinition], cx: T0, g: T1) -> Either[hydra.errors.Error, frozenlist[tuple[str, str]]]:
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda d: generate_type_file(ns, d, cx, g)), defs), (lambda class_files: Right(class_files)))

def is_std_container_type(typ: hydra.core.Type) -> bool:
    while True:
        @lru_cache(1)
        def t() -> hydra.core.Type:
            return hydra.strip.deannotate_type(typ)
        match t():
            case hydra.core.TypeApplication(value=at):
                typ = at.function
                continue

            case hydra.core.TypeList():
                return True

            case hydra.core.TypeMap():
                return True

            case hydra.core.TypeMaybe():
                return True

            case hydra.core.TypeSet():
                return True

            case _:
                return False

def is_struct_type(raw_type: hydra.core.Type) -> bool:
    @lru_cache(1)
    def t() -> hydra.core.Type:
        return hydra.resolution.fully_strip_type(raw_type)
    @lru_cache(1)
    def is_literal() -> bool:
        match t():
            case hydra.core.TypeLiteral():
                return True

            case _:
                return False
    return hydra.lib.logic.and_(hydra.lib.logic.not_(is_literal()), hydra.lib.logic.not_(hydra.predicates.is_enum_type(raw_type)))

def is_template_type(typ: hydra.core.Type):
    @lru_cache(1)
    def t() -> hydra.core.Type:
        return hydra.strip.deannotate_type(typ)
    def _hoist_t_body_1(v1):
        match v1:
            case hydra.core.LiteralTypeString():
                return True

            case _:
                return False
    def _hoist_t_body_2(v1):
        match v1:
            case hydra.core.TypeLiteral(value=lt):
                return _hoist_t_body_1(lt)

            case _:
                return False
    return hydra.lib.logic.or_(_hoist_t_body_2(t()), is_std_container_type(typ))

def module_to_cpp(mod: hydra.packaging.Module, defs: frozenlist[hydra.packaging.Definition], cx: T0, g: T1) -> Either[hydra.errors.Error, FrozenDict[str, str]]:
    ns = mod.namespace
    @lru_cache(1)
    def type_defs() -> frozenlist[hydra.packaging.TypeDefinition]:
        return hydra.lib.pairs.first(hydra.environment.partition_definitions(defs))
    return hydra.lib.eithers.bind(generate_type_files(ns, type_defs(), cx, g), (lambda type_files: Right(hydra.lib.maps.from_list(type_files))))
