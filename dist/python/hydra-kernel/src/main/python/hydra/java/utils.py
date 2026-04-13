# Note: this is an automatically generated file. Do not edit.

r"""Java utilities for constructing Java syntax trees."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.errors
import hydra.formatting
import hydra.java.environment
import hydra.java.language
import hydra.java.names
import hydra.java.syntax
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

T0 = TypeVar("T0")

def add_expressions(exprs: frozenlist[hydra.java.syntax.MultiplicativeExpression]) -> hydra.java.syntax.AdditiveExpression:
    @lru_cache(1)
    def first() -> hydra.java.syntax.AdditiveExpression:
        return cast(hydra.java.syntax.AdditiveExpression, hydra.java.syntax.AdditiveExpressionUnary(hydra.lib.lists.head(exprs)))
    @lru_cache(1)
    def rest() -> frozenlist[hydra.java.syntax.MultiplicativeExpression]:
        return hydra.lib.lists.tail(exprs)
    return hydra.lib.lists.foldl((lambda ae, me: cast(hydra.java.syntax.AdditiveExpression, hydra.java.syntax.AdditiveExpressionPlus(hydra.java.syntax.AdditiveExpression_Binary(ae, me)))), first(), rest())

def add_in_scope_var(name: hydra.core.Name, aliases: hydra.java.environment.Aliases) -> hydra.java.environment.Aliases:
    return hydra.java.environment.Aliases(aliases.current_namespace, aliases.packages, aliases.branch_vars, aliases.recursive_vars, aliases.in_scope_type_params, aliases.polymorphic_locals, hydra.lib.sets.insert(name, aliases.in_scope_java_vars), aliases.var_renames, aliases.lambda_vars, aliases.type_var_subst, aliases.trusted_type_vars, aliases.method_codomain, aliases.thunked_vars)

def add_in_scope_vars(names: frozenlist[hydra.core.Name], aliases: hydra.java.environment.Aliases) -> hydra.java.environment.Aliases:
    return hydra.lib.lists.foldl((lambda a, n: add_in_scope_var(n, a)), aliases, names)

def java_type_variable_to_type(tv: hydra.java.syntax.TypeVariable) -> hydra.java.syntax.Type:
    return cast(hydra.java.syntax.Type, hydra.java.syntax.TypeReference(cast(hydra.java.syntax.ReferenceType, hydra.java.syntax.ReferenceTypeVariable(tv))))

def add_java_type_parameter(rt: hydra.java.syntax.ReferenceType, t: hydra.java.syntax.Type, cx: T0):
    def _hoist_hydra_java_utils_add_java_type_parameter_1(rt, v1):
        match v1:
            case hydra.java.syntax.ClassOrInterfaceTypeClass(value=ct):
                anns = ct.annotations
                qual = ct.qualifier
                id = ct.identifier
                args = ct.arguments
                return Right(cast(hydra.java.syntax.Type, hydra.java.syntax.TypeReference(cast(hydra.java.syntax.ReferenceType, hydra.java.syntax.ReferenceTypeClassOrInterface(cast(hydra.java.syntax.ClassOrInterfaceType, hydra.java.syntax.ClassOrInterfaceTypeClass(hydra.java.syntax.ClassType(anns, qual, id, hydra.lib.lists.concat2(args, (cast(hydra.java.syntax.TypeArgument, hydra.java.syntax.TypeArgumentReference(rt)),))))))))))

            case hydra.java.syntax.ClassOrInterfaceTypeInterface():
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("expected a Java class type"))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_hydra_java_utils_add_java_type_parameter_2(rt, v1):
        match v1:
            case hydra.java.syntax.ReferenceTypeClassOrInterface(value=cit):
                return _hoist_hydra_java_utils_add_java_type_parameter_1(rt, cit)

            case hydra.java.syntax.ReferenceTypeVariable(value=tv):
                return Right(java_type_variable_to_type(tv))

            case hydra.java.syntax.ReferenceTypeArray():
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("expected a Java class or interface type, or a variable"))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    match t:
        case hydra.java.syntax.TypeReference(value=rt1):
            return _hoist_hydra_java_utils_add_java_type_parameter_2(rt, rt1)

        case hydra.java.syntax.TypePrimitive():
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("expected a reference type"))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def add_var_rename(original: hydra.core.Name, renamed: hydra.core.Name, aliases: hydra.java.environment.Aliases) -> hydra.java.environment.Aliases:
    return hydra.java.environment.Aliases(aliases.current_namespace, aliases.packages, aliases.branch_vars, aliases.recursive_vars, aliases.in_scope_type_params, aliases.polymorphic_locals, aliases.in_scope_java_vars, hydra.lib.maps.insert(original, renamed, aliases.var_renames), aliases.lambda_vars, aliases.type_var_subst, aliases.trusted_type_vars, aliases.method_codomain, aliases.thunked_vars)

def field_expression(var_id: hydra.java.syntax.Identifier, field_id: hydra.java.syntax.Identifier) -> hydra.java.syntax.ExpressionName:
    return hydra.java.syntax.ExpressionName(Just(hydra.java.syntax.AmbiguousName((var_id,))), field_id)

def is_escaped(s: str) -> bool:
    return hydra.lib.equality.equal(hydra.lib.strings.char_at(0, s), 36)

def unescape(s: str) -> str:
    return hydra.lib.strings.from_list(hydra.lib.lists.tail(hydra.lib.strings.to_list(s)))

def sanitize_java_name(name: str) -> str:
    return hydra.lib.logic.if_else(is_escaped(name), (lambda : unescape(name)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(name, "_"), (lambda : "ignored"), (lambda : hydra.formatting.sanitize_with_underscores(hydra.java.language.reserved_words(), name)))))

def java_identifier(s: str) -> hydra.java.syntax.Identifier:
    return hydra.java.syntax.Identifier(sanitize_java_name(s))

def field_name_to_java_identifier(fname: hydra.core.Name) -> hydra.java.syntax.Identifier:
    return java_identifier(fname.value)

def java_identifier_to_java_expression_name(id: hydra.java.syntax.Identifier) -> hydra.java.syntax.ExpressionName:
    return hydra.java.syntax.ExpressionName(Nothing(), id)

def field_name_to_java_expression(fname: hydra.core.Name) -> hydra.java.syntax.Expression:
    return cast(hydra.java.syntax.Expression, hydra.java.syntax.ExpressionAssignment(cast(hydra.java.syntax.AssignmentExpression, hydra.java.syntax.AssignmentExpressionConditional(cast(hydra.java.syntax.ConditionalExpression, hydra.java.syntax.ConditionalExpressionSimple(hydra.java.syntax.ConditionalOrExpression((hydra.java.syntax.ConditionalAndExpression((hydra.java.syntax.InclusiveOrExpression((hydra.java.syntax.ExclusiveOrExpression((hydra.java.syntax.AndExpression((cast(hydra.java.syntax.EqualityExpression, hydra.java.syntax.EqualityExpressionUnary(cast(hydra.java.syntax.RelationalExpression, hydra.java.syntax.RelationalExpressionSimple(cast(hydra.java.syntax.ShiftExpression, hydra.java.syntax.ShiftExpressionUnary(cast(hydra.java.syntax.AdditiveExpression, hydra.java.syntax.AdditiveExpressionUnary(cast(hydra.java.syntax.MultiplicativeExpression, hydra.java.syntax.MultiplicativeExpressionUnary(cast(hydra.java.syntax.UnaryExpression, hydra.java.syntax.UnaryExpressionOther(cast(hydra.java.syntax.UnaryExpressionNotPlusMinus, hydra.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.java.syntax.PostfixExpression, hydra.java.syntax.PostfixExpressionName(java_identifier_to_java_expression_name(field_name_to_java_identifier(fname)))))))))))))))))),)),)),)),)),))))))))

def java_variable_declarator_id(id: hydra.java.syntax.Identifier) -> hydra.java.syntax.VariableDeclaratorId:
    return hydra.java.syntax.VariableDeclaratorId(id, Nothing())

def java_variable_declarator(id: hydra.java.syntax.Identifier, minit: Maybe[hydra.java.syntax.VariableInitializer]) -> hydra.java.syntax.VariableDeclarator:
    return hydra.java.syntax.VariableDeclarator(java_variable_declarator_id(id), minit)

def field_name_to_java_variable_declarator(fname: hydra.core.Name) -> hydra.java.syntax.VariableDeclarator:
    return java_variable_declarator(java_identifier(fname.value), Nothing())

def field_name_to_java_variable_declarator_id(fname: hydra.core.Name) -> hydra.java.syntax.VariableDeclaratorId:
    return java_variable_declarator_id(java_identifier(fname.value))

def final_var_declaration_statement(id: hydra.java.syntax.Identifier, rhs: hydra.java.syntax.Expression) -> hydra.java.syntax.BlockStatement:
    return cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementLocalVariableDeclaration(hydra.java.syntax.LocalVariableDeclarationStatement(hydra.java.syntax.LocalVariableDeclaration((cast(hydra.java.syntax.VariableModifier, hydra.java.syntax.VariableModifierFinal()),), cast(hydra.java.syntax.LocalVariableType, hydra.java.syntax.LocalVariableTypeVar()), (java_variable_declarator(id, Just(cast(hydra.java.syntax.VariableInitializer, hydra.java.syntax.VariableInitializerExpression(rhs)))),)))))

def import_aliases_for_module(mod: hydra.packaging.Module) -> hydra.java.environment.Aliases:
    return hydra.java.environment.Aliases(mod.namespace, hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), Nothing(), hydra.lib.sets.empty())

def java_method_body(mstmts: Maybe[frozenlist[hydra.java.syntax.BlockStatement]]) -> hydra.java.syntax.MethodBody:
    return hydra.lib.maybes.cases(mstmts, (lambda : cast(hydra.java.syntax.MethodBody, hydra.java.syntax.MethodBodyNone())), (lambda stmts: cast(hydra.java.syntax.MethodBody, hydra.java.syntax.MethodBodyBlock(hydra.java.syntax.Block(stmts)))))

def java_method_header(tparams: frozenlist[hydra.java.syntax.TypeParameter], method_name: str, params: frozenlist[hydra.java.syntax.FormalParameter], result: hydra.java.syntax.Result) -> hydra.java.syntax.MethodHeader:
    return hydra.java.syntax.MethodHeader(tparams, result, hydra.java.syntax.MethodDeclarator(hydra.java.syntax.Identifier(method_name), Nothing(), params), Nothing())

def interface_method_declaration(mods: frozenlist[hydra.java.syntax.InterfaceMethodModifier], tparams: frozenlist[hydra.java.syntax.TypeParameter], method_name: str, params: frozenlist[hydra.java.syntax.FormalParameter], result: hydra.java.syntax.Result, stmts: Maybe[frozenlist[hydra.java.syntax.BlockStatement]]) -> hydra.java.syntax.InterfaceMemberDeclaration:
    return cast(hydra.java.syntax.InterfaceMemberDeclaration, hydra.java.syntax.InterfaceMemberDeclarationInterfaceMethod(hydra.java.syntax.InterfaceMethodDeclaration(mods, java_method_header(tparams, method_name, params, result), java_method_body(stmts))))

def java_additive_expression_to_java_expression(ae: hydra.java.syntax.AdditiveExpression) -> hydra.java.syntax.Expression:
    return cast(hydra.java.syntax.Expression, hydra.java.syntax.ExpressionAssignment(cast(hydra.java.syntax.AssignmentExpression, hydra.java.syntax.AssignmentExpressionConditional(cast(hydra.java.syntax.ConditionalExpression, hydra.java.syntax.ConditionalExpressionSimple(hydra.java.syntax.ConditionalOrExpression((hydra.java.syntax.ConditionalAndExpression((hydra.java.syntax.InclusiveOrExpression((hydra.java.syntax.ExclusiveOrExpression((hydra.java.syntax.AndExpression((cast(hydra.java.syntax.EqualityExpression, hydra.java.syntax.EqualityExpressionUnary(cast(hydra.java.syntax.RelationalExpression, hydra.java.syntax.RelationalExpressionSimple(cast(hydra.java.syntax.ShiftExpression, hydra.java.syntax.ShiftExpressionUnary(ae)))))),)),)),)),)),))))))))

def java_primary_to_java_expression(p: hydra.java.syntax.Primary) -> hydra.java.syntax.Expression:
    return cast(hydra.java.syntax.Expression, hydra.java.syntax.ExpressionAssignment(cast(hydra.java.syntax.AssignmentExpression, hydra.java.syntax.AssignmentExpressionConditional(cast(hydra.java.syntax.ConditionalExpression, hydra.java.syntax.ConditionalExpressionSimple(hydra.java.syntax.ConditionalOrExpression((hydra.java.syntax.ConditionalAndExpression((hydra.java.syntax.InclusiveOrExpression((hydra.java.syntax.ExclusiveOrExpression((hydra.java.syntax.AndExpression((cast(hydra.java.syntax.EqualityExpression, hydra.java.syntax.EqualityExpressionUnary(cast(hydra.java.syntax.RelationalExpression, hydra.java.syntax.RelationalExpressionSimple(cast(hydra.java.syntax.ShiftExpression, hydra.java.syntax.ShiftExpressionUnary(cast(hydra.java.syntax.AdditiveExpression, hydra.java.syntax.AdditiveExpressionUnary(cast(hydra.java.syntax.MultiplicativeExpression, hydra.java.syntax.MultiplicativeExpressionUnary(cast(hydra.java.syntax.UnaryExpression, hydra.java.syntax.UnaryExpressionOther(cast(hydra.java.syntax.UnaryExpressionNotPlusMinus, hydra.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.java.syntax.PostfixExpression, hydra.java.syntax.PostfixExpressionPrimary(p)))))))))))))))),)),)),)),)),))))))))

def java_array_creation(prim_type: hydra.java.syntax.PrimitiveTypeWithAnnotations, minit: Maybe[hydra.java.syntax.ArrayInitializer]) -> hydra.java.syntax.Expression:
    @lru_cache(1)
    def init_() -> hydra.java.syntax.ArrayInitializer:
        return hydra.lib.maybes.cases(minit, (lambda : hydra.java.syntax.ArrayInitializer(())), (lambda i: i))
    return java_primary_to_java_expression(cast(hydra.java.syntax.Primary, hydra.java.syntax.PrimaryArrayCreation(cast(hydra.java.syntax.ArrayCreationExpression, hydra.java.syntax.ArrayCreationExpressionPrimitiveArray(hydra.java.syntax.ArrayCreationExpression_PrimitiveArray(prim_type, (), init_()))))))

def java_array_initializer(exprs: frozenlist[hydra.java.syntax.Expression]) -> hydra.java.syntax.ArrayInitializer:
    return hydra.java.syntax.ArrayInitializer((hydra.lib.lists.map((lambda e: cast(hydra.java.syntax.VariableInitializer, hydra.java.syntax.VariableInitializerExpression(e))), exprs),))

def java_assignment_statement(lhs: hydra.java.syntax.LeftHandSide, rhs: hydra.java.syntax.Expression) -> hydra.java.syntax.Statement:
    return cast(hydra.java.syntax.Statement, hydra.java.syntax.StatementWithoutTrailing(cast(hydra.java.syntax.StatementWithoutTrailingSubstatement, hydra.java.syntax.StatementWithoutTrailingSubstatementExpression(hydra.java.syntax.ExpressionStatement(cast(hydra.java.syntax.StatementExpression, hydra.java.syntax.StatementExpressionAssignment(hydra.java.syntax.Assignment(lhs, hydra.java.syntax.AssignmentOperator.SIMPLE, rhs))))))))

def java_boolean(b: bool) -> hydra.java.syntax.Literal:
    return cast(hydra.java.syntax.Literal, hydra.java.syntax.LiteralBoolean(b))

def java_literal_to_java_primary(lit: hydra.java.syntax.Literal) -> hydra.java.syntax.Primary:
    return cast(hydra.java.syntax.Primary, hydra.java.syntax.PrimaryNoNewArray(cast(hydra.java.syntax.PrimaryNoNewArrayExpression, hydra.java.syntax.PrimaryNoNewArrayExpressionLiteral(lit))))

def java_boolean_expression(b: bool) -> hydra.java.syntax.Expression:
    return java_primary_to_java_expression(java_literal_to_java_primary(java_boolean(b)))

def java_primitive_type_to_java_type(pt: hydra.java.syntax.PrimitiveType) -> hydra.java.syntax.Type:
    return cast(hydra.java.syntax.Type, hydra.java.syntax.TypePrimitive(hydra.java.syntax.PrimitiveTypeWithAnnotations(pt, ())))

@lru_cache(1)
def java_boolean_type() -> hydra.java.syntax.Type:
    return java_primitive_type_to_java_type(cast(hydra.java.syntax.PrimitiveType, hydra.java.syntax.PrimitiveTypeBoolean()))

@lru_cache(1)
def java_byte_primitive_type() -> hydra.java.syntax.PrimitiveTypeWithAnnotations:
    return hydra.java.syntax.PrimitiveTypeWithAnnotations(cast(hydra.java.syntax.PrimitiveType, hydra.java.syntax.PrimitiveTypeNumeric(cast(hydra.java.syntax.NumericType, hydra.java.syntax.NumericTypeIntegral(hydra.java.syntax.IntegralType.BYTE)))), ())

def java_cast_expression(rt: hydra.java.syntax.ReferenceType, expr: hydra.java.syntax.UnaryExpression) -> hydra.java.syntax.CastExpression:
    return cast(hydra.java.syntax.CastExpression, hydra.java.syntax.CastExpressionNotPlusMinus(hydra.java.syntax.CastExpression_NotPlusMinus(hydra.java.syntax.CastExpression_RefAndBounds(rt, ()), expr)))

def java_cast_expression_to_java_expression(ce: hydra.java.syntax.CastExpression) -> hydra.java.syntax.Expression:
    return cast(hydra.java.syntax.Expression, hydra.java.syntax.ExpressionAssignment(cast(hydra.java.syntax.AssignmentExpression, hydra.java.syntax.AssignmentExpressionConditional(cast(hydra.java.syntax.ConditionalExpression, hydra.java.syntax.ConditionalExpressionSimple(hydra.java.syntax.ConditionalOrExpression((hydra.java.syntax.ConditionalAndExpression((hydra.java.syntax.InclusiveOrExpression((hydra.java.syntax.ExclusiveOrExpression((hydra.java.syntax.AndExpression((cast(hydra.java.syntax.EqualityExpression, hydra.java.syntax.EqualityExpressionUnary(cast(hydra.java.syntax.RelationalExpression, hydra.java.syntax.RelationalExpressionSimple(cast(hydra.java.syntax.ShiftExpression, hydra.java.syntax.ShiftExpressionUnary(cast(hydra.java.syntax.AdditiveExpression, hydra.java.syntax.AdditiveExpressionUnary(cast(hydra.java.syntax.MultiplicativeExpression, hydra.java.syntax.MultiplicativeExpressionUnary(cast(hydra.java.syntax.UnaryExpression, hydra.java.syntax.UnaryExpressionOther(cast(hydra.java.syntax.UnaryExpressionNotPlusMinus, hydra.java.syntax.UnaryExpressionNotPlusMinusCast(ce)))))))))))))),)),)),)),)),))))))))

def java_cast_primitive(pt: hydra.java.syntax.PrimitiveType, expr: hydra.java.syntax.UnaryExpression) -> hydra.java.syntax.CastExpression:
    return cast(hydra.java.syntax.CastExpression, hydra.java.syntax.CastExpressionPrimitive(hydra.java.syntax.CastExpression_Primitive(hydra.java.syntax.PrimitiveTypeWithAnnotations(pt, ()), expr)))

def java_variable_name(name: hydra.core.Name) -> hydra.java.syntax.Identifier:
    return java_identifier(hydra.names.local_name_of(name))

def java_decl_name(name: hydra.core.Name) -> hydra.java.syntax.TypeIdentifier:
    return hydra.java.syntax.TypeIdentifier(java_variable_name(name))

def java_type_identifier(s: str) -> hydra.java.syntax.TypeIdentifier:
    return hydra.java.syntax.TypeIdentifier(hydra.java.syntax.Identifier(s))

def name_to_qualified_java_name(aliases: hydra.java.environment.Aliases, qualify: bool, name: hydra.core.Name, mlocal: Maybe[str]) -> tuple[hydra.java.syntax.TypeIdentifier, hydra.java.syntax.ClassTypeQualifier]:
    @lru_cache(1)
    def qn() -> hydra.packaging.QualifiedName:
        return hydra.names.qualify_name(name)
    ns_ = qn().namespace
    local = qn().local
    @lru_cache(1)
    def alias() -> Maybe[hydra.java.syntax.PackageName]:
        return hydra.lib.maybes.cases(ns_, (lambda : Nothing()), (lambda n: Just(hydra.lib.maybes.cases(hydra.lib.maps.lookup(n, aliases.packages), (lambda : hydra.java.names.java_package_name(hydra.lib.strings.split_on(".", n.value))), (lambda id: id)))))
    @lru_cache(1)
    def pkg() -> hydra.java.syntax.ClassTypeQualifier:
        return hydra.lib.logic.if_else(qualify, (lambda : hydra.lib.maybes.cases(alias(), (lambda : cast(hydra.java.syntax.ClassTypeQualifier, hydra.java.syntax.ClassTypeQualifierNone())), (lambda p: cast(hydra.java.syntax.ClassTypeQualifier, hydra.java.syntax.ClassTypeQualifierPackage(p))))), (lambda : cast(hydra.java.syntax.ClassTypeQualifier, hydra.java.syntax.ClassTypeQualifierNone())))
    @lru_cache(1)
    def jid() -> hydra.java.syntax.TypeIdentifier:
        return java_type_identifier(hydra.lib.maybes.cases(mlocal, (lambda : sanitize_java_name(local)), (lambda l: hydra.lib.strings.cat2(hydra.lib.strings.cat2(sanitize_java_name(local), "."), sanitize_java_name(l)))))
    return (jid(), pkg())

def name_to_java_class_type(aliases: hydra.java.environment.Aliases, qualify: bool, args: frozenlist[hydra.java.syntax.TypeArgument], name: hydra.core.Name, mlocal: Maybe[str]) -> hydra.java.syntax.ClassType:
    @lru_cache(1)
    def result() -> tuple[hydra.java.syntax.TypeIdentifier, hydra.java.syntax.ClassTypeQualifier]:
        return name_to_qualified_java_name(aliases, qualify, name, mlocal)
    @lru_cache(1)
    def id() -> hydra.java.syntax.TypeIdentifier:
        return hydra.lib.pairs.first(result())
    @lru_cache(1)
    def pkg() -> hydra.java.syntax.ClassTypeQualifier:
        return hydra.lib.pairs.second(result())
    return hydra.java.syntax.ClassType((), pkg(), id(), args)

def java_class_declaration(aliases: hydra.java.environment.Aliases, tparams: frozenlist[hydra.java.syntax.TypeParameter], el_name: hydra.core.Name, mods: frozenlist[hydra.java.syntax.ClassModifier], supname: Maybe[hydra.core.Name], impls: frozenlist[hydra.java.syntax.InterfaceType], body_decls: frozenlist[hydra.java.syntax.ClassBodyDeclarationWithComments]) -> hydra.java.syntax.ClassDeclaration:
    @lru_cache(1)
    def extends_() -> Maybe[hydra.java.syntax.ClassType]:
        return hydra.lib.maybes.map((lambda n: name_to_java_class_type(aliases, True, (), n, Nothing())), supname)
    return cast(hydra.java.syntax.ClassDeclaration, hydra.java.syntax.ClassDeclarationNormal(hydra.java.syntax.NormalClassDeclaration(mods, java_decl_name(el_name), tparams, extends_(), impls, hydra.java.syntax.ClassBody(body_decls))))

def java_class_type(args: frozenlist[hydra.java.syntax.ReferenceType], pkg: Maybe[hydra.java.syntax.PackageName], id: str) -> hydra.java.syntax.ClassType:
    @lru_cache(1)
    def qual() -> hydra.java.syntax.ClassTypeQualifier:
        return hydra.lib.maybes.cases(pkg, (lambda : cast(hydra.java.syntax.ClassTypeQualifier, hydra.java.syntax.ClassTypeQualifierNone())), (lambda p: cast(hydra.java.syntax.ClassTypeQualifier, hydra.java.syntax.ClassTypeQualifierPackage(p))))
    @lru_cache(1)
    def targs() -> frozenlist[hydra.java.syntax.TypeArgument]:
        return hydra.lib.lists.map((lambda rt: cast(hydra.java.syntax.TypeArgument, hydra.java.syntax.TypeArgumentReference(rt))), args)
    return hydra.java.syntax.ClassType((), qual(), java_type_identifier(id), targs())

def java_class_type_to_java_type(ct: hydra.java.syntax.ClassType) -> hydra.java.syntax.Type:
    return cast(hydra.java.syntax.Type, hydra.java.syntax.TypeReference(cast(hydra.java.syntax.ReferenceType, hydra.java.syntax.ReferenceTypeClassOrInterface(cast(hydra.java.syntax.ClassOrInterfaceType, hydra.java.syntax.ClassOrInterfaceTypeClass(ct))))))

def java_conditional_and_expression_to_java_expression(cae: hydra.java.syntax.ConditionalAndExpression) -> hydra.java.syntax.Expression:
    return cast(hydra.java.syntax.Expression, hydra.java.syntax.ExpressionAssignment(cast(hydra.java.syntax.AssignmentExpression, hydra.java.syntax.AssignmentExpressionConditional(cast(hydra.java.syntax.ConditionalExpression, hydra.java.syntax.ConditionalExpressionSimple(hydra.java.syntax.ConditionalOrExpression((cae,))))))))

def java_constructor_call(ci: hydra.java.syntax.ClassOrInterfaceTypeToInstantiate, args: frozenlist[hydra.java.syntax.Expression], mbody: Maybe[hydra.java.syntax.ClassBody]) -> hydra.java.syntax.Expression:
    return cast(hydra.java.syntax.Expression, hydra.java.syntax.ExpressionAssignment(cast(hydra.java.syntax.AssignmentExpression, hydra.java.syntax.AssignmentExpressionConditional(cast(hydra.java.syntax.ConditionalExpression, hydra.java.syntax.ConditionalExpressionSimple(hydra.java.syntax.ConditionalOrExpression((hydra.java.syntax.ConditionalAndExpression((hydra.java.syntax.InclusiveOrExpression((hydra.java.syntax.ExclusiveOrExpression((hydra.java.syntax.AndExpression((cast(hydra.java.syntax.EqualityExpression, hydra.java.syntax.EqualityExpressionUnary(cast(hydra.java.syntax.RelationalExpression, hydra.java.syntax.RelationalExpressionSimple(cast(hydra.java.syntax.ShiftExpression, hydra.java.syntax.ShiftExpressionUnary(cast(hydra.java.syntax.AdditiveExpression, hydra.java.syntax.AdditiveExpressionUnary(cast(hydra.java.syntax.MultiplicativeExpression, hydra.java.syntax.MultiplicativeExpressionUnary(cast(hydra.java.syntax.UnaryExpression, hydra.java.syntax.UnaryExpressionOther(cast(hydra.java.syntax.UnaryExpressionNotPlusMinus, hydra.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.java.syntax.PostfixExpression, hydra.java.syntax.PostfixExpressionPrimary(cast(hydra.java.syntax.Primary, hydra.java.syntax.PrimaryNoNewArray(cast(hydra.java.syntax.PrimaryNoNewArrayExpression, hydra.java.syntax.PrimaryNoNewArrayExpressionClassInstance(hydra.java.syntax.ClassInstanceCreationExpression(Nothing(), hydra.java.syntax.UnqualifiedClassInstanceCreationExpression((), ci, args, mbody)))))))))))))))))))))),)),)),)),)),))))))))

def java_constructor_name(id: hydra.java.syntax.Identifier, targs: Maybe[hydra.java.syntax.TypeArgumentsOrDiamond]) -> hydra.java.syntax.ClassOrInterfaceTypeToInstantiate:
    return hydra.java.syntax.ClassOrInterfaceTypeToInstantiate((hydra.java.syntax.AnnotatedIdentifier((), id),), targs)

def java_expression_to_java_unary_expression(e: hydra.java.syntax.Expression) -> hydra.java.syntax.UnaryExpression:
    return cast(hydra.java.syntax.UnaryExpression, hydra.java.syntax.UnaryExpressionOther(cast(hydra.java.syntax.UnaryExpressionNotPlusMinus, hydra.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.java.syntax.PostfixExpression, hydra.java.syntax.PostfixExpressionPrimary(cast(hydra.java.syntax.Primary, hydra.java.syntax.PrimaryNoNewArray(cast(hydra.java.syntax.PrimaryNoNewArrayExpression, hydra.java.syntax.PrimaryNoNewArrayExpressionParens(e))))))))))

def java_double_cast_expression(raw_rt: hydra.java.syntax.ReferenceType, target_rt: hydra.java.syntax.ReferenceType, expr: hydra.java.syntax.UnaryExpression) -> hydra.java.syntax.CastExpression:
    @lru_cache(1)
    def first_cast() -> hydra.java.syntax.Expression:
        return java_cast_expression_to_java_expression(java_cast_expression(raw_rt, expr))
    return java_cast_expression(target_rt, java_expression_to_java_unary_expression(first_cast()))

def java_double_cast_expression_to_java_expression(raw_rt: hydra.java.syntax.ReferenceType, target_rt: hydra.java.syntax.ReferenceType, expr: hydra.java.syntax.UnaryExpression) -> hydra.java.syntax.Expression:
    return java_cast_expression_to_java_expression(java_double_cast_expression(raw_rt, target_rt, expr))

java_empty_statement = cast(hydra.java.syntax.Statement, hydra.java.syntax.StatementWithoutTrailing(cast(hydra.java.syntax.StatementWithoutTrailingSubstatement, hydra.java.syntax.StatementWithoutTrailingSubstatementEmpty())))

def java_equality_expression_to_java_expression(ee: hydra.java.syntax.EqualityExpression) -> hydra.java.syntax.Expression:
    return cast(hydra.java.syntax.Expression, hydra.java.syntax.ExpressionAssignment(cast(hydra.java.syntax.AssignmentExpression, hydra.java.syntax.AssignmentExpressionConditional(cast(hydra.java.syntax.ConditionalExpression, hydra.java.syntax.ConditionalExpressionSimple(hydra.java.syntax.ConditionalOrExpression((hydra.java.syntax.ConditionalAndExpression((hydra.java.syntax.InclusiveOrExpression((hydra.java.syntax.ExclusiveOrExpression((hydra.java.syntax.AndExpression((ee,)),)),)),)),))))))))

def java_equality_expression_to_java_inclusive_or_expression(ee: hydra.java.syntax.EqualityExpression) -> hydra.java.syntax.InclusiveOrExpression:
    return hydra.java.syntax.InclusiveOrExpression((hydra.java.syntax.ExclusiveOrExpression((hydra.java.syntax.AndExpression((ee,)),)),))

def java_equals(lhs: hydra.java.syntax.EqualityExpression, rhs: hydra.java.syntax.RelationalExpression) -> hydra.java.syntax.EqualityExpression:
    return cast(hydra.java.syntax.EqualityExpression, hydra.java.syntax.EqualityExpressionEqual(hydra.java.syntax.EqualityExpression_Binary(lhs, rhs)))

def java_literal_to_java_relational_expression(lit: hydra.java.syntax.Literal) -> hydra.java.syntax.RelationalExpression:
    return cast(hydra.java.syntax.RelationalExpression, hydra.java.syntax.RelationalExpressionSimple(cast(hydra.java.syntax.ShiftExpression, hydra.java.syntax.ShiftExpressionUnary(cast(hydra.java.syntax.AdditiveExpression, hydra.java.syntax.AdditiveExpressionUnary(cast(hydra.java.syntax.MultiplicativeExpression, hydra.java.syntax.MultiplicativeExpressionUnary(cast(hydra.java.syntax.UnaryExpression, hydra.java.syntax.UnaryExpressionOther(cast(hydra.java.syntax.UnaryExpressionNotPlusMinus, hydra.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.java.syntax.PostfixExpression, hydra.java.syntax.PostfixExpressionPrimary(cast(hydra.java.syntax.Primary, hydra.java.syntax.PrimaryNoNewArray(cast(hydra.java.syntax.PrimaryNoNewArrayExpression, hydra.java.syntax.PrimaryNoNewArrayExpressionLiteral(lit))))))))))))))))))

def java_equals_null(lhs: hydra.java.syntax.EqualityExpression) -> hydra.java.syntax.EqualityExpression:
    return java_equals(lhs, java_literal_to_java_relational_expression(cast(hydra.java.syntax.Literal, hydra.java.syntax.LiteralNull())))

def java_expression_name_to_java_expression(en: hydra.java.syntax.ExpressionName) -> hydra.java.syntax.Expression:
    return cast(hydra.java.syntax.Expression, hydra.java.syntax.ExpressionAssignment(cast(hydra.java.syntax.AssignmentExpression, hydra.java.syntax.AssignmentExpressionConditional(cast(hydra.java.syntax.ConditionalExpression, hydra.java.syntax.ConditionalExpressionSimple(hydra.java.syntax.ConditionalOrExpression((hydra.java.syntax.ConditionalAndExpression((hydra.java.syntax.InclusiveOrExpression((hydra.java.syntax.ExclusiveOrExpression((hydra.java.syntax.AndExpression((cast(hydra.java.syntax.EqualityExpression, hydra.java.syntax.EqualityExpressionUnary(cast(hydra.java.syntax.RelationalExpression, hydra.java.syntax.RelationalExpressionSimple(cast(hydra.java.syntax.ShiftExpression, hydra.java.syntax.ShiftExpressionUnary(cast(hydra.java.syntax.AdditiveExpression, hydra.java.syntax.AdditiveExpressionUnary(cast(hydra.java.syntax.MultiplicativeExpression, hydra.java.syntax.MultiplicativeExpressionUnary(cast(hydra.java.syntax.UnaryExpression, hydra.java.syntax.UnaryExpressionOther(cast(hydra.java.syntax.UnaryExpressionNotPlusMinus, hydra.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.java.syntax.PostfixExpression, hydra.java.syntax.PostfixExpressionName(en)))))))))))))))),)),)),)),)),))))))))

def java_expression_to_java_primary(e: hydra.java.syntax.Expression):
    r"""Convert an Expression to a Primary, avoiding unnecessary parentheses when the expression is already a simple primary chain."""

    @lru_cache(1)
    def fallback() -> hydra.java.syntax.Primary:
        return cast(hydra.java.syntax.Primary, hydra.java.syntax.PrimaryNoNewArray(cast(hydra.java.syntax.PrimaryNoNewArrayExpression, hydra.java.syntax.PrimaryNoNewArrayExpressionParens(e))))
    def _hoist_fallback_body_1(v1):
        match v1:
            case hydra.java.syntax.ConditionalExpressionSimple(value=cor):
                cands = cor.value
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(cands), 1), (lambda : (iors := hydra.lib.lists.head(cands).value, hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(iors), 1), (lambda : (xors := hydra.lib.lists.head(iors).value, hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(xors), 1), (lambda : (ands := hydra.lib.lists.head(xors).value, hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(ands), 1), (lambda : (eqs := hydra.lib.lists.head(ands).value, (_hoist_eqs_body_1 := (lambda v12: (lambda p: p)(v12.value) if isinstance(v12, hydra.java.syntax.PostfixExpressionPrimary) else fallback()), _hoist_eqs_body_2 := (lambda v12: (lambda pf: _hoist_eqs_body_1(pf))(v12.value) if isinstance(v12, hydra.java.syntax.UnaryExpressionNotPlusMinusPostfix) else fallback()), _hoist_eqs_body_3 := (lambda v12: (lambda npm: _hoist_eqs_body_2(npm))(v12.value) if isinstance(v12, hydra.java.syntax.UnaryExpressionOther) else fallback()), _hoist_eqs_body_4 := (lambda v12: (lambda unary: _hoist_eqs_body_3(unary))(v12.value) if isinstance(v12, hydra.java.syntax.MultiplicativeExpressionUnary) else fallback()), _hoist_eqs_body_5 := (lambda v12: (lambda mul: _hoist_eqs_body_4(mul))(v12.value) if isinstance(v12, hydra.java.syntax.AdditiveExpressionUnary) else fallback()), _hoist_eqs_body_6 := (lambda v12: (lambda add: _hoist_eqs_body_5(add))(v12.value) if isinstance(v12, hydra.java.syntax.ShiftExpressionUnary) else fallback()), _hoist_eqs_body_7 := (lambda v12: (lambda shift: _hoist_eqs_body_6(shift))(v12.value) if isinstance(v12, hydra.java.syntax.RelationalExpressionSimple) else fallback()), _hoist_eqs_body_8 := (lambda v12: (lambda rel: _hoist_eqs_body_7(rel))(v12.value) if isinstance(v12, hydra.java.syntax.EqualityExpressionUnary) else fallback()), hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(eqs), 1), (lambda : _hoist_eqs_body_8(hydra.lib.lists.head(eqs))), (lambda : fallback())))[8])[1]), (lambda : fallback())))[1]), (lambda : fallback())))[1]), (lambda : fallback())))[1]), (lambda : fallback()))

            case _:
                return fallback()
    def _hoist_fallback_body_2(v1):
        match v1:
            case hydra.java.syntax.AssignmentExpressionConditional(value=ce):
                return _hoist_fallback_body_1(ce)

            case _:
                return fallback()
    match e:
        case hydra.java.syntax.ExpressionAssignment(value=ae):
            return _hoist_fallback_body_2(ae)

        case _:
            return fallback()

def java_field_access_to_java_expression(fa: hydra.java.syntax.FieldAccess) -> hydra.java.syntax.Expression:
    return cast(hydra.java.syntax.Expression, hydra.java.syntax.ExpressionAssignment(cast(hydra.java.syntax.AssignmentExpression, hydra.java.syntax.AssignmentExpressionConditional(cast(hydra.java.syntax.ConditionalExpression, hydra.java.syntax.ConditionalExpressionSimple(hydra.java.syntax.ConditionalOrExpression((hydra.java.syntax.ConditionalAndExpression((hydra.java.syntax.InclusiveOrExpression((hydra.java.syntax.ExclusiveOrExpression((hydra.java.syntax.AndExpression((cast(hydra.java.syntax.EqualityExpression, hydra.java.syntax.EqualityExpressionUnary(cast(hydra.java.syntax.RelationalExpression, hydra.java.syntax.RelationalExpressionSimple(cast(hydra.java.syntax.ShiftExpression, hydra.java.syntax.ShiftExpressionUnary(cast(hydra.java.syntax.AdditiveExpression, hydra.java.syntax.AdditiveExpressionUnary(cast(hydra.java.syntax.MultiplicativeExpression, hydra.java.syntax.MultiplicativeExpressionUnary(cast(hydra.java.syntax.UnaryExpression, hydra.java.syntax.UnaryExpressionOther(cast(hydra.java.syntax.UnaryExpressionNotPlusMinus, hydra.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.java.syntax.PostfixExpression, hydra.java.syntax.PostfixExpressionPrimary(cast(hydra.java.syntax.Primary, hydra.java.syntax.PrimaryNoNewArray(cast(hydra.java.syntax.PrimaryNoNewArrayExpression, hydra.java.syntax.PrimaryNoNewArrayExpressionFieldAccess(fa)))))))))))))))))))),)),)),)),)),))))))))

def java_identifier_to_java_expression(id: hydra.java.syntax.Identifier) -> hydra.java.syntax.Expression:
    return cast(hydra.java.syntax.Expression, hydra.java.syntax.ExpressionAssignment(cast(hydra.java.syntax.AssignmentExpression, hydra.java.syntax.AssignmentExpressionConditional(cast(hydra.java.syntax.ConditionalExpression, hydra.java.syntax.ConditionalExpressionSimple(hydra.java.syntax.ConditionalOrExpression((hydra.java.syntax.ConditionalAndExpression((hydra.java.syntax.InclusiveOrExpression((hydra.java.syntax.ExclusiveOrExpression((hydra.java.syntax.AndExpression((cast(hydra.java.syntax.EqualityExpression, hydra.java.syntax.EqualityExpressionUnary(cast(hydra.java.syntax.RelationalExpression, hydra.java.syntax.RelationalExpressionSimple(cast(hydra.java.syntax.ShiftExpression, hydra.java.syntax.ShiftExpressionUnary(cast(hydra.java.syntax.AdditiveExpression, hydra.java.syntax.AdditiveExpressionUnary(cast(hydra.java.syntax.MultiplicativeExpression, hydra.java.syntax.MultiplicativeExpressionUnary(cast(hydra.java.syntax.UnaryExpression, hydra.java.syntax.UnaryExpressionOther(cast(hydra.java.syntax.UnaryExpressionNotPlusMinus, hydra.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.java.syntax.PostfixExpression, hydra.java.syntax.PostfixExpressionName(hydra.java.syntax.ExpressionName(Nothing(), id))))))))))))))))),)),)),)),)),))))))))

def java_identifier_to_java_relational_expression(id: hydra.java.syntax.Identifier) -> hydra.java.syntax.RelationalExpression:
    return cast(hydra.java.syntax.RelationalExpression, hydra.java.syntax.RelationalExpressionSimple(cast(hydra.java.syntax.ShiftExpression, hydra.java.syntax.ShiftExpressionUnary(cast(hydra.java.syntax.AdditiveExpression, hydra.java.syntax.AdditiveExpressionUnary(cast(hydra.java.syntax.MultiplicativeExpression, hydra.java.syntax.MultiplicativeExpressionUnary(cast(hydra.java.syntax.UnaryExpression, hydra.java.syntax.UnaryExpressionOther(cast(hydra.java.syntax.UnaryExpressionNotPlusMinus, hydra.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.java.syntax.PostfixExpression, hydra.java.syntax.PostfixExpressionName(hydra.java.syntax.ExpressionName(Nothing(), id)))))))))))))))

def java_identifier_to_java_unary_expression(id: hydra.java.syntax.Identifier) -> hydra.java.syntax.UnaryExpression:
    return cast(hydra.java.syntax.UnaryExpression, hydra.java.syntax.UnaryExpressionOther(cast(hydra.java.syntax.UnaryExpressionNotPlusMinus, hydra.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.java.syntax.PostfixExpression, hydra.java.syntax.PostfixExpressionName(hydra.java.syntax.ExpressionName(Nothing(), id)))))))

def java_instance_of(lhs: hydra.java.syntax.RelationalExpression, rhs: hydra.java.syntax.ReferenceType) -> hydra.java.syntax.RelationalExpression:
    return cast(hydra.java.syntax.RelationalExpression, hydra.java.syntax.RelationalExpressionInstanceof(hydra.java.syntax.RelationalExpression_InstanceOf(lhs, rhs)))

def java_int(i: int) -> hydra.java.syntax.Literal:
    return cast(hydra.java.syntax.Literal, hydra.java.syntax.LiteralInteger(hydra.java.syntax.IntegerLiteral(i)))

def java_int_expression(i: int) -> hydra.java.syntax.Expression:
    return java_primary_to_java_expression(java_literal_to_java_primary(java_int(i)))

@lru_cache(1)
def java_int_type() -> hydra.java.syntax.Type:
    return java_primitive_type_to_java_type(cast(hydra.java.syntax.PrimitiveType, hydra.java.syntax.PrimitiveTypeNumeric(cast(hydra.java.syntax.NumericType, hydra.java.syntax.NumericTypeIntegral(hydra.java.syntax.IntegralType.INT)))))

def java_interface_declaration_to_java_class_body_declaration(nid: hydra.java.syntax.NormalInterfaceDeclaration) -> hydra.java.syntax.ClassBodyDeclaration:
    return cast(hydra.java.syntax.ClassBodyDeclaration, hydra.java.syntax.ClassBodyDeclarationClassMember(cast(hydra.java.syntax.ClassMemberDeclaration, hydra.java.syntax.ClassMemberDeclarationInterface(cast(hydra.java.syntax.InterfaceDeclaration, hydra.java.syntax.InterfaceDeclarationNormalInterface(nid))))))

def variable_to_java_identifier(name: hydra.core.Name) -> hydra.java.syntax.Identifier:
    v = name.value
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(v, "_"), (lambda : hydra.java.syntax.Identifier("ignored")), (lambda : hydra.java.syntax.Identifier(sanitize_java_name(v))))

def java_lambda(v: hydra.core.Name, body: hydra.java.syntax.Expression) -> hydra.java.syntax.Expression:
    return cast(hydra.java.syntax.Expression, hydra.java.syntax.ExpressionLambda(hydra.java.syntax.LambdaExpression(cast(hydra.java.syntax.LambdaParameters, hydra.java.syntax.LambdaParametersSingle(variable_to_java_identifier(v))), cast(hydra.java.syntax.LambdaBody, hydra.java.syntax.LambdaBodyExpression(body)))))

def java_lambda_from_block(v: hydra.core.Name, block: hydra.java.syntax.Block) -> hydra.java.syntax.Expression:
    return cast(hydra.java.syntax.Expression, hydra.java.syntax.ExpressionLambda(hydra.java.syntax.LambdaExpression(cast(hydra.java.syntax.LambdaParameters, hydra.java.syntax.LambdaParametersSingle(variable_to_java_identifier(v))), cast(hydra.java.syntax.LambdaBody, hydra.java.syntax.LambdaBodyBlock(block)))))

def java_literal_to_java_expression(lit: hydra.java.syntax.Literal) -> hydra.java.syntax.Expression:
    return cast(hydra.java.syntax.Expression, hydra.java.syntax.ExpressionAssignment(cast(hydra.java.syntax.AssignmentExpression, hydra.java.syntax.AssignmentExpressionConditional(cast(hydra.java.syntax.ConditionalExpression, hydra.java.syntax.ConditionalExpressionSimple(hydra.java.syntax.ConditionalOrExpression((hydra.java.syntax.ConditionalAndExpression((hydra.java.syntax.InclusiveOrExpression((hydra.java.syntax.ExclusiveOrExpression((hydra.java.syntax.AndExpression((cast(hydra.java.syntax.EqualityExpression, hydra.java.syntax.EqualityExpressionUnary(cast(hydra.java.syntax.RelationalExpression, hydra.java.syntax.RelationalExpressionSimple(cast(hydra.java.syntax.ShiftExpression, hydra.java.syntax.ShiftExpressionUnary(cast(hydra.java.syntax.AdditiveExpression, hydra.java.syntax.AdditiveExpressionUnary(cast(hydra.java.syntax.MultiplicativeExpression, hydra.java.syntax.MultiplicativeExpressionUnary(cast(hydra.java.syntax.UnaryExpression, hydra.java.syntax.UnaryExpressionOther(cast(hydra.java.syntax.UnaryExpressionNotPlusMinus, hydra.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.java.syntax.PostfixExpression, hydra.java.syntax.PostfixExpressionPrimary(cast(hydra.java.syntax.Primary, hydra.java.syntax.PrimaryNoNewArray(cast(hydra.java.syntax.PrimaryNoNewArrayExpression, hydra.java.syntax.PrimaryNoNewArrayExpressionLiteral(lit)))))))))))))))))))),)),)),)),)),))))))))

def java_literal_to_java_multiplicative_expression(lit: hydra.java.syntax.Literal) -> hydra.java.syntax.MultiplicativeExpression:
    return cast(hydra.java.syntax.MultiplicativeExpression, hydra.java.syntax.MultiplicativeExpressionUnary(cast(hydra.java.syntax.UnaryExpression, hydra.java.syntax.UnaryExpressionOther(cast(hydra.java.syntax.UnaryExpressionNotPlusMinus, hydra.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.java.syntax.PostfixExpression, hydra.java.syntax.PostfixExpressionPrimary(cast(hydra.java.syntax.Primary, hydra.java.syntax.PrimaryNoNewArray(cast(hydra.java.syntax.PrimaryNoNewArrayExpression, hydra.java.syntax.PrimaryNoNewArrayExpressionLiteral(lit))))))))))))

def java_member_field(mods: frozenlist[hydra.java.syntax.FieldModifier], jt: hydra.java.syntax.Type, v: hydra.java.syntax.VariableDeclarator) -> hydra.java.syntax.ClassBodyDeclaration:
    return cast(hydra.java.syntax.ClassBodyDeclaration, hydra.java.syntax.ClassBodyDeclarationClassMember(cast(hydra.java.syntax.ClassMemberDeclaration, hydra.java.syntax.ClassMemberDeclarationField(hydra.java.syntax.FieldDeclaration(mods, hydra.java.syntax.UnannType(jt), (v,))))))

def java_method_declaration_to_java_class_body_declaration(md: hydra.java.syntax.MethodDeclaration) -> hydra.java.syntax.ClassBodyDeclaration:
    return cast(hydra.java.syntax.ClassBodyDeclaration, hydra.java.syntax.ClassBodyDeclarationClassMember(cast(hydra.java.syntax.ClassMemberDeclaration, hydra.java.syntax.ClassMemberDeclarationMethod(md))))

def java_method_invocation_to_java_expression(mi: hydra.java.syntax.MethodInvocation) -> hydra.java.syntax.Expression:
    return cast(hydra.java.syntax.Expression, hydra.java.syntax.ExpressionAssignment(cast(hydra.java.syntax.AssignmentExpression, hydra.java.syntax.AssignmentExpressionConditional(cast(hydra.java.syntax.ConditionalExpression, hydra.java.syntax.ConditionalExpressionSimple(hydra.java.syntax.ConditionalOrExpression((hydra.java.syntax.ConditionalAndExpression((hydra.java.syntax.InclusiveOrExpression((hydra.java.syntax.ExclusiveOrExpression((hydra.java.syntax.AndExpression((cast(hydra.java.syntax.EqualityExpression, hydra.java.syntax.EqualityExpressionUnary(cast(hydra.java.syntax.RelationalExpression, hydra.java.syntax.RelationalExpressionSimple(cast(hydra.java.syntax.ShiftExpression, hydra.java.syntax.ShiftExpressionUnary(cast(hydra.java.syntax.AdditiveExpression, hydra.java.syntax.AdditiveExpressionUnary(cast(hydra.java.syntax.MultiplicativeExpression, hydra.java.syntax.MultiplicativeExpressionUnary(cast(hydra.java.syntax.UnaryExpression, hydra.java.syntax.UnaryExpressionOther(cast(hydra.java.syntax.UnaryExpressionNotPlusMinus, hydra.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.java.syntax.PostfixExpression, hydra.java.syntax.PostfixExpressionPrimary(cast(hydra.java.syntax.Primary, hydra.java.syntax.PrimaryNoNewArray(cast(hydra.java.syntax.PrimaryNoNewArrayExpression, hydra.java.syntax.PrimaryNoNewArrayExpressionMethodInvocation(mi)))))))))))))))))))),)),)),)),)),))))))))

def java_method_invocation_to_java_postfix_expression(mi: hydra.java.syntax.MethodInvocation) -> hydra.java.syntax.PostfixExpression:
    return cast(hydra.java.syntax.PostfixExpression, hydra.java.syntax.PostfixExpressionPrimary(cast(hydra.java.syntax.Primary, hydra.java.syntax.PrimaryNoNewArray(cast(hydra.java.syntax.PrimaryNoNewArrayExpression, hydra.java.syntax.PrimaryNoNewArrayExpressionMethodInvocation(mi))))))

def java_method_invocation_to_java_primary(mi: hydra.java.syntax.MethodInvocation) -> hydra.java.syntax.Primary:
    return cast(hydra.java.syntax.Primary, hydra.java.syntax.PrimaryNoNewArray(cast(hydra.java.syntax.PrimaryNoNewArrayExpression, hydra.java.syntax.PrimaryNoNewArrayExpressionMethodInvocation(mi))))

def java_method_invocation_to_java_statement(mi: hydra.java.syntax.MethodInvocation) -> hydra.java.syntax.Statement:
    return cast(hydra.java.syntax.Statement, hydra.java.syntax.StatementWithoutTrailing(cast(hydra.java.syntax.StatementWithoutTrailingSubstatement, hydra.java.syntax.StatementWithoutTrailingSubstatementExpression(hydra.java.syntax.ExpressionStatement(cast(hydra.java.syntax.StatementExpression, hydra.java.syntax.StatementExpressionMethodInvocation(mi)))))))

def java_multiplicative_expression_to_java_relational_expression(me: hydra.java.syntax.MultiplicativeExpression) -> hydra.java.syntax.RelationalExpression:
    return cast(hydra.java.syntax.RelationalExpression, hydra.java.syntax.RelationalExpressionSimple(cast(hydra.java.syntax.ShiftExpression, hydra.java.syntax.ShiftExpressionUnary(cast(hydra.java.syntax.AdditiveExpression, hydra.java.syntax.AdditiveExpressionUnary(me))))))

def java_package_declaration(ns: hydra.packaging.Namespace) -> hydra.java.syntax.PackageDeclaration:
    return hydra.java.syntax.PackageDeclaration((), hydra.lib.lists.map((lambda s: hydra.java.syntax.Identifier(s)), hydra.lib.strings.split_on(".", ns.value)))

def java_postfix_expression_to_java_equality_expression(pe: hydra.java.syntax.PostfixExpression) -> hydra.java.syntax.EqualityExpression:
    return cast(hydra.java.syntax.EqualityExpression, hydra.java.syntax.EqualityExpressionUnary(cast(hydra.java.syntax.RelationalExpression, hydra.java.syntax.RelationalExpressionSimple(cast(hydra.java.syntax.ShiftExpression, hydra.java.syntax.ShiftExpressionUnary(cast(hydra.java.syntax.AdditiveExpression, hydra.java.syntax.AdditiveExpressionUnary(cast(hydra.java.syntax.MultiplicativeExpression, hydra.java.syntax.MultiplicativeExpressionUnary(cast(hydra.java.syntax.UnaryExpression, hydra.java.syntax.UnaryExpressionOther(cast(hydra.java.syntax.UnaryExpressionNotPlusMinus, hydra.java.syntax.UnaryExpressionNotPlusMinusPostfix(pe))))))))))))))

def java_postfix_expression_to_java_expression(pe: hydra.java.syntax.PostfixExpression) -> hydra.java.syntax.Expression:
    return cast(hydra.java.syntax.Expression, hydra.java.syntax.ExpressionAssignment(cast(hydra.java.syntax.AssignmentExpression, hydra.java.syntax.AssignmentExpressionConditional(cast(hydra.java.syntax.ConditionalExpression, hydra.java.syntax.ConditionalExpressionSimple(hydra.java.syntax.ConditionalOrExpression((hydra.java.syntax.ConditionalAndExpression((hydra.java.syntax.InclusiveOrExpression((hydra.java.syntax.ExclusiveOrExpression((hydra.java.syntax.AndExpression((cast(hydra.java.syntax.EqualityExpression, hydra.java.syntax.EqualityExpressionUnary(cast(hydra.java.syntax.RelationalExpression, hydra.java.syntax.RelationalExpressionSimple(cast(hydra.java.syntax.ShiftExpression, hydra.java.syntax.ShiftExpressionUnary(cast(hydra.java.syntax.AdditiveExpression, hydra.java.syntax.AdditiveExpressionUnary(cast(hydra.java.syntax.MultiplicativeExpression, hydra.java.syntax.MultiplicativeExpressionUnary(cast(hydra.java.syntax.UnaryExpression, hydra.java.syntax.UnaryExpressionOther(cast(hydra.java.syntax.UnaryExpressionNotPlusMinus, hydra.java.syntax.UnaryExpressionNotPlusMinusPostfix(pe)))))))))))))),)),)),)),)),))))))))

def java_postfix_expression_to_java_inclusive_or_expression(pe: hydra.java.syntax.PostfixExpression) -> hydra.java.syntax.InclusiveOrExpression:
    return hydra.java.syntax.InclusiveOrExpression((hydra.java.syntax.ExclusiveOrExpression((hydra.java.syntax.AndExpression((cast(hydra.java.syntax.EqualityExpression, hydra.java.syntax.EqualityExpressionUnary(cast(hydra.java.syntax.RelationalExpression, hydra.java.syntax.RelationalExpressionSimple(cast(hydra.java.syntax.ShiftExpression, hydra.java.syntax.ShiftExpressionUnary(cast(hydra.java.syntax.AdditiveExpression, hydra.java.syntax.AdditiveExpressionUnary(cast(hydra.java.syntax.MultiplicativeExpression, hydra.java.syntax.MultiplicativeExpressionUnary(cast(hydra.java.syntax.UnaryExpression, hydra.java.syntax.UnaryExpressionOther(cast(hydra.java.syntax.UnaryExpressionNotPlusMinus, hydra.java.syntax.UnaryExpressionNotPlusMinusPostfix(pe)))))))))))))),)),)),))

def java_postfix_expression_to_java_relational_expression(pe: hydra.java.syntax.PostfixExpression) -> hydra.java.syntax.RelationalExpression:
    return cast(hydra.java.syntax.RelationalExpression, hydra.java.syntax.RelationalExpressionSimple(cast(hydra.java.syntax.ShiftExpression, hydra.java.syntax.ShiftExpressionUnary(cast(hydra.java.syntax.AdditiveExpression, hydra.java.syntax.AdditiveExpressionUnary(cast(hydra.java.syntax.MultiplicativeExpression, hydra.java.syntax.MultiplicativeExpressionUnary(cast(hydra.java.syntax.UnaryExpression, hydra.java.syntax.UnaryExpressionOther(cast(hydra.java.syntax.UnaryExpressionNotPlusMinus, hydra.java.syntax.UnaryExpressionNotPlusMinusPostfix(pe))))))))))))

def java_postfix_expression_to_java_unary_expression(pe: hydra.java.syntax.PostfixExpression) -> hydra.java.syntax.UnaryExpression:
    return cast(hydra.java.syntax.UnaryExpression, hydra.java.syntax.UnaryExpressionOther(cast(hydra.java.syntax.UnaryExpressionNotPlusMinus, hydra.java.syntax.UnaryExpressionNotPlusMinusPostfix(pe))))

def java_primary_to_java_unary_expression(p: hydra.java.syntax.Primary) -> hydra.java.syntax.UnaryExpression:
    return cast(hydra.java.syntax.UnaryExpression, hydra.java.syntax.UnaryExpressionOther(cast(hydra.java.syntax.UnaryExpressionNotPlusMinus, hydra.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.java.syntax.PostfixExpression, hydra.java.syntax.PostfixExpressionPrimary(p))))))

def java_ref_type(args: frozenlist[hydra.java.syntax.ReferenceType], pkg: Maybe[hydra.java.syntax.PackageName], id: str) -> hydra.java.syntax.Type:
    return cast(hydra.java.syntax.Type, hydra.java.syntax.TypeReference(cast(hydra.java.syntax.ReferenceType, hydra.java.syntax.ReferenceTypeClassOrInterface(cast(hydra.java.syntax.ClassOrInterfaceType, hydra.java.syntax.ClassOrInterfaceTypeClass(java_class_type(args, pkg, id)))))))

def java_reference_type_to_raw_type(rt: hydra.java.syntax.ReferenceType):
    def _hoist_hydra_java_utils_java_reference_type_to_raw_type_1(v1):
        match v1:
            case hydra.java.syntax.ClassOrInterfaceTypeClass(value=ct):
                anns = ct.annotations
                qual = ct.qualifier
                id = ct.identifier
                return cast(hydra.java.syntax.ReferenceType, hydra.java.syntax.ReferenceTypeClassOrInterface(cast(hydra.java.syntax.ClassOrInterfaceType, hydra.java.syntax.ClassOrInterfaceTypeClass(hydra.java.syntax.ClassType(anns, qual, id, ())))))

            case hydra.java.syntax.ClassOrInterfaceTypeInterface(value=it):
                ct = it.value
                anns = ct.annotations
                qual = ct.qualifier
                id = ct.identifier
                return cast(hydra.java.syntax.ReferenceType, hydra.java.syntax.ReferenceTypeClassOrInterface(cast(hydra.java.syntax.ClassOrInterfaceType, hydra.java.syntax.ClassOrInterfaceTypeInterface(hydra.java.syntax.InterfaceType(hydra.java.syntax.ClassType(anns, qual, id, ()))))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    match rt:
        case hydra.java.syntax.ReferenceTypeClassOrInterface(value=cit):
            return _hoist_hydra_java_utils_java_reference_type_to_raw_type_1(cit)

        case _:
            return rt

def java_relational_expression_to_java_equality_expression(re: hydra.java.syntax.RelationalExpression) -> hydra.java.syntax.EqualityExpression:
    return cast(hydra.java.syntax.EqualityExpression, hydra.java.syntax.EqualityExpressionUnary(re))

def java_relational_expression_to_java_expression(re: hydra.java.syntax.RelationalExpression) -> hydra.java.syntax.Expression:
    return java_equality_expression_to_java_expression(cast(hydra.java.syntax.EqualityExpression, hydra.java.syntax.EqualityExpressionUnary(re)))

def java_relational_expression_to_java_unary_expression(re: hydra.java.syntax.RelationalExpression) -> hydra.java.syntax.UnaryExpression:
    return cast(hydra.java.syntax.UnaryExpression, hydra.java.syntax.UnaryExpressionOther(cast(hydra.java.syntax.UnaryExpressionNotPlusMinus, hydra.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.java.syntax.PostfixExpression, hydra.java.syntax.PostfixExpressionPrimary(cast(hydra.java.syntax.Primary, hydra.java.syntax.PrimaryNoNewArray(cast(hydra.java.syntax.PrimaryNoNewArrayExpression, hydra.java.syntax.PrimaryNoNewArrayExpressionParens(cast(hydra.java.syntax.Expression, hydra.java.syntax.ExpressionAssignment(cast(hydra.java.syntax.AssignmentExpression, hydra.java.syntax.AssignmentExpressionConditional(cast(hydra.java.syntax.ConditionalExpression, hydra.java.syntax.ConditionalExpressionSimple(hydra.java.syntax.ConditionalOrExpression((hydra.java.syntax.ConditionalAndExpression((hydra.java.syntax.InclusiveOrExpression((hydra.java.syntax.ExclusiveOrExpression((hydra.java.syntax.AndExpression((cast(hydra.java.syntax.EqualityExpression, hydra.java.syntax.EqualityExpressionUnary(re)),)),)),)),)),))))))))))))))))))

def java_return_statement(mex: Maybe[hydra.java.syntax.Expression]) -> hydra.java.syntax.Statement:
    return cast(hydra.java.syntax.Statement, hydra.java.syntax.StatementWithoutTrailing(cast(hydra.java.syntax.StatementWithoutTrailingSubstatement, hydra.java.syntax.StatementWithoutTrailingSubstatementReturn(hydra.java.syntax.ReturnStatement(mex)))))

def java_statements_to_block(stmts: frozenlist[hydra.java.syntax.Statement]) -> hydra.java.syntax.Block:
    return hydra.java.syntax.Block(hydra.lib.lists.map((lambda s: cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(s))), stmts))

def java_string(s: str) -> hydra.java.syntax.Literal:
    return cast(hydra.java.syntax.Literal, hydra.java.syntax.LiteralString(hydra.java.syntax.StringLiteral(s)))

def java_string_multiplicative_expression(s: str) -> hydra.java.syntax.MultiplicativeExpression:
    return java_literal_to_java_multiplicative_expression(java_string(s))

java_this = cast(hydra.java.syntax.Expression, hydra.java.syntax.ExpressionAssignment(cast(hydra.java.syntax.AssignmentExpression, hydra.java.syntax.AssignmentExpressionConditional(cast(hydra.java.syntax.ConditionalExpression, hydra.java.syntax.ConditionalExpressionSimple(hydra.java.syntax.ConditionalOrExpression((hydra.java.syntax.ConditionalAndExpression((hydra.java.syntax.InclusiveOrExpression((hydra.java.syntax.ExclusiveOrExpression((hydra.java.syntax.AndExpression((cast(hydra.java.syntax.EqualityExpression, hydra.java.syntax.EqualityExpressionUnary(cast(hydra.java.syntax.RelationalExpression, hydra.java.syntax.RelationalExpressionSimple(cast(hydra.java.syntax.ShiftExpression, hydra.java.syntax.ShiftExpressionUnary(cast(hydra.java.syntax.AdditiveExpression, hydra.java.syntax.AdditiveExpressionUnary(cast(hydra.java.syntax.MultiplicativeExpression, hydra.java.syntax.MultiplicativeExpressionUnary(cast(hydra.java.syntax.UnaryExpression, hydra.java.syntax.UnaryExpressionOther(cast(hydra.java.syntax.UnaryExpressionNotPlusMinus, hydra.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.java.syntax.PostfixExpression, hydra.java.syntax.PostfixExpressionPrimary(cast(hydra.java.syntax.Primary, hydra.java.syntax.PrimaryNoNewArray(cast(hydra.java.syntax.PrimaryNoNewArrayExpression, hydra.java.syntax.PrimaryNoNewArrayExpressionThis()))))))))))))))))))),)),)),)),)),))))))))

def java_throw_statement(e: hydra.java.syntax.Expression) -> hydra.java.syntax.Statement:
    return cast(hydra.java.syntax.Statement, hydra.java.syntax.StatementWithoutTrailing(cast(hydra.java.syntax.StatementWithoutTrailingSubstatement, hydra.java.syntax.StatementWithoutTrailingSubstatementThrow(hydra.java.syntax.ThrowStatement(e)))))

def java_throw_illegal_argument_exception(args: frozenlist[hydra.java.syntax.Expression]) -> hydra.java.syntax.Statement:
    return java_throw_statement(java_constructor_call(java_constructor_name(hydra.java.syntax.Identifier("IllegalArgumentException"), Nothing()), args, Nothing()))

def java_throw_illegal_state_exception(args: frozenlist[hydra.java.syntax.Expression]) -> hydra.java.syntax.Statement:
    return java_throw_statement(java_constructor_call(java_constructor_name(hydra.java.syntax.Identifier("IllegalStateException"), Nothing()), args, Nothing()))

def name_to_java_type_identifier(aliases: hydra.java.environment.Aliases, qualify: bool, name: hydra.core.Name) -> hydra.java.syntax.TypeIdentifier:
    return hydra.lib.pairs.first(name_to_qualified_java_name(aliases, qualify, name, Nothing()))

def java_type_from_type_name(aliases: hydra.java.environment.Aliases, el_name: hydra.core.Name) -> hydra.java.syntax.Type:
    return java_type_variable_to_type(hydra.java.syntax.TypeVariable((), name_to_java_type_identifier(aliases, False, el_name)))

def java_type_identifier_to_java_type_argument(id: hydra.java.syntax.TypeIdentifier) -> hydra.java.syntax.TypeArgument:
    return cast(hydra.java.syntax.TypeArgument, hydra.java.syntax.TypeArgumentReference(cast(hydra.java.syntax.ReferenceType, hydra.java.syntax.ReferenceTypeVariable(hydra.java.syntax.TypeVariable((), id)))))

def java_type_name(id: hydra.java.syntax.Identifier) -> hydra.java.syntax.TypeName:
    return hydra.java.syntax.TypeName(hydra.java.syntax.TypeIdentifier(id), Nothing())

def java_type_parameter(v: str) -> hydra.java.syntax.TypeParameter:
    return hydra.java.syntax.TypeParameter((), java_type_identifier(v), Nothing())

def java_type_to_java_formal_parameter(jt: hydra.java.syntax.Type, fname: hydra.core.Name) -> hydra.java.syntax.FormalParameter:
    return cast(hydra.java.syntax.FormalParameter, hydra.java.syntax.FormalParameterSimple(hydra.java.syntax.FormalParameter_Simple((), hydra.java.syntax.UnannType(jt), field_name_to_java_variable_declarator_id(fname))))

def java_type_to_java_reference_type(t: hydra.java.syntax.Type, cx: T0) -> Either[hydra.errors.Error, hydra.java.syntax.ReferenceType]:
    match t:
        case hydra.java.syntax.TypeReference(value=rt):
            return Right(rt)

        case hydra.java.syntax.TypePrimitive():
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("expected a Java reference type"))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def java_type_to_java_result(jt: hydra.java.syntax.Type) -> hydra.java.syntax.Result:
    return cast(hydra.java.syntax.Result, hydra.java.syntax.ResultType(hydra.java.syntax.UnannType(jt)))

def java_type_to_java_type_argument(t: hydra.java.syntax.Type) -> hydra.java.syntax.TypeArgument:
    match t:
        case hydra.java.syntax.TypeReference(value=rt):
            return cast(hydra.java.syntax.TypeArgument, hydra.java.syntax.TypeArgumentReference(rt))

        case hydra.java.syntax.TypePrimitive():
            return cast(hydra.java.syntax.TypeArgument, hydra.java.syntax.TypeArgumentWildcard(hydra.java.syntax.Wildcard((), Nothing())))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def java_type_variable(v: str) -> hydra.java.syntax.ReferenceType:
    return cast(hydra.java.syntax.ReferenceType, hydra.java.syntax.ReferenceTypeVariable(hydra.java.syntax.TypeVariable((), java_type_identifier(hydra.formatting.capitalize(v)))))

def java_unary_expression_to_java_expression(ue: hydra.java.syntax.UnaryExpression) -> hydra.java.syntax.Expression:
    return cast(hydra.java.syntax.Expression, hydra.java.syntax.ExpressionAssignment(cast(hydra.java.syntax.AssignmentExpression, hydra.java.syntax.AssignmentExpressionConditional(cast(hydra.java.syntax.ConditionalExpression, hydra.java.syntax.ConditionalExpressionSimple(hydra.java.syntax.ConditionalOrExpression((hydra.java.syntax.ConditionalAndExpression((hydra.java.syntax.InclusiveOrExpression((hydra.java.syntax.ExclusiveOrExpression((hydra.java.syntax.AndExpression((cast(hydra.java.syntax.EqualityExpression, hydra.java.syntax.EqualityExpressionUnary(cast(hydra.java.syntax.RelationalExpression, hydra.java.syntax.RelationalExpressionSimple(cast(hydra.java.syntax.ShiftExpression, hydra.java.syntax.ShiftExpressionUnary(cast(hydra.java.syntax.AdditiveExpression, hydra.java.syntax.AdditiveExpressionUnary(cast(hydra.java.syntax.MultiplicativeExpression, hydra.java.syntax.MultiplicativeExpressionUnary(ue)))))))))),)),)),)),)),))))))))

def java_unary_expression_to_java_relational_expression(ue: hydra.java.syntax.UnaryExpression) -> hydra.java.syntax.RelationalExpression:
    return cast(hydra.java.syntax.RelationalExpression, hydra.java.syntax.RelationalExpressionSimple(cast(hydra.java.syntax.ShiftExpression, hydra.java.syntax.ShiftExpressionUnary(cast(hydra.java.syntax.AdditiveExpression, hydra.java.syntax.AdditiveExpressionUnary(cast(hydra.java.syntax.MultiplicativeExpression, hydra.java.syntax.MultiplicativeExpressionUnary(ue))))))))

def lookup_java_var_name(aliases: hydra.java.environment.Aliases, name: hydra.core.Name) -> hydra.core.Name:
    return hydra.lib.maybes.cases(hydra.lib.maps.lookup(name, aliases.var_renames), (lambda : name), (lambda renamed: renamed))

def make_constructor(aliases: hydra.java.environment.Aliases, el_name: hydra.core.Name, private: bool, params: frozenlist[hydra.java.syntax.FormalParameter], stmts: frozenlist[hydra.java.syntax.BlockStatement]) -> hydra.java.syntax.ClassBodyDeclaration:
    @lru_cache(1)
    def nm() -> hydra.java.syntax.SimpleTypeName:
        return hydra.java.syntax.SimpleTypeName(name_to_java_type_identifier(aliases, False, el_name))
    @lru_cache(1)
    def cons() -> hydra.java.syntax.ConstructorDeclarator:
        return hydra.java.syntax.ConstructorDeclarator((), nm(), Nothing(), params)
    @lru_cache(1)
    def mods() -> frozenlist[hydra.java.syntax.ConstructorModifier]:
        return (hydra.lib.logic.if_else(private, (lambda : cast(hydra.java.syntax.ConstructorModifier, hydra.java.syntax.ConstructorModifierPrivate())), (lambda : cast(hydra.java.syntax.ConstructorModifier, hydra.java.syntax.ConstructorModifierPublic()))),)
    body = hydra.java.syntax.ConstructorBody(Nothing(), stmts)
    return cast(hydra.java.syntax.ClassBodyDeclaration, hydra.java.syntax.ClassBodyDeclarationConstructorDeclaration(hydra.java.syntax.ConstructorDeclaration(mods(), cons(), Nothing(), body)))

def method_declaration(mods: frozenlist[hydra.java.syntax.MethodModifier], tparams: frozenlist[hydra.java.syntax.TypeParameter], anns: frozenlist[hydra.java.syntax.Annotation], method_name: str, params: frozenlist[hydra.java.syntax.FormalParameter], result: hydra.java.syntax.Result, stmts: Maybe[frozenlist[hydra.java.syntax.BlockStatement]]) -> hydra.java.syntax.ClassBodyDeclaration:
    return java_method_declaration_to_java_class_body_declaration(hydra.java.syntax.MethodDeclaration(anns, mods, java_method_header(tparams, method_name, params, result), java_method_body(stmts)))

def method_invocation(lhs: Maybe[Either[hydra.java.syntax.ExpressionName, hydra.java.syntax.Primary]], method_name: hydra.java.syntax.Identifier, args: frozenlist[hydra.java.syntax.Expression]) -> hydra.java.syntax.MethodInvocation:
    @lru_cache(1)
    def header() -> hydra.java.syntax.MethodInvocation_Header:
        return hydra.lib.maybes.cases(lhs, (lambda : cast(hydra.java.syntax.MethodInvocation_Header, hydra.java.syntax.MethodInvocation_HeaderSimple(hydra.java.syntax.MethodName(method_name)))), (lambda either: cast(hydra.java.syntax.MethodInvocation_Header, hydra.java.syntax.MethodInvocation_HeaderComplex(hydra.java.syntax.MethodInvocation_Complex(hydra.lib.eithers.either((lambda en: cast(hydra.java.syntax.MethodInvocation_Variant, hydra.java.syntax.MethodInvocation_VariantExpression(en))), (lambda p: cast(hydra.java.syntax.MethodInvocation_Variant, hydra.java.syntax.MethodInvocation_VariantPrimary(p))), either), (), method_name)))))
    return hydra.java.syntax.MethodInvocation(header(), args)

def method_invocation_static(self: hydra.java.syntax.Identifier, method_name: hydra.java.syntax.Identifier, args: frozenlist[hydra.java.syntax.Expression]) -> hydra.java.syntax.MethodInvocation:
    return method_invocation(Just(Left(java_identifier_to_java_expression_name(self))), method_name, args)

def method_invocation_static_with_type_args(self: hydra.java.syntax.Identifier, method_name: hydra.java.syntax.Identifier, targs: frozenlist[hydra.java.syntax.TypeArgument], args: frozenlist[hydra.java.syntax.Expression]) -> hydra.java.syntax.MethodInvocation:
    @lru_cache(1)
    def header() -> hydra.java.syntax.MethodInvocation_Header:
        return cast(hydra.java.syntax.MethodInvocation_Header, hydra.java.syntax.MethodInvocation_HeaderComplex(hydra.java.syntax.MethodInvocation_Complex(cast(hydra.java.syntax.MethodInvocation_Variant, hydra.java.syntax.MethodInvocation_VariantExpression(java_identifier_to_java_expression_name(self))), targs, method_name)))
    return hydra.java.syntax.MethodInvocation(header(), args)

def name_to_java_name(aliases: hydra.java.environment.Aliases, name: hydra.core.Name) -> hydra.java.syntax.Identifier:
    @lru_cache(1)
    def qn() -> hydra.packaging.QualifiedName:
        return hydra.names.qualify_name(name)
    ns_ = qn().namespace
    local = qn().local
    return hydra.lib.logic.if_else(is_escaped(name.value), (lambda : hydra.java.syntax.Identifier(sanitize_java_name(local))), (lambda : hydra.lib.maybes.cases(ns_, (lambda : hydra.java.syntax.Identifier(local)), (lambda gname: (parts := hydra.lib.maybes.cases(hydra.lib.maps.lookup(gname, aliases.packages), (lambda : hydra.lib.strings.split_on(".", gname.value)), (lambda pkg_name: hydra.lib.lists.map((lambda i: i.value), pkg_name.value))), all_parts := hydra.lib.lists.concat2(parts, (sanitize_java_name(local),)), hydra.java.syntax.Identifier(hydra.lib.strings.intercalate(".", all_parts)))[2]))))

def name_to_java_reference_type(aliases: hydra.java.environment.Aliases, qualify: bool, args: frozenlist[hydra.java.syntax.TypeArgument], name: hydra.core.Name, mlocal: Maybe[str]) -> hydra.java.syntax.ReferenceType:
    return cast(hydra.java.syntax.ReferenceType, hydra.java.syntax.ReferenceTypeClassOrInterface(cast(hydra.java.syntax.ClassOrInterfaceType, hydra.java.syntax.ClassOrInterfaceTypeClass(name_to_java_class_type(aliases, qualify, args, name, mlocal)))))

@lru_cache(1)
def override_annotation() -> hydra.java.syntax.Annotation:
    return cast(hydra.java.syntax.Annotation, hydra.java.syntax.AnnotationMarker(hydra.java.syntax.MarkerAnnotation(java_type_name(hydra.java.syntax.Identifier("Override")))))

def reference_type_to_result(rt: hydra.java.syntax.ReferenceType) -> hydra.java.syntax.Result:
    return java_type_to_java_result(cast(hydra.java.syntax.Type, hydra.java.syntax.TypeReference(rt)))

@lru_cache(1)
def suppress_warnings_unchecked_annotation() -> hydra.java.syntax.Annotation:
    return cast(hydra.java.syntax.Annotation, hydra.java.syntax.AnnotationSingleElement(hydra.java.syntax.SingleElementAnnotation(java_type_name(hydra.java.syntax.Identifier("SuppressWarnings")), Just(cast(hydra.java.syntax.ElementValue, hydra.java.syntax.ElementValueConditionalExpression(cast(hydra.java.syntax.ConditionalExpression, hydra.java.syntax.ConditionalExpressionSimple(hydra.java.syntax.ConditionalOrExpression((hydra.java.syntax.ConditionalAndExpression((java_postfix_expression_to_java_inclusive_or_expression(cast(hydra.java.syntax.PostfixExpression, hydra.java.syntax.PostfixExpressionPrimary(java_literal_to_java_primary(java_string("unchecked"))))),)),))))))))))

def type_parameter_to_reference_type(tp: hydra.java.syntax.TypeParameter) -> hydra.java.syntax.ReferenceType:
    return java_type_variable(tp.identifier.value.value)

@lru_cache(1)
def visitor_type_variable() -> hydra.java.syntax.ReferenceType:
    return java_type_variable("r")

def to_accept_method(abstract: bool, vtparams: frozenlist[hydra.java.syntax.TypeParameter]) -> hydra.java.syntax.ClassBodyDeclaration:
    @lru_cache(1)
    def mods() -> frozenlist[hydra.java.syntax.MethodModifier]:
        return hydra.lib.logic.if_else(abstract, (lambda : (cast(hydra.java.syntax.MethodModifier, hydra.java.syntax.MethodModifierPublic()), cast(hydra.java.syntax.MethodModifier, hydra.java.syntax.MethodModifierAbstract()))), (lambda : (cast(hydra.java.syntax.MethodModifier, hydra.java.syntax.MethodModifierPublic()),)))
    @lru_cache(1)
    def tparams() -> frozenlist[hydra.java.syntax.TypeParameter]:
        return (java_type_parameter(hydra.java.names.visitor_return_parameter),)
    @lru_cache(1)
    def anns() -> frozenlist[hydra.java.syntax.Annotation]:
        return hydra.lib.logic.if_else(abstract, (lambda : ()), (lambda : (override_annotation(),)))
    @lru_cache(1)
    def type_args() -> frozenlist[hydra.java.syntax.TypeArgument]:
        return hydra.lib.lists.map((lambda tp: cast(hydra.java.syntax.TypeArgument, hydra.java.syntax.TypeArgumentReference(type_parameter_to_reference_type(tp)))), vtparams)
    @lru_cache(1)
    def ref() -> hydra.java.syntax.Type:
        return java_class_type_to_java_type(hydra.java.syntax.ClassType((), cast(hydra.java.syntax.ClassTypeQualifier, hydra.java.syntax.ClassTypeQualifierNone()), java_type_identifier(hydra.java.names.visitor_name), hydra.lib.lists.concat2(type_args(), (cast(hydra.java.syntax.TypeArgument, hydra.java.syntax.TypeArgumentReference(visitor_type_variable())),))))
    @lru_cache(1)
    def param() -> hydra.java.syntax.FormalParameter:
        return java_type_to_java_formal_parameter(ref(), hydra.core.Name("visitor"))
    @lru_cache(1)
    def result() -> hydra.java.syntax.Result:
        return java_type_to_java_result(cast(hydra.java.syntax.Type, hydra.java.syntax.TypeReference(visitor_type_variable())))
    @lru_cache(1)
    def return_expr() -> hydra.java.syntax.Expression:
        return java_method_invocation_to_java_expression(method_invocation_static(hydra.java.syntax.Identifier("visitor"), hydra.java.syntax.Identifier(hydra.java.names.visit_method_name), (java_this,)))
    @lru_cache(1)
    def body() -> Maybe[frozenlist[hydra.java.syntax.BlockStatement]]:
        return hydra.lib.logic.if_else(abstract, (lambda : Nothing()), (lambda : Just((cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementStatement(java_return_statement(Just(return_expr())))),))))
    return method_declaration(mods(), tparams(), anns(), hydra.java.names.accept_method_name, (param(),), result(), body())

def to_assign_stmt(fname: hydra.core.Name) -> hydra.java.syntax.Statement:
    @lru_cache(1)
    def id() -> hydra.java.syntax.Identifier:
        return field_name_to_java_identifier(fname)
    @lru_cache(1)
    def lhs() -> hydra.java.syntax.LeftHandSide:
        return cast(hydra.java.syntax.LeftHandSide, hydra.java.syntax.LeftHandSideFieldAccess(hydra.java.syntax.FieldAccess(cast(hydra.java.syntax.FieldAccess_Qualifier, hydra.java.syntax.FieldAccess_QualifierPrimary(cast(hydra.java.syntax.Primary, hydra.java.syntax.PrimaryNoNewArray(cast(hydra.java.syntax.PrimaryNoNewArrayExpression, hydra.java.syntax.PrimaryNoNewArrayExpressionThis()))))), id())))
    @lru_cache(1)
    def rhs() -> hydra.java.syntax.Expression:
        return field_name_to_java_expression(fname)
    return java_assignment_statement(lhs(), rhs())

def to_java_array_type(t: hydra.java.syntax.Type, cx: T0):
    def _hoist_hydra_java_utils_to_java_array_type_1(v1):
        match v1:
            case hydra.java.syntax.ReferenceTypeClassOrInterface(value=cit):
                return Right(cast(hydra.java.syntax.Type, hydra.java.syntax.TypeReference(cast(hydra.java.syntax.ReferenceType, hydra.java.syntax.ReferenceTypeArray(hydra.java.syntax.ArrayType(hydra.java.syntax.Dims(((),)), cast(hydra.java.syntax.ArrayType_Variant, hydra.java.syntax.ArrayType_VariantClassOrInterface(cit))))))))

            case hydra.java.syntax.ReferenceTypeArray(value=at):
                old_dims = at.dims.value
                @lru_cache(1)
                def new_dims() -> hydra.java.syntax.Dims:
                    return hydra.java.syntax.Dims(hydra.lib.lists.concat2(old_dims, ((),)))
                variant = at.variant
                return Right(cast(hydra.java.syntax.Type, hydra.java.syntax.TypeReference(cast(hydra.java.syntax.ReferenceType, hydra.java.syntax.ReferenceTypeArray(hydra.java.syntax.ArrayType(new_dims(), variant))))))

            case hydra.java.syntax.ReferenceTypeVariable():
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("don't know how to make Java reference type into array type"))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    match t:
        case hydra.java.syntax.TypeReference(value=rt):
            return _hoist_hydra_java_utils_to_java_array_type_1(rt)

        case hydra.java.syntax.TypePrimitive():
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("don't know how to make Java type into array type"))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def type_parameter_to_type_argument(tp: hydra.java.syntax.TypeParameter) -> hydra.java.syntax.TypeArgument:
    return java_type_identifier_to_java_type_argument(tp.identifier)

def un_type_parameter(tp: hydra.java.syntax.TypeParameter) -> str:
    return tp.identifier.value.value

def unique_var_name_go(aliases: hydra.java.environment.Aliases, base: str, n: int) -> hydra.core.Name:
    @lru_cache(1)
    def candidate() -> hydra.core.Name:
        return hydra.core.Name(hydra.lib.strings.cat2(base, hydra.lib.literals.show_int32(n)))
    return hydra.lib.logic.if_else(hydra.lib.sets.member(candidate(), aliases.in_scope_java_vars), (lambda : unique_var_name_go(aliases, base, hydra.lib.math.add(n, 1))), (lambda : candidate()))

def unique_var_name(aliases: hydra.java.environment.Aliases, name: hydra.core.Name) -> hydra.core.Name:
    return hydra.lib.logic.if_else(hydra.lib.sets.member(name, aliases.in_scope_java_vars), (lambda : unique_var_name_go(aliases, name.value, 2)), (lambda : name))

def var_declaration_statement(id: hydra.java.syntax.Identifier, rhs: hydra.java.syntax.Expression) -> hydra.java.syntax.BlockStatement:
    return cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementLocalVariableDeclaration(hydra.java.syntax.LocalVariableDeclarationStatement(hydra.java.syntax.LocalVariableDeclaration((), cast(hydra.java.syntax.LocalVariableType, hydra.java.syntax.LocalVariableTypeVar()), (java_variable_declarator(id, Just(cast(hydra.java.syntax.VariableInitializer, hydra.java.syntax.VariableInitializerExpression(rhs)))),)))))

def variable_declaration_statement(aliases: T0, jtype: hydra.java.syntax.Type, id: hydra.java.syntax.Identifier, rhs: hydra.java.syntax.Expression) -> hydra.java.syntax.BlockStatement:
    @lru_cache(1)
    def init_() -> hydra.java.syntax.VariableInitializer:
        return cast(hydra.java.syntax.VariableInitializer, hydra.java.syntax.VariableInitializerExpression(rhs))
    @lru_cache(1)
    def vdec() -> hydra.java.syntax.VariableDeclarator:
        return java_variable_declarator(id, Just(init_()))
    return cast(hydra.java.syntax.BlockStatement, hydra.java.syntax.BlockStatementLocalVariableDeclaration(hydra.java.syntax.LocalVariableDeclarationStatement(hydra.java.syntax.LocalVariableDeclaration((), cast(hydra.java.syntax.LocalVariableType, hydra.java.syntax.LocalVariableTypeType(hydra.java.syntax.UnannType(jtype))), (vdec(),)))))

def variant_class_name(qualify: bool, el_name: hydra.core.Name, fname: hydra.core.Name) -> hydra.core.Name:
    @lru_cache(1)
    def qn() -> hydra.packaging.QualifiedName:
        return hydra.names.qualify_name(el_name)
    ns_ = qn().namespace
    local = qn().local
    @lru_cache(1)
    def flocal() -> str:
        return hydra.formatting.capitalize(fname.value)
    @lru_cache(1)
    def local1() -> str:
        return hydra.lib.logic.if_else(qualify, (lambda : hydra.lib.strings.cat2(hydra.lib.strings.cat2(local, "."), flocal())), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(flocal(), local), (lambda : hydra.lib.strings.cat2(flocal(), "_")), (lambda : flocal()))))
    return hydra.names.unqualify_name(hydra.packaging.QualifiedName(ns_, local1()))
