# Note: this is an automatically generated file. Do not edit.

r"""Java utilities for constructing Java syntax trees."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Just, Left, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.ext.java.helpers
import hydra.ext.java.language
import hydra.ext.java.names
import hydra.ext.java.syntax
import hydra.formatting
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.module
import hydra.names

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def add_expressions(exprs: frozenlist[hydra.ext.java.syntax.MultiplicativeExpression]) -> hydra.ext.java.syntax.AdditiveExpression:
    @lru_cache(1)
    def first() -> hydra.ext.java.syntax.AdditiveExpression:
        return cast(hydra.ext.java.syntax.AdditiveExpression, hydra.ext.java.syntax.AdditiveExpressionUnary(hydra.lib.lists.head(exprs)))
    @lru_cache(1)
    def rest() -> frozenlist[hydra.ext.java.syntax.MultiplicativeExpression]:
        return hydra.lib.lists.tail(exprs)
    return hydra.lib.lists.foldl((lambda ae, me: cast(hydra.ext.java.syntax.AdditiveExpression, hydra.ext.java.syntax.AdditiveExpressionPlus(hydra.ext.java.syntax.AdditiveExpression_Binary(ae, me)))), first(), rest())

def add_in_scope_var(name: hydra.core.Name, aliases: hydra.ext.java.helpers.Aliases) -> hydra.ext.java.helpers.Aliases:
    return hydra.ext.java.helpers.Aliases(aliases.current_namespace, aliases.packages, aliases.branch_vars, aliases.recursive_vars, aliases.in_scope_type_params, aliases.polymorphic_locals, hydra.lib.sets.insert(name, aliases.in_scope_java_vars), aliases.var_renames, aliases.lambda_vars, aliases.type_var_subst, aliases.trusted_type_vars, aliases.method_codomain, aliases.thunked_vars)

def add_in_scope_vars(names: frozenlist[hydra.core.Name], aliases: hydra.ext.java.helpers.Aliases) -> hydra.ext.java.helpers.Aliases:
    return hydra.lib.lists.foldl((lambda a, n: add_in_scope_var(n, a)), aliases, names)

def java_type_variable_to_type(tv: hydra.ext.java.syntax.TypeVariable) -> hydra.ext.java.syntax.Type:
    return cast(hydra.ext.java.syntax.Type, hydra.ext.java.syntax.TypeReference(cast(hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.ReferenceTypeVariable(tv))))

def add_java_type_parameter(rt: hydra.ext.java.syntax.ReferenceType, t: hydra.ext.java.syntax.Type) -> hydra.compute.Flow[T0, hydra.ext.java.syntax.Type]:
    def _hoist_hydra_ext_java_utils_add_java_type_parameter_1(rt: hydra.ext.java.syntax.ReferenceType, v1: hydra.ext.java.syntax.ClassOrInterfaceType) -> hydra.compute.Flow[T1, hydra.ext.java.syntax.Type]:
        match v1:
            case hydra.ext.java.syntax.ClassOrInterfaceTypeClass(value=ct):
                @lru_cache(1)
                def anns() -> frozenlist[hydra.ext.java.syntax.Annotation]:
                    return ct.annotations
                @lru_cache(1)
                def qual() -> hydra.ext.java.syntax.ClassTypeQualifier:
                    return ct.qualifier
                @lru_cache(1)
                def id() -> hydra.ext.java.syntax.TypeIdentifier:
                    return ct.identifier
                @lru_cache(1)
                def args() -> frozenlist[hydra.ext.java.syntax.TypeArgument]:
                    return ct.arguments
                return hydra.lib.flows.pure(cast(hydra.ext.java.syntax.Type, hydra.ext.java.syntax.TypeReference(cast(hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.ReferenceTypeClassOrInterface(cast(hydra.ext.java.syntax.ClassOrInterfaceType, hydra.ext.java.syntax.ClassOrInterfaceTypeClass(hydra.ext.java.syntax.ClassType(anns(), qual(), id(), hydra.lib.lists.concat2(args(), (cast(hydra.ext.java.syntax.TypeArgument, hydra.ext.java.syntax.TypeArgumentReference(rt)),))))))))))
            
            case hydra.ext.java.syntax.ClassOrInterfaceTypeInterface():
                return hydra.lib.flows.fail("expected a Java class type")
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_hydra_ext_java_utils_add_java_type_parameter_2(rt: hydra.ext.java.syntax.ReferenceType, v1: hydra.ext.java.syntax.ReferenceType) -> hydra.compute.Flow[T1, hydra.ext.java.syntax.Type]:
        match v1:
            case hydra.ext.java.syntax.ReferenceTypeClassOrInterface(value=cit):
                return _hoist_hydra_ext_java_utils_add_java_type_parameter_1(rt, cit)
            
            case hydra.ext.java.syntax.ReferenceTypeVariable(value=tv):
                return hydra.lib.flows.pure(java_type_variable_to_type(tv))
            
            case hydra.ext.java.syntax.ReferenceTypeArray():
                return hydra.lib.flows.fail("expected a Java class or interface type, or a variable")
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    match t:
        case hydra.ext.java.syntax.TypeReference(value=rt1):
            return _hoist_hydra_ext_java_utils_add_java_type_parameter_2(rt, rt1)
        
        case hydra.ext.java.syntax.TypePrimitive():
            return hydra.lib.flows.fail("expected a reference type")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def add_var_rename(original: hydra.core.Name, renamed: hydra.core.Name, aliases: hydra.ext.java.helpers.Aliases) -> hydra.ext.java.helpers.Aliases:
    return hydra.ext.java.helpers.Aliases(aliases.current_namespace, aliases.packages, aliases.branch_vars, aliases.recursive_vars, aliases.in_scope_type_params, aliases.polymorphic_locals, aliases.in_scope_java_vars, hydra.lib.maps.insert(original, renamed, aliases.var_renames), aliases.lambda_vars, aliases.type_var_subst, aliases.trusted_type_vars, aliases.method_codomain, aliases.thunked_vars)

def field_expression(var_id: hydra.ext.java.syntax.Identifier, field_id: hydra.ext.java.syntax.Identifier) -> hydra.ext.java.syntax.ExpressionName:
    return hydra.ext.java.syntax.ExpressionName(Just(hydra.ext.java.syntax.AmbiguousName((var_id,))), field_id)

def is_escaped(s: str) -> bool:
    return hydra.lib.equality.equal(hydra.lib.strings.char_at(0, s), 36)

def unescape(s: str) -> str:
    return hydra.lib.strings.from_list(hydra.lib.lists.tail(hydra.lib.strings.to_list(s)))

def sanitize_java_name(name: str) -> str:
    return hydra.lib.logic.if_else(is_escaped(name), (lambda : unescape(name)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(name, "_"), (lambda : "ignored"), (lambda : hydra.formatting.sanitize_with_underscores(hydra.ext.java.language.reserved_words(), name)))))

def java_identifier(s: str) -> hydra.ext.java.syntax.Identifier:
    return hydra.ext.java.syntax.Identifier(sanitize_java_name(s))

def field_name_to_java_identifier(fname: hydra.core.Name) -> hydra.ext.java.syntax.Identifier:
    return java_identifier(fname.value)

def java_identifier_to_java_expression_name(id: hydra.ext.java.syntax.Identifier) -> hydra.ext.java.syntax.ExpressionName:
    return hydra.ext.java.syntax.ExpressionName(Nothing(), id)

def field_name_to_java_expression(fname: hydra.core.Name) -> hydra.ext.java.syntax.Expression:
    return cast(hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.ExpressionAssignment(cast(hydra.ext.java.syntax.AssignmentExpression, hydra.ext.java.syntax.AssignmentExpressionConditional(cast(hydra.ext.java.syntax.ConditionalExpression, hydra.ext.java.syntax.ConditionalExpressionSimple(hydra.ext.java.syntax.ConditionalOrExpression((hydra.ext.java.syntax.ConditionalAndExpression((hydra.ext.java.syntax.InclusiveOrExpression((hydra.ext.java.syntax.ExclusiveOrExpression((hydra.ext.java.syntax.AndExpression((cast(hydra.ext.java.syntax.EqualityExpression, hydra.ext.java.syntax.EqualityExpressionUnary(cast(hydra.ext.java.syntax.RelationalExpression, hydra.ext.java.syntax.RelationalExpressionSimple(cast(hydra.ext.java.syntax.ShiftExpression, hydra.ext.java.syntax.ShiftExpressionUnary(cast(hydra.ext.java.syntax.AdditiveExpression, hydra.ext.java.syntax.AdditiveExpressionUnary(cast(hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.MultiplicativeExpressionUnary(cast(hydra.ext.java.syntax.UnaryExpression, hydra.ext.java.syntax.UnaryExpressionOther(cast(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus, hydra.ext.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.ext.java.syntax.PostfixExpression, hydra.ext.java.syntax.PostfixExpressionName(java_identifier_to_java_expression_name(field_name_to_java_identifier(fname)))))))))))))))))),)),)),)),)),))))))))

def java_variable_declarator_id(id: hydra.ext.java.syntax.Identifier) -> hydra.ext.java.syntax.VariableDeclaratorId:
    return hydra.ext.java.syntax.VariableDeclaratorId(id, Nothing())

def java_variable_declarator(id: hydra.ext.java.syntax.Identifier, minit: Maybe[hydra.ext.java.syntax.VariableInitializer]) -> hydra.ext.java.syntax.VariableDeclarator:
    return hydra.ext.java.syntax.VariableDeclarator(java_variable_declarator_id(id), minit)

def field_name_to_java_variable_declarator(fname: hydra.core.Name) -> hydra.ext.java.syntax.VariableDeclarator:
    return java_variable_declarator(java_identifier(fname.value), Nothing())

def field_name_to_java_variable_declarator_id(fname: hydra.core.Name) -> hydra.ext.java.syntax.VariableDeclaratorId:
    return java_variable_declarator_id(java_identifier(fname.value))

def import_aliases_for_module(mod: hydra.module.Module) -> hydra.ext.java.helpers.Aliases:
    return hydra.ext.java.helpers.Aliases(mod.namespace, hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), Nothing(), hydra.lib.sets.empty())

def java_method_body(mstmts: Maybe[frozenlist[hydra.ext.java.syntax.BlockStatement]]) -> hydra.ext.java.syntax.MethodBody:
    return hydra.lib.maybes.cases(mstmts, cast(hydra.ext.java.syntax.MethodBody, hydra.ext.java.syntax.MethodBodyNone()), (lambda stmts: cast(hydra.ext.java.syntax.MethodBody, hydra.ext.java.syntax.MethodBodyBlock(hydra.ext.java.syntax.Block(stmts)))))

def java_method_header(tparams: frozenlist[hydra.ext.java.syntax.TypeParameter], method_name: str, params: frozenlist[hydra.ext.java.syntax.FormalParameter], result: hydra.ext.java.syntax.Result) -> hydra.ext.java.syntax.MethodHeader:
    return hydra.ext.java.syntax.MethodHeader(tparams, result, hydra.ext.java.syntax.MethodDeclarator(hydra.ext.java.syntax.Identifier(method_name), Nothing(), params), Nothing())

def interface_method_declaration(mods: frozenlist[hydra.ext.java.syntax.InterfaceMethodModifier], tparams: frozenlist[hydra.ext.java.syntax.TypeParameter], method_name: str, params: frozenlist[hydra.ext.java.syntax.FormalParameter], result: hydra.ext.java.syntax.Result, stmts: Maybe[frozenlist[hydra.ext.java.syntax.BlockStatement]]) -> hydra.ext.java.syntax.InterfaceMemberDeclaration:
    return cast(hydra.ext.java.syntax.InterfaceMemberDeclaration, hydra.ext.java.syntax.InterfaceMemberDeclarationInterfaceMethod(hydra.ext.java.syntax.InterfaceMethodDeclaration(mods, java_method_header(tparams, method_name, params, result), java_method_body(stmts))))

def java_additive_expression_to_java_expression(ae: hydra.ext.java.syntax.AdditiveExpression) -> hydra.ext.java.syntax.Expression:
    return cast(hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.ExpressionAssignment(cast(hydra.ext.java.syntax.AssignmentExpression, hydra.ext.java.syntax.AssignmentExpressionConditional(cast(hydra.ext.java.syntax.ConditionalExpression, hydra.ext.java.syntax.ConditionalExpressionSimple(hydra.ext.java.syntax.ConditionalOrExpression((hydra.ext.java.syntax.ConditionalAndExpression((hydra.ext.java.syntax.InclusiveOrExpression((hydra.ext.java.syntax.ExclusiveOrExpression((hydra.ext.java.syntax.AndExpression((cast(hydra.ext.java.syntax.EqualityExpression, hydra.ext.java.syntax.EqualityExpressionUnary(cast(hydra.ext.java.syntax.RelationalExpression, hydra.ext.java.syntax.RelationalExpressionSimple(cast(hydra.ext.java.syntax.ShiftExpression, hydra.ext.java.syntax.ShiftExpressionUnary(ae)))))),)),)),)),)),))))))))

def java_primary_to_java_expression(p: hydra.ext.java.syntax.Primary) -> hydra.ext.java.syntax.Expression:
    return cast(hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.ExpressionAssignment(cast(hydra.ext.java.syntax.AssignmentExpression, hydra.ext.java.syntax.AssignmentExpressionConditional(cast(hydra.ext.java.syntax.ConditionalExpression, hydra.ext.java.syntax.ConditionalExpressionSimple(hydra.ext.java.syntax.ConditionalOrExpression((hydra.ext.java.syntax.ConditionalAndExpression((hydra.ext.java.syntax.InclusiveOrExpression((hydra.ext.java.syntax.ExclusiveOrExpression((hydra.ext.java.syntax.AndExpression((cast(hydra.ext.java.syntax.EqualityExpression, hydra.ext.java.syntax.EqualityExpressionUnary(cast(hydra.ext.java.syntax.RelationalExpression, hydra.ext.java.syntax.RelationalExpressionSimple(cast(hydra.ext.java.syntax.ShiftExpression, hydra.ext.java.syntax.ShiftExpressionUnary(cast(hydra.ext.java.syntax.AdditiveExpression, hydra.ext.java.syntax.AdditiveExpressionUnary(cast(hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.MultiplicativeExpressionUnary(cast(hydra.ext.java.syntax.UnaryExpression, hydra.ext.java.syntax.UnaryExpressionOther(cast(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus, hydra.ext.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.ext.java.syntax.PostfixExpression, hydra.ext.java.syntax.PostfixExpressionPrimary(p)))))))))))))))),)),)),)),)),))))))))

def java_array_creation(prim_type: hydra.ext.java.syntax.PrimitiveTypeWithAnnotations, minit: Maybe[hydra.ext.java.syntax.ArrayInitializer]) -> hydra.ext.java.syntax.Expression:
    @lru_cache(1)
    def init_() -> hydra.ext.java.syntax.ArrayInitializer:
        return hydra.lib.maybes.cases(minit, hydra.ext.java.syntax.ArrayInitializer(()), (lambda i: i))
    return java_primary_to_java_expression(cast(hydra.ext.java.syntax.Primary, hydra.ext.java.syntax.PrimaryArrayCreation(cast(hydra.ext.java.syntax.ArrayCreationExpression, hydra.ext.java.syntax.ArrayCreationExpressionPrimitiveArray(hydra.ext.java.syntax.ArrayCreationExpression_PrimitiveArray(prim_type, (), init_()))))))

def java_array_initializer(exprs: frozenlist[hydra.ext.java.syntax.Expression]) -> hydra.ext.java.syntax.ArrayInitializer:
    return hydra.ext.java.syntax.ArrayInitializer((hydra.lib.lists.map((lambda e: cast(hydra.ext.java.syntax.VariableInitializer, hydra.ext.java.syntax.VariableInitializerExpression(e))), exprs),))

def java_assignment_statement(lhs2: hydra.ext.java.syntax.LeftHandSide, rhs2: hydra.ext.java.syntax.Expression) -> hydra.ext.java.syntax.Statement:
    return cast(hydra.ext.java.syntax.Statement, hydra.ext.java.syntax.StatementWithoutTrailing(cast(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement, hydra.ext.java.syntax.StatementWithoutTrailingSubstatementExpression(hydra.ext.java.syntax.ExpressionStatement(cast(hydra.ext.java.syntax.StatementExpression, hydra.ext.java.syntax.StatementExpressionAssignment(hydra.ext.java.syntax.Assignment(lhs2, hydra.ext.java.syntax.AssignmentOperator.SIMPLE, rhs2))))))))

def java_boolean(b: bool) -> hydra.ext.java.syntax.Literal:
    return cast(hydra.ext.java.syntax.Literal, hydra.ext.java.syntax.LiteralBoolean(b))

def java_literal_to_java_primary(lit: hydra.ext.java.syntax.Literal) -> hydra.ext.java.syntax.Primary:
    return cast(hydra.ext.java.syntax.Primary, hydra.ext.java.syntax.PrimaryNoNewArray(cast(hydra.ext.java.syntax.PrimaryNoNewArray, hydra.ext.java.syntax.PrimaryNoNewArrayLiteral(lit))))

def java_boolean_expression(b: bool) -> hydra.ext.java.syntax.Expression:
    return java_primary_to_java_expression(java_literal_to_java_primary(java_boolean(b)))

def java_primitive_type_to_java_type(pt: hydra.ext.java.syntax.PrimitiveType) -> hydra.ext.java.syntax.Type:
    return cast(hydra.ext.java.syntax.Type, hydra.ext.java.syntax.TypePrimitive(hydra.ext.java.syntax.PrimitiveTypeWithAnnotations(pt, ())))

@lru_cache(1)
def java_boolean_type() -> hydra.ext.java.syntax.Type:
    return java_primitive_type_to_java_type(cast(hydra.ext.java.syntax.PrimitiveType, hydra.ext.java.syntax.PrimitiveTypeBoolean()))

@lru_cache(1)
def java_byte_primitive_type() -> hydra.ext.java.syntax.PrimitiveTypeWithAnnotations:
    return hydra.ext.java.syntax.PrimitiveTypeWithAnnotations(cast(hydra.ext.java.syntax.PrimitiveType, hydra.ext.java.syntax.PrimitiveTypeNumeric(cast(hydra.ext.java.syntax.NumericType, hydra.ext.java.syntax.NumericTypeIntegral(hydra.ext.java.syntax.IntegralType.BYTE)))), ())

def java_cast_expression(rt: hydra.ext.java.syntax.ReferenceType, expr: hydra.ext.java.syntax.UnaryExpression) -> hydra.ext.java.syntax.CastExpression:
    return cast(hydra.ext.java.syntax.CastExpression, hydra.ext.java.syntax.CastExpressionNotPlusMinus(hydra.ext.java.syntax.CastExpression_NotPlusMinus(hydra.ext.java.syntax.CastExpression_RefAndBounds(rt, ()), expr)))

def java_cast_expression_to_java_expression(ce: hydra.ext.java.syntax.CastExpression) -> hydra.ext.java.syntax.Expression:
    return cast(hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.ExpressionAssignment(cast(hydra.ext.java.syntax.AssignmentExpression, hydra.ext.java.syntax.AssignmentExpressionConditional(cast(hydra.ext.java.syntax.ConditionalExpression, hydra.ext.java.syntax.ConditionalExpressionSimple(hydra.ext.java.syntax.ConditionalOrExpression((hydra.ext.java.syntax.ConditionalAndExpression((hydra.ext.java.syntax.InclusiveOrExpression((hydra.ext.java.syntax.ExclusiveOrExpression((hydra.ext.java.syntax.AndExpression((cast(hydra.ext.java.syntax.EqualityExpression, hydra.ext.java.syntax.EqualityExpressionUnary(cast(hydra.ext.java.syntax.RelationalExpression, hydra.ext.java.syntax.RelationalExpressionSimple(cast(hydra.ext.java.syntax.ShiftExpression, hydra.ext.java.syntax.ShiftExpressionUnary(cast(hydra.ext.java.syntax.AdditiveExpression, hydra.ext.java.syntax.AdditiveExpressionUnary(cast(hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.MultiplicativeExpressionUnary(cast(hydra.ext.java.syntax.UnaryExpression, hydra.ext.java.syntax.UnaryExpressionOther(cast(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus, hydra.ext.java.syntax.UnaryExpressionNotPlusMinusCast(ce)))))))))))))),)),)),)),)),))))))))

def java_cast_primitive(pt: hydra.ext.java.syntax.PrimitiveType, expr: hydra.ext.java.syntax.UnaryExpression) -> hydra.ext.java.syntax.CastExpression:
    return cast(hydra.ext.java.syntax.CastExpression, hydra.ext.java.syntax.CastExpressionPrimitive(hydra.ext.java.syntax.CastExpression_Primitive(hydra.ext.java.syntax.PrimitiveTypeWithAnnotations(pt, ()), expr)))

def java_variable_name(name: hydra.core.Name) -> hydra.ext.java.syntax.Identifier:
    return java_identifier(hydra.names.local_name_of(name))

def java_decl_name(name: hydra.core.Name) -> hydra.ext.java.syntax.TypeIdentifier:
    return hydra.ext.java.syntax.TypeIdentifier(java_variable_name(name))

def java_type_identifier(s: str) -> hydra.ext.java.syntax.TypeIdentifier:
    return hydra.ext.java.syntax.TypeIdentifier(hydra.ext.java.syntax.Identifier(s))

def name_to_qualified_java_name(aliases: hydra.ext.java.helpers.Aliases, qualify: bool, name: hydra.core.Name, mlocal: Maybe[str]) -> tuple[hydra.ext.java.syntax.TypeIdentifier, hydra.ext.java.syntax.ClassTypeQualifier]:
    @lru_cache(1)
    def qn() -> hydra.module.QualifiedName:
        return hydra.names.qualify_name(name)
    @lru_cache(1)
    def ns_() -> Maybe[hydra.module.Namespace]:
        return qn().namespace
    @lru_cache(1)
    def local() -> str:
        return qn().local
    @lru_cache(1)
    def alias() -> Maybe[hydra.ext.java.syntax.PackageName]:
        return hydra.lib.maybes.cases(ns_(), Nothing(), (lambda n: Just(hydra.lib.maybes.cases(hydra.lib.maps.lookup(n, aliases.packages), hydra.ext.java.names.java_package_name(hydra.lib.strings.split_on(".", n.value)), (lambda id: id)))))
    @lru_cache(1)
    def pkg() -> hydra.ext.java.syntax.ClassTypeQualifier:
        return hydra.lib.logic.if_else(qualify, (lambda : hydra.lib.maybes.cases(alias(), cast(hydra.ext.java.syntax.ClassTypeQualifier, hydra.ext.java.syntax.ClassTypeQualifierNone()), (lambda p: cast(hydra.ext.java.syntax.ClassTypeQualifier, hydra.ext.java.syntax.ClassTypeQualifierPackage(p))))), (lambda : cast(hydra.ext.java.syntax.ClassTypeQualifier, hydra.ext.java.syntax.ClassTypeQualifierNone())))
    @lru_cache(1)
    def jid() -> hydra.ext.java.syntax.TypeIdentifier:
        return java_type_identifier(hydra.lib.maybes.cases(mlocal, sanitize_java_name(local()), (lambda l: hydra.lib.strings.cat2(hydra.lib.strings.cat2(sanitize_java_name(local()), "."), sanitize_java_name(l)))))
    return (jid(), pkg())

def name_to_java_class_type(aliases: hydra.ext.java.helpers.Aliases, qualify: bool, args: frozenlist[hydra.ext.java.syntax.TypeArgument], name: hydra.core.Name, mlocal: Maybe[str]) -> hydra.ext.java.syntax.ClassType:
    @lru_cache(1)
    def result() -> tuple[hydra.ext.java.syntax.TypeIdentifier, hydra.ext.java.syntax.ClassTypeQualifier]:
        return name_to_qualified_java_name(aliases, qualify, name, mlocal)
    @lru_cache(1)
    def id() -> hydra.ext.java.syntax.TypeIdentifier:
        return hydra.lib.pairs.first(result())
    @lru_cache(1)
    def pkg() -> hydra.ext.java.syntax.ClassTypeQualifier:
        return hydra.lib.pairs.second(result())
    return hydra.ext.java.syntax.ClassType((), pkg(), id(), args)

def java_class_declaration(aliases: hydra.ext.java.helpers.Aliases, tparams: frozenlist[hydra.ext.java.syntax.TypeParameter], el_name: hydra.core.Name, mods: frozenlist[hydra.ext.java.syntax.ClassModifier], supname: Maybe[hydra.core.Name], impls: frozenlist[hydra.ext.java.syntax.InterfaceType], body_decls: frozenlist[hydra.ext.java.syntax.ClassBodyDeclarationWithComments]) -> hydra.ext.java.syntax.ClassDeclaration:
    @lru_cache(1)
    def extends_() -> Maybe[hydra.ext.java.syntax.ClassType]:
        return hydra.lib.maybes.map((lambda n: name_to_java_class_type(aliases, True, (), n, Nothing())), supname)
    return cast(hydra.ext.java.syntax.ClassDeclaration, hydra.ext.java.syntax.ClassDeclarationNormal(hydra.ext.java.syntax.NormalClassDeclaration(mods, java_decl_name(el_name), tparams, extends_(), impls, hydra.ext.java.syntax.ClassBody(body_decls))))

def java_class_type(args: frozenlist[hydra.ext.java.syntax.ReferenceType], pkg: Maybe[hydra.ext.java.syntax.PackageName], id: str) -> hydra.ext.java.syntax.ClassType:
    @lru_cache(1)
    def qual() -> hydra.ext.java.syntax.ClassTypeQualifier:
        return hydra.lib.maybes.cases(pkg, cast(hydra.ext.java.syntax.ClassTypeQualifier, hydra.ext.java.syntax.ClassTypeQualifierNone()), (lambda p: cast(hydra.ext.java.syntax.ClassTypeQualifier, hydra.ext.java.syntax.ClassTypeQualifierPackage(p))))
    @lru_cache(1)
    def targs() -> frozenlist[hydra.ext.java.syntax.TypeArgument]:
        return hydra.lib.lists.map((lambda rt: cast(hydra.ext.java.syntax.TypeArgument, hydra.ext.java.syntax.TypeArgumentReference(rt))), args)
    return hydra.ext.java.syntax.ClassType((), qual(), java_type_identifier(id), targs())

def java_class_type_to_java_type(ct: hydra.ext.java.syntax.ClassType) -> hydra.ext.java.syntax.Type:
    return cast(hydra.ext.java.syntax.Type, hydra.ext.java.syntax.TypeReference(cast(hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.ReferenceTypeClassOrInterface(cast(hydra.ext.java.syntax.ClassOrInterfaceType, hydra.ext.java.syntax.ClassOrInterfaceTypeClass(ct))))))

def java_conditional_and_expression_to_java_expression(cae: hydra.ext.java.syntax.ConditionalAndExpression) -> hydra.ext.java.syntax.Expression:
    return cast(hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.ExpressionAssignment(cast(hydra.ext.java.syntax.AssignmentExpression, hydra.ext.java.syntax.AssignmentExpressionConditional(cast(hydra.ext.java.syntax.ConditionalExpression, hydra.ext.java.syntax.ConditionalExpressionSimple(hydra.ext.java.syntax.ConditionalOrExpression((cae,))))))))

def java_constructor_call(ci: hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate, args: frozenlist[hydra.ext.java.syntax.Expression], mbody: Maybe[hydra.ext.java.syntax.ClassBody]) -> hydra.ext.java.syntax.Expression:
    return cast(hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.ExpressionAssignment(cast(hydra.ext.java.syntax.AssignmentExpression, hydra.ext.java.syntax.AssignmentExpressionConditional(cast(hydra.ext.java.syntax.ConditionalExpression, hydra.ext.java.syntax.ConditionalExpressionSimple(hydra.ext.java.syntax.ConditionalOrExpression((hydra.ext.java.syntax.ConditionalAndExpression((hydra.ext.java.syntax.InclusiveOrExpression((hydra.ext.java.syntax.ExclusiveOrExpression((hydra.ext.java.syntax.AndExpression((cast(hydra.ext.java.syntax.EqualityExpression, hydra.ext.java.syntax.EqualityExpressionUnary(cast(hydra.ext.java.syntax.RelationalExpression, hydra.ext.java.syntax.RelationalExpressionSimple(cast(hydra.ext.java.syntax.ShiftExpression, hydra.ext.java.syntax.ShiftExpressionUnary(cast(hydra.ext.java.syntax.AdditiveExpression, hydra.ext.java.syntax.AdditiveExpressionUnary(cast(hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.MultiplicativeExpressionUnary(cast(hydra.ext.java.syntax.UnaryExpression, hydra.ext.java.syntax.UnaryExpressionOther(cast(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus, hydra.ext.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.ext.java.syntax.PostfixExpression, hydra.ext.java.syntax.PostfixExpressionPrimary(cast(hydra.ext.java.syntax.Primary, hydra.ext.java.syntax.PrimaryNoNewArray(cast(hydra.ext.java.syntax.PrimaryNoNewArray, hydra.ext.java.syntax.PrimaryNoNewArrayClassInstance(hydra.ext.java.syntax.ClassInstanceCreationExpression(Nothing(), hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression((), ci, args, mbody)))))))))))))))))))))),)),)),)),)),))))))))

def java_constructor_name(id: hydra.ext.java.syntax.Identifier, targs: Maybe[hydra.ext.java.syntax.TypeArgumentsOrDiamond]) -> hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate:
    return hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate((hydra.ext.java.syntax.AnnotatedIdentifier((), id),), targs)

def java_expression_to_java_unary_expression(e: hydra.ext.java.syntax.Expression) -> hydra.ext.java.syntax.UnaryExpression:
    return cast(hydra.ext.java.syntax.UnaryExpression, hydra.ext.java.syntax.UnaryExpressionOther(cast(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus, hydra.ext.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.ext.java.syntax.PostfixExpression, hydra.ext.java.syntax.PostfixExpressionPrimary(cast(hydra.ext.java.syntax.Primary, hydra.ext.java.syntax.PrimaryNoNewArray(cast(hydra.ext.java.syntax.PrimaryNoNewArray, hydra.ext.java.syntax.PrimaryNoNewArrayParens(e))))))))))

def java_double_cast_expression(raw_rt: hydra.ext.java.syntax.ReferenceType, target_rt: hydra.ext.java.syntax.ReferenceType, expr: hydra.ext.java.syntax.UnaryExpression) -> hydra.ext.java.syntax.CastExpression:
    @lru_cache(1)
    def first_cast() -> hydra.ext.java.syntax.Expression:
        return java_cast_expression_to_java_expression(java_cast_expression(raw_rt, expr))
    return java_cast_expression(target_rt, java_expression_to_java_unary_expression(first_cast()))

def java_double_cast_expression_to_java_expression(raw_rt: hydra.ext.java.syntax.ReferenceType, target_rt: hydra.ext.java.syntax.ReferenceType, expr: hydra.ext.java.syntax.UnaryExpression) -> hydra.ext.java.syntax.Expression:
    return java_cast_expression_to_java_expression(java_double_cast_expression(raw_rt, target_rt, expr))

java_empty_statement = cast(hydra.ext.java.syntax.Statement, hydra.ext.java.syntax.StatementWithoutTrailing(cast(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement, hydra.ext.java.syntax.StatementWithoutTrailingSubstatementEmpty())))

def java_equality_expression_to_java_expression(ee: hydra.ext.java.syntax.EqualityExpression) -> hydra.ext.java.syntax.Expression:
    return cast(hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.ExpressionAssignment(cast(hydra.ext.java.syntax.AssignmentExpression, hydra.ext.java.syntax.AssignmentExpressionConditional(cast(hydra.ext.java.syntax.ConditionalExpression, hydra.ext.java.syntax.ConditionalExpressionSimple(hydra.ext.java.syntax.ConditionalOrExpression((hydra.ext.java.syntax.ConditionalAndExpression((hydra.ext.java.syntax.InclusiveOrExpression((hydra.ext.java.syntax.ExclusiveOrExpression((hydra.ext.java.syntax.AndExpression((ee,)),)),)),)),))))))))

def java_equality_expression_to_java_inclusive_or_expression(ee: hydra.ext.java.syntax.EqualityExpression) -> hydra.ext.java.syntax.InclusiveOrExpression:
    return hydra.ext.java.syntax.InclusiveOrExpression((hydra.ext.java.syntax.ExclusiveOrExpression((hydra.ext.java.syntax.AndExpression((ee,)),)),))

def java_equals(lhs2: hydra.ext.java.syntax.EqualityExpression, rhs2: hydra.ext.java.syntax.RelationalExpression) -> hydra.ext.java.syntax.EqualityExpression:
    return cast(hydra.ext.java.syntax.EqualityExpression, hydra.ext.java.syntax.EqualityExpressionEqual(hydra.ext.java.syntax.EqualityExpression_Binary(lhs2, rhs2)))

def java_literal_to_java_relational_expression(lit: hydra.ext.java.syntax.Literal) -> hydra.ext.java.syntax.RelationalExpression:
    return cast(hydra.ext.java.syntax.RelationalExpression, hydra.ext.java.syntax.RelationalExpressionSimple(cast(hydra.ext.java.syntax.ShiftExpression, hydra.ext.java.syntax.ShiftExpressionUnary(cast(hydra.ext.java.syntax.AdditiveExpression, hydra.ext.java.syntax.AdditiveExpressionUnary(cast(hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.MultiplicativeExpressionUnary(cast(hydra.ext.java.syntax.UnaryExpression, hydra.ext.java.syntax.UnaryExpressionOther(cast(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus, hydra.ext.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.ext.java.syntax.PostfixExpression, hydra.ext.java.syntax.PostfixExpressionPrimary(cast(hydra.ext.java.syntax.Primary, hydra.ext.java.syntax.PrimaryNoNewArray(cast(hydra.ext.java.syntax.PrimaryNoNewArray, hydra.ext.java.syntax.PrimaryNoNewArrayLiteral(lit))))))))))))))))))

def java_equals_null(lhs2: hydra.ext.java.syntax.EqualityExpression) -> hydra.ext.java.syntax.EqualityExpression:
    return java_equals(lhs2, java_literal_to_java_relational_expression(cast(hydra.ext.java.syntax.Literal, hydra.ext.java.syntax.LiteralNull())))

def java_expression_name_to_java_expression(en: hydra.ext.java.syntax.ExpressionName) -> hydra.ext.java.syntax.Expression:
    return cast(hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.ExpressionAssignment(cast(hydra.ext.java.syntax.AssignmentExpression, hydra.ext.java.syntax.AssignmentExpressionConditional(cast(hydra.ext.java.syntax.ConditionalExpression, hydra.ext.java.syntax.ConditionalExpressionSimple(hydra.ext.java.syntax.ConditionalOrExpression((hydra.ext.java.syntax.ConditionalAndExpression((hydra.ext.java.syntax.InclusiveOrExpression((hydra.ext.java.syntax.ExclusiveOrExpression((hydra.ext.java.syntax.AndExpression((cast(hydra.ext.java.syntax.EqualityExpression, hydra.ext.java.syntax.EqualityExpressionUnary(cast(hydra.ext.java.syntax.RelationalExpression, hydra.ext.java.syntax.RelationalExpressionSimple(cast(hydra.ext.java.syntax.ShiftExpression, hydra.ext.java.syntax.ShiftExpressionUnary(cast(hydra.ext.java.syntax.AdditiveExpression, hydra.ext.java.syntax.AdditiveExpressionUnary(cast(hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.MultiplicativeExpressionUnary(cast(hydra.ext.java.syntax.UnaryExpression, hydra.ext.java.syntax.UnaryExpressionOther(cast(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus, hydra.ext.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.ext.java.syntax.PostfixExpression, hydra.ext.java.syntax.PostfixExpressionName(en)))))))))))))))),)),)),)),)),))))))))

def java_expression_to_java_primary(e: hydra.ext.java.syntax.Expression) -> hydra.ext.java.syntax.Primary:
    return cast(hydra.ext.java.syntax.Primary, hydra.ext.java.syntax.PrimaryNoNewArray(cast(hydra.ext.java.syntax.PrimaryNoNewArray, hydra.ext.java.syntax.PrimaryNoNewArrayParens(e))))

def java_field_access_to_java_expression(fa: hydra.ext.java.syntax.FieldAccess) -> hydra.ext.java.syntax.Expression:
    return cast(hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.ExpressionAssignment(cast(hydra.ext.java.syntax.AssignmentExpression, hydra.ext.java.syntax.AssignmentExpressionConditional(cast(hydra.ext.java.syntax.ConditionalExpression, hydra.ext.java.syntax.ConditionalExpressionSimple(hydra.ext.java.syntax.ConditionalOrExpression((hydra.ext.java.syntax.ConditionalAndExpression((hydra.ext.java.syntax.InclusiveOrExpression((hydra.ext.java.syntax.ExclusiveOrExpression((hydra.ext.java.syntax.AndExpression((cast(hydra.ext.java.syntax.EqualityExpression, hydra.ext.java.syntax.EqualityExpressionUnary(cast(hydra.ext.java.syntax.RelationalExpression, hydra.ext.java.syntax.RelationalExpressionSimple(cast(hydra.ext.java.syntax.ShiftExpression, hydra.ext.java.syntax.ShiftExpressionUnary(cast(hydra.ext.java.syntax.AdditiveExpression, hydra.ext.java.syntax.AdditiveExpressionUnary(cast(hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.MultiplicativeExpressionUnary(cast(hydra.ext.java.syntax.UnaryExpression, hydra.ext.java.syntax.UnaryExpressionOther(cast(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus, hydra.ext.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.ext.java.syntax.PostfixExpression, hydra.ext.java.syntax.PostfixExpressionPrimary(cast(hydra.ext.java.syntax.Primary, hydra.ext.java.syntax.PrimaryNoNewArray(cast(hydra.ext.java.syntax.PrimaryNoNewArray, hydra.ext.java.syntax.PrimaryNoNewArrayFieldAccess(fa)))))))))))))))))))),)),)),)),)),))))))))

def java_identifier_to_java_expression(id: hydra.ext.java.syntax.Identifier) -> hydra.ext.java.syntax.Expression:
    return cast(hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.ExpressionAssignment(cast(hydra.ext.java.syntax.AssignmentExpression, hydra.ext.java.syntax.AssignmentExpressionConditional(cast(hydra.ext.java.syntax.ConditionalExpression, hydra.ext.java.syntax.ConditionalExpressionSimple(hydra.ext.java.syntax.ConditionalOrExpression((hydra.ext.java.syntax.ConditionalAndExpression((hydra.ext.java.syntax.InclusiveOrExpression((hydra.ext.java.syntax.ExclusiveOrExpression((hydra.ext.java.syntax.AndExpression((cast(hydra.ext.java.syntax.EqualityExpression, hydra.ext.java.syntax.EqualityExpressionUnary(cast(hydra.ext.java.syntax.RelationalExpression, hydra.ext.java.syntax.RelationalExpressionSimple(cast(hydra.ext.java.syntax.ShiftExpression, hydra.ext.java.syntax.ShiftExpressionUnary(cast(hydra.ext.java.syntax.AdditiveExpression, hydra.ext.java.syntax.AdditiveExpressionUnary(cast(hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.MultiplicativeExpressionUnary(cast(hydra.ext.java.syntax.UnaryExpression, hydra.ext.java.syntax.UnaryExpressionOther(cast(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus, hydra.ext.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.ext.java.syntax.PostfixExpression, hydra.ext.java.syntax.PostfixExpressionName(hydra.ext.java.syntax.ExpressionName(Nothing(), id))))))))))))))))),)),)),)),)),))))))))

def java_identifier_to_java_relational_expression(id: hydra.ext.java.syntax.Identifier) -> hydra.ext.java.syntax.RelationalExpression:
    return cast(hydra.ext.java.syntax.RelationalExpression, hydra.ext.java.syntax.RelationalExpressionSimple(cast(hydra.ext.java.syntax.ShiftExpression, hydra.ext.java.syntax.ShiftExpressionUnary(cast(hydra.ext.java.syntax.AdditiveExpression, hydra.ext.java.syntax.AdditiveExpressionUnary(cast(hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.MultiplicativeExpressionUnary(cast(hydra.ext.java.syntax.UnaryExpression, hydra.ext.java.syntax.UnaryExpressionOther(cast(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus, hydra.ext.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.ext.java.syntax.PostfixExpression, hydra.ext.java.syntax.PostfixExpressionName(hydra.ext.java.syntax.ExpressionName(Nothing(), id)))))))))))))))

def java_identifier_to_java_unary_expression(id: hydra.ext.java.syntax.Identifier) -> hydra.ext.java.syntax.UnaryExpression:
    return cast(hydra.ext.java.syntax.UnaryExpression, hydra.ext.java.syntax.UnaryExpressionOther(cast(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus, hydra.ext.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.ext.java.syntax.PostfixExpression, hydra.ext.java.syntax.PostfixExpressionName(hydra.ext.java.syntax.ExpressionName(Nothing(), id)))))))

def java_instance_of(lhs2: hydra.ext.java.syntax.RelationalExpression, rhs2: hydra.ext.java.syntax.ReferenceType) -> hydra.ext.java.syntax.RelationalExpression:
    return cast(hydra.ext.java.syntax.RelationalExpression, hydra.ext.java.syntax.RelationalExpressionInstanceof(hydra.ext.java.syntax.RelationalExpression_InstanceOf(lhs2, rhs2)))

def java_int(i: int) -> hydra.ext.java.syntax.Literal:
    return cast(hydra.ext.java.syntax.Literal, hydra.ext.java.syntax.LiteralInteger(hydra.ext.java.syntax.IntegerLiteral(i)))

def java_int_expression(i: int) -> hydra.ext.java.syntax.Expression:
    return java_primary_to_java_expression(java_literal_to_java_primary(java_int(i)))

@lru_cache(1)
def java_int_type() -> hydra.ext.java.syntax.Type:
    return java_primitive_type_to_java_type(cast(hydra.ext.java.syntax.PrimitiveType, hydra.ext.java.syntax.PrimitiveTypeNumeric(cast(hydra.ext.java.syntax.NumericType, hydra.ext.java.syntax.NumericTypeIntegral(hydra.ext.java.syntax.IntegralType.INT)))))

def java_interface_declaration_to_java_class_body_declaration(nid: hydra.ext.java.syntax.NormalInterfaceDeclaration) -> hydra.ext.java.syntax.ClassBodyDeclaration:
    return cast(hydra.ext.java.syntax.ClassBodyDeclaration, hydra.ext.java.syntax.ClassBodyDeclarationClassMember(cast(hydra.ext.java.syntax.ClassMemberDeclaration, hydra.ext.java.syntax.ClassMemberDeclarationInterface(cast(hydra.ext.java.syntax.InterfaceDeclaration, hydra.ext.java.syntax.InterfaceDeclarationNormalInterface(nid))))))

def variable_to_java_identifier(name: hydra.core.Name) -> hydra.ext.java.syntax.Identifier:
    @lru_cache(1)
    def v() -> str:
        return name.value
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(v(), "_"), (lambda : hydra.ext.java.syntax.Identifier("ignored")), (lambda : hydra.ext.java.syntax.Identifier(sanitize_java_name(v()))))

def java_lambda(v: hydra.core.Name, body: hydra.ext.java.syntax.Expression) -> hydra.ext.java.syntax.Expression:
    return cast(hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.ExpressionLambda(hydra.ext.java.syntax.LambdaExpression(cast(hydra.ext.java.syntax.LambdaParameters, hydra.ext.java.syntax.LambdaParametersSingle(variable_to_java_identifier(v))), cast(hydra.ext.java.syntax.LambdaBody, hydra.ext.java.syntax.LambdaBodyExpression(body)))))

def java_lambda_from_block(v: hydra.core.Name, block: hydra.ext.java.syntax.Block) -> hydra.ext.java.syntax.Expression:
    return cast(hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.ExpressionLambda(hydra.ext.java.syntax.LambdaExpression(cast(hydra.ext.java.syntax.LambdaParameters, hydra.ext.java.syntax.LambdaParametersSingle(variable_to_java_identifier(v))), cast(hydra.ext.java.syntax.LambdaBody, hydra.ext.java.syntax.LambdaBodyBlock(block)))))

def java_literal_to_java_expression(lit: hydra.ext.java.syntax.Literal) -> hydra.ext.java.syntax.Expression:
    return cast(hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.ExpressionAssignment(cast(hydra.ext.java.syntax.AssignmentExpression, hydra.ext.java.syntax.AssignmentExpressionConditional(cast(hydra.ext.java.syntax.ConditionalExpression, hydra.ext.java.syntax.ConditionalExpressionSimple(hydra.ext.java.syntax.ConditionalOrExpression((hydra.ext.java.syntax.ConditionalAndExpression((hydra.ext.java.syntax.InclusiveOrExpression((hydra.ext.java.syntax.ExclusiveOrExpression((hydra.ext.java.syntax.AndExpression((cast(hydra.ext.java.syntax.EqualityExpression, hydra.ext.java.syntax.EqualityExpressionUnary(cast(hydra.ext.java.syntax.RelationalExpression, hydra.ext.java.syntax.RelationalExpressionSimple(cast(hydra.ext.java.syntax.ShiftExpression, hydra.ext.java.syntax.ShiftExpressionUnary(cast(hydra.ext.java.syntax.AdditiveExpression, hydra.ext.java.syntax.AdditiveExpressionUnary(cast(hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.MultiplicativeExpressionUnary(cast(hydra.ext.java.syntax.UnaryExpression, hydra.ext.java.syntax.UnaryExpressionOther(cast(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus, hydra.ext.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.ext.java.syntax.PostfixExpression, hydra.ext.java.syntax.PostfixExpressionPrimary(cast(hydra.ext.java.syntax.Primary, hydra.ext.java.syntax.PrimaryNoNewArray(cast(hydra.ext.java.syntax.PrimaryNoNewArray, hydra.ext.java.syntax.PrimaryNoNewArrayLiteral(lit)))))))))))))))))))),)),)),)),)),))))))))

def java_literal_to_java_multiplicative_expression(lit: hydra.ext.java.syntax.Literal) -> hydra.ext.java.syntax.MultiplicativeExpression:
    return cast(hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.MultiplicativeExpressionUnary(cast(hydra.ext.java.syntax.UnaryExpression, hydra.ext.java.syntax.UnaryExpressionOther(cast(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus, hydra.ext.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.ext.java.syntax.PostfixExpression, hydra.ext.java.syntax.PostfixExpressionPrimary(cast(hydra.ext.java.syntax.Primary, hydra.ext.java.syntax.PrimaryNoNewArray(cast(hydra.ext.java.syntax.PrimaryNoNewArray, hydra.ext.java.syntax.PrimaryNoNewArrayLiteral(lit))))))))))))

def java_member_field(mods: frozenlist[hydra.ext.java.syntax.FieldModifier], jt: hydra.ext.java.syntax.Type, v: hydra.ext.java.syntax.VariableDeclarator) -> hydra.ext.java.syntax.ClassBodyDeclaration:
    return cast(hydra.ext.java.syntax.ClassBodyDeclaration, hydra.ext.java.syntax.ClassBodyDeclarationClassMember(cast(hydra.ext.java.syntax.ClassMemberDeclaration, hydra.ext.java.syntax.ClassMemberDeclarationField(hydra.ext.java.syntax.FieldDeclaration(mods, hydra.ext.java.syntax.UnannType(jt), (v,))))))

def java_method_declaration_to_java_class_body_declaration(md: hydra.ext.java.syntax.MethodDeclaration) -> hydra.ext.java.syntax.ClassBodyDeclaration:
    return cast(hydra.ext.java.syntax.ClassBodyDeclaration, hydra.ext.java.syntax.ClassBodyDeclarationClassMember(cast(hydra.ext.java.syntax.ClassMemberDeclaration, hydra.ext.java.syntax.ClassMemberDeclarationMethod(md))))

def java_method_invocation_to_java_expression(mi: hydra.ext.java.syntax.MethodInvocation) -> hydra.ext.java.syntax.Expression:
    return cast(hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.ExpressionAssignment(cast(hydra.ext.java.syntax.AssignmentExpression, hydra.ext.java.syntax.AssignmentExpressionConditional(cast(hydra.ext.java.syntax.ConditionalExpression, hydra.ext.java.syntax.ConditionalExpressionSimple(hydra.ext.java.syntax.ConditionalOrExpression((hydra.ext.java.syntax.ConditionalAndExpression((hydra.ext.java.syntax.InclusiveOrExpression((hydra.ext.java.syntax.ExclusiveOrExpression((hydra.ext.java.syntax.AndExpression((cast(hydra.ext.java.syntax.EqualityExpression, hydra.ext.java.syntax.EqualityExpressionUnary(cast(hydra.ext.java.syntax.RelationalExpression, hydra.ext.java.syntax.RelationalExpressionSimple(cast(hydra.ext.java.syntax.ShiftExpression, hydra.ext.java.syntax.ShiftExpressionUnary(cast(hydra.ext.java.syntax.AdditiveExpression, hydra.ext.java.syntax.AdditiveExpressionUnary(cast(hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.MultiplicativeExpressionUnary(cast(hydra.ext.java.syntax.UnaryExpression, hydra.ext.java.syntax.UnaryExpressionOther(cast(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus, hydra.ext.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.ext.java.syntax.PostfixExpression, hydra.ext.java.syntax.PostfixExpressionPrimary(cast(hydra.ext.java.syntax.Primary, hydra.ext.java.syntax.PrimaryNoNewArray(cast(hydra.ext.java.syntax.PrimaryNoNewArray, hydra.ext.java.syntax.PrimaryNoNewArrayMethodInvocation(mi)))))))))))))))))))),)),)),)),)),))))))))

def java_method_invocation_to_java_postfix_expression(mi: hydra.ext.java.syntax.MethodInvocation) -> hydra.ext.java.syntax.PostfixExpression:
    return cast(hydra.ext.java.syntax.PostfixExpression, hydra.ext.java.syntax.PostfixExpressionPrimary(cast(hydra.ext.java.syntax.Primary, hydra.ext.java.syntax.PrimaryNoNewArray(cast(hydra.ext.java.syntax.PrimaryNoNewArray, hydra.ext.java.syntax.PrimaryNoNewArrayMethodInvocation(mi))))))

def java_method_invocation_to_java_primary(mi: hydra.ext.java.syntax.MethodInvocation) -> hydra.ext.java.syntax.Primary:
    return cast(hydra.ext.java.syntax.Primary, hydra.ext.java.syntax.PrimaryNoNewArray(cast(hydra.ext.java.syntax.PrimaryNoNewArray, hydra.ext.java.syntax.PrimaryNoNewArrayMethodInvocation(mi))))

def java_method_invocation_to_java_statement(mi: hydra.ext.java.syntax.MethodInvocation) -> hydra.ext.java.syntax.Statement:
    return cast(hydra.ext.java.syntax.Statement, hydra.ext.java.syntax.StatementWithoutTrailing(cast(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement, hydra.ext.java.syntax.StatementWithoutTrailingSubstatementExpression(hydra.ext.java.syntax.ExpressionStatement(cast(hydra.ext.java.syntax.StatementExpression, hydra.ext.java.syntax.StatementExpressionMethodInvocation(mi)))))))

def java_multiplicative_expression_to_java_relational_expression(me: hydra.ext.java.syntax.MultiplicativeExpression) -> hydra.ext.java.syntax.RelationalExpression:
    return cast(hydra.ext.java.syntax.RelationalExpression, hydra.ext.java.syntax.RelationalExpressionSimple(cast(hydra.ext.java.syntax.ShiftExpression, hydra.ext.java.syntax.ShiftExpressionUnary(cast(hydra.ext.java.syntax.AdditiveExpression, hydra.ext.java.syntax.AdditiveExpressionUnary(me))))))

def java_package_declaration(ns: hydra.module.Namespace) -> hydra.ext.java.syntax.PackageDeclaration:
    return hydra.ext.java.syntax.PackageDeclaration((), hydra.lib.lists.map((lambda s: hydra.ext.java.syntax.Identifier(s)), hydra.lib.strings.split_on(".", ns.value)))

def java_postfix_expression_to_java_equality_expression(pe: hydra.ext.java.syntax.PostfixExpression) -> hydra.ext.java.syntax.EqualityExpression:
    return cast(hydra.ext.java.syntax.EqualityExpression, hydra.ext.java.syntax.EqualityExpressionUnary(cast(hydra.ext.java.syntax.RelationalExpression, hydra.ext.java.syntax.RelationalExpressionSimple(cast(hydra.ext.java.syntax.ShiftExpression, hydra.ext.java.syntax.ShiftExpressionUnary(cast(hydra.ext.java.syntax.AdditiveExpression, hydra.ext.java.syntax.AdditiveExpressionUnary(cast(hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.MultiplicativeExpressionUnary(cast(hydra.ext.java.syntax.UnaryExpression, hydra.ext.java.syntax.UnaryExpressionOther(cast(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus, hydra.ext.java.syntax.UnaryExpressionNotPlusMinusPostfix(pe))))))))))))))

def java_postfix_expression_to_java_expression(pe: hydra.ext.java.syntax.PostfixExpression) -> hydra.ext.java.syntax.Expression:
    return cast(hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.ExpressionAssignment(cast(hydra.ext.java.syntax.AssignmentExpression, hydra.ext.java.syntax.AssignmentExpressionConditional(cast(hydra.ext.java.syntax.ConditionalExpression, hydra.ext.java.syntax.ConditionalExpressionSimple(hydra.ext.java.syntax.ConditionalOrExpression((hydra.ext.java.syntax.ConditionalAndExpression((hydra.ext.java.syntax.InclusiveOrExpression((hydra.ext.java.syntax.ExclusiveOrExpression((hydra.ext.java.syntax.AndExpression((cast(hydra.ext.java.syntax.EqualityExpression, hydra.ext.java.syntax.EqualityExpressionUnary(cast(hydra.ext.java.syntax.RelationalExpression, hydra.ext.java.syntax.RelationalExpressionSimple(cast(hydra.ext.java.syntax.ShiftExpression, hydra.ext.java.syntax.ShiftExpressionUnary(cast(hydra.ext.java.syntax.AdditiveExpression, hydra.ext.java.syntax.AdditiveExpressionUnary(cast(hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.MultiplicativeExpressionUnary(cast(hydra.ext.java.syntax.UnaryExpression, hydra.ext.java.syntax.UnaryExpressionOther(cast(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus, hydra.ext.java.syntax.UnaryExpressionNotPlusMinusPostfix(pe)))))))))))))),)),)),)),)),))))))))

def java_postfix_expression_to_java_inclusive_or_expression(pe: hydra.ext.java.syntax.PostfixExpression) -> hydra.ext.java.syntax.InclusiveOrExpression:
    return hydra.ext.java.syntax.InclusiveOrExpression((hydra.ext.java.syntax.ExclusiveOrExpression((hydra.ext.java.syntax.AndExpression((cast(hydra.ext.java.syntax.EqualityExpression, hydra.ext.java.syntax.EqualityExpressionUnary(cast(hydra.ext.java.syntax.RelationalExpression, hydra.ext.java.syntax.RelationalExpressionSimple(cast(hydra.ext.java.syntax.ShiftExpression, hydra.ext.java.syntax.ShiftExpressionUnary(cast(hydra.ext.java.syntax.AdditiveExpression, hydra.ext.java.syntax.AdditiveExpressionUnary(cast(hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.MultiplicativeExpressionUnary(cast(hydra.ext.java.syntax.UnaryExpression, hydra.ext.java.syntax.UnaryExpressionOther(cast(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus, hydra.ext.java.syntax.UnaryExpressionNotPlusMinusPostfix(pe)))))))))))))),)),)),))

def java_postfix_expression_to_java_relational_expression(pe: hydra.ext.java.syntax.PostfixExpression) -> hydra.ext.java.syntax.RelationalExpression:
    return cast(hydra.ext.java.syntax.RelationalExpression, hydra.ext.java.syntax.RelationalExpressionSimple(cast(hydra.ext.java.syntax.ShiftExpression, hydra.ext.java.syntax.ShiftExpressionUnary(cast(hydra.ext.java.syntax.AdditiveExpression, hydra.ext.java.syntax.AdditiveExpressionUnary(cast(hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.MultiplicativeExpressionUnary(cast(hydra.ext.java.syntax.UnaryExpression, hydra.ext.java.syntax.UnaryExpressionOther(cast(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus, hydra.ext.java.syntax.UnaryExpressionNotPlusMinusPostfix(pe))))))))))))

def java_postfix_expression_to_java_unary_expression(pe: hydra.ext.java.syntax.PostfixExpression) -> hydra.ext.java.syntax.UnaryExpression:
    return cast(hydra.ext.java.syntax.UnaryExpression, hydra.ext.java.syntax.UnaryExpressionOther(cast(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus, hydra.ext.java.syntax.UnaryExpressionNotPlusMinusPostfix(pe))))

def java_primary_to_java_unary_expression(p: hydra.ext.java.syntax.Primary) -> hydra.ext.java.syntax.UnaryExpression:
    return cast(hydra.ext.java.syntax.UnaryExpression, hydra.ext.java.syntax.UnaryExpressionOther(cast(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus, hydra.ext.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.ext.java.syntax.PostfixExpression, hydra.ext.java.syntax.PostfixExpressionPrimary(p))))))

def java_ref_type(args: frozenlist[hydra.ext.java.syntax.ReferenceType], pkg: Maybe[hydra.ext.java.syntax.PackageName], id: str) -> hydra.ext.java.syntax.Type:
    return cast(hydra.ext.java.syntax.Type, hydra.ext.java.syntax.TypeReference(cast(hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.ReferenceTypeClassOrInterface(cast(hydra.ext.java.syntax.ClassOrInterfaceType, hydra.ext.java.syntax.ClassOrInterfaceTypeClass(java_class_type(args, pkg, id)))))))

def java_reference_type_to_raw_type(rt: hydra.ext.java.syntax.ReferenceType) -> hydra.ext.java.syntax.ReferenceType:
    def _hoist_hydra_ext_java_utils_java_reference_type_to_raw_type_1(v1: hydra.ext.java.syntax.ClassOrInterfaceType) -> hydra.ext.java.syntax.ReferenceType:
        match v1:
            case hydra.ext.java.syntax.ClassOrInterfaceTypeClass(value=ct):
                @lru_cache(1)
                def anns() -> frozenlist[hydra.ext.java.syntax.Annotation]:
                    return ct.annotations
                @lru_cache(1)
                def qual() -> hydra.ext.java.syntax.ClassTypeQualifier:
                    return ct.qualifier
                @lru_cache(1)
                def id() -> hydra.ext.java.syntax.TypeIdentifier:
                    return ct.identifier
                return cast(hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.ReferenceTypeClassOrInterface(cast(hydra.ext.java.syntax.ClassOrInterfaceType, hydra.ext.java.syntax.ClassOrInterfaceTypeClass(hydra.ext.java.syntax.ClassType(anns(), qual(), id(), ())))))
            
            case hydra.ext.java.syntax.ClassOrInterfaceTypeInterface(value=it):
                @lru_cache(1)
                def ct() -> hydra.ext.java.syntax.ClassType:
                    return it.value
                @lru_cache(1)
                def anns() -> frozenlist[hydra.ext.java.syntax.Annotation]:
                    return ct().annotations
                @lru_cache(1)
                def qual() -> hydra.ext.java.syntax.ClassTypeQualifier:
                    return ct().qualifier
                @lru_cache(1)
                def id() -> hydra.ext.java.syntax.TypeIdentifier:
                    return ct().identifier
                return cast(hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.ReferenceTypeClassOrInterface(cast(hydra.ext.java.syntax.ClassOrInterfaceType, hydra.ext.java.syntax.ClassOrInterfaceTypeInterface(hydra.ext.java.syntax.InterfaceType(hydra.ext.java.syntax.ClassType(anns(), qual(), id(), ()))))))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    match rt:
        case hydra.ext.java.syntax.ReferenceTypeClassOrInterface(value=cit):
            return _hoist_hydra_ext_java_utils_java_reference_type_to_raw_type_1(cit)
        
        case _:
            return rt

def java_relational_expression_to_java_equality_expression(re: hydra.ext.java.syntax.RelationalExpression) -> hydra.ext.java.syntax.EqualityExpression:
    return cast(hydra.ext.java.syntax.EqualityExpression, hydra.ext.java.syntax.EqualityExpressionUnary(re))

def java_relational_expression_to_java_expression(re: hydra.ext.java.syntax.RelationalExpression) -> hydra.ext.java.syntax.Expression:
    return java_equality_expression_to_java_expression(cast(hydra.ext.java.syntax.EqualityExpression, hydra.ext.java.syntax.EqualityExpressionUnary(re)))

def java_relational_expression_to_java_unary_expression(re: hydra.ext.java.syntax.RelationalExpression) -> hydra.ext.java.syntax.UnaryExpression:
    return cast(hydra.ext.java.syntax.UnaryExpression, hydra.ext.java.syntax.UnaryExpressionOther(cast(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus, hydra.ext.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.ext.java.syntax.PostfixExpression, hydra.ext.java.syntax.PostfixExpressionPrimary(cast(hydra.ext.java.syntax.Primary, hydra.ext.java.syntax.PrimaryNoNewArray(cast(hydra.ext.java.syntax.PrimaryNoNewArray, hydra.ext.java.syntax.PrimaryNoNewArrayParens(cast(hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.ExpressionAssignment(cast(hydra.ext.java.syntax.AssignmentExpression, hydra.ext.java.syntax.AssignmentExpressionConditional(cast(hydra.ext.java.syntax.ConditionalExpression, hydra.ext.java.syntax.ConditionalExpressionSimple(hydra.ext.java.syntax.ConditionalOrExpression((hydra.ext.java.syntax.ConditionalAndExpression((hydra.ext.java.syntax.InclusiveOrExpression((hydra.ext.java.syntax.ExclusiveOrExpression((hydra.ext.java.syntax.AndExpression((cast(hydra.ext.java.syntax.EqualityExpression, hydra.ext.java.syntax.EqualityExpressionUnary(re)),)),)),)),)),))))))))))))))))))

def java_return_statement(mex: Maybe[hydra.ext.java.syntax.Expression]) -> hydra.ext.java.syntax.Statement:
    return cast(hydra.ext.java.syntax.Statement, hydra.ext.java.syntax.StatementWithoutTrailing(cast(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement, hydra.ext.java.syntax.StatementWithoutTrailingSubstatementReturn(hydra.ext.java.syntax.ReturnStatement(mex)))))

def java_statements_to_block(stmts: frozenlist[hydra.ext.java.syntax.Statement]) -> hydra.ext.java.syntax.Block:
    return hydra.ext.java.syntax.Block(hydra.lib.lists.map((lambda s: cast(hydra.ext.java.syntax.BlockStatement, hydra.ext.java.syntax.BlockStatementStatement(s))), stmts))

def java_string(s: str) -> hydra.ext.java.syntax.Literal:
    return cast(hydra.ext.java.syntax.Literal, hydra.ext.java.syntax.LiteralString(hydra.ext.java.syntax.StringLiteral(s)))

def java_string_multiplicative_expression(s: str) -> hydra.ext.java.syntax.MultiplicativeExpression:
    return java_literal_to_java_multiplicative_expression(java_string(s))

java_this = cast(hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.ExpressionAssignment(cast(hydra.ext.java.syntax.AssignmentExpression, hydra.ext.java.syntax.AssignmentExpressionConditional(cast(hydra.ext.java.syntax.ConditionalExpression, hydra.ext.java.syntax.ConditionalExpressionSimple(hydra.ext.java.syntax.ConditionalOrExpression((hydra.ext.java.syntax.ConditionalAndExpression((hydra.ext.java.syntax.InclusiveOrExpression((hydra.ext.java.syntax.ExclusiveOrExpression((hydra.ext.java.syntax.AndExpression((cast(hydra.ext.java.syntax.EqualityExpression, hydra.ext.java.syntax.EqualityExpressionUnary(cast(hydra.ext.java.syntax.RelationalExpression, hydra.ext.java.syntax.RelationalExpressionSimple(cast(hydra.ext.java.syntax.ShiftExpression, hydra.ext.java.syntax.ShiftExpressionUnary(cast(hydra.ext.java.syntax.AdditiveExpression, hydra.ext.java.syntax.AdditiveExpressionUnary(cast(hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.MultiplicativeExpressionUnary(cast(hydra.ext.java.syntax.UnaryExpression, hydra.ext.java.syntax.UnaryExpressionOther(cast(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus, hydra.ext.java.syntax.UnaryExpressionNotPlusMinusPostfix(cast(hydra.ext.java.syntax.PostfixExpression, hydra.ext.java.syntax.PostfixExpressionPrimary(cast(hydra.ext.java.syntax.Primary, hydra.ext.java.syntax.PrimaryNoNewArray(cast(hydra.ext.java.syntax.PrimaryNoNewArray, hydra.ext.java.syntax.PrimaryNoNewArrayThis()))))))))))))))))))),)),)),)),)),))))))))

def java_throw_statement(e: hydra.ext.java.syntax.Expression) -> hydra.ext.java.syntax.Statement:
    return cast(hydra.ext.java.syntax.Statement, hydra.ext.java.syntax.StatementWithoutTrailing(cast(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement, hydra.ext.java.syntax.StatementWithoutTrailingSubstatementThrow(hydra.ext.java.syntax.ThrowStatement(e)))))

def java_throw_illegal_argument_exception(args: frozenlist[hydra.ext.java.syntax.Expression]) -> hydra.ext.java.syntax.Statement:
    return java_throw_statement(java_constructor_call(java_constructor_name(hydra.ext.java.syntax.Identifier("IllegalArgumentException"), Nothing()), args, Nothing()))

def java_throw_illegal_state_exception(args: frozenlist[hydra.ext.java.syntax.Expression]) -> hydra.ext.java.syntax.Statement:
    return java_throw_statement(java_constructor_call(java_constructor_name(hydra.ext.java.syntax.Identifier("IllegalStateException"), Nothing()), args, Nothing()))

def name_to_java_type_identifier(aliases: hydra.ext.java.helpers.Aliases, qualify: bool, name: hydra.core.Name) -> hydra.ext.java.syntax.TypeIdentifier:
    return hydra.lib.pairs.first(name_to_qualified_java_name(aliases, qualify, name, Nothing()))

def java_type_from_type_name(aliases: hydra.ext.java.helpers.Aliases, el_name: hydra.core.Name) -> hydra.ext.java.syntax.Type:
    return java_type_variable_to_type(hydra.ext.java.syntax.TypeVariable((), name_to_java_type_identifier(aliases, False, el_name)))

def java_type_identifier_to_java_type_argument(id: hydra.ext.java.syntax.TypeIdentifier) -> hydra.ext.java.syntax.TypeArgument:
    return cast(hydra.ext.java.syntax.TypeArgument, hydra.ext.java.syntax.TypeArgumentReference(cast(hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.ReferenceTypeVariable(hydra.ext.java.syntax.TypeVariable((), id)))))

def java_type_name(id: hydra.ext.java.syntax.Identifier) -> hydra.ext.java.syntax.TypeName:
    return hydra.ext.java.syntax.TypeName(hydra.ext.java.syntax.TypeIdentifier(id), Nothing())

def java_type_parameter(v: str) -> hydra.ext.java.syntax.TypeParameter:
    return hydra.ext.java.syntax.TypeParameter((), java_type_identifier(v), Nothing())

def java_type_to_java_formal_parameter(jt: hydra.ext.java.syntax.Type, fname: hydra.core.Name) -> hydra.ext.java.syntax.FormalParameter:
    return cast(hydra.ext.java.syntax.FormalParameter, hydra.ext.java.syntax.FormalParameterSimple(hydra.ext.java.syntax.FormalParameter_Simple((), hydra.ext.java.syntax.UnannType(jt), field_name_to_java_variable_declarator_id(fname))))

def java_type_to_java_reference_type(t: hydra.ext.java.syntax.Type) -> hydra.compute.Flow[T0, hydra.ext.java.syntax.ReferenceType]:
    match t:
        case hydra.ext.java.syntax.TypeReference(value=rt):
            return hydra.lib.flows.pure(rt)
        
        case hydra.ext.java.syntax.TypePrimitive():
            return hydra.lib.flows.fail("expected a Java reference type")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def java_type_to_java_result(jt: hydra.ext.java.syntax.Type) -> hydra.ext.java.syntax.Result:
    return cast(hydra.ext.java.syntax.Result, hydra.ext.java.syntax.ResultType(hydra.ext.java.syntax.UnannType(jt)))

def java_type_to_java_type_argument(t: hydra.ext.java.syntax.Type) -> hydra.ext.java.syntax.TypeArgument:
    match t:
        case hydra.ext.java.syntax.TypeReference(value=rt):
            return cast(hydra.ext.java.syntax.TypeArgument, hydra.ext.java.syntax.TypeArgumentReference(rt))
        
        case hydra.ext.java.syntax.TypePrimitive():
            return cast(hydra.ext.java.syntax.TypeArgument, hydra.ext.java.syntax.TypeArgumentWildcard(hydra.ext.java.syntax.Wildcard((), Nothing())))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def java_type_variable(v: str) -> hydra.ext.java.syntax.ReferenceType:
    return cast(hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.ReferenceTypeVariable(hydra.ext.java.syntax.TypeVariable((), java_type_identifier(hydra.formatting.capitalize(v)))))

def java_unary_expression_to_java_expression(ue: hydra.ext.java.syntax.UnaryExpression) -> hydra.ext.java.syntax.Expression:
    return cast(hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.ExpressionAssignment(cast(hydra.ext.java.syntax.AssignmentExpression, hydra.ext.java.syntax.AssignmentExpressionConditional(cast(hydra.ext.java.syntax.ConditionalExpression, hydra.ext.java.syntax.ConditionalExpressionSimple(hydra.ext.java.syntax.ConditionalOrExpression((hydra.ext.java.syntax.ConditionalAndExpression((hydra.ext.java.syntax.InclusiveOrExpression((hydra.ext.java.syntax.ExclusiveOrExpression((hydra.ext.java.syntax.AndExpression((cast(hydra.ext.java.syntax.EqualityExpression, hydra.ext.java.syntax.EqualityExpressionUnary(cast(hydra.ext.java.syntax.RelationalExpression, hydra.ext.java.syntax.RelationalExpressionSimple(cast(hydra.ext.java.syntax.ShiftExpression, hydra.ext.java.syntax.ShiftExpressionUnary(cast(hydra.ext.java.syntax.AdditiveExpression, hydra.ext.java.syntax.AdditiveExpressionUnary(cast(hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.MultiplicativeExpressionUnary(ue)))))))))),)),)),)),)),))))))))

def java_unary_expression_to_java_relational_expression(ue: hydra.ext.java.syntax.UnaryExpression) -> hydra.ext.java.syntax.RelationalExpression:
    return cast(hydra.ext.java.syntax.RelationalExpression, hydra.ext.java.syntax.RelationalExpressionSimple(cast(hydra.ext.java.syntax.ShiftExpression, hydra.ext.java.syntax.ShiftExpressionUnary(cast(hydra.ext.java.syntax.AdditiveExpression, hydra.ext.java.syntax.AdditiveExpressionUnary(cast(hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.MultiplicativeExpressionUnary(ue))))))))

def lookup_java_var_name(aliases: hydra.ext.java.helpers.Aliases, name: hydra.core.Name) -> hydra.core.Name:
    return hydra.lib.maybes.cases(hydra.lib.maps.lookup(name, aliases.var_renames), name, (lambda renamed: renamed))

def make_constructor(aliases: hydra.ext.java.helpers.Aliases, el_name: hydra.core.Name, private: bool, params: frozenlist[hydra.ext.java.syntax.FormalParameter], stmts: frozenlist[hydra.ext.java.syntax.BlockStatement]) -> hydra.ext.java.syntax.ClassBodyDeclaration:
    @lru_cache(1)
    def nm() -> hydra.ext.java.syntax.SimpleTypeName:
        return hydra.ext.java.syntax.SimpleTypeName(name_to_java_type_identifier(aliases, False, el_name))
    @lru_cache(1)
    def cons() -> hydra.ext.java.syntax.ConstructorDeclarator:
        return hydra.ext.java.syntax.ConstructorDeclarator((), nm(), Nothing(), params)
    @lru_cache(1)
    def mods() -> frozenlist[hydra.ext.java.syntax.ConstructorModifier]:
        return (hydra.lib.logic.if_else(private, (lambda : cast(hydra.ext.java.syntax.ConstructorModifier, hydra.ext.java.syntax.ConstructorModifierPrivate())), (lambda : cast(hydra.ext.java.syntax.ConstructorModifier, hydra.ext.java.syntax.ConstructorModifierPublic()))),)
    @lru_cache(1)
    def body() -> hydra.ext.java.syntax.ConstructorBody:
        return hydra.ext.java.syntax.ConstructorBody(Nothing(), stmts)
    return cast(hydra.ext.java.syntax.ClassBodyDeclaration, hydra.ext.java.syntax.ClassBodyDeclarationConstructorDeclaration(hydra.ext.java.syntax.ConstructorDeclaration(mods(), cons(), Nothing(), body())))

def method_declaration(mods: frozenlist[hydra.ext.java.syntax.MethodModifier], tparams: frozenlist[hydra.ext.java.syntax.TypeParameter], anns: frozenlist[hydra.ext.java.syntax.Annotation], method_name: str, params: frozenlist[hydra.ext.java.syntax.FormalParameter], result: hydra.ext.java.syntax.Result, stmts: Maybe[frozenlist[hydra.ext.java.syntax.BlockStatement]]) -> hydra.ext.java.syntax.ClassBodyDeclaration:
    return java_method_declaration_to_java_class_body_declaration(hydra.ext.java.syntax.MethodDeclaration(anns, mods, java_method_header(tparams, method_name, params, result), java_method_body(stmts)))

def method_invocation(lhs2: Maybe[Either[hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary]], method_name: hydra.ext.java.syntax.Identifier, args: frozenlist[hydra.ext.java.syntax.Expression]) -> hydra.ext.java.syntax.MethodInvocation:
    @lru_cache(1)
    def header() -> hydra.ext.java.syntax.MethodInvocation_Header:
        return hydra.lib.maybes.cases(lhs2, cast(hydra.ext.java.syntax.MethodInvocation_Header, hydra.ext.java.syntax.MethodInvocation_HeaderSimple(hydra.ext.java.syntax.MethodName(method_name))), (lambda either: cast(hydra.ext.java.syntax.MethodInvocation_Header, hydra.ext.java.syntax.MethodInvocation_HeaderComplex(hydra.ext.java.syntax.MethodInvocation_Complex(hydra.lib.eithers.either((lambda en: cast(hydra.ext.java.syntax.MethodInvocation_Variant, hydra.ext.java.syntax.MethodInvocation_VariantExpression(en))), (lambda p: cast(hydra.ext.java.syntax.MethodInvocation_Variant, hydra.ext.java.syntax.MethodInvocation_VariantPrimary(p))), either), (), method_name)))))
    return hydra.ext.java.syntax.MethodInvocation(header(), args)

def method_invocation_static(self: hydra.ext.java.syntax.Identifier, method_name: hydra.ext.java.syntax.Identifier, args: frozenlist[hydra.ext.java.syntax.Expression]) -> hydra.ext.java.syntax.MethodInvocation:
    return method_invocation(Just(Left(java_identifier_to_java_expression_name(self))), method_name, args)

def method_invocation_static_with_type_args(self: hydra.ext.java.syntax.Identifier, method_name: hydra.ext.java.syntax.Identifier, targs: frozenlist[hydra.ext.java.syntax.TypeArgument], args: frozenlist[hydra.ext.java.syntax.Expression]) -> hydra.ext.java.syntax.MethodInvocation:
    @lru_cache(1)
    def header() -> hydra.ext.java.syntax.MethodInvocation_Header:
        return cast(hydra.ext.java.syntax.MethodInvocation_Header, hydra.ext.java.syntax.MethodInvocation_HeaderComplex(hydra.ext.java.syntax.MethodInvocation_Complex(cast(hydra.ext.java.syntax.MethodInvocation_Variant, hydra.ext.java.syntax.MethodInvocation_VariantExpression(java_identifier_to_java_expression_name(self))), targs, method_name)))
    return hydra.ext.java.syntax.MethodInvocation(header(), args)

def name_to_java_name(aliases: hydra.ext.java.helpers.Aliases, name: hydra.core.Name) -> hydra.ext.java.syntax.Identifier:
    @lru_cache(1)
    def qn() -> hydra.module.QualifiedName:
        return hydra.names.qualify_name(name)
    @lru_cache(1)
    def ns_() -> Maybe[hydra.module.Namespace]:
        return qn().namespace
    @lru_cache(1)
    def local() -> str:
        return qn().local
    return hydra.lib.logic.if_else(is_escaped(name.value), (lambda : hydra.ext.java.syntax.Identifier(sanitize_java_name(local()))), (lambda : hydra.lib.maybes.cases(ns_(), hydra.ext.java.syntax.Identifier(local()), (lambda gname: (parts := hydra.lib.maybes.cases(hydra.lib.maps.lookup(gname, aliases.packages), hydra.lib.strings.split_on(".", gname.value), (lambda pkg_name: hydra.lib.lists.map((lambda i: i.value), pkg_name.value))), all_parts := hydra.lib.lists.concat2(parts, (sanitize_java_name(local()),)), hydra.ext.java.syntax.Identifier(hydra.lib.strings.intercalate(".", all_parts)))[2]))))

def name_to_java_reference_type(aliases: hydra.ext.java.helpers.Aliases, qualify: bool, args: frozenlist[hydra.ext.java.syntax.TypeArgument], name: hydra.core.Name, mlocal: Maybe[str]) -> hydra.ext.java.syntax.ReferenceType:
    return cast(hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.ReferenceTypeClassOrInterface(cast(hydra.ext.java.syntax.ClassOrInterfaceType, hydra.ext.java.syntax.ClassOrInterfaceTypeClass(name_to_java_class_type(aliases, qualify, args, name, mlocal)))))

@lru_cache(1)
def override_annotation() -> hydra.ext.java.syntax.Annotation:
    return cast(hydra.ext.java.syntax.Annotation, hydra.ext.java.syntax.AnnotationMarker(hydra.ext.java.syntax.MarkerAnnotation(java_type_name(hydra.ext.java.syntax.Identifier("Override")))))

def reference_type_to_result(rt: hydra.ext.java.syntax.ReferenceType) -> hydra.ext.java.syntax.Result:
    return java_type_to_java_result(cast(hydra.ext.java.syntax.Type, hydra.ext.java.syntax.TypeReference(rt)))

@lru_cache(1)
def suppress_warnings_unchecked_annotation() -> hydra.ext.java.syntax.Annotation:
    return cast(hydra.ext.java.syntax.Annotation, hydra.ext.java.syntax.AnnotationSingleElement(hydra.ext.java.syntax.SingleElementAnnotation(java_type_name(hydra.ext.java.syntax.Identifier("SuppressWarnings")), Just(cast(hydra.ext.java.syntax.ElementValue, hydra.ext.java.syntax.ElementValueConditionalExpression(cast(hydra.ext.java.syntax.ConditionalExpression, hydra.ext.java.syntax.ConditionalExpressionSimple(hydra.ext.java.syntax.ConditionalOrExpression((hydra.ext.java.syntax.ConditionalAndExpression((java_postfix_expression_to_java_inclusive_or_expression(cast(hydra.ext.java.syntax.PostfixExpression, hydra.ext.java.syntax.PostfixExpressionPrimary(java_literal_to_java_primary(java_string("unchecked"))))),)),))))))))))

def type_parameter_to_reference_type(tp: hydra.ext.java.syntax.TypeParameter) -> hydra.ext.java.syntax.ReferenceType:
    return java_type_variable(tp.identifier.value.value)

@lru_cache(1)
def visitor_type_variable() -> hydra.ext.java.syntax.ReferenceType:
    return java_type_variable("r")

def to_accept_method(abstract: bool, vtparams: frozenlist[hydra.ext.java.syntax.TypeParameter]) -> hydra.ext.java.syntax.ClassBodyDeclaration:
    @lru_cache(1)
    def mods() -> frozenlist[hydra.ext.java.syntax.MethodModifier]:
        return hydra.lib.logic.if_else(abstract, (lambda : (cast(hydra.ext.java.syntax.MethodModifier, hydra.ext.java.syntax.MethodModifierPublic()), cast(hydra.ext.java.syntax.MethodModifier, hydra.ext.java.syntax.MethodModifierAbstract()))), (lambda : (cast(hydra.ext.java.syntax.MethodModifier, hydra.ext.java.syntax.MethodModifierPublic()),)))
    @lru_cache(1)
    def tparams() -> frozenlist[hydra.ext.java.syntax.TypeParameter]:
        return (java_type_parameter(hydra.ext.java.names.visitor_return_parameter),)
    @lru_cache(1)
    def anns() -> frozenlist[hydra.ext.java.syntax.Annotation]:
        return hydra.lib.logic.if_else(abstract, (lambda : ()), (lambda : (override_annotation(),)))
    @lru_cache(1)
    def type_args() -> frozenlist[hydra.ext.java.syntax.TypeArgument]:
        return hydra.lib.lists.map((lambda tp: cast(hydra.ext.java.syntax.TypeArgument, hydra.ext.java.syntax.TypeArgumentReference(type_parameter_to_reference_type(tp)))), vtparams)
    @lru_cache(1)
    def ref() -> hydra.ext.java.syntax.Type:
        return java_class_type_to_java_type(hydra.ext.java.syntax.ClassType((), cast(hydra.ext.java.syntax.ClassTypeQualifier, hydra.ext.java.syntax.ClassTypeQualifierNone()), java_type_identifier(hydra.ext.java.names.visitor_name), hydra.lib.lists.concat2(type_args(), (cast(hydra.ext.java.syntax.TypeArgument, hydra.ext.java.syntax.TypeArgumentReference(visitor_type_variable())),))))
    @lru_cache(1)
    def param() -> hydra.ext.java.syntax.FormalParameter:
        return java_type_to_java_formal_parameter(ref(), hydra.core.Name("visitor"))
    @lru_cache(1)
    def result() -> hydra.ext.java.syntax.Result:
        return java_type_to_java_result(cast(hydra.ext.java.syntax.Type, hydra.ext.java.syntax.TypeReference(visitor_type_variable())))
    @lru_cache(1)
    def return_expr() -> hydra.ext.java.syntax.Expression:
        return java_method_invocation_to_java_expression(method_invocation_static(hydra.ext.java.syntax.Identifier("visitor"), hydra.ext.java.syntax.Identifier(hydra.ext.java.names.visit_method_name), (java_this,)))
    @lru_cache(1)
    def body() -> Maybe[frozenlist[hydra.ext.java.syntax.BlockStatement]]:
        return hydra.lib.logic.if_else(abstract, (lambda : Nothing()), (lambda : Just((cast(hydra.ext.java.syntax.BlockStatement, hydra.ext.java.syntax.BlockStatementStatement(java_return_statement(Just(return_expr())))),))))
    return method_declaration(mods(), tparams(), anns(), hydra.ext.java.names.accept_method_name, (param(),), result(), body())

def to_assign_stmt(fname: hydra.core.Name) -> hydra.ext.java.syntax.Statement:
    @lru_cache(1)
    def id() -> hydra.ext.java.syntax.Identifier:
        return field_name_to_java_identifier(fname)
    @lru_cache(1)
    def lhs() -> hydra.ext.java.syntax.LeftHandSide:
        return cast(hydra.ext.java.syntax.LeftHandSide, hydra.ext.java.syntax.LeftHandSideFieldAccess(hydra.ext.java.syntax.FieldAccess(cast(hydra.ext.java.syntax.FieldAccess_Qualifier, hydra.ext.java.syntax.FieldAccess_QualifierPrimary(cast(hydra.ext.java.syntax.Primary, hydra.ext.java.syntax.PrimaryNoNewArray(cast(hydra.ext.java.syntax.PrimaryNoNewArray, hydra.ext.java.syntax.PrimaryNoNewArrayThis()))))), id())))
    @lru_cache(1)
    def rhs() -> hydra.ext.java.syntax.Expression:
        return field_name_to_java_expression(fname)
    return java_assignment_statement(lhs(), rhs())

def to_java_array_type(t: hydra.ext.java.syntax.Type) -> hydra.compute.Flow[T0, hydra.ext.java.syntax.Type]:
    def _hoist_hydra_ext_java_utils_to_java_array_type_1(v1: hydra.ext.java.syntax.ReferenceType) -> hydra.compute.Flow[T1, hydra.ext.java.syntax.Type]:
        match v1:
            case hydra.ext.java.syntax.ReferenceTypeClassOrInterface(value=cit):
                return hydra.lib.flows.pure(cast(hydra.ext.java.syntax.Type, hydra.ext.java.syntax.TypeReference(cast(hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.ReferenceTypeArray(hydra.ext.java.syntax.ArrayType(hydra.ext.java.syntax.Dims(((),)), cast(hydra.ext.java.syntax.ArrayType_Variant, hydra.ext.java.syntax.ArrayType_VariantClassOrInterface(cit))))))))
            
            case hydra.ext.java.syntax.ReferenceTypeArray(value=at):
                @lru_cache(1)
                def old_dims() -> frozenlist[frozenlist[hydra.ext.java.syntax.Annotation]]:
                    return at.dims.value
                @lru_cache(1)
                def new_dims() -> hydra.ext.java.syntax.Dims:
                    return hydra.ext.java.syntax.Dims(hydra.lib.lists.concat2(old_dims(), ((),)))
                @lru_cache(1)
                def variant() -> hydra.ext.java.syntax.ArrayType_Variant:
                    return at.variant
                return hydra.lib.flows.pure(cast(hydra.ext.java.syntax.Type, hydra.ext.java.syntax.TypeReference(cast(hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.ReferenceTypeArray(hydra.ext.java.syntax.ArrayType(new_dims(), variant()))))))
            
            case hydra.ext.java.syntax.ReferenceTypeVariable():
                return hydra.lib.flows.fail("don't know how to make Java reference type into array type")
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    match t:
        case hydra.ext.java.syntax.TypeReference(value=rt):
            return _hoist_hydra_ext_java_utils_to_java_array_type_1(rt)
        
        case hydra.ext.java.syntax.TypePrimitive():
            return hydra.lib.flows.fail("don't know how to make Java type into array type")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def type_parameter_to_type_argument(tp: hydra.ext.java.syntax.TypeParameter) -> hydra.ext.java.syntax.TypeArgument:
    return java_type_identifier_to_java_type_argument(tp.identifier)

def un_type_parameter(tp: hydra.ext.java.syntax.TypeParameter) -> str:
    return tp.identifier.value.value

def unique_var_name_go(aliases: hydra.ext.java.helpers.Aliases, base: str, n: int) -> hydra.core.Name:
    @lru_cache(1)
    def candidate() -> hydra.core.Name:
        return hydra.core.Name(hydra.lib.strings.cat2(base, hydra.lib.literals.show_int32(n)))
    return hydra.lib.logic.if_else(hydra.lib.sets.member(candidate(), aliases.in_scope_java_vars), (lambda : unique_var_name_go(aliases, base, hydra.lib.math.add(n, 1))), (lambda : candidate()))

def unique_var_name(aliases: hydra.ext.java.helpers.Aliases, name: hydra.core.Name) -> hydra.core.Name:
    return hydra.lib.logic.if_else(hydra.lib.sets.member(name, aliases.in_scope_java_vars), (lambda : unique_var_name_go(aliases, name.value, 2)), (lambda : name))

def var_declaration_statement(id: hydra.ext.java.syntax.Identifier, rhs2: hydra.ext.java.syntax.Expression) -> hydra.ext.java.syntax.BlockStatement:
    return cast(hydra.ext.java.syntax.BlockStatement, hydra.ext.java.syntax.BlockStatementLocalVariableDeclaration(hydra.ext.java.syntax.LocalVariableDeclarationStatement(hydra.ext.java.syntax.LocalVariableDeclaration((), cast(hydra.ext.java.syntax.LocalVariableType, hydra.ext.java.syntax.LocalVariableTypeVar()), (java_variable_declarator(id, Just(cast(hydra.ext.java.syntax.VariableInitializer, hydra.ext.java.syntax.VariableInitializerExpression(rhs2)))),)))))

def variable_declaration_statement(aliases: T0, jtype: hydra.ext.java.syntax.Type, id: hydra.ext.java.syntax.Identifier, rhs2: hydra.ext.java.syntax.Expression) -> hydra.ext.java.syntax.BlockStatement:
    @lru_cache(1)
    def init_() -> hydra.ext.java.syntax.VariableInitializer:
        return cast(hydra.ext.java.syntax.VariableInitializer, hydra.ext.java.syntax.VariableInitializerExpression(rhs2))
    @lru_cache(1)
    def vdec() -> hydra.ext.java.syntax.VariableDeclarator:
        return java_variable_declarator(id, Just(init_()))
    return cast(hydra.ext.java.syntax.BlockStatement, hydra.ext.java.syntax.BlockStatementLocalVariableDeclaration(hydra.ext.java.syntax.LocalVariableDeclarationStatement(hydra.ext.java.syntax.LocalVariableDeclaration((), cast(hydra.ext.java.syntax.LocalVariableType, hydra.ext.java.syntax.LocalVariableTypeType(hydra.ext.java.syntax.UnannType(jtype))), (vdec(),)))))

def variant_class_name(qualify: bool, el_name: hydra.core.Name, fname: hydra.core.Name) -> hydra.core.Name:
    @lru_cache(1)
    def qn() -> hydra.module.QualifiedName:
        return hydra.names.qualify_name(el_name)
    @lru_cache(1)
    def ns_() -> Maybe[hydra.module.Namespace]:
        return qn().namespace
    @lru_cache(1)
    def local() -> str:
        return qn().local
    @lru_cache(1)
    def flocal() -> str:
        return hydra.formatting.capitalize(fname.value)
    @lru_cache(1)
    def local1() -> str:
        return hydra.lib.logic.if_else(qualify, (lambda : hydra.lib.strings.cat2(hydra.lib.strings.cat2(local(), "."), flocal())), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(flocal(), local()), (lambda : hydra.lib.strings.cat2(flocal(), "_")), (lambda : flocal()))))
    return hydra.names.unqualify_name(hydra.module.QualifiedName(ns_(), local1()))
