# Note: this is an automatically generated file. Do not edit.

r"""Java serializer: converts Java AST to concrete syntax."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Just, Maybe, Nothing, frozenlist
from typing import TypeVar
import hydra.constants
import hydra.core
import hydra.ext.java.syntax
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.strings
import hydra.serialization

T0 = TypeVar("T0")

def hex_digit(n: int) -> int:
    return hydra.lib.logic.if_else(hydra.lib.equality.lt(n, 10), (lambda : hydra.lib.math.add(n, 48)), (lambda : hydra.lib.math.add(hydra.lib.math.sub(n, 10), 65)))

def pad_hex4(n: int) -> str:
    @lru_cache(1)
    def d3() -> int:
        return hydra.lib.math.div(n, 4096)
    @lru_cache(1)
    def r3() -> int:
        return hydra.lib.math.mod(n, 4096)
    @lru_cache(1)
    def d2() -> int:
        return hydra.lib.math.div(r3(), 256)
    @lru_cache(1)
    def r2() -> int:
        return hydra.lib.math.mod(r3(), 256)
    @lru_cache(1)
    def d1() -> int:
        return hydra.lib.math.div(r2(), 16)
    @lru_cache(1)
    def d0() -> int:
        return hydra.lib.math.mod(r2(), 16)
    return hydra.lib.strings.from_list((hex_digit(d3()), hex_digit(d2()), hex_digit(d1()), hex_digit(d0())))

def java_unicode_escape(n: int) -> str:
    return hydra.lib.logic.if_else(hydra.lib.equality.gt(n, 65535), (lambda : (n_ := hydra.lib.math.sub(n, 65536), (hi := hydra.lib.math.add(55296, hydra.lib.math.div(n_, 1024)), (lo := hydra.lib.math.add(56320, hydra.lib.math.mod(n_, 1024)), hydra.lib.strings.cat2(hydra.lib.strings.cat2("\\u", pad_hex4(hi)), hydra.lib.strings.cat2("\\u", pad_hex4(lo))))[1])[1])[1]), (lambda : hydra.lib.strings.cat2("\\u", pad_hex4(n))))

def escape_java_char(c: int) -> str:
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(c, 34), (lambda : "\\\""), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(c, 92), (lambda : "\\\\"), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(c, 10), (lambda : "\\n"), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(c, 13), (lambda : "\\r"), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(c, 9), (lambda : "\\t"), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(c, 8), (lambda : "\\b"), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(c, 12), (lambda : "\\f"), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.gte(c, 32), hydra.lib.equality.lt(c, 127)), (lambda : hydra.lib.strings.from_list((c,))), (lambda : java_unicode_escape(c)))))))))))))))))

def escape_java_string(s: str) -> str:
    return hydra.lib.strings.cat(hydra.lib.lists.map((lambda c: escape_java_char(c)), hydra.lib.strings.to_list(s)))

def sanitize_java_comment(s: str) -> str:
    r"""Sanitize a string for use in a Java comment."""
    
    return hydra.lib.strings.intercalate("&gt;", hydra.lib.strings.split_on(">", hydra.lib.strings.intercalate("&lt;", hydra.lib.strings.split_on("<", s))))

def single_line_comment(c: str) -> hydra.ast.Expr:
    r"""Create a single-line Java comment."""
    
    return hydra.serialization.cst(hydra.lib.strings.cat2("// ", sanitize_java_comment(c)))

def with_comments(mc: Maybe[str], expr: hydra.ast.Expr) -> hydra.ast.Expr:
    r"""Wrap an expression with optional Javadoc comments."""
    
    return hydra.lib.maybes.maybe(expr, (lambda c: hydra.serialization.newline_sep((hydra.serialization.cst(hydra.lib.strings.cat2("/**\n", hydra.lib.strings.cat2(hydra.lib.strings.intercalate("\n", hydra.lib.lists.map((lambda l: hydra.lib.strings.cat2(" * ", l)), hydra.lib.strings.lines(sanitize_java_comment(c)))), "\n */"))), expr))), mc)

def write_identifier(id: hydra.ext.java.syntax.Identifier) -> hydra.ast.Expr:
    return hydra.serialization.cst(id.value)

def write_package_or_type_name(potn: hydra.ext.java.syntax.PackageOrTypeName) -> hydra.ast.Expr:
    return hydra.serialization.dot_sep(hydra.lib.lists.map((lambda x1: write_identifier(x1)), potn.value))

def write_type_identifier(tid: hydra.ext.java.syntax.TypeIdentifier) -> hydra.ast.Expr:
    return write_identifier(tid.value)

def write_type_name(tn: hydra.ext.java.syntax.TypeName) -> hydra.ast.Expr:
    @lru_cache(1)
    def id() -> hydra.ext.java.syntax.TypeIdentifier:
        return tn.identifier
    @lru_cache(1)
    def mqual() -> Maybe[hydra.ext.java.syntax.PackageOrTypeName]:
        return tn.qualifier
    return hydra.serialization.dot_sep(hydra.lib.maybes.cat((hydra.lib.maybes.map((lambda x1: write_package_or_type_name(x1)), mqual()), Just(write_type_identifier(id())))))

def write_marker_annotation(ma: hydra.ext.java.syntax.MarkerAnnotation) -> hydra.ast.Expr:
    return hydra.serialization.prefix("@", write_type_name(ma.value))

def write_conditional_expression_ternary_cond(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:ConditionalExpression_TernaryCond")

def write_conditional_expression_ternary_lambda(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:ConditionalExpression_TernaryLambda")

def write_pre_decrement_expression(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:PreDecrementExpression")

def write_pre_increment_expression(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:PreIncrementExpression")

def write_cast_expression_lambda(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:CastExpression_Lambda")

def write_dims(d: hydra.ext.java.syntax.Dims) -> hydra.ast.Expr:
    return hydra.serialization.no_sep(hydra.lib.lists.map((lambda _: hydra.serialization.cst("[]")), d.value))

def write_floating_point_type(ft: hydra.ext.java.syntax.FloatingPointType) -> hydra.ast.Expr:
    match ft:
        case hydra.ext.java.syntax.FloatingPointType.FLOAT:
            return hydra.serialization.cst("float")
        
        case hydra.ext.java.syntax.FloatingPointType.DOUBLE:
            return hydra.serialization.cst("double")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_integral_type(t: hydra.ext.java.syntax.IntegralType) -> hydra.ast.Expr:
    match t:
        case hydra.ext.java.syntax.IntegralType.BYTE:
            return hydra.serialization.cst("byte")
        
        case hydra.ext.java.syntax.IntegralType.SHORT:
            return hydra.serialization.cst("short")
        
        case hydra.ext.java.syntax.IntegralType.INT:
            return hydra.serialization.cst("int")
        
        case hydra.ext.java.syntax.IntegralType.LONG:
            return hydra.serialization.cst("long")
        
        case hydra.ext.java.syntax.IntegralType.CHAR:
            return hydra.serialization.cst("char")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_numeric_type(nt: hydra.ext.java.syntax.NumericType) -> hydra.ast.Expr:
    match nt:
        case hydra.ext.java.syntax.NumericTypeIntegral(value=it):
            return write_integral_type(it)
        
        case hydra.ext.java.syntax.NumericTypeFloatingPoint(value=ft):
            return write_floating_point_type(ft)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_primitive_type(pt: hydra.ext.java.syntax.PrimitiveType) -> hydra.ast.Expr:
    match pt:
        case hydra.ext.java.syntax.PrimitiveTypeNumeric(value=nt):
            return write_numeric_type(nt)
        
        case hydra.ext.java.syntax.PrimitiveTypeBoolean():
            return hydra.serialization.cst("boolean")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_ambiguous_name(an: hydra.ext.java.syntax.AmbiguousName) -> hydra.ast.Expr:
    return hydra.serialization.dot_sep(hydra.lib.lists.map((lambda x1: write_identifier(x1)), an.value))

def write_expression_name(en: hydra.ext.java.syntax.ExpressionName) -> hydra.ast.Expr:
    @lru_cache(1)
    def mqual() -> Maybe[hydra.ext.java.syntax.AmbiguousName]:
        return en.qualifier
    @lru_cache(1)
    def id() -> hydra.ext.java.syntax.Identifier:
        return en.identifier
    return hydra.serialization.dot_sep(hydra.lib.maybes.cat((hydra.lib.maybes.map((lambda x1: write_ambiguous_name(x1)), mqual()), Just(write_identifier(id())))))

def write_post_decrement_expression(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:PostDecrementExpression")

def write_post_increment_expression(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:PostIncrementExpression")

def write_array_access(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:ArrayAccess")

def write_enum_declaration(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:EnumDeclaration")

def write_variable_declarator_id(vdi: hydra.ext.java.syntax.VariableDeclaratorId) -> hydra.ast.Expr:
    @lru_cache(1)
    def id() -> hydra.ext.java.syntax.Identifier:
        return vdi.identifier
    @lru_cache(1)
    def mdims() -> Maybe[hydra.ext.java.syntax.Dims]:
        return vdi.dims
    return hydra.serialization.no_sep(hydra.lib.maybes.cat((Just(write_identifier(id())), hydra.lib.maybes.map((lambda x1: write_dims(x1)), mdims()))))

def write_annotation_type_declaration(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:AnnotationTypeDeclaration")

def write_constant_modifier(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:ConstantModifier")

def write_variable_arity_parameter(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:VariableArityParameter")

def write_throws(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:Throws")

def write_explicit_constructor_invocation(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:ExplicitConstructorInvocation")

def write_simple_type_name(stn: hydra.ext.java.syntax.SimpleTypeName) -> hydra.ast.Expr:
    return write_type_identifier(stn.value)

def write_instance_initializer(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:InstanceInitializer")

def write_static_initializer(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:StaticInitializer")

def write_for_statement(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:ForStatement")

def write_if_then_else_statement(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:IfThenElseStatement")

def write_labeled_statement(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:LabeledStatement")

def write_assert_statement(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:AssertStatement")

def write_break_statement(bs: hydra.ext.java.syntax.BreakStatement) -> hydra.ast.Expr:
    @lru_cache(1)
    def mlabel() -> Maybe[hydra.ext.java.syntax.Identifier]:
        return bs.value
    return hydra.serialization.with_semi(hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(hydra.serialization.cst("break")), hydra.lib.maybes.map((lambda x1: write_identifier(x1)), mlabel())))))

def write_continue_statement(cs: hydra.ext.java.syntax.ContinueStatement) -> hydra.ast.Expr:
    @lru_cache(1)
    def mlabel() -> Maybe[hydra.ext.java.syntax.Identifier]:
        return cs.value
    return hydra.serialization.with_semi(hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(hydra.serialization.cst("continue")), hydra.lib.maybes.map((lambda x1: write_identifier(x1)), mlabel())))))

def write_do_statement(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:DoStatement")

def write_annotated_identifier(ai: hydra.ext.java.syntax.AnnotatedIdentifier) -> hydra.ast.Expr:
    return write_identifier(ai.identifier)

def write_method_name(mn: hydra.ext.java.syntax.MethodName) -> hydra.ast.Expr:
    return write_identifier(mn.value)

def write_switch_statement(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:SwitchStatement")

def write_synchronized_statement(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:SynchronizedStatement")

def write_try_statement(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:TryStatement")

def write_while_statement(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:WhileStatement")

def write_lambda_parameters(p: hydra.ext.java.syntax.LambdaParameters) -> hydra.ast.Expr:
    match p:
        case hydra.ext.java.syntax.LambdaParametersTuple(value=l):
            return hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: write_lambda_parameters(x1)), l))
        
        case hydra.ext.java.syntax.LambdaParametersSingle(value=id):
            return write_identifier(id)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_class_literal(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:ClassLiteral")

def write_floating_point_literal(fl: hydra.ext.java.syntax.FloatingPointLiteral) -> hydra.ast.Expr:
    return hydra.serialization.cst(hydra.lib.literals.show_bigfloat(fl.value))

def write_integer_literal(il: hydra.ext.java.syntax.IntegerLiteral) -> hydra.ast.Expr:
    @lru_cache(1)
    def i() -> int:
        return il.value
    @lru_cache(1)
    def suffix() -> str:
        return hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.lib.equality.gt(i(), 2147483647), hydra.lib.equality.lt(i(), -2147483648)), (lambda : "L"), (lambda : ""))
    return hydra.serialization.cst(hydra.lib.strings.cat2(hydra.lib.literals.show_bigint(i()), suffix()))

def write_string_literal(sl: hydra.ext.java.syntax.StringLiteral) -> hydra.ast.Expr:
    r"""Serialize a Java string literal with proper Unicode escaping."""
    
    @lru_cache(1)
    def s() -> str:
        return sl.value
    return hydra.serialization.cst(hydra.lib.strings.cat2("\"", hydra.lib.strings.cat2(escape_java_string(s()), "\"")))

def write_literal(l: hydra.ext.java.syntax.Literal) -> hydra.ast.Expr:
    match l:
        case hydra.ext.java.syntax.LiteralNull():
            return hydra.serialization.cst("null")
        
        case hydra.ext.java.syntax.LiteralInteger(value=il):
            return write_integer_literal(il)
        
        case hydra.ext.java.syntax.LiteralFloatingPoint(value=fl):
            return write_floating_point_literal(fl)
        
        case hydra.ext.java.syntax.LiteralBoolean(value=b):
            return hydra.serialization.cst(hydra.lib.logic.if_else(b, (lambda : "true"), (lambda : "false")))
        
        case hydra.ext.java.syntax.LiteralCharacter(value=c):
            @lru_cache(1)
            def ci() -> int:
                return hydra.lib.literals.bigint_to_int32(hydra.lib.literals.uint16_to_bigint(c))
            return hydra.serialization.cst(hydra.lib.strings.cat2("'", hydra.lib.strings.cat2(hydra.lib.logic.if_else(hydra.lib.equality.equal(ci(), 39), (lambda : "\\'"), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(ci(), 92), (lambda : "\\\\"), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(ci(), 10), (lambda : "\\n"), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(ci(), 13), (lambda : "\\r"), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(ci(), 9), (lambda : "\\t"), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.gte(ci(), 32), hydra.lib.equality.lt(ci(), 127)), (lambda : hydra.lib.strings.from_list((ci(),))), (lambda : java_unicode_escape(ci()))))))))))))), "'")))
        
        case hydra.ext.java.syntax.LiteralString(value=sl):
            return write_string_literal(sl)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_method_reference(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:MethodReference")

def write_package_name(pn: hydra.ext.java.syntax.PackageName) -> hydra.ast.Expr:
    return hydra.serialization.dot_sep(hydra.lib.lists.map((lambda x1: write_identifier(x1)), pn.value))

def write_additional_bound(ab: hydra.ext.java.syntax.AdditionalBound) -> hydra.ast.Expr:
    return hydra.serialization.space_sep((hydra.serialization.cst("&"), write_interface_type(ab.value)))

def write_additive_expression(e: hydra.ext.java.syntax.AdditiveExpression) -> hydra.ast.Expr:
    match e:
        case hydra.ext.java.syntax.AdditiveExpressionUnary(value=m):
            return write_multiplicative_expression(m)
        
        case hydra.ext.java.syntax.AdditiveExpressionPlus(value=b):
            return hydra.serialization.infix_ws("+", write_additive_expression(b.lhs), write_multiplicative_expression(b.rhs))
        
        case hydra.ext.java.syntax.AdditiveExpressionMinus(value=b2):
            return hydra.serialization.infix_ws("-", write_additive_expression(b2.lhs), write_multiplicative_expression(b2.rhs))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_and_expression(ae: hydra.ext.java.syntax.AndExpression) -> hydra.ast.Expr:
    return hydra.serialization.infix_ws_list("&", hydra.lib.lists.map((lambda x1: write_equality_expression(x1)), ae.value))

def write_annotation(ann: hydra.ext.java.syntax.Annotation) -> hydra.ast.Expr:
    match ann:
        case hydra.ext.java.syntax.AnnotationNormal(value=n):
            return write_normal_annotation(n)
        
        case hydra.ext.java.syntax.AnnotationMarker(value=m):
            return write_marker_annotation(m)
        
        case hydra.ext.java.syntax.AnnotationSingleElement(value=s):
            return write_single_element_annotation(s)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_array_creation_expression(ace: hydra.ext.java.syntax.ArrayCreationExpression) -> hydra.ast.Expr:
    match ace:
        case hydra.ext.java.syntax.ArrayCreationExpressionPrimitiveArray(value=pa):
            @lru_cache(1)
            def pt() -> hydra.ext.java.syntax.PrimitiveTypeWithAnnotations:
                return pa.type
            @lru_cache(1)
            def ai() -> hydra.ext.java.syntax.ArrayInitializer:
                return pa.array
            return hydra.serialization.space_sep((hydra.serialization.cst("new"), hydra.serialization.no_sep((write_primitive_type_with_annotations(pt()), hydra.serialization.cst("[]"))), write_array_initializer(ai())))
        
        case hydra.ext.java.syntax.ArrayCreationExpressionClassOrInterfaceArray():
            return hydra.serialization.cst("STUB:ArrayCreationExpression")
        
        case hydra.ext.java.syntax.ArrayCreationExpressionPrimitive():
            return hydra.serialization.cst("STUB:ArrayCreationExpression")
        
        case hydra.ext.java.syntax.ArrayCreationExpressionClassOrInterface():
            return hydra.serialization.cst("STUB:ArrayCreationExpression")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_array_initializer(ai: hydra.ext.java.syntax.ArrayInitializer) -> hydra.ast.Expr:
    @lru_cache(1)
    def groups() -> frozenlist[frozenlist[hydra.ext.java.syntax.VariableInitializer]]:
        return ai.value
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(groups()), 1), (lambda : hydra.serialization.no_sep((hydra.serialization.cst("{"), hydra.serialization.comma_sep(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: write_variable_initializer(x1)), hydra.lib.lists.head(groups()))), hydra.serialization.cst("}")))), (lambda : hydra.serialization.cst("{}")))

def write_array_type(at: hydra.ext.java.syntax.ArrayType) -> hydra.ast.Expr:
    @lru_cache(1)
    def dims() -> hydra.ext.java.syntax.Dims:
        return at.dims
    @lru_cache(1)
    def variant() -> hydra.ext.java.syntax.ArrayType_Variant:
        return at.variant
    @lru_cache(1)
    def var_expr() -> hydra.ast.Expr:
        match variant():
            case hydra.ext.java.syntax.ArrayType_VariantPrimitive(value=pt):
                return write_primitive_type_with_annotations(pt)
            
            case hydra.ext.java.syntax.ArrayType_VariantClassOrInterface(value=cit):
                return write_class_or_interface_type(cit)
            
            case hydra.ext.java.syntax.ArrayType_VariantVariable(value=tv):
                return write_type_variable(tv)
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.serialization.no_sep((var_expr(), write_dims(dims())))

def write_assignment(a: hydra.ext.java.syntax.Assignment) -> hydra.ast.Expr:
    @lru_cache(1)
    def lhs() -> hydra.ext.java.syntax.LeftHandSide:
        return a.lhs
    @lru_cache(1)
    def op() -> hydra.ext.java.syntax.AssignmentOperator:
        return a.op
    @lru_cache(1)
    def rhs() -> hydra.ext.java.syntax.Expression:
        return a.expression
    @lru_cache(1)
    def ctop() -> str:
        match op():
            case hydra.ext.java.syntax.AssignmentOperator.SIMPLE:
                return "="
            
            case hydra.ext.java.syntax.AssignmentOperator.TIMES:
                return "*="
            
            case hydra.ext.java.syntax.AssignmentOperator.DIV:
                return "/="
            
            case hydra.ext.java.syntax.AssignmentOperator.MOD:
                return "%="
            
            case hydra.ext.java.syntax.AssignmentOperator.PLUS:
                return "+="
            
            case hydra.ext.java.syntax.AssignmentOperator.MINUS:
                return "-="
            
            case hydra.ext.java.syntax.AssignmentOperator.SHIFT_LEFT:
                return "<<="
            
            case hydra.ext.java.syntax.AssignmentOperator.SHIFT_RIGHT:
                return ">>="
            
            case hydra.ext.java.syntax.AssignmentOperator.SHIFT_RIGHT_ZERO_FILL:
                return ">>>="
            
            case hydra.ext.java.syntax.AssignmentOperator.AND:
                return "&="
            
            case hydra.ext.java.syntax.AssignmentOperator.XOR:
                return "^="
            
            case hydra.ext.java.syntax.AssignmentOperator.OR:
                return "|="
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.serialization.infix_ws(ctop(), write_left_hand_side(lhs()), write_expression(rhs()))

def write_assignment_expression(e: hydra.ext.java.syntax.AssignmentExpression) -> hydra.ast.Expr:
    match e:
        case hydra.ext.java.syntax.AssignmentExpressionConditional(value=c):
            return write_conditional_expression(c)
        
        case hydra.ext.java.syntax.AssignmentExpressionAssignment(value=a):
            return write_assignment(a)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_block(b: hydra.ext.java.syntax.Block) -> hydra.ast.Expr:
    return hydra.serialization.curly_block(hydra.serialization.full_block_style, hydra.serialization.newline_sep(hydra.lib.lists.map((lambda x1: write_block_statement(x1)), b.value)))

def write_block_statement(s: hydra.ext.java.syntax.BlockStatement) -> hydra.ast.Expr:
    match s:
        case hydra.ext.java.syntax.BlockStatementLocalVariableDeclaration(value=d):
            return write_local_variable_declaration_statement(d)
        
        case hydra.ext.java.syntax.BlockStatementClass(value=cd):
            return write_class_declaration(cd)
        
        case hydra.ext.java.syntax.BlockStatementStatement(value=s2):
            return write_statement(s2)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_cast_expression(e: hydra.ext.java.syntax.CastExpression) -> hydra.ast.Expr:
    match e:
        case hydra.ext.java.syntax.CastExpressionPrimitive(value=p):
            return write_cast_expression_primitive(p)
        
        case hydra.ext.java.syntax.CastExpressionNotPlusMinus(value=npm):
            return write_cast_expression_not_plus_minus(npm)
        
        case hydra.ext.java.syntax.CastExpressionLambda(value=l):
            return write_cast_expression_lambda(l)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_cast_expression_not_plus_minus(npm: hydra.ext.java.syntax.CastExpression_NotPlusMinus) -> hydra.ast.Expr:
    @lru_cache(1)
    def rb() -> hydra.ext.java.syntax.CastExpression_RefAndBounds:
        return npm.ref_and_bounds
    @lru_cache(1)
    def ex() -> hydra.ext.java.syntax.UnaryExpression:
        return npm.expression
    return hydra.serialization.space_sep((write_cast_expression_ref_and_bounds(rb()), write_unary_expression(ex())))

def write_cast_expression_primitive(cp: hydra.ext.java.syntax.CastExpression_Primitive) -> hydra.ast.Expr:
    @lru_cache(1)
    def pt() -> hydra.ext.java.syntax.PrimitiveTypeWithAnnotations:
        return cp.type
    @lru_cache(1)
    def ex() -> hydra.ext.java.syntax.UnaryExpression:
        return cp.expression
    return hydra.serialization.space_sep((hydra.serialization.paren_list(False, (write_primitive_type_with_annotations(pt()),)), write_unary_expression(ex())))

def write_cast_expression_ref_and_bounds(rab: hydra.ext.java.syntax.CastExpression_RefAndBounds) -> hydra.ast.Expr:
    @lru_cache(1)
    def rt() -> hydra.ext.java.syntax.ReferenceType:
        return rab.type
    @lru_cache(1)
    def adds() -> frozenlist[hydra.ext.java.syntax.AdditionalBound]:
        return rab.bounds
    return hydra.serialization.paren_list(False, (hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(write_reference_type(rt())), hydra.lib.logic.if_else(hydra.lib.lists.null(adds()), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_additional_bound(x1)), adds())))))))),))

def write_class_body(cb: hydra.ext.java.syntax.ClassBody) -> hydra.ast.Expr:
    return hydra.serialization.curly_block(hydra.serialization.full_block_style, hydra.serialization.double_newline_sep(hydra.lib.lists.map((lambda x1: write_class_body_declaration_with_comments(x1)), cb.value)))

def write_class_body_declaration(d: hydra.ext.java.syntax.ClassBodyDeclaration) -> hydra.ast.Expr:
    match d:
        case hydra.ext.java.syntax.ClassBodyDeclarationClassMember(value=d2):
            return write_class_member_declaration(d2)
        
        case hydra.ext.java.syntax.ClassBodyDeclarationInstanceInitializer(value=i):
            return write_instance_initializer(i)
        
        case hydra.ext.java.syntax.ClassBodyDeclarationStaticInitializer(value=i2):
            return write_static_initializer(i2)
        
        case hydra.ext.java.syntax.ClassBodyDeclarationConstructorDeclaration(value=d22):
            return write_constructor_declaration(d22)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_class_body_declaration_with_comments(cbdwc: hydra.ext.java.syntax.ClassBodyDeclarationWithComments) -> hydra.ast.Expr:
    @lru_cache(1)
    def d() -> hydra.ext.java.syntax.ClassBodyDeclaration:
        return cbdwc.value
    @lru_cache(1)
    def mc() -> Maybe[str]:
        return cbdwc.comments
    return with_comments(mc(), write_class_body_declaration(d()))

def write_class_declaration(d: hydra.ext.java.syntax.ClassDeclaration) -> hydra.ast.Expr:
    match d:
        case hydra.ext.java.syntax.ClassDeclarationNormal(value=nd):
            return write_normal_class_declaration(nd)
        
        case hydra.ext.java.syntax.ClassDeclarationEnum(value=ed):
            return write_enum_declaration(ed)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_class_instance_creation_expression(cice: hydra.ext.java.syntax.ClassInstanceCreationExpression) -> hydra.ast.Expr:
    @lru_cache(1)
    def mqual() -> Maybe[hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier]:
        return cice.qualifier
    @lru_cache(1)
    def e() -> hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression:
        return cice.expression
    return hydra.lib.maybes.maybe(write_unqualified_class_instance_creation_expression(e()), (lambda q: hydra.serialization.dot_sep((write_class_instance_creation_expression_qualifier(q), write_unqualified_class_instance_creation_expression(e())))), mqual())

def write_class_instance_creation_expression_qualifier(q: hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier) -> hydra.ast.Expr:
    match q:
        case hydra.ext.java.syntax.ClassInstanceCreationExpression_QualifierExpression(value=en):
            return write_expression_name(en)
        
        case hydra.ext.java.syntax.ClassInstanceCreationExpression_QualifierPrimary(value=p):
            return write_primary(p)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_class_member_declaration(d: hydra.ext.java.syntax.ClassMemberDeclaration) -> hydra.ast.Expr:
    match d:
        case hydra.ext.java.syntax.ClassMemberDeclarationField(value=fd):
            return write_field_declaration(fd)
        
        case hydra.ext.java.syntax.ClassMemberDeclarationMethod(value=md):
            return write_method_declaration(md)
        
        case hydra.ext.java.syntax.ClassMemberDeclarationClass(value=cd):
            return write_class_declaration(cd)
        
        case hydra.ext.java.syntax.ClassMemberDeclarationInterface(value=id):
            return write_interface_declaration(id)
        
        case hydra.ext.java.syntax.ClassMemberDeclarationNone():
            return hydra.serialization.cst(";")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_class_modifier(m: hydra.ext.java.syntax.ClassModifier) -> hydra.ast.Expr:
    match m:
        case hydra.ext.java.syntax.ClassModifierAnnotation(value=ann):
            return write_annotation(ann)
        
        case hydra.ext.java.syntax.ClassModifierPublic():
            return hydra.serialization.cst("public")
        
        case hydra.ext.java.syntax.ClassModifierProtected():
            return hydra.serialization.cst("protected")
        
        case hydra.ext.java.syntax.ClassModifierPrivate():
            return hydra.serialization.cst("private")
        
        case hydra.ext.java.syntax.ClassModifierAbstract():
            return hydra.serialization.cst("abstract")
        
        case hydra.ext.java.syntax.ClassModifierStatic():
            return hydra.serialization.cst("static")
        
        case hydra.ext.java.syntax.ClassModifierFinal():
            return hydra.serialization.cst("final")
        
        case hydra.ext.java.syntax.ClassModifierStrictfp():
            return hydra.serialization.cst("strictfp")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_class_or_interface_type(cit: hydra.ext.java.syntax.ClassOrInterfaceType) -> hydra.ast.Expr:
    match cit:
        case hydra.ext.java.syntax.ClassOrInterfaceTypeClass(value=ct):
            return write_class_type(ct)
        
        case hydra.ext.java.syntax.ClassOrInterfaceTypeInterface(value=it):
            return write_interface_type(it)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_class_or_interface_type_to_instantiate(coitti: hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate) -> hydra.ast.Expr:
    @lru_cache(1)
    def ids() -> frozenlist[hydra.ext.java.syntax.AnnotatedIdentifier]:
        return coitti.identifiers
    @lru_cache(1)
    def margs() -> Maybe[hydra.ext.java.syntax.TypeArgumentsOrDiamond]:
        return coitti.type_arguments
    return hydra.serialization.no_sep(hydra.lib.maybes.cat((Just(hydra.serialization.dot_sep(hydra.lib.lists.map((lambda x1: write_annotated_identifier(x1)), ids()))), hydra.lib.maybes.map((lambda x1: write_type_arguments_or_diamond(x1)), margs()))))

def write_class_type(ct: hydra.ext.java.syntax.ClassType) -> hydra.ast.Expr:
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
    @lru_cache(1)
    def qualified_id() -> hydra.ast.Expr:
        match qual():
            case hydra.ext.java.syntax.ClassTypeQualifierNone():
                return write_type_identifier(id())
            
            case hydra.ext.java.syntax.ClassTypeQualifierPackage(value=pkg):
                return hydra.serialization.dot_sep((write_package_name(pkg), write_type_identifier(id())))
            
            case hydra.ext.java.syntax.ClassTypeQualifierParent(value=cit):
                return hydra.serialization.dot_sep((write_class_or_interface_type(cit), write_type_identifier(id())))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.serialization.no_sep(hydra.lib.maybes.cat((Just(hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(anns()), (lambda : Nothing()), (lambda : Just(hydra.serialization.comma_sep(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: write_annotation(x1)), anns()))))), Just(qualified_id()))))), hydra.lib.logic.if_else(hydra.lib.lists.null(args()), (lambda : Nothing()), (lambda : Just(hydra.serialization.angle_braces_list(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: write_type_argument(x1)), args()))))))))

def write_conditional_and_expression(cae: hydra.ext.java.syntax.ConditionalAndExpression) -> hydra.ast.Expr:
    return hydra.serialization.infix_ws_list("&&", hydra.lib.lists.map((lambda x1: write_inclusive_or_expression(x1)), cae.value))

def write_conditional_expression(c: hydra.ext.java.syntax.ConditionalExpression) -> hydra.ast.Expr:
    match c:
        case hydra.ext.java.syntax.ConditionalExpressionSimple(value=co):
            return write_conditional_or_expression(co)
        
        case hydra.ext.java.syntax.ConditionalExpressionTernaryCond(value=tc):
            return write_conditional_expression_ternary_cond(tc)
        
        case hydra.ext.java.syntax.ConditionalExpressionTernaryLambda(value=tl):
            return write_conditional_expression_ternary_lambda(tl)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_conditional_or_expression(coe: hydra.ext.java.syntax.ConditionalOrExpression) -> hydra.ast.Expr:
    return hydra.serialization.infix_ws_list("||", hydra.lib.lists.map((lambda x1: write_conditional_and_expression(x1)), coe.value))

def write_constant_declaration(cd: hydra.ext.java.syntax.ConstantDeclaration) -> hydra.ast.Expr:
    @lru_cache(1)
    def mods() -> frozenlist[hydra.ext.java.syntax.ConstantModifier]:
        return cd.modifiers
    @lru_cache(1)
    def typ() -> hydra.ext.java.syntax.UnannType:
        return cd.type
    @lru_cache(1)
    def vars() -> frozenlist[hydra.ext.java.syntax.VariableDeclarator]:
        return cd.variables
    return hydra.serialization.with_semi(hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(mods()), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_constant_modifier(x1)), mods()))))), Just(write_unann_type(typ())), Just(hydra.serialization.comma_sep(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: write_variable_declarator(x1)), vars())))))))

def write_constructor_body(cb: hydra.ext.java.syntax.ConstructorBody) -> hydra.ast.Expr:
    @lru_cache(1)
    def minvoc() -> Maybe[hydra.ext.java.syntax.ExplicitConstructorInvocation]:
        return cb.invocation
    @lru_cache(1)
    def stmts() -> frozenlist[hydra.ext.java.syntax.BlockStatement]:
        return cb.statements
    return hydra.serialization.curly_block(hydra.serialization.full_block_style, hydra.serialization.double_newline_sep(hydra.lib.maybes.cat((hydra.lib.maybes.map((lambda x1: write_explicit_constructor_invocation(x1)), minvoc()), Just(hydra.serialization.newline_sep(hydra.lib.lists.map((lambda x1: write_block_statement(x1)), stmts())))))))

def write_constructor_declaration(cd: hydra.ext.java.syntax.ConstructorDeclaration) -> hydra.ast.Expr:
    @lru_cache(1)
    def mods() -> frozenlist[hydra.ext.java.syntax.ConstructorModifier]:
        return cd.modifiers
    @lru_cache(1)
    def cons() -> hydra.ext.java.syntax.ConstructorDeclarator:
        return cd.constructor
    @lru_cache(1)
    def mthrows() -> Maybe[hydra.ext.java.syntax.Throws]:
        return cd.throws
    @lru_cache(1)
    def body() -> hydra.ext.java.syntax.ConstructorBody:
        return cd.body
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(mods()), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_constructor_modifier(x1)), mods()))))), Just(write_constructor_declarator(cons())), hydra.lib.maybes.map((lambda x1: write_throws(x1)), mthrows()), Just(write_constructor_body(body())))))

def write_constructor_declarator(cd: hydra.ext.java.syntax.ConstructorDeclarator) -> hydra.ast.Expr:
    @lru_cache(1)
    def tparams() -> frozenlist[hydra.ext.java.syntax.TypeParameter]:
        return cd.parameters
    @lru_cache(1)
    def name() -> hydra.ext.java.syntax.SimpleTypeName:
        return cd.name
    @lru_cache(1)
    def fparams() -> frozenlist[hydra.ext.java.syntax.FormalParameter]:
        return cd.formal_parameters
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(tparams()), (lambda : Nothing()), (lambda : Just(hydra.serialization.angle_braces_list(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: write_type_parameter(x1)), tparams()))))), Just(write_simple_type_name(name())), Just(hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: write_formal_parameter(x1)), fparams()))))))

def write_constructor_modifier(m: hydra.ext.java.syntax.ConstructorModifier) -> hydra.ast.Expr:
    match m:
        case hydra.ext.java.syntax.ConstructorModifierAnnotation(value=ann):
            return write_annotation(ann)
        
        case hydra.ext.java.syntax.ConstructorModifierPublic():
            return hydra.serialization.cst("public")
        
        case hydra.ext.java.syntax.ConstructorModifierProtected():
            return hydra.serialization.cst("protected")
        
        case hydra.ext.java.syntax.ConstructorModifierPrivate():
            return hydra.serialization.cst("private")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_element_value(ev: hydra.ext.java.syntax.ElementValue) -> hydra.ast.Expr:
    match ev:
        case hydra.ext.java.syntax.ElementValueConditionalExpression(value=c):
            return write_conditional_expression(c)
        
        case hydra.ext.java.syntax.ElementValueElementValueArrayInitializer(value=evai):
            return hydra.serialization.comma_sep(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: write_element_value(x1)), evai.value))
        
        case hydra.ext.java.syntax.ElementValueAnnotation(value=ann):
            return write_annotation(ann)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_element_value_pair(evp: hydra.ext.java.syntax.ElementValuePair) -> hydra.ast.Expr:
    @lru_cache(1)
    def k() -> hydra.ext.java.syntax.Identifier:
        return evp.key
    @lru_cache(1)
    def v() -> hydra.ext.java.syntax.ElementValue:
        return evp.value
    return hydra.serialization.infix_ws("=", write_identifier(k()), write_element_value(v()))

def write_equality_expression(e: hydra.ext.java.syntax.EqualityExpression) -> hydra.ast.Expr:
    match e:
        case hydra.ext.java.syntax.EqualityExpressionUnary(value=r):
            return write_relational_expression(r)
        
        case hydra.ext.java.syntax.EqualityExpressionEqual(value=b):
            return hydra.serialization.infix_ws("==", write_equality_expression(b.lhs), write_relational_expression(b.rhs))
        
        case hydra.ext.java.syntax.EqualityExpressionNotEqual(value=b2):
            return hydra.serialization.infix_ws("!=", write_equality_expression(b2.lhs), write_relational_expression(b2.rhs))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_exclusive_or_expression(eoe: hydra.ext.java.syntax.ExclusiveOrExpression) -> hydra.ast.Expr:
    return hydra.serialization.infix_ws_list("^", hydra.lib.lists.map((lambda x1: write_and_expression(x1)), eoe.value))

def write_expression(e: hydra.ext.java.syntax.Expression) -> hydra.ast.Expr:
    match e:
        case hydra.ext.java.syntax.ExpressionLambda(value=l):
            return write_lambda_expression(l)
        
        case hydra.ext.java.syntax.ExpressionAssignment(value=a):
            return write_assignment_expression(a)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_expression_statement(es: hydra.ext.java.syntax.ExpressionStatement) -> hydra.ast.Expr:
    return hydra.serialization.with_semi(write_statement_expression(es.value))

def write_field_access(fa: hydra.ext.java.syntax.FieldAccess) -> hydra.ast.Expr:
    @lru_cache(1)
    def qual() -> hydra.ext.java.syntax.FieldAccess_Qualifier:
        return fa.qualifier
    @lru_cache(1)
    def id() -> hydra.ext.java.syntax.Identifier:
        return fa.identifier
    match qual():
        case hydra.ext.java.syntax.FieldAccess_QualifierPrimary(value=p):
            return hydra.serialization.dot_sep((write_primary(p), write_identifier(id())))
        
        case hydra.ext.java.syntax.FieldAccess_QualifierSuper():
            return hydra.serialization.dot_sep((hydra.serialization.cst("super"), write_identifier(id())))
        
        case hydra.ext.java.syntax.FieldAccess_QualifierTyped(value=tn):
            return hydra.serialization.dot_sep((write_type_name(tn), hydra.serialization.cst("super"), write_identifier(id())))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_field_declaration(fd: hydra.ext.java.syntax.FieldDeclaration) -> hydra.ast.Expr:
    @lru_cache(1)
    def mods() -> frozenlist[hydra.ext.java.syntax.FieldModifier]:
        return fd.modifiers
    @lru_cache(1)
    def typ() -> hydra.ext.java.syntax.UnannType:
        return fd.unann_type
    @lru_cache(1)
    def vars() -> frozenlist[hydra.ext.java.syntax.VariableDeclarator]:
        return fd.variable_declarators
    return hydra.serialization.with_semi(hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(mods()), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_field_modifier(x1)), mods()))))), Just(write_unann_type(typ())), Just(hydra.serialization.comma_sep(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: write_variable_declarator(x1)), vars())))))))

def write_field_modifier(m: hydra.ext.java.syntax.FieldModifier) -> hydra.ast.Expr:
    match m:
        case hydra.ext.java.syntax.FieldModifierAnnotation(value=ann):
            return write_annotation(ann)
        
        case hydra.ext.java.syntax.FieldModifierPublic():
            return hydra.serialization.cst("public")
        
        case hydra.ext.java.syntax.FieldModifierProtected():
            return hydra.serialization.cst("protected")
        
        case hydra.ext.java.syntax.FieldModifierPrivate():
            return hydra.serialization.cst("private")
        
        case hydra.ext.java.syntax.FieldModifierStatic():
            return hydra.serialization.cst("static")
        
        case hydra.ext.java.syntax.FieldModifierFinal():
            return hydra.serialization.cst("final")
        
        case hydra.ext.java.syntax.FieldModifierTransient():
            return hydra.serialization.cst("transient")
        
        case hydra.ext.java.syntax.FieldModifierVolatile():
            return hydra.serialization.cst("volatile")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_formal_parameter(p: hydra.ext.java.syntax.FormalParameter) -> hydra.ast.Expr:
    match p:
        case hydra.ext.java.syntax.FormalParameterSimple(value=s):
            return write_formal_parameter_simple(s)
        
        case hydra.ext.java.syntax.FormalParameterVariableArity(value=v):
            return write_variable_arity_parameter(v)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_formal_parameter_simple(fps: hydra.ext.java.syntax.FormalParameter_Simple) -> hydra.ast.Expr:
    @lru_cache(1)
    def mods() -> frozenlist[hydra.ext.java.syntax.VariableModifier]:
        return fps.modifiers
    @lru_cache(1)
    def typ() -> hydra.ext.java.syntax.UnannType:
        return fps.type
    @lru_cache(1)
    def id() -> hydra.ext.java.syntax.VariableDeclaratorId:
        return fps.id
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(mods()), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_variable_modifier(x1)), mods()))))), Just(write_unann_type(typ())), Just(write_variable_declarator_id(id())))))

def write_if_then_statement(its: hydra.ext.java.syntax.IfThenStatement) -> hydra.ast.Expr:
    @lru_cache(1)
    def cond() -> hydra.ext.java.syntax.Expression:
        return its.expression
    @lru_cache(1)
    def thn() -> hydra.ext.java.syntax.Statement:
        return its.statement
    return hydra.serialization.space_sep((hydra.serialization.cst("if"), hydra.serialization.paren_list(False, (write_expression(cond()),)), hydra.serialization.curly_block(hydra.serialization.full_block_style, write_statement(thn()))))

def write_inclusive_or_expression(ioe: hydra.ext.java.syntax.InclusiveOrExpression) -> hydra.ast.Expr:
    return hydra.serialization.infix_ws_list("|", hydra.lib.lists.map((lambda x1: write_exclusive_or_expression(x1)), ioe.value))

def write_interface_body(ib: hydra.ext.java.syntax.InterfaceBody) -> hydra.ast.Expr:
    return hydra.serialization.curly_block(hydra.serialization.full_block_style, hydra.serialization.double_newline_sep(hydra.lib.lists.map((lambda x1: write_interface_member_declaration(x1)), ib.value)))

def write_interface_declaration(d: hydra.ext.java.syntax.InterfaceDeclaration) -> hydra.ast.Expr:
    match d:
        case hydra.ext.java.syntax.InterfaceDeclarationNormalInterface(value=n):
            return write_normal_interface_declaration(n)
        
        case hydra.ext.java.syntax.InterfaceDeclarationAnnotationType(value=a):
            return write_annotation_type_declaration(a)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_interface_member_declaration(d: hydra.ext.java.syntax.InterfaceMemberDeclaration) -> hydra.ast.Expr:
    match d:
        case hydra.ext.java.syntax.InterfaceMemberDeclarationConstant(value=c):
            return write_constant_declaration(c)
        
        case hydra.ext.java.syntax.InterfaceMemberDeclarationInterfaceMethod(value=im):
            return write_interface_method_declaration(im)
        
        case hydra.ext.java.syntax.InterfaceMemberDeclarationClass(value=cd):
            return write_class_declaration(cd)
        
        case hydra.ext.java.syntax.InterfaceMemberDeclarationInterface(value=id):
            return write_interface_declaration(id)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_interface_method_declaration(imd: hydra.ext.java.syntax.InterfaceMethodDeclaration) -> hydra.ast.Expr:
    @lru_cache(1)
    def mods() -> frozenlist[hydra.ext.java.syntax.InterfaceMethodModifier]:
        return imd.modifiers
    @lru_cache(1)
    def header() -> hydra.ext.java.syntax.MethodHeader:
        return imd.header
    @lru_cache(1)
    def body() -> hydra.ext.java.syntax.MethodBody:
        return imd.body
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(mods()), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_interface_method_modifier(x1)), mods()))))), Just(write_method_header(header())), Just(write_method_body(body())))))

def write_interface_method_modifier(m: hydra.ext.java.syntax.InterfaceMethodModifier) -> hydra.ast.Expr:
    match m:
        case hydra.ext.java.syntax.InterfaceMethodModifierAnnotation(value=a):
            return write_annotation(a)
        
        case hydra.ext.java.syntax.InterfaceMethodModifierPublic():
            return hydra.serialization.cst("public")
        
        case hydra.ext.java.syntax.InterfaceMethodModifierPrivate():
            return hydra.serialization.cst("private")
        
        case hydra.ext.java.syntax.InterfaceMethodModifierAbstract():
            return hydra.serialization.cst("abstract")
        
        case hydra.ext.java.syntax.InterfaceMethodModifierDefault():
            return hydra.serialization.cst("default")
        
        case hydra.ext.java.syntax.InterfaceMethodModifierStatic():
            return hydra.serialization.cst("static")
        
        case hydra.ext.java.syntax.InterfaceMethodModifierStrictfp():
            return hydra.serialization.cst("strictfp")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_interface_modifier(m: hydra.ext.java.syntax.InterfaceModifier) -> hydra.ast.Expr:
    match m:
        case hydra.ext.java.syntax.InterfaceModifierAnnotation(value=a):
            return write_annotation(a)
        
        case hydra.ext.java.syntax.InterfaceModifierPublic():
            return hydra.serialization.cst("public")
        
        case hydra.ext.java.syntax.InterfaceModifierProtected():
            return hydra.serialization.cst("protected")
        
        case hydra.ext.java.syntax.InterfaceModifierPrivate():
            return hydra.serialization.cst("private")
        
        case hydra.ext.java.syntax.InterfaceModifierAbstract():
            return hydra.serialization.cst("abstract")
        
        case hydra.ext.java.syntax.InterfaceModifierStatic():
            return hydra.serialization.cst("static")
        
        case hydra.ext.java.syntax.InterfaceModifierStrictfb():
            return hydra.serialization.cst("strictfb")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_interface_type(it: hydra.ext.java.syntax.InterfaceType) -> hydra.ast.Expr:
    return write_class_type(it.value)

def write_lambda_body(b: hydra.ext.java.syntax.LambdaBody) -> hydra.ast.Expr:
    match b:
        case hydra.ext.java.syntax.LambdaBodyExpression(value=e):
            return write_expression(e)
        
        case hydra.ext.java.syntax.LambdaBodyBlock(value=b2):
            return write_block(b2)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_lambda_expression(le: hydra.ext.java.syntax.LambdaExpression) -> hydra.ast.Expr:
    @lru_cache(1)
    def params() -> hydra.ext.java.syntax.LambdaParameters:
        return le.parameters
    @lru_cache(1)
    def body() -> hydra.ext.java.syntax.LambdaBody:
        return le.body
    return hydra.serialization.infix_ws("->", write_lambda_parameters(params()), write_lambda_body(body()))

def write_left_hand_side(lhs2: hydra.ext.java.syntax.LeftHandSide) -> hydra.ast.Expr:
    match lhs2:
        case hydra.ext.java.syntax.LeftHandSideExpressionName(value=en):
            return write_expression_name(en)
        
        case hydra.ext.java.syntax.LeftHandSideFieldAccess(value=fa):
            return write_field_access(fa)
        
        case hydra.ext.java.syntax.LeftHandSideArrayAccess(value=aa):
            return write_array_access(aa)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_local_name(t: hydra.ext.java.syntax.LocalVariableType) -> hydra.ast.Expr:
    match t:
        case hydra.ext.java.syntax.LocalVariableTypeType(value=ut):
            return write_unann_type(ut)
        
        case hydra.ext.java.syntax.LocalVariableTypeVar():
            return hydra.serialization.cst("var")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_local_variable_declaration(lvd: hydra.ext.java.syntax.LocalVariableDeclaration) -> hydra.ast.Expr:
    @lru_cache(1)
    def mods() -> frozenlist[hydra.ext.java.syntax.VariableModifier]:
        return lvd.modifiers
    @lru_cache(1)
    def t() -> hydra.ext.java.syntax.LocalVariableType:
        return lvd.type
    @lru_cache(1)
    def decls() -> frozenlist[hydra.ext.java.syntax.VariableDeclarator]:
        return lvd.declarators
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(mods()), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_variable_modifier(x1)), mods()))))), Just(write_local_name(t())), Just(hydra.serialization.comma_sep(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: write_variable_declarator(x1)), decls()))))))

def write_local_variable_declaration_statement(lvds: hydra.ext.java.syntax.LocalVariableDeclarationStatement) -> hydra.ast.Expr:
    return hydra.serialization.with_semi(write_local_variable_declaration(lvds.value))

def write_method_body(b: hydra.ext.java.syntax.MethodBody) -> hydra.ast.Expr:
    match b:
        case hydra.ext.java.syntax.MethodBodyBlock(value=block):
            return write_block(block)
        
        case hydra.ext.java.syntax.MethodBodyNone():
            return hydra.serialization.cst(";")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_method_declaration(md: hydra.ext.java.syntax.MethodDeclaration) -> hydra.ast.Expr:
    @lru_cache(1)
    def anns() -> frozenlist[hydra.ext.java.syntax.Annotation]:
        return md.annotations
    @lru_cache(1)
    def mods() -> frozenlist[hydra.ext.java.syntax.MethodModifier]:
        return md.modifiers
    @lru_cache(1)
    def header() -> hydra.ext.java.syntax.MethodHeader:
        return md.header
    @lru_cache(1)
    def body() -> hydra.ext.java.syntax.MethodBody:
        return md.body
    @lru_cache(1)
    def header_and_body() -> hydra.ast.Expr:
        return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(mods()), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_method_modifier(x1)), mods()))))), Just(write_method_header(header())), Just(write_method_body(body())))))
    return hydra.serialization.newline_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(anns()), (lambda : Nothing()), (lambda : Just(hydra.serialization.newline_sep(hydra.lib.lists.map((lambda x1: write_annotation(x1)), anns()))))), Just(header_and_body()))))

def write_method_declarator(md: hydra.ext.java.syntax.MethodDeclarator) -> hydra.ast.Expr:
    @lru_cache(1)
    def id() -> hydra.ext.java.syntax.Identifier:
        return md.identifier
    @lru_cache(1)
    def params() -> frozenlist[hydra.ext.java.syntax.FormalParameter]:
        return md.formal_parameters
    return hydra.serialization.no_sep((write_identifier(id()), hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: write_formal_parameter(x1)), params()))))

def write_method_header(mh: hydra.ext.java.syntax.MethodHeader) -> hydra.ast.Expr:
    @lru_cache(1)
    def params() -> frozenlist[hydra.ext.java.syntax.TypeParameter]:
        return mh.parameters
    @lru_cache(1)
    def result() -> hydra.ext.java.syntax.Result:
        return mh.result
    @lru_cache(1)
    def decl() -> hydra.ext.java.syntax.MethodDeclarator:
        return mh.declarator
    @lru_cache(1)
    def mthrows() -> Maybe[hydra.ext.java.syntax.Throws]:
        return mh.throws
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(params()), (lambda : Nothing()), (lambda : Just(hydra.serialization.angle_braces_list(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: write_type_parameter(x1)), params()))))), Just(write_result(result())), Just(write_method_declarator(decl())), hydra.lib.maybes.map((lambda x1: write_throws(x1)), mthrows()))))

def write_method_invocation(mi: hydra.ext.java.syntax.MethodInvocation) -> hydra.ast.Expr:
    @lru_cache(1)
    def header() -> hydra.ext.java.syntax.MethodInvocation_Header:
        return mi.header
    @lru_cache(1)
    def args() -> frozenlist[hydra.ext.java.syntax.Expression]:
        return mi.arguments
    @lru_cache(1)
    def arg_sec() -> hydra.ast.Expr:
        return hydra.serialization.paren_list(True, hydra.lib.lists.map((lambda x1: write_expression(x1)), args()))
    @lru_cache(1)
    def header_sec() -> hydra.ast.Expr:
        match header():
            case hydra.ext.java.syntax.MethodInvocation_HeaderSimple(value=mname):
                return write_method_name(mname)
            
            case hydra.ext.java.syntax.MethodInvocation_HeaderComplex(value=cx):
                @lru_cache(1)
                def cvar() -> hydra.ext.java.syntax.MethodInvocation_Variant:
                    return cx.variant
                @lru_cache(1)
                def targs() -> frozenlist[hydra.ext.java.syntax.TypeArgument]:
                    return cx.type_arguments
                @lru_cache(1)
                def cid() -> hydra.ext.java.syntax.Identifier:
                    return cx.identifier
                @lru_cache(1)
                def id_sec() -> hydra.ast.Expr:
                    return hydra.serialization.no_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(targs()), (lambda : Nothing()), (lambda : Just(hydra.serialization.angle_braces_list(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: write_type_argument(x1)), targs()))))), Just(write_identifier(cid())))))
                match cvar():
                    case hydra.ext.java.syntax.MethodInvocation_VariantType(value=tname):
                        return hydra.serialization.dot_sep((write_type_name(tname), id_sec()))
                    
                    case hydra.ext.java.syntax.MethodInvocation_VariantExpression(value=en):
                        return hydra.serialization.dot_sep((write_expression_name(en), id_sec()))
                    
                    case hydra.ext.java.syntax.MethodInvocation_VariantPrimary(value=p):
                        return hydra.serialization.dot_sep((write_primary(p), id_sec()))
                    
                    case hydra.ext.java.syntax.MethodInvocation_VariantSuper():
                        return hydra.serialization.dot_sep((hydra.serialization.cst("super"), id_sec()))
                    
                    case hydra.ext.java.syntax.MethodInvocation_VariantTypeSuper(value=tname2):
                        return hydra.serialization.dot_sep((write_type_name(tname2), hydra.serialization.cst("super"), id_sec()))
                    
                    case _:
                        raise AssertionError("Unreachable: all variants handled")
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.serialization.no_sep((header_sec(), arg_sec()))

def write_method_modifier(m: hydra.ext.java.syntax.MethodModifier) -> hydra.ast.Expr:
    match m:
        case hydra.ext.java.syntax.MethodModifierAnnotation(value=ann):
            return write_annotation(ann)
        
        case hydra.ext.java.syntax.MethodModifierPublic():
            return hydra.serialization.cst("public")
        
        case hydra.ext.java.syntax.MethodModifierProtected():
            return hydra.serialization.cst("protected")
        
        case hydra.ext.java.syntax.MethodModifierPrivate():
            return hydra.serialization.cst("private")
        
        case hydra.ext.java.syntax.MethodModifierAbstract():
            return hydra.serialization.cst("abstract")
        
        case hydra.ext.java.syntax.MethodModifierFinal():
            return hydra.serialization.cst("final")
        
        case hydra.ext.java.syntax.MethodModifierSynchronized():
            return hydra.serialization.cst("synchronized")
        
        case hydra.ext.java.syntax.MethodModifierNative():
            return hydra.serialization.cst("native")
        
        case hydra.ext.java.syntax.MethodModifierStrictfb():
            return hydra.serialization.cst("strictfb")
        
        case _:
            raise TypeError("Unsupported MethodModifier")

def write_multiplicative_expression(e: hydra.ext.java.syntax.MultiplicativeExpression) -> hydra.ast.Expr:
    match e:
        case hydra.ext.java.syntax.MultiplicativeExpressionUnary(value=u):
            return write_unary_expression(u)
        
        case hydra.ext.java.syntax.MultiplicativeExpressionTimes(value=b):
            return hydra.serialization.infix_ws("*", write_multiplicative_expression(b.lhs), write_unary_expression(b.rhs))
        
        case hydra.ext.java.syntax.MultiplicativeExpressionDivide(value=b2):
            return hydra.serialization.infix_ws("/", write_multiplicative_expression(b2.lhs), write_unary_expression(b2.rhs))
        
        case hydra.ext.java.syntax.MultiplicativeExpressionMod(value=b3):
            return hydra.serialization.infix_ws("%", write_multiplicative_expression(b3.lhs), write_unary_expression(b3.rhs))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_normal_annotation(na: hydra.ext.java.syntax.NormalAnnotation) -> hydra.ast.Expr:
    @lru_cache(1)
    def tname() -> hydra.ext.java.syntax.TypeName:
        return na.type_name
    @lru_cache(1)
    def pairs() -> frozenlist[hydra.ext.java.syntax.ElementValuePair]:
        return na.pairs
    return hydra.serialization.prefix("@", hydra.serialization.no_sep((write_type_name(tname()), hydra.serialization.comma_sep(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: write_element_value_pair(x1)), pairs())))))

def write_normal_class_declaration(ncd: hydra.ext.java.syntax.NormalClassDeclaration) -> hydra.ast.Expr:
    @lru_cache(1)
    def mods() -> frozenlist[hydra.ext.java.syntax.ClassModifier]:
        return ncd.modifiers
    @lru_cache(1)
    def id() -> hydra.ext.java.syntax.TypeIdentifier:
        return ncd.identifier
    @lru_cache(1)
    def tparams() -> frozenlist[hydra.ext.java.syntax.TypeParameter]:
        return ncd.parameters
    @lru_cache(1)
    def msuperc() -> Maybe[hydra.ext.java.syntax.ClassType]:
        return ncd.extends
    @lru_cache(1)
    def superi() -> frozenlist[hydra.ext.java.syntax.InterfaceType]:
        return ncd.implements
    @lru_cache(1)
    def body() -> hydra.ext.java.syntax.ClassBody:
        return ncd.body
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(mods()), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_class_modifier(x1)), mods()))))), Just(hydra.serialization.cst("class")), Just(hydra.serialization.no_sep(hydra.lib.maybes.cat((Just(write_type_identifier(id())), hydra.lib.logic.if_else(hydra.lib.lists.null(tparams()), (lambda : Nothing()), (lambda : Just(hydra.serialization.angle_braces_list(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: write_type_parameter(x1)), tparams()))))))))), hydra.lib.maybes.map((lambda c: hydra.serialization.space_sep((hydra.serialization.cst("extends"), write_class_type(c)))), msuperc()), hydra.lib.logic.if_else(hydra.lib.lists.null(superi()), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep((hydra.serialization.cst("implements"), hydra.serialization.comma_sep(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: write_interface_type(x1)), superi()))))))), Just(write_class_body(body())))))

def write_normal_interface_declaration(nid: hydra.ext.java.syntax.NormalInterfaceDeclaration) -> hydra.ast.Expr:
    @lru_cache(1)
    def mods() -> frozenlist[hydra.ext.java.syntax.InterfaceModifier]:
        return nid.modifiers
    @lru_cache(1)
    def id() -> hydra.ext.java.syntax.TypeIdentifier:
        return nid.identifier
    @lru_cache(1)
    def tparams() -> frozenlist[hydra.ext.java.syntax.TypeParameter]:
        return nid.parameters
    @lru_cache(1)
    def extends() -> frozenlist[hydra.ext.java.syntax.InterfaceType]:
        return nid.extends
    @lru_cache(1)
    def body() -> hydra.ext.java.syntax.InterfaceBody:
        return nid.body
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(mods()), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_interface_modifier(x1)), mods()))))), Just(hydra.serialization.cst("interface")), Just(hydra.serialization.no_sep(hydra.lib.maybes.cat((Just(write_type_identifier(id())), hydra.lib.logic.if_else(hydra.lib.lists.null(tparams()), (lambda : Nothing()), (lambda : Just(hydra.serialization.angle_braces_list(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: write_type_parameter(x1)), tparams()))))))))), hydra.lib.logic.if_else(hydra.lib.lists.null(extends()), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep((hydra.serialization.cst("extends"), hydra.serialization.comma_sep(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: write_interface_type(x1)), extends()))))))), Just(write_interface_body(body())))))

def write_postfix_expression(e: hydra.ext.java.syntax.PostfixExpression) -> hydra.ast.Expr:
    match e:
        case hydra.ext.java.syntax.PostfixExpressionPrimary(value=p):
            return write_primary(p)
        
        case hydra.ext.java.syntax.PostfixExpressionName(value=en):
            return write_expression_name(en)
        
        case hydra.ext.java.syntax.PostfixExpressionPostIncrement(value=pi):
            return write_post_increment_expression(pi)
        
        case hydra.ext.java.syntax.PostfixExpressionPostDecrement(value=pd):
            return write_post_decrement_expression(pd)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_primary(p: hydra.ext.java.syntax.Primary) -> hydra.ast.Expr:
    match p:
        case hydra.ext.java.syntax.PrimaryNoNewArray(value=n):
            return write_primary_no_new_array(n)
        
        case hydra.ext.java.syntax.PrimaryArrayCreation(value=a):
            return write_array_creation_expression(a)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_primary_no_new_array(p: hydra.ext.java.syntax.PrimaryNoNewArray) -> hydra.ast.Expr:
    match p:
        case hydra.ext.java.syntax.PrimaryNoNewArrayLiteral(value=l):
            return write_literal(l)
        
        case hydra.ext.java.syntax.PrimaryNoNewArrayClassLiteral(value=cl):
            return write_class_literal(cl)
        
        case hydra.ext.java.syntax.PrimaryNoNewArrayThis():
            return hydra.serialization.cst("this")
        
        case hydra.ext.java.syntax.PrimaryNoNewArrayDotThis(value=n):
            return hydra.serialization.dot_sep((write_type_name(n), hydra.serialization.cst("this")))
        
        case hydra.ext.java.syntax.PrimaryNoNewArrayParens(value=e):
            return hydra.serialization.paren_list(False, (write_expression(e),))
        
        case hydra.ext.java.syntax.PrimaryNoNewArrayClassInstance(value=ci):
            return write_class_instance_creation_expression(ci)
        
        case hydra.ext.java.syntax.PrimaryNoNewArrayFieldAccess(value=fa):
            return write_field_access(fa)
        
        case hydra.ext.java.syntax.PrimaryNoNewArrayArrayAccess(value=aa):
            return write_array_access(aa)
        
        case hydra.ext.java.syntax.PrimaryNoNewArrayMethodInvocation(value=mi):
            return write_method_invocation(mi)
        
        case hydra.ext.java.syntax.PrimaryNoNewArrayMethodReference(value=mr):
            return write_method_reference(mr)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_primitive_type_with_annotations(ptwa: hydra.ext.java.syntax.PrimitiveTypeWithAnnotations) -> hydra.ast.Expr:
    @lru_cache(1)
    def pt() -> hydra.ext.java.syntax.PrimitiveType:
        return ptwa.type
    @lru_cache(1)
    def anns() -> frozenlist[hydra.ext.java.syntax.Annotation]:
        return ptwa.annotations
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(anns()), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_annotation(x1)), anns()))))), Just(write_primitive_type(pt())))))

def write_reference_type(rt: hydra.ext.java.syntax.ReferenceType) -> hydra.ast.Expr:
    match rt:
        case hydra.ext.java.syntax.ReferenceTypeClassOrInterface(value=cit):
            return write_class_or_interface_type(cit)
        
        case hydra.ext.java.syntax.ReferenceTypeVariable(value=v):
            return write_type_variable(v)
        
        case hydra.ext.java.syntax.ReferenceTypeArray(value=at):
            return write_array_type(at)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_relational_expression(e: hydra.ext.java.syntax.RelationalExpression) -> hydra.ast.Expr:
    match e:
        case hydra.ext.java.syntax.RelationalExpressionSimple(value=s):
            return write_shift_expression(s)
        
        case hydra.ext.java.syntax.RelationalExpressionLessThan(value=lt):
            return write_relational_expression_less_than(lt)
        
        case hydra.ext.java.syntax.RelationalExpressionGreaterThan(value=gt):
            return write_relational_expression_greater_than(gt)
        
        case hydra.ext.java.syntax.RelationalExpressionLessThanEqual(value=lte):
            return write_relational_expression_less_than_equal(lte)
        
        case hydra.ext.java.syntax.RelationalExpressionGreaterThanEqual(value=gte):
            return write_relational_expression_greater_than_equal(gte)
        
        case hydra.ext.java.syntax.RelationalExpressionInstanceof(value=i):
            return write_relational_expression_instance_of(i)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_relational_expression_greater_than(gt: hydra.ext.java.syntax.RelationalExpression_GreaterThan) -> hydra.ast.Expr:
    return hydra.serialization.infix_ws(">", write_relational_expression(gt.lhs), write_shift_expression(gt.rhs))

def write_relational_expression_greater_than_equal(gte: hydra.ext.java.syntax.RelationalExpression_GreaterThanEqual) -> hydra.ast.Expr:
    return hydra.serialization.infix_ws(">=", write_relational_expression(gte.lhs), write_shift_expression(gte.rhs))

def write_relational_expression_instance_of(io: hydra.ext.java.syntax.RelationalExpression_InstanceOf) -> hydra.ast.Expr:
    return hydra.serialization.infix_ws("instanceof", write_relational_expression(io.lhs), write_reference_type(io.rhs))

def write_relational_expression_less_than(lt: hydra.ext.java.syntax.RelationalExpression_LessThan) -> hydra.ast.Expr:
    return hydra.serialization.infix_ws("<", write_relational_expression(lt.lhs), write_shift_expression(lt.rhs))

def write_relational_expression_less_than_equal(lte: hydra.ext.java.syntax.RelationalExpression_LessThanEqual) -> hydra.ast.Expr:
    return hydra.serialization.infix_ws("<=", write_relational_expression(lte.lhs), write_shift_expression(lte.rhs))

def write_result(r: hydra.ext.java.syntax.Result) -> hydra.ast.Expr:
    match r:
        case hydra.ext.java.syntax.ResultType(value=t):
            return write_unann_type(t)
        
        case hydra.ext.java.syntax.ResultVoid():
            return hydra.serialization.cst("void")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_return_statement(rs: hydra.ext.java.syntax.ReturnStatement) -> hydra.ast.Expr:
    @lru_cache(1)
    def mex() -> Maybe[hydra.ext.java.syntax.Expression]:
        return rs.value
    return hydra.serialization.with_semi(hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(hydra.serialization.cst("return")), hydra.lib.maybes.map((lambda x1: write_expression(x1)), mex())))))

def write_shift_expression(e: hydra.ext.java.syntax.ShiftExpression) -> hydra.ast.Expr:
    match e:
        case hydra.ext.java.syntax.ShiftExpressionUnary(value=a):
            return write_additive_expression(a)
        
        case hydra.ext.java.syntax.ShiftExpressionShiftLeft(value=b):
            return hydra.serialization.infix_ws("<<", write_shift_expression(b.lhs), write_additive_expression(b.rhs))
        
        case hydra.ext.java.syntax.ShiftExpressionShiftRight(value=b2):
            return hydra.serialization.infix_ws(">>", write_shift_expression(b2.lhs), write_additive_expression(b2.rhs))
        
        case hydra.ext.java.syntax.ShiftExpressionShiftRightZeroFill(value=b3):
            return hydra.serialization.infix_ws(">>>", write_shift_expression(b3.lhs), write_additive_expression(b3.rhs))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_single_element_annotation(sea: hydra.ext.java.syntax.SingleElementAnnotation) -> hydra.ast.Expr:
    @lru_cache(1)
    def tname() -> hydra.ext.java.syntax.TypeName:
        return sea.name
    @lru_cache(1)
    def mv() -> Maybe[hydra.ext.java.syntax.ElementValue]:
        return sea.value
    return hydra.lib.maybes.maybe(write_marker_annotation(hydra.ext.java.syntax.MarkerAnnotation(tname())), (lambda v: hydra.serialization.prefix("@", hydra.serialization.no_sep((write_type_name(tname()), hydra.serialization.paren_list(False, (write_element_value(v),)))))), mv())

def write_statement(s: hydra.ext.java.syntax.Statement) -> hydra.ast.Expr:
    match s:
        case hydra.ext.java.syntax.StatementWithoutTrailing(value=s2):
            return write_statement_without_trailing_substatement(s2)
        
        case hydra.ext.java.syntax.StatementLabeled(value=l):
            return write_labeled_statement(l)
        
        case hydra.ext.java.syntax.StatementIfThen(value=it):
            return write_if_then_statement(it)
        
        case hydra.ext.java.syntax.StatementIfThenElse(value=ite):
            return write_if_then_else_statement(ite)
        
        case hydra.ext.java.syntax.StatementWhile(value=w):
            return write_while_statement(w)
        
        case hydra.ext.java.syntax.StatementFor(value=f):
            return write_for_statement(f)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_statement_expression(e: hydra.ext.java.syntax.StatementExpression) -> hydra.ast.Expr:
    match e:
        case hydra.ext.java.syntax.StatementExpressionAssignment(value=a):
            return write_assignment(a)
        
        case hydra.ext.java.syntax.StatementExpressionPreIncrement(value=pi):
            return write_pre_increment_expression(pi)
        
        case hydra.ext.java.syntax.StatementExpressionPreDecrement(value=pd):
            return write_pre_decrement_expression(pd)
        
        case hydra.ext.java.syntax.StatementExpressionPostIncrement(value=pi2):
            return write_post_increment_expression(pi2)
        
        case hydra.ext.java.syntax.StatementExpressionPostDecrement(value=pd2):
            return write_post_decrement_expression(pd2)
        
        case hydra.ext.java.syntax.StatementExpressionMethodInvocation(value=m):
            return write_method_invocation(m)
        
        case hydra.ext.java.syntax.StatementExpressionClassInstanceCreation(value=cic):
            return write_class_instance_creation_expression(cic)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_statement_without_trailing_substatement(s: hydra.ext.java.syntax.StatementWithoutTrailingSubstatement) -> hydra.ast.Expr:
    match s:
        case hydra.ext.java.syntax.StatementWithoutTrailingSubstatementBlock(value=b):
            return write_block(b)
        
        case hydra.ext.java.syntax.StatementWithoutTrailingSubstatementEmpty():
            return hydra.serialization.cst(";")
        
        case hydra.ext.java.syntax.StatementWithoutTrailingSubstatementExpression(value=e):
            return write_expression_statement(e)
        
        case hydra.ext.java.syntax.StatementWithoutTrailingSubstatementAssert(value=a):
            return write_assert_statement(a)
        
        case hydra.ext.java.syntax.StatementWithoutTrailingSubstatementSwitch(value=s2):
            return write_switch_statement(s2)
        
        case hydra.ext.java.syntax.StatementWithoutTrailingSubstatementDo(value=d):
            return write_do_statement(d)
        
        case hydra.ext.java.syntax.StatementWithoutTrailingSubstatementBreak(value=b2):
            return write_break_statement(b2)
        
        case hydra.ext.java.syntax.StatementWithoutTrailingSubstatementContinue(value=c):
            return write_continue_statement(c)
        
        case hydra.ext.java.syntax.StatementWithoutTrailingSubstatementReturn(value=r):
            return write_return_statement(r)
        
        case hydra.ext.java.syntax.StatementWithoutTrailingSubstatementSynchronized(value=s22):
            return write_synchronized_statement(s22)
        
        case hydra.ext.java.syntax.StatementWithoutTrailingSubstatementThrow(value=t):
            return write_throw_statement(t)
        
        case hydra.ext.java.syntax.StatementWithoutTrailingSubstatementTry(value=t2):
            return write_try_statement(t2)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_throw_statement(ts: hydra.ext.java.syntax.ThrowStatement) -> hydra.ast.Expr:
    return hydra.serialization.with_semi(hydra.serialization.space_sep((hydra.serialization.cst("throw"), write_expression(ts.value))))

def write_type(t: hydra.ext.java.syntax.Type) -> hydra.ast.Expr:
    match t:
        case hydra.ext.java.syntax.TypePrimitive(value=pt):
            return write_primitive_type_with_annotations(pt)
        
        case hydra.ext.java.syntax.TypeReference(value=rt):
            return write_reference_type(rt)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_type_argument(a: hydra.ext.java.syntax.TypeArgument) -> hydra.ast.Expr:
    match a:
        case hydra.ext.java.syntax.TypeArgumentReference(value=rt):
            return write_reference_type(rt)
        
        case hydra.ext.java.syntax.TypeArgumentWildcard(value=w):
            return write_wildcard(w)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_type_arguments_or_diamond(targs: hydra.ext.java.syntax.TypeArgumentsOrDiamond) -> hydra.ast.Expr:
    match targs:
        case hydra.ext.java.syntax.TypeArgumentsOrDiamondArguments(value=args):
            return hydra.serialization.angle_braces_list(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: write_type_argument(x1)), args))
        
        case hydra.ext.java.syntax.TypeArgumentsOrDiamondDiamond():
            return hydra.serialization.cst("<>")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_type_bound(b: hydra.ext.java.syntax.TypeBound) -> hydra.ast.Expr:
    match b:
        case hydra.ext.java.syntax.TypeBoundVariable(value=tv):
            return write_type_variable(tv)
        
        case hydra.ext.java.syntax.TypeBoundClassOrInterface(value=ci):
            @lru_cache(1)
            def cit() -> hydra.ext.java.syntax.ClassOrInterfaceType:
                return ci.type
            @lru_cache(1)
            def additional() -> frozenlist[hydra.ext.java.syntax.AdditionalBound]:
                return ci.additional
            return hydra.lib.logic.if_else(hydra.lib.lists.null(additional()), (lambda : write_class_or_interface_type(cit())), (lambda : hydra.serialization.space_sep(hydra.lib.lists.cons(write_class_or_interface_type(cit()), hydra.lib.lists.map((lambda x1: write_additional_bound(x1)), additional())))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_type_parameter(tp: hydra.ext.java.syntax.TypeParameter) -> hydra.ast.Expr:
    @lru_cache(1)
    def mods() -> frozenlist[hydra.ext.java.syntax.TypeParameterModifier]:
        return tp.modifiers
    @lru_cache(1)
    def id() -> hydra.ext.java.syntax.TypeIdentifier:
        return tp.identifier
    @lru_cache(1)
    def bound() -> Maybe[hydra.ext.java.syntax.TypeBound]:
        return tp.bound
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(mods()), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_type_parameter_modifier(x1)), mods()))))), Just(write_type_identifier(id())), hydra.lib.maybes.map((lambda b: hydra.serialization.space_sep((hydra.serialization.cst("extends"), write_type_bound(b)))), bound()))))

def write_type_parameter_modifier(tpm: hydra.ext.java.syntax.TypeParameterModifier) -> hydra.ast.Expr:
    return write_annotation(tpm.value)

def write_type_variable(tv: hydra.ext.java.syntax.TypeVariable) -> hydra.ast.Expr:
    @lru_cache(1)
    def anns() -> frozenlist[hydra.ext.java.syntax.Annotation]:
        return tv.annotations
    @lru_cache(1)
    def id() -> hydra.ext.java.syntax.TypeIdentifier:
        return tv.identifier
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(anns()), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_annotation(x1)), anns()))))), Just(write_type_identifier(id())))))

def write_unann_type(ut: hydra.ext.java.syntax.UnannType) -> hydra.ast.Expr:
    return write_type(ut.value)

def write_unary_expression(e: hydra.ext.java.syntax.UnaryExpression) -> hydra.ast.Expr:
    match e:
        case hydra.ext.java.syntax.UnaryExpressionPreIncrement(value=pi):
            return write_pre_increment_expression(pi)
        
        case hydra.ext.java.syntax.UnaryExpressionPreDecrement(value=pd):
            return write_pre_decrement_expression(pd)
        
        case hydra.ext.java.syntax.UnaryExpressionPlus(value=p):
            return hydra.serialization.space_sep((hydra.serialization.cst("+"), write_unary_expression(p)))
        
        case hydra.ext.java.syntax.UnaryExpressionMinus(value=m):
            return hydra.serialization.space_sep((hydra.serialization.cst("-"), write_unary_expression(m)))
        
        case hydra.ext.java.syntax.UnaryExpressionOther(value=o):
            return write_unary_expression_not_plus_minus(o)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_unary_expression_not_plus_minus(e: hydra.ext.java.syntax.UnaryExpressionNotPlusMinus) -> hydra.ast.Expr:
    match e:
        case hydra.ext.java.syntax.UnaryExpressionNotPlusMinusPostfix(value=p):
            return write_postfix_expression(p)
        
        case hydra.ext.java.syntax.UnaryExpressionNotPlusMinusTilde(value=u):
            return hydra.serialization.space_sep((hydra.serialization.cst("~"), write_unary_expression(u)))
        
        case hydra.ext.java.syntax.UnaryExpressionNotPlusMinusNot(value=u2):
            return hydra.serialization.no_sep((hydra.serialization.cst("!"), write_unary_expression(u2)))
        
        case hydra.ext.java.syntax.UnaryExpressionNotPlusMinusCast(value=c):
            return write_cast_expression(c)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_unqualified_class_instance_creation_expression(ucice: hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression) -> hydra.ast.Expr:
    @lru_cache(1)
    def targs() -> frozenlist[hydra.ext.java.syntax.TypeArgument]:
        return ucice.type_arguments
    @lru_cache(1)
    def cit() -> hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate:
        return ucice.class_or_interface
    @lru_cache(1)
    def args() -> frozenlist[hydra.ext.java.syntax.Expression]:
        return ucice.arguments
    @lru_cache(1)
    def mbody() -> Maybe[hydra.ext.java.syntax.ClassBody]:
        return ucice.body
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(hydra.serialization.cst("new")), hydra.lib.logic.if_else(hydra.lib.lists.null(targs()), (lambda : Nothing()), (lambda : Just(hydra.serialization.angle_braces_list(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: write_type_argument(x1)), targs()))))), Just(hydra.serialization.no_sep((write_class_or_interface_type_to_instantiate(cit()), hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: write_expression(x1)), args()))))), hydra.lib.maybes.map((lambda x1: write_class_body(x1)), mbody()))))

def write_variable_declarator(vd: hydra.ext.java.syntax.VariableDeclarator) -> hydra.ast.Expr:
    @lru_cache(1)
    def id() -> hydra.ext.java.syntax.VariableDeclaratorId:
        return vd.id
    @lru_cache(1)
    def minit() -> Maybe[hydra.ext.java.syntax.VariableInitializer]:
        return vd.initializer
    @lru_cache(1)
    def id_sec() -> hydra.ast.Expr:
        return write_variable_declarator_id(id())
    return hydra.lib.maybes.maybe(id_sec(), (lambda init: hydra.serialization.infix_ws("=", id_sec(), write_variable_initializer(init))), minit())

def write_variable_initializer(i: hydra.ext.java.syntax.VariableInitializer) -> hydra.ast.Expr:
    match i:
        case hydra.ext.java.syntax.VariableInitializerExpression(value=e):
            return write_expression(e)
        
        case hydra.ext.java.syntax.VariableInitializerArrayInitializer(value=ai):
            return write_array_initializer(ai)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_variable_modifier(m: hydra.ext.java.syntax.VariableModifier) -> hydra.ast.Expr:
    match m:
        case hydra.ext.java.syntax.VariableModifierAnnotation(value=ann):
            return write_annotation(ann)
        
        case hydra.ext.java.syntax.VariableModifierFinal():
            return hydra.serialization.cst("final")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_wildcard(w: hydra.ext.java.syntax.Wildcard) -> hydra.ast.Expr:
    @lru_cache(1)
    def anns() -> frozenlist[hydra.ext.java.syntax.Annotation]:
        return w.annotations
    @lru_cache(1)
    def mbounds() -> Maybe[hydra.ext.java.syntax.WildcardBounds]:
        return w.wildcard
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(anns()), (lambda : Nothing()), (lambda : Just(hydra.serialization.comma_sep(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: write_annotation(x1)), anns()))))), Just(hydra.serialization.cst("*")), hydra.lib.maybes.map((lambda x1: write_wildcard_bounds(x1)), mbounds()))))

def write_wildcard_bounds(b: hydra.ext.java.syntax.WildcardBounds) -> hydra.ast.Expr:
    match b:
        case hydra.ext.java.syntax.WildcardBoundsExtends(value=rt):
            return hydra.serialization.space_sep((hydra.serialization.cst("extends"), write_reference_type(rt)))
        
        case hydra.ext.java.syntax.WildcardBoundsSuper(value=rt2):
            return hydra.serialization.space_sep((hydra.serialization.cst("super"), write_reference_type(rt2)))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_import_declaration(imp: hydra.ext.java.syntax.ImportDeclaration) -> hydra.ast.Expr:
    match imp:
        case hydra.ext.java.syntax.ImportDeclarationSingleType(value=st):
            return hydra.serialization.with_semi(hydra.serialization.space_sep((hydra.serialization.cst("import"), write_type_name(st.value))))
        
        case hydra.ext.java.syntax.ImportDeclarationTypeImportOnDemand():
            return hydra.serialization.cst("STUB:ImportDeclarationTypeImportOnDemand")
        
        case hydra.ext.java.syntax.ImportDeclarationSingleStaticImport():
            return hydra.serialization.cst("STUB:ImportDeclarationSingleStaticImport")
        
        case hydra.ext.java.syntax.ImportDeclarationStaticImportOnDemand():
            return hydra.serialization.cst("STUB:ImportDeclarationStaticImportOnDemand")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_package_modifier(pm: hydra.ext.java.syntax.PackageModifier) -> hydra.ast.Expr:
    return write_annotation(pm.value)

def write_package_declaration(pd: hydra.ext.java.syntax.PackageDeclaration) -> hydra.ast.Expr:
    @lru_cache(1)
    def mods() -> frozenlist[hydra.ext.java.syntax.PackageModifier]:
        return pd.modifiers
    @lru_cache(1)
    def ids() -> frozenlist[hydra.ext.java.syntax.Identifier]:
        return pd.identifiers
    return hydra.serialization.with_semi(hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(mods()), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_package_modifier(x1)), mods()))))), Just(hydra.serialization.space_sep((hydra.serialization.cst("package"), hydra.serialization.cst(hydra.lib.strings.intercalate(".", hydra.lib.lists.map((lambda id: id.value), ids()))))))))))

def write_type_declaration(d: hydra.ext.java.syntax.TypeDeclaration) -> hydra.ast.Expr:
    match d:
        case hydra.ext.java.syntax.TypeDeclarationClass(value=d2):
            return write_class_declaration(d2)
        
        case hydra.ext.java.syntax.TypeDeclarationInterface(value=d22):
            return write_interface_declaration(d22)
        
        case hydra.ext.java.syntax.TypeDeclarationNone():
            return hydra.serialization.cst(";")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_type_declaration_with_comments(tdwc: hydra.ext.java.syntax.TypeDeclarationWithComments) -> hydra.ast.Expr:
    @lru_cache(1)
    def d() -> hydra.ext.java.syntax.TypeDeclaration:
        return tdwc.value
    @lru_cache(1)
    def mc() -> Maybe[str]:
        return tdwc.comments
    return with_comments(mc(), write_type_declaration(d()))

def write_compilation_unit(u: hydra.ext.java.syntax.CompilationUnit) -> hydra.ast.Expr:
    match u:
        case hydra.ext.java.syntax.CompilationUnitOrdinary(value=ocu):
            @lru_cache(1)
            def mpkg() -> Maybe[hydra.ext.java.syntax.PackageDeclaration]:
                return ocu.package
            @lru_cache(1)
            def imports() -> frozenlist[hydra.ext.java.syntax.ImportDeclaration]:
                return ocu.imports
            @lru_cache(1)
            def types() -> frozenlist[hydra.ext.java.syntax.TypeDeclarationWithComments]:
                return ocu.types
            @lru_cache(1)
            def warning() -> Maybe[hydra.ast.Expr]:
                return Just(single_line_comment(hydra.constants.warning_auto_generated_file))
            @lru_cache(1)
            def pkg_sec() -> Maybe[hydra.ast.Expr]:
                return hydra.lib.maybes.map((lambda x1: write_package_declaration(x1)), mpkg())
            @lru_cache(1)
            def imports_sec() -> Maybe[hydra.ast.Expr]:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(imports()), (lambda : Nothing()), (lambda : Just(hydra.serialization.newline_sep(hydra.lib.lists.map((lambda x1: write_import_declaration(x1)), imports())))))
            @lru_cache(1)
            def types_sec() -> Maybe[hydra.ast.Expr]:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(types()), (lambda : Nothing()), (lambda : Just(hydra.serialization.double_newline_sep(hydra.lib.lists.map((lambda x1: write_type_declaration_with_comments(x1)), types())))))
            return hydra.serialization.double_newline_sep(hydra.lib.maybes.cat((warning(), pkg_sec(), imports_sec(), types_sec())))
        
        case _:
            raise TypeError("Unsupported CompilationUnit")

def write_receiver_parameter(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:ReceiverParameter")
