# Note: this is an automatically generated file. Do not edit.

r"""Java serializer: converts Java AST to concrete syntax."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.constants
import hydra.core
import hydra.java.syntax
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

def java_float_literal_text(s: str) -> str:
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(s, "NaN"), (lambda : "Double.NaN"), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(s, "Infinity"), (lambda : "Double.POSITIVE_INFINITY"), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(s, "-Infinity"), (lambda : "Double.NEGATIVE_INFINITY"), (lambda : s))))))

def sanitize_java_comment(s: str) -> str:
    r"""Sanitize a string for use in a Java comment."""

    return hydra.lib.strings.intercalate("&gt;", hydra.lib.strings.split_on(">", hydra.lib.strings.intercalate("&lt;", hydra.lib.strings.split_on("<", s))))

def single_line_comment(c: str) -> hydra.ast.Expr:
    r"""Create a single-line Java comment."""

    return hydra.serialization.cst(hydra.lib.strings.cat2("// ", sanitize_java_comment(c)))

def with_comments(mc: Maybe[str], expr: hydra.ast.Expr) -> hydra.ast.Expr:
    r"""Wrap an expression with optional Javadoc comments."""

    return hydra.lib.maybes.maybe((lambda : expr), (lambda c: hydra.serialization.newline_sep((hydra.serialization.cst(hydra.lib.strings.cat2("/**\n", hydra.lib.strings.cat2(hydra.lib.strings.intercalate("\n", hydra.lib.lists.map((lambda l: hydra.lib.strings.cat2(" * ", l)), hydra.lib.strings.lines(sanitize_java_comment(c)))), "\n */"))), expr))), mc)

def write_identifier(id: hydra.java.syntax.Identifier) -> hydra.ast.Expr:
    return hydra.serialization.cst(id.value)

def write_package_or_type_name(potn: hydra.java.syntax.PackageOrTypeName) -> hydra.ast.Expr:
    return hydra.serialization.dot_sep(hydra.lib.lists.map((lambda x1: write_identifier(x1)), potn.value))

def write_type_identifier(tid: hydra.java.syntax.TypeIdentifier) -> hydra.ast.Expr:
    return write_identifier(tid.value)

def write_type_name(tn: hydra.java.syntax.TypeName) -> hydra.ast.Expr:
    id = tn.identifier
    mqual = tn.qualifier
    return hydra.serialization.dot_sep(hydra.lib.maybes.cat((hydra.lib.maybes.map((lambda x1: write_package_or_type_name(x1)), mqual), Just(write_type_identifier(id)))))

def write_marker_annotation(ma: hydra.java.syntax.MarkerAnnotation) -> hydra.ast.Expr:
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

def write_dims(d: hydra.java.syntax.Dims) -> hydra.ast.Expr:
    return hydra.serialization.no_sep(hydra.lib.lists.map((lambda _: hydra.serialization.cst("[]")), d.value))

def write_floating_point_type(ft: hydra.java.syntax.FloatingPointType) -> hydra.ast.Expr:
    match ft:
        case hydra.java.syntax.FloatingPointType.FLOAT:
            return hydra.serialization.cst("float")

        case hydra.java.syntax.FloatingPointType.DOUBLE:
            return hydra.serialization.cst("double")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_integral_type(t: hydra.java.syntax.IntegralType) -> hydra.ast.Expr:
    match t:
        case hydra.java.syntax.IntegralType.BYTE:
            return hydra.serialization.cst("byte")

        case hydra.java.syntax.IntegralType.SHORT:
            return hydra.serialization.cst("short")

        case hydra.java.syntax.IntegralType.INT:
            return hydra.serialization.cst("int")

        case hydra.java.syntax.IntegralType.LONG:
            return hydra.serialization.cst("long")

        case hydra.java.syntax.IntegralType.CHAR:
            return hydra.serialization.cst("char")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_numeric_type(nt: hydra.java.syntax.NumericType) -> hydra.ast.Expr:
    match nt:
        case hydra.java.syntax.NumericTypeIntegral(value=it):
            return write_integral_type(it)

        case hydra.java.syntax.NumericTypeFloatingPoint(value=ft):
            return write_floating_point_type(ft)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_primitive_type(pt: hydra.java.syntax.PrimitiveType) -> hydra.ast.Expr:
    match pt:
        case hydra.java.syntax.PrimitiveTypeNumeric(value=nt):
            return write_numeric_type(nt)

        case hydra.java.syntax.PrimitiveTypeBoolean():
            return hydra.serialization.cst("boolean")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_ambiguous_name(an: hydra.java.syntax.AmbiguousName) -> hydra.ast.Expr:
    return hydra.serialization.dot_sep(hydra.lib.lists.map((lambda x1: write_identifier(x1)), an.value))

def write_expression_name(en: hydra.java.syntax.ExpressionName) -> hydra.ast.Expr:
    mqual = en.qualifier
    id = en.identifier
    return hydra.serialization.dot_sep(hydra.lib.maybes.cat((hydra.lib.maybes.map((lambda x1: write_ambiguous_name(x1)), mqual), Just(write_identifier(id)))))

def write_post_decrement_expression(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:PostDecrementExpression")

def write_post_increment_expression(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:PostIncrementExpression")

def write_array_access(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:ArrayAccess")

def write_enum_declaration(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:EnumDeclaration")

def write_variable_declarator_id(vdi: hydra.java.syntax.VariableDeclaratorId) -> hydra.ast.Expr:
    id = vdi.identifier
    mdims = vdi.dims
    return hydra.serialization.no_sep(hydra.lib.maybes.cat((Just(write_identifier(id)), hydra.lib.maybes.map((lambda x1: write_dims(x1)), mdims))))

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

def write_simple_type_name(stn: hydra.java.syntax.SimpleTypeName) -> hydra.ast.Expr:
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

def write_break_statement(bs: hydra.java.syntax.BreakStatement) -> hydra.ast.Expr:
    mlabel = bs.value
    return hydra.serialization.with_semi(hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(hydra.serialization.cst("break")), hydra.lib.maybes.map((lambda x1: write_identifier(x1)), mlabel)))))

def write_continue_statement(cs: hydra.java.syntax.ContinueStatement) -> hydra.ast.Expr:
    mlabel = cs.value
    return hydra.serialization.with_semi(hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(hydra.serialization.cst("continue")), hydra.lib.maybes.map((lambda x1: write_identifier(x1)), mlabel)))))

def write_do_statement(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:DoStatement")

def write_annotated_identifier(ai: hydra.java.syntax.AnnotatedIdentifier) -> hydra.ast.Expr:
    return write_identifier(ai.identifier)

def write_method_name(mn: hydra.java.syntax.MethodName) -> hydra.ast.Expr:
    return write_identifier(mn.value)

def write_switch_statement(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:SwitchStatement")

def write_synchronized_statement(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:SynchronizedStatement")

def write_try_statement(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:TryStatement")

def write_lambda_parameters(p: hydra.java.syntax.LambdaParameters) -> hydra.ast.Expr:
    match p:
        case hydra.java.syntax.LambdaParametersTuple(value=l):
            return hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: write_lambda_parameters(x1)), l))

        case hydra.java.syntax.LambdaParametersSingle(value=id):
            return write_identifier(id)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_class_literal(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:ClassLiteral")

def write_floating_point_literal(fl: hydra.java.syntax.FloatingPointLiteral) -> hydra.ast.Expr:
    return hydra.serialization.cst(java_float_literal_text(hydra.lib.literals.show_bigfloat(fl.value)))

def write_integer_literal(il: hydra.java.syntax.IntegerLiteral) -> hydra.ast.Expr:
    i = il.value
    @lru_cache(1)
    def suffix() -> str:
        return hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.lib.equality.gt(i, 2147483647), hydra.lib.equality.lt(i, -2147483648)), (lambda : "L"), (lambda : ""))
    return hydra.serialization.cst(hydra.lib.strings.cat2(hydra.lib.literals.show_bigint(i), suffix()))

def write_string_literal(sl: hydra.java.syntax.StringLiteral) -> hydra.ast.Expr:
    r"""Serialize a Java string literal with proper Unicode escaping."""

    s = sl.value
    return hydra.serialization.cst(hydra.lib.strings.cat2("\"", hydra.lib.strings.cat2(escape_java_string(s), "\"")))

def write_literal(l: hydra.java.syntax.Literal) -> hydra.ast.Expr:
    match l:
        case hydra.java.syntax.LiteralNull():
            return hydra.serialization.cst("null")

        case hydra.java.syntax.LiteralInteger(value=il):
            return write_integer_literal(il)

        case hydra.java.syntax.LiteralFloatingPoint(value=fl):
            return write_floating_point_literal(fl)

        case hydra.java.syntax.LiteralBoolean(value=b):
            return hydra.serialization.cst(hydra.lib.logic.if_else(b, (lambda : "true"), (lambda : "false")))

        case hydra.java.syntax.LiteralCharacter(value=c):
            @lru_cache(1)
            def ci() -> int:
                return hydra.lib.literals.bigint_to_int32(hydra.lib.literals.uint16_to_bigint(c))
            return hydra.serialization.cst(hydra.lib.strings.cat2("'", hydra.lib.strings.cat2(hydra.lib.logic.if_else(hydra.lib.equality.equal(ci(), 39), (lambda : "\\'"), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(ci(), 92), (lambda : "\\\\"), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(ci(), 10), (lambda : "\\n"), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(ci(), 13), (lambda : "\\r"), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(ci(), 9), (lambda : "\\t"), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.gte(ci(), 32), hydra.lib.equality.lt(ci(), 127)), (lambda : hydra.lib.strings.from_list((ci(),))), (lambda : java_unicode_escape(ci()))))))))))))), "'")))

        case hydra.java.syntax.LiteralString(value=sl):
            return write_string_literal(sl)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_method_reference(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:MethodReference")

def write_package_name(pn: hydra.java.syntax.PackageName) -> hydra.ast.Expr:
    return hydra.serialization.dot_sep(hydra.lib.lists.map((lambda x1: write_identifier(x1)), pn.value))

def write_additional_bound(ab: hydra.java.syntax.AdditionalBound) -> hydra.ast.Expr:
    return hydra.serialization.space_sep((hydra.serialization.cst("&"), write_interface_type(ab.value)))

def write_additive_expression(e: hydra.java.syntax.AdditiveExpression) -> hydra.ast.Expr:
    match e:
        case hydra.java.syntax.AdditiveExpressionUnary(value=m):
            return write_multiplicative_expression(m)

        case hydra.java.syntax.AdditiveExpressionPlus(value=b):
            return hydra.serialization.infix_ws("+", write_additive_expression(b.lhs), write_multiplicative_expression(b.rhs))

        case hydra.java.syntax.AdditiveExpressionMinus(value=b2):
            return hydra.serialization.infix_ws("-", write_additive_expression(b2.lhs), write_multiplicative_expression(b2.rhs))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_and_expression(ae: hydra.java.syntax.AndExpression) -> hydra.ast.Expr:
    return hydra.serialization.infix_ws_list("&", hydra.lib.lists.map((lambda x1: write_equality_expression(x1)), ae.value))

def write_annotation(ann: hydra.java.syntax.Annotation) -> hydra.ast.Expr:
    match ann:
        case hydra.java.syntax.AnnotationNormal(value=n):
            return write_normal_annotation(n)

        case hydra.java.syntax.AnnotationMarker(value=m):
            return write_marker_annotation(m)

        case hydra.java.syntax.AnnotationSingleElement(value=s):
            return write_single_element_annotation(s)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_array_creation_expression(ace: hydra.java.syntax.ArrayCreationExpression) -> hydra.ast.Expr:
    match ace:
        case hydra.java.syntax.ArrayCreationExpressionPrimitiveArray(value=pa):
            pt = pa.type
            ai = pa.array
            return hydra.serialization.space_sep((hydra.serialization.cst("new"), hydra.serialization.no_sep((write_primitive_type_with_annotations(pt), hydra.serialization.cst("[]"))), write_array_initializer(ai)))

        case hydra.java.syntax.ArrayCreationExpressionClassOrInterfaceArray():
            return hydra.serialization.cst("STUB:ArrayCreationExpression")

        case hydra.java.syntax.ArrayCreationExpressionPrimitive():
            return hydra.serialization.cst("STUB:ArrayCreationExpression")

        case hydra.java.syntax.ArrayCreationExpressionClassOrInterface():
            return hydra.serialization.cst("STUB:ArrayCreationExpression")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_array_initializer(ai: hydra.java.syntax.ArrayInitializer) -> hydra.ast.Expr:
    groups = ai.value
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(groups), 1), (lambda : hydra.serialization.no_sep((hydra.serialization.cst("{"), hydra.serialization.comma_sep(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_variable_initializer(x1)), hydra.lib.lists.head(groups))), hydra.serialization.cst("}")))), (lambda : hydra.serialization.cst("{}")))

def write_array_type(at: hydra.java.syntax.ArrayType) -> hydra.ast.Expr:
    dims = at.dims
    variant = at.variant
    @lru_cache(1)
    def var_expr() -> hydra.ast.Expr:
        match variant:
            case hydra.java.syntax.ArrayType_VariantPrimitive(value=pt):
                return write_primitive_type_with_annotations(pt)

            case hydra.java.syntax.ArrayType_VariantClassOrInterface(value=cit):
                return write_class_or_interface_type(cit)

            case hydra.java.syntax.ArrayType_VariantVariable(value=tv):
                return write_type_variable(tv)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.serialization.no_sep((var_expr(), write_dims(dims)))

def write_assignment(a: hydra.java.syntax.Assignment) -> hydra.ast.Expr:
    lhs = a.lhs
    op = a.op
    rhs = a.expression
    ctop = (lambda _: "=")(op) if op else (lambda _: "*=")(op) if op else (lambda _: "/=")(op) if op else (lambda _: "%=")(op) if op else (lambda _: "+=")(op) if op else (lambda _: "-=")(op) if op else (lambda _: "<<=")(op) if op else (lambda _: ">>=")(op) if op else (lambda _: ">>>=")(op) if op else (lambda _: "&=")(op) if op else (lambda _: "^=")(op) if op else (lambda _: "|=")(op) if op else hydra.dsl.python.unsupported("no matching case in inline union elimination")
    return hydra.serialization.infix_ws(ctop, write_left_hand_side(lhs), write_expression(rhs))

def write_assignment_expression(e: hydra.java.syntax.AssignmentExpression) -> hydra.ast.Expr:
    match e:
        case hydra.java.syntax.AssignmentExpressionConditional(value=c):
            return write_conditional_expression(c)

        case hydra.java.syntax.AssignmentExpressionAssignment(value=a):
            return write_assignment(a)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_block(b: hydra.java.syntax.Block) -> hydra.ast.Expr:
    return hydra.serialization.curly_block(hydra.serialization.full_block_style, hydra.serialization.newline_sep(hydra.lib.lists.map((lambda x1: write_block_statement(x1)), b.value)))

def write_block_statement(s: hydra.java.syntax.BlockStatement) -> hydra.ast.Expr:
    match s:
        case hydra.java.syntax.BlockStatementLocalVariableDeclaration(value=d):
            return write_local_variable_declaration_statement(d)

        case hydra.java.syntax.BlockStatementClass(value=cd):
            return write_class_declaration(cd)

        case hydra.java.syntax.BlockStatementStatement(value=s2):
            return write_statement(s2)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_cast_expression(e: hydra.java.syntax.CastExpression) -> hydra.ast.Expr:
    match e:
        case hydra.java.syntax.CastExpressionPrimitive(value=p):
            return write_cast_expression_primitive(p)

        case hydra.java.syntax.CastExpressionNotPlusMinus(value=npm):
            return write_cast_expression_not_plus_minus(npm)

        case hydra.java.syntax.CastExpressionLambda(value=l):
            return write_cast_expression_lambda(l)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_cast_expression_not_plus_minus(npm: hydra.java.syntax.CastExpression_NotPlusMinus) -> hydra.ast.Expr:
    rb = npm.ref_and_bounds
    ex = npm.expression
    return hydra.serialization.space_sep((write_cast_expression_ref_and_bounds(rb), write_unary_expression(ex)))

def write_cast_expression_primitive(cp: hydra.java.syntax.CastExpression_Primitive) -> hydra.ast.Expr:
    pt = cp.type
    ex = cp.expression
    return hydra.serialization.space_sep((hydra.serialization.paren_list(False, (write_primitive_type_with_annotations(pt),)), write_unary_expression(ex)))

def write_cast_expression_ref_and_bounds(rab: hydra.java.syntax.CastExpression_RefAndBounds) -> hydra.ast.Expr:
    rt = rab.type
    adds = rab.bounds
    return hydra.serialization.paren_list(False, (hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(write_reference_type(rt)), hydra.lib.logic.if_else(hydra.lib.lists.null(adds), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_additional_bound(x1)), adds)))))))),))

def write_class_body(cb: hydra.java.syntax.ClassBody) -> hydra.ast.Expr:
    return hydra.serialization.curly_block(hydra.serialization.full_block_style, hydra.serialization.double_newline_sep(hydra.lib.lists.map((lambda x1: write_class_body_declaration_with_comments(x1)), cb.value)))

def write_class_body_declaration(d: hydra.java.syntax.ClassBodyDeclaration) -> hydra.ast.Expr:
    match d:
        case hydra.java.syntax.ClassBodyDeclarationClassMember(value=d2):
            return write_class_member_declaration(d2)

        case hydra.java.syntax.ClassBodyDeclarationInstanceInitializer(value=i):
            return write_instance_initializer(i)

        case hydra.java.syntax.ClassBodyDeclarationStaticInitializer(value=i2):
            return write_static_initializer(i2)

        case hydra.java.syntax.ClassBodyDeclarationConstructorDeclaration(value=d22):
            return write_constructor_declaration(d22)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_class_body_declaration_with_comments(cbdwc: hydra.java.syntax.ClassBodyDeclarationWithComments) -> hydra.ast.Expr:
    d = cbdwc.value
    mc = cbdwc.comments
    return with_comments(mc, write_class_body_declaration(d))

def write_class_declaration(d: hydra.java.syntax.ClassDeclaration) -> hydra.ast.Expr:
    match d:
        case hydra.java.syntax.ClassDeclarationNormal(value=nd):
            return write_normal_class_declaration(nd)

        case hydra.java.syntax.ClassDeclarationEnum(value=ed):
            return write_enum_declaration(ed)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_class_instance_creation_expression(cice: hydra.java.syntax.ClassInstanceCreationExpression) -> hydra.ast.Expr:
    mqual = cice.qualifier
    e = cice.expression
    return hydra.lib.maybes.maybe((lambda : write_unqualified_class_instance_creation_expression(e)), (lambda q: hydra.serialization.dot_sep((write_class_instance_creation_expression_qualifier(q), write_unqualified_class_instance_creation_expression(e)))), mqual)

def write_class_instance_creation_expression_qualifier(q: hydra.java.syntax.ClassInstanceCreationExpression_Qualifier) -> hydra.ast.Expr:
    match q:
        case hydra.java.syntax.ClassInstanceCreationExpression_QualifierExpression(value=en):
            return write_expression_name(en)

        case hydra.java.syntax.ClassInstanceCreationExpression_QualifierPrimary(value=p):
            return write_primary(p)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_class_member_declaration(d: hydra.java.syntax.ClassMemberDeclaration) -> hydra.ast.Expr:
    match d:
        case hydra.java.syntax.ClassMemberDeclarationField(value=fd):
            return write_field_declaration(fd)

        case hydra.java.syntax.ClassMemberDeclarationMethod(value=md):
            return write_method_declaration(md)

        case hydra.java.syntax.ClassMemberDeclarationClass(value=cd):
            return write_class_declaration(cd)

        case hydra.java.syntax.ClassMemberDeclarationInterface(value=id):
            return write_interface_declaration(id)

        case hydra.java.syntax.ClassMemberDeclarationNone():
            return hydra.serialization.cst(";")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_class_modifier(m: hydra.java.syntax.ClassModifier) -> hydra.ast.Expr:
    match m:
        case hydra.java.syntax.ClassModifierAnnotation(value=ann):
            return write_annotation(ann)

        case hydra.java.syntax.ClassModifierPublic():
            return hydra.serialization.cst("public")

        case hydra.java.syntax.ClassModifierProtected():
            return hydra.serialization.cst("protected")

        case hydra.java.syntax.ClassModifierPrivate():
            return hydra.serialization.cst("private")

        case hydra.java.syntax.ClassModifierAbstract():
            return hydra.serialization.cst("abstract")

        case hydra.java.syntax.ClassModifierStatic():
            return hydra.serialization.cst("static")

        case hydra.java.syntax.ClassModifierFinal():
            return hydra.serialization.cst("final")

        case hydra.java.syntax.ClassModifierStrictfp():
            return hydra.serialization.cst("strictfp")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_class_or_interface_type(cit: hydra.java.syntax.ClassOrInterfaceType) -> hydra.ast.Expr:
    match cit:
        case hydra.java.syntax.ClassOrInterfaceTypeClass(value=ct):
            return write_class_type(ct)

        case hydra.java.syntax.ClassOrInterfaceTypeInterface(value=it):
            return write_interface_type(it)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_class_or_interface_type_to_instantiate(coitti: hydra.java.syntax.ClassOrInterfaceTypeToInstantiate) -> hydra.ast.Expr:
    ids = coitti.identifiers
    margs = coitti.type_arguments
    return hydra.serialization.no_sep(hydra.lib.maybes.cat((Just(hydra.serialization.dot_sep(hydra.lib.lists.map((lambda x1: write_annotated_identifier(x1)), ids))), hydra.lib.maybes.map((lambda x1: write_type_arguments_or_diamond(x1)), margs))))

def write_class_type(ct: hydra.java.syntax.ClassType) -> hydra.ast.Expr:
    anns = ct.annotations
    qual = ct.qualifier
    id = ct.identifier
    args = ct.arguments
    @lru_cache(1)
    def qualified_id() -> hydra.ast.Expr:
        match qual:
            case hydra.java.syntax.ClassTypeQualifierNone():
                return write_type_identifier(id)

            case hydra.java.syntax.ClassTypeQualifierPackage(value=pkg):
                return hydra.serialization.dot_sep((write_package_name(pkg), write_type_identifier(id)))

            case hydra.java.syntax.ClassTypeQualifierParent(value=cit):
                return hydra.serialization.dot_sep((write_class_or_interface_type(cit), write_type_identifier(id)))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.serialization.no_sep(hydra.lib.maybes.cat((Just(hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(anns), (lambda : Nothing()), (lambda : Just(hydra.serialization.comma_sep(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_annotation(x1)), anns))))), Just(qualified_id()))))), hydra.lib.logic.if_else(hydra.lib.lists.null(args), (lambda : Nothing()), (lambda : Just(hydra.serialization.angle_braces_list(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_type_argument(x1)), args))))))))

def write_conditional_and_expression(cae: hydra.java.syntax.ConditionalAndExpression) -> hydra.ast.Expr:
    return hydra.serialization.infix_ws_list("&&", hydra.lib.lists.map((lambda x1: write_inclusive_or_expression(x1)), cae.value))

def write_conditional_expression(c: hydra.java.syntax.ConditionalExpression) -> hydra.ast.Expr:
    match c:
        case hydra.java.syntax.ConditionalExpressionSimple(value=co):
            return write_conditional_or_expression(co)

        case hydra.java.syntax.ConditionalExpressionTernaryCond(value=tc):
            return write_conditional_expression_ternary_cond(tc)

        case hydra.java.syntax.ConditionalExpressionTernaryLambda(value=tl):
            return write_conditional_expression_ternary_lambda(tl)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_conditional_or_expression(coe: hydra.java.syntax.ConditionalOrExpression) -> hydra.ast.Expr:
    return hydra.serialization.infix_ws_list("||", hydra.lib.lists.map((lambda x1: write_conditional_and_expression(x1)), coe.value))

def write_constant_declaration(cd: hydra.java.syntax.ConstantDeclaration) -> hydra.ast.Expr:
    mods = cd.modifiers
    typ = cd.type
    vars = cd.variables
    return hydra.serialization.with_semi(hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(mods), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_constant_modifier(x1)), mods))))), Just(write_unann_type(typ)), Just(hydra.serialization.comma_sep(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_variable_declarator(x1)), vars)))))))

def write_constructor_body(cb: hydra.java.syntax.ConstructorBody) -> hydra.ast.Expr:
    minvoc = cb.invocation
    stmts = cb.statements
    return hydra.serialization.curly_block(hydra.serialization.full_block_style, hydra.serialization.double_newline_sep(hydra.lib.maybes.cat((hydra.lib.maybes.map((lambda x1: write_explicit_constructor_invocation(x1)), minvoc), Just(hydra.serialization.newline_sep(hydra.lib.lists.map((lambda x1: write_block_statement(x1)), stmts)))))))

def write_constructor_declaration(cd: hydra.java.syntax.ConstructorDeclaration) -> hydra.ast.Expr:
    mods = cd.modifiers
    cons = cd.constructor
    mthrows = cd.throws
    body = cd.body
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(mods), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_constructor_modifier(x1)), mods))))), Just(write_constructor_declarator(cons)), hydra.lib.maybes.map((lambda x1: write_throws(x1)), mthrows), Just(write_constructor_body(body)))))

def write_constructor_declarator(cd: hydra.java.syntax.ConstructorDeclarator) -> hydra.ast.Expr:
    tparams = cd.parameters
    name = cd.name
    fparams = cd.formal_parameters
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(tparams), (lambda : Nothing()), (lambda : Just(hydra.serialization.angle_braces_list(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_type_parameter(x1)), tparams))))), Just(write_simple_type_name(name)), Just(hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: write_formal_parameter(x1)), fparams))))))

def write_constructor_modifier(m: hydra.java.syntax.ConstructorModifier) -> hydra.ast.Expr:
    match m:
        case hydra.java.syntax.ConstructorModifierAnnotation(value=ann):
            return write_annotation(ann)

        case hydra.java.syntax.ConstructorModifierPublic():
            return hydra.serialization.cst("public")

        case hydra.java.syntax.ConstructorModifierProtected():
            return hydra.serialization.cst("protected")

        case hydra.java.syntax.ConstructorModifierPrivate():
            return hydra.serialization.cst("private")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_element_value(ev: hydra.java.syntax.ElementValue) -> hydra.ast.Expr:
    match ev:
        case hydra.java.syntax.ElementValueConditionalExpression(value=c):
            return write_conditional_expression(c)

        case hydra.java.syntax.ElementValueElementValueArrayInitializer(value=evai):
            return hydra.serialization.comma_sep(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_element_value(x1)), evai.value))

        case hydra.java.syntax.ElementValueAnnotation(value=ann):
            return write_annotation(ann)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_element_value_pair(evp: hydra.java.syntax.ElementValuePair) -> hydra.ast.Expr:
    k = evp.key
    v = evp.value
    return hydra.serialization.infix_ws("=", write_identifier(k), write_element_value(v))

def write_equality_expression(e: hydra.java.syntax.EqualityExpression) -> hydra.ast.Expr:
    match e:
        case hydra.java.syntax.EqualityExpressionUnary(value=r):
            return write_relational_expression(r)

        case hydra.java.syntax.EqualityExpressionEqual(value=b):
            return hydra.serialization.infix_ws("==", write_equality_expression(b.lhs), write_relational_expression(b.rhs))

        case hydra.java.syntax.EqualityExpressionNotEqual(value=b2):
            return hydra.serialization.infix_ws("!=", write_equality_expression(b2.lhs), write_relational_expression(b2.rhs))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_exclusive_or_expression(eoe: hydra.java.syntax.ExclusiveOrExpression) -> hydra.ast.Expr:
    return hydra.serialization.infix_ws_list("^", hydra.lib.lists.map((lambda x1: write_and_expression(x1)), eoe.value))

def write_expression(e: hydra.java.syntax.Expression) -> hydra.ast.Expr:
    match e:
        case hydra.java.syntax.ExpressionLambda(value=l):
            return write_lambda_expression(l)

        case hydra.java.syntax.ExpressionAssignment(value=a):
            return write_assignment_expression(a)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_expression_statement(es: hydra.java.syntax.ExpressionStatement) -> hydra.ast.Expr:
    return hydra.serialization.with_semi(write_statement_expression(es.value))

def write_field_access(fa: hydra.java.syntax.FieldAccess) -> hydra.ast.Expr:
    qual = fa.qualifier
    id = fa.identifier
    match qual:
        case hydra.java.syntax.FieldAccess_QualifierPrimary(value=p):
            return hydra.serialization.dot_sep((write_primary(p), write_identifier(id)))

        case hydra.java.syntax.FieldAccess_QualifierSuper():
            return hydra.serialization.dot_sep((hydra.serialization.cst("super"), write_identifier(id)))

        case hydra.java.syntax.FieldAccess_QualifierTyped(value=tn):
            return hydra.serialization.dot_sep((write_type_name(tn), hydra.serialization.cst("super"), write_identifier(id)))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_field_declaration(fd: hydra.java.syntax.FieldDeclaration) -> hydra.ast.Expr:
    mods = fd.modifiers
    typ = fd.unann_type
    vars = fd.variable_declarators
    return hydra.serialization.with_semi(hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(mods), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_field_modifier(x1)), mods))))), Just(write_unann_type(typ)), Just(hydra.serialization.comma_sep(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_variable_declarator(x1)), vars)))))))

def write_field_modifier(m: hydra.java.syntax.FieldModifier) -> hydra.ast.Expr:
    match m:
        case hydra.java.syntax.FieldModifierAnnotation(value=ann):
            return write_annotation(ann)

        case hydra.java.syntax.FieldModifierPublic():
            return hydra.serialization.cst("public")

        case hydra.java.syntax.FieldModifierProtected():
            return hydra.serialization.cst("protected")

        case hydra.java.syntax.FieldModifierPrivate():
            return hydra.serialization.cst("private")

        case hydra.java.syntax.FieldModifierStatic():
            return hydra.serialization.cst("static")

        case hydra.java.syntax.FieldModifierFinal():
            return hydra.serialization.cst("final")

        case hydra.java.syntax.FieldModifierTransient():
            return hydra.serialization.cst("transient")

        case hydra.java.syntax.FieldModifierVolatile():
            return hydra.serialization.cst("volatile")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_formal_parameter(p: hydra.java.syntax.FormalParameter) -> hydra.ast.Expr:
    match p:
        case hydra.java.syntax.FormalParameterSimple(value=s):
            return write_formal_parameter_simple(s)

        case hydra.java.syntax.FormalParameterVariableArity(value=v):
            return write_variable_arity_parameter(v)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_formal_parameter_simple(fps: hydra.java.syntax.FormalParameter_Simple) -> hydra.ast.Expr:
    mods = fps.modifiers
    typ = fps.type
    id = fps.id
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(mods), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_variable_modifier(x1)), mods))))), Just(write_unann_type(typ)), Just(write_variable_declarator_id(id)))))

def write_if_then_statement(its: hydra.java.syntax.IfThenStatement) -> hydra.ast.Expr:
    cond = its.expression
    thn = its.statement
    return hydra.serialization.space_sep((hydra.serialization.cst("if"), hydra.serialization.paren_list(False, (write_expression(cond),)), hydra.serialization.curly_block(hydra.serialization.full_block_style, write_statement(thn))))

def write_inclusive_or_expression(ioe: hydra.java.syntax.InclusiveOrExpression) -> hydra.ast.Expr:
    return hydra.serialization.infix_ws_list("|", hydra.lib.lists.map((lambda x1: write_exclusive_or_expression(x1)), ioe.value))

def write_interface_body(ib: hydra.java.syntax.InterfaceBody) -> hydra.ast.Expr:
    return hydra.serialization.curly_block(hydra.serialization.full_block_style, hydra.serialization.double_newline_sep(hydra.lib.lists.map((lambda x1: write_interface_member_declaration(x1)), ib.value)))

def write_interface_declaration(d: hydra.java.syntax.InterfaceDeclaration) -> hydra.ast.Expr:
    match d:
        case hydra.java.syntax.InterfaceDeclarationNormalInterface(value=n):
            return write_normal_interface_declaration(n)

        case hydra.java.syntax.InterfaceDeclarationAnnotationType(value=a):
            return write_annotation_type_declaration(a)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_interface_member_declaration(d: hydra.java.syntax.InterfaceMemberDeclaration) -> hydra.ast.Expr:
    match d:
        case hydra.java.syntax.InterfaceMemberDeclarationConstant(value=c):
            return write_constant_declaration(c)

        case hydra.java.syntax.InterfaceMemberDeclarationInterfaceMethod(value=im):
            return write_interface_method_declaration(im)

        case hydra.java.syntax.InterfaceMemberDeclarationClass(value=cd):
            return write_class_declaration(cd)

        case hydra.java.syntax.InterfaceMemberDeclarationInterface(value=id):
            return write_interface_declaration(id)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_interface_method_declaration(imd: hydra.java.syntax.InterfaceMethodDeclaration) -> hydra.ast.Expr:
    mods = imd.modifiers
    header = imd.header
    body = imd.body
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(mods), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_interface_method_modifier(x1)), mods))))), Just(write_method_header(header)), Just(write_method_body(body)))))

def write_interface_method_modifier(m: hydra.java.syntax.InterfaceMethodModifier) -> hydra.ast.Expr:
    match m:
        case hydra.java.syntax.InterfaceMethodModifierAnnotation(value=a):
            return write_annotation(a)

        case hydra.java.syntax.InterfaceMethodModifierPublic():
            return hydra.serialization.cst("public")

        case hydra.java.syntax.InterfaceMethodModifierPrivate():
            return hydra.serialization.cst("private")

        case hydra.java.syntax.InterfaceMethodModifierAbstract():
            return hydra.serialization.cst("abstract")

        case hydra.java.syntax.InterfaceMethodModifierDefault():
            return hydra.serialization.cst("default")

        case hydra.java.syntax.InterfaceMethodModifierStatic():
            return hydra.serialization.cst("static")

        case hydra.java.syntax.InterfaceMethodModifierStrictfp():
            return hydra.serialization.cst("strictfp")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_interface_modifier(m: hydra.java.syntax.InterfaceModifier) -> hydra.ast.Expr:
    match m:
        case hydra.java.syntax.InterfaceModifierAnnotation(value=a):
            return write_annotation(a)

        case hydra.java.syntax.InterfaceModifierPublic():
            return hydra.serialization.cst("public")

        case hydra.java.syntax.InterfaceModifierProtected():
            return hydra.serialization.cst("protected")

        case hydra.java.syntax.InterfaceModifierPrivate():
            return hydra.serialization.cst("private")

        case hydra.java.syntax.InterfaceModifierAbstract():
            return hydra.serialization.cst("abstract")

        case hydra.java.syntax.InterfaceModifierStatic():
            return hydra.serialization.cst("static")

        case hydra.java.syntax.InterfaceModifierStrictfb():
            return hydra.serialization.cst("strictfb")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_interface_type(it: hydra.java.syntax.InterfaceType) -> hydra.ast.Expr:
    return write_class_type(it.value)

def write_lambda_body(b: hydra.java.syntax.LambdaBody) -> hydra.ast.Expr:
    match b:
        case hydra.java.syntax.LambdaBodyExpression(value=e):
            return write_expression(e)

        case hydra.java.syntax.LambdaBodyBlock(value=b2):
            return write_block(b2)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_lambda_expression(le: hydra.java.syntax.LambdaExpression) -> hydra.ast.Expr:
    params = le.parameters
    body = le.body
    return hydra.serialization.infix_ws("->", write_lambda_parameters(params), write_lambda_body(body))

def write_left_hand_side(lhs: hydra.java.syntax.LeftHandSide) -> hydra.ast.Expr:
    match lhs:
        case hydra.java.syntax.LeftHandSideExpressionName(value=en):
            return write_expression_name(en)

        case hydra.java.syntax.LeftHandSideFieldAccess(value=fa):
            return write_field_access(fa)

        case hydra.java.syntax.LeftHandSideArrayAccess(value=aa):
            return write_array_access(aa)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_local_name(t: hydra.java.syntax.LocalVariableType) -> hydra.ast.Expr:
    match t:
        case hydra.java.syntax.LocalVariableTypeType(value=ut):
            return write_unann_type(ut)

        case hydra.java.syntax.LocalVariableTypeVar():
            return hydra.serialization.cst("var")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_local_variable_declaration(lvd: hydra.java.syntax.LocalVariableDeclaration) -> hydra.ast.Expr:
    mods = lvd.modifiers
    t = lvd.type
    decls = lvd.declarators
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(mods), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_variable_modifier(x1)), mods))))), Just(write_local_name(t)), Just(hydra.serialization.comma_sep(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_variable_declarator(x1)), decls))))))

def write_local_variable_declaration_statement(lvds: hydra.java.syntax.LocalVariableDeclarationStatement) -> hydra.ast.Expr:
    return hydra.serialization.with_semi(write_local_variable_declaration(lvds.value))

def write_method_body(b: hydra.java.syntax.MethodBody) -> hydra.ast.Expr:
    match b:
        case hydra.java.syntax.MethodBodyBlock(value=block):
            return write_block(block)

        case hydra.java.syntax.MethodBodyNone():
            return hydra.serialization.cst(";")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_method_declaration(md: hydra.java.syntax.MethodDeclaration) -> hydra.ast.Expr:
    anns = md.annotations
    mods = md.modifiers
    header = md.header
    body = md.body
    @lru_cache(1)
    def header_and_body() -> hydra.ast.Expr:
        return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(mods), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_method_modifier(x1)), mods))))), Just(write_method_header(header)), Just(write_method_body(body)))))
    return hydra.serialization.newline_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(anns), (lambda : Nothing()), (lambda : Just(hydra.serialization.newline_sep(hydra.lib.lists.map((lambda x1: write_annotation(x1)), anns))))), Just(header_and_body()))))

def write_method_declarator(md: hydra.java.syntax.MethodDeclarator) -> hydra.ast.Expr:
    id = md.identifier
    params = md.formal_parameters
    return hydra.serialization.no_sep((write_identifier(id), hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: write_formal_parameter(x1)), params))))

def write_method_header(mh: hydra.java.syntax.MethodHeader) -> hydra.ast.Expr:
    params = mh.parameters
    result = mh.result
    decl = mh.declarator
    mthrows = mh.throws
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(params), (lambda : Nothing()), (lambda : Just(hydra.serialization.angle_braces_list(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_type_parameter(x1)), params))))), Just(write_result(result)), Just(write_method_declarator(decl)), hydra.lib.maybes.map((lambda x1: write_throws(x1)), mthrows))))

def write_method_invocation(mi: hydra.java.syntax.MethodInvocation) -> hydra.ast.Expr:
    header = mi.header
    args = mi.arguments
    @lru_cache(1)
    def arg_sec() -> hydra.ast.Expr:
        return hydra.serialization.paren_list(True, hydra.lib.lists.map((lambda x1: write_expression(x1)), args))
    @lru_cache(1)
    def header_sec():
        match header:
            case hydra.java.syntax.MethodInvocation_HeaderSimple(value=mname):
                return write_method_name(mname)

            case hydra.java.syntax.MethodInvocation_HeaderComplex(value=cx):
                cvar = cx.variant
                targs = cx.type_arguments
                cid = cx.identifier
                @lru_cache(1)
                def id_sec() -> hydra.ast.Expr:
                    return hydra.serialization.no_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(targs), (lambda : Nothing()), (lambda : Just(hydra.serialization.angle_braces_list(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_type_argument(x1)), targs))))), Just(write_identifier(cid)))))
                def _hoist_cvar_body_1(v1):
                    match v1:
                        case hydra.java.syntax.MethodInvocation_VariantType(value=tname):
                            return hydra.serialization.dot_sep((write_type_name(tname), id_sec()))

                        case hydra.java.syntax.MethodInvocation_VariantExpression(value=en):
                            return hydra.serialization.dot_sep((write_expression_name(en), id_sec()))

                        case hydra.java.syntax.MethodInvocation_VariantPrimary(value=p):
                            return hydra.serialization.dot_sep((write_primary(p), id_sec()))

                        case hydra.java.syntax.MethodInvocation_VariantSuper():
                            return hydra.serialization.dot_sep((hydra.serialization.cst("super"), id_sec()))

                        case hydra.java.syntax.MethodInvocation_VariantTypeSuper(value=tname):
                            return hydra.serialization.dot_sep((write_type_name(tname), hydra.serialization.cst("super"), id_sec()))

                        case _:
                            raise AssertionError("Unreachable: all variants handled")
                return _hoist_cvar_body_1(cvar)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.serialization.no_sep((header_sec(), arg_sec()))

def write_method_modifier(m: hydra.java.syntax.MethodModifier) -> hydra.ast.Expr:
    match m:
        case hydra.java.syntax.MethodModifierAnnotation(value=ann):
            return write_annotation(ann)

        case hydra.java.syntax.MethodModifierPublic():
            return hydra.serialization.cst("public")

        case hydra.java.syntax.MethodModifierProtected():
            return hydra.serialization.cst("protected")

        case hydra.java.syntax.MethodModifierPrivate():
            return hydra.serialization.cst("private")

        case hydra.java.syntax.MethodModifierAbstract():
            return hydra.serialization.cst("abstract")

        case hydra.java.syntax.MethodModifierFinal():
            return hydra.serialization.cst("final")

        case hydra.java.syntax.MethodModifierSynchronized():
            return hydra.serialization.cst("synchronized")

        case hydra.java.syntax.MethodModifierNative():
            return hydra.serialization.cst("native")

        case hydra.java.syntax.MethodModifierStrictfb():
            return hydra.serialization.cst("strictfb")

        case _:
            raise TypeError("Unsupported MethodModifier")

def write_multiplicative_expression(e: hydra.java.syntax.MultiplicativeExpression) -> hydra.ast.Expr:
    match e:
        case hydra.java.syntax.MultiplicativeExpressionUnary(value=u):
            return write_unary_expression(u)

        case hydra.java.syntax.MultiplicativeExpressionTimes(value=b):
            return hydra.serialization.infix_ws("*", write_multiplicative_expression(b.lhs), write_unary_expression(b.rhs))

        case hydra.java.syntax.MultiplicativeExpressionDivide(value=b2):
            return hydra.serialization.infix_ws("/", write_multiplicative_expression(b2.lhs), write_unary_expression(b2.rhs))

        case hydra.java.syntax.MultiplicativeExpressionMod(value=b3):
            return hydra.serialization.infix_ws("%", write_multiplicative_expression(b3.lhs), write_unary_expression(b3.rhs))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_normal_annotation(na: hydra.java.syntax.NormalAnnotation) -> hydra.ast.Expr:
    tname = na.type_name
    pairs = na.pairs
    return hydra.serialization.prefix("@", hydra.serialization.no_sep((write_type_name(tname), hydra.serialization.comma_sep(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_element_value_pair(x1)), pairs)))))

def write_normal_class_declaration(ncd: hydra.java.syntax.NormalClassDeclaration) -> hydra.ast.Expr:
    mods = ncd.modifiers
    id = ncd.identifier
    tparams = ncd.parameters
    msuperc = ncd.extends
    superi = ncd.implements
    body = ncd.body
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(mods), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_class_modifier(x1)), mods))))), Just(hydra.serialization.cst("class")), Just(hydra.serialization.no_sep(hydra.lib.maybes.cat((Just(write_type_identifier(id)), hydra.lib.logic.if_else(hydra.lib.lists.null(tparams), (lambda : Nothing()), (lambda : Just(hydra.serialization.angle_braces_list(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_type_parameter(x1)), tparams))))))))), hydra.lib.maybes.map((lambda c: hydra.serialization.space_sep((hydra.serialization.cst("extends"), write_class_type(c)))), msuperc), hydra.lib.logic.if_else(hydra.lib.lists.null(superi), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep((hydra.serialization.cst("implements"), hydra.serialization.comma_sep(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_interface_type(x1)), superi))))))), Just(write_class_body(body)))))

def write_normal_interface_declaration(nid: hydra.java.syntax.NormalInterfaceDeclaration) -> hydra.ast.Expr:
    mods = nid.modifiers
    id = nid.identifier
    tparams = nid.parameters
    extends = nid.extends
    body = nid.body
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(mods), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_interface_modifier(x1)), mods))))), Just(hydra.serialization.cst("interface")), Just(hydra.serialization.no_sep(hydra.lib.maybes.cat((Just(write_type_identifier(id)), hydra.lib.logic.if_else(hydra.lib.lists.null(tparams), (lambda : Nothing()), (lambda : Just(hydra.serialization.angle_braces_list(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_type_parameter(x1)), tparams))))))))), hydra.lib.logic.if_else(hydra.lib.lists.null(extends), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep((hydra.serialization.cst("extends"), hydra.serialization.comma_sep(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_interface_type(x1)), extends))))))), Just(write_interface_body(body)))))

def write_postfix_expression(e: hydra.java.syntax.PostfixExpression) -> hydra.ast.Expr:
    match e:
        case hydra.java.syntax.PostfixExpressionPrimary(value=p):
            return write_primary(p)

        case hydra.java.syntax.PostfixExpressionName(value=en):
            return write_expression_name(en)

        case hydra.java.syntax.PostfixExpressionPostIncrement(value=pi):
            return write_post_increment_expression(pi)

        case hydra.java.syntax.PostfixExpressionPostDecrement(value=pd):
            return write_post_decrement_expression(pd)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_primary(p: hydra.java.syntax.Primary) -> hydra.ast.Expr:
    match p:
        case hydra.java.syntax.PrimaryNoNewArray(value=n):
            return write_primary_no_new_array_expression_expression(n)

        case hydra.java.syntax.PrimaryArrayCreation(value=a):
            return write_array_creation_expression(a)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_primary_no_new_array_expression_expression(p: hydra.java.syntax.PrimaryNoNewArrayExpression) -> hydra.ast.Expr:
    match p:
        case hydra.java.syntax.PrimaryNoNewArrayExpressionLiteral(value=l):
            return write_literal(l)

        case hydra.java.syntax.PrimaryNoNewArrayExpressionClassLiteral(value=cl):
            return write_class_literal(cl)

        case hydra.java.syntax.PrimaryNoNewArrayExpressionThis():
            return hydra.serialization.cst("this")

        case hydra.java.syntax.PrimaryNoNewArrayExpressionDotThis(value=n):
            return hydra.serialization.dot_sep((write_type_name(n), hydra.serialization.cst("this")))

        case hydra.java.syntax.PrimaryNoNewArrayExpressionParens(value=e):
            return hydra.serialization.paren_list(False, (write_expression(e),))

        case hydra.java.syntax.PrimaryNoNewArrayExpressionClassInstance(value=ci):
            return write_class_instance_creation_expression(ci)

        case hydra.java.syntax.PrimaryNoNewArrayExpressionFieldAccess(value=fa):
            return write_field_access(fa)

        case hydra.java.syntax.PrimaryNoNewArrayExpressionArrayAccess(value=aa):
            return write_array_access(aa)

        case hydra.java.syntax.PrimaryNoNewArrayExpressionMethodInvocation(value=mi):
            return write_method_invocation(mi)

        case hydra.java.syntax.PrimaryNoNewArrayExpressionMethodReference(value=mr):
            return write_method_reference(mr)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_primitive_type_with_annotations(ptwa: hydra.java.syntax.PrimitiveTypeWithAnnotations) -> hydra.ast.Expr:
    pt = ptwa.type
    anns = ptwa.annotations
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(anns), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_annotation(x1)), anns))))), Just(write_primitive_type(pt)))))

def write_reference_type(rt: hydra.java.syntax.ReferenceType) -> hydra.ast.Expr:
    match rt:
        case hydra.java.syntax.ReferenceTypeClassOrInterface(value=cit):
            return write_class_or_interface_type(cit)

        case hydra.java.syntax.ReferenceTypeVariable(value=v):
            return write_type_variable(v)

        case hydra.java.syntax.ReferenceTypeArray(value=at):
            return write_array_type(at)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_relational_expression(e: hydra.java.syntax.RelationalExpression) -> hydra.ast.Expr:
    match e:
        case hydra.java.syntax.RelationalExpressionSimple(value=s):
            return write_shift_expression(s)

        case hydra.java.syntax.RelationalExpressionLessThan(value=lt):
            return write_relational_expression_less_than(lt)

        case hydra.java.syntax.RelationalExpressionGreaterThan(value=gt):
            return write_relational_expression_greater_than(gt)

        case hydra.java.syntax.RelationalExpressionLessThanEqual(value=lte):
            return write_relational_expression_less_than_equal(lte)

        case hydra.java.syntax.RelationalExpressionGreaterThanEqual(value=gte):
            return write_relational_expression_greater_than_equal(gte)

        case hydra.java.syntax.RelationalExpressionInstanceof(value=i):
            return write_relational_expression_instance_of(i)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_relational_expression_greater_than(gt: hydra.java.syntax.RelationalExpression_GreaterThan) -> hydra.ast.Expr:
    return hydra.serialization.infix_ws(">", write_relational_expression(gt.lhs), write_shift_expression(gt.rhs))

def write_relational_expression_greater_than_equal(gte: hydra.java.syntax.RelationalExpression_GreaterThanEqual) -> hydra.ast.Expr:
    return hydra.serialization.infix_ws(">=", write_relational_expression(gte.lhs), write_shift_expression(gte.rhs))

def write_relational_expression_instance_of(io: hydra.java.syntax.RelationalExpression_InstanceOf) -> hydra.ast.Expr:
    return hydra.serialization.infix_ws("instanceof", write_relational_expression(io.lhs), write_reference_type(io.rhs))

def write_relational_expression_less_than(lt: hydra.java.syntax.RelationalExpression_LessThan) -> hydra.ast.Expr:
    return hydra.serialization.infix_ws("<", write_relational_expression(lt.lhs), write_shift_expression(lt.rhs))

def write_relational_expression_less_than_equal(lte: hydra.java.syntax.RelationalExpression_LessThanEqual) -> hydra.ast.Expr:
    return hydra.serialization.infix_ws("<=", write_relational_expression(lte.lhs), write_shift_expression(lte.rhs))

def write_result(r: hydra.java.syntax.Result) -> hydra.ast.Expr:
    match r:
        case hydra.java.syntax.ResultType(value=t):
            return write_unann_type(t)

        case hydra.java.syntax.ResultVoid():
            return hydra.serialization.cst("void")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_return_statement(rs: hydra.java.syntax.ReturnStatement) -> hydra.ast.Expr:
    mex = rs.value
    return hydra.serialization.with_semi(hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(hydra.serialization.cst("return")), hydra.lib.maybes.map((lambda x1: write_expression(x1)), mex)))))

def write_shift_expression(e: hydra.java.syntax.ShiftExpression) -> hydra.ast.Expr:
    match e:
        case hydra.java.syntax.ShiftExpressionUnary(value=a):
            return write_additive_expression(a)

        case hydra.java.syntax.ShiftExpressionShiftLeft(value=b):
            return hydra.serialization.infix_ws("<<", write_shift_expression(b.lhs), write_additive_expression(b.rhs))

        case hydra.java.syntax.ShiftExpressionShiftRight(value=b2):
            return hydra.serialization.infix_ws(">>", write_shift_expression(b2.lhs), write_additive_expression(b2.rhs))

        case hydra.java.syntax.ShiftExpressionShiftRightZeroFill(value=b3):
            return hydra.serialization.infix_ws(">>>", write_shift_expression(b3.lhs), write_additive_expression(b3.rhs))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_single_element_annotation(sea: hydra.java.syntax.SingleElementAnnotation) -> hydra.ast.Expr:
    tname = sea.name
    mv = sea.value
    return hydra.lib.maybes.maybe((lambda : write_marker_annotation(hydra.java.syntax.MarkerAnnotation(tname))), (lambda v: hydra.serialization.prefix("@", hydra.serialization.no_sep((write_type_name(tname), hydra.serialization.paren_list(False, (write_element_value(v),)))))), mv)

def write_statement(s: hydra.java.syntax.Statement) -> hydra.ast.Expr:
    match s:
        case hydra.java.syntax.StatementWithoutTrailing(value=s2):
            return write_statement_without_trailing_substatement(s2)

        case hydra.java.syntax.StatementLabeled(value=l):
            return write_labeled_statement(l)

        case hydra.java.syntax.StatementIfThen(value=it):
            return write_if_then_statement(it)

        case hydra.java.syntax.StatementIfThenElse(value=ite):
            return write_if_then_else_statement(ite)

        case hydra.java.syntax.StatementWhile(value=w):
            return write_while_statement(w)

        case hydra.java.syntax.StatementFor(value=f):
            return write_for_statement(f)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_statement_expression(e: hydra.java.syntax.StatementExpression) -> hydra.ast.Expr:
    match e:
        case hydra.java.syntax.StatementExpressionAssignment(value=a):
            return write_assignment(a)

        case hydra.java.syntax.StatementExpressionPreIncrement(value=pi):
            return write_pre_increment_expression(pi)

        case hydra.java.syntax.StatementExpressionPreDecrement(value=pd):
            return write_pre_decrement_expression(pd)

        case hydra.java.syntax.StatementExpressionPostIncrement(value=pi2):
            return write_post_increment_expression(pi2)

        case hydra.java.syntax.StatementExpressionPostDecrement(value=pd2):
            return write_post_decrement_expression(pd2)

        case hydra.java.syntax.StatementExpressionMethodInvocation(value=m):
            return write_method_invocation(m)

        case hydra.java.syntax.StatementExpressionClassInstanceCreation(value=cic):
            return write_class_instance_creation_expression(cic)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_statement_without_trailing_substatement(s: hydra.java.syntax.StatementWithoutTrailingSubstatement) -> hydra.ast.Expr:
    match s:
        case hydra.java.syntax.StatementWithoutTrailingSubstatementBlock(value=b):
            return write_block(b)

        case hydra.java.syntax.StatementWithoutTrailingSubstatementEmpty():
            return hydra.serialization.cst(";")

        case hydra.java.syntax.StatementWithoutTrailingSubstatementExpression(value=e):
            return write_expression_statement(e)

        case hydra.java.syntax.StatementWithoutTrailingSubstatementAssert(value=a):
            return write_assert_statement(a)

        case hydra.java.syntax.StatementWithoutTrailingSubstatementSwitch(value=s2):
            return write_switch_statement(s2)

        case hydra.java.syntax.StatementWithoutTrailingSubstatementDo(value=d):
            return write_do_statement(d)

        case hydra.java.syntax.StatementWithoutTrailingSubstatementBreak(value=b2):
            return write_break_statement(b2)

        case hydra.java.syntax.StatementWithoutTrailingSubstatementContinue(value=c):
            return write_continue_statement(c)

        case hydra.java.syntax.StatementWithoutTrailingSubstatementReturn(value=r):
            return write_return_statement(r)

        case hydra.java.syntax.StatementWithoutTrailingSubstatementSynchronized(value=s22):
            return write_synchronized_statement(s22)

        case hydra.java.syntax.StatementWithoutTrailingSubstatementThrow(value=t):
            return write_throw_statement(t)

        case hydra.java.syntax.StatementWithoutTrailingSubstatementTry(value=t2):
            return write_try_statement(t2)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_throw_statement(ts: hydra.java.syntax.ThrowStatement) -> hydra.ast.Expr:
    return hydra.serialization.with_semi(hydra.serialization.space_sep((hydra.serialization.cst("throw"), write_expression(ts.value))))

def write_type(t: hydra.java.syntax.Type) -> hydra.ast.Expr:
    match t:
        case hydra.java.syntax.TypePrimitive(value=pt):
            return write_primitive_type_with_annotations(pt)

        case hydra.java.syntax.TypeReference(value=rt):
            return write_reference_type(rt)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_type_argument(a: hydra.java.syntax.TypeArgument) -> hydra.ast.Expr:
    match a:
        case hydra.java.syntax.TypeArgumentReference(value=rt):
            return write_reference_type(rt)

        case hydra.java.syntax.TypeArgumentWildcard(value=w):
            return write_wildcard(w)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_type_arguments_or_diamond(targs: hydra.java.syntax.TypeArgumentsOrDiamond) -> hydra.ast.Expr:
    match targs:
        case hydra.java.syntax.TypeArgumentsOrDiamondArguments(value=args):
            return hydra.serialization.angle_braces_list(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_type_argument(x1)), args))

        case hydra.java.syntax.TypeArgumentsOrDiamondDiamond():
            return hydra.serialization.cst("<>")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_type_bound(b: hydra.java.syntax.TypeBound) -> hydra.ast.Expr:
    match b:
        case hydra.java.syntax.TypeBoundVariable(value=tv):
            return write_type_variable(tv)

        case hydra.java.syntax.TypeBoundClassOrInterface(value=ci):
            cit = ci.type
            additional = ci.additional
            return hydra.lib.logic.if_else(hydra.lib.lists.null(additional), (lambda : write_class_or_interface_type(cit)), (lambda : hydra.serialization.space_sep(hydra.lib.lists.cons(write_class_or_interface_type(cit), hydra.lib.lists.map((lambda x1: write_additional_bound(x1)), additional)))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_type_parameter(tp: hydra.java.syntax.TypeParameter) -> hydra.ast.Expr:
    mods = tp.modifiers
    id = tp.identifier
    bound = tp.bound
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(mods), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_type_parameter_modifier(x1)), mods))))), Just(write_type_identifier(id)), hydra.lib.maybes.map((lambda b: hydra.serialization.space_sep((hydra.serialization.cst("extends"), write_type_bound(b)))), bound))))

def write_type_parameter_modifier(tpm: hydra.java.syntax.TypeParameterModifier) -> hydra.ast.Expr:
    return write_annotation(tpm.value)

def write_type_variable(tv: hydra.java.syntax.TypeVariable) -> hydra.ast.Expr:
    anns = tv.annotations
    id = tv.identifier
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(anns), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_annotation(x1)), anns))))), Just(write_type_identifier(id)))))

def write_unann_type(ut: hydra.java.syntax.UnannType) -> hydra.ast.Expr:
    return write_type(ut.value)

def write_unary_expression(e: hydra.java.syntax.UnaryExpression) -> hydra.ast.Expr:
    match e:
        case hydra.java.syntax.UnaryExpressionPreIncrement(value=pi):
            return write_pre_increment_expression(pi)

        case hydra.java.syntax.UnaryExpressionPreDecrement(value=pd):
            return write_pre_decrement_expression(pd)

        case hydra.java.syntax.UnaryExpressionPlus(value=p):
            return hydra.serialization.space_sep((hydra.serialization.cst("+"), write_unary_expression(p)))

        case hydra.java.syntax.UnaryExpressionMinus(value=m):
            return hydra.serialization.space_sep((hydra.serialization.cst("-"), write_unary_expression(m)))

        case hydra.java.syntax.UnaryExpressionOther(value=o):
            return write_unary_expression_not_plus_minus(o)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_unary_expression_not_plus_minus(e: hydra.java.syntax.UnaryExpressionNotPlusMinus) -> hydra.ast.Expr:
    match e:
        case hydra.java.syntax.UnaryExpressionNotPlusMinusPostfix(value=p):
            return write_postfix_expression(p)

        case hydra.java.syntax.UnaryExpressionNotPlusMinusTilde(value=u):
            return hydra.serialization.space_sep((hydra.serialization.cst("~"), write_unary_expression(u)))

        case hydra.java.syntax.UnaryExpressionNotPlusMinusNot(value=u2):
            return hydra.serialization.no_sep((hydra.serialization.cst("!"), write_unary_expression(u2)))

        case hydra.java.syntax.UnaryExpressionNotPlusMinusCast(value=c):
            return write_cast_expression(c)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_unqualified_class_instance_creation_expression(ucice: hydra.java.syntax.UnqualifiedClassInstanceCreationExpression) -> hydra.ast.Expr:
    targs = ucice.type_arguments
    cit = ucice.class_or_interface
    args = ucice.arguments
    mbody = ucice.body
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(hydra.serialization.cst("new")), hydra.lib.logic.if_else(hydra.lib.lists.null(targs), (lambda : Nothing()), (lambda : Just(hydra.serialization.angle_braces_list(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_type_argument(x1)), targs))))), Just(hydra.serialization.no_sep((write_class_or_interface_type_to_instantiate(cit), hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: write_expression(x1)), args))))), hydra.lib.maybes.map((lambda x1: write_class_body(x1)), mbody))))

def write_variable_declarator(vd: hydra.java.syntax.VariableDeclarator) -> hydra.ast.Expr:
    id = vd.id
    minit = vd.initializer
    @lru_cache(1)
    def id_sec() -> hydra.ast.Expr:
        return write_variable_declarator_id(id)
    return hydra.lib.maybes.maybe((lambda : id_sec()), (lambda init: hydra.serialization.infix_ws("=", id_sec(), write_variable_initializer(init))), minit)

def write_variable_initializer(i: hydra.java.syntax.VariableInitializer) -> hydra.ast.Expr:
    match i:
        case hydra.java.syntax.VariableInitializerExpression(value=e):
            return write_expression(e)

        case hydra.java.syntax.VariableInitializerArrayInitializer(value=ai):
            return write_array_initializer(ai)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_variable_modifier(m: hydra.java.syntax.VariableModifier) -> hydra.ast.Expr:
    match m:
        case hydra.java.syntax.VariableModifierAnnotation(value=ann):
            return write_annotation(ann)

        case hydra.java.syntax.VariableModifierFinal():
            return hydra.serialization.cst("final")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_while_statement(ws: hydra.java.syntax.WhileStatement) -> hydra.ast.Expr:
    mcond = ws.cond
    body = ws.body
    @lru_cache(1)
    def cond_ser() -> hydra.ast.Expr:
        return hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("true")), (lambda c: write_expression(c)), mcond)
    return hydra.serialization.space_sep((hydra.serialization.cst("while"), hydra.serialization.paren_list(False, (cond_ser(),)), hydra.serialization.curly_block(hydra.serialization.full_block_style, write_statement(body))))

def write_wildcard(w: hydra.java.syntax.Wildcard) -> hydra.ast.Expr:
    anns = w.annotations
    mbounds = w.wildcard
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(anns), (lambda : Nothing()), (lambda : Just(hydra.serialization.comma_sep(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_annotation(x1)), anns))))), Just(hydra.serialization.cst("*")), hydra.lib.maybes.map((lambda x1: write_wildcard_bounds(x1)), mbounds))))

def write_wildcard_bounds(b: hydra.java.syntax.WildcardBounds) -> hydra.ast.Expr:
    match b:
        case hydra.java.syntax.WildcardBoundsExtends(value=rt):
            return hydra.serialization.space_sep((hydra.serialization.cst("extends"), write_reference_type(rt)))

        case hydra.java.syntax.WildcardBoundsSuper(value=rt2):
            return hydra.serialization.space_sep((hydra.serialization.cst("super"), write_reference_type(rt2)))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_import_declaration(imp: hydra.java.syntax.ImportDeclaration) -> hydra.ast.Expr:
    match imp:
        case hydra.java.syntax.ImportDeclarationSingleType(value=st):
            return hydra.serialization.with_semi(hydra.serialization.space_sep((hydra.serialization.cst("import"), write_type_name(st.value))))

        case hydra.java.syntax.ImportDeclarationTypeImportOnDemand():
            return hydra.serialization.cst("STUB:ImportDeclarationTypeImportOnDemand")

        case hydra.java.syntax.ImportDeclarationSingleStaticImport():
            return hydra.serialization.cst("STUB:ImportDeclarationSingleStaticImport")

        case hydra.java.syntax.ImportDeclarationStaticImportOnDemand():
            return hydra.serialization.cst("STUB:ImportDeclarationStaticImportOnDemand")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_package_modifier(pm: hydra.java.syntax.PackageModifier) -> hydra.ast.Expr:
    return write_annotation(pm.value)

def write_package_declaration(pd: hydra.java.syntax.PackageDeclaration) -> hydra.ast.Expr:
    mods = pd.modifiers
    ids = pd.identifiers
    return hydra.serialization.with_semi(hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.logic.if_else(hydra.lib.lists.null(mods), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: write_package_modifier(x1)), mods))))), Just(hydra.serialization.space_sep((hydra.serialization.cst("package"), hydra.serialization.cst(hydra.lib.strings.intercalate(".", hydra.lib.lists.map((lambda id: id.value), ids))))))))))

def write_type_declaration(d: hydra.java.syntax.TypeDeclaration) -> hydra.ast.Expr:
    match d:
        case hydra.java.syntax.TypeDeclarationClass(value=d2):
            return write_class_declaration(d2)

        case hydra.java.syntax.TypeDeclarationInterface(value=d22):
            return write_interface_declaration(d22)

        case hydra.java.syntax.TypeDeclarationNone():
            return hydra.serialization.cst(";")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_type_declaration_with_comments(tdwc: hydra.java.syntax.TypeDeclarationWithComments) -> hydra.ast.Expr:
    d = tdwc.value
    mc = tdwc.comments
    return with_comments(mc, write_type_declaration(d))

def write_compilation_unit(u: hydra.java.syntax.CompilationUnit) -> hydra.ast.Expr:
    match u:
        case hydra.java.syntax.CompilationUnitOrdinary(value=ocu):
            mpkg = ocu.package
            imports = ocu.imports
            types = ocu.types
            @lru_cache(1)
            def warning() -> Maybe[hydra.ast.Expr]:
                return Just(single_line_comment(hydra.constants.warning_auto_generated_file))
            @lru_cache(1)
            def pkg_sec() -> Maybe[hydra.ast.Expr]:
                return hydra.lib.maybes.map((lambda x1: write_package_declaration(x1)), mpkg)
            @lru_cache(1)
            def imports_sec() -> Maybe[hydra.ast.Expr]:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(imports), (lambda : Nothing()), (lambda : Just(hydra.serialization.newline_sep(hydra.lib.lists.map((lambda x1: write_import_declaration(x1)), imports)))))
            @lru_cache(1)
            def types_sec() -> Maybe[hydra.ast.Expr]:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(types), (lambda : Nothing()), (lambda : Just(hydra.serialization.double_newline_sep(hydra.lib.lists.map((lambda x1: write_type_declaration_with_comments(x1)), types)))))
            return hydra.serialization.double_newline_sep(hydra.lib.maybes.cat((warning(), pkg_sec(), imports_sec(), types_sec())))

        case _:
            raise TypeError("Unsupported CompilationUnit")

def write_receiver_parameter(_: T0) -> hydra.ast.Expr:
    return hydra.serialization.cst("STUB:ReceiverParameter")
