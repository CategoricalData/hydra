# Note: this is an automatically generated file. Do not edit.

r"""Rust serializer: converts Rust AST to concrete syntax."""

from __future__ import annotations
from functools import lru_cache
from hydra.dsl.python import Just, Maybe, Nothing, frozenlist
from typing import cast
import hydra.core
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maybes
import hydra.lib.strings
import hydra.rust.syntax
import hydra.serialization

def binary_op_to_expr(op: hydra.rust.syntax.BinaryOp):
    def _hoist_hydra_rust_serde_binary_op_to_expr_1(v1):
        match v1:
            case hydra.rust.syntax.BinaryOp.ADD:
                return "+"

            case hydra.rust.syntax.BinaryOp.SUB:
                return "-"

            case hydra.rust.syntax.BinaryOp.MUL:
                return "*"

            case hydra.rust.syntax.BinaryOp.DIV:
                return "/"

            case hydra.rust.syntax.BinaryOp.REM:
                return "%"

            case hydra.rust.syntax.BinaryOp.AND:
                return "&&"

            case hydra.rust.syntax.BinaryOp.OR:
                return "||"

            case hydra.rust.syntax.BinaryOp.BIT_AND:
                return "&"

            case hydra.rust.syntax.BinaryOp.BIT_OR:
                return "|"

            case hydra.rust.syntax.BinaryOp.BIT_XOR:
                return "^"

            case hydra.rust.syntax.BinaryOp.SHL:
                return "<<"

            case hydra.rust.syntax.BinaryOp.SHR:
                return ">>"

            case hydra.rust.syntax.BinaryOp.EQ:
                return "=="

            case hydra.rust.syntax.BinaryOp.NE:
                return "!="

            case hydra.rust.syntax.BinaryOp.LT:
                return "<"

            case hydra.rust.syntax.BinaryOp.LE:
                return "<="

            case hydra.rust.syntax.BinaryOp.GT:
                return ">"

            case hydra.rust.syntax.BinaryOp.GE:
                return ">="

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.serialization.cst(_hoist_hydra_rust_serde_binary_op_to_expr_1(op))

def macro_invocation_to_expr(m: hydra.rust.syntax.MacroInvocation) -> hydra.ast.Expr:
    r"""Serialize a macro invocation."""

    path = m.path
    delim = m.delimiter
    tokens = m.tokens
    @lru_cache(1)
    def path_str() -> str:
        return hydra.lib.strings.intercalate("::", path)
    open = (lambda _: "(")(delim) if delim else (lambda _: "[")(delim) if delim else (lambda _: "{")(delim) if delim else hydra.dsl.python.unsupported("no matching case in inline union elimination")
    close = (lambda _: ")")(delim) if delim else (lambda _: "]")(delim) if delim else (lambda _: "}")(delim) if delim else hydra.dsl.python.unsupported("no matching case in inline union elimination")
    return hydra.serialization.cst(hydra.lib.strings.cat((path_str(), "!", open, tokens, close)))

def derives_to_expr(derives: frozenlist[str]) -> Maybe[hydra.ast.Expr]:
    r"""Serialize derive macros to an attribute expression."""

    return hydra.lib.logic.if_else(hydra.lib.lists.null(derives), (lambda : Nothing()), (lambda : Just(hydra.serialization.cst(hydra.lib.strings.cat(("#[derive(", hydra.lib.strings.intercalate(", ", derives), ")]"))))))

def to_rust_doc_comment(c: str) -> str:
    r"""Convert a string to Rust doc comments."""

    return hydra.lib.strings.intercalate("\n", hydra.lib.lists.map((lambda s: hydra.lib.strings.cat2("/// ", s)), hydra.lib.strings.lines(c)))

def float_literal_to_expr(fl: hydra.rust.syntax.FloatLiteral) -> hydra.ast.Expr:
    r"""Serialize a float literal."""

    val = fl.value
    suf = fl.suffix
    @lru_cache(1)
    def val_str() -> str:
        return hydra.lib.literals.show_float64(val)
    @lru_cache(1)
    def suf_str() -> str:
        return hydra.lib.maybes.maybe((lambda : ""), (lambda s: s), suf)
    return hydra.serialization.cst(hydra.lib.strings.cat2(val_str(), suf_str()))

def integer_literal_to_expr(il: hydra.rust.syntax.IntegerLiteral) -> hydra.ast.Expr:
    r"""Serialize an integer literal."""

    val = il.value
    suf = il.suffix
    @lru_cache(1)
    def val_str() -> str:
        return hydra.lib.literals.show_bigint(val)
    @lru_cache(1)
    def suf_str() -> str:
        return hydra.lib.maybes.maybe((lambda : ""), (lambda s: s), suf)
    return hydra.serialization.cst(hydra.lib.strings.cat2(val_str(), suf_str()))

def literal_to_expr(lit: hydra.rust.syntax.Literal) -> hydra.ast.Expr:
    r"""Serialize a literal."""

    match lit:
        case hydra.rust.syntax.LiteralInteger(value=il):
            return integer_literal_to_expr(il)

        case hydra.rust.syntax.LiteralFloat(value=fl):
            return float_literal_to_expr(fl)

        case hydra.rust.syntax.LiteralString(value=s):
            return hydra.serialization.cst(hydra.lib.literals.show_string(s))

        case hydra.rust.syntax.LiteralRawString(value=s2):
            return hydra.serialization.cst(hydra.lib.strings.cat(("r\"", s2, "\"")))

        case hydra.rust.syntax.LiteralByteString():
            return hydra.serialization.cst("b\"...\"")

        case hydra.rust.syntax.LiteralChar(value=c):
            return hydra.serialization.cst(hydra.lib.strings.cat(("'", hydra.lib.literals.show_uint32(c), "'")))

        case hydra.rust.syntax.LiteralByte(value=b):
            return hydra.serialization.cst(hydra.lib.strings.cat(("b'", hydra.lib.literals.show_uint8(b), "'")))

        case hydra.rust.syntax.LiteralBool(value=b2):
            return hydra.serialization.cst(hydra.lib.logic.if_else(b2, (lambda : "true"), (lambda : "false")))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def use_tree_to_expr(tree: hydra.rust.syntax.UseTree) -> hydra.ast.Expr:
    r"""Serialize a use tree."""

    match tree:
        case hydra.rust.syntax.UseTreePath(value=p):
            return hydra.serialization.cst(hydra.lib.strings.intercalate("::", p.segments))

        case hydra.rust.syntax.UseTreeRename(value=r):
            path = r.path
            alias = r.alias
            return hydra.serialization.space_sep((hydra.serialization.cst(hydra.lib.strings.intercalate("::", path)), hydra.serialization.cst("as"), hydra.serialization.cst(alias)))

        case hydra.rust.syntax.UseTreeGlob(value=segs):
            return hydra.serialization.cst(hydra.lib.strings.cat2(hydra.lib.strings.intercalate("::", segs), "::*"))

        case hydra.rust.syntax.UseTreeGroup(value=g):
            prefix = g.prefix
            trees = g.trees
            @lru_cache(1)
            def prefix_str() -> str:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(prefix), (lambda : ""), (lambda : hydra.lib.strings.cat2(hydra.lib.strings.intercalate("::", prefix), "::")))
            return hydra.serialization.cst(hydra.lib.strings.cat((prefix_str(), "{", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map((lambda t: hydra.serialization.print_expr(use_tree_to_expr(t))), trees)), "}")))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def use_declaration_to_expr(use: hydra.rust.syntax.UseDeclaration) -> hydra.ast.Expr:
    r"""Serialize a use declaration."""

    pub = use.public
    tree = use.tree
    @lru_cache(1)
    def pub_kw() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(pub, (lambda : Just(hydra.serialization.cst("pub"))), (lambda : Nothing()))
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((pub_kw(), Just(hydra.serialization.cst("use")), Just(use_tree_to_expr(tree)), Just(hydra.serialization.cst(";")))))

def array_expr_to_expr(a: hydra.rust.syntax.ArrayExpr) -> hydra.ast.Expr:
    r"""Serialize an array expression."""

    match a:
        case hydra.rust.syntax.ArrayExprElements(value=es):
            return hydra.serialization.bracket_list(hydra.serialization.half_block_style, hydra.lib.lists.map((lambda x1: expression_to_expr(x1)), es))

        case hydra.rust.syntax.ArrayExprRepeat(value=r):
            elem = r.element
            len = r.length
            return hydra.serialization.cst(hydra.lib.strings.cat(("[", hydra.serialization.print_expr(expression_to_expr(elem)), "; ", hydra.serialization.print_expr(expression_to_expr(len)), "]")))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def assign_expr_to_expr(a: hydra.rust.syntax.AssignExpr) -> hydra.ast.Expr:
    r"""Serialize an assignment expression."""

    target = a.target
    val = a.value
    return hydra.serialization.space_sep((expression_to_expr(target), hydra.serialization.cst("="), expression_to_expr(val)))

def binary_expr_to_expr(b: hydra.rust.syntax.BinaryExpr) -> hydra.ast.Expr:
    r"""Serialize a binary expression."""

    left = b.left
    op = b.op
    right = b.right
    return hydra.serialization.space_sep((expression_to_expr(left), binary_op_to_expr(op), expression_to_expr(right)))

def block_to_expr(b: hydra.rust.syntax.Block) -> hydra.ast.Expr:
    r"""Serialize a block."""

    stmts = b.statements
    expr = b.expression
    @lru_cache(1)
    def stmt_exprs() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda x1: statement_to_expr(x1)), stmts)
    @lru_cache(1)
    def expr_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda e: (expression_to_expr(e),)), expr)
    @lru_cache(1)
    def all_parts() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.concat2(stmt_exprs(), expr_part())
    return hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.half_block_style, all_parts())

def call_expr_to_expr(c: hydra.rust.syntax.CallExpr) -> hydra.ast.Expr:
    r"""Serialize a function call expression."""

    func = c.function
    args = c.args
    return hydra.serialization.space_sep((expression_to_expr(func), hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: expression_to_expr(x1)), args))))

def cast_expr_to_expr(c: hydra.rust.syntax.CastExpr) -> hydra.ast.Expr:
    r"""Serialize a cast expression."""

    expr = c.expr
    typ = c.type
    return hydra.serialization.space_sep((expression_to_expr(expr), hydra.serialization.cst("as"), type_to_expr(typ)))

def closure_expr_to_expr(c: hydra.rust.syntax.ClosureExpr) -> hydra.ast.Expr:
    r"""Serialize a closure expression."""

    move = c.move
    params = c.params
    ret_type = c.return_type
    body = c.body
    @lru_cache(1)
    def move_kw() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(move, (lambda : Just(hydra.serialization.cst("move"))), (lambda : Nothing()))
    @lru_cache(1)
    def params_str() -> str:
        return hydra.lib.strings.cat(("|", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map((lambda x1: closure_param_to_str(x1)), params)), "|"))
    @lru_cache(1)
    def ret_part() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda t: Just(hydra.serialization.space_sep((hydra.serialization.cst("->"), type_to_expr(t))))), ret_type)
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((move_kw(), Just(hydra.serialization.cst(params_str())), ret_part(), Just(expression_to_expr(body)))))

def closure_param_to_str(cp: hydra.rust.syntax.ClosureParam) -> str:
    r"""Serialize a closure parameter to string."""

    pat = cp.pattern
    typ = cp.type
    @lru_cache(1)
    def pat_str() -> str:
        return hydra.serialization.print_expr(pattern_to_expr(pat))
    return hydra.lib.maybes.maybe((lambda : pat_str()), (lambda t: hydra.lib.strings.cat((pat_str(), ": ", hydra.serialization.print_expr(type_to_expr(t))))), typ)

def compound_assign_expr_to_expr(c: hydra.rust.syntax.CompoundAssignExpr) -> hydra.ast.Expr:
    r"""Serialize a compound assignment expression."""

    target = c.target
    op = c.op
    val = c.value
    op_str = (lambda _: "+=")(op) if op else (lambda _: "-=")(op) if op else (lambda _: "*=")(op) if op else (lambda _: "/=")(op) if op else (lambda _: "%=")(op) if op else (lambda _: "&=")(op) if op else (lambda _: "|=")(op) if op else (lambda _: "^=")(op) if op else (lambda _: "<<=")(op) if op else (lambda _: ">>=")(op) if op else hydra.dsl.python.unsupported("no matching case in inline union elimination")
    return hydra.serialization.space_sep((expression_to_expr(target), hydra.serialization.cst(op_str), expression_to_expr(val)))

def const_def_to_expr(c: hydra.rust.syntax.ConstDef) -> hydra.ast.Expr:
    r"""Serialize a const definition."""

    name = c.name
    typ = c.type
    val = c.value
    return hydra.serialization.space_sep((hydra.serialization.cst("const"), hydra.serialization.cst(hydra.lib.strings.cat2(name, ":")), type_to_expr(typ), hydra.serialization.cst("="), expression_to_expr(val), hydra.serialization.cst(";")))

def enum_def_to_expr(e: hydra.rust.syntax.EnumDef) -> hydra.ast.Expr:
    r"""Serialize an enum definition."""

    name = e.name
    generics = e.generics
    where_c = e.where_clause
    variants = e.variants
    derives = e.derives
    doc_c = e.doc
    @lru_cache(1)
    def derives_attr() -> Maybe[hydra.ast.Expr]:
        return derives_to_expr(derives)
    @lru_cache(1)
    def doc_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda d: (hydra.serialization.cst(to_rust_doc_comment(d)),)), doc_c)
    @lru_cache(1)
    def header() -> hydra.ast.Expr:
        return hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(hydra.serialization.cst("enum")), Just(hydra.serialization.cst(name)), generic_params_to_expr(generics))))
    @lru_cache(1)
    def where_part() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda w: Just(where_clause_to_expr(w))), where_c)
    @lru_cache(1)
    def body() -> hydra.ast.Expr:
        return hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.half_block_style, hydra.lib.lists.map((lambda x1: enum_variant_to_expr(x1)), variants))
    return hydra.serialization.newline_sep(hydra.lib.lists.concat((doc_part(), hydra.lib.maybes.maybe((lambda : ()), (lambda d: (d,)), derives_attr()), (hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(header()), where_part(), Just(body())))),))))

def enum_variant_body_to_expr(body: hydra.rust.syntax.EnumVariantBody) -> hydra.ast.Expr:
    r"""Serialize an enum variant body."""

    match body:
        case hydra.rust.syntax.EnumVariantBodyUnit():
            return hydra.serialization.cst("")

        case hydra.rust.syntax.EnumVariantBodyTuple(value=types):
            return hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: type_to_expr(x1)), types))

        case hydra.rust.syntax.EnumVariantBodyStruct(value=fields):
            return hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.half_block_style, hydra.lib.lists.map((lambda x1: struct_field_to_expr(x1)), fields))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def enum_variant_to_expr(v: hydra.rust.syntax.EnumVariant) -> hydra.ast.Expr:
    r"""Serialize an enum variant."""

    name = v.name
    body = v.body
    doc_c = v.doc
    @lru_cache(1)
    def doc_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda d: (hydra.serialization.cst(to_rust_doc_comment(d)),)), doc_c)
    return hydra.serialization.newline_sep(hydra.lib.lists.concat((doc_part(), (hydra.serialization.space_sep((hydra.serialization.cst(name), enum_variant_body_to_expr(body))),))))

def expr_path_to_expr(ep: hydra.rust.syntax.ExprPath) -> hydra.ast.Expr:
    r"""Serialize an expression path."""

    global_ = ep.global_
    segs = ep.segments
    @lru_cache(1)
    def prefix() -> str:
        return hydra.lib.logic.if_else(global_, (lambda : "::"), (lambda : ""))
    @lru_cache(1)
    def seg_strs() -> frozenlist[str]:
        return hydra.lib.lists.map((lambda s: hydra.serialization.print_expr(path_segment_to_expr(s))), segs)
    return hydra.serialization.cst(hydra.lib.strings.cat2(prefix(), hydra.lib.strings.intercalate("::", seg_strs())))

def expression_to_expr(expr: hydra.rust.syntax.Expression) -> hydra.ast.Expr:
    r"""Serialize a Rust expression."""

    match expr:
        case hydra.rust.syntax.ExpressionLiteral(value=l):
            return literal_to_expr(l)

        case hydra.rust.syntax.ExpressionPath(value=p):
            return expr_path_to_expr(p)

        case hydra.rust.syntax.ExpressionBlock(value=b):
            return block_to_expr(b)

        case hydra.rust.syntax.ExpressionCall(value=c):
            return call_expr_to_expr(c)

        case hydra.rust.syntax.ExpressionMethodCall(value=m):
            return method_call_expr_to_expr(m)

        case hydra.rust.syntax.ExpressionFieldAccess(value=f):
            return field_access_expr_to_expr(f)

        case hydra.rust.syntax.ExpressionTupleIndex(value=t):
            return tuple_index_expr_to_expr(t)

        case hydra.rust.syntax.ExpressionClosure(value=c2):
            return closure_expr_to_expr(c2)

        case hydra.rust.syntax.ExpressionIf(value=i):
            return if_expr_to_expr(i)

        case hydra.rust.syntax.ExpressionMatch(value=m2):
            return match_expr_to_expr(m2)

        case hydra.rust.syntax.ExpressionLoop(value=l2):
            return loop_expr_to_expr(l2)

        case hydra.rust.syntax.ExpressionWhile(value=w):
            return while_expr_to_expr(w)

        case hydra.rust.syntax.ExpressionFor(value=f2):
            return for_expr_to_expr(f2)

        case hydra.rust.syntax.ExpressionBinary(value=b2):
            return binary_expr_to_expr(b2)

        case hydra.rust.syntax.ExpressionUnary(value=u):
            return unary_expr_to_expr(u)

        case hydra.rust.syntax.ExpressionReference(value=r):
            return ref_expr_to_expr(r)

        case hydra.rust.syntax.ExpressionDereference(value=d):
            return hydra.serialization.prefix("*", expression_to_expr(d))

        case hydra.rust.syntax.ExpressionStruct(value=s):
            return struct_expr_to_expr(s)

        case hydra.rust.syntax.ExpressionTuple(value=es):
            return hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: expression_to_expr(x1)), es))

        case hydra.rust.syntax.ExpressionArray(value=a):
            return array_expr_to_expr(a)

        case hydra.rust.syntax.ExpressionIndex(value=i2):
            return index_expr_to_expr(i2)

        case hydra.rust.syntax.ExpressionRange(value=r2):
            return range_expr_to_expr(r2)

        case hydra.rust.syntax.ExpressionReturn(value=mr):
            return hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("return")), (lambda e: hydra.serialization.space_sep((hydra.serialization.cst("return"), expression_to_expr(e)))), mr)

        case hydra.rust.syntax.ExpressionBreak(value=mb):
            return hydra.lib.maybes.maybe((lambda : hydra.serialization.cst("break")), (lambda e: hydra.serialization.space_sep((hydra.serialization.cst("break"), expression_to_expr(e)))), mb)

        case hydra.rust.syntax.ExpressionContinue():
            return hydra.serialization.cst("continue")

        case hydra.rust.syntax.ExpressionTry(value=e):
            return hydra.serialization.cst(hydra.lib.strings.cat2(hydra.serialization.print_expr(expression_to_expr(e)), "?"))

        case hydra.rust.syntax.ExpressionCast(value=c3):
            return cast_expr_to_expr(c3)

        case hydra.rust.syntax.ExpressionTypeAscription(value=t2):
            return type_ascription_expr_to_expr(t2)

        case hydra.rust.syntax.ExpressionAwait(value=e2):
            return hydra.serialization.cst(hydra.lib.strings.cat2(hydra.serialization.print_expr(expression_to_expr(e2)), ".await"))

        case hydra.rust.syntax.ExpressionAssign(value=a2):
            return assign_expr_to_expr(a2)

        case hydra.rust.syntax.ExpressionCompoundAssign(value=c4):
            return compound_assign_expr_to_expr(c4)

        case hydra.rust.syntax.ExpressionMacro(value=m3):
            return macro_invocation_to_expr(m3)

        case hydra.rust.syntax.ExpressionParen(value=e3):
            return hydra.serialization.parenthesize(expression_to_expr(e3))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def field_access_expr_to_expr(f: hydra.rust.syntax.FieldAccessExpr) -> hydra.ast.Expr:
    r"""Serialize a field access expression."""

    obj = f.object
    field = f.field
    return hydra.serialization.cst(hydra.lib.strings.cat((hydra.serialization.print_expr(expression_to_expr(obj)), ".", field)))

def field_pattern_to_expr(fp: hydra.rust.syntax.FieldPattern) -> hydra.ast.Expr:
    r"""Serialize a field pattern."""

    name = fp.name
    pat = fp.pattern
    return hydra.lib.maybes.maybe((lambda : hydra.serialization.cst(name)), (lambda p: hydra.serialization.space_sep((hydra.serialization.cst(hydra.lib.strings.cat2(name, ":")), pattern_to_expr(p)))), pat)

def field_value_to_expr(fv: hydra.rust.syntax.FieldValue) -> hydra.ast.Expr:
    r"""Serialize a field-value pair."""

    name = fv.name
    val = fv.value
    return hydra.lib.maybes.maybe((lambda : hydra.serialization.cst(name)), (lambda v: hydra.serialization.space_sep((hydra.serialization.cst(hydra.lib.strings.cat2(name, ":")), expression_to_expr(v)))), val)

def fn_def_to_expr(f: hydra.rust.syntax.FnDef) -> hydra.ast.Expr:
    r"""Serialize a function definition."""

    name = f.name
    generics = f.generics
    where_c = f.where_clause
    params = f.params
    ret_type = f.return_type
    body = f.body
    is_async = f.async_
    is_const = f.const
    is_unsafe = f.unsafe
    doc_c = f.doc
    @lru_cache(1)
    def doc_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda d: (hydra.serialization.cst(to_rust_doc_comment(d)),)), doc_c)
    @lru_cache(1)
    def async_kw() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(is_async, (lambda : Just(hydra.serialization.cst("async"))), (lambda : Nothing()))
    @lru_cache(1)
    def const_kw() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(is_const, (lambda : Just(hydra.serialization.cst("const"))), (lambda : Nothing()))
    @lru_cache(1)
    def unsafe_kw() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(is_unsafe, (lambda : Just(hydra.serialization.cst("unsafe"))), (lambda : Nothing()))
    @lru_cache(1)
    def fn_kw() -> hydra.ast.Expr:
        return hydra.serialization.cst("fn")
    @lru_cache(1)
    def name_expr() -> hydra.ast.Expr:
        return hydra.serialization.cst(name)
    @lru_cache(1)
    def generics_expr() -> Maybe[hydra.ast.Expr]:
        return generic_params_to_expr(generics)
    @lru_cache(1)
    def params_expr() -> hydra.ast.Expr:
        return hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: fn_param_to_expr(x1)), params))
    @lru_cache(1)
    def ret_type_expr() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda t: Just(hydra.serialization.space_sep((hydra.serialization.cst("->"), type_to_expr(t))))), ret_type)
    @lru_cache(1)
    def where_expr() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda w: Just(where_clause_to_expr(w))), where_c)
    @lru_cache(1)
    def header() -> hydra.ast.Expr:
        return hydra.serialization.space_sep(hydra.lib.maybes.cat((async_kw(), const_kw(), unsafe_kw(), Just(fn_kw()), Just(name_expr()), generics_expr(), Just(params_expr()), ret_type_expr(), where_expr())))
    return hydra.serialization.newline_sep(hydra.lib.lists.concat((doc_part(), (hydra.serialization.space_sep((header(), block_to_expr(body))),))))

def fn_param_to_expr(param: hydra.rust.syntax.FnParam) -> hydra.ast.Expr:
    r"""Serialize a function parameter."""

    pat = param.pattern
    typ = param.type
    return hydra.serialization.space_sep((hydra.serialization.cst(hydra.lib.strings.cat2(hydra.serialization.print_expr(pattern_to_expr(pat)), ":")), type_to_expr(typ)))

def for_expr_to_expr(f: hydra.rust.syntax.ForExpr) -> hydra.ast.Expr:
    r"""Serialize a for expression."""

    label = f.label
    pat = f.pattern
    iter = f.iter
    body = f.body
    @lru_cache(1)
    def label_part() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda lbl: Just(hydra.serialization.cst(hydra.lib.strings.cat2("'", hydra.lib.strings.cat2(lbl, ":"))))), label)
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((label_part(), Just(hydra.serialization.cst("for")), Just(pattern_to_expr(pat)), Just(hydra.serialization.cst("in")), Just(expression_to_expr(iter)), Just(block_to_expr(body)))))

def generic_arg_to_expr(arg: hydra.rust.syntax.GenericArg) -> hydra.ast.Expr:
    r"""Serialize a generic argument."""

    match arg:
        case hydra.rust.syntax.GenericArgType(value=t):
            return type_to_expr(t)

        case hydra.rust.syntax.GenericArgLifetime(value=lt):
            return hydra.serialization.cst(hydra.lib.strings.cat2("'", lt.name))

        case hydra.rust.syntax.GenericArgConst(value=e):
            return expression_to_expr(e)

        case hydra.rust.syntax.GenericArgBinding(value=tb):
            name = tb.name
            typ = tb.type
            return hydra.serialization.space_sep((hydra.serialization.cst(name), hydra.serialization.cst("="), type_to_expr(typ)))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def generic_arguments_to_expr(args: hydra.rust.syntax.GenericArguments) -> Maybe[hydra.ast.Expr]:
    r"""Serialize generic arguments."""

    match args:
        case hydra.rust.syntax.GenericArgumentsNone():
            return Nothing()

        case hydra.rust.syntax.GenericArgumentsAngleBracketed(value=ab):
            args = ab.args
            return Just(hydra.serialization.angle_braces_list(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: generic_arg_to_expr(x1)), args)))

        case hydra.rust.syntax.GenericArgumentsParenthesized(value=pa):
            inputs = pa.inputs
            output = pa.output
            @lru_cache(1)
            def input_part() -> hydra.ast.Expr:
                return hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: type_to_expr(x1)), inputs))
            @lru_cache(1)
            def output_part() -> Maybe[hydra.ast.Expr]:
                return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda t: Just(hydra.serialization.space_sep((hydra.serialization.cst("->"), type_to_expr(t))))), output)
            return Just(hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(input_part()), output_part()))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def generic_param_to_expr(gp: hydra.rust.syntax.GenericParam) -> hydra.ast.Expr:
    r"""Serialize a generic parameter."""

    name = gp.name
    bounds = gp.bounds
    return hydra.lib.logic.if_else(hydra.lib.lists.null(bounds), (lambda : hydra.serialization.cst(name)), (lambda : hydra.serialization.space_sep((hydra.serialization.cst(hydra.lib.strings.cat2(name, ":")), hydra.serialization.cst(hydra.lib.strings.intercalate(" + ", hydra.lib.lists.map((lambda b: hydra.serialization.print_expr(type_param_bound_to_expr(b))), bounds)))))))

def generic_params_to_expr(gps: frozenlist[hydra.rust.syntax.GenericParam]) -> Maybe[hydra.ast.Expr]:
    r"""Serialize a list of generic parameters."""

    return hydra.lib.logic.if_else(hydra.lib.lists.null(gps), (lambda : Nothing()), (lambda : Just(hydra.serialization.angle_braces_list(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: generic_param_to_expr(x1)), gps)))))

def identifier_pattern_to_expr(ip: hydra.rust.syntax.IdentifierPattern) -> hydra.ast.Expr:
    r"""Serialize an identifier pattern."""

    name = ip.name
    mut = ip.mutable
    at_pat = ip.at_pattern
    @lru_cache(1)
    def mut_kw() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(mut, (lambda : Just(hydra.serialization.cst("mut"))), (lambda : Nothing()))
    @lru_cache(1)
    def at_part() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda p: Just(hydra.serialization.space_sep((hydra.serialization.cst("@"), pattern_to_expr(p))))), at_pat)
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((mut_kw(), Just(hydra.serialization.cst(name)), at_part())))

def if_expr_to_expr(i: hydra.rust.syntax.IfExpr) -> hydra.ast.Expr:
    r"""Serialize an if expression."""

    cond = i.condition
    then_b = i.then_block
    else_b = i.else_branch
    @lru_cache(1)
    def cond_expr() -> hydra.ast.Expr:
        match cond:
            case hydra.rust.syntax.IfConditionBool(value=e):
                return expression_to_expr(e)

            case hydra.rust.syntax.IfConditionLet(value=lc):
                pat = lc.pattern
                expr = lc.expr
                return hydra.serialization.space_sep((hydra.serialization.cst("let"), pattern_to_expr(pat), hydra.serialization.cst("="), expression_to_expr(expr)))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    @lru_cache(1)
    def else_part() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda e: Just(hydra.serialization.space_sep((hydra.serialization.cst("else"), expression_to_expr(e))))), else_b)
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(hydra.serialization.cst("if")), Just(cond_expr()), Just(block_to_expr(then_b)), else_part())))

def impl_block_to_expr(i: hydra.rust.syntax.ImplBlock) -> hydra.ast.Expr:
    r"""Serialize an impl block."""

    generics = i.generics
    where_c = i.where_clause
    trait = i.trait
    self_type = i.self_type
    items = i.items
    @lru_cache(1)
    def generics_expr() -> Maybe[hydra.ast.Expr]:
        return generic_params_to_expr(generics)
    @lru_cache(1)
    def trait_part() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda t: Just(hydra.serialization.space_sep((type_path_to_expr(t), hydra.serialization.cst("for"))))), trait)
    @lru_cache(1)
    def where_part() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda w: Just(where_clause_to_expr(w))), where_c)
    @lru_cache(1)
    def header() -> hydra.ast.Expr:
        return hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(hydra.serialization.cst("impl")), generics_expr(), trait_part(), Just(type_to_expr(self_type)), where_part())))
    @lru_cache(1)
    def body() -> hydra.ast.Expr:
        return hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.half_block_style, hydra.lib.lists.map((lambda x1: impl_item_to_expr(x1)), items))
    return hydra.serialization.space_sep((header(), body()))

def impl_item_to_expr(item: hydra.rust.syntax.ImplItem) -> hydra.ast.Expr:
    r"""Serialize an impl item."""

    match item:
        case hydra.rust.syntax.ImplItemMethod(value=m):
            return impl_method_to_expr(m)

        case hydra.rust.syntax.ImplItemType(value=t):
            return type_alias_to_expr(t)

        case hydra.rust.syntax.ImplItemConst(value=c):
            return const_def_to_expr(c)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def impl_method_to_expr(m: hydra.rust.syntax.ImplMethod) -> hydra.ast.Expr:
    r"""Serialize an impl method."""

    name = m.name
    generics = m.generics
    where_c = m.where_clause
    params = m.params
    ret_type = m.return_type
    body = m.body
    pub = m.public
    doc_c = m.doc
    @lru_cache(1)
    def doc_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda d: (hydra.serialization.cst(to_rust_doc_comment(d)),)), doc_c)
    @lru_cache(1)
    def pub_kw() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(pub, (lambda : Just(hydra.serialization.cst("pub"))), (lambda : Nothing()))
    @lru_cache(1)
    def generics_expr() -> Maybe[hydra.ast.Expr]:
        return generic_params_to_expr(generics)
    @lru_cache(1)
    def params_expr() -> hydra.ast.Expr:
        return hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: method_param_to_expr(x1)), params))
    @lru_cache(1)
    def ret_type_expr() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda t: Just(hydra.serialization.space_sep((hydra.serialization.cst("->"), type_to_expr(t))))), ret_type)
    @lru_cache(1)
    def where_expr() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda w: Just(where_clause_to_expr(w))), where_c)
    @lru_cache(1)
    def header() -> hydra.ast.Expr:
        return hydra.serialization.space_sep(hydra.lib.maybes.cat((pub_kw(), Just(hydra.serialization.cst("fn")), Just(hydra.serialization.cst(name)), generics_expr(), Just(params_expr()), ret_type_expr(), where_expr())))
    return hydra.serialization.newline_sep(hydra.lib.lists.concat((doc_part(), (hydra.serialization.space_sep((header(), block_to_expr(body))),))))

def index_expr_to_expr(i: hydra.rust.syntax.IndexExpr) -> hydra.ast.Expr:
    r"""Serialize an index expression."""

    obj = i.object
    idx = i.index
    return hydra.serialization.cst(hydra.lib.strings.cat((hydra.serialization.print_expr(expression_to_expr(obj)), "[", hydra.serialization.print_expr(expression_to_expr(idx)), "]")))

def item_to_expr(item: hydra.rust.syntax.Item) -> hydra.ast.Expr:
    r"""Serialize a Rust item to an AST expression."""

    match item:
        case hydra.rust.syntax.ItemUse(value=u):
            return use_declaration_to_expr(u)

        case hydra.rust.syntax.ItemStruct(value=s):
            return struct_def_to_expr(s)

        case hydra.rust.syntax.ItemEnum(value=e):
            return enum_def_to_expr(e)

        case hydra.rust.syntax.ItemFn(value=f):
            return fn_def_to_expr(f)

        case hydra.rust.syntax.ItemTypeAlias(value=t):
            return type_alias_to_expr(t)

        case hydra.rust.syntax.ItemImpl(value=i):
            return impl_block_to_expr(i)

        case hydra.rust.syntax.ItemTrait(value=t2):
            return trait_def_to_expr(t2)

        case hydra.rust.syntax.ItemMod(value=m):
            return mod_def_to_expr(m)

        case hydra.rust.syntax.ItemConst(value=c):
            return const_def_to_expr(c)

        case hydra.rust.syntax.ItemStatic(value=s2):
            return static_def_to_expr(s2)

        case hydra.rust.syntax.ItemMacro(value=m2):
            return macro_invocation_to_expr(m2)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def let_statement_to_expr(l: hydra.rust.syntax.LetStatement) -> hydra.ast.Expr:
    r"""Serialize a let statement."""

    pat = l.pattern
    mut = l.mutable
    typ = l.type
    init = l.init
    @lru_cache(1)
    def mut_kw() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(mut, (lambda : Just(hydra.serialization.cst("mut"))), (lambda : Nothing()))
    @lru_cache(1)
    def typ_part() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda t: Just(hydra.serialization.space_sep((hydra.serialization.cst(":"), type_to_expr(t))))), typ)
    @lru_cache(1)
    def init_part() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda e: Just(hydra.serialization.space_sep((hydra.serialization.cst("="), expression_to_expr(e))))), init)
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(hydra.serialization.cst("let")), mut_kw(), Just(pattern_to_expr(pat)), typ_part(), init_part(), Just(hydra.serialization.cst(";")))))

def loop_expr_to_expr(l: hydra.rust.syntax.LoopExpr) -> hydra.ast.Expr:
    r"""Serialize a loop expression."""

    label = l.label
    body = l.body
    @lru_cache(1)
    def label_part() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda lbl: Just(hydra.serialization.cst(hydra.lib.strings.cat2("'", hydra.lib.strings.cat2(lbl, ":"))))), label)
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((label_part(), Just(hydra.serialization.cst("loop")), Just(block_to_expr(body)))))

def match_arm_to_expr(arm: hydra.rust.syntax.MatchArm) -> hydra.ast.Expr:
    r"""Serialize a match arm."""

    pat = arm.pattern
    guard = arm.guard
    body = arm.body
    @lru_cache(1)
    def guard_part() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda g: Just(hydra.serialization.space_sep((hydra.serialization.cst("if"), expression_to_expr(g))))), guard)
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(pattern_to_expr(pat)), guard_part(), Just(hydra.serialization.cst("=>")), Just(expression_to_expr(body)), Just(hydra.serialization.cst(",")))))

def match_expr_to_expr(m: hydra.rust.syntax.MatchExpr) -> hydra.ast.Expr:
    r"""Serialize a match expression."""

    scrut = m.scrutinee
    arms = m.arms
    return hydra.serialization.space_sep((hydra.serialization.cst("match"), expression_to_expr(scrut), hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.half_block_style, hydra.lib.lists.map((lambda x1: match_arm_to_expr(x1)), arms))))

def method_call_expr_to_expr(m: hydra.rust.syntax.MethodCallExpr) -> hydra.ast.Expr:
    r"""Serialize a method call expression."""

    recv = m.receiver
    method = m.method
    turbo = m.turbofish
    args = m.args
    @lru_cache(1)
    def turbo_part() -> str:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(turbo), (lambda : ""), (lambda : hydra.lib.strings.cat(("::<", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map((lambda t: hydra.serialization.print_expr(type_to_expr(t))), turbo)), ">"))))
    return hydra.serialization.cst(hydra.lib.strings.cat((hydra.serialization.print_expr(expression_to_expr(recv)), ".", method, turbo_part(), "(", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map((lambda a: hydra.serialization.print_expr(expression_to_expr(a))), args)), ")")))

def method_param_to_expr(param: hydra.rust.syntax.MethodParam):
    def _hoist_hydra_rust_serde_method_param_to_expr_1(v1):
        match v1:
            case hydra.rust.syntax.SelfParam.OWNED:
                return hydra.serialization.cst("self")

            case hydra.rust.syntax.SelfParam.REF:
                return hydra.serialization.cst("&self")

            case hydra.rust.syntax.SelfParam.REF_MUT:
                return hydra.serialization.cst("&mut self")

            case _:
                raise AssertionError("Unreachable: all variants handled")
    match param:
        case hydra.rust.syntax.MethodParamSelf(value=sp):
            return _hoist_hydra_rust_serde_method_param_to_expr_1(sp)

        case hydra.rust.syntax.MethodParamRegular(value=fp):
            return fn_param_to_expr(fp)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def mod_def_to_expr(m: hydra.rust.syntax.ModDef) -> hydra.ast.Expr:
    r"""Serialize a module definition."""

    name = m.name
    body = m.body
    return hydra.lib.maybes.maybe((lambda : hydra.serialization.space_sep((hydra.serialization.cst("mod"), hydra.serialization.cst(name), hydra.serialization.cst(";")))), (lambda items: hydra.serialization.space_sep((hydra.serialization.cst("mod"), hydra.serialization.cst(name), hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.half_block_style, hydra.lib.lists.map((lambda x1: item_to_expr(x1)), items))))), body)

def path_segment_to_expr(seg: hydra.rust.syntax.PathSegment) -> hydra.ast.Expr:
    r"""Serialize a path segment."""

    name = seg.name
    args = seg.arguments
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(hydra.serialization.cst(name)), generic_arguments_to_expr(args))))

def pattern_to_expr(pat: hydra.rust.syntax.Pattern) -> hydra.ast.Expr:
    r"""Serialize a pattern."""

    match pat:
        case hydra.rust.syntax.PatternWildcard():
            return hydra.serialization.cst("_")

        case hydra.rust.syntax.PatternIdentifier(value=ip):
            return identifier_pattern_to_expr(ip)

        case hydra.rust.syntax.PatternLiteral(value=l):
            return literal_to_expr(l)

        case hydra.rust.syntax.PatternReference(value=rp):
            return ref_pattern_to_expr(rp)

        case hydra.rust.syntax.PatternStruct(value=sp):
            return struct_pattern_to_expr(sp)

        case hydra.rust.syntax.PatternTupleStruct(value=tsp):
            return tuple_struct_pattern_to_expr(tsp)

        case hydra.rust.syntax.PatternTuple(value=ps):
            return hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: pattern_to_expr(x1)), ps))

        case hydra.rust.syntax.PatternSlice(value=ps2):
            return hydra.serialization.bracket_list(hydra.serialization.half_block_style, hydra.lib.lists.map((lambda x1: pattern_to_expr(x1)), ps2))

        case hydra.rust.syntax.PatternOr(value=ps3):
            return hydra.serialization.cst(hydra.lib.strings.intercalate(" | ", hydra.lib.lists.map((lambda p: hydra.serialization.print_expr(pattern_to_expr(p))), ps3)))

        case hydra.rust.syntax.PatternPath(value=ep):
            return expr_path_to_expr(ep)

        case hydra.rust.syntax.PatternRange(value=rp2):
            return range_pattern_to_expr(rp2)

        case hydra.rust.syntax.PatternRest():
            return hydra.serialization.cst("..")

        case hydra.rust.syntax.PatternParen(value=p):
            return hydra.serialization.parenthesize(pattern_to_expr(p))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def range_expr_to_expr(r: hydra.rust.syntax.RangeExpr) -> hydra.ast.Expr:
    r"""Serialize a range expression."""

    from_ = r.from_
    to = r.to
    incl = r.inclusive
    @lru_cache(1)
    def from_str() -> str:
        return hydra.lib.maybes.maybe((lambda : ""), (lambda f: hydra.serialization.print_expr(expression_to_expr(f))), from_)
    @lru_cache(1)
    def to_str() -> str:
        return hydra.lib.maybes.maybe((lambda : ""), (lambda t: hydra.serialization.print_expr(expression_to_expr(t))), to)
    @lru_cache(1)
    def op() -> str:
        return hydra.lib.logic.if_else(incl, (lambda : "..="), (lambda : ".."))
    return hydra.serialization.cst(hydra.lib.strings.cat((from_str(), op(), to_str())))

def range_pattern_to_expr(rp: hydra.rust.syntax.RangePattern) -> hydra.ast.Expr:
    r"""Serialize a range pattern."""

    from_ = rp.from_
    to = rp.to
    incl = rp.inclusive
    @lru_cache(1)
    def from_str() -> str:
        return hydra.lib.maybes.maybe((lambda : ""), (lambda p: hydra.serialization.print_expr(pattern_to_expr(p))), from_)
    @lru_cache(1)
    def to_str() -> str:
        return hydra.lib.maybes.maybe((lambda : ""), (lambda p: hydra.serialization.print_expr(pattern_to_expr(p))), to)
    @lru_cache(1)
    def op() -> str:
        return hydra.lib.logic.if_else(incl, (lambda : "..="), (lambda : ".."))
    return hydra.serialization.cst(hydra.lib.strings.cat((from_str(), op(), to_str())))

def ref_expr_to_expr(r: hydra.rust.syntax.RefExpr) -> hydra.ast.Expr:
    r"""Serialize a reference expression."""

    mut = r.mutable
    expr = r.expr
    @lru_cache(1)
    def prefix() -> str:
        return hydra.lib.logic.if_else(mut, (lambda : "&mut "), (lambda : "&"))
    return hydra.serialization.cst(hydra.lib.strings.cat2(prefix(), hydra.serialization.print_expr(expression_to_expr(expr))))

def ref_pattern_to_expr(rp: hydra.rust.syntax.RefPattern) -> hydra.ast.Expr:
    r"""Serialize a reference pattern."""

    mut = rp.mutable
    pat = rp.pattern
    @lru_cache(1)
    def prefix() -> str:
        return hydra.lib.logic.if_else(mut, (lambda : "&mut "), (lambda : "&"))
    return hydra.serialization.cst(hydra.lib.strings.cat2(prefix(), hydra.serialization.print_expr(pattern_to_expr(pat))))

def reference_type_to_expr(rt: hydra.rust.syntax.ReferenceType) -> hydra.ast.Expr:
    r"""Serialize a reference type."""

    lt = rt.lifetime
    mut = rt.mutable
    t = rt.type
    @lru_cache(1)
    def lt_part() -> str:
        return hydra.lib.maybes.maybe((lambda : ""), (lambda l: hydra.lib.strings.cat2("'", hydra.lib.strings.cat2(l.name, " "))), lt)
    @lru_cache(1)
    def mut_part() -> str:
        return hydra.lib.logic.if_else(mut, (lambda : "mut "), (lambda : ""))
    return hydra.serialization.cst(hydra.lib.strings.cat(("&", lt_part(), mut_part(), hydra.serialization.print_expr(type_to_expr(t)))))

def statement_to_expr(stmt: hydra.rust.syntax.Statement) -> hydra.ast.Expr:
    r"""Serialize a statement."""

    match stmt:
        case hydra.rust.syntax.StatementLet(value=l):
            return let_statement_to_expr(l)

        case hydra.rust.syntax.StatementExpression(value=e):
            return hydra.serialization.space_sep((expression_to_expr(e), hydra.serialization.cst(";")))

        case hydra.rust.syntax.StatementItem(value=i):
            return item_to_expr(i)

        case hydra.rust.syntax.StatementEmpty():
            return hydra.serialization.cst(";")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def static_def_to_expr(s: hydra.rust.syntax.StaticDef) -> hydra.ast.Expr:
    r"""Serialize a static definition."""

    name = s.name
    typ = s.type
    val = s.value
    mut = s.mutable
    @lru_cache(1)
    def mut_kw() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(mut, (lambda : Just(hydra.serialization.cst("mut"))), (lambda : Nothing()))
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(hydra.serialization.cst("static")), mut_kw(), Just(hydra.serialization.cst(hydra.lib.strings.cat2(name, ":"))), Just(type_to_expr(typ)), Just(hydra.serialization.cst("=")), Just(expression_to_expr(val)), Just(hydra.serialization.cst(";")))))

def struct_body_to_expr(body: hydra.rust.syntax.StructBody) -> hydra.ast.Expr:
    r"""Serialize a struct body."""

    match body:
        case hydra.rust.syntax.StructBodyNamed(value=fields):
            return hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.half_block_style, hydra.lib.lists.map((lambda x1: struct_field_to_expr(x1)), fields))

        case hydra.rust.syntax.StructBodyTuple(value=fields2):
            return hydra.serialization.space_sep((hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda f: type_to_expr(f.type)), fields2)), hydra.serialization.cst(";")))

        case hydra.rust.syntax.StructBodyUnit():
            return hydra.serialization.cst(";")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def struct_def_to_expr(s: hydra.rust.syntax.StructDef) -> hydra.ast.Expr:
    r"""Serialize a struct definition."""

    name = s.name
    generics = s.generics
    where_c = s.where_clause
    body = s.body
    derives = s.derives
    doc_c = s.doc
    @lru_cache(1)
    def derives_attr() -> Maybe[hydra.ast.Expr]:
        return derives_to_expr(derives)
    @lru_cache(1)
    def doc_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda d: (hydra.serialization.cst(to_rust_doc_comment(d)),)), doc_c)
    @lru_cache(1)
    def header() -> hydra.ast.Expr:
        return hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(hydra.serialization.cst("struct")), Just(hydra.serialization.cst(name)), generic_params_to_expr(generics))))
    @lru_cache(1)
    def where_part() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda w: Just(where_clause_to_expr(w))), where_c)
    return hydra.serialization.newline_sep(hydra.lib.lists.concat((doc_part(), hydra.lib.maybes.maybe((lambda : ()), (lambda d: (d,)), derives_attr()), (hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(header()), where_part(), Just(struct_body_to_expr(body))))),))))

def struct_expr_to_expr(s: hydra.rust.syntax.StructExpr) -> hydra.ast.Expr:
    r"""Serialize a struct literal expression."""

    path = s.path
    fields = s.fields
    rest = s.rest
    @lru_cache(1)
    def field_exprs() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda x1: field_value_to_expr(x1)), fields)
    @lru_cache(1)
    def rest_expr() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda r: (hydra.serialization.space_sep((hydra.serialization.cst(".."), expression_to_expr(r))),)), rest)
    @lru_cache(1)
    def all_fields() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.concat2(field_exprs(), rest_expr())
    return hydra.serialization.space_sep((expr_path_to_expr(path), hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.half_block_style, all_fields())))

def struct_field_to_expr(field: hydra.rust.syntax.StructField) -> hydra.ast.Expr:
    r"""Serialize a struct field."""

    name = field.name
    typ = field.type
    pub = field.public
    doc_c = field.doc
    @lru_cache(1)
    def pub_kw() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(pub, (lambda : Just(hydra.serialization.cst("pub"))), (lambda : Nothing()))
    @lru_cache(1)
    def doc_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda d: (hydra.serialization.cst(to_rust_doc_comment(d)),)), doc_c)
    return hydra.serialization.newline_sep(hydra.lib.lists.concat((doc_part(), (hydra.serialization.space_sep(hydra.lib.maybes.cat((pub_kw(), Just(hydra.serialization.cst(hydra.lib.strings.cat2(name, ":"))), Just(type_to_expr(typ))))),))))

def struct_pattern_to_expr(sp: hydra.rust.syntax.StructPattern) -> hydra.ast.Expr:
    r"""Serialize a struct pattern."""

    path = sp.path
    fields = sp.fields
    rest = sp.rest
    @lru_cache(1)
    def field_exprs() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda x1: field_pattern_to_expr(x1)), fields)
    @lru_cache(1)
    def rest_expr() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(rest, (lambda : (hydra.serialization.cst(".."),)), (lambda : ()))
    @lru_cache(1)
    def all_fields() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.concat2(field_exprs(), rest_expr())
    return hydra.serialization.space_sep((expr_path_to_expr(path), hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.half_block_style, all_fields())))

def trait_const_to_expr(c: hydra.rust.syntax.TraitConst) -> hydra.ast.Expr:
    r"""Serialize a trait associated constant."""

    name = c.name
    typ = c.type
    def_ = c.default
    @lru_cache(1)
    def def_part() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda d: Just(hydra.serialization.space_sep((hydra.serialization.cst("="), expression_to_expr(d))))), def_)
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(hydra.serialization.cst("const")), Just(hydra.serialization.cst(hydra.lib.strings.cat2(name, ":"))), Just(type_to_expr(typ)), def_part(), Just(hydra.serialization.cst(";")))))

def trait_def_to_expr(t: hydra.rust.syntax.TraitDef) -> hydra.ast.Expr:
    r"""Serialize a trait definition."""

    name = t.name
    generics = t.generics
    where_c = t.where_clause
    supers = t.super_traits
    items = t.items
    is_unsafe = t.unsafe
    doc_c = t.doc
    @lru_cache(1)
    def doc_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda d: (hydra.serialization.cst(to_rust_doc_comment(d)),)), doc_c)
    @lru_cache(1)
    def unsafe_kw() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(is_unsafe, (lambda : Just(hydra.serialization.cst("unsafe"))), (lambda : Nothing()))
    @lru_cache(1)
    def generics_expr() -> Maybe[hydra.ast.Expr]:
        return generic_params_to_expr(generics)
    @lru_cache(1)
    def super_part() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(supers), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep((hydra.serialization.cst(":"), hydra.serialization.cst(hydra.lib.strings.intercalate(" + ", hydra.lib.lists.map((lambda b: hydra.serialization.print_expr(type_param_bound_to_expr(b))), supers))))))))
    @lru_cache(1)
    def where_part() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda w: Just(where_clause_to_expr(w))), where_c)
    @lru_cache(1)
    def header() -> hydra.ast.Expr:
        return hydra.serialization.space_sep(hydra.lib.maybes.cat((unsafe_kw(), Just(hydra.serialization.cst("trait")), Just(hydra.serialization.cst(name)), generics_expr(), super_part(), where_part())))
    @lru_cache(1)
    def body() -> hydra.ast.Expr:
        return hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.half_block_style, hydra.lib.lists.map((lambda x1: trait_item_to_expr(x1)), items))
    return hydra.serialization.newline_sep(hydra.lib.lists.concat((doc_part(), (hydra.serialization.space_sep((header(), body())),))))

def trait_item_to_expr(item: hydra.rust.syntax.TraitItem) -> hydra.ast.Expr:
    r"""Serialize a trait item."""

    match item:
        case hydra.rust.syntax.TraitItemMethod(value=m):
            return trait_method_to_expr(m)

        case hydra.rust.syntax.TraitItemType(value=t):
            return trait_type_to_expr(t)

        case hydra.rust.syntax.TraitItemConst(value=c):
            return trait_const_to_expr(c)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def trait_method_to_expr(m: hydra.rust.syntax.TraitMethod) -> hydra.ast.Expr:
    r"""Serialize a trait method."""

    name = m.name
    generics = m.generics
    params = m.params
    ret_type = m.return_type
    def_body = m.default_body
    @lru_cache(1)
    def generics_expr() -> Maybe[hydra.ast.Expr]:
        return generic_params_to_expr(generics)
    @lru_cache(1)
    def params_expr() -> hydra.ast.Expr:
        return hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: method_param_to_expr(x1)), params))
    @lru_cache(1)
    def ret_type_expr() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda t: Just(hydra.serialization.space_sep((hydra.serialization.cst("->"), type_to_expr(t))))), ret_type)
    @lru_cache(1)
    def header() -> hydra.ast.Expr:
        return hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(hydra.serialization.cst("fn")), Just(hydra.serialization.cst(name)), generics_expr(), Just(params_expr()), ret_type_expr())))
    return hydra.lib.maybes.maybe((lambda : hydra.serialization.space_sep((header(), hydra.serialization.cst(";")))), (lambda body: hydra.serialization.space_sep((header(), block_to_expr(body)))), def_body)

def trait_type_to_expr(t: hydra.rust.syntax.TraitType) -> hydra.ast.Expr:
    r"""Serialize a trait associated type."""

    name = t.name
    bounds = t.bounds
    def_ = t.default
    @lru_cache(1)
    def bounds_part() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(bounds), (lambda : Nothing()), (lambda : Just(hydra.serialization.space_sep((hydra.serialization.cst(":"), hydra.serialization.cst(hydra.lib.strings.intercalate(" + ", hydra.lib.lists.map((lambda b: hydra.serialization.print_expr(type_param_bound_to_expr(b))), bounds))))))))
    @lru_cache(1)
    def def_part() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda d: Just(hydra.serialization.space_sep((hydra.serialization.cst("="), type_to_expr(d))))), def_)
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(hydra.serialization.cst("type")), Just(hydra.serialization.cst(name)), bounds_part(), def_part(), Just(hydra.serialization.cst(";")))))

def tuple_index_expr_to_expr(t: hydra.rust.syntax.TupleIndexExpr) -> hydra.ast.Expr:
    r"""Serialize a tuple index expression."""

    tuple = t.tuple
    idx = t.index
    return hydra.serialization.cst(hydra.lib.strings.cat((hydra.serialization.print_expr(expression_to_expr(tuple)), ".", hydra.lib.literals.show_int32(idx))))

def tuple_struct_pattern_to_expr(tsp: hydra.rust.syntax.TupleStructPattern) -> hydra.ast.Expr:
    r"""Serialize a tuple struct pattern."""

    path = tsp.path
    elems = tsp.elements
    return hydra.serialization.space_sep((expr_path_to_expr(path), hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: pattern_to_expr(x1)), elems))))

def type_alias_to_expr(ta: hydra.rust.syntax.TypeAlias) -> hydra.ast.Expr:
    r"""Serialize a type alias."""

    name = ta.name
    generics = ta.generics
    typ = ta.type
    doc_c = ta.doc
    @lru_cache(1)
    def doc_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda d: (hydra.serialization.cst(to_rust_doc_comment(d)),)), doc_c)
    return hydra.serialization.newline_sep(hydra.lib.lists.concat((doc_part(), (hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(hydra.serialization.cst("type")), Just(hydra.serialization.cst(name)), generic_params_to_expr(generics), Just(hydra.serialization.cst("=")), Just(type_to_expr(typ)), Just(hydra.serialization.cst(";"))))),))))

def type_ascription_expr_to_expr(t: hydra.rust.syntax.TypeAscriptionExpr) -> hydra.ast.Expr:
    r"""Serialize a type ascription expression."""

    expr = t.expr
    typ = t.type
    return hydra.serialization.space_sep((expression_to_expr(expr), hydra.serialization.cst(":"), type_to_expr(typ)))

def type_param_bound_to_expr(bound: hydra.rust.syntax.TypeParamBound) -> hydra.ast.Expr:
    r"""Serialize a type parameter bound."""

    match bound:
        case hydra.rust.syntax.TypeParamBoundTrait(value=tp):
            return type_path_to_expr(tp)

        case hydra.rust.syntax.TypeParamBoundLifetime(value=lt):
            return hydra.serialization.cst(hydra.lib.strings.cat2("'", lt.name))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def type_path_to_expr(tp: hydra.rust.syntax.TypePath) -> hydra.ast.Expr:
    r"""Serialize a type path."""

    global_ = tp.global_
    segs = tp.segments
    @lru_cache(1)
    def prefix() -> str:
        return hydra.lib.logic.if_else(global_, (lambda : "::"), (lambda : ""))
    @lru_cache(1)
    def seg_strs() -> frozenlist[str]:
        return hydra.lib.lists.map((lambda s: hydra.serialization.print_expr(path_segment_to_expr(s))), segs)
    return hydra.serialization.cst(hydra.lib.strings.cat2(prefix(), hydra.lib.strings.intercalate("::", seg_strs())))

def type_to_expr(typ: hydra.rust.syntax.Type) -> hydra.ast.Expr:
    r"""Serialize a Rust type."""

    match typ:
        case hydra.rust.syntax.TypePath_(value=tp):
            return type_path_to_expr(tp)

        case hydra.rust.syntax.TypeReference(value=rt):
            return reference_type_to_expr(rt)

        case hydra.rust.syntax.TypeSlice(value=t):
            return hydra.serialization.bracket_list(hydra.serialization.inline_style, (type_to_expr(t),))

        case hydra.rust.syntax.TypeArray(value=at):
            elem = at.element
            len = at.length
            return hydra.serialization.cst(hydra.lib.strings.cat(("[", hydra.serialization.print_expr(type_to_expr(elem)), "; ", hydra.serialization.print_expr(expression_to_expr(len)), "]")))

        case hydra.rust.syntax.TypeTuple(value=ts):
            return hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: type_to_expr(x1)), ts))

        case hydra.rust.syntax.TypeFnPointer(value=fp):
            params = fp.params
            ret = fp.return_type
            return hydra.serialization.space_sep((hydra.serialization.cst("fn"), hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: type_to_expr(x1)), params)), hydra.serialization.cst("->"), type_to_expr(ret)))

        case hydra.rust.syntax.TypeImplTrait(value=bounds):
            return hydra.serialization.space_sep((hydra.serialization.cst("impl"), hydra.serialization.cst(hydra.lib.strings.intercalate(" + ", hydra.lib.lists.map((lambda b: hydra.serialization.print_expr(type_param_bound_to_expr(b))), bounds)))))

        case hydra.rust.syntax.TypeDynTrait(value=bounds2):
            return hydra.serialization.space_sep((hydra.serialization.cst("dyn"), hydra.serialization.cst(hydra.lib.strings.intercalate(" + ", hydra.lib.lists.map((lambda b: hydra.serialization.print_expr(type_param_bound_to_expr(b))), bounds2)))))

        case hydra.rust.syntax.TypeInferred():
            return hydra.serialization.cst("_")

        case hydra.rust.syntax.TypeUnit():
            return hydra.serialization.cst("()")

        case hydra.rust.syntax.TypeNever():
            return hydra.serialization.cst("!")

        case hydra.rust.syntax.TypeRawPointer(value=rp):
            mut = rp.mutable
            t = rp.type
            @lru_cache(1)
            def kw() -> str:
                return hydra.lib.logic.if_else(mut, (lambda : "*mut"), (lambda : "*const"))
            return hydra.serialization.space_sep((hydra.serialization.cst(kw()), type_to_expr(t)))

        case hydra.rust.syntax.TypeMacro(value=m):
            return macro_invocation_to_expr(m)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def unary_expr_to_expr(u: hydra.rust.syntax.UnaryExpr) -> hydra.ast.Expr:
    r"""Serialize a unary expression."""

    op = u.op
    operand = u.operand
    op_str = (lambda _: "-")(op) if op else (lambda _: "!")(op) if op else hydra.dsl.python.unsupported("no matching case in inline union elimination")
    return hydra.serialization.cst(hydra.lib.strings.cat2(op_str, hydra.serialization.print_expr(expression_to_expr(operand))))

def where_clause_to_expr(wc: hydra.rust.syntax.WhereClause) -> hydra.ast.Expr:
    r"""Serialize a where clause."""

    preds = wc.predicates
    @lru_cache(1)
    def pred_exprs() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda p: (typ := p.type, bounds := p.bounds, hydra.serialization.space_sep((type_to_expr(typ), hydra.serialization.cst(":"), hydra.serialization.cst(hydra.lib.strings.intercalate(" + ", hydra.lib.lists.map((lambda b: hydra.serialization.print_expr(type_param_bound_to_expr(b))), bounds))))))[2]), preds)
    return hydra.serialization.space_sep((hydra.serialization.cst("where"), hydra.serialization.comma_sep(hydra.serialization.inline_style, pred_exprs())))

def while_expr_to_expr(w: hydra.rust.syntax.WhileExpr) -> hydra.ast.Expr:
    r"""Serialize a while expression."""

    label = w.label
    cond = w.condition
    body = w.body
    @lru_cache(1)
    def label_part() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda lbl: Just(hydra.serialization.cst(hydra.lib.strings.cat2("'", hydra.lib.strings.cat2(lbl, ":"))))), label)
    @lru_cache(1)
    def cond_expr() -> hydra.ast.Expr:
        match cond:
            case hydra.rust.syntax.IfConditionBool(value=e):
                return expression_to_expr(e)

            case hydra.rust.syntax.IfConditionLet(value=lc):
                pat = lc.pattern
                expr = lc.expr
                return hydra.serialization.space_sep((hydra.serialization.cst("let"), pattern_to_expr(pat), hydra.serialization.cst("="), expression_to_expr(expr)))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((label_part(), Just(hydra.serialization.cst("while")), Just(cond_expr()), Just(block_to_expr(body)))))

def attribute_to_expr(attr: hydra.rust.syntax.Attribute) -> hydra.ast.Expr:
    r"""Serialize an attribute."""

    inner = attr.inner
    path = attr.path
    tokens = attr.tokens
    @lru_cache(1)
    def prefix() -> str:
        return hydra.lib.logic.if_else(inner, (lambda : "#!["), (lambda : "#["))
    @lru_cache(1)
    def path_str() -> str:
        return hydra.lib.strings.intercalate("::", path)
    @lru_cache(1)
    def tokens_part() -> str:
        return hydra.lib.maybes.maybe((lambda : ""), (lambda t: hydra.lib.strings.cat(("(", t, ")"))), tokens)
    return hydra.serialization.cst(hydra.lib.strings.cat((prefix(), path_str(), tokens_part(), "]")))

def visibility_to_expr(vis: hydra.rust.syntax.Visibility) -> Maybe[hydra.ast.Expr]:
    r"""Serialize visibility to an optional expression."""

    match vis:
        case hydra.rust.syntax.VisibilityPublic():
            return Just(hydra.serialization.cst("pub"))

        case hydra.rust.syntax.VisibilityCrate():
            return Just(hydra.serialization.cst("pub(crate)"))

        case hydra.rust.syntax.VisibilityRestricted(value=path):
            return Just(hydra.serialization.cst(hydra.lib.strings.cat(("pub(in ", hydra.lib.strings.intercalate("::", path), ")"))))

        case hydra.rust.syntax.VisibilityPrivate():
            return Nothing()

        case _:
            raise AssertionError("Unreachable: all variants handled")

def item_with_comments_to_expr(iwc: hydra.rust.syntax.ItemWithComments) -> hydra.ast.Expr:
    r"""Serialize an item with optional doc comments and visibility."""

    doc = iwc.doc
    vis = iwc.visibility
    item = iwc.item
    @lru_cache(1)
    def doc_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda d: (hydra.serialization.cst(to_rust_doc_comment(d)),)), doc)
    @lru_cache(1)
    def vis_part() -> Maybe[hydra.ast.Expr]:
        return visibility_to_expr(vis)
    @lru_cache(1)
    def item_part() -> hydra.ast.Expr:
        return item_to_expr(item)
    return hydra.serialization.newline_sep(hydra.lib.lists.concat((doc_part(), (hydra.serialization.space_sep(hydra.lib.maybes.cat((vis_part(), Just(item_part())))),))))

def crate_to_expr(crate: hydra.rust.syntax.Crate) -> hydra.ast.Expr:
    r"""Serialize a Rust crate to an AST expression."""

    return hydra.serialization.double_newline_sep(hydra.lib.lists.map((lambda x1: item_with_comments_to_expr(x1)), crate.items))

def to_rust_comment(c: str) -> str:
    r"""Convert a string to Rust line comments."""

    return hydra.lib.strings.intercalate("\n", hydra.lib.lists.map((lambda s: hydra.lib.strings.cat2("// ", s)), hydra.lib.strings.lines(c)))
