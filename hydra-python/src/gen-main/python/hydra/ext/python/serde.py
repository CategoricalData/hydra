# Note: this is an automatically generated file. Do not edit.

r"""Python serializer: converts Python AST to concrete syntax."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Just, Maybe, Nothing, frozenlist
import hydra.constants
import hydra.core
import hydra.ext.python.syntax
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maybes
import hydra.lib.strings
import hydra.serialization

def encode_name(n: hydra.ext.python.syntax.Name) -> hydra.ast.Expr:
    r"""Serialize a Python name/identifier."""
    
    return hydra.serialization.cst(n.value)

def encode_lambda_param_no_default(p: hydra.ext.python.syntax.LambdaParamNoDefault) -> hydra.ast.Expr:
    r"""Serialize a lambda parameter without default."""
    
    return encode_name(p.value)

def encode_lambda_parameters(lp: hydra.ext.python.syntax.LambdaParameters) -> hydra.ast.Expr:
    r"""Serialize lambda parameters."""
    
    @lru_cache(1)
    def nodef() -> frozenlist[hydra.ext.python.syntax.LambdaParamNoDefault]:
        return lp.param_no_default
    return hydra.serialization.comma_sep(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: encode_lambda_param_no_default(x1)), nodef()))

def encode_number(num: hydra.ext.python.syntax.Number) -> hydra.ast.Expr:
    r"""Serialize a Python number literal."""
    
    match num:
        case hydra.ext.python.syntax.NumberFloat(value=f):
            return hydra.serialization.cst(hydra.lib.literals.show_bigfloat(f))
        
        case hydra.ext.python.syntax.NumberInteger(value=i):
            return hydra.serialization.cst(hydra.lib.literals.show_bigint(i))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def escape_python_string(double_quoted: bool, s: str) -> str:
    r"""Escape special characters in a Python string and wrap in quotes."""
    
    def replace(old: str, new: str, str: str) -> str:
        return hydra.lib.strings.intercalate(new, hydra.lib.strings.split_on(old, str))
    @lru_cache(1)
    def s1() -> str:
        return replace("\\", "\\\\", s)
    @lru_cache(1)
    def s2() -> str:
        return replace("\x00", "\\x00", s1())
    @lru_cache(1)
    def s3() -> str:
        return replace("\n", "\\n", s2())
    @lru_cache(1)
    def s4() -> str:
        return replace("\t", "\\t", s3())
    @lru_cache(1)
    def s5() -> str:
        return replace("\r", "\\r", s4())
    @lru_cache(1)
    def escaped() -> str:
        return hydra.lib.logic.if_else(double_quoted, (lambda : replace("\"", "\\\"", s5())), (lambda : replace("'", "\\'", s5())))
    @lru_cache(1)
    def quote() -> str:
        return hydra.lib.logic.if_else(double_quoted, (lambda : "\""), (lambda : "'"))
    return hydra.lib.strings.cat2(quote(), hydra.lib.strings.cat2(escaped(), quote()))

def encode_string(s: hydra.ext.python.syntax.String) -> hydra.ast.Expr:
    r"""Serialize a Python string literal."""
    
    @lru_cache(1)
    def content() -> str:
        return s.value
    @lru_cache(1)
    def style() -> hydra.ext.python.syntax.QuoteStyle:
        return s.quote_style
    match style():
        case hydra.ext.python.syntax.QuoteStyle.SINGLE:
            return hydra.serialization.cst(escape_python_string(False, content()))
        
        case hydra.ext.python.syntax.QuoteStyle.DOUBLE:
            return hydra.serialization.cst(escape_python_string(True, content()))
        
        case hydra.ext.python.syntax.QuoteStyle.TRIPLE:
            return hydra.serialization.no_sep((hydra.serialization.cst("r\"\"\""), hydra.serialization.cst(content()), hydra.serialization.cst("\"\"\"")))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_args(args: hydra.ext.python.syntax.Args) -> hydra.ast.Expr:
    r"""Serialize function arguments."""
    
    @lru_cache(1)
    def pos() -> frozenlist[hydra.ext.python.syntax.PosArg]:
        return args.positional
    @lru_cache(1)
    def ks() -> frozenlist[hydra.ext.python.syntax.KwargOrStarred]:
        return args.kwarg_or_starred
    @lru_cache(1)
    def kss() -> frozenlist[hydra.ext.python.syntax.KwargOrDoubleStarred]:
        return args.kwarg_or_double_starred
    return hydra.serialization.comma_sep(hydra.serialization.inline_style(), hydra.lib.lists.concat((hydra.lib.lists.map((lambda x1: encode_pos_arg(x1)), pos()), hydra.lib.lists.map((lambda x1: encode_kwarg_or_starred(x1)), ks()), hydra.lib.lists.map((lambda x1: encode_kwarg_or_double_starred(x1)), kss()))))

def encode_assignment_expression(ae: hydra.ext.python.syntax.AssignmentExpression) -> hydra.ast.Expr:
    r"""Serialize an assignment expression (walrus operator)."""
    
    @lru_cache(1)
    def name() -> hydra.ext.python.syntax.Name:
        return ae.name
    @lru_cache(1)
    def expr() -> hydra.ext.python.syntax.Expression:
        return ae.expression
    return hydra.serialization.space_sep((encode_name(name()), hydra.serialization.cst(":="), encode_expression(expr())))

def encode_atom(atom: hydra.ext.python.syntax.Atom) -> hydra.ast.Expr:
    r"""Serialize a Python atom (literal or basic expression)."""
    
    match atom:
        case hydra.ext.python.syntax.AtomDict(value=d):
            return encode_dict(d)
        
        case hydra.ext.python.syntax.AtomDictcomp():
            return hydra.serialization.cst("{...}")
        
        case hydra.ext.python.syntax.AtomEllipsis():
            return hydra.serialization.cst("...")
        
        case hydra.ext.python.syntax.AtomFalse():
            return hydra.serialization.cst("False")
        
        case hydra.ext.python.syntax.AtomGenexp():
            return hydra.serialization.cst("(...)")
        
        case hydra.ext.python.syntax.AtomGroup(value=g):
            return encode_group(g)
        
        case hydra.ext.python.syntax.AtomList(value=l):
            return encode_list(l)
        
        case hydra.ext.python.syntax.AtomListcomp():
            return hydra.serialization.cst("[...]")
        
        case hydra.ext.python.syntax.AtomName(value=n):
            return encode_name(n)
        
        case hydra.ext.python.syntax.AtomNone():
            return hydra.serialization.cst("None")
        
        case hydra.ext.python.syntax.AtomNumber(value=n2):
            return encode_number(n2)
        
        case hydra.ext.python.syntax.AtomSet(value=s):
            return encode_set(s)
        
        case hydra.ext.python.syntax.AtomSetcomp():
            return hydra.serialization.cst("{...}")
        
        case hydra.ext.python.syntax.AtomString(value=s2):
            return encode_string(s2)
        
        case hydra.ext.python.syntax.AtomTrue():
            return hydra.serialization.cst("True")
        
        case hydra.ext.python.syntax.AtomTuple(value=t):
            return encode_tuple(t)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_await_primary(ap: hydra.ext.python.syntax.AwaitPrimary) -> hydra.ast.Expr:
    r"""Serialize an await primary expression."""
    
    @lru_cache(1)
    def await_() -> bool:
        return ap.await_
    @lru_cache(1)
    def primary() -> hydra.ext.python.syntax.Primary:
        return ap.primary
    return hydra.lib.logic.if_else(await_(), (lambda : hydra.serialization.space_sep((hydra.serialization.cst("await"), encode_primary(primary())))), (lambda : encode_primary(primary())))

def encode_bitwise_and(band: hydra.ext.python.syntax.BitwiseAnd) -> hydra.ast.Expr:
    r"""Serialize a bitwise AND expression."""
    
    @lru_cache(1)
    def lhs() -> Maybe[hydra.ext.python.syntax.BitwiseAnd]:
        return band.lhs
    @lru_cache(1)
    def rhs() -> hydra.ext.python.syntax.ShiftExpression:
        return band.rhs
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.maybes.map((lambda l: hydra.serialization.space_sep((encode_bitwise_and(l), hydra.serialization.cst("&")))), lhs()), Just(encode_shift_expression(rhs())))))

def encode_bitwise_or(bor: hydra.ext.python.syntax.BitwiseOr) -> hydra.ast.Expr:
    r"""Serialize a bitwise OR expression."""
    
    @lru_cache(1)
    def lhs() -> Maybe[hydra.ext.python.syntax.BitwiseOr]:
        return bor.lhs
    @lru_cache(1)
    def rhs() -> hydra.ext.python.syntax.BitwiseXor:
        return bor.rhs
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.maybes.map((lambda l: hydra.serialization.space_sep((encode_bitwise_or(l), hydra.serialization.cst("|")))), lhs()), Just(encode_bitwise_xor(rhs())))))

def encode_bitwise_xor(bxor: hydra.ext.python.syntax.BitwiseXor) -> hydra.ast.Expr:
    r"""Serialize a bitwise XOR expression."""
    
    @lru_cache(1)
    def lhs() -> Maybe[hydra.ext.python.syntax.BitwiseXor]:
        return bxor.lhs
    @lru_cache(1)
    def rhs() -> hydra.ext.python.syntax.BitwiseAnd:
        return bxor.rhs
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.maybes.map((lambda l: hydra.serialization.space_sep((encode_bitwise_xor(l), hydra.serialization.cst("^")))), lhs()), Just(encode_bitwise_and(rhs())))))

def encode_comparison(cmp: hydra.ext.python.syntax.Comparison) -> hydra.ast.Expr:
    r"""Serialize a comparison expression."""
    
    return encode_bitwise_or(cmp.lhs)

def encode_conjunction(c: hydra.ext.python.syntax.Conjunction) -> hydra.ast.Expr:
    r"""Serialize a conjunction (and expression)."""
    
    return hydra.serialization.symbol_sep("and", hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: encode_inversion(x1)), c.value))

def encode_dict(d: hydra.ext.python.syntax.Dict) -> hydra.ast.Expr:
    r"""Serialize a Python dictionary."""
    
    return hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.half_block_style, hydra.lib.lists.map((lambda x1: encode_double_starred_kvpair(x1)), d.value))

def encode_disjunction(d: hydra.ext.python.syntax.Disjunction) -> hydra.ast.Expr:
    r"""Serialize a disjunction (or expression)."""
    
    return hydra.serialization.symbol_sep("or", hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: encode_conjunction(x1)), d.value))

def encode_double_starred_kvpair(dskv: hydra.ext.python.syntax.DoubleStarredKvpair) -> hydra.ast.Expr:
    r"""Serialize a double-starred key-value pair."""
    
    match dskv:
        case hydra.ext.python.syntax.DoubleStarredKvpairPair(value=p):
            return encode_kvpair(p)
        
        case hydra.ext.python.syntax.DoubleStarredKvpairStarred(value=e):
            return hydra.serialization.no_sep((hydra.serialization.cst("**"), encode_bitwise_or(e)))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_expression(expr: hydra.ext.python.syntax.Expression) -> hydra.ast.Expr:
    r"""Serialize a Python expression."""
    
    match expr:
        case hydra.ext.python.syntax.ExpressionSimple(value=d):
            return encode_disjunction(d)
        
        case hydra.ext.python.syntax.ExpressionConditional():
            return hydra.serialization.cst("... if ... else ...")
        
        case hydra.ext.python.syntax.ExpressionLambda(value=l):
            return encode_lambda(l)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_factor(f: hydra.ext.python.syntax.Factor) -> hydra.ast.Expr:
    r"""Serialize a factor expression."""
    
    match f:
        case hydra.ext.python.syntax.FactorPositive(value=inner):
            return hydra.serialization.no_sep((hydra.serialization.cst("+"), encode_factor(inner)))
        
        case hydra.ext.python.syntax.FactorNegative(value=inner2):
            return hydra.serialization.no_sep((hydra.serialization.cst("-"), encode_factor(inner2)))
        
        case hydra.ext.python.syntax.FactorComplement(value=inner3):
            return hydra.serialization.no_sep((hydra.serialization.cst("~"), encode_factor(inner3)))
        
        case hydra.ext.python.syntax.FactorSimple(value=p):
            return encode_power(p)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_group(g: hydra.ext.python.syntax.Group) -> hydra.ast.Expr:
    r"""Serialize a parenthesized group."""
    
    match g:
        case hydra.ext.python.syntax.GroupExpression(value=ne):
            return encode_named_expression(ne)
        
        case hydra.ext.python.syntax.GroupYield():
            return hydra.serialization.cst("(yield ...)")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_inversion(i: hydra.ext.python.syntax.Inversion) -> hydra.ast.Expr:
    r"""Serialize an inversion (not expression)."""
    
    match i:
        case hydra.ext.python.syntax.InversionNot(value=other):
            return hydra.serialization.space_sep((hydra.serialization.cst("not"), encode_inversion(other)))
        
        case hydra.ext.python.syntax.InversionSimple(value=c):
            return encode_comparison(c)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_kvpair(kv: hydra.ext.python.syntax.Kvpair) -> hydra.ast.Expr:
    r"""Serialize a key-value pair."""
    
    @lru_cache(1)
    def k() -> hydra.ext.python.syntax.Expression:
        return kv.key
    @lru_cache(1)
    def v() -> hydra.ext.python.syntax.Expression:
        return kv.value
    return hydra.serialization.space_sep((hydra.serialization.no_sep((encode_expression(k()), hydra.serialization.cst(":"))), encode_expression(v())))

def encode_kwarg(k: hydra.ext.python.syntax.Kwarg) -> hydra.ast.Expr:
    r"""Serialize a keyword argument."""
    
    @lru_cache(1)
    def name() -> hydra.ext.python.syntax.Name:
        return k.name
    @lru_cache(1)
    def expr() -> hydra.ext.python.syntax.Expression:
        return k.value
    return hydra.serialization.no_sep((encode_name(name()), hydra.serialization.cst("="), encode_expression(expr())))

def encode_kwarg_or_double_starred(kds: hydra.ext.python.syntax.KwargOrDoubleStarred) -> hydra.ast.Expr:
    r"""Serialize a kwarg or double starred."""
    
    match kds:
        case hydra.ext.python.syntax.KwargOrDoubleStarredKwarg(value=k):
            return encode_kwarg(k)
        
        case hydra.ext.python.syntax.KwargOrDoubleStarredDoubleStarred(value=e):
            return hydra.serialization.no_sep((hydra.serialization.cst("**"), encode_expression(e)))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_kwarg_or_starred(ks: hydra.ext.python.syntax.KwargOrStarred) -> hydra.ast.Expr:
    r"""Serialize a kwarg or starred."""
    
    match ks:
        case hydra.ext.python.syntax.KwargOrStarredKwarg(value=k):
            return encode_kwarg(k)
        
        case hydra.ext.python.syntax.KwargOrStarredStarred(value=se):
            return encode_starred_expression(se)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_lambda(l: hydra.ext.python.syntax.Lambda) -> hydra.ast.Expr:
    r"""Serialize a lambda expression."""
    
    @lru_cache(1)
    def params() -> hydra.ext.python.syntax.LambdaParameters:
        return l.params
    @lru_cache(1)
    def body() -> hydra.ext.python.syntax.Expression:
        return l.body
    return hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst("lambda"), hydra.serialization.no_sep((encode_lambda_parameters(params()), hydra.serialization.cst(":"))), encode_expression(body()))))

def encode_list(l: hydra.ext.python.syntax.List) -> hydra.ast.Expr:
    r"""Serialize a Python list."""
    
    return hydra.serialization.bracket_list_adaptive(hydra.lib.lists.map((lambda x1: encode_star_named_expression(x1)), l.value))

def encode_named_expression(ne: hydra.ext.python.syntax.NamedExpression) -> hydra.ast.Expr:
    r"""Serialize a named expression."""
    
    match ne:
        case hydra.ext.python.syntax.NamedExpressionSimple(value=e):
            return encode_expression(e)
        
        case hydra.ext.python.syntax.NamedExpressionAssignment(value=ae):
            return encode_assignment_expression(ae)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_pos_arg(pa: hydra.ext.python.syntax.PosArg) -> hydra.ast.Expr:
    r"""Serialize a positional argument."""
    
    match pa:
        case hydra.ext.python.syntax.PosArgStarred(value=se):
            return encode_starred_expression(se)
        
        case hydra.ext.python.syntax.PosArgAssignment(value=ae):
            return encode_assignment_expression(ae)
        
        case hydra.ext.python.syntax.PosArgExpression(value=e):
            return encode_expression(e)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_power(p: hydra.ext.python.syntax.Power) -> hydra.ast.Expr:
    r"""Serialize a power expression."""
    
    @lru_cache(1)
    def lhs() -> hydra.ext.python.syntax.AwaitPrimary:
        return p.lhs
    @lru_cache(1)
    def rhs() -> Maybe[hydra.ext.python.syntax.Factor]:
        return p.rhs
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(encode_await_primary(lhs())), hydra.lib.maybes.map((lambda r: hydra.serialization.space_sep((hydra.serialization.cst("**"), encode_factor(r)))), rhs()))))

def encode_primary(p: hydra.ext.python.syntax.Primary) -> hydra.ast.Expr:
    r"""Serialize a primary expression."""
    
    match p:
        case hydra.ext.python.syntax.PrimarySimple(value=a):
            return encode_atom(a)
        
        case hydra.ext.python.syntax.PrimaryCompound(value=pwr):
            return encode_primary_with_rhs(pwr)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_primary_rhs(rhs: hydra.ext.python.syntax.PrimaryRhs) -> hydra.ast.Expr:
    r"""Serialize a primary RHS."""
    
    match rhs:
        case hydra.ext.python.syntax.PrimaryRhsCall(value=args):
            return hydra.serialization.no_sep((hydra.serialization.cst("("), encode_args(args), hydra.serialization.cst(")")))
        
        case hydra.ext.python.syntax.PrimaryRhsProject(value=name):
            return hydra.serialization.no_sep((hydra.serialization.cst("."), encode_name(name)))
        
        case hydra.ext.python.syntax.PrimaryRhsSlices(value=slices):
            return hydra.serialization.no_sep((hydra.serialization.cst("["), encode_slices(slices), hydra.serialization.cst("]")))
        
        case hydra.ext.python.syntax.PrimaryRhsGenexp():
            return hydra.serialization.cst("[...]")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_primary_with_rhs(pwr: hydra.ext.python.syntax.PrimaryWithRhs) -> hydra.ast.Expr:
    r"""Serialize a primary with RHS."""
    
    @lru_cache(1)
    def prim() -> hydra.ext.python.syntax.Primary:
        return pwr.primary
    @lru_cache(1)
    def rhs() -> hydra.ext.python.syntax.PrimaryRhs:
        return pwr.rhs
    return hydra.serialization.no_sep((encode_primary(prim()), encode_primary_rhs(rhs())))

def encode_set(s: hydra.ext.python.syntax.Set) -> hydra.ast.Expr:
    r"""Serialize a Python set."""
    
    return hydra.serialization.braces_list_adaptive(hydra.lib.lists.map((lambda x1: encode_star_named_expression(x1)), s.value))

def encode_shift_expression(se: hydra.ext.python.syntax.ShiftExpression) -> hydra.ast.Expr:
    r"""Serialize a shift expression."""
    
    return encode_sum(se.rhs)

def encode_slice(s: hydra.ext.python.syntax.Slice) -> hydra.ast.Expr:
    r"""Serialize a slice."""
    
    match s:
        case hydra.ext.python.syntax.SliceNamed(value=ne):
            return encode_named_expression(ne)
        
        case hydra.ext.python.syntax.SliceSlice():
            return hydra.serialization.cst(":")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_slice_or_starred_expression(s: hydra.ext.python.syntax.SliceOrStarredExpression) -> hydra.ast.Expr:
    r"""Serialize a slice or starred expression."""
    
    match s:
        case hydra.ext.python.syntax.SliceOrStarredExpressionSlice(value=sl):
            return encode_slice(sl)
        
        case hydra.ext.python.syntax.SliceOrStarredExpressionStarred(value=se):
            return encode_starred_expression(se)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_slices(s: hydra.ext.python.syntax.Slices) -> hydra.ast.Expr:
    r"""Serialize slices."""
    
    @lru_cache(1)
    def hd() -> hydra.ext.python.syntax.Slice:
        return s.head
    @lru_cache(1)
    def tl() -> frozenlist[hydra.ext.python.syntax.SliceOrStarredExpression]:
        return s.tail
    return hydra.serialization.comma_sep(hydra.serialization.inline_style(), hydra.lib.lists.cons(encode_slice(hd()), hydra.lib.lists.map((lambda x1: encode_slice_or_starred_expression(x1)), tl())))

def encode_star_named_expression(sne: hydra.ext.python.syntax.StarNamedExpression) -> hydra.ast.Expr:
    r"""Serialize a star named expression."""
    
    match sne:
        case hydra.ext.python.syntax.StarNamedExpressionStar(value=bor):
            return hydra.serialization.no_sep((hydra.serialization.cst("*"), encode_bitwise_or(bor)))
        
        case hydra.ext.python.syntax.StarNamedExpressionSimple(value=ne):
            return encode_named_expression(ne)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_starred_expression(se: hydra.ext.python.syntax.StarredExpression) -> hydra.ast.Expr:
    r"""Serialize a starred expression."""
    
    return hydra.serialization.no_sep((hydra.serialization.cst("*"), encode_expression(se.value)))

def encode_sum(s: hydra.ext.python.syntax.Sum) -> hydra.ast.Expr:
    r"""Serialize a sum expression."""
    
    return encode_term(s.rhs)

def encode_term(t: hydra.ext.python.syntax.Term) -> hydra.ast.Expr:
    r"""Serialize a term expression."""
    
    return encode_factor(t.rhs)

def encode_tuple(t: hydra.ext.python.syntax.Tuple) -> hydra.ast.Expr:
    r"""Serialize a Python tuple."""
    
    @lru_cache(1)
    def es() -> frozenlist[hydra.ext.python.syntax.StarNamedExpression]:
        return t.value
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(es()), 1), (lambda : hydra.serialization.parens(hydra.serialization.no_sep((encode_star_named_expression(hydra.lib.lists.head(es())), hydra.serialization.cst(","))))), (lambda : hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: encode_star_named_expression(x1)), es()))))

def encode_star_expression(se: hydra.ext.python.syntax.StarExpression) -> hydra.ast.Expr:
    r"""Serialize a star expression."""
    
    match se:
        case hydra.ext.python.syntax.StarExpressionStar(value=bor):
            return hydra.serialization.no_sep((hydra.serialization.cst("*"), encode_bitwise_or(bor)))
        
        case hydra.ext.python.syntax.StarExpressionSimple(value=e):
            return encode_expression(e)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_annotated_rhs(arhs: hydra.ext.python.syntax.AnnotatedRhs) -> hydra.ast.Expr:
    def _hoist_hydra_ext_python_serde_encode_annotated_rhs_1(v1: hydra.ext.python.syntax.AnnotatedRhs) -> hydra.ast.Expr:
        match v1:
            case hydra.ext.python.syntax.AnnotatedRhsStar(value=ses):
                return hydra.serialization.comma_sep(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: encode_star_expression(x1)), ses))
            
            case hydra.ext.python.syntax.AnnotatedRhsYield():
                return hydra.serialization.cst("yield ...")
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.serialization.space_sep((hydra.serialization.cst("="), _hoist_hydra_ext_python_serde_encode_annotated_rhs_1(arhs)))

def encode_single_target(st: hydra.ext.python.syntax.SingleTarget) -> hydra.ast.Expr:
    r"""Serialize a single target."""
    
    match st:
        case hydra.ext.python.syntax.SingleTargetName(value=n):
            return encode_name(n)
        
        case hydra.ext.python.syntax.SingleTargetParens():
            return hydra.serialization.cst("(...)")
        
        case hydra.ext.python.syntax.SingleTargetSubscriptAttributeTarget():
            return hydra.serialization.cst("...")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_typed_assignment(ta: hydra.ext.python.syntax.TypedAssignment) -> hydra.ast.Expr:
    r"""Serialize a typed assignment."""
    
    @lru_cache(1)
    def lhs() -> hydra.ext.python.syntax.SingleTarget:
        return ta.lhs
    @lru_cache(1)
    def typ() -> hydra.ext.python.syntax.Expression:
        return ta.type
    @lru_cache(1)
    def rhs() -> Maybe[hydra.ext.python.syntax.AnnotatedRhs]:
        return ta.rhs
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(hydra.serialization.no_sep((encode_single_target(lhs()), hydra.serialization.cst(":")))), Just(encode_expression(typ())), hydra.lib.maybes.map((lambda x1: encode_annotated_rhs(x1)), rhs()))))

def encode_star_atom(sa: hydra.ext.python.syntax.StarAtom) -> hydra.ast.Expr:
    r"""Serialize a star atom."""
    
    match sa:
        case hydra.ext.python.syntax.StarAtomName(value=n):
            return encode_name(n)
        
        case hydra.ext.python.syntax.StarAtomTargetWithStarAtom():
            return hydra.serialization.cst("(...)")
        
        case hydra.ext.python.syntax.StarAtomStarTargetsTupleSeq():
            return hydra.serialization.cst("(...)")
        
        case hydra.ext.python.syntax.StarAtomStarTargetsListSeq():
            return hydra.serialization.cst("[...]")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_target_with_star_atom(t: hydra.ext.python.syntax.TargetWithStarAtom) -> hydra.ast.Expr:
    r"""Serialize a target with star atom."""
    
    match t:
        case hydra.ext.python.syntax.TargetWithStarAtomAtom(value=a):
            return encode_star_atom(a)
        
        case hydra.ext.python.syntax.TargetWithStarAtomProject():
            return hydra.serialization.cst("...")
        
        case hydra.ext.python.syntax.TargetWithStarAtomSlices():
            return hydra.serialization.cst("...")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_star_target(st: hydra.ext.python.syntax.StarTarget) -> hydra.ast.Expr:
    r"""Serialize a star target."""
    
    match st:
        case hydra.ext.python.syntax.StarTargetUnstarred(value=t):
            return encode_target_with_star_atom(t)
        
        case hydra.ext.python.syntax.StarTargetStarred(value=inner):
            return hydra.serialization.no_sep((hydra.serialization.cst("*"), encode_star_target(inner)))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_untyped_assignment(ua: hydra.ext.python.syntax.UntypedAssignment) -> hydra.ast.Expr:
    r"""Serialize an untyped assignment."""
    
    @lru_cache(1)
    def targets() -> frozenlist[hydra.ext.python.syntax.StarTarget]:
        return ua.targets
    @lru_cache(1)
    def rhs() -> hydra.ext.python.syntax.AnnotatedRhs:
        return ua.rhs
    return hydra.serialization.space_sep(hydra.lib.lists.concat((hydra.lib.lists.map((lambda x1: encode_star_target(x1)), targets()), (encode_annotated_rhs(rhs()),))))

def encode_assignment(a: hydra.ext.python.syntax.Assignment) -> hydra.ast.Expr:
    r"""Serialize an assignment."""
    
    match a:
        case hydra.ext.python.syntax.AssignmentTyped(value=t):
            return encode_typed_assignment(t)
        
        case hydra.ext.python.syntax.AssignmentUntyped(value=u):
            return encode_untyped_assignment(u)
        
        case hydra.ext.python.syntax.AssignmentAug():
            return hydra.serialization.cst("... += ...")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_dotted_name(dn: hydra.ext.python.syntax.DottedName) -> hydra.ast.Expr:
    r"""Serialize a dotted name (e.g., module.submodule)."""
    
    return hydra.serialization.cst(hydra.lib.strings.intercalate(".", hydra.lib.lists.map((lambda n: n.value), dn.value)))

def encode_import_from_as_name(ifan: hydra.ext.python.syntax.ImportFromAsName) -> hydra.ast.Expr:
    r"""Serialize an import from as name."""
    
    @lru_cache(1)
    def name() -> hydra.ext.python.syntax.Name:
        return ifan.name
    @lru_cache(1)
    def alias() -> Maybe[hydra.ext.python.syntax.Name]:
        return ifan.as_
    return hydra.lib.maybes.maybe(encode_name(name()), (lambda a: hydra.serialization.space_sep((encode_name(name()), hydra.serialization.cst("as"), encode_name(a)))), alias())

def encode_import_from_targets(t: hydra.ext.python.syntax.ImportFromTargets) -> hydra.ast.Expr:
    r"""Serialize import from targets."""
    
    match t:
        case hydra.ext.python.syntax.ImportFromTargetsSimple(value=names):
            return hydra.serialization.comma_sep(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: encode_import_from_as_name(x1)), names))
        
        case hydra.ext.python.syntax.ImportFromTargetsParens(value=names2):
            return hydra.serialization.no_sep((hydra.serialization.cst("("), hydra.serialization.comma_sep(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: encode_import_from_as_name(x1)), names2)), hydra.serialization.cst(")")))
        
        case hydra.ext.python.syntax.ImportFromTargetsStar():
            return hydra.serialization.cst("*")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_relative_import_prefix(p: hydra.ext.python.syntax.RelativeImportPrefix) -> hydra.ast.Expr:
    r"""Serialize a relative import prefix."""
    
    match p:
        case hydra.ext.python.syntax.RelativeImportPrefix.DOT:
            return hydra.serialization.cst(".")
        
        case hydra.ext.python.syntax.RelativeImportPrefix.ELLIPSIS:
            return hydra.serialization.cst("...")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_import_from(if_: hydra.ext.python.syntax.ImportFrom) -> hydra.ast.Expr:
    r"""Serialize an import from statement."""
    
    @lru_cache(1)
    def prefixes() -> frozenlist[hydra.ext.python.syntax.RelativeImportPrefix]:
        return if_.prefixes
    @lru_cache(1)
    def name() -> Maybe[hydra.ext.python.syntax.DottedName]:
        return if_.dotted_name
    @lru_cache(1)
    def targets() -> hydra.ext.python.syntax.ImportFromTargets:
        return if_.targets
    @lru_cache(1)
    def lhs() -> hydra.ast.Expr:
        return hydra.serialization.no_sep(hydra.lib.maybes.cat(hydra.lib.lists.concat((hydra.lib.lists.map((lambda p: Just(encode_relative_import_prefix(p))), prefixes()), (hydra.lib.maybes.map((lambda x1: encode_dotted_name(x1)), name()),)))))
    return hydra.serialization.space_sep((hydra.serialization.cst("from"), lhs(), hydra.serialization.cst("import"), encode_import_from_targets(targets())))

def encode_dotted_as_name(dan: hydra.ext.python.syntax.DottedAsName) -> hydra.ast.Expr:
    r"""Serialize a dotted as name."""
    
    @lru_cache(1)
    def name() -> hydra.ext.python.syntax.DottedName:
        return dan.name
    @lru_cache(1)
    def alias() -> Maybe[hydra.ext.python.syntax.Name]:
        return dan.as_
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(encode_dotted_name(name())), hydra.lib.maybes.map((lambda a: hydra.serialization.space_sep((hydra.serialization.cst("as"), encode_name(a)))), alias()))))

def encode_import_name(in_: hydra.ext.python.syntax.ImportName) -> hydra.ast.Expr:
    r"""Serialize an import name."""
    
    return hydra.serialization.space_sep((hydra.serialization.cst("import"), hydra.serialization.comma_sep(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: encode_dotted_as_name(x1)), in_.value))))

def encode_import_statement(is_: hydra.ext.python.syntax.ImportStatement) -> hydra.ast.Expr:
    r"""Serialize an import statement."""
    
    match is_:
        case hydra.ext.python.syntax.ImportStatementName(value=n):
            return encode_import_name(n)
        
        case hydra.ext.python.syntax.ImportStatementFrom(value=f):
            return encode_import_from(f)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_raise_expression(re: hydra.ext.python.syntax.RaiseExpression) -> hydra.ast.Expr:
    r"""Serialize a raise expression."""
    
    @lru_cache(1)
    def expr() -> hydra.ext.python.syntax.Expression:
        return re.expression
    @lru_cache(1)
    def from_() -> Maybe[hydra.ext.python.syntax.Expression]:
        return re.from_
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(encode_expression(expr())), hydra.lib.maybes.map((lambda f: hydra.serialization.space_sep((hydra.serialization.cst("from"), encode_expression(f)))), from_()))))

def encode_raise_statement(rs: hydra.ext.python.syntax.RaiseStatement) -> hydra.ast.Expr:
    r"""Serialize a raise statement."""
    
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(hydra.serialization.cst("raise")), hydra.lib.maybes.map((lambda x1: encode_raise_expression(x1)), rs.value))))

def encode_return_statement(rs: hydra.ext.python.syntax.ReturnStatement) -> hydra.ast.Expr:
    r"""Serialize a return statement."""
    
    return hydra.serialization.space_sep((hydra.serialization.cst("return"), hydra.serialization.comma_sep(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: encode_star_expression(x1)), rs.value))))

def encode_simple_type_parameter(stp: hydra.ext.python.syntax.SimpleTypeParameter) -> hydra.ast.Expr:
    r"""Serialize a simple type parameter."""
    
    return encode_name(stp.name)

def encode_type_parameter(tp: hydra.ext.python.syntax.TypeParameter) -> hydra.ast.Expr:
    r"""Serialize a type parameter."""
    
    match tp:
        case hydra.ext.python.syntax.TypeParameterSimple(value=s):
            return encode_simple_type_parameter(s)
        
        case hydra.ext.python.syntax.TypeParameterStar():
            return hydra.serialization.cst("*...")
        
        case hydra.ext.python.syntax.TypeParameterDoubleStar():
            return hydra.serialization.cst("**...")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_type_alias(ta: hydra.ext.python.syntax.TypeAlias) -> hydra.ast.Expr:
    r"""Serialize a type alias."""
    
    @lru_cache(1)
    def name() -> hydra.ext.python.syntax.Name:
        return ta.name
    @lru_cache(1)
    def tparams() -> frozenlist[hydra.ext.python.syntax.TypeParameter]:
        return ta.type_params
    @lru_cache(1)
    def expr() -> hydra.ext.python.syntax.Expression:
        return ta.expression
    @lru_cache(1)
    def alias() -> hydra.ast.Expr:
        return hydra.serialization.no_sep(hydra.lib.maybes.cat((Just(encode_name(name())), hydra.lib.logic.if_else(hydra.lib.lists.null(tparams()), (lambda : Nothing()), (lambda : Just(hydra.serialization.bracket_list(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: encode_type_parameter(x1)), tparams()))))))))
    return hydra.serialization.space_sep((hydra.serialization.cst("type"), alias(), hydra.serialization.cst("="), encode_expression(expr())))

def encode_simple_statement(ss: hydra.ext.python.syntax.SimpleStatement) -> hydra.ast.Expr:
    r"""Serialize a simple (single-line) Python statement."""
    
    match ss:
        case hydra.ext.python.syntax.SimpleStatementAssignment(value=a):
            return encode_assignment(a)
        
        case hydra.ext.python.syntax.SimpleStatementStarExpressions(value=es):
            return hydra.serialization.newline_sep(hydra.lib.lists.map((lambda x1: encode_star_expression(x1)), es))
        
        case hydra.ext.python.syntax.SimpleStatementReturn(value=r):
            return encode_return_statement(r)
        
        case hydra.ext.python.syntax.SimpleStatementRaise(value=r2):
            return encode_raise_statement(r2)
        
        case hydra.ext.python.syntax.SimpleStatementPass():
            return hydra.serialization.cst("pass")
        
        case hydra.ext.python.syntax.SimpleStatementBreak():
            return hydra.serialization.cst("break")
        
        case hydra.ext.python.syntax.SimpleStatementContinue():
            return hydra.serialization.cst("continue")
        
        case hydra.ext.python.syntax.SimpleStatementImport(value=i):
            return encode_import_statement(i)
        
        case hydra.ext.python.syntax.SimpleStatementTypeAlias(value=t):
            return encode_type_alias(t)
        
        case hydra.ext.python.syntax.SimpleStatementAssert():
            return hydra.serialization.cst("assert ...")
        
        case hydra.ext.python.syntax.SimpleStatementGlobal():
            return hydra.serialization.cst("global ...")
        
        case hydra.ext.python.syntax.SimpleStatementNonlocal():
            return hydra.serialization.cst("nonlocal ...")
        
        case hydra.ext.python.syntax.SimpleStatementDel():
            return hydra.serialization.cst("del ...")
        
        case _:
            raise TypeError("Unsupported SimpleStatement")

def encode_decorators(decs: hydra.ext.python.syntax.Decorators) -> hydra.ast.Expr:
    r"""Serialize decorators."""
    
    return hydra.serialization.newline_sep(hydra.lib.lists.map((lambda ne: hydra.serialization.no_sep((hydra.serialization.cst("@"), encode_named_expression(ne)))), decs.value))

def encode_annotation(ann: hydra.ext.python.syntax.Annotation) -> hydra.ast.Expr:
    r"""Serialize a type annotation."""
    
    return hydra.serialization.space_sep((hydra.serialization.cst(":"), encode_expression(ann.value)))

def encode_param(p: hydra.ext.python.syntax.Param) -> hydra.ast.Expr:
    r"""Serialize a parameter."""
    
    @lru_cache(1)
    def name() -> hydra.ext.python.syntax.Name:
        return p.name
    @lru_cache(1)
    def ann() -> Maybe[hydra.ext.python.syntax.Annotation]:
        return p.annotation
    return hydra.serialization.no_sep(hydra.lib.maybes.cat((Just(encode_name(name())), hydra.lib.maybes.map((lambda x1: encode_annotation(x1)), ann()))))

def encode_param_no_default(pnd: hydra.ext.python.syntax.ParamNoDefault) -> hydra.ast.Expr:
    r"""Serialize a parameter without default."""
    
    return encode_param(pnd.param)

def encode_param_no_default_parameters(pndp: hydra.ext.python.syntax.ParamNoDefaultParameters) -> hydra.ast.Expr:
    r"""Serialize parameters without defaults."""
    
    @lru_cache(1)
    def nodef() -> frozenlist[hydra.ext.python.syntax.ParamNoDefault]:
        return pndp.param_no_default
    return hydra.serialization.comma_sep(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: encode_param_no_default(x1)), nodef()))

def encode_parameters(p: hydra.ext.python.syntax.Parameters) -> hydra.ast.Expr:
    r"""Serialize function parameters."""
    
    match p:
        case hydra.ext.python.syntax.ParametersParamNoDefault(value=pnd):
            return encode_param_no_default_parameters(pnd)
        
        case hydra.ext.python.syntax.ParametersSlashNoDefault():
            return hydra.serialization.cst("...")
        
        case hydra.ext.python.syntax.ParametersSlashWithDefault():
            return hydra.serialization.cst("...")
        
        case _:
            raise TypeError("Unsupported Parameters")

def encode_guard(g: hydra.ext.python.syntax.Guard) -> hydra.ast.Expr:
    r"""Serialize a guard clause."""
    
    return hydra.serialization.space_sep((hydra.serialization.cst("if"), encode_named_expression(g.value)))

def encode_pattern_capture_target(pct: hydra.ext.python.syntax.PatternCaptureTarget) -> hydra.ast.Expr:
    r"""Serialize a pattern capture target."""
    
    return encode_name(pct.value)

def encode_capture_pattern(cp: hydra.ext.python.syntax.CapturePattern) -> hydra.ast.Expr:
    r"""Serialize a capture pattern."""
    
    return encode_pattern_capture_target(cp.value)

def encode_name_or_attribute(noa: hydra.ext.python.syntax.NameOrAttribute) -> hydra.ast.Expr:
    r"""Serialize a name or attribute."""
    
    return hydra.serialization.dot_sep(hydra.lib.lists.map((lambda x1: encode_name(x1)), noa.value))

def encode_attribute(attr: hydra.ext.python.syntax.Attribute) -> hydra.ast.Expr:
    r"""Serialize an attribute access."""
    
    return hydra.serialization.dot_sep(hydra.lib.lists.map((lambda x1: encode_name(x1)), attr.value))

def encode_value_pattern(vp: hydra.ext.python.syntax.ValuePattern) -> hydra.ast.Expr:
    r"""Serialize a value pattern."""
    
    return encode_attribute(vp.value)

def encode_class_pattern(cp: hydra.ext.python.syntax.ClassPattern) -> hydra.ast.Expr:
    r"""Serialize a class pattern."""
    
    @lru_cache(1)
    def noa() -> hydra.ext.python.syntax.NameOrAttribute:
        return cp.name_or_attribute
    @lru_cache(1)
    def pos() -> Maybe[hydra.ext.python.syntax.PositionalPatterns]:
        return cp.positional_patterns
    @lru_cache(1)
    def kw() -> Maybe[hydra.ext.python.syntax.KeywordPatterns]:
        return cp.keyword_patterns
    return hydra.serialization.no_sep(hydra.lib.maybes.cat((Just(encode_name_or_attribute(noa())), Just(hydra.serialization.cst("(")), hydra.lib.maybes.map((lambda x1: encode_positional_patterns(x1)), pos()), hydra.lib.maybes.map((lambda x1: encode_keyword_patterns(x1)), kw()), Just(hydra.serialization.cst(")")))))

def encode_closed_pattern(cp: hydra.ext.python.syntax.ClosedPattern) -> hydra.ast.Expr:
    r"""Serialize a closed pattern."""
    
    match cp:
        case hydra.ext.python.syntax.ClosedPatternLiteral():
            return hydra.serialization.cst("...")
        
        case hydra.ext.python.syntax.ClosedPatternCapture(value=c):
            return encode_capture_pattern(c)
        
        case hydra.ext.python.syntax.ClosedPatternWildcard():
            return hydra.serialization.cst("_")
        
        case hydra.ext.python.syntax.ClosedPatternValue(value=v):
            return encode_value_pattern(v)
        
        case hydra.ext.python.syntax.ClosedPatternGroup():
            return hydra.serialization.cst("(...)")
        
        case hydra.ext.python.syntax.ClosedPatternSequence():
            return hydra.serialization.cst("[...]")
        
        case hydra.ext.python.syntax.ClosedPatternMapping():
            return hydra.serialization.cst("{...}")
        
        case hydra.ext.python.syntax.ClosedPatternClass(value=c2):
            return encode_class_pattern(c2)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_keyword_pattern(kp: hydra.ext.python.syntax.KeywordPattern) -> hydra.ast.Expr:
    r"""Serialize a keyword pattern."""
    
    @lru_cache(1)
    def name() -> hydra.ext.python.syntax.Name:
        return kp.name
    @lru_cache(1)
    def pat() -> hydra.ext.python.syntax.Pattern:
        return kp.pattern
    return hydra.serialization.no_sep((encode_name(name()), hydra.serialization.cst("="), encode_pattern(pat())))

def encode_keyword_patterns(kp: hydra.ext.python.syntax.KeywordPatterns) -> hydra.ast.Expr:
    r"""Serialize keyword patterns."""
    
    return hydra.serialization.comma_sep(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: encode_keyword_pattern(x1)), kp.value))

def encode_or_pattern(op: hydra.ext.python.syntax.OrPattern) -> hydra.ast.Expr:
    r"""Serialize an or pattern."""
    
    return hydra.serialization.symbol_sep("|", hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: encode_closed_pattern(x1)), op.value))

def encode_pattern(p: hydra.ext.python.syntax.Pattern) -> hydra.ast.Expr:
    r"""Serialize a pattern."""
    
    match p:
        case hydra.ext.python.syntax.PatternOr(value=op):
            return encode_or_pattern(op)
        
        case hydra.ext.python.syntax.PatternAs():
            return hydra.serialization.cst("... as ...")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_positional_patterns(pp: hydra.ext.python.syntax.PositionalPatterns) -> hydra.ast.Expr:
    r"""Serialize positional patterns."""
    
    return hydra.serialization.comma_sep(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: encode_pattern(x1)), pp.value))

def encode_patterns(ps: hydra.ext.python.syntax.Patterns) -> hydra.ast.Expr:
    r"""Serialize patterns."""
    
    match ps:
        case hydra.ext.python.syntax.PatternsPattern(value=p):
            return encode_pattern(p)
        
        case hydra.ext.python.syntax.PatternsSequence():
            return hydra.serialization.cst("...")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_subject_expression(se: hydra.ext.python.syntax.SubjectExpression) -> hydra.ast.Expr:
    r"""Serialize a subject expression."""
    
    match se:
        case hydra.ext.python.syntax.SubjectExpressionSimple(value=ne):
            return encode_named_expression(ne)
        
        case hydra.ext.python.syntax.SubjectExpressionTuple():
            return hydra.serialization.cst("*...")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def to_python_comments(doc_: str) -> str:
    r"""Convert a doc string to Python comment format."""
    
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(doc_, ""), (lambda : ""), (lambda : hydra.lib.strings.intercalate("\n", hydra.lib.lists.map((lambda line: hydra.lib.strings.cat2("# ", line)), hydra.lib.strings.lines(doc_)))))

def encode_annotated_statement(as_: hydra.ext.python.syntax.AnnotatedStatement) -> hydra.ast.Expr:
    r"""Serialize an annotated statement (with optional doc comment)."""
    
    @lru_cache(1)
    def doc_() -> str:
        return as_.comment
    @lru_cache(1)
    def stmt() -> hydra.ext.python.syntax.Statement:
        return as_.statement
    return hydra.serialization.newline_sep((hydra.serialization.cst(to_python_comments(doc_())), encode_statement(stmt())))

def encode_block(b: hydra.ext.python.syntax.Block) -> hydra.ast.Expr:
    r"""Serialize a block."""
    
    match b:
        case hydra.ext.python.syntax.BlockIndented(value=groups):
            return hydra.serialization.tab_indent_double_space(hydra.lib.lists.map((lambda stmts: hydra.serialization.newline_sep(hydra.lib.lists.map((lambda x1: encode_statement(x1)), stmts))), groups))
        
        case hydra.ext.python.syntax.BlockSimple(value=ss):
            return hydra.serialization.semicolon_sep(hydra.lib.lists.map((lambda x1: encode_simple_statement(x1)), ss))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_case_block(cb: hydra.ext.python.syntax.CaseBlock) -> hydra.ast.Expr:
    r"""Serialize a case block."""
    
    @lru_cache(1)
    def patterns() -> hydra.ext.python.syntax.Patterns:
        return cb.patterns
    @lru_cache(1)
    def guard() -> Maybe[hydra.ext.python.syntax.Guard]:
        return cb.guard
    @lru_cache(1)
    def body() -> hydra.ext.python.syntax.Block:
        return cb.body
    return hydra.serialization.newline_sep((hydra.serialization.no_sep((hydra.serialization.space_sep(hydra.lib.maybes.cat((Just(hydra.serialization.cst("case")), Just(encode_patterns(patterns())), hydra.lib.maybes.map((lambda x1: encode_guard(x1)), guard())))), hydra.serialization.cst(":"))), encode_block(body())))

def encode_class_definition(cd: hydra.ext.python.syntax.ClassDefinition) -> hydra.ast.Expr:
    r"""Serialize a class definition."""
    
    @lru_cache(1)
    def decs() -> Maybe[hydra.ext.python.syntax.Decorators]:
        return cd.decorators
    @lru_cache(1)
    def name() -> hydra.ext.python.syntax.Name:
        return cd.name
    @lru_cache(1)
    def args() -> Maybe[hydra.ext.python.syntax.Args]:
        return cd.arguments
    @lru_cache(1)
    def body() -> hydra.ext.python.syntax.Block:
        return cd.body
    @lru_cache(1)
    def arg_part() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.map((lambda a: hydra.serialization.no_sep((hydra.serialization.cst("("), encode_args(a), hydra.serialization.cst(")")))), args())
    return hydra.serialization.newline_sep(hydra.lib.maybes.cat((hydra.lib.maybes.map((lambda x1: encode_decorators(x1)), decs()), Just(hydra.serialization.no_sep(hydra.lib.maybes.cat((Just(hydra.serialization.space_sep((hydra.serialization.cst("class"), encode_name(name())))), arg_part(), Just(hydra.serialization.cst(":")))))), Just(encode_block(body())))))

def encode_compound_statement(cs: hydra.ext.python.syntax.CompoundStatement) -> hydra.ast.Expr:
    r"""Serialize a compound (multi-line) Python statement."""
    
    match cs:
        case hydra.ext.python.syntax.CompoundStatementFunction(value=f):
            return encode_function_definition(f)
        
        case hydra.ext.python.syntax.CompoundStatementIf():
            return hydra.serialization.cst("if ...")
        
        case hydra.ext.python.syntax.CompoundStatementClassDef(value=c):
            return encode_class_definition(c)
        
        case hydra.ext.python.syntax.CompoundStatementWith():
            return hydra.serialization.cst("with ...")
        
        case hydra.ext.python.syntax.CompoundStatementFor():
            return hydra.serialization.cst("for ...")
        
        case hydra.ext.python.syntax.CompoundStatementTry():
            return hydra.serialization.cst("try ...")
        
        case hydra.ext.python.syntax.CompoundStatementWhile():
            return hydra.serialization.cst("while ...")
        
        case hydra.ext.python.syntax.CompoundStatementMatch(value=m):
            return encode_match_statement(m)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_function_def_raw(fdr: hydra.ext.python.syntax.FunctionDefRaw) -> hydra.ast.Expr:
    r"""Serialize a raw function definition."""
    
    @lru_cache(1)
    def async_() -> bool:
        return fdr.async_
    @lru_cache(1)
    def name() -> hydra.ext.python.syntax.Name:
        return fdr.name
    @lru_cache(1)
    def tparams() -> frozenlist[hydra.ext.python.syntax.TypeParameter]:
        return fdr.type_params
    @lru_cache(1)
    def params() -> Maybe[hydra.ext.python.syntax.Parameters]:
        return fdr.params
    @lru_cache(1)
    def ret_type() -> Maybe[hydra.ext.python.syntax.Expression]:
        return fdr.return_type
    @lru_cache(1)
    def block() -> hydra.ext.python.syntax.Block:
        return fdr.block
    @lru_cache(1)
    def async_kw() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(async_(), (lambda : Just(hydra.serialization.cst("async"))), (lambda : Nothing()))
    @lru_cache(1)
    def tparam_part() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(tparams()), (lambda : Nothing()), (lambda : Just(hydra.serialization.bracket_list(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: encode_type_parameter(x1)), tparams())))))
    @lru_cache(1)
    def param_part() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.map((lambda x1: encode_parameters(x1)), params())
    @lru_cache(1)
    def ret_part() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.map((lambda t: hydra.serialization.space_sep((hydra.serialization.cst("->"), encode_expression(t)))), ret_type())
    return hydra.serialization.newline_sep((hydra.serialization.no_sep((hydra.serialization.space_sep(hydra.lib.maybes.cat((async_kw(), Just(hydra.serialization.cst("def")), Just(hydra.serialization.no_sep(hydra.lib.maybes.cat((Just(encode_name(name())), tparam_part(), Just(hydra.serialization.cst("(")), param_part(), Just(hydra.serialization.cst(")")))))), ret_part()))), hydra.serialization.cst(":"))), encode_block(block())))

def encode_function_definition(fd: hydra.ext.python.syntax.FunctionDefinition) -> hydra.ast.Expr:
    r"""Serialize a function definition."""
    
    @lru_cache(1)
    def decs() -> Maybe[hydra.ext.python.syntax.Decorators]:
        return fd.decorators
    @lru_cache(1)
    def raw() -> hydra.ext.python.syntax.FunctionDefRaw:
        return fd.raw
    return hydra.serialization.newline_sep(hydra.lib.maybes.cat((hydra.lib.maybes.map((lambda x1: encode_decorators(x1)), decs()), Just(encode_function_def_raw(raw())))))

def encode_match_statement(ms: hydra.ext.python.syntax.MatchStatement) -> hydra.ast.Expr:
    r"""Serialize a match statement."""
    
    @lru_cache(1)
    def subj() -> hydra.ext.python.syntax.SubjectExpression:
        return ms.subject
    @lru_cache(1)
    def cases() -> frozenlist[hydra.ext.python.syntax.CaseBlock]:
        return ms.cases
    return hydra.serialization.newline_sep((hydra.serialization.space_sep((hydra.serialization.cst("match"), hydra.serialization.no_sep((encode_subject_expression(subj()), hydra.serialization.cst(":"))))), hydra.serialization.tab_indent_double_space(hydra.lib.lists.map((lambda x1: encode_case_block(x1)), cases()))))

def encode_statement(stmt: hydra.ext.python.syntax.Statement) -> hydra.ast.Expr:
    r"""Serialize a Python statement."""
    
    match stmt:
        case hydra.ext.python.syntax.StatementAnnotated(value=a):
            return encode_annotated_statement(a)
        
        case hydra.ext.python.syntax.StatementSimple(value=ss):
            return hydra.serialization.newline_sep(hydra.lib.lists.map((lambda x1: encode_simple_statement(x1)), ss))
        
        case hydra.ext.python.syntax.StatementCompound(value=c):
            return encode_compound_statement(c)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_lambda_star_etc(lse: hydra.ext.python.syntax.LambdaStarEtc) -> hydra.ast.Expr:
    r"""Serialize lambda star etc."""
    
    match lse:
        case hydra.ext.python.syntax.LambdaStarEtcParamNoDefault(value=p):
            return encode_lambda_param_no_default(p)
        
        case hydra.ext.python.syntax.LambdaStarEtcStar():
            return hydra.serialization.cst("*...")
        
        case hydra.ext.python.syntax.LambdaStarEtcParamMaybeDefault():
            return hydra.serialization.cst("...")
        
        case hydra.ext.python.syntax.LambdaStarEtcKwds():
            return hydra.serialization.cst("**...")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_module(mod: hydra.ext.python.syntax.Module) -> hydra.ast.Expr:
    r"""Serialize a Python module to an AST expression."""
    
    @lru_cache(1)
    def warning() -> hydra.ast.Expr:
        return hydra.serialization.cst(to_python_comments(hydra.constants.warning_auto_generated_file))
    @lru_cache(1)
    def groups() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda group: hydra.serialization.newline_sep(hydra.lib.lists.map((lambda x1: encode_statement(x1)), group))), mod.value)
    return hydra.serialization.double_newline_sep(hydra.lib.lists.cons(warning(), groups()))
