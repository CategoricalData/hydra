# Note: this is an automatically generated file. Do not edit.

r"""Serialization functions for converting Scala AST to abstract expressions."""

from __future__ import annotations
from functools import lru_cache
from hydra.dsl.python import Nothing, frozenlist
from typing import cast
import hydra.ast
import hydra.core
import hydra.ext.java.serde
import hydra.ext.scala.syntax
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.strings
import hydra.serialization

# The dot operator for member access.
dot_op = hydra.ast.Op(hydra.ast.Symbol("."), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsNone())), hydra.ast.Precedence(0), hydra.ast.Associativity.LEFT)

@lru_cache(1)
def function_arrow_op() -> hydra.ast.Op:
    r"""The function arrow operator (=>)."""

    return hydra.serialization.op("=>", hydra.lib.math.negate(1), hydra.ast.Associativity.RIGHT)

# The match operator.
match_op = hydra.ast.Op(hydra.ast.Symbol("match"), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsSpace()), cast(hydra.ast.Ws, hydra.ast.WsBreakAndIndent("  "))), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE)

def write_data_name(dn: hydra.ext.scala.syntax.Data_Name) -> hydra.ast.Expr:
    r"""Convert a data name to an expression."""

    return hydra.serialization.cst(dn.value.value)

def write_name(name: hydra.ext.scala.syntax.Name) -> hydra.ast.Expr:
    r"""Convert a name to an expression."""

    match name:
        case hydra.ext.scala.syntax.NameValue(value=s):
            return hydra.serialization.cst(s)

        case _:
            raise TypeError("Unsupported Name")

def write_type_name(tn: hydra.ext.scala.syntax.Type_Name) -> hydra.ast.Expr:
    r"""Convert a type name to an expression."""

    return hydra.serialization.cst(tn.value)

def write_type_param(tp: hydra.ext.scala.syntax.Type_Param) -> hydra.ast.Expr:
    r"""Convert a type parameter to an expression."""

    return write_name(tp.name)

def write_type(typ: hydra.ext.scala.syntax.Type):
    def _hoist_hydra_ext_scala_serde_write_type_1(v1):
        match v1:
            case hydra.ext.scala.syntax.Type_RefName(value=name):
                return write_type_name(name)

            case _:
                raise TypeError("Unsupported Type_Ref")
    def _hoist_hydra_ext_scala_serde_write_type_2(v1):
        match v1:
            case hydra.ext.scala.syntax.Type_FunctionTypeFunction(value=tf):
                @lru_cache(1)
                def dom() -> hydra.ext.scala.syntax.Type:
                    return hydra.lib.lists.head(tf.params)
                cod = tf.res
                return hydra.serialization.ifx(function_arrow_op(), write_type(dom()), write_type(cod))

            case _:
                raise TypeError("Unsupported Type_FunctionType")
    match typ:
        case hydra.ext.scala.syntax.TypeRef(value=tr):
            return _hoist_hydra_ext_scala_serde_write_type_1(tr)

        case hydra.ext.scala.syntax.TypeApply(value=ta):
            fun = ta.tpe
            args = ta.args
            return hydra.serialization.no_sep((write_type(fun), hydra.serialization.bracket_list(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_type(x1)), args))))

        case hydra.ext.scala.syntax.TypeFunctionType(value=ft):
            return _hoist_hydra_ext_scala_serde_write_type_2(ft)

        case hydra.ext.scala.syntax.TypeLambda(value=tl):
            params = tl.tparams
            body = tl.tpe
            return hydra.serialization.no_sep((write_type(body), hydra.serialization.bracket_list(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_type_param(x1)), params))))

        case hydra.ext.scala.syntax.TypeVar(value=tv):
            return write_type_name(tv.name)

        case _:
            raise TypeError("Unsupported Type")

def write_data_param(dp: hydra.ext.scala.syntax.Data_Param) -> hydra.ast.Expr:
    r"""Convert a data parameter to an expression."""

    name = dp.name
    stype = dp.decltpe
    return hydra.serialization.no_sep(hydra.lib.maybes.cat((hydra.lib.maybes.pure(write_name(name)), hydra.lib.maybes.map((lambda t: hydra.serialization.space_sep((hydra.serialization.cst(":"), write_type(t)))), stype))))

def write_lit(lit: hydra.ext.scala.syntax.Lit) -> hydra.ast.Expr:
    r"""Convert a literal to an expression."""

    match lit:
        case hydra.ext.scala.syntax.LitBoolean(value=b):
            return hydra.serialization.cst(hydra.lib.logic.if_else(b, (lambda : "true"), (lambda : "false")))

        case hydra.ext.scala.syntax.LitByte(value=i):
            return hydra.serialization.cst(hydra.lib.strings.cat2(hydra.lib.literals.show_int8(i), ".toByte"))

        case hydra.ext.scala.syntax.LitShort(value=i2):
            return hydra.serialization.cst(hydra.lib.strings.cat2(hydra.lib.literals.show_int16(i2), ".toShort"))

        case hydra.ext.scala.syntax.LitInt(value=i3):
            return hydra.serialization.cst(hydra.lib.literals.show_int32(i3))

        case hydra.ext.scala.syntax.LitLong(value=i4):
            return hydra.serialization.cst(hydra.lib.strings.cat2(hydra.lib.literals.show_int64(i4), "L"))

        case hydra.ext.scala.syntax.LitFloat(value=f):
            return hydra.serialization.cst(hydra.lib.strings.cat2(hydra.lib.literals.show_float32(f), "f"))

        case hydra.ext.scala.syntax.LitDouble(value=f2):
            return hydra.serialization.cst(hydra.lib.literals.show_float64(f2))

        case hydra.ext.scala.syntax.LitUnit():
            return hydra.serialization.cst("()")

        case hydra.ext.scala.syntax.LitString(value=s):
            return hydra.serialization.cst(hydra.lib.strings.cat2("\"", hydra.lib.strings.cat2(hydra.ext.java.serde.escape_java_string(s), "\"")))

        case _:
            return hydra.serialization.cst("TODO:literal")

def write_init(init: hydra.ext.scala.syntax.Init) -> hydra.ast.Expr:
    r"""Convert an init to an expression."""

    return write_type(init.tpe)

def write_mod(m: hydra.ext.scala.syntax.Mod) -> hydra.ast.Expr:
    r"""Convert a modifier to an expression."""

    match m:
        case hydra.ext.scala.syntax.ModCase():
            return hydra.serialization.cst("case")

        case hydra.ext.scala.syntax.ModSealed():
            return hydra.serialization.cst("sealed")

        case hydra.ext.scala.syntax.ModAbstract():
            return hydra.serialization.cst("abstract")

        case hydra.ext.scala.syntax.ModFinal():
            return hydra.serialization.cst("final")

        case hydra.ext.scala.syntax.ModOverride():
            return hydra.serialization.cst("override")

        case hydra.ext.scala.syntax.ModImplicit():
            return hydra.serialization.cst("implicit")

        case hydra.ext.scala.syntax.ModLazy():
            return hydra.serialization.cst("lazy")

        case hydra.ext.scala.syntax.ModPrivate():
            return hydra.serialization.cst("private")

        case hydra.ext.scala.syntax.ModProtected():
            return hydra.serialization.cst("protected")

        case _:
            raise TypeError("Unsupported Mod")

def write_importer(imp: hydra.ext.scala.syntax.Importer) -> hydra.ast.Expr:
    r"""Convert an importer to an expression."""

    ref = imp.ref
    importees = imp.importees
    @lru_cache(1)
    def ref_name() -> str:
        match ref:
            case hydra.ext.scala.syntax.Data_RefName(value=dn):
                return dn.value.value

            case _:
                raise TypeError("Unsupported Data_Ref")
    @lru_cache(1)
    def for_importees():
        def _hoist_for_importees_1(v1):
            match v1:
                case hydra.ext.scala.syntax.NameValue(value=s):
                    return s

                case _:
                    raise TypeError("Unsupported Name")
        def _hoist_for_importees_2(v1):
            match v1:
                case hydra.ext.scala.syntax.ImporteeWildcard():
                    return hydra.serialization.cst("*")

                case hydra.ext.scala.syntax.ImporteeName(value=in_):
                    return hydra.serialization.cst(_hoist_for_importees_1(in_.name))

                case _:
                    raise TypeError("Unsupported Importee")
        def _hoist_for_importees_3(v1):
            match v1:
                case hydra.ext.scala.syntax.NameValue(value=s):
                    return s

                case _:
                    raise TypeError("Unsupported Name")
        def _hoist_for_importees_4(v1):
            match v1:
                case hydra.ext.scala.syntax.ImporteeWildcard():
                    return hydra.serialization.cst("*")

                case hydra.ext.scala.syntax.ImporteeName(value=in_):
                    return hydra.serialization.cst(_hoist_for_importees_3(in_.name))

                case _:
                    raise TypeError("Unsupported Importee")
        return hydra.lib.logic.if_else(hydra.lib.lists.null(importees), (lambda : hydra.serialization.cst("")), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(importees), 1), (lambda : hydra.serialization.no_sep((hydra.serialization.cst("."), _hoist_for_importees_2(hydra.lib.lists.head(importees))))), (lambda : hydra.serialization.no_sep((hydra.serialization.cst("."), hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.inline_style, hydra.lib.lists.map((lambda it: _hoist_for_importees_4(it)), importees))))))))
    return hydra.serialization.space_sep((hydra.serialization.cst("import"), hydra.serialization.no_sep((hydra.serialization.cst(ref_name()), for_importees()))))

def write_import_export_stat(ie: hydra.ext.scala.syntax.ImportExportStat) -> hydra.ast.Expr:
    r"""Convert an import/export statement to an expression."""

    match ie:
        case hydra.ext.scala.syntax.ImportExportStatImport(value=imp):
            importers = imp.importers
            return hydra.serialization.newline_sep(hydra.lib.lists.map((lambda x1: write_importer(x1)), importers))

        case _:
            raise TypeError("Unsupported ImportExportStat")

def write_case(c: hydra.ext.scala.syntax.Case) -> hydra.ast.Expr:
    r"""Convert a case clause to an expression."""

    pat = c.pat
    term = c.body
    return hydra.serialization.space_sep((hydra.serialization.cst("case"), write_pat(pat), hydra.serialization.cst("=>"), write_term(term)))

def write_data_function_data(ft: hydra.ext.scala.syntax.Data_FunctionData) -> hydra.ast.Expr:
    r"""Convert function data to an expression."""

    match ft:
        case hydra.ext.scala.syntax.Data_FunctionDataFunction(value=f):
            params = f.params
            body = f.body
            @lru_cache(1)
            def body_expr() -> hydra.ast.Expr:
                return write_term(body)
            @lru_cache(1)
            def body_len() -> int:
                return hydra.serialization.expression_length(body_expr())
            return hydra.lib.logic.if_else(hydra.lib.equality.gt(body_len(), 60), (lambda : hydra.serialization.no_sep((hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: write_data_param(x1)), params)), hydra.serialization.cst(" =>\n  "), body_expr()))), (lambda : hydra.serialization.space_sep((hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: write_data_param(x1)), params)), hydra.serialization.cst("=>"), body_expr()))))

        case _:
            raise TypeError("Unsupported Data_FunctionData")

def write_data_ref(ref: hydra.ext.scala.syntax.Data_Ref) -> hydra.ast.Expr:
    r"""Convert a data reference to an expression."""

    match ref:
        case hydra.ext.scala.syntax.Data_RefName(value=name):
            return write_data_name(name)

        case hydra.ext.scala.syntax.Data_RefSelect(value=sel):
            return write_data_select(sel)

        case _:
            raise TypeError("Unsupported Data_Ref")

def write_data_select(sel: hydra.ext.scala.syntax.Data_Select) -> hydra.ast.Expr:
    r"""Convert a data select to an expression."""

    arg = sel.qual
    name = sel.name
    return hydra.serialization.ifx(dot_op, write_term(arg), write_term(cast(hydra.ext.scala.syntax.Data, hydra.ext.scala.syntax.DataRef(cast(hydra.ext.scala.syntax.Data_Ref, hydra.ext.scala.syntax.Data_RefName(name))))))

def write_defn(def_: hydra.ext.scala.syntax.Defn) -> hydra.ast.Expr:
    r"""Convert a definition to an expression."""

    match def_:
        case hydra.ext.scala.syntax.DefnDef(value=dd):
            name = dd.name
            tparams = dd.tparams
            paramss = dd.paramss
            scod = dd.decltpe
            body = dd.body
            @lru_cache(1)
            def tparams_expr() -> Maybe[hydra.ast.Expr]:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(tparams), (lambda : Nothing()), (lambda : hydra.lib.maybes.pure(hydra.serialization.bracket_list(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_type_param(x1)), tparams)))))
            @lru_cache(1)
            def scod_expr() -> Maybe[hydra.ast.Expr]:
                return hydra.lib.maybes.map((lambda t: hydra.serialization.space_sep((hydra.serialization.cst(":"), write_type(t)))), scod)
            @lru_cache(1)
            def paramss_exprs() -> frozenlist[hydra.ast.Expr]:
                return hydra.lib.lists.map((lambda ps: hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: write_data_param(x1)), ps))), paramss)
            @lru_cache(1)
            def name_and_params() -> hydra.ast.Expr:
                return hydra.serialization.no_sep(hydra.lib.maybes.cat(hydra.lib.lists.concat(((hydra.lib.maybes.pure(write_data_name(name)),), (tparams_expr(),), hydra.lib.lists.map((lambda pe: hydra.lib.maybes.pure(pe)), paramss_exprs()), (scod_expr(),)))))
            @lru_cache(1)
            def body_expr() -> hydra.ast.Expr:
                return write_term(body)
            @lru_cache(1)
            def def_sig() -> hydra.ast.Expr:
                return hydra.serialization.space_sep((hydra.serialization.cst("def"), name_and_params(), hydra.serialization.cst("=")))
            @lru_cache(1)
            def body_len() -> int:
                return hydra.serialization.expression_length(body_expr())
            return hydra.lib.logic.if_else(hydra.lib.equality.gt(body_len(), 80), (lambda : hydra.serialization.no_sep((def_sig(), hydra.serialization.cst("\n  "), body_expr()))), (lambda : hydra.serialization.space_sep((def_sig(), body_expr()))))

        case hydra.ext.scala.syntax.DefnType(value=dt):
            name = dt.name
            tparams = dt.tparams
            body = dt.body
            return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.maybes.pure(hydra.serialization.cst("type")), hydra.lib.maybes.pure(write_type_name(name)), hydra.lib.logic.if_else(hydra.lib.lists.null(tparams), (lambda : Nothing()), (lambda : hydra.lib.maybes.pure(hydra.serialization.bracket_list(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_type_param(x1)), tparams))))), hydra.lib.maybes.pure(hydra.serialization.cst("=")), hydra.lib.maybes.pure(write_type(body)))))

        case hydra.ext.scala.syntax.DefnVal(value=dv):
            mods = dv.mods
            pats = dv.pats
            typ = dv.decltpe
            rhs = dv.rhs
            @lru_cache(1)
            def first_pat() -> hydra.ext.scala.syntax.Pat:
                return hydra.lib.lists.head(pats)
            @lru_cache(1)
            def pat_name():
                def _hoist_pat_name_1(v1):
                    match v1:
                        case hydra.ext.scala.syntax.PatVar(value=pv):
                            return pv.name

                        case _:
                            raise TypeError("Unsupported Pat")
                return _hoist_pat_name_1(first_pat())
            name_str = pat_name().value.value
            @lru_cache(1)
            def name_and_type() -> hydra.ast.Expr:
                return hydra.lib.maybes.maybe((lambda : hydra.serialization.cst(name_str)), (lambda t: hydra.serialization.space_sep((hydra.serialization.cst(hydra.lib.strings.cat2(name_str, ":")), write_type(t)))), typ)
            @lru_cache(1)
            def val_keyword() -> str:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(mods), (lambda : "val"), (lambda : "lazy val"))
            return hydra.serialization.space_sep((hydra.serialization.cst(val_keyword()), name_and_type(), hydra.serialization.cst("="), write_term(rhs)))

        case hydra.ext.scala.syntax.DefnClass(value=dc):
            mods = dc.mods
            name = dc.name
            tparams = dc.tparams
            ctor = dc.ctor
            paramss = ctor.paramss
            @lru_cache(1)
            def tparams_expr() -> Maybe[hydra.ast.Expr]:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(tparams), (lambda : Nothing()), (lambda : hydra.lib.maybes.pure(hydra.serialization.bracket_list(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_type_param(x1)), tparams)))))
            @lru_cache(1)
            def params_expr() -> Maybe[hydra.ast.Expr]:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(paramss), (lambda : Nothing()), (lambda : hydra.lib.maybes.pure(hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: write_data_param(x1)), hydra.lib.lists.concat(paramss))))))
            @lru_cache(1)
            def name_and_params() -> hydra.ast.Expr:
                return hydra.serialization.no_sep(hydra.lib.maybes.cat((hydra.lib.maybes.pure(write_type_name(name)), tparams_expr(), params_expr())))
            return hydra.serialization.space_sep(hydra.lib.lists.concat((hydra.lib.lists.map((lambda x1: write_mod(x1)), mods), (hydra.serialization.cst("class"), name_and_params()))))

        case hydra.ext.scala.syntax.DefnEnum(value=de):
            name = de.name
            tparams = de.tparams
            template = de.template
            stats = template.stats
            @lru_cache(1)
            def enum_header() -> hydra.ast.Expr:
                return hydra.serialization.space_sep((hydra.serialization.cst("enum"), hydra.serialization.no_sep(hydra.lib.maybes.cat((hydra.lib.maybes.pure(write_type_name(name)), hydra.lib.logic.if_else(hydra.lib.lists.null(tparams), (lambda : Nothing()), (lambda : hydra.lib.maybes.pure(hydra.serialization.bracket_list(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_type_param(x1)), tparams)))))))), hydra.serialization.cst(":")))
            @lru_cache(1)
            def enum_cases() -> frozenlist[hydra.ast.Expr]:
                return hydra.lib.lists.map((lambda s: hydra.serialization.space_sep((hydra.serialization.cst("  "), write_stat(s)))), stats)
            return hydra.serialization.newline_sep(hydra.lib.lists.concat(((enum_header(),), enum_cases())))

        case hydra.ext.scala.syntax.DefnEnumCase(value=dec):
            name = dec.name
            ctor = dec.ctor
            inits = dec.inits
            paramss = ctor.paramss
            @lru_cache(1)
            def all_params() -> frozenlist[hydra.ext.scala.syntax.Data_Param]:
                return hydra.lib.lists.concat(paramss)
            @lru_cache(1)
            def params() -> hydra.ast.Expr:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(all_params()), (lambda : hydra.serialization.cst("")), (lambda : hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: write_data_param(x1)), all_params()))))
            @lru_cache(1)
            def extends_clause() -> hydra.ast.Expr:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(inits), (lambda : hydra.serialization.cst("")), (lambda : hydra.serialization.space_sep((hydra.serialization.cst("extends"), hydra.serialization.comma_sep(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_init(x1)), inits))))))
            return hydra.serialization.space_sep((hydra.serialization.cst("case"), hydra.serialization.no_sep((write_data_name(name), params())), extends_clause()))

        case _:
            raise TypeError("Unsupported Defn")

def write_pat(pat: hydra.ext.scala.syntax.Pat) -> hydra.ast.Expr:
    r"""Convert a pattern to an expression."""

    match pat:
        case hydra.ext.scala.syntax.PatExtract(value=pe):
            fun = pe.fun
            args = pe.args
            return hydra.lib.logic.if_else(hydra.lib.lists.null(args), (lambda : write_term(fun)), (lambda : hydra.serialization.no_sep((write_term(fun), hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: write_pat(x1)), args))))))

        case hydra.ext.scala.syntax.PatVar(value=pv):
            return write_data_name(pv.name)

        case hydra.ext.scala.syntax.PatWildcard():
            return hydra.serialization.cst("_")

        case _:
            raise TypeError("Unsupported Pat")

def write_stat(stat: hydra.ext.scala.syntax.Stat) -> hydra.ast.Expr:
    r"""Convert a statement to an expression."""

    match stat:
        case hydra.ext.scala.syntax.StatTerm(value=t):
            return write_term(t)

        case hydra.ext.scala.syntax.StatDefn(value=def_):
            return write_defn(def_)

        case hydra.ext.scala.syntax.StatImportExport(value=ie):
            return write_import_export_stat(ie)

        case _:
            raise TypeError("Unsupported Stat")

def write_term(term: hydra.ext.scala.syntax.Data) -> hydra.ast.Expr:
    r"""Convert a term to an expression."""

    match term:
        case hydra.ext.scala.syntax.DataLit(value=lit):
            return write_lit(lit)

        case hydra.ext.scala.syntax.DataRef(value=ref):
            return write_data_ref(ref)

        case hydra.ext.scala.syntax.DataApply(value=app):
            fun = app.fun
            args = app.args
            return hydra.serialization.no_sep((write_term(fun), hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: write_term(x1)), args))))

        case hydra.ext.scala.syntax.DataAssign(value=a):
            lhs = a.lhs
            rhs = a.rhs
            return hydra.serialization.space_sep((write_term(lhs), hydra.serialization.cst("->"), write_term(rhs)))

        case hydra.ext.scala.syntax.DataTuple(value=tup):
            return hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: write_term(x1)), tup.args))

        case hydra.ext.scala.syntax.DataMatch(value=m):
            expr = m.expr
            m_cases = m.cases
            return hydra.serialization.ifx(match_op, write_term(expr), hydra.serialization.newline_sep(hydra.lib.lists.map((lambda x1: write_case(x1)), m_cases)))

        case hydra.ext.scala.syntax.DataFunctionData(value=ft):
            return write_data_function_data(ft)

        case hydra.ext.scala.syntax.DataBlock(value=blk):
            stats = blk.stats
            return hydra.serialization.curly_block(hydra.serialization.full_block_style, hydra.serialization.newline_sep(hydra.lib.lists.map((lambda x1: write_stat(x1)), stats)))

        case _:
            raise TypeError("Unsupported Data")

def write_pkg(pkg: hydra.ext.scala.syntax.Pkg) -> hydra.ast.Expr:
    r"""Convert a package to an expression."""

    name = pkg.name
    stats = pkg.stats
    @lru_cache(1)
    def package() -> hydra.ast.Expr:
        return hydra.serialization.space_sep((hydra.serialization.cst("package"), write_data_name(name)))
    return hydra.serialization.double_newline_sep(hydra.lib.lists.concat(((package(),), hydra.lib.lists.map((lambda x1: write_stat(x1)), stats))))
