package hydra.ext.scala.serde

import hydra.ast.*

import hydra.ext.scala.syntax.*

lazy val dotOp: hydra.ast.Op = hydra.ast.Op(".", hydra.ast.Padding(hydra.ast.Ws.none, hydra.ast.Ws.none), 0, hydra.ast.Associativity.left)

lazy val functionArrowOp: hydra.ast.Op = hydra.serialization.op("=>")(hydra.lib.math.negate(1))(hydra.ast.Associativity.right)

lazy val matchOp: hydra.ast.Op = hydra.ast.Op("match", hydra.ast.Padding(hydra.ast.Ws.space, hydra.ast.Ws.breakAndIndent("  ")),
   0, hydra.ast.Associativity.none)

def writeCase(c: hydra.ext.scala.syntax.Case): hydra.ast.Expr =
  {
  lazy val pat: hydra.ext.scala.syntax.Pat = (c.pat)
  lazy val term: hydra.ext.scala.syntax.Data = (c.body)
  hydra.serialization.spaceSep(Seq(hydra.serialization.cst("case"), hydra.ext.scala.serde.writePat(pat),
     hydra.serialization.cst("=>"), hydra.ext.scala.serde.writeTerm(term)))
}

def writeData_FunctionData(ft: hydra.ext.scala.syntax.Data_FunctionData): hydra.ast.Expr =
  ft match
  case hydra.ext.scala.syntax.Data_FunctionData.function(v_Data_FunctionData_function_f) => {
    lazy val params: Seq[hydra.ext.scala.syntax.Data_Param] = (v_Data_FunctionData_function_f.params)
    lazy val body: hydra.ext.scala.syntax.Data = (v_Data_FunctionData_function_f.body)
    lazy val bodyExpr: hydra.ast.Expr = hydra.ext.scala.serde.writeTerm(body)
    lazy val bodyLen: Int = hydra.serialization.expressionLength(bodyExpr)
    hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.equality.gt[Int](bodyLen)(60))(hydra.serialization.noSep(Seq(hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.ext.scala.syntax.Data_Param,
       hydra.ast.Expr](hydra.ext.scala.serde.writeData_Param)(params)), hydra.serialization.cst(" =>\n  "),
       bodyExpr)))(hydra.serialization.spaceSep(Seq(hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.ext.scala.syntax.Data_Param,
       hydra.ast.Expr](hydra.ext.scala.serde.writeData_Param)(params)), hydra.serialization.cst("=>"),
       bodyExpr)))
  }

def writeData_Name(dn: hydra.ext.scala.syntax.Data_Name): hydra.ast.Expr = hydra.serialization.cst(dn.value)

def writeData_Param(dp: hydra.ext.scala.syntax.Data_Param): hydra.ast.Expr =
  {
  lazy val name: hydra.ext.scala.syntax.Name = (dp.name)
  lazy val stype: Option[hydra.ext.scala.syntax.Type] = (dp.decltpe)
  hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.maybes.pure[hydra.ast.Expr](hydra.ext.scala.serde.writeName(name)),
     hydra.lib.maybes.map[hydra.ext.scala.syntax.Type, hydra.ast.Expr]((t: hydra.ext.scala.syntax.Type) =>
    hydra.serialization.spaceSep(Seq(hydra.serialization.cst(":"), hydra.ext.scala.serde.writeType(t))))(stype))))
}

def writeData_Ref(ref: hydra.ext.scala.syntax.Data_Ref): hydra.ast.Expr =
  ref match
  case hydra.ext.scala.syntax.Data_Ref.name(v_Data_Ref_name_name) => hydra.ext.scala.serde.writeData_Name(v_Data_Ref_name_name)
  case hydra.ext.scala.syntax.Data_Ref.select(v_Data_Ref_select_sel) => hydra.ext.scala.serde.writeData_Select(v_Data_Ref_select_sel)

def writeData_Select(sel: hydra.ext.scala.syntax.Data_Select): hydra.ast.Expr =
  {
  lazy val arg: hydra.ext.scala.syntax.Data = (sel.qual)
  lazy val name: hydra.ext.scala.syntax.Data_Name = (sel.name)
  hydra.serialization.ifx(hydra.ext.scala.serde.dotOp)(hydra.ext.scala.serde.writeTerm(arg))(hydra.ext.scala.serde.writeTerm(hydra.ext.scala.syntax.Data.ref(hydra.ext.scala.syntax.Data_Ref.name(name))))
}

def writeDefn(`def`: hydra.ext.scala.syntax.Defn): hydra.ast.Expr =
  `def` match
  case hydra.ext.scala.syntax.Defn.`def`(v_Defn_def_dd) => {
    lazy val name: hydra.ext.scala.syntax.Data_Name = (v_Defn_def_dd.name)
    lazy val tparams: Seq[hydra.ext.scala.syntax.Type_Param] = (v_Defn_def_dd.tparams)
    lazy val paramss: Seq[Seq[hydra.ext.scala.syntax.Data_Param]] = (v_Defn_def_dd.paramss)
    lazy val scod: Option[hydra.ext.scala.syntax.Type] = (v_Defn_def_dd.decltpe)
    lazy val body: hydra.ext.scala.syntax.Data = (v_Defn_def_dd.body)
    lazy val tparamsExpr: Option[hydra.ast.Expr] = hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.scala.syntax.Type_Param](tparams))(None)(hydra.lib.maybes.pure[hydra.ast.Expr](hydra.serialization.bracketList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.scala.syntax.Type_Param,
       hydra.ast.Expr](hydra.ext.scala.serde.writeType_Param)(tparams))))
    lazy val scodExpr: Option[hydra.ast.Expr] = hydra.lib.maybes.map[hydra.ext.scala.syntax.Type, hydra.ast.Expr]((t: hydra.ext.scala.syntax.Type) =>
      hydra.serialization.spaceSep(Seq(hydra.serialization.cst(":"), hydra.ext.scala.serde.writeType(t))))(scod)
    lazy val paramssExprs: Seq[hydra.ast.Expr] = hydra.lib.lists.map[Seq[hydra.ext.scala.syntax.Data_Param],
       hydra.ast.Expr]((ps: Seq[hydra.ext.scala.syntax.Data_Param]) =>
      hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.ext.scala.syntax.Data_Param, hydra.ast.Expr](hydra.ext.scala.serde.writeData_Param)(ps)))(paramss)
    lazy val nameAndParams: hydra.ast.Expr = hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](hydra.lib.lists.concat[Option[hydra.ast.Expr]](Seq(Seq(hydra.lib.maybes.pure[hydra.ast.Expr](hydra.ext.scala.serde.writeData_Name(name))),
       Seq(tparamsExpr), hydra.lib.lists.map[hydra.ast.Expr, Option[hydra.ast.Expr]]((pe: hydra.ast.Expr) => hydra.lib.maybes.pure[hydra.ast.Expr](pe))(paramssExprs),
       Seq(scodExpr)))))
    {
      lazy val bodyExpr: hydra.ast.Expr = hydra.ext.scala.serde.writeTerm(body)
      {
        lazy val defSig: hydra.ast.Expr = hydra.serialization.spaceSep(Seq(hydra.serialization.cst("def"), nameAndParams, hydra.serialization.cst("=")))
        {
          lazy val bodyLen: Int = hydra.serialization.expressionLength(bodyExpr)
          hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.equality.gt[Int](bodyLen)(80))(hydra.serialization.noSep(Seq(defSig,
             hydra.serialization.cst("\n  "), bodyExpr)))(hydra.serialization.spaceSep(Seq(defSig, bodyExpr)))
        }
      }
    }
  }
  case hydra.ext.scala.syntax.Defn.`type`(v_Defn_type_dt) => {
    lazy val name: hydra.ext.scala.syntax.Type_Name = (v_Defn_type_dt.name)
    lazy val tparams: Seq[hydra.ext.scala.syntax.Type_Param] = (v_Defn_type_dt.tparams)
    lazy val body: hydra.ext.scala.syntax.Type = (v_Defn_type_dt.body)
    hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.maybes.pure[hydra.ast.Expr](hydra.serialization.cst("type")),
       hydra.lib.maybes.pure[hydra.ast.Expr](hydra.ext.scala.serde.writeType_Name(name)), hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.scala.syntax.Type_Param](tparams))(None)(hydra.lib.maybes.pure[hydra.ast.Expr](hydra.serialization.bracketList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.scala.syntax.Type_Param,
       hydra.ast.Expr](hydra.ext.scala.serde.writeType_Param)(tparams)))), hydra.lib.maybes.pure[hydra.ast.Expr](hydra.serialization.cst("=")),
       hydra.lib.maybes.pure[hydra.ast.Expr](hydra.ext.scala.serde.writeType(body)))))
  }
  case hydra.ext.scala.syntax.Defn.`val`(v_Defn_val_dv) => {
    lazy val mods: Seq[hydra.ext.scala.syntax.Mod] = (v_Defn_val_dv.mods)
    lazy val pats: Seq[hydra.ext.scala.syntax.Pat] = (v_Defn_val_dv.pats)
    lazy val typ: Option[hydra.ext.scala.syntax.Type] = (v_Defn_val_dv.decltpe)
    lazy val rhs: hydra.ext.scala.syntax.Data = (v_Defn_val_dv.rhs)
    lazy val firstPat: hydra.ext.scala.syntax.Pat = hydra.lib.lists.head[hydra.ext.scala.syntax.Pat](pats)
    lazy val patName: hydra.ext.scala.syntax.Data_Name = firstPat match
      case hydra.ext.scala.syntax.Pat.`var`(v_Pat_var_pv) => (v_Pat_var_pv.name)
    lazy val nameStr: scala.Predef.String = (patName.value)
    lazy val nameAndType: hydra.ast.Expr = hydra.lib.maybes.maybe[hydra.ast.Expr, hydra.ext.scala.syntax.Type](hydra.serialization.cst(nameStr))((t: hydra.ext.scala.syntax.Type) =>
      hydra.serialization.spaceSep(Seq(hydra.serialization.cst(hydra.lib.strings.cat2(nameStr)(":")), hydra.ext.scala.serde.writeType(t))))(typ)
    lazy val valKeyword: scala.Predef.String = hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.lists.`null`[hydra.ext.scala.syntax.Mod](mods))("val")("lazy val")
    hydra.serialization.spaceSep(Seq(hydra.serialization.cst(valKeyword), nameAndType, hydra.serialization.cst("="), hydra.ext.scala.serde.writeTerm(rhs)))
  }
  case hydra.ext.scala.syntax.Defn.`class`(v_Defn_class_dc) => {
    lazy val mods: Seq[hydra.ext.scala.syntax.Mod] = (v_Defn_class_dc.mods)
    lazy val name: hydra.ext.scala.syntax.Type_Name = (v_Defn_class_dc.name)
    lazy val tparams: Seq[hydra.ext.scala.syntax.Type_Param] = (v_Defn_class_dc.tparams)
    lazy val ctor: hydra.ext.scala.syntax.Ctor_Primary = (v_Defn_class_dc.ctor)
    lazy val paramss: Seq[Seq[hydra.ext.scala.syntax.Data_Param]] = (ctor.paramss)
    lazy val tparamsExpr: Option[hydra.ast.Expr] = hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.scala.syntax.Type_Param](tparams))(None)(hydra.lib.maybes.pure[hydra.ast.Expr](hydra.serialization.bracketList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.scala.syntax.Type_Param,
       hydra.ast.Expr](hydra.ext.scala.serde.writeType_Param)(tparams))))
    lazy val paramsExpr: Option[hydra.ast.Expr] = hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[Seq[hydra.ext.scala.syntax.Data_Param]](paramss))(None)(hydra.lib.maybes.pure[hydra.ast.Expr](hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.ext.scala.syntax.Data_Param,
       hydra.ast.Expr](hydra.ext.scala.serde.writeData_Param)(hydra.lib.lists.concat[hydra.ext.scala.syntax.Data_Param](paramss)))))
    lazy val nameAndParams: hydra.ast.Expr = hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.maybes.pure[hydra.ast.Expr](hydra.ext.scala.serde.writeType_Name(name)),
       tparamsExpr, paramsExpr)))
    hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(hydra.lib.lists.map[hydra.ext.scala.syntax.Mod,
       hydra.ast.Expr](hydra.ext.scala.serde.writeMod)(mods), Seq(hydra.serialization.cst("class"), nameAndParams))))
  }
  case hydra.ext.scala.syntax.Defn.`enum`(v_Defn_enum_de) => {
    lazy val name: hydra.ext.scala.syntax.Type_Name = (v_Defn_enum_de.name)
    lazy val tparams: Seq[hydra.ext.scala.syntax.Type_Param] = (v_Defn_enum_de.tparams)
    lazy val template: hydra.ext.scala.syntax.Template = (v_Defn_enum_de.template)
    lazy val stats: Seq[hydra.ext.scala.syntax.Stat] = (template.stats)
    lazy val enumHeader: hydra.ast.Expr = hydra.serialization.spaceSep(Seq(hydra.serialization.cst("enum"),
       hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.maybes.pure[hydra.ast.Expr](hydra.ext.scala.serde.writeType_Name(name)),
       hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.scala.syntax.Type_Param](tparams))(None)(hydra.lib.maybes.pure[hydra.ast.Expr](hydra.serialization.bracketList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.scala.syntax.Type_Param,
       hydra.ast.Expr](hydra.ext.scala.serde.writeType_Param)(tparams))))))), hydra.serialization.cst(":")))
    lazy val enumCases: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.ext.scala.syntax.Stat, hydra.ast.Expr]((s: hydra.ext.scala.syntax.Stat) =>
      hydra.serialization.spaceSep(Seq(hydra.serialization.cst("  "), hydra.ext.scala.serde.writeStat(s))))(stats)
    hydra.serialization.newlineSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(enumHeader), enumCases)))
  }
  case hydra.ext.scala.syntax.Defn.enumCase(v_Defn_enumCase_dec) => {
    lazy val name: hydra.ext.scala.syntax.Data_Name = (v_Defn_enumCase_dec.name)
    lazy val ctor: hydra.ext.scala.syntax.Ctor_Primary = (v_Defn_enumCase_dec.ctor)
    lazy val inits: Seq[hydra.ext.scala.syntax.Init] = (v_Defn_enumCase_dec.inits)
    lazy val paramss: Seq[Seq[hydra.ext.scala.syntax.Data_Param]] = (ctor.paramss)
    lazy val allParams: Seq[hydra.ext.scala.syntax.Data_Param] = hydra.lib.lists.concat[hydra.ext.scala.syntax.Data_Param](paramss)
    lazy val params: hydra.ast.Expr = hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.lists.`null`[hydra.ext.scala.syntax.Data_Param](allParams))(hydra.serialization.cst(""))(hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.ext.scala.syntax.Data_Param,
       hydra.ast.Expr](hydra.ext.scala.serde.writeData_Param)(allParams)))
    lazy val extendsClause: hydra.ast.Expr = hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.lists.`null`[hydra.ext.scala.syntax.Init](inits))(hydra.serialization.cst(""))(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("extends"),
       hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.scala.syntax.Init,
       hydra.ast.Expr](hydra.ext.scala.serde.writeInit)(inits)))))
    hydra.serialization.spaceSep(Seq(hydra.serialization.cst("case"), hydra.serialization.noSep(Seq(hydra.ext.scala.serde.writeData_Name(name),
       params)), extendsClause))
  }

def writeImportExportStat(ie: hydra.ext.scala.syntax.ImportExportStat): hydra.ast.Expr =
  ie match
  case hydra.ext.scala.syntax.ImportExportStat.`import`(v_ImportExportStat_import_imp) => {
    lazy val importers: Seq[hydra.ext.scala.syntax.Importer] = (v_ImportExportStat_import_imp.importers)
    hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.ext.scala.syntax.Importer, hydra.ast.Expr](hydra.ext.scala.serde.writeImporter)(importers))
  }

def writeImporter(imp: hydra.ext.scala.syntax.Importer): hydra.ast.Expr =
  {
  lazy val ref: hydra.ext.scala.syntax.Data_Ref = (imp.ref)
  lazy val importees: Seq[hydra.ext.scala.syntax.Importee] = (imp.importees)
  lazy val refName: scala.Predef.String = ref match
    case hydra.ext.scala.syntax.Data_Ref.name(v_Data_Ref_name_dn) => (v_Data_Ref_name_dn.value)
  lazy val forImportees: hydra.ast.Expr = hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.lists.`null`[hydra.ext.scala.syntax.Importee](importees))(hydra.serialization.cst(""))(hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.ext.scala.syntax.Importee](importees))(1))(hydra.serialization.noSep(Seq(hydra.serialization.cst("."),
     hydra.lib.lists.head[hydra.ext.scala.syntax.Importee](importees) match
    case hydra.ext.scala.syntax.Importee.wildcard => hydra.serialization.cst("*")
    case hydra.ext.scala.syntax.Importee.name(v_Importee_name_in) => hydra.serialization.cst(v_Importee_name_in.name match
      case hydra.ext.scala.syntax.Name.value(v_Name_value_s) => v_Name_value_s))))(hydra.serialization.noSep(Seq(hydra.serialization.cst("."),
         hydra.serialization.curlyBracesList(None)(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.scala.syntax.Importee,
         hydra.ast.Expr]((it: hydra.ext.scala.syntax.Importee) =>
    it match
    case hydra.ext.scala.syntax.Importee.wildcard => hydra.serialization.cst("*")
    case hydra.ext.scala.syntax.Importee.name(v_Importee_name_in) => hydra.serialization.cst(v_Importee_name_in.name match
      case hydra.ext.scala.syntax.Name.value(v_Name_value_s) => v_Name_value_s))(importees))))))
  hydra.serialization.spaceSep(Seq(hydra.serialization.cst("import"), hydra.serialization.noSep(Seq(hydra.serialization.cst(refName), forImportees))))
}

def writeInit(init: hydra.ext.scala.syntax.Init): hydra.ast.Expr = hydra.ext.scala.serde.writeType(init.tpe)

def writeLit(lit: hydra.ext.scala.syntax.Lit): hydra.ast.Expr =
  lit match
  case hydra.ext.scala.syntax.Lit.boolean(v_Lit_boolean_b) => hydra.serialization.cst(hydra.lib.logic.ifElse[scala.Predef.String](v_Lit_boolean_b)("true")("false"))
  case hydra.ext.scala.syntax.Lit.byte(v_Lit_byte_i) => hydra.serialization.cst(hydra.lib.strings.cat2(hydra.lib.literals.showInt8(v_Lit_byte_i))(".toByte"))
  case hydra.ext.scala.syntax.Lit.short(v_Lit_short_i) => hydra.serialization.cst(hydra.lib.strings.cat2(hydra.lib.literals.showInt16(v_Lit_short_i))(".toShort"))
  case hydra.ext.scala.syntax.Lit.int(v_Lit_int_i) => hydra.serialization.cst(hydra.lib.literals.showInt32(v_Lit_int_i))
  case hydra.ext.scala.syntax.Lit.long(v_Lit_long_i) => hydra.serialization.cst(hydra.lib.strings.cat2(hydra.lib.literals.showInt64(v_Lit_long_i))("L"))
  case hydra.ext.scala.syntax.Lit.float(v_Lit_float_f) => hydra.serialization.cst(hydra.lib.strings.cat2(hydra.lib.literals.showFloat32(v_Lit_float_f))("f"))
  case hydra.ext.scala.syntax.Lit.double(v_Lit_double_f) => hydra.serialization.cst(hydra.lib.literals.showFloat64(v_Lit_double_f))
  case hydra.ext.scala.syntax.Lit.unit => hydra.serialization.cst("()")
  case hydra.ext.scala.syntax.Lit.string(v_Lit_string_s) => hydra.serialization.cst(hydra.lib.strings.cat2("\"")(hydra.lib.strings.cat2(hydra.ext.java.serde.escapeJavaString(v_Lit_string_s))("\"")))
  case hydra.ext.scala.syntax.Lit.bytes(v_Lit_bytes_bs) => hydra.serialization.cst(hydra.lib.strings.cat2("Array[Byte](")(hydra.lib.strings.cat2(hydra.lib.strings.intercalate(", ")(hydra.lib.lists.map[Int,
     scala.Predef.String]((b: Int) =>
    hydra.lib.strings.cat2(hydra.lib.literals.showInt32(b))(".toByte"))(v_Lit_bytes_bs)))(")")))
  case _ => hydra.serialization.cst("TODO:literal")

def writeMod(m: hydra.ext.scala.syntax.Mod): hydra.ast.Expr =
  m match
  case hydra.ext.scala.syntax.Mod.`case` => hydra.serialization.cst("case")
  case hydra.ext.scala.syntax.Mod.`sealed` => hydra.serialization.cst("sealed")
  case hydra.ext.scala.syntax.Mod.`abstract` => hydra.serialization.cst("abstract")
  case hydra.ext.scala.syntax.Mod.`final` => hydra.serialization.cst("final")
  case hydra.ext.scala.syntax.Mod.`override` => hydra.serialization.cst("override")
  case hydra.ext.scala.syntax.Mod.`implicit` => hydra.serialization.cst("implicit")
  case hydra.ext.scala.syntax.Mod.`lazy` => hydra.serialization.cst("lazy")
  case hydra.ext.scala.syntax.Mod.`private`(v_Mod_private__) => hydra.serialization.cst("private")
  case hydra.ext.scala.syntax.Mod.`protected`(v_Mod_protected__) => hydra.serialization.cst("protected")

def writeName(name: hydra.ext.scala.syntax.Name): hydra.ast.Expr =
  name match
  case hydra.ext.scala.syntax.Name.value(v_Name_value_s) => hydra.serialization.cst(v_Name_value_s)

def writePat(pat: hydra.ext.scala.syntax.Pat): hydra.ast.Expr =
  pat match
  case hydra.ext.scala.syntax.Pat.extract(v_Pat_extract_pe) => {
    lazy val fun: hydra.ext.scala.syntax.Data = (v_Pat_extract_pe.fun)
    lazy val args: Seq[hydra.ext.scala.syntax.Pat] = (v_Pat_extract_pe.args)
    hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.lists.`null`[hydra.ext.scala.syntax.Pat](args))(hydra.ext.scala.serde.writeTerm(fun))(hydra.serialization.noSep(Seq(hydra.ext.scala.serde.writeTerm(fun),
       hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.ext.scala.syntax.Pat, hydra.ast.Expr](hydra.ext.scala.serde.writePat)(args)))))
  }
  case hydra.ext.scala.syntax.Pat.`var`(v_Pat_var_pv) => hydra.ext.scala.serde.writeData_Name(v_Pat_var_pv.name)
  case hydra.ext.scala.syntax.Pat.wildcard => hydra.serialization.cst("_")

def writePkg(pkg: hydra.ext.scala.syntax.Pkg): hydra.ast.Expr =
  {
  lazy val name: hydra.ext.scala.syntax.Data_Name = (pkg.name)
  lazy val stats: Seq[hydra.ext.scala.syntax.Stat] = (pkg.stats)
  lazy val `package`: hydra.ast.Expr = hydra.serialization.spaceSep(Seq(hydra.serialization.cst("package"), hydra.ext.scala.serde.writeData_Name(name)))
  hydra.serialization.doubleNewlineSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(`package`), hydra.lib.lists.map[hydra.ext.scala.syntax.Stat,
     hydra.ast.Expr](hydra.ext.scala.serde.writeStat)(stats))))
}

def writeStat(stat: hydra.ext.scala.syntax.Stat): hydra.ast.Expr =
  stat match
  case hydra.ext.scala.syntax.Stat.term(v_Stat_term_t) => hydra.ext.scala.serde.writeTerm(v_Stat_term_t)
  case hydra.ext.scala.syntax.Stat.defn(v_Stat_defn_def) => hydra.ext.scala.serde.writeDefn(v_Stat_defn_def)
  case hydra.ext.scala.syntax.Stat.importExport(v_Stat_importExport_ie) => hydra.ext.scala.serde.writeImportExportStat(v_Stat_importExport_ie)

def writeTerm(term: hydra.ext.scala.syntax.Data): hydra.ast.Expr =
  term match
  case hydra.ext.scala.syntax.Data.lit(v_Data_lit_lit) => hydra.ext.scala.serde.writeLit(v_Data_lit_lit)
  case hydra.ext.scala.syntax.Data.ref(v_Data_ref_ref) => hydra.ext.scala.serde.writeData_Ref(v_Data_ref_ref)
  case hydra.ext.scala.syntax.Data.apply(v_Data_apply_app) => {
    lazy val fun: hydra.ext.scala.syntax.Data = (v_Data_apply_app.fun)
    lazy val args: Seq[hydra.ext.scala.syntax.Data] = (v_Data_apply_app.args)
    hydra.serialization.noSep(Seq(hydra.ext.scala.serde.writeTerm(fun), hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.ext.scala.syntax.Data,
       hydra.ast.Expr](hydra.ext.scala.serde.writeTerm)(args))))
  }
  case hydra.ext.scala.syntax.Data.assign(v_Data_assign_a) => {
    lazy val lhs: hydra.ext.scala.syntax.Data = (v_Data_assign_a.lhs)
    lazy val rhs: hydra.ext.scala.syntax.Data = (v_Data_assign_a.rhs)
    hydra.serialization.spaceSep(Seq(hydra.ext.scala.serde.writeTerm(lhs), hydra.serialization.cst("->"), hydra.ext.scala.serde.writeTerm(rhs)))
  }
  case hydra.ext.scala.syntax.Data.tuple(v_Data_tuple_tup) => hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.ext.scala.syntax.Data,
     hydra.ast.Expr](hydra.ext.scala.serde.writeTerm)(v_Data_tuple_tup.args))
  case hydra.ext.scala.syntax.Data.`match`(v_Data_match_m) => {
    lazy val expr: hydra.ext.scala.syntax.Data = (v_Data_match_m.expr)
    lazy val mCases: Seq[hydra.ext.scala.syntax.Case] = (v_Data_match_m.cases)
    hydra.serialization.ifx(hydra.ext.scala.serde.matchOp)(hydra.ext.scala.serde.writeTerm(expr))(hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.ext.scala.syntax.Case,
       hydra.ast.Expr](hydra.ext.scala.serde.writeCase)(mCases)))
  }
  case hydra.ext.scala.syntax.Data.functionData(v_Data_functionData_ft) => hydra.ext.scala.serde.writeData_FunctionData(v_Data_functionData_ft)
  case hydra.ext.scala.syntax.Data.block(v_Data_block_blk) => {
    lazy val stats: Seq[hydra.ext.scala.syntax.Stat] = (v_Data_block_blk.stats)
    hydra.serialization.curlyBlock(hydra.serialization.fullBlockStyle)(hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.ext.scala.syntax.Stat,
       hydra.ast.Expr](hydra.ext.scala.serde.writeStat)(stats)))
  }

def writeType(typ: hydra.ext.scala.syntax.Type): hydra.ast.Expr =
  typ match
  case hydra.ext.scala.syntax.Type.ref(v_Type_ref_tr) => v_Type_ref_tr match
    case hydra.ext.scala.syntax.Type_Ref.name(v_Type_Ref_name_name) => hydra.ext.scala.serde.writeType_Name(v_Type_Ref_name_name)
  case hydra.ext.scala.syntax.Type.apply(v_Type_apply_ta) => {
    lazy val fun: hydra.ext.scala.syntax.Type = (v_Type_apply_ta.tpe)
    lazy val args: Seq[hydra.ext.scala.syntax.Type] = (v_Type_apply_ta.args)
    hydra.serialization.noSep(Seq(hydra.ext.scala.serde.writeType(fun), hydra.serialization.bracketList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.scala.syntax.Type,
       hydra.ast.Expr](hydra.ext.scala.serde.writeType)(args))))
  }
  case hydra.ext.scala.syntax.Type.functionType(v_Type_functionType_ft) => v_Type_functionType_ft match
    case hydra.ext.scala.syntax.Type_FunctionType.function(v_Type_FunctionType_function_tf) => {
      lazy val dom: hydra.ext.scala.syntax.Type = hydra.lib.lists.head[hydra.ext.scala.syntax.Type](v_Type_FunctionType_function_tf.params)
      lazy val cod: hydra.ext.scala.syntax.Type = (v_Type_FunctionType_function_tf.res)
      hydra.serialization.ifx(hydra.ext.scala.serde.functionArrowOp)(hydra.ext.scala.serde.writeType(dom))(hydra.ext.scala.serde.writeType(cod))
    }
  case hydra.ext.scala.syntax.Type.lambda(v_Type_lambda_tl) => {
    lazy val params: Seq[hydra.ext.scala.syntax.Type_Param] = (v_Type_lambda_tl.tparams)
    lazy val body: hydra.ext.scala.syntax.Type = (v_Type_lambda_tl.tpe)
    hydra.serialization.noSep(Seq(hydra.ext.scala.serde.writeType(body), hydra.serialization.bracketList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.scala.syntax.Type_Param,
       hydra.ast.Expr](hydra.ext.scala.serde.writeType_Param)(params))))
  }
  case hydra.ext.scala.syntax.Type.`var`(v_Type_var_tv) => hydra.ext.scala.serde.writeType_Name(v_Type_var_tv.name)

def writeType_Name(tn: hydra.ext.scala.syntax.Type_Name): hydra.ast.Expr = hydra.serialization.cst(tn.value)

def writeType_Param(tp: hydra.ext.scala.syntax.Type_Param): hydra.ast.Expr = hydra.ext.scala.serde.writeName(tp.name)
