package hydra.ext.scala.utils

import hydra.core.*

import hydra.ext.scala.syntax.*

import hydra.module.*

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.logic

import hydra.lib.math

import hydra.lib.maybes

import hydra.lib.sets

import hydra.lib.strings

def nameOfType[T0](cx: T0)(t: hydra.core.Type): Option[hydra.core.Name] =
  hydra.rewriting.deannotateType(t) match
  case hydra.core.Type.variable(v_Type_variable_name) => Some(v_Type_variable_name)
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.ext.scala.utils.nameOfType(cx)(v_Type_forall_ft.body)
  case _ => None

def qualifyUnionFieldName(dlft: scala.Predef.String)(sname: Option[hydra.core.Name])(fname: hydra.core.Name): scala.Predef.String =
  hydra.lib.strings.cat2(hydra.lib.maybes.maybe[scala.Predef.String, hydra.core.Name](dlft)((n: hydra.core.Name) =>
  hydra.lib.strings.cat2(hydra.ext.scala.utils.scalaTypeName(true)(n))("."))(sname))(hydra.ext.scala.utils.scalaEscapeName(fname))

def sapply(fun: hydra.ext.scala.syntax.Data)(args: Seq[hydra.ext.scala.syntax.Data]): hydra.ext.scala.syntax.Data = hydra.ext.scala.syntax.Data.apply(hydra.ext.scala.syntax.Data_Apply(fun,
   args))

def sapplyTypes(fun: hydra.ext.scala.syntax.Data)(typeArgs: Seq[hydra.ext.scala.syntax.Type]): hydra.ext.scala.syntax.Data =
  {
  def typeToStr(t: hydra.ext.scala.syntax.Type): scala.Predef.String = hydra.ext.scala.utils.typeToString(t)
  lazy val typeStrings: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.ext.scala.syntax.Type, scala.Predef.String](typeToStr)(typeArgs)
  lazy val typeArgStr: scala.Predef.String = hydra.lib.strings.cat(Seq("[", hydra.lib.strings.intercalate(", ")(typeStrings), "]"))
  fun match
    case hydra.ext.scala.syntax.Data.ref(v_Data_ref_ref) => v_Data_ref_ref match
      case hydra.ext.scala.syntax.Data_Ref.name(v_Data_Ref_name_dn) => {
        lazy val nameStr: hydra.ext.scala.syntax.PredefString = (v_Data_Ref_name_dn.value)
        lazy val rawName: scala.Predef.String = nameStr
        hydra.ext.scala.utils.sname(hydra.lib.strings.cat2(rawName)(typeArgStr))
      }
      case _ => fun
    case _ => fun
}

def sassign(lhs: hydra.ext.scala.syntax.Data)(rhs: hydra.ext.scala.syntax.Data): hydra.ext.scala.syntax.Data = hydra.ext.scala.syntax.Data.assign(hydra.ext.scala.syntax.Data_Assign(lhs,
   rhs))

def scalaEscapeName(s: scala.Predef.String): scala.Predef.String =
  {
  lazy val sanitized: scala.Predef.String = hydra.lib.strings.fromList(hydra.lib.lists.map[Int, Int]((c: Int) =>
    hydra.lib.logic.ifElse[Int](hydra.lib.equality.equal[Int](c)(39))(95)(c))(hydra.lib.strings.toList(s)))
  lazy val sanitized2: scala.Predef.String = hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[scala.Predef.String](sanitized)("_"))("_x")(sanitized)
  lazy val sanitized3: scala.Predef.String = hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[scala.Predef.String](sanitized2)("toString"))("toString_")(sanitized2)
  lazy val needsBackticks: Boolean = hydra.lib.logic.or(hydra.lib.sets.member[scala.Predef.String](sanitized3)(hydra.ext.scala.utils.scalaReservedWords))(hydra.lib.logic.and(hydra.lib.equality.gt[Int](hydra.lib.strings.length(sanitized3))(0))(hydra.lib.equality.equal[Int](hydra.lib.strings.charAt(hydra.lib.math.sub(hydra.lib.strings.length(sanitized3))(1))(sanitized3))(95)))
  hydra.lib.logic.ifElse[scala.Predef.String](needsBackticks)(hydra.lib.strings.cat(Seq("`", sanitized3, "`")))(sanitized3)
}

lazy val scalaReservedWords: scala.collection.immutable.Set[scala.Predef.String] = hydra.ext.scala.language.scalaReservedWords

def scalaTypeName(qualify: Boolean)(name: hydra.core.Name): scala.Predef.String =
  hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.logic.or(qualify)(hydra.lib.sets.member[scala.Predef.String](hydra.names.localNameOf(name))(hydra.ext.scala.utils.scalaReservedWords)))(name)(hydra.names.localNameOf(name))

def slambda(v: scala.Predef.String)(body: hydra.ext.scala.syntax.Data)(sdom: Option[hydra.ext.scala.syntax.Type]): hydra.ext.scala.syntax.Data =
  hydra.ext.scala.syntax.Data.functionData(hydra.ext.scala.syntax.Data_FunctionData.function(hydra.ext.scala.syntax.Data_Function(Seq(hydra.ext.scala.syntax.Data_Param(Seq(),
     hydra.ext.scala.syntax.Name.value(v), sdom, None)), body)))

def sname(s: scala.Predef.String): hydra.ext.scala.syntax.Data =
  hydra.ext.scala.syntax.Data.ref(hydra.ext.scala.syntax.Data_Ref.name(hydra.ext.scala.syntax.Data_Name(s)))

def sprim(name: hydra.core.Name): hydra.ext.scala.syntax.Data =
  {
  lazy val qname: hydra.module.QualifiedName = hydra.names.qualifyName(name)
  lazy val prefix: scala.Predef.String = hydra.lib.maybes.fromJust[hydra.module.Namespace](qname.namespace)
  lazy val local: scala.Predef.String = hydra.ext.scala.utils.scalaEscapeName(qname.local)
  hydra.ext.scala.utils.sname(hydra.lib.strings.cat2(hydra.lib.strings.cat2(prefix)("."))(local))
}

def stapply(t: hydra.ext.scala.syntax.Type)(args: Seq[hydra.ext.scala.syntax.Type]): hydra.ext.scala.syntax.Type = hydra.ext.scala.syntax.Type.apply(hydra.ext.scala.syntax.Type_Apply(t,
   args))

def stapply1(t1: hydra.ext.scala.syntax.Type)(t2: hydra.ext.scala.syntax.Type): hydra.ext.scala.syntax.Type = hydra.ext.scala.utils.stapply(t1)(Seq(t2))

def stapply2(t1: hydra.ext.scala.syntax.Type)(t2: hydra.ext.scala.syntax.Type)(t3: hydra.ext.scala.syntax.Type): hydra.ext.scala.syntax.Type = hydra.ext.scala.utils.stapply(t1)(Seq(t2,
   t3))

def stparam(name: hydra.core.Name): hydra.ext.scala.syntax.Type_Param =
  {
  lazy val v: scala.Predef.String = hydra.formatting.capitalize(name)
  hydra.ext.scala.syntax.Type_Param(Seq(), hydra.ext.scala.syntax.Name.value(v), Seq(), Seq(), Seq(), Seq())
}

def stref(s: scala.Predef.String): hydra.ext.scala.syntax.Type =
  hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name(s)))

def svar(name: hydra.core.Name): hydra.ext.scala.syntax.Pat =
  {
  lazy val v: scala.Predef.String = name
  hydra.ext.scala.syntax.Pat.`var`(hydra.ext.scala.syntax.Pat_Var(hydra.ext.scala.syntax.Data_Name(v)))
}

def typeToString(t: hydra.ext.scala.syntax.Type): scala.Predef.String =
  t match
  case hydra.ext.scala.syntax.Type.ref(v_Type_ref_tr) => v_Type_ref_tr match
    case hydra.ext.scala.syntax.Type_Ref.name(v_Type_Ref_name_tn) => (v_Type_Ref_name_tn.value)
    case _ => "Any"
  case hydra.ext.scala.syntax.Type.`var`(v_Type_var_tv) => (v_Type_var_tv.name.value)
  case hydra.ext.scala.syntax.Type.functionType(v_Type_functionType_ft) => v_Type_functionType_ft match
    case hydra.ext.scala.syntax.Type_FunctionType.function(v_Type_FunctionType_function_fn) => {
      lazy val params: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.ext.scala.syntax.Type, scala.Predef.String](hydra.ext.scala.utils.typeToString)(v_Type_FunctionType_function_fn.params)
      lazy val res: scala.Predef.String = hydra.ext.scala.utils.typeToString(v_Type_FunctionType_function_fn.res)
      hydra.lib.strings.cat(Seq("(", hydra.lib.strings.intercalate(", ")(params), ") => ", res))
    }
    case _ => "Any"
  case hydra.ext.scala.syntax.Type.apply(v_Type_apply_ta) => {
    lazy val base: scala.Predef.String = hydra.ext.scala.utils.typeToString(v_Type_apply_ta.tpe)
    lazy val argStrs: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.ext.scala.syntax.Type, scala.Predef.String](hydra.ext.scala.utils.typeToString)(v_Type_apply_ta.args)
    hydra.lib.strings.cat(Seq(base, "[", hydra.lib.strings.intercalate(", ")(argStrs), "]"))
  }
  case _ => "Any"
