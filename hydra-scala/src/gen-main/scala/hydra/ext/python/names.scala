package hydra.ext.python.names

import hydra.core.*

import hydra.ext.python.helpers.*

import hydra.ext.python.syntax.*

import hydra.module.*

import hydra.util.*

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.strings

val useFutureAnnotations: Boolean = true

def encodeConstantForFieldName[T0, T1](env: T0)(tname: T1)(fname: hydra.core.Name): hydra.ext.python.syntax.Name =
  hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(hydra.util.CaseConvention.upperSnake)(fname)

def encodeConstantForTypeName[T0, T1](env: T0)(tname: T1): hydra.ext.python.syntax.Name = "TYPE_"

def encodeEnumValue(v1: hydra.ext.python.helpers.PythonEnvironment)(v2: hydra.core.Name): hydra.ext.python.syntax.Name =
  hydra.ext.python.names.encodeName(false)(hydra.util.CaseConvention.upperSnake)(v1)(v2)

def encodeFieldName(env: hydra.ext.python.helpers.PythonEnvironment)(fname: hydra.core.Name): hydra.ext.python.syntax.Name =
  hydra.ext.python.names.encodeName(false)(hydra.util.CaseConvention.lowerSnake)(env)(fname)

def encodeName(isQualified: Boolean)(conv: hydra.util.CaseConvention)(env: hydra.ext.python.helpers.PythonEnvironment)(name: hydra.core.Name): hydra.ext.python.syntax.Name =
  {
  val namespaces: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName] = (env.namespaces)
  val focusPair: Tuple2[hydra.module.Namespace, hydra.ext.python.syntax.DottedName] = (namespaces.focus)
  val focusNs: hydra.module.Namespace = pairs.first[hydra.module.Namespace, hydra.ext.python.syntax.DottedName](focusPair)
  val boundVars: Map[hydra.core.Name, hydra.ext.python.syntax.Name] = pairs.second[Seq[hydra.core.Name],
     Map[hydra.core.Name, hydra.ext.python.syntax.Name]](env.boundTypeVariables)
  val qualName: hydra.module.QualifiedName = hydra.names.qualifyName(name)
  val mns: Option[hydra.module.Namespace] = (qualName.namespace)
  val local: scala.Predef.String = (qualName.local)
  val pyLocal: scala.Predef.String = hydra.ext.python.names.sanitizePythonName(hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(conv)(local))
  def pyNs(nsVal: hydra.module.Namespace): scala.Predef.String =
    strings.intercalate(".")(lists.map[scala.Predef.String, scala.Predef.String]((v1: scala.Predef.String) =>
    hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(hydra.util.CaseConvention.lowerSnake)(v1))(strings.splitOn(".")(nsVal)))
  logic.ifElse[hydra.ext.python.syntax.Name](isQualified)(maybes.maybe[hydra.ext.python.syntax.Name, hydra.ext.python.syntax.Name](logic.ifElse[hydra.ext.python.syntax.Name](equality.equal[Option[hydra.module.Namespace]](mns)(Some(focusNs)))(logic.ifElse[scala.Predef.String](hydra.ext.python.names.useFutureAnnotations)(pyLocal)(hydra.ext.python.serde.escapePythonString(true)(pyLocal)))(maybes.maybe[hydra.ext.python.syntax.Name,
     hydra.module.Namespace](pyLocal)((nsVal: hydra.module.Namespace) => strings.cat2(pyNs(nsVal))(strings.cat2(".")(pyLocal)))(mns)))((n: hydra.ext.python.syntax.Name) => n)(maps.lookup[hydra.core.Name,
     hydra.ext.python.syntax.Name](name)(boundVars)))(pyLocal)
}

def encodeNameQualified(env: hydra.ext.python.helpers.PythonEnvironment)(name: hydra.core.Name): hydra.ext.python.syntax.Name =
  {
  val namespaces: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName] = (env.namespaces)
  val focusPair: Tuple2[hydra.module.Namespace, hydra.ext.python.syntax.DottedName] = (namespaces.focus)
  val focusNs: hydra.module.Namespace = pairs.first[hydra.module.Namespace, hydra.ext.python.syntax.DottedName](focusPair)
  val boundVars: Map[hydra.core.Name, hydra.ext.python.syntax.Name] = pairs.second[Seq[hydra.core.Name],
     Map[hydra.core.Name, hydra.ext.python.syntax.Name]](env.boundTypeVariables)
  val qualName: hydra.module.QualifiedName = hydra.names.qualifyName(name)
  val mns: Option[hydra.module.Namespace] = (qualName.namespace)
  val local: scala.Predef.String = (qualName.local)
  maybes.maybe[hydra.ext.python.syntax.Name, hydra.ext.python.syntax.Name](logic.ifElse[hydra.ext.python.syntax.Name](equality.equal[Option[hydra.module.Namespace]](mns)(Some(focusNs)))(logic.ifElse[scala.Predef.String](hydra.ext.python.names.useFutureAnnotations)(local)(hydra.ext.python.serde.escapePythonString(true)(local)))(strings.intercalate(".")(lists.map[scala.Predef.String,
     scala.Predef.String](hydra.ext.python.names.sanitizePythonName)(strings.splitOn(".")(name)))))((n: hydra.ext.python.syntax.Name) => n)(maps.lookup[hydra.core.Name,
     hydra.ext.python.syntax.Name](name)(boundVars))
}

def encodeNamespace(nsVal: hydra.module.Namespace): hydra.ext.python.syntax.DottedName =
  lists.map[scala.Predef.String, hydra.ext.python.syntax.Name]((part: scala.Predef.String) =>
  hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(hydra.util.CaseConvention.lowerSnake)(part))(strings.splitOn(".")(nsVal))

def encodeTypeVariable(name: hydra.core.Name): hydra.ext.python.syntax.Name = hydra.formatting.capitalize(name)

def sanitizePythonName(v1: scala.Predef.String): scala.Predef.String =
  hydra.formatting.sanitizeWithUnderscores(hydra.ext.python.language.pythonReservedWords)(v1)

def termVariableReference(v1: hydra.ext.python.helpers.PythonEnvironment)(v2: hydra.core.Name): hydra.ext.python.syntax.Expression =
  hydra.ext.python.names.variableReference(hydra.util.CaseConvention.lowerSnake)(false)(v1)(v2)

def typeVariableReference(v1: hydra.ext.python.helpers.PythonEnvironment)(v2: hydra.core.Name): hydra.ext.python.syntax.Expression =
  hydra.ext.python.names.variableReference(hydra.util.CaseConvention.pascal)(false)(v1)(v2)

def variantName(isQualified: Boolean)(env: hydra.ext.python.helpers.PythonEnvironment)(tname: hydra.core.Name)(fname: hydra.core.Name): hydra.ext.python.syntax.Name =
  hydra.ext.python.names.encodeName(isQualified)(hydra.util.CaseConvention.pascal)(env)(strings.cat2(tname)(hydra.formatting.capitalize(fname)))

def variableReference(conv: hydra.util.CaseConvention)(quoted: Boolean)(env: hydra.ext.python.helpers.PythonEnvironment)(name: hydra.core.Name): hydra.ext.python.syntax.Expression =
  {
  val pyName: hydra.ext.python.syntax.Name = hydra.ext.python.names.encodeName(true)(conv)(env)(name)
  val unquoted: hydra.ext.python.syntax.Expression = hydra.ext.python.syntax.Expression.simple(Seq(Seq(hydra.ext.python.syntax.Inversion.simple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(None,
     hydra.ext.python.syntax.BitwiseXor(None, hydra.ext.python.syntax.BitwiseAnd(None, hydra.ext.python.syntax.ShiftExpression(None,
     hydra.ext.python.syntax.Sum(None, hydra.ext.python.syntax.Term(None, hydra.ext.python.syntax.Factor.simple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(false,
     hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name(pyName))), None)))))))),
     Seq())))))
  val namespaces: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName] = (env.namespaces)
  val focusPair: Tuple2[hydra.module.Namespace, hydra.ext.python.syntax.DottedName] = (namespaces.focus)
  val focusNs: hydra.module.Namespace = pairs.first[hydra.module.Namespace, hydra.ext.python.syntax.DottedName](focusPair)
  val mns: Option[hydra.module.Namespace] = hydra.names.namespaceOf(name)
  val sameNamespace: Boolean = maybes.maybe[Boolean, hydra.module.Namespace](false)((ns: hydra.module.Namespace) => equality.equal[hydra.module.Namespace](ns)(focusNs))(mns)
  logic.ifElse[hydra.ext.python.syntax.Expression](logic.and(quoted)(sameNamespace))(hydra.ext.python.syntax.Expression.simple(Seq(Seq(hydra.ext.python.syntax.Inversion.simple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(None,
     hydra.ext.python.syntax.BitwiseXor(None, hydra.ext.python.syntax.BitwiseAnd(None, hydra.ext.python.syntax.ShiftExpression(None,
     hydra.ext.python.syntax.Sum(None, hydra.ext.python.syntax.Term(None, hydra.ext.python.syntax.Factor.simple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(false,
     hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.string(hydra.ext.python.syntax.String(pyName,
     hydra.ext.python.syntax.QuoteStyle.double)))), None)))))))), Seq()))))))(unquoted)
}
