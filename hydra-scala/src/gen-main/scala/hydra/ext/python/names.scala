package hydra.ext.python.names

import hydra.core.*

import hydra.ext.python.environment.*

import hydra.ext.python.syntax.*

import hydra.packaging.*

import hydra.util.*

def encodeConstantForFieldName[T0, T1](env: T0)(tname: T1)(fname: hydra.core.Name): hydra.ext.python.syntax.Name =
  hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(hydra.util.CaseConvention.upperSnake)(fname)

def encodeConstantForTypeName[T0, T1](env: T0)(tname: T1): hydra.ext.python.syntax.Name = "TYPE_"

def encodeEnumValue(v1: hydra.ext.python.environment.PythonEnvironment)(v2: hydra.core.Name): hydra.ext.python.syntax.Name =
  hydra.ext.python.names.encodeName(false)(hydra.util.CaseConvention.upperSnake)(v1)(v2)

def encodeFieldName(env: hydra.ext.python.environment.PythonEnvironment)(fname: hydra.core.Name): hydra.ext.python.syntax.Name =
  hydra.ext.python.names.encodeName(false)(hydra.util.CaseConvention.lowerSnake)(env)(fname)

def encodeName(isQualified: Boolean)(conv: hydra.util.CaseConvention)(env: hydra.ext.python.environment.PythonEnvironment)(name: hydra.core.Name): hydra.ext.python.syntax.Name =
  {
  lazy val namespaces: hydra.packaging.Namespaces[hydra.ext.python.syntax.DottedName] = (env.namespaces)
  lazy val focusPair: Tuple2[hydra.packaging.Namespace, hydra.ext.python.syntax.DottedName] = (namespaces.focus)
  lazy val focusNs: hydra.packaging.Namespace = hydra.lib.pairs.first[hydra.packaging.Namespace, hydra.ext.python.syntax.DottedName](focusPair)
  lazy val boundVars: Map[hydra.core.Name, hydra.ext.python.syntax.Name] = hydra.lib.pairs.second[Seq[hydra.core.Name],
     Map[hydra.core.Name, hydra.ext.python.syntax.Name]](env.boundTypeVariables)
  lazy val qualName: hydra.packaging.QualifiedName = hydra.names.qualifyName(name)
  lazy val mns: Option[hydra.packaging.Namespace] = (qualName.namespace)
  lazy val local: scala.Predef.String = (qualName.local)
  lazy val pyLocal: scala.Predef.String = hydra.ext.python.names.sanitizePythonName(hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(conv)(local))
  def pyNs(nsVal: hydra.packaging.Namespace): scala.Predef.String =
    hydra.lib.strings.intercalate(".")(hydra.lib.lists.map[scala.Predef.String, scala.Predef.String]((v1: scala.Predef.String) =>
    hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(hydra.util.CaseConvention.lowerSnake)(v1))(hydra.lib.strings.splitOn(".")(nsVal)))
  hydra.lib.logic.ifElse[hydra.ext.python.syntax.Name](isQualified)(hydra.lib.maybes.maybe[hydra.ext.python.syntax.Name,
     hydra.ext.python.syntax.Name](hydra.lib.logic.ifElse[hydra.ext.python.syntax.Name](hydra.lib.equality.equal[Option[hydra.packaging.Namespace]](mns)(Some(focusNs)))(hydra.lib.logic.ifElse[scala.Predef.String](hydra.ext.python.names.useFutureAnnotations)(pyLocal)(hydra.ext.python.serde.escapePythonString(true)(pyLocal)))(hydra.lib.maybes.maybe[hydra.ext.python.syntax.Name,
     hydra.packaging.Namespace](pyLocal)((nsVal: hydra.packaging.Namespace) =>
    hydra.lib.strings.cat2(pyNs(nsVal))(hydra.lib.strings.cat2(".")(pyLocal)))(mns)))((n: hydra.ext.python.syntax.Name) => n)(hydra.lib.maps.lookup[hydra.core.Name,
       hydra.ext.python.syntax.Name](name)(boundVars)))(pyLocal)
}

def encodeNameQualified(env: hydra.ext.python.environment.PythonEnvironment)(name: hydra.core.Name): hydra.ext.python.syntax.Name =
  {
  lazy val namespaces: hydra.packaging.Namespaces[hydra.ext.python.syntax.DottedName] = (env.namespaces)
  lazy val focusPair: Tuple2[hydra.packaging.Namespace, hydra.ext.python.syntax.DottedName] = (namespaces.focus)
  lazy val focusNs: hydra.packaging.Namespace = hydra.lib.pairs.first[hydra.packaging.Namespace, hydra.ext.python.syntax.DottedName](focusPair)
  lazy val boundVars: Map[hydra.core.Name, hydra.ext.python.syntax.Name] = hydra.lib.pairs.second[Seq[hydra.core.Name],
     Map[hydra.core.Name, hydra.ext.python.syntax.Name]](env.boundTypeVariables)
  lazy val qualName: hydra.packaging.QualifiedName = hydra.names.qualifyName(name)
  lazy val mns: Option[hydra.packaging.Namespace] = (qualName.namespace)
  lazy val local: scala.Predef.String = (qualName.local)
  hydra.lib.maybes.maybe[hydra.ext.python.syntax.Name, hydra.ext.python.syntax.Name](hydra.lib.logic.ifElse[hydra.ext.python.syntax.Name](hydra.lib.equality.equal[Option[hydra.packaging.Namespace]](mns)(Some(focusNs)))(hydra.lib.logic.ifElse[scala.Predef.String](hydra.ext.python.names.useFutureAnnotations)(local)(hydra.ext.python.serde.escapePythonString(true)(local)))(hydra.lib.strings.intercalate(".")(hydra.lib.lists.map[scala.Predef.String,
     scala.Predef.String](hydra.ext.python.names.sanitizePythonName)(hydra.lib.strings.splitOn(".")(name)))))((n: hydra.ext.python.syntax.Name) => n)(hydra.lib.maps.lookup[hydra.core.Name,
     hydra.ext.python.syntax.Name](name)(boundVars))
}

def encodeNamespace(nsVal: hydra.packaging.Namespace): hydra.ext.python.syntax.DottedName =
  hydra.lib.lists.map[scala.Predef.String, hydra.ext.python.syntax.Name]((part: scala.Predef.String) =>
  hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(hydra.util.CaseConvention.lowerSnake)(part))(hydra.lib.strings.splitOn(".")(nsVal))

def encodeTypeVariable(name: hydra.core.Name): hydra.ext.python.syntax.Name = hydra.formatting.capitalize(name)

def sanitizePythonName(v1: scala.Predef.String): scala.Predef.String =
  hydra.formatting.sanitizeWithUnderscores(hydra.ext.python.language.pythonReservedWords)(v1)

def termVariableReference(v1: hydra.ext.python.environment.PythonEnvironment)(v2: hydra.core.Name): hydra.ext.python.syntax.Expression =
  hydra.ext.python.names.variableReference(hydra.util.CaseConvention.lowerSnake)(false)(v1)(v2)

def typeVariableReference(v1: hydra.ext.python.environment.PythonEnvironment)(v2: hydra.core.Name): hydra.ext.python.syntax.Expression =
  hydra.ext.python.names.variableReference(hydra.util.CaseConvention.pascal)(false)(v1)(v2)

lazy val useFutureAnnotations: Boolean = true

def variableReference(conv: hydra.util.CaseConvention)(quoted: Boolean)(env: hydra.ext.python.environment.PythonEnvironment)(name: hydra.core.Name): hydra.ext.python.syntax.Expression =
  {
  lazy val pyName: hydra.ext.python.syntax.Name = hydra.ext.python.names.encodeName(true)(conv)(env)(name)
  lazy val unquoted: hydra.ext.python.syntax.Expression = hydra.ext.python.syntax.Expression.simple(Seq(Seq(hydra.ext.python.syntax.Inversion.simple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(None,
     hydra.ext.python.syntax.BitwiseXor(None, hydra.ext.python.syntax.BitwiseAnd(None, hydra.ext.python.syntax.ShiftExpression(None,
     hydra.ext.python.syntax.Sum(None, hydra.ext.python.syntax.Term(None, hydra.ext.python.syntax.Factor.simple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(false,
     hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name(pyName))), None)))))))),
     Seq())))))
  lazy val namespaces: hydra.packaging.Namespaces[hydra.ext.python.syntax.DottedName] = (env.namespaces)
  lazy val focusPair: Tuple2[hydra.packaging.Namespace, hydra.ext.python.syntax.DottedName] = (namespaces.focus)
  lazy val focusNs: hydra.packaging.Namespace = hydra.lib.pairs.first[hydra.packaging.Namespace, hydra.ext.python.syntax.DottedName](focusPair)
  lazy val mns: Option[hydra.packaging.Namespace] = hydra.names.namespaceOf(name)
  lazy val sameNamespace: Boolean = hydra.lib.maybes.maybe[Boolean, hydra.packaging.Namespace](false)((ns: hydra.packaging.Namespace) =>
    hydra.lib.equality.equal[hydra.packaging.Namespace](ns)(focusNs))(mns)
  hydra.lib.logic.ifElse[hydra.ext.python.syntax.Expression](hydra.lib.logic.and(quoted)(sameNamespace))(hydra.ext.python.syntax.Expression.simple(Seq(Seq(hydra.ext.python.syntax.Inversion.simple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(None,
     hydra.ext.python.syntax.BitwiseXor(None, hydra.ext.python.syntax.BitwiseAnd(None, hydra.ext.python.syntax.ShiftExpression(None,
     hydra.ext.python.syntax.Sum(None, hydra.ext.python.syntax.Term(None, hydra.ext.python.syntax.Factor.simple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(false,
     hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.string(hydra.ext.python.syntax.String(pyName,
     hydra.ext.python.syntax.QuoteStyle.double)))), None)))))))), Seq()))))))(unquoted)
}

def variantName(isQualified: Boolean)(env: hydra.ext.python.environment.PythonEnvironment)(tname: hydra.core.Name)(fname: hydra.core.Name): hydra.ext.python.syntax.Name =
  hydra.ext.python.names.encodeName(isQualified)(hydra.util.CaseConvention.pascal)(env)(hydra.lib.strings.cat2(tname)(hydra.formatting.capitalize(fname)))
