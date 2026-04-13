package hydra.java.names

import hydra.java.syntax.*

lazy val acceptMethodName: scala.Predef.String = "accept"

lazy val applyMethodName: scala.Predef.String = "apply"

lazy val compareToMethodName: scala.Predef.String = "compareTo"

lazy val equalsMethodName: scala.Predef.String = "equals"

lazy val getMethodName: scala.Predef.String = "get"

lazy val hashCodeMethodName: scala.Predef.String = "hashCode"

lazy val hydraCorePackageName: Option[hydra.java.syntax.PackageName] = Some(hydra.java.names.javaPackageName(Seq("hydra", "core")))

lazy val hydraUtilPackageName: Option[hydra.java.syntax.PackageName] = Some(hydra.java.names.javaPackageName(Seq("hydra", "util")))

lazy val instanceName: scala.Predef.String = "instance"

lazy val javaLangPackageName: Option[hydra.java.syntax.PackageName] = Some(hydra.java.names.javaPackageName(Seq("java", "lang")))

def javaPackageName(parts: Seq[scala.Predef.String]): hydra.java.syntax.PackageName =
  hydra.lib.lists.map[scala.Predef.String, hydra.java.syntax.Identifier]((p: scala.Predef.String) => p)(parts)

lazy val javaUtilFunctionPackageName: Option[hydra.java.syntax.PackageName] = Some(hydra.java.names.javaPackageName(Seq("java", "util", "function")))

lazy val javaUtilPackageName: Option[hydra.java.syntax.PackageName] = Some(hydra.java.names.javaPackageName(Seq("java", "util")))

lazy val otherInstanceName: scala.Predef.String = "other"

lazy val otherwiseMethodName: scala.Predef.String = "otherwise"

lazy val partialVisitorName: scala.Predef.String = "PartialVisitor"

lazy val setMethodName: scala.Predef.String = "set"

lazy val valueFieldName: scala.Predef.String = "value"

lazy val visitMethodName: scala.Predef.String = "visit"

lazy val visitorName: scala.Predef.String = "Visitor"

lazy val visitorReturnParameter: scala.Predef.String = "R"
