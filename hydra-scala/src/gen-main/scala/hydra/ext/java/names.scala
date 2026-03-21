package hydra.ext.java.names

import hydra.ext.java.syntax.*

import hydra.lib.lists

val acceptMethodName: scala.Predef.String = "accept"

val applyMethodName: scala.Predef.String = "apply"

val compareToMethodName: scala.Predef.String = "compareTo"

val equalsMethodName: scala.Predef.String = "equals"

val getMethodName: scala.Predef.String = "get"

val hashCodeMethodName: scala.Predef.String = "hashCode"

val instanceName: scala.Predef.String = "instance"

val otherInstanceName: scala.Predef.String = "other"

val otherwiseMethodName: scala.Predef.String = "otherwise"

val partialVisitorName: scala.Predef.String = "PartialVisitor"

val setMethodName: scala.Predef.String = "set"

val valueFieldName: scala.Predef.String = "value"

val visitMethodName: scala.Predef.String = "visit"

val visitorName: scala.Predef.String = "Visitor"

val visitorReturnParameter: scala.Predef.String = "R"

def javaPackageName(parts: Seq[scala.Predef.String]): hydra.ext.java.syntax.PackageName =
  lists.map[scala.Predef.String, hydra.ext.java.syntax.Identifier]((p: scala.Predef.String) => p)(parts)

val hydraCorePackageName: Option[hydra.ext.java.syntax.PackageName] = Some(hydra.ext.java.names.javaPackageName(Seq("hydra", "core")))

val hydraUtilPackageName: Option[hydra.ext.java.syntax.PackageName] = Some(hydra.ext.java.names.javaPackageName(Seq("hydra", "util")))

val javaLangPackageName: Option[hydra.ext.java.syntax.PackageName] = Some(hydra.ext.java.names.javaPackageName(Seq("java", "lang")))

val javaUtilFunctionPackageName: Option[hydra.ext.java.syntax.PackageName] = Some(hydra.ext.java.names.javaPackageName(Seq("java", "util", "function")))

val javaUtilPackageName: Option[hydra.ext.java.syntax.PackageName] = Some(hydra.ext.java.names.javaPackageName(Seq("java", "util")))
