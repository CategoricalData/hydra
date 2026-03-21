package hydra.names

import hydra.core.*

import hydra.module.*

import hydra.util.*

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.sets

import hydra.lib.strings

def compactName(namespaces: Map[hydra.module.Namespace, scala.Predef.String])(name: hydra.core.Name): scala.Predef.String =
  {
  val qualName: hydra.module.QualifiedName = hydra.names.qualifyName(name)
  val mns: Option[hydra.module.Namespace] = (qualName.namespace)
  val local: scala.Predef.String = (qualName.local)
  maybes.maybe[scala.Predef.String, hydra.module.Namespace](name)((ns: hydra.module.Namespace) =>
    maybes.maybe[scala.Predef.String, scala.Predef.String](local)((pre: scala.Predef.String) => strings.cat(Seq(pre,
       ":", local)))(maps.lookup[hydra.module.Namespace, scala.Predef.String](ns)(namespaces)))(mns)
}

def localNameOf(`arg_`: hydra.core.Name): scala.Predef.String = (hydra.names.qualifyName(`arg_`).local)

def namespaceOf(`arg_`: hydra.core.Name): Option[hydra.module.Namespace] = (hydra.names.qualifyName(`arg_`).namespace)

def namespaceToFilePath(caseConv: hydra.util.CaseConvention)(ext: hydra.module.FileExtension)(ns: hydra.module.Namespace): scala.Predef.String =
  {
  val parts: Seq[scala.Predef.String] = lists.map[scala.Predef.String, scala.Predef.String]((v1: scala.Predef.String) =>
    hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(caseConv)(v1))(strings.splitOn(".")(ns))
  strings.cat2(strings.cat2(strings.intercalate("/")(parts))("."))(ext)
}

def qname(ns: hydra.module.Namespace)(name: scala.Predef.String): hydra.core.Name = strings.cat(Seq(ns, ".", name))

def qualifyName(name: hydra.core.Name): hydra.module.QualifiedName =
  {
  val parts: Seq[scala.Predef.String] = lists.reverse[scala.Predef.String](strings.splitOn(".")(name))
  logic.ifElse[hydra.module.QualifiedName](equality.equal[Int](1)(lists.length[scala.Predef.String](parts)))(hydra.module.QualifiedName(None,
     name))(hydra.module.QualifiedName(Some(strings.intercalate(".")(lists.reverse[scala.Predef.String](lists.tail[scala.Predef.String](parts)))),
     lists.head[scala.Predef.String](parts)))
}

def uniqueLabel(visited: scala.collection.immutable.Set[scala.Predef.String])(l: scala.Predef.String): scala.Predef.String =
  logic.ifElse[scala.Predef.String](sets.member[scala.Predef.String](l)(visited))(hydra.names.uniqueLabel(visited)(strings.cat2(l)("'")))(l)

def unqualifyName(qname: hydra.module.QualifiedName): hydra.core.Name =
  {
  val prefix: scala.Predef.String = maybes.maybe[scala.Predef.String, hydra.module.Namespace]("")((n: hydra.module.Namespace) => strings.cat2(n)("."))(qname.namespace)
  strings.cat2(prefix)(qname.local)
}
