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
  lazy val qualName: hydra.module.QualifiedName = hydra.names.qualifyName(name)
  lazy val mns: Option[hydra.module.Namespace] = (qualName.namespace)
  lazy val local: scala.Predef.String = (qualName.local)
  hydra.lib.maybes.maybe[scala.Predef.String, hydra.module.Namespace](name)((ns: hydra.module.Namespace) =>
    hydra.lib.maybes.maybe[scala.Predef.String, scala.Predef.String](local)((pre: scala.Predef.String) => hydra.lib.strings.cat(Seq(pre,
       ":", local)))(hydra.lib.maps.lookup[hydra.module.Namespace, scala.Predef.String](ns)(namespaces)))(mns)
}

def localNameOf(`arg_`: hydra.core.Name): scala.Predef.String = (hydra.names.qualifyName(`arg_`).local)

def namespaceOf(`arg_`: hydra.core.Name): Option[hydra.module.Namespace] = (hydra.names.qualifyName(`arg_`).namespace)

def namespaceToFilePath(caseConv: hydra.util.CaseConvention)(ext: hydra.module.FileExtension)(ns: hydra.module.Namespace): scala.Predef.String =
  {
  lazy val parts: Seq[scala.Predef.String] = hydra.lib.lists.map[scala.Predef.String, scala.Predef.String]((v1: scala.Predef.String) =>
    hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(caseConv)(v1))(hydra.lib.strings.splitOn(".")(ns))
  hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.intercalate("/")(parts))("."))(ext)
}

def qname(ns: hydra.module.Namespace)(name: scala.Predef.String): hydra.core.Name = hydra.lib.strings.cat(Seq(ns, ".", name))

def qualifyName(name: hydra.core.Name): hydra.module.QualifiedName =
  {
  lazy val parts: Seq[scala.Predef.String] = hydra.lib.lists.reverse[scala.Predef.String](hydra.lib.strings.splitOn(".")(name))
  hydra.lib.logic.ifElse[hydra.module.QualifiedName](hydra.lib.equality.equal[Int](1)(hydra.lib.lists.length[scala.Predef.String](parts)))(hydra.module.QualifiedName(None,
     name))(hydra.module.QualifiedName(Some(hydra.lib.strings.intercalate(".")(hydra.lib.lists.reverse[scala.Predef.String](hydra.lib.lists.tail[scala.Predef.String](parts)))),
     hydra.lib.lists.head[scala.Predef.String](parts)))
}

def uniqueLabel(visited: scala.collection.immutable.Set[scala.Predef.String])(l: scala.Predef.String): scala.Predef.String =
  hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.sets.member[scala.Predef.String](l)(visited))(hydra.names.uniqueLabel(visited)(hydra.lib.strings.cat2(l)("'")))(l)

def unqualifyName(qname: hydra.module.QualifiedName): hydra.core.Name =
  {
  lazy val prefix: scala.Predef.String = hydra.lib.maybes.maybe[scala.Predef.String, hydra.module.Namespace]("")((n: hydra.module.Namespace) => hydra.lib.strings.cat2(n)("."))(qname.namespace)
  hydra.lib.strings.cat2(prefix)(qname.local)
}
