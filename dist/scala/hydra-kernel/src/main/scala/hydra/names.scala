package hydra.names

import hydra.core.*

import hydra.packaging.*

import hydra.util.*

def compactName(namespaces: Map[hydra.packaging.Namespace, scala.Predef.String])(name: hydra.core.Name): scala.Predef.String =
  {
  lazy val qualName: hydra.packaging.QualifiedName = hydra.names.qualifyName(name)
  lazy val mns: Option[hydra.packaging.Namespace] = (qualName.namespace)
  lazy val local: scala.Predef.String = (qualName.local)
  hydra.lib.maybes.maybe[scala.Predef.String, hydra.packaging.Namespace](name)((ns: hydra.packaging.Namespace) =>
    hydra.lib.maybes.maybe[scala.Predef.String, scala.Predef.String](local)((pre: scala.Predef.String) => hydra.lib.strings.cat(Seq(pre,
       ":", local)))(hydra.lib.maps.lookup[hydra.packaging.Namespace, scala.Predef.String](ns)(namespaces)))(mns)
}

def freshName(cx: hydra.context.Context): Tuple2[hydra.core.Name, hydra.context.Context] =
  {
  lazy val count: Int = hydra.annotations.getCount(hydra.constants.key_freshTypeVariableCount)(cx)
  Tuple2(hydra.names.normalTypeVariable(count), hydra.annotations.putCount(hydra.constants.key_freshTypeVariableCount)(hydra.lib.math.add(count)(1))(cx))
}

def freshNames(n: Int)(cx: hydra.context.Context): Tuple2[Seq[hydra.core.Name], hydra.context.Context] =
  {
  def go[T0](acc: Tuple2[Seq[hydra.core.Name], hydra.context.Context])(_x: T0): Tuple2[Seq[hydra.core.Name],
     hydra.context.Context] =
    {
    lazy val names: Seq[hydra.core.Name] = hydra.lib.pairs.first[Seq[hydra.core.Name], hydra.context.Context](acc)
    lazy val cx0: hydra.context.Context = hydra.lib.pairs.second[Seq[hydra.core.Name], hydra.context.Context](acc)
    lazy val result: Tuple2[hydra.core.Name, hydra.context.Context] = hydra.names.freshName(cx0)
    lazy val name: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, hydra.context.Context](result)
    lazy val cx1: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Name, hydra.context.Context](result)
    Tuple2(hydra.lib.lists.concat2[hydra.core.Name](names)(hydra.lib.lists.pure[hydra.core.Name](name)), cx1)
  }
  hydra.lib.lists.foldl[Tuple2[Seq[hydra.core.Name], hydra.context.Context], Unit](go)(Tuple2(Seq(),
     cx))(hydra.lib.lists.replicate[Unit](n)(()))
}

def localNameOf(`arg_`: hydra.core.Name): scala.Predef.String = (hydra.names.qualifyName(`arg_`).local)

def nameToFilePath(nsConv: hydra.util.CaseConvention)(localConv: hydra.util.CaseConvention)(ext: hydra.packaging.FileExtension)(name: hydra.core.Name): scala.Predef.String =
  {
  lazy val qualName: hydra.packaging.QualifiedName = hydra.names.qualifyName(name)
  lazy val ns: Option[hydra.packaging.Namespace] = (qualName.namespace)
  lazy val local: scala.Predef.String = (qualName.local)
  def nsToFilePath(ns2: hydra.packaging.Namespace): scala.Predef.String =
    hydra.lib.strings.intercalate("/")(hydra.lib.lists.map[scala.Predef.String, scala.Predef.String]((part: scala.Predef.String) =>
    hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(nsConv)(part))(hydra.lib.strings.splitOn(".")(ns2)))
  lazy val prefix: scala.Predef.String = hydra.lib.maybes.maybe[scala.Predef.String,
     hydra.packaging.Namespace]("")((n: hydra.packaging.Namespace) => hydra.lib.strings.cat2(nsToFilePath(n))("/"))(ns)
  lazy val suffix: scala.Predef.String = hydra.formatting.convertCase(hydra.util.CaseConvention.pascal)(localConv)(local)
  hydra.lib.strings.cat(Seq(prefix, suffix, ".", ext))
}

def namespaceOf(`arg_`: hydra.core.Name): Option[hydra.packaging.Namespace] = (hydra.names.qualifyName(`arg_`).namespace)

def namespaceToFilePath(caseConv: hydra.util.CaseConvention)(ext: hydra.packaging.FileExtension)(ns: hydra.packaging.Namespace): scala.Predef.String =
  {
  lazy val parts: Seq[scala.Predef.String] = hydra.lib.lists.map[scala.Predef.String,
     scala.Predef.String]((v1: scala.Predef.String) =>
    hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(caseConv)(v1))(hydra.lib.strings.splitOn(".")(ns))
  hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.intercalate("/")(parts))("."))(ext)
}

def normalTypeVariable(i: Int): hydra.core.Name = hydra.lib.strings.cat2("t")(hydra.lib.literals.showInt32(i))

def qname(ns: hydra.packaging.Namespace)(name: scala.Predef.String): hydra.core.Name = hydra.lib.strings.cat(Seq(ns,
   ".", name))

def qualifyName(name: hydra.core.Name): hydra.packaging.QualifiedName =
  {
  lazy val parts: Seq[scala.Predef.String] = hydra.lib.lists.reverse[scala.Predef.String](hydra.lib.strings.splitOn(".")(name))
  hydra.lib.maybes.maybe[hydra.packaging.QualifiedName, Tuple2[scala.Predef.String,
     Seq[scala.Predef.String]]](hydra.packaging.QualifiedName(None, name))((uc: Tuple2[scala.Predef.String,
     Seq[scala.Predef.String]]) =>
    {
    lazy val localName: scala.Predef.String = hydra.lib.pairs.first[scala.Predef.String, Seq[scala.Predef.String]](uc)
    {
      lazy val restReversed: Seq[scala.Predef.String] = hydra.lib.pairs.second[scala.Predef.String,
         Seq[scala.Predef.String]](uc)
      hydra.lib.logic.ifElse[hydra.packaging.QualifiedName](hydra.lib.lists.`null`[scala.Predef.String](restReversed))(hydra.packaging.QualifiedName(None,
         name))(hydra.packaging.QualifiedName(Some(hydra.lib.strings.intercalate(".")(hydra.lib.lists.reverse[scala.Predef.String](restReversed))),
         localName))
    }
  })(hydra.lib.lists.uncons[scala.Predef.String](parts))
}

def uniqueLabel(visited: scala.collection.immutable.Set[scala.Predef.String])(l: scala.Predef.String): scala.Predef.String =
  hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.sets.member[scala.Predef.String](l)(visited))(hydra.names.uniqueLabel(visited)(hydra.lib.strings.cat2(l)("'")))(l)

def unqualifyName(qname: hydra.packaging.QualifiedName): hydra.core.Name =
  {
  lazy val prefix: scala.Predef.String = hydra.lib.maybes.maybe[scala.Predef.String,
     hydra.packaging.Namespace]("")((n: hydra.packaging.Namespace) => hydra.lib.strings.cat2(n)("."))(qname.namespace)
  hydra.lib.strings.cat2(prefix)(qname.local)
}
