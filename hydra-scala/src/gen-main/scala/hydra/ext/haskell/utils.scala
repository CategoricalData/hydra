package hydra.ext.haskell.utils

import hydra.core.*

import hydra.ext.haskell.syntax.*

import hydra.module.*

import hydra.lib.eithers

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.sets

import hydra.lib.strings

def applicationPattern(name: hydra.ext.haskell.syntax.Name)(args: Seq[hydra.ext.haskell.syntax.Pattern]): hydra.ext.haskell.syntax.Pattern =
  hydra.ext.haskell.syntax.Pattern.application(hydra.ext.haskell.syntax.ApplicationPattern(name, args))

def elementReference(namespaces: hydra.module.Namespaces[hydra.ext.haskell.syntax.ModuleName])(name: hydra.core.Name): hydra.ext.haskell.syntax.Name =
  {
  val namespacePair: Tuple2[hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName] = (namespaces.focus)
  val gname: hydra.module.Namespace = hydra.lib.pairs.first[hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName](namespacePair)
  val gmod: scala.Predef.String = hydra.lib.pairs.second[hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName](namespacePair)
  val namespacesMap: Map[hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName] = (namespaces.mapping)
  val qname: hydra.module.QualifiedName = hydra.names.qualifyName(name)
  val local: scala.Predef.String = (qname.local)
  val escLocal: scala.Predef.String = hydra.ext.haskell.utils.sanitizeHaskellName(local)
  val mns: Option[hydra.module.Namespace] = (qname.namespace)
  hydra.lib.maybes.cases[hydra.module.Namespace, hydra.ext.haskell.syntax.Name](qname.namespace)(hydra.ext.haskell.utils.simpleName(local))((ns: hydra.module.Namespace) =>
    hydra.lib.maybes.cases[hydra.ext.haskell.syntax.ModuleName, hydra.ext.haskell.syntax.Name](hydra.lib.maps.lookup[hydra.module.Namespace,
       hydra.ext.haskell.syntax.ModuleName](ns)(namespacesMap))(hydra.ext.haskell.utils.simpleName(local))((mn: hydra.ext.haskell.syntax.ModuleName) =>
    {
    val aliasStr: scala.Predef.String = mn
    hydra.lib.logic.ifElse[hydra.ext.haskell.syntax.Name](hydra.lib.equality.equal[hydra.module.Namespace](ns)(gname))(hydra.ext.haskell.utils.simpleName(escLocal))(hydra.ext.haskell.utils.rawName(hydra.lib.strings.cat(Seq(aliasStr,
       ".", hydra.ext.haskell.utils.sanitizeHaskellName(local)))))
  }))
}

def hsapp(l: hydra.ext.haskell.syntax.Expression)(r: hydra.ext.haskell.syntax.Expression): hydra.ext.haskell.syntax.Expression =
  hydra.ext.haskell.syntax.Expression.application(hydra.ext.haskell.syntax.ApplicationExpression(l, r))

def hslambda(name: hydra.ext.haskell.syntax.Name)(rhs: hydra.ext.haskell.syntax.Expression): hydra.ext.haskell.syntax.Expression =
  hydra.ext.haskell.syntax.Expression.lambda(hydra.ext.haskell.syntax.LambdaExpression(Seq(hydra.ext.haskell.syntax.Pattern.name(name)), rhs))

def hslit(lit: hydra.ext.haskell.syntax.Literal): hydra.ext.haskell.syntax.Expression = hydra.ext.haskell.syntax.Expression.literal(lit)

def hsvar(s: scala.Predef.String): hydra.ext.haskell.syntax.Expression = hydra.ext.haskell.syntax.Expression.variable(hydra.ext.haskell.utils.rawName(s))

def namespacesForModule(mod: hydra.module.Module)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.module.Namespaces[hydra.ext.haskell.syntax.ModuleName]] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], scala.collection.immutable.Set[hydra.module.Namespace],
     hydra.module.Namespaces[hydra.ext.haskell.syntax.ModuleName]](hydra.schemas.moduleDependencyNamespaces(cx)(g)(true)(true)(true)(true)(mod))((nss: scala.collection.immutable.Set[hydra.module.Namespace]) =>
  {
  val ns: hydra.module.Namespace = (mod.namespace)
  {
    def toModuleName(namespace: hydra.module.Namespace): hydra.ext.haskell.syntax.ModuleName =
      {
      val namespaceStr: scala.Predef.String = namespace
      val parts: Seq[scala.Predef.String] = hydra.lib.strings.splitOn(".")(namespaceStr)
      val lastPart: scala.Predef.String = hydra.lib.lists.last[scala.Predef.String](parts)
      val capitalized: scala.Predef.String = hydra.formatting.capitalize(lastPart)
      capitalized
    }
    {
      def toPair(name: hydra.module.Namespace): Tuple2[hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName] = Tuple2(name, toModuleName(name))
      {
        def addPair[T0](state: Tuple2[Map[T0, hydra.ext.haskell.syntax.ModuleName], scala.collection.immutable.Set[hydra.ext.haskell.syntax.ModuleName]])(namePair: Tuple2[T0,
           hydra.ext.haskell.syntax.ModuleName]): Tuple2[Map[T0, hydra.ext.haskell.syntax.ModuleName],
           scala.collection.immutable.Set[hydra.ext.haskell.syntax.ModuleName]] =
          {
          val currentMap: Map[T0, hydra.ext.haskell.syntax.ModuleName] = hydra.lib.pairs.first[Map[T0,
             hydra.ext.haskell.syntax.ModuleName], scala.collection.immutable.Set[hydra.ext.haskell.syntax.ModuleName]](state)
          val currentSet: scala.collection.immutable.Set[hydra.ext.haskell.syntax.ModuleName] = hydra.lib.pairs.second[Map[T0,
             hydra.ext.haskell.syntax.ModuleName], scala.collection.immutable.Set[hydra.ext.haskell.syntax.ModuleName]](state)
          val name: T0 = hydra.lib.pairs.first[T0, hydra.ext.haskell.syntax.ModuleName](namePair)
          val alias: hydra.ext.haskell.syntax.ModuleName = hydra.lib.pairs.second[T0, hydra.ext.haskell.syntax.ModuleName](namePair)
          val aliasStr: scala.Predef.String = alias
          hydra.lib.logic.ifElse[Tuple2[Map[T0, hydra.ext.haskell.syntax.ModuleName], scala.collection.immutable.Set[hydra.ext.haskell.syntax.ModuleName]]](hydra.lib.sets.member[hydra.ext.haskell.syntax.ModuleName](alias)(currentSet))(addPair(state)(Tuple2(name,
             hydra.lib.strings.cat2(aliasStr)("_"))))(Tuple2(hydra.lib.maps.insert[T0, hydra.ext.haskell.syntax.ModuleName](name)(alias)(currentMap),
             hydra.lib.sets.insert[hydra.ext.haskell.syntax.ModuleName](alias)(currentSet)))
        }
        {
          val focusPair: Tuple2[hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName] = toPair(ns)
          {
            val nssAsList: Seq[hydra.module.Namespace] = hydra.lib.sets.toList[hydra.module.Namespace](nss)
            {
              val nssPairs: Seq[Tuple2[hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName]] = hydra.lib.lists.map[hydra.module.Namespace,
                 Tuple2[hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName]](toPair)(nssAsList)
              {
                def emptyState[T0, T1, T2]: Tuple2[Map[T0, T1], scala.collection.immutable.Set[T2]] = Tuple2(hydra.lib.maps.empty[T0,
                   T1], hydra.lib.sets.empty[T2])
                {
                  val finalState: Tuple2[Map[hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName],
                     scala.collection.immutable.Set[hydra.ext.haskell.syntax.ModuleName]] = hydra.lib.lists.foldl[Tuple2[Map[hydra.module.Namespace,
                     hydra.ext.haskell.syntax.ModuleName], scala.collection.immutable.Set[hydra.ext.haskell.syntax.ModuleName]],
                     Tuple2[hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName]](addPair)(emptyState)(nssPairs)
                  {
                    val resultMap: Map[hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName] = hydra.lib.pairs.first[Map[hydra.module.Namespace,
                       hydra.ext.haskell.syntax.ModuleName], scala.collection.immutable.Set[hydra.ext.haskell.syntax.ModuleName]](finalState)
                    Right(hydra.module.Namespaces(focusPair, resultMap))
                  }
                }
              }
            }
          }
        }
      }
    }
  }
})

def newtypeAccessorName(name: hydra.core.Name): scala.Predef.String = hydra.lib.strings.cat2("un")(hydra.names.localNameOf(name))

def rawName(n: scala.Predef.String): hydra.ext.haskell.syntax.Name =
  hydra.ext.haskell.syntax.Name.normal(hydra.ext.haskell.syntax.QualifiedName(Seq(), n))

def recordFieldReference(namespaces: hydra.module.Namespaces[hydra.ext.haskell.syntax.ModuleName])(sname: hydra.core.Name)(fname: hydra.core.Name): hydra.ext.haskell.syntax.Name =
  {
  val fnameStr: scala.Predef.String = fname
  val qname: hydra.module.QualifiedName = hydra.names.qualifyName(sname)
  val ns: Option[hydra.module.Namespace] = (qname.namespace)
  val typeNameStr: scala.Predef.String = hydra.ext.haskell.utils.typeNameForRecord(sname)
  val decapitalized: scala.Predef.String = hydra.formatting.decapitalize(typeNameStr)
  val capitalized: scala.Predef.String = hydra.formatting.capitalize(fnameStr)
  val nm: scala.Predef.String = hydra.lib.strings.cat2(decapitalized)(capitalized)
  val qualName: hydra.module.QualifiedName = hydra.module.QualifiedName(ns, nm)
  val unqualName: hydra.core.Name = hydra.names.unqualifyName(qualName)
  hydra.ext.haskell.utils.elementReference(namespaces)(unqualName)
}

def sanitizeHaskellName(v1: scala.Predef.String): scala.Predef.String =
  hydra.formatting.sanitizeWithUnderscores(hydra.ext.haskell.language.reservedWords)(v1)

def simpleName(`arg_`: scala.Predef.String): hydra.ext.haskell.syntax.Name =
  hydra.ext.haskell.utils.rawName(hydra.ext.haskell.utils.sanitizeHaskellName(`arg_`))

def simpleValueBinding(hname: hydra.ext.haskell.syntax.Name)(rhs: hydra.ext.haskell.syntax.Expression)(bindings: Option[hydra.ext.haskell.syntax.LocalBindings]): hydra.ext.haskell.syntax.ValueBinding =
  {
  val pat: hydra.ext.haskell.syntax.Pattern = hydra.ext.haskell.syntax.Pattern.application(hydra.ext.haskell.syntax.ApplicationPattern(hname, Seq()))
  val rightHandSide: hydra.ext.haskell.syntax.RightHandSide = rhs
  hydra.ext.haskell.syntax.ValueBinding.simple(hydra.ext.haskell.syntax.SimpleValueBinding(pat, rightHandSide, bindings))
}

def toTypeApplication(types: Seq[hydra.ext.haskell.syntax.Type]): hydra.ext.haskell.syntax.Type =
  {
  def app(l: Seq[hydra.ext.haskell.syntax.Type]): hydra.ext.haskell.syntax.Type =
    hydra.lib.logic.ifElse[hydra.ext.haskell.syntax.Type](hydra.lib.equality.gt[Int](hydra.lib.lists.length[hydra.ext.haskell.syntax.Type](l))(1))(hydra.ext.haskell.syntax.Type.application(hydra.ext.haskell.syntax.ApplicationType(app(hydra.lib.lists.tail[hydra.ext.haskell.syntax.Type](l)),
       hydra.lib.lists.head[hydra.ext.haskell.syntax.Type](l))))(hydra.lib.lists.head[hydra.ext.haskell.syntax.Type](l))
  app(hydra.lib.lists.reverse[hydra.ext.haskell.syntax.Type](types))
}

def typeNameForRecord(sname: hydra.core.Name): scala.Predef.String =
  {
  val snameStr: scala.Predef.String = sname
  val parts: Seq[scala.Predef.String] = hydra.lib.strings.splitOn(".")(snameStr)
  hydra.lib.lists.last[scala.Predef.String](parts)
}

def unionFieldReference(boundNames: scala.collection.immutable.Set[hydra.core.Name])(namespaces: hydra.module.Namespaces[hydra.ext.haskell.syntax.ModuleName])(sname: hydra.core.Name)(fname: hydra.core.Name): hydra.ext.haskell.syntax.Name =
  {
  val fnameStr: scala.Predef.String = fname
  val qname: hydra.module.QualifiedName = hydra.names.qualifyName(sname)
  val ns: Option[hydra.module.Namespace] = (qname.namespace)
  val typeNameStr: scala.Predef.String = hydra.ext.haskell.utils.typeNameForRecord(sname)
  val capitalizedTypeName: scala.Predef.String = hydra.formatting.capitalize(typeNameStr)
  val capitalizedFieldName: scala.Predef.String = hydra.formatting.capitalize(fnameStr)
  def deconflict(name: scala.Predef.String): scala.Predef.String =
    {
    val tname: hydra.core.Name = hydra.names.unqualifyName(hydra.module.QualifiedName(ns, name))
    hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.sets.member[hydra.core.Name](tname)(boundNames))(deconflict(hydra.lib.strings.cat2(name)("_")))(name)
  }
  val nm: scala.Predef.String = deconflict(hydra.lib.strings.cat2(capitalizedTypeName)(capitalizedFieldName))
  val qualName: hydra.module.QualifiedName = hydra.module.QualifiedName(ns, nm)
  val unqualName: hydra.core.Name = hydra.names.unqualifyName(qualName)
  hydra.ext.haskell.utils.elementReference(namespaces)(unqualName)
}

def unpackForallType(t: hydra.core.Type): Tuple2[Seq[hydra.core.Name], hydra.core.Type] =
  hydra.rewriting.deannotateType(t) match
  case hydra.core.Type.forall(v_Type_forall_fat) => {
    val v: hydra.core.Name = (v_Type_forall_fat.parameter)
    val tbody: hydra.core.Type = (v_Type_forall_fat.body)
    val recursiveResult: Tuple2[Seq[hydra.core.Name], hydra.core.Type] = hydra.ext.haskell.utils.unpackForallType(tbody)
    val vars: Seq[hydra.core.Name] = hydra.lib.pairs.first[Seq[hydra.core.Name], hydra.core.Type](recursiveResult)
    val finalType: hydra.core.Type = hydra.lib.pairs.second[Seq[hydra.core.Name], hydra.core.Type](recursiveResult)
    Tuple2(hydra.lib.lists.cons[hydra.core.Name](v)(vars), finalType)
  }
  case _ => Tuple2(Seq(), t)
