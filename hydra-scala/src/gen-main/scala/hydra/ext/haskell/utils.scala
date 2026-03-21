package hydra.ext.haskell.utils

import hydra.core.*

import hydra.ext.haskell.ast.*

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

def applicationPattern(name: hydra.ext.haskell.ast.Name)(args: Seq[hydra.ext.haskell.ast.Pattern]): hydra.ext.haskell.ast.Pattern =
  hydra.ext.haskell.ast.Pattern.application(hydra.ext.haskell.ast.ApplicationPattern(name, args))

def elementReference(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName])(name: hydra.core.Name): hydra.ext.haskell.ast.Name =
  {
  val namespacePair: Tuple2[hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName] = (namespaces.focus)
  val gname: hydra.module.Namespace = pairs.first[hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName](namespacePair)
  val gmod: scala.Predef.String = pairs.second[hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName](namespacePair)
  val namespacesMap: Map[hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName] = (namespaces.mapping)
  val qname: hydra.module.QualifiedName = hydra.names.qualifyName(name)
  val local: scala.Predef.String = (qname.local)
  val escLocal: scala.Predef.String = hydra.ext.haskell.utils.sanitizeHaskellName(local)
  val mns: Option[hydra.module.Namespace] = (qname.namespace)
  maybes.cases[hydra.module.Namespace, hydra.ext.haskell.ast.Name](qname.namespace)(hydra.ext.haskell.utils.simpleName(local))((ns: hydra.module.Namespace) =>
    maybes.cases[hydra.ext.haskell.ast.ModuleName, hydra.ext.haskell.ast.Name](maps.lookup[hydra.module.Namespace,
       hydra.ext.haskell.ast.ModuleName](ns)(namespacesMap))(hydra.ext.haskell.utils.simpleName(local))((mn: hydra.ext.haskell.ast.ModuleName) =>
    {
    val aliasStr: scala.Predef.String = mn
    logic.ifElse[hydra.ext.haskell.ast.Name](equality.equal[hydra.module.Namespace](ns)(gname))(hydra.ext.haskell.utils.simpleName(escLocal))(hydra.ext.haskell.utils.rawName(strings.cat(Seq(aliasStr,
       ".", hydra.ext.haskell.utils.sanitizeHaskellName(local)))))
  }))
}

def hsapp(l: hydra.ext.haskell.ast.Expression)(r: hydra.ext.haskell.ast.Expression): hydra.ext.haskell.ast.Expression =
  hydra.ext.haskell.ast.Expression.application(hydra.ext.haskell.ast.ApplicationExpression(l, r))

def hslambda(name: hydra.ext.haskell.ast.Name)(rhs: hydra.ext.haskell.ast.Expression): hydra.ext.haskell.ast.Expression =
  hydra.ext.haskell.ast.Expression.lambda(hydra.ext.haskell.ast.LambdaExpression(Seq(hydra.ext.haskell.ast.Pattern.name(name)), rhs))

def hslit(lit: hydra.ext.haskell.ast.Literal): hydra.ext.haskell.ast.Expression = hydra.ext.haskell.ast.Expression.literal(lit)

def hsvar(s: scala.Predef.String): hydra.ext.haskell.ast.Expression = hydra.ext.haskell.ast.Expression.variable(hydra.ext.haskell.utils.rawName(s))

def namespacesForModule(mod: hydra.module.Module)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName]] =
  eithers.bind[hydra.context.InContext[hydra.error.Error], scala.collection.immutable.Set[hydra.module.Namespace],
     hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName]](hydra.schemas.moduleDependencyNamespaces(cx)(g)(true)(true)(true)(true)(mod))((nss: scala.collection.immutable.Set[hydra.module.Namespace]) =>
  {
  val ns: hydra.module.Namespace = (mod.namespace)
  {
    def toModuleName(namespace: hydra.module.Namespace): hydra.ext.haskell.ast.ModuleName =
      {
      val namespaceStr: scala.Predef.String = namespace
      val parts: Seq[scala.Predef.String] = strings.splitOn(".")(namespaceStr)
      val lastPart: scala.Predef.String = lists.last[scala.Predef.String](parts)
      val capitalized: scala.Predef.String = hydra.formatting.capitalize(lastPart)
      capitalized
    }
    {
      def toPair(name: hydra.module.Namespace): Tuple2[hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName] = Tuple2(name, toModuleName(name))
      {
        def addPair[T0](state: Tuple2[Map[T0, hydra.ext.haskell.ast.ModuleName], scala.collection.immutable.Set[hydra.ext.haskell.ast.ModuleName]])(namePair: Tuple2[T0,
           hydra.ext.haskell.ast.ModuleName]): Tuple2[Map[T0, hydra.ext.haskell.ast.ModuleName], scala.collection.immutable.Set[hydra.ext.haskell.ast.ModuleName]] =
          {
          val currentMap: Map[T0, hydra.ext.haskell.ast.ModuleName] = pairs.first[Map[T0, hydra.ext.haskell.ast.ModuleName],
             scala.collection.immutable.Set[hydra.ext.haskell.ast.ModuleName]](state)
          val currentSet: scala.collection.immutable.Set[hydra.ext.haskell.ast.ModuleName] = pairs.second[Map[T0,
             hydra.ext.haskell.ast.ModuleName], scala.collection.immutable.Set[hydra.ext.haskell.ast.ModuleName]](state)
          val name: T0 = pairs.first[T0, hydra.ext.haskell.ast.ModuleName](namePair)
          val alias: hydra.ext.haskell.ast.ModuleName = pairs.second[T0, hydra.ext.haskell.ast.ModuleName](namePair)
          val aliasStr: scala.Predef.String = alias
          logic.ifElse[Tuple2[Map[T0, hydra.ext.haskell.ast.ModuleName], scala.collection.immutable.Set[hydra.ext.haskell.ast.ModuleName]]](sets.member[hydra.ext.haskell.ast.ModuleName](alias)(currentSet))(addPair(state)(Tuple2(name,
             strings.cat2(aliasStr)("_"))))(Tuple2(maps.insert[T0, hydra.ext.haskell.ast.ModuleName](name)(alias)(currentMap),
             sets.insert[hydra.ext.haskell.ast.ModuleName](alias)(currentSet)))
        }
        {
          val focusPair: Tuple2[hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName] = toPair(ns)
          {
            val nssAsList: Seq[hydra.module.Namespace] = sets.toList[hydra.module.Namespace](nss)
            {
              val nssPairs: Seq[Tuple2[hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName]] = lists.map[hydra.module.Namespace,
                 Tuple2[hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName]](toPair)(nssAsList)
              {
                def emptyState[T0, T1, T2]: Tuple2[Map[T0, T1], scala.collection.immutable.Set[T2]] = Tuple2(maps.empty[T0, T1], sets.empty[T2])
                {
                  val finalState: Tuple2[Map[hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName],
                     scala.collection.immutable.Set[hydra.ext.haskell.ast.ModuleName]] = lists.foldl[Tuple2[Map[hydra.module.Namespace,
                     hydra.ext.haskell.ast.ModuleName], scala.collection.immutable.Set[hydra.ext.haskell.ast.ModuleName]],
                     Tuple2[hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName]](addPair)(emptyState)(nssPairs)
                  {
                    val resultMap: Map[hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName] = pairs.first[Map[hydra.module.Namespace,
                       hydra.ext.haskell.ast.ModuleName], scala.collection.immutable.Set[hydra.ext.haskell.ast.ModuleName]](finalState)
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

def newtypeAccessorName(name: hydra.core.Name): scala.Predef.String = strings.cat2("un")(hydra.names.localNameOf(name))

def rawName(n: scala.Predef.String): hydra.ext.haskell.ast.Name = hydra.ext.haskell.ast.Name.normal(hydra.ext.haskell.ast.QualifiedName(Seq(), n))

def recordFieldReference(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName])(sname: hydra.core.Name)(fname: hydra.core.Name): hydra.ext.haskell.ast.Name =
  {
  val fnameStr: scala.Predef.String = fname
  val qname: hydra.module.QualifiedName = hydra.names.qualifyName(sname)
  val ns: Option[hydra.module.Namespace] = (qname.namespace)
  val typeNameStr: scala.Predef.String = hydra.ext.haskell.utils.typeNameForRecord(sname)
  val decapitalized: scala.Predef.String = hydra.formatting.decapitalize(typeNameStr)
  val capitalized: scala.Predef.String = hydra.formatting.capitalize(fnameStr)
  val nm: scala.Predef.String = strings.cat2(decapitalized)(capitalized)
  val qualName: hydra.module.QualifiedName = hydra.module.QualifiedName(ns, nm)
  val unqualName: hydra.core.Name = hydra.names.unqualifyName(qualName)
  hydra.ext.haskell.utils.elementReference(namespaces)(unqualName)
}

def sanitizeHaskellName(v1: scala.Predef.String): scala.Predef.String =
  hydra.formatting.sanitizeWithUnderscores(hydra.ext.haskell.language.reservedWords)(v1)

def simpleName(`arg_`: scala.Predef.String): hydra.ext.haskell.ast.Name =
  hydra.ext.haskell.utils.rawName(hydra.ext.haskell.utils.sanitizeHaskellName(`arg_`))

def simpleValueBinding(hname: hydra.ext.haskell.ast.Name)(rhs: hydra.ext.haskell.ast.Expression)(bindings: Option[hydra.ext.haskell.ast.LocalBindings]): hydra.ext.haskell.ast.ValueBinding =
  {
  val pat: hydra.ext.haskell.ast.Pattern = hydra.ext.haskell.ast.Pattern.application(hydra.ext.haskell.ast.ApplicationPattern(hname, Seq()))
  val rightHandSide: hydra.ext.haskell.ast.RightHandSide = rhs
  hydra.ext.haskell.ast.ValueBinding.simple(hydra.ext.haskell.ast.SimpleValueBinding(pat, rightHandSide, bindings))
}

def toTypeApplication(types: Seq[hydra.ext.haskell.ast.Type]): hydra.ext.haskell.ast.Type =
  {
  def app(l: Seq[hydra.ext.haskell.ast.Type]): hydra.ext.haskell.ast.Type =
    logic.ifElse[hydra.ext.haskell.ast.Type](equality.gt[Int](lists.length[hydra.ext.haskell.ast.Type](l))(1))(hydra.ext.haskell.ast.Type.application(hydra.ext.haskell.ast.ApplicationType(app(lists.tail[hydra.ext.haskell.ast.Type](l)),
       lists.head[hydra.ext.haskell.ast.Type](l))))(lists.head[hydra.ext.haskell.ast.Type](l))
  app(lists.reverse[hydra.ext.haskell.ast.Type](types))
}

def typeNameForRecord(sname: hydra.core.Name): scala.Predef.String =
  {
  val snameStr: scala.Predef.String = sname
  val parts: Seq[scala.Predef.String] = strings.splitOn(".")(snameStr)
  lists.last[scala.Predef.String](parts)
}

def unionFieldReference(boundNames: scala.collection.immutable.Set[hydra.core.Name])(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName])(sname: hydra.core.Name)(fname: hydra.core.Name): hydra.ext.haskell.ast.Name =
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
    logic.ifElse[scala.Predef.String](sets.member[hydra.core.Name](tname)(boundNames))(deconflict(strings.cat2(name)("_")))(name)
  }
  val nm: scala.Predef.String = deconflict(strings.cat2(capitalizedTypeName)(capitalizedFieldName))
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
    val vars: Seq[hydra.core.Name] = pairs.first[Seq[hydra.core.Name], hydra.core.Type](recursiveResult)
    val finalType: hydra.core.Type = pairs.second[Seq[hydra.core.Name], hydra.core.Type](recursiveResult)
    Tuple2(lists.cons[hydra.core.Name](v)(vars), finalType)
  }
  case _ => Tuple2(Seq(), t)
