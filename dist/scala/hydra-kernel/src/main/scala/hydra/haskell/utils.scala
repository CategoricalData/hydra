package hydra.haskell.utils

import hydra.core.*

import hydra.haskell.syntax.*

import hydra.packaging.*

def applicationPattern(name: hydra.haskell.syntax.Name)(args: Seq[hydra.haskell.syntax.Pattern]): hydra.haskell.syntax.Pattern =
  hydra.haskell.syntax.Pattern.application(hydra.haskell.syntax.ApplicationPattern(name, args))

def elementReference(namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName])(name: hydra.core.Name): hydra.haskell.syntax.Name =
  {
  lazy val namespacePair: Tuple2[hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName] = (namespaces.focus)
  lazy val gname: hydra.packaging.Namespace = hydra.lib.pairs.first[hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName](namespacePair)
  lazy val gmod: scala.Predef.String = hydra.lib.pairs.second[hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName](namespacePair)
  lazy val namespacesMap: Map[hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName] = (namespaces.mapping)
  lazy val qname: hydra.packaging.QualifiedName = hydra.names.qualifyName(name)
  lazy val local: scala.Predef.String = (qname.local)
  lazy val escLocal: scala.Predef.String = hydra.haskell.utils.sanitizeHaskellName(local)
  lazy val mns: Option[hydra.packaging.Namespace] = (qname.namespace)
  hydra.lib.maybes.cases[hydra.packaging.Namespace, hydra.haskell.syntax.Name](qname.namespace)(hydra.haskell.utils.simpleName(local))((ns: hydra.packaging.Namespace) =>
    hydra.lib.maybes.cases[hydra.haskell.syntax.ModuleName, hydra.haskell.syntax.Name](hydra.lib.maps.lookup[hydra.packaging.Namespace,
       hydra.haskell.syntax.ModuleName](ns)(namespacesMap))(hydra.haskell.utils.simpleName(local))((mn: hydra.haskell.syntax.ModuleName) =>
    {
    lazy val aliasStr: scala.Predef.String = mn
    hydra.lib.logic.ifElse[hydra.haskell.syntax.Name](hydra.lib.equality.equal[hydra.packaging.Namespace](ns)(gname))(hydra.haskell.utils.simpleName(escLocal))(hydra.haskell.utils.rawName(hydra.lib.strings.cat(Seq(aliasStr,
       ".", hydra.haskell.utils.sanitizeHaskellName(local)))))
  }))
}

def hsapp(l: hydra.haskell.syntax.Expression)(r: hydra.haskell.syntax.Expression): hydra.haskell.syntax.Expression =
  hydra.haskell.syntax.Expression.application(hydra.haskell.syntax.ApplicationExpression(l, r))

def hslambda(name: hydra.haskell.syntax.Name)(rhs: hydra.haskell.syntax.Expression): hydra.haskell.syntax.Expression =
  hydra.haskell.syntax.Expression.lambda(hydra.haskell.syntax.LambdaExpression(Seq(hydra.haskell.syntax.Pattern.name(name)), rhs))

def hslit(lit: hydra.haskell.syntax.Literal): hydra.haskell.syntax.Expression = hydra.haskell.syntax.Expression.literal(lit)

def hsvar(s: scala.Predef.String): hydra.haskell.syntax.Expression = hydra.haskell.syntax.Expression.variable(hydra.haskell.utils.rawName(s))

def namespacesForModule[T0](mod: hydra.packaging.Module)(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName]] =
  hydra.lib.eithers.bind[hydra.errors.Error, scala.collection.immutable.Set[hydra.packaging.Namespace],
     hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName]](hydra.analysis.moduleDependencyNamespaces(cx)(g)(true)(true)(true)(true)(mod))((nss: scala.collection.immutable.Set[hydra.packaging.Namespace]) =>
  {
  lazy val ns: hydra.packaging.Namespace = (mod.namespace)
  {
    def toModuleName(namespace: hydra.packaging.Namespace): hydra.haskell.syntax.ModuleName =
      {
      lazy val namespaceStr: scala.Predef.String = namespace
      lazy val parts: Seq[scala.Predef.String] = hydra.lib.strings.splitOn(".")(namespaceStr)
      lazy val lastPart: scala.Predef.String = hydra.lib.lists.last[scala.Predef.String](parts)
      lazy val capitalized: scala.Predef.String = hydra.formatting.capitalize(lastPart)
      capitalized
    }
    {
      def toPair(name: hydra.packaging.Namespace): Tuple2[hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName] = Tuple2(name, toModuleName(name))
      {
        def addPair[T1](state: Tuple2[Map[T1, hydra.haskell.syntax.ModuleName], scala.collection.immutable.Set[hydra.haskell.syntax.ModuleName]])(namePair: Tuple2[T1,
           hydra.haskell.syntax.ModuleName]): Tuple2[Map[T1, hydra.haskell.syntax.ModuleName], scala.collection.immutable.Set[hydra.haskell.syntax.ModuleName]] =
          {
          lazy val currentMap: Map[T1, hydra.haskell.syntax.ModuleName] = hydra.lib.pairs.first[Map[T1,
             hydra.haskell.syntax.ModuleName], scala.collection.immutable.Set[hydra.haskell.syntax.ModuleName]](state)
          lazy val currentSet: scala.collection.immutable.Set[hydra.haskell.syntax.ModuleName] = hydra.lib.pairs.second[Map[T1,
             hydra.haskell.syntax.ModuleName], scala.collection.immutable.Set[hydra.haskell.syntax.ModuleName]](state)
          lazy val name: T1 = hydra.lib.pairs.first[T1, hydra.haskell.syntax.ModuleName](namePair)
          lazy val alias: hydra.haskell.syntax.ModuleName = hydra.lib.pairs.second[T1, hydra.haskell.syntax.ModuleName](namePair)
          lazy val aliasStr: scala.Predef.String = alias
          hydra.lib.logic.ifElse[Tuple2[Map[T1, hydra.haskell.syntax.ModuleName], scala.collection.immutable.Set[hydra.haskell.syntax.ModuleName]]](hydra.lib.sets.member[hydra.haskell.syntax.ModuleName](alias)(currentSet))(addPair(state)(Tuple2(name,
             hydra.lib.strings.cat2(aliasStr)("_"))))(Tuple2(hydra.lib.maps.insert[T1, hydra.haskell.syntax.ModuleName](name)(alias)(currentMap),
             hydra.lib.sets.insert[hydra.haskell.syntax.ModuleName](alias)(currentSet)))
        }
        {
          lazy val focusPair: Tuple2[hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName] = toPair(ns)
          {
            lazy val nssAsList: Seq[hydra.packaging.Namespace] = hydra.lib.sets.toList[hydra.packaging.Namespace](nss)
            {
              lazy val nssPairs: Seq[Tuple2[hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName]] = hydra.lib.lists.map[hydra.packaging.Namespace,
                 Tuple2[hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName]](toPair)(nssAsList)
              {
                def emptyState[T1, T2, T3]: Tuple2[Map[T1, T2], scala.collection.immutable.Set[T3]] = Tuple2(hydra.lib.maps.empty[T1,
                   T2], hydra.lib.sets.empty[T3])
                {
                  lazy val finalState: Tuple2[Map[hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName],
                     scala.collection.immutable.Set[hydra.haskell.syntax.ModuleName]] = hydra.lib.lists.foldl[Tuple2[Map[hydra.packaging.Namespace,
                     hydra.haskell.syntax.ModuleName], scala.collection.immutable.Set[hydra.haskell.syntax.ModuleName]],
                     Tuple2[hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName]](addPair)(emptyState)(nssPairs)
                  {
                    lazy val resultMap: Map[hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName] = hydra.lib.pairs.first[Map[hydra.packaging.Namespace,
                       hydra.haskell.syntax.ModuleName], scala.collection.immutable.Set[hydra.haskell.syntax.ModuleName]](finalState)
                    Right(hydra.packaging.Namespaces(focusPair, resultMap))
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

def rawName(n: scala.Predef.String): hydra.haskell.syntax.Name = hydra.haskell.syntax.Name.normal(hydra.haskell.syntax.QualifiedName(Seq(), n))

def recordFieldReference(namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName])(sname: hydra.core.Name)(fname: hydra.core.Name): hydra.haskell.syntax.Name =
  {
  lazy val fnameStr: scala.Predef.String = fname
  lazy val qname: hydra.packaging.QualifiedName = hydra.names.qualifyName(sname)
  lazy val ns: Option[hydra.packaging.Namespace] = (qname.namespace)
  lazy val typeNameStr: scala.Predef.String = hydra.haskell.utils.typeNameForRecord(sname)
  lazy val decapitalized: scala.Predef.String = hydra.formatting.decapitalize(typeNameStr)
  lazy val capitalized: scala.Predef.String = hydra.formatting.capitalize(fnameStr)
  lazy val nm: scala.Predef.String = hydra.lib.strings.cat2(decapitalized)(capitalized)
  lazy val qualName: hydra.packaging.QualifiedName = hydra.packaging.QualifiedName(ns, nm)
  lazy val unqualName: hydra.core.Name = hydra.names.unqualifyName(qualName)
  hydra.haskell.utils.elementReference(namespaces)(unqualName)
}

def sanitizeHaskellName(v1: scala.Predef.String): scala.Predef.String =
  hydra.formatting.sanitizeWithUnderscores(hydra.haskell.language.reservedWords)(v1)

def simpleName(`arg_`: scala.Predef.String): hydra.haskell.syntax.Name = hydra.haskell.utils.rawName(hydra.haskell.utils.sanitizeHaskellName(`arg_`))

def simpleValueBinding(hname: hydra.haskell.syntax.Name)(rhs: hydra.haskell.syntax.Expression)(bindings: Option[hydra.haskell.syntax.LocalBindings]): hydra.haskell.syntax.ValueBinding =
  {
  lazy val pat: hydra.haskell.syntax.Pattern = hydra.haskell.syntax.Pattern.application(hydra.haskell.syntax.ApplicationPattern(hname, Seq()))
  lazy val rightHandSide: hydra.haskell.syntax.RightHandSide = rhs
  hydra.haskell.syntax.ValueBinding.simple(hydra.haskell.syntax.SimpleValueBinding(pat, rightHandSide, bindings))
}

def toTypeApplication(types: Seq[hydra.haskell.syntax.Type]): hydra.haskell.syntax.Type =
  {
  def app(l: Seq[hydra.haskell.syntax.Type]): hydra.haskell.syntax.Type =
    hydra.lib.logic.ifElse[hydra.haskell.syntax.Type](hydra.lib.equality.gt[Int](hydra.lib.lists.length[hydra.haskell.syntax.Type](l))(1))(hydra.haskell.syntax.Type.application(hydra.haskell.syntax.ApplicationType(app(hydra.lib.lists.tail[hydra.haskell.syntax.Type](l)),
       hydra.lib.lists.head[hydra.haskell.syntax.Type](l))))(hydra.lib.lists.head[hydra.haskell.syntax.Type](l))
  app(hydra.lib.lists.reverse[hydra.haskell.syntax.Type](types))
}

def typeNameForRecord(sname: hydra.core.Name): scala.Predef.String =
  {
  lazy val snameStr: scala.Predef.String = sname
  lazy val parts: Seq[scala.Predef.String] = hydra.lib.strings.splitOn(".")(snameStr)
  hydra.lib.lists.last[scala.Predef.String](parts)
}

def unionFieldReference(boundNames: scala.collection.immutable.Set[hydra.core.Name])(namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName])(sname: hydra.core.Name)(fname: hydra.core.Name): hydra.haskell.syntax.Name =
  {
  lazy val fnameStr: scala.Predef.String = fname
  lazy val qname: hydra.packaging.QualifiedName = hydra.names.qualifyName(sname)
  lazy val ns: Option[hydra.packaging.Namespace] = (qname.namespace)
  lazy val typeNameStr: scala.Predef.String = hydra.haskell.utils.typeNameForRecord(sname)
  lazy val capitalizedTypeName: scala.Predef.String = hydra.formatting.capitalize(typeNameStr)
  lazy val capitalizedFieldName: scala.Predef.String = hydra.formatting.capitalize(fnameStr)
  def deconflict(name: scala.Predef.String): scala.Predef.String =
    {
    lazy val tname: hydra.core.Name = hydra.names.unqualifyName(hydra.packaging.QualifiedName(ns, name))
    hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.sets.member[hydra.core.Name](tname)(boundNames))(deconflict(hydra.lib.strings.cat2(name)("_")))(name)
  }
  lazy val nm: scala.Predef.String = deconflict(hydra.lib.strings.cat2(capitalizedTypeName)(capitalizedFieldName))
  lazy val qualName: hydra.packaging.QualifiedName = hydra.packaging.QualifiedName(ns, nm)
  lazy val unqualName: hydra.core.Name = hydra.names.unqualifyName(qualName)
  hydra.haskell.utils.elementReference(namespaces)(unqualName)
}

def unpackForallType(t: hydra.core.Type): Tuple2[Seq[hydra.core.Name], hydra.core.Type] =
  hydra.strip.deannotateType(t) match
  case hydra.core.Type.forall(v_Type_forall_fat) => {
    lazy val v: hydra.core.Name = (v_Type_forall_fat.parameter)
    lazy val tbody: hydra.core.Type = (v_Type_forall_fat.body)
    lazy val recursiveResult: Tuple2[Seq[hydra.core.Name], hydra.core.Type] = hydra.haskell.utils.unpackForallType(tbody)
    lazy val vars: Seq[hydra.core.Name] = hydra.lib.pairs.first[Seq[hydra.core.Name], hydra.core.Type](recursiveResult)
    lazy val finalType: hydra.core.Type = hydra.lib.pairs.second[Seq[hydra.core.Name], hydra.core.Type](recursiveResult)
    Tuple2(hydra.lib.lists.cons[hydra.core.Name](v)(vars), finalType)
  }
  case _ => Tuple2(Seq(), t)
