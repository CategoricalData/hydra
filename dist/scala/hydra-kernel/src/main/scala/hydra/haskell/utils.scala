package hydra.haskell.utils

import hydra.core.*

import hydra.haskell.syntax.*

import hydra.packaging.*

def applicationPattern(name: hydra.haskell.syntax.Name)(args: Seq[hydra.haskell.syntax.Pattern]): hydra.haskell.syntax.Pattern =
  hydra.haskell.syntax.Pattern.application(hydra.haskell.syntax.ApplicationPattern(name, args))

def elementReference(namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName])(name: hydra.core.Name): hydra.haskell.syntax.Name =
  {
  lazy val namespacePair: Tuple2[hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName] = (namespaces.focus)
  lazy val gname: hydra.packaging.Namespace = hydra.lib.pairs.first[hydra.packaging.Namespace,
     hydra.haskell.syntax.ModuleName](namespacePair)
  lazy val gmod: scala.Predef.String = hydra.lib.pairs.second[hydra.packaging.Namespace,
     hydra.haskell.syntax.ModuleName](namespacePair)
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
  hydra.haskell.syntax.Expression.lambda(hydra.haskell.syntax.LambdaExpression(Seq(hydra.haskell.syntax.Pattern.name(name)),
     rhs))

def hslit(lit: hydra.haskell.syntax.Literal): hydra.haskell.syntax.Expression = hydra.haskell.syntax.Expression.literal(lit)

def hsvar(s: scala.Predef.String): hydra.haskell.syntax.Expression = hydra.haskell.syntax.Expression.variable(hydra.haskell.utils.rawName(s))

def namespacesForModule[T0](mod: hydra.packaging.Module)(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName]] =
  hydra.lib.eithers.bind[hydra.errors.Error, scala.collection.immutable.Set[hydra.packaging.Namespace],
     hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName]](hydra.analysis.moduleDependencyNamespaces(cx)(g)(true)(true)(true)(true)(mod))((nss: scala.collection.immutable.Set[hydra.packaging.Namespace]) =>
  {
  lazy val ns: hydra.packaging.Namespace = (mod.namespace)
  {
    def segmentsOf(namespace: hydra.packaging.Namespace): Seq[scala.Predef.String] = hydra.lib.strings.splitOn(".")(namespace)
    {
      def aliasFromSuffix(segs: Seq[scala.Predef.String])(n: Int): hydra.haskell.syntax.ModuleName =
        {
        lazy val dropCount: Int = hydra.lib.math.sub(hydra.lib.lists.length[scala.Predef.String](segs))(n)
        lazy val suffix: Seq[scala.Predef.String] = hydra.lib.lists.drop[scala.Predef.String](dropCount)(segs)
        lazy val capitalizedSuffix: Seq[scala.Predef.String] = hydra.lib.lists.map[scala.Predef.String,
           scala.Predef.String](hydra.formatting.capitalize)(suffix)
        hydra.lib.strings.cat(capitalizedSuffix)
      }
      {
        def toModuleName(namespace: hydra.packaging.Namespace): hydra.haskell.syntax.ModuleName = aliasFromSuffix(segmentsOf(namespace))(1)
        {
          lazy val focusPair: Tuple2[hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName] = Tuple2(ns,
             toModuleName(ns))
          {
            lazy val nssAsList: Seq[hydra.packaging.Namespace] = hydra.lib.sets.toList[hydra.packaging.Namespace](nss)
            {
              lazy val segsMap: Map[hydra.packaging.Namespace, Seq[scala.Predef.String]] = hydra.lib.maps.fromList[hydra.packaging.Namespace,
                 Seq[scala.Predef.String]](hydra.lib.lists.map[hydra.packaging.Namespace,
                 Tuple2[hydra.packaging.Namespace, Seq[scala.Predef.String]]]((nm: hydra.packaging.Namespace) => Tuple2(nm,
                 segmentsOf(nm)))(nssAsList))
              {
                lazy val maxSegs: Int = hydra.lib.lists.foldl[Int, Int]((a: Int) =>
                  (b: Int) =>
                  hydra.lib.logic.ifElse[Int](hydra.lib.equality.gt[Int](a)(b))(a)(b))(1)(hydra.lib.lists.map[hydra.packaging.Namespace,
                     Int]((nm: hydra.packaging.Namespace) => hydra.lib.lists.length[scala.Predef.String](segmentsOf(nm)))(nssAsList))
                {
                  lazy val initialState: Map[hydra.packaging.Namespace, Int] = hydra.lib.maps.fromList[hydra.packaging.Namespace,
                     Int](hydra.lib.lists.map[hydra.packaging.Namespace, Tuple2[hydra.packaging.Namespace,
                     Int]]((nm: hydra.packaging.Namespace) => Tuple2(nm, 1))(nssAsList))
                  {
                    def segsFor(nm: hydra.packaging.Namespace): Seq[scala.Predef.String] =
                      hydra.lib.maybes.fromMaybe[Seq[scala.Predef.String]](Seq())(hydra.lib.maps.lookup[hydra.packaging.Namespace,
                         Seq[scala.Predef.String]](nm)(segsMap))
                    {
                      def takenFor[T1](state: Map[T1, Int])(nm: T1): Int = hydra.lib.maybes.fromMaybe[Int](1)(hydra.lib.maps.lookup[T1,
                         Int](nm)(state))
                      {
                        def growStep[T1](state: Map[hydra.packaging.Namespace, Int])(_ign: T1): Map[hydra.packaging.Namespace,
                           Int] =
                          {
                          lazy val aliasEntries: Seq[Tuple2[hydra.packaging.Namespace,
                             Tuple2[Int, Tuple2[Int, scala.Predef.String]]]] = hydra.lib.lists.map[hydra.packaging.Namespace,
                             Tuple2[hydra.packaging.Namespace, Tuple2[Int, Tuple2[Int,
                             scala.Predef.String]]]]((nm: hydra.packaging.Namespace) =>
                            {
                            lazy val segs: Seq[scala.Predef.String] = segsFor(nm)
                            lazy val n: Int = takenFor(state)(nm)
                            lazy val segCount: Int = hydra.lib.lists.length[scala.Predef.String](segs)
                            lazy val aliasStr: scala.Predef.String = aliasFromSuffix(segs)(n)
                            Tuple2(nm, Tuple2(n, Tuple2(segCount, aliasStr)))
                          })(nssAsList)
                          lazy val aliasCounts: Map[scala.Predef.String, Int] = hydra.lib.lists.foldl[Map[scala.Predef.String,
                             Int], Tuple2[hydra.packaging.Namespace, Tuple2[Int, Tuple2[Int,
                             scala.Predef.String]]]]((m: Map[scala.Predef.String,
                             Int]) =>
                            (e: Tuple2[hydra.packaging.Namespace, Tuple2[Int, Tuple2[Int, scala.Predef.String]]]) =>
                            {
                            lazy val k: scala.Predef.String = hydra.lib.pairs.second[Int,
                               scala.Predef.String](hydra.lib.pairs.second[Int, Tuple2[Int,
                               scala.Predef.String]](hydra.lib.pairs.second[hydra.packaging.Namespace,
                               Tuple2[Int, Tuple2[Int, scala.Predef.String]]](e)))
                            hydra.lib.maps.insert[scala.Predef.String, Int](k)(hydra.lib.math.add(1)(hydra.lib.maybes.fromMaybe[Int](0)(hydra.lib.maps.lookup[scala.Predef.String,
                               Int](k)(m))))(m)
                          })(hydra.lib.maps.empty[scala.Predef.String, Int])(aliasEntries)
                          lazy val aliasMinSegs: Map[scala.Predef.String, Int] = hydra.lib.lists.foldl[Map[scala.Predef.String,
                             Int], Tuple2[hydra.packaging.Namespace, Tuple2[Int, Tuple2[Int,
                             scala.Predef.String]]]]((m: Map[scala.Predef.String,
                             Int]) =>
                            (e: Tuple2[hydra.packaging.Namespace, Tuple2[Int, Tuple2[Int, scala.Predef.String]]]) =>
                            {
                            lazy val segCount: Int = hydra.lib.pairs.first[Int, scala.Predef.String](hydra.lib.pairs.second[Int,
                               Tuple2[Int, scala.Predef.String]](hydra.lib.pairs.second[hydra.packaging.Namespace,
                               Tuple2[Int, Tuple2[Int, scala.Predef.String]]](e)))
                            lazy val k: scala.Predef.String = hydra.lib.pairs.second[Int,
                               scala.Predef.String](hydra.lib.pairs.second[Int, Tuple2[Int,
                               scala.Predef.String]](hydra.lib.pairs.second[hydra.packaging.Namespace,
                               Tuple2[Int, Tuple2[Int, scala.Predef.String]]](e)))
                            lazy val existing: Option[Int] = hydra.lib.maps.lookup[scala.Predef.String, Int](k)(m)
                            hydra.lib.maps.insert[scala.Predef.String, Int](k)(hydra.lib.maybes.cases[Int,
                               Int](existing)(segCount)((prev: Int) =>
                              hydra.lib.logic.ifElse[Int](hydra.lib.equality.lt[Int](segCount)(prev))(segCount)(prev)))(m)
                          })(hydra.lib.maps.empty[scala.Predef.String, Int])(aliasEntries)
                          lazy val aliasMinSegsCount: Map[scala.Predef.String, Int] = hydra.lib.lists.foldl[Map[scala.Predef.String,
                             Int], Tuple2[hydra.packaging.Namespace, Tuple2[Int, Tuple2[Int,
                             scala.Predef.String]]]]((m: Map[scala.Predef.String,
                             Int]) =>
                            (e: Tuple2[hydra.packaging.Namespace, Tuple2[Int, Tuple2[Int, scala.Predef.String]]]) =>
                            {
                            lazy val segCount: Int = hydra.lib.pairs.first[Int, scala.Predef.String](hydra.lib.pairs.second[Int,
                               Tuple2[Int, scala.Predef.String]](hydra.lib.pairs.second[hydra.packaging.Namespace,
                               Tuple2[Int, Tuple2[Int, scala.Predef.String]]](e)))
                            lazy val k: scala.Predef.String = hydra.lib.pairs.second[Int,
                               scala.Predef.String](hydra.lib.pairs.second[Int, Tuple2[Int,
                               scala.Predef.String]](hydra.lib.pairs.second[hydra.packaging.Namespace,
                               Tuple2[Int, Tuple2[Int, scala.Predef.String]]](e)))
                            lazy val minSegs: Int = hydra.lib.maybes.fromMaybe[Int](segCount)(hydra.lib.maps.lookup[scala.Predef.String,
                               Int](k)(aliasMinSegs))
                            hydra.lib.logic.ifElse[Map[scala.Predef.String, Int]](hydra.lib.equality.equal[Int](segCount)(minSegs))(hydra.lib.maps.insert[scala.Predef.String,
                               Int](k)(hydra.lib.math.add(1)(hydra.lib.maybes.fromMaybe[Int](0)(hydra.lib.maps.lookup[scala.Predef.String,
                               Int](k)(m))))(m))(m)
                          })(hydra.lib.maps.empty[scala.Predef.String, Int])(aliasEntries)
                          hydra.lib.maps.fromList[hydra.packaging.Namespace, Int](hydra.lib.lists.map[Tuple2[hydra.packaging.Namespace,
                             Tuple2[Int, Tuple2[Int, scala.Predef.String]]], Tuple2[hydra.packaging.Namespace,
                             Int]]((e: Tuple2[hydra.packaging.Namespace, Tuple2[Int,
                             Tuple2[Int, scala.Predef.String]]]) =>
                            {
                            lazy val nm: hydra.packaging.Namespace = hydra.lib.pairs.first[hydra.packaging.Namespace,
                               Tuple2[Int, Tuple2[Int, scala.Predef.String]]](e)
                            lazy val n: Int = hydra.lib.pairs.first[Int, Tuple2[Int,
                               scala.Predef.String]](hydra.lib.pairs.second[hydra.packaging.Namespace,
                               Tuple2[Int, Tuple2[Int, scala.Predef.String]]](e))
                            lazy val segCount: Int = hydra.lib.pairs.first[Int, scala.Predef.String](hydra.lib.pairs.second[Int,
                               Tuple2[Int, scala.Predef.String]](hydra.lib.pairs.second[hydra.packaging.Namespace,
                               Tuple2[Int, Tuple2[Int, scala.Predef.String]]](e)))
                            lazy val aliasStr: scala.Predef.String = hydra.lib.pairs.second[Int,
                               scala.Predef.String](hydra.lib.pairs.second[Int, Tuple2[Int,
                               scala.Predef.String]](hydra.lib.pairs.second[hydra.packaging.Namespace,
                               Tuple2[Int, Tuple2[Int, scala.Predef.String]]](e)))
                            lazy val count: Int = hydra.lib.maybes.fromMaybe[Int](0)(hydra.lib.maps.lookup[scala.Predef.String,
                               Int](aliasStr)(aliasCounts))
                            lazy val minSegs: Int = hydra.lib.maybes.fromMaybe[Int](segCount)(hydra.lib.maps.lookup[scala.Predef.String,
                               Int](aliasStr)(aliasMinSegs))
                            lazy val minSegsCount: Int = hydra.lib.maybes.fromMaybe[Int](0)(hydra.lib.maps.lookup[scala.Predef.String,
                               Int](aliasStr)(aliasMinSegsCount))
                            lazy val canGrow: Boolean = hydra.lib.logic.and(hydra.lib.equality.gt[Int](count)(1))(hydra.lib.logic.and(hydra.lib.equality.gt[Int](segCount)(n))(hydra.lib.logic.or(hydra.lib.equality.gt[Int](segCount)(minSegs))(hydra.lib.equality.gt[Int](minSegsCount)(1))))
                            lazy val newN: Int = hydra.lib.logic.ifElse[Int](canGrow)(hydra.lib.math.add(n)(1))(n)
                            Tuple2(nm, newN)
                          })(aliasEntries))
                        }
                        {
                          lazy val finalState: Map[hydra.packaging.Namespace, Int] = hydra.lib.lists.foldl[Map[hydra.packaging.Namespace,
                             Int], Unit](growStep)(initialState)(hydra.lib.lists.replicate[Unit](maxSegs)(()))
                          {
                            lazy val resultMap: Map[hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName] = hydra.lib.maps.fromList[hydra.packaging.Namespace,
                               hydra.haskell.syntax.ModuleName](hydra.lib.lists.map[hydra.packaging.Namespace,
                               Tuple2[hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName]]((nm: hydra.packaging.Namespace) =>
                              Tuple2(nm, aliasFromSuffix(segsFor(nm))(takenFor(finalState)(nm))))(nssAsList))
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
        }
      }
    }
  }
})

def newtypeAccessorName(name: hydra.core.Name): scala.Predef.String = hydra.lib.strings.cat2("un")(hydra.names.localNameOf(name))

def rawName(n: scala.Predef.String): hydra.haskell.syntax.Name = hydra.haskell.syntax.Name.normal(hydra.haskell.syntax.QualifiedName(Seq(),
   n))

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
  lazy val pat: hydra.haskell.syntax.Pattern = hydra.haskell.syntax.Pattern.application(hydra.haskell.syntax.ApplicationPattern(hname,
     Seq()))
  lazy val rightHandSide: hydra.haskell.syntax.RightHandSide = rhs
  hydra.haskell.syntax.ValueBinding.simple(hydra.haskell.syntax.SimpleValueBinding(pat, rightHandSide, bindings))
}

def toTypeApplication(types: Seq[hydra.haskell.syntax.Type]): hydra.haskell.syntax.Type =
  {
  lazy val dummyType: hydra.haskell.syntax.Type = hydra.haskell.syntax.Type.variable(hydra.haskell.syntax.Name.normal(hydra.haskell.syntax.QualifiedName(Seq(),
     "")))
  def app(l: Seq[hydra.haskell.syntax.Type]): hydra.haskell.syntax.Type =
    hydra.lib.maybes.fromMaybe[hydra.haskell.syntax.Type](dummyType)(hydra.lib.maybes.map[Tuple2[hydra.haskell.syntax.Type,
       Seq[hydra.haskell.syntax.Type]], hydra.haskell.syntax.Type]((p: Tuple2[hydra.haskell.syntax.Type,
       Seq[hydra.haskell.syntax.Type]]) =>
    hydra.lib.logic.ifElse[hydra.haskell.syntax.Type](hydra.lib.lists.`null`[hydra.haskell.syntax.Type](hydra.lib.pairs.second[hydra.haskell.syntax.Type,
       Seq[hydra.haskell.syntax.Type]](p)))(hydra.lib.pairs.first[hydra.haskell.syntax.Type,
       Seq[hydra.haskell.syntax.Type]](p))(hydra.haskell.syntax.Type.application(hydra.haskell.syntax.ApplicationType(app(hydra.lib.pairs.second[hydra.haskell.syntax.Type,
       Seq[hydra.haskell.syntax.Type]](p)), hydra.lib.pairs.first[hydra.haskell.syntax.Type,
       Seq[hydra.haskell.syntax.Type]](p)))))(hydra.lib.lists.uncons[hydra.haskell.syntax.Type](l)))
  app(hydra.lib.lists.reverse[hydra.haskell.syntax.Type](types))
}

def typeNameForRecord(sname: hydra.core.Name): scala.Predef.String =
  {
  lazy val snameStr: scala.Predef.String = sname
  lazy val parts: Seq[scala.Predef.String] = hydra.lib.strings.splitOn(".")(snameStr)
  hydra.lib.maybes.fromMaybe[scala.Predef.String](snameStr)(hydra.lib.lists.maybeLast[scala.Predef.String](parts))
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
