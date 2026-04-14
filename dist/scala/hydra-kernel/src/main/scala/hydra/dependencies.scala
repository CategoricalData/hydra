package hydra.dependencies

import hydra.coders.*

import hydra.core.*

import hydra.errors.*

import hydra.packaging.*

def definitionsWithDependencies[T0](cx: T0)(graph: hydra.graph.Graph)(original: Seq[hydra.core.Binding]): Either[hydra.errors.Error, Seq[hydra.core.Binding]] =
  {
  def depNames(el: hydra.core.Binding): Seq[hydra.core.Name] =
    hydra.lib.sets.toList[hydra.core.Name](hydra.dependencies.termDependencyNames(true)(false)(false)(el.term))
  lazy val allDepNames: Seq[hydra.core.Name] = hydra.lib.lists.nub[hydra.core.Name](hydra.lib.lists.concat2[hydra.core.Name](hydra.lib.lists.map[hydra.core.Binding,
     hydra.core.Name]((x: hydra.core.Binding) => (x.name))(original))(hydra.lib.lists.concat[hydra.core.Name](hydra.lib.lists.map[hydra.core.Binding,
     Seq[hydra.core.Name]](depNames)(original))))
  hydra.lib.eithers.mapList[hydra.core.Name, hydra.core.Binding, hydra.errors.Error]((name: hydra.core.Name) => hydra.lexical.requireBinding(graph)(name))(allDepNames)
}

def flattenLetTerms(term: hydra.core.Term): hydra.core.Term =
  {
  def rewriteBinding(binding: hydra.core.Binding): Tuple2[hydra.core.Binding, Seq[hydra.core.Binding]] =
    {
    lazy val key0: hydra.core.Name = (binding.name)
    lazy val val0: hydra.core.Term = (binding.term)
    lazy val t: Option[hydra.core.TypeScheme] = (binding.`type`)
    val0 match
      case hydra.core.Term.annotated(v_Term_annotated_at) => {
        lazy val val1: hydra.core.Term = (v_Term_annotated_at.body)
        {
          lazy val ann: Map[hydra.core.Name, hydra.core.Term] = (v_Term_annotated_at.annotation)
          {
            lazy val recursive: Tuple2[hydra.core.Binding, Seq[hydra.core.Binding]] = rewriteBinding(hydra.core.Binding(key0, val1, t))
            {
              lazy val innerBinding: hydra.core.Binding = hydra.lib.pairs.first[hydra.core.Binding, Seq[hydra.core.Binding]](recursive)
              {
                lazy val deps: Seq[hydra.core.Binding] = hydra.lib.pairs.second[hydra.core.Binding, Seq[hydra.core.Binding]](recursive)
                {
                  lazy val val2: hydra.core.Term = (innerBinding.term)
                  Tuple2(hydra.core.Binding(key0, hydra.core.Term.annotated(hydra.core.AnnotatedTerm(val2, ann)), t), deps)
                }
              }
            }
          }
        }
      }
      case hydra.core.Term.let(v_Term_let_innerLet) => {
        lazy val bindings1: Seq[hydra.core.Binding] = (v_Term_let_innerLet.bindings)
        {
          lazy val body1: hydra.core.Term = (v_Term_let_innerLet.body)
          {
            lazy val prefix: scala.Predef.String = hydra.lib.strings.cat2(key0)("_")
            {
              def qualify(n: hydra.core.Name): hydra.core.Name = hydra.lib.strings.cat2(prefix)(n)
              {
                def toSubstPair(b: hydra.core.Binding): Tuple2[hydra.core.Name, hydra.core.Name] = Tuple2(b.name, qualify(b.name))
                {
                  lazy val subst: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.maps.fromList[hydra.core.Name,
                     hydra.core.Name](hydra.lib.lists.map[hydra.core.Binding, Tuple2[hydra.core.Name,
                     hydra.core.Name]](toSubstPair)(bindings1))
                  {
                    def replaceVars(v1: hydra.core.Term): hydra.core.Term = hydra.variables.substituteVariables(subst)(v1)
                    {
                      lazy val newBody: hydra.core.Term = replaceVars(body1)
                      {
                        def newBinding(b: hydra.core.Binding): hydra.core.Binding = hydra.core.Binding(qualify(b.name), replaceVars(b.term), (b.`type`))
                        Tuple2(hydra.core.Binding(key0, newBody, t), hydra.lib.lists.map[hydra.core.Binding, hydra.core.Binding](newBinding)(bindings1))
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      case _ => Tuple2(hydra.core.Binding(key0, val0, t), Seq())
  }
  def flattenBodyLet(bindings: Seq[hydra.core.Binding])(body: hydra.core.Term): Tuple2[Seq[hydra.core.Binding], hydra.core.Term] =
    body match
    case hydra.core.Term.let(v_Term_let_innerLt) => {
      lazy val innerBindings: Seq[hydra.core.Binding] = (v_Term_let_innerLt.bindings)
      {
        lazy val innerBody: hydra.core.Term = (v_Term_let_innerLt.body)
        flattenBodyLet(hydra.lib.lists.concat2[hydra.core.Binding](bindings)(innerBindings))(innerBody)
      }
    }
    case _ => Tuple2(hydra.lib.lists.concat2[hydra.core.Binding](Seq())(bindings), body)
  def flatten[T0](recurse: (T0 => hydra.core.Term))(term2: T0): hydra.core.Term =
    {
    lazy val rewritten: hydra.core.Term = recurse(term2)
    rewritten match
      case hydra.core.Term.let(v_Term_let_lt) => {
        lazy val bindings: Seq[hydra.core.Binding] = (v_Term_let_lt.bindings)
        {
          lazy val body: hydra.core.Term = (v_Term_let_lt.body)
          {
            def forResult[T1](hr: Tuple2[T1, Seq[T1]]): Seq[T1] =
              hydra.lib.lists.concat2[T1](hydra.lib.pairs.second[T1, Seq[T1]](hr))(hydra.lib.lists.pure[T1](hydra.lib.pairs.first[T1, Seq[T1]](hr)))
            {
              lazy val flattenedBindings: Seq[hydra.core.Binding] = hydra.lib.lists.concat[hydra.core.Binding](hydra.lib.lists.map[hydra.core.Binding,
                 Seq[hydra.core.Binding]]((`arg_`: hydra.core.Binding) => forResult(rewriteBinding(`arg_`)))(bindings))
              {
                lazy val merged: Tuple2[Seq[hydra.core.Binding], hydra.core.Term] = flattenBodyLet(flattenedBindings)(body)
                {
                  lazy val newBindings: Seq[hydra.core.Binding] = hydra.lib.pairs.first[Seq[hydra.core.Binding], hydra.core.Term](merged)
                  {
                    lazy val newBody: hydra.core.Term = hydra.lib.pairs.second[Seq[hydra.core.Binding], hydra.core.Term](merged)
                    hydra.core.Term.let(hydra.core.Let(newBindings, newBody))
                  }
                }
              }
            }
          }
        }
      }
      case _ => rewritten
  }
  hydra.rewriting.rewriteTerm(flatten)(term)
}

def inlineType(schema: Map[hydra.core.Name, hydra.core.Type])(typ: hydra.core.Type): Either[hydra.errors.Error, hydra.core.Type] =
  {
  def f[T0](recurse: (T0 => Either[hydra.errors.Error, hydra.core.Type]))(typ2: T0): Either[hydra.errors.Error, hydra.core.Type] =
    {
    def afterRecurse(tr: hydra.core.Type): Either[hydra.errors.Error, hydra.core.Type] =
      tr match
      case hydra.core.Type.variable(v_Type_variable_v) => hydra.lib.maybes.maybe[Either[hydra.errors.Error,
         hydra.core.Type], hydra.core.Type](Left(hydra.errors.Error.other(hydra.lib.strings.cat2("No such type in schema: ")(v_Type_variable_v))))((v1: hydra.core.Type) => hydra.dependencies.inlineType(schema)(v1))(hydra.lib.maps.lookup[hydra.core.Name,
         hydra.core.Type](v_Type_variable_v)(schema))
      case _ => Right(tr)
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Type, hydra.core.Type](recurse(typ2))((tr: hydra.core.Type) => afterRecurse(tr))
  }
  hydra.rewriting.rewriteTypeM(f)(typ)
}

def isLambda(term: hydra.core.Term): Boolean =
  hydra.strip.deannotateTerm(term) match
  case hydra.core.Term.lambda(v_Term_lambda__) => true
  case hydra.core.Term.let(v_Term_let_lt) => hydra.dependencies.isLambda(v_Term_let_lt.body)
  case _ => false

def liftLambdaAboveLet(term0: hydra.core.Term): hydra.core.Term =
  {
  def rewrite(recurse: (hydra.core.Term => hydra.core.Term))(term: hydra.core.Term): hydra.core.Term =
    {
    def rewriteBinding(b: hydra.core.Binding): hydra.core.Binding = hydra.core.Binding(b.name, rewrite(recurse)(b.term), (b.`type`))
    def rewriteBindings(bs: Seq[hydra.core.Binding]): Seq[hydra.core.Binding] = hydra.lib.lists.map[hydra.core.Binding, hydra.core.Binding](rewriteBinding)(bs)
    def digForLambdas(original: hydra.core.Term)(cons: (hydra.core.Term => hydra.core.Term))(term2: hydra.core.Term): hydra.core.Term =
      term2 match
      case hydra.core.Term.annotated(v_Term_annotated_at) => digForLambdas(original)((t: hydra.core.Term) =>
        hydra.core.Term.annotated(hydra.core.AnnotatedTerm(cons(t), (v_Term_annotated_at.annotation))))(v_Term_annotated_at.body)
      case hydra.core.Term.lambda(v_Term_lambda_l) => hydra.core.Term.lambda(hydra.core.Lambda(v_Term_lambda_l.parameter,
         (v_Term_lambda_l.domain), digForLambdas(cons(v_Term_lambda_l.body))((t: hydra.core.Term) => cons(t))(v_Term_lambda_l.body)))
      case hydra.core.Term.let(v_Term_let_l) => digForLambdas(original)((t: hydra.core.Term) =>
        cons(hydra.core.Term.let(hydra.core.Let(rewriteBindings(v_Term_let_l.bindings), t))))(v_Term_let_l.body)
      case _ => recurse(original)
    term match
      case hydra.core.Term.let(v_Term_let_l) => digForLambdas(term)((t: hydra.core.Term) =>
        hydra.core.Term.let(hydra.core.Let(rewriteBindings(v_Term_let_l.bindings), t)))(v_Term_let_l.body)
      case _ => recurse(term)
  }
  hydra.rewriting.rewriteTerm(rewrite)(term0)
}

def pruneLet(l: hydra.core.Let): hydra.core.Let =
  {
  lazy val bindingMap: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.maps.fromList[hydra.core.Name,
     hydra.core.Term](hydra.lib.lists.map[hydra.core.Binding, Tuple2[hydra.core.Name, hydra.core.Term]]((b: hydra.core.Binding) => Tuple2(b.name,
     (b.term)))(l.bindings))
  lazy val rootName: hydra.core.Name = "[[[root]]]"
  def adj(n: hydra.core.Name): scala.collection.immutable.Set[hydra.core.Name] =
    hydra.lib.sets.intersection[hydra.core.Name](hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.maps.keys[hydra.core.Name,
       hydra.core.Term](bindingMap)))(hydra.variables.freeVariablesInTerm(hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Name](n)(rootName))(l.body)(hydra.lib.maybes.fromJust[hydra.core.Term](hydra.lib.maps.lookup[hydra.core.Name,
       hydra.core.Term](n)(bindingMap)))))
  lazy val reachable: scala.collection.immutable.Set[hydra.core.Name] = hydra.sorting.findReachableNodes(adj)(rootName)
  lazy val prunedBindings: Seq[hydra.core.Binding] = hydra.lib.lists.filter[hydra.core.Binding]((b: hydra.core.Binding) => hydra.lib.sets.member[hydra.core.Name](b.name)(reachable))(l.bindings)
  hydra.core.Let(prunedBindings, (l.body))
}

def replaceTypedefs(types: Map[hydra.core.Name, hydra.core.TypeScheme])(typ0: hydra.core.Type): hydra.core.Type =
  {
  def rewrite(recurse: (hydra.core.Type => hydra.core.Type))(typ: hydra.core.Type): hydra.core.Type =
    typ match
    case hydra.core.Type.annotated(v_Type_annotated_at) => hydra.core.Type.annotated(hydra.core.AnnotatedType(rewrite(recurse)(v_Type_annotated_at.body),
       (v_Type_annotated_at.annotation)))
    case hydra.core.Type.record(v_Type_record__) => typ
    case hydra.core.Type.union(v_Type_union__) => typ
    case hydra.core.Type.variable(v_Type_variable_v) => {
      def forMono(t: hydra.core.Type): hydra.core.Type =
        t match
        case hydra.core.Type.record(v_Type_record__) => typ
        case hydra.core.Type.union(v_Type_union__) => typ
        case hydra.core.Type.wrap(v_Type_wrap__) => typ
        case _ => rewrite(recurse)(t)
      {
        def forTypeScheme(ts: hydra.core.TypeScheme): hydra.core.Type =
          {
          lazy val t: hydra.core.Type = (ts.`type`)
          hydra.lib.logic.ifElse[hydra.core.Type](hydra.lib.lists.`null`[hydra.core.Name](ts.variables))(forMono(t))(typ)
        }
        hydra.lib.maybes.maybe[hydra.core.Type, hydra.core.TypeScheme](typ)((ts: hydra.core.TypeScheme) => forTypeScheme(ts))(hydra.lib.maps.lookup[hydra.core.Name,
           hydra.core.TypeScheme](v_Type_variable_v)(types))
      }
    }
    case hydra.core.Type.wrap(v_Type_wrap__) => typ
    case _ => recurse(typ)
  hydra.rewriting.rewriteType(rewrite)(typ0)
}

def simplifyTerm(term: hydra.core.Term): hydra.core.Term =
  {
  def simplify[T0](recurse: (hydra.core.Term => T0))(term2: hydra.core.Term): T0 =
    {
    def forRhs(rhs: hydra.core.Term)(`var`: hydra.core.Name)(body: hydra.core.Term): hydra.core.Term =
      hydra.strip.deannotateTerm(rhs) match
      case hydra.core.Term.variable(v_Term_variable_v) => hydra.dependencies.simplifyTerm(hydra.variables.substituteVariable(`var`)(v_Term_variable_v)(body))
      case _ => term2
    def forLhs(lhs: hydra.core.Term)(rhs: hydra.core.Term): hydra.core.Term =
      hydra.strip.deannotateTerm(lhs) match
      case hydra.core.Term.lambda(v_Term_lambda_l) => {
        lazy val `var`: hydra.core.Name = (v_Term_lambda_l.parameter)
        {
          lazy val body: hydra.core.Term = (v_Term_lambda_l.body)
          hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.sets.member[hydra.core.Name](`var`)(hydra.variables.freeVariablesInTerm(body)))(forRhs(rhs)(`var`)(body))(hydra.dependencies.simplifyTerm(body))
        }
      }
      case _ => term2
    def forTerm(stripped: hydra.core.Term): hydra.core.Term =
      stripped match
      case hydra.core.Term.application(v_Term_application_app) => {
        lazy val lhs: hydra.core.Term = (v_Term_application_app.function)
        {
          lazy val rhs: hydra.core.Term = (v_Term_application_app.argument)
          forLhs(lhs)(rhs)
        }
      }
      case _ => term2
    lazy val stripped: hydra.core.Term = hydra.strip.deannotateTerm(term2)
    recurse(forTerm(stripped))
  }
  hydra.rewriting.rewriteTerm(simplify)(term)
}

def termDependencyNames(binds: Boolean)(withPrims: Boolean)(withNoms: Boolean)(term0: hydra.core.Term): scala.collection.immutable.Set[hydra.core.Name] =
  {
  def addNames(names: scala.collection.immutable.Set[hydra.core.Name])(term: hydra.core.Term): scala.collection.immutable.Set[hydra.core.Name] =
    {
    def nominal(name: hydra.core.Name): scala.collection.immutable.Set[hydra.core.Name] =
      hydra.lib.logic.ifElse[scala.collection.immutable.Set[hydra.core.Name]](withNoms)(hydra.lib.sets.insert[hydra.core.Name](name)(names))(names)
    def prim(name: hydra.core.Name): scala.collection.immutable.Set[hydra.core.Name] =
      hydra.lib.logic.ifElse[scala.collection.immutable.Set[hydra.core.Name]](withPrims)(hydra.lib.sets.insert[hydra.core.Name](name)(names))(names)
    def `var`(name: hydra.core.Name): scala.collection.immutable.Set[hydra.core.Name] =
      hydra.lib.logic.ifElse[scala.collection.immutable.Set[hydra.core.Name]](binds)(hydra.lib.sets.insert[hydra.core.Name](name)(names))(names)
    term match
      case hydra.core.Term.cases(v_Term_cases_caseStmt) => nominal(v_Term_cases_caseStmt.typeName)
      case hydra.core.Term.project(v_Term_project_proj) => nominal(v_Term_project_proj.typeName)
      case hydra.core.Term.unwrap(v_Term_unwrap_name) => nominal(v_Term_unwrap_name)
      case hydra.core.Term.record(v_Term_record_record) => nominal(v_Term_record_record.typeName)
      case hydra.core.Term.inject(v_Term_inject_injection) => nominal(v_Term_inject_injection.typeName)
      case hydra.core.Term.variable(v_Term_variable_name) => `var`(v_Term_variable_name)
      case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => nominal(v_Term_wrap_wrappedTerm.typeName)
      case _ => names
  }
  hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)(addNames)(hydra.lib.sets.empty[hydra.core.Name])(term0)
}

def toShortNames(original: Seq[hydra.core.Name]): Map[hydra.core.Name, hydra.core.Name] =
  {
  def addName(acc: Map[scala.Predef.String, scala.collection.immutable.Set[hydra.core.Name]])(name: hydra.core.Name): Map[scala.Predef.String,
     scala.collection.immutable.Set[hydra.core.Name]] =
    {
    lazy val local: scala.Predef.String = hydra.names.localNameOf(name)
    lazy val group: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.maybes.fromMaybe[scala.collection.immutable.Set[hydra.core.Name]](hydra.lib.sets.empty[hydra.core.Name])(hydra.lib.maps.lookup[scala.Predef.String,
       scala.collection.immutable.Set[hydra.core.Name]](local)(acc))
    hydra.lib.maps.insert[scala.Predef.String, scala.collection.immutable.Set[hydra.core.Name]](local)(hydra.lib.sets.insert[hydra.core.Name](name)(group))(acc)
  }
  def groupNamesByLocal(names: Seq[hydra.core.Name]): Map[scala.Predef.String, scala.collection.immutable.Set[hydra.core.Name]] =
    hydra.lib.lists.foldl[Map[scala.Predef.String, scala.collection.immutable.Set[hydra.core.Name]], hydra.core.Name](addName)(hydra.lib.maps.empty[scala.Predef.String,
       scala.collection.immutable.Set[hydra.core.Name]])(names)
  lazy val groups: Map[scala.Predef.String, scala.collection.immutable.Set[hydra.core.Name]] = groupNamesByLocal(original)
  def renameGroup[T0](localNames: Tuple2[scala.Predef.String, scala.collection.immutable.Set[T0]]): Seq[Tuple2[T0, hydra.core.Name]] =
    {
    lazy val local: scala.Predef.String = hydra.lib.pairs.first[scala.Predef.String, scala.collection.immutable.Set[T0]](localNames)
    lazy val names: scala.collection.immutable.Set[T0] = hydra.lib.pairs.second[scala.Predef.String, scala.collection.immutable.Set[T0]](localNames)
    def rangeFrom(start: Int): Seq[Int] = hydra.lib.lists.cons[Int](start)(rangeFrom(hydra.lib.math.add(start)(1)))
    def rename[T1](name: T1)(i: Int): Tuple2[T1, hydra.core.Name] =
      Tuple2(name, hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.gt[Int](i)(1))(hydra.lib.strings.cat2(local)(hydra.lib.literals.showInt32(i)))(local))
    hydra.lib.lists.zipWith[T0, Int, Tuple2[T0, hydra.core.Name]](rename)(hydra.lib.sets.toList[T0](names))(rangeFrom(1))
  }
  hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Name](hydra.lib.lists.concat[Tuple2[hydra.core.Name,
     hydra.core.Name]](hydra.lib.lists.map[Tuple2[scala.Predef.String, scala.collection.immutable.Set[hydra.core.Name]],
     Seq[Tuple2[hydra.core.Name, hydra.core.Name]]](renameGroup)(hydra.lib.maps.toList[scala.Predef.String,
     scala.collection.immutable.Set[hydra.core.Name]](groups))))
}

def topologicalSortBindingMap(bindingMap: Map[hydra.core.Name, hydra.core.Term]): Seq[Seq[Tuple2[hydra.core.Name, hydra.core.Term]]] =
  {
  lazy val bindings: Seq[Tuple2[hydra.core.Name, hydra.core.Term]] = hydra.lib.maps.toList[hydra.core.Name, hydra.core.Term](bindingMap)
  lazy val keys: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.lists.map[Tuple2[hydra.core.Name,
     hydra.core.Term], hydra.core.Name](hydra.lib.pairs.first[hydra.core.Name, hydra.core.Term])(bindings))
  def hasTypeAnnotation(term: hydra.core.Term): Boolean =
    term match
    case hydra.core.Term.annotated(v_Term_annotated_at) => hasTypeAnnotation(v_Term_annotated_at.body)
    case _ => false
  def depsOf[T0](nameAndTerm: Tuple2[T0, hydra.core.Term]): Tuple2[T0, Seq[hydra.core.Name]] =
    {
    lazy val name: T0 = hydra.lib.pairs.first[T0, hydra.core.Term](nameAndTerm)
    lazy val term: hydra.core.Term = hydra.lib.pairs.second[T0, hydra.core.Term](nameAndTerm)
    Tuple2(name, hydra.lib.logic.ifElse[Seq[hydra.core.Name]](hasTypeAnnotation(term))(Seq())(hydra.lib.sets.toList[hydra.core.Name](hydra.lib.sets.intersection[hydra.core.Name](keys)(hydra.variables.freeVariablesInTerm(term)))))
  }
  def toPair(name: hydra.core.Name): Tuple2[hydra.core.Name, hydra.core.Term] =
    Tuple2(name, hydra.lib.maybes.fromMaybe[hydra.core.Term](hydra.core.Term.literal(hydra.core.Literal.string("Impossible!")))(hydra.lib.maps.lookup[hydra.core.Name,
       hydra.core.Term](name)(bindingMap)))
  hydra.lib.lists.map[Seq[hydra.core.Name], Seq[Tuple2[hydra.core.Name, hydra.core.Term]]]((v1) =>
    hydra.lib.lists.map[hydra.core.Name, Tuple2[hydra.core.Name, hydra.core.Term]](toPair)(v1))(hydra.sorting.topologicalSortComponents(hydra.lib.lists.map[Tuple2[hydra.core.Name,
       hydra.core.Term], Tuple2[hydra.core.Name, Seq[hydra.core.Name]]](depsOf)(bindings)))
}

def topologicalSortBindings(els: Seq[hydra.core.Binding]): Either[Seq[Seq[hydra.core.Name]], Seq[hydra.core.Name]] =
  {
  def adjlist(e: hydra.core.Binding): Tuple2[hydra.core.Name, Seq[hydra.core.Name]] =
    Tuple2(e.name, hydra.lib.sets.toList[hydra.core.Name](hydra.dependencies.termDependencyNames(false)(true)(true)(e.term)))
  hydra.sorting.topologicalSort(hydra.lib.lists.map[hydra.core.Binding, Tuple2[hydra.core.Name, Seq[hydra.core.Name]]](adjlist)(els))
}

def topologicalSortTypeDefinitions(defs: Seq[hydra.packaging.TypeDefinition]): Seq[Seq[hydra.packaging.TypeDefinition]] =
  {
  def toPair(`def`: hydra.packaging.TypeDefinition): Tuple2[hydra.core.Name, Seq[hydra.core.Name]] =
    Tuple2(`def`.name, hydra.lib.sets.toList[hydra.core.Name](hydra.dependencies.typeDependencyNames(false)(`def`.`type`.`type`)))
  lazy val nameToDef: Map[hydra.core.Name, hydra.packaging.TypeDefinition] = hydra.lib.maps.fromList[hydra.core.Name,
     hydra.packaging.TypeDefinition](hydra.lib.lists.map[hydra.packaging.TypeDefinition, Tuple2[hydra.core.Name,
     hydra.packaging.TypeDefinition]]((d: hydra.packaging.TypeDefinition) => Tuple2(d.name, d))(defs))
  lazy val sorted: Seq[Seq[hydra.core.Name]] = hydra.sorting.topologicalSortComponents(hydra.lib.lists.map[hydra.packaging.TypeDefinition,
     Tuple2[hydra.core.Name, Seq[hydra.core.Name]]](toPair)(defs))
  hydra.lib.lists.map[Seq[hydra.core.Name], Seq[hydra.packaging.TypeDefinition]]((names: Seq[hydra.core.Name]) =>
    hydra.lib.maybes.cat[hydra.packaging.TypeDefinition](hydra.lib.lists.map[hydra.core.Name, Option[hydra.packaging.TypeDefinition]]((n: hydra.core.Name) =>
    hydra.lib.maps.lookup[hydra.core.Name, hydra.packaging.TypeDefinition](n)(nameToDef))(names)))(sorted)
}

def typeDependencyNames(withSchema: Boolean)(typ: hydra.core.Type): scala.collection.immutable.Set[hydra.core.Name] =
  hydra.lib.logic.ifElse[scala.collection.immutable.Set[hydra.core.Name]](withSchema)(hydra.lib.sets.union[hydra.core.Name](hydra.variables.freeVariablesInType(typ))(hydra.dependencies.typeNamesInType(typ)))(hydra.variables.freeVariablesInType(typ))

def typeNamesInType[T0](typ0: hydra.core.Type): scala.collection.immutable.Set[T0] =
  {
  def addNames[T1, T2](names: T1)(typ: T2): T1 = names
  hydra.rewriting.foldOverType(hydra.coders.TraversalOrder.pre)(addNames)(hydra.lib.sets.empty[T0])(typ0)
}
