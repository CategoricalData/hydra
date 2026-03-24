package hydra.rewriting

import hydra.accessors.*

import hydra.coders.*

import hydra.core.*

import hydra.graph.*

import hydra.lib.eithers

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.literals

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.math

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.sets

import hydra.lib.strings

def applyInsideTypeLambdasAndAnnotations(f: (hydra.core.Term => hydra.core.Term))(term0: hydra.core.Term): hydra.core.Term =
  term0 match
  case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.rewriting.applyInsideTypeLambdasAndAnnotations(f)(v_Term_annotated_at.body), (v_Term_annotated_at.annotation)))
  case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.core.Term.typeLambda(hydra.core.TypeLambda(v_Term_typeLambda_tl.parameter, hydra.rewriting.applyInsideTypeLambdasAndAnnotations(f)(v_Term_typeLambda_tl.body)))
  case _ => f(term0)

def extendGraphForLambda(g: hydra.graph.Graph)(lam: hydra.core.Lambda): hydra.graph.Graph =
  {
  lazy val `var`: hydra.core.Name = (lam.parameter)
  hydra.graph.Graph(g.boundTerms, hydra.lib.maybes.maybe[Map[hydra.core.Name, hydra.core.TypeScheme], hydra.core.Type](g.boundTypes)((dom: hydra.core.Type) =>
    hydra.lib.maps.insert[hydra.core.Name, hydra.core.TypeScheme](`var`)(hydra.rewriting.fTypeToTypeScheme(dom))(g.boundTypes))(lam.domain), (g.classConstraints), hydra.lib.sets.insert[hydra.core.Name](`var`)(g.lambdaVariables), hydra.lib.maps.delete[hydra.core.Name, hydra.core.Term](`var`)(g.metadata), (g.primitives), (g.schemaTypes), (g.typeVariables))
}

def extendGraphForLet(forBinding: (hydra.graph.Graph => hydra.core.Binding => Option[hydra.core.Term]))(g: hydra.graph.Graph)(letrec: hydra.core.Let): hydra.graph.Graph =
  {
  lazy val bindings: Seq[hydra.core.Binding] = (letrec.bindings)
  lazy val g2: hydra.graph.Graph = hydra.rewriting.extendGraphWithBindings(bindings)(g)
  hydra.graph.Graph(hydra.lib.maps.union[hydra.core.Name, hydra.core.Term](hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Term](hydra.lib.lists.map[hydra.core.Binding, Tuple2[hydra.core.Name, hydra.core.Term]]((b: hydra.core.Binding) => Tuple2(b.name, (b.term)))(bindings)))(g.boundTerms), hydra.lib.maps.union[hydra.core.Name, hydra.core.TypeScheme](hydra.lib.maps.fromList[hydra.core.Name, hydra.core.TypeScheme](hydra.lib.maybes.cat[Tuple2[hydra.core.Name, hydra.core.TypeScheme]](hydra.lib.lists.map[hydra.core.Binding, Option[Tuple2[hydra.core.Name, hydra.core.TypeScheme]]]((b: hydra.core.Binding) =>
    hydra.lib.maybes.map[hydra.core.TypeScheme, Tuple2[hydra.core.Name, hydra.core.TypeScheme]]((ts: hydra.core.TypeScheme) => Tuple2(b.name, ts))(b.`type`))(bindings))))(g.boundTypes), (g.classConstraints), hydra.lib.lists.foldl[scala.collection.immutable.Set[hydra.core.Name], hydra.core.Binding]((s: scala.collection.immutable.Set[hydra.core.Name]) =>
    (b: hydra.core.Binding) => hydra.lib.sets.delete[hydra.core.Name](b.name)(s))(g.lambdaVariables)(bindings), (hydra.lib.lists.foldl[hydra.graph.Graph, hydra.core.Binding]((gAcc: hydra.graph.Graph) =>
    (b: hydra.core.Binding) =>
    {
    lazy val m: Map[hydra.core.Name, hydra.core.Term] = (gAcc.metadata)
    {
      lazy val newMeta: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.maybes.maybe[Map[hydra.core.Name, hydra.core.Term], hydra.core.Term](hydra.lib.maps.delete[hydra.core.Name, hydra.core.Term](b.name)(m))((t: hydra.core.Term) =>
        hydra.lib.maps.insert[hydra.core.Name, hydra.core.Term](b.name)(t)(m))(forBinding(gAcc)(b))
      hydra.graph.Graph(gAcc.boundTerms, (gAcc.boundTypes), (gAcc.classConstraints), (gAcc.lambdaVariables), newMeta, (gAcc.primitives), (gAcc.schemaTypes), (gAcc.typeVariables))
    }
  })(g2)(bindings).metadata), (g.primitives), (g.schemaTypes), (g.typeVariables))
}

def extendGraphForTypeLambda(g: hydra.graph.Graph)(tlam: hydra.core.TypeLambda): hydra.graph.Graph =
  {
  lazy val name: hydra.core.Name = (tlam.parameter)
  hydra.graph.Graph(g.boundTerms, (g.boundTypes), (g.classConstraints), (g.lambdaVariables), (g.metadata), (g.primitives), (g.schemaTypes), hydra.lib.sets.insert[hydra.core.Name](name)(g.typeVariables))
}

def extendGraphWithBindings(bindings: Seq[hydra.core.Binding])(g: hydra.graph.Graph): hydra.graph.Graph =
  {
  lazy val newTerms: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Term](hydra.lib.lists.map[hydra.core.Binding, Tuple2[hydra.core.Name, hydra.core.Term]]((b: hydra.core.Binding) => Tuple2(b.name, (b.term)))(bindings))
  lazy val newTypes: Map[hydra.core.Name, hydra.core.TypeScheme] = hydra.lib.maps.fromList[hydra.core.Name, hydra.core.TypeScheme](hydra.lib.maybes.cat[Tuple2[hydra.core.Name, hydra.core.TypeScheme]](hydra.lib.lists.map[hydra.core.Binding, Option[Tuple2[hydra.core.Name, hydra.core.TypeScheme]]]((b: hydra.core.Binding) =>
    hydra.lib.maybes.map[hydra.core.TypeScheme, Tuple2[hydra.core.Name, hydra.core.TypeScheme]]((ts: hydra.core.TypeScheme) => Tuple2(b.name, ts))(b.`type`))(bindings)))
  hydra.graph.Graph(hydra.lib.maps.union[hydra.core.Name, hydra.core.Term](newTerms)(g.boundTerms), hydra.lib.maps.union[hydra.core.Name, hydra.core.TypeScheme](newTypes)(g.boundTypes), (g.classConstraints), (g.lambdaVariables), (g.metadata), (g.primitives), (g.schemaTypes), (g.typeVariables))
}

def deannotateAndDetypeTerm(t: hydra.core.Term): hydra.core.Term =
  t match
  case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.rewriting.deannotateAndDetypeTerm(v_Term_annotated_at.body)
  case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => hydra.rewriting.deannotateAndDetypeTerm(v_Term_typeApplication_tt.body)
  case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => hydra.rewriting.deannotateAndDetypeTerm(v_Term_typeLambda_ta.body)
  case _ => t

def deannotateTerm(t: hydra.core.Term): hydra.core.Term =
  t match
  case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.rewriting.deannotateTerm(v_Term_annotated_at.body)
  case _ => t

def deannotateType(t: hydra.core.Type): hydra.core.Type =
  t match
  case hydra.core.Type.annotated(v_Type_annotated_arg_) => hydra.rewriting.deannotateType(`v_Type_annotated_arg_`.body)
  case _ => t

def deannotateTypeParameters(t: hydra.core.Type): hydra.core.Type =
  hydra.rewriting.deannotateType(t) match
  case hydra.core.Type.forall(v_Type_forall_lt) => hydra.rewriting.deannotateTypeParameters(v_Type_forall_lt.body)
  case _ => t

def deannotateTypeRecursive(typ: hydra.core.Type): hydra.core.Type =
  {
  def strip[T0](recurse: (T0 => hydra.core.Type))(typ2: T0): hydra.core.Type =
    {
    lazy val rewritten: hydra.core.Type = recurse(typ2)
    rewritten match
      case hydra.core.Type.annotated(v_Type_annotated_at) => (v_Type_annotated_at.body)
      case _ => rewritten
  }
  hydra.rewriting.rewriteType(strip)(typ)
}

def deannotateTypeSchemeRecursive(ts: hydra.core.TypeScheme): hydra.core.TypeScheme =
  {
  lazy val vars: Seq[hydra.core.Name] = (ts.variables)
  lazy val typ: hydra.core.Type = (ts.`type`)
  lazy val constraints: Option[Map[hydra.core.Name, hydra.core.TypeVariableMetadata]] = (ts.constraints)
  hydra.core.TypeScheme(vars, hydra.rewriting.deannotateTypeRecursive(typ), constraints)
}

def detypeTerm(t: hydra.core.Term): hydra.core.Term =
  t match
  case hydra.core.Term.annotated(v_Term_annotated_at) => {
    lazy val subj: hydra.core.Term = (v_Term_annotated_at.body)
    {
      lazy val ann: Map[hydra.core.Name, hydra.core.Term] = (v_Term_annotated_at.annotation)
      hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.rewriting.detypeTerm(subj), ann))
    }
  }
  case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => hydra.rewriting.deannotateAndDetypeTerm(v_Term_typeApplication_tt.body)
  case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => hydra.rewriting.deannotateAndDetypeTerm(v_Term_typeLambda_ta.body)
  case _ => t

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
                  lazy val subst: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Name](hydra.lib.lists.map[hydra.core.Binding, Tuple2[hydra.core.Name, hydra.core.Name]](toSubstPair)(bindings1))
                  {
                    def replaceVars(v1: hydra.core.Term): hydra.core.Term = hydra.rewriting.substituteVariables(subst)(v1)
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
              lazy val flattenedBindings: Seq[hydra.core.Binding] = hydra.lib.lists.concat[hydra.core.Binding](hydra.lib.lists.map[hydra.core.Binding, Seq[hydra.core.Binding]]((`arg_`: hydra.core.Binding) => forResult(rewriteBinding(`arg_`)))(bindings))
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

def foldOverTerm[T0](order: hydra.coders.TraversalOrder)(fld: (T0 => hydra.core.Term => T0))(b0: T0)(term: hydra.core.Term): T0 =
  order match
  case hydra.coders.TraversalOrder.pre => hydra.lib.lists.foldl[T0, hydra.core.Term]((v1: T0) =>
    (v2: hydra.core.Term) => hydra.rewriting.foldOverTerm(order)(fld)(v1)(v2))(fld(b0)(term))(hydra.rewriting.subterms(term))
  case hydra.coders.TraversalOrder.post => fld(hydra.lib.lists.foldl[T0, hydra.core.Term]((v1: T0) =>
    (v2: hydra.core.Term) => hydra.rewriting.foldOverTerm(order)(fld)(v1)(v2))(b0)(hydra.rewriting.subterms(term)))(term)

def foldOverType[T0](order: hydra.coders.TraversalOrder)(fld: (T0 => hydra.core.Type => T0))(b0: T0)(typ: hydra.core.Type): T0 =
  order match
  case hydra.coders.TraversalOrder.pre => hydra.lib.lists.foldl[T0, hydra.core.Type]((v1: T0) =>
    (v2: hydra.core.Type) => hydra.rewriting.foldOverType(order)(fld)(v1)(v2))(fld(b0)(typ))(hydra.rewriting.subtypes(typ))
  case hydra.coders.TraversalOrder.post => fld(hydra.lib.lists.foldl[T0, hydra.core.Type]((v1: T0) =>
    (v2: hydra.core.Type) => hydra.rewriting.foldOverType(order)(fld)(v1)(v2))(b0)(hydra.rewriting.subtypes(typ)))(typ)

def foldTermWithGraphAndPath[T0](f: ((T0 => hydra.core.Term => T0) => Seq[hydra.accessors.TermAccessor] => hydra.graph.Graph => T0 => hydra.core.Term => T0))(cx0: hydra.graph.Graph)(val0: T0)(term0: hydra.core.Term): T0 =
  {
  def wrapper[T1](recurse: (T0 => hydra.core.Term => Tuple2[T0, T1]))(path: Seq[hydra.accessors.TermAccessor])(cx: hydra.graph.Graph)(`val`: T0)(term: hydra.core.Term): Tuple2[T0, hydra.core.Term] =
    {
    def recurseForUser(valIn: T0)(subterm: hydra.core.Term): T0 =
      {
      lazy val r: Tuple2[T0, T1] = recurse(valIn)(subterm)
      hydra.lib.pairs.first[T0, T1](r)
    }
    Tuple2(f(recurseForUser)(path)(cx)(`val`)(term), term)
  }
  lazy val result: Tuple2[T0, hydra.core.Term] = hydra.rewriting.rewriteAndFoldTermWithGraphAndPath(wrapper)(cx0)(val0)(term0)
  hydra.lib.pairs.first[T0, hydra.core.Term](result)
}

def fTypeToTypeScheme(typ: hydra.core.Type): hydra.core.TypeScheme =
  {
  def gatherForall(vars: Seq[hydra.core.Name])(typ2: hydra.core.Type): hydra.core.TypeScheme =
    hydra.rewriting.deannotateType(typ2) match
    case hydra.core.Type.forall(v_Type_forall_ft) => gatherForall(hydra.lib.lists.cons[hydra.core.Name](v_Type_forall_ft.parameter)(vars))(v_Type_forall_ft.body)
    case _ => hydra.core.TypeScheme(hydra.lib.lists.reverse[hydra.core.Name](vars), typ2, None)
  gatherForall(Seq())(typ)
}

def freeTypeVariablesInTerm(term0: hydra.core.Term): scala.collection.immutable.Set[hydra.core.Name] =
  {
  def allOf[T0](sets: Seq[scala.collection.immutable.Set[T0]]): scala.collection.immutable.Set[T0] =
    hydra.lib.lists.foldl[scala.collection.immutable.Set[T0], scala.collection.immutable.Set[T0]](hydra.lib.sets.union[T0])(hydra.lib.sets.empty[T0])(sets)
  def tryType(tvars: scala.collection.immutable.Set[hydra.core.Name])(typ: hydra.core.Type): scala.collection.immutable.Set[hydra.core.Name] =
    hydra.lib.sets.difference[hydra.core.Name](hydra.rewriting.freeVariablesInType(typ))(tvars)
  def getAll(vars: scala.collection.immutable.Set[hydra.core.Name])(term: hydra.core.Term): scala.collection.immutable.Set[hydra.core.Name] =
    {
    def recurse(v1: hydra.core.Term): scala.collection.immutable.Set[hydra.core.Name] = getAll(vars)(v1)
    lazy val dflt: scala.collection.immutable.Set[hydra.core.Name] = allOf(hydra.lib.lists.map[hydra.core.Term, scala.collection.immutable.Set[hydra.core.Name]](recurse)(hydra.rewriting.subterms(term)))
    term match
      case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
        case hydra.core.Function.elimination => dflt
        case hydra.core.Function.lambda(v_Function_lambda_l) => {
          lazy val domt: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.maybes.maybe[scala.collection.immutable.Set[hydra.core.Name], hydra.core.Type](hydra.lib.sets.empty[hydra.core.Name])((v1: hydra.core.Type) => tryType(vars)(v1))(v_Function_lambda_l.domain)
          hydra.lib.sets.union[hydra.core.Name](domt)(recurse(v_Function_lambda_l.body))
        }
        case _ => dflt
      case hydra.core.Term.let(v_Term_let_l) => {
        def forBinding(b: hydra.core.Binding): scala.collection.immutable.Set[hydra.core.Name] =
          {
          lazy val newVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.maybes.maybe[scala.collection.immutable.Set[hydra.core.Name], hydra.core.TypeScheme](vars)((ts: hydra.core.TypeScheme) =>
            hydra.lib.sets.union[hydra.core.Name](vars)(hydra.lib.sets.fromList[hydra.core.Name](ts.variables)))(b.`type`)
          hydra.lib.sets.union[hydra.core.Name](getAll(newVars)(b.term))(hydra.lib.maybes.maybe[scala.collection.immutable.Set[hydra.core.Name], hydra.core.TypeScheme](hydra.lib.sets.empty[hydra.core.Name])((ts: hydra.core.TypeScheme) => tryType(newVars)(ts.`type`))(b.`type`))
        }
        hydra.lib.sets.union[hydra.core.Name](allOf(hydra.lib.lists.map[hydra.core.Binding, scala.collection.immutable.Set[hydra.core.Name]](forBinding)(v_Term_let_l.bindings)))(recurse(v_Term_let_l.body))
      }
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => hydra.lib.sets.union[hydra.core.Name](tryType(vars)(v_Term_typeApplication_tt.`type`))(recurse(v_Term_typeApplication_tt.body))
      case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.lib.sets.union[hydra.core.Name](tryType(vars)(hydra.core.Type.variable(v_Term_typeLambda_tl.parameter)))(recurse(v_Term_typeLambda_tl.body))
      case _ => dflt
  }
  getAll(hydra.lib.sets.empty[hydra.core.Name])(term0)
}

def freeVariablesInTerm(term: hydra.core.Term): scala.collection.immutable.Set[hydra.core.Name] =
  {
  def dfltVars[T0](_x: T0): scala.collection.immutable.Set[hydra.core.Name] =
    hydra.lib.lists.foldl[scala.collection.immutable.Set[hydra.core.Name], hydra.core.Term]((s: scala.collection.immutable.Set[hydra.core.Name]) =>
    (t: hydra.core.Term) =>
    hydra.lib.sets.union[hydra.core.Name](s)(hydra.rewriting.freeVariablesInTerm(t)))(hydra.lib.sets.empty[hydra.core.Name])(hydra.rewriting.subterms(term))
  term match
    case hydra.core.Term.function(v_Term_function_v1) => v_Term_function_v1 match
      case hydra.core.Function.lambda(v_Function_lambda_l) => hydra.lib.sets.delete[hydra.core.Name](v_Function_lambda_l.parameter)(hydra.rewriting.freeVariablesInTerm(v_Function_lambda_l.body))
      case _ => dfltVars(())
    case hydra.core.Term.let(v_Term_let_l) => hydra.lib.sets.difference[hydra.core.Name](dfltVars(()))(hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.lists.map[hydra.core.Binding, hydra.core.Name]((x: hydra.core.Binding) => (x.name))(v_Term_let_l.bindings)))
    case hydra.core.Term.variable(v_Term_variable_v) => hydra.lib.sets.singleton[hydra.core.Name](v_Term_variable_v)
    case _ => dfltVars(())
}

def freeVariablesInType(typ: hydra.core.Type): scala.collection.immutable.Set[hydra.core.Name] =
  {
  lazy val dfltVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.lists.foldl[scala.collection.immutable.Set[hydra.core.Name], hydra.core.Type]((s: scala.collection.immutable.Set[hydra.core.Name]) =>
    (t: hydra.core.Type) =>
    hydra.lib.sets.union[hydra.core.Name](s)(hydra.rewriting.freeVariablesInType(t)))(hydra.lib.sets.empty[hydra.core.Name])(hydra.rewriting.subtypes(typ))
  typ match
    case hydra.core.Type.forall(v_Type_forall_lt) => hydra.lib.sets.delete[hydra.core.Name](v_Type_forall_lt.parameter)(hydra.rewriting.freeVariablesInType(v_Type_forall_lt.body))
    case hydra.core.Type.variable(v_Type_variable_v) => hydra.lib.sets.singleton[hydra.core.Name](v_Type_variable_v)
    case _ => dfltVars
}

def freeVariablesInTypeOrdered(typ: hydra.core.Type): Seq[hydra.core.Name] =
  {
  def collectVars(boundVars: scala.collection.immutable.Set[hydra.core.Name])(t: hydra.core.Type): Seq[hydra.core.Name] =
    t match
    case hydra.core.Type.variable(v_Type_variable_v) => hydra.lib.logic.ifElse[Seq[hydra.core.Name]](hydra.lib.sets.member[hydra.core.Name](v_Type_variable_v)(boundVars))(Seq())(Seq(v_Type_variable_v))
    case hydra.core.Type.forall(v_Type_forall_ft) => collectVars(hydra.lib.sets.insert[hydra.core.Name](v_Type_forall_ft.parameter)(boundVars))(v_Type_forall_ft.body)
    case _ => hydra.lib.lists.concat[hydra.core.Name](hydra.lib.lists.map[hydra.core.Type, Seq[hydra.core.Name]]((v1: hydra.core.Type) => collectVars(boundVars)(v1))(hydra.rewriting.subtypes(t)))
  hydra.lib.lists.nub[hydra.core.Name](collectVars(hydra.lib.sets.empty[hydra.core.Name])(typ))
}

def freeVariablesInTypeSchemeSimple(ts: hydra.core.TypeScheme): scala.collection.immutable.Set[hydra.core.Name] =
  {
  lazy val vars: Seq[hydra.core.Name] = (ts.variables)
  lazy val t: hydra.core.Type = (ts.`type`)
  hydra.lib.sets.difference[hydra.core.Name](hydra.rewriting.freeVariablesInTypeSimple(t))(hydra.lib.sets.fromList[hydra.core.Name](vars))
}

def freeVariablesInTypeScheme(ts: hydra.core.TypeScheme): scala.collection.immutable.Set[hydra.core.Name] =
  {
  lazy val vars: Seq[hydra.core.Name] = (ts.variables)
  lazy val t: hydra.core.Type = (ts.`type`)
  hydra.lib.sets.difference[hydra.core.Name](hydra.rewriting.freeVariablesInType(t))(hydra.lib.sets.fromList[hydra.core.Name](vars))
}

def freeVariablesInTypeSimple(typ: hydra.core.Type): scala.collection.immutable.Set[hydra.core.Name] =
  {
  def helper(types: scala.collection.immutable.Set[hydra.core.Name])(typ2: hydra.core.Type): scala.collection.immutable.Set[hydra.core.Name] =
    typ2 match
    case hydra.core.Type.variable(v_Type_variable_v) => hydra.lib.sets.insert[hydra.core.Name](v_Type_variable_v)(types)
    case _ => types
  hydra.rewriting.foldOverType(hydra.coders.TraversalOrder.pre)(helper)(hydra.lib.sets.empty[hydra.core.Name])(typ)
}

def inlineType(schema: Map[hydra.core.Name, hydra.core.Type])(typ: hydra.core.Type): Either[scala.Predef.String, hydra.core.Type] =
  {
  def f[T0](recurse: (T0 => Either[scala.Predef.String, hydra.core.Type]))(typ2: T0): Either[scala.Predef.String, hydra.core.Type] =
    {
    def afterRecurse(tr: hydra.core.Type): Either[scala.Predef.String, hydra.core.Type] =
      tr match
      case hydra.core.Type.variable(v_Type_variable_v) => hydra.lib.maybes.maybe[Either[scala.Predef.String, hydra.core.Type], hydra.core.Type](Left(hydra.lib.strings.cat2("No such type in schema: ")(v_Type_variable_v)))((v1: hydra.core.Type) => hydra.rewriting.inlineType(schema)(v1))(hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Type](v_Type_variable_v)(schema))
      case _ => Right(tr)
    hydra.lib.eithers.bind[scala.Predef.String, hydra.core.Type, hydra.core.Type](recurse(typ2))((tr: hydra.core.Type) => afterRecurse(tr))
  }
  hydra.rewriting.rewriteTypeM(f)(typ)
}

def isFreeVariableInTerm(v: hydra.core.Name)(term: hydra.core.Term): Boolean =
  hydra.lib.logic.not(hydra.lib.sets.member[hydra.core.Name](v)(hydra.rewriting.freeVariablesInTerm(term)))

def isLambda(term: hydra.core.Term): Boolean =
  hydra.rewriting.deannotateTerm(term) match
  case hydra.core.Term.function(v_Term_function_v1) => v_Term_function_v1 match
    case hydra.core.Function.lambda => true
    case _ => false
  case hydra.core.Term.let(v_Term_let_lt) => hydra.rewriting.isLambda(v_Term_let_lt.body)
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
      case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
        case hydra.core.Function.lambda(v_Function_lambda_l) => hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda(v_Function_lambda_l.parameter, (v_Function_lambda_l.domain), digForLambdas(cons(v_Function_lambda_l.body))((t: hydra.core.Term) => cons(t))(v_Function_lambda_l.body))))
        case _ => recurse(original)
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

def mapBeneathTypeAnnotations(f: (hydra.core.Type => hydra.core.Type))(t: hydra.core.Type): hydra.core.Type =
  t match
  case hydra.core.Type.annotated(v_Type_annotated_at) => hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.rewriting.mapBeneathTypeAnnotations(f)(v_Type_annotated_at.body), (v_Type_annotated_at.annotation)))
  case _ => f(t)

def normalizeTypeVariablesInTerm(term: hydra.core.Term): hydra.core.Term =
  {
  def replaceName[T0](subst: Map[T0, T0])(v: T0): T0 = hydra.lib.maybes.fromMaybe[T0](v)(hydra.lib.maps.lookup[T0, T0](v)(subst))
  def substType(subst: Map[hydra.core.Name, hydra.core.Name])(typ: hydra.core.Type): hydra.core.Type =
    {
    def rewrite(recurse: (hydra.core.Type => hydra.core.Type))(typ2: hydra.core.Type): hydra.core.Type =
      typ2 match
      case hydra.core.Type.variable(v_Type_variable_v) => hydra.core.Type.variable(replaceName(subst)(v_Type_variable_v))
      case _ => recurse(typ2)
    hydra.rewriting.rewriteType(rewrite)(typ)
  }
  def rewriteWithSubst(state: Tuple2[Tuple2[Map[hydra.core.Name, hydra.core.Name], scala.collection.immutable.Set[hydra.core.Name]], Int])(term0: hydra.core.Term): hydra.core.Term =
    {
    lazy val sb: Tuple2[Map[hydra.core.Name, hydra.core.Name], scala.collection.immutable.Set[hydra.core.Name]] = hydra.lib.pairs.first[Tuple2[Map[hydra.core.Name, hydra.core.Name], scala.collection.immutable.Set[hydra.core.Name]], Int](state)
    lazy val next: Int = hydra.lib.pairs.second[Tuple2[Map[hydra.core.Name, hydra.core.Name], scala.collection.immutable.Set[hydra.core.Name]], Int](state)
    lazy val subst: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.pairs.first[Map[hydra.core.Name, hydra.core.Name], scala.collection.immutable.Set[hydra.core.Name]](sb)
    lazy val boundVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.pairs.second[Map[hydra.core.Name, hydra.core.Name], scala.collection.immutable.Set[hydra.core.Name]](sb)
    def rewrite(recurse: (hydra.core.Term => hydra.core.Term))(term2: hydra.core.Term): hydra.core.Term =
      term2 match
      case hydra.core.Term.function(v_Term_function_v1) => v_Term_function_v1 match
        case hydra.core.Function.elimination => recurse(term2)
        case hydra.core.Function.lambda(v_Function_lambda_l) => {
          lazy val domain: Option[hydra.core.Type] = (v_Function_lambda_l.domain)
          hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda(v_Function_lambda_l.parameter, hydra.lib.maybes.map[hydra.core.Type, hydra.core.Type]((v12: hydra.core.Type) => substType(subst)(v12))(domain), rewriteWithSubst(Tuple2(Tuple2(subst, boundVars), next))(v_Function_lambda_l.body))))
        }
        case _ => recurse(term2)
      case hydra.core.Term.let(v_Term_let_lt) => {
        lazy val bindings0: Seq[hydra.core.Binding] = (v_Term_let_lt.bindings)
        {
          lazy val body0: hydra.core.Term = (v_Term_let_lt.body)
          {
            def step(acc: Seq[hydra.core.Binding])(bs: Seq[hydra.core.Binding]): Seq[hydra.core.Binding] =
              hydra.lib.logic.ifElse[Seq[hydra.core.Binding]](hydra.lib.lists.`null`[hydra.core.Binding](bs))(hydra.lib.lists.reverse[hydra.core.Binding](acc))({
              lazy val b: hydra.core.Binding = hydra.lib.lists.head[hydra.core.Binding](bs)
              {
                lazy val tl: Seq[hydra.core.Binding] = hydra.lib.lists.tail[hydra.core.Binding](bs)
                {
                  lazy val noType: Seq[hydra.core.Binding] = {
                    lazy val newVal: hydra.core.Term = rewriteWithSubst(Tuple2(Tuple2(subst, boundVars), next))(b.term)
                    {
                      lazy val b1: hydra.core.Binding = hydra.core.Binding(b.name, newVal, None)
                      step(hydra.lib.lists.cons[hydra.core.Binding](b1)(acc))(tl)
                    }
                  }
                  {
                    def withType(ts: hydra.core.TypeScheme): Seq[hydra.core.Binding] =
                      {
                      lazy val vars: Seq[hydra.core.Name] = (ts.variables)
                      lazy val typ: hydra.core.Type = (ts.`type`)
                      lazy val k: Int = hydra.lib.lists.length[hydra.core.Name](vars)
                      def gen(i: Int)(rem: Int)(acc2: Seq[hydra.core.Name]): Seq[hydra.core.Name] =
                        {
                        lazy val ti: hydra.core.Name = hydra.lib.strings.cat2("t")(hydra.lib.literals.showInt32(hydra.lib.math.add(next)(i)))
                        hydra.lib.logic.ifElse[Seq[hydra.core.Name]](hydra.lib.equality.equal[Int](rem)(0))(hydra.lib.lists.reverse[hydra.core.Name](acc2))(gen(hydra.lib.math.add(i)(1))(hydra.lib.math.sub(rem)(1))(hydra.lib.lists.cons[hydra.core.Name](ti)(acc2)))
                      }
                      lazy val newVars: Seq[hydra.core.Name] = gen(0)(k)(Seq())
                      lazy val newSubst: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Name](hydra.lib.lists.zip[hydra.core.Name, hydra.core.Name](vars)(newVars)))(subst)
                      lazy val newBound: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.union[hydra.core.Name](boundVars)(hydra.lib.sets.fromList[hydra.core.Name](newVars))
                      lazy val newVal: hydra.core.Term = rewriteWithSubst(Tuple2(Tuple2(newSubst, newBound), hydra.lib.math.add(next)(k)))(b.term)
                      def renameConstraintKeys[T0](constraintMap: Map[hydra.core.Name, T0]): Map[hydra.core.Name, T0] =
                        hydra.lib.maps.fromList[hydra.core.Name, T0](hydra.lib.lists.map[Tuple2[hydra.core.Name, T0], Tuple2[hydra.core.Name, T0]]((p: Tuple2[hydra.core.Name, T0]) =>
                        {
                        lazy val oldName: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, T0](p)
                        {
                          lazy val meta: T0 = hydra.lib.pairs.second[hydra.core.Name, T0](p)
                          {
                            lazy val newName: hydra.core.Name = hydra.lib.maybes.fromMaybe[hydra.core.Name](oldName)(hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Name](oldName)(newSubst))
                            Tuple2(newName, meta)
                          }
                        }
                      })(hydra.lib.maps.toList[hydra.core.Name, T0](constraintMap)))
                      lazy val oldConstraints: Option[Map[hydra.core.Name, hydra.core.TypeVariableMetadata]] = (ts.constraints)
                      lazy val newConstraints: Option[Map[hydra.core.Name, hydra.core.TypeVariableMetadata]] = hydra.lib.maybes.map[Map[hydra.core.Name, hydra.core.TypeVariableMetadata], Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](renameConstraintKeys)(oldConstraints)
                      lazy val b1: hydra.core.Binding = hydra.core.Binding(b.name, newVal, Some(hydra.core.TypeScheme(newVars, substType(newSubst)(typ), newConstraints)))
                      step(hydra.lib.lists.cons[hydra.core.Binding](b1)(acc))(tl)
                    }
                    hydra.lib.maybes.maybe[Seq[hydra.core.Binding], hydra.core.TypeScheme](noType)((ts: hydra.core.TypeScheme) => withType(ts))(b.`type`)
                  }
                }
              }
            })
            {
              lazy val bindings1: Seq[hydra.core.Binding] = step(Seq())(bindings0)
              hydra.core.Term.let(hydra.core.Let(bindings1, rewriteWithSubst(Tuple2(Tuple2(subst, boundVars), next))(body0)))
            }
          }
        }
      }
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(rewriteWithSubst(Tuple2(Tuple2(subst, boundVars), next))(v_Term_typeApplication_tt.body), substType(subst)(v_Term_typeApplication_tt.`type`)))
      case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => hydra.core.Term.typeLambda(hydra.core.TypeLambda(replaceName(subst)(v_Term_typeLambda_ta.parameter), rewriteWithSubst(Tuple2(Tuple2(subst, boundVars), next))(v_Term_typeLambda_ta.body)))
      case _ => recurse(term2)
    hydra.rewriting.rewriteTerm(rewrite)(term0)
  }
  rewriteWithSubst(Tuple2(Tuple2(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name], hydra.lib.sets.empty[hydra.core.Name]), 0))(term)
}

def pruneLet(l: hydra.core.Let): hydra.core.Let =
  {
  lazy val bindingMap: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Term](hydra.lib.lists.map[hydra.core.Binding, Tuple2[hydra.core.Name, hydra.core.Term]]((b: hydra.core.Binding) => Tuple2(b.name, (b.term)))(l.bindings))
  lazy val rootName: hydra.core.Name = "[[[root]]]"
  def adj(n: hydra.core.Name): scala.collection.immutable.Set[hydra.core.Name] =
    hydra.lib.sets.intersection[hydra.core.Name](hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.maps.keys[hydra.core.Name, hydra.core.Term](bindingMap)))(hydra.rewriting.freeVariablesInTerm(hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Name](n)(rootName))(l.body)(hydra.lib.maybes.fromJust[hydra.core.Term](hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Term](n)(bindingMap)))))
  lazy val reachable: scala.collection.immutable.Set[hydra.core.Name] = hydra.sorting.findReachableNodes(adj)(rootName)
  lazy val prunedBindings: Seq[hydra.core.Binding] = hydra.lib.lists.filter[hydra.core.Binding]((b: hydra.core.Binding) => hydra.lib.sets.member[hydra.core.Name](b.name)(reachable))(l.bindings)
  hydra.core.Let(prunedBindings, (l.body))
}

def removeTermAnnotations(term: hydra.core.Term): hydra.core.Term =
  {
  def remove(recurse: (hydra.core.Term => hydra.core.Term))(term2: hydra.core.Term): hydra.core.Term =
    {
    lazy val rewritten: hydra.core.Term = recurse(term2)
    term2 match
      case hydra.core.Term.annotated(v_Term_annotated_at) => (v_Term_annotated_at.body)
      case _ => rewritten
  }
  hydra.rewriting.rewriteTerm(remove)(term)
}

def removeTypeAnnotations(typ: hydra.core.Type): hydra.core.Type =
  {
  def remove[T0](recurse: (T0 => hydra.core.Type))(typ2: T0): hydra.core.Type =
    {
    lazy val rewritten: hydra.core.Type = recurse(typ2)
    rewritten match
      case hydra.core.Type.annotated(v_Type_annotated_at) => (v_Type_annotated_at.body)
      case _ => rewritten
  }
  hydra.rewriting.rewriteType(remove)(typ)
}

def removeTypeAnnotationsFromTerm(term: hydra.core.Term): hydra.core.Term =
  {
  def strip[T0](recurse: (T0 => hydra.core.Term))(term2: T0): hydra.core.Term =
    {
    lazy val rewritten: hydra.core.Term = recurse(term2)
    def stripBinding(b: hydra.core.Binding): hydra.core.Binding = hydra.core.Binding(b.name, (b.term), None)
    rewritten match
      case hydra.core.Term.let(v_Term_let_lt) => hydra.core.Term.let(hydra.core.Let(hydra.lib.lists.map[hydra.core.Binding, hydra.core.Binding](stripBinding)(v_Term_let_lt.bindings), (v_Term_let_lt.body)))
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => (v_Term_typeApplication_tt.body)
      case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => (v_Term_typeLambda_ta.body)
      case _ => rewritten
  }
  hydra.rewriting.rewriteTerm(strip)(term)
}

def removeTypesFromTerm(term: hydra.core.Term): hydra.core.Term =
  {
  def strip[T0](recurse: (T0 => hydra.core.Term))(term2: T0): hydra.core.Term =
    {
    lazy val rewritten: hydra.core.Term = recurse(term2)
    def stripBinding(b: hydra.core.Binding): hydra.core.Binding = hydra.core.Binding(b.name, (b.term), None)
    rewritten match
      case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
        case hydra.core.Function.elimination(v_Function_elimination_e) => hydra.core.Term.function(hydra.core.Function.elimination(v_Function_elimination_e))
        case hydra.core.Function.lambda(v_Function_lambda_l) => hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda(v_Function_lambda_l.parameter, None, (v_Function_lambda_l.body))))
        case _ => hydra.core.Term.function(v_Term_function_f)
      case hydra.core.Term.let(v_Term_let_lt) => hydra.core.Term.let(hydra.core.Let(hydra.lib.lists.map[hydra.core.Binding, hydra.core.Binding](stripBinding)(v_Term_let_lt.bindings), (v_Term_let_lt.body)))
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => (v_Term_typeApplication_tt.body)
      case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => (v_Term_typeLambda_ta.body)
      case _ => rewritten
  }
  hydra.rewriting.rewriteTerm(strip)(term)
}

def replaceFreeTermVariable(vold: hydra.core.Name)(tnew: hydra.core.Term)(term: hydra.core.Term): hydra.core.Term =
  {
  def rewrite(recurse: (hydra.core.Term => hydra.core.Term))(t: hydra.core.Term): hydra.core.Term =
    t match
    case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
      case hydra.core.Function.lambda(v_Function_lambda_l) => {
        lazy val v: hydra.core.Name = (v_Function_lambda_l.parameter)
        hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Name](v)(vold))(t)(recurse(t))
      }
      case _ => recurse(t)
    case hydra.core.Term.variable(v_Term_variable_v) => hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Name](v_Term_variable_v)(vold))(tnew)(hydra.core.Term.variable(v_Term_variable_v))
    case _ => recurse(t)
  hydra.rewriting.rewriteTerm(rewrite)(term)
}

def replaceFreeTypeVariable(v: hydra.core.Name)(rep: hydra.core.Type)(typ: hydra.core.Type): hydra.core.Type =
  {
  def mapExpr(recurse: (hydra.core.Type => hydra.core.Type))(t: hydra.core.Type): hydra.core.Type =
    t match
    case hydra.core.Type.forall(v_Type_forall_ft) => hydra.lib.logic.ifElse[hydra.core.Type](hydra.lib.equality.equal[hydra.core.Name](v)(v_Type_forall_ft.parameter))(t)(hydra.core.Type.forall(hydra.core.ForallType(v_Type_forall_ft.parameter, recurse(v_Type_forall_ft.body))))
    case hydra.core.Type.variable(v_Type_variable_v_) => hydra.lib.logic.ifElse[hydra.core.Type](hydra.lib.equality.equal[hydra.core.Name](v)(`v_Type_variable_v_`))(rep)(t)
    case _ => recurse(t)
  hydra.rewriting.rewriteType(mapExpr)(typ)
}

def replaceTypedefs(types: Map[hydra.core.Name, hydra.core.TypeScheme])(typ0: hydra.core.Type): hydra.core.Type =
  {
  def rewrite(recurse: (hydra.core.Type => hydra.core.Type))(typ: hydra.core.Type): hydra.core.Type =
    typ match
    case hydra.core.Type.annotated(v_Type_annotated_at) => hydra.core.Type.annotated(hydra.core.AnnotatedType(rewrite(recurse)(v_Type_annotated_at.body), (v_Type_annotated_at.annotation)))
    case hydra.core.Type.record => typ
    case hydra.core.Type.union => typ
    case hydra.core.Type.variable(v_Type_variable_v) => {
      def forMono(t: hydra.core.Type): hydra.core.Type =
        t match
        case hydra.core.Type.record => typ
        case hydra.core.Type.union => typ
        case hydra.core.Type.wrap => typ
        case _ => rewrite(recurse)(t)
      {
        def forTypeScheme(ts: hydra.core.TypeScheme): hydra.core.Type =
          {
          lazy val t: hydra.core.Type = (ts.`type`)
          hydra.lib.logic.ifElse[hydra.core.Type](hydra.lib.lists.`null`[hydra.core.Name](ts.variables))(forMono(t))(typ)
        }
        hydra.lib.maybes.maybe[hydra.core.Type, hydra.core.TypeScheme](typ)((ts: hydra.core.TypeScheme) => forTypeScheme(ts))(hydra.lib.maps.lookup[hydra.core.Name, hydra.core.TypeScheme](v_Type_variable_v)(types))
      }
    }
    case hydra.core.Type.wrap => typ
    case _ => recurse(typ)
  hydra.rewriting.rewriteType(rewrite)(typ0)
}

def rewriteAndFoldTerm[T0](f: ((T0 => hydra.core.Term => Tuple2[T0, hydra.core.Term]) => T0 => hydra.core.Term => Tuple2[T0, hydra.core.Term]))(term0: T0)(v1: hydra.core.Term): Tuple2[T0, hydra.core.Term] =
  {
  def fsub[T1](recurse: (T1 => hydra.core.Term => Tuple2[T1, hydra.core.Term]))(val0: T1)(term02: hydra.core.Term): Tuple2[T1, hydra.core.Term] =
    {
    def forSingle[T2, T3, T4, T5, T6](rec: (T2 => T3 => Tuple2[T4, T5]))(cons: (T5 => T6))(`val`: T2)(term: T3): Tuple2[T4, T6] =
      {
      lazy val r: Tuple2[T4, T5] = rec(`val`)(term)
      Tuple2(hydra.lib.pairs.first[T4, T5](r), cons(hydra.lib.pairs.second[T4, T5](r)))
    }
    def forMany[T2, T3, T4, T5](rec: (T2 => T3 => Tuple2[T2, T4]))(cons: (Seq[T4] => T5))(`val`: T2)(els: Seq[T3]): Tuple2[T2, T5] =
      {
      lazy val rr: Tuple2[T2, Seq[T4]] = hydra.lib.lists.foldl[Tuple2[T2, Seq[T4]], T3]((r: Tuple2[T2, Seq[T4]]) =>
        (el: T3) =>
        {
        lazy val r2: Tuple2[T2, T4] = rec(hydra.lib.pairs.first[T2, Seq[T4]](r))(el)
        Tuple2(hydra.lib.pairs.first[T2, T4](r2), hydra.lib.lists.cons[T4](hydra.lib.pairs.second[T2, T4](r2))(hydra.lib.pairs.second[T2, Seq[T4]](r)))
      })(Tuple2(`val`, Seq()))(els)
      Tuple2(hydra.lib.pairs.first[T2, Seq[T4]](rr), cons(hydra.lib.lists.reverse[T4](hydra.lib.pairs.second[T2, Seq[T4]](rr))))
    }
    def forField(`val`: T1)(field: hydra.core.Field): Tuple2[T1, hydra.core.Field] =
      {
      lazy val r: Tuple2[T1, hydra.core.Term] = recurse(`val`)(field.term)
      Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](r), hydra.core.Field(field.name, hydra.lib.pairs.second[T1, hydra.core.Term](r)))
    }
    def forFields(v1: T1)(v2: Seq[hydra.core.Field]): Tuple2[T1, Seq[hydra.core.Field]] = forMany(forField)((x: Seq[hydra.core.Field]) => x)(v1)(v2)
    def forPair(`val`: T1)(kv: Tuple2[hydra.core.Term, hydra.core.Term]): Tuple2[T1, Tuple2[hydra.core.Term, hydra.core.Term]] =
      {
      lazy val rk: Tuple2[T1, hydra.core.Term] = recurse(`val`)(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](kv))
      lazy val rv: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.pairs.first[T1, hydra.core.Term](rk))(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](kv))
      Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rv), Tuple2(hydra.lib.pairs.second[T1, hydra.core.Term](rk), hydra.lib.pairs.second[T1, hydra.core.Term](rv)))
    }
    def forBinding(`val`: T1)(binding: hydra.core.Binding): Tuple2[T1, hydra.core.Binding] =
      {
      lazy val r: Tuple2[T1, hydra.core.Term] = recurse(`val`)(binding.term)
      Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](r), hydra.core.Binding(binding.name, hydra.lib.pairs.second[T1, hydra.core.Term](r), (binding.`type`)))
    }
    def forElimination(`val`: T1)(elm: hydra.core.Elimination): Tuple2[T1, hydra.core.Elimination] =
      {
      lazy val r: Tuple2[T1, hydra.core.Elimination] = elm match
        case hydra.core.Elimination.union(v_Elimination_union_cs) => {
          lazy val rmd: Option[Tuple2[T1, hydra.core.Term]] = hydra.lib.maybes.map[hydra.core.Term, Tuple2[T1, hydra.core.Term]]((v1: hydra.core.Term) => recurse(`val`)(v1))(v_Elimination_union_cs.default)
          {
            lazy val val1: T1 = hydra.lib.maybes.maybe[T1, Tuple2[T1, hydra.core.Term]](`val`)(hydra.lib.pairs.first[T1, hydra.core.Term])(rmd)
            {
              lazy val rcases: Tuple2[T1, Seq[hydra.core.Field]] = forFields(val1)(v_Elimination_union_cs.cases)
              Tuple2(hydra.lib.pairs.first[T1, Seq[hydra.core.Field]](rcases), hydra.core.Elimination.union(hydra.core.CaseStatement(v_Elimination_union_cs.typeName, hydra.lib.maybes.map[Tuple2[T1, hydra.core.Term], hydra.core.Term](hydra.lib.pairs.second[T1, hydra.core.Term])(rmd), hydra.lib.pairs.second[T1, Seq[hydra.core.Field]](rcases))))
            }
          }
        }
        case _ => Tuple2(`val`, elm)
      Tuple2(hydra.lib.pairs.first[T1, hydra.core.Elimination](r), hydra.lib.pairs.second[T1, hydra.core.Elimination](r))
    }
    def forFunction(`val`: T1)(fun: hydra.core.Function): Tuple2[T1, hydra.core.Function] =
      fun match
      case hydra.core.Function.elimination(v_Function_elimination_elm) => {
        lazy val re: Tuple2[T1, hydra.core.Elimination] = forElimination(`val`)(v_Function_elimination_elm)
        Tuple2(hydra.lib.pairs.first[T1, hydra.core.Elimination](re), hydra.core.Function.elimination(hydra.lib.pairs.second[T1, hydra.core.Elimination](re)))
      }
      case hydra.core.Function.lambda(v_Function_lambda_l) => {
        lazy val rl: Tuple2[T1, hydra.core.Term] = recurse(`val`)(v_Function_lambda_l.body)
        Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rl), hydra.core.Function.lambda(hydra.core.Lambda(v_Function_lambda_l.parameter, (v_Function_lambda_l.domain), hydra.lib.pairs.second[T1, hydra.core.Term](rl))))
      }
      case _ => Tuple2(`val`, fun)
    lazy val dflt: Tuple2[T1, hydra.core.Term] = Tuple2(val0, term02)
    term02 match
      case hydra.core.Term.annotated(v_Term_annotated_at) => forSingle(recurse)((t: hydra.core.Term) =>
        hydra.core.Term.annotated(hydra.core.AnnotatedTerm(t, (v_Term_annotated_at.annotation))))(val0)(v_Term_annotated_at.body)
      case hydra.core.Term.application(v_Term_application_a) => {
        lazy val rlhs: Tuple2[T1, hydra.core.Term] = recurse(val0)(v_Term_application_a.function)
        {
          lazy val rrhs: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.pairs.first[T1, hydra.core.Term](rlhs))(v_Term_application_a.argument)
          Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rrhs), hydra.core.Term.application(hydra.core.Application(hydra.lib.pairs.second[T1, hydra.core.Term](rlhs), hydra.lib.pairs.second[T1, hydra.core.Term](rrhs))))
        }
      }
      case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.either[hydra.core.Term, hydra.core.Term, Tuple2[T1, hydra.core.Term]]((l: hydra.core.Term) =>
        {
        lazy val rl: Tuple2[T1, hydra.core.Term] = recurse(val0)(l)
        Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rl), hydra.core.Term.either(Left(hydra.lib.pairs.second[T1, hydra.core.Term](rl))))
      })((r: hydra.core.Term) =>
        {
        lazy val rr: Tuple2[T1, hydra.core.Term] = recurse(val0)(r)
        Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rr), hydra.core.Term.either(Right(hydra.lib.pairs.second[T1, hydra.core.Term](rr))))
      })(v_Term_either_e)
      case hydra.core.Term.function(v_Term_function_f2) => forSingle(forFunction)((f3: hydra.core.Function) => hydra.core.Term.function(f3))(val0)(v_Term_function_f2)
      case hydra.core.Term.let(v_Term_let_l) => {
        lazy val renv: Tuple2[T1, hydra.core.Term] = recurse(val0)(v_Term_let_l.body)
        forMany(forBinding)((bins: Seq[hydra.core.Binding]) =>
          hydra.core.Term.let(hydra.core.Let(bins, hydra.lib.pairs.second[T1, hydra.core.Term](renv))))(hydra.lib.pairs.first[T1, hydra.core.Term](renv))(v_Term_let_l.bindings)
      }
      case hydra.core.Term.list(v_Term_list_els) => forMany(recurse)((x: Seq[hydra.core.Term]) => hydra.core.Term.list(x))(val0)(v_Term_list_els)
      case hydra.core.Term.map(v_Term_map_m) => forMany(forPair)((pairs: Seq[Tuple2[hydra.core.Term, hydra.core.Term]]) =>
        hydra.core.Term.map(hydra.lib.maps.fromList[hydra.core.Term, hydra.core.Term](pairs)))(val0)(hydra.lib.maps.toList[hydra.core.Term, hydra.core.Term](v_Term_map_m))
      case hydra.core.Term.maybe(v_Term_maybe_mt) => hydra.lib.maybes.maybe[Tuple2[T1, hydra.core.Term], hydra.core.Term](dflt)((t: hydra.core.Term) =>
        forSingle(recurse)((t1: hydra.core.Term) => hydra.core.Term.maybe(Some(t1)))(val0)(t))(v_Term_maybe_mt)
      case hydra.core.Term.pair(v_Term_pair_p) => {
        lazy val rf: Tuple2[T1, hydra.core.Term] = recurse(val0)(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](v_Term_pair_p))
        {
          lazy val rs: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.pairs.first[T1, hydra.core.Term](rf))(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](v_Term_pair_p))
          Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rs), hydra.core.Term.pair(Tuple2(hydra.lib.pairs.second[T1, hydra.core.Term](rf), hydra.lib.pairs.second[T1, hydra.core.Term](rs))))
        }
      }
      case hydra.core.Term.record(v_Term_record_r) => forMany(forField)((fields: Seq[hydra.core.Field]) =>
        hydra.core.Term.record(hydra.core.Record(v_Term_record_r.typeName, fields)))(val0)(v_Term_record_r.fields)
      case hydra.core.Term.set(v_Term_set_els) => forMany(recurse)((e: Seq[hydra.core.Term]) =>
        hydra.core.Term.set(hydra.lib.sets.fromList[hydra.core.Term](e)))(val0)(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_els))
      case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => forSingle(recurse)((t: hydra.core.Term) =>
        hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(t, (v_Term_typeApplication_ta.`type`))))(val0)(v_Term_typeApplication_ta.body)
      case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => forSingle(recurse)((t: hydra.core.Term) =>
        hydra.core.Term.typeLambda(hydra.core.TypeLambda(v_Term_typeLambda_tl.parameter, t)))(val0)(v_Term_typeLambda_tl.body)
      case hydra.core.Term.union(v_Term_union_inj) => forSingle(recurse)((t: hydra.core.Term) =>
        hydra.core.Term.union(hydra.core.Injection(v_Term_union_inj.typeName, hydra.core.Field(v_Term_union_inj.field.name, t))))(val0)(v_Term_union_inj.field.term)
      case hydra.core.Term.wrap(v_Term_wrap_wt) => forSingle(recurse)((t: hydra.core.Term) =>
        hydra.core.Term.wrap(hydra.core.WrappedTerm(v_Term_wrap_wt.typeName, t)))(val0)(v_Term_wrap_wt.body)
      case _ => dflt
  }
  def recurse(v1: T0)(v2: hydra.core.Term): Tuple2[T0, hydra.core.Term] = f((v12: T0) => (v22: hydra.core.Term) => fsub(recurse)(v12)(v22))(v1)(v2)
  recurse(term0)(v1)
}

def rewriteAndFoldTermWithGraph[T0](f: ((T0 => hydra.core.Term => Tuple2[T0, hydra.core.Term]) => hydra.graph.Graph => T0 => hydra.core.Term => Tuple2[T0, hydra.core.Term]))(cx0: hydra.graph.Graph)(val0: T0)(term0: hydra.core.Term): Tuple2[T0, hydra.core.Term] =
  {
  def wrapper[T1](lowLevelRecurse: (Tuple2[T0, hydra.graph.Graph] => hydra.core.Term => Tuple2[Tuple2[T0, T1], hydra.core.Term]))(valAndCx: Tuple2[T0, hydra.graph.Graph])(term: hydra.core.Term): Tuple2[Tuple2[T0, hydra.graph.Graph], hydra.core.Term] =
    {
    lazy val `val`: T0 = hydra.lib.pairs.first[T0, hydra.graph.Graph](valAndCx)
    lazy val cx: hydra.graph.Graph = hydra.lib.pairs.second[T0, hydra.graph.Graph](valAndCx)
    lazy val cx1: hydra.graph.Graph = term match
      case hydra.core.Term.function(v_Term_function_fun) => v_Term_function_fun match
        case hydra.core.Function.lambda(v_Function_lambda_l) => hydra.rewriting.extendGraphForLambda(cx)(v_Function_lambda_l)
        case _ => cx
      case hydra.core.Term.let(v_Term_let_l) => hydra.rewriting.extendGraphForLet((_x: hydra.graph.Graph) => (_2: hydra.core.Binding) => None)(cx)(v_Term_let_l)
      case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.rewriting.extendGraphForTypeLambda(cx)(v_Term_typeLambda_tl)
      case _ => cx
    def recurseForUser(newVal: T0)(subterm: hydra.core.Term): Tuple2[T0, hydra.core.Term] =
      {
      lazy val result: Tuple2[Tuple2[T0, T1], hydra.core.Term] = lowLevelRecurse(Tuple2(newVal, cx1))(subterm)
      Tuple2(hydra.lib.pairs.first[T0, T1](hydra.lib.pairs.first[Tuple2[T0, T1], hydra.core.Term](result)), hydra.lib.pairs.second[Tuple2[T0, T1], hydra.core.Term](result))
    }
    lazy val fResult: Tuple2[T0, hydra.core.Term] = f(recurseForUser)(cx1)(`val`)(term)
    Tuple2(Tuple2(hydra.lib.pairs.first[T0, hydra.core.Term](fResult), cx), hydra.lib.pairs.second[T0, hydra.core.Term](fResult))
  }
  lazy val result: Tuple2[Tuple2[T0, hydra.graph.Graph], hydra.core.Term] = hydra.rewriting.rewriteAndFoldTerm(wrapper)(Tuple2(val0, cx0))(term0)
  Tuple2(hydra.lib.pairs.first[T0, hydra.graph.Graph](hydra.lib.pairs.first[Tuple2[T0, hydra.graph.Graph], hydra.core.Term](result)), hydra.lib.pairs.second[Tuple2[T0, hydra.graph.Graph], hydra.core.Term](result))
}

def rewriteAndFoldTermWithGraphAndPath[T0](f: ((T0 => hydra.core.Term => Tuple2[T0, hydra.core.Term]) => Seq[hydra.accessors.TermAccessor] => hydra.graph.Graph => T0 => hydra.core.Term => Tuple2[T0, hydra.core.Term]))(cx0: hydra.graph.Graph)(val0: T0)(term0: hydra.core.Term): Tuple2[T0, hydra.core.Term] =
  {
  def wrapper[T1](recurse: (Seq[hydra.accessors.TermAccessor] => Tuple2[hydra.graph.Graph, T0] => hydra.core.Term => Tuple2[Tuple2[T1, T0], hydra.core.Term]))(path: Seq[hydra.accessors.TermAccessor])(cxAndVal: Tuple2[hydra.graph.Graph, T0])(term: hydra.core.Term): Tuple2[Tuple2[hydra.graph.Graph, T0], hydra.core.Term] =
    {
    lazy val cx: hydra.graph.Graph = hydra.lib.pairs.first[hydra.graph.Graph, T0](cxAndVal)
    lazy val `val`: T0 = hydra.lib.pairs.second[hydra.graph.Graph, T0](cxAndVal)
    lazy val cx1: hydra.graph.Graph = term match
      case hydra.core.Term.function(v_Term_function_fun) => v_Term_function_fun match
        case hydra.core.Function.lambda(v_Function_lambda_l) => hydra.rewriting.extendGraphForLambda(cx)(v_Function_lambda_l)
        case _ => cx
      case hydra.core.Term.let(v_Term_let_l) => hydra.rewriting.extendGraphForLet((_x: hydra.graph.Graph) => (_2: hydra.core.Binding) => None)(cx)(v_Term_let_l)
      case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.rewriting.extendGraphForTypeLambda(cx)(v_Term_typeLambda_tl)
      case _ => cx
    def recurseForUser(valIn: T0)(termIn: hydra.core.Term): Tuple2[T0, hydra.core.Term] =
      {
      lazy val result: Tuple2[Tuple2[T1, T0], hydra.core.Term] = recurse(path)(Tuple2(cx1, valIn))(termIn)
      Tuple2(hydra.lib.pairs.second[T1, T0](hydra.lib.pairs.first[Tuple2[T1, T0], hydra.core.Term](result)), hydra.lib.pairs.second[Tuple2[T1, T0], hydra.core.Term](result))
    }
    lazy val fResult: Tuple2[T0, hydra.core.Term] = f(recurseForUser)(path)(cx1)(`val`)(term)
    Tuple2(Tuple2(cx, hydra.lib.pairs.first[T0, hydra.core.Term](fResult)), hydra.lib.pairs.second[T0, hydra.core.Term](fResult))
  }
  lazy val result: Tuple2[Tuple2[hydra.graph.Graph, T0], hydra.core.Term] = hydra.rewriting.rewriteAndFoldTermWithPath(wrapper)(Tuple2(cx0, val0))(term0)
  Tuple2(hydra.lib.pairs.second[hydra.graph.Graph, T0](hydra.lib.pairs.first[Tuple2[hydra.graph.Graph, T0], hydra.core.Term](result)), hydra.lib.pairs.second[Tuple2[hydra.graph.Graph, T0], hydra.core.Term](result))
}

def rewriteAndFoldTermWithPath[T0](f: ((Seq[hydra.accessors.TermAccessor] => T0 => hydra.core.Term => Tuple2[T0, hydra.core.Term]) => Seq[hydra.accessors.TermAccessor] => T0 => hydra.core.Term => Tuple2[T0, hydra.core.Term]))(term0: T0)(v1: hydra.core.Term): Tuple2[T0, hydra.core.Term] =
  {
  def fsub[T1](recurse: (Seq[hydra.accessors.TermAccessor] => T1 => hydra.core.Term => Tuple2[T1, hydra.core.Term]))(path: Seq[hydra.accessors.TermAccessor])(val0: T1)(term02: hydra.core.Term): Tuple2[T1, hydra.core.Term] =
    {
    def forSingleWithAccessor[T2, T3, T4, T5, T6](rec: (Seq[hydra.accessors.TermAccessor] => T2 => T3 => Tuple2[T4, T5]))(cons: (T5 => T6))(accessor: hydra.accessors.TermAccessor)(`val`: T2)(term: T3): Tuple2[T4, T6] =
      {
      lazy val r: Tuple2[T4, T5] = rec(hydra.lib.lists.concat2[hydra.accessors.TermAccessor](path)(Seq(accessor)))(`val`)(term)
      Tuple2(hydra.lib.pairs.first[T4, T5](r), cons(hydra.lib.pairs.second[T4, T5](r)))
    }
    def forManyWithAccessors[T2, T3, T4, T5](rec: (Seq[hydra.accessors.TermAccessor] => T2 => T3 => Tuple2[T2, T4]))(cons: (Seq[T4] => T5))(`val`: T2)(accessorTermPairs: Seq[Tuple2[hydra.accessors.TermAccessor, T3]]): Tuple2[T2, T5] =
      {
      lazy val rr: Tuple2[T2, Seq[T4]] = hydra.lib.lists.foldl[Tuple2[T2, Seq[T4]], Tuple2[hydra.accessors.TermAccessor, T3]]((r: Tuple2[T2, Seq[T4]]) =>
        (atp: Tuple2[hydra.accessors.TermAccessor, T3]) =>
        {
        lazy val r2: Tuple2[T2, T4] = rec(hydra.lib.lists.concat2[hydra.accessors.TermAccessor](path)(Seq(hydra.lib.pairs.first[hydra.accessors.TermAccessor, T3](atp))))(hydra.lib.pairs.first[T2, Seq[T4]](r))(hydra.lib.pairs.second[hydra.accessors.TermAccessor, T3](atp))
        Tuple2(hydra.lib.pairs.first[T2, T4](r2), hydra.lib.lists.cons[T4](hydra.lib.pairs.second[T2, T4](r2))(hydra.lib.pairs.second[T2, Seq[T4]](r)))
      })(Tuple2(`val`, Seq()))(accessorTermPairs)
      Tuple2(hydra.lib.pairs.first[T2, Seq[T4]](rr), cons(hydra.lib.lists.reverse[T4](hydra.lib.pairs.second[T2, Seq[T4]](rr))))
    }
    def forFieldWithAccessor(mkAccessor: (hydra.core.Name => hydra.accessors.TermAccessor))(`val`: T1)(field: hydra.core.Field): Tuple2[T1, hydra.core.Field] =
      {
      lazy val r: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.accessors.TermAccessor](path)(Seq(mkAccessor(field.name))))(`val`)(field.term)
      Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](r), hydra.core.Field(field.name, hydra.lib.pairs.second[T1, hydra.core.Term](r)))
    }
    def forFieldsWithAccessor(mkAccessor: (hydra.core.Name => hydra.accessors.TermAccessor))(v1: T1)(v2: Seq[Tuple2[hydra.accessors.TermAccessor, hydra.core.Field]]): Tuple2[T1, Seq[hydra.core.Field]] =
      forManyWithAccessors((path1: Seq[hydra.accessors.TermAccessor]) =>
      (val1: T1) =>
      (field1: hydra.core.Field) => forFieldWithAccessor(mkAccessor)(val1)(field1))((x: Seq[hydra.core.Field]) => x)(v1)(v2)
    def forPairWithAccessors(keyAccessor: hydra.accessors.TermAccessor)(valAccessor: hydra.accessors.TermAccessor)(`val`: T1)(kv: Tuple2[hydra.core.Term, hydra.core.Term]): Tuple2[T1, Tuple2[hydra.core.Term, hydra.core.Term]] =
      {
      lazy val rk: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.accessors.TermAccessor](path)(Seq(keyAccessor)))(`val`)(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](kv))
      lazy val rv: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.accessors.TermAccessor](path)(Seq(valAccessor)))(hydra.lib.pairs.first[T1, hydra.core.Term](rk))(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](kv))
      Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rv), Tuple2(hydra.lib.pairs.second[T1, hydra.core.Term](rk), hydra.lib.pairs.second[T1, hydra.core.Term](rv)))
    }
    def forBindingWithAccessor(`val`: T1)(binding: hydra.core.Binding): Tuple2[T1, hydra.core.Binding] =
      {
      lazy val r: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.accessors.TermAccessor](path)(Seq(hydra.accessors.TermAccessor.letBinding(binding.name))))(`val`)(binding.term)
      Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](r), hydra.core.Binding(binding.name, hydra.lib.pairs.second[T1, hydra.core.Term](r), (binding.`type`)))
    }
    def forElimination(`val`: T1)(elm: hydra.core.Elimination): Tuple2[T1, hydra.core.Elimination] =
      {
      lazy val r: Tuple2[T1, hydra.core.Elimination] = elm match
        case hydra.core.Elimination.union(v_Elimination_union_cs) => {
          lazy val rmd: Option[Tuple2[T1, hydra.core.Term]] = hydra.lib.maybes.map[hydra.core.Term, Tuple2[T1, hydra.core.Term]]((`def`: hydra.core.Term) =>
            recurse(hydra.lib.lists.concat2[hydra.accessors.TermAccessor](path)(Seq(hydra.accessors.TermAccessor.unionCasesDefault)))(`val`)(`def`))(v_Elimination_union_cs.default)
          {
            lazy val val1: T1 = hydra.lib.maybes.maybe[T1, Tuple2[T1, hydra.core.Term]](`val`)(hydra.lib.pairs.first[T1, hydra.core.Term])(rmd)
            {
              lazy val rcases: Tuple2[T1, Seq[hydra.core.Term]] = forManyWithAccessors(recurse)((x: Seq[hydra.core.Term]) => x)(val1)(hydra.lib.lists.map[hydra.core.Field, Tuple2[hydra.accessors.TermAccessor, hydra.core.Term]]((f2: hydra.core.Field) =>
                Tuple2(hydra.accessors.TermAccessor.unionCasesBranch(f2.name), (f2.term)))(v_Elimination_union_cs.cases))
              Tuple2(hydra.lib.pairs.first[T1, Seq[hydra.core.Term]](rcases), hydra.core.Elimination.union(hydra.core.CaseStatement(v_Elimination_union_cs.typeName, hydra.lib.maybes.map[Tuple2[T1, hydra.core.Term], hydra.core.Term](hydra.lib.pairs.second[T1, hydra.core.Term])(rmd), hydra.lib.lists.map[Tuple2[hydra.core.Name, hydra.core.Term], hydra.core.Field]((ft: Tuple2[hydra.core.Name, hydra.core.Term]) =>
                hydra.core.Field(hydra.lib.pairs.first[hydra.core.Name, hydra.core.Term](ft), hydra.lib.pairs.second[hydra.core.Name, hydra.core.Term](ft)))(hydra.lib.lists.zip[hydra.core.Name, hydra.core.Term](hydra.lib.lists.map[hydra.core.Field, hydra.core.Name]((x: hydra.core.Field) => (x.name))(v_Elimination_union_cs.cases))(hydra.lib.pairs.second[T1, Seq[hydra.core.Term]](rcases))))))
            }
          }
        }
        case _ => Tuple2(`val`, elm)
      Tuple2(hydra.lib.pairs.first[T1, hydra.core.Elimination](r), hydra.lib.pairs.second[T1, hydra.core.Elimination](r))
    }
    def forFunction(`val`: T1)(fun: hydra.core.Function): Tuple2[T1, hydra.core.Function] =
      fun match
      case hydra.core.Function.elimination(v_Function_elimination_elm) => {
        lazy val re: Tuple2[T1, hydra.core.Elimination] = forElimination(`val`)(v_Function_elimination_elm)
        Tuple2(hydra.lib.pairs.first[T1, hydra.core.Elimination](re), hydra.core.Function.elimination(hydra.lib.pairs.second[T1, hydra.core.Elimination](re)))
      }
      case hydra.core.Function.lambda(v_Function_lambda_l) => {
        lazy val rl: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.accessors.TermAccessor](path)(Seq(hydra.accessors.TermAccessor.lambdaBody)))(`val`)(v_Function_lambda_l.body)
        Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rl), hydra.core.Function.lambda(hydra.core.Lambda(v_Function_lambda_l.parameter, (v_Function_lambda_l.domain), hydra.lib.pairs.second[T1, hydra.core.Term](rl))))
      }
      case _ => Tuple2(`val`, fun)
    lazy val dflt: Tuple2[T1, hydra.core.Term] = Tuple2(val0, term02)
    term02 match
      case hydra.core.Term.annotated(v_Term_annotated_at) => forSingleWithAccessor(recurse)((t: hydra.core.Term) =>
        hydra.core.Term.annotated(hydra.core.AnnotatedTerm(t, (v_Term_annotated_at.annotation))))(hydra.accessors.TermAccessor.annotatedBody)(val0)(v_Term_annotated_at.body)
      case hydra.core.Term.application(v_Term_application_a) => {
        lazy val rlhs: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.accessors.TermAccessor](path)(Seq(hydra.accessors.TermAccessor.applicationFunction)))(val0)(v_Term_application_a.function)
        {
          lazy val rrhs: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.accessors.TermAccessor](path)(Seq(hydra.accessors.TermAccessor.applicationArgument)))(hydra.lib.pairs.first[T1, hydra.core.Term](rlhs))(v_Term_application_a.argument)
          Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rrhs), hydra.core.Term.application(hydra.core.Application(hydra.lib.pairs.second[T1, hydra.core.Term](rlhs), hydra.lib.pairs.second[T1, hydra.core.Term](rrhs))))
        }
      }
      case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.either[hydra.core.Term, hydra.core.Term, Tuple2[T1, hydra.core.Term]]((l: hydra.core.Term) =>
        {
        lazy val rl: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.accessors.TermAccessor](path)(Seq(hydra.accessors.TermAccessor.sumTerm)))(val0)(l)
        Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rl), hydra.core.Term.either(Left(hydra.lib.pairs.second[T1, hydra.core.Term](rl))))
      })((r: hydra.core.Term) =>
        {
        lazy val rr: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.accessors.TermAccessor](path)(Seq(hydra.accessors.TermAccessor.sumTerm)))(val0)(r)
        Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rr), hydra.core.Term.either(Right(hydra.lib.pairs.second[T1, hydra.core.Term](rr))))
      })(v_Term_either_e)
      case hydra.core.Term.function(v_Term_function_f2) => {
        lazy val rf: Tuple2[T1, hydra.core.Function] = forFunction(val0)(v_Term_function_f2)
        Tuple2(hydra.lib.pairs.first[T1, hydra.core.Function](rf), hydra.core.Term.function(hydra.lib.pairs.second[T1, hydra.core.Function](rf)))
      }
      case hydra.core.Term.let(v_Term_let_l) => {
        lazy val renv: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.accessors.TermAccessor](path)(Seq(hydra.accessors.TermAccessor.letBody)))(val0)(v_Term_let_l.body)
        {
          lazy val rbindings: Tuple2[T1, Seq[hydra.core.Binding]] = hydra.lib.lists.foldl[Tuple2[T1, Seq[hydra.core.Binding]], hydra.core.Binding]((r: Tuple2[T1, Seq[hydra.core.Binding]]) =>
            (binding: hydra.core.Binding) =>
            {
            lazy val rb: Tuple2[T1, hydra.core.Binding] = forBindingWithAccessor(hydra.lib.pairs.first[T1, Seq[hydra.core.Binding]](r))(binding)
            Tuple2(hydra.lib.pairs.first[T1, hydra.core.Binding](rb), hydra.lib.lists.cons[hydra.core.Binding](hydra.lib.pairs.second[T1, hydra.core.Binding](rb))(hydra.lib.pairs.second[T1, Seq[hydra.core.Binding]](r)))
          })(Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](renv), Seq()))(v_Term_let_l.bindings)
          Tuple2(hydra.lib.pairs.first[T1, Seq[hydra.core.Binding]](rbindings), hydra.core.Term.let(hydra.core.Let(hydra.lib.lists.reverse[hydra.core.Binding](hydra.lib.pairs.second[T1, Seq[hydra.core.Binding]](rbindings)), hydra.lib.pairs.second[T1, hydra.core.Term](renv))))
        }
      }
      case hydra.core.Term.list(v_Term_list_els) => {
        lazy val idx: Int = 0
        {
          lazy val rr: Tuple2[Int, Tuple2[T1, Seq[hydra.core.Term]]] = hydra.lib.lists.foldl[Tuple2[Int, Tuple2[T1, Seq[hydra.core.Term]]], hydra.core.Term]((r: Tuple2[Int, Tuple2[T1, Seq[hydra.core.Term]]]) =>
            (el: hydra.core.Term) =>
            {
            lazy val r2: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.accessors.TermAccessor](path)(Seq(hydra.accessors.TermAccessor.listElement(hydra.lib.pairs.first[Int, Tuple2[T1, Seq[hydra.core.Term]]](r)))))(hydra.lib.pairs.first[T1, Seq[hydra.core.Term]](hydra.lib.pairs.second[Int, Tuple2[T1, Seq[hydra.core.Term]]](r)))(el)
            Tuple2(hydra.lib.math.add(hydra.lib.pairs.first[Int, Tuple2[T1, Seq[hydra.core.Term]]](r))(1), Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](r2), hydra.lib.lists.cons[hydra.core.Term](hydra.lib.pairs.second[T1, hydra.core.Term](r2))(hydra.lib.pairs.second[T1, Seq[hydra.core.Term]](hydra.lib.pairs.second[Int, Tuple2[T1, Seq[hydra.core.Term]]](r)))))
          })(Tuple2(idx, Tuple2(val0, Seq())))(v_Term_list_els)
          Tuple2(hydra.lib.pairs.first[T1, Seq[hydra.core.Term]](hydra.lib.pairs.second[Int, Tuple2[T1, Seq[hydra.core.Term]]](rr)), hydra.core.Term.list(hydra.lib.lists.reverse[hydra.core.Term](hydra.lib.pairs.second[T1, Seq[hydra.core.Term]](hydra.lib.pairs.second[Int, Tuple2[T1, Seq[hydra.core.Term]]](rr)))))
        }
      }
      case hydra.core.Term.map(v_Term_map_m) => {
        lazy val idx: Int = 0
        {
          lazy val rr: Tuple2[Int, Tuple2[T1, Seq[Tuple2[hydra.core.Term, hydra.core.Term]]]] = hydra.lib.lists.foldl[Tuple2[Int, Tuple2[T1, Seq[Tuple2[hydra.core.Term, hydra.core.Term]]]], Tuple2[hydra.core.Term, hydra.core.Term]]((r: Tuple2[Int, Tuple2[T1, Seq[Tuple2[hydra.core.Term, hydra.core.Term]]]]) =>
            (kv: Tuple2[hydra.core.Term, hydra.core.Term]) =>
            {
            lazy val rk: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.accessors.TermAccessor](path)(Seq(hydra.accessors.TermAccessor.mapKey(hydra.lib.pairs.first[Int, Tuple2[T1, Seq[Tuple2[hydra.core.Term, hydra.core.Term]]]](r)))))(hydra.lib.pairs.first[T1, Seq[Tuple2[hydra.core.Term, hydra.core.Term]]](hydra.lib.pairs.second[Int, Tuple2[T1, Seq[Tuple2[hydra.core.Term, hydra.core.Term]]]](r)))(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](kv))
            {
              lazy val rv: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.accessors.TermAccessor](path)(Seq(hydra.accessors.TermAccessor.mapValue(hydra.lib.pairs.first[Int, Tuple2[T1, Seq[Tuple2[hydra.core.Term, hydra.core.Term]]]](r)))))(hydra.lib.pairs.first[T1, hydra.core.Term](rk))(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](kv))
              Tuple2(hydra.lib.math.add(hydra.lib.pairs.first[Int, Tuple2[T1, Seq[Tuple2[hydra.core.Term, hydra.core.Term]]]](r))(1), Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rv), hydra.lib.lists.cons[Tuple2[hydra.core.Term, hydra.core.Term]](Tuple2(hydra.lib.pairs.second[T1, hydra.core.Term](rk), hydra.lib.pairs.second[T1, hydra.core.Term](rv)))(hydra.lib.pairs.second[T1, Seq[Tuple2[hydra.core.Term, hydra.core.Term]]](hydra.lib.pairs.second[Int, Tuple2[T1, Seq[Tuple2[hydra.core.Term, hydra.core.Term]]]](r)))))
            }
          })(Tuple2(idx, Tuple2(val0, Seq())))(hydra.lib.maps.toList[hydra.core.Term, hydra.core.Term](v_Term_map_m))
          Tuple2(hydra.lib.pairs.first[T1, Seq[Tuple2[hydra.core.Term, hydra.core.Term]]](hydra.lib.pairs.second[Int, Tuple2[T1, Seq[Tuple2[hydra.core.Term, hydra.core.Term]]]](rr)), hydra.core.Term.map(hydra.lib.maps.fromList[hydra.core.Term, hydra.core.Term](hydra.lib.lists.reverse[Tuple2[hydra.core.Term, hydra.core.Term]](hydra.lib.pairs.second[T1, Seq[Tuple2[hydra.core.Term, hydra.core.Term]]](hydra.lib.pairs.second[Int, Tuple2[T1, Seq[Tuple2[hydra.core.Term, hydra.core.Term]]]](rr))))))
        }
      }
      case hydra.core.Term.maybe(v_Term_maybe_mt) => hydra.lib.maybes.maybe[Tuple2[T1, hydra.core.Term], hydra.core.Term](dflt)((t: hydra.core.Term) =>
        forSingleWithAccessor(recurse)((t1: hydra.core.Term) => hydra.core.Term.maybe(Some(t1)))(hydra.accessors.TermAccessor.maybeTerm)(val0)(t))(v_Term_maybe_mt)
      case hydra.core.Term.pair(v_Term_pair_p) => {
        lazy val rf: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.accessors.TermAccessor](path)(Seq(hydra.accessors.TermAccessor.productTerm(0))))(val0)(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](v_Term_pair_p))
        {
          lazy val rs: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.accessors.TermAccessor](path)(Seq(hydra.accessors.TermAccessor.productTerm(1))))(hydra.lib.pairs.first[T1, hydra.core.Term](rf))(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](v_Term_pair_p))
          Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rs), hydra.core.Term.pair(Tuple2(hydra.lib.pairs.second[T1, hydra.core.Term](rf), hydra.lib.pairs.second[T1, hydra.core.Term](rs))))
        }
      }
      case hydra.core.Term.record(v_Term_record_r) => {
        lazy val rfields: Tuple2[T1, Seq[hydra.core.Term]] = forManyWithAccessors(recurse)((x: Seq[hydra.core.Term]) => x)(val0)(hydra.lib.lists.map[hydra.core.Field, Tuple2[hydra.accessors.TermAccessor, hydra.core.Term]]((f2: hydra.core.Field) =>
          Tuple2(hydra.accessors.TermAccessor.recordField(f2.name), (f2.term)))(v_Term_record_r.fields))
        Tuple2(hydra.lib.pairs.first[T1, Seq[hydra.core.Term]](rfields), hydra.core.Term.record(hydra.core.Record(v_Term_record_r.typeName, hydra.lib.lists.map[Tuple2[hydra.core.Name, hydra.core.Term], hydra.core.Field]((ft: Tuple2[hydra.core.Name, hydra.core.Term]) =>
          hydra.core.Field(hydra.lib.pairs.first[hydra.core.Name, hydra.core.Term](ft), hydra.lib.pairs.second[hydra.core.Name, hydra.core.Term](ft)))(hydra.lib.lists.zip[hydra.core.Name, hydra.core.Term](hydra.lib.lists.map[hydra.core.Field, hydra.core.Name]((x: hydra.core.Field) => (x.name))(v_Term_record_r.fields))(hydra.lib.pairs.second[T1, Seq[hydra.core.Term]](rfields))))))
      }
      case hydra.core.Term.set(v_Term_set_els) => {
        lazy val idx: Int = 0
        {
          lazy val rr: Tuple2[Int, Tuple2[T1, Seq[hydra.core.Term]]] = hydra.lib.lists.foldl[Tuple2[Int, Tuple2[T1, Seq[hydra.core.Term]]], hydra.core.Term]((r: Tuple2[Int, Tuple2[T1, Seq[hydra.core.Term]]]) =>
            (el: hydra.core.Term) =>
            {
            lazy val r2: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.accessors.TermAccessor](path)(Seq(hydra.accessors.TermAccessor.setElement(hydra.lib.pairs.first[Int, Tuple2[T1, Seq[hydra.core.Term]]](r)))))(hydra.lib.pairs.first[T1, Seq[hydra.core.Term]](hydra.lib.pairs.second[Int, Tuple2[T1, Seq[hydra.core.Term]]](r)))(el)
            Tuple2(hydra.lib.math.add(hydra.lib.pairs.first[Int, Tuple2[T1, Seq[hydra.core.Term]]](r))(1), Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](r2), hydra.lib.lists.cons[hydra.core.Term](hydra.lib.pairs.second[T1, hydra.core.Term](r2))(hydra.lib.pairs.second[T1, Seq[hydra.core.Term]](hydra.lib.pairs.second[Int, Tuple2[T1, Seq[hydra.core.Term]]](r)))))
          })(Tuple2(idx, Tuple2(val0, Seq())))(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_els))
          Tuple2(hydra.lib.pairs.first[T1, Seq[hydra.core.Term]](hydra.lib.pairs.second[Int, Tuple2[T1, Seq[hydra.core.Term]]](rr)), hydra.core.Term.set(hydra.lib.sets.fromList[hydra.core.Term](hydra.lib.lists.reverse[hydra.core.Term](hydra.lib.pairs.second[T1, Seq[hydra.core.Term]](hydra.lib.pairs.second[Int, Tuple2[T1, Seq[hydra.core.Term]]](rr))))))
        }
      }
      case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => forSingleWithAccessor(recurse)((t: hydra.core.Term) =>
        hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(t, (v_Term_typeApplication_ta.`type`))))(hydra.accessors.TermAccessor.typeApplicationTerm)(val0)(v_Term_typeApplication_ta.body)
      case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => forSingleWithAccessor(recurse)((t: hydra.core.Term) =>
        hydra.core.Term.typeLambda(hydra.core.TypeLambda(v_Term_typeLambda_tl.parameter, t)))(hydra.accessors.TermAccessor.typeLambdaBody)(val0)(v_Term_typeLambda_tl.body)
      case hydra.core.Term.union(v_Term_union_inj) => forSingleWithAccessor(recurse)((t: hydra.core.Term) =>
        hydra.core.Term.union(hydra.core.Injection(v_Term_union_inj.typeName, hydra.core.Field(v_Term_union_inj.field.name, t))))(hydra.accessors.TermAccessor.injectionTerm)(val0)(v_Term_union_inj.field.term)
      case hydra.core.Term.wrap(v_Term_wrap_wt) => forSingleWithAccessor(recurse)((t: hydra.core.Term) =>
        hydra.core.Term.wrap(hydra.core.WrappedTerm(v_Term_wrap_wt.typeName, t)))(hydra.accessors.TermAccessor.wrappedTerm)(val0)(v_Term_wrap_wt.body)
      case _ => dflt
  }
  def recurse(v1: Seq[hydra.accessors.TermAccessor])(v2: T0)(v3: hydra.core.Term): Tuple2[T0, hydra.core.Term] =
    f((v12: Seq[hydra.accessors.TermAccessor]) =>
    (v22: T0) => (v32: hydra.core.Term) => fsub(recurse)(v12)(v22)(v32))(v1)(v2)(v3)
  recurse(Seq())(term0)(v1)
}

def rewriteTerm(f: ((hydra.core.Term => hydra.core.Term) => hydra.core.Term => hydra.core.Term))(term0: hydra.core.Term): hydra.core.Term =
  {
  def fsub(recurse: (hydra.core.Term => hydra.core.Term))(term: hydra.core.Term): hydra.core.Term =
    {
    def forField(f2: hydra.core.Field): hydra.core.Field = hydra.core.Field(f2.name, recurse(f2.term))
    def forElimination(elm: hydra.core.Elimination): hydra.core.Elimination =
      elm match
      case hydra.core.Elimination.record(v_Elimination_record_p) => hydra.core.Elimination.record(v_Elimination_record_p)
      case hydra.core.Elimination.union(v_Elimination_union_cs) => hydra.core.Elimination.union(hydra.core.CaseStatement(v_Elimination_union_cs.typeName, hydra.lib.maybes.map[hydra.core.Term, hydra.core.Term](recurse)(v_Elimination_union_cs.default), hydra.lib.lists.map[hydra.core.Field, hydra.core.Field](forField)(v_Elimination_union_cs.cases)))
      case hydra.core.Elimination.wrap(v_Elimination_wrap_name) => hydra.core.Elimination.wrap(v_Elimination_wrap_name)
    def forFunction(fun: hydra.core.Function): hydra.core.Function =
      fun match
      case hydra.core.Function.elimination(v_Function_elimination_elm) => hydra.core.Function.elimination(forElimination(v_Function_elimination_elm))
      case hydra.core.Function.lambda(v_Function_lambda_l) => hydra.core.Function.lambda(hydra.core.Lambda(v_Function_lambda_l.parameter, (v_Function_lambda_l.domain), recurse(v_Function_lambda_l.body)))
      case hydra.core.Function.primitive(v_Function_primitive_name) => hydra.core.Function.primitive(v_Function_primitive_name)
    def forLet(lt: hydra.core.Let): hydra.core.Let =
      {
      def mapBinding(b: hydra.core.Binding): hydra.core.Binding = hydra.core.Binding(b.name, recurse(b.term), (b.`type`))
      hydra.core.Let(hydra.lib.lists.map[hydra.core.Binding, hydra.core.Binding](mapBinding)(lt.bindings), recurse(lt.body))
    }
    def forMap(m: Map[hydra.core.Term, hydra.core.Term]): Map[hydra.core.Term, hydra.core.Term] =
      {
      def forPair(p: Tuple2[hydra.core.Term, hydra.core.Term]): Tuple2[hydra.core.Term, hydra.core.Term] =
        Tuple2(recurse(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](p)), recurse(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](p)))
      hydra.lib.maps.fromList[hydra.core.Term, hydra.core.Term](hydra.lib.lists.map[Tuple2[hydra.core.Term, hydra.core.Term], Tuple2[hydra.core.Term, hydra.core.Term]](forPair)(hydra.lib.maps.toList[hydra.core.Term, hydra.core.Term](m)))
    }
    term match
      case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.core.Term.annotated(hydra.core.AnnotatedTerm(recurse(v_Term_annotated_at.body), (v_Term_annotated_at.annotation)))
      case hydra.core.Term.application(v_Term_application_a) => hydra.core.Term.application(hydra.core.Application(recurse(v_Term_application_a.function), recurse(v_Term_application_a.argument)))
      case hydra.core.Term.either(v_Term_either_e) => hydra.core.Term.either(hydra.lib.eithers.either[hydra.core.Term, hydra.core.Term, Either[hydra.core.Term, hydra.core.Term]]((l: hydra.core.Term) => Left(recurse(l)))((r: hydra.core.Term) => Right(recurse(r)))(v_Term_either_e))
      case hydra.core.Term.function(v_Term_function_fun) => hydra.core.Term.function(forFunction(v_Term_function_fun))
      case hydra.core.Term.let(v_Term_let_lt) => hydra.core.Term.let(forLet(v_Term_let_lt))
      case hydra.core.Term.list(v_Term_list_els) => hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Term, hydra.core.Term](recurse)(v_Term_list_els))
      case hydra.core.Term.literal(v_Term_literal_v) => hydra.core.Term.literal(v_Term_literal_v)
      case hydra.core.Term.map(v_Term_map_m) => hydra.core.Term.map(forMap(v_Term_map_m))
      case hydra.core.Term.maybe(v_Term_maybe_m) => hydra.core.Term.maybe(hydra.lib.maybes.map[hydra.core.Term, hydra.core.Term](recurse)(v_Term_maybe_m))
      case hydra.core.Term.pair(v_Term_pair_p) => hydra.core.Term.pair(Tuple2(recurse(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](v_Term_pair_p)), recurse(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](v_Term_pair_p))))
      case hydra.core.Term.record(v_Term_record_r) => hydra.core.Term.record(hydra.core.Record(v_Term_record_r.typeName, hydra.lib.lists.map[hydra.core.Field, hydra.core.Field](forField)(v_Term_record_r.fields)))
      case hydra.core.Term.set(v_Term_set_s) => hydra.core.Term.set(hydra.lib.sets.fromList[hydra.core.Term](hydra.lib.lists.map[hydra.core.Term, hydra.core.Term](recurse)(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_s))))
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(recurse(v_Term_typeApplication_tt.body), (v_Term_typeApplication_tt.`type`)))
      case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => hydra.core.Term.typeLambda(hydra.core.TypeLambda(v_Term_typeLambda_ta.parameter, recurse(v_Term_typeLambda_ta.body)))
      case hydra.core.Term.union(v_Term_union_i) => hydra.core.Term.union(hydra.core.Injection(v_Term_union_i.typeName, forField(v_Term_union_i.field)))
      case hydra.core.Term.unit => hydra.core.Term.unit
      case hydra.core.Term.variable(v_Term_variable_v) => hydra.core.Term.variable(v_Term_variable_v)
      case hydra.core.Term.wrap(v_Term_wrap_wt) => hydra.core.Term.wrap(hydra.core.WrappedTerm(v_Term_wrap_wt.typeName, recurse(v_Term_wrap_wt.body)))
  }
  def recurse(v1: hydra.core.Term): hydra.core.Term = f((v12: hydra.core.Term) => fsub(recurse)(v12))(v1)
  recurse(term0)
}

def rewriteTermM[T0](f: ((hydra.core.Term => Either[T0, hydra.core.Term]) => hydra.core.Term => Either[T0, hydra.core.Term]))(term0: hydra.core.Term): Either[T0, hydra.core.Term] =
  {
  def fsub[T1](recurse: (hydra.core.Term => Either[T1, hydra.core.Term]))(term: hydra.core.Term): Either[T1, hydra.core.Term] =
    {
    def forField(field: hydra.core.Field): Either[T1, hydra.core.Field] =
      hydra.lib.eithers.bind[T1, hydra.core.Term, hydra.core.Field](recurse(field.term))((t: hydra.core.Term) => Right(hydra.core.Field(field.name, t)))
    def forPair(kv: Tuple2[hydra.core.Term, hydra.core.Term]): Either[T1, Tuple2[hydra.core.Term, hydra.core.Term]] =
      hydra.lib.eithers.bind[T1, hydra.core.Term, Tuple2[hydra.core.Term, hydra.core.Term]](recurse(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](kv)))((k: hydra.core.Term) =>
      hydra.lib.eithers.bind[T1, hydra.core.Term, Tuple2[hydra.core.Term, hydra.core.Term]](recurse(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](kv)))((v: hydra.core.Term) => Right(Tuple2(k, v))))
    def mapBinding(b: hydra.core.Binding): Either[T1, hydra.core.Binding] =
      hydra.lib.eithers.bind[T1, hydra.core.Term, hydra.core.Binding](recurse(b.term))((v: hydra.core.Term) => Right(hydra.core.Binding(b.name, v, (b.`type`))))
    term match
      case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.lib.eithers.bind[T1, hydra.core.Term, hydra.core.Term](recurse(v_Term_annotated_at.body))((ex: hydra.core.Term) =>
        Right(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(ex, (v_Term_annotated_at.annotation)))))
      case hydra.core.Term.application(v_Term_application_app) => hydra.lib.eithers.bind[T1, hydra.core.Term, hydra.core.Term](recurse(v_Term_application_app.function))((lhs: hydra.core.Term) =>
        hydra.lib.eithers.bind[T1, hydra.core.Term, hydra.core.Term](recurse(v_Term_application_app.argument))((rhs: hydra.core.Term) =>
        Right(hydra.core.Term.application(hydra.core.Application(lhs, rhs)))))
      case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.bind[T1, Either[hydra.core.Term, hydra.core.Term], hydra.core.Term](hydra.lib.eithers.either[hydra.core.Term, hydra.core.Term, Either[T1, Either[hydra.core.Term, hydra.core.Term]]]((l: hydra.core.Term) =>
        hydra.lib.eithers.map[hydra.core.Term, Either[hydra.core.Term, hydra.core.Term], T1]((x: hydra.core.Term) => Left(x))(recurse(l)))((r: hydra.core.Term) =>
        hydra.lib.eithers.map[hydra.core.Term, Either[hydra.core.Term, hydra.core.Term], T1]((x: hydra.core.Term) => Right(x))(recurse(r)))(v_Term_either_e))((re: Either[hydra.core.Term, hydra.core.Term]) => Right(hydra.core.Term.either(re)))
      case hydra.core.Term.function(v_Term_function_fun) => {
        def forElm(e: hydra.core.Elimination): Either[T1, hydra.core.Function] =
          e match
          case hydra.core.Elimination.record(v_Elimination_record_p) => Right(hydra.core.Function.elimination(hydra.core.Elimination.record(v_Elimination_record_p)))
          case hydra.core.Elimination.union(v_Elimination_union_cs) => {
            lazy val n: hydra.core.Name = (v_Elimination_union_cs.typeName)
            {
              lazy val `def`: Option[hydra.core.Term] = (v_Elimination_union_cs.default)
              {
                lazy val cases: Seq[hydra.core.Field] = (v_Elimination_union_cs.cases)
                hydra.lib.eithers.bind[T1, Option[hydra.core.Term], hydra.core.Function](hydra.lib.maybes.maybe[Either[T1, Option[hydra.core.Term]], hydra.core.Term](Right(None))((t: hydra.core.Term) =>
                  hydra.lib.eithers.map[hydra.core.Term, Option[hydra.core.Term], T1](hydra.lib.maybes.pure[hydra.core.Term])(recurse(t)))(`def`))((rdef: Option[hydra.core.Term]) =>
                  hydra.lib.eithers.map[Seq[hydra.core.Field], hydra.core.Function, T1]((rcases: Seq[hydra.core.Field]) =>
                  hydra.core.Function.elimination(hydra.core.Elimination.union(hydra.core.CaseStatement(n, rdef, rcases))))(hydra.lib.eithers.mapList[hydra.core.Field, hydra.core.Field, T1](forField)(cases)))
              }
            }
          }
          case hydra.core.Elimination.wrap(v_Elimination_wrap_name) => Right(hydra.core.Function.elimination(hydra.core.Elimination.wrap(v_Elimination_wrap_name)))
        {
          def forFun(fun2: hydra.core.Function): Either[T1, hydra.core.Function] =
            fun2 match
            case hydra.core.Function.elimination(v_Function_elimination_e) => forElm(v_Function_elimination_e)
            case hydra.core.Function.lambda(v_Function_lambda_l) => {
              lazy val v: hydra.core.Name = (v_Function_lambda_l.parameter)
              {
                lazy val d: Option[hydra.core.Type] = (v_Function_lambda_l.domain)
                {
                  lazy val body: hydra.core.Term = (v_Function_lambda_l.body)
                  hydra.lib.eithers.bind[T1, hydra.core.Term, hydra.core.Function](recurse(body))((rbody: hydra.core.Term) =>
                    Right(hydra.core.Function.lambda(hydra.core.Lambda(v, d, rbody))))
                }
              }
            }
            case hydra.core.Function.primitive(v_Function_primitive_name) => Right(hydra.core.Function.primitive(v_Function_primitive_name))
          hydra.lib.eithers.bind[T1, hydra.core.Function, hydra.core.Term](forFun(v_Term_function_fun))((rfun: hydra.core.Function) => Right(hydra.core.Term.function(rfun)))
        }
      }
      case hydra.core.Term.let(v_Term_let_lt) => {
        lazy val bindings: Seq[hydra.core.Binding] = (v_Term_let_lt.bindings)
        {
          lazy val env: hydra.core.Term = (v_Term_let_lt.body)
          hydra.lib.eithers.bind[T1, Seq[hydra.core.Binding], hydra.core.Term](hydra.lib.eithers.mapList[hydra.core.Binding, hydra.core.Binding, T1](mapBinding)(bindings))((rbindings: Seq[hydra.core.Binding]) =>
            hydra.lib.eithers.bind[T1, hydra.core.Term, hydra.core.Term](recurse(env))((renv: hydra.core.Term) => Right(hydra.core.Term.let(hydra.core.Let(rbindings, renv)))))
        }
      }
      case hydra.core.Term.list(v_Term_list_els) => hydra.lib.eithers.bind[T1, Seq[hydra.core.Term], hydra.core.Term](hydra.lib.eithers.mapList[hydra.core.Term, hydra.core.Term, T1](recurse)(v_Term_list_els))((rels: Seq[hydra.core.Term]) => Right(hydra.core.Term.list(rels)))
      case hydra.core.Term.literal(v_Term_literal_v) => Right(hydra.core.Term.literal(v_Term_literal_v))
      case hydra.core.Term.map(v_Term_map_m) => hydra.lib.eithers.bind[T1, Seq[Tuple2[hydra.core.Term, hydra.core.Term]], hydra.core.Term](hydra.lib.eithers.mapList[Tuple2[hydra.core.Term, hydra.core.Term], Tuple2[hydra.core.Term, hydra.core.Term], T1](forPair)(hydra.lib.maps.toList[hydra.core.Term, hydra.core.Term](v_Term_map_m)))((pairs: Seq[Tuple2[hydra.core.Term, hydra.core.Term]]) =>
        Right(hydra.core.Term.map(hydra.lib.maps.fromList[hydra.core.Term, hydra.core.Term](pairs))))
      case hydra.core.Term.maybe(v_Term_maybe_m) => hydra.lib.eithers.bind[T1, Option[hydra.core.Term], hydra.core.Term](hydra.lib.eithers.mapMaybe[hydra.core.Term, hydra.core.Term, T1](recurse)(v_Term_maybe_m))((rm: Option[hydra.core.Term]) => Right(hydra.core.Term.maybe(rm)))
      case hydra.core.Term.pair(v_Term_pair_p) => hydra.lib.eithers.bind[T1, hydra.core.Term, hydra.core.Term](recurse(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](v_Term_pair_p)))((rf: hydra.core.Term) =>
        hydra.lib.eithers.bind[T1, hydra.core.Term, hydra.core.Term](recurse(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](v_Term_pair_p)))((rs: hydra.core.Term) => Right(hydra.core.Term.pair(Tuple2(rf, rs)))))
      case hydra.core.Term.record(v_Term_record_r) => {
        lazy val n: hydra.core.Name = (v_Term_record_r.typeName)
        {
          lazy val fields: Seq[hydra.core.Field] = (v_Term_record_r.fields)
          hydra.lib.eithers.map[Seq[hydra.core.Field], hydra.core.Term, T1]((rfields: Seq[hydra.core.Field]) => hydra.core.Term.record(hydra.core.Record(n, rfields)))(hydra.lib.eithers.mapList[hydra.core.Field, hydra.core.Field, T1](forField)(fields))
        }
      }
      case hydra.core.Term.set(v_Term_set_s) => hydra.lib.eithers.bind[T1, Seq[hydra.core.Term], hydra.core.Term](hydra.lib.eithers.mapList[hydra.core.Term, hydra.core.Term, T1](recurse)(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_s)))((rlist: Seq[hydra.core.Term]) =>
        Right(hydra.core.Term.set(hydra.lib.sets.fromList[hydra.core.Term](rlist))))
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => hydra.lib.eithers.bind[T1, hydra.core.Term, hydra.core.Term](recurse(v_Term_typeApplication_tt.body))((t: hydra.core.Term) =>
        Right(hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(t, (v_Term_typeApplication_tt.`type`)))))
      case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => {
        lazy val v: hydra.core.Name = (v_Term_typeLambda_tl.parameter)
        {
          lazy val body: hydra.core.Term = (v_Term_typeLambda_tl.body)
          hydra.lib.eithers.bind[T1, hydra.core.Term, hydra.core.Term](recurse(body))((rbody: hydra.core.Term) =>
            Right(hydra.core.Term.typeLambda(hydra.core.TypeLambda(v, rbody))))
        }
      }
      case hydra.core.Term.union(v_Term_union_i) => {
        lazy val n: hydra.core.Name = (v_Term_union_i.typeName)
        {
          lazy val field: hydra.core.Field = (v_Term_union_i.field)
          hydra.lib.eithers.map[hydra.core.Field, hydra.core.Term, T1]((rfield: hydra.core.Field) => hydra.core.Term.union(hydra.core.Injection(n, rfield)))(forField(field))
        }
      }
      case hydra.core.Term.unit => Right(hydra.core.Term.unit)
      case hydra.core.Term.variable(v_Term_variable_v) => Right(hydra.core.Term.variable(v_Term_variable_v))
      case hydra.core.Term.wrap(v_Term_wrap_wt) => {
        lazy val name: hydra.core.Name = (v_Term_wrap_wt.typeName)
        {
          lazy val t: hydra.core.Term = (v_Term_wrap_wt.body)
          hydra.lib.eithers.bind[T1, hydra.core.Term, hydra.core.Term](recurse(t))((rt: hydra.core.Term) =>
            Right(hydra.core.Term.wrap(hydra.core.WrappedTerm(name, rt))))
        }
      }
  }
  def recurse(v1: hydra.core.Term): Either[T0, hydra.core.Term] = f((v12: hydra.core.Term) => fsub(recurse)(v12))(v1)
  recurse(term0)
}

def rewriteTermWithContext[T0](f: ((T0 => hydra.core.Term => hydra.core.Term) => T0 => hydra.core.Term => hydra.core.Term))(cx0: T0)(term0: hydra.core.Term): hydra.core.Term =
  {
  def forSubterms[T1](recurse0: (T1 => hydra.core.Term => hydra.core.Term))(cx: T1)(term: hydra.core.Term): hydra.core.Term =
    {
    def recurse(v1: hydra.core.Term): hydra.core.Term = recurse0(cx)(v1)
    def forField(field: hydra.core.Field): hydra.core.Field = hydra.core.Field(field.name, recurse(field.term))
    def forElimination(elm: hydra.core.Elimination): hydra.core.Elimination =
      elm match
      case hydra.core.Elimination.record(v_Elimination_record_p) => hydra.core.Elimination.record(v_Elimination_record_p)
      case hydra.core.Elimination.union(v_Elimination_union_cs) => hydra.core.Elimination.union(hydra.core.CaseStatement(v_Elimination_union_cs.typeName, hydra.lib.maybes.map[hydra.core.Term, hydra.core.Term](recurse)(v_Elimination_union_cs.default), hydra.lib.lists.map[hydra.core.Field, hydra.core.Field](forField)(v_Elimination_union_cs.cases)))
      case hydra.core.Elimination.wrap(v_Elimination_wrap_name) => hydra.core.Elimination.wrap(v_Elimination_wrap_name)
    def forFunction(fun: hydra.core.Function): hydra.core.Function =
      fun match
      case hydra.core.Function.elimination(v_Function_elimination_elm) => hydra.core.Function.elimination(forElimination(v_Function_elimination_elm))
      case hydra.core.Function.lambda(v_Function_lambda_l) => hydra.core.Function.lambda(hydra.core.Lambda(v_Function_lambda_l.parameter, (v_Function_lambda_l.domain), recurse(v_Function_lambda_l.body)))
      case hydra.core.Function.primitive(v_Function_primitive_name) => hydra.core.Function.primitive(v_Function_primitive_name)
    def forLet(lt: hydra.core.Let): hydra.core.Let =
      {
      def mapBinding(b: hydra.core.Binding): hydra.core.Binding = hydra.core.Binding(b.name, recurse(b.term), (b.`type`))
      hydra.core.Let(hydra.lib.lists.map[hydra.core.Binding, hydra.core.Binding](mapBinding)(lt.bindings), recurse(lt.body))
    }
    def forMap(m: Map[hydra.core.Term, hydra.core.Term]): Map[hydra.core.Term, hydra.core.Term] =
      {
      def forPair(p: Tuple2[hydra.core.Term, hydra.core.Term]): Tuple2[hydra.core.Term, hydra.core.Term] =
        Tuple2(recurse(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](p)), recurse(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](p)))
      hydra.lib.maps.fromList[hydra.core.Term, hydra.core.Term](hydra.lib.lists.map[Tuple2[hydra.core.Term, hydra.core.Term], Tuple2[hydra.core.Term, hydra.core.Term]](forPair)(hydra.lib.maps.toList[hydra.core.Term, hydra.core.Term](m)))
    }
    term match
      case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.core.Term.annotated(hydra.core.AnnotatedTerm(recurse(v_Term_annotated_at.body), (v_Term_annotated_at.annotation)))
      case hydra.core.Term.application(v_Term_application_a) => hydra.core.Term.application(hydra.core.Application(recurse(v_Term_application_a.function), recurse(v_Term_application_a.argument)))
      case hydra.core.Term.either(v_Term_either_e) => hydra.core.Term.either(hydra.lib.eithers.either[hydra.core.Term, hydra.core.Term, Either[hydra.core.Term, hydra.core.Term]]((l: hydra.core.Term) => Left(recurse(l)))((r: hydra.core.Term) => Right(recurse(r)))(v_Term_either_e))
      case hydra.core.Term.function(v_Term_function_fun) => hydra.core.Term.function(forFunction(v_Term_function_fun))
      case hydra.core.Term.let(v_Term_let_lt) => hydra.core.Term.let(forLet(v_Term_let_lt))
      case hydra.core.Term.list(v_Term_list_els) => hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Term, hydra.core.Term](recurse)(v_Term_list_els))
      case hydra.core.Term.literal(v_Term_literal_v) => hydra.core.Term.literal(v_Term_literal_v)
      case hydra.core.Term.map(v_Term_map_m) => hydra.core.Term.map(forMap(v_Term_map_m))
      case hydra.core.Term.maybe(v_Term_maybe_m) => hydra.core.Term.maybe(hydra.lib.maybes.map[hydra.core.Term, hydra.core.Term](recurse)(v_Term_maybe_m))
      case hydra.core.Term.pair(v_Term_pair_p) => hydra.core.Term.pair(Tuple2(recurse(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](v_Term_pair_p)), recurse(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](v_Term_pair_p))))
      case hydra.core.Term.record(v_Term_record_r) => hydra.core.Term.record(hydra.core.Record(v_Term_record_r.typeName, hydra.lib.lists.map[hydra.core.Field, hydra.core.Field](forField)(v_Term_record_r.fields)))
      case hydra.core.Term.set(v_Term_set_s) => hydra.core.Term.set(hydra.lib.sets.fromList[hydra.core.Term](hydra.lib.lists.map[hydra.core.Term, hydra.core.Term](recurse)(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_s))))
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(recurse(v_Term_typeApplication_tt.body), (v_Term_typeApplication_tt.`type`)))
      case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => hydra.core.Term.typeLambda(hydra.core.TypeLambda(v_Term_typeLambda_ta.parameter, recurse(v_Term_typeLambda_ta.body)))
      case hydra.core.Term.union(v_Term_union_i) => hydra.core.Term.union(hydra.core.Injection(v_Term_union_i.typeName, forField(v_Term_union_i.field)))
      case hydra.core.Term.unit => hydra.core.Term.unit
      case hydra.core.Term.variable(v_Term_variable_v) => hydra.core.Term.variable(v_Term_variable_v)
      case hydra.core.Term.wrap(v_Term_wrap_wt) => hydra.core.Term.wrap(hydra.core.WrappedTerm(v_Term_wrap_wt.typeName, recurse(v_Term_wrap_wt.body)))
  }
  def rewrite(cx: T0)(term: hydra.core.Term): hydra.core.Term = f((v1: T0) => (v2: hydra.core.Term) => forSubterms(rewrite)(v1)(v2))(cx)(term)
  rewrite(cx0)(term0)
}

def rewriteTermWithGraph[T0](f: ((hydra.core.Term => T0) => hydra.graph.Graph => hydra.core.Term => T0))(cx0: hydra.graph.Graph)(term0: hydra.core.Term): T0 =
  {
  def f2(recurse: (hydra.graph.Graph => hydra.core.Term => T0))(cx: hydra.graph.Graph)(term: hydra.core.Term): T0 =
    {
    def recurse1(term2: hydra.core.Term): T0 = recurse(cx)(term2)
    term match
      case hydra.core.Term.function(v_Term_function_fun) => v_Term_function_fun match
        case hydra.core.Function.lambda(v_Function_lambda_l) => {
          lazy val cx1: hydra.graph.Graph = hydra.rewriting.extendGraphForLambda(cx)(v_Function_lambda_l)
          {
            def recurse2(term2: hydra.core.Term): T0 = recurse(cx1)(term2)
            f(recurse2)(cx1)(term)
          }
        }
        case _ => f(recurse1)(cx)(term)
      case hydra.core.Term.let(v_Term_let_l) => {
        lazy val cx1: hydra.graph.Graph = hydra.rewriting.extendGraphForLet((_x: hydra.graph.Graph) => (_2: hydra.core.Binding) => None)(cx)(v_Term_let_l)
        {
          def recurse2(term2: hydra.core.Term): T0 = recurse(cx1)(term2)
          f(recurse2)(cx1)(term)
        }
      }
      case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => {
        lazy val cx1: hydra.graph.Graph = hydra.rewriting.extendGraphForTypeLambda(cx)(v_Term_typeLambda_tl)
        {
          def recurse2(term2: hydra.core.Term): T0 = recurse(cx1)(term2)
          f(recurse2)(cx1)(term)
        }
      }
      case _ => f(recurse1)(cx)(term)
  }
  def rewrite(cx: hydra.graph.Graph)(term: hydra.core.Term): T0 = f2(rewrite)(cx)(term)
  rewrite(cx0)(term0)
}

def rewriteTermWithContextM[T0, T1](f: ((T0 => hydra.core.Term => Either[T1, hydra.core.Term]) => T0 => hydra.core.Term => Either[T1, hydra.core.Term]))(cx0: T0)(term0: hydra.core.Term): Either[T1, hydra.core.Term] =
  {
  def forSubterms[T2, T3](recurse0: (T2 => hydra.core.Term => Either[T3, hydra.core.Term]))(cx: T2)(term: hydra.core.Term): Either[T3, hydra.core.Term] =
    {
    def recurse(v1: hydra.core.Term): Either[T3, hydra.core.Term] = recurse0(cx)(v1)
    def forField(field: hydra.core.Field): Either[T3, hydra.core.Field] =
      hydra.lib.eithers.bind[T3, hydra.core.Term, hydra.core.Field](recurse(field.term))((t: hydra.core.Term) => Right(hydra.core.Field(field.name, t)))
    def forPair(kv: Tuple2[hydra.core.Term, hydra.core.Term]): Either[T3, Tuple2[hydra.core.Term, hydra.core.Term]] =
      hydra.lib.eithers.bind[T3, hydra.core.Term, Tuple2[hydra.core.Term, hydra.core.Term]](recurse(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](kv)))((k: hydra.core.Term) =>
      hydra.lib.eithers.bind[T3, hydra.core.Term, Tuple2[hydra.core.Term, hydra.core.Term]](recurse(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](kv)))((v: hydra.core.Term) => Right(Tuple2(k, v))))
    def forElimination(e: hydra.core.Elimination): Either[T3, hydra.core.Function] =
      e match
      case hydra.core.Elimination.record(v_Elimination_record_p) => Right(hydra.core.Function.elimination(hydra.core.Elimination.record(v_Elimination_record_p)))
      case hydra.core.Elimination.union(v_Elimination_union_cs) => {
        lazy val n: hydra.core.Name = (v_Elimination_union_cs.typeName)
        {
          lazy val `def`: Option[hydra.core.Term] = (v_Elimination_union_cs.default)
          {
            lazy val cases: Seq[hydra.core.Field] = (v_Elimination_union_cs.cases)
            hydra.lib.eithers.bind[T3, Option[hydra.core.Term], hydra.core.Function](hydra.lib.maybes.maybe[Either[T3, Option[hydra.core.Term]], hydra.core.Term](Right(None))((t: hydra.core.Term) =>
              hydra.lib.eithers.map[hydra.core.Term, Option[hydra.core.Term], T3](hydra.lib.maybes.pure[hydra.core.Term])(recurse(t)))(`def`))((rdef: Option[hydra.core.Term]) =>
              hydra.lib.eithers.map[Seq[hydra.core.Field], hydra.core.Function, T3]((rcases: Seq[hydra.core.Field]) =>
              hydra.core.Function.elimination(hydra.core.Elimination.union(hydra.core.CaseStatement(n, rdef, rcases))))(hydra.lib.eithers.mapList[hydra.core.Field, hydra.core.Field, T3](forField)(cases)))
          }
        }
      }
      case hydra.core.Elimination.wrap(v_Elimination_wrap_name) => Right(hydra.core.Function.elimination(hydra.core.Elimination.wrap(v_Elimination_wrap_name)))
    def forFunction(fun: hydra.core.Function): Either[T3, hydra.core.Function] =
      fun match
      case hydra.core.Function.elimination(v_Function_elimination_e) => forElimination(v_Function_elimination_e)
      case hydra.core.Function.lambda(v_Function_lambda_l) => {
        lazy val v: hydra.core.Name = (v_Function_lambda_l.parameter)
        {
          lazy val d: Option[hydra.core.Type] = (v_Function_lambda_l.domain)
          {
            lazy val body: hydra.core.Term = (v_Function_lambda_l.body)
            hydra.lib.eithers.bind[T3, hydra.core.Term, hydra.core.Function](recurse(body))((rbody: hydra.core.Term) =>
              Right(hydra.core.Function.lambda(hydra.core.Lambda(v, d, rbody))))
          }
        }
      }
      case hydra.core.Function.primitive(v_Function_primitive_name) => Right(hydra.core.Function.primitive(v_Function_primitive_name))
    def mapBinding(b: hydra.core.Binding): Either[T3, hydra.core.Binding] =
      hydra.lib.eithers.bind[T3, hydra.core.Term, hydra.core.Binding](recurse(b.term))((v: hydra.core.Term) => Right(hydra.core.Binding(b.name, v, (b.`type`))))
    term match
      case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.lib.eithers.bind[T3, hydra.core.Term, hydra.core.Term](recurse(v_Term_annotated_at.body))((ex: hydra.core.Term) =>
        Right(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(ex, (v_Term_annotated_at.annotation)))))
      case hydra.core.Term.application(v_Term_application_app) => hydra.lib.eithers.bind[T3, hydra.core.Term, hydra.core.Term](recurse(v_Term_application_app.function))((lhs: hydra.core.Term) =>
        hydra.lib.eithers.bind[T3, hydra.core.Term, hydra.core.Term](recurse(v_Term_application_app.argument))((rhs: hydra.core.Term) =>
        Right(hydra.core.Term.application(hydra.core.Application(lhs, rhs)))))
      case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.bind[T3, Either[hydra.core.Term, hydra.core.Term], hydra.core.Term](hydra.lib.eithers.either[hydra.core.Term, hydra.core.Term, Either[T3, Either[hydra.core.Term, hydra.core.Term]]]((l: hydra.core.Term) =>
        hydra.lib.eithers.map[hydra.core.Term, Either[hydra.core.Term, hydra.core.Term], T3]((x: hydra.core.Term) => Left(x))(recurse(l)))((r: hydra.core.Term) =>
        hydra.lib.eithers.map[hydra.core.Term, Either[hydra.core.Term, hydra.core.Term], T3]((x: hydra.core.Term) => Right(x))(recurse(r)))(v_Term_either_e))((re: Either[hydra.core.Term, hydra.core.Term]) => Right(hydra.core.Term.either(re)))
      case hydra.core.Term.function(v_Term_function_fun) => hydra.lib.eithers.bind[T3, hydra.core.Function, hydra.core.Term](forFunction(v_Term_function_fun))((rfun: hydra.core.Function) => Right(hydra.core.Term.function(rfun)))
      case hydra.core.Term.let(v_Term_let_lt) => {
        lazy val bindings: Seq[hydra.core.Binding] = (v_Term_let_lt.bindings)
        {
          lazy val body: hydra.core.Term = (v_Term_let_lt.body)
          hydra.lib.eithers.bind[T3, Seq[hydra.core.Binding], hydra.core.Term](hydra.lib.eithers.mapList[hydra.core.Binding, hydra.core.Binding, T3](mapBinding)(bindings))((rbindings: Seq[hydra.core.Binding]) =>
            hydra.lib.eithers.bind[T3, hydra.core.Term, hydra.core.Term](recurse(body))((rbody: hydra.core.Term) => Right(hydra.core.Term.let(hydra.core.Let(rbindings, rbody)))))
        }
      }
      case hydra.core.Term.list(v_Term_list_els) => hydra.lib.eithers.bind[T3, Seq[hydra.core.Term], hydra.core.Term](hydra.lib.eithers.mapList[hydra.core.Term, hydra.core.Term, T3](recurse)(v_Term_list_els))((rels: Seq[hydra.core.Term]) => Right(hydra.core.Term.list(rels)))
      case hydra.core.Term.literal(v_Term_literal_v) => Right(hydra.core.Term.literal(v_Term_literal_v))
      case hydra.core.Term.map(v_Term_map_m) => hydra.lib.eithers.bind[T3, Seq[Tuple2[hydra.core.Term, hydra.core.Term]], hydra.core.Term](hydra.lib.eithers.mapList[Tuple2[hydra.core.Term, hydra.core.Term], Tuple2[hydra.core.Term, hydra.core.Term], T3](forPair)(hydra.lib.maps.toList[hydra.core.Term, hydra.core.Term](v_Term_map_m)))((pairs: Seq[Tuple2[hydra.core.Term, hydra.core.Term]]) =>
        Right(hydra.core.Term.map(hydra.lib.maps.fromList[hydra.core.Term, hydra.core.Term](pairs))))
      case hydra.core.Term.maybe(v_Term_maybe_m) => hydra.lib.eithers.bind[T3, Option[hydra.core.Term], hydra.core.Term](hydra.lib.eithers.mapMaybe[hydra.core.Term, hydra.core.Term, T3](recurse)(v_Term_maybe_m))((rm: Option[hydra.core.Term]) => Right(hydra.core.Term.maybe(rm)))
      case hydra.core.Term.pair(v_Term_pair_p) => hydra.lib.eithers.bind[T3, hydra.core.Term, hydra.core.Term](recurse(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](v_Term_pair_p)))((rfirst: hydra.core.Term) =>
        hydra.lib.eithers.bind[T3, hydra.core.Term, hydra.core.Term](recurse(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](v_Term_pair_p)))((rsecond: hydra.core.Term) => Right(hydra.core.Term.pair(Tuple2(rfirst, rsecond)))))
      case hydra.core.Term.record(v_Term_record_r) => {
        lazy val n: hydra.core.Name = (v_Term_record_r.typeName)
        {
          lazy val fields: Seq[hydra.core.Field] = (v_Term_record_r.fields)
          hydra.lib.eithers.map[Seq[hydra.core.Field], hydra.core.Term, T3]((rfields: Seq[hydra.core.Field]) => hydra.core.Term.record(hydra.core.Record(n, rfields)))(hydra.lib.eithers.mapList[hydra.core.Field, hydra.core.Field, T3](forField)(fields))
        }
      }
      case hydra.core.Term.set(v_Term_set_s) => hydra.lib.eithers.bind[T3, Seq[hydra.core.Term], hydra.core.Term](hydra.lib.eithers.mapList[hydra.core.Term, hydra.core.Term, T3](recurse)(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_s)))((rlist: Seq[hydra.core.Term]) =>
        Right(hydra.core.Term.set(hydra.lib.sets.fromList[hydra.core.Term](rlist))))
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => hydra.lib.eithers.bind[T3, hydra.core.Term, hydra.core.Term](recurse(v_Term_typeApplication_tt.body))((t: hydra.core.Term) =>
        Right(hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(t, (v_Term_typeApplication_tt.`type`)))))
      case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => {
        lazy val v: hydra.core.Name = (v_Term_typeLambda_tl.parameter)
        {
          lazy val body: hydra.core.Term = (v_Term_typeLambda_tl.body)
          hydra.lib.eithers.bind[T3, hydra.core.Term, hydra.core.Term](recurse(body))((rbody: hydra.core.Term) =>
            Right(hydra.core.Term.typeLambda(hydra.core.TypeLambda(v, rbody))))
        }
      }
      case hydra.core.Term.union(v_Term_union_i) => {
        lazy val n: hydra.core.Name = (v_Term_union_i.typeName)
        {
          lazy val field: hydra.core.Field = (v_Term_union_i.field)
          hydra.lib.eithers.map[hydra.core.Field, hydra.core.Term, T3]((rfield: hydra.core.Field) => hydra.core.Term.union(hydra.core.Injection(n, rfield)))(forField(field))
        }
      }
      case hydra.core.Term.unit => Right(hydra.core.Term.unit)
      case hydra.core.Term.variable(v_Term_variable_v) => Right(hydra.core.Term.variable(v_Term_variable_v))
      case hydra.core.Term.wrap(v_Term_wrap_wt) => {
        lazy val name: hydra.core.Name = (v_Term_wrap_wt.typeName)
        {
          lazy val t: hydra.core.Term = (v_Term_wrap_wt.body)
          hydra.lib.eithers.bind[T3, hydra.core.Term, hydra.core.Term](recurse(t))((rt: hydra.core.Term) =>
            Right(hydra.core.Term.wrap(hydra.core.WrappedTerm(name, rt))))
        }
      }
  }
  def rewrite(cx: T0)(term: hydra.core.Term): Either[T1, hydra.core.Term] = f((v1: T0) => (v2: hydra.core.Term) => forSubterms(rewrite)(v1)(v2))(cx)(term)
  rewrite(cx0)(term0)
}

def rewriteType(f: ((hydra.core.Type => hydra.core.Type) => hydra.core.Type => hydra.core.Type))(typ0: hydra.core.Type): hydra.core.Type =
  {
  def fsub(recurse: (hydra.core.Type => hydra.core.Type))(typ: hydra.core.Type): hydra.core.Type =
    {
    def forField(field: hydra.core.FieldType): hydra.core.FieldType = hydra.core.FieldType(field.name, recurse(field.`type`))
    typ match
      case hydra.core.Type.annotated(v_Type_annotated_at) => hydra.core.Type.annotated(hydra.core.AnnotatedType(recurse(v_Type_annotated_at.body), (v_Type_annotated_at.annotation)))
      case hydra.core.Type.application(v_Type_application_app) => hydra.core.Type.application(hydra.core.ApplicationType(recurse(v_Type_application_app.function), recurse(v_Type_application_app.argument)))
      case hydra.core.Type.either(v_Type_either_et) => hydra.core.Type.either(hydra.core.EitherType(recurse(v_Type_either_et.left), recurse(v_Type_either_et.right)))
      case hydra.core.Type.pair(v_Type_pair_pt) => hydra.core.Type.pair(hydra.core.PairType(recurse(v_Type_pair_pt.first), recurse(v_Type_pair_pt.second)))
      case hydra.core.Type.function(v_Type_function_fun) => hydra.core.Type.function(hydra.core.FunctionType(recurse(v_Type_function_fun.domain), recurse(v_Type_function_fun.codomain)))
      case hydra.core.Type.forall(v_Type_forall_lt) => hydra.core.Type.forall(hydra.core.ForallType(v_Type_forall_lt.parameter, recurse(v_Type_forall_lt.body)))
      case hydra.core.Type.list(v_Type_list_t) => hydra.core.Type.list(recurse(v_Type_list_t))
      case hydra.core.Type.literal(v_Type_literal_lt) => hydra.core.Type.literal(v_Type_literal_lt)
      case hydra.core.Type.map(v_Type_map_mt) => hydra.core.Type.map(hydra.core.MapType(recurse(v_Type_map_mt.keys), recurse(v_Type_map_mt.values)))
      case hydra.core.Type.maybe(v_Type_maybe_t) => hydra.core.Type.maybe(recurse(v_Type_maybe_t))
      case hydra.core.Type.record(v_Type_record_rt) => hydra.core.Type.record(hydra.lib.lists.map[hydra.core.FieldType, hydra.core.FieldType](forField)(v_Type_record_rt))
      case hydra.core.Type.set(v_Type_set_t) => hydra.core.Type.set(recurse(v_Type_set_t))
      case hydra.core.Type.union(v_Type_union_rt) => hydra.core.Type.union(hydra.lib.lists.map[hydra.core.FieldType, hydra.core.FieldType](forField)(v_Type_union_rt))
      case hydra.core.Type.unit => hydra.core.Type.unit
      case hydra.core.Type.variable(v_Type_variable_v) => hydra.core.Type.variable(v_Type_variable_v)
      case hydra.core.Type.void => hydra.core.Type.void
      case hydra.core.Type.wrap(v_Type_wrap_wt) => hydra.core.Type.wrap(recurse(v_Type_wrap_wt))
  }
  def recurse(v1: hydra.core.Type): hydra.core.Type = f((v12: hydra.core.Type) => fsub(recurse)(v12))(v1)
  recurse(typ0)
}

def rewriteTypeM[T0](f: ((hydra.core.Type => Either[T0, hydra.core.Type]) => hydra.core.Type => Either[T0, hydra.core.Type]))(typ0: hydra.core.Type): Either[T0, hydra.core.Type] =
  {
  def fsub[T1](recurse: (hydra.core.Type => Either[T1, hydra.core.Type]))(typ: hydra.core.Type): Either[T1, hydra.core.Type] =
    typ match
    case hydra.core.Type.annotated(v_Type_annotated_at) => hydra.lib.eithers.bind[T1, hydra.core.Type, hydra.core.Type](recurse(v_Type_annotated_at.body))((t: hydra.core.Type) =>
      Right(hydra.core.Type.annotated(hydra.core.AnnotatedType(t, (v_Type_annotated_at.annotation)))))
    case hydra.core.Type.application(v_Type_application_at) => hydra.lib.eithers.bind[T1, hydra.core.Type, hydra.core.Type](recurse(v_Type_application_at.function))((lhs: hydra.core.Type) =>
      hydra.lib.eithers.bind[T1, hydra.core.Type, hydra.core.Type](recurse(v_Type_application_at.argument))((rhs: hydra.core.Type) =>
      Right(hydra.core.Type.application(hydra.core.ApplicationType(lhs, rhs)))))
    case hydra.core.Type.either(v_Type_either_et) => hydra.lib.eithers.bind[T1, hydra.core.Type, hydra.core.Type](recurse(v_Type_either_et.left))((left: hydra.core.Type) =>
      hydra.lib.eithers.bind[T1, hydra.core.Type, hydra.core.Type](recurse(v_Type_either_et.right))((right: hydra.core.Type) =>
      Right(hydra.core.Type.either(hydra.core.EitherType(left, right)))))
    case hydra.core.Type.pair(v_Type_pair_pt) => hydra.lib.eithers.bind[T1, hydra.core.Type, hydra.core.Type](recurse(v_Type_pair_pt.first))((pairFirst: hydra.core.Type) =>
      hydra.lib.eithers.bind[T1, hydra.core.Type, hydra.core.Type](recurse(v_Type_pair_pt.second))((pairSecond: hydra.core.Type) =>
      Right(hydra.core.Type.pair(hydra.core.PairType(pairFirst, pairSecond)))))
    case hydra.core.Type.function(v_Type_function_ft) => hydra.lib.eithers.bind[T1, hydra.core.Type, hydra.core.Type](recurse(v_Type_function_ft.domain))((dom: hydra.core.Type) =>
      hydra.lib.eithers.bind[T1, hydra.core.Type, hydra.core.Type](recurse(v_Type_function_ft.codomain))((cod: hydra.core.Type) =>
      Right(hydra.core.Type.function(hydra.core.FunctionType(dom, cod)))))
    case hydra.core.Type.forall(v_Type_forall_ft) => hydra.lib.eithers.bind[T1, hydra.core.Type, hydra.core.Type](recurse(v_Type_forall_ft.body))((b: hydra.core.Type) =>
      Right(hydra.core.Type.forall(hydra.core.ForallType(v_Type_forall_ft.parameter, b))))
    case hydra.core.Type.list(v_Type_list_t) => hydra.lib.eithers.bind[T1, hydra.core.Type, hydra.core.Type](recurse(v_Type_list_t))((rt: hydra.core.Type) => Right(hydra.core.Type.list(rt)))
    case hydra.core.Type.literal(v_Type_literal_lt) => Right(hydra.core.Type.literal(v_Type_literal_lt))
    case hydra.core.Type.map(v_Type_map_mt) => hydra.lib.eithers.bind[T1, hydra.core.Type, hydra.core.Type](recurse(v_Type_map_mt.keys))((kt: hydra.core.Type) =>
      hydra.lib.eithers.bind[T1, hydra.core.Type, hydra.core.Type](recurse(v_Type_map_mt.values))((vt: hydra.core.Type) => Right(hydra.core.Type.map(hydra.core.MapType(kt, vt)))))
    case hydra.core.Type.maybe(v_Type_maybe_t) => hydra.lib.eithers.bind[T1, hydra.core.Type, hydra.core.Type](recurse(v_Type_maybe_t))((rt: hydra.core.Type) => Right(hydra.core.Type.maybe(rt)))
    case hydra.core.Type.record(v_Type_record_rt) => {
      def forField(f2: hydra.core.FieldType): Either[T1, hydra.core.FieldType] =
        hydra.lib.eithers.bind[T1, hydra.core.Type, hydra.core.FieldType](recurse(f2.`type`))((t: hydra.core.Type) => Right(hydra.core.FieldType(f2.name, t)))
      hydra.lib.eithers.bind[T1, Seq[hydra.core.FieldType], hydra.core.Type](hydra.lib.eithers.mapList[hydra.core.FieldType, hydra.core.FieldType, T1](forField)(v_Type_record_rt))((rfields: Seq[hydra.core.FieldType]) => Right(hydra.core.Type.record(rfields)))
    }
    case hydra.core.Type.set(v_Type_set_t) => hydra.lib.eithers.bind[T1, hydra.core.Type, hydra.core.Type](recurse(v_Type_set_t))((rt: hydra.core.Type) => Right(hydra.core.Type.set(rt)))
    case hydra.core.Type.union(v_Type_union_rt) => {
      def forField(f2: hydra.core.FieldType): Either[T1, hydra.core.FieldType] =
        hydra.lib.eithers.bind[T1, hydra.core.Type, hydra.core.FieldType](recurse(f2.`type`))((t: hydra.core.Type) => Right(hydra.core.FieldType(f2.name, t)))
      hydra.lib.eithers.bind[T1, Seq[hydra.core.FieldType], hydra.core.Type](hydra.lib.eithers.mapList[hydra.core.FieldType, hydra.core.FieldType, T1](forField)(v_Type_union_rt))((rfields: Seq[hydra.core.FieldType]) => Right(hydra.core.Type.union(rfields)))
    }
    case hydra.core.Type.unit => Right(hydra.core.Type.unit)
    case hydra.core.Type.variable(v_Type_variable_v) => Right(hydra.core.Type.variable(v_Type_variable_v))
    case hydra.core.Type.void => Right(hydra.core.Type.void)
    case hydra.core.Type.wrap(v_Type_wrap_wt) => hydra.lib.eithers.bind[T1, hydra.core.Type, hydra.core.Type](recurse(v_Type_wrap_wt))((t: hydra.core.Type) => Right(hydra.core.Type.wrap(t)))
  def recurse(v1: hydra.core.Type): Either[T0, hydra.core.Type] = f((v12: hydra.core.Type) => fsub(recurse)(v12))(v1)
  recurse(typ0)
}

def simplifyTerm(term: hydra.core.Term): hydra.core.Term =
  {
  def simplify[T0](recurse: (hydra.core.Term => T0))(term2: hydra.core.Term): T0 =
    {
    def forRhs(rhs: hydra.core.Term)(`var`: hydra.core.Name)(body: hydra.core.Term): hydra.core.Term =
      hydra.rewriting.deannotateTerm(rhs) match
      case hydra.core.Term.variable(v_Term_variable_v) => hydra.rewriting.simplifyTerm(hydra.rewriting.substituteVariable(`var`)(v_Term_variable_v)(body))
      case _ => term2
    def forLhs(lhs: hydra.core.Term)(rhs: hydra.core.Term): hydra.core.Term =
      {
      def forFun(fun: hydra.core.Function): hydra.core.Term =
        fun match
        case hydra.core.Function.lambda(v_Function_lambda_l) => {
          lazy val `var`: hydra.core.Name = (v_Function_lambda_l.parameter)
          {
            lazy val body: hydra.core.Term = (v_Function_lambda_l.body)
            hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.sets.member[hydra.core.Name](`var`)(hydra.rewriting.freeVariablesInTerm(body)))(forRhs(rhs)(`var`)(body))(hydra.rewriting.simplifyTerm(body))
          }
        }
        case _ => term2
      hydra.rewriting.deannotateTerm(lhs) match
        case hydra.core.Term.function(v_Term_function_fun) => forFun(v_Term_function_fun)
        case _ => term2
    }
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
    lazy val stripped: hydra.core.Term = hydra.rewriting.deannotateTerm(term2)
    recurse(forTerm(stripped))
  }
  hydra.rewriting.rewriteTerm(simplify)(term)
}

def substituteTypeVariables(subst: Map[hydra.core.Name, hydra.core.Name])(typ: hydra.core.Type): hydra.core.Type =
  {
  def replace(recurse: (hydra.core.Type => hydra.core.Type))(typ2: hydra.core.Type): hydra.core.Type =
    typ2 match
    case hydra.core.Type.variable(v_Type_variable_n) => hydra.core.Type.variable(hydra.lib.maybes.fromMaybe[hydra.core.Name](v_Type_variable_n)(hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Name](v_Type_variable_n)(subst)))
    case _ => recurse(typ2)
  hydra.rewriting.rewriteType(replace)(typ)
}

def substituteTypeVariablesInTerm(subst: Map[hydra.core.Name, hydra.core.Name])(term: hydra.core.Term): hydra.core.Term =
  {
  def st(v1: hydra.core.Type): hydra.core.Type = hydra.rewriting.substituteTypeVariables(subst)(v1)
  def stOpt(mt: Option[hydra.core.Type]): Option[hydra.core.Type] = hydra.lib.maybes.map[hydra.core.Type, hydra.core.Type](st)(mt)
  def stScheme(ts: hydra.core.TypeScheme): hydra.core.TypeScheme = hydra.core.TypeScheme(ts.variables, st(ts.`type`), (ts.constraints))
  def stSchemeOpt(mts: Option[hydra.core.TypeScheme]): Option[hydra.core.TypeScheme] =
    hydra.lib.maybes.map[hydra.core.TypeScheme, hydra.core.TypeScheme](stScheme)(mts)
  def replace(recurse: (hydra.core.Term => hydra.core.Term))(t: hydra.core.Term): hydra.core.Term =
    t match
    case hydra.core.Term.function(v_Term_function_v1) => v_Term_function_v1 match
      case hydra.core.Function.lambda(v_Function_lambda_l) => hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda(v_Function_lambda_l.parameter, stOpt(v_Function_lambda_l.domain), recurse(v_Function_lambda_l.body))))
      case _ => recurse(t)
    case hydra.core.Term.let(v_Term_let_lt) => {
      def mapBinding(b: hydra.core.Binding): hydra.core.Binding = hydra.core.Binding(b.name, recurse(b.term), stSchemeOpt(b.`type`))
      hydra.core.Term.let(hydra.core.Let(hydra.lib.lists.map[hydra.core.Binding, hydra.core.Binding](mapBinding)(v_Term_let_lt.bindings), recurse(v_Term_let_lt.body)))
    }
    case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(recurse(v_Term_typeApplication_tt.body), st(v_Term_typeApplication_tt.`type`)))
    case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.core.Term.typeLambda(hydra.core.TypeLambda(hydra.lib.maybes.fromMaybe[hydra.core.Name](v_Term_typeLambda_tl.parameter)(hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Name](v_Term_typeLambda_tl.parameter)(subst)), recurse(v_Term_typeLambda_tl.body)))
    case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.core.Term.annotated(hydra.core.AnnotatedTerm(recurse(v_Term_annotated_at.body), (v_Term_annotated_at.annotation)))
    case _ => recurse(t)
  hydra.rewriting.rewriteTerm(replace)(term)
}

def substituteVariable(from: hydra.core.Name)(to: hydra.core.Name)(term: hydra.core.Term): hydra.core.Term =
  {
  def replace(recurse: (hydra.core.Term => hydra.core.Term))(term2: hydra.core.Term): hydra.core.Term =
    term2 match
    case hydra.core.Term.variable(v_Term_variable_x) => hydra.core.Term.variable(hydra.lib.logic.ifElse[hydra.core.Name](hydra.lib.equality.equal[hydra.core.Name](v_Term_variable_x)(from))(to)(v_Term_variable_x))
    case hydra.core.Term.function(v_Term_function_v1) => v_Term_function_v1 match
      case hydra.core.Function.lambda(v_Function_lambda_l) => hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Name](v_Function_lambda_l.parameter)(from))(term2)(recurse(term2))
      case _ => recurse(term2)
    case _ => recurse(term2)
  hydra.rewriting.rewriteTerm(replace)(term)
}

def substituteVariables(subst: Map[hydra.core.Name, hydra.core.Name])(term: hydra.core.Term): hydra.core.Term =
  {
  def replace(recurse: (hydra.core.Term => hydra.core.Term))(term2: hydra.core.Term): hydra.core.Term =
    term2 match
    case hydra.core.Term.variable(v_Term_variable_n) => hydra.core.Term.variable(hydra.lib.maybes.fromMaybe[hydra.core.Name](v_Term_variable_n)(hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Name](v_Term_variable_n)(subst)))
    case hydra.core.Term.function(v_Term_function_v1) => v_Term_function_v1 match
      case hydra.core.Function.lambda(v_Function_lambda_l) => hydra.lib.maybes.maybe[hydra.core.Term, hydra.core.Name](recurse(term2))((_x: hydra.core.Name) => term2)(hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Name](v_Function_lambda_l.parameter)(subst))
      case _ => recurse(term2)
    case _ => recurse(term2)
  hydra.rewriting.rewriteTerm(replace)(term)
}

def stripTypeLambdas(t: hydra.core.Term): hydra.core.Term =
  t match
  case hydra.core.Term.annotated(v_Term_annotated_at) => {
    lazy val subj: hydra.core.Term = (v_Term_annotated_at.body)
    {
      lazy val ann: Map[hydra.core.Name, hydra.core.Term] = (v_Term_annotated_at.annotation)
      hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.rewriting.stripTypeLambdas(subj), ann))
    }
  }
  case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => hydra.rewriting.stripTypeLambdas(v_Term_typeLambda_ta.body)
  case _ => t

def subterms(v1: hydra.core.Term): Seq[hydra.core.Term] =
  v1 match
  case hydra.core.Term.annotated(v_Term_annotated_at) => Seq(v_Term_annotated_at.body)
  case hydra.core.Term.application(v_Term_application_p) => Seq(v_Term_application_p.function, (v_Term_application_p.argument))
  case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.either[hydra.core.Term, hydra.core.Term, Seq[hydra.core.Term]]((l: hydra.core.Term) => Seq(l))((r: hydra.core.Term) => Seq(r))(v_Term_either_e)
  case hydra.core.Term.function(v_Term_function_v12) => v_Term_function_v12 match
    case hydra.core.Function.elimination(v_Function_elimination_v13) => v_Function_elimination_v13 match
      case hydra.core.Elimination.union(v_Elimination_union_cs) => hydra.lib.lists.concat2[hydra.core.Term](hydra.lib.maybes.maybe[Seq[hydra.core.Term], hydra.core.Term](Seq())((t: hydra.core.Term) => Seq(t))(v_Elimination_union_cs.default))(hydra.lib.lists.map[hydra.core.Field, hydra.core.Term]((x: hydra.core.Field) => (x.term))(v_Elimination_union_cs.cases))
      case _ => Seq()
    case hydra.core.Function.lambda(v_Function_lambda_l) => Seq(v_Function_lambda_l.body)
    case _ => Seq()
  case hydra.core.Term.let(v_Term_let_lt) => hydra.lib.lists.cons[hydra.core.Term](v_Term_let_lt.body)(hydra.lib.lists.map[hydra.core.Binding, hydra.core.Term]((x: hydra.core.Binding) => (x.term))(v_Term_let_lt.bindings))
  case hydra.core.Term.list(v_Term_list_l) => v_Term_list_l
  case hydra.core.Term.literal => Seq()
  case hydra.core.Term.map(v_Term_map_m) => hydra.lib.lists.concat[hydra.core.Term](hydra.lib.lists.map[Tuple2[hydra.core.Term, hydra.core.Term], Seq[hydra.core.Term]]((p: Tuple2[hydra.core.Term, hydra.core.Term]) =>
    Seq(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](p), hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](p)))(hydra.lib.maps.toList[hydra.core.Term, hydra.core.Term](v_Term_map_m)))
  case hydra.core.Term.maybe(v_Term_maybe_m) => hydra.lib.maybes.maybe[Seq[hydra.core.Term], hydra.core.Term](Seq())((t: hydra.core.Term) => Seq(t))(v_Term_maybe_m)
  case hydra.core.Term.pair(v_Term_pair_p) => Seq(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](v_Term_pair_p), hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](v_Term_pair_p))
  case hydra.core.Term.record(v_Term_record_rt) => hydra.lib.lists.map[hydra.core.Field, hydra.core.Term]((x: hydra.core.Field) => (x.term))(v_Term_record_rt.fields)
  case hydra.core.Term.set(v_Term_set_l) => hydra.lib.sets.toList[hydra.core.Term](v_Term_set_l)
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => Seq(v_Term_typeApplication_ta.body)
  case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => Seq(v_Term_typeLambda_ta.body)
  case hydra.core.Term.union(v_Term_union_ut) => Seq(v_Term_union_ut.field.term)
  case hydra.core.Term.unit => Seq()
  case hydra.core.Term.variable => Seq()
  case hydra.core.Term.wrap(v_Term_wrap_n) => Seq(v_Term_wrap_n.body)

def subtermsWithAccessors(v1: hydra.core.Term): Seq[Tuple2[hydra.accessors.TermAccessor, hydra.core.Term]] =
  v1 match
  case hydra.core.Term.annotated(v_Term_annotated_at) => Seq(Tuple2(hydra.accessors.TermAccessor.annotatedBody, (v_Term_annotated_at.body)))
  case hydra.core.Term.application(v_Term_application_p) => Seq(Tuple2(hydra.accessors.TermAccessor.applicationFunction, (v_Term_application_p.function)), Tuple2(hydra.accessors.TermAccessor.applicationArgument, (v_Term_application_p.argument)))
  case hydra.core.Term.either => Seq()
  case hydra.core.Term.function(v_Term_function_v12) => v_Term_function_v12 match
    case hydra.core.Function.elimination(v_Function_elimination_v13) => v_Function_elimination_v13 match
      case hydra.core.Elimination.union(v_Elimination_union_cs) => hydra.lib.lists.concat2[Tuple2[hydra.accessors.TermAccessor, hydra.core.Term]](hydra.lib.maybes.maybe[Seq[Tuple2[hydra.accessors.TermAccessor, hydra.core.Term]], hydra.core.Term](Seq())((t: hydra.core.Term) =>
        Seq(Tuple2(hydra.accessors.TermAccessor.unionCasesDefault, t)))(v_Elimination_union_cs.default))(hydra.lib.lists.map[hydra.core.Field, Tuple2[hydra.accessors.TermAccessor, hydra.core.Term]]((f: hydra.core.Field) =>
        Tuple2(hydra.accessors.TermAccessor.unionCasesBranch(f.name), (f.term)))(v_Elimination_union_cs.cases))
      case _ => Seq()
    case hydra.core.Function.lambda(v_Function_lambda_l) => Seq(Tuple2(hydra.accessors.TermAccessor.lambdaBody, (v_Function_lambda_l.body)))
    case _ => Seq()
  case hydra.core.Term.let(v_Term_let_lt) => hydra.lib.lists.cons[Tuple2[hydra.accessors.TermAccessor, hydra.core.Term]](Tuple2(hydra.accessors.TermAccessor.letBody, (v_Term_let_lt.body)))(hydra.lib.lists.map[hydra.core.Binding, Tuple2[hydra.accessors.TermAccessor, hydra.core.Term]]((b: hydra.core.Binding) =>
    Tuple2(hydra.accessors.TermAccessor.letBinding(b.name), (b.term)))(v_Term_let_lt.bindings))
  case hydra.core.Term.list(v_Term_list_l) => hydra.lib.lists.map[hydra.core.Term, Tuple2[hydra.accessors.TermAccessor, hydra.core.Term]]((e: hydra.core.Term) => Tuple2(hydra.accessors.TermAccessor.listElement(0), e))(v_Term_list_l)
  case hydra.core.Term.literal => Seq()
  case hydra.core.Term.map(v_Term_map_m) => hydra.lib.lists.concat[Tuple2[hydra.accessors.TermAccessor, hydra.core.Term]](hydra.lib.lists.map[Tuple2[hydra.core.Term, hydra.core.Term], Seq[Tuple2[hydra.accessors.TermAccessor, hydra.core.Term]]]((p: Tuple2[hydra.core.Term, hydra.core.Term]) =>
    Seq(Tuple2(hydra.accessors.TermAccessor.mapKey(0), hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](p)), Tuple2(hydra.accessors.TermAccessor.mapValue(0), hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](p))))(hydra.lib.maps.toList[hydra.core.Term, hydra.core.Term](v_Term_map_m)))
  case hydra.core.Term.maybe(v_Term_maybe_m) => hydra.lib.maybes.maybe[Seq[Tuple2[hydra.accessors.TermAccessor, hydra.core.Term]], hydra.core.Term](Seq())((t: hydra.core.Term) => Seq(Tuple2(hydra.accessors.TermAccessor.maybeTerm, t)))(v_Term_maybe_m)
  case hydra.core.Term.pair => Seq()
  case hydra.core.Term.record(v_Term_record_rt) => hydra.lib.lists.map[hydra.core.Field, Tuple2[hydra.accessors.TermAccessor, hydra.core.Term]]((f: hydra.core.Field) =>
    Tuple2(hydra.accessors.TermAccessor.recordField(f.name), (f.term)))(v_Term_record_rt.fields)
  case hydra.core.Term.set(v_Term_set_s) => hydra.lib.lists.map[hydra.core.Term, Tuple2[hydra.accessors.TermAccessor, hydra.core.Term]]((e: hydra.core.Term) => Tuple2(hydra.accessors.TermAccessor.listElement(0), e))(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_s))
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => Seq(Tuple2(hydra.accessors.TermAccessor.typeApplicationTerm, (v_Term_typeApplication_ta.body)))
  case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => Seq(Tuple2(hydra.accessors.TermAccessor.typeLambdaBody, (v_Term_typeLambda_ta.body)))
  case hydra.core.Term.union(v_Term_union_ut) => Seq(Tuple2(hydra.accessors.TermAccessor.injectionTerm, (v_Term_union_ut.field.term)))
  case hydra.core.Term.unit => Seq()
  case hydra.core.Term.variable => Seq()
  case hydra.core.Term.wrap(v_Term_wrap_n) => Seq(Tuple2(hydra.accessors.TermAccessor.wrappedTerm, (v_Term_wrap_n.body)))

def subtypes(v1: hydra.core.Type): Seq[hydra.core.Type] =
  v1 match
  case hydra.core.Type.annotated(v_Type_annotated_at) => Seq(v_Type_annotated_at.body)
  case hydra.core.Type.application(v_Type_application_at) => Seq(v_Type_application_at.function, (v_Type_application_at.argument))
  case hydra.core.Type.either(v_Type_either_et) => Seq(v_Type_either_et.left, (v_Type_either_et.right))
  case hydra.core.Type.pair(v_Type_pair_pt) => Seq(v_Type_pair_pt.first, (v_Type_pair_pt.second))
  case hydra.core.Type.function(v_Type_function_ft) => Seq(v_Type_function_ft.domain, (v_Type_function_ft.codomain))
  case hydra.core.Type.forall(v_Type_forall_lt) => Seq(v_Type_forall_lt.body)
  case hydra.core.Type.list(v_Type_list_lt) => Seq(v_Type_list_lt)
  case hydra.core.Type.literal => Seq()
  case hydra.core.Type.map(v_Type_map_mt) => Seq(v_Type_map_mt.keys, (v_Type_map_mt.values))
  case hydra.core.Type.maybe(v_Type_maybe_ot) => Seq(v_Type_maybe_ot)
  case hydra.core.Type.record(v_Type_record_rt) => hydra.lib.lists.map[hydra.core.FieldType, hydra.core.Type]((x: hydra.core.FieldType) => (x.`type`))(v_Type_record_rt)
  case hydra.core.Type.set(v_Type_set_st) => Seq(v_Type_set_st)
  case hydra.core.Type.union(v_Type_union_rt) => hydra.lib.lists.map[hydra.core.FieldType, hydra.core.Type]((x: hydra.core.FieldType) => (x.`type`))(v_Type_union_rt)
  case hydra.core.Type.unit => Seq()
  case hydra.core.Type.variable => Seq()
  case hydra.core.Type.void => Seq()
  case hydra.core.Type.wrap(v_Type_wrap_nt) => Seq(v_Type_wrap_nt)

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
      case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
        case hydra.core.Function.primitive(v_Function_primitive_name) => prim(v_Function_primitive_name)
        case hydra.core.Function.elimination(v_Function_elimination_e) => v_Function_elimination_e match
          case hydra.core.Elimination.record(v_Elimination_record_proj) => nominal(v_Elimination_record_proj.typeName)
          case hydra.core.Elimination.union(v_Elimination_union_caseStmt) => nominal(v_Elimination_union_caseStmt.typeName)
          case hydra.core.Elimination.wrap(v_Elimination_wrap_name) => nominal(v_Elimination_wrap_name)
        case _ => names
      case hydra.core.Term.record(v_Term_record_record) => nominal(v_Term_record_record.typeName)
      case hydra.core.Term.union(v_Term_union_injection) => nominal(v_Term_union_injection.typeName)
      case hydra.core.Term.variable(v_Term_variable_name) => `var`(v_Term_variable_name)
      case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => nominal(v_Term_wrap_wrappedTerm.typeName)
      case _ => names
  }
  hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)(addNames)(hydra.lib.sets.empty[hydra.core.Name])(term0)
}

def toShortNames(original: Seq[hydra.core.Name]): Map[hydra.core.Name, hydra.core.Name] =
  {
  def addName(acc: Map[scala.Predef.String, scala.collection.immutable.Set[hydra.core.Name]])(name: hydra.core.Name): Map[scala.Predef.String, scala.collection.immutable.Set[hydra.core.Name]] =
    {
    lazy val local: scala.Predef.String = hydra.names.localNameOf(name)
    lazy val group: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.maybes.fromMaybe[scala.collection.immutable.Set[hydra.core.Name]](hydra.lib.sets.empty[hydra.core.Name])(hydra.lib.maps.lookup[scala.Predef.String, scala.collection.immutable.Set[hydra.core.Name]](local)(acc))
    hydra.lib.maps.insert[scala.Predef.String, scala.collection.immutable.Set[hydra.core.Name]](local)(hydra.lib.sets.insert[hydra.core.Name](name)(group))(acc)
  }
  def groupNamesByLocal(names: Seq[hydra.core.Name]): Map[scala.Predef.String, scala.collection.immutable.Set[hydra.core.Name]] =
    hydra.lib.lists.foldl[Map[scala.Predef.String, scala.collection.immutable.Set[hydra.core.Name]], hydra.core.Name](addName)(hydra.lib.maps.empty[scala.Predef.String, scala.collection.immutable.Set[hydra.core.Name]])(names)
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
  hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Name](hydra.lib.lists.concat[Tuple2[hydra.core.Name, hydra.core.Name]](hydra.lib.lists.map[Tuple2[scala.Predef.String, scala.collection.immutable.Set[hydra.core.Name]], Seq[Tuple2[hydra.core.Name, hydra.core.Name]]](renameGroup)(hydra.lib.maps.toList[scala.Predef.String, scala.collection.immutable.Set[hydra.core.Name]](groups))))
}

def topologicalSortBindingMap(bindingMap: Map[hydra.core.Name, hydra.core.Term]): Seq[Seq[Tuple2[hydra.core.Name, hydra.core.Term]]] =
  {
  lazy val bindings: Seq[Tuple2[hydra.core.Name, hydra.core.Term]] = hydra.lib.maps.toList[hydra.core.Name, hydra.core.Term](bindingMap)
  lazy val keys: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.lists.map[Tuple2[hydra.core.Name, hydra.core.Term], hydra.core.Name](hydra.lib.pairs.first[hydra.core.Name, hydra.core.Term])(bindings))
  def hasTypeAnnotation(term: hydra.core.Term): Boolean =
    term match
    case hydra.core.Term.annotated(v_Term_annotated_at) => hasTypeAnnotation(v_Term_annotated_at.body)
    case _ => false
  def depsOf[T0](nameAndTerm: Tuple2[T0, hydra.core.Term]): Tuple2[T0, Seq[hydra.core.Name]] =
    {
    lazy val name: T0 = hydra.lib.pairs.first[T0, hydra.core.Term](nameAndTerm)
    lazy val term: hydra.core.Term = hydra.lib.pairs.second[T0, hydra.core.Term](nameAndTerm)
    Tuple2(name, hydra.lib.logic.ifElse[Seq[hydra.core.Name]](hasTypeAnnotation(term))(Seq())(hydra.lib.sets.toList[hydra.core.Name](hydra.lib.sets.intersection[hydra.core.Name](keys)(hydra.rewriting.freeVariablesInTerm(term)))))
  }
  def toPair(name: hydra.core.Name): Tuple2[hydra.core.Name, hydra.core.Term] =
    Tuple2(name, hydra.lib.maybes.fromMaybe[hydra.core.Term](hydra.core.Term.literal(hydra.core.Literal.string("Impossible!")))(hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Term](name)(bindingMap)))
  hydra.lib.lists.map[Seq[hydra.core.Name], Seq[Tuple2[hydra.core.Name, hydra.core.Term]]]((v1: Seq[hydra.core.Name]) =>
    hydra.lib.lists.map[hydra.core.Name, Tuple2[hydra.core.Name, hydra.core.Term]](toPair)(v1))(hydra.sorting.topologicalSortComponents(hydra.lib.lists.map[Tuple2[hydra.core.Name, hydra.core.Term], Tuple2[hydra.core.Name, Seq[hydra.core.Name]]](depsOf)(bindings)))
}

def topologicalSortBindings(els: Seq[hydra.core.Binding]): Either[Seq[Seq[hydra.core.Name]], Seq[hydra.core.Name]] =
  {
  def adjlist(e: hydra.core.Binding): Tuple2[hydra.core.Name, Seq[hydra.core.Name]] =
    Tuple2(e.name, hydra.lib.sets.toList[hydra.core.Name](hydra.rewriting.termDependencyNames(false)(true)(true)(e.term)))
  hydra.sorting.topologicalSort(hydra.lib.lists.map[hydra.core.Binding, Tuple2[hydra.core.Name, Seq[hydra.core.Name]]](adjlist)(els))
}

def typeDependencyNames(withSchema: Boolean)(typ: hydra.core.Type): scala.collection.immutable.Set[hydra.core.Name] =
  hydra.lib.logic.ifElse[scala.collection.immutable.Set[hydra.core.Name]](withSchema)(hydra.lib.sets.union[hydra.core.Name](hydra.rewriting.freeVariablesInType(typ))(hydra.rewriting.typeNamesInType(typ)))(hydra.rewriting.freeVariablesInType(typ))

def typeNamesInType[T0](typ0: hydra.core.Type): scala.collection.immutable.Set[T0] =
  {
  def addNames[T1, T2](names: T1)(typ: T2): T1 = names
  hydra.rewriting.foldOverType(hydra.coders.TraversalOrder.pre)(addNames)(hydra.lib.sets.empty[T0])(typ0)
}

def typeSchemeToFType(ts: hydra.core.TypeScheme): hydra.core.Type =
  {
  lazy val vars: Seq[hydra.core.Name] = (ts.variables)
  lazy val body: hydra.core.Type = (ts.`type`)
  hydra.lib.lists.foldl[hydra.core.Type, hydra.core.Name]((t: hydra.core.Type) =>
    (v: hydra.core.Name) => hydra.core.Type.forall(hydra.core.ForallType(v, t)))(body)(hydra.lib.lists.reverse[hydra.core.Name](vars))
}

def unshadowVariables(term0: hydra.core.Term): hydra.core.Term =
  {
  def freshName[T0](base: hydra.core.Name)(i: Int)(m: Map[hydra.core.Name, T0]): hydra.core.Name =
    {
    lazy val candidate: hydra.core.Name = hydra.lib.strings.cat2(base)(hydra.lib.literals.showInt32(i))
    hydra.lib.logic.ifElse[hydra.core.Name](hydra.lib.maps.member[hydra.core.Name, T0](candidate)(m))(freshName(base)(hydra.lib.math.add(i)(1))(m))(candidate)
  }
  def f(recurse: (Map[hydra.core.Name, hydra.core.Name] => hydra.core.Term => hydra.core.Term))(m: Map[hydra.core.Name, hydra.core.Name])(term: hydra.core.Term): hydra.core.Term =
    term match
    case hydra.core.Term.function(v_Term_function_fn) => v_Term_function_fn match
      case hydra.core.Function.lambda(v_Function_lambda_l) => {
        lazy val v: hydra.core.Name = (v_Function_lambda_l.parameter)
        {
          lazy val domain: Option[hydra.core.Type] = (v_Function_lambda_l.domain)
          {
            lazy val body: hydra.core.Term = (v_Function_lambda_l.body)
            hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.maps.member[hydra.core.Name, hydra.core.Name](v)(m))({
              lazy val v2: hydra.core.Name = freshName(v)(2)(m)
              {
                lazy val m2: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.maps.insert[hydra.core.Name, hydra.core.Name](v)(v2)(hydra.lib.maps.insert[hydra.core.Name, hydra.core.Name](v2)(v2)(m))
                hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda(v2, domain, f(recurse)(m2)(body))))
              }
            })(hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda(v, domain, f(recurse)(hydra.lib.maps.insert[hydra.core.Name, hydra.core.Name](v)(v)(m))(body)))))
          }
        }
      }
      case _ => recurse(m)(term)
    case hydra.core.Term.let(v_Term_let_lt) => {
      lazy val m2: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.lists.foldl[Map[hydra.core.Name, hydra.core.Name], hydra.core.Binding]((acc: Map[hydra.core.Name, hydra.core.Name]) =>
        (b: hydra.core.Binding) =>
        {
        lazy val bname: hydra.core.Name = (b.name)
        hydra.lib.logic.ifElse[Map[hydra.core.Name, hydra.core.Name]](hydra.lib.maps.member[hydra.core.Name, hydra.core.Name](bname)(acc))(acc)(hydra.lib.maps.insert[hydra.core.Name, hydra.core.Name](bname)(bname)(acc))
      })(m)(v_Term_let_lt.bindings)
      recurse(m2)(term)
    }
    case hydra.core.Term.variable(v_Term_variable_v) => hydra.core.Term.variable(hydra.lib.maybes.maybe[hydra.core.Name, hydra.core.Name](v_Term_variable_v)((renamed: hydra.core.Name) => renamed)(hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Name](v_Term_variable_v)(m)))
    case _ => recurse(m)(term)
  hydra.rewriting.rewriteTermWithContext(f)(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name])(term0)
}
