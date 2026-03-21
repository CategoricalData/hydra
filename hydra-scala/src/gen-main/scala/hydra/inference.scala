package hydra.inference

import hydra.context.*

import hydra.core.*

import hydra.error.*

import hydra.graph.*

import hydra.typing.*

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

def bindConstraints(flowCx: hydra.context.Context)(cx: hydra.graph.Graph)(constraints: Seq[hydra.typing.TypeConstraint]): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.TypeSubst] =
  eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.TypeSubst, hydra.typing.TypeSubst](eithers.bimap[hydra.context.InContext[hydra.error.UnificationError],
     hydra.typing.TypeSubst, hydra.context.InContext[hydra.error.Error], hydra.typing.TypeSubst]((_ic: hydra.context.InContext[hydra.error.UnificationError]) =>
  hydra.context.InContext(hydra.error.Error.other(_ic.`object`.message), (_ic.context)))((_a: hydra.typing.TypeSubst) => _a)(hydra.unification.unifyTypeConstraints(flowCx)(cx.schemaTypes)(constraints)))((s: hydra.typing.TypeSubst) =>
  eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.TypeSubst, hydra.typing.TypeSubst](hydra.checking.checkTypeSubst(flowCx)(cx)(s))((_x: hydra.typing.TypeSubst) => Right(s)))

def bindUnboundTypeVariables(cx: hydra.graph.Graph)(term0: hydra.core.Term): hydra.core.Term =
  {
  val svars: scala.collection.immutable.Set[hydra.core.Name] = sets.fromList[hydra.core.Name](maps.keys[hydra.core.Name, hydra.core.TypeScheme](cx.schemaTypes))
  def rewrite(recurse: (hydra.core.Term => hydra.core.Term))(term: hydra.core.Term): hydra.core.Term =
    term match
    case hydra.core.Term.let(v_Term_let_l) => {
      def forBinding(b: hydra.core.Binding): hydra.core.Binding =
        {
        val bname: hydra.core.Name = (b.name)
        val bterm: hydra.core.Term = (b.term)
        maybes.maybe[hydra.core.Binding, hydra.core.TypeScheme](hydra.core.Binding(bname, hydra.inference.bindUnboundTypeVariables(cx)(bterm),
           None))((ts: hydra.core.TypeScheme) =>
          {
          val bvars: scala.collection.immutable.Set[hydra.core.Name] = sets.fromList[hydra.core.Name](ts.variables)
          {
            val unboundInType: scala.collection.immutable.Set[hydra.core.Name] = hydra.rewriting.freeVariablesInType(ts.`type`)
            {
              val unboundInTerm: scala.collection.immutable.Set[hydra.core.Name] = hydra.rewriting.freeTypeVariablesInTerm(bterm)
              {
                val unbound: Seq[hydra.core.Name] = sets.toList[hydra.core.Name](sets.difference[hydra.core.Name](sets.union[hydra.core.Name](unboundInType)(unboundInTerm))(sets.union[hydra.core.Name](svars)(bvars)))
                {
                  val ts2: hydra.core.TypeScheme = hydra.core.TypeScheme(lists.concat2[hydra.core.Name](ts.variables)(unbound), (ts.`type`), (ts.constraints))
                  {
                    val bterm2: hydra.core.Term = lists.foldl[hydra.core.Term, hydra.core.Name]((t: hydra.core.Term) =>
                      (v: hydra.core.Name) => hydra.core.Term.typeLambda(hydra.core.TypeLambda(v, t)))(bterm)(unbound)
                    hydra.core.Binding(bname, bterm2, Some(ts2))
                  }
                }
              }
            }
          }
        })(b.`type`)
      }
      hydra.core.Term.let(hydra.core.Let(lists.map[hydra.core.Binding, hydra.core.Binding](forBinding)(v_Term_let_l.bindings),
         hydra.inference.bindUnboundTypeVariables(cx)(v_Term_let_l.body)))
    }
    case _ => recurse(term)
  hydra.rewriting.rewriteTerm(rewrite)(term0)
}

def buildTypeApplicationTerm(tvars: Seq[hydra.core.Name])(body: hydra.core.Term): hydra.core.Term =
  lists.foldl[hydra.core.Term, hydra.core.Name]((t: hydra.core.Term) =>
  (v: hydra.core.Name) =>
  hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(t, hydra.core.Type.variable(v))))(body)(tvars)

def extendContext(pairs: Seq[Tuple2[hydra.core.Name, hydra.core.TypeScheme]])(cx: hydra.graph.Graph): hydra.graph.Graph =
  hydra.graph.Graph(cx.boundTerms, maps.union[hydra.core.Name, hydra.core.TypeScheme](maps.fromList[hydra.core.Name,
     hydra.core.TypeScheme](pairs))(cx.boundTypes), (cx.classConstraints), (cx.lambdaVariables), (cx.metadata),
     (cx.primitives), (cx.schemaTypes), (cx.typeVariables))

def finalizeInferredTerm(flowCx: hydra.context.Context)(cx: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error],
   hydra.core.Term] =
  {
  val term2: hydra.core.Term = hydra.inference.bindUnboundTypeVariables(cx)(term)
  eithers.bind[hydra.context.InContext[hydra.error.Error], Unit, hydra.core.Term](hydra.checking.checkForUnboundTypeVariables(flowCx)(cx)(term2))((_x: Unit) => Right(hydra.rewriting.normalizeTypeVariablesInTerm(term2)))
}

def forInferredTerm[T0](fcx: hydra.context.Context)(cx: hydra.graph.Graph)(term: hydra.core.Term)(desc: scala.Predef.String)(f: (hydra.typing.InferenceResult => T0)): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[T0, hydra.context.Context]] =
  eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult, Tuple2[T0, hydra.context.Context]](hydra.inference.inferTypeOfTerm(fcx)(cx)(term)(desc))((rp: hydra.typing.InferenceResult) => Right(Tuple2(f(rp),
     (rp.context))))

def freeVariablesInContext(cx: hydra.graph.Graph): scala.collection.immutable.Set[hydra.core.Name] =
  lists.foldl[scala.collection.immutable.Set[hydra.core.Name], scala.collection.immutable.Set[hydra.core.Name]](sets.union[hydra.core.Name])(sets.empty[hydra.core.Name])(lists.map[hydra.core.TypeScheme,
     scala.collection.immutable.Set[hydra.core.Name]](hydra.rewriting.freeVariablesInTypeSchemeSimple)(maps.elems[hydra.core.Name,
     hydra.core.TypeScheme](cx.boundTypes)))

def freshVariableType(cx: hydra.context.Context): Tuple2[hydra.core.Type, hydra.context.Context] =
  {
  val result: Tuple2[hydra.core.Name, hydra.context.Context] = hydra.schemas.freshName(cx)
  val name: hydra.core.Name = pairs.first[hydra.core.Name, hydra.context.Context](result)
  val cx2: hydra.context.Context = pairs.second[hydra.core.Name, hydra.context.Context](result)
  Tuple2(hydra.core.Type.variable(name), cx2)
}

def generalize(cx: hydra.graph.Graph)(typ: hydra.core.Type): hydra.core.TypeScheme =
  {
  def isTypeVarName(name: hydra.core.Name): Boolean =
    {
    val parts: Seq[scala.Predef.String] = strings.splitOn(".")(name)
    equality.lte[Int](lists.length[scala.Predef.String](parts))(1)
  }
  val vars: Seq[hydra.core.Name] = lists.nub[hydra.core.Name](lists.filter[hydra.core.Name]((v: hydra.core.Name) =>
    logic.and(hydra.inference.isUnbound(cx)(v))(isTypeVarName(v)))(hydra.rewriting.freeVariablesInTypeOrdered(typ)))
  val allConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = (cx.classConstraints)
  val relevantConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = maps.fromList[hydra.core.Name,
     hydra.core.TypeVariableMetadata](maybes.cat[Tuple2[hydra.core.Name, hydra.core.TypeVariableMetadata]](lists.map[hydra.core.Name,
     Option[Tuple2[hydra.core.Name, hydra.core.TypeVariableMetadata]]]((v: hydra.core.Name) =>
    maybes.map[hydra.core.TypeVariableMetadata, Tuple2[hydra.core.Name, hydra.core.TypeVariableMetadata]]((meta: hydra.core.TypeVariableMetadata) => Tuple2(v,
       meta))(maps.lookup[hydra.core.Name, hydra.core.TypeVariableMetadata](v)(allConstraints)))(vars)))
  val constraintsMaybe: Option[Map[hydra.core.Name, hydra.core.TypeVariableMetadata]] = logic.ifElse[Option[Map[hydra.core.Name,
     hydra.core.TypeVariableMetadata]]](maps.`null`[hydra.core.Name, hydra.core.TypeVariableMetadata](relevantConstraints))(None)(Some(relevantConstraints))
  hydra.core.TypeScheme(vars, typ, constraintsMaybe)
}

def inferGraphTypes(fcx0: hydra.context.Context)(bindings0: Seq[hydra.core.Binding])(g0: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]], hydra.context.Context]] =
  {
  val fcx: hydra.context.Context = hydra.context.Context(lists.cons[scala.Predef.String]("graph inference")(fcx0.trace), (fcx0.messages), (fcx0.other))
  val let0: hydra.core.Let = hydra.core.Let(bindings0, hydra.core.Term.unit)
  def fromLetTerm(l: hydra.core.Let): Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]] =
    {
    val bindings: Seq[hydra.core.Binding] = (l.bindings)
    val prims: Map[hydra.core.Name, hydra.graph.Primitive] = (g0.primitives)
    val schemaTypes: Map[hydra.core.Name, hydra.core.TypeScheme] = (g0.schemaTypes)
    val g: hydra.graph.Graph = hydra.graph.Graph(hydra.lexical.buildGraph(bindings)(maps.empty[hydra.core.Name,
       Option[hydra.core.Term]])(prims).boundTerms, (hydra.lexical.buildGraph(bindings)(maps.empty[hydra.core.Name,
       Option[hydra.core.Term]])(prims).boundTypes), (hydra.lexical.buildGraph(bindings)(maps.empty[hydra.core.Name,
       Option[hydra.core.Term]])(prims).classConstraints), (hydra.lexical.buildGraph(bindings)(maps.empty[hydra.core.Name,
       Option[hydra.core.Term]])(prims).lambdaVariables), (hydra.lexical.buildGraph(bindings)(maps.empty[hydra.core.Name,
       Option[hydra.core.Term]])(prims).metadata), (hydra.lexical.buildGraph(bindings)(maps.empty[hydra.core.Name,
       Option[hydra.core.Term]])(prims).primitives), schemaTypes, (hydra.lexical.buildGraph(bindings)(maps.empty[hydra.core.Name,
       Option[hydra.core.Term]])(prims).typeVariables))
    Tuple2(g, bindings)
  }
  eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult, Tuple2[Tuple2[hydra.graph.Graph,
     Seq[hydra.core.Binding]], hydra.context.Context]](hydra.inference.inferTypeOfTerm(fcx)(g0)(hydra.core.Term.let(let0))("graph term"))((result: hydra.typing.InferenceResult) =>
    {
    val fcx2: hydra.context.Context = (result.context)
    {
      val term: hydra.core.Term = (result.term)
      eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Term, Tuple2[Tuple2[hydra.graph.Graph,
         Seq[hydra.core.Binding]], hydra.context.Context]](hydra.inference.finalizeInferredTerm(fcx2)(g0)(term))((finalized: hydra.core.Term) =>
        finalized match
        case hydra.core.Term.let(v_Term_let_l) => Right(Tuple2(fromLetTerm(v_Term_let_l), fcx2))
        case hydra.core.Term.variable(v_Term_variable__) => Left(hydra.context.InContext(hydra.error.Error.other("Expected inferred graph as let term"), fcx2)))
    }
  })
}

def inferInGraphContext(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] = hydra.inference.inferTypeOfTerm(fcx)(cx)(term)("single term")

def inferMany(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(pairs: Seq[Tuple2[hydra.core.Term, scala.Predef.String]]): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
   hydra.core.TypeVariableMetadata]]]], hydra.context.Context]] =
  logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Tuple2[Tuple2[Seq[hydra.core.Term],
     Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
     hydra.context.Context]]](lists.`null`[Tuple2[hydra.core.Term, scala.Predef.String]](pairs))(Right(Tuple2(Tuple2(Seq(),
     Tuple2(Seq(), Tuple2(hydra.substitution.idTypeSubst, maps.empty[hydra.core.Name, hydra.core.TypeVariableMetadata]))),
     fcx)))({
  val dflt: Either[hydra.context.InContext[hydra.error.Error], Tuple2[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
     Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]], hydra.context.Context]] = {
    val e: hydra.core.Term = pairs.first[hydra.core.Term, scala.Predef.String](lists.head[Tuple2[hydra.core.Term, scala.Predef.String]](pairs))
    {
      val desc: scala.Predef.String = pairs.second[hydra.core.Term, scala.Predef.String](lists.head[Tuple2[hydra.core.Term, scala.Predef.String]](pairs))
      {
        val tl: Seq[Tuple2[hydra.core.Term, scala.Predef.String]] = lists.tail[Tuple2[hydra.core.Term, scala.Predef.String]](pairs)
        eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult, Tuple2[Tuple2[Seq[hydra.core.Term],
           Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
           hydra.context.Context]](hydra.inference.inferTypeOfTerm(fcx)(cx)(e)(desc))((result1: hydra.typing.InferenceResult) =>
          {
          val fcx2: hydra.context.Context = (result1.context)
          {
            val e1: hydra.core.Term = (result1.term)
            {
              val t1: hydra.core.Type = (result1.`type`)
              {
                val s1: hydra.typing.TypeSubst = (result1.subst)
                {
                  val c1: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = (result1.classConstraints)
                  eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Tuple2[Seq[hydra.core.Term],
                     Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                     hydra.core.TypeVariableMetadata]]]], hydra.context.Context], Tuple2[Tuple2[Seq[hydra.core.Term],
                     Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                     hydra.core.TypeVariableMetadata]]]], hydra.context.Context]](hydra.inference.inferMany(fcx2)(hydra.substitution.substInContext(s1)(cx))(tl))((rp2: Tuple2[Tuple2[Seq[hydra.core.Term],
                     Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                     hydra.core.TypeVariableMetadata]]]], hydra.context.Context]) =>
                    {
                    val result2: Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                       Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]] = pairs.first[Tuple2[Seq[hydra.core.Term],
                       Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                       hydra.core.TypeVariableMetadata]]]], hydra.context.Context](rp2)
                    {
                      val fcx3: hydra.context.Context = pairs.second[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
                         Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
                         hydra.context.Context](rp2)
                      {
                        val e2: Seq[hydra.core.Term] = pairs.first[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
                           Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](result2)
                        {
                          val t2: Seq[hydra.core.Type] = pairs.first[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                             Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](pairs.second[Seq[hydra.core.Term],
                             Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                             hydra.core.TypeVariableMetadata]]]](result2))
                          {
                            val s2: hydra.typing.TypeSubst = pairs.first[hydra.typing.TypeSubst, Map[hydra.core.Name,
                               hydra.core.TypeVariableMetadata]](pairs.second[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                               Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](pairs.second[Seq[hydra.core.Term],
                               Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                               hydra.core.TypeVariableMetadata]]]](result2)))
                            {
                              val c2: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = pairs.second[hydra.typing.TypeSubst,
                                 Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](pairs.second[Seq[hydra.core.Type],
                                 Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](pairs.second[Seq[hydra.core.Term],
                                 Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                                 hydra.core.TypeVariableMetadata]]]](result2)))
                              {
                                val c1Subst: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.substitution.substInClassConstraints(s2)(c1)
                                {
                                  val mergedConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.inference.mergeClassConstraints(c1Subst)(c2)
                                  Right(Tuple2(Tuple2(lists.cons[hydra.core.Term](hydra.substitution.substTypesInTerm(s2)(e1))(e2),
                                     Tuple2(lists.cons[hydra.core.Type](hydra.substitution.substInType(s2)(t1))(t2),
                                     Tuple2(hydra.substitution.composeTypeSubst(s1)(s2), mergedConstraints))),
                                     fcx3))
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  })
                }
              }
            }
          }
        })
      }
    }
  }
  dflt
})

def inferTypeOf(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[Tuple2[hydra.core.Term, hydra.core.TypeScheme], hydra.context.Context]] =
  {
  val letTerm: hydra.core.Term = hydra.core.Term.let(hydra.core.Let(Seq(hydra.core.Binding("ignoredVariableName",
     term, None)), hydra.core.Term.literal(hydra.core.Literal.string("ignoredBody"))))
  eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult, Tuple2[Tuple2[hydra.core.Term,
     hydra.core.TypeScheme], hydra.context.Context]](hydra.inference.inferTypeOfTerm(fcx)(cx)(letTerm)("infer type of term"))((result: hydra.typing.InferenceResult) =>
    {
    val fcx2: hydra.context.Context = (result.context)
    eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Term, Tuple2[Tuple2[hydra.core.Term,
       hydra.core.TypeScheme], hydra.context.Context]](hydra.inference.finalizeInferredTerm(fcx2)(cx)(result.term))((finalized: hydra.core.Term) =>
      eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Let, Tuple2[Tuple2[hydra.core.Term,
         hydra.core.TypeScheme], hydra.context.Context]](hydra.extract.core.let(fcx2)(cx)(finalized))((letResult: hydra.core.Let) =>
      {
      val bindings: Seq[hydra.core.Binding] = (letResult.bindings)
      logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Tuple2[Tuple2[hydra.core.Term, hydra.core.TypeScheme],
         hydra.context.Context]]](equality.equal[Int](1)(lists.length[hydra.core.Binding](bindings)))({
        val binding: hydra.core.Binding = lists.head[hydra.core.Binding](bindings)
        {
          val term1: hydra.core.Term = (binding.term)
          {
            val mts: Option[hydra.core.TypeScheme] = (binding.`type`)
            maybes.maybe[Either[hydra.context.InContext[hydra.error.Error], Tuple2[Tuple2[hydra.core.Term,
               hydra.core.TypeScheme], hydra.context.Context]], hydra.core.TypeScheme](Left(hydra.context.InContext(hydra.error.Error.other("Expected a type scheme"),
               fcx2)))((ts: hydra.core.TypeScheme) => Right(Tuple2(Tuple2(term1, ts), fcx2)))(mts)
          }
        }
      })(Left(hydra.context.InContext(hydra.error.Error.other(strings.cat(Seq("Expected a single binding with a type scheme, but got: ",
         literals.showInt32(lists.length[hydra.core.Binding](bindings)), " bindings"))), fcx2)))
    }))
  })
}

def inferTypeOfAnnotatedTerm(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(at: hydra.core.AnnotatedTerm): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] =
  {
  val term: hydra.core.Term = (at.body)
  val ann: Map[hydra.core.Name, hydra.core.Term] = (at.annotation)
  eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult, hydra.typing.InferenceResult](hydra.inference.inferTypeOfTerm(fcx)(cx)(term)("annotated term"))((result: hydra.typing.InferenceResult) =>
    {
    val fcx2: hydra.context.Context = (result.context)
    {
      val iterm: hydra.core.Term = (result.term)
      {
        val itype: hydra.core.Type = (result.`type`)
        {
          val isubst: hydra.typing.TypeSubst = (result.subst)
          {
            val iconstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = (result.classConstraints)
            Right(hydra.typing.InferenceResult(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(iterm, ann)), itype, isubst, iconstraints, fcx2))
          }
        }
      }
    }
  })
}

def inferTypeOfApplication(fcx0: hydra.context.Context)(cx: hydra.graph.Graph)(app: hydra.core.Application): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] =
  {
  val fcx: hydra.context.Context = hydra.context.Context(lists.cons[scala.Predef.String]("application")(fcx0.trace), (fcx0.messages), (fcx0.other))
  val e0: hydra.core.Term = (app.function)
  val e1: hydra.core.Term = (app.argument)
  eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult, hydra.typing.InferenceResult](hydra.inference.inferTypeOfTerm(fcx)(cx)(e0)("lhs"))((lhsResult: hydra.typing.InferenceResult) =>
    {
    val fcx2: hydra.context.Context = (lhsResult.context)
    {
      val a: hydra.core.Term = (lhsResult.term)
      {
        val t0: hydra.core.Type = (lhsResult.`type`)
        {
          val s0: hydra.typing.TypeSubst = (lhsResult.subst)
          {
            val c0: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = (lhsResult.classConstraints)
            eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult, hydra.typing.InferenceResult](hydra.inference.inferTypeOfTerm(fcx2)(hydra.substitution.substInContext(s0)(cx))(e1)("rhs"))((rhsResult: hydra.typing.InferenceResult) =>
              {
              val fcx3: hydra.context.Context = (rhsResult.context)
              {
                val b: hydra.core.Term = (rhsResult.term)
                {
                  val t1: hydra.core.Type = (rhsResult.`type`)
                  {
                    val s1: hydra.typing.TypeSubst = (rhsResult.subst)
                    {
                      val c1: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = (rhsResult.classConstraints)
                      {
                        val vResult: Tuple2[hydra.core.Name, hydra.context.Context] = hydra.schemas.freshName(fcx3)
                        {
                          val v: hydra.core.Name = pairs.first[hydra.core.Name, hydra.context.Context](vResult)
                          {
                            val fcx4: hydra.context.Context = pairs.second[hydra.core.Name, hydra.context.Context](vResult)
                            eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.TypeSubst,
                               hydra.typing.InferenceResult](eithers.bimap[hydra.context.InContext[hydra.error.UnificationError],
                               hydra.typing.TypeSubst, hydra.context.InContext[hydra.error.Error], hydra.typing.TypeSubst]((_ic: hydra.context.InContext[hydra.error.UnificationError]) =>
                              hydra.context.InContext(hydra.error.Error.other(_ic.`object`.message), (_ic.context)))((_a: hydra.typing.TypeSubst) => _a)(hydra.unification.unifyTypes(fcx4)(cx.schemaTypes)(hydra.substitution.substInType(s1)(t0))(hydra.core.Type.function(hydra.core.FunctionType(t1,
                                 hydra.core.Type.variable(v))))("application lhs")))((s2: hydra.typing.TypeSubst) =>
                              eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.TypeSubst,
                                 hydra.typing.InferenceResult](hydra.checking.checkTypeSubst(fcx4)(cx)(s2))((_x: hydra.typing.TypeSubst) =>
                              {
                              val rExpr: hydra.core.Term = hydra.core.Term.application(hydra.core.Application(hydra.substitution.substTypesInTerm(hydra.substitution.composeTypeSubst(s1)(s2))(a),
                                 hydra.substitution.substTypesInTerm(s2)(b)))
                              {
                                val rType: hydra.core.Type = hydra.substitution.substInType(s2)(hydra.core.Type.variable(v))
                                {
                                  val rSubst: hydra.typing.TypeSubst = hydra.substitution.composeTypeSubstList(Seq(s0, s1, s2))
                                  {
                                    val c0Subst: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.substitution.substInClassConstraints(s2)(hydra.substitution.substInClassConstraints(s1)(c0))
                                    {
                                      val c1Subst: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.substitution.substInClassConstraints(s2)(c1)
                                      {
                                        val rConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.inference.mergeClassConstraints(c0Subst)(c1Subst)
                                        Right(hydra.typing.InferenceResult(rExpr, rType, rSubst, rConstraints, fcx4))
                                      }
                                    }
                                  }
                                }
                              }
                            }))
                          }
                        }
                      }
                    }
                  }
                }
              }
            })
          }
        }
      }
    }
  })
}

def inferTypeOfCaseStatement(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(caseStmt: hydra.core.CaseStatement): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] =
  {
  val tname: hydra.core.Name = (caseStmt.typeName)
  val dflt: Option[hydra.core.Term] = (caseStmt.default)
  val cases: Seq[hydra.core.Field] = (caseStmt.cases)
  val fnames: Seq[hydra.core.Name] = lists.map[hydra.core.Field, hydra.core.Name]((x: hydra.core.Field) => (x.name))(cases)
  eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.TypeScheme, hydra.context.Context],
     hydra.typing.InferenceResult](hydra.schemas.requireSchemaType(fcx)(cx.schemaTypes)(tname))((stRp: Tuple2[hydra.core.TypeScheme,
     hydra.context.Context]) =>
    {
    val schemaType: hydra.core.TypeScheme = pairs.first[hydra.core.TypeScheme, hydra.context.Context](stRp)
    {
      val fcx2: hydra.context.Context = pairs.second[hydra.core.TypeScheme, hydra.context.Context](stRp)
      {
        val svars: Seq[hydra.core.Name] = (schemaType.variables)
        {
          val stype: hydra.core.Type = (schemaType.`type`)
          eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.core.FieldType], hydra.typing.InferenceResult](hydra.extract.core.unionType(fcx2)(tname)(stype))((sfields: Seq[hydra.core.FieldType]) =>
            eithers.bind[hydra.context.InContext[hydra.error.Error], Option[hydra.typing.InferenceResult],
               hydra.typing.InferenceResult](eithers.mapMaybe[hydra.core.Term, hydra.typing.InferenceResult,
               hydra.context.InContext[hydra.error.Error]]((t: hydra.core.Term) =>
            hydra.inference.inferTypeOfTerm(fcx2)(cx)(t)(strings.cat(Seq("case ", tname, ".<default>"))))(dflt))((dfltRp: Option[hydra.typing.InferenceResult]) =>
            {
            val dfltResult: Option[hydra.typing.InferenceResult] = dfltRp
            {
              val fcx3: hydra.context.Context = maybes.fromMaybe[hydra.context.Context](fcx2)(maybes.map[hydra.typing.InferenceResult,
                 hydra.context.Context]((x: hydra.typing.InferenceResult) => (x.context))(dfltRp))
              eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Tuple2[Seq[hydra.core.Term],
                 Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
                 hydra.context.Context], hydra.typing.InferenceResult](hydra.inference.inferMany(fcx3)(cx)(lists.map[hydra.core.Field,
                 Tuple2[hydra.core.Term, scala.Predef.String]]((f: hydra.core.Field) =>
                Tuple2(f.term, strings.cat(Seq("case ", tname, ".", (f.name)))))(cases)))((caseRp: Tuple2[Tuple2[Seq[hydra.core.Term],
                   Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
                   hydra.context.Context]) =>
                {
                val caseResults: Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                   Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]] = pairs.first[Tuple2[Seq[hydra.core.Term],
                   Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
                   hydra.context.Context](caseRp)
                {
                  val fcx4: hydra.context.Context = pairs.second[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
                     Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
                     hydra.context.Context](caseRp)
                  {
                    val iterms: Seq[hydra.core.Term] = pairs.first[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
                       Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](caseResults)
                    {
                      val itypes: Seq[hydra.core.Type] = pairs.first[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                         Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](pairs.second[Seq[hydra.core.Term],
                         Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                         hydra.core.TypeVariableMetadata]]]](caseResults))
                      {
                        val isubst: hydra.typing.TypeSubst = pairs.first[hydra.typing.TypeSubst, Map[hydra.core.Name,
                           hydra.core.TypeVariableMetadata]](pairs.second[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                           Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](pairs.second[Seq[hydra.core.Term],
                           Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                           hydra.core.TypeVariableMetadata]]]](caseResults)))
                        {
                          val caseElemConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = pairs.second[hydra.typing.TypeSubst,
                             Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](pairs.second[Seq[hydra.core.Type],
                             Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](pairs.second[Seq[hydra.core.Term],
                             Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                             hydra.core.TypeVariableMetadata]]]](caseResults)))
                          {
                            val codvResult: Tuple2[hydra.core.Name, hydra.context.Context] = hydra.schemas.freshName(fcx4)
                            {
                              val codv: hydra.core.Name = pairs.first[hydra.core.Name, hydra.context.Context](codvResult)
                              {
                                val fcx5: hydra.context.Context = pairs.second[hydra.core.Name, hydra.context.Context](codvResult)
                                {
                                  val cod: hydra.core.Type = hydra.core.Type.variable(codv)
                                  {
                                    val caseMap: Map[hydra.core.Name, hydra.core.Type] = maps.fromList[hydra.core.Name,
                                       hydra.core.Type](lists.map[hydra.core.FieldType, Tuple2[hydra.core.Name,
                                       hydra.core.Type]]((ft: hydra.core.FieldType) => Tuple2(ft.name,
                                       (ft.`type`)))(sfields))
                                    {
                                      val dfltConstraints: Seq[hydra.typing.TypeConstraint] = maybes.toList[hydra.typing.TypeConstraint](maybes.map[hydra.typing.InferenceResult,
                                         hydra.typing.TypeConstraint]((r: hydra.typing.InferenceResult) =>
                                        hydra.typing.TypeConstraint(cod, hydra.substitution.substInType(isubst)(r.`type`), "match default"))(dfltResult))
                                      {
                                        val caseConstraints: Seq[hydra.typing.TypeConstraint] = maybes.cat[hydra.typing.TypeConstraint](lists.zipWith[hydra.core.Name,
                                           hydra.core.Type, Option[hydra.typing.TypeConstraint]]((fname: hydra.core.Name) =>
                                          (itype: hydra.core.Type) =>
                                          maybes.map[hydra.core.Type, hydra.typing.TypeConstraint]((ftype: hydra.core.Type) =>
                                          hydra.typing.TypeConstraint(itype, hydra.core.Type.function(hydra.core.FunctionType(ftype,
                                             cod)), "case type"))(maps.lookup[hydra.core.Name, hydra.core.Type](fname)(caseMap)))(fnames)(itypes))
                                        {
                                          val dfltClassConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = maybes.fromMaybe[Map[hydra.core.Name,
                                             hydra.core.TypeVariableMetadata]](maps.empty[hydra.core.Name,
                                             hydra.core.TypeVariableMetadata])(maybes.map[hydra.typing.InferenceResult,
                                             Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]((x: hydra.typing.InferenceResult) => (x.classConstraints))(dfltResult))
                                          {
                                            val allElemConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.inference.mergeClassConstraints(caseElemConstraints)(dfltClassConstraints)
                                            eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult,
                                               hydra.typing.InferenceResult](hydra.inference.mapConstraints(fcx5)(cx)((subst: hydra.typing.TypeSubst) =>
                                              hydra.inference.yieldWithConstraints(fcx5)(hydra.inference.buildTypeApplicationTerm(svars)(hydra.core.Term.function(hydra.core.Function.elimination(hydra.core.Elimination.union(hydra.core.CaseStatement(tname,
                                                 maybes.map[hydra.typing.InferenceResult, hydra.core.Term]((x: hydra.typing.InferenceResult) => (x.term))(dfltResult),
                                                 lists.zipWith[hydra.core.Name, hydra.core.Term, hydra.core.Field]((n: hydra.core.Name) => (t: hydra.core.Term) => hydra.core.Field(n,
                                                 t))(fnames)(iterms)))))))(hydra.core.Type.function(hydra.core.FunctionType(hydra.schemas.nominalApplication(tname)(lists.map[hydra.core.Name,
                                                 hydra.core.Type]((x: hydra.core.Name) => hydra.core.Type.variable(x))(svars)),
                                                 cod)))(hydra.substitution.composeTypeSubstList(lists.concat[hydra.typing.TypeSubst](Seq(maybes.toList[hydra.typing.TypeSubst](maybes.map[hydra.typing.InferenceResult,
                                                 hydra.typing.TypeSubst]((x: hydra.typing.InferenceResult) => (x.subst))(dfltResult)),
                                                 Seq(isubst, subst)))))(hydra.substitution.substInClassConstraints(subst)(allElemConstraints)))(lists.concat[hydra.typing.TypeConstraint](Seq(dfltConstraints,
                                                 caseConstraints))))((mcResult: hydra.typing.InferenceResult) => Right(mcResult))
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
                }
              })
            }
          }))
        }
      }
    }
  })
}

def inferTypeOfCollection(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(typCons: (hydra.core.Type => hydra.core.Type))(trmCons: (Seq[hydra.core.Term] => hydra.core.Term))(desc: scala.Predef.String)(classNames: scala.collection.immutable.Set[hydra.core.Name])(els: Seq[hydra.core.Term]): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] =
  {
  val varResult: Tuple2[hydra.core.Name, hydra.context.Context] = hydra.schemas.freshName(fcx)
  val `var`: hydra.core.Name = pairs.first[hydra.core.Name, hydra.context.Context](varResult)
  val fcx2: hydra.context.Context = pairs.second[hydra.core.Name, hydra.context.Context](varResult)
  val classConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = logic.ifElse[Map[hydra.core.Name,
     hydra.core.TypeVariableMetadata]](sets.`null`[hydra.core.Name](classNames))(maps.empty[hydra.core.Name,
     hydra.core.TypeVariableMetadata])(maps.singleton[hydra.core.Name, hydra.core.TypeVariableMetadata](`var`)(hydra.core.TypeVariableMetadata(classNames)))
  logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult]](lists.`null`[hydra.core.Term](els))(Right(hydra.inference.yieldWithConstraints(fcx2)(hydra.inference.buildTypeApplicationTerm(Seq(`var`))(trmCons(Seq())))(typCons(hydra.core.Type.variable(`var`)))(hydra.substitution.idTypeSubst)(classConstraints)))(eithers.bind[hydra.context.InContext[hydra.error.Error],
     Tuple2[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
     hydra.core.TypeVariableMetadata]]]], hydra.context.Context], hydra.typing.InferenceResult](hydra.inference.inferMany(fcx2)(cx)(lists.zip[hydra.core.Term,
     scala.Predef.String](els)(lists.map[Int, scala.Predef.String]((i: Int) => strings.cat(Seq("#", literals.showInt32(i))))(math.range(1)(math.add(lists.length[hydra.core.Term](els))(1))))))((resultsRp: Tuple2[Tuple2[Seq[hydra.core.Term],
     Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
     hydra.context.Context]) =>
    {
    val results: Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
       Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]] = pairs.first[Tuple2[Seq[hydra.core.Term],
       Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
       hydra.context.Context](resultsRp)
    {
      val fcx3: hydra.context.Context = pairs.second[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
         Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]], hydra.context.Context](resultsRp)
      {
        val terms: Seq[hydra.core.Term] = pairs.first[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
           Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](results)
        {
          val types: Seq[hydra.core.Type] = pairs.first[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
             Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](pairs.second[Seq[hydra.core.Term],
             Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](results))
          {
            val subst1: hydra.typing.TypeSubst = pairs.first[hydra.typing.TypeSubst, Map[hydra.core.Name,
               hydra.core.TypeVariableMetadata]](pairs.second[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
               Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](pairs.second[Seq[hydra.core.Term],
               Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](results)))
            {
              val elemConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = pairs.second[hydra.typing.TypeSubst,
                 Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](pairs.second[Seq[hydra.core.Type],
                 Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](pairs.second[Seq[hydra.core.Term],
                 Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](results)))
              {
                val constraints: Seq[hydra.typing.TypeConstraint] = lists.map[hydra.core.Type, hydra.typing.TypeConstraint]((t: hydra.core.Type) =>
                  hydra.typing.TypeConstraint(hydra.core.Type.variable(`var`), t, desc))(types)
                {
                  val allConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.inference.mergeClassConstraints(classConstraints)(elemConstraints)
                  eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult,
                     hydra.typing.InferenceResult](hydra.inference.mapConstraints(fcx3)(cx)((subst2: hydra.typing.TypeSubst) =>
                    {
                    val iterm: hydra.core.Term = trmCons(terms)
                    {
                      val itype: hydra.core.Type = typCons(hydra.core.Type.variable(`var`))
                      {
                        val isubst: hydra.typing.TypeSubst = hydra.substitution.composeTypeSubst(subst1)(subst2)
                        hydra.inference.yieldWithConstraints(fcx3)(iterm)(itype)(isubst)(hydra.substitution.substInClassConstraints(subst2)(allConstraints))
                      }
                    }
                  })(constraints))((mcResult: hydra.typing.InferenceResult) => Right(mcResult))
                }
              }
            }
          }
        }
      }
    }
  }))
}

def inferTypeOfEither(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(e: Either[hydra.core.Term, hydra.core.Term]): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] =
  eithers.either[hydra.core.Term, hydra.core.Term, Either[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult]]((l: hydra.core.Term) =>
  eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult, hydra.typing.InferenceResult](hydra.inference.inferTypeOfTerm(fcx)(cx)(l)("either left value"))((r1: hydra.typing.InferenceResult) =>
  {
  val fcx2: hydra.context.Context = (r1.context)
  {
    val iterm: hydra.core.Term = (r1.term)
    {
      val leftType: hydra.core.Type = (r1.`type`)
      {
        val subst: hydra.typing.TypeSubst = (r1.subst)
        {
          val fvResult: Tuple2[hydra.core.Type, hydra.context.Context] = hydra.inference.freshVariableType(fcx2)
          {
            val rightType: hydra.core.Type = pairs.first[hydra.core.Type, hydra.context.Context](fvResult)
            {
              val fcx3: hydra.context.Context = pairs.second[hydra.core.Type, hydra.context.Context](fvResult)
              {
                val eitherTerm: hydra.core.Term = hydra.core.Term.either(Left(iterm))
                {
                  val termWithLeftType: hydra.core.Term = hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(eitherTerm, leftType))
                  {
                    val termWithBothTypes: hydra.core.Term = hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(termWithLeftType, rightType))
                    {
                      val eitherType: hydra.core.Type = hydra.core.Type.either(hydra.core.EitherType(leftType, rightType))
                      Right(hydra.inference.yieldChecked(fcx3)(termWithBothTypes)(eitherType)(subst))
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
}))((r: hydra.core.Term) =>
  eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult, hydra.typing.InferenceResult](hydra.inference.inferTypeOfTerm(fcx)(cx)(r)("either right value"))((r1: hydra.typing.InferenceResult) =>
  {
  val fcx2: hydra.context.Context = (r1.context)
  {
    val iterm: hydra.core.Term = (r1.term)
    {
      val rightType: hydra.core.Type = (r1.`type`)
      {
        val subst: hydra.typing.TypeSubst = (r1.subst)
        {
          val fvResult: Tuple2[hydra.core.Type, hydra.context.Context] = hydra.inference.freshVariableType(fcx2)
          {
            val leftType: hydra.core.Type = pairs.first[hydra.core.Type, hydra.context.Context](fvResult)
            {
              val fcx3: hydra.context.Context = pairs.second[hydra.core.Type, hydra.context.Context](fvResult)
              {
                val eitherTerm: hydra.core.Term = hydra.core.Term.either(Right(iterm))
                {
                  val termWithLeftType: hydra.core.Term = hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(eitherTerm, leftType))
                  {
                    val termWithBothTypes: hydra.core.Term = hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(termWithLeftType, rightType))
                    {
                      val eitherType: hydra.core.Type = hydra.core.Type.either(hydra.core.EitherType(leftType, rightType))
                      Right(hydra.inference.yieldChecked(fcx3)(termWithBothTypes)(eitherType)(subst))
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
}))(e)

def inferTypeOfElimination(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(elm: hydra.core.Elimination): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] =
  elm match
  case hydra.core.Elimination.record(v_Elimination_record_p) => hydra.inference.inferTypeOfProjection(fcx)(cx)(v_Elimination_record_p)
  case hydra.core.Elimination.union(v_Elimination_union_c) => hydra.inference.inferTypeOfCaseStatement(fcx)(cx)(v_Elimination_union_c)
  case hydra.core.Elimination.wrap(v_Elimination_wrap_tname) => hydra.inference.inferTypeOfUnwrap(fcx)(cx)(v_Elimination_wrap_tname)

def inferTypeOfFunction(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(f: hydra.core.Function): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] =
  f match
  case hydra.core.Function.elimination(v_Function_elimination_elm) => hydra.inference.inferTypeOfElimination(fcx)(cx)(v_Function_elimination_elm)
  case hydra.core.Function.lambda(v_Function_lambda_l) => hydra.inference.inferTypeOfLambda(fcx)(cx)(v_Function_lambda_l)
  case hydra.core.Function.primitive(v_Function_primitive_name) => hydra.inference.inferTypeOfPrimitive(fcx)(cx)(v_Function_primitive_name)

def inferTypeOfInjection(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(injection: hydra.core.Injection): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] =
  {
  val tname: hydra.core.Name = (injection.typeName)
  val field: hydra.core.Field = (injection.field)
  val fname: hydra.core.Name = (field.name)
  val term: hydra.core.Term = (field.term)
  eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult, hydra.typing.InferenceResult](hydra.inference.inferTypeOfTerm(fcx)(cx)(term)("injected term"))((result: hydra.typing.InferenceResult) =>
    {
    val fcx2: hydra.context.Context = (result.context)
    eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.TypeScheme, hydra.context.Context],
       hydra.typing.InferenceResult](hydra.schemas.requireSchemaType(fcx2)(cx.schemaTypes)(tname))((stRp: Tuple2[hydra.core.TypeScheme,
       hydra.context.Context]) =>
      {
      val schemaType: hydra.core.TypeScheme = pairs.first[hydra.core.TypeScheme, hydra.context.Context](stRp)
      {
        val fcx3: hydra.context.Context = pairs.second[hydra.core.TypeScheme, hydra.context.Context](stRp)
        {
          val svars: Seq[hydra.core.Name] = (schemaType.variables)
          {
            val stype: hydra.core.Type = (schemaType.`type`)
            {
              val iterm: hydra.core.Term = (result.term)
              {
                val ityp: hydra.core.Type = (result.`type`)
                {
                  val isubst: hydra.typing.TypeSubst = (result.subst)
                  eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.core.FieldType],
                     hydra.typing.InferenceResult](hydra.extract.core.unionType(fcx3)(tname)(stype))((sfields: Seq[hydra.core.FieldType]) =>
                    eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, hydra.typing.InferenceResult](hydra.schemas.findFieldType(fcx3)(fname)(sfields))((ftyp: hydra.core.Type) =>
                    eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult,
                       hydra.typing.InferenceResult](hydra.inference.mapConstraints(fcx3)(cx)((subst: hydra.typing.TypeSubst) =>
                    hydra.inference.`yield`(fcx3)(hydra.inference.buildTypeApplicationTerm(svars)(hydra.core.Term.union(hydra.core.Injection(tname,
                       hydra.core.Field(fname, iterm)))))(hydra.schemas.nominalApplication(tname)(lists.map[hydra.core.Name,
                       hydra.core.Type]((x: hydra.core.Name) => hydra.core.Type.variable(x))(svars)))(hydra.substitution.composeTypeSubst(isubst)(subst)))(Seq(hydra.typing.TypeConstraint(ftyp,
                       ityp, "schema type of injected field"))))((mcResult: hydra.typing.InferenceResult) => Right(mcResult))))
                }
              }
            }
          }
        }
      }
    })
  })
}

def inferTypeOfLambda(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(lambda: hydra.core.Lambda): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] =
  {
  val `var`: hydra.core.Name = (lambda.parameter)
  val body: hydra.core.Term = (lambda.body)
  val vdomResult: Tuple2[hydra.core.Name, hydra.context.Context] = hydra.schemas.freshName(fcx)
  val vdom: hydra.core.Name = pairs.first[hydra.core.Name, hydra.context.Context](vdomResult)
  val fcx2: hydra.context.Context = pairs.second[hydra.core.Name, hydra.context.Context](vdomResult)
  val dom: hydra.core.Type = hydra.core.Type.variable(vdom)
  val cx2: hydra.graph.Graph = hydra.inference.extendContext(Seq(Tuple2(`var`, hydra.core.TypeScheme(Seq(), dom, None))))(cx)
  eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult, hydra.typing.InferenceResult](hydra.inference.inferTypeOfTerm(fcx2)(cx2)(body)("lambda body"))((result: hydra.typing.InferenceResult) =>
    {
    val fcx3: hydra.context.Context = (result.context)
    {
      val iterm: hydra.core.Term = (result.term)
      {
        val icod: hydra.core.Type = (result.`type`)
        {
          val isubst: hydra.typing.TypeSubst = (result.subst)
          {
            val rdom: hydra.core.Type = hydra.substitution.substInType(isubst)(dom)
            {
              val rterm: hydra.core.Term = hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda(`var`, Some(rdom), iterm)))
              {
                val rtype: hydra.core.Type = hydra.core.Type.function(hydra.core.FunctionType(rdom, icod))
                {
                  val vars: scala.collection.immutable.Set[hydra.core.Name] = sets.unions[hydra.core.Name](Seq(hydra.rewriting.freeVariablesInType(rdom),
                     hydra.rewriting.freeVariablesInType(icod), hydra.inference.freeVariablesInContext(hydra.substitution.substInContext(isubst)(cx2))))
                  {
                    val cx3: hydra.graph.Graph = hydra.substitution.substInContext(isubst)(cx)
                    {
                      val iconstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.substitution.substInClassConstraints(isubst)(result.classConstraints)
                      Right(hydra.typing.InferenceResult(rterm, rtype, isubst, iconstraints, fcx3))
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
}

def inferTypeOfLet(fcx0: hydra.context.Context)(cx: hydra.graph.Graph)(let0: hydra.core.Let): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] =
  {
  val fcx: hydra.context.Context = hydra.context.Context(lists.cons[scala.Predef.String]("let")(fcx0.trace), (fcx0.messages), (fcx0.other))
  val bindings0: Seq[hydra.core.Binding] = (let0.bindings)
  val body0: hydra.core.Term = (let0.body)
  val names: Seq[hydra.core.Name] = lists.map[hydra.core.Binding, hydra.core.Name]((x: hydra.core.Binding) => (x.name))(bindings0)
  val nameSet: scala.collection.immutable.Set[hydra.core.Name] = sets.fromList[hydra.core.Name](names)
  def toPair(binding: hydra.core.Binding): Tuple2[hydra.core.Name, Seq[hydra.core.Name]] =
    {
    val name: hydra.core.Name = (binding.name)
    val term: hydra.core.Term = (binding.term)
    Tuple2(name, lists.filter[hydra.core.Name]((n: hydra.core.Name) => sets.member[hydra.core.Name](n)(nameSet))(sets.toList[hydra.core.Name](hydra.rewriting.freeVariablesInTerm(term))))
  }
  val adjList: Seq[Tuple2[hydra.core.Name, Seq[hydra.core.Name]]] = lists.map[hydra.core.Binding, Tuple2[hydra.core.Name,
     Seq[hydra.core.Name]]](toPair)(bindings0)
  val groups: Seq[Seq[hydra.core.Name]] = hydra.sorting.topologicalSortComponents(adjList)
  val bindingMap: Map[hydra.core.Name, hydra.core.Binding] = maps.fromList[hydra.core.Name, hydra.core.Binding](lists.zip[hydra.core.Name,
     hydra.core.Binding](names)(bindings0))
  def createLet(e: hydra.core.Term)(group: Seq[hydra.core.Name]): hydra.core.Term =
    hydra.core.Term.let(hydra.core.Let(maybes.cat[hydra.core.Binding](lists.map[hydra.core.Name, Option[hydra.core.Binding]]((n: hydra.core.Name) =>
    maps.lookup[hydra.core.Name, hydra.core.Binding](n)(bindingMap))(group)), e))
  val rewrittenLet: hydra.core.Term = lists.foldl[hydra.core.Term, Seq[hydra.core.Name]](createLet)(body0)(lists.reverse[Seq[hydra.core.Name]](groups))
  def restoreLet(iterm: hydra.core.Term): hydra.core.Term =
    {
    def helper(level: Int)(bins: Seq[hydra.core.Binding])(term: hydra.core.Term): Tuple2[Seq[hydra.core.Binding], hydra.core.Term] =
      {
      def nonzero(term2: hydra.core.Term): Tuple2[Seq[hydra.core.Binding], hydra.core.Term] =
        term2 match
        case hydra.core.Term.let(v_Term_let_l) => {
          val bs: Seq[hydra.core.Binding] = (v_Term_let_l.bindings)
          {
            val letBody: hydra.core.Term = (v_Term_let_l.body)
            helper(math.sub(level)(1))(lists.concat[hydra.core.Binding](Seq(bs, bins)))(letBody)
          }
        }
      logic.ifElse[Tuple2[Seq[hydra.core.Binding], hydra.core.Term]](equality.equal[Int](level)(0))(Tuple2(bins, term))(nonzero(term))
    }
    val result: Tuple2[Seq[hydra.core.Binding], hydra.core.Term] = helper(lists.length[Seq[hydra.core.Name]](groups))(Seq())(iterm)
    val bindingList: Seq[hydra.core.Binding] = pairs.first[Seq[hydra.core.Binding], hydra.core.Term](result)
    val e: hydra.core.Term = pairs.second[Seq[hydra.core.Binding], hydra.core.Term](result)
    val bindingMap2: Map[hydra.core.Name, hydra.core.Binding] = maps.fromList[hydra.core.Name, hydra.core.Binding](lists.map[hydra.core.Binding,
       Tuple2[hydra.core.Name, hydra.core.Binding]]((b: hydra.core.Binding) => Tuple2(b.name, b))(bindingList))
    hydra.core.Term.let(hydra.core.Let(maybes.cat[hydra.core.Binding](lists.map[hydra.core.Name, Option[hydra.core.Binding]]((n: hydra.core.Name) =>
      maps.lookup[hydra.core.Name, hydra.core.Binding](n)(bindingMap2))(names)), e))
  }
  def rewriteResult(iresult: hydra.typing.InferenceResult): hydra.typing.InferenceResult =
    {
    val fcxR: hydra.context.Context = (iresult.context)
    val iterm: hydra.core.Term = (iresult.term)
    val itype: hydra.core.Type = (iresult.`type`)
    val isubst: hydra.typing.TypeSubst = (iresult.subst)
    val iconstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = (iresult.classConstraints)
    hydra.typing.InferenceResult(restoreLet(iterm), itype, isubst, iconstraints, fcxR)
  }
  val res: Either[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult] = rewrittenLet match
    case hydra.core.Term.let(v_Term_let_l) => hydra.inference.inferTypeOfLetNormalized(fcx)(cx)(v_Term_let_l)
    case _ => hydra.inference.inferTypeOfTerm(fcx)(cx)(rewrittenLet)("empty let term")
  eithers.map[hydra.typing.InferenceResult, hydra.typing.InferenceResult, hydra.context.InContext[hydra.error.Error]](rewriteResult)(res)
}

def inferTypeOfLetNormalized(fcx0: hydra.context.Context)(cx0: hydra.graph.Graph)(letTerm: hydra.core.Let): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] =
  {
  val fcx: hydra.context.Context = hydra.context.Context(lists.cons[scala.Predef.String]("let-normalized")(fcx0.trace), (fcx0.messages), (fcx0.other))
  val bins0: Seq[hydra.core.Binding] = (letTerm.bindings)
  val body0: hydra.core.Term = (letTerm.body)
  val bnames: Seq[hydra.core.Name] = lists.map[hydra.core.Binding, hydra.core.Name]((x: hydra.core.Binding) => (x.name))(bins0)
  val bvarsResult: Tuple2[Seq[hydra.core.Name], hydra.context.Context] = hydra.schemas.freshNames(lists.length[hydra.core.Binding](bins0))(fcx)
  val bvars: Seq[hydra.core.Name] = pairs.first[Seq[hydra.core.Name], hydra.context.Context](bvarsResult)
  val fcx2: hydra.context.Context = pairs.second[Seq[hydra.core.Name], hydra.context.Context](bvarsResult)
  val tbins0: Seq[hydra.core.Type] = lists.map[hydra.core.Name, hydra.core.Type]((x: hydra.core.Name) => hydra.core.Type.variable(x))(bvars)
  val cx1: hydra.graph.Graph = hydra.inference.extendContext(lists.zip[hydra.core.Name, hydra.core.TypeScheme](bnames)(lists.map[hydra.core.Type,
     hydra.core.TypeScheme]((t: hydra.core.Type) => hydra.core.TypeScheme(Seq(), t, None))(tbins0)))(cx0)
  eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
     Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]], hydra.context.Context],
     hydra.typing.InferenceResult](hydra.inference.inferTypesOfTemporaryBindings(fcx2)(cx1)(bins0))((irRp: Tuple2[Tuple2[Seq[hydra.core.Term],
     Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
     hydra.context.Context]) =>
    {
    val inferredResult: Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
       Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]] = pairs.first[Tuple2[Seq[hydra.core.Term],
       Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
       hydra.context.Context](irRp)
    {
      val fcx3: hydra.context.Context = pairs.second[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
         Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]], hydra.context.Context](irRp)
      {
        val bterms1: Seq[hydra.core.Term] = pairs.first[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
           Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](inferredResult)
        {
          val tbins1: Seq[hydra.core.Type] = pairs.first[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
             Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](pairs.second[Seq[hydra.core.Term],
             Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](inferredResult))
          {
            val substAndConstraints: Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]] = pairs.second[Seq[hydra.core.Type],
               Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](pairs.second[Seq[hydra.core.Term],
               Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](inferredResult))
            {
              val s1: hydra.typing.TypeSubst = pairs.first[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](substAndConstraints)
              {
                val inferredConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = pairs.second[hydra.typing.TypeSubst,
                   Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](substAndConstraints)
                eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.TypeSubst, hydra.typing.InferenceResult](eithers.bimap[hydra.context.InContext[hydra.error.UnificationError],
                   hydra.typing.TypeSubst, hydra.context.InContext[hydra.error.Error], hydra.typing.TypeSubst]((_ic: hydra.context.InContext[hydra.error.UnificationError]) =>
                  hydra.context.InContext(hydra.error.Error.other(_ic.`object`.message), (_ic.context)))((_a: hydra.typing.TypeSubst) => _a)(hydra.unification.unifyTypeLists(fcx3)(cx0.schemaTypes)(lists.map[hydra.core.Type,
                     hydra.core.Type]((v1: hydra.core.Type) => hydra.substitution.substInType(s1)(v1))(tbins0))(tbins1)("temporary type bindings")))((s2: hydra.typing.TypeSubst) =>
                  eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.TypeSubst, hydra.typing.InferenceResult](hydra.checking.checkTypeSubst(fcx3)(cx0)(s2))((_x: hydra.typing.TypeSubst) =>
                  {
                  val g2base: hydra.graph.Graph = hydra.substitution.substInContext(hydra.substitution.composeTypeSubst(s1)(s2))(cx0)
                  {
                    val constraintsWithS2: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.substitution.substInClassConstraints(s2)(inferredConstraints)
                    {
                      val composedSubst: hydra.typing.TypeSubst = hydra.substitution.composeTypeSubst(s1)(s2)
                      {
                        val originalBindingConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = lists.foldl[Map[hydra.core.Name,
                           hydra.core.TypeVariableMetadata], hydra.core.Binding]((acc: Map[hydra.core.Name,
                           hydra.core.TypeVariableMetadata]) =>
                          (b: hydra.core.Binding) =>
                          maybes.maybe[Map[hydra.core.Name, hydra.core.TypeVariableMetadata], hydra.core.TypeScheme](acc)((ts: hydra.core.TypeScheme) =>
                          maybes.maybe[Map[hydra.core.Name, hydra.core.TypeVariableMetadata], Map[hydra.core.Name,
                             hydra.core.TypeVariableMetadata]](acc)((c: Map[hydra.core.Name, hydra.core.TypeVariableMetadata]) => hydra.inference.mergeClassConstraints(acc)(c))(ts.constraints))(b.`type`))(maps.empty[hydra.core.Name,
                             hydra.core.TypeVariableMetadata])(bins0)
                        {
                          val originalConstraintsSubst: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.substitution.substInClassConstraints(composedSubst)(originalBindingConstraints)
                          {
                            val allInferredConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.inference.mergeClassConstraints(constraintsWithS2)(originalConstraintsSubst)
                            {
                              val mergedConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.inference.mergeClassConstraints(g2base.classConstraints)(allInferredConstraints)
                              {
                                val g2: hydra.graph.Graph = hydra.graph.Graph(g2base.boundTerms, (g2base.boundTypes),
                                   mergedConstraints, (g2base.lambdaVariables), (g2base.metadata), (g2base.primitives),
                                   (g2base.schemaTypes), (g2base.typeVariables))
                                {
                                  val bterms1Subst: Seq[hydra.core.Term] = lists.map[hydra.core.Term,
                                     hydra.core.Term]((v1: hydra.core.Term) => hydra.substitution.substTypesInTerm(s2)(v1))(bterms1)
                                  {
                                    val tsbins1: Seq[Tuple2[hydra.core.Name, hydra.core.TypeScheme]] = lists.zip[hydra.core.Name,
                                       hydra.core.TypeScheme](bnames)(lists.map[hydra.core.Type, hydra.core.TypeScheme]((t: hydra.core.Type) =>
                                      hydra.inference.generalize(g2)(hydra.substitution.substInType(s2)(t)))(tbins1))
                                    eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult,
                                       hydra.typing.InferenceResult](hydra.inference.inferTypeOfTerm(fcx3)(hydra.inference.extendContext(tsbins1)(g2))(body0)("let body"))((bodyResult: hydra.typing.InferenceResult) =>
                                      {
                                      val fcx4: hydra.context.Context = (bodyResult.context)
                                      {
                                        val body1: hydra.core.Term = (bodyResult.term)
                                        {
                                          val tbody: hydra.core.Type = (bodyResult.`type`)
                                          {
                                            val sbody: hydra.typing.TypeSubst = (bodyResult.subst)
                                            {
                                              val st1: hydra.typing.TermSubst = maps.fromList[hydra.core.Name,
                                                 hydra.core.Term](lists.map[Tuple2[hydra.core.Name, hydra.core.TypeScheme],
                                                 Tuple2[hydra.core.Name, hydra.core.Term]]((pair: Tuple2[hydra.core.Name,
                                                 hydra.core.TypeScheme]) =>
                                                {
                                                val name: hydra.core.Name = pairs.first[hydra.core.Name, hydra.core.TypeScheme](pair)
                                                {
                                                  val ts: hydra.core.TypeScheme = pairs.second[hydra.core.Name, hydra.core.TypeScheme](pair)
                                                  Tuple2(name, hydra.inference.buildTypeApplicationTerm(ts.variables)(hydra.core.Term.variable(name)))
                                                }
                                              })(tsbins1))
                                              {
                                                def createBinding(bindingPair: Tuple2[Tuple2[hydra.core.Name,
                                                   hydra.core.TypeScheme], hydra.core.Term]): hydra.core.Binding =
                                                  {
                                                  val nameTsPair: Tuple2[hydra.core.Name, hydra.core.TypeScheme] = pairs.first[Tuple2[hydra.core.Name,
                                                     hydra.core.TypeScheme], hydra.core.Term](bindingPair)
                                                  val term: hydra.core.Term = pairs.second[Tuple2[hydra.core.Name,
                                                     hydra.core.TypeScheme], hydra.core.Term](bindingPair)
                                                  val name: hydra.core.Name = pairs.first[hydra.core.Name, hydra.core.TypeScheme](nameTsPair)
                                                  val ts: hydra.core.TypeScheme = pairs.second[hydra.core.Name, hydra.core.TypeScheme](nameTsPair)
                                                  val finalTs: hydra.core.TypeScheme = hydra.substitution.substInTypeScheme(sbody)(ts)
                                                  val typeLambdaTerm: hydra.core.Term = lists.foldl[hydra.core.Term, hydra.core.Name]((b: hydra.core.Term) =>
                                                    (v: hydra.core.Name) => hydra.core.Term.typeLambda(hydra.core.TypeLambda(v,
                                                       b)))(hydra.substitution.substituteInTerm(st1)(term))(lists.reverse[hydra.core.Name](finalTs.variables))
                                                  hydra.core.Binding(name, hydra.substitution.substTypesInTerm(hydra.substitution.composeTypeSubst(sbody)(s2))(typeLambdaTerm),
                                                     Some(finalTs))
                                                }
                                                {
                                                  val bins1: Seq[hydra.core.Binding] = lists.map[Tuple2[Tuple2[hydra.core.Name,
                                                     hydra.core.TypeScheme], hydra.core.Term], hydra.core.Binding](createBinding)(lists.zip[Tuple2[hydra.core.Name,
                                                     hydra.core.TypeScheme], hydra.core.Term](tsbins1)(bterms1Subst))
                                                  {
                                                    val bodyConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.substitution.substInClassConstraints(sbody)(bodyResult.classConstraints)
                                                    {
                                                      val bindingConstraintsSubst: Map[hydra.core.Name,
                                                         hydra.core.TypeVariableMetadata] = hydra.substitution.substInClassConstraints(sbody)(constraintsWithS2)
                                                      {
                                                        val allConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.inference.mergeClassConstraints(bindingConstraintsSubst)(bodyConstraints)
                                                        Right(hydra.typing.InferenceResult(hydra.core.Term.let(hydra.core.Let(bins1,
                                                           body1)), tbody, hydra.substitution.composeTypeSubstList(Seq(s1,
                                                           s2, sbody)), allConstraints, fcx4))
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
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }))
              }
            }
          }
        }
      }
    }
  })
}

def inferTypeOfList(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(v1: Seq[hydra.core.Term]): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] =
  hydra.inference.inferTypeOfCollection(fcx)(cx)((x: hydra.core.Type) => hydra.core.Type.list(x))((x: Seq[hydra.core.Term]) => hydra.core.Term.list(x))("list element")(sets.empty[hydra.core.Name])(v1)

def inferTypeOfLiteral(fcx: hydra.context.Context)(lit: hydra.core.Literal): hydra.typing.InferenceResult =
  hydra.typing.InferenceResult(hydra.core.Term.literal(lit), hydra.core.Type.literal(hydra.reflect.literalType(lit)),
     hydra.substitution.idTypeSubst, maps.empty[hydra.core.Name, hydra.core.TypeVariableMetadata], fcx)

def inferTypeOfMap(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(m: Map[hydra.core.Term, hydra.core.Term]): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] =
  {
  val kvarResult: Tuple2[hydra.core.Name, hydra.context.Context] = hydra.schemas.freshName(fcx)
  val kvar: hydra.core.Name = pairs.first[hydra.core.Name, hydra.context.Context](kvarResult)
  val fcx2: hydra.context.Context = pairs.second[hydra.core.Name, hydra.context.Context](kvarResult)
  val vvarResult: Tuple2[hydra.core.Name, hydra.context.Context] = hydra.schemas.freshName(fcx2)
  val vvar: hydra.core.Name = pairs.first[hydra.core.Name, hydra.context.Context](vvarResult)
  val fcx3: hydra.context.Context = pairs.second[hydra.core.Name, hydra.context.Context](vvarResult)
  val keyConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = maps.singleton[hydra.core.Name,
     hydra.core.TypeVariableMetadata](kvar)(hydra.core.TypeVariableMetadata(sets.singleton[hydra.core.Name]("ordering")))
  logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult]](maps.`null`[hydra.core.Term,
     hydra.core.Term](m))(Right(hydra.inference.yieldWithConstraints(fcx3)(hydra.inference.buildTypeApplicationTerm(Seq(kvar,
     vvar))(hydra.core.Term.map(maps.empty[hydra.core.Term, hydra.core.Term])))(hydra.core.Type.map(hydra.core.MapType(hydra.core.Type.variable(kvar),
     hydra.core.Type.variable(vvar))))(hydra.substitution.idTypeSubst)(keyConstraints)))(eithers.bind[hydra.context.InContext[hydra.error.Error],
     Tuple2[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
     hydra.core.TypeVariableMetadata]]]], hydra.context.Context], hydra.typing.InferenceResult](hydra.inference.inferMany(fcx3)(cx)(lists.map[hydra.core.Term,
     Tuple2[hydra.core.Term, scala.Predef.String]]((k: hydra.core.Term) => Tuple2(k, "map key"))(maps.keys[hydra.core.Term,
     hydra.core.Term](m))))((kRp: Tuple2[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
     Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]], hydra.context.Context]) =>
    {
    val kResults: Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
       Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]] = pairs.first[Tuple2[Seq[hydra.core.Term],
       Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
       hydra.context.Context](kRp)
    {
      val fcx4: hydra.context.Context = pairs.second[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
         Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]], hydra.context.Context](kRp)
      {
        val kterms: Seq[hydra.core.Term] = pairs.first[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
           Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](kResults)
        {
          val ktypes: Seq[hydra.core.Type] = pairs.first[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
             Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](pairs.second[Seq[hydra.core.Term],
             Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](kResults))
          {
            val ksubst: hydra.typing.TypeSubst = pairs.first[hydra.typing.TypeSubst, Map[hydra.core.Name,
               hydra.core.TypeVariableMetadata]](pairs.second[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
               Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](pairs.second[Seq[hydra.core.Term],
               Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](kResults)))
            {
              val kElemConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = pairs.second[hydra.typing.TypeSubst,
                 Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](pairs.second[Seq[hydra.core.Type],
                 Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](pairs.second[Seq[hydra.core.Term],
                 Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](kResults)))
              eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Tuple2[Seq[hydra.core.Term],
                 Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
                 hydra.context.Context], hydra.typing.InferenceResult](hydra.inference.inferMany(fcx4)(hydra.substitution.substInContext(ksubst)(cx))(lists.map[hydra.core.Term,
                 Tuple2[hydra.core.Term, scala.Predef.String]]((v: hydra.core.Term) => Tuple2(v, "map value"))(maps.elems[hydra.core.Term,
                 hydra.core.Term](m))))((vRp: Tuple2[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
                 Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
                 hydra.context.Context]) =>
                {
                val vResults: Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                   Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]] = pairs.first[Tuple2[Seq[hydra.core.Term],
                   Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
                   hydra.context.Context](vRp)
                {
                  val fcx5: hydra.context.Context = pairs.second[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
                     Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
                     hydra.context.Context](vRp)
                  {
                    val vterms: Seq[hydra.core.Term] = pairs.first[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
                       Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](vResults)
                    {
                      val vtypes: Seq[hydra.core.Type] = pairs.first[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                         Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](pairs.second[Seq[hydra.core.Term],
                         Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                         hydra.core.TypeVariableMetadata]]]](vResults))
                      {
                        val vsubst: hydra.typing.TypeSubst = pairs.first[hydra.typing.TypeSubst, Map[hydra.core.Name,
                           hydra.core.TypeVariableMetadata]](pairs.second[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                           Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](pairs.second[Seq[hydra.core.Term],
                           Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                           hydra.core.TypeVariableMetadata]]]](vResults)))
                        {
                          val vElemConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = pairs.second[hydra.typing.TypeSubst,
                             Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](pairs.second[Seq[hydra.core.Type],
                             Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](pairs.second[Seq[hydra.core.Term],
                             Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                             hydra.core.TypeVariableMetadata]]]](vResults)))
                          {
                            val kcons: Seq[hydra.typing.TypeConstraint] = lists.map[hydra.core.Type, hydra.typing.TypeConstraint]((t: hydra.core.Type) =>
                              hydra.typing.TypeConstraint(hydra.core.Type.variable(kvar), t, "map key"))(ktypes)
                            {
                              val vcons: Seq[hydra.typing.TypeConstraint] = lists.map[hydra.core.Type, hydra.typing.TypeConstraint]((t: hydra.core.Type) =>
                                hydra.typing.TypeConstraint(hydra.core.Type.variable(vvar), t, "map value"))(vtypes)
                              {
                                val allMapConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.inference.mergeClassConstraints(keyConstraints)(hydra.inference.mergeClassConstraints(kElemConstraints)(vElemConstraints))
                                eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult,
                                   hydra.typing.InferenceResult](hydra.inference.mapConstraints(fcx5)(cx)((subst: hydra.typing.TypeSubst) =>
                                  hydra.inference.yieldWithConstraints(fcx5)(hydra.core.Term.map(maps.fromList[hydra.core.Term,
                                     hydra.core.Term](lists.zip[hydra.core.Term, hydra.core.Term](kterms)(vterms))))(hydra.core.Type.map(hydra.core.MapType(hydra.core.Type.variable(kvar),
                                     hydra.core.Type.variable(vvar))))(hydra.substitution.composeTypeSubstList(Seq(ksubst,
                                     vsubst, subst)))(hydra.substitution.substInClassConstraints(subst)(allMapConstraints)))(lists.concat[hydra.typing.TypeConstraint](Seq(kcons,
                                     vcons))))((mcResult: hydra.typing.InferenceResult) => Right(mcResult))
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              })
            }
          }
        }
      }
    }
  }))
}

def inferTypeOfOptional(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(m: Option[hydra.core.Term]): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] =
  {
  def trmCons(terms: Seq[hydra.core.Term]): hydra.core.Term =
    logic.ifElse[hydra.core.Term](lists.`null`[hydra.core.Term](terms))(hydra.core.Term.maybe(None))(hydra.core.Term.maybe(Some(lists.head[hydra.core.Term](terms))))
  hydra.inference.inferTypeOfCollection(fcx)(cx)((x: hydra.core.Type) => hydra.core.Type.maybe(x))(trmCons)("optional element")(sets.empty[hydra.core.Name])(maybes.maybe[Seq[hydra.core.Term],
     hydra.core.Term](Seq())(lists.singleton[hydra.core.Term])(m))
}

def inferTypeOfPair(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(p: Tuple2[hydra.core.Term, hydra.core.Term]): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] =
  eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
     Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]], hydra.context.Context],
     hydra.typing.InferenceResult](hydra.inference.inferMany(fcx)(cx)(Seq(Tuple2(pairs.first[hydra.core.Term,
     hydra.core.Term](p), "pair first element"), Tuple2(pairs.second[hydra.core.Term, hydra.core.Term](p),
     "pair second element"))))((rp: Tuple2[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
     Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]], hydra.context.Context]) =>
  {
  val results: Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
     Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]] = pairs.first[Tuple2[Seq[hydra.core.Term],
     Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
     hydra.context.Context](rp)
  {
    val fcx2: hydra.context.Context = pairs.second[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
       Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]], hydra.context.Context](rp)
    {
      val iterms: Seq[hydra.core.Term] = pairs.first[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
         Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](results)
      {
        val itypes: Seq[hydra.core.Type] = pairs.first[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
           Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](pairs.second[Seq[hydra.core.Term],
           Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](results))
        {
          val isubst: hydra.typing.TypeSubst = pairs.first[hydra.typing.TypeSubst, Map[hydra.core.Name,
             hydra.core.TypeVariableMetadata]](pairs.second[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
             Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](pairs.second[Seq[hydra.core.Term],
             Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](results)))
          {
            val pairElemConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = pairs.second[hydra.typing.TypeSubst,
               Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](pairs.second[Seq[hydra.core.Type],
               Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](pairs.second[Seq[hydra.core.Term],
               Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](results)))
            {
              val ifst: hydra.core.Term = lists.head[hydra.core.Term](iterms)
              {
                val isnd: hydra.core.Term = lists.head[hydra.core.Term](lists.tail[hydra.core.Term](iterms))
                {
                  val tyFst: hydra.core.Type = lists.head[hydra.core.Type](itypes)
                  {
                    val tySnd: hydra.core.Type = lists.head[hydra.core.Type](lists.tail[hydra.core.Type](itypes))
                    {
                      val pairTerm: hydra.core.Term = hydra.core.Term.pair(Tuple2(ifst, isnd))
                      {
                        val termWithTypes: hydra.core.Term = hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(pairTerm,
                           tyFst)), tySnd))
                        Right(hydra.inference.yieldWithConstraints(fcx2)(termWithTypes)(hydra.core.Type.pair(hydra.core.PairType(tyFst,
                           tySnd)))(isubst)(pairElemConstraints))
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

def inferTypeOfPrimitive(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(name: hydra.core.Name): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] =
  maybes.maybe[Either[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult], hydra.core.TypeScheme](Left(hydra.context.InContext(hydra.error.Error.other(strings.cat2("No such primitive: ")(name)),
     fcx)))((scheme: hydra.core.TypeScheme) =>
  {
  val tsResult: Tuple2[hydra.core.TypeScheme, hydra.context.Context] = hydra.schemas.instantiateTypeScheme(fcx)(scheme)
  {
    val ts: hydra.core.TypeScheme = pairs.first[hydra.core.TypeScheme, hydra.context.Context](tsResult)
    {
      val fcx2: hydra.context.Context = pairs.second[hydra.core.TypeScheme, hydra.context.Context](tsResult)
      {
        val constraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = maybes.fromMaybe[Map[hydra.core.Name,
           hydra.core.TypeVariableMetadata]](maps.empty[hydra.core.Name, hydra.core.TypeVariableMetadata])(ts.constraints)
        Right(hydra.inference.yieldCheckedWithConstraints(fcx2)(hydra.inference.buildTypeApplicationTerm(ts.variables)(hydra.core.Term.function(hydra.core.Function.primitive(name))))(ts.`type`)(hydra.substitution.idTypeSubst)(constraints))
      }
    }
  }
})(maybes.map[hydra.graph.Primitive, hydra.core.TypeScheme]((x: hydra.graph.Primitive) => (x.`type`))(maps.lookup[hydra.core.Name,
   hydra.graph.Primitive](name)(cx.primitives)))

def inferTypeOfProjection(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(proj: hydra.core.Projection): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] =
  {
  val tname: hydra.core.Name = (proj.typeName)
  val fname: hydra.core.Name = (proj.field)
  eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.TypeScheme, hydra.context.Context],
     hydra.typing.InferenceResult](hydra.schemas.requireSchemaType(fcx)(cx.schemaTypes)(tname))((stRp: Tuple2[hydra.core.TypeScheme,
     hydra.context.Context]) =>
    {
    val schemaType: hydra.core.TypeScheme = pairs.first[hydra.core.TypeScheme, hydra.context.Context](stRp)
    {
      val fcx2: hydra.context.Context = pairs.second[hydra.core.TypeScheme, hydra.context.Context](stRp)
      {
        val svars: Seq[hydra.core.Name] = (schemaType.variables)
        {
          val stype: hydra.core.Type = (schemaType.`type`)
          eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.core.FieldType], hydra.typing.InferenceResult](hydra.extract.core.recordType(fcx2)(tname)(stype))((sfields: Seq[hydra.core.FieldType]) =>
            eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, hydra.typing.InferenceResult](hydra.schemas.findFieldType(fcx2)(fname)(sfields))((ftyp: hydra.core.Type) =>
            Right(hydra.inference.`yield`(fcx2)(hydra.inference.buildTypeApplicationTerm(svars)(hydra.core.Term.function(hydra.core.Function.elimination(hydra.core.Elimination.record(hydra.core.Projection(tname,
               fname))))))(hydra.core.Type.function(hydra.core.FunctionType(hydra.schemas.nominalApplication(tname)(lists.map[hydra.core.Name,
               hydra.core.Type]((x: hydra.core.Name) => hydra.core.Type.variable(x))(svars)), ftyp)))(hydra.substitution.idTypeSubst))))
        }
      }
    }
  })
}

def inferTypeOfRecord(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(record: hydra.core.Record): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] =
  {
  val tname: hydra.core.Name = (record.typeName)
  val fields: Seq[hydra.core.Field] = (record.fields)
  val fnames: Seq[hydra.core.Name] = lists.map[hydra.core.Field, hydra.core.Name]((x: hydra.core.Field) => (x.name))(fields)
  eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.TypeScheme, hydra.context.Context],
     hydra.typing.InferenceResult](hydra.schemas.requireSchemaType(fcx)(cx.schemaTypes)(tname))((stRp: Tuple2[hydra.core.TypeScheme,
     hydra.context.Context]) =>
    {
    val schemaType: hydra.core.TypeScheme = pairs.first[hydra.core.TypeScheme, hydra.context.Context](stRp)
    {
      val fcx2: hydra.context.Context = pairs.second[hydra.core.TypeScheme, hydra.context.Context](stRp)
      eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
         Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]], hydra.context.Context],
         hydra.typing.InferenceResult](hydra.inference.inferMany(fcx2)(cx)(lists.map[hydra.core.Field,
         Tuple2[hydra.core.Term, scala.Predef.String]]((f: hydra.core.Field) => Tuple2(f.term, strings.cat2("field ")(f.name)))(fields)))((rp: Tuple2[Tuple2[Seq[hydra.core.Term],
         Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
         hydra.context.Context]) =>
        {
        val results: Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
           Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]] = pairs.first[Tuple2[Seq[hydra.core.Term],
           Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
           hydra.context.Context](rp)
        {
          val fcx3: hydra.context.Context = pairs.second[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
             Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
             hydra.context.Context](rp)
          {
            val svars: Seq[hydra.core.Name] = (schemaType.variables)
            {
              val stype: hydra.core.Type = (schemaType.`type`)
              {
                val iterms: Seq[hydra.core.Term] = pairs.first[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
                   Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](results)
                {
                  val itypes: Seq[hydra.core.Type] = pairs.first[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                     Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](pairs.second[Seq[hydra.core.Term],
                     Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                     hydra.core.TypeVariableMetadata]]]](results))
                  {
                    val isubst: hydra.typing.TypeSubst = pairs.first[hydra.typing.TypeSubst, Map[hydra.core.Name,
                       hydra.core.TypeVariableMetadata]](pairs.second[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                       Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](pairs.second[Seq[hydra.core.Term],
                       Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                       hydra.core.TypeVariableMetadata]]]](results)))
                    {
                      val recElemConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = pairs.second[hydra.typing.TypeSubst,
                         Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](pairs.second[Seq[hydra.core.Type],
                         Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](pairs.second[Seq[hydra.core.Term],
                         Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                         hydra.core.TypeVariableMetadata]]]](results)))
                      {
                        val ityp: hydra.core.Type = hydra.core.Type.record(lists.zipWith[hydra.core.Name,
                           hydra.core.Type, hydra.core.FieldType]((n: hydra.core.Name) => (t: hydra.core.Type) => hydra.core.FieldType(n,
                           t))(fnames)(itypes))
                        eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult,
                           hydra.typing.InferenceResult](hydra.inference.mapConstraints(fcx3)(cx)((subst: hydra.typing.TypeSubst) =>
                          hydra.inference.yieldWithConstraints(fcx3)(hydra.inference.buildTypeApplicationTerm(svars)(hydra.core.Term.record(hydra.core.Record(tname,
                             lists.zipWith[hydra.core.Name, hydra.core.Term, hydra.core.Field]((n: hydra.core.Name) => (t: hydra.core.Term) => hydra.core.Field(n,
                             t))(fnames)(iterms)))))(hydra.schemas.nominalApplication(tname)(lists.map[hydra.core.Name,
                             hydra.core.Type]((x: hydra.core.Name) => hydra.core.Type.variable(x))(svars)))(hydra.substitution.composeTypeSubst(isubst)(subst))(hydra.substitution.substInClassConstraints(subst)(recElemConstraints)))(Seq(hydra.typing.TypeConstraint(stype,
                             ityp, "schema type of record"))))((mcResult: hydra.typing.InferenceResult) => Right(mcResult))
                      }
                    }
                  }
                }
              }
            }
          }
        }
      })
    }
  })
}

def inferTypeOfSet(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(s: scala.collection.immutable.Set[hydra.core.Term]): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] =
  hydra.inference.inferTypeOfCollection(fcx)(cx)((x: hydra.core.Type) => hydra.core.Type.set(x))((terms: Seq[hydra.core.Term]) => hydra.core.Term.set(sets.fromList[hydra.core.Term](terms)))("set element")(sets.singleton[hydra.core.Name]("ordering"))(sets.toList[hydra.core.Term](s))

def inferTypeOfTerm(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(term: hydra.core.Term)(desc: scala.Predef.String): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] =
  {
  val fcx2: hydra.context.Context = hydra.context.Context(lists.cons[scala.Predef.String](desc)(fcx.trace), (fcx.messages), (fcx.other))
  term match
    case hydra.core.Term.annotated(v_Term_annotated_a) => hydra.inference.inferTypeOfAnnotatedTerm(fcx2)(cx)(v_Term_annotated_a)
    case hydra.core.Term.application(v_Term_application_a) => hydra.inference.inferTypeOfApplication(fcx2)(cx)(v_Term_application_a)
    case hydra.core.Term.either(v_Term_either_e) => hydra.inference.inferTypeOfEither(fcx2)(cx)(v_Term_either_e)
    case hydra.core.Term.function(v_Term_function_f) => hydra.inference.inferTypeOfFunction(fcx2)(cx)(v_Term_function_f)
    case hydra.core.Term.let(v_Term_let_l) => hydra.inference.inferTypeOfLet(fcx2)(cx)(v_Term_let_l)
    case hydra.core.Term.list(v_Term_list_els) => hydra.inference.inferTypeOfList(fcx2)(cx)(v_Term_list_els)
    case hydra.core.Term.literal(v_Term_literal_l) => Right(hydra.inference.inferTypeOfLiteral(fcx2)(v_Term_literal_l))
    case hydra.core.Term.map(v_Term_map_m) => hydra.inference.inferTypeOfMap(fcx2)(cx)(v_Term_map_m)
    case hydra.core.Term.maybe(v_Term_maybe_m) => hydra.inference.inferTypeOfOptional(fcx2)(cx)(v_Term_maybe_m)
    case hydra.core.Term.pair(v_Term_pair_p) => hydra.inference.inferTypeOfPair(fcx2)(cx)(v_Term_pair_p)
    case hydra.core.Term.record(v_Term_record_r) => hydra.inference.inferTypeOfRecord(fcx2)(cx)(v_Term_record_r)
    case hydra.core.Term.set(v_Term_set_s) => hydra.inference.inferTypeOfSet(fcx2)(cx)(v_Term_set_s)
    case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => hydra.inference.inferTypeOfTypeApplication(fcx2)(cx)(v_Term_typeApplication_tt)
    case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => hydra.inference.inferTypeOfTypeLambda(fcx2)(cx)(v_Term_typeLambda_ta)
    case hydra.core.Term.union(v_Term_union_i) => hydra.inference.inferTypeOfInjection(fcx2)(cx)(v_Term_union_i)
    case hydra.core.Term.unit => Right(hydra.inference.inferTypeOfUnit(fcx2))
    case hydra.core.Term.variable(v_Term_variable_name) => hydra.inference.inferTypeOfVariable(fcx2)(cx)(v_Term_variable_name)
    case hydra.core.Term.wrap(v_Term_wrap_w) => hydra.inference.inferTypeOfWrappedTerm(fcx2)(cx)(v_Term_wrap_w)
}

def inferTypeOfTypeLambda(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(ta: hydra.core.TypeLambda): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] = hydra.inference.inferTypeOfTerm(fcx)(cx)(ta.body)("type abstraction")

def inferTypeOfTypeApplication(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(tt: hydra.core.TypeApplicationTerm): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] = hydra.inference.inferTypeOfTerm(fcx)(cx)(tt.body)("type application term")

def inferTypeOfUnit(fcx: hydra.context.Context): hydra.typing.InferenceResult =
  hydra.typing.InferenceResult(hydra.core.Term.unit, hydra.core.Type.unit, hydra.substitution.idTypeSubst,
     maps.empty[hydra.core.Name, hydra.core.TypeVariableMetadata], fcx)

def inferTypeOfUnwrap(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(tname: hydra.core.Name): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] =
  eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.TypeScheme, hydra.context.Context],
     hydra.typing.InferenceResult](hydra.schemas.requireSchemaType(fcx)(cx.schemaTypes)(tname))((stRp: Tuple2[hydra.core.TypeScheme,
     hydra.context.Context]) =>
  {
  val schemaType: hydra.core.TypeScheme = pairs.first[hydra.core.TypeScheme, hydra.context.Context](stRp)
  {
    val fcx2: hydra.context.Context = pairs.second[hydra.core.TypeScheme, hydra.context.Context](stRp)
    {
      val svars: Seq[hydra.core.Name] = (schemaType.variables)
      {
        val stype: hydra.core.Type = (schemaType.`type`)
        eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, hydra.typing.InferenceResult](hydra.extract.core.wrappedType(fcx2)(tname)(stype))((wtyp: hydra.core.Type) =>
          Right(hydra.inference.`yield`(fcx2)(hydra.inference.buildTypeApplicationTerm(svars)(hydra.core.Term.function(hydra.core.Function.elimination(hydra.core.Elimination.wrap(tname)))))(hydra.core.Type.function(hydra.core.FunctionType(hydra.schemas.nominalApplication(tname)(lists.map[hydra.core.Name,
             hydra.core.Type]((x: hydra.core.Name) => hydra.core.Type.variable(x))(svars)), wtyp)))(hydra.substitution.idTypeSubst)))
      }
    }
  }
})

def inferTypeOfVariable(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(name: hydra.core.Name): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] =
  maybes.maybe[Either[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult], hydra.core.TypeScheme](Left(hydra.context.InContext(hydra.error.Error.other(strings.cat2("Variable not bound to type: ")(name)),
     fcx)))((scheme: hydra.core.TypeScheme) =>
  {
  val tsResult: Tuple2[hydra.core.TypeScheme, hydra.context.Context] = hydra.schemas.instantiateTypeScheme(fcx)(scheme)
  {
    val ts: hydra.core.TypeScheme = pairs.first[hydra.core.TypeScheme, hydra.context.Context](tsResult)
    {
      val fcx2: hydra.context.Context = pairs.second[hydra.core.TypeScheme, hydra.context.Context](tsResult)
      {
        val constraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = maybes.fromMaybe[Map[hydra.core.Name,
           hydra.core.TypeVariableMetadata]](maps.empty[hydra.core.Name, hydra.core.TypeVariableMetadata])(ts.constraints)
        Right(hydra.typing.InferenceResult(hydra.inference.buildTypeApplicationTerm(ts.variables)(hydra.core.Term.variable(name)),
           (ts.`type`), hydra.substitution.idTypeSubst, constraints, fcx2))
      }
    }
  }
})(maps.lookup[hydra.core.Name, hydra.core.TypeScheme](name)(cx.boundTypes))

def inferTypeOfWrappedTerm(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(wt: hydra.core.WrappedTerm): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] =
  {
  val tname: hydra.core.Name = (wt.typeName)
  val term: hydra.core.Term = (wt.body)
  eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.TypeScheme, hydra.context.Context],
     hydra.typing.InferenceResult](hydra.schemas.requireSchemaType(fcx)(cx.schemaTypes)(tname))((stRp: Tuple2[hydra.core.TypeScheme,
     hydra.context.Context]) =>
    {
    val schemaType: hydra.core.TypeScheme = pairs.first[hydra.core.TypeScheme, hydra.context.Context](stRp)
    {
      val fcx2: hydra.context.Context = pairs.second[hydra.core.TypeScheme, hydra.context.Context](stRp)
      eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult, hydra.typing.InferenceResult](hydra.inference.inferTypeOfTerm(fcx2)(cx)(term)("wrapped term"))((result: hydra.typing.InferenceResult) =>
        {
        val fcx3: hydra.context.Context = (result.context)
        {
          val svars: Seq[hydra.core.Name] = (schemaType.variables)
          {
            val stype: hydra.core.Type = (schemaType.`type`)
            {
              val iterm: hydra.core.Term = (result.term)
              {
                val itype: hydra.core.Type = (result.`type`)
                {
                  val isubst: hydra.typing.TypeSubst = (result.subst)
                  {
                    val ityp: hydra.core.Type = hydra.core.Type.wrap(itype)
                    eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult,
                       hydra.typing.InferenceResult](hydra.inference.mapConstraints(fcx3)(cx)((subst: hydra.typing.TypeSubst) =>
                      hydra.inference.`yield`(fcx3)(hydra.inference.buildTypeApplicationTerm(svars)(hydra.core.Term.wrap(hydra.core.WrappedTerm(tname,
                         iterm))))(hydra.schemas.nominalApplication(tname)(lists.map[hydra.core.Name,
                         hydra.core.Type]((x: hydra.core.Name) => hydra.core.Type.variable(x))(svars)))(hydra.substitution.composeTypeSubst(isubst)(subst)))(Seq(hydra.typing.TypeConstraint(stype,
                         ityp, "schema type of wrapper"))))((mcResult: hydra.typing.InferenceResult) => Right(mcResult))
                  }
                }
              }
            }
          }
        }
      })
    }
  })
}

def inferTypesOfTemporaryBindings(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(bins: Seq[hydra.core.Binding]): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
   hydra.core.TypeVariableMetadata]]]], hydra.context.Context]] =
  logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Tuple2[Tuple2[Seq[hydra.core.Term],
     Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
     hydra.context.Context]]](lists.`null`[hydra.core.Binding](bins))(Right(Tuple2(Tuple2(Seq(), Tuple2(Seq(),
     Tuple2(hydra.substitution.idTypeSubst, maps.empty[hydra.core.Name, hydra.core.TypeVariableMetadata]))),
     fcx)))({
  val dflt: Either[hydra.context.InContext[hydra.error.Error], Tuple2[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
     Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]], hydra.context.Context]] = {
    val binding: hydra.core.Binding = lists.head[hydra.core.Binding](bins)
    {
      val k: hydra.core.Name = (binding.name)
      {
        val v: hydra.core.Term = (binding.term)
        {
          val tl: Seq[hydra.core.Binding] = lists.tail[hydra.core.Binding](bins)
          eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.InferenceResult, Tuple2[Tuple2[Seq[hydra.core.Term],
             Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
             hydra.context.Context]](hydra.inference.inferTypeOfTerm(fcx)(cx)(v)(strings.cat(Seq("temporary let binding '",
             k, "'"))))((result1: hydra.typing.InferenceResult) =>
            {
            val fcx2: hydra.context.Context = (result1.context)
            {
              val j: hydra.core.Term = (result1.term)
              {
                val u_prime: hydra.core.Type = (result1.`type`)
                {
                  val u: hydra.typing.TypeSubst = (result1.subst)
                  {
                    val c1Inferred: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = (result1.classConstraints)
                    eithers.bind[hydra.context.InContext[hydra.error.Error], Map[hydra.core.Name, hydra.core.TypeVariableMetadata],
                       Tuple2[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                       Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]], hydra.context.Context]](maybes.maybe[Either[hydra.context.InContext[hydra.error.Error],
                       Map[hydra.core.Name, hydra.core.TypeVariableMetadata]], hydra.core.TypeScheme](Right(maps.empty[hydra.core.Name,
                       hydra.core.TypeVariableMetadata]))((ts: hydra.core.TypeScheme) =>
                      {
                      val tsResult: Tuple2[hydra.core.TypeScheme, hydra.context.Context] = hydra.schemas.instantiateTypeScheme(fcx2)(ts)
                      {
                        val instantiatedTs: hydra.core.TypeScheme = pairs.first[hydra.core.TypeScheme, hydra.context.Context](tsResult)
                        {
                          val freshConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = maybes.fromMaybe[Map[hydra.core.Name,
                             hydra.core.TypeVariableMetadata]](maps.empty[hydra.core.Name, hydra.core.TypeVariableMetadata])(instantiatedTs.constraints)
                          eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.TypeSubst,
                             Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](eithers.bimap[hydra.context.InContext[hydra.error.UnificationError],
                             hydra.typing.TypeSubst, hydra.context.InContext[hydra.error.Error], hydra.typing.TypeSubst]((_ic: hydra.context.InContext[hydra.error.UnificationError]) =>
                            hydra.context.InContext(hydra.error.Error.other(_ic.`object`.message), (_ic.context)))((_a: hydra.typing.TypeSubst) => _a)(hydra.unification.unifyTypes(fcx2)(cx.schemaTypes)(instantiatedTs.`type`)(u_prime)("original binding type")))((unifySubst: hydra.typing.TypeSubst) =>
                            Right(hydra.substitution.substInClassConstraints(unifySubst)(freshConstraints)))
                        }
                      }
                    })(binding.`type`))((originalBindingConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata]) =>
                      {
                      val c1: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.inference.mergeClassConstraints(c1Inferred)(originalBindingConstraints)
                      eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Tuple2[Seq[hydra.core.Term],
                         Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                         hydra.core.TypeVariableMetadata]]]], hydra.context.Context], Tuple2[Tuple2[Seq[hydra.core.Term],
                         Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                         hydra.core.TypeVariableMetadata]]]], hydra.context.Context]](hydra.inference.inferTypesOfTemporaryBindings(fcx2)(hydra.substitution.substInContext(u)(cx))(tl))((rp2: Tuple2[Tuple2[Seq[hydra.core.Term],
                         Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                         hydra.core.TypeVariableMetadata]]]], hydra.context.Context]) =>
                        {
                        val result2: Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                           Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]] = pairs.first[Tuple2[Seq[hydra.core.Term],
                           Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                           hydra.core.TypeVariableMetadata]]]], hydra.context.Context](rp2)
                        {
                          val fcx3: hydra.context.Context = pairs.second[Tuple2[Seq[hydra.core.Term],
                             Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                             hydra.core.TypeVariableMetadata]]]], hydra.context.Context](rp2)
                          {
                            val h: Seq[hydra.core.Term] = pairs.first[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
                               Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](result2)
                            {
                              val r_prime: Seq[hydra.core.Type] = pairs.first[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                                 Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](pairs.second[Seq[hydra.core.Term],
                                 Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                                 hydra.core.TypeVariableMetadata]]]](result2))
                              {
                                val restPair: Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]] = pairs.second[Seq[hydra.core.Type],
                                   Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](pairs.second[Seq[hydra.core.Term],
                                   Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                                   hydra.core.TypeVariableMetadata]]]](result2))
                                {
                                  val r: hydra.typing.TypeSubst = pairs.first[hydra.typing.TypeSubst,
                                     Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](restPair)
                                  {
                                    val c2: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = pairs.second[hydra.typing.TypeSubst,
                                       Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](restPair)
                                    {
                                      val c1Subst: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.substitution.substInClassConstraints(r)(c1)
                                      {
                                        val mergedConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.inference.mergeClassConstraints(c1Subst)(c2)
                                        Right(Tuple2(Tuple2(lists.cons[hydra.core.Term](hydra.substitution.substTypesInTerm(r)(j))(h),
                                           Tuple2(lists.cons[hydra.core.Type](hydra.substitution.substInType(r)(u_prime))(r_prime),
                                           Tuple2(hydra.substitution.composeTypeSubst(u)(r), mergedConstraints))),
                                           fcx3))
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      })
                    })
                  }
                }
              }
            }
          })
        }
      }
    }
  }
  dflt
})

def isUnbound(cx: hydra.graph.Graph)(v: hydra.core.Name): Boolean =
  logic.and(logic.not(sets.member[hydra.core.Name](v)(hydra.inference.freeVariablesInContext(cx))))(logic.not(maps.member[hydra.core.Name,
     hydra.core.TypeScheme](v)(cx.schemaTypes)))

def mapConstraints[T0](flowCx: hydra.context.Context)(cx: hydra.graph.Graph)(f: (hydra.typing.TypeSubst => T0))(constraints: Seq[hydra.typing.TypeConstraint]): Either[hydra.context.InContext[hydra.error.Error],
   T0] =
  eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.TypeSubst, T0](eithers.bimap[hydra.context.InContext[hydra.error.UnificationError],
     hydra.typing.TypeSubst, hydra.context.InContext[hydra.error.Error], hydra.typing.TypeSubst]((_ic: hydra.context.InContext[hydra.error.UnificationError]) =>
  hydra.context.InContext(hydra.error.Error.other(_ic.`object`.message), (_ic.context)))((_a: hydra.typing.TypeSubst) => _a)(hydra.unification.unifyTypeConstraints(flowCx)(cx.schemaTypes)(constraints)))((s: hydra.typing.TypeSubst) =>
  eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.TypeSubst, T0](hydra.checking.checkTypeSubst(flowCx)(cx)(s))((_x: hydra.typing.TypeSubst) => Right(f(s))))

def mergeClassConstraints[T0](m1: Map[T0, hydra.core.TypeVariableMetadata])(m2: Map[T0, hydra.core.TypeVariableMetadata]): Map[T0,
   hydra.core.TypeVariableMetadata] =
  lists.foldl[Map[T0, hydra.core.TypeVariableMetadata], Tuple2[T0, hydra.core.TypeVariableMetadata]]((acc: Map[T0, hydra.core.TypeVariableMetadata]) =>
  (pair: Tuple2[T0, hydra.core.TypeVariableMetadata]) =>
  {
  val k: T0 = pairs.first[T0, hydra.core.TypeVariableMetadata](pair)
  {
    val v: hydra.core.TypeVariableMetadata = pairs.second[T0, hydra.core.TypeVariableMetadata](pair)
    maybes.maybe[Map[T0, hydra.core.TypeVariableMetadata], hydra.core.TypeVariableMetadata](maps.insert[T0,
       hydra.core.TypeVariableMetadata](k)(v)(acc))((existing: hydra.core.TypeVariableMetadata) =>
      {
      val merged: hydra.core.TypeVariableMetadata = hydra.core.TypeVariableMetadata(sets.union[hydra.core.Name](existing.classes)(v.classes))
      maps.insert[T0, hydra.core.TypeVariableMetadata](k)(merged)(acc)
    })(maps.lookup[T0, hydra.core.TypeVariableMetadata](k)(acc))
  }
})(m1)(maps.toList[T0, hydra.core.TypeVariableMetadata](m2))

def showInferenceResult(result: hydra.typing.InferenceResult): scala.Predef.String =
  {
  val term: hydra.core.Term = (result.term)
  val typ: hydra.core.Type = (result.`type`)
  val subst: hydra.typing.TypeSubst = (result.subst)
  strings.cat(Seq("{term=", hydra.show.core.term(term), ", type=", hydra.show.core.`type`(typ), ", subst=", hydra.show.typing.typeSubst(subst), "}"))
}

def `yield`(fcx: hydra.context.Context)(term: hydra.core.Term)(typ: hydra.core.Type)(subst: hydra.typing.TypeSubst): hydra.typing.InferenceResult =
  hydra.typing.InferenceResult(hydra.substitution.substTypesInTerm(subst)(term), hydra.substitution.substInType(subst)(typ),
     subst, maps.empty[hydra.core.Name, hydra.core.TypeVariableMetadata], fcx)

def yieldChecked(fcx: hydra.context.Context)(term: hydra.core.Term)(typ: hydra.core.Type)(subst: hydra.typing.TypeSubst): hydra.typing.InferenceResult =
  {
  val iterm: hydra.core.Term = hydra.substitution.substTypesInTerm(subst)(term)
  val itype: hydra.core.Type = hydra.substitution.substInType(subst)(typ)
  hydra.typing.InferenceResult(iterm, itype, subst, maps.empty[hydra.core.Name, hydra.core.TypeVariableMetadata], fcx)
}

def yieldCheckedWithConstraints(fcx: hydra.context.Context)(term: hydra.core.Term)(typ: hydra.core.Type)(subst: hydra.typing.TypeSubst)(constraints: Map[hydra.core.Name,
   hydra.core.TypeVariableMetadata]): hydra.typing.InferenceResult =
  {
  val iterm: hydra.core.Term = hydra.substitution.substTypesInTerm(subst)(term)
  val itype: hydra.core.Type = hydra.substitution.substInType(subst)(typ)
  val iconstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.substitution.substInClassConstraints(subst)(constraints)
  hydra.typing.InferenceResult(iterm, itype, subst, iconstraints, fcx)
}

def yieldDebug[T0](fcx: hydra.context.Context)(cx: T0)(debugId: scala.Predef.String)(term: hydra.core.Term)(typ: hydra.core.Type)(subst: hydra.typing.TypeSubst): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.InferenceResult] =
  {
  val rterm: hydra.core.Term = hydra.substitution.substTypesInTerm(subst)(term)
  val rtyp: hydra.core.Type = hydra.substitution.substInType(subst)(typ)
  eithers.bind[hydra.context.InContext[hydra.error.Error], Unit, hydra.typing.InferenceResult](hydra.annotations.debugIf(fcx)(debugId)(strings.cat(Seq("\n\tterm: ",
     hydra.show.core.term(term), "\n\ttyp: ", hydra.show.core.`type`(typ), "\n\tsubst: ", hydra.show.typing.typeSubst(subst),
     "\n\trterm: ", hydra.show.core.term(rterm), "\n\trtyp: ", hydra.show.core.`type`(rtyp)))))((result: Unit) =>
    Right(hydra.typing.InferenceResult(rterm, rtyp, subst, maps.empty[hydra.core.Name, hydra.core.TypeVariableMetadata], fcx)))
}

def yieldWithConstraints(fcx: hydra.context.Context)(term: hydra.core.Term)(typ: hydra.core.Type)(subst: hydra.typing.TypeSubst)(constraints: Map[hydra.core.Name,
   hydra.core.TypeVariableMetadata]): hydra.typing.InferenceResult =
  hydra.typing.InferenceResult(hydra.substitution.substTypesInTerm(subst)(term), hydra.substitution.substInType(subst)(typ), subst, constraints, fcx)
