package hydra.inference

import hydra.context.*

import hydra.core.*

import hydra.errors.*

import hydra.graph.*

import hydra.typing.*

def bindConstraints[T0](flowCx: T0)(cx: hydra.graph.Graph)(constraints: Seq[hydra.typing.TypeConstraint]): Either[hydra.errors.Error,
   hydra.typing.TypeSubst] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.TypeSubst, hydra.typing.TypeSubst](hydra.lib.eithers.bimap[hydra.errors.UnificationError,
     hydra.typing.TypeSubst, hydra.errors.Error, hydra.typing.TypeSubst]((_e: hydra.errors.UnificationError) => hydra.errors.Error.unification(_e))((_a: hydra.typing.TypeSubst) => _a)(hydra.unification.unifyTypeConstraints(flowCx)(cx.schemaTypes)(constraints)))((s: hydra.typing.TypeSubst) =>
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.TypeSubst, hydra.typing.TypeSubst](hydra.checking.checkTypeSubst(flowCx)(cx)(s))((_x: hydra.typing.TypeSubst) => Right(s)))

def bindUnboundTypeVariables(cx: hydra.graph.Graph)(term0: hydra.core.Term): hydra.core.Term =
  {
  lazy val svars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.maps.keys[hydra.core.Name,
     hydra.core.TypeScheme](cx.schemaTypes))
  def rewrite(recurse: (hydra.core.Term => hydra.core.Term))(term: hydra.core.Term): hydra.core.Term =
    term match
    case hydra.core.Term.let(v_Term_let_l) => {
      def forBinding(b: hydra.core.Binding): hydra.core.Binding =
        {
        lazy val bname: hydra.core.Name = (b.name)
        lazy val bterm: hydra.core.Term = (b.term)
        hydra.lib.maybes.maybe[hydra.core.Binding, hydra.core.TypeScheme](hydra.core.Binding(bname,
           hydra.inference.bindUnboundTypeVariables(cx)(bterm), None))((ts: hydra.core.TypeScheme) =>
          {
          lazy val bvars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](ts.variables)
          {
            lazy val excluded: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.union[hydra.core.Name](svars)(bvars)
            {
              lazy val inType: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.difference[hydra.core.Name](hydra.variables.freeVariablesInType(ts.`type`))(excluded)
              {
                lazy val phantoms: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.difference[hydra.core.Name](hydra.variables.freeTypeVariablesInTerm(bterm))(hydra.lib.sets.union[hydra.core.Name](excluded)(inType))
                {
                  lazy val phantomSubst: hydra.typing.TypeSubst = hydra.lib.maps.fromList[hydra.core.Name,
                     hydra.core.Type](hydra.lib.lists.map[hydra.core.Name, Tuple2[hydra.core.Name,
                     hydra.core.Type]]((v: hydra.core.Name) => Tuple2(v, hydra.core.Type.unit))(hydra.lib.sets.toList[hydra.core.Name](phantoms)))
                  {
                    lazy val bterm1: hydra.core.Term = hydra.substitution.substTypesInTerm(phantomSubst)(bterm)
                    {
                      lazy val unbound: Seq[hydra.core.Name] = hydra.lib.sets.toList[hydra.core.Name](inType)
                      {
                        lazy val ts2: hydra.core.TypeScheme = hydra.core.TypeScheme(hydra.lib.lists.concat2[hydra.core.Name](ts.variables)(unbound),
                           (ts.`type`), (ts.constraints))
                        {
                          lazy val bterm2: hydra.core.Term = hydra.lib.lists.foldl[hydra.core.Term,
                             hydra.core.Name]((t: hydra.core.Term) =>
                            (v: hydra.core.Name) => hydra.core.Term.typeLambda(hydra.core.TypeLambda(v,
                               t)))(bterm1)(unbound)
                          hydra.core.Binding(bname, bterm2, Some(ts2))
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        })(b.`type`)
      }
      hydra.core.Term.let(hydra.core.Let(hydra.lib.lists.map[hydra.core.Binding, hydra.core.Binding](forBinding)(v_Term_let_l.bindings),
         hydra.inference.bindUnboundTypeVariables(cx)(v_Term_let_l.body)))
    }
    case _ => recurse(term)
  hydra.rewriting.rewriteTerm(rewrite)(term0)
}

def buildTypeApplicationTerm(tvars: Seq[hydra.core.Name])(body: hydra.core.Term): hydra.core.Term =
  hydra.lib.lists.foldl[hydra.core.Term, hydra.core.Name]((t: hydra.core.Term) =>
  (v: hydra.core.Name) =>
  hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(t, hydra.core.Type.variable(v))))(body)(tvars)

def extendContext(pairs: Seq[Tuple2[hydra.core.Name, hydra.core.TypeScheme]])(cx: hydra.graph.Graph): hydra.graph.Graph =
  hydra.graph.Graph(cx.boundTerms, hydra.lib.maps.union[hydra.core.Name, hydra.core.TypeScheme](hydra.lib.maps.fromList[hydra.core.Name,
     hydra.core.TypeScheme](pairs))(cx.boundTypes), (cx.classConstraints), (cx.lambdaVariables),
     (cx.metadata), (cx.primitives), (cx.schemaTypes), (cx.typeVariables))

def finalizeInferredTerm[T0](flowCx: T0)(cx: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  {
  lazy val term2: hydra.core.Term = hydra.inference.bindUnboundTypeVariables(cx)(term)
  hydra.lib.eithers.bind[hydra.errors.Error, Unit, hydra.core.Term](hydra.checking.checkForUnboundTypeVariables(flowCx)(cx)(term2))((_x: Unit) => Right(hydra.variables.normalizeTypeVariablesInTerm(term2)))
}

def forInferredTerm[T0](fcx: hydra.context.Context)(cx: hydra.graph.Graph)(term: hydra.core.Term)(desc: scala.Predef.String)(f: (hydra.typing.InferenceResult => T0)): Either[hydra.errors.Error,
   Tuple2[T0, hydra.context.Context]] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.InferenceResult, Tuple2[T0,
     hydra.context.Context]](hydra.inference.inferTypeOfTerm(fcx)(cx)(term)(desc))((rp: hydra.typing.InferenceResult) => Right(Tuple2(f(rp),
     (rp.context))))

def freeVariablesInContext(cx: hydra.graph.Graph): scala.collection.immutable.Set[hydra.core.Name] =
  hydra.lib.lists.foldl[scala.collection.immutable.Set[hydra.core.Name], scala.collection.immutable.Set[hydra.core.Name]](hydra.lib.sets.union[hydra.core.Name])(hydra.lib.sets.empty[hydra.core.Name])(hydra.lib.lists.map[hydra.core.TypeScheme,
     scala.collection.immutable.Set[hydra.core.Name]](hydra.variables.freeVariablesInTypeSchemeSimple)(hydra.lib.maps.elems[hydra.core.Name,
     hydra.core.TypeScheme](cx.boundTypes)))

def freshVariableType(cx: hydra.context.Context): Tuple2[hydra.core.Type, hydra.context.Context] =
  {
  lazy val result: Tuple2[hydra.core.Name, hydra.context.Context] = hydra.names.freshName(cx)
  lazy val name: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, hydra.context.Context](result)
  lazy val cx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Name, hydra.context.Context](result)
  Tuple2(hydra.core.Type.variable(name), cx2)
}

def generalize(cx: hydra.graph.Graph)(typ: hydra.core.Type): hydra.core.TypeScheme =
  {
  def isTypeVarName(name: hydra.core.Name): Boolean =
    {
    lazy val parts: Seq[scala.Predef.String] = hydra.lib.strings.splitOn(".")(name)
    hydra.lib.equality.lte[Int](hydra.lib.lists.length[scala.Predef.String](parts))(1)
  }
  lazy val vars: Seq[hydra.core.Name] = hydra.lib.lists.nub[hydra.core.Name](hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) =>
    hydra.lib.logic.and(hydra.inference.isUnbound(cx)(v))(isTypeVarName(v)))(hydra.variables.freeVariablesInTypeOrdered(typ)))
  lazy val allConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = (cx.classConstraints)
  lazy val relevantConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.lib.maps.fromList[hydra.core.Name,
     hydra.core.TypeVariableMetadata](hydra.lib.maybes.cat[Tuple2[hydra.core.Name,
     hydra.core.TypeVariableMetadata]](hydra.lib.lists.map[hydra.core.Name, Option[Tuple2[hydra.core.Name,
     hydra.core.TypeVariableMetadata]]]((v: hydra.core.Name) =>
    hydra.lib.maybes.map[hydra.core.TypeVariableMetadata, Tuple2[hydra.core.Name,
       hydra.core.TypeVariableMetadata]]((meta: hydra.core.TypeVariableMetadata) => Tuple2(v,
       meta))(hydra.lib.maps.lookup[hydra.core.Name, hydra.core.TypeVariableMetadata](v)(allConstraints)))(vars)))
  lazy val constraintsMaybe: Option[Map[hydra.core.Name, hydra.core.TypeVariableMetadata]] = hydra.lib.logic.ifElse[Option[Map[hydra.core.Name,
     hydra.core.TypeVariableMetadata]]](hydra.lib.maps.`null`[hydra.core.Name, hydra.core.TypeVariableMetadata](relevantConstraints))(None)(Some(relevantConstraints))
  hydra.core.TypeScheme(vars, typ, constraintsMaybe)
}

def inferGraphTypes(fcx0: hydra.context.Context)(bindings0: Seq[hydra.core.Binding])(g0: hydra.graph.Graph): Either[hydra.errors.Error,
   Tuple2[Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]], hydra.context.Context]] =
  {
  lazy val fcx: hydra.context.Context = hydra.context.Context(hydra.lib.lists.cons[scala.Predef.String]("graph inference")(fcx0.trace),
     (fcx0.messages), (fcx0.other))
  lazy val let0: hydra.core.Let = hydra.core.Let(bindings0, hydra.core.Term.unit)
  def fromLetTerm(l: hydra.core.Let): Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]] =
    {
    lazy val bindings: Seq[hydra.core.Binding] = (l.bindings)
    lazy val prims: Map[hydra.core.Name, hydra.graph.Primitive] = (g0.primitives)
    lazy val schemaTypes: Map[hydra.core.Name, hydra.core.TypeScheme] = (g0.schemaTypes)
    lazy val rawG: hydra.graph.Graph = hydra.lexical.buildGraph(bindings)(hydra.lib.maps.empty[hydra.core.Name,
       Option[hydra.core.Term]])(prims)
    lazy val g: hydra.graph.Graph = hydra.graph.Graph(rawG.boundTerms, (rawG.boundTypes),
       (rawG.classConstraints), (rawG.lambdaVariables), (rawG.metadata), (rawG.primitives),
       schemaTypes, (rawG.typeVariables))
    Tuple2(g, bindings)
  }
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.InferenceResult, Tuple2[Tuple2[hydra.graph.Graph,
     Seq[hydra.core.Binding]], hydra.context.Context]](hydra.inference.inferTypeOfTerm(fcx)(g0)(hydra.core.Term.let(let0))("graph term"))((result: hydra.typing.InferenceResult) =>
    {
    lazy val fcx2: hydra.context.Context = (result.context)
    {
      lazy val term: hydra.core.Term = (result.term)
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, Tuple2[Tuple2[hydra.graph.Graph,
         Seq[hydra.core.Binding]], hydra.context.Context]](hydra.inference.finalizeInferredTerm(fcx2)(g0)(term))((finalized: hydra.core.Term) =>
        finalized match
        case hydra.core.Term.let(v_Term_let_l) => Right(Tuple2(fromLetTerm(v_Term_let_l), fcx2))
        case hydra.core.Term.variable(v_Term_variable__) => Left(hydra.errors.Error.other("Expected inferred graph as let term")))
    }
  })
}

def inferInGraphContext(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.Error,
   hydra.typing.InferenceResult] = hydra.inference.inferTypeOfTerm(fcx)(cx)(term)("single term")

def inferMany(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(pairs: Seq[Tuple2[hydra.core.Term,
   scala.Predef.String]]): Either[hydra.errors.Error, Tuple2[Tuple2[Seq[hydra.core.Term],
   Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
   hydra.core.TypeVariableMetadata]]]], hydra.context.Context]] =
  {
  def emptyResult[T0, T1, T2, T3, T4]: Either[T0, Tuple2[Tuple2[Seq[T1], Tuple2[Seq[T2],
     Tuple2[hydra.typing.TypeSubst, Map[T3, T4]]]], hydra.context.Context]] =
    Right(Tuple2(Tuple2(Seq(), Tuple2(Seq(), Tuple2(hydra.substitution.idTypeSubst,
       hydra.lib.maps.empty[T3, T4]))), fcx))
  hydra.lib.maybes.maybe[Either[hydra.errors.Error, Tuple2[Tuple2[Seq[hydra.core.Term],
     Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
     hydra.core.TypeVariableMetadata]]]], hydra.context.Context]], Tuple2[Tuple2[hydra.core.Term,
     scala.Predef.String], Seq[Tuple2[hydra.core.Term, scala.Predef.String]]]](emptyResult)((pairsUc: Tuple2[Tuple2[hydra.core.Term,
     scala.Predef.String], Seq[Tuple2[hydra.core.Term, scala.Predef.String]]]) =>
    {
    lazy val headPair: Tuple2[hydra.core.Term, scala.Predef.String] = hydra.lib.pairs.first[Tuple2[hydra.core.Term,
       scala.Predef.String], Seq[Tuple2[hydra.core.Term, scala.Predef.String]]](pairsUc)
    {
      lazy val tl: Seq[Tuple2[hydra.core.Term, scala.Predef.String]] = hydra.lib.pairs.second[Tuple2[hydra.core.Term,
         scala.Predef.String], Seq[Tuple2[hydra.core.Term, scala.Predef.String]]](pairsUc)
      {
        lazy val e: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, scala.Predef.String](headPair)
        {
          lazy val desc: scala.Predef.String = hydra.lib.pairs.second[hydra.core.Term, scala.Predef.String](headPair)
          hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.InferenceResult,
             Tuple2[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
             Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]], hydra.context.Context]](hydra.inference.inferTypeOfTerm(fcx)(cx)(e)(desc))((result1: hydra.typing.InferenceResult) =>
            {
            lazy val fcx2: hydra.context.Context = (result1.context)
            {
              lazy val e1: hydra.core.Term = (result1.term)
              {
                lazy val t1: hydra.core.Type = (result1.`type`)
                {
                  lazy val s1: hydra.typing.TypeSubst = (result1.subst)
                  {
                    lazy val c1: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = (result1.classConstraints)
                    hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[Tuple2[Seq[hydra.core.Term],
                       Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                       Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]], hydra.context.Context],
                       Tuple2[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
                       Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
                       hydra.context.Context]](hydra.inference.inferMany(fcx2)(hydra.substitution.substInContext(s1)(cx))(tl))((rp2: Tuple2[Tuple2[Seq[hydra.core.Term],
                       Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                       Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]], hydra.context.Context]) =>
                      {
                      lazy val result2: Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
                         Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]] = hydra.lib.pairs.first[Tuple2[Seq[hydra.core.Term],
                         Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                         Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
                         hydra.context.Context](rp2)
                      {
                        lazy val fcx3: hydra.context.Context = hydra.lib.pairs.second[Tuple2[Seq[hydra.core.Term],
                           Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                           Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
                           hydra.context.Context](rp2)
                        {
                          lazy val e2: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term],
                             Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                             Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](result2)
                          {
                            lazy val t2: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type],
                               Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                               hydra.core.TypeVariableMetadata]]](hydra.lib.pairs.second[Seq[hydra.core.Term],
                               Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                               Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](result2))
                            {
                              lazy val s2: hydra.typing.TypeSubst = hydra.lib.pairs.first[hydra.typing.TypeSubst,
                                 Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](hydra.lib.pairs.second[Seq[hydra.core.Type],
                                 Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                                 hydra.core.TypeVariableMetadata]]](hydra.lib.pairs.second[Seq[hydra.core.Term],
                                 Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                                 Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](result2)))
                              {
                                lazy val c2: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.lib.pairs.second[hydra.typing.TypeSubst,
                                   Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](hydra.lib.pairs.second[Seq[hydra.core.Type],
                                   Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                                   hydra.core.TypeVariableMetadata]]](hydra.lib.pairs.second[Seq[hydra.core.Term],
                                   Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                                   Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](result2)))
                                {
                                  lazy val c1Subst: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.substitution.substInClassConstraints(s2)(c1)
                                  {
                                    lazy val mergedConstraints: Map[hydra.core.Name,
                                       hydra.core.TypeVariableMetadata] = hydra.inference.mergeClassConstraints(c1Subst)(c2)
                                    Right(Tuple2(Tuple2(hydra.lib.lists.cons[hydra.core.Term](hydra.substitution.substTypesInTerm(s2)(e1))(e2),
                                       Tuple2(hydra.lib.lists.cons[hydra.core.Type](hydra.substitution.substInType(s2)(t1))(t2),
                                       Tuple2(hydra.substitution.composeTypeSubst(s1)(s2),
                                       mergedConstraints))), fcx3))
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
  })(hydra.lib.lists.uncons[Tuple2[hydra.core.Term, scala.Predef.String]](pairs))
}

def inferTypeOf(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.Error,
   Tuple2[Tuple2[hydra.core.Term, hydra.core.TypeScheme], hydra.context.Context]] =
  {
  lazy val letTerm: hydra.core.Term = hydra.core.Term.let(hydra.core.Let(Seq(hydra.core.Binding("ignoredVariableName",
     term, None)), hydra.core.Term.literal(hydra.core.Literal.string("ignoredBody"))))
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.InferenceResult, Tuple2[Tuple2[hydra.core.Term,
     hydra.core.TypeScheme], hydra.context.Context]](hydra.inference.inferTypeOfTerm(fcx)(cx)(letTerm)("infer type of term"))((result: hydra.typing.InferenceResult) =>
    {
    lazy val fcx2: hydra.context.Context = (result.context)
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, Tuple2[Tuple2[hydra.core.Term,
       hydra.core.TypeScheme], hydra.context.Context]](hydra.inference.finalizeInferredTerm(fcx2)(cx)(result.term))((finalized: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Let, Tuple2[Tuple2[hydra.core.Term,
         hydra.core.TypeScheme], hydra.context.Context]](hydra.extract.core.let(cx)(finalized))((letResult: hydra.core.Let) =>
      {
      lazy val bindings: Seq[hydra.core.Binding] = (letResult.bindings)
      {
        def wrongCount[T0]: Either[hydra.errors.Error, T0] =
          Left(hydra.errors.Error.other(hydra.lib.strings.cat(Seq("Expected a single binding with a type scheme, but got: ",
             hydra.lib.literals.showInt32(hydra.lib.lists.length[hydra.core.Binding](bindings)),
             " bindings"))))
        hydra.lib.logic.ifElse[Either[hydra.errors.Error, Tuple2[Tuple2[hydra.core.Term,
           hydra.core.TypeScheme], hydra.context.Context]]](hydra.lib.equality.equal[Int](1)(hydra.lib.lists.length[hydra.core.Binding](bindings)))(hydra.lib.maybes.maybe[Either[hydra.errors.Error,
           Tuple2[Tuple2[hydra.core.Term, hydra.core.TypeScheme], hydra.context.Context]],
           hydra.core.Binding](wrongCount)((binding: hydra.core.Binding) =>
          {
          lazy val term1: hydra.core.Term = (binding.term)
          {
            lazy val mts: Option[hydra.core.TypeScheme] = (binding.`type`)
            hydra.lib.maybes.maybe[Either[hydra.errors.Error, Tuple2[Tuple2[hydra.core.Term,
               hydra.core.TypeScheme], hydra.context.Context]], hydra.core.TypeScheme](Left(hydra.errors.Error.other("Expected a type scheme")))((ts: hydra.core.TypeScheme) => Right(Tuple2(Tuple2(term1,
               ts), fcx2)))(mts)
          }
        })(hydra.lib.lists.maybeHead[hydra.core.Binding](bindings)))(wrongCount)
      }
    }))
  })
}

def inferTypeOfAnnotatedTerm(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(at: hydra.core.AnnotatedTerm): Either[hydra.errors.Error,
   hydra.typing.InferenceResult] =
  {
  lazy val term: hydra.core.Term = (at.body)
  lazy val ann: Map[hydra.core.Name, hydra.core.Term] = (at.annotation)
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.InferenceResult, hydra.typing.InferenceResult](hydra.inference.inferTypeOfTerm(fcx)(cx)(term)("annotated term"))((result: hydra.typing.InferenceResult) =>
    {
    lazy val fcx2: hydra.context.Context = (result.context)
    {
      lazy val iterm: hydra.core.Term = (result.term)
      {
        lazy val itype: hydra.core.Type = (result.`type`)
        {
          lazy val isubst: hydra.typing.TypeSubst = (result.subst)
          {
            lazy val iconstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = (result.classConstraints)
            Right(hydra.typing.InferenceResult(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(iterm,
               ann)), itype, isubst, iconstraints, fcx2))
          }
        }
      }
    }
  })
}

def inferTypeOfApplication(fcx0: hydra.context.Context)(cx: hydra.graph.Graph)(app: hydra.core.Application): Either[hydra.errors.Error,
   hydra.typing.InferenceResult] =
  {
  lazy val fcx: hydra.context.Context = hydra.context.Context(hydra.lib.lists.cons[scala.Predef.String]("application")(fcx0.trace),
     (fcx0.messages), (fcx0.other))
  lazy val e0: hydra.core.Term = (app.function)
  lazy val e1: hydra.core.Term = (app.argument)
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.InferenceResult, hydra.typing.InferenceResult](hydra.inference.inferTypeOfTerm(fcx)(cx)(e0)("lhs"))((lhsResult: hydra.typing.InferenceResult) =>
    {
    lazy val fcx2: hydra.context.Context = (lhsResult.context)
    {
      lazy val a: hydra.core.Term = (lhsResult.term)
      {
        lazy val t0: hydra.core.Type = (lhsResult.`type`)
        {
          lazy val s0: hydra.typing.TypeSubst = (lhsResult.subst)
          {
            lazy val c0: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = (lhsResult.classConstraints)
            hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.InferenceResult,
               hydra.typing.InferenceResult](hydra.inference.inferTypeOfTerm(fcx2)(hydra.substitution.substInContext(s0)(cx))(e1)("rhs"))((rhsResult: hydra.typing.InferenceResult) =>
              {
              lazy val fcx3: hydra.context.Context = (rhsResult.context)
              {
                lazy val b: hydra.core.Term = (rhsResult.term)
                {
                  lazy val t1: hydra.core.Type = (rhsResult.`type`)
                  {
                    lazy val s1: hydra.typing.TypeSubst = (rhsResult.subst)
                    {
                      lazy val c1: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = (rhsResult.classConstraints)
                      {
                        lazy val vResult: Tuple2[hydra.core.Name, hydra.context.Context] = hydra.names.freshName(fcx3)
                        {
                          lazy val v: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name,
                             hydra.context.Context](vResult)
                          {
                            lazy val fcx4: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Name,
                               hydra.context.Context](vResult)
                            hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.TypeSubst,
                               hydra.typing.InferenceResult](hydra.lib.eithers.bimap[hydra.errors.UnificationError,
                               hydra.typing.TypeSubst, hydra.errors.Error, hydra.typing.TypeSubst]((_e: hydra.errors.UnificationError) => hydra.errors.Error.unification(_e))((_a: hydra.typing.TypeSubst) => _a)(hydra.unification.unifyTypes(fcx4)(cx.schemaTypes)(hydra.substitution.substInType(s1)(t0))(hydra.core.Type.function(hydra.core.FunctionType(t1,
                               hydra.core.Type.variable(v))))("application lhs")))((s2: hydra.typing.TypeSubst) =>
                              hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.TypeSubst,
                                 hydra.typing.InferenceResult](hydra.checking.checkTypeSubst(fcx4)(cx)(s2))((_x: hydra.typing.TypeSubst) =>
                              {
                              lazy val rExpr: hydra.core.Term = hydra.core.Term.application(hydra.core.Application(hydra.substitution.substTypesInTerm(hydra.substitution.composeTypeSubst(s1)(s2))(a),
                                 hydra.substitution.substTypesInTerm(s2)(b)))
                              {
                                lazy val rType: hydra.core.Type = hydra.substitution.substInType(s2)(hydra.core.Type.variable(v))
                                {
                                  lazy val rSubst: hydra.typing.TypeSubst = hydra.substitution.composeTypeSubstList(Seq(s0,
                                     s1, s2))
                                  {
                                    lazy val c0Subst: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.substitution.substInClassConstraints(s2)(hydra.substitution.substInClassConstraints(s1)(c0))
                                    {
                                      lazy val c1Subst: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.substitution.substInClassConstraints(s2)(c1)
                                      {
                                        lazy val rConstraints: Map[hydra.core.Name,
                                           hydra.core.TypeVariableMetadata] = hydra.inference.mergeClassConstraints(c0Subst)(c1Subst)
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

def inferTypeOfCaseStatement(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(caseStmt: hydra.core.CaseStatement): Either[hydra.errors.Error,
   hydra.typing.InferenceResult] =
  {
  lazy val tname: hydra.core.Name = (caseStmt.typeName)
  lazy val dflt: Option[hydra.core.Term] = (caseStmt.default)
  lazy val cases: Seq[hydra.core.Field] = (caseStmt.cases)
  lazy val fnames: Seq[hydra.core.Name] = hydra.lib.lists.map[hydra.core.Field, hydra.core.Name]((x: hydra.core.Field) => (x.name))(cases)
  hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[hydra.core.TypeScheme, hydra.context.Context],
     hydra.typing.InferenceResult](hydra.resolution.requireSchemaType(fcx)(cx.schemaTypes)(tname))((stRp: Tuple2[hydra.core.TypeScheme,
     hydra.context.Context]) =>
    {
    lazy val schemaType: hydra.core.TypeScheme = hydra.lib.pairs.first[hydra.core.TypeScheme,
       hydra.context.Context](stRp)
    {
      lazy val fcx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.TypeScheme, hydra.context.Context](stRp)
      {
        lazy val svars: Seq[hydra.core.Name] = (schemaType.variables)
        {
          lazy val stype: hydra.core.Type = (schemaType.`type`)
          hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.FieldType], hydra.typing.InferenceResult](hydra.extract.core.unionType(tname)(stype))((sfields: Seq[hydra.core.FieldType]) =>
            hydra.lib.eithers.bind[hydra.errors.Error, Option[hydra.typing.InferenceResult],
               hydra.typing.InferenceResult](hydra.lib.eithers.mapMaybe[hydra.core.Term,
               hydra.typing.InferenceResult, hydra.errors.Error]((t: hydra.core.Term) =>
            hydra.inference.inferTypeOfTerm(fcx2)(cx)(t)(hydra.lib.strings.cat(Seq("case ",
               tname, ".<default>"))))(dflt))((dfltRp: Option[hydra.typing.InferenceResult]) =>
            {
            lazy val dfltResult: Option[hydra.typing.InferenceResult] = dfltRp
            {
              lazy val fcx3: hydra.context.Context = hydra.lib.maybes.fromMaybe[hydra.context.Context](fcx2)(hydra.lib.maybes.map[hydra.typing.InferenceResult,
                 hydra.context.Context]((x: hydra.typing.InferenceResult) => (x.context))(dfltRp))
              hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[Tuple2[Seq[hydra.core.Term],
                 Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                 hydra.core.TypeVariableMetadata]]]], hydra.context.Context], hydra.typing.InferenceResult](hydra.inference.inferMany(fcx3)(cx)(hydra.lib.lists.map[hydra.core.Field,
                 Tuple2[hydra.core.Term, scala.Predef.String]]((f: hydra.core.Field) =>
                Tuple2(f.term, hydra.lib.strings.cat(Seq("case ", tname, ".", (f.name)))))(cases)))((caseRp: Tuple2[Tuple2[Seq[hydra.core.Term],
                   Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                   hydra.core.TypeVariableMetadata]]]], hydra.context.Context]) =>
                {
                lazy val caseResults: Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
                   Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]] = hydra.lib.pairs.first[Tuple2[Seq[hydra.core.Term],
                   Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                   hydra.core.TypeVariableMetadata]]]], hydra.context.Context](caseRp)
                {
                  lazy val fcx4: hydra.context.Context = hydra.lib.pairs.second[Tuple2[Seq[hydra.core.Term],
                     Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                     hydra.core.TypeVariableMetadata]]]], hydra.context.Context](caseRp)
                  {
                    lazy val iterms: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term],
                       Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                       Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](caseResults)
                    {
                      lazy val itypes: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type],
                         Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](hydra.lib.pairs.second[Seq[hydra.core.Term],
                         Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                         Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](caseResults))
                      {
                        lazy val isubst: hydra.typing.TypeSubst = hydra.lib.pairs.first[hydra.typing.TypeSubst,
                           Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](hydra.lib.pairs.second[Seq[hydra.core.Type],
                           Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](hydra.lib.pairs.second[Seq[hydra.core.Term],
                           Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                           Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](caseResults)))
                        {
                          lazy val caseElemConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.lib.pairs.second[hydra.typing.TypeSubst,
                             Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](hydra.lib.pairs.second[Seq[hydra.core.Type],
                             Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](hydra.lib.pairs.second[Seq[hydra.core.Term],
                             Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                             Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](caseResults)))
                          {
                            lazy val codvResult: Tuple2[hydra.core.Name, hydra.context.Context] = hydra.names.freshName(fcx4)
                            {
                              lazy val codv: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name,
                                 hydra.context.Context](codvResult)
                              {
                                lazy val fcx5: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Name,
                                   hydra.context.Context](codvResult)
                                {
                                  lazy val cod: hydra.core.Type = hydra.core.Type.variable(codv)
                                  {
                                    lazy val caseMap: Map[hydra.core.Name, hydra.core.Type] = hydra.lib.maps.fromList[hydra.core.Name,
                                       hydra.core.Type](hydra.lib.lists.map[hydra.core.FieldType,
                                       Tuple2[hydra.core.Name, hydra.core.Type]]((ft: hydra.core.FieldType) => Tuple2(ft.name,
                                       (ft.`type`)))(sfields))
                                    {
                                      lazy val dfltConstraints: Seq[hydra.typing.TypeConstraint] = hydra.lib.maybes.toList[hydra.typing.TypeConstraint](hydra.lib.maybes.map[hydra.typing.InferenceResult,
                                         hydra.typing.TypeConstraint]((r: hydra.typing.InferenceResult) =>
                                        hydra.typing.TypeConstraint(cod, hydra.substitution.substInType(isubst)(r.`type`),
                                           "match default"))(dfltResult))
                                      {
                                        lazy val caseConstraints: Seq[hydra.typing.TypeConstraint] = hydra.lib.maybes.cat[hydra.typing.TypeConstraint](hydra.lib.lists.zipWith[hydra.core.Name,
                                           hydra.core.Type, Option[hydra.typing.TypeConstraint]]((fname: hydra.core.Name) =>
                                          (itype: hydra.core.Type) =>
                                          hydra.lib.maybes.map[hydra.core.Type, hydra.typing.TypeConstraint]((ftype: hydra.core.Type) =>
                                          hydra.typing.TypeConstraint(itype, hydra.core.Type.function(hydra.core.FunctionType(ftype,
                                             cod)), "case type"))(hydra.lib.maps.lookup[hydra.core.Name,
                                             hydra.core.Type](fname)(caseMap)))(fnames)(itypes))
                                        {
                                          lazy val dfltClassConstraints: Map[hydra.core.Name,
                                             hydra.core.TypeVariableMetadata] = hydra.lib.maybes.fromMaybe[Map[hydra.core.Name,
                                             hydra.core.TypeVariableMetadata]](hydra.lib.maps.empty[hydra.core.Name,
                                             hydra.core.TypeVariableMetadata])(hydra.lib.maybes.map[hydra.typing.InferenceResult,
                                             Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]((x: hydra.typing.InferenceResult) => (x.classConstraints))(dfltResult))
                                          {
                                            lazy val allElemConstraints: Map[hydra.core.Name,
                                               hydra.core.TypeVariableMetadata] = hydra.inference.mergeClassConstraints(caseElemConstraints)(dfltClassConstraints)
                                            hydra.lib.eithers.bind[hydra.errors.Error,
                                               hydra.typing.InferenceResult, hydra.typing.InferenceResult](hydra.inference.mapConstraints(fcx5)(cx)((subst: hydra.typing.TypeSubst) =>
                                              hydra.inference.yieldWithConstraints(fcx5)(hydra.inference.buildTypeApplicationTerm(svars)(hydra.core.Term.cases(hydra.core.CaseStatement(tname,
                                                 hydra.lib.maybes.map[hydra.typing.InferenceResult,
                                                 hydra.core.Term]((x: hydra.typing.InferenceResult) => (x.term))(dfltResult),
                                                 hydra.lib.lists.zipWith[hydra.core.Name,
                                                 hydra.core.Term, hydra.core.Field]((n: hydra.core.Name) => (t: hydra.core.Term) => hydra.core.Field(n,
                                                 t))(fnames)(iterms)))))(hydra.core.Type.function(hydra.core.FunctionType(hydra.resolution.nominalApplication(tname)(hydra.lib.lists.map[hydra.core.Name,
                                                 hydra.core.Type]((x: hydra.core.Name) => hydra.core.Type.variable(x))(svars)),
                                                 cod)))(hydra.substitution.composeTypeSubstList(hydra.lib.lists.concat[hydra.typing.TypeSubst](Seq(hydra.lib.maybes.toList[hydra.typing.TypeSubst](hydra.lib.maybes.map[hydra.typing.InferenceResult,
                                                 hydra.typing.TypeSubst]((x: hydra.typing.InferenceResult) => (x.subst))(dfltResult)),
                                                 Seq(isubst, subst)))))(hydra.substitution.substInClassConstraints(subst)(allElemConstraints)))(hydra.lib.lists.concat[hydra.typing.TypeConstraint](Seq(dfltConstraints,
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

def inferTypeOfCollection(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(typCons: (hydra.core.Type => hydra.core.Type))(trmCons: (Seq[hydra.core.Term] => hydra.core.Term))(desc: scala.Predef.String)(classNames: scala.collection.immutable.Set[hydra.core.Name])(els: Seq[hydra.core.Term]): Either[hydra.errors.Error,
   hydra.typing.InferenceResult] =
  {
  lazy val varResult: Tuple2[hydra.core.Name, hydra.context.Context] = hydra.names.freshName(fcx)
  lazy val `var`: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, hydra.context.Context](varResult)
  lazy val fcx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Name, hydra.context.Context](varResult)
  lazy val classConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.lib.logic.ifElse[Map[hydra.core.Name,
     hydra.core.TypeVariableMetadata]](hydra.lib.sets.`null`[hydra.core.Name](classNames))(hydra.lib.maps.empty[hydra.core.Name,
     hydra.core.TypeVariableMetadata])(hydra.lib.maps.singleton[hydra.core.Name, hydra.core.TypeVariableMetadata](`var`)(hydra.core.TypeVariableMetadata(classNames)))
  hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.typing.InferenceResult]](hydra.lib.lists.`null`[hydra.core.Term](els))(Right(hydra.inference.yieldWithConstraints(fcx2)(hydra.inference.buildTypeApplicationTerm(Seq(`var`))(trmCons(Seq())))(typCons(hydra.core.Type.variable(`var`)))(hydra.substitution.idTypeSubst)(classConstraints)))(hydra.lib.eithers.bind[hydra.errors.Error,
     Tuple2[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
     Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]], hydra.context.Context],
     hydra.typing.InferenceResult](hydra.inference.inferMany(fcx2)(cx)(hydra.lib.lists.zip[hydra.core.Term,
     scala.Predef.String](els)(hydra.lib.lists.map[Int, scala.Predef.String]((i: Int) =>
    hydra.lib.strings.cat(Seq("#", hydra.lib.literals.showInt32(i))))(hydra.lib.math.range(1)(hydra.lib.math.add(hydra.lib.lists.length[hydra.core.Term](els))(1))))))((resultsRp: Tuple2[Tuple2[Seq[hydra.core.Term],
       Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
       hydra.core.TypeVariableMetadata]]]], hydra.context.Context]) =>
    {
    lazy val results: Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
       Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]] = hydra.lib.pairs.first[Tuple2[Seq[hydra.core.Term],
       Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
       hydra.core.TypeVariableMetadata]]]], hydra.context.Context](resultsRp)
    {
      lazy val fcx3: hydra.context.Context = hydra.lib.pairs.second[Tuple2[Seq[hydra.core.Term],
         Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
         hydra.core.TypeVariableMetadata]]]], hydra.context.Context](resultsRp)
      {
        lazy val terms: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term],
           Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
           hydra.core.TypeVariableMetadata]]]](results)
        {
          lazy val types: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type],
             Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](hydra.lib.pairs.second[Seq[hydra.core.Term],
             Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
             hydra.core.TypeVariableMetadata]]]](results))
          {
            lazy val subst1: hydra.typing.TypeSubst = hydra.lib.pairs.first[hydra.typing.TypeSubst,
               Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](hydra.lib.pairs.second[Seq[hydra.core.Type],
               Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](hydra.lib.pairs.second[Seq[hydra.core.Term],
               Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
               hydra.core.TypeVariableMetadata]]]](results)))
            {
              lazy val elemConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.lib.pairs.second[hydra.typing.TypeSubst,
                 Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](hydra.lib.pairs.second[Seq[hydra.core.Type],
                 Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](hydra.lib.pairs.second[Seq[hydra.core.Term],
                 Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                 hydra.core.TypeVariableMetadata]]]](results)))
              {
                lazy val constraints: Seq[hydra.typing.TypeConstraint] = hydra.lib.lists.map[hydra.core.Type,
                   hydra.typing.TypeConstraint]((t: hydra.core.Type) =>
                  hydra.typing.TypeConstraint(hydra.core.Type.variable(`var`), t, desc))(types)
                {
                  lazy val allConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.inference.mergeClassConstraints(classConstraints)(elemConstraints)
                  hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.InferenceResult,
                     hydra.typing.InferenceResult](hydra.inference.mapConstraints(fcx3)(cx)((subst2: hydra.typing.TypeSubst) =>
                    {
                    lazy val iterm: hydra.core.Term = trmCons(terms)
                    {
                      lazy val itype: hydra.core.Type = typCons(hydra.core.Type.variable(`var`))
                      {
                        lazy val isubst: hydra.typing.TypeSubst = hydra.substitution.composeTypeSubst(subst1)(subst2)
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

def inferTypeOfEither(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(e: Either[hydra.core.Term,
   hydra.core.Term]): Either[hydra.errors.Error, hydra.typing.InferenceResult] =
  hydra.lib.eithers.either[hydra.core.Term, hydra.core.Term, Either[hydra.errors.Error,
     hydra.typing.InferenceResult]]((l: hydra.core.Term) =>
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.InferenceResult, hydra.typing.InferenceResult](hydra.inference.inferTypeOfTerm(fcx)(cx)(l)("either left value"))((r1: hydra.typing.InferenceResult) =>
  {
  lazy val fcx2: hydra.context.Context = (r1.context)
  {
    lazy val iterm: hydra.core.Term = (r1.term)
    {
      lazy val leftType: hydra.core.Type = (r1.`type`)
      {
        lazy val subst: hydra.typing.TypeSubst = (r1.subst)
        {
          lazy val fvResult: Tuple2[hydra.core.Type, hydra.context.Context] = hydra.inference.freshVariableType(fcx2)
          {
            lazy val rightType: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type,
               hydra.context.Context](fvResult)
            {
              lazy val fcx3: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Type,
                 hydra.context.Context](fvResult)
              {
                lazy val eitherTerm: hydra.core.Term = hydra.core.Term.either(Left(iterm))
                {
                  lazy val termWithLeftType: hydra.core.Term = hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(eitherTerm,
                     leftType))
                  {
                    lazy val termWithBothTypes: hydra.core.Term = hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(termWithLeftType,
                       rightType))
                    {
                      lazy val eitherType: hydra.core.Type = hydra.core.Type.either(hydra.core.EitherType(leftType,
                         rightType))
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
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.InferenceResult, hydra.typing.InferenceResult](hydra.inference.inferTypeOfTerm(fcx)(cx)(r)("either right value"))((r1: hydra.typing.InferenceResult) =>
  {
  lazy val fcx2: hydra.context.Context = (r1.context)
  {
    lazy val iterm: hydra.core.Term = (r1.term)
    {
      lazy val rightType: hydra.core.Type = (r1.`type`)
      {
        lazy val subst: hydra.typing.TypeSubst = (r1.subst)
        {
          lazy val fvResult: Tuple2[hydra.core.Type, hydra.context.Context] = hydra.inference.freshVariableType(fcx2)
          {
            lazy val leftType: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](fvResult)
            {
              lazy val fcx3: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Type,
                 hydra.context.Context](fvResult)
              {
                lazy val eitherTerm: hydra.core.Term = hydra.core.Term.either(Right(iterm))
                {
                  lazy val termWithLeftType: hydra.core.Term = hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(eitherTerm,
                     leftType))
                  {
                    lazy val termWithBothTypes: hydra.core.Term = hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(termWithLeftType,
                       rightType))
                    {
                      lazy val eitherType: hydra.core.Type = hydra.core.Type.either(hydra.core.EitherType(leftType,
                         rightType))
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

def inferTypeOfInjection(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(injection: hydra.core.Injection): Either[hydra.errors.Error,
   hydra.typing.InferenceResult] =
  {
  lazy val tname: hydra.core.Name = (injection.typeName)
  lazy val field: hydra.core.Field = (injection.field)
  lazy val fname: hydra.core.Name = (field.name)
  lazy val term: hydra.core.Term = (field.term)
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.InferenceResult, hydra.typing.InferenceResult](hydra.inference.inferTypeOfTerm(fcx)(cx)(term)("injected term"))((result: hydra.typing.InferenceResult) =>
    {
    lazy val fcx2: hydra.context.Context = (result.context)
    hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[hydra.core.TypeScheme, hydra.context.Context],
       hydra.typing.InferenceResult](hydra.resolution.requireSchemaType(fcx2)(cx.schemaTypes)(tname))((stRp: Tuple2[hydra.core.TypeScheme,
       hydra.context.Context]) =>
      {
      lazy val schemaType: hydra.core.TypeScheme = hydra.lib.pairs.first[hydra.core.TypeScheme,
         hydra.context.Context](stRp)
      {
        lazy val fcx3: hydra.context.Context = hydra.lib.pairs.second[hydra.core.TypeScheme,
           hydra.context.Context](stRp)
        {
          lazy val svars: Seq[hydra.core.Name] = (schemaType.variables)
          {
            lazy val stype: hydra.core.Type = (schemaType.`type`)
            {
              lazy val iterm: hydra.core.Term = (result.term)
              {
                lazy val ityp: hydra.core.Type = (result.`type`)
                {
                  lazy val isubst: hydra.typing.TypeSubst = (result.subst)
                  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.FieldType],
                     hydra.typing.InferenceResult](hydra.extract.core.unionType(tname)(stype))((sfields: Seq[hydra.core.FieldType]) =>
                    hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Type, hydra.typing.InferenceResult](hydra.resolution.findFieldType(fcx3)(fname)(sfields))((ftyp: hydra.core.Type) =>
                    hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.InferenceResult,
                       hydra.typing.InferenceResult](hydra.inference.mapConstraints(fcx3)(cx)((subst: hydra.typing.TypeSubst) =>
                    hydra.inference.`yield`(fcx3)(hydra.inference.buildTypeApplicationTerm(svars)(hydra.core.Term.inject(hydra.core.Injection(tname,
                       hydra.core.Field(fname, iterm)))))(hydra.resolution.nominalApplication(tname)(hydra.lib.lists.map[hydra.core.Name,
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

def inferTypeOfLambda(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(lambda: hydra.core.Lambda): Either[hydra.errors.Error,
   hydra.typing.InferenceResult] =
  {
  lazy val `var`: hydra.core.Name = (lambda.parameter)
  lazy val body: hydra.core.Term = (lambda.body)
  lazy val vdomResult: Tuple2[hydra.core.Name, hydra.context.Context] = hydra.names.freshName(fcx)
  lazy val vdom: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, hydra.context.Context](vdomResult)
  lazy val fcx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Name, hydra.context.Context](vdomResult)
  lazy val dom: hydra.core.Type = hydra.core.Type.variable(vdom)
  lazy val cx2: hydra.graph.Graph = hydra.inference.extendContext(Seq(Tuple2(`var`,
     hydra.core.TypeScheme(Seq(), dom, None))))(cx)
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.InferenceResult, hydra.typing.InferenceResult](hydra.inference.inferTypeOfTerm(fcx2)(cx2)(body)("lambda body"))((result: hydra.typing.InferenceResult) =>
    {
    lazy val fcx3: hydra.context.Context = (result.context)
    {
      lazy val iterm: hydra.core.Term = (result.term)
      {
        lazy val icod: hydra.core.Type = (result.`type`)
        {
          lazy val isubst: hydra.typing.TypeSubst = (result.subst)
          {
            lazy val rdom: hydra.core.Type = hydra.substitution.substInType(isubst)(dom)
            {
              lazy val rterm: hydra.core.Term = hydra.core.Term.lambda(hydra.core.Lambda(`var`, Some(rdom), iterm))
              {
                lazy val rtype: hydra.core.Type = hydra.core.Type.function(hydra.core.FunctionType(rdom, icod))
                {
                  lazy val vars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.unions[hydra.core.Name](Seq(hydra.variables.freeVariablesInType(rdom),
                     hydra.variables.freeVariablesInType(icod), hydra.inference.freeVariablesInContext(hydra.substitution.substInContext(isubst)(cx2))))
                  {
                    lazy val cx3: hydra.graph.Graph = hydra.substitution.substInContext(isubst)(cx)
                    {
                      lazy val iconstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.substitution.substInClassConstraints(isubst)(result.classConstraints)
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

def inferTypeOfLet(fcx0: hydra.context.Context)(cx: hydra.graph.Graph)(let0: hydra.core.Let): Either[hydra.errors.Error,
   hydra.typing.InferenceResult] =
  {
  lazy val fcx: hydra.context.Context = hydra.context.Context(hydra.lib.lists.cons[scala.Predef.String]("let")(fcx0.trace),
     (fcx0.messages), (fcx0.other))
  lazy val bindings0: Seq[hydra.core.Binding] = (let0.bindings)
  lazy val body0: hydra.core.Term = (let0.body)
  lazy val names: Seq[hydra.core.Name] = hydra.lib.lists.map[hydra.core.Binding, hydra.core.Name]((x: hydra.core.Binding) => (x.name))(bindings0)
  lazy val nameSet: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](names)
  def toPair(binding: hydra.core.Binding): Tuple2[hydra.core.Name, Seq[hydra.core.Name]] =
    {
    lazy val name: hydra.core.Name = (binding.name)
    lazy val term: hydra.core.Term = (binding.term)
    Tuple2(name, hydra.lib.lists.filter[hydra.core.Name]((n: hydra.core.Name) => hydra.lib.sets.member[hydra.core.Name](n)(nameSet))(hydra.lib.sets.toList[hydra.core.Name](hydra.variables.freeVariablesInTerm(term))))
  }
  lazy val adjList: Seq[Tuple2[hydra.core.Name, Seq[hydra.core.Name]]] = hydra.lib.lists.map[hydra.core.Binding,
     Tuple2[hydra.core.Name, Seq[hydra.core.Name]]](toPair)(bindings0)
  lazy val groups: Seq[Seq[hydra.core.Name]] = hydra.sorting.topologicalSortComponents(adjList)
  lazy val bindingMap: Map[hydra.core.Name, hydra.core.Binding] = hydra.lib.maps.fromList[hydra.core.Name,
     hydra.core.Binding](hydra.lib.lists.zip[hydra.core.Name, hydra.core.Binding](names)(bindings0))
  def createLet(e: hydra.core.Term)(group: Seq[hydra.core.Name]): hydra.core.Term =
    hydra.core.Term.let(hydra.core.Let(hydra.lib.maybes.cat[hydra.core.Binding](hydra.lib.lists.map[hydra.core.Name,
       Option[hydra.core.Binding]]((n: hydra.core.Name) =>
    hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Binding](n)(bindingMap))(group)), e))
  lazy val rewrittenLet: hydra.core.Term = hydra.lib.lists.foldl[hydra.core.Term,
     Seq[hydra.core.Name]](createLet)(body0)(hydra.lib.lists.reverse[Seq[hydra.core.Name]](groups))
  def restoreLet(iterm: hydra.core.Term): hydra.core.Term =
    {
    def helper(level: Int)(bins: Seq[hydra.core.Binding])(term: hydra.core.Term): Tuple2[Seq[hydra.core.Binding],
       hydra.core.Term] =
      {
      def nonzero(term2: hydra.core.Term): Tuple2[Seq[hydra.core.Binding], hydra.core.Term] =
        term2 match
        case hydra.core.Term.let(v_Term_let_l) => {
          lazy val bs: Seq[hydra.core.Binding] = (v_Term_let_l.bindings)
          {
            lazy val letBody: hydra.core.Term = (v_Term_let_l.body)
            helper(hydra.lib.math.sub(level)(1))(hydra.lib.lists.concat[hydra.core.Binding](Seq(bs, bins)))(letBody)
          }
        }
      hydra.lib.logic.ifElse[Tuple2[Seq[hydra.core.Binding], hydra.core.Term]](hydra.lib.equality.equal[Int](level)(0))(Tuple2(bins,
         term))(nonzero(term))
    }
    lazy val result: Tuple2[Seq[hydra.core.Binding], hydra.core.Term] = helper(hydra.lib.lists.length[Seq[hydra.core.Name]](groups))(Seq())(iterm)
    lazy val bindingList: Seq[hydra.core.Binding] = hydra.lib.pairs.first[Seq[hydra.core.Binding],
       hydra.core.Term](result)
    lazy val e: hydra.core.Term = hydra.lib.pairs.second[Seq[hydra.core.Binding], hydra.core.Term](result)
    lazy val bindingMap2: Map[hydra.core.Name, hydra.core.Binding] = hydra.lib.maps.fromList[hydra.core.Name,
       hydra.core.Binding](hydra.lib.lists.map[hydra.core.Binding, Tuple2[hydra.core.Name,
       hydra.core.Binding]]((b: hydra.core.Binding) => Tuple2(b.name, b))(bindingList))
    hydra.core.Term.let(hydra.core.Let(hydra.lib.maybes.cat[hydra.core.Binding](hydra.lib.lists.map[hydra.core.Name,
       Option[hydra.core.Binding]]((n: hydra.core.Name) =>
      hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Binding](n)(bindingMap2))(names)), e))
  }
  def rewriteResult(iresult: hydra.typing.InferenceResult): hydra.typing.InferenceResult =
    {
    lazy val fcxR: hydra.context.Context = (iresult.context)
    lazy val iterm: hydra.core.Term = (iresult.term)
    lazy val itype: hydra.core.Type = (iresult.`type`)
    lazy val isubst: hydra.typing.TypeSubst = (iresult.subst)
    lazy val iconstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = (iresult.classConstraints)
    hydra.typing.InferenceResult(restoreLet(iterm), itype, isubst, iconstraints, fcxR)
  }
  lazy val res: Either[hydra.errors.Error, hydra.typing.InferenceResult] = rewrittenLet match
    case hydra.core.Term.let(v_Term_let_l) => hydra.inference.inferTypeOfLetNormalized(fcx)(cx)(v_Term_let_l)
    case _ => hydra.inference.inferTypeOfTerm(fcx)(cx)(rewrittenLet)("empty let term")
  hydra.lib.eithers.map[hydra.typing.InferenceResult, hydra.typing.InferenceResult,
     hydra.errors.Error](rewriteResult)(res)
}

def inferTypeOfLetNormalized(fcx0: hydra.context.Context)(cx0: hydra.graph.Graph)(letTerm: hydra.core.Let): Either[hydra.errors.Error,
   hydra.typing.InferenceResult] =
  {
  lazy val fcx: hydra.context.Context = hydra.context.Context(hydra.lib.lists.cons[scala.Predef.String]("let-normalized")(fcx0.trace),
     (fcx0.messages), (fcx0.other))
  lazy val bins0: Seq[hydra.core.Binding] = (letTerm.bindings)
  lazy val body0: hydra.core.Term = (letTerm.body)
  lazy val bnames: Seq[hydra.core.Name] = hydra.lib.lists.map[hydra.core.Binding,
     hydra.core.Name]((x: hydra.core.Binding) => (x.name))(bins0)
  lazy val bvarsResult: Tuple2[Seq[hydra.core.Name], hydra.context.Context] = hydra.names.freshNames(hydra.lib.lists.length[hydra.core.Binding](bins0))(fcx)
  lazy val bvars: Seq[hydra.core.Name] = hydra.lib.pairs.first[Seq[hydra.core.Name], hydra.context.Context](bvarsResult)
  lazy val fcx2: hydra.context.Context = hydra.lib.pairs.second[Seq[hydra.core.Name],
     hydra.context.Context](bvarsResult)
  lazy val tbins0: Seq[hydra.core.Type] = hydra.lib.lists.map[hydra.core.Name, hydra.core.Type]((x: hydra.core.Name) => hydra.core.Type.variable(x))(bvars)
  lazy val cx1: hydra.graph.Graph = hydra.inference.extendContext(hydra.lib.lists.zip[hydra.core.Name,
     hydra.core.TypeScheme](bnames)(hydra.lib.lists.map[hydra.core.Type, hydra.core.TypeScheme]((t: hydra.core.Type) => hydra.core.TypeScheme(Seq(),
     t, None))(tbins0)))(cx0)
  hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
     Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
     hydra.context.Context], hydra.typing.InferenceResult](hydra.inference.inferTypesOfTemporaryBindings(fcx2)(cx1)(bins0))((irRp: Tuple2[Tuple2[Seq[hydra.core.Term],
     Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
     hydra.core.TypeVariableMetadata]]]], hydra.context.Context]) =>
    {
    lazy val inferredResult: Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
       Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]] = hydra.lib.pairs.first[Tuple2[Seq[hydra.core.Term],
       Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
       hydra.core.TypeVariableMetadata]]]], hydra.context.Context](irRp)
    {
      lazy val fcx3: hydra.context.Context = hydra.lib.pairs.second[Tuple2[Seq[hydra.core.Term],
         Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
         hydra.core.TypeVariableMetadata]]]], hydra.context.Context](irRp)
      {
        lazy val bterms1: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term],
           Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
           hydra.core.TypeVariableMetadata]]]](inferredResult)
        {
          lazy val tbins1: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type],
             Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](hydra.lib.pairs.second[Seq[hydra.core.Term],
             Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
             hydra.core.TypeVariableMetadata]]]](inferredResult))
          {
            lazy val substAndConstraints: Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
               hydra.core.TypeVariableMetadata]] = hydra.lib.pairs.second[Seq[hydra.core.Type],
               Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](hydra.lib.pairs.second[Seq[hydra.core.Term],
               Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
               hydra.core.TypeVariableMetadata]]]](inferredResult))
            {
              lazy val s1: hydra.typing.TypeSubst = hydra.lib.pairs.first[hydra.typing.TypeSubst,
                 Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](substAndConstraints)
              {
                lazy val inferredConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.lib.pairs.second[hydra.typing.TypeSubst,
                   Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](substAndConstraints)
                hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.TypeSubst,
                   hydra.typing.InferenceResult](hydra.lib.eithers.bimap[hydra.errors.UnificationError,
                   hydra.typing.TypeSubst, hydra.errors.Error, hydra.typing.TypeSubst]((_e: hydra.errors.UnificationError) => hydra.errors.Error.unification(_e))((_a: hydra.typing.TypeSubst) => _a)(hydra.unification.unifyTypeLists(fcx3)(cx0.schemaTypes)(hydra.lib.lists.map[hydra.core.Type,
                   hydra.core.Type]((v1: hydra.core.Type) => hydra.substitution.substInType(s1)(v1))(tbins0))(tbins1)("temporary type bindings")))((s2: hydra.typing.TypeSubst) =>
                  hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.TypeSubst,
                     hydra.typing.InferenceResult](hydra.checking.checkTypeSubst(fcx3)(cx0)(s2))((_x: hydra.typing.TypeSubst) =>
                  {
                  lazy val g2base: hydra.graph.Graph = hydra.substitution.substInContext(hydra.substitution.composeTypeSubst(s1)(s2))(cx0)
                  {
                    lazy val constraintsWithS2: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.substitution.substInClassConstraints(s2)(inferredConstraints)
                    {
                      lazy val composedSubst: hydra.typing.TypeSubst = hydra.substitution.composeTypeSubst(s1)(s2)
                      {
                        lazy val originalBindingConstraints: Map[hydra.core.Name,
                           hydra.core.TypeVariableMetadata] = hydra.lib.lists.foldl[Map[hydra.core.Name,
                           hydra.core.TypeVariableMetadata], hydra.core.Binding]((acc: Map[hydra.core.Name,
                           hydra.core.TypeVariableMetadata]) =>
                          (b: hydra.core.Binding) =>
                          hydra.lib.maybes.maybe[Map[hydra.core.Name, hydra.core.TypeVariableMetadata],
                             hydra.core.TypeScheme](acc)((ts: hydra.core.TypeScheme) =>
                          hydra.lib.maybes.maybe[Map[hydra.core.Name, hydra.core.TypeVariableMetadata],
                             Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](acc)((c: Map[hydra.core.Name,
                             hydra.core.TypeVariableMetadata]) => hydra.inference.mergeClassConstraints(acc)(c))(ts.constraints))(b.`type`))(hydra.lib.maps.empty[hydra.core.Name,
                             hydra.core.TypeVariableMetadata])(bins0)
                        {
                          lazy val originalConstraintsSubst: Map[hydra.core.Name,
                             hydra.core.TypeVariableMetadata] = hydra.substitution.substInClassConstraints(composedSubst)(originalBindingConstraints)
                          {
                            lazy val allInferredConstraints: Map[hydra.core.Name,
                               hydra.core.TypeVariableMetadata] = hydra.inference.mergeClassConstraints(constraintsWithS2)(originalConstraintsSubst)
                            {
                              lazy val mergedConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.inference.mergeClassConstraints(g2base.classConstraints)(allInferredConstraints)
                              {
                                lazy val g2: hydra.graph.Graph = hydra.graph.Graph(g2base.boundTerms,
                                   (g2base.boundTypes), mergedConstraints, (g2base.lambdaVariables),
                                   (g2base.metadata), (g2base.primitives), (g2base.schemaTypes),
                                   (g2base.typeVariables))
                                {
                                  lazy val bterms1Subst: Seq[hydra.core.Term] = hydra.lib.lists.map[hydra.core.Term,
                                     hydra.core.Term]((v1: hydra.core.Term) => hydra.substitution.substTypesInTerm(s2)(v1))(bterms1)
                                  {
                                    lazy val tsbins1: Seq[Tuple2[hydra.core.Name,
                                       hydra.core.TypeScheme]] = hydra.lib.lists.zip[hydra.core.Name,
                                       hydra.core.TypeScheme](bnames)(hydra.lib.lists.map[hydra.core.Type,
                                       hydra.core.TypeScheme]((t: hydra.core.Type) =>
                                      hydra.inference.generalize(g2)(hydra.substitution.substInType(s2)(t)))(tbins1))
                                    hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.InferenceResult,
                                       hydra.typing.InferenceResult](hydra.inference.inferTypeOfTerm(fcx3)(hydra.inference.extendContext(tsbins1)(g2))(body0)("let body"))((bodyResult: hydra.typing.InferenceResult) =>
                                      {
                                      lazy val fcx4: hydra.context.Context = (bodyResult.context)
                                      {
                                        lazy val body1: hydra.core.Term = (bodyResult.term)
                                        {
                                          lazy val tbody: hydra.core.Type = (bodyResult.`type`)
                                          {
                                            lazy val sbody: hydra.typing.TypeSubst = (bodyResult.subst)
                                            {
                                              lazy val st1: hydra.typing.TermSubst = hydra.lib.maps.fromList[hydra.core.Name,
                                                 hydra.core.Term](hydra.lib.lists.map[Tuple2[hydra.core.Name,
                                                 hydra.core.TypeScheme], Tuple2[hydra.core.Name,
                                                 hydra.core.Term]]((pair: Tuple2[hydra.core.Name,
                                                 hydra.core.TypeScheme]) =>
                                                {
                                                lazy val name: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name,
                                                   hydra.core.TypeScheme](pair)
                                                {
                                                  lazy val ts: hydra.core.TypeScheme = hydra.lib.pairs.second[hydra.core.Name,
                                                     hydra.core.TypeScheme](pair)
                                                  Tuple2(name, hydra.inference.buildTypeApplicationTerm(ts.variables)(hydra.core.Term.variable(name)))
                                                }
                                              })(tsbins1))
                                              {
                                                def createBinding(bindingPair: Tuple2[Tuple2[hydra.core.Name,
                                                   hydra.core.TypeScheme], hydra.core.Term]): hydra.core.Binding =
                                                  {
                                                  lazy val nameTsPair: Tuple2[hydra.core.Name,
                                                     hydra.core.TypeScheme] = hydra.lib.pairs.first[Tuple2[hydra.core.Name,
                                                     hydra.core.TypeScheme], hydra.core.Term](bindingPair)
                                                  lazy val term: hydra.core.Term = hydra.lib.pairs.second[Tuple2[hydra.core.Name,
                                                     hydra.core.TypeScheme], hydra.core.Term](bindingPair)
                                                  lazy val name: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name,
                                                     hydra.core.TypeScheme](nameTsPair)
                                                  lazy val ts: hydra.core.TypeScheme = hydra.lib.pairs.second[hydra.core.Name,
                                                     hydra.core.TypeScheme](nameTsPair)
                                                  lazy val finalTs: hydra.core.TypeScheme = hydra.substitution.substInTypeScheme(sbody)(ts)
                                                  lazy val typeLambdaTerm: hydra.core.Term = hydra.lib.lists.foldl[hydra.core.Term,
                                                     hydra.core.Name]((b: hydra.core.Term) =>
                                                    (v: hydra.core.Name) => hydra.core.Term.typeLambda(hydra.core.TypeLambda(v,
                                                       b)))(hydra.substitution.substituteInTerm(st1)(term))(hydra.lib.lists.reverse[hydra.core.Name](finalTs.variables))
                                                  hydra.core.Binding(name, hydra.substitution.substTypesInTerm(hydra.substitution.composeTypeSubst(sbody)(s2))(typeLambdaTerm),
                                                     Some(finalTs))
                                                }
                                                {
                                                  lazy val bins1: Seq[hydra.core.Binding] = hydra.lib.lists.map[Tuple2[Tuple2[hydra.core.Name,
                                                     hydra.core.TypeScheme], hydra.core.Term],
                                                     hydra.core.Binding](createBinding)(hydra.lib.lists.zip[Tuple2[hydra.core.Name,
                                                     hydra.core.TypeScheme], hydra.core.Term](tsbins1)(bterms1Subst))
                                                  {
                                                    lazy val bodyConstraints: Map[hydra.core.Name,
                                                       hydra.core.TypeVariableMetadata] = hydra.substitution.substInClassConstraints(sbody)(bodyResult.classConstraints)
                                                    {
                                                      lazy val bindingConstraintsSubst: Map[hydra.core.Name,
                                                         hydra.core.TypeVariableMetadata] = hydra.substitution.substInClassConstraints(sbody)(constraintsWithS2)
                                                      {
                                                        lazy val allConstraints: Map[hydra.core.Name,
                                                           hydra.core.TypeVariableMetadata] = hydra.inference.mergeClassConstraints(bindingConstraintsSubst)(bodyConstraints)
                                                        Right(hydra.typing.InferenceResult(hydra.core.Term.let(hydra.core.Let(bins1,
                                                           body1)), tbody, hydra.substitution.composeTypeSubstList(Seq(s1,
                                                           s2, sbody)), allConstraints,
                                                           fcx4))
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

def inferTypeOfList(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(v1: Seq[hydra.core.Term]): Either[hydra.errors.Error,
   hydra.typing.InferenceResult] =
  hydra.inference.inferTypeOfCollection(fcx)(cx)((x: hydra.core.Type) => hydra.core.Type.list(x))((x: Seq[hydra.core.Term]) => hydra.core.Term.list(x))("list element")(hydra.lib.sets.empty[hydra.core.Name])(v1)

def inferTypeOfLiteral(fcx: hydra.context.Context)(lit: hydra.core.Literal): hydra.typing.InferenceResult =
  hydra.typing.InferenceResult(hydra.core.Term.literal(lit), hydra.core.Type.literal(hydra.reflect.literalType(lit)),
     hydra.substitution.idTypeSubst, hydra.lib.maps.empty[hydra.core.Name, hydra.core.TypeVariableMetadata],
     fcx)

def inferTypeOfMap(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(m: Map[hydra.core.Term,
   hydra.core.Term]): Either[hydra.errors.Error, hydra.typing.InferenceResult] =
  {
  lazy val kvarResult: Tuple2[hydra.core.Name, hydra.context.Context] = hydra.names.freshName(fcx)
  lazy val kvar: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, hydra.context.Context](kvarResult)
  lazy val fcx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Name, hydra.context.Context](kvarResult)
  lazy val vvarResult: Tuple2[hydra.core.Name, hydra.context.Context] = hydra.names.freshName(fcx2)
  lazy val vvar: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, hydra.context.Context](vvarResult)
  lazy val fcx3: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Name, hydra.context.Context](vvarResult)
  lazy val keyConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.lib.maps.singleton[hydra.core.Name,
     hydra.core.TypeVariableMetadata](kvar)(hydra.core.TypeVariableMetadata(hydra.lib.sets.singleton[hydra.core.Name]("ordering")))
  hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.typing.InferenceResult]](hydra.lib.maps.`null`[hydra.core.Term,
     hydra.core.Term](m))(Right(hydra.inference.yieldWithConstraints(fcx3)(hydra.inference.buildTypeApplicationTerm(Seq(kvar,
     vvar))(hydra.core.Term.map(hydra.lib.maps.empty[hydra.core.Term, hydra.core.Term])))(hydra.core.Type.map(hydra.core.MapType(hydra.core.Type.variable(kvar),
     hydra.core.Type.variable(vvar))))(hydra.substitution.idTypeSubst)(keyConstraints)))(hydra.lib.eithers.bind[hydra.errors.Error,
     Tuple2[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
     Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]], hydra.context.Context],
     hydra.typing.InferenceResult](hydra.inference.inferMany(fcx3)(cx)(hydra.lib.lists.map[hydra.core.Term,
     Tuple2[hydra.core.Term, scala.Predef.String]]((k: hydra.core.Term) => Tuple2(k,
     "map key"))(hydra.lib.maps.keys[hydra.core.Term, hydra.core.Term](m))))((kRp: Tuple2[Tuple2[Seq[hydra.core.Term],
     Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
     hydra.core.TypeVariableMetadata]]]], hydra.context.Context]) =>
    {
    lazy val kResults: Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
       Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]] = hydra.lib.pairs.first[Tuple2[Seq[hydra.core.Term],
       Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
       hydra.core.TypeVariableMetadata]]]], hydra.context.Context](kRp)
    {
      lazy val fcx4: hydra.context.Context = hydra.lib.pairs.second[Tuple2[Seq[hydra.core.Term],
         Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
         hydra.core.TypeVariableMetadata]]]], hydra.context.Context](kRp)
      {
        lazy val kterms: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term],
           Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
           hydra.core.TypeVariableMetadata]]]](kResults)
        {
          lazy val ktypes: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type],
             Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](hydra.lib.pairs.second[Seq[hydra.core.Term],
             Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
             hydra.core.TypeVariableMetadata]]]](kResults))
          {
            lazy val ksubst: hydra.typing.TypeSubst = hydra.lib.pairs.first[hydra.typing.TypeSubst,
               Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](hydra.lib.pairs.second[Seq[hydra.core.Type],
               Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](hydra.lib.pairs.second[Seq[hydra.core.Term],
               Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
               hydra.core.TypeVariableMetadata]]]](kResults)))
            {
              lazy val kElemConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.lib.pairs.second[hydra.typing.TypeSubst,
                 Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](hydra.lib.pairs.second[Seq[hydra.core.Type],
                 Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](hydra.lib.pairs.second[Seq[hydra.core.Term],
                 Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                 hydra.core.TypeVariableMetadata]]]](kResults)))
              hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[Tuple2[Seq[hydra.core.Term],
                 Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                 hydra.core.TypeVariableMetadata]]]], hydra.context.Context], hydra.typing.InferenceResult](hydra.inference.inferMany(fcx4)(hydra.substitution.substInContext(ksubst)(cx))(hydra.lib.lists.map[hydra.core.Term,
                 Tuple2[hydra.core.Term, scala.Predef.String]]((v: hydra.core.Term) => Tuple2(v,
                 "map value"))(hydra.lib.maps.elems[hydra.core.Term, hydra.core.Term](m))))((vRp: Tuple2[Tuple2[Seq[hydra.core.Term],
                 Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                 hydra.core.TypeVariableMetadata]]]], hydra.context.Context]) =>
                {
                lazy val vResults: Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
                   Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]] = hydra.lib.pairs.first[Tuple2[Seq[hydra.core.Term],
                   Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                   hydra.core.TypeVariableMetadata]]]], hydra.context.Context](vRp)
                {
                  lazy val fcx5: hydra.context.Context = hydra.lib.pairs.second[Tuple2[Seq[hydra.core.Term],
                     Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                     hydra.core.TypeVariableMetadata]]]], hydra.context.Context](vRp)
                  {
                    lazy val vterms: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term],
                       Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                       Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](vResults)
                    {
                      lazy val vtypes: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type],
                         Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](hydra.lib.pairs.second[Seq[hydra.core.Term],
                         Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                         Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](vResults))
                      {
                        lazy val vsubst: hydra.typing.TypeSubst = hydra.lib.pairs.first[hydra.typing.TypeSubst,
                           Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](hydra.lib.pairs.second[Seq[hydra.core.Type],
                           Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](hydra.lib.pairs.second[Seq[hydra.core.Term],
                           Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                           Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](vResults)))
                        {
                          lazy val vElemConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.lib.pairs.second[hydra.typing.TypeSubst,
                             Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](hydra.lib.pairs.second[Seq[hydra.core.Type],
                             Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](hydra.lib.pairs.second[Seq[hydra.core.Term],
                             Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                             Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](vResults)))
                          {
                            lazy val kcons: Seq[hydra.typing.TypeConstraint] = hydra.lib.lists.map[hydra.core.Type,
                               hydra.typing.TypeConstraint]((t: hydra.core.Type) =>
                              hydra.typing.TypeConstraint(hydra.core.Type.variable(kvar), t, "map key"))(ktypes)
                            {
                              lazy val vcons: Seq[hydra.typing.TypeConstraint] = hydra.lib.lists.map[hydra.core.Type,
                                 hydra.typing.TypeConstraint]((t: hydra.core.Type) =>
                                hydra.typing.TypeConstraint(hydra.core.Type.variable(vvar), t, "map value"))(vtypes)
                              {
                                lazy val allMapConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.inference.mergeClassConstraints(keyConstraints)(hydra.inference.mergeClassConstraints(kElemConstraints)(vElemConstraints))
                                hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.InferenceResult,
                                   hydra.typing.InferenceResult](hydra.inference.mapConstraints(fcx5)(cx)((subst: hydra.typing.TypeSubst) =>
                                  hydra.inference.yieldWithConstraints(fcx5)(hydra.core.Term.map(hydra.lib.maps.fromList[hydra.core.Term,
                                     hydra.core.Term](hydra.lib.lists.zip[hydra.core.Term,
                                     hydra.core.Term](kterms)(vterms))))(hydra.core.Type.map(hydra.core.MapType(hydra.core.Type.variable(kvar),
                                     hydra.core.Type.variable(vvar))))(hydra.substitution.composeTypeSubstList(Seq(ksubst,
                                     vsubst, subst)))(hydra.substitution.substInClassConstraints(subst)(allMapConstraints)))(hydra.lib.lists.concat[hydra.typing.TypeConstraint](Seq(kcons,
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

def inferTypeOfOptional(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(m: Option[hydra.core.Term]): Either[hydra.errors.Error,
   hydra.typing.InferenceResult] =
  {
  def trmCons(terms: Seq[hydra.core.Term]): hydra.core.Term = hydra.core.Term.maybe(hydra.lib.lists.maybeHead[hydra.core.Term](terms))
  hydra.inference.inferTypeOfCollection(fcx)(cx)((x: hydra.core.Type) => hydra.core.Type.maybe(x))(trmCons)("optional element")(hydra.lib.sets.empty[hydra.core.Name])(hydra.lib.maybes.maybe[Seq[hydra.core.Term],
     hydra.core.Term](Seq())(hydra.lib.lists.singleton[hydra.core.Term])(m))
}

def inferTypeOfPair(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(p: Tuple2[hydra.core.Term,
   hydra.core.Term]): Either[hydra.errors.Error, hydra.typing.InferenceResult] =
  hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
     Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
     hydra.context.Context], hydra.typing.InferenceResult](hydra.inference.inferMany(fcx)(cx)(Seq(Tuple2(hydra.lib.pairs.first[hydra.core.Term,
     hydra.core.Term](p), "pair first element"), Tuple2(hydra.lib.pairs.second[hydra.core.Term,
     hydra.core.Term](p), "pair second element"))))((rp: Tuple2[Tuple2[Seq[hydra.core.Term],
     Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
     hydra.core.TypeVariableMetadata]]]], hydra.context.Context]) =>
  {
  lazy val results: Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
     Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]] = hydra.lib.pairs.first[Tuple2[Seq[hydra.core.Term],
     Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
     hydra.core.TypeVariableMetadata]]]], hydra.context.Context](rp)
  {
    lazy val fcx2: hydra.context.Context = hydra.lib.pairs.second[Tuple2[Seq[hydra.core.Term],
       Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
       hydra.core.TypeVariableMetadata]]]], hydra.context.Context](rp)
    {
      lazy val iterms: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term],
         Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
         hydra.core.TypeVariableMetadata]]]](results)
      {
        lazy val itypes: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type],
           Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](hydra.lib.pairs.second[Seq[hydra.core.Term],
           Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
           hydra.core.TypeVariableMetadata]]]](results))
        {
          lazy val isubst: hydra.typing.TypeSubst = hydra.lib.pairs.first[hydra.typing.TypeSubst,
             Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](hydra.lib.pairs.second[Seq[hydra.core.Type],
             Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](hydra.lib.pairs.second[Seq[hydra.core.Term],
             Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
             hydra.core.TypeVariableMetadata]]]](results)))
          {
            lazy val pairElemConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.lib.pairs.second[hydra.typing.TypeSubst,
               Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](hydra.lib.pairs.second[Seq[hydra.core.Type],
               Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](hydra.lib.pairs.second[Seq[hydra.core.Term],
               Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
               hydra.core.TypeVariableMetadata]]]](results)))
            {
              def arityErr[T0]: Either[hydra.errors.Error, T0] =
                Left(hydra.errors.Error.other("inferTypeOfPair: expected 2 inferred terms/types"))
              hydra.lib.maybes.maybe[Either[hydra.errors.Error, hydra.typing.InferenceResult],
                 Tuple2[hydra.core.Term, Seq[hydra.core.Term]]](arityErr)((termsUc: Tuple2[hydra.core.Term,
                 Seq[hydra.core.Term]]) =>
                {
                lazy val ifst: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, Seq[hydra.core.Term]](termsUc)
                hydra.lib.maybes.maybe[Either[hydra.errors.Error, hydra.typing.InferenceResult],
                   Tuple2[hydra.core.Term, Seq[hydra.core.Term]]](arityErr)((termsUc2: Tuple2[hydra.core.Term,
                   Seq[hydra.core.Term]]) =>
                  {
                  lazy val isnd: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term,
                     Seq[hydra.core.Term]](termsUc2)
                  hydra.lib.maybes.maybe[Either[hydra.errors.Error, hydra.typing.InferenceResult],
                     Tuple2[hydra.core.Type, Seq[hydra.core.Type]]](arityErr)((typesUc: Tuple2[hydra.core.Type,
                     Seq[hydra.core.Type]]) =>
                    {
                    lazy val tyFst: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type,
                       Seq[hydra.core.Type]](typesUc)
                    hydra.lib.maybes.maybe[Either[hydra.errors.Error, hydra.typing.InferenceResult],
                       Tuple2[hydra.core.Type, Seq[hydra.core.Type]]](arityErr)((typesUc2: Tuple2[hydra.core.Type,
                       Seq[hydra.core.Type]]) =>
                      {
                      lazy val tySnd: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type,
                         Seq[hydra.core.Type]](typesUc2)
                      {
                        lazy val pairTerm: hydra.core.Term = hydra.core.Term.pair(Tuple2(ifst, isnd))
                        {
                          lazy val termWithTypes: hydra.core.Term = hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(pairTerm,
                             tyFst)), tySnd))
                          Right(hydra.inference.yieldWithConstraints(fcx2)(termWithTypes)(hydra.core.Type.pair(hydra.core.PairType(tyFst,
                             tySnd)))(isubst)(pairElemConstraints))
                        }
                      }
                    })(hydra.lib.lists.uncons[hydra.core.Type](hydra.lib.pairs.second[hydra.core.Type,
                       Seq[hydra.core.Type]](typesUc)))
                  })(hydra.lib.lists.uncons[hydra.core.Type](itypes))
                })(hydra.lib.lists.uncons[hydra.core.Term](hydra.lib.pairs.second[hydra.core.Term,
                   Seq[hydra.core.Term]](termsUc)))
              })(hydra.lib.lists.uncons[hydra.core.Term](iterms))
            }
          }
        }
      }
    }
  }
})

def inferTypeOfPrimitive(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(name: hydra.core.Name): Either[hydra.errors.Error,
   hydra.typing.InferenceResult] =
  hydra.lib.maybes.maybe[Either[hydra.errors.Error, hydra.typing.InferenceResult],
     hydra.core.TypeScheme](Left(hydra.errors.Error.resolution(hydra.errors.ResolutionError.noSuchPrimitive(hydra.errors.NoSuchPrimitiveError(name)))))((scheme: hydra.core.TypeScheme) =>
  {
  lazy val tsResult: Tuple2[hydra.core.TypeScheme, hydra.context.Context] = hydra.resolution.instantiateTypeScheme(fcx)(scheme)
  {
    lazy val ts: hydra.core.TypeScheme = hydra.lib.pairs.first[hydra.core.TypeScheme, hydra.context.Context](tsResult)
    {
      lazy val fcx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.TypeScheme,
         hydra.context.Context](tsResult)
      {
        lazy val constraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.lib.maybes.fromMaybe[Map[hydra.core.Name,
           hydra.core.TypeVariableMetadata]](hydra.lib.maps.empty[hydra.core.Name,
           hydra.core.TypeVariableMetadata])(ts.constraints)
        Right(hydra.inference.yieldCheckedWithConstraints(fcx2)(hydra.inference.buildTypeApplicationTerm(ts.variables)(hydra.core.Term.variable(name)))(ts.`type`)(hydra.substitution.idTypeSubst)(constraints))
      }
    }
  }
})(hydra.lib.maybes.map[hydra.graph.Primitive, hydra.core.TypeScheme]((x: hydra.graph.Primitive) => (x.`type`))(hydra.lib.maps.lookup[hydra.core.Name,
   hydra.graph.Primitive](name)(cx.primitives)))

def inferTypeOfProjection(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(proj: hydra.core.Projection): Either[hydra.errors.Error,
   hydra.typing.InferenceResult] =
  {
  lazy val tname: hydra.core.Name = (proj.typeName)
  lazy val fname: hydra.core.Name = (proj.field)
  hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[hydra.core.TypeScheme, hydra.context.Context],
     hydra.typing.InferenceResult](hydra.resolution.requireSchemaType(fcx)(cx.schemaTypes)(tname))((stRp: Tuple2[hydra.core.TypeScheme,
     hydra.context.Context]) =>
    {
    lazy val schemaType: hydra.core.TypeScheme = hydra.lib.pairs.first[hydra.core.TypeScheme,
       hydra.context.Context](stRp)
    {
      lazy val fcx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.TypeScheme, hydra.context.Context](stRp)
      {
        lazy val svars: Seq[hydra.core.Name] = (schemaType.variables)
        {
          lazy val stype: hydra.core.Type = (schemaType.`type`)
          hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.FieldType], hydra.typing.InferenceResult](hydra.extract.core.recordType(tname)(stype))((sfields: Seq[hydra.core.FieldType]) =>
            hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Type, hydra.typing.InferenceResult](hydra.resolution.findFieldType(fcx2)(fname)(sfields))((ftyp: hydra.core.Type) =>
            Right(hydra.inference.`yield`(fcx2)(hydra.inference.buildTypeApplicationTerm(svars)(hydra.core.Term.project(hydra.core.Projection(tname,
               fname))))(hydra.core.Type.function(hydra.core.FunctionType(hydra.resolution.nominalApplication(tname)(hydra.lib.lists.map[hydra.core.Name,
               hydra.core.Type]((x: hydra.core.Name) => hydra.core.Type.variable(x))(svars)),
               ftyp)))(hydra.substitution.idTypeSubst))))
        }
      }
    }
  })
}

def inferTypeOfRecord(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(record: hydra.core.Record): Either[hydra.errors.Error,
   hydra.typing.InferenceResult] =
  {
  lazy val tname: hydra.core.Name = (record.typeName)
  lazy val fields: Seq[hydra.core.Field] = (record.fields)
  lazy val fnames: Seq[hydra.core.Name] = hydra.lib.lists.map[hydra.core.Field, hydra.core.Name]((x: hydra.core.Field) => (x.name))(fields)
  hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[hydra.core.TypeScheme, hydra.context.Context],
     hydra.typing.InferenceResult](hydra.resolution.requireSchemaType(fcx)(cx.schemaTypes)(tname))((stRp: Tuple2[hydra.core.TypeScheme,
     hydra.context.Context]) =>
    {
    lazy val schemaType: hydra.core.TypeScheme = hydra.lib.pairs.first[hydra.core.TypeScheme,
       hydra.context.Context](stRp)
    {
      lazy val fcx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.TypeScheme, hydra.context.Context](stRp)
      hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[Tuple2[Seq[hydra.core.Term],
         Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
         hydra.core.TypeVariableMetadata]]]], hydra.context.Context], hydra.typing.InferenceResult](hydra.inference.inferMany(fcx2)(cx)(hydra.lib.lists.map[hydra.core.Field,
         Tuple2[hydra.core.Term, scala.Predef.String]]((f: hydra.core.Field) => Tuple2(f.term,
         hydra.lib.strings.cat2("field ")(f.name)))(fields)))((rp: Tuple2[Tuple2[Seq[hydra.core.Term],
         Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
         hydra.core.TypeVariableMetadata]]]], hydra.context.Context]) =>
        {
        lazy val results: Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
           Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]] = hydra.lib.pairs.first[Tuple2[Seq[hydra.core.Term],
           Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
           hydra.core.TypeVariableMetadata]]]], hydra.context.Context](rp)
        {
          lazy val fcx3: hydra.context.Context = hydra.lib.pairs.second[Tuple2[Seq[hydra.core.Term],
             Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
             hydra.core.TypeVariableMetadata]]]], hydra.context.Context](rp)
          {
            lazy val svars: Seq[hydra.core.Name] = (schemaType.variables)
            {
              lazy val stype: hydra.core.Type = (schemaType.`type`)
              {
                lazy val iterms: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term],
                   Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                   hydra.core.TypeVariableMetadata]]]](results)
                {
                  lazy val itypes: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type],
                     Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](hydra.lib.pairs.second[Seq[hydra.core.Term],
                     Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                     hydra.core.TypeVariableMetadata]]]](results))
                  {
                    lazy val isubst: hydra.typing.TypeSubst = hydra.lib.pairs.first[hydra.typing.TypeSubst,
                       Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](hydra.lib.pairs.second[Seq[hydra.core.Type],
                       Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](hydra.lib.pairs.second[Seq[hydra.core.Term],
                       Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                       Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](results)))
                    {
                      lazy val recElemConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.lib.pairs.second[hydra.typing.TypeSubst,
                         Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](hydra.lib.pairs.second[Seq[hydra.core.Type],
                         Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]](hydra.lib.pairs.second[Seq[hydra.core.Term],
                         Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                         Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](results)))
                      {
                        lazy val ityp: hydra.core.Type = hydra.core.Type.record(hydra.lib.lists.zipWith[hydra.core.Name,
                           hydra.core.Type, hydra.core.FieldType]((n: hydra.core.Name) => (t: hydra.core.Type) => hydra.core.FieldType(n,
                           t))(fnames)(itypes))
                        hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.InferenceResult,
                           hydra.typing.InferenceResult](hydra.inference.mapConstraints(fcx3)(cx)((subst: hydra.typing.TypeSubst) =>
                          hydra.inference.yieldWithConstraints(fcx3)(hydra.inference.buildTypeApplicationTerm(svars)(hydra.core.Term.record(hydra.core.Record(tname,
                             hydra.lib.lists.zipWith[hydra.core.Name, hydra.core.Term,
                             hydra.core.Field]((n: hydra.core.Name) => (t: hydra.core.Term) => hydra.core.Field(n,
                             t))(fnames)(iterms)))))(hydra.resolution.nominalApplication(tname)(hydra.lib.lists.map[hydra.core.Name,
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

def inferTypeOfSet(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(s: scala.collection.immutable.Set[hydra.core.Term]): Either[hydra.errors.Error,
   hydra.typing.InferenceResult] =
  hydra.inference.inferTypeOfCollection(fcx)(cx)((x: hydra.core.Type) => hydra.core.Type.set(x))((terms: Seq[hydra.core.Term]) =>
  hydra.core.Term.set(hydra.lib.sets.fromList[hydra.core.Term](terms)))("set element")(hydra.lib.sets.singleton[hydra.core.Name]("ordering"))(hydra.lib.sets.toList[hydra.core.Term](s))

def inferTypeOfTerm(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(term: hydra.core.Term)(desc: scala.Predef.String): Either[hydra.errors.Error,
   hydra.typing.InferenceResult] =
  {
  lazy val fcx2: hydra.context.Context = hydra.context.Context(hydra.lib.lists.cons[scala.Predef.String](desc)(fcx.trace),
     (fcx.messages), (fcx.other))
  term match
    case hydra.core.Term.annotated(v_Term_annotated_a) => hydra.inference.inferTypeOfAnnotatedTerm(fcx2)(cx)(v_Term_annotated_a)
    case hydra.core.Term.application(v_Term_application_a) => hydra.inference.inferTypeOfApplication(fcx2)(cx)(v_Term_application_a)
    case hydra.core.Term.cases(v_Term_cases_c) => hydra.inference.inferTypeOfCaseStatement(fcx2)(cx)(v_Term_cases_c)
    case hydra.core.Term.either(v_Term_either_e) => hydra.inference.inferTypeOfEither(fcx2)(cx)(v_Term_either_e)
    case hydra.core.Term.lambda(v_Term_lambda_l) => hydra.inference.inferTypeOfLambda(fcx2)(cx)(v_Term_lambda_l)
    case hydra.core.Term.let(v_Term_let_l) => hydra.inference.inferTypeOfLet(fcx2)(cx)(v_Term_let_l)
    case hydra.core.Term.list(v_Term_list_els) => hydra.inference.inferTypeOfList(fcx2)(cx)(v_Term_list_els)
    case hydra.core.Term.literal(v_Term_literal_l) => Right(hydra.inference.inferTypeOfLiteral(fcx2)(v_Term_literal_l))
    case hydra.core.Term.map(v_Term_map_m) => hydra.inference.inferTypeOfMap(fcx2)(cx)(v_Term_map_m)
    case hydra.core.Term.maybe(v_Term_maybe_m) => hydra.inference.inferTypeOfOptional(fcx2)(cx)(v_Term_maybe_m)
    case hydra.core.Term.pair(v_Term_pair_p) => hydra.inference.inferTypeOfPair(fcx2)(cx)(v_Term_pair_p)
    case hydra.core.Term.project(v_Term_project_p) => hydra.inference.inferTypeOfProjection(fcx2)(cx)(v_Term_project_p)
    case hydra.core.Term.record(v_Term_record_r) => hydra.inference.inferTypeOfRecord(fcx2)(cx)(v_Term_record_r)
    case hydra.core.Term.set(v_Term_set_s) => hydra.inference.inferTypeOfSet(fcx2)(cx)(v_Term_set_s)
    case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => hydra.inference.inferTypeOfTypeApplication(fcx2)(cx)(v_Term_typeApplication_tt)
    case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => hydra.inference.inferTypeOfTypeLambda(fcx2)(cx)(v_Term_typeLambda_ta)
    case hydra.core.Term.inject(v_Term_inject_i) => hydra.inference.inferTypeOfInjection(fcx2)(cx)(v_Term_inject_i)
    case hydra.core.Term.unit => Right(hydra.inference.inferTypeOfUnit(fcx2))
    case hydra.core.Term.unwrap(v_Term_unwrap_tname) => hydra.inference.inferTypeOfUnwrap(fcx2)(cx)(v_Term_unwrap_tname)
    case hydra.core.Term.variable(v_Term_variable_name) => hydra.inference.inferTypeOfVariable(fcx2)(cx)(v_Term_variable_name)
    case hydra.core.Term.wrap(v_Term_wrap_w) => hydra.inference.inferTypeOfWrappedTerm(fcx2)(cx)(v_Term_wrap_w)
}

def inferTypeOfTypeApplication(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(tt: hydra.core.TypeApplicationTerm): Either[hydra.errors.Error,
   hydra.typing.InferenceResult] = hydra.inference.inferTypeOfTerm(fcx)(cx)(tt.body)("type application term")

def inferTypeOfTypeLambda(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(ta: hydra.core.TypeLambda): Either[hydra.errors.Error,
   hydra.typing.InferenceResult] = hydra.inference.inferTypeOfTerm(fcx)(cx)(ta.body)("type abstraction")

def inferTypeOfUnit(fcx: hydra.context.Context): hydra.typing.InferenceResult =
  hydra.typing.InferenceResult(hydra.core.Term.unit, hydra.core.Type.unit, hydra.substitution.idTypeSubst,
     hydra.lib.maps.empty[hydra.core.Name, hydra.core.TypeVariableMetadata], fcx)

def inferTypeOfUnwrap(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(tname: hydra.core.Name): Either[hydra.errors.Error,
   hydra.typing.InferenceResult] =
  hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[hydra.core.TypeScheme, hydra.context.Context],
     hydra.typing.InferenceResult](hydra.resolution.requireSchemaType(fcx)(cx.schemaTypes)(tname))((stRp: Tuple2[hydra.core.TypeScheme,
     hydra.context.Context]) =>
  {
  lazy val schemaType: hydra.core.TypeScheme = hydra.lib.pairs.first[hydra.core.TypeScheme, hydra.context.Context](stRp)
  {
    lazy val fcx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.TypeScheme, hydra.context.Context](stRp)
    {
      lazy val svars: Seq[hydra.core.Name] = (schemaType.variables)
      {
        lazy val stype: hydra.core.Type = (schemaType.`type`)
        hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Type, hydra.typing.InferenceResult](hydra.extract.core.wrappedType(tname)(stype))((wtyp: hydra.core.Type) =>
          Right(hydra.inference.`yield`(fcx2)(hydra.inference.buildTypeApplicationTerm(svars)(hydra.core.Term.unwrap(tname)))(hydra.core.Type.function(hydra.core.FunctionType(hydra.resolution.nominalApplication(tname)(hydra.lib.lists.map[hydra.core.Name,
             hydra.core.Type]((x: hydra.core.Name) => hydra.core.Type.variable(x))(svars)),
             wtyp)))(hydra.substitution.idTypeSubst)))
      }
    }
  }
})

def inferTypeOfVariable(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(name: hydra.core.Name): Either[hydra.errors.Error,
   hydra.typing.InferenceResult] =
  hydra.lib.maybes.maybe[Either[hydra.errors.Error, hydra.typing.InferenceResult],
     hydra.core.TypeScheme](hydra.lib.maybes.maybe[Either[hydra.errors.Error, hydra.typing.InferenceResult],
     hydra.core.TypeScheme](Left(hydra.errors.Error.resolution(hydra.errors.ResolutionError.noSuchBinding(hydra.errors.NoSuchBindingError(name)))))((scheme: hydra.core.TypeScheme) =>
  {
  lazy val tsResult: Tuple2[hydra.core.TypeScheme, hydra.context.Context] = hydra.resolution.instantiateTypeScheme(fcx)(scheme)
  {
    lazy val ts: hydra.core.TypeScheme = hydra.lib.pairs.first[hydra.core.TypeScheme, hydra.context.Context](tsResult)
    {
      lazy val fcx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.TypeScheme,
         hydra.context.Context](tsResult)
      {
        lazy val constraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.lib.maybes.fromMaybe[Map[hydra.core.Name,
           hydra.core.TypeVariableMetadata]](hydra.lib.maps.empty[hydra.core.Name,
           hydra.core.TypeVariableMetadata])(ts.constraints)
        Right(hydra.inference.yieldCheckedWithConstraints(fcx2)(hydra.inference.buildTypeApplicationTerm(ts.variables)(hydra.core.Term.variable(name)))(ts.`type`)(hydra.substitution.idTypeSubst)(constraints))
      }
    }
  }
})(hydra.lib.maybes.map[hydra.graph.Primitive, hydra.core.TypeScheme]((x: hydra.graph.Primitive) => (x.`type`))(hydra.lib.maps.lookup[hydra.core.Name,
   hydra.graph.Primitive](name)(cx.primitives))))((scheme: hydra.core.TypeScheme) =>
  {
  lazy val tsResult: Tuple2[hydra.core.TypeScheme, hydra.context.Context] = hydra.resolution.instantiateTypeScheme(fcx)(scheme)
  {
    lazy val ts: hydra.core.TypeScheme = hydra.lib.pairs.first[hydra.core.TypeScheme, hydra.context.Context](tsResult)
    {
      lazy val fcx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.TypeScheme,
         hydra.context.Context](tsResult)
      {
        lazy val constraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.lib.maybes.fromMaybe[Map[hydra.core.Name,
           hydra.core.TypeVariableMetadata]](hydra.lib.maps.empty[hydra.core.Name,
           hydra.core.TypeVariableMetadata])(ts.constraints)
        Right(hydra.typing.InferenceResult(hydra.inference.buildTypeApplicationTerm(ts.variables)(hydra.core.Term.variable(name)),
           (ts.`type`), hydra.substitution.idTypeSubst, constraints, fcx2))
      }
    }
  }
})(hydra.lib.maps.lookup[hydra.core.Name, hydra.core.TypeScheme](name)(cx.boundTypes))

def inferTypeOfWrappedTerm(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(wt: hydra.core.WrappedTerm): Either[hydra.errors.Error,
   hydra.typing.InferenceResult] =
  {
  lazy val tname: hydra.core.Name = (wt.typeName)
  lazy val term: hydra.core.Term = (wt.body)
  hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[hydra.core.TypeScheme, hydra.context.Context],
     hydra.typing.InferenceResult](hydra.resolution.requireSchemaType(fcx)(cx.schemaTypes)(tname))((stRp: Tuple2[hydra.core.TypeScheme,
     hydra.context.Context]) =>
    {
    lazy val schemaType: hydra.core.TypeScheme = hydra.lib.pairs.first[hydra.core.TypeScheme,
       hydra.context.Context](stRp)
    {
      lazy val fcx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.TypeScheme, hydra.context.Context](stRp)
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.InferenceResult, hydra.typing.InferenceResult](hydra.inference.inferTypeOfTerm(fcx2)(cx)(term)("wrapped term"))((result: hydra.typing.InferenceResult) =>
        {
        lazy val fcx3: hydra.context.Context = (result.context)
        {
          lazy val svars: Seq[hydra.core.Name] = (schemaType.variables)
          {
            lazy val stype: hydra.core.Type = (schemaType.`type`)
            {
              lazy val iterm: hydra.core.Term = (result.term)
              {
                lazy val itype: hydra.core.Type = (result.`type`)
                {
                  lazy val isubst: hydra.typing.TypeSubst = (result.subst)
                  {
                    lazy val ityp: hydra.core.Type = hydra.core.Type.wrap(itype)
                    hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.InferenceResult,
                       hydra.typing.InferenceResult](hydra.inference.mapConstraints(fcx3)(cx)((subst: hydra.typing.TypeSubst) =>
                      hydra.inference.`yield`(fcx3)(hydra.inference.buildTypeApplicationTerm(svars)(hydra.core.Term.wrap(hydra.core.WrappedTerm(tname,
                         iterm))))(hydra.resolution.nominalApplication(tname)(hydra.lib.lists.map[hydra.core.Name,
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

def inferTypesOfTemporaryBindings(fcx: hydra.context.Context)(cx: hydra.graph.Graph)(bins: Seq[hydra.core.Binding]): Either[hydra.errors.Error,
   Tuple2[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
   Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]], hydra.context.Context]] =
  {
  def emptyResult[T0, T1, T2, T3, T4]: Either[T0, Tuple2[Tuple2[Seq[T1], Tuple2[Seq[T2],
     Tuple2[hydra.typing.TypeSubst, Map[T3, T4]]]], hydra.context.Context]] =
    Right(Tuple2(Tuple2(Seq(), Tuple2(Seq(), Tuple2(hydra.substitution.idTypeSubst,
       hydra.lib.maps.empty[T3, T4]))), fcx))
  hydra.lib.maybes.maybe[Either[hydra.errors.Error, Tuple2[Tuple2[Seq[hydra.core.Term],
     Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
     hydra.core.TypeVariableMetadata]]]], hydra.context.Context]], Tuple2[hydra.core.Binding,
     Seq[hydra.core.Binding]]](emptyResult)((binsUc: Tuple2[hydra.core.Binding, Seq[hydra.core.Binding]]) =>
    {
    lazy val binding: hydra.core.Binding = hydra.lib.pairs.first[hydra.core.Binding, Seq[hydra.core.Binding]](binsUc)
    {
      lazy val tl: Seq[hydra.core.Binding] = hydra.lib.pairs.second[hydra.core.Binding, Seq[hydra.core.Binding]](binsUc)
      {
        lazy val k: hydra.core.Name = (binding.name)
        {
          lazy val v: hydra.core.Term = (binding.term)
          hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.InferenceResult,
             Tuple2[Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
             Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]], hydra.context.Context]](hydra.inference.inferTypeOfTerm(fcx)(cx)(v)(hydra.lib.strings.cat(Seq("temporary let binding '",
             k, "'"))))((result1: hydra.typing.InferenceResult) =>
            {
            lazy val fcx2: hydra.context.Context = (result1.context)
            {
              lazy val j: hydra.core.Term = (result1.term)
              {
                lazy val u_prime: hydra.core.Type = (result1.`type`)
                {
                  lazy val u: hydra.typing.TypeSubst = (result1.subst)
                  {
                    lazy val c1Inferred: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = (result1.classConstraints)
                    hydra.lib.eithers.bind[hydra.errors.Error, Map[hydra.core.Name,
                       hydra.core.TypeVariableMetadata], Tuple2[Tuple2[Seq[hydra.core.Term],
                       Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                       Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]], hydra.context.Context]](hydra.lib.maybes.maybe[Either[hydra.errors.Error,
                       Map[hydra.core.Name, hydra.core.TypeVariableMetadata]], hydra.core.TypeScheme](Right(hydra.lib.maps.empty[hydra.core.Name,
                       hydra.core.TypeVariableMetadata]))((ts: hydra.core.TypeScheme) =>
                      {
                      lazy val tsResult: Tuple2[hydra.core.TypeScheme, hydra.context.Context] = hydra.resolution.instantiateTypeScheme(fcx2)(ts)
                      {
                        lazy val instantiatedTs: hydra.core.TypeScheme = hydra.lib.pairs.first[hydra.core.TypeScheme,
                           hydra.context.Context](tsResult)
                        {
                          lazy val freshConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.lib.maybes.fromMaybe[Map[hydra.core.Name,
                             hydra.core.TypeVariableMetadata]](hydra.lib.maps.empty[hydra.core.Name,
                             hydra.core.TypeVariableMetadata])(instantiatedTs.constraints)
                          hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.TypeSubst,
                             Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](hydra.lib.eithers.bimap[hydra.errors.UnificationError,
                             hydra.typing.TypeSubst, hydra.errors.Error, hydra.typing.TypeSubst]((_e: hydra.errors.UnificationError) => hydra.errors.Error.unification(_e))((_a: hydra.typing.TypeSubst) => _a)(hydra.unification.unifyTypes(fcx2)(cx.schemaTypes)(instantiatedTs.`type`)(u_prime)("original binding type")))((unifySubst: hydra.typing.TypeSubst) =>
                            Right(hydra.substitution.substInClassConstraints(unifySubst)(freshConstraints)))
                        }
                      }
                    })(binding.`type`))((originalBindingConstraints: Map[hydra.core.Name,
                       hydra.core.TypeVariableMetadata]) =>
                      {
                      lazy val c1: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.inference.mergeClassConstraints(c1Inferred)(originalBindingConstraints)
                      hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[Tuple2[Seq[hydra.core.Term],
                         Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                         Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
                         hydra.context.Context], Tuple2[Tuple2[Seq[hydra.core.Term],
                         Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                         Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
                         hydra.context.Context]](hydra.inference.inferTypesOfTemporaryBindings(fcx2)(hydra.substitution.substInContext(u)(cx))(tl))((rp2: Tuple2[Tuple2[Seq[hydra.core.Term],
                         Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                         Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
                         hydra.context.Context]) =>
                        {
                        lazy val result2: Tuple2[Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Type],
                           Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]] = hydra.lib.pairs.first[Tuple2[Seq[hydra.core.Term],
                           Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                           Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
                           hydra.context.Context](rp2)
                        {
                          lazy val fcx3: hydra.context.Context = hydra.lib.pairs.second[Tuple2[Seq[hydra.core.Term],
                             Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                             Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]],
                             hydra.context.Context](rp2)
                          {
                            lazy val h: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term],
                               Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                               Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](result2)
                            {
                              lazy val r_prime: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type],
                                 Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                                 hydra.core.TypeVariableMetadata]]](hydra.lib.pairs.second[Seq[hydra.core.Term],
                                 Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                                 Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](result2))
                              {
                                lazy val restPair: Tuple2[hydra.typing.TypeSubst,
                                   Map[hydra.core.Name, hydra.core.TypeVariableMetadata]] = hydra.lib.pairs.second[Seq[hydra.core.Type],
                                   Tuple2[hydra.typing.TypeSubst, Map[hydra.core.Name,
                                   hydra.core.TypeVariableMetadata]]](hydra.lib.pairs.second[Seq[hydra.core.Term],
                                   Tuple2[Seq[hydra.core.Type], Tuple2[hydra.typing.TypeSubst,
                                   Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]]](result2))
                                {
                                  lazy val r: hydra.typing.TypeSubst = hydra.lib.pairs.first[hydra.typing.TypeSubst,
                                     Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](restPair)
                                  {
                                    lazy val c2: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.lib.pairs.second[hydra.typing.TypeSubst,
                                       Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](restPair)
                                    {
                                      lazy val c1Subst: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.substitution.substInClassConstraints(r)(c1)
                                      {
                                        lazy val mergedConstraints: Map[hydra.core.Name,
                                           hydra.core.TypeVariableMetadata] = hydra.inference.mergeClassConstraints(c1Subst)(c2)
                                        Right(Tuple2(Tuple2(hydra.lib.lists.cons[hydra.core.Term](hydra.substitution.substTypesInTerm(r)(j))(h),
                                           Tuple2(hydra.lib.lists.cons[hydra.core.Type](hydra.substitution.substInType(r)(u_prime))(r_prime),
                                           Tuple2(hydra.substitution.composeTypeSubst(u)(r),
                                           mergedConstraints))), fcx3))
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
  })(hydra.lib.lists.uncons[hydra.core.Binding](bins))
}

def isUnbound(cx: hydra.graph.Graph)(v: hydra.core.Name): Boolean =
  hydra.lib.logic.and(hydra.lib.logic.not(hydra.lib.sets.member[hydra.core.Name](v)(hydra.inference.freeVariablesInContext(cx))))(hydra.lib.logic.not(hydra.lib.maps.member[hydra.core.Name,
     hydra.core.TypeScheme](v)(cx.schemaTypes)))

def mapConstraints[T0, T1](flowCx: T0)(cx: hydra.graph.Graph)(f: (hydra.typing.TypeSubst => T1))(constraints: Seq[hydra.typing.TypeConstraint]): Either[hydra.errors.Error,
   T1] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.TypeSubst, T1](hydra.lib.eithers.bimap[hydra.errors.UnificationError,
     hydra.typing.TypeSubst, hydra.errors.Error, hydra.typing.TypeSubst]((_e: hydra.errors.UnificationError) => hydra.errors.Error.unification(_e))((_a: hydra.typing.TypeSubst) => _a)(hydra.unification.unifyTypeConstraints(flowCx)(cx.schemaTypes)(constraints)))((s: hydra.typing.TypeSubst) =>
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.TypeSubst, T1](hydra.checking.checkTypeSubst(flowCx)(cx)(s))((_x: hydra.typing.TypeSubst) => Right(f(s))))

def mergeClassConstraints[T0](m1: Map[T0, hydra.core.TypeVariableMetadata])(m2: Map[T0,
   hydra.core.TypeVariableMetadata]): Map[T0, hydra.core.TypeVariableMetadata] =
  hydra.lib.lists.foldl[Map[T0, hydra.core.TypeVariableMetadata], Tuple2[T0, hydra.core.TypeVariableMetadata]]((acc: Map[T0,
     hydra.core.TypeVariableMetadata]) =>
  (pair: Tuple2[T0, hydra.core.TypeVariableMetadata]) =>
  {
  lazy val k: T0 = hydra.lib.pairs.first[T0, hydra.core.TypeVariableMetadata](pair)
  {
    lazy val v: hydra.core.TypeVariableMetadata = hydra.lib.pairs.second[T0, hydra.core.TypeVariableMetadata](pair)
    hydra.lib.maybes.maybe[Map[T0, hydra.core.TypeVariableMetadata], hydra.core.TypeVariableMetadata](hydra.lib.maps.insert[T0,
       hydra.core.TypeVariableMetadata](k)(v)(acc))((existing: hydra.core.TypeVariableMetadata) =>
      {
      lazy val merged: hydra.core.TypeVariableMetadata = hydra.core.TypeVariableMetadata(hydra.lib.sets.union[hydra.core.Name](existing.classes)(v.classes))
      hydra.lib.maps.insert[T0, hydra.core.TypeVariableMetadata](k)(merged)(acc)
    })(hydra.lib.maps.lookup[T0, hydra.core.TypeVariableMetadata](k)(acc))
  }
})(m1)(hydra.lib.maps.toList[T0, hydra.core.TypeVariableMetadata](m2))

def showInferenceResult(result: hydra.typing.InferenceResult): scala.Predef.String =
  {
  lazy val term: hydra.core.Term = (result.term)
  lazy val typ: hydra.core.Type = (result.`type`)
  lazy val subst: hydra.typing.TypeSubst = (result.subst)
  hydra.lib.strings.cat(Seq("{term=", hydra.show.core.term(term), ", type=", hydra.show.core.`type`(typ),
     ", subst=", hydra.show.typing.typeSubst(subst), "}"))
}

def `yield`(fcx: hydra.context.Context)(term: hydra.core.Term)(typ: hydra.core.Type)(subst: hydra.typing.TypeSubst): hydra.typing.InferenceResult =
  hydra.typing.InferenceResult(hydra.substitution.substTypesInTerm(subst)(term), hydra.substitution.substInType(subst)(typ),
     subst, hydra.lib.maps.empty[hydra.core.Name, hydra.core.TypeVariableMetadata],
     fcx)

def yieldChecked(fcx: hydra.context.Context)(term: hydra.core.Term)(typ: hydra.core.Type)(subst: hydra.typing.TypeSubst): hydra.typing.InferenceResult =
  {
  lazy val iterm: hydra.core.Term = hydra.substitution.substTypesInTerm(subst)(term)
  lazy val itype: hydra.core.Type = hydra.substitution.substInType(subst)(typ)
  hydra.typing.InferenceResult(iterm, itype, subst, hydra.lib.maps.empty[hydra.core.Name,
     hydra.core.TypeVariableMetadata], fcx)
}

def yieldCheckedWithConstraints(fcx: hydra.context.Context)(term: hydra.core.Term)(typ: hydra.core.Type)(subst: hydra.typing.TypeSubst)(constraints: Map[hydra.core.Name,
   hydra.core.TypeVariableMetadata]): hydra.typing.InferenceResult =
  {
  lazy val iterm: hydra.core.Term = hydra.substitution.substTypesInTerm(subst)(term)
  lazy val itype: hydra.core.Type = hydra.substitution.substInType(subst)(typ)
  lazy val iconstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.substitution.substInClassConstraints(subst)(constraints)
  hydra.typing.InferenceResult(iterm, itype, subst, iconstraints, fcx)
}

def yieldDebug[T0](fcx: hydra.context.Context)(cx: T0)(debugId: scala.Predef.String)(term: hydra.core.Term)(typ: hydra.core.Type)(subst: hydra.typing.TypeSubst): Either[hydra.errors.Error,
   hydra.typing.InferenceResult] =
  {
  lazy val rterm: hydra.core.Term = hydra.substitution.substTypesInTerm(subst)(term)
  lazy val rtyp: hydra.core.Type = hydra.substitution.substInType(subst)(typ)
  hydra.lib.eithers.bind[hydra.errors.Error, Unit, hydra.typing.InferenceResult](hydra.annotations.debugIf(fcx)(debugId)(hydra.lib.strings.cat(Seq("\n\tterm: ",
     hydra.show.core.term(term), "\n\ttyp: ", hydra.show.core.`type`(typ), "\n\tsubst: ",
     hydra.show.typing.typeSubst(subst), "\n\trterm: ", hydra.show.core.term(rterm),
     "\n\trtyp: ", hydra.show.core.`type`(rtyp)))))((result: Unit) =>
    Right(hydra.typing.InferenceResult(rterm, rtyp, subst, hydra.lib.maps.empty[hydra.core.Name,
       hydra.core.TypeVariableMetadata], fcx)))
}

def yieldWithConstraints(fcx: hydra.context.Context)(term: hydra.core.Term)(typ: hydra.core.Type)(subst: hydra.typing.TypeSubst)(constraints: Map[hydra.core.Name,
   hydra.core.TypeVariableMetadata]): hydra.typing.InferenceResult =
  hydra.typing.InferenceResult(hydra.substitution.substTypesInTerm(subst)(term), hydra.substitution.substInType(subst)(typ),
     subst, constraints, fcx)
