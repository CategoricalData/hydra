package hydra.analysis

import hydra.coders.*

import hydra.context.*

import hydra.core.*

import hydra.errors.*

import hydra.packaging.*

import hydra.typing.*

def addNamesToNamespaces[T0](encodeNamespace: (hydra.packaging.Namespace => T0))(names: scala.collection.immutable.Set[hydra.core.Name])(ns0: hydra.packaging.Namespaces[T0]): hydra.packaging.Namespaces[T0] =
  {
  lazy val nss: scala.collection.immutable.Set[hydra.packaging.Namespace] = hydra.lib.sets.fromList[hydra.packaging.Namespace](hydra.lib.maybes.cat[hydra.packaging.Namespace](hydra.lib.lists.map[hydra.core.Name,
     Option[hydra.packaging.Namespace]](hydra.names.namespaceOf)(hydra.lib.sets.toList[hydra.core.Name](names))))
  def toPair(ns: hydra.packaging.Namespace): Tuple2[hydra.packaging.Namespace, T0] = Tuple2(ns, encodeNamespace(ns))
  hydra.packaging.Namespaces(ns0.focus, hydra.lib.maps.union[hydra.packaging.Namespace, T0](ns0.mapping)(hydra.lib.maps.fromList[hydra.packaging.Namespace,
     T0](hydra.lib.lists.map[hydra.packaging.Namespace, Tuple2[hydra.packaging.Namespace, T0]](toPair)(hydra.lib.sets.toList[hydra.packaging.Namespace](nss)))))
}

def analyzeFunctionTerm[T0, T1](cx: hydra.context.Context)(getTC: (T0 => hydra.graph.Graph))(setTC: (hydra.graph.Graph => T0 => T0))(env: T0)(term: hydra.core.Term): Either[T1,
   hydra.typing.FunctionStructure[T0]] =
  hydra.analysis.analyzeFunctionTermWith(cx)((g: hydra.graph.Graph) =>
  (b: hydra.core.Binding) =>
  hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.predicates.isComplexBinding(g)(b))(Some(hydra.core.Term.literal(hydra.core.Literal.boolean(true))))(None))(getTC)(setTC)(env)(term)

def analyzeFunctionTermWith[T0, T1](cx: hydra.context.Context)(forBinding: (hydra.graph.Graph => hydra.core.Binding => Option[hydra.core.Term]))(getTC: (T0 => hydra.graph.Graph))(setTC: (hydra.graph.Graph => T0 => T0))(env: T0)(term: hydra.core.Term): Either[T1,
   hydra.typing.FunctionStructure[T0]] =
  hydra.analysis.analyzeFunctionTermWith_gather(cx)(forBinding)(getTC)(setTC)(true)(env)(Seq())(Seq())(Seq())(Seq())(Seq())(term)

def analyzeFunctionTermWith_finish[T0, T1](cx: hydra.context.Context)(getTC: (T0 => hydra.graph.Graph))(fEnv: T0)(tparams: Seq[hydra.core.Name])(args: Seq[hydra.core.Name])(bindings: Seq[hydra.core.Binding])(doms: Seq[hydra.core.Type])(tapps: Seq[hydra.core.Type])(body: hydra.core.Term): Either[T1,
   hydra.typing.FunctionStructure[T0]] =
  {
  lazy val bodyWithTapps: hydra.core.Term = hydra.lib.lists.foldl[hydra.core.Term, hydra.core.Type]((trm: hydra.core.Term) =>
    (typ: hydra.core.Type) =>
    hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(trm, typ)))(body)(tapps)
  lazy val mcod: Option[hydra.core.Type] = hydra.lib.eithers.either[hydra.context.InContext[hydra.errors.Error],
     hydra.core.Type, Option[hydra.core.Type]]((_x: hydra.context.InContext[hydra.errors.Error]) => None)((c: hydra.core.Type) => Some(c))(hydra.checking.typeOfTerm(cx)(getTC(fEnv))(bodyWithTapps))
  Right(hydra.typing.FunctionStructure(hydra.lib.lists.reverse[hydra.core.Name](tparams), hydra.lib.lists.reverse[hydra.core.Name](args),
     bindings, bodyWithTapps, hydra.lib.lists.reverse[hydra.core.Type](doms), mcod, fEnv))
}

def analyzeFunctionTermWith_gather[T0, T1](cx: hydra.context.Context)(forBinding: (hydra.graph.Graph => hydra.core.Binding => Option[hydra.core.Term]))(getTC: (T0 => hydra.graph.Graph))(setTC: (hydra.graph.Graph => T0 => T0))(argMode: Boolean)(gEnv: T0)(tparams: Seq[hydra.core.Name])(args: Seq[hydra.core.Name])(bindings: Seq[hydra.core.Binding])(doms: Seq[hydra.core.Type])(tapps: Seq[hydra.core.Type])(t: hydra.core.Term): Either[T1,
   hydra.typing.FunctionStructure[T0]] =
  hydra.strip.deannotateTerm(t) match
  case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
    case hydra.core.Function.lambda(v_Function_lambda_lam) => hydra.lib.logic.ifElse[Either[T1, hydra.typing.FunctionStructure[T0]]](argMode)({
      lazy val v: hydra.core.Name = (v_Function_lambda_lam.parameter)
      {
        lazy val dom: hydra.core.Type = hydra.lib.maybes.maybe[hydra.core.Type, hydra.core.Type](hydra.core.Type.variable("_"))((`x_`: hydra.core.Type) => `x_`)(v_Function_lambda_lam.domain)
        {
          lazy val body: hydra.core.Term = (v_Function_lambda_lam.body)
          {
            lazy val newEnv: T0 = setTC(hydra.scoping.extendGraphForLambda(getTC(gEnv))(v_Function_lambda_lam))(gEnv)
            hydra.analysis.analyzeFunctionTermWith_gather(cx)(forBinding)(getTC)(setTC)(argMode)(newEnv)(tparams)(hydra.lib.lists.cons[hydra.core.Name](v)(args))(bindings)(hydra.lib.lists.cons[hydra.core.Type](dom)(doms))(tapps)(body)
          }
        }
      }
    })(hydra.analysis.analyzeFunctionTermWith_finish(cx)(getTC)(gEnv)(tparams)(args)(bindings)(doms)(tapps)(t))
    case _ => hydra.analysis.analyzeFunctionTermWith_finish(cx)(getTC)(gEnv)(tparams)(args)(bindings)(doms)(tapps)(t)
  case hydra.core.Term.let(v_Term_let_lt) => {
    lazy val newBindings: Seq[hydra.core.Binding] = (v_Term_let_lt.bindings)
    {
      lazy val body: hydra.core.Term = (v_Term_let_lt.body)
      {
        lazy val newEnv: T0 = setTC(hydra.scoping.extendGraphForLet(forBinding)(getTC(gEnv))(v_Term_let_lt))(gEnv)
        hydra.analysis.analyzeFunctionTermWith_gather(cx)(forBinding)(getTC)(setTC)(false)(newEnv)(tparams)(args)(hydra.lib.lists.concat2[hydra.core.Binding](bindings)(newBindings))(doms)(tapps)(body)
      }
    }
  }
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => {
    lazy val taBody: hydra.core.Term = (v_Term_typeApplication_ta.body)
    {
      lazy val typ: hydra.core.Type = (v_Term_typeApplication_ta.`type`)
      hydra.analysis.analyzeFunctionTermWith_gather(cx)(forBinding)(getTC)(setTC)(argMode)(gEnv)(tparams)(args)(bindings)(doms)(hydra.lib.lists.cons[hydra.core.Type](typ)(tapps))(taBody)
    }
  }
  case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => {
    lazy val tvar: hydra.core.Name = (v_Term_typeLambda_tl.parameter)
    {
      lazy val tlBody: hydra.core.Term = (v_Term_typeLambda_tl.body)
      {
        lazy val newEnv: T0 = setTC(hydra.scoping.extendGraphForTypeLambda(getTC(gEnv))(v_Term_typeLambda_tl))(gEnv)
        hydra.analysis.analyzeFunctionTermWith_gather(cx)(forBinding)(getTC)(setTC)(argMode)(newEnv)(hydra.lib.lists.cons[hydra.core.Name](tvar)(tparams))(args)(bindings)(doms)(tapps)(tlBody)
      }
    }
  }
  case _ => hydra.analysis.analyzeFunctionTermWith_finish(cx)(getTC)(gEnv)(tparams)(args)(bindings)(doms)(tapps)(t)

def definitionDependencyNamespaces(defs: Seq[hydra.packaging.Definition]): scala.collection.immutable.Set[hydra.packaging.Namespace] =
  {
  def defNames(`def`: hydra.packaging.Definition): scala.collection.immutable.Set[hydra.core.Name] =
    `def` match
    case hydra.packaging.Definition.`type`(v_Definition_type_typeDef) => hydra.dependencies.typeDependencyNames(true)(v_Definition_type_typeDef.`type`.`type`)
    case hydra.packaging.Definition.term(v_Definition_term_termDef) => hydra.dependencies.termDependencyNames(true)(true)(true)(v_Definition_term_termDef.term)
  lazy val allNames: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.unions[hydra.core.Name](hydra.lib.lists.map[hydra.packaging.Definition,
     scala.collection.immutable.Set[hydra.core.Name]](defNames)(defs))
  hydra.lib.sets.fromList[hydra.packaging.Namespace](hydra.lib.maybes.cat[hydra.packaging.Namespace](hydra.lib.lists.map[hydra.core.Name,
     Option[hydra.packaging.Namespace]](hydra.names.namespaceOf)(hydra.lib.sets.toList[hydra.core.Name](allNames))))
}

def dependencyNamespaces(cx: hydra.context.Context)(graph: hydra.graph.Graph)(binds: Boolean)(withPrims: Boolean)(withNoms: Boolean)(withSchema: Boolean)(els: Seq[hydra.core.Binding]): Either[hydra.context.InContext[hydra.errors.Error],
   scala.collection.immutable.Set[hydra.packaging.Namespace]] =
  {
  def depNames(el: hydra.core.Binding): Either[hydra.context.InContext[hydra.errors.Error], scala.collection.immutable.Set[hydra.core.Name]] =
    {
    lazy val term: hydra.core.Term = (el.term)
    lazy val deannotatedTerm: hydra.core.Term = hydra.strip.deannotateTerm(term)
    lazy val dataNames: scala.collection.immutable.Set[hydra.core.Name] = hydra.dependencies.termDependencyNames(binds)(withPrims)(withNoms)(term)
    lazy val schemaNames: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.logic.ifElse[scala.collection.immutable.Set[hydra.core.Name]](withSchema)(hydra.lib.maybes.maybe[scala.collection.immutable.Set[hydra.core.Name],
       hydra.core.TypeScheme](hydra.lib.sets.empty[hydra.core.Name])((ts: hydra.core.TypeScheme) => hydra.dependencies.typeDependencyNames(true)(ts.`type`))(el.`type`))(hydra.lib.sets.empty[hydra.core.Name])
    hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], scala.collection.immutable.Set[hydra.core.Name]]](hydra.predicates.isEncodedType(deannotatedTerm))(hydra.lib.eithers.map[hydra.core.Type,
       scala.collection.immutable.Set[hydra.core.Name], hydra.context.InContext[hydra.errors.Error]]((typ: hydra.core.Type) =>
      hydra.lib.sets.unions[hydra.core.Name](Seq(dataNames, schemaNames, hydra.dependencies.typeDependencyNames(true)(typ))))(hydra.lib.eithers.bimap[hydra.errors.Error,
         hydra.core.Type, hydra.context.InContext[hydra.errors.Error], hydra.core.Type]((_wc_e: hydra.errors.Error) =>
      hydra.context.InContext(_wc_e, hydra.context.Context(hydra.lib.lists.cons[scala.Predef.String]("dependency namespace (type)")(cx.trace),
         (cx.messages), (cx.other))))((_wc_a: hydra.core.Type) => _wc_a)(hydra.lib.eithers.bimap[hydra.errors.DecodingError,
         hydra.core.Type, hydra.errors.Error, hydra.core.Type]((_e: hydra.errors.DecodingError) => hydra.errors.Error.other(_e))((_a: hydra.core.Type) => _a)(hydra.decode.core.`type`(graph)(term)))))(hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error],
         scala.collection.immutable.Set[hydra.core.Name]]](hydra.predicates.isEncodedTerm(deannotatedTerm))(hydra.lib.eithers.map[hydra.core.Term,
         scala.collection.immutable.Set[hydra.core.Name], hydra.context.InContext[hydra.errors.Error]]((decodedTerm: hydra.core.Term) =>
      hydra.lib.sets.unions[hydra.core.Name](Seq(dataNames, schemaNames, hydra.dependencies.termDependencyNames(binds)(withPrims)(withNoms)(decodedTerm))))(hydra.lib.eithers.bimap[hydra.errors.Error,
         hydra.core.Term, hydra.context.InContext[hydra.errors.Error], hydra.core.Term]((_wc_e: hydra.errors.Error) =>
      hydra.context.InContext(_wc_e, hydra.context.Context(hydra.lib.lists.cons[scala.Predef.String]("dependency namespace (term)")(cx.trace),
         (cx.messages), (cx.other))))((_wc_a: hydra.core.Term) => _wc_a)(hydra.lib.eithers.bimap[hydra.errors.DecodingError,
         hydra.core.Term, hydra.errors.Error, hydra.core.Term]((_e: hydra.errors.DecodingError) => hydra.errors.Error.other(_e))((_a: hydra.core.Term) => _a)(hydra.decode.core.term(graph)(term)))))(Right(hydra.lib.sets.unions[hydra.core.Name](Seq(dataNames,
         schemaNames)))))
  }
  hydra.lib.eithers.map[Seq[scala.collection.immutable.Set[hydra.core.Name]], scala.collection.immutable.Set[hydra.packaging.Namespace],
     hydra.context.InContext[hydra.errors.Error]]((namesList: Seq[scala.collection.immutable.Set[hydra.core.Name]]) =>
    hydra.lib.sets.fromList[hydra.packaging.Namespace](hydra.lib.maybes.cat[hydra.packaging.Namespace](hydra.lib.lists.map[hydra.core.Name,
       Option[hydra.packaging.Namespace]](hydra.names.namespaceOf)(hydra.lib.sets.toList[hydra.core.Name](hydra.lib.sets.unions[hydra.core.Name](namesList))))))(hydra.lib.eithers.mapList[hydra.core.Binding,
       scala.collection.immutable.Set[hydra.core.Name], hydra.context.InContext[hydra.errors.Error]](depNames)(els))
}

def gatherApplications(term: hydra.core.Term): Tuple2[Seq[hydra.core.Term], hydra.core.Term] =
  {
  def go(args: Seq[hydra.core.Term])(t: hydra.core.Term): Tuple2[Seq[hydra.core.Term], hydra.core.Term] =
    hydra.strip.deannotateTerm(t) match
    case hydra.core.Term.application(v_Term_application_app) => {
      lazy val lhs: hydra.core.Term = (v_Term_application_app.function)
      {
        lazy val rhs: hydra.core.Term = (v_Term_application_app.argument)
        go(hydra.lib.lists.cons[hydra.core.Term](rhs)(args))(lhs)
      }
    }
    case _ => Tuple2(args, t)
  go(Seq())(term)
}

def gatherArgs(term: hydra.core.Term)(args: Seq[hydra.core.Term]): Tuple2[hydra.core.Term, Seq[hydra.core.Term]] =
  hydra.strip.deannotateTerm(term) match
  case hydra.core.Term.application(v_Term_application_app) => {
    lazy val lhs: hydra.core.Term = (v_Term_application_app.function)
    {
      lazy val rhs: hydra.core.Term = (v_Term_application_app.argument)
      hydra.analysis.gatherArgs(lhs)(hydra.lib.lists.cons[hydra.core.Term](rhs)(args))
    }
  }
  case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => {
    lazy val body: hydra.core.Term = (v_Term_typeLambda_tl.body)
    hydra.analysis.gatherArgs(body)(args)
  }
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => {
    lazy val body: hydra.core.Term = (v_Term_typeApplication_ta.body)
    hydra.analysis.gatherArgs(body)(args)
  }
  case _ => Tuple2(term, args)

def gatherArgsWithTypeApps(term: hydra.core.Term)(args: Seq[hydra.core.Term])(tyArgs: Seq[hydra.core.Type]): Tuple2[hydra.core.Term,
   Tuple2[Seq[hydra.core.Term], Seq[hydra.core.Type]]] =
  hydra.strip.deannotateTerm(term) match
  case hydra.core.Term.application(v_Term_application_app) => {
    lazy val lhs: hydra.core.Term = (v_Term_application_app.function)
    {
      lazy val rhs: hydra.core.Term = (v_Term_application_app.argument)
      hydra.analysis.gatherArgsWithTypeApps(lhs)(hydra.lib.lists.cons[hydra.core.Term](rhs)(args))(tyArgs)
    }
  }
  case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => {
    lazy val body: hydra.core.Term = (v_Term_typeLambda_tl.body)
    hydra.analysis.gatherArgsWithTypeApps(body)(args)(tyArgs)
  }
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => {
    lazy val body: hydra.core.Term = (v_Term_typeApplication_ta.body)
    {
      lazy val typ: hydra.core.Type = (v_Term_typeApplication_ta.`type`)
      hydra.analysis.gatherArgsWithTypeApps(body)(args)(hydra.lib.lists.cons[hydra.core.Type](typ)(tyArgs))
    }
  }
  case _ => Tuple2(term, Tuple2(args, tyArgs))

def isSelfTailRecursive(funcName: hydra.core.Name)(body: hydra.core.Term): Boolean =
  {
  lazy val callsSelf: Boolean = hydra.lib.logic.not(hydra.variables.isFreeVariableInTerm(funcName)(body))
  hydra.lib.logic.ifElse[Boolean](callsSelf)(hydra.analysis.isTailRecursiveInTailPosition(funcName)(body))(false)
}

def isSimpleAssignment(term: hydra.core.Term): Boolean =
  term match
  case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.analysis.isSimpleAssignment(v_Term_annotated_at.body)
  case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
    case hydra.core.Function.lambda(v_Function_lambda__) => false
    case _ => true
  case hydra.core.Term.let(v_Term_let__) => false
  case hydra.core.Term.typeLambda(v_Term_typeLambda__) => false
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => hydra.analysis.isSimpleAssignment(v_Term_typeApplication_ta.body)
  case _ => {
    lazy val baseTerm: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, Seq[hydra.core.Term]](hydra.analysis.gatherArgs(term)(Seq()))
    baseTerm match
      case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
        case hydra.core.Function.elimination(v_Function_elimination_elim) => v_Function_elimination_elim match
          case hydra.core.Elimination.union(v_Elimination_union__) => false
          case _ => true
        case _ => true
      case _ => true
  }

def isTailRecursiveInTailPosition(funcName: hydra.core.Name)(term: hydra.core.Term): Boolean =
  {
  lazy val stripped: hydra.core.Term = hydra.strip.deannotateAndDetypeTerm(term)
  stripped match
    case hydra.core.Term.application(v_Term_application_app) => {
      lazy val gathered: Tuple2[Seq[hydra.core.Term], hydra.core.Term] = hydra.analysis.gatherApplications(stripped)
      {
        lazy val gatherArgs: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term], hydra.core.Term](gathered)
        {
          lazy val gatherFun: hydra.core.Term = hydra.lib.pairs.second[Seq[hydra.core.Term], hydra.core.Term](gathered)
          {
            lazy val strippedFun: hydra.core.Term = hydra.strip.deannotateAndDetypeTerm(gatherFun)
            strippedFun match
              case hydra.core.Term.variable(v_Term_variable_vname) => hydra.lib.logic.ifElse[Boolean](hydra.lib.equality.equal[hydra.core.Name](v_Term_variable_vname)(funcName))({
                lazy val argsNoFunc: Boolean = hydra.lib.lists.foldl[Boolean, hydra.core.Term]((ok: Boolean) =>
                  (arg: hydra.core.Term) =>
                  hydra.lib.logic.and(ok)(hydra.variables.isFreeVariableInTerm(funcName)(arg)))(true)(gatherArgs)
                {
                  lazy val argsNoLambda: Boolean = hydra.lib.lists.foldl[Boolean, hydra.core.Term]((ok: Boolean) =>
                    (arg: hydra.core.Term) =>
                    hydra.lib.logic.and(ok)(hydra.lib.logic.not(hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)((found: Boolean) =>
                    (t: hydra.core.Term) =>
                    hydra.lib.logic.or(found)(t match
                    case hydra.core.Term.function(v_Term_function_f2) => v_Term_function_f2 match
                      case hydra.core.Function.lambda(v_Function_lambda_lam) => {
                        lazy val ignore: hydra.core.Term = (v_Function_lambda_lam.body)
                        true
                      }
                      case _ => false
                    case _ => false))(false)(arg))))(true)(gatherArgs)
                  hydra.lib.logic.and(argsNoFunc)(argsNoLambda)
                }
              })(hydra.variables.isFreeVariableInTerm(funcName)(term))
              case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
                case hydra.core.Function.elimination(v_Function_elimination_e) => v_Function_elimination_e match
                  case hydra.core.Elimination.union(v_Elimination_union_cs) => {
                    lazy val `cases_`: Seq[hydra.core.Field] = (v_Elimination_union_cs.cases)
                    {
                      lazy val dflt: Option[hydra.core.Term] = (v_Elimination_union_cs.default)
                      {
                        lazy val branchesOk: Boolean = hydra.lib.lists.foldl[Boolean, hydra.core.Field]((ok: Boolean) =>
                          (field: hydra.core.Field) =>
                          hydra.lib.logic.and(ok)(hydra.analysis.isTailRecursiveInTailPosition(funcName)(field.term)))(true)(`cases_`)
                        {
                          lazy val dfltOk: Boolean = hydra.lib.maybes.maybe[Boolean, hydra.core.Term](true)((d: hydra.core.Term) => hydra.analysis.isTailRecursiveInTailPosition(funcName)(d))(dflt)
                          {
                            lazy val argsOk: Boolean = hydra.lib.lists.foldl[Boolean, hydra.core.Term]((ok: Boolean) =>
                              (arg: hydra.core.Term) =>
                              hydra.lib.logic.and(ok)(hydra.variables.isFreeVariableInTerm(funcName)(arg)))(true)(gatherArgs)
                            hydra.lib.logic.and(hydra.lib.logic.and(branchesOk)(dfltOk))(argsOk)
                          }
                        }
                      }
                    }
                  }
                  case _ => hydra.variables.isFreeVariableInTerm(funcName)(term)
                case _ => hydra.variables.isFreeVariableInTerm(funcName)(term)
              case _ => hydra.variables.isFreeVariableInTerm(funcName)(term)
          }
        }
      }
    }
    case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
      case hydra.core.Function.lambda(v_Function_lambda_lam) => hydra.analysis.isTailRecursiveInTailPosition(funcName)(v_Function_lambda_lam.body)
      case _ => hydra.variables.isFreeVariableInTerm(funcName)(term)
    case hydra.core.Term.let(v_Term_let_lt) => {
      lazy val bindingsOk: Boolean = hydra.lib.lists.foldl[Boolean, hydra.core.Binding]((ok: Boolean) =>
        (b: hydra.core.Binding) =>
        hydra.lib.logic.and(ok)(hydra.variables.isFreeVariableInTerm(funcName)(b.term)))(true)(v_Term_let_lt.bindings)
      hydra.lib.logic.and(bindingsOk)(hydra.analysis.isTailRecursiveInTailPosition(funcName)(v_Term_let_lt.body))
    }
    case _ => hydra.variables.isFreeVariableInTerm(funcName)(term)
}

def moduleContainsBinaryLiterals(mod: hydra.packaging.Module): Boolean =
  {
  def checkTerm(found: Boolean)(term: hydra.core.Term): Boolean =
    hydra.lib.logic.or(found)(term match
    case hydra.core.Term.literal(v_Term_literal_lit) => v_Term_literal_lit match
      case hydra.core.Literal.binary(v_Literal_binary__) => true
      case _ => false
    case _ => false)
  def termContainsBinary(term: hydra.core.Term): Boolean =
    hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)(checkTerm)(false)(term)
  lazy val defTerms: Seq[hydra.core.Term] = hydra.lib.maybes.cat[hydra.core.Term](hydra.lib.lists.map[hydra.packaging.Definition,
     Option[hydra.core.Term]]((d: hydra.packaging.Definition) =>
    d match
    case hydra.packaging.Definition.term(v_Definition_term_td) => Some(v_Definition_term_td.term)
    case _ => None)(mod.definitions))
  hydra.lib.lists.foldl[Boolean, hydra.core.Term]((acc: Boolean) =>
    (t: hydra.core.Term) => hydra.lib.logic.or(acc)(termContainsBinary(t)))(false)(defTerms)
}

def moduleDependencyNamespaces(cx: hydra.context.Context)(graph: hydra.graph.Graph)(binds: Boolean)(withPrims: Boolean)(withNoms: Boolean)(withSchema: Boolean)(mod: hydra.packaging.Module): Either[hydra.context.InContext[hydra.errors.Error],
   scala.collection.immutable.Set[hydra.packaging.Namespace]] =
  {
  lazy val allBindings: Seq[hydra.core.Binding] = hydra.lib.maybes.cat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Definition,
     Option[hydra.core.Binding]]((d: hydra.packaging.Definition) =>
    d match
    case hydra.packaging.Definition.`type`(v_Definition_type_td) => Some({
      lazy val schemaTerm: hydra.core.Term = hydra.core.Term.variable("hydra.core.Type")
      {
        lazy val dataTerm: hydra.core.Term = hydra.annotations.normalizeTermAnnotations(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.encode.core.`type`(v_Definition_type_td.`type`.`type`),
           hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Term](Seq(Tuple2(hydra.constants.key_type,
           schemaTerm))))))
        hydra.core.Binding(v_Definition_type_td.name, dataTerm, Some(hydra.core.TypeScheme(Seq(), hydra.core.Type.variable("hydra.core.Type"), None)))
      }
    })
    case hydra.packaging.Definition.term(v_Definition_term_td) => Some(hydra.core.Binding(v_Definition_term_td.name,
       (v_Definition_term_td.term), (v_Definition_term_td.`type`)))
    case _ => None)(mod.definitions))
  hydra.lib.eithers.map[scala.collection.immutable.Set[hydra.packaging.Namespace], scala.collection.immutable.Set[hydra.packaging.Namespace],
     hydra.context.InContext[hydra.errors.Error]]((deps: scala.collection.immutable.Set[hydra.packaging.Namespace]) =>
    hydra.lib.sets.delete[hydra.packaging.Namespace](mod.namespace)(deps))(hydra.analysis.dependencyNamespaces(cx)(graph)(binds)(withPrims)(withNoms)(withSchema)(allBindings))
}

def namespacesForDefinitions[T0](encodeNamespace: (hydra.packaging.Namespace => T0))(focusNs: hydra.packaging.Namespace)(defs: Seq[hydra.packaging.Definition]): hydra.packaging.Namespaces[T0] =
  {
  lazy val nss: scala.collection.immutable.Set[hydra.packaging.Namespace] = hydra.lib.sets.delete[hydra.packaging.Namespace](focusNs)(hydra.analysis.definitionDependencyNamespaces(defs))
  def toPair(ns: hydra.packaging.Namespace): Tuple2[hydra.packaging.Namespace, T0] = Tuple2(ns, encodeNamespace(ns))
  hydra.packaging.Namespaces(toPair(focusNs), hydra.lib.maps.fromList[hydra.packaging.Namespace, T0](hydra.lib.lists.map[hydra.packaging.Namespace,
     Tuple2[hydra.packaging.Namespace, T0]](toPair)(hydra.lib.sets.toList[hydra.packaging.Namespace](nss))))
}
