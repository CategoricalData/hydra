package hydra.coderUtils

import hydra.coders.*

import hydra.core.*

import hydra.graph.*

import hydra.module.*

import hydra.typing.*

import hydra.util.*

import hydra.lib.eithers

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.math

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.sets

import hydra.lib.strings

def normalizeComment(s: scala.Predef.String): scala.Predef.String =
  {
  lazy val stripped: scala.Predef.String = hydra.formatting.stripLeadingAndTrailingWhitespace(s)
  hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.strings.`null`(stripped))("")({
    lazy val lastIdx: Int = hydra.lib.math.sub(hydra.lib.strings.length(stripped))(1)
    {
      lazy val lastChar: Int = hydra.lib.strings.charAt(lastIdx)(stripped)
      hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](lastChar)(46))(stripped)(hydra.lib.strings.cat2(stripped)("."))
    }
  })
}

def gatherApplications(term: hydra.core.Term): Tuple2[Seq[hydra.core.Term], hydra.core.Term] =
  {
  def go(args: Seq[hydra.core.Term])(t: hydra.core.Term): Tuple2[Seq[hydra.core.Term], hydra.core.Term] =
    hydra.rewriting.deannotateTerm(t) match
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
  hydra.rewriting.deannotateTerm(term) match
  case hydra.core.Term.application(v_Term_application_app) => {
    lazy val lhs: hydra.core.Term = (v_Term_application_app.function)
    {
      lazy val rhs: hydra.core.Term = (v_Term_application_app.argument)
      hydra.coderUtils.gatherArgs(lhs)(hydra.lib.lists.cons[hydra.core.Term](rhs)(args))
    }
  }
  case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => {
    lazy val body: hydra.core.Term = (v_Term_typeLambda_tl.body)
    hydra.coderUtils.gatherArgs(body)(args)
  }
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => {
    lazy val body: hydra.core.Term = (v_Term_typeApplication_ta.body)
    hydra.coderUtils.gatherArgs(body)(args)
  }
  case _ => Tuple2(term, args)

def gatherArgsWithTypeApps(term: hydra.core.Term)(args: Seq[hydra.core.Term])(tyArgs: Seq[hydra.core.Type]): Tuple2[hydra.core.Term,
   Tuple2[Seq[hydra.core.Term], Seq[hydra.core.Type]]] =
  hydra.rewriting.deannotateTerm(term) match
  case hydra.core.Term.application(v_Term_application_app) => {
    lazy val lhs: hydra.core.Term = (v_Term_application_app.function)
    {
      lazy val rhs: hydra.core.Term = (v_Term_application_app.argument)
      hydra.coderUtils.gatherArgsWithTypeApps(lhs)(hydra.lib.lists.cons[hydra.core.Term](rhs)(args))(tyArgs)
    }
  }
  case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => {
    lazy val body: hydra.core.Term = (v_Term_typeLambda_tl.body)
    hydra.coderUtils.gatherArgsWithTypeApps(body)(args)(tyArgs)
  }
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => {
    lazy val body: hydra.core.Term = (v_Term_typeApplication_ta.body)
    {
      lazy val typ: hydra.core.Type = (v_Term_typeApplication_ta.`type`)
      hydra.coderUtils.gatherArgsWithTypeApps(body)(args)(hydra.lib.lists.cons[hydra.core.Type](typ)(tyArgs))
    }
  }
  case _ => Tuple2(term, Tuple2(args, tyArgs))

def isSimpleAssignment(term: hydra.core.Term): Boolean =
  term match
  case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.coderUtils.isSimpleAssignment(v_Term_annotated_at.body)
  case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
    case hydra.core.Function.lambda => false
    case _ => true
  case hydra.core.Term.let => false
  case hydra.core.Term.typeLambda => false
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => hydra.coderUtils.isSimpleAssignment(v_Term_typeApplication_ta.body)
  case _ => {
    lazy val baseTerm: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, Seq[hydra.core.Term]](hydra.coderUtils.gatherArgs(term)(Seq()))
    baseTerm match
      case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
        case hydra.core.Function.elimination(v_Function_elimination_elim) => v_Function_elimination_elim match
          case hydra.core.Elimination.union => false
          case _ => true
        case _ => true
      case _ => true
  }

def isComplexTerm(tc: hydra.graph.Graph)(t: hydra.core.Term): Boolean =
  t match
  case hydra.core.Term.let => true
  case hydra.core.Term.typeApplication => true
  case hydra.core.Term.typeLambda => true
  case hydra.core.Term.variable(v_Term_variable_name) => hydra.coderUtils.isComplexVariable(tc)(v_Term_variable_name)
  case _ => hydra.lib.lists.foldl[Boolean, hydra.core.Term]((b: Boolean) =>
    (sub: hydra.core.Term) =>
    hydra.lib.logic.or(b)(hydra.coderUtils.isComplexTerm(tc)(sub)))(false)(hydra.rewriting.subterms(t))

def isComplexVariable(tc: hydra.graph.Graph)(name: hydra.core.Name): Boolean =
  {
  lazy val metaLookup: Option[hydra.core.Term] = hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Term](name)(tc.metadata)
  hydra.lib.logic.ifElse[Boolean](hydra.lib.maybes.isJust[hydra.core.Term](metaLookup))(true)(hydra.lib.logic.ifElse[Boolean](hydra.lib.sets.member[hydra.core.Name](name)(tc.lambdaVariables))(true)({
    lazy val typeLookup: Option[hydra.core.TypeScheme] = hydra.lib.maps.lookup[hydra.core.Name, hydra.core.TypeScheme](name)(tc.boundTypes)
    hydra.lib.maybes.maybe[Boolean, hydra.core.TypeScheme](true)((ts: hydra.core.TypeScheme) =>
      hydra.lib.equality.gt[Int](hydra.arity.typeSchemeArity(ts))(0))(typeLookup)
  }))
}

def isComplexBinding(tc: hydra.graph.Graph)(b: hydra.core.Binding): Boolean =
  {
  lazy val term: hydra.core.Term = (b.term)
  lazy val mts: Option[hydra.core.TypeScheme] = (b.`type`)
  hydra.lib.maybes.cases[hydra.core.TypeScheme, Boolean](mts)(hydra.coderUtils.isComplexTerm(tc)(term))((ts: hydra.core.TypeScheme) =>
    {
    lazy val isPolymorphic: Boolean = hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.core.Name](ts.variables))
    {
      lazy val isNonNullary: Boolean = hydra.lib.equality.gt[Int](hydra.arity.typeArity(ts.`type`))(0)
      {
        lazy val isComplex: Boolean = hydra.coderUtils.isComplexTerm(tc)(term)
        hydra.lib.logic.or(hydra.lib.logic.or(isPolymorphic)(isNonNullary))(isComplex)
      }
    }
  })
}

def isTrivialTerm(t: hydra.core.Term): Boolean =
  hydra.rewriting.deannotateTerm(t) match
  case hydra.core.Term.literal => true
  case hydra.core.Term.variable => true
  case hydra.core.Term.unit => true
  case hydra.core.Term.application(v_Term_application_app) => {
    lazy val fun: hydra.core.Term = (v_Term_application_app.function)
    {
      lazy val arg: hydra.core.Term = (v_Term_application_app.argument)
      fun match
        case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
          case hydra.core.Function.elimination(v_Function_elimination_e) => v_Function_elimination_e match
            case hydra.core.Elimination.record => hydra.coderUtils.isTrivialTerm(arg)
            case hydra.core.Elimination.wrap => hydra.coderUtils.isTrivialTerm(arg)
            case _ => false
          case _ => false
        case _ => false
    }
  }
  case hydra.core.Term.maybe(v_Term_maybe_opt) => hydra.lib.maybes.maybe[Boolean, hydra.core.Term](true)((inner: hydra.core.Term) => hydra.coderUtils.isTrivialTerm(inner))(v_Term_maybe_opt)
  case hydra.core.Term.record(v_Term_record_rec) => hydra.lib.lists.foldl[Boolean, hydra.core.Field]((acc: Boolean) =>
    (fld: hydra.core.Field) =>
    hydra.lib.logic.and(acc)(hydra.coderUtils.isTrivialTerm(fld.term)))(true)(v_Term_record_rec.fields)
  case hydra.core.Term.wrap(v_Term_wrap_wt) => hydra.coderUtils.isTrivialTerm(v_Term_wrap_wt.body)
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => hydra.coderUtils.isTrivialTerm(v_Term_typeApplication_ta.body)
  case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.coderUtils.isTrivialTerm(v_Term_typeLambda_tl.body)
  case _ => false

def isSelfTailRecursive(funcName: hydra.core.Name)(body: hydra.core.Term): Boolean =
  {
  lazy val callsSelf: Boolean = hydra.lib.logic.not(hydra.rewriting.isFreeVariableInTerm(funcName)(body))
  hydra.lib.logic.ifElse[Boolean](callsSelf)(hydra.coderUtils.isTailRecursiveInTailPosition(funcName)(body))(false)
}

def isTailRecursiveInTailPosition(funcName: hydra.core.Name)(term: hydra.core.Term): Boolean =
  {
  lazy val stripped: hydra.core.Term = hydra.rewriting.deannotateAndDetypeTerm(term)
  stripped match
    case hydra.core.Term.application => {
      lazy val gathered: Tuple2[Seq[hydra.core.Term], hydra.core.Term] = hydra.coderUtils.gatherApplications(stripped)
      {
        lazy val gatherArgs: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term], hydra.core.Term](gathered)
        {
          lazy val gatherFun: hydra.core.Term = hydra.lib.pairs.second[Seq[hydra.core.Term], hydra.core.Term](gathered)
          {
            lazy val strippedFun: hydra.core.Term = hydra.rewriting.deannotateAndDetypeTerm(gatherFun)
            strippedFun match
              case hydra.core.Term.variable(v_Term_variable_vname) => hydra.lib.logic.ifElse[Boolean](hydra.lib.equality.equal[hydra.core.Name](v_Term_variable_vname)(funcName))({
                lazy val argsNoFunc: Boolean = hydra.lib.lists.foldl[Boolean, hydra.core.Term]((ok: Boolean) =>
                  (arg: hydra.core.Term) =>
                  hydra.lib.logic.and(ok)(hydra.rewriting.isFreeVariableInTerm(funcName)(arg)))(true)(gatherArgs)
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
              })(hydra.rewriting.isFreeVariableInTerm(funcName)(term))
              case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
                case hydra.core.Function.elimination(v_Function_elimination_e) => v_Function_elimination_e match
                  case hydra.core.Elimination.union(v_Elimination_union_cs) => {
                    lazy val `cases_`: Seq[hydra.core.Field] = (v_Elimination_union_cs.cases)
                    {
                      lazy val dflt: Option[hydra.core.Term] = (v_Elimination_union_cs.default)
                      {
                        lazy val branchesOk: Boolean = hydra.lib.lists.foldl[Boolean, hydra.core.Field]((ok: Boolean) =>
                          (field: hydra.core.Field) =>
                          hydra.lib.logic.and(ok)(hydra.coderUtils.isTailRecursiveInTailPosition(funcName)(field.term)))(true)(`cases_`)
                        {
                          lazy val dfltOk: Boolean = hydra.lib.maybes.maybe[Boolean, hydra.core.Term](true)((d: hydra.core.Term) => hydra.coderUtils.isTailRecursiveInTailPosition(funcName)(d))(dflt)
                          {
                            lazy val argsOk: Boolean = hydra.lib.lists.foldl[Boolean, hydra.core.Term]((ok: Boolean) =>
                              (arg: hydra.core.Term) =>
                              hydra.lib.logic.and(ok)(hydra.rewriting.isFreeVariableInTerm(funcName)(arg)))(true)(gatherArgs)
                            hydra.lib.logic.and(hydra.lib.logic.and(branchesOk)(dfltOk))(argsOk)
                          }
                        }
                      }
                    }
                  }
                  case _ => hydra.rewriting.isFreeVariableInTerm(funcName)(term)
                case _ => hydra.rewriting.isFreeVariableInTerm(funcName)(term)
              case _ => hydra.rewriting.isFreeVariableInTerm(funcName)(term)
          }
        }
      }
    }
    case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
      case hydra.core.Function.lambda(v_Function_lambda_lam) => hydra.coderUtils.isTailRecursiveInTailPosition(funcName)(v_Function_lambda_lam.body)
      case _ => hydra.rewriting.isFreeVariableInTerm(funcName)(term)
    case hydra.core.Term.let(v_Term_let_lt) => {
      lazy val bindingsOk: Boolean = hydra.lib.lists.foldl[Boolean, hydra.core.Binding]((ok: Boolean) =>
        (b: hydra.core.Binding) =>
        hydra.lib.logic.and(ok)(hydra.rewriting.isFreeVariableInTerm(funcName)(b.term)))(true)(v_Term_let_lt.bindings)
      hydra.lib.logic.and(bindingsOk)(hydra.coderUtils.isTailRecursiveInTailPosition(funcName)(v_Term_let_lt.body))
    }
    case _ => hydra.rewriting.isFreeVariableInTerm(funcName)(term)
}

def nameToFilePath(nsConv: hydra.util.CaseConvention)(localConv: hydra.util.CaseConvention)(ext: hydra.module.FileExtension)(name: hydra.core.Name): scala.Predef.String =
  {
  lazy val qualName: hydra.module.QualifiedName = hydra.names.qualifyName(name)
  lazy val ns: Option[hydra.module.Namespace] = (qualName.namespace)
  lazy val local: scala.Predef.String = (qualName.local)
  def nsToFilePath(ns2: hydra.module.Namespace): scala.Predef.String =
    hydra.lib.strings.intercalate("/")(hydra.lib.lists.map[scala.Predef.String, scala.Predef.String]((part: scala.Predef.String) =>
    hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(nsConv)(part))(hydra.lib.strings.splitOn(".")(ns2)))
  lazy val prefix: scala.Predef.String = hydra.lib.maybes.maybe[scala.Predef.String, hydra.module.Namespace]("")((n: hydra.module.Namespace) => hydra.lib.strings.cat2(nsToFilePath(n))("/"))(ns)
  lazy val suffix: scala.Predef.String = hydra.formatting.convertCase(hydra.util.CaseConvention.pascal)(localConv)(local)
  hydra.lib.strings.cat(Seq(prefix, suffix, ".", ext))
}

def reorderDefs(defs: Seq[hydra.module.Definition]): Seq[hydra.module.Definition] =
  {
  lazy val partitioned: Tuple2[Seq[hydra.module.TypeDefinition], Seq[hydra.module.TermDefinition]] = hydra.schemas.partitionDefinitions(defs)
  lazy val typeDefsRaw: Seq[hydra.module.TypeDefinition] = hydra.lib.pairs.first[Seq[hydra.module.TypeDefinition],
     Seq[hydra.module.TermDefinition]](partitioned)
  lazy val nameFirst: Seq[hydra.module.TypeDefinition] = hydra.lib.lists.filter[hydra.module.TypeDefinition]((td: hydra.module.TypeDefinition) =>
    hydra.lib.equality.equal[hydra.core.Name](td.name)("hydra.core.Name"))(typeDefsRaw)
  lazy val nameRest: Seq[hydra.module.TypeDefinition] = hydra.lib.lists.filter[hydra.module.TypeDefinition]((td: hydra.module.TypeDefinition) =>
    hydra.lib.logic.not(hydra.lib.equality.equal[hydra.core.Name](td.name)("hydra.core.Name")))(typeDefsRaw)
  lazy val typeDefs: Seq[hydra.module.Definition] = hydra.lib.lists.concat[hydra.module.Definition](Seq(hydra.lib.lists.map[hydra.module.TypeDefinition,
     hydra.module.Definition]((td: hydra.module.TypeDefinition) => hydra.module.Definition.`type`(td))(nameFirst),
     hydra.lib.lists.map[hydra.module.TypeDefinition, hydra.module.Definition]((td: hydra.module.TypeDefinition) => hydra.module.Definition.`type`(td))(nameRest)))
  lazy val termDefsWrapped: Seq[hydra.module.Definition] = hydra.lib.lists.map[hydra.module.TermDefinition,
     hydra.module.Definition]((td: hydra.module.TermDefinition) => hydra.module.Definition.term(td))(hydra.lib.pairs.second[Seq[hydra.module.TypeDefinition],
     Seq[hydra.module.TermDefinition]](partitioned))
  lazy val sortedTermDefs: Seq[hydra.module.Definition] = hydra.lib.lists.concat[hydra.module.Definition](hydra.sorting.topologicalSortNodes((d: hydra.module.Definition) =>
    d match
    case hydra.module.Definition.term(v_Definition_term_td) => (v_Definition_term_td.name))((d: hydra.module.Definition) =>
    d match
    case hydra.module.Definition.term(v_Definition_term_td) => hydra.lib.sets.toList[hydra.core.Name](hydra.rewriting.freeVariablesInTerm(v_Definition_term_td.term))
    case _ => Seq())(termDefsWrapped))
  hydra.lib.lists.concat[hydra.module.Definition](Seq(typeDefs, sortedTermDefs))
}

def commentsFromBinding(cx: hydra.context.Context)(g: hydra.graph.Graph)(b: hydra.core.Binding): Either[hydra.context.InContext[hydra.errors.Error],
   Option[scala.Predef.String]] = hydra.annotations.getTermDescription(cx)(g)(b.term)

def commentsFromFieldType(cx: hydra.context.Context)(g: hydra.graph.Graph)(ft: hydra.core.FieldType): Either[hydra.context.InContext[hydra.errors.Error],
   Option[scala.Predef.String]] = hydra.annotations.getTypeDescription(cx)(g)(ft.`type`)

def typeOfTerm(cx: hydra.context.Context)(g: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type] =
  hydra.lib.eithers.map[Tuple2[hydra.core.Type, hydra.context.Context], hydra.core.Type, hydra.context.InContext[hydra.errors.Error]](hydra.lib.pairs.first[hydra.core.Type,
     hydra.context.Context])(hydra.checking.typeOf(cx)(g)(Seq())(term))

def bindingMetadata(tc: hydra.graph.Graph)(b: hydra.core.Binding): Option[hydra.core.Term] =
  hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.coderUtils.isComplexBinding(tc)(b))(Some(hydra.core.Term.literal(hydra.core.Literal.boolean(true))))(None)

def analyzeFunctionTerm[T0, T1](cx: hydra.context.Context)(getTC: (T0 => hydra.graph.Graph))(setTC: (hydra.graph.Graph => T0 => T0))(env: T0)(term: hydra.core.Term): Either[T1,
   hydra.typing.FunctionStructure[T0]] =
  hydra.coderUtils.analyzeFunctionTermWith(cx)(hydra.coderUtils.bindingMetadata)(getTC)(setTC)(env)(term)

def analyzeFunctionTermWith[T0, T1](cx: hydra.context.Context)(forBinding: (hydra.graph.Graph => hydra.core.Binding => Option[hydra.core.Term]))(getTC: (T0 => hydra.graph.Graph))(setTC: (hydra.graph.Graph => T0 => T0))(env: T0)(term: hydra.core.Term): Either[T1,
   hydra.typing.FunctionStructure[T0]] =
  hydra.coderUtils.analyzeFunctionTermWith_gather(cx)(forBinding)(getTC)(setTC)(true)(env)(Seq())(Seq())(Seq())(Seq())(Seq())(term)

def analyzeFunctionTermWith_finish[T0, T1](cx: hydra.context.Context)(getTC: (T0 => hydra.graph.Graph))(fEnv: T0)(tparams: Seq[hydra.core.Name])(args: Seq[hydra.core.Name])(bindings: Seq[hydra.core.Binding])(doms: Seq[hydra.core.Type])(tapps: Seq[hydra.core.Type])(body: hydra.core.Term): Either[T1,
   hydra.typing.FunctionStructure[T0]] =
  {
  lazy val bodyWithTapps: hydra.core.Term = hydra.lib.lists.foldl[hydra.core.Term, hydra.core.Type]((trm: hydra.core.Term) =>
    (typ: hydra.core.Type) =>
    hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(trm, typ)))(body)(tapps)
  lazy val mcod: Option[hydra.core.Type] = hydra.lib.eithers.either[hydra.context.InContext[hydra.errors.Error],
     hydra.core.Type, Option[hydra.core.Type]]((_x: hydra.context.InContext[hydra.errors.Error]) => None)((c: hydra.core.Type) => Some(c))(hydra.coderUtils.typeOfTerm(cx)(getTC(fEnv))(bodyWithTapps))
  Right(hydra.typing.FunctionStructure(hydra.lib.lists.reverse[hydra.core.Name](tparams), hydra.lib.lists.reverse[hydra.core.Name](args),
     bindings, bodyWithTapps, hydra.lib.lists.reverse[hydra.core.Type](doms), mcod, fEnv))
}

def analyzeFunctionTermWith_gather[T0, T1](cx: hydra.context.Context)(forBinding: (hydra.graph.Graph => hydra.core.Binding => Option[hydra.core.Term]))(getTC: (T0 => hydra.graph.Graph))(setTC: (hydra.graph.Graph => T0 => T0))(argMode: Boolean)(gEnv: T0)(tparams: Seq[hydra.core.Name])(args: Seq[hydra.core.Name])(bindings: Seq[hydra.core.Binding])(doms: Seq[hydra.core.Type])(tapps: Seq[hydra.core.Type])(t: hydra.core.Term): Either[T1,
   hydra.typing.FunctionStructure[T0]] =
  hydra.rewriting.deannotateTerm(t) match
  case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
    case hydra.core.Function.lambda(v_Function_lambda_lam) => hydra.lib.logic.ifElse[Either[T1, hydra.typing.FunctionStructure[T0]]](argMode)({
      lazy val v: hydra.core.Name = (v_Function_lambda_lam.parameter)
      {
        lazy val dom: hydra.core.Type = hydra.lib.maybes.maybe[hydra.core.Type, hydra.core.Type](hydra.core.Type.variable("_"))((`x_`: hydra.core.Type) => `x_`)(v_Function_lambda_lam.domain)
        {
          lazy val body: hydra.core.Term = (v_Function_lambda_lam.body)
          {
            lazy val newEnv: T0 = setTC(hydra.rewriting.extendGraphForLambda(getTC(gEnv))(v_Function_lambda_lam))(gEnv)
            hydra.coderUtils.analyzeFunctionTermWith_gather(cx)(forBinding)(getTC)(setTC)(argMode)(newEnv)(tparams)(hydra.lib.lists.cons[hydra.core.Name](v)(args))(bindings)(hydra.lib.lists.cons[hydra.core.Type](dom)(doms))(tapps)(body)
          }
        }
      }
    })(hydra.coderUtils.analyzeFunctionTermWith_finish(cx)(getTC)(gEnv)(tparams)(args)(bindings)(doms)(tapps)(t))
    case _ => hydra.coderUtils.analyzeFunctionTermWith_finish(cx)(getTC)(gEnv)(tparams)(args)(bindings)(doms)(tapps)(t)
  case hydra.core.Term.let(v_Term_let_lt) => {
    lazy val newBindings: Seq[hydra.core.Binding] = (v_Term_let_lt.bindings)
    {
      lazy val body: hydra.core.Term = (v_Term_let_lt.body)
      {
        lazy val newEnv: T0 = setTC(hydra.rewriting.extendGraphForLet(forBinding)(getTC(gEnv))(v_Term_let_lt))(gEnv)
        hydra.coderUtils.analyzeFunctionTermWith_gather(cx)(forBinding)(getTC)(setTC)(false)(newEnv)(tparams)(args)(hydra.lib.lists.concat2[hydra.core.Binding](bindings)(newBindings))(doms)(tapps)(body)
      }
    }
  }
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => {
    lazy val taBody: hydra.core.Term = (v_Term_typeApplication_ta.body)
    {
      lazy val typ: hydra.core.Type = (v_Term_typeApplication_ta.`type`)
      hydra.coderUtils.analyzeFunctionTermWith_gather(cx)(forBinding)(getTC)(setTC)(argMode)(gEnv)(tparams)(args)(bindings)(doms)(hydra.lib.lists.cons[hydra.core.Type](typ)(tapps))(taBody)
    }
  }
  case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => {
    lazy val tvar: hydra.core.Name = (v_Term_typeLambda_tl.parameter)
    {
      lazy val tlBody: hydra.core.Term = (v_Term_typeLambda_tl.body)
      {
        lazy val newEnv: T0 = setTC(hydra.rewriting.extendGraphForTypeLambda(getTC(gEnv))(v_Term_typeLambda_tl))(gEnv)
        hydra.coderUtils.analyzeFunctionTermWith_gather(cx)(forBinding)(getTC)(setTC)(argMode)(newEnv)(hydra.lib.lists.cons[hydra.core.Name](tvar)(tparams))(args)(bindings)(doms)(tapps)(tlBody)
      }
    }
  }
  case _ => hydra.coderUtils.analyzeFunctionTermWith_finish(cx)(getTC)(gEnv)(tparams)(args)(bindings)(doms)(tapps)(t)

def reorderDefs(defs: Seq[hydra.module.Definition]): Seq[hydra.module.Definition] =
  {
  lazy val partitioned: Tuple2[Seq[hydra.module.TypeDefinition], Seq[hydra.module.TermDefinition]] = hydra.schemas.partitionDefinitions(defs)
  lazy val typeDefsRaw: Seq[hydra.module.TypeDefinition] = hydra.lib.pairs.first[Seq[hydra.module.TypeDefinition],
     Seq[hydra.module.TermDefinition]](partitioned)
  lazy val nameFirst: Seq[hydra.module.TypeDefinition] = hydra.lib.lists.filter[hydra.module.TypeDefinition]((td: hydra.module.TypeDefinition) =>
    hydra.lib.equality.equal[hydra.core.Name](td.name)("hydra.core.Name"))(typeDefsRaw)
  lazy val nameRest: Seq[hydra.module.TypeDefinition] = hydra.lib.lists.filter[hydra.module.TypeDefinition]((td: hydra.module.TypeDefinition) =>
    hydra.lib.logic.not(hydra.lib.equality.equal[hydra.core.Name](td.name)("hydra.core.Name")))(typeDefsRaw)
  lazy val typeDefs: Seq[hydra.module.Definition] = hydra.lib.lists.concat[hydra.module.Definition](Seq(hydra.lib.lists.map[hydra.module.TypeDefinition,
     hydra.module.Definition]((td: hydra.module.TypeDefinition) => hydra.module.Definition.`type`(td))(nameFirst),
     hydra.lib.lists.map[hydra.module.TypeDefinition, hydra.module.Definition]((td: hydra.module.TypeDefinition) => hydra.module.Definition.`type`(td))(nameRest)))
  lazy val termDefsWrapped: Seq[hydra.module.Definition] = hydra.lib.lists.map[hydra.module.TermDefinition,
     hydra.module.Definition]((td: hydra.module.TermDefinition) => hydra.module.Definition.term(td))(hydra.lib.pairs.second[Seq[hydra.module.TypeDefinition],
     Seq[hydra.module.TermDefinition]](partitioned))
  lazy val sortedTermDefs: Seq[hydra.module.Definition] = hydra.lib.lists.concat[hydra.module.Definition](hydra.sorting.topologicalSortNodes((d: hydra.module.Definition) =>
    d match
    case hydra.module.Definition.term(v_Definition_term_td) => (v_Definition_term_td.name))((d: hydra.module.Definition) =>
    d match
    case hydra.module.Definition.term(v_Definition_term_td) => hydra.lib.sets.toList[hydra.core.Name](hydra.rewriting.freeVariablesInTerm(v_Definition_term_td.term))
    case _ => Seq())(termDefsWrapped))
  hydra.lib.lists.concat[hydra.module.Definition](Seq(typeDefs, sortedTermDefs))
}
