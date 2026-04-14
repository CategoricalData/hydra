package hydra.python.coder

import hydra.coders.*

import hydra.core.*

import hydra.errors.*

import hydra.graph.*

import hydra.packaging.*

import hydra.python.environment.*

import hydra.python.syntax.*

import hydra.typing.*

import hydra.util.*

def analyzePythonFunction[T0](cx: hydra.context.Context)(env: hydra.python.environment.PythonEnvironment)(term: hydra.core.Term): Either[T0,
   hydra.typing.FunctionStructure[hydra.python.environment.PythonEnvironment]] =
  hydra.analysis.analyzeFunctionTermWith(cx)(hydra.python.coder.pythonBindingMetadata)(hydra.python.coder.pythonEnvironmentGetGraph)(hydra.python.coder.pythonEnvironmentSetGraph)(env)(term)

def classVariantPatternUnit(pyVariantName: hydra.python.syntax.Name): hydra.python.syntax.ClosedPattern =
  hydra.python.syntax.ClosedPattern.`class`(hydra.python.syntax.ClassPattern(Seq(pyVariantName), None, None))

def classVariantPatternWithCapture(env: hydra.python.environment.PythonEnvironment)(pyVariantName: hydra.python.syntax.Name)(varName: hydra.core.Name): hydra.python.syntax.ClosedPattern =
  {
  lazy val pyVarNameAttr: hydra.python.syntax.NameOrAttribute = Seq(pyVariantName)
  lazy val capturePattern: hydra.python.syntax.ClosedPattern = hydra.python.syntax.ClosedPattern.capture(hydra.python.names.encodeName(false)(hydra.util.CaseConvention.lowerSnake)(env)(varName))
  lazy val keywordPattern: hydra.python.syntax.KeywordPattern = hydra.python.syntax.KeywordPattern("value",
     hydra.python.syntax.Pattern.or(Seq(capturePattern)))
  hydra.python.syntax.ClosedPattern.`class`(hydra.python.syntax.ClassPattern(pyVarNameAttr,
     None, Some(Seq(keywordPattern))))
}

def collectTypeVariables(initial: scala.collection.immutable.Set[hydra.core.Name])(typ: hydra.core.Type): scala.collection.immutable.Set[hydra.core.Name] =
  hydra.strip.deannotateType(typ) match
  case hydra.core.Type.forall(v_Type_forall_ft) => {
    lazy val v: hydra.core.Name = (v_Type_forall_ft.parameter)
    {
      lazy val body: hydra.core.Type = (v_Type_forall_ft.body)
      hydra.python.coder.collectTypeVariables(hydra.lib.sets.insert[hydra.core.Name](v)(initial))(body)
    }
  }
  case _ => {
    lazy val freeVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.variables.freeVariablesInType(typ)
    {
      def isTypeVar(n: hydra.core.Name): Boolean = hydra.python.coder.isTypeVariableName(n)
      {
        lazy val filteredList: Seq[hydra.core.Name] = hydra.lib.lists.filter[hydra.core.Name](isTypeVar)(hydra.lib.sets.toList[hydra.core.Name](freeVars))
        hydra.lib.sets.union[hydra.core.Name](initial)(hydra.lib.sets.fromList[hydra.core.Name](filteredList))
      }
    }
  }

def condImportSymbol[T0](name: T0)(flag: Boolean): Option[T0] = hydra.lib.logic.ifElse[Option[T0]](flag)(Some(name))(None)

lazy val dataclassDecorator: hydra.python.syntax.NamedExpression = hydra.python.syntax.NamedExpression.simple(hydra.python.utils.pyPrimaryToPyExpression(hydra.python.utils.primaryWithRhs(hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name("dataclass")))(hydra.python.syntax.PrimaryRhs.call(hydra.python.syntax.Args(Seq(),
   Seq(hydra.python.syntax.KwargOrStarred.kwarg(hydra.python.syntax.Kwarg("frozen",
   hydra.python.utils.pyAtomToPyExpression(hydra.python.syntax.Atom.`true`)))), Seq())))))

def deconflictVariantName(isQualified: Boolean)(env: hydra.python.environment.PythonEnvironment)(unionName: hydra.core.Name)(fname: hydra.core.Name)(g: hydra.graph.Graph): hydra.python.syntax.Name =
  {
  lazy val candidateHydraName: hydra.core.Name = hydra.lib.strings.cat2(unionName)(hydra.formatting.capitalize(fname))
  lazy val termCollision: Boolean = hydra.lib.maps.member[hydra.core.Name, hydra.core.Term](candidateHydraName)(g.boundTerms)
  lazy val typeCollision: Boolean = hydra.lib.maps.member[hydra.core.Name, hydra.core.TypeScheme](candidateHydraName)(g.schemaTypes)
  lazy val collision: Boolean = hydra.lib.logic.or(termCollision)(typeCollision)
  hydra.lib.logic.ifElse[hydra.python.syntax.Name](collision)(hydra.lib.strings.cat2(hydra.python.names.variantName(isQualified)(env)(unionName)(fname))("_"))(hydra.python.names.variantName(isQualified)(env)(unionName)(fname))
}

def deduplicateCaseVariables(`cases_`: Seq[hydra.core.Field]): Seq[hydra.core.Field] =
  {
  def rewriteCase(state: Tuple2[Map[hydra.core.Name, Int], Seq[hydra.core.Field]])(field: hydra.core.Field): Tuple2[Map[hydra.core.Name,
     Int], Seq[hydra.core.Field]] =
    {
    lazy val countByName: Map[hydra.core.Name, Int] = hydra.lib.pairs.first[Map[hydra.core.Name,
       Int], Seq[hydra.core.Field]](state)
    lazy val done: Seq[hydra.core.Field] = hydra.lib.pairs.second[Map[hydra.core.Name,
       Int], Seq[hydra.core.Field]](state)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    hydra.strip.deannotateAndDetypeTerm(fterm) match
      case hydra.core.Term.lambda(v_Term_lambda_lam) => {
        lazy val v: hydra.core.Name = (v_Term_lambda_lam.parameter)
        {
          lazy val mdom: Option[hydra.core.Type] = (v_Term_lambda_lam.domain)
          {
            lazy val body: hydra.core.Term = (v_Term_lambda_lam.body)
            hydra.lib.maybes.maybe[Tuple2[Map[hydra.core.Name, Int], Seq[hydra.core.Field]],
               Int](Tuple2(hydra.lib.maps.insert[hydra.core.Name, Int](v)(1)(countByName),
               hydra.lib.lists.cons[hydra.core.Field](field)(done)))((count: Int) =>
              {
              lazy val count2: Int = hydra.lib.math.add(count)(1)
              {
                lazy val v2: hydra.core.Name = hydra.lib.strings.cat2(v)(hydra.lib.literals.showInt32(count2))
                {
                  lazy val newBody: hydra.core.Term = hydra.reduction.alphaConvert(v)(v2)(body)
                  {
                    lazy val newLam: hydra.core.Lambda = hydra.core.Lambda(v2, mdom, newBody)
                    {
                      lazy val newTerm: hydra.core.Term = hydra.core.Term.lambda(newLam)
                      {
                        lazy val newField: hydra.core.Field = hydra.core.Field(fname, newTerm)
                        Tuple2(hydra.lib.maps.insert[hydra.core.Name, Int](v)(count2)(countByName),
                           hydra.lib.lists.cons[hydra.core.Field](newField)(done))
                      }
                    }
                  }
                }
              }
            })(hydra.lib.maps.lookup[hydra.core.Name, Int](v)(countByName))
          }
        }
      }
      case _ => Tuple2(countByName, hydra.lib.lists.cons[hydra.core.Field](field)(done))
  }
  lazy val result: Tuple2[Map[hydra.core.Name, Int], Seq[hydra.core.Field]] = hydra.lib.lists.foldl[Tuple2[Map[hydra.core.Name,
     Int], Seq[hydra.core.Field]], hydra.core.Field](rewriteCase)(Tuple2(hydra.lib.maps.empty[hydra.core.Name,
     Int], Seq()))(`cases_`)
  hydra.lib.lists.reverse[hydra.core.Field](hydra.lib.pairs.second[Map[hydra.core.Name,
     Int], Seq[hydra.core.Field]](result))
}

def digForWrap(isTermAnnot: Boolean)(meta: hydra.python.environment.PythonModuleMetadata)(typ: hydra.core.Type): hydra.python.environment.PythonModuleMetadata =
  hydra.strip.deannotateType(typ) match
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.python.coder.digForWrap(isTermAnnot)(meta)(v_Type_forall_ft.body)
  case hydra.core.Type.wrap(v_Type_wrap__) => hydra.lib.logic.ifElse[hydra.python.environment.PythonModuleMetadata](isTermAnnot)(meta)(hydra.python.coder.setMetaUsesNode(meta)(true))
  case _ => meta

def eliminateUnitVar(v: hydra.core.Name)(term0: hydra.core.Term): hydra.core.Term =
  {
  def rewriteField(rewrite: (hydra.core.Term => hydra.core.Term))(fld: hydra.core.Field): hydra.core.Field = hydra.core.Field(fld.name,
     rewrite(fld.term))
  def rewriteBinding(rewrite: (hydra.core.Term => hydra.core.Term))(bnd: hydra.core.Binding): hydra.core.Binding = hydra.core.Binding(bnd.name,
     rewrite(bnd.term), (bnd.`type`))
  def rewrite(recurse: (hydra.core.Term => hydra.core.Term))(term: hydra.core.Term): hydra.core.Term =
    hydra.strip.deannotateAndDetypeTerm(term) match
    case hydra.core.Term.variable(v_Term_variable_n) => hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Name](v_Term_variable_n)(v))(hydra.core.Term.unit)(term)
    case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.core.Term.annotated(hydra.core.AnnotatedTerm(recurse(v_Term_annotated_at.body),
       (v_Term_annotated_at.annotation)))
    case hydra.core.Term.application(v_Term_application_app) => hydra.core.Term.application(hydra.core.Application(recurse(v_Term_application_app.function),
       recurse(v_Term_application_app.argument)))
    case hydra.core.Term.lambda(v_Term_lambda_lam) => hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Name](v_Term_lambda_lam.parameter)(v))(term)(hydra.core.Term.lambda(hydra.core.Lambda(v_Term_lambda_lam.parameter,
       (v_Term_lambda_lam.domain), recurse(v_Term_lambda_lam.body))))
    case hydra.core.Term.cases(v_Term_cases_cs) => hydra.core.Term.cases(hydra.core.CaseStatement(v_Term_cases_cs.typeName,
       hydra.lib.maybes.map[hydra.core.Term, hydra.core.Term](recurse)(v_Term_cases_cs.default),
       hydra.lib.lists.map[hydra.core.Field, hydra.core.Field]((v1: hydra.core.Field) => rewriteField(recurse)(v1))(v_Term_cases_cs.cases)))
    case hydra.core.Term.let(v_Term_let_lt) => hydra.core.Term.let(hydra.core.Let(hydra.lib.lists.map[hydra.core.Binding,
       hydra.core.Binding]((v1: hydra.core.Binding) => rewriteBinding(recurse)(v1))(v_Term_let_lt.bindings),
       recurse(v_Term_let_lt.body)))
    case hydra.core.Term.list(v_Term_list_ts) => hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Term,
       hydra.core.Term](recurse)(v_Term_list_ts))
    case hydra.core.Term.map(v_Term_map_m) => hydra.core.Term.map(hydra.lib.maps.fromList[hydra.core.Term,
       hydra.core.Term](hydra.lib.lists.map[Tuple2[hydra.core.Term, hydra.core.Term],
       Tuple2[hydra.core.Term, hydra.core.Term]]((kv: Tuple2[hydra.core.Term, hydra.core.Term]) =>
      Tuple2(recurse(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](kv)),
         recurse(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](kv))))(hydra.lib.maps.toList[hydra.core.Term,
         hydra.core.Term](v_Term_map_m))))
    case hydra.core.Term.record(v_Term_record_rec) => hydra.core.Term.record(hydra.core.Record(v_Term_record_rec.typeName,
       hydra.lib.lists.map[hydra.core.Field, hydra.core.Field]((v1: hydra.core.Field) => rewriteField(recurse)(v1))(v_Term_record_rec.fields)))
    case hydra.core.Term.set(v_Term_set_s) => hydra.core.Term.set(hydra.lib.sets.map[hydra.core.Term,
       hydra.core.Term](recurse)(v_Term_set_s))
    case hydra.core.Term.inject(v_Term_inject_inj) => hydra.core.Term.inject(hydra.core.Injection(v_Term_inject_inj.typeName,
       rewriteField(recurse)(v_Term_inject_inj.field)))
    case hydra.core.Term.maybe(v_Term_maybe_mt) => hydra.core.Term.maybe(hydra.lib.maybes.map[hydra.core.Term,
       hydra.core.Term](recurse)(v_Term_maybe_mt))
    case hydra.core.Term.pair(v_Term_pair_p) => hydra.core.Term.pair(Tuple2(recurse(hydra.lib.pairs.first[hydra.core.Term,
       hydra.core.Term](v_Term_pair_p)), recurse(hydra.lib.pairs.second[hydra.core.Term,
       hydra.core.Term](v_Term_pair_p))))
    case hydra.core.Term.wrap(v_Term_wrap_wt) => hydra.core.Term.wrap(hydra.core.WrappedTerm(v_Term_wrap_wt.typeName,
       recurse(v_Term_wrap_wt.body)))
    case hydra.core.Term.either(v_Term_either_e) => hydra.core.Term.either(hydra.lib.eithers.bimap[hydra.core.Term,
       hydra.core.Term, hydra.core.Term, hydra.core.Term](recurse)(recurse)(v_Term_either_e))
    case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(recurse(v_Term_typeApplication_ta.body),
       (v_Term_typeApplication_ta.`type`)))
    case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.core.Term.typeLambda(hydra.core.TypeLambda(v_Term_typeLambda_tl.parameter,
       recurse(v_Term_typeLambda_tl.body)))
    case _ => term
  def go(term: hydra.core.Term): hydra.core.Term = rewrite(go)(term)
  go(term0)
}

def emptyMetadata(ns: hydra.packaging.Namespaces[hydra.python.syntax.DottedName]): hydra.python.environment.PythonModuleMetadata =
  hydra.python.environment.PythonModuleMetadata(ns, hydra.lib.sets.empty[hydra.core.Name],
     false, false, false, false, false, false, false, false, false, false, false,
     false, false, false, false, false, false, false, false, false)

def encodeApplication(cx: hydra.context.Context)(env: hydra.python.environment.PythonEnvironment)(app: hydra.core.Application): Either[hydra.errors.Error,
   hydra.python.syntax.Expression] =
  {
  lazy val g: hydra.graph.Graph = hydra.python.coder.pythonEnvironmentGetGraph(env)
  lazy val term: hydra.core.Term = hydra.core.Term.application(app)
  lazy val gathered: Tuple2[hydra.core.Term, Seq[hydra.core.Term]] = hydra.analysis.gatherArgs(term)(Seq())
  lazy val fun: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, Seq[hydra.core.Term]](gathered)
  lazy val args: Seq[hydra.core.Term] = hydra.lib.pairs.second[hydra.core.Term, Seq[hydra.core.Term]](gathered)
  lazy val knownArity: Int = hydra.python.coder.termArityWithPrimitives(g)(fun)
  lazy val arity: Int = hydra.lib.math.max(knownArity)(hydra.lib.lists.length[hydra.core.Term](args))
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.python.syntax.Expression],
     hydra.python.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Term, hydra.python.syntax.Expression,
     hydra.errors.Error]((t: hydra.core.Term) => hydra.python.coder.encodeTermInline(cx)(env)(false)(t))(args))((pargs: Seq[hydra.python.syntax.Expression]) =>
    {
    lazy val hargs: Seq[hydra.python.syntax.Expression] = hydra.lib.lists.take[hydra.python.syntax.Expression](arity)(pargs)
    {
      lazy val rargs: Seq[hydra.python.syntax.Expression] = hydra.lib.lists.drop[hydra.python.syntax.Expression](arity)(pargs)
      hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[hydra.python.syntax.Expression,
         Seq[hydra.python.syntax.Expression]], hydra.python.syntax.Expression](hydra.python.coder.encodeApplicationInner(cx)(env)(fun)(hargs)(rargs))((result: Tuple2[hydra.python.syntax.Expression,
         Seq[hydra.python.syntax.Expression]]) =>
        {
        lazy val lhs: hydra.python.syntax.Expression = hydra.lib.pairs.first[hydra.python.syntax.Expression,
           Seq[hydra.python.syntax.Expression]](result)
        {
          lazy val remainingRargs: Seq[hydra.python.syntax.Expression] = hydra.lib.pairs.second[hydra.python.syntax.Expression,
             Seq[hydra.python.syntax.Expression]](result)
          {
            lazy val pyapp: hydra.python.syntax.Expression = hydra.lib.lists.foldl[hydra.python.syntax.Expression,
               hydra.python.syntax.Expression]((t: hydra.python.syntax.Expression) =>
              (a: hydra.python.syntax.Expression) =>
              hydra.python.utils.functionCall(hydra.python.utils.pyExpressionToPyPrimary(t))(Seq(a)))(lhs)(remainingRargs)
            Right(pyapp)
          }
        }
      })
    }
  })
}

def encodeApplicationInner(cx: hydra.context.Context)(env: hydra.python.environment.PythonEnvironment)(fun: hydra.core.Term)(hargs: Seq[hydra.python.syntax.Expression])(rargs: Seq[hydra.python.syntax.Expression]): Either[hydra.errors.Error,
   Tuple2[hydra.python.syntax.Expression, Seq[hydra.python.syntax.Expression]]] =
  {
  lazy val firstArg: hydra.python.syntax.Expression = hydra.lib.lists.head[hydra.python.syntax.Expression](hargs)
  lazy val restArgs: Seq[hydra.python.syntax.Expression] = hydra.lib.lists.tail[hydra.python.syntax.Expression](hargs)
  def withRest(e: hydra.python.syntax.Expression): hydra.python.syntax.Expression =
    hydra.lib.logic.ifElse[hydra.python.syntax.Expression](hydra.lib.lists.`null`[hydra.python.syntax.Expression](restArgs))(e)(hydra.python.utils.functionCall(hydra.python.utils.pyExpressionToPyPrimary(e))(restArgs))
  lazy val defaultCase: Either[hydra.errors.Error, Tuple2[hydra.python.syntax.Expression,
     Seq[hydra.python.syntax.Expression]]] = hydra.lib.eithers.bind[hydra.errors.Error,
     hydra.python.syntax.Expression, Tuple2[hydra.python.syntax.Expression, Seq[hydra.python.syntax.Expression]]](hydra.python.coder.encodeTermInline(cx)(env)(false)(fun))((pfun: hydra.python.syntax.Expression) =>
    Right(Tuple2(hydra.python.utils.functionCall(hydra.python.utils.pyExpressionToPyPrimary(pfun))(hargs), rargs)))
  hydra.strip.deannotateAndDetypeTerm(fun) match
    case hydra.core.Term.project(v_Term_project_proj) => {
      lazy val fname: hydra.core.Name = (v_Term_project_proj.field)
      {
        lazy val fieldExpr: hydra.python.syntax.Expression = hydra.python.utils.projectFromExpression(firstArg)(hydra.python.names.encodeFieldName(env)(fname))
        Right(Tuple2(withRest(fieldExpr), rargs))
      }
    }
    case hydra.core.Term.cases(v_Term_cases_cs) => hydra.lib.eithers.bind[hydra.errors.Error,
       hydra.python.syntax.Expression, Tuple2[hydra.python.syntax.Expression, Seq[hydra.python.syntax.Expression]]](hydra.python.coder.encodeUnionEliminationInline(cx)(env)(v_Term_cases_cs)(firstArg))((inlineExpr: hydra.python.syntax.Expression) => Right(Tuple2(withRest(inlineExpr),
       rargs)))
    case hydra.core.Term.unwrap(v_Term_unwrap__) => {
      lazy val valueExpr: hydra.python.syntax.Expression = hydra.python.utils.projectFromExpression(firstArg)("value")
      {
        lazy val allArgs: Seq[hydra.python.syntax.Expression] = hydra.lib.lists.concat2[hydra.python.syntax.Expression](restArgs)(rargs)
        hydra.lib.logic.ifElse[Either[hydra.errors.Error, Tuple2[hydra.python.syntax.Expression,
           Seq[hydra.python.syntax.Expression]]]](hydra.lib.lists.`null`[hydra.python.syntax.Expression](allArgs))(Right(Tuple2(valueExpr,
           Seq())))(Right(Tuple2(hydra.python.utils.functionCall(hydra.python.utils.pyExpressionToPyPrimary(valueExpr))(allArgs),
           Seq())))
      }
    }
    case hydra.core.Term.lambda(v_Term_lambda__) => hydra.lib.eithers.bind[hydra.errors.Error,
       hydra.python.syntax.Expression, Tuple2[hydra.python.syntax.Expression, Seq[hydra.python.syntax.Expression]]](hydra.python.coder.encodeTermInline(cx)(env)(false)(fun))((pfun: hydra.python.syntax.Expression) =>
      Right(Tuple2(hydra.python.utils.functionCall(hydra.python.utils.pyExpressionToPyPrimary(pfun))(hargs), rargs)))
    case hydra.core.Term.variable(v_Term_variable_name) => {
      lazy val g: hydra.graph.Graph = hydra.python.coder.pythonEnvironmentGetGraph(env)
      {
        lazy val allArgs: Seq[hydra.python.syntax.Expression] = hydra.lib.lists.concat2[hydra.python.syntax.Expression](hargs)(rargs)
        hydra.lib.maybes.cases[hydra.graph.Primitive, Either[hydra.errors.Error, Tuple2[hydra.python.syntax.Expression,
           Seq[hydra.python.syntax.Expression]]]](hydra.lib.maps.lookup[hydra.core.Name,
           hydra.graph.Primitive](v_Term_variable_name)(g.primitives))(hydra.lib.maybes.maybe[Either[hydra.errors.Error,
           Tuple2[hydra.python.syntax.Expression, Seq[hydra.python.syntax.Expression]]],
           hydra.core.Binding](hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Expression,
           Tuple2[hydra.python.syntax.Expression, Seq[hydra.python.syntax.Expression]]](hydra.python.coder.encodeVariable(cx)(env)(v_Term_variable_name)(hargs))((expr: hydra.python.syntax.Expression) => Right(Tuple2(expr,
           rargs))))((el: hydra.core.Binding) =>
          hydra.lib.maybes.maybe[Either[hydra.errors.Error, Tuple2[hydra.python.syntax.Expression,
             Seq[hydra.python.syntax.Expression]]], hydra.core.TypeScheme](hydra.lib.eithers.bind[hydra.errors.Error,
             hydra.python.syntax.Expression, Tuple2[hydra.python.syntax.Expression,
             Seq[hydra.python.syntax.Expression]]](hydra.python.coder.encodeVariable(cx)(env)(v_Term_variable_name)(hargs))((expr: hydra.python.syntax.Expression) => Right(Tuple2(expr,
             rargs))))((ts: hydra.core.TypeScheme) =>
          {
          lazy val elArity: Int = hydra.arity.typeSchemeArity(ts)
          {
            lazy val consumeCount: Int = hydra.lib.math.min(elArity)(hydra.lib.lists.length[hydra.python.syntax.Expression](allArgs))
            {
              lazy val consumedArgs: Seq[hydra.python.syntax.Expression] = hydra.lib.lists.take[hydra.python.syntax.Expression](consumeCount)(allArgs)
              {
                lazy val remainingArgs: Seq[hydra.python.syntax.Expression] = hydra.lib.lists.drop[hydra.python.syntax.Expression](consumeCount)(allArgs)
                hydra.lib.logic.ifElse[Either[hydra.errors.Error, Tuple2[hydra.python.syntax.Expression,
                   Seq[hydra.python.syntax.Expression]]]](hydra.lib.lists.`null`[hydra.python.syntax.Expression](consumedArgs))(hydra.lib.eithers.bind[hydra.errors.Error,
                   hydra.python.syntax.Expression, Tuple2[hydra.python.syntax.Expression,
                   Seq[hydra.python.syntax.Expression]]](hydra.python.coder.encodeVariable(cx)(env)(v_Term_variable_name)(Seq()))((expr: hydra.python.syntax.Expression) => Right(Tuple2(expr,
                   rargs))))(Right(Tuple2(hydra.python.utils.functionCall(hydra.python.utils.pyNameToPyPrimary(hydra.python.names.encodeName(true)(hydra.util.CaseConvention.lowerSnake)(env)(v_Term_variable_name)))(consumedArgs),
                   remainingArgs)))
              }
            }
          }
        })(el.`type`))(hydra.lexical.lookupBinding(g)(v_Term_variable_name)))((_prim: hydra.graph.Primitive) =>
          {
          lazy val wrappedArgs: Seq[hydra.python.syntax.Expression] = hydra.python.coder.wrapLazyArguments(v_Term_variable_name)(hargs)
          hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Expression,
             Tuple2[hydra.python.syntax.Expression, Seq[hydra.python.syntax.Expression]]](hydra.python.coder.encodeVariable(cx)(env)(v_Term_variable_name)(wrappedArgs))((expr: hydra.python.syntax.Expression) => Right(Tuple2(expr,
             rargs)))
        })
      }
    }
    case _ => defaultCase
}

def encodeApplicationType[T0](env: hydra.python.environment.PythonEnvironment)(at: hydra.core.ApplicationType): Either[T0,
   hydra.python.syntax.Expression] =
  {
  def gatherParams(t: hydra.core.Type)(ps: Seq[hydra.core.Type]): Tuple2[hydra.core.Type, Seq[hydra.core.Type]] =
    hydra.strip.deannotateType(t) match
    case hydra.core.Type.application(v_Type_application_appT) => gatherParams(v_Type_application_appT.function)(hydra.lib.lists.cons[hydra.core.Type](v_Type_application_appT.argument)(ps))
    case hydra.core.Type.annotated(v_Type_annotated__) => Tuple2(t, ps)
    case hydra.core.Type.function(v_Type_function__) => Tuple2(t, ps)
    case hydra.core.Type.forall(v_Type_forall__) => Tuple2(t, ps)
    case hydra.core.Type.list(v_Type_list__) => Tuple2(t, ps)
    case hydra.core.Type.literal(v_Type_literal__) => Tuple2(t, ps)
    case hydra.core.Type.map(v_Type_map__) => Tuple2(t, ps)
    case hydra.core.Type.maybe(v_Type_maybe__) => Tuple2(t, ps)
    case hydra.core.Type.either(v_Type_either__) => Tuple2(t, ps)
    case hydra.core.Type.pair(v_Type_pair__) => Tuple2(t, ps)
    case hydra.core.Type.record(v_Type_record__) => Tuple2(t, ps)
    case hydra.core.Type.set(v_Type_set__) => Tuple2(t, ps)
    case hydra.core.Type.union(v_Type_union__) => Tuple2(t, ps)
    case hydra.core.Type.unit => Tuple2(t, ps)
    case hydra.core.Type.variable(v_Type_variable__) => Tuple2(t, ps)
    case hydra.core.Type.void => Tuple2(t, ps)
    case hydra.core.Type.wrap(v_Type_wrap__) => Tuple2(t, ps)
  lazy val bodyAndArgs: Tuple2[hydra.core.Type, Seq[hydra.core.Type]] = gatherParams(hydra.core.Type.application(at))(Seq())
  lazy val body: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, Seq[hydra.core.Type]](bodyAndArgs)
  lazy val args: Seq[hydra.core.Type] = hydra.lib.pairs.second[hydra.core.Type, Seq[hydra.core.Type]](bodyAndArgs)
  hydra.lib.eithers.bind[T0, hydra.python.syntax.Expression, hydra.python.syntax.Expression](hydra.python.coder.encodeType(env)(body))((pyBody: hydra.python.syntax.Expression) =>
    hydra.lib.eithers.bind[T0, Seq[hydra.python.syntax.Expression], hydra.python.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Type,
       hydra.python.syntax.Expression, T0]((v1: hydra.core.Type) => hydra.python.coder.encodeType(env)(v1))(args))((pyArgs: Seq[hydra.python.syntax.Expression]) =>
    Right(hydra.python.utils.primaryAndParams(hydra.python.utils.pyExpressionToPyPrimary(pyBody))(pyArgs))))
}

def encodeBindingAs(cx: hydra.context.Context)(env: hydra.python.environment.PythonEnvironment)(binding: hydra.core.Binding): Either[hydra.errors.Error,
   hydra.python.syntax.Statement] =
  {
  lazy val name1: hydra.core.Name = (binding.name)
  lazy val term1: hydra.core.Term = (binding.term)
  lazy val mts: Option[hydra.core.TypeScheme] = (binding.`type`)
  lazy val fname: hydra.python.syntax.Name = hydra.python.names.encodeName(true)(hydra.util.CaseConvention.lowerSnake)(env)(name1)
  hydra.lib.maybes.maybe[Either[hydra.errors.Error, hydra.python.syntax.Statement], hydra.core.TypeScheme]({
    lazy val gathered: Tuple2[Seq[hydra.core.Name], hydra.core.Term] = hydra.python.coder.gatherLambdas(term1)
    {
      lazy val lambdaParams: Seq[hydra.core.Name] = hydra.lib.pairs.first[Seq[hydra.core.Name],
         hydra.core.Term](gathered)
      {
        lazy val innerBody: hydra.core.Term = hydra.lib.pairs.second[Seq[hydra.core.Name], hydra.core.Term](gathered)
        {
          lazy val mcsa: Option[Tuple2[hydra.core.Name, Tuple2[Option[hydra.core.Term],
             Tuple2[Seq[hydra.core.Field], hydra.core.Term]]]] = hydra.python.coder.isCaseStatementApplication(innerBody)
          hydra.lib.maybes.maybe[Either[hydra.errors.Error, hydra.python.syntax.Statement],
             Tuple2[hydra.core.Name, Tuple2[Option[hydra.core.Term], Tuple2[Seq[hydra.core.Field],
             hydra.core.Term]]]]({
            lazy val mcs: Option[hydra.core.CaseStatement] = hydra.python.coder.extractCaseElimination(term1)
            hydra.lib.maybes.maybe[Either[hydra.errors.Error, hydra.python.syntax.Statement],
               hydra.core.CaseStatement](hydra.lib.eithers.map[Seq[hydra.python.syntax.Statement],
               hydra.python.syntax.Statement, hydra.errors.Error]((stmts: Seq[hydra.python.syntax.Statement]) => hydra.lib.lists.head[hydra.python.syntax.Statement](stmts))(hydra.python.coder.encodeTermMultiline(cx)(env)(term1)))((cs: hydra.core.CaseStatement) =>
              {
              lazy val tname: hydra.core.Name = (cs.typeName)
              {
                lazy val dflt: Option[hydra.core.Term] = (cs.default)
                {
                  lazy val `cases_`: Seq[hydra.core.Field] = (cs.cases)
                  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.FieldType],
                     hydra.python.syntax.Statement](hydra.resolution.requireUnionType(cx)(hydra.python.coder.pythonEnvironmentGetGraph(env))(tname))((rt: Seq[hydra.core.FieldType]) =>
                    {
                    lazy val isEnum: Boolean = hydra.predicates.isEnumRowType(rt)
                    {
                      lazy val isFull: Boolean = hydra.python.coder.isCasesFull(rt)(`cases_`)
                      {
                        lazy val innerParam: hydra.python.syntax.Param = hydra.python.syntax.Param("x", None)
                        {
                          lazy val param: hydra.python.syntax.ParamNoDefault = hydra.python.syntax.ParamNoDefault(innerParam,
                             None)
                          {
                            lazy val params: hydra.python.syntax.Parameters = hydra.python.syntax.Parameters.paramNoDefault(hydra.python.syntax.ParamNoDefaultParameters(Seq(param),
                               Seq(), None))
                            hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.python.syntax.CaseBlock],
                               hydra.python.syntax.Statement](hydra.lib.eithers.mapList[hydra.core.Field,
                               hydra.python.syntax.CaseBlock, hydra.errors.Error]((v1: hydra.core.Field) =>
                              hydra.python.coder.encodeCaseBlock(cx)(env)(tname)(rt)(isEnum)((e: hydra.python.environment.PythonEnvironment) =>
                              (t: hydra.core.Term) => hydra.python.coder.encodeTermMultiline(cx)(e)(t))(v1))(`cases_`))((pyCases: Seq[hydra.python.syntax.CaseBlock]) =>
                              hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.python.syntax.CaseBlock],
                                 hydra.python.syntax.Statement](hydra.python.coder.encodeDefaultCaseBlock((t: hydra.core.Term) => hydra.python.coder.encodeTermInline(cx)(env)(false)(t))(isFull)(dflt)(tname))((pyDflt: Seq[hydra.python.syntax.CaseBlock]) =>
                              {
                              lazy val subj: hydra.python.syntax.SubjectExpression = hydra.python.syntax.SubjectExpression.simple(hydra.python.syntax.NamedExpression.simple(hydra.python.utils.pyNameToPyExpression("x")))
                              {
                                lazy val allCases: Seq[hydra.python.syntax.CaseBlock] = hydra.lib.lists.concat2[hydra.python.syntax.CaseBlock](pyCases)(pyDflt)
                                {
                                  lazy val matchStmt: hydra.python.syntax.Statement = hydra.python.syntax.Statement.compound(hydra.python.syntax.CompoundStatement.`match`(hydra.python.syntax.MatchStatement(subj,
                                     allCases)))
                                  {
                                    lazy val body: hydra.python.syntax.Block = hydra.python.utils.indentedBlock(None)(Seq(Seq(matchStmt)))
                                    {
                                      lazy val funcDefRaw: hydra.python.syntax.FunctionDefRaw = hydra.python.syntax.FunctionDefRaw(false,
                                         fname, Seq(), Some(params), None, None, body)
                                      Right(hydra.python.syntax.Statement.compound(hydra.python.syntax.CompoundStatement.function(hydra.python.syntax.FunctionDefinition(None,
                                         funcDefRaw))))
                                    }
                                  }
                                }
                              }
                            }))
                          }
                        }
                      }
                    }
                  })
                }
              }
            })(mcs)
          })((csa: Tuple2[hydra.core.Name, Tuple2[Option[hydra.core.Term], Tuple2[Seq[hydra.core.Field],
             hydra.core.Term]]]) =>
            hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.python.syntax.Statement]](hydra.lib.lists.`null`[hydra.core.Name](lambdaParams))({
            lazy val mcs: Option[hydra.core.CaseStatement] = hydra.python.coder.extractCaseElimination(term1)
            hydra.lib.maybes.maybe[Either[hydra.errors.Error, hydra.python.syntax.Statement],
               hydra.core.CaseStatement](hydra.lib.eithers.map[Seq[hydra.python.syntax.Statement],
               hydra.python.syntax.Statement, hydra.errors.Error]((stmts: Seq[hydra.python.syntax.Statement]) => hydra.lib.lists.head[hydra.python.syntax.Statement](stmts))(hydra.python.coder.encodeTermMultiline(cx)(env)(term1)))((cs: hydra.core.CaseStatement) =>
              {
              lazy val tname: hydra.core.Name = (cs.typeName)
              {
                lazy val dflt: Option[hydra.core.Term] = (cs.default)
                {
                  lazy val `cases_`: Seq[hydra.core.Field] = (cs.cases)
                  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.FieldType],
                     hydra.python.syntax.Statement](hydra.resolution.requireUnionType(cx)(hydra.python.coder.pythonEnvironmentGetGraph(env))(tname))((rt: Seq[hydra.core.FieldType]) =>
                    {
                    lazy val isEnum: Boolean = hydra.predicates.isEnumRowType(rt)
                    {
                      lazy val isFull: Boolean = hydra.python.coder.isCasesFull(rt)(`cases_`)
                      {
                        lazy val innerParam: hydra.python.syntax.Param = hydra.python.syntax.Param("x", None)
                        {
                          lazy val param: hydra.python.syntax.ParamNoDefault = hydra.python.syntax.ParamNoDefault(innerParam,
                             None)
                          {
                            lazy val params: hydra.python.syntax.Parameters = hydra.python.syntax.Parameters.paramNoDefault(hydra.python.syntax.ParamNoDefaultParameters(Seq(param),
                               Seq(), None))
                            hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.python.syntax.CaseBlock],
                               hydra.python.syntax.Statement](hydra.lib.eithers.mapList[hydra.core.Field,
                               hydra.python.syntax.CaseBlock, hydra.errors.Error]((v1: hydra.core.Field) =>
                              hydra.python.coder.encodeCaseBlock(cx)(env)(tname)(rt)(isEnum)((e: hydra.python.environment.PythonEnvironment) =>
                              (t: hydra.core.Term) => hydra.python.coder.encodeTermMultiline(cx)(e)(t))(v1))(`cases_`))((pyCases: Seq[hydra.python.syntax.CaseBlock]) =>
                              hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.python.syntax.CaseBlock],
                                 hydra.python.syntax.Statement](hydra.python.coder.encodeDefaultCaseBlock((t: hydra.core.Term) => hydra.python.coder.encodeTermInline(cx)(env)(false)(t))(isFull)(dflt)(tname))((pyDflt: Seq[hydra.python.syntax.CaseBlock]) =>
                              {
                              lazy val subj: hydra.python.syntax.SubjectExpression = hydra.python.syntax.SubjectExpression.simple(hydra.python.syntax.NamedExpression.simple(hydra.python.utils.pyNameToPyExpression("x")))
                              {
                                lazy val allCases: Seq[hydra.python.syntax.CaseBlock] = hydra.lib.lists.concat2[hydra.python.syntax.CaseBlock](pyCases)(pyDflt)
                                {
                                  lazy val matchStmt: hydra.python.syntax.Statement = hydra.python.syntax.Statement.compound(hydra.python.syntax.CompoundStatement.`match`(hydra.python.syntax.MatchStatement(subj,
                                     allCases)))
                                  {
                                    lazy val body: hydra.python.syntax.Block = hydra.python.utils.indentedBlock(None)(Seq(Seq(matchStmt)))
                                    {
                                      lazy val funcDefRaw: hydra.python.syntax.FunctionDefRaw = hydra.python.syntax.FunctionDefRaw(false,
                                         fname, Seq(), Some(params), None, None, body)
                                      Right(hydra.python.syntax.Statement.compound(hydra.python.syntax.CompoundStatement.function(hydra.python.syntax.FunctionDefinition(None,
                                         funcDefRaw))))
                                    }
                                  }
                                }
                              }
                            }))
                          }
                        }
                      }
                    }
                  })
                }
              }
            })(mcs)
          })({
            lazy val tname: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name,
               Tuple2[Option[hydra.core.Term], Tuple2[Seq[hydra.core.Field], hydra.core.Term]]](csa)
            {
              lazy val rest1: Tuple2[Option[hydra.core.Term], Tuple2[Seq[hydra.core.Field],
                 hydra.core.Term]] = hydra.lib.pairs.second[hydra.core.Name, Tuple2[Option[hydra.core.Term],
                 Tuple2[Seq[hydra.core.Field], hydra.core.Term]]](csa)
              {
                lazy val dflt: Option[hydra.core.Term] = hydra.lib.pairs.first[Option[hydra.core.Term],
                   Tuple2[Seq[hydra.core.Field], hydra.core.Term]](rest1)
                {
                  lazy val rest2: Tuple2[Seq[hydra.core.Field], hydra.core.Term] = hydra.lib.pairs.second[Option[hydra.core.Term],
                     Tuple2[Seq[hydra.core.Field], hydra.core.Term]](rest1)
                  {
                    lazy val `cases_`: Seq[hydra.core.Field] = hydra.lib.pairs.first[Seq[hydra.core.Field],
                       hydra.core.Term](rest2)
                    hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.FieldType],
                       hydra.python.syntax.Statement](hydra.resolution.requireUnionType(cx)(hydra.python.coder.pythonEnvironmentGetGraph(env))(tname))((rt: Seq[hydra.core.FieldType]) =>
                      {
                      lazy val isEnum: Boolean = hydra.predicates.isEnumRowType(rt)
                      {
                        lazy val isFull: Boolean = hydra.python.coder.isCasesFull(rt)(`cases_`)
                        {
                          lazy val capturedVarNames: Seq[hydra.core.Name] = hydra.lib.lists.init[hydra.core.Name](lambdaParams)
                          {
                            lazy val matchLambdaParam: hydra.core.Name = hydra.lib.lists.last[hydra.core.Name](lambdaParams)
                            {
                              lazy val capturedParams: Seq[hydra.python.syntax.ParamNoDefault] = hydra.lib.lists.map[hydra.core.Name,
                                 hydra.python.syntax.ParamNoDefault]((n: hydra.core.Name) =>
                                hydra.python.syntax.ParamNoDefault(hydra.python.syntax.Param(hydra.python.names.encodeName(false)(hydra.util.CaseConvention.lowerSnake)(env)(n),
                                   None), None))(capturedVarNames)
                              {
                                lazy val matchArgName: hydra.python.syntax.Name = hydra.python.names.encodeName(false)(hydra.util.CaseConvention.lowerSnake)(env)(matchLambdaParam)
                                {
                                  lazy val matchParam: hydra.python.syntax.ParamNoDefault = hydra.python.syntax.ParamNoDefault(hydra.python.syntax.Param(matchArgName,
                                     None), None)
                                  {
                                    lazy val allParams: Seq[hydra.python.syntax.ParamNoDefault] = hydra.lib.lists.concat2[hydra.python.syntax.ParamNoDefault](capturedParams)(Seq(matchParam))
                                    {
                                      lazy val params: hydra.python.syntax.Parameters = hydra.python.syntax.Parameters.paramNoDefault(hydra.python.syntax.ParamNoDefaultParameters(allParams,
                                         Seq(), None))
                                      {
                                        lazy val envWithParams: hydra.python.environment.PythonEnvironment = hydra.python.coder.extendEnvWithLambdaParams(env)(term1)
                                        hydra.lib.eithers.bind[hydra.errors.Error,
                                           Seq[hydra.python.syntax.CaseBlock], hydra.python.syntax.Statement](hydra.lib.eithers.mapList[hydra.core.Field,
                                           hydra.python.syntax.CaseBlock, hydra.errors.Error]((v1: hydra.core.Field) =>
                                          hydra.python.coder.encodeCaseBlock(cx)(envWithParams)(tname)(rt)(isEnum)((e: hydra.python.environment.PythonEnvironment) =>
                                          (t: hydra.core.Term) => hydra.python.coder.encodeTermMultiline(cx)(e)(t))(v1))(`cases_`))((pyCases: Seq[hydra.python.syntax.CaseBlock]) =>
                                          hydra.lib.eithers.bind[hydra.errors.Error,
                                             Seq[hydra.python.syntax.CaseBlock], hydra.python.syntax.Statement](hydra.python.coder.encodeDefaultCaseBlock((t: hydra.core.Term) =>
                                          hydra.python.coder.encodeTermInline(cx)(envWithParams)(false)(t))(isFull)(dflt)(tname))((pyDflt: Seq[hydra.python.syntax.CaseBlock]) =>
                                          {
                                          lazy val subj: hydra.python.syntax.SubjectExpression = hydra.python.syntax.SubjectExpression.simple(hydra.python.syntax.NamedExpression.simple(hydra.python.utils.pyNameToPyExpression(matchArgName)))
                                          {
                                            lazy val allCases: Seq[hydra.python.syntax.CaseBlock] = hydra.lib.lists.concat2[hydra.python.syntax.CaseBlock](pyCases)(pyDflt)
                                            {
                                              lazy val matchStmt: hydra.python.syntax.Statement = hydra.python.syntax.Statement.compound(hydra.python.syntax.CompoundStatement.`match`(hydra.python.syntax.MatchStatement(subj,
                                                 allCases)))
                                              {
                                                lazy val body: hydra.python.syntax.Block = hydra.python.utils.indentedBlock(None)(Seq(Seq(matchStmt)))
                                                {
                                                  lazy val funcDefRaw: hydra.python.syntax.FunctionDefRaw = hydra.python.syntax.FunctionDefRaw(false,
                                                     fname, Seq(), Some(params), None,
                                                     None, body)
                                                  Right(hydra.python.syntax.Statement.compound(hydra.python.syntax.CompoundStatement.function(hydra.python.syntax.FunctionDefinition(None,
                                                     funcDefRaw))))
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
                        }
                      }
                    })
                  }
                }
              }
            }
          }))(mcsa)
        }
      }
    }
  })((ts: hydra.core.TypeScheme) =>
    hydra.lib.eithers.bind[hydra.errors.Error, Option[scala.Predef.String], hydra.python.syntax.Statement](hydra.annotations.getTermDescription(cx)(hydra.python.coder.pythonEnvironmentGetGraph(env))(term1))((comment: Option[scala.Predef.String]) =>
    {
    lazy val normComment: Option[scala.Predef.String] = hydra.lib.maybes.map[scala.Predef.String,
       scala.Predef.String](hydra.formatting.normalizeComment)(comment)
    hydra.python.coder.encodeTermAssignment(cx)(env)(name1)(term1)(ts)(normComment)
  }))(mts)
}

def encodeBindingAsAssignment(cx: hydra.context.Context)(allowThunking: Boolean)(env: hydra.python.environment.PythonEnvironment)(binding: hydra.core.Binding): Either[hydra.errors.Error,
   hydra.python.syntax.NamedExpression] =
  {
  lazy val name: hydra.core.Name = (binding.name)
  lazy val term: hydra.core.Term = (binding.term)
  lazy val mts: Option[hydra.core.TypeScheme] = (binding.`type`)
  lazy val pyName: hydra.python.syntax.Name = hydra.python.names.encodeName(false)(hydra.util.CaseConvention.lowerSnake)(env)(name)
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Expression, hydra.python.syntax.NamedExpression](hydra.python.coder.encodeTermInline(cx)(env)(false)(term))((pbody: hydra.python.syntax.Expression) =>
    {
    lazy val tc: hydra.graph.Graph = (env.graph)
    {
      lazy val isComplexVar: Boolean = hydra.predicates.isComplexVariable(tc)(name)
      {
        lazy val termIsComplex: Boolean = hydra.predicates.isComplexTerm(tc)(term)
        {
          lazy val isTrivial: Boolean = hydra.predicates.isTrivialTerm(term)
          {
            lazy val needsThunk: Boolean = hydra.lib.logic.ifElse[Boolean](isTrivial)(false)(hydra.lib.maybes.maybe[Boolean,
               hydra.core.TypeScheme](hydra.lib.logic.and(allowThunking)(hydra.lib.logic.or(isComplexVar)(termIsComplex)))((ts: hydra.core.TypeScheme) =>
              hydra.lib.logic.and(allowThunking)(hydra.lib.logic.and(hydra.lib.equality.equal[Int](hydra.arity.typeSchemeArity(ts))(0))(hydra.lib.logic.or(isComplexVar)(termIsComplex))))(mts))
            {
              lazy val pterm: hydra.python.syntax.Expression = hydra.lib.logic.ifElse[hydra.python.syntax.Expression](needsThunk)(hydra.python.coder.makeThunk(pbody))(pbody)
              Right(hydra.python.syntax.NamedExpression.assignment(hydra.python.syntax.AssignmentExpression(pyName,
                 pterm)))
            }
          }
        }
      }
    }
  })
}

def encodeBindingsAsDefs[T0, T1, T2, T3](env: T0)(encodeBinding: (T0 => T1 => Either[T2,
   T3]))(bindings: Seq[T1]): Either[T2, Seq[T3]] =
  hydra.lib.eithers.mapList[T1, T3, T2]((v1: T1) => encodeBinding(env)(v1))(bindings)

def encodeCaseBlock[T0, T1](cx: T0)(env: hydra.python.environment.PythonEnvironment)(tname: hydra.core.Name)(rowType: Seq[hydra.core.FieldType])(isEnum: Boolean)(encodeBody: (hydra.python.environment.PythonEnvironment => hydra.core.Term => Either[T1,
   Seq[hydra.python.syntax.Statement]]))(field: hydra.core.Field): Either[T1, hydra.python.syntax.CaseBlock] =
  {
  lazy val fname: hydra.core.Name = (field.name)
  lazy val fterm: hydra.core.Term = (field.term)
  lazy val stripped: hydra.core.Term = hydra.strip.deannotateAndDetypeTerm(fterm)
  lazy val effectiveLambda: hydra.core.Lambda = stripped match
    case hydra.core.Term.lambda(v_Term_lambda_lam) => v_Term_lambda_lam
    case _ => {
      lazy val syntheticVar: hydra.core.Name = "_matchValue"
      hydra.core.Lambda(syntheticVar, None, hydra.core.Term.application(hydra.core.Application(stripped,
         hydra.core.Term.variable(syntheticVar))))
    }
  lazy val v: hydra.core.Name = (effectiveLambda.parameter)
  lazy val rawBody: hydra.core.Term = (effectiveLambda.body)
  lazy val isUnitVariant: Boolean = hydra.python.coder.isVariantUnitType(rowType)(fname)
  lazy val effectiveBody: hydra.core.Term = hydra.lib.logic.ifElse[hydra.core.Term](isUnitVariant)(hydra.python.coder.eliminateUnitVar(v)(rawBody))(rawBody)
  lazy val shouldCapture: Boolean = hydra.lib.logic.not(hydra.lib.logic.or(isUnitVariant)(hydra.lib.logic.or(hydra.variables.isFreeVariableInTerm(v)(rawBody))(hydra.predicates.isUnitTerm(rawBody))))
  lazy val env2: hydra.python.environment.PythonEnvironment = hydra.python.coder.pythonEnvironmentSetGraph(hydra.scoping.extendGraphForLambda(hydra.python.coder.pythonEnvironmentGetGraph(env))(effectiveLambda))(env)
  lazy val pyVariantName: hydra.python.syntax.Name = hydra.python.coder.deconflictVariantName(true)(env2)(tname)(fname)(env2.graph)
  lazy val pattern: hydra.python.syntax.ClosedPattern = hydra.python.coder.variantClosedPattern(env2)(tname)(fname)(pyVariantName)(rowType)(isEnum)(v)(shouldCapture)
  hydra.lib.eithers.bind[T1, Seq[hydra.python.syntax.Statement], hydra.python.syntax.CaseBlock](encodeBody(env2)(effectiveBody))((stmts: Seq[hydra.python.syntax.Statement]) =>
    {
    lazy val pyBody: hydra.python.syntax.Block = hydra.python.utils.indentedBlock(None)(Seq(stmts))
    Right(hydra.python.syntax.CaseBlock(hydra.python.utils.pyClosedPatternToPyPatterns(pattern), None, pyBody))
  })
}

def encodeDefaultCaseBlock[T0, T1](encodeTerm: (T0 => Either[T1, hydra.python.syntax.Expression]))(isFull: Boolean)(mdflt: Option[T0])(tname: hydra.core.Name): Either[T1,
   Seq[hydra.python.syntax.CaseBlock]] =
  hydra.lib.eithers.bind[T1, hydra.python.syntax.Statement, Seq[hydra.python.syntax.CaseBlock]](hydra.lib.maybes.maybe[Either[T1,
     hydra.python.syntax.Statement], T0](Right(hydra.lib.logic.ifElse[hydra.python.syntax.Statement](isFull)(hydra.python.utils.raiseAssertionError("Unreachable: all variants handled"))(hydra.python.utils.raiseTypeError(hydra.lib.strings.cat2("Unsupported ")(hydra.names.localNameOf(tname))))))((d: T0) =>
  hydra.lib.eithers.bind[T1, hydra.python.syntax.Expression, hydra.python.syntax.Statement](encodeTerm(d))((pyexpr: hydra.python.syntax.Expression) => Right(hydra.python.utils.returnSingle(pyexpr))))(mdflt))((stmt: hydra.python.syntax.Statement) =>
  {
  lazy val patterns: hydra.python.syntax.Patterns = hydra.python.utils.pyClosedPatternToPyPatterns(hydra.python.syntax.ClosedPattern.wildcard)
  {
    lazy val body: hydra.python.syntax.Block = hydra.python.utils.indentedBlock(None)(Seq(Seq(stmt)))
    Right(Seq(hydra.python.syntax.CaseBlock(patterns, None, body)))
  }
})

def encodeDefinition(cx: hydra.context.Context)(env: hydra.python.environment.PythonEnvironment)(`def_`: hydra.packaging.Definition): Either[hydra.errors.Error,
   Seq[Seq[hydra.python.syntax.Statement]]] =
  `def_` match
  case hydra.packaging.Definition.term(v_Definition_term_td) => {
    lazy val name: hydra.core.Name = (v_Definition_term_td.name)
    {
      lazy val term: hydra.core.Term = (v_Definition_term_td.term)
      {
        lazy val typ: hydra.core.TypeScheme = hydra.lib.maybes.maybe[hydra.core.TypeScheme,
           hydra.core.TypeScheme](hydra.core.TypeScheme(Seq(), hydra.core.Type.variable("hydra.core.Unit"),
           None))((x: hydra.core.TypeScheme) => x)(v_Definition_term_td.`type`)
        hydra.lib.eithers.bind[hydra.errors.Error, Option[scala.Predef.String], Seq[Seq[hydra.python.syntax.Statement]]](hydra.annotations.getTermDescription(cx)(hydra.python.coder.pythonEnvironmentGetGraph(env))(term))((comment: Option[scala.Predef.String]) =>
          {
          lazy val normComment: Option[scala.Predef.String] = hydra.lib.maybes.map[scala.Predef.String,
             scala.Predef.String](hydra.formatting.normalizeComment)(comment)
          hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Statement,
             Seq[Seq[hydra.python.syntax.Statement]]](hydra.python.coder.encodeTermAssignment(cx)(env)(name)(term)(typ)(normComment))((stmt: hydra.python.syntax.Statement) => Right(Seq(Seq(stmt))))
        })
      }
    }
  }
  case hydra.packaging.Definition.`type`(v_Definition_type_td) => {
    lazy val name: hydra.core.Name = (v_Definition_type_td.name)
    {
      lazy val typ: hydra.core.Type = (v_Definition_type_td.`type`.`type`)
      hydra.lib.eithers.bind[hydra.errors.Error, Option[scala.Predef.String], Seq[Seq[hydra.python.syntax.Statement]]](hydra.annotations.getTypeDescription(cx)(hydra.python.coder.pythonEnvironmentGetGraph(env))(typ))((comment: Option[scala.Predef.String]) =>
        {
        lazy val normComment: Option[scala.Predef.String] = hydra.lib.maybes.map[scala.Predef.String,
           scala.Predef.String](hydra.formatting.normalizeComment)(comment)
        hydra.python.coder.encodeTypeAssignment(cx)(env)(name)(typ)(normComment)
      })
    }
  }

def encodeEnumValueAssignment[T0](cx: T0)(env: hydra.python.environment.PythonEnvironment)(fieldType: hydra.core.FieldType): Either[hydra.errors.Error,
   Seq[hydra.python.syntax.Statement]] =
  {
  lazy val fname: hydra.core.Name = (fieldType.name)
  lazy val ftype: hydra.core.Type = (fieldType.`type`)
  hydra.lib.eithers.bind[hydra.errors.Error, Option[scala.Predef.String], Seq[hydra.python.syntax.Statement]](hydra.annotations.getTypeDescription(cx)(hydra.python.coder.pythonEnvironmentGetGraph(env))(ftype))((mcomment: Option[scala.Predef.String]) =>
    {
    lazy val pyName: hydra.python.syntax.Name = hydra.python.names.encodeEnumValue(env)(fname)
    {
      lazy val fnameStr: scala.Predef.String = fname
      {
        lazy val pyValue: hydra.python.syntax.Expression = hydra.python.utils.functionCall(hydra.python.utils.pyNameToPyPrimary(hydra.python.names.encodeName(true)(hydra.util.CaseConvention.pascal)(env)("hydra.core.Name")))(Seq(hydra.python.utils.doubleQuotedString(fnameStr)))
        {
          lazy val assignStmt: hydra.python.syntax.Statement = hydra.python.utils.assignmentStatement(pyName)(pyValue)
          Right(hydra.lib.maybes.maybe[Seq[hydra.python.syntax.Statement], scala.Predef.String](Seq(assignStmt))((c: scala.Predef.String) =>
            Seq(assignStmt, hydra.python.utils.pyExpressionToPyStatement(hydra.python.utils.tripleQuotedString(c))))(mcomment))
        }
      }
    }
  })
}

def encodeField[T0, T1, T2](cx: T0)(env: hydra.python.environment.PythonEnvironment)(field: hydra.core.Field)(encodeTerm: (hydra.core.Term => Either[T1,
   T2])): Either[T1, Tuple2[hydra.python.syntax.Name, T2]] =
  {
  lazy val fname: hydra.core.Name = (field.name)
  lazy val fterm: hydra.core.Term = (field.term)
  hydra.lib.eithers.bind[T1, T2, Tuple2[hydra.python.syntax.Name, T2]](encodeTerm(fterm))((pterm: T2) =>
    Right(Tuple2(hydra.python.names.encodeFieldName(env)(fname), pterm)))
}

def encodeFieldType[T0](cx: T0)(env: hydra.python.environment.PythonEnvironment)(fieldType: hydra.core.FieldType): Either[hydra.errors.Error,
   hydra.python.syntax.Statement] =
  {
  lazy val fname: hydra.core.Name = (fieldType.name)
  lazy val ftype: hydra.core.Type = (fieldType.`type`)
  hydra.lib.eithers.bind[hydra.errors.Error, Option[scala.Predef.String], hydra.python.syntax.Statement](hydra.annotations.getTypeDescription(cx)(hydra.python.coder.pythonEnvironmentGetGraph(env))(ftype))((comment: Option[scala.Predef.String]) =>
    {
    lazy val pyName: hydra.python.syntax.SingleTarget = hydra.python.syntax.SingleTarget.name(hydra.python.names.encodeFieldName(env)(fname))
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Expression, hydra.python.syntax.Statement](hydra.python.coder.encodeType(env)(ftype))((pyType: hydra.python.syntax.Expression) =>
      {
      lazy val annotatedPyType: hydra.python.syntax.Expression = hydra.python.utils.annotatedExpression(comment)(pyType)
      Right(hydra.python.utils.pyAssignmentToPyStatement(hydra.python.syntax.Assignment.typed(hydra.python.syntax.TypedAssignment(pyName,
         annotatedPyType, None))))
    })
  })
}

def encodeFloatValue[T0](fv: hydra.core.FloatValue): Either[T0, hydra.python.syntax.Expression] =
  fv match
  case hydra.core.FloatValue.bigfloat(v_FloatValue_bigfloat_f) => Right(hydra.python.utils.functionCall(hydra.python.utils.pyNameToPyPrimary("Decimal"))(Seq(hydra.python.utils.singleQuotedString(hydra.lib.literals.showBigfloat(v_FloatValue_bigfloat_f)))))
  case hydra.core.FloatValue.float32(v_FloatValue_float32_f) => hydra.python.coder.encodeFloatValue_encodeFloat32(v_FloatValue_float32_f)
  case hydra.core.FloatValue.float64(v_FloatValue_float64_f) => hydra.python.coder.encodeFloatValue_encodeFloat64(v_FloatValue_float64_f)

def encodeFloatValue_encodeFloat32[T0](v: Float): Either[T0, hydra.python.syntax.Expression] =
  {
  lazy val s: scala.Predef.String = hydra.lib.literals.showFloat32(v)
  hydra.lib.logic.ifElse[Either[T0, hydra.python.syntax.Expression]](hydra.lib.equality.equal[scala.Predef.String](s)("NaN"))(Right(hydra.python.coder.encodeFloatValue_pySpecialFloat("nan")))(hydra.lib.logic.ifElse[Either[T0,
     hydra.python.syntax.Expression]](hydra.lib.equality.equal[scala.Predef.String](s)("Infinity"))(Right(hydra.python.coder.encodeFloatValue_pySpecialFloat("inf")))(hydra.lib.logic.ifElse[Either[T0,
     hydra.python.syntax.Expression]](hydra.lib.equality.equal[scala.Predef.String](s)("-Infinity"))(Right(hydra.python.coder.encodeFloatValue_pySpecialFloat("-inf")))(Right(hydra.python.utils.pyAtomToPyExpression(hydra.python.syntax.Atom.number(hydra.python.syntax.Number.float(hydra.lib.literals.float32ToBigfloat(v))))))))
}

def encodeFloatValue_encodeFloat64[T0](v: Double): Either[T0, hydra.python.syntax.Expression] =
  {
  lazy val s: scala.Predef.String = hydra.lib.literals.showFloat64(v)
  hydra.lib.logic.ifElse[Either[T0, hydra.python.syntax.Expression]](hydra.lib.equality.equal[scala.Predef.String](s)("NaN"))(Right(hydra.python.coder.encodeFloatValue_pySpecialFloat("nan")))(hydra.lib.logic.ifElse[Either[T0,
     hydra.python.syntax.Expression]](hydra.lib.equality.equal[scala.Predef.String](s)("Infinity"))(Right(hydra.python.coder.encodeFloatValue_pySpecialFloat("inf")))(hydra.lib.logic.ifElse[Either[T0,
     hydra.python.syntax.Expression]](hydra.lib.equality.equal[scala.Predef.String](s)("-Infinity"))(Right(hydra.python.coder.encodeFloatValue_pySpecialFloat("-inf")))(hydra.lib.logic.ifElse[Either[T0,
     hydra.python.syntax.Expression]](hydra.lib.equality.equal[scala.Predef.String](s)("-0.0"))(Right(hydra.python.coder.encodeFloatValue_pySpecialFloat("-0.0")))(Right(hydra.python.utils.pyAtomToPyExpression(hydra.python.syntax.Atom.number(hydra.python.syntax.Number.float(hydra.lib.literals.float64ToBigfloat(v)))))))))
}

def encodeFloatValue_pySpecialFloat(value: scala.Predef.String): hydra.python.syntax.Expression =
  hydra.python.utils.functionCall(hydra.python.utils.pyNameToPyPrimary("float"))(Seq(hydra.python.utils.singleQuotedString(value)))

def encodeForallType[T0](env: hydra.python.environment.PythonEnvironment)(lt: hydra.core.ForallType): Either[T0,
   hydra.python.syntax.Expression] =
  {
  def gatherParams(t: hydra.core.Type)(ps: Seq[hydra.core.Name]): Tuple2[hydra.core.Type, Seq[hydra.core.Name]] =
    hydra.strip.deannotateType(t) match
    case hydra.core.Type.forall(v_Type_forall_forallT) => gatherParams(v_Type_forall_forallT.body)(hydra.lib.lists.cons[hydra.core.Name](v_Type_forall_forallT.parameter)(ps))
    case hydra.core.Type.annotated(v_Type_annotated__) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.application(v_Type_application__) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.function(v_Type_function__) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.list(v_Type_list__) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.literal(v_Type_literal__) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.map(v_Type_map__) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.maybe(v_Type_maybe__) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.either(v_Type_either__) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.pair(v_Type_pair__) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.record(v_Type_record__) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.set(v_Type_set__) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.union(v_Type_union__) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.unit => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.variable(v_Type_variable__) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.void => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.wrap(v_Type_wrap__) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
  lazy val bodyAndParams: Tuple2[hydra.core.Type, Seq[hydra.core.Name]] = gatherParams(hydra.core.Type.forall(lt))(Seq())
  lazy val body: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, Seq[hydra.core.Name]](bodyAndParams)
  lazy val params: Seq[hydra.core.Name] = hydra.lib.pairs.second[hydra.core.Type, Seq[hydra.core.Name]](bodyAndParams)
  hydra.lib.eithers.bind[T0, hydra.python.syntax.Expression, hydra.python.syntax.Expression](hydra.python.coder.encodeType(env)(body))((pyBody: hydra.python.syntax.Expression) =>
    Right(hydra.python.utils.primaryAndParams(hydra.python.utils.pyExpressionToPyPrimary(pyBody))(hydra.lib.lists.map[hydra.core.Name,
       hydra.python.syntax.Expression]((n: hydra.core.Name) =>
    hydra.python.syntax.Expression.simple(Seq(Seq(hydra.python.syntax.Inversion.simple(hydra.python.syntax.Comparison(hydra.python.syntax.BitwiseOr(None,
       hydra.python.syntax.BitwiseXor(None, hydra.python.syntax.BitwiseAnd(None, hydra.python.syntax.ShiftExpression(None,
       hydra.python.syntax.Sum(None, hydra.python.syntax.Term(None, hydra.python.syntax.Factor.simple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(false,
       hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name(n))), None)))))))),
       Seq()))))))(params))))
}

def encodeFunctionDefinition(cx: hydra.context.Context)(env: hydra.python.environment.PythonEnvironment)(name: hydra.core.Name)(tparams: Seq[hydra.core.Name])(args: Seq[hydra.core.Name])(body: hydra.core.Term)(doms: Seq[hydra.core.Type])(mcod: Option[hydra.core.Type])(comment: Option[scala.Predef.String])(prefixes: Seq[hydra.python.syntax.Statement]): Either[hydra.errors.Error,
   hydra.python.syntax.Statement] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.python.syntax.ParamNoDefault],
     hydra.python.syntax.Statement](hydra.lib.eithers.mapList[Tuple2[hydra.core.Name,
     hydra.core.Type], hydra.python.syntax.ParamNoDefault, hydra.errors.Error]((pair: Tuple2[hydra.core.Name,
     hydra.core.Type]) =>
  {
  lazy val argName: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, hydra.core.Type](pair)
  {
    lazy val typ: hydra.core.Type = hydra.lib.pairs.second[hydra.core.Name, hydra.core.Type](pair)
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Expression, hydra.python.syntax.ParamNoDefault](hydra.python.coder.encodeType(env)(typ))((pyTyp: hydra.python.syntax.Expression) =>
      Right(hydra.python.syntax.ParamNoDefault(hydra.python.syntax.Param(hydra.python.names.encodeName(false)(hydra.util.CaseConvention.lowerSnake)(env)(argName),
         Some(pyTyp)), None)))
  }
})(hydra.lib.lists.zip[hydra.core.Name, hydra.core.Type](args)(doms)))((pyArgs: Seq[hydra.python.syntax.ParamNoDefault]) =>
  {
  lazy val pyParams: hydra.python.syntax.Parameters = hydra.python.syntax.Parameters.paramNoDefault(hydra.python.syntax.ParamNoDefaultParameters(pyArgs,
     Seq(), None))
  {
    lazy val isTCO: Boolean = hydra.lib.logic.and(hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.core.Name](args)))(hydra.analysis.isSelfTailRecursive(name)(body))
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Block, hydra.python.syntax.Statement](hydra.lib.logic.ifElse[Either[hydra.errors.Error,
       hydra.python.syntax.Block]](isTCO)(hydra.lib.eithers.bind[hydra.errors.Error,
       Seq[hydra.python.syntax.Statement], hydra.python.syntax.Block](hydra.python.coder.encodeTermMultilineTCO(cx)(env)(name)(args)(body))((tcoStmts: Seq[hydra.python.syntax.Statement]) =>
      {
      lazy val trueExpr: hydra.python.syntax.NamedExpression = hydra.python.syntax.NamedExpression.simple(hydra.python.utils.pyAtomToPyExpression(hydra.python.syntax.Atom.`true`))
      {
        lazy val whileBody: hydra.python.syntax.Block = hydra.python.utils.indentedBlock(None)(Seq(hydra.lib.lists.concat2[hydra.python.syntax.Statement](prefixes)(tcoStmts)))
        {
          lazy val whileStmt: hydra.python.syntax.Statement = hydra.python.syntax.Statement.compound(hydra.python.syntax.CompoundStatement.`while`(hydra.python.syntax.WhileStatement(trueExpr,
             whileBody, None)))
          Right(hydra.python.utils.indentedBlock(comment)(Seq(Seq(whileStmt))))
        }
      }
    }))(hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.python.syntax.Statement],
       hydra.python.syntax.Block](hydra.python.coder.encodeTermMultiline(cx)(env)(body))((stmts: Seq[hydra.python.syntax.Statement]) =>
      Right(hydra.python.utils.indentedBlock(comment)(Seq(hydra.lib.lists.concat2[hydra.python.syntax.Statement](prefixes)(stmts)))))))((block: hydra.python.syntax.Block) =>
      hydra.lib.eithers.bind[hydra.errors.Error, Option[hydra.python.syntax.Expression],
         hydra.python.syntax.Statement](hydra.lib.maybes.maybe[Either[hydra.errors.Error,
         Option[hydra.python.syntax.Expression]], hydra.core.Type](Right(None))((cod: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Expression, Option[hydra.python.syntax.Expression]](hydra.python.coder.encodeType(env)(cod))((pytyp: hydra.python.syntax.Expression) => Right(Some(pytyp))))(mcod))((mreturnType: Option[hydra.python.syntax.Expression]) =>
      {
      lazy val pyTparams: Seq[hydra.python.syntax.TypeParameter] = hydra.lib.logic.ifElse[Seq[hydra.python.syntax.TypeParameter]](hydra.python.coder.useInlineTypeParams)(hydra.lib.lists.map[hydra.core.Name,
         hydra.python.syntax.TypeParameter]((`arg_`: hydra.core.Name) =>
        hydra.python.utils.pyNameToPyTypeParameter(hydra.python.names.encodeTypeVariable(`arg_`)))(tparams))(Seq())
      {
        lazy val isThunk: Boolean = hydra.lib.lists.`null`[hydra.core.Name](args)
        {
          lazy val mDecorators: Option[hydra.python.syntax.Decorators] = hydra.lib.logic.ifElse[Option[hydra.python.syntax.Decorators]](isThunk)(Some(Seq(hydra.python.coder.lruCacheDecorator)))(None)
          {
            lazy val pyName: hydra.python.syntax.Name = hydra.python.names.encodeName(false)(hydra.util.CaseConvention.lowerSnake)(env)(name)
            Right(hydra.python.syntax.Statement.compound(hydra.python.syntax.CompoundStatement.function(hydra.python.syntax.FunctionDefinition(mDecorators,
               hydra.python.syntax.FunctionDefRaw(false, pyName, pyTparams, Some(pyParams),
               mreturnType, None, block)))))
          }
        }
      }
    }))
  }
})

def encodeFunctionType[T0](env: hydra.python.environment.PythonEnvironment)(ft: hydra.core.FunctionType): Either[T0,
   hydra.python.syntax.Expression] =
  {
  def gatherParams(rdoms: Seq[hydra.core.Type])(ftype: hydra.core.FunctionType): Tuple2[Seq[hydra.core.Type],
     hydra.core.Type] =
    {
    lazy val innerCod: hydra.core.Type = (ftype.codomain)
    lazy val dom: hydra.core.Type = (ftype.domain)
    hydra.strip.deannotateType(innerCod) match
      case hydra.core.Type.function(v_Type_function_ft2) => gatherParams(hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms))(v_Type_function_ft2)
      case hydra.core.Type.annotated(v_Type_annotated__) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)),
         innerCod)
      case hydra.core.Type.application(v_Type_application__) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)),
         innerCod)
      case hydra.core.Type.forall(v_Type_forall__) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)),
         innerCod)
      case hydra.core.Type.list(v_Type_list__) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)),
         innerCod)
      case hydra.core.Type.literal(v_Type_literal__) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)),
         innerCod)
      case hydra.core.Type.map(v_Type_map__) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)),
         innerCod)
      case hydra.core.Type.maybe(v_Type_maybe__) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)),
         innerCod)
      case hydra.core.Type.either(v_Type_either__) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)),
         innerCod)
      case hydra.core.Type.pair(v_Type_pair__) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)),
         innerCod)
      case hydra.core.Type.record(v_Type_record__) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)),
         innerCod)
      case hydra.core.Type.set(v_Type_set__) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)),
         innerCod)
      case hydra.core.Type.union(v_Type_union__) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)),
         innerCod)
      case hydra.core.Type.unit => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)),
         innerCod)
      case hydra.core.Type.variable(v_Type_variable__) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)),
         innerCod)
      case hydra.core.Type.void => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)),
         innerCod)
      case hydra.core.Type.wrap(v_Type_wrap__) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)),
         innerCod)
  }
  lazy val domsAndCod: Tuple2[Seq[hydra.core.Type], hydra.core.Type] = gatherParams(Seq())(ft)
  lazy val doms: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.core.Type](domsAndCod)
  lazy val cod: hydra.core.Type = hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.core.Type](domsAndCod)
  hydra.lib.eithers.bind[T0, Seq[hydra.python.syntax.Expression], hydra.python.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Type,
     hydra.python.syntax.Expression, T0]((v1: hydra.core.Type) => hydra.python.coder.encodeType(env)(v1))(doms))((pydoms: Seq[hydra.python.syntax.Expression]) =>
    hydra.lib.eithers.bind[T0, hydra.python.syntax.Expression, hydra.python.syntax.Expression](hydra.python.coder.encodeType(env)(cod))((pycod: hydra.python.syntax.Expression) =>
    Right(hydra.python.utils.pyPrimaryToPyExpression(hydra.python.utils.primaryWithSlices(hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name("Callable")))(hydra.python.utils.pyPrimaryToPySlice(hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.list(hydra.python.utils.pyList(pydoms)))))(Seq(hydra.python.syntax.SliceOrStarredExpression.slice(hydra.python.utils.pyExpressionToPySlice(pycod))))))))
}

def encodeIntegerValue[T0](iv: hydra.core.IntegerValue): Either[T0, hydra.python.syntax.Expression] =
  {
  def toPyInt[T1](n: BigInt): Either[T1, hydra.python.syntax.Expression] =
    Right(hydra.python.utils.pyAtomToPyExpression(hydra.python.syntax.Atom.number(hydra.python.syntax.Number.integer(n))))
  iv match
    case hydra.core.IntegerValue.bigint(v_IntegerValue_bigint_i) => toPyInt(v_IntegerValue_bigint_i)
    case hydra.core.IntegerValue.int8(v_IntegerValue_int8_i) => toPyInt(hydra.lib.literals.int8ToBigint(v_IntegerValue_int8_i))
    case hydra.core.IntegerValue.int16(v_IntegerValue_int16_i) => toPyInt(hydra.lib.literals.int16ToBigint(v_IntegerValue_int16_i))
    case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => toPyInt(hydra.lib.literals.int32ToBigint(v_IntegerValue_int32_i))
    case hydra.core.IntegerValue.int64(v_IntegerValue_int64_i) => toPyInt(hydra.lib.literals.int64ToBigint(v_IntegerValue_int64_i))
    case hydra.core.IntegerValue.uint8(v_IntegerValue_uint8_i) => toPyInt(hydra.lib.literals.uint8ToBigint(v_IntegerValue_uint8_i))
    case hydra.core.IntegerValue.uint16(v_IntegerValue_uint16_i) => toPyInt(hydra.lib.literals.uint16ToBigint(v_IntegerValue_uint16_i))
    case hydra.core.IntegerValue.uint32(v_IntegerValue_uint32_i) => toPyInt(hydra.lib.literals.uint32ToBigint(v_IntegerValue_uint32_i))
    case hydra.core.IntegerValue.uint64(v_IntegerValue_uint64_i) => toPyInt(hydra.lib.literals.uint64ToBigint(v_IntegerValue_uint64_i))
}

def encodeLiteral[T0](lit: hydra.core.Literal): Either[T0, hydra.python.syntax.Expression] =
  lit match
  case hydra.core.Literal.binary(v_Literal_binary_bs) => {
    lazy val byteValues: Seq[Int] = hydra.lib.literals.binaryToBytes(v_Literal_binary_bs)
    Right(hydra.python.utils.functionCall(hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name("bytes")))(Seq(hydra.python.utils.pyAtomToPyExpression(hydra.python.syntax.Atom.list(hydra.python.utils.pyList(hydra.lib.lists.map[Int,
       hydra.python.syntax.Expression]((byteVal: Int) =>
      hydra.python.utils.pyAtomToPyExpression(hydra.python.syntax.Atom.number(hydra.python.syntax.Number.integer(hydra.lib.literals.int32ToBigint(byteVal)))))(byteValues)))))))
  }
  case hydra.core.Literal.boolean(v_Literal_boolean_b) => Right(hydra.python.utils.pyAtomToPyExpression(hydra.lib.logic.ifElse[hydra.python.syntax.Atom](v_Literal_boolean_b)(hydra.python.syntax.Atom.`true`)(hydra.python.syntax.Atom.`false`)))
  case hydra.core.Literal.float(v_Literal_float_f) => hydra.python.coder.encodeFloatValue(v_Literal_float_f)
  case hydra.core.Literal.integer(v_Literal_integer_i) => hydra.python.coder.encodeIntegerValue(v_Literal_integer_i)
  case hydra.core.Literal.string(v_Literal_string_s) => Right(hydra.python.utils.stringToPyExpression(hydra.python.syntax.QuoteStyle.double)(v_Literal_string_s))

def encodeLiteralType[T0](lt: hydra.core.LiteralType): Either[T0, hydra.python.syntax.Expression] =
  {
  lazy val findName: scala.Predef.String = lt match
    case hydra.core.LiteralType.binary => "bytes"
    case hydra.core.LiteralType.boolean => "bool"
    case hydra.core.LiteralType.float(v_LiteralType_float_ft) => v_LiteralType_float_ft match
      case hydra.core.FloatType.bigfloat => "Decimal"
      case hydra.core.FloatType.float32 => "float"
      case hydra.core.FloatType.float64 => "float"
    case hydra.core.LiteralType.integer(v_LiteralType_integer__) => "int"
    case hydra.core.LiteralType.string => "str"
  Right(hydra.python.syntax.Expression.simple(Seq(Seq(hydra.python.syntax.Inversion.simple(hydra.python.syntax.Comparison(hydra.python.syntax.BitwiseOr(None,
     hydra.python.syntax.BitwiseXor(None, hydra.python.syntax.BitwiseAnd(None, hydra.python.syntax.ShiftExpression(None,
     hydra.python.syntax.Sum(None, hydra.python.syntax.Term(None, hydra.python.syntax.Factor.simple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(false,
     hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name(findName))),
     None)))))))), Seq()))))))
}

def encodeNameConstants(env: hydra.python.environment.PythonEnvironment)(name: hydra.core.Name)(fields: Seq[hydra.core.FieldType]): Seq[hydra.python.syntax.Statement] =
  {
  def toStmt(pair: Tuple2[hydra.python.syntax.Name, hydra.core.Name]): hydra.python.syntax.Statement =
    hydra.python.utils.assignmentStatement(hydra.lib.pairs.first[hydra.python.syntax.Name,
       hydra.core.Name](pair))(hydra.python.utils.functionCall(hydra.python.utils.pyNameToPyPrimary(hydra.python.names.encodeName(true)(hydra.util.CaseConvention.pascal)(env)("hydra.core.Name")))(Seq(hydra.python.utils.doubleQuotedString(hydra.lib.pairs.second[hydra.python.syntax.Name,
       hydra.core.Name](pair)))))
  lazy val namePair: Tuple2[hydra.python.syntax.Name, hydra.core.Name] = Tuple2(hydra.python.names.encodeConstantForTypeName(env)(name),
     name)
  lazy val fieldPairs: Seq[Tuple2[hydra.python.syntax.Name, hydra.core.Name]] = hydra.lib.lists.map[hydra.core.FieldType,
     Tuple2[hydra.python.syntax.Name, hydra.core.Name]]((field: hydra.core.FieldType) =>
    Tuple2(hydra.python.names.encodeConstantForFieldName(env)(name)(field.name), (field.name)))(fields)
  hydra.lib.lists.map[Tuple2[hydra.python.syntax.Name, hydra.core.Name], hydra.python.syntax.Statement](toStmt)(hydra.lib.lists.cons[Tuple2[hydra.python.syntax.Name,
     hydra.core.Name]](namePair)(fieldPairs))
}

def encodePythonModule(cx: hydra.context.Context)(g: hydra.graph.Graph)(mod: hydra.packaging.Module)(defs0: Seq[hydra.packaging.Definition]): Either[hydra.errors.Error,
   hydra.python.syntax.Module] =
  {
  lazy val defs: Seq[hydra.packaging.Definition] = hydra.environment.reorderDefs(defs0)
  lazy val meta0: hydra.python.environment.PythonModuleMetadata = hydra.python.coder.gatherMetadata(mod.namespace)(defs)
  lazy val namespaces0: hydra.packaging.Namespaces[hydra.python.syntax.DottedName] = (meta0.namespaces)
  lazy val env0: hydra.python.environment.PythonEnvironment = hydra.python.coder.initialEnvironment(namespaces0)(g)
  lazy val isTypeMod: Boolean = hydra.python.coder.isTypeModuleCheck(defs0)
  hydra.python.coder.withDefinitions(env0)(defs)((env: hydra.python.environment.PythonEnvironment) =>
    hydra.lib.eithers.bind[hydra.errors.Error, Seq[Seq[hydra.python.syntax.Statement]],
       hydra.python.syntax.Module](hydra.lib.eithers.map[Seq[Seq[Seq[hydra.python.syntax.Statement]]],
       Seq[Seq[hydra.python.syntax.Statement]], hydra.errors.Error]((xs: Seq[Seq[Seq[hydra.python.syntax.Statement]]]) =>
    hydra.lib.lists.concat[Seq[hydra.python.syntax.Statement]](xs))(hydra.lib.eithers.mapList[hydra.packaging.Definition,
       Seq[Seq[hydra.python.syntax.Statement]], hydra.errors.Error]((d: hydra.packaging.Definition) => hydra.python.coder.encodeDefinition(cx)(env)(d))(defs)))((defStmts: Seq[Seq[hydra.python.syntax.Statement]]) =>
    {
    lazy val meta2: hydra.python.environment.PythonModuleMetadata = hydra.lib.logic.ifElse[hydra.python.environment.PythonModuleMetadata](hydra.lib.logic.and(hydra.lib.logic.not(isTypeMod))(hydra.python.coder.useInlineTypeParams))(hydra.python.coder.setMetaUsesTypeVar(meta0)(false))(meta0)
    {
      lazy val meta: hydra.python.environment.PythonModuleMetadata = hydra.lib.logic.ifElse[hydra.python.environment.PythonModuleMetadata](hydra.lib.logic.and(isTypeMod)(hydra.lib.equality.equal[hydra.python.environment.PythonVersion](hydra.python.coder.targetPythonVersion)(hydra.python.environment.PythonVersion.python310)))(hydra.python.coder.setMetaUsesTypeAlias(meta2)(true))(meta2)
      {
        lazy val namespaces: hydra.packaging.Namespaces[hydra.python.syntax.DottedName] = (meta0.namespaces)
        {
          lazy val commentStmts: Seq[hydra.python.syntax.Statement] = hydra.lib.maybes.maybe[Seq[hydra.python.syntax.Statement],
             scala.Predef.String](Seq())((c: scala.Predef.String) => Seq(hydra.python.utils.commentStatement(c)))(hydra.lib.maybes.map[scala.Predef.String,
             scala.Predef.String](hydra.formatting.normalizeComment)(mod.description))
          {
            lazy val importStmts: Seq[hydra.python.syntax.Statement] = hydra.python.coder.moduleImports(namespaces)(meta)
            {
              lazy val tvars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.logic.ifElse[scala.collection.immutable.Set[hydra.core.Name]](hydra.lib.logic.or(isTypeMod)(hydra.lib.logic.not(hydra.python.coder.useInlineTypeParams)))(meta.typeVariables)(hydra.lib.sets.empty[hydra.core.Name])
              {
                lazy val tvarStmts: Seq[hydra.python.syntax.Statement] = hydra.lib.lists.map[hydra.core.Name,
                   hydra.python.syntax.Statement]((tv: hydra.core.Name) =>
                  hydra.python.coder.tvarStatement(hydra.python.names.encodeTypeVariable(tv)))(hydra.lib.sets.toList[hydra.core.Name](tvars))
                {
                  lazy val body: Seq[Seq[hydra.python.syntax.Statement]] = hydra.lib.lists.filter[Seq[hydra.python.syntax.Statement]]((group: Seq[hydra.python.syntax.Statement]) =>
                    hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.python.syntax.Statement](group)))(hydra.lib.lists.concat[Seq[hydra.python.syntax.Statement]](Seq(Seq(commentStmts,
                       importStmts, tvarStmts), defStmts)))
                  Right(body)
                }
              }
            }
          }
        }
      }
    }
  }))
}

def encodeRecordType[T0](cx: T0)(env: hydra.python.environment.PythonEnvironment)(name: hydra.core.Name)(rowType: Seq[hydra.core.FieldType])(comment: Option[scala.Predef.String]): Either[hydra.errors.Error,
   hydra.python.syntax.Statement] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.python.syntax.Statement], hydra.python.syntax.Statement](hydra.lib.eithers.mapList[hydra.core.FieldType,
     hydra.python.syntax.Statement, hydra.errors.Error]((v1: hydra.core.FieldType) => hydra.python.coder.encodeFieldType(cx)(env)(v1))(rowType))((pyFields: Seq[hydra.python.syntax.Statement]) =>
  {
  lazy val constStmts: Seq[hydra.python.syntax.Statement] = hydra.python.coder.encodeNameConstants(env)(name)(rowType)
  {
    lazy val body: hydra.python.syntax.Block = hydra.python.utils.indentedBlock(comment)(Seq(pyFields, constStmts))
    {
      lazy val boundVars: Tuple2[Seq[hydra.core.Name], Map[hydra.core.Name, hydra.python.syntax.Name]] = (env.boundTypeVariables)
      {
        lazy val tparamList: Seq[hydra.core.Name] = hydra.lib.pairs.first[Seq[hydra.core.Name],
           Map[hydra.core.Name, hydra.python.syntax.Name]](boundVars)
        {
          lazy val mGenericArg: Option[hydra.python.syntax.Expression] = hydra.python.coder.genericArg(tparamList)
          {
            lazy val args: Option[hydra.python.syntax.Args] = hydra.lib.maybes.maybe[Option[hydra.python.syntax.Args],
               hydra.python.syntax.Expression](None)((a: hydra.python.syntax.Expression) => Some(hydra.python.utils.pyExpressionsToPyArgs(Seq(a))))(mGenericArg)
            {
              lazy val decs: Option[hydra.python.syntax.Decorators] = Some(Seq(hydra.python.coder.dataclassDecorator))
              {
                lazy val pyName: hydra.python.syntax.Name = hydra.python.names.encodeName(false)(hydra.util.CaseConvention.pascal)(env)(name)
                {
                  def noTypeParams[T1]: Seq[T1] = Seq()
                  Right(hydra.python.utils.pyClassDefinitionToPyStatement(hydra.python.syntax.ClassDefinition(decs,
                     pyName, noTypeParams, args, body)))
                }
              }
            }
          }
        }
      }
    }
  }
})

def encodeTermAssignment(cx: hydra.context.Context)(env: hydra.python.environment.PythonEnvironment)(name: hydra.core.Name)(term: hydra.core.Term)(ts: hydra.core.TypeScheme)(comment: Option[scala.Predef.String]): Either[hydra.errors.Error,
   hydra.python.syntax.Statement] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.FunctionStructure[hydra.python.environment.PythonEnvironment],
     hydra.python.syntax.Statement](hydra.python.coder.analyzePythonFunction(cx)(env)(term))((fs: hydra.typing.FunctionStructure[hydra.python.environment.PythonEnvironment]) =>
  {
  lazy val tparams: Seq[hydra.core.Name] = (fs.typeParams)
  {
    lazy val params: Seq[hydra.core.Name] = (fs.params)
    {
      lazy val bindings: Seq[hydra.core.Binding] = (fs.bindings)
      {
        lazy val body: hydra.core.Term = (fs.body)
        {
          lazy val doms: Seq[hydra.core.Type] = (fs.domains)
          {
            lazy val mcod: Option[hydra.core.Type] = (fs.codomain)
            {
              lazy val env2: hydra.python.environment.PythonEnvironment = (fs.environment)
              {
                lazy val tc: hydra.graph.Graph = (env2.graph)
                {
                  lazy val binding: hydra.core.Binding = hydra.core.Binding(name, term, Some(ts))
                  {
                    lazy val isComplex: Boolean = hydra.predicates.isComplexBinding(tc)(binding)
                    {
                      lazy val isTrivial: Boolean = hydra.predicates.isTrivialTerm(term)
                      hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.python.syntax.Statement]](hydra.lib.logic.and(isComplex)(hydra.lib.logic.not(isTrivial)))(hydra.lib.eithers.bind[hydra.errors.Error,
                         Seq[hydra.python.syntax.Statement], hydra.python.syntax.Statement](hydra.lib.eithers.mapList[hydra.core.Binding,
                         hydra.python.syntax.Statement, hydra.errors.Error]((v1: hydra.core.Binding) => hydra.python.coder.encodeBindingAs(cx)(env2)(v1))(bindings))((bindingStmts: Seq[hydra.python.syntax.Statement]) =>
                        hydra.python.coder.encodeFunctionDefinition(cx)(env2)(name)(tparams)(params)(body)(doms)(mcod)(comment)(bindingStmts)))(hydra.lib.eithers.bind[hydra.errors.Error,
                           hydra.python.syntax.Expression, hydra.python.syntax.Statement](hydra.python.coder.encodeTermInline(cx)(env2)(false)(body))((bodyExpr: hydra.python.syntax.Expression) =>
                        {
                        lazy val pyName: hydra.python.syntax.Name = hydra.python.names.encodeName(false)(hydra.util.CaseConvention.lowerSnake)(env2)(name)
                        Right(hydra.python.utils.annotatedStatement(comment)(hydra.python.utils.assignmentStatement(pyName)(bodyExpr)))
                      }))
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

def encodeTermInline(cx: hydra.context.Context)(env: hydra.python.environment.PythonEnvironment)(noCast: Boolean)(term: hydra.core.Term): Either[hydra.errors.Error,
   hydra.python.syntax.Expression] =
  {
  def encode(t: hydra.core.Term): Either[hydra.errors.Error, hydra.python.syntax.Expression] = hydra.python.coder.encodeTermInline(cx)(env)(false)(t)
  def stripTypeApps(t: hydra.core.Term): hydra.core.Term =
    t match
    case hydra.core.Term.annotated(v_Term_annotated_ann) => stripTypeApps(v_Term_annotated_ann.body)
    case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => stripTypeApps(v_Term_typeApplication_ta.body)
    case _ => t
  def withCast[T1](pyexp: hydra.python.syntax.Expression): Either[T1, hydra.python.syntax.Expression] =
    hydra.lib.logic.ifElse[Either[T1, hydra.python.syntax.Expression]](hydra.lib.logic.or(noCast)(env.skipCasts))(Right(pyexp))({
    lazy val tc: hydra.graph.Graph = (env.graph)
    {
      lazy val mtyp: Either[hydra.errors.Error, hydra.core.Type] = hydra.lib.eithers.map[Tuple2[hydra.core.Type,
         hydra.context.Context], hydra.core.Type, hydra.errors.Error]((_r: Tuple2[hydra.core.Type,
         hydra.context.Context]) =>
        hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](_r))(hydra.checking.typeOf(cx)(tc)(Seq())(term))
      hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Type, Either[T1, hydra.python.syntax.Expression]]((_x: hydra.errors.Error) => Right(pyexp))((typ: hydra.core.Type) =>
        hydra.lib.eithers.either((_x) => Right(pyexp))((pytyp: hydra.python.syntax.Expression) => Right(hydra.python.utils.castTo(pytyp)(pyexp)))(hydra.python.coder.encodeType(env)(typ)))(mtyp)
    }
  })
  hydra.strip.deannotateAndDetypeTerm(term) match
    case hydra.core.Term.application(v_Term_application_app) => hydra.python.coder.encodeApplication(cx)(env)(v_Term_application_app)
    case hydra.core.Term.either(v_Term_either_et) => hydra.lib.eithers.either[hydra.core.Term,
       hydra.core.Term, Either[hydra.errors.Error, hydra.python.syntax.Expression]]((t1: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Expression, hydra.python.syntax.Expression](encode(t1))((pyexp: hydra.python.syntax.Expression) =>
      withCast(hydra.python.utils.functionCall(hydra.python.utils.pyNameToPyPrimary("Left"))(Seq(pyexp)))))((t1: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Expression, hydra.python.syntax.Expression](encode(t1))((pyexp: hydra.python.syntax.Expression) =>
      withCast(hydra.python.utils.functionCall(hydra.python.utils.pyNameToPyPrimary("Right"))(Seq(pyexp)))))(v_Term_either_et)
    case hydra.core.Term.lambda(v_Term_lambda_lam) => hydra.lib.eithers.bind[hydra.errors.Error,
       hydra.typing.FunctionStructure[hydra.python.environment.PythonEnvironment],
       hydra.python.syntax.Expression](hydra.python.coder.analyzePythonFunction(cx)(env)(hydra.core.Term.lambda(v_Term_lambda_lam)))((fs: hydra.typing.FunctionStructure[hydra.python.environment.PythonEnvironment]) =>
      {
      lazy val params: Seq[hydra.core.Name] = (fs.params)
      {
        lazy val bindings: Seq[hydra.core.Binding] = (fs.bindings)
        {
          lazy val innerBody: hydra.core.Term = (fs.body)
          {
            lazy val innerEnv0: hydra.python.environment.PythonEnvironment = (fs.environment)
            {
              lazy val bindingNames: Seq[hydra.core.Name] = hydra.lib.lists.map[hydra.core.Binding,
                 hydra.core.Name]((b: hydra.core.Binding) => (b.name))(bindings)
              {
                lazy val innerEnv: hydra.python.environment.PythonEnvironment = hydra.python.environment.PythonEnvironment(innerEnv0.namespaces,
                   (innerEnv0.boundTypeVariables), (innerEnv0.graph), (innerEnv0.nullaryBindings),
                   (innerEnv0.version), (innerEnv0.skipCasts), hydra.lib.sets.union[hydra.core.Name](hydra.lib.sets.fromList[hydra.core.Name](bindingNames))(innerEnv0.inlineVariables))
                hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Expression,
                   hydra.python.syntax.Expression](hydra.python.coder.encodeTermInline(cx)(innerEnv)(false)(innerBody))((pbody: hydra.python.syntax.Expression) =>
                  {
                  lazy val pparams: Seq[hydra.python.syntax.Name] = hydra.lib.lists.map[hydra.core.Name,
                     hydra.python.syntax.Name]((v1: hydra.core.Name) =>
                    hydra.python.names.encodeName(false)(hydra.util.CaseConvention.lowerSnake)(innerEnv)(v1))(params)
                  hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.python.syntax.Expression]](hydra.lib.lists.`null`[hydra.core.Binding](bindings))(Right(hydra.python.coder.makeUncurriedLambda(pparams)(pbody)))(hydra.lib.eithers.bind[hydra.errors.Error,
                     Seq[hydra.python.syntax.NamedExpression], hydra.python.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Binding,
                     hydra.python.syntax.NamedExpression, hydra.errors.Error]((v1: hydra.core.Binding) =>
                    hydra.python.coder.encodeBindingAsAssignment(cx)(false)(innerEnv)(v1))(bindings))((pbindingExprs: Seq[hydra.python.syntax.NamedExpression]) =>
                    {
                    lazy val pbindingStarExprs: Seq[hydra.python.syntax.StarNamedExpression] = hydra.lib.lists.map[hydra.python.syntax.NamedExpression,
                       hydra.python.syntax.StarNamedExpression]((ne: hydra.python.syntax.NamedExpression) => hydra.python.syntax.StarNamedExpression.simple(ne))(pbindingExprs)
                    {
                      lazy val pbodyStarExpr: hydra.python.syntax.StarNamedExpression = hydra.python.utils.pyExpressionToPyStarNamedExpression(pbody)
                      {
                        lazy val tupleElements: Seq[hydra.python.syntax.StarNamedExpression] = hydra.lib.lists.concat2[hydra.python.syntax.StarNamedExpression](pbindingStarExprs)(Seq(pbodyStarExpr))
                        {
                          lazy val tupleExpr: hydra.python.syntax.Expression = hydra.python.utils.pyAtomToPyExpression(hydra.python.syntax.Atom.tuple(tupleElements))
                          {
                            lazy val indexValue: hydra.python.syntax.Expression = hydra.python.utils.pyAtomToPyExpression(hydra.python.syntax.Atom.number(hydra.python.syntax.Number.integer(hydra.lib.literals.int32ToBigint(hydra.lib.lists.length[hydra.core.Binding](bindings)))))
                            {
                              lazy val indexedExpr: hydra.python.syntax.Primary = hydra.python.utils.primaryWithExpressionSlices(hydra.python.utils.pyExpressionToPyPrimary(tupleExpr))(Seq(indexValue))
                              Right(hydra.python.coder.makeUncurriedLambda(pparams)(hydra.python.utils.pyPrimaryToPyExpression(indexedExpr)))
                            }
                          }
                        }
                      }
                    }
                  }))
                })
              }
            }
          }
        }
      }
    })
    case hydra.core.Term.project(v_Term_project_proj) => {
      lazy val fname: hydra.core.Name = (v_Term_project_proj.field)
      Right(hydra.python.coder.makeCurriedLambda(Seq("v1"))(hydra.python.utils.projectFromExpression(hydra.python.syntax.Expression.simple(Seq(Seq(hydra.python.syntax.Inversion.simple(hydra.python.syntax.Comparison(hydra.python.syntax.BitwiseOr(None,
         hydra.python.syntax.BitwiseXor(None, hydra.python.syntax.BitwiseAnd(None,
         hydra.python.syntax.ShiftExpression(None, hydra.python.syntax.Sum(None, hydra.python.syntax.Term(None,
         hydra.python.syntax.Factor.simple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(false,
         hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name("v1"))),
         None)))))))), Seq()))))))(hydra.python.names.encodeFieldName(env)(fname))))
    }
    case hydra.core.Term.unwrap(v_Term_unwrap__) => Right(hydra.python.coder.makeCurriedLambda(Seq("v1"))(hydra.python.utils.projectFromExpression(hydra.python.syntax.Expression.simple(Seq(Seq(hydra.python.syntax.Inversion.simple(hydra.python.syntax.Comparison(hydra.python.syntax.BitwiseOr(None,
       hydra.python.syntax.BitwiseXor(None, hydra.python.syntax.BitwiseAnd(None, hydra.python.syntax.ShiftExpression(None,
       hydra.python.syntax.Sum(None, hydra.python.syntax.Term(None, hydra.python.syntax.Factor.simple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(false,
       hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name("v1"))), None)))))))),
       Seq()))))))("value")))
    case hydra.core.Term.cases(v_Term_cases__) => Right(hydra.python.coder.unsupportedExpression("case expressions as values are not yet supported"))
    case hydra.core.Term.let(v_Term_let_lt) => {
      lazy val bindings: Seq[hydra.core.Binding] = (v_Term_let_lt.bindings)
      {
        lazy val body: hydra.core.Term = (v_Term_let_lt.body)
        hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.python.syntax.Expression]](hydra.lib.lists.`null`[hydra.core.Binding](bindings))(hydra.python.coder.encodeTermInline(cx)(env)(false)(body))(hydra.python.coder.withLetInline(env)(v_Term_let_lt)((innerEnv: hydra.python.environment.PythonEnvironment) =>
          hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.python.syntax.NamedExpression],
             hydra.python.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Binding,
             hydra.python.syntax.NamedExpression, hydra.errors.Error]((v1: hydra.core.Binding) =>
          hydra.python.coder.encodeBindingAsAssignment(cx)(false)(innerEnv)(v1))(bindings))((pbindingExprs: Seq[hydra.python.syntax.NamedExpression]) =>
          hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Expression,
             hydra.python.syntax.Expression](hydra.python.coder.encodeTermInline(cx)(innerEnv)(false)(body))((pbody: hydra.python.syntax.Expression) =>
          {
          lazy val pbindingStarExprs: Seq[hydra.python.syntax.StarNamedExpression] = hydra.lib.lists.map[hydra.python.syntax.NamedExpression,
             hydra.python.syntax.StarNamedExpression]((ne: hydra.python.syntax.NamedExpression) => hydra.python.syntax.StarNamedExpression.simple(ne))(pbindingExprs)
          {
            lazy val pbodyStarExpr: hydra.python.syntax.StarNamedExpression = hydra.python.utils.pyExpressionToPyStarNamedExpression(pbody)
            {
              lazy val tupleElements: Seq[hydra.python.syntax.StarNamedExpression] = hydra.lib.lists.concat2[hydra.python.syntax.StarNamedExpression](pbindingStarExprs)(Seq(pbodyStarExpr))
              {
                lazy val tupleExpr: hydra.python.syntax.Expression = hydra.python.utils.pyAtomToPyExpression(hydra.python.syntax.Atom.tuple(tupleElements))
                {
                  lazy val indexValue: hydra.python.syntax.Expression = hydra.python.utils.pyAtomToPyExpression(hydra.python.syntax.Atom.number(hydra.python.syntax.Number.integer(hydra.lib.literals.int32ToBigint(hydra.lib.lists.length[hydra.core.Binding](bindings)))))
                  {
                    lazy val indexedExpr: hydra.python.syntax.Primary = hydra.python.utils.primaryWithExpressionSlices(hydra.python.utils.pyExpressionToPyPrimary(tupleExpr))(Seq(indexValue))
                    Right(hydra.python.utils.pyPrimaryToPyExpression(indexedExpr))
                  }
                }
              }
            }
          }
        }))))
      }
    }
    case hydra.core.Term.list(v_Term_list_terms) => hydra.lib.eithers.bind[hydra.errors.Error,
       Seq[hydra.python.syntax.Expression], hydra.python.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Term,
       hydra.python.syntax.Expression, hydra.errors.Error](encode)(v_Term_list_terms))((pyExprs: Seq[hydra.python.syntax.Expression]) =>
      Right(hydra.python.utils.pyAtomToPyExpression(hydra.python.syntax.Atom.tuple(hydra.lib.lists.map[hydra.python.syntax.Expression,
         hydra.python.syntax.StarNamedExpression](hydra.python.utils.pyExpressionToPyStarNamedExpression)(pyExprs)))))
    case hydra.core.Term.literal(v_Term_literal_lit) => hydra.python.coder.encodeLiteral(v_Term_literal_lit)
    case hydra.core.Term.map(v_Term_map_m) => hydra.lib.eithers.bind[hydra.errors.Error,
       Seq[hydra.python.syntax.DoubleStarredKvpair], hydra.python.syntax.Expression](hydra.lib.eithers.mapList[Tuple2[hydra.core.Term,
       hydra.core.Term], hydra.python.syntax.DoubleStarredKvpair, hydra.errors.Error]((kv: Tuple2[hydra.core.Term,
       hydra.core.Term]) =>
      {
      lazy val k: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](kv)
      {
        lazy val v: hydra.core.Term = hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](kv)
        hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Expression,
           hydra.python.syntax.DoubleStarredKvpair](encode(k))((pyK: hydra.python.syntax.Expression) =>
          hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Expression,
             hydra.python.syntax.DoubleStarredKvpair](encode(v))((pyV: hydra.python.syntax.Expression) =>
          Right(hydra.python.syntax.DoubleStarredKvpair.pair(hydra.python.syntax.Kvpair(pyK, pyV)))))
      }
    })(hydra.lib.maps.toList[hydra.core.Term, hydra.core.Term](v_Term_map_m)))((pairs: Seq[hydra.python.syntax.DoubleStarredKvpair]) =>
      Right(hydra.python.utils.functionCall(hydra.python.utils.pyNameToPyPrimary("FrozenDict"))(Seq(hydra.python.utils.pyAtomToPyExpression(hydra.python.syntax.Atom.dict(pairs))))))
    case hydra.core.Term.maybe(v_Term_maybe_mt) => hydra.lib.maybes.maybe[Either[hydra.errors.Error,
       hydra.python.syntax.Expression], hydra.core.Term](Right(hydra.python.utils.functionCall(hydra.python.utils.pyNameToPyPrimary("Nothing"))(Seq())))((t1: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Expression, hydra.python.syntax.Expression](encode(t1))((pyexp: hydra.python.syntax.Expression) =>
      withCast(hydra.python.utils.functionCall(hydra.python.utils.pyNameToPyPrimary("Just"))(Seq(pyexp)))))(v_Term_maybe_mt)
    case hydra.core.Term.pair(v_Term_pair_p) => {
      lazy val t1: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](v_Term_pair_p)
      {
        lazy val t2: hydra.core.Term = hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](v_Term_pair_p)
        hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Expression,
           hydra.python.syntax.Expression](encode(t1))((pyExpr1: hydra.python.syntax.Expression) =>
          hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Expression,
             hydra.python.syntax.Expression](encode(t2))((pyExpr2: hydra.python.syntax.Expression) =>
          Right(hydra.python.utils.pyAtomToPyExpression(hydra.python.syntax.Atom.tuple(Seq(hydra.python.utils.pyExpressionToPyStarNamedExpression(pyExpr1),
             hydra.python.utils.pyExpressionToPyStarNamedExpression(pyExpr2)))))))
      }
    }
    case hydra.core.Term.record(v_Term_record_r) => {
      lazy val tname: hydra.core.Name = (v_Term_record_r.typeName)
      {
        lazy val fields: Seq[hydra.core.Field] = (v_Term_record_r.fields)
        hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.python.syntax.Expression],
           hydra.python.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Field,
           hydra.python.syntax.Expression, hydra.errors.Error]((fld: hydra.core.Field) => encode(fld.term))(fields))((pargs: Seq[hydra.python.syntax.Expression]) =>
          Right(hydra.python.utils.functionCall(hydra.python.utils.pyNameToPyPrimary(hydra.python.names.encodeNameQualified(env)(tname)))(pargs)))
      }
    }
    case hydra.core.Term.set(v_Term_set_s) => hydra.lib.eithers.bind[hydra.errors.Error,
       Seq[hydra.python.syntax.Expression], hydra.python.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Term,
       hydra.python.syntax.Expression, hydra.errors.Error](encode)(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_s)))((pyEls: Seq[hydra.python.syntax.Expression]) =>
      Right(hydra.python.utils.functionCall(hydra.python.utils.pyNameToPyPrimary("frozenset"))(Seq(hydra.python.utils.pyAtomToPyExpression(hydra.python.syntax.Atom.set(hydra.lib.lists.map[hydra.python.syntax.Expression,
         hydra.python.syntax.StarNamedExpression](hydra.python.utils.pyExpressionToPyStarNamedExpression)(pyEls)))))))
    case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => {
      lazy val body: hydra.core.Term = (v_Term_typeApplication_ta.body)
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Expression, hydra.python.syntax.Expression](hydra.python.coder.encodeTermInline(cx)(env)(true)(stripTypeApps(body)))((pybase: hydra.python.syntax.Expression) => withCast(pybase))
    }
    case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => {
      lazy val body: hydra.core.Term = (v_Term_typeLambda_tl.body)
      hydra.python.coder.withTypeLambda(env)(v_Term_typeLambda_tl)((env2: hydra.python.environment.PythonEnvironment) => hydra.python.coder.encodeTermInline(cx)(env2)(noCast)(body))
    }
    case hydra.core.Term.inject(v_Term_inject_inj) => {
      lazy val tname: hydra.core.Name = (v_Term_inject_inj.typeName)
      {
        lazy val field: hydra.core.Field = (v_Term_inject_inj.field)
        hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.FieldType], hydra.python.syntax.Expression](hydra.resolution.requireUnionType(cx)(hydra.python.coder.pythonEnvironmentGetGraph(env))(tname))((rt: Seq[hydra.core.FieldType]) =>
          hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.python.syntax.Expression]](hydra.predicates.isEnumRowType(rt))(Right(hydra.python.utils.projectFromExpression(hydra.python.utils.pyNameToPyExpression(hydra.python.names.encodeNameQualified(env)(tname)))(hydra.python.names.encodeEnumValue(env)(field.name))))({
          lazy val fname: hydra.core.Name = (field.name)
          {
            lazy val isUnitVariant: Boolean = hydra.lib.maybes.maybe[Boolean, hydra.core.FieldType](false)((ft: hydra.core.FieldType) =>
              hydra.predicates.isUnitType(hydra.strip.deannotateType(ft.`type`)))(hydra.lib.lists.find[hydra.core.FieldType]((ft: hydra.core.FieldType) =>
              hydra.lib.equality.equal[scala.Predef.String](ft.name)(fname))(rt))
            hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.python.syntax.Expression],
               hydra.python.syntax.Expression](hydra.lib.logic.ifElse[Either[hydra.errors.Error,
               Seq[hydra.python.syntax.Expression]]](hydra.lib.logic.or(hydra.predicates.isUnitTerm(field.term))(isUnitVariant))(Right(Seq()))(hydra.lib.eithers.bind[hydra.errors.Error,
               hydra.python.syntax.Expression, Seq[hydra.python.syntax.Expression]](encode(field.term))((parg: hydra.python.syntax.Expression) => Right(Seq(parg)))))((args: Seq[hydra.python.syntax.Expression]) =>
              {
              lazy val deconflictedName: hydra.python.syntax.Name = hydra.python.coder.deconflictVariantName(true)(env)(tname)(fname)(env.graph)
              Right(hydra.python.utils.castTo(hydra.python.names.typeVariableReference(env)(tname))(hydra.python.utils.functionCall(hydra.python.utils.pyNameToPyPrimary(deconflictedName))(args)))
            })
          }
        }))
      }
    }
    case hydra.core.Term.unit => Right(hydra.python.utils.pyNameToPyExpression(hydra.python.utils.pyNone))
    case hydra.core.Term.variable(v_Term_variable_name) => hydra.python.coder.encodeVariable(cx)(env)(v_Term_variable_name)(Seq())
    case hydra.core.Term.wrap(v_Term_wrap_wrapped) => {
      lazy val tname: hydra.core.Name = (v_Term_wrap_wrapped.typeName)
      {
        lazy val inner: hydra.core.Term = (v_Term_wrap_wrapped.body)
        hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Expression,
           hydra.python.syntax.Expression](encode(inner))((parg: hydra.python.syntax.Expression) =>
          Right(hydra.python.utils.functionCall(hydra.python.utils.pyNameToPyPrimary(hydra.python.names.encodeNameQualified(env)(tname)))(Seq(parg))))
      }
    }
}

def encodeTermMultiline(cx: hydra.context.Context)(env: hydra.python.environment.PythonEnvironment)(term: hydra.core.Term): Either[hydra.errors.Error,
   Seq[hydra.python.syntax.Statement]] =
  {
  lazy val dfltLogic: Either[hydra.errors.Error, Seq[hydra.python.syntax.Statement]] = hydra.lib.eithers.bind[hydra.errors.Error,
     hydra.typing.FunctionStructure[hydra.python.environment.PythonEnvironment], Seq[hydra.python.syntax.Statement]](hydra.python.coder.analyzePythonFunction(cx)(env)(term))((fs: hydra.typing.FunctionStructure[hydra.python.environment.PythonEnvironment]) =>
    {
    lazy val params: Seq[hydra.core.Name] = (fs.params)
    {
      lazy val bindings: Seq[hydra.core.Binding] = (fs.bindings)
      {
        lazy val innerBody: hydra.core.Term = (fs.body)
        {
          lazy val env2: hydra.python.environment.PythonEnvironment = (fs.environment)
          hydra.lib.logic.ifElse[Either[hydra.errors.Error, Seq[hydra.python.syntax.Statement]]](hydra.lib.lists.`null`[hydra.core.Binding](bindings))(hydra.lib.eithers.bind[hydra.errors.Error,
             hydra.python.syntax.Expression, Seq[hydra.python.syntax.Statement]](hydra.python.coder.encodeTermInline(cx)(env)(false)(term))((expr: hydra.python.syntax.Expression) => Right(Seq(hydra.python.utils.returnSingle(expr)))))(hydra.lib.eithers.bind[hydra.errors.Error,
             Seq[hydra.python.syntax.Statement], Seq[hydra.python.syntax.Statement]](hydra.lib.eithers.mapList[hydra.core.Binding,
             hydra.python.syntax.Statement, hydra.errors.Error]((v1: hydra.core.Binding) => hydra.python.coder.encodeBindingAs(cx)(env2)(v1))(bindings))((bindingStmts: Seq[hydra.python.syntax.Statement]) =>
            hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.python.syntax.Statement],
               Seq[hydra.python.syntax.Statement]](hydra.python.coder.encodeTermMultiline(cx)(env2)(innerBody))((bodyStmts: Seq[hydra.python.syntax.Statement]) =>
            Right(hydra.lib.lists.concat2[hydra.python.syntax.Statement](bindingStmts)(bodyStmts)))))
        }
      }
    }
  })
  lazy val gathered: Tuple2[Seq[hydra.core.Term], hydra.core.Term] = hydra.analysis.gatherApplications(term)
  lazy val args: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term], hydra.core.Term](gathered)
  lazy val body: hydra.core.Term = hydra.lib.pairs.second[Seq[hydra.core.Term], hydra.core.Term](gathered)
  hydra.lib.logic.ifElse[Either[hydra.errors.Error, Seq[hydra.python.syntax.Statement]]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Term](args))(1))({
    lazy val arg: hydra.core.Term = hydra.lib.lists.head[hydra.core.Term](args)
    hydra.strip.deannotateAndDetypeTerm(body) match
      case hydra.core.Term.cases(v_Term_cases_cs) => {
        lazy val tname: hydra.core.Name = (v_Term_cases_cs.typeName)
        {
          lazy val dflt: Option[hydra.core.Term] = (v_Term_cases_cs.default)
          {
            lazy val `cases_`: Seq[hydra.core.Field] = (v_Term_cases_cs.cases)
            hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.FieldType],
               Seq[hydra.python.syntax.Statement]](hydra.resolution.requireUnionType(cx)(hydra.python.coder.pythonEnvironmentGetGraph(env))(tname))((rt: Seq[hydra.core.FieldType]) =>
              {
              lazy val isEnum: Boolean = hydra.predicates.isEnumRowType(rt)
              {
                lazy val isFull: Boolean = hydra.python.coder.isCasesFull(rt)(`cases_`)
                hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Expression,
                   Seq[hydra.python.syntax.Statement]](hydra.python.coder.encodeTermInline(cx)(env)(false)(arg))((pyArg: hydra.python.syntax.Expression) =>
                  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.python.syntax.CaseBlock],
                     Seq[hydra.python.syntax.Statement]](hydra.lib.eithers.mapList[hydra.core.Field,
                     hydra.python.syntax.CaseBlock, hydra.errors.Error]((v1: hydra.core.Field) =>
                  hydra.python.coder.encodeCaseBlock(cx)(env)(tname)(rt)(isEnum)((e: hydra.python.environment.PythonEnvironment) =>
                  (t: hydra.core.Term) => hydra.python.coder.encodeTermMultiline(cx)(e)(t))(v1))(hydra.python.coder.deduplicateCaseVariables(`cases_`)))((pyCases: Seq[hydra.python.syntax.CaseBlock]) =>
                  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.python.syntax.CaseBlock],
                     Seq[hydra.python.syntax.Statement]](hydra.python.coder.encodeDefaultCaseBlock((t: hydra.core.Term) => hydra.python.coder.encodeTermInline(cx)(env)(false)(t))(isFull)(dflt)(tname))((pyDflt: Seq[hydra.python.syntax.CaseBlock]) =>
                  {
                  lazy val subj: hydra.python.syntax.SubjectExpression = hydra.python.syntax.SubjectExpression.simple(hydra.python.syntax.NamedExpression.simple(pyArg))
                  {
                    lazy val matchStmt: hydra.python.syntax.Statement = hydra.python.syntax.Statement.compound(hydra.python.syntax.CompoundStatement.`match`(hydra.python.syntax.MatchStatement(subj,
                       hydra.lib.lists.concat2[hydra.python.syntax.CaseBlock](pyCases)(pyDflt))))
                    Right(Seq(matchStmt))
                  }
                })))
              }
            })
          }
        }
      }
      case _ => dfltLogic
  })(dfltLogic)
}

def encodeTermMultilineTCO(cx: hydra.context.Context)(env: hydra.python.environment.PythonEnvironment)(funcName: hydra.core.Name)(paramNames: Seq[hydra.core.Name])(term: hydra.core.Term): Either[hydra.errors.Error,
   Seq[hydra.python.syntax.Statement]] =
  {
  lazy val stripped: hydra.core.Term = hydra.strip.deannotateAndDetypeTerm(term)
  lazy val gathered: Tuple2[Seq[hydra.core.Term], hydra.core.Term] = hydra.analysis.gatherApplications(stripped)
  lazy val gatherArgs: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term], hydra.core.Term](gathered)
  lazy val gatherFun: hydra.core.Term = hydra.lib.pairs.second[Seq[hydra.core.Term], hydra.core.Term](gathered)
  lazy val strippedFun: hydra.core.Term = hydra.strip.deannotateAndDetypeTerm(gatherFun)
  lazy val isSelfCall: Boolean = strippedFun match
    case hydra.core.Term.variable(v_Term_variable_n) => hydra.lib.equality.equal[hydra.core.Name](v_Term_variable_n)(funcName)
    case _ => false
  hydra.lib.logic.ifElse[Either[hydra.errors.Error, Seq[hydra.python.syntax.Statement]]](hydra.lib.logic.and(isSelfCall)(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Term](gatherArgs))(hydra.lib.lists.length[hydra.core.Name](paramNames))))(hydra.lib.eithers.bind[hydra.errors.Error,
     Seq[hydra.python.syntax.Expression], Seq[hydra.python.syntax.Statement]](hydra.lib.eithers.mapList[hydra.core.Term,
     hydra.python.syntax.Expression, hydra.errors.Error]((a: hydra.core.Term) => hydra.python.coder.encodeTermInline(cx)(env)(false)(a))(gatherArgs))((pyArgs: Seq[hydra.python.syntax.Expression]) =>
    {
    lazy val assignments: Seq[hydra.python.syntax.Statement] = hydra.lib.lists.map[Tuple2[hydra.core.Name,
       hydra.python.syntax.Expression], hydra.python.syntax.Statement]((pair: Tuple2[hydra.core.Name,
       hydra.python.syntax.Expression]) =>
      {
      lazy val paramName: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, hydra.python.syntax.Expression](pair)
      {
        lazy val pyArg: hydra.python.syntax.Expression = hydra.lib.pairs.second[hydra.core.Name,
           hydra.python.syntax.Expression](pair)
        hydra.python.utils.assignmentStatement(hydra.python.names.encodeName(false)(hydra.util.CaseConvention.lowerSnake)(env)(paramName))(pyArg)
      }
    })(hydra.lib.lists.zip[hydra.core.Name, hydra.python.syntax.Expression](paramNames)(pyArgs))
    {
      lazy val continueStmt: hydra.python.syntax.Statement = hydra.python.syntax.Statement.simple(Seq(hydra.python.syntax.SimpleStatement.continue))
      Right(hydra.lib.lists.concat2[hydra.python.syntax.Statement](assignments)(Seq(continueStmt)))
    }
  }))({
    lazy val gathered2: Tuple2[Seq[hydra.core.Term], hydra.core.Term] = hydra.analysis.gatherApplications(term)
    {
      lazy val args2: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term], hydra.core.Term](gathered2)
      {
        lazy val body2: hydra.core.Term = hydra.lib.pairs.second[Seq[hydra.core.Term], hydra.core.Term](gathered2)
        hydra.lib.logic.ifElse[Either[hydra.errors.Error, Seq[hydra.python.syntax.Statement]]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Term](args2))(1))({
          lazy val arg: hydra.core.Term = hydra.lib.lists.head[hydra.core.Term](args2)
          hydra.strip.deannotateAndDetypeTerm(body2) match
            case hydra.core.Term.cases(v_Term_cases_cs) => {
              lazy val tname: hydra.core.Name = (v_Term_cases_cs.typeName)
              {
                lazy val dflt: Option[hydra.core.Term] = (v_Term_cases_cs.default)
                {
                  lazy val `cases_`: Seq[hydra.core.Field] = (v_Term_cases_cs.cases)
                  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.FieldType],
                     Seq[hydra.python.syntax.Statement]](hydra.resolution.requireUnionType(cx)(hydra.python.coder.pythonEnvironmentGetGraph(env))(tname))((rt: Seq[hydra.core.FieldType]) =>
                    {
                    lazy val isEnum: Boolean = hydra.predicates.isEnumRowType(rt)
                    {
                      lazy val isFull: Boolean = hydra.python.coder.isCasesFull(rt)(`cases_`)
                      hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Expression,
                         Seq[hydra.python.syntax.Statement]](hydra.python.coder.encodeTermInline(cx)(env)(false)(arg))((pyArg: hydra.python.syntax.Expression) =>
                        hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.python.syntax.CaseBlock],
                           Seq[hydra.python.syntax.Statement]](hydra.lib.eithers.mapList[hydra.core.Field,
                           hydra.python.syntax.CaseBlock, hydra.errors.Error]((v1: hydra.core.Field) =>
                        hydra.python.coder.encodeCaseBlock(cx)(env)(tname)(rt)(isEnum)((e2: hydra.python.environment.PythonEnvironment) =>
                        (t2: hydra.core.Term) =>
                        hydra.python.coder.encodeTermMultilineTCO(cx)(e2)(funcName)(paramNames)(t2))(v1))(hydra.python.coder.deduplicateCaseVariables(`cases_`)))((pyCases: Seq[hydra.python.syntax.CaseBlock]) =>
                        hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.python.syntax.CaseBlock],
                           Seq[hydra.python.syntax.Statement]](hydra.python.coder.encodeDefaultCaseBlock((t2: hydra.core.Term) => hydra.python.coder.encodeTermInline(cx)(env)(false)(t2))(isFull)(dflt)(tname))((pyDflt: Seq[hydra.python.syntax.CaseBlock]) =>
                        {
                        lazy val subj: hydra.python.syntax.SubjectExpression = hydra.python.syntax.SubjectExpression.simple(hydra.python.syntax.NamedExpression.simple(pyArg))
                        {
                          lazy val matchStmt: hydra.python.syntax.Statement = hydra.python.syntax.Statement.compound(hydra.python.syntax.CompoundStatement.`match`(hydra.python.syntax.MatchStatement(subj,
                             hydra.lib.lists.concat2[hydra.python.syntax.CaseBlock](pyCases)(pyDflt))))
                          Right(Seq(matchStmt))
                        }
                      })))
                    }
                  })
                }
              }
            }
            case _ => hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Expression,
               Seq[hydra.python.syntax.Statement]](hydra.python.coder.encodeTermInline(cx)(env)(false)(term))((expr: hydra.python.syntax.Expression) => Right(Seq(hydra.python.utils.returnSingle(expr))))
        })(hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Expression,
           Seq[hydra.python.syntax.Statement]](hydra.python.coder.encodeTermInline(cx)(env)(false)(term))((expr: hydra.python.syntax.Expression) => Right(Seq(hydra.python.utils.returnSingle(expr)))))
      }
    }
  })
}

def encodeType[T0](env: hydra.python.environment.PythonEnvironment)(typ: hydra.core.Type): Either[T0,
   hydra.python.syntax.Expression] =
  {
  def dflt[T1]: Either[T1, hydra.python.syntax.Expression] =
    Right(hydra.python.utils.doubleQuotedString(hydra.lib.strings.cat2("type = ")(hydra.show.core.`type`(hydra.strip.deannotateType(typ)))))
  hydra.strip.deannotateType(typ) match
    case hydra.core.Type.application(v_Type_application_at) => hydra.python.coder.encodeApplicationType(env)(v_Type_application_at)
    case hydra.core.Type.function(v_Type_function_ft) => hydra.python.coder.encodeFunctionType(env)(v_Type_function_ft)
    case hydra.core.Type.forall(v_Type_forall_lt) => hydra.python.coder.encodeForallType(env)(v_Type_forall_lt)
    case hydra.core.Type.list(v_Type_list_et) => hydra.lib.eithers.bind[T0, hydra.python.syntax.Expression,
       hydra.python.syntax.Expression](hydra.python.coder.encodeType(env)(v_Type_list_et))((pyet: hydra.python.syntax.Expression) =>
      Right(hydra.python.utils.nameAndParams("frozenlist")(Seq(pyet))))
    case hydra.core.Type.map(v_Type_map_mt) => hydra.lib.eithers.bind[T0, hydra.python.syntax.Expression,
       hydra.python.syntax.Expression](hydra.python.coder.encodeType(env)(v_Type_map_mt.keys))((pykt: hydra.python.syntax.Expression) =>
      hydra.lib.eithers.bind[T0, hydra.python.syntax.Expression, hydra.python.syntax.Expression](hydra.python.coder.encodeType(env)(v_Type_map_mt.values))((pyvt: hydra.python.syntax.Expression) =>
      Right(hydra.python.utils.nameAndParams("FrozenDict")(Seq(pykt, pyvt)))))
    case hydra.core.Type.literal(v_Type_literal_lt) => hydra.python.coder.encodeLiteralType(v_Type_literal_lt)
    case hydra.core.Type.maybe(v_Type_maybe_et) => hydra.lib.eithers.bind[T0, hydra.python.syntax.Expression,
       hydra.python.syntax.Expression](hydra.python.coder.encodeType(env)(v_Type_maybe_et))((ptype: hydra.python.syntax.Expression) =>
      Right(hydra.python.utils.pyPrimaryToPyExpression(hydra.python.utils.primaryWithExpressionSlices(hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name("Maybe")))(Seq(ptype)))))
    case hydra.core.Type.either(v_Type_either_eitherT) => hydra.lib.eithers.bind[T0,
       hydra.python.syntax.Expression, hydra.python.syntax.Expression](hydra.python.coder.encodeType(env)(v_Type_either_eitherT.left))((pyleft: hydra.python.syntax.Expression) =>
      hydra.lib.eithers.bind[T0, hydra.python.syntax.Expression, hydra.python.syntax.Expression](hydra.python.coder.encodeType(env)(v_Type_either_eitherT.right))((pyright: hydra.python.syntax.Expression) =>
      Right(hydra.python.utils.pyPrimaryToPyExpression(hydra.python.utils.primaryWithExpressionSlices(hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name("Either")))(Seq(pyleft,
         pyright))))))
    case hydra.core.Type.pair(v_Type_pair_pairT) => hydra.lib.eithers.bind[T0, hydra.python.syntax.Expression,
       hydra.python.syntax.Expression](hydra.python.coder.encodeType(env)(v_Type_pair_pairT.first))((pyFirst: hydra.python.syntax.Expression) =>
      hydra.lib.eithers.bind[T0, hydra.python.syntax.Expression, hydra.python.syntax.Expression](hydra.python.coder.encodeType(env)(v_Type_pair_pairT.second))((pySecond: hydra.python.syntax.Expression) =>
      Right(hydra.python.utils.nameAndParams("tuple")(Seq(pyFirst, pySecond)))))
    case hydra.core.Type.record(v_Type_record__) => dflt
    case hydra.core.Type.set(v_Type_set_et) => hydra.lib.eithers.bind[T0, hydra.python.syntax.Expression,
       hydra.python.syntax.Expression](hydra.python.coder.encodeType(env)(v_Type_set_et))((pyet: hydra.python.syntax.Expression) =>
      Right(hydra.python.utils.nameAndParams("frozenset")(Seq(pyet))))
    case hydra.core.Type.union(v_Type_union__) => dflt
    case hydra.core.Type.unit => Right(hydra.python.utils.pyNameToPyExpression(hydra.python.utils.pyNone))
    case hydra.core.Type.void => Right(hydra.python.utils.pyNameToPyExpression(hydra.python.utils.pyNone))
    case hydra.core.Type.variable(v_Type_variable_name) => Right(hydra.python.names.typeVariableReference(env)(v_Type_variable_name))
    case hydra.core.Type.wrap(v_Type_wrap__) => dflt
    case hydra.core.Type.annotated(v_Type_annotated__) => dflt
}

def encodeTypeAssignment[T0](cx: T0)(env: hydra.python.environment.PythonEnvironment)(name: hydra.core.Name)(typ: hydra.core.Type)(comment: Option[scala.Predef.String]): Either[hydra.errors.Error,
   Seq[Seq[hydra.python.syntax.Statement]]] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.python.syntax.Statement], Seq[Seq[hydra.python.syntax.Statement]]](hydra.python.coder.encodeTypeAssignmentInner(cx)(env)(name)(typ)(comment))((defStmts: Seq[hydra.python.syntax.Statement]) =>
  Right(hydra.lib.lists.map[hydra.python.syntax.Statement, Seq[hydra.python.syntax.Statement]]((s: hydra.python.syntax.Statement) => Seq(s))(defStmts)))

def encodeTypeAssignmentInner[T0](cx: T0)(env: hydra.python.environment.PythonEnvironment)(name: hydra.core.Name)(typ: hydra.core.Type)(comment: Option[scala.Predef.String]): Either[hydra.errors.Error,
   Seq[hydra.python.syntax.Statement]] =
  {
  lazy val stripped: hydra.core.Type = hydra.strip.deannotateType(typ)
  def dflt[T1]: Either[T1, Seq[hydra.python.syntax.Statement]] =
    hydra.lib.eithers.bind[T1, hydra.python.syntax.Expression, Seq[hydra.python.syntax.Statement]](hydra.python.coder.encodeType(env)(typ))((typeExpr: hydra.python.syntax.Expression) =>
    Right(hydra.python.coder.encodeTypeDefSingle(env)(name)(comment)(typeExpr)))
  stripped match
    case hydra.core.Type.forall(v_Type_forall_ft) => {
      lazy val tvar: hydra.core.Name = (v_Type_forall_ft.parameter)
      {
        lazy val body: hydra.core.Type = (v_Type_forall_ft.body)
        {
          lazy val newEnv: hydra.python.environment.PythonEnvironment = hydra.python.coder.extendEnvWithTypeVar(env)(tvar)
          hydra.python.coder.encodeTypeAssignmentInner(cx)(newEnv)(name)(body)(comment)
        }
      }
    }
    case hydra.core.Type.record(v_Type_record_rt) => hydra.lib.eithers.map[hydra.python.syntax.Statement,
       Seq[hydra.python.syntax.Statement], hydra.errors.Error]((s: hydra.python.syntax.Statement) => Seq(s))(hydra.python.coder.encodeRecordType(cx)(env)(name)(v_Type_record_rt)(comment))
    case hydra.core.Type.union(v_Type_union_rt) => hydra.python.coder.encodeUnionType(cx)(env)(name)(v_Type_union_rt)(comment)
    case hydra.core.Type.wrap(v_Type_wrap_wt) => hydra.python.coder.encodeWrappedType(env)(name)(v_Type_wrap_wt)(comment)
    case _ => dflt
}

def encodeTypeDefSingle(env: hydra.python.environment.PythonEnvironment)(name: hydra.core.Name)(comment: Option[scala.Predef.String])(typeExpr: hydra.python.syntax.Expression): Seq[hydra.python.syntax.Statement] =
  {
  lazy val pyName: hydra.python.syntax.Name = hydra.python.names.encodeName(false)(hydra.util.CaseConvention.pascal)(env)(name)
  lazy val tparams: Seq[hydra.python.syntax.TypeParameter] = hydra.python.coder.environmentTypeParameters(env)
  Seq(hydra.python.coder.typeAliasStatementFor(env)(pyName)(tparams)(comment)(typeExpr))
}

def encodeTypeQuoted[T0](env: hydra.python.environment.PythonEnvironment)(typ: hydra.core.Type): Either[T0,
   hydra.python.syntax.Expression] =
  hydra.lib.eithers.bind[T0, hydra.python.syntax.Expression, hydra.python.syntax.Expression](hydra.python.coder.encodeType(env)(typ))((pytype: hydra.python.syntax.Expression) =>
  Right(hydra.lib.logic.ifElse[hydra.python.syntax.Expression](hydra.lib.sets.`null`[hydra.core.Name](hydra.variables.freeVariablesInType(typ)))(pytype)(hydra.python.utils.doubleQuotedString(hydra.serialization.printExpr(hydra.python.serde.encodeExpression(pytype))))))

def encodeUnionEliminationInline(cx: hydra.context.Context)(env: hydra.python.environment.PythonEnvironment)(cs: hydra.core.CaseStatement)(pyArg: hydra.python.syntax.Expression): Either[hydra.errors.Error,
   hydra.python.syntax.Expression] =
  {
  lazy val tname: hydra.core.Name = (cs.typeName)
  lazy val mdefault: Option[hydra.core.Term] = (cs.default)
  lazy val `cases_`: Seq[hydra.core.Field] = (cs.cases)
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.FieldType], hydra.python.syntax.Expression](hydra.resolution.requireUnionType(cx)(hydra.python.coder.pythonEnvironmentGetGraph(env))(tname))((rt: Seq[hydra.core.FieldType]) =>
    {
    lazy val isEnum: Boolean = hydra.predicates.isEnumRowType(rt)
    {
      lazy val valueExpr: hydra.python.syntax.Expression = hydra.python.utils.projectFromExpression(pyArg)("value")
      {
        lazy val isinstancePrimary: hydra.python.syntax.Primary = hydra.python.utils.pyNameToPyPrimary("isinstance")
        hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Expression,
           hydra.python.syntax.Expression](hydra.lib.maybes.maybe[Either[hydra.errors.Error,
           hydra.python.syntax.Expression], hydra.core.Term](Right(hydra.python.coder.unsupportedExpression("no matching case in inline union elimination")))((dflt: hydra.core.Term) => hydra.python.coder.encodeTermInline(cx)(env)(false)(dflt))(mdefault))((pyDefault: hydra.python.syntax.Expression) =>
          {
          def encodeBranch(field: hydra.core.Field): Either[hydra.errors.Error, Tuple2[hydra.python.syntax.Expression,
             hydra.python.syntax.Expression]] =
            {
            lazy val fname: hydra.core.Name = (field.name)
            lazy val fterm: hydra.core.Term = (field.term)
            lazy val isUnitVariant: Boolean = hydra.python.coder.isVariantUnitType(rt)(fname)
            lazy val pyVariantName: hydra.python.syntax.Name = hydra.python.coder.deconflictVariantName(true)(env)(tname)(fname)(env.graph)
            lazy val isinstanceCheck: hydra.python.syntax.Expression = hydra.lib.logic.ifElse[hydra.python.syntax.Expression](isEnum)(hydra.python.syntax.Expression.simple(Seq(Seq(hydra.python.syntax.Inversion.simple(hydra.python.syntax.Comparison(hydra.python.utils.pyExpressionToBitwiseOr(pyArg),
               Seq(hydra.python.syntax.CompareOpBitwiseOrPair(hydra.python.syntax.CompareOp.eq,
               hydra.python.utils.pyExpressionToBitwiseOr(hydra.python.utils.pyNameToPyExpression(pyVariantName))))))))))(hydra.python.utils.functionCall(isinstancePrimary)(Seq(pyArg,
               hydra.python.utils.pyNameToPyExpression(pyVariantName))))
            hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Expression,
               Tuple2[hydra.python.syntax.Expression, hydra.python.syntax.Expression]](hydra.python.coder.encodeTermInline(cx)(env)(false)(fterm))((pyBranch: hydra.python.syntax.Expression) =>
              {
              lazy val pyResult: hydra.python.syntax.Expression = hydra.lib.logic.ifElse[hydra.python.syntax.Expression](isEnum)(hydra.python.utils.functionCall(hydra.python.utils.pyExpressionToPyPrimary(pyBranch))(Seq(pyArg)))(hydra.lib.logic.ifElse[hydra.python.syntax.Expression](isUnitVariant)(hydra.python.utils.functionCall(hydra.python.utils.pyExpressionToPyPrimary(pyBranch))(Seq(pyArg)))(hydra.python.utils.functionCall(hydra.python.utils.pyExpressionToPyPrimary(pyBranch))(Seq(valueExpr))))
              Right(Tuple2(isinstanceCheck, pyResult))
            })
          }
          hydra.lib.eithers.bind[hydra.errors.Error, Seq[Tuple2[hydra.python.syntax.Expression,
             hydra.python.syntax.Expression]], hydra.python.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Field,
             Tuple2[hydra.python.syntax.Expression, hydra.python.syntax.Expression],
             hydra.errors.Error](encodeBranch)(`cases_`))((encodedBranches: Seq[Tuple2[hydra.python.syntax.Expression,
             hydra.python.syntax.Expression]]) =>
            {
            def buildChain(elseExpr: hydra.python.syntax.Expression)(branchPair: Tuple2[hydra.python.syntax.Expression,
               hydra.python.syntax.Expression]): hydra.python.syntax.Expression =
              {
              lazy val checkExpr: hydra.python.syntax.Expression = hydra.lib.pairs.first[hydra.python.syntax.Expression,
                 hydra.python.syntax.Expression](branchPair)
              lazy val resultExpr: hydra.python.syntax.Expression = hydra.lib.pairs.second[hydra.python.syntax.Expression,
                 hydra.python.syntax.Expression](branchPair)
              hydra.python.syntax.Expression.conditional(hydra.python.syntax.Conditional(hydra.python.utils.pyExpressionToDisjunction(resultExpr),
                 hydra.python.utils.pyExpressionToDisjunction(checkExpr), elseExpr))
            }
            Right(hydra.lib.lists.foldl[hydra.python.syntax.Expression, Tuple2[hydra.python.syntax.Expression,
               hydra.python.syntax.Expression]](buildChain)(pyDefault)(hydra.lib.lists.reverse[Tuple2[hydra.python.syntax.Expression,
               hydra.python.syntax.Expression]](encodedBranches)))
          })
        })
      }
    }
  })
}

def encodeUnionField[T0](cx: T0)(env: hydra.python.environment.PythonEnvironment)(unionName: hydra.core.Name)(fieldType: hydra.core.FieldType): Either[hydra.errors.Error,
   hydra.python.syntax.Statement] =
  {
  lazy val fname: hydra.core.Name = (fieldType.name)
  lazy val ftype: hydra.core.Type = (fieldType.`type`)
  hydra.lib.eithers.bind[hydra.errors.Error, Option[scala.Predef.String], hydra.python.syntax.Statement](hydra.annotations.getTypeDescription(cx)(hydra.python.coder.pythonEnvironmentGetGraph(env))(ftype))((fcomment: Option[scala.Predef.String]) =>
    {
    lazy val isUnit: Boolean = hydra.lib.equality.equal[hydra.core.Type](hydra.strip.deannotateType(ftype))(hydra.core.Type.unit)
    {
      lazy val varName: hydra.python.syntax.Name = hydra.python.coder.deconflictVariantName(false)(env)(unionName)(fname)(env.graph)
      {
        lazy val tparamNames: Seq[hydra.core.Name] = hydra.python.coder.findTypeParams(env)(ftype)
        {
          lazy val tparamPyNames: Seq[hydra.python.syntax.Name] = hydra.lib.lists.map[hydra.core.Name,
             hydra.python.syntax.Name](hydra.python.names.encodeTypeVariable)(tparamNames)
          {
            lazy val fieldParams: Seq[hydra.python.syntax.TypeParameter] = hydra.lib.lists.map[hydra.python.syntax.Name,
               hydra.python.syntax.TypeParameter](hydra.python.utils.pyNameToPyTypeParameter)(tparamPyNames)
            {
              lazy val body: hydra.python.syntax.Block = hydra.lib.logic.ifElse[hydra.python.syntax.Block](isUnit)(hydra.python.utils.indentedBlock(fcomment)(Seq(hydra.python.utils.unitVariantMethods(varName))))(hydra.python.utils.indentedBlock(fcomment)(Seq()))
              hydra.lib.eithers.bind[hydra.errors.Error, Option[hydra.python.syntax.Args],
                 hydra.python.syntax.Statement](hydra.lib.logic.ifElse[Either[hydra.errors.Error,
                 Option[hydra.python.syntax.Args]]](isUnit)(Right(None))(hydra.lib.eithers.bind[hydra.errors.Error,
                 hydra.python.syntax.Expression, Option[hydra.python.syntax.Args]](hydra.python.coder.encodeTypeQuoted(env)(ftype))((quotedType: hydra.python.syntax.Expression) =>
                Right(Some(hydra.python.coder.variantArgs(quotedType)(Seq()))))))((margs: Option[hydra.python.syntax.Args]) =>
                Right(hydra.python.utils.pyClassDefinitionToPyStatement(hydra.python.syntax.ClassDefinition(None,
                   varName, fieldParams, margs, body))))
            }
          }
        }
      }
    }
  })
}

def encodeUnionFieldAlt(env: hydra.python.environment.PythonEnvironment)(unionName: hydra.core.Name)(fieldType: hydra.core.FieldType): hydra.python.syntax.Primary =
  {
  lazy val fname: hydra.core.Name = (fieldType.name)
  lazy val ftype: hydra.core.Type = (fieldType.`type`)
  lazy val tparamNames: Seq[hydra.core.Name] = hydra.python.coder.findTypeParams(env)(ftype)
  lazy val tparams: Seq[hydra.python.syntax.Name] = hydra.lib.lists.map[hydra.core.Name,
     hydra.python.syntax.Name](hydra.python.names.encodeTypeVariable)(tparamNames)
  lazy val namePrim: hydra.python.syntax.Primary = hydra.python.utils.pyNameToPyPrimary(hydra.python.names.variantName(false)(env)(unionName)(fname))
  hydra.lib.logic.ifElse[hydra.python.syntax.Primary](hydra.lib.lists.`null`[hydra.python.syntax.Name](tparams))(namePrim)({
    lazy val tparamExprs: Seq[hydra.python.syntax.Expression] = hydra.lib.lists.map[hydra.python.syntax.Name,
       hydra.python.syntax.Expression](hydra.python.utils.pyNameToPyExpression)(tparams)
    hydra.python.utils.primaryWithExpressionSlices(namePrim)(tparamExprs)
  })
}

def encodeUnionType[T0](cx: T0)(env: hydra.python.environment.PythonEnvironment)(name: hydra.core.Name)(rowType: Seq[hydra.core.FieldType])(comment: Option[scala.Predef.String]): Either[hydra.errors.Error,
   Seq[hydra.python.syntax.Statement]] =
  hydra.lib.logic.ifElse[Either[hydra.errors.Error, Seq[hydra.python.syntax.Statement]]](hydra.predicates.isEnumRowType(rowType))(hydra.lib.eithers.bind[hydra.errors.Error,
     Seq[Seq[hydra.python.syntax.Statement]], Seq[hydra.python.syntax.Statement]](hydra.lib.eithers.mapList[hydra.core.FieldType,
     Seq[hydra.python.syntax.Statement], hydra.errors.Error]((v1: hydra.core.FieldType) => hydra.python.coder.encodeEnumValueAssignment(cx)(env)(v1))(rowType))((vals: Seq[Seq[hydra.python.syntax.Statement]]) =>
  {
  lazy val body: hydra.python.syntax.Block = hydra.python.utils.indentedBlock(comment)(vals)
  {
    lazy val enumName: hydra.python.syntax.Name = "Enum"
    {
      lazy val args: Option[hydra.python.syntax.Args] = Some(hydra.python.utils.pyExpressionsToPyArgs(Seq(hydra.python.utils.pyNameToPyExpression(enumName))))
      {
        lazy val pyName: hydra.python.syntax.Name = hydra.python.names.encodeName(false)(hydra.util.CaseConvention.pascal)(env)(name)
        {
          lazy val typeConstStmt: hydra.python.syntax.Statement = hydra.python.utils.dottedAssignmentStatement(pyName)(hydra.python.names.encodeConstantForTypeName(env)(name))(hydra.python.utils.functionCall(hydra.python.utils.pyNameToPyPrimary(hydra.python.names.encodeName(true)(hydra.util.CaseConvention.pascal)(env)("hydra.core.Name")))(Seq(hydra.python.utils.doubleQuotedString(name))))
          Right(Seq(hydra.python.utils.pyClassDefinitionToPyStatement(hydra.python.syntax.ClassDefinition(None,
             pyName, Seq(), args, body)), typeConstStmt))
        }
      }
    }
  }
}))({
  lazy val constStmts: Seq[hydra.python.syntax.Statement] = hydra.python.coder.encodeNameConstants(env)(name)(rowType)
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.python.syntax.Statement], Seq[hydra.python.syntax.Statement]](hydra.lib.eithers.mapList[hydra.core.FieldType,
     hydra.python.syntax.Statement, hydra.errors.Error]((v1: hydra.core.FieldType) => hydra.python.coder.encodeUnionField(cx)(env)(name)(v1))(rowType))((fieldStmts: Seq[hydra.python.syntax.Statement]) =>
    {
    lazy val tparams: Seq[hydra.python.syntax.TypeParameter] = hydra.python.coder.environmentTypeParameters(env)
    {
      lazy val unionAlts: Seq[hydra.python.syntax.Primary] = hydra.lib.lists.map[hydra.core.FieldType,
         hydra.python.syntax.Primary]((v1: hydra.core.FieldType) => hydra.python.coder.encodeUnionFieldAlt(env)(name)(v1))(rowType)
      {
        lazy val unionStmts: Seq[hydra.python.syntax.Statement] = hydra.python.coder.unionTypeStatementsFor(env)(hydra.python.names.encodeName(false)(hydra.util.CaseConvention.pascal)(env)(name))(tparams)(comment)(hydra.python.utils.orExpression(unionAlts))(constStmts)
        Right(hydra.lib.lists.concat2[hydra.python.syntax.Statement](fieldStmts)(unionStmts))
      }
    }
  })
})

def encodeVariable[T0](cx: T0)(env: hydra.python.environment.PythonEnvironment)(name: hydra.core.Name)(args: Seq[hydra.python.syntax.Expression]): Either[hydra.errors.Error,
   hydra.python.syntax.Expression] =
  {
  lazy val g: hydra.graph.Graph = hydra.python.coder.pythonEnvironmentGetGraph(env)
  lazy val tc: hydra.graph.Graph = (env.graph)
  lazy val tcTypes: Map[hydra.core.Name, hydra.core.TypeScheme] = (tc.boundTypes)
  lazy val tcLambdaVars: scala.collection.immutable.Set[hydra.core.Name] = (tc.lambdaVariables)
  lazy val tcMetadata: Map[hydra.core.Name, hydra.core.Term] = (tc.metadata)
  lazy val inlineVars: scala.collection.immutable.Set[hydra.core.Name] = (env.inlineVariables)
  lazy val mTypScheme: Option[hydra.core.TypeScheme] = hydra.lib.maps.lookup[hydra.core.Name,
     hydra.core.TypeScheme](name)(tcTypes)
  lazy val mTyp: Option[hydra.core.Type] = hydra.lib.maybes.map[hydra.core.TypeScheme,
     hydra.core.Type]((`ts_`: hydra.core.TypeScheme) => (`ts_`.`type`))(mTypScheme)
  lazy val asVariable: hydra.python.syntax.Expression = hydra.python.names.termVariableReference(env)(name)
  lazy val asFunctionCall: hydra.python.syntax.Expression = hydra.python.utils.functionCall(hydra.python.utils.pyNameToPyPrimary(hydra.python.names.encodeName(true)(hydra.util.CaseConvention.lowerSnake)(env)(name)))(args)
  hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.python.syntax.Expression]](hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.python.syntax.Expression](args)))(hydra.lib.maybes.maybe[Either[hydra.errors.Error,
     hydra.python.syntax.Expression], hydra.graph.Primitive](Right(asFunctionCall))((prim: hydra.graph.Primitive) =>
    {
    lazy val primArity: Int = hydra.arity.primitiveArity(prim)
    hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.python.syntax.Expression]](hydra.lib.equality.equal[Int](primArity)(hydra.lib.lists.length[hydra.python.syntax.Expression](args)))(Right(asFunctionCall))({
      lazy val numRemaining: Int = hydra.lib.math.sub(primArity)(hydra.lib.lists.length[hydra.python.syntax.Expression](args))
      {
        lazy val remainingParams: Seq[hydra.python.syntax.Name] = hydra.lib.lists.map[Int,
           hydra.python.syntax.Name]((i: Int) => hydra.lib.strings.cat2("x")(hydra.lib.literals.showInt32(i)))(hydra.lib.math.range(1)(numRemaining))
        {
          lazy val remainingExprs: Seq[hydra.python.syntax.Expression] = hydra.lib.lists.map[hydra.python.syntax.Name,
             hydra.python.syntax.Expression]((n: hydra.python.syntax.Name) =>
            hydra.python.syntax.Expression.simple(Seq(Seq(hydra.python.syntax.Inversion.simple(hydra.python.syntax.Comparison(hydra.python.syntax.BitwiseOr(None,
               hydra.python.syntax.BitwiseXor(None, hydra.python.syntax.BitwiseAnd(None,
               hydra.python.syntax.ShiftExpression(None, hydra.python.syntax.Sum(None,
               hydra.python.syntax.Term(None, hydra.python.syntax.Factor.simple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(false,
               hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name(n))),
               None)))))))), Seq()))))))(remainingParams)
          {
            lazy val allArgs: Seq[hydra.python.syntax.Expression] = hydra.lib.lists.concat2[hydra.python.syntax.Expression](args)(remainingExprs)
            {
              lazy val fullCall: hydra.python.syntax.Expression = hydra.python.utils.functionCall(hydra.python.utils.pyNameToPyPrimary(hydra.python.names.encodeName(true)(hydra.util.CaseConvention.lowerSnake)(env)(name)))(allArgs)
              Right(hydra.python.coder.makeUncurriedLambda(remainingParams)(fullCall))
            }
          }
        }
      }
    })
  })(hydra.lexical.lookupPrimitive(g)(name)))(hydra.lib.maybes.maybe[Either[hydra.errors.Error,
     hydra.python.syntax.Expression], hydra.core.Type](hydra.lib.logic.ifElse[Either[hydra.errors.Error,
     hydra.python.syntax.Expression]](hydra.lib.sets.member[hydra.core.Name](name)(tcLambdaVars))(Right(asVariable))(hydra.lib.logic.ifElse[Either[hydra.errors.Error,
     hydra.python.syntax.Expression]](hydra.lib.sets.member[hydra.core.Name](name)(inlineVars))(Right(asVariable))(hydra.lib.maybes.maybe[Either[hydra.errors.Error,
     hydra.python.syntax.Expression], hydra.graph.Primitive](hydra.lib.maybes.maybe[Either[hydra.errors.Error,
     hydra.python.syntax.Expression], hydra.core.Binding](hydra.lib.maybes.maybe[Either[hydra.errors.Error,
     hydra.python.syntax.Expression], hydra.core.Term](Left(hydra.errors.Error.other(hydra.lib.strings.cat2("Unknown variable: ")(name))))((_x: hydra.core.Term) => Right(asFunctionCall))(hydra.lib.maps.lookup[hydra.core.Name,
     hydra.core.Term](name)(tcMetadata)))((el: hydra.core.Binding) =>
    {
    lazy val elTrivial1: Boolean = hydra.predicates.isTrivialTerm(el.term)
    hydra.lib.maybes.maybe[Either[hydra.errors.Error, hydra.python.syntax.Expression],
       hydra.core.TypeScheme](Right(asVariable))((ts: hydra.core.TypeScheme) =>
      hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.python.syntax.Expression]](hydra.lib.logic.and(hydra.lib.logic.and(hydra.lib.equality.equal[Int](hydra.arity.typeSchemeArity(ts))(0))(hydra.predicates.isComplexBinding(tc)(el)))(hydra.lib.logic.not(elTrivial1)))(Right(asFunctionCall))({
      lazy val asFunctionRef: hydra.python.syntax.Expression = hydra.lib.logic.ifElse[hydra.python.syntax.Expression](hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.core.Name](ts.variables)))(hydra.python.coder.makeSimpleLambda(hydra.arity.typeArity(ts.`type`))(asVariable))(asVariable)
      Right(asFunctionRef)
    }))(el.`type`)
  })(hydra.lexical.lookupBinding(g)(name)))((prim: hydra.graph.Primitive) =>
    {
    lazy val primArity: Int = hydra.arity.primitiveArity(prim)
    hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.python.syntax.Expression]](hydra.lib.equality.equal[Int](primArity)(0))(Right(asFunctionCall))({
      lazy val ts: hydra.core.TypeScheme = (prim.`type`)
      {
        lazy val asFunctionRef: hydra.python.syntax.Expression = hydra.lib.logic.ifElse[hydra.python.syntax.Expression](hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.core.Name](ts.variables)))(hydra.python.coder.makeSimpleLambda(hydra.arity.typeArity(ts.`type`))(asVariable))(asVariable)
        Right(asFunctionRef)
      }
    })
  })(hydra.lexical.lookupPrimitive(g)(name)))))((typ: hydra.core.Type) =>
    hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.python.syntax.Expression]](hydra.lib.sets.member[hydra.core.Name](name)(tcLambdaVars))(Right(asVariable))(hydra.lib.logic.ifElse[Either[hydra.errors.Error,
       hydra.python.syntax.Expression]](hydra.lib.sets.member[hydra.core.Name](name)(inlineVars))({
    lazy val asFunctionRef: hydra.python.syntax.Expression = hydra.lib.logic.ifElse[hydra.python.syntax.Expression](hydra.lib.logic.not(hydra.lib.sets.`null`[hydra.core.Name](hydra.variables.freeVariablesInType(typ))))(hydra.python.coder.makeSimpleLambda(hydra.arity.typeArity(typ))(asVariable))(asVariable)
    Right(asFunctionRef)
  })(hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.python.syntax.Expression]](hydra.lib.logic.not(hydra.lib.maps.member[hydra.core.Name,
     hydra.core.Term](name)(tcMetadata)))(hydra.lib.maybes.maybe[Either[hydra.errors.Error,
     hydra.python.syntax.Expression], hydra.core.Binding]({
    lazy val asFunctionRef: hydra.python.syntax.Expression = hydra.lib.logic.ifElse[hydra.python.syntax.Expression](hydra.lib.logic.not(hydra.lib.sets.`null`[hydra.core.Name](hydra.variables.freeVariablesInType(typ))))(hydra.python.coder.makeSimpleLambda(hydra.arity.typeArity(typ))(asVariable))(asVariable)
    Right(asFunctionRef)
  })((el: hydra.core.Binding) =>
    {
    lazy val elTrivial: Boolean = hydra.predicates.isTrivialTerm(el.term)
    hydra.lib.maybes.maybe[Either[hydra.errors.Error, hydra.python.syntax.Expression],
       hydra.core.TypeScheme](hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.python.syntax.Expression]](hydra.lib.logic.and(hydra.lib.equality.equal[Int](hydra.arity.typeArity(typ))(0))(hydra.lib.logic.not(elTrivial)))(Right(asFunctionCall))({
      lazy val asFunctionRef: hydra.python.syntax.Expression = hydra.lib.logic.ifElse[hydra.python.syntax.Expression](hydra.lib.logic.not(hydra.lib.sets.`null`[hydra.core.Name](hydra.variables.freeVariablesInType(typ))))(hydra.python.coder.makeSimpleLambda(hydra.arity.typeArity(typ))(asVariable))(asVariable)
      Right(asFunctionRef)
    }))((ts: hydra.core.TypeScheme) =>
      hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.python.syntax.Expression]](hydra.lib.logic.and(hydra.lib.logic.and(hydra.lib.equality.equal[Int](hydra.arity.typeArity(typ))(0))(hydra.predicates.isComplexBinding(tc)(el)))(hydra.lib.logic.not(elTrivial)))(Right(asFunctionCall))({
      lazy val asFunctionRef: hydra.python.syntax.Expression = hydra.lib.logic.ifElse[hydra.python.syntax.Expression](hydra.lib.logic.not(hydra.lib.sets.`null`[hydra.core.Name](hydra.variables.freeVariablesInType(typ))))(hydra.python.coder.makeSimpleLambda(hydra.arity.typeArity(typ))(asVariable))(asVariable)
      Right(asFunctionRef)
    }))(el.`type`)
  })(hydra.lexical.lookupBinding(g)(name)))(hydra.lib.logic.ifElse[Either[hydra.errors.Error,
     hydra.python.syntax.Expression]](hydra.lib.logic.and(hydra.lib.equality.equal[Int](hydra.arity.typeArity(typ))(0))(hydra.predicates.isComplexVariable(tc)(name)))(Right(asFunctionCall))({
    lazy val asFunctionRef: hydra.python.syntax.Expression = hydra.lib.logic.ifElse[hydra.python.syntax.Expression](hydra.lib.logic.not(hydra.lib.sets.`null`[hydra.core.Name](hydra.variables.freeVariablesInType(typ))))(hydra.python.coder.makeSimpleLambda(hydra.arity.typeArity(typ))(asVariable))(asVariable)
    Right(asFunctionRef)
  })))))(mTyp))
}

def encodeWrappedType[T0](env: hydra.python.environment.PythonEnvironment)(name: hydra.core.Name)(typ: hydra.core.Type)(comment: Option[scala.Predef.String]): Either[T0,
   Seq[hydra.python.syntax.Statement]] =
  {
  lazy val tparamList: Seq[hydra.core.Name] = hydra.lib.pairs.first[Seq[hydra.core.Name],
     Map[hydra.core.Name, hydra.python.syntax.Name]](env.boundTypeVariables)
  hydra.lib.eithers.bind[T0, hydra.python.syntax.Expression, Seq[hydra.python.syntax.Statement]](hydra.python.coder.encodeTypeQuoted(env)(typ))((ptypeQuoted: hydra.python.syntax.Expression) =>
    {
    lazy val pyName: hydra.python.syntax.Name = hydra.python.names.encodeName(false)(hydra.util.CaseConvention.pascal)(env)(name)
    {
      lazy val body: hydra.python.syntax.Block = hydra.python.utils.indentedBlock(comment)(Seq())
      {
        lazy val typeConstStmt: hydra.python.syntax.Statement = hydra.python.utils.dottedAssignmentStatement(pyName)(hydra.python.names.encodeConstantForTypeName(env)(name))(hydra.python.utils.functionCall(hydra.python.utils.pyNameToPyPrimary(hydra.python.names.encodeName(true)(hydra.util.CaseConvention.pascal)(env)("hydra.core.Name")))(Seq(hydra.python.utils.doubleQuotedString(name))))
        Right(Seq(hydra.python.utils.pyClassDefinitionToPyStatement(hydra.python.syntax.ClassDefinition(None,
           pyName, hydra.lib.lists.map[hydra.core.Name, hydra.python.syntax.TypeParameter]((`arg_`: hydra.core.Name) =>
          hydra.python.utils.pyNameToPyTypeParameter(hydra.python.names.encodeTypeVariable(`arg_`)))(hydra.python.coder.findTypeParams(env)(typ)),
             Some(hydra.python.coder.variantArgs(ptypeQuoted)(tparamList)), body)),
             typeConstStmt))
      }
    }
  })
}

def enumVariantPattern(env: hydra.python.environment.PythonEnvironment)(typeName: hydra.core.Name)(fieldName: hydra.core.Name): hydra.python.syntax.ClosedPattern =
  hydra.python.syntax.ClosedPattern.value(Seq(hydra.python.names.encodeName(true)(hydra.util.CaseConvention.pascal)(env)(typeName),
     hydra.python.names.encodeEnumValue(env)(fieldName)))

def environmentTypeParameters(env: hydra.python.environment.PythonEnvironment): Seq[hydra.python.syntax.TypeParameter] =
  hydra.lib.lists.map[hydra.core.Name, hydra.python.syntax.TypeParameter]((`arg_`: hydra.core.Name) =>
  hydra.python.utils.pyNameToPyTypeParameter(hydra.python.names.encodeTypeVariable(`arg_`)))(hydra.lib.pairs.first[Seq[hydra.core.Name],
     Map[hydra.core.Name, hydra.python.syntax.Name]](env.boundTypeVariables))

def extendEnvWithLambdaParams(env: hydra.python.environment.PythonEnvironment)(term: hydra.core.Term): hydra.python.environment.PythonEnvironment =
  {
  def go(e: hydra.python.environment.PythonEnvironment)(t: hydra.core.Term): hydra.python.environment.PythonEnvironment =
    hydra.strip.deannotateAndDetypeTerm(t) match
    case hydra.core.Term.lambda(v_Term_lambda_lam) => {
      lazy val newTc: hydra.graph.Graph = hydra.scoping.extendGraphForLambda(hydra.python.coder.pythonEnvironmentGetGraph(e))(v_Term_lambda_lam)
      {
        lazy val newEnv: hydra.python.environment.PythonEnvironment = hydra.python.coder.pythonEnvironmentSetGraph(newTc)(e)
        go(newEnv)(v_Term_lambda_lam.body)
      }
    }
    case _ => e
  go(env)(term)
}

def extendEnvWithTypeVar(env: hydra.python.environment.PythonEnvironment)(`var_`: hydra.core.Name): hydra.python.environment.PythonEnvironment =
  {
  lazy val oldBound: Tuple2[Seq[hydra.core.Name], Map[hydra.core.Name, hydra.python.syntax.Name]] = (env.boundTypeVariables)
  lazy val tparamList: Seq[hydra.core.Name] = hydra.lib.pairs.first[Seq[hydra.core.Name],
     Map[hydra.core.Name, hydra.python.syntax.Name]](oldBound)
  lazy val tparamMap: Map[hydra.core.Name, hydra.python.syntax.Name] = hydra.lib.pairs.second[Seq[hydra.core.Name],
     Map[hydra.core.Name, hydra.python.syntax.Name]](oldBound)
  lazy val newList: Seq[hydra.core.Name] = hydra.lib.lists.concat2[hydra.core.Name](tparamList)(Seq(`var_`))
  lazy val newMap: Map[hydra.core.Name, hydra.python.syntax.Name] = hydra.lib.maps.insert[hydra.core.Name,
     hydra.python.syntax.Name](`var_`)(hydra.python.names.encodeTypeVariable(`var_`))(tparamMap)
  hydra.python.environment.PythonEnvironment(env.namespaces, Tuple2(newList, newMap),
     (env.graph), (env.nullaryBindings), (env.version), (env.skipCasts), (env.inlineVariables))
}

def extendMetaForTerm(topLevel: Boolean)(meta0: hydra.python.environment.PythonModuleMetadata)(term: hydra.core.Term): hydra.python.environment.PythonModuleMetadata =
  {
  def step(meta: hydra.python.environment.PythonModuleMetadata)(t: hydra.core.Term): hydra.python.environment.PythonModuleMetadata =
    t match
    case hydra.core.Term.either(v_Term_either_e) => {
      lazy val metaWithCast: hydra.python.environment.PythonModuleMetadata = hydra.python.coder.setMetaUsesCast(true)(meta)
      hydra.lib.eithers.either[hydra.core.Term, hydra.core.Term, hydra.python.environment.PythonModuleMetadata]((_x: hydra.core.Term) => hydra.python.coder.setMetaUsesLeft(metaWithCast)(true))((_x: hydra.core.Term) => hydra.python.coder.setMetaUsesRight(metaWithCast)(true))(v_Term_either_e)
    }
    case hydra.core.Term.lambda(v_Term_lambda_lam) => hydra.lib.maybes.maybe[hydra.python.environment.PythonModuleMetadata,
       hydra.core.Type](meta)((dom: hydra.core.Type) =>
      hydra.lib.logic.ifElse[hydra.python.environment.PythonModuleMetadata](topLevel)(hydra.python.coder.extendMetaForType(true)(false)(dom)(meta))(meta))(v_Term_lambda_lam.domain)
    case hydra.core.Term.let(v_Term_let_lt) => {
      lazy val bindings: Seq[hydra.core.Binding] = (v_Term_let_lt.bindings)
      hydra.lib.lists.foldl[hydra.python.environment.PythonModuleMetadata, hydra.core.Binding]({
        def forBinding(m: hydra.python.environment.PythonModuleMetadata)(b: hydra.core.Binding): hydra.python.environment.PythonModuleMetadata =
          hydra.lib.maybes.maybe[hydra.python.environment.PythonModuleMetadata, hydra.core.TypeScheme](m)((ts: hydra.core.TypeScheme) =>
          {
          lazy val term1: hydra.core.Term = (b.term)
          hydra.lib.logic.ifElse[hydra.python.environment.PythonModuleMetadata](hydra.analysis.isSimpleAssignment(term1))(m)(hydra.python.coder.extendMetaForType(true)(true)(ts.`type`)(m))
        })(b.`type`)
        forBinding
      })(meta)(bindings)
    }
    case hydra.core.Term.literal(v_Term_literal_l) => v_Term_literal_l match
      case hydra.core.Literal.float(v_Literal_float_fv) => v_Literal_float_fv match
        case hydra.core.FloatValue.bigfloat(v_FloatValue_bigfloat__) => hydra.python.coder.setMetaUsesDecimal(meta)(true)
        case _ => meta
      case _ => meta
    case hydra.core.Term.map(v_Term_map__) => hydra.python.coder.setMetaUsesFrozenDict(meta)(true)
    case hydra.core.Term.maybe(v_Term_maybe_m) => hydra.lib.maybes.maybe[hydra.python.environment.PythonModuleMetadata,
       hydra.core.Term](hydra.python.coder.setMetaUsesNothing(meta)(true))((_x: hydra.core.Term) => hydra.python.coder.setMetaUsesJust(meta)(true))(v_Term_maybe_m)
    case hydra.core.Term.inject(v_Term_inject__) => hydra.python.coder.setMetaUsesCast(true)(meta)
    case _ => meta
  hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)(step)(meta0)(term)
}

def extendMetaForType(topLevel: Boolean)(isTermAnnot: Boolean)(typ: hydra.core.Type)(meta: hydra.python.environment.PythonModuleMetadata): hydra.python.environment.PythonModuleMetadata =
  {
  lazy val currentTvars: scala.collection.immutable.Set[hydra.core.Name] = (meta.typeVariables)
  lazy val newTvars: scala.collection.immutable.Set[hydra.core.Name] = hydra.python.coder.collectTypeVariables(currentTvars)(typ)
  lazy val metaWithTvars: hydra.python.environment.PythonModuleMetadata = hydra.python.coder.setMetaTypeVariables(meta)(newTvars)
  lazy val metaWithSubtypes: hydra.python.environment.PythonModuleMetadata = hydra.lib.lists.foldl[hydra.python.environment.PythonModuleMetadata,
     hydra.core.Type]((m: hydra.python.environment.PythonModuleMetadata) =>
    (t: hydra.core.Type) =>
    hydra.python.coder.extendMetaForType(false)(isTermAnnot)(t)(m))(metaWithTvars)(hydra.rewriting.subtypes(typ))
  hydra.strip.deannotateType(typ) match
    case hydra.core.Type.function(v_Type_function_ft) => {
      lazy val cod: hydra.core.Type = (v_Type_function_ft.codomain)
      {
        lazy val dom: hydra.core.Type = (v_Type_function_ft.domain)
        {
          lazy val meta2: hydra.python.environment.PythonModuleMetadata = hydra.python.coder.extendMetaForType(topLevel)(isTermAnnot)(cod)(metaWithSubtypes)
          {
            lazy val meta3: hydra.python.environment.PythonModuleMetadata = hydra.python.coder.extendMetaForType(false)(isTermAnnot)(dom)(meta2)
            hydra.lib.logic.ifElse[hydra.python.environment.PythonModuleMetadata](hydra.lib.logic.and(isTermAnnot)(topLevel))(meta3)(hydra.python.coder.setMetaUsesCallable(meta3)(true))
          }
        }
      }
    }
    case hydra.core.Type.list(v_Type_list__) => hydra.python.coder.setMetaUsesFrozenList(metaWithSubtypes)(true)
    case hydra.core.Type.map(v_Type_map__) => hydra.python.coder.setMetaUsesFrozenDict(metaWithSubtypes)(true)
    case hydra.core.Type.maybe(v_Type_maybe__) => hydra.python.coder.setMetaUsesMaybe(metaWithSubtypes)(true)
    case hydra.core.Type.either(v_Type_either__) => hydra.python.coder.setMetaUsesEither(metaWithSubtypes)(true)
    case hydra.core.Type.literal(v_Type_literal_lt) => v_Type_literal_lt match
      case hydra.core.LiteralType.float(v_LiteralType_float_ft) => v_LiteralType_float_ft match
        case hydra.core.FloatType.bigfloat => hydra.python.coder.setMetaUsesDecimal(metaWithSubtypes)(true)
        case _ => metaWithSubtypes
      case _ => metaWithSubtypes
    case hydra.core.Type.union(v_Type_union_rt) => hydra.lib.logic.ifElse[hydra.python.environment.PythonModuleMetadata](hydra.predicates.isEnumRowType(v_Type_union_rt))(hydra.python.coder.setMetaUsesEnum(metaWithSubtypes)(true))(hydra.lib.logic.ifElse[hydra.python.environment.PythonModuleMetadata](hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.core.FieldType](v_Type_union_rt)))(hydra.python.coder.setMetaUsesNode(metaWithSubtypes)(true))(metaWithSubtypes))
    case hydra.core.Type.forall(v_Type_forall_ft) => {
      lazy val body: hydra.core.Type = (v_Type_forall_ft.body)
      {
        lazy val metaForWrap: hydra.python.environment.PythonModuleMetadata = hydra.python.coder.digForWrap(isTermAnnot)(metaWithSubtypes)(body)
        hydra.strip.deannotateType(body) match
          case hydra.core.Type.record(v_Type_record__) => hydra.python.coder.setMetaUsesGeneric(metaForWrap)(true)
          case _ => metaForWrap
      }
    }
    case hydra.core.Type.record(v_Type_record_rt) => {
      lazy val hasAnnotated: Boolean = hydra.lib.lists.foldl[Boolean, hydra.core.FieldType]((b: Boolean) =>
        (ft: hydra.core.FieldType) =>
        hydra.lib.logic.or(b)(hydra.annotations.hasTypeDescription(ft.`type`)))(false)(v_Type_record_rt)
      {
        lazy val meta1: hydra.python.environment.PythonModuleMetadata = hydra.lib.logic.ifElse[hydra.python.environment.PythonModuleMetadata](hydra.lib.lists.`null`[hydra.core.FieldType](v_Type_record_rt))(metaWithSubtypes)(hydra.python.coder.setMetaUsesDataclass(metaWithSubtypes)(true))
        hydra.lib.logic.ifElse[hydra.python.environment.PythonModuleMetadata](hasAnnotated)(hydra.python.coder.setMetaUsesAnnotated(meta1)(true))(meta1)
      }
    }
    case hydra.core.Type.wrap(v_Type_wrap__) => hydra.lib.logic.ifElse[hydra.python.environment.PythonModuleMetadata](isTermAnnot)(metaWithSubtypes)(hydra.python.coder.setMetaUsesNode(metaWithSubtypes)(true))
    case _ => metaWithSubtypes
}

def extendMetaForTypes(types: Seq[hydra.core.Type])(meta: hydra.python.environment.PythonModuleMetadata): hydra.python.environment.PythonModuleMetadata =
  {
  lazy val names: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.unions[hydra.core.Name](hydra.lib.lists.map[hydra.core.Type,
     scala.collection.immutable.Set[hydra.core.Name]]((t: hydra.core.Type) => hydra.dependencies.typeDependencyNames(false)(t))(types))
  lazy val currentNs: hydra.packaging.Namespaces[hydra.python.syntax.DottedName] = (meta.namespaces)
  lazy val updatedNs: hydra.packaging.Namespaces[hydra.python.syntax.DottedName] = hydra.analysis.addNamesToNamespaces(hydra.python.names.encodeNamespace)(names)(currentNs)
  lazy val meta1: hydra.python.environment.PythonModuleMetadata = hydra.python.coder.setMetaNamespaces(updatedNs)(meta)
  hydra.lib.lists.foldl[hydra.python.environment.PythonModuleMetadata, hydra.core.Type]((m: hydra.python.environment.PythonModuleMetadata) =>
    (t: hydra.core.Type) => hydra.python.coder.extendMetaForType(true)(false)(t)(m))(meta1)(types)
}

def extractCaseElimination(term: hydra.core.Term): Option[hydra.core.CaseStatement] =
  hydra.strip.deannotateAndDetypeTerm(term) match
  case hydra.core.Term.cases(v_Term_cases_cs) => Some(v_Term_cases_cs)
  case _ => None

def findTypeParams(env: hydra.python.environment.PythonEnvironment)(typ: hydra.core.Type): Seq[hydra.core.Name] =
  {
  lazy val boundVars: Map[hydra.core.Name, hydra.python.syntax.Name] = hydra.lib.pairs.second[Seq[hydra.core.Name],
     Map[hydra.core.Name, hydra.python.syntax.Name]](env.boundTypeVariables)
  def isBound(v: hydra.core.Name): Boolean =
    hydra.lib.maybes.isJust[hydra.python.syntax.Name](hydra.lib.maps.lookup[hydra.core.Name,
       hydra.python.syntax.Name](v)(boundVars))
  hydra.lib.lists.filter[hydra.core.Name](isBound)(hydra.lib.sets.toList[hydra.core.Name](hydra.variables.freeVariablesInType(typ)))
}

def gatherLambdas(term: hydra.core.Term): Tuple2[Seq[hydra.core.Name], hydra.core.Term] =
  {
  def go(params: Seq[hydra.core.Name])(t: hydra.core.Term): Tuple2[Seq[hydra.core.Name], hydra.core.Term] =
    hydra.strip.deannotateAndDetypeTerm(t) match
    case hydra.core.Term.lambda(v_Term_lambda_l) => go(hydra.lib.lists.concat2[hydra.core.Name](params)(Seq(v_Term_lambda_l.parameter)))(v_Term_lambda_l.body)
    case _ => Tuple2(params, t)
  go(Seq())(term)
}

def gatherMetadata(focusNs: hydra.packaging.Namespace)(defs: Seq[hydra.packaging.Definition]): hydra.python.environment.PythonModuleMetadata =
  {
  lazy val start: hydra.python.environment.PythonModuleMetadata = hydra.python.coder.emptyMetadata(hydra.python.utils.findNamespaces(focusNs)(defs))
  def addDef(meta: hydra.python.environment.PythonModuleMetadata)(`def`: hydra.packaging.Definition): hydra.python.environment.PythonModuleMetadata =
    `def` match
    case hydra.packaging.Definition.term(v_Definition_term_termDef) => {
      lazy val term: hydra.core.Term = (v_Definition_term_termDef.term)
      {
        lazy val typ: hydra.core.Type = hydra.lib.maybes.maybe[hydra.core.Type, hydra.core.TypeScheme](hydra.core.Type.variable("hydra.core.Unit"))((x: hydra.core.TypeScheme) => (x.`type`))(v_Definition_term_termDef.`type`)
        {
          lazy val meta2: hydra.python.environment.PythonModuleMetadata = hydra.python.coder.extendMetaForType(true)(true)(typ)(meta)
          hydra.python.coder.extendMetaForTerm(true)(meta2)(term)
        }
      }
    }
    case hydra.packaging.Definition.`type`(v_Definition_type_typeDef) => {
      lazy val typ: hydra.core.Type = (v_Definition_type_typeDef.`type`.`type`)
      {
        lazy val meta2: hydra.python.environment.PythonModuleMetadata = hydra.python.coder.setMetaUsesName(meta)(true)
        hydra.rewriting.foldOverType(hydra.coders.TraversalOrder.pre)((m: hydra.python.environment.PythonModuleMetadata) =>
          (t: hydra.core.Type) => hydra.python.coder.extendMetaForType(true)(false)(t)(m))(meta2)(typ)
      }
    }
  lazy val result: hydra.python.environment.PythonModuleMetadata = hydra.lib.lists.foldl[hydra.python.environment.PythonModuleMetadata,
     hydra.packaging.Definition](addDef)(start)(defs)
  lazy val tvars: scala.collection.immutable.Set[hydra.core.Name] = (result.typeVariables)
  lazy val result2: hydra.python.environment.PythonModuleMetadata = hydra.python.coder.setMetaUsesCast(true)(hydra.python.coder.setMetaUsesLruCache(true)(result))
  hydra.python.coder.setMetaUsesTypeVar(result2)(hydra.lib.logic.not(hydra.lib.sets.`null`[hydra.core.Name](tvars)))
}

def genericArg(tparamList: Seq[hydra.core.Name]): Option[hydra.python.syntax.Expression] =
  hydra.lib.logic.ifElse[Option[hydra.python.syntax.Expression]](hydra.lib.lists.`null`[hydra.core.Name](tparamList))(None)(Some(hydra.python.utils.pyPrimaryToPyExpression(hydra.python.utils.primaryWithExpressionSlices(hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name("Generic")))(hydra.lib.lists.map[hydra.core.Name,
     hydra.python.syntax.Expression]((n: hydra.core.Name) =>
  hydra.python.syntax.Expression.simple(Seq(Seq(hydra.python.syntax.Inversion.simple(hydra.python.syntax.Comparison(hydra.python.syntax.BitwiseOr(None,
     hydra.python.syntax.BitwiseXor(None, hydra.python.syntax.BitwiseAnd(None, hydra.python.syntax.ShiftExpression(None,
     hydra.python.syntax.Sum(None, hydra.python.syntax.Term(None, hydra.python.syntax.Factor.simple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(false,
     hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name(hydra.python.names.encodeTypeVariable(n)))),
     None)))))))), Seq()))))))(tparamList)))))

def initialEnvironment(namespaces: hydra.packaging.Namespaces[hydra.python.syntax.DottedName])(tcontext: hydra.graph.Graph): hydra.python.environment.PythonEnvironment =
  hydra.python.environment.PythonEnvironment(namespaces, Tuple2(Seq(), hydra.lib.maps.empty[hydra.core.Name,
     hydra.python.syntax.Name]), tcontext, hydra.lib.sets.empty[hydra.core.Name],
     hydra.python.coder.targetPythonVersion, true, hydra.lib.sets.empty[hydra.core.Name])

def initialMetadata(ns: hydra.packaging.Namespace): hydra.python.environment.PythonModuleMetadata =
  {
  lazy val dottedNs: hydra.python.syntax.DottedName = hydra.python.names.encodeNamespace(ns)
  lazy val emptyNs: hydra.packaging.Namespaces[hydra.python.syntax.DottedName] = hydra.packaging.Namespaces(Tuple2(ns,
     dottedNs), hydra.lib.maps.empty[hydra.packaging.Namespace, hydra.python.syntax.DottedName])
  hydra.python.environment.PythonModuleMetadata(emptyNs, hydra.lib.sets.empty[hydra.core.Name],
     false, false, false, false, false, false, false, false, false, false, false,
     false, false, false, false, false, false, false, false, false)
}

def isCaseStatementApplication(term: hydra.core.Term): Option[Tuple2[hydra.core.Name,
   Tuple2[Option[hydra.core.Term], Tuple2[Seq[hydra.core.Field], hydra.core.Term]]]] =
  {
  lazy val gathered: Tuple2[Seq[hydra.core.Term], hydra.core.Term] = hydra.analysis.gatherApplications(term)
  lazy val args: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term], hydra.core.Term](gathered)
  lazy val body: hydra.core.Term = hydra.lib.pairs.second[Seq[hydra.core.Term], hydra.core.Term](gathered)
  hydra.lib.logic.ifElse[Option[Tuple2[hydra.core.Name, Tuple2[Option[hydra.core.Term],
     Tuple2[Seq[hydra.core.Field], hydra.core.Term]]]]](hydra.lib.logic.not(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Term](args))(1)))(None)({
    lazy val arg: hydra.core.Term = hydra.lib.lists.head[hydra.core.Term](args)
    hydra.strip.deannotateAndDetypeTerm(body) match
      case hydra.core.Term.cases(v_Term_cases_cs) => Some(Tuple2(v_Term_cases_cs.typeName,
         Tuple2(v_Term_cases_cs.default, Tuple2(v_Term_cases_cs.cases, arg))))
      case _ => None
  })
}

def isCasesFull[T0, T1](rowType: Seq[T0])(`cases_`: Seq[T1]): Boolean =
  {
  lazy val numCases: Int = hydra.lib.lists.length[T1](`cases_`)
  lazy val numFields: Int = hydra.lib.lists.length[T0](rowType)
  hydra.lib.logic.not(hydra.lib.equality.lt[Int](numCases)(numFields))
}

def isTypeModuleCheck(defs: Seq[hydra.packaging.Definition]): Boolean =
  hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.packaging.Definition](hydra.lib.lists.filter[hydra.packaging.Definition]((d: hydra.packaging.Definition) =>
  d match
  case hydra.packaging.Definition.`type`(v_Definition_type__) => true
  case _ => false)(defs)))

def isTypeVariableName(name: hydra.core.Name): Boolean =
  hydra.lib.equality.equal[Int](1)(hydra.lib.lists.length[scala.Predef.String](hydra.lib.strings.splitOn(".")(name)))

def isVariantUnitType(rowType: Seq[hydra.core.FieldType])(fieldName: hydra.core.Name): Boolean =
  {
  lazy val mfield: Option[hydra.core.FieldType] = hydra.lib.lists.find[hydra.core.FieldType]((ft: hydra.core.FieldType) =>
    hydra.lib.equality.equal[hydra.core.Name](ft.name)(fieldName))(rowType)
  hydra.lib.maybes.fromMaybe[Boolean](false)(hydra.lib.maybes.map[hydra.core.FieldType,
     Boolean]((ft: hydra.core.FieldType) =>
    hydra.predicates.isUnitType(hydra.strip.deannotateType(ft.`type`)))(mfield))
}

lazy val lruCacheDecorator: hydra.python.syntax.NamedExpression = hydra.python.syntax.NamedExpression.simple(hydra.python.utils.functionCall(hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name("lru_cache")))(Seq(hydra.python.coder.pyInt(BigInt("1")))))

def makeCurriedLambda(params: Seq[hydra.python.syntax.Name])(body: hydra.python.syntax.Expression): hydra.python.syntax.Expression =
  hydra.lib.lists.foldl[hydra.python.syntax.Expression, hydra.python.syntax.Name]((acc: hydra.python.syntax.Expression) =>
  (p: hydra.python.syntax.Name) =>
  hydra.python.syntax.Expression.lambda(hydra.python.syntax.Lambda(hydra.python.syntax.LambdaParameters(None,
     Seq(p), Seq(), None), acc)))(body)(hydra.lib.lists.reverse[hydra.python.syntax.Name](params))

def makePyGraph(g: hydra.graph.Graph)(m: hydra.python.environment.PythonModuleMetadata): hydra.python.environment.PyGraph = hydra.python.environment.PyGraph(g,
   m)

def makeSimpleLambda(arity: Int)(lhs: hydra.python.syntax.Expression): hydra.python.syntax.Expression =
  {
  lazy val args: Seq[hydra.python.syntax.Name] = hydra.lib.lists.map[Int, hydra.python.syntax.Name]((i: Int) => hydra.lib.strings.cat2("x")(hydra.lib.literals.showInt32(i)))(hydra.lib.math.range(1)(arity))
  hydra.lib.logic.ifElse[hydra.python.syntax.Expression](hydra.lib.equality.equal[Int](arity)(0))(lhs)(hydra.python.syntax.Expression.lambda(hydra.python.syntax.Lambda(hydra.python.syntax.LambdaParameters(None,
     hydra.lib.lists.map[hydra.python.syntax.Name, hydra.python.syntax.LambdaParamNoDefault]((a: hydra.python.syntax.Name) => a)(args),
     Seq(), None), hydra.python.utils.functionCall(hydra.python.utils.pyExpressionToPyPrimary(lhs))(hydra.lib.lists.map[hydra.python.syntax.Name,
     hydra.python.syntax.Expression]((a: hydra.python.syntax.Name) =>
    hydra.python.syntax.Expression.simple(Seq(Seq(hydra.python.syntax.Inversion.simple(hydra.python.syntax.Comparison(hydra.python.syntax.BitwiseOr(None,
       hydra.python.syntax.BitwiseXor(None, hydra.python.syntax.BitwiseAnd(None, hydra.python.syntax.ShiftExpression(None,
       hydra.python.syntax.Sum(None, hydra.python.syntax.Term(None, hydra.python.syntax.Factor.simple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(false,
       hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name(a))), None)))))))),
       Seq()))))))(args)))))
}

def makeThunk(pbody: hydra.python.syntax.Expression): hydra.python.syntax.Expression =
  hydra.python.utils.functionCall(hydra.python.utils.pyExpressionToPyPrimary(hydra.python.utils.functionCall(hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name("lru_cache")))(Seq(hydra.python.coder.pyInt(BigInt("1"))))))(Seq(hydra.python.coder.wrapInNullaryLambda(pbody)))

def makeUncurriedLambda(params: Seq[hydra.python.syntax.Name])(body: hydra.python.syntax.Expression): hydra.python.syntax.Expression =
  hydra.python.syntax.Expression.lambda(hydra.python.syntax.Lambda(hydra.python.syntax.LambdaParameters(None,
     hydra.lib.lists.map[hydra.python.syntax.Name, hydra.python.syntax.LambdaParamNoDefault]((p: hydra.python.syntax.Name) => p)(params),
     Seq(), None), body))

def moduleDomainImports(namespaces: hydra.packaging.Namespaces[hydra.python.syntax.DottedName]): Seq[hydra.python.syntax.ImportStatement] =
  {
  lazy val names: Seq[hydra.python.syntax.DottedName] = hydra.lib.lists.sort[hydra.python.syntax.DottedName](hydra.lib.maps.elems[hydra.packaging.Namespace,
     hydra.python.syntax.DottedName](namespaces.mapping))
  hydra.lib.lists.map[hydra.python.syntax.DottedName, hydra.python.syntax.ImportStatement]((ns: hydra.python.syntax.DottedName) =>
    hydra.python.syntax.ImportStatement.name(Seq(hydra.python.syntax.DottedAsName(ns, None))))(names)
}

def moduleImports(namespaces: hydra.packaging.Namespaces[hydra.python.syntax.DottedName])(meta: hydra.python.environment.PythonModuleMetadata): Seq[hydra.python.syntax.Statement] =
  hydra.lib.lists.map[hydra.python.syntax.ImportStatement, hydra.python.syntax.Statement]((imp: hydra.python.syntax.ImportStatement) =>
  hydra.python.utils.pySimpleStatementToPyStatement(hydra.python.syntax.SimpleStatement.`import`(imp)))(hydra.lib.lists.concat[hydra.python.syntax.ImportStatement](Seq(hydra.python.coder.moduleStandardImports(meta),
     hydra.python.coder.moduleDomainImports(namespaces))))

def moduleStandardImports(meta: hydra.python.environment.PythonModuleMetadata): Seq[hydra.python.syntax.ImportStatement] =
  {
  lazy val pairs: Seq[Tuple2[scala.Predef.String, Seq[Option[scala.Predef.String]]]] = Seq(Tuple2("__future__",
     Seq(hydra.python.coder.condImportSymbol("annotations")(hydra.python.names.useFutureAnnotations))),
     Tuple2("collections.abc", Seq(hydra.python.coder.condImportSymbol("Callable")(meta.usesCallable))),
     Tuple2("dataclasses", Seq(hydra.python.coder.condImportSymbol("dataclass")(meta.usesDataclass))),
     Tuple2("decimal", Seq(hydra.python.coder.condImportSymbol("Decimal")(meta.usesDecimal))),
     Tuple2("enum", Seq(hydra.python.coder.condImportSymbol("Enum")(meta.usesEnum))),
     Tuple2("functools", Seq(hydra.python.coder.condImportSymbol("lru_cache")(meta.usesLruCache))),
     Tuple2("hydra.dsl.python", Seq(hydra.python.coder.condImportSymbol("Either")(meta.usesEither),
     hydra.python.coder.condImportSymbol("FrozenDict")(meta.usesFrozenDict), hydra.python.coder.condImportSymbol("Just")(meta.usesJust),
     hydra.python.coder.condImportSymbol("Left")(meta.usesLeft), hydra.python.coder.condImportSymbol("Maybe")(meta.usesMaybe),
     hydra.python.coder.condImportSymbol("Node")(meta.usesNode), hydra.python.coder.condImportSymbol("Nothing")(meta.usesNothing),
     hydra.python.coder.condImportSymbol("Right")(meta.usesRight), hydra.python.coder.condImportSymbol("frozenlist")(meta.usesFrozenList))),
     Tuple2("typing", Seq(hydra.python.coder.condImportSymbol("Annotated")(meta.usesAnnotated),
     hydra.python.coder.condImportSymbol("Generic")(meta.usesGeneric), hydra.python.coder.condImportSymbol("TypeAlias")(meta.usesTypeAlias),
     hydra.python.coder.condImportSymbol("TypeVar")(meta.usesTypeVar), hydra.python.coder.condImportSymbol("cast")(meta.usesCast))))
  lazy val simplified: Seq[Tuple2[scala.Predef.String, Seq[scala.Predef.String]]] = hydra.lib.maybes.cat[Tuple2[scala.Predef.String,
     Seq[scala.Predef.String]]](hydra.lib.lists.map[Tuple2[scala.Predef.String, Seq[Option[scala.Predef.String]]],
     Option[Tuple2[scala.Predef.String, Seq[scala.Predef.String]]]]((p: Tuple2[scala.Predef.String,
     Seq[Option[scala.Predef.String]]]) =>
    {
    lazy val modName: scala.Predef.String = hydra.lib.pairs.first[scala.Predef.String,
       Seq[Option[scala.Predef.String]]](p)
    {
      lazy val symbols: Seq[scala.Predef.String] = hydra.lib.maybes.cat[scala.Predef.String](hydra.lib.pairs.second[scala.Predef.String,
         Seq[Option[scala.Predef.String]]](p))
      hydra.lib.logic.ifElse[Option[Tuple2[scala.Predef.String, Seq[scala.Predef.String]]]](hydra.lib.lists.`null`[scala.Predef.String](symbols))(None)(Some(Tuple2(modName,
         symbols)))
    }
  })(pairs))
  hydra.lib.lists.map[Tuple2[scala.Predef.String, Seq[scala.Predef.String]], hydra.python.syntax.ImportStatement]((p: Tuple2[scala.Predef.String,
     Seq[scala.Predef.String]]) =>
    hydra.python.coder.standardImportStatement(hydra.lib.pairs.first[scala.Predef.String,
       Seq[scala.Predef.String]](p))(hydra.lib.pairs.second[scala.Predef.String, Seq[scala.Predef.String]](p)))(simplified)
}

def moduleToPython(mod: hydra.packaging.Module)(defs: Seq[hydra.packaging.Definition])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   Map[scala.Predef.String, scala.Predef.String]] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.python.syntax.Module, Map[scala.Predef.String,
     scala.Predef.String]](hydra.python.coder.encodePythonModule(cx)(g)(mod)(defs))((file: hydra.python.syntax.Module) =>
  {
  lazy val s: scala.Predef.String = hydra.serialization.printExpr(hydra.serialization.parenthesize(hydra.python.serde.encodeModule(file)))
  {
    lazy val path: scala.Predef.String = hydra.names.namespaceToFilePath(hydra.util.CaseConvention.lowerSnake)("py")(mod.namespace)
    Right(hydra.lib.maps.singleton[scala.Predef.String, scala.Predef.String](path)(s))
  }
})

def pyGraphGraph(pyg: hydra.python.environment.PyGraph): hydra.graph.Graph = (pyg.graph)

def pyGraphMetadata(pyg: hydra.python.environment.PyGraph): hydra.python.environment.PythonModuleMetadata = (pyg.metadata)

def pyInt(n: BigInt): hydra.python.syntax.Expression =
  hydra.python.utils.pyAtomToPyExpression(hydra.python.syntax.Atom.number(hydra.python.syntax.Number.integer(n)))

def pythonBindingMetadata(g: hydra.graph.Graph)(b: hydra.core.Binding): Option[hydra.core.Term] =
  hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.python.coder.shouldThunkBinding(g)(b))(hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.predicates.isComplexBinding(g)(b))(Some(hydra.core.Term.literal(hydra.core.Literal.boolean(true))))(None))(None)

def pythonEnvironmentGetGraph(env: hydra.python.environment.PythonEnvironment): hydra.graph.Graph = (env.graph)

def pythonEnvironmentSetGraph(tc: hydra.graph.Graph)(env: hydra.python.environment.PythonEnvironment): hydra.python.environment.PythonEnvironment =
  hydra.python.environment.PythonEnvironment(env.namespaces, (env.boundTypeVariables),
     tc, (env.nullaryBindings), (env.version), (env.skipCasts), (env.inlineVariables))

def setMetaNamespaces(ns: hydra.packaging.Namespaces[hydra.python.syntax.DottedName])(m: hydra.python.environment.PythonModuleMetadata): hydra.python.environment.PythonModuleMetadata =
  hydra.python.environment.PythonModuleMetadata(ns, (m.typeVariables), (m.usesAnnotated),
     (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass),
     (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList),
     (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode),
     (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def setMetaTypeVariables(m: hydra.python.environment.PythonModuleMetadata)(tvars: scala.collection.immutable.Set[hydra.core.Name]): hydra.python.environment.PythonModuleMetadata =
  hydra.python.environment.PythonModuleMetadata(m.namespaces, tvars, (m.usesAnnotated),
     (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass),
     (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList),
     (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode),
     (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def setMetaUsesAnnotated(m: hydra.python.environment.PythonModuleMetadata)(b: Boolean): hydra.python.environment.PythonModuleMetadata =
  hydra.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), b,
     (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass),
     (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList),
     (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode),
     (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def setMetaUsesCallable(m: hydra.python.environment.PythonModuleMetadata)(b: Boolean): hydra.python.environment.PythonModuleMetadata =
  hydra.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated),
     b, (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass), (m.usesDecimal),
     (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric),
     (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing),
     (m.usesRight), (m.usesTypeVar))

def setMetaUsesCast(b: Boolean)(m: hydra.python.environment.PythonModuleMetadata): hydra.python.environment.PythonModuleMetadata =
  hydra.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated),
     (m.usesCallable), b, (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass),
     (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList),
     (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode),
     (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def setMetaUsesDataclass(m: hydra.python.environment.PythonModuleMetadata)(b: Boolean): hydra.python.environment.PythonModuleMetadata =
  hydra.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated),
     (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), b, (m.usesDecimal),
     (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric),
     (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing),
     (m.usesRight), (m.usesTypeVar))

def setMetaUsesDecimal(m: hydra.python.environment.PythonModuleMetadata)(b: Boolean): hydra.python.environment.PythonModuleMetadata =
  hydra.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated),
     (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass),
     b, (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric),
     (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing),
     (m.usesRight), (m.usesTypeVar))

def setMetaUsesEither(m: hydra.python.environment.PythonModuleMetadata)(b: Boolean): hydra.python.environment.PythonModuleMetadata =
  hydra.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated),
     (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass),
     (m.usesDecimal), b, (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric),
     (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing),
     (m.usesRight), (m.usesTypeVar))

def setMetaUsesEnum(m: hydra.python.environment.PythonModuleMetadata)(b: Boolean): hydra.python.environment.PythonModuleMetadata =
  hydra.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated),
     (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass),
     (m.usesDecimal), (m.usesEither), b, (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric),
     (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing),
     (m.usesRight), (m.usesTypeVar))

def setMetaUsesFrozenDict(m: hydra.python.environment.PythonModuleMetadata)(b: Boolean): hydra.python.environment.PythonModuleMetadata =
  hydra.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated),
     (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass),
     (m.usesDecimal), (m.usesEither), (m.usesEnum), b, (m.usesFrozenList), (m.usesGeneric),
     (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing),
     (m.usesRight), (m.usesTypeVar))

def setMetaUsesFrozenList(m: hydra.python.environment.PythonModuleMetadata)(b: Boolean): hydra.python.environment.PythonModuleMetadata =
  hydra.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated),
     (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass),
     (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), b, (m.usesGeneric),
     (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing),
     (m.usesRight), (m.usesTypeVar))

def setMetaUsesGeneric(m: hydra.python.environment.PythonModuleMetadata)(b: Boolean): hydra.python.environment.PythonModuleMetadata =
  hydra.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated),
     (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass),
     (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList),
     b, (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing),
     (m.usesRight), (m.usesTypeVar))

def setMetaUsesJust(m: hydra.python.environment.PythonModuleMetadata)(b: Boolean): hydra.python.environment.PythonModuleMetadata =
  hydra.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated),
     (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass),
     (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList),
     (m.usesGeneric), b, (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode),
     (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def setMetaUsesLeft(m: hydra.python.environment.PythonModuleMetadata)(b: Boolean): hydra.python.environment.PythonModuleMetadata =
  hydra.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated),
     (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass),
     (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList),
     (m.usesGeneric), (m.usesJust), b, (m.usesMaybe), (m.usesName), (m.usesNode),
     (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def setMetaUsesLruCache(b: Boolean)(m: hydra.python.environment.PythonModuleMetadata): hydra.python.environment.PythonModuleMetadata =
  hydra.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated),
     (m.usesCallable), (m.usesCast), b, (m.usesTypeAlias), (m.usesDataclass), (m.usesDecimal),
     (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric),
     (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing),
     (m.usesRight), (m.usesTypeVar))

def setMetaUsesMaybe(m: hydra.python.environment.PythonModuleMetadata)(b: Boolean): hydra.python.environment.PythonModuleMetadata =
  hydra.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated),
     (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass),
     (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList),
     (m.usesGeneric), (m.usesJust), (m.usesLeft), b, (m.usesName), (m.usesNode), (m.usesNothing),
     (m.usesRight), (m.usesTypeVar))

def setMetaUsesName(m: hydra.python.environment.PythonModuleMetadata)(b: Boolean): hydra.python.environment.PythonModuleMetadata =
  hydra.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated),
     (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass),
     (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList),
     (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), b, (m.usesNode),
     (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def setMetaUsesNode(m: hydra.python.environment.PythonModuleMetadata)(b: Boolean): hydra.python.environment.PythonModuleMetadata =
  hydra.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated),
     (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass),
     (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList),
     (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), b,
     (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def setMetaUsesNothing(m: hydra.python.environment.PythonModuleMetadata)(b: Boolean): hydra.python.environment.PythonModuleMetadata =
  hydra.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated),
     (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass),
     (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList),
     (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode),
     b, (m.usesRight), (m.usesTypeVar))

def setMetaUsesRight(m: hydra.python.environment.PythonModuleMetadata)(b: Boolean): hydra.python.environment.PythonModuleMetadata =
  hydra.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated),
     (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass),
     (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList),
     (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode),
     (m.usesNothing), b, (m.usesTypeVar))

def setMetaUsesTypeAlias(m: hydra.python.environment.PythonModuleMetadata)(b: Boolean): hydra.python.environment.PythonModuleMetadata =
  hydra.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated),
     (m.usesCallable), (m.usesCast), (m.usesLruCache), b, (m.usesDataclass), (m.usesDecimal),
     (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric),
     (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing),
     (m.usesRight), (m.usesTypeVar))

def setMetaUsesTypeVar(m: hydra.python.environment.PythonModuleMetadata)(b: Boolean): hydra.python.environment.PythonModuleMetadata =
  hydra.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated),
     (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass),
     (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList),
     (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode),
     (m.usesNothing), (m.usesRight), b)

def shouldThunkBinding(g: hydra.graph.Graph)(b: hydra.core.Binding): Boolean =
  hydra.lib.logic.and(hydra.predicates.isComplexBinding(g)(b))(hydra.lib.logic.not(hydra.predicates.isTrivialTerm(b.term)))

def standardImportStatement(modName: scala.Predef.String)(symbols: Seq[scala.Predef.String]): hydra.python.syntax.ImportStatement =
  hydra.python.syntax.ImportStatement.from(hydra.python.syntax.ImportFrom(Seq(), Some(Seq(modName)),
     hydra.python.syntax.ImportFromTargets.simple(hydra.lib.lists.map[scala.Predef.String,
     hydra.python.syntax.ImportFromAsName]((s: scala.Predef.String) => hydra.python.syntax.ImportFromAsName(s,
     None))(symbols))))

lazy val targetPythonVersion: hydra.python.environment.PythonVersion = hydra.python.utils.targetPythonVersion

def termArityWithPrimitives(graph: hydra.graph.Graph)(term: hydra.core.Term): Int =
  hydra.strip.deannotateAndDetypeTerm(term) match
  case hydra.core.Term.application(v_Term_application_app) => hydra.lib.math.max(0)(hydra.lib.math.sub(hydra.python.coder.termArityWithPrimitives(graph)(v_Term_application_app.function))(1))
  case hydra.core.Term.lambda(v_Term_lambda_lam) => hydra.lib.math.add(1)(hydra.python.coder.termArityWithPrimitives(graph)(v_Term_lambda_lam.body))
  case hydra.core.Term.project(v_Term_project__) => 1
  case hydra.core.Term.unwrap(v_Term_unwrap__) => 1
  case hydra.core.Term.cases(v_Term_cases__) => 1
  case hydra.core.Term.variable(v_Term_variable_name) => hydra.lib.maybes.maybe[Int,
     hydra.core.Binding](0)((el: hydra.core.Binding) =>
    hydra.lib.maybes.maybe[Int, hydra.core.TypeScheme](hydra.arity.termArity(el.term))((ts: hydra.core.TypeScheme) => hydra.arity.typeSchemeArity(ts))(el.`type`))(hydra.lexical.lookupBinding(graph)(v_Term_variable_name))
  case _ => 0

def tvarStatement(name: hydra.python.syntax.Name): hydra.python.syntax.Statement =
  hydra.python.utils.assignmentStatement(name)(hydra.python.utils.functionCall(hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name("TypeVar")))(Seq(hydra.python.utils.doubleQuotedString(name))))

def typeAliasStatementFor(env: hydra.python.environment.PythonEnvironment)(name: hydra.python.syntax.Name)(tparams: Seq[hydra.python.syntax.TypeParameter])(mcomment: Option[scala.Predef.String])(tyexpr: hydra.python.syntax.Expression): hydra.python.syntax.Statement =
  hydra.lib.logic.ifElse[hydra.python.syntax.Statement](hydra.python.coder.useInlineTypeParamsFor(env.version))(hydra.python.utils.typeAliasStatement(name)(tparams)(mcomment)(tyexpr))(hydra.python.utils.typeAliasStatement310(name)(tparams)(mcomment)(tyexpr))

def unionTypeStatementsFor(env: hydra.python.environment.PythonEnvironment)(name: hydra.python.syntax.Name)(tparams: Seq[hydra.python.syntax.TypeParameter])(mcomment: Option[scala.Predef.String])(tyexpr: hydra.python.syntax.Expression)(extraStmts: Seq[hydra.python.syntax.Statement]): Seq[hydra.python.syntax.Statement] =
  hydra.lib.logic.ifElse[Seq[hydra.python.syntax.Statement]](hydra.python.coder.useInlineTypeParamsFor(env.version))(hydra.lib.lists.concat2[hydra.python.syntax.Statement](Seq(hydra.python.utils.typeAliasStatement(name)(tparams)(mcomment)(tyexpr)))(extraStmts))(hydra.python.utils.unionTypeClassStatements310(name)(mcomment)(tyexpr)(extraStmts))

def unsupportedExpression(msg: scala.Predef.String): hydra.python.syntax.Expression =
  hydra.python.utils.functionCall(hydra.python.utils.pyExpressionToPyPrimary(hydra.python.utils.projectFromExpression(hydra.python.utils.projectFromExpression(hydra.python.utils.projectFromExpression(hydra.python.syntax.Expression.simple(Seq(Seq(hydra.python.syntax.Inversion.simple(hydra.python.syntax.Comparison(hydra.python.syntax.BitwiseOr(None,
     hydra.python.syntax.BitwiseXor(None, hydra.python.syntax.BitwiseAnd(None, hydra.python.syntax.ShiftExpression(None,
     hydra.python.syntax.Sum(None, hydra.python.syntax.Term(None, hydra.python.syntax.Factor.simple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(false,
     hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name("hydra"))),
     None)))))))), Seq()))))))("dsl"))("python"))("unsupported")))(Seq(hydra.python.utils.stringToPyExpression(hydra.python.syntax.QuoteStyle.double)(msg)))

lazy val useInlineTypeParams: Boolean = hydra.python.coder.useInlineTypeParamsFor(hydra.python.utils.targetPythonVersion)

def useInlineTypeParamsFor(version: hydra.python.environment.PythonVersion): Boolean =
  hydra.lib.equality.equal[hydra.python.environment.PythonVersion](version)(hydra.python.environment.PythonVersion.python312)

def variantArgs(ptype: hydra.python.syntax.Expression)(tparams: Seq[hydra.core.Name]): hydra.python.syntax.Args =
  hydra.python.utils.pyExpressionsToPyArgs(hydra.lib.maybes.cat[hydra.python.syntax.Expression](Seq(Some(hydra.python.utils.pyPrimaryToPyExpression(hydra.python.utils.primaryWithExpressionSlices(hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name("Node")))(Seq(ptype)))),
     hydra.python.coder.genericArg(tparams))))

def variantClosedPattern[T0](env: hydra.python.environment.PythonEnvironment)(typeName: hydra.core.Name)(fieldName: hydra.core.Name)(pyVariantName: hydra.python.syntax.Name)(rowType: T0)(isEnum: Boolean)(varName: hydra.core.Name)(shouldCapture: Boolean): hydra.python.syntax.ClosedPattern =
  hydra.lib.logic.ifElse[hydra.python.syntax.ClosedPattern](isEnum)(hydra.python.coder.enumVariantPattern(env)(typeName)(fieldName))(hydra.lib.logic.ifElse[hydra.python.syntax.ClosedPattern](hydra.lib.logic.not(shouldCapture))(hydra.python.coder.classVariantPatternUnit(pyVariantName))(hydra.python.coder.classVariantPatternWithCapture(env)(pyVariantName)(varName)))

def wildcardCaseBlock(stmt: hydra.python.syntax.Statement): hydra.python.syntax.CaseBlock =
  hydra.python.syntax.CaseBlock(hydra.python.utils.pyClosedPatternToPyPatterns(hydra.python.syntax.ClosedPattern.wildcard),
     None, hydra.python.utils.indentedBlock(None)(Seq(Seq(stmt))))

def withDefinitions[T0](env: hydra.python.environment.PythonEnvironment)(defs: Seq[hydra.packaging.Definition])(body: (hydra.python.environment.PythonEnvironment => T0)): T0 =
  {
  lazy val bindings: Seq[hydra.core.Binding] = hydra.lib.maybes.cat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Definition,
     Option[hydra.core.Binding]]((`def_`: hydra.packaging.Definition) =>
    `def_` match
    case hydra.packaging.Definition.term(v_Definition_term_td) => Some(hydra.core.Binding(v_Definition_term_td.name,
       (v_Definition_term_td.term), (v_Definition_term_td.`type`)))
    case hydra.packaging.Definition.`type`(v_Definition_type__) => None
    case _ => None)(defs))
  lazy val dummyLet: hydra.core.Let = hydra.core.Let(bindings, hydra.core.Term.literal(hydra.core.Literal.string("dummy")))
  hydra.python.coder.withLet(env)(dummyLet)(body)
}

def withLambda[T0](v1: hydra.python.environment.PythonEnvironment)(v2: hydra.core.Lambda)(v3: (hydra.python.environment.PythonEnvironment => T0)): T0 =
  hydra.environment.withLambdaContext(hydra.python.coder.pythonEnvironmentGetGraph)(hydra.python.coder.pythonEnvironmentSetGraph)(v1)(v2)(v3)

def withLet[T0](v1: hydra.python.environment.PythonEnvironment)(v2: hydra.core.Let)(v3: (hydra.python.environment.PythonEnvironment => T0)): T0 =
  hydra.environment.withLetContext(hydra.python.coder.pythonEnvironmentGetGraph)(hydra.python.coder.pythonEnvironmentSetGraph)(hydra.python.coder.pythonBindingMetadata)(v1)(v2)(v3)

def withLetInline[T0](env: hydra.python.environment.PythonEnvironment)(lt: hydra.core.Let)(body: (hydra.python.environment.PythonEnvironment => T0)): T0 =
  {
  lazy val bindingNames: Seq[hydra.core.Name] = hydra.lib.lists.map[hydra.core.Binding,
     hydra.core.Name]((b: hydra.core.Binding) => (b.name))(lt.bindings)
  lazy val inlineVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](bindingNames)
  def noMetadata[T1, T2, T3](tc: T1)(b: T2): Option[T3] = None
  hydra.environment.withLetContext(hydra.python.coder.pythonEnvironmentGetGraph)(hydra.python.coder.pythonEnvironmentSetGraph)(noMetadata)(env)(lt)((innerEnv: hydra.python.environment.PythonEnvironment) =>
    {
    lazy val updatedEnv: hydra.python.environment.PythonEnvironment = hydra.python.environment.PythonEnvironment(innerEnv.namespaces,
       (innerEnv.boundTypeVariables), (innerEnv.graph), (innerEnv.nullaryBindings),
       (innerEnv.version), (innerEnv.skipCasts), hydra.lib.sets.union[hydra.core.Name](inlineVars)(innerEnv.inlineVariables))
    body(updatedEnv)
  })
}

def withTypeLambda[T0](v1: hydra.python.environment.PythonEnvironment)(v2: hydra.core.TypeLambda)(v3: (hydra.python.environment.PythonEnvironment => T0)): T0 =
  hydra.environment.withTypeLambdaContext(hydra.python.coder.pythonEnvironmentGetGraph)(hydra.python.coder.pythonEnvironmentSetGraph)(v1)(v2)(v3)

def wrapInNullaryLambda(expr: hydra.python.syntax.Expression): hydra.python.syntax.Expression =
  hydra.python.syntax.Expression.lambda(hydra.python.syntax.Lambda(hydra.python.syntax.LambdaParameters(None,
     Seq(), Seq(), None), expr))

def wrapLazyArguments(name: hydra.core.Name)(args: Seq[hydra.python.syntax.Expression]): Seq[hydra.python.syntax.Expression] =
  hydra.lib.logic.ifElse[Seq[hydra.python.syntax.Expression]](hydra.lib.logic.and(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.logic.ifElse"))(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.python.syntax.Expression](args))(3)))(Seq(hydra.lib.lists.at[hydra.python.syntax.Expression](0)(args),
     hydra.python.coder.wrapInNullaryLambda(hydra.lib.lists.at[hydra.python.syntax.Expression](1)(args)),
     hydra.python.coder.wrapInNullaryLambda(hydra.lib.lists.at[hydra.python.syntax.Expression](2)(args))))(hydra.lib.logic.ifElse[Seq[hydra.python.syntax.Expression]](hydra.lib.logic.and(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.maybes.cases"))(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.python.syntax.Expression](args))(3)))(Seq(hydra.lib.lists.at[hydra.python.syntax.Expression](0)(args),
     hydra.python.coder.wrapInNullaryLambda(hydra.lib.lists.at[hydra.python.syntax.Expression](1)(args)),
     hydra.lib.lists.at[hydra.python.syntax.Expression](2)(args)))(hydra.lib.logic.ifElse[Seq[hydra.python.syntax.Expression]](hydra.lib.logic.and(hydra.lib.logic.or(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.maybes.maybe"))(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.maybes.fromMaybe")))(hydra.lib.equality.gte[Int](hydra.lib.lists.length[hydra.python.syntax.Expression](args))(1)))(hydra.lib.lists.cons[hydra.python.syntax.Expression](hydra.python.coder.wrapInNullaryLambda(hydra.lib.lists.at[hydra.python.syntax.Expression](0)(args)))(hydra.lib.lists.tail[hydra.python.syntax.Expression](args)))(args)))
