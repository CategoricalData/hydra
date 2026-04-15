package hydra.java.coder

import hydra.core.*

import hydra.errors.*

import hydra.graph.*

import hydra.java.environment.*

import hydra.java.syntax.*

import hydra.packaging.*

import hydra.typing.*

import hydra.util.*

def addComment[T0](decl: hydra.java.syntax.ClassBodyDeclaration)(field: hydra.core.FieldType)(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.ClassBodyDeclarationWithComments] =
  hydra.lib.eithers.map[Option[scala.Predef.String], hydra.java.syntax.ClassBodyDeclarationWithComments,
     hydra.errors.Error]((c: Option[scala.Predef.String]) => hydra.java.syntax.ClassBodyDeclarationWithComments(decl,
     c))(hydra.annotations.commentsFromFieldType(cx)(g)(field))

def analyzeJavaFunction[T0, T1](env: hydra.java.environment.JavaEnvironment)(term: hydra.core.Term)(cx: hydra.context.Context)(g: T0): Either[T1,
   hydra.typing.FunctionStructure[hydra.java.environment.JavaEnvironment]] =
  hydra.analysis.analyzeFunctionTerm(cx)(hydra.java.coder.javaEnvGetGraph)(hydra.java.coder.javaEnvSetGraph)(env)(term)

def annotateBodyWithCod(typ: hydra.core.Type)(term: hydra.core.Term): hydra.core.Term =
  {
  def setAnn(t: hydra.core.Term): hydra.core.Term =
    hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(typ)))(t)
  hydra.strip.deannotateTerm(term) match
    case hydra.core.Term.typeApplication(v_Term_typeApplication__ta) => setAnn(term)
    case hydra.core.Term.application(v_Term_application_app) => {
      lazy val lhs: hydra.core.Term = (v_Term_application_app.function)
      {
        lazy val rhs: hydra.core.Term = (v_Term_application_app.argument)
        {
          lazy val annotatedRhs: hydra.core.Term = hydra.strip.deannotateTerm(rhs) match
            case hydra.core.Term.typeApplication(v_Term_typeApplication__ta2) => hydra.java.coder.annotateBodyWithCod(hydra.java.coder.extractArgType(lhs)(typ))(rhs)
            case _ => rhs
          setAnn(hydra.core.Term.application(hydra.core.Application(lhs, annotatedRhs)))
        }
      }
    }
    case _ => setAnn(term)
}

def annotateLambdaArgs[T0, T1](cname: hydra.core.Name)(tApps: Seq[hydra.core.Type])(argTerms: Seq[hydra.core.Term])(cx: T0)(g: hydra.graph.Graph): Either[T1,
   Seq[hydra.core.Term]] =
  hydra.lib.logic.ifElse[Either[T1, Seq[hydra.core.Term]]](hydra.lib.lists.`null`[hydra.core.Type](tApps))(Right(argTerms))(hydra.lib.eithers.bind[T1,
     Option[hydra.core.TypeScheme], Seq[hydra.core.Term]](hydra.lib.eithers.bind[T1, Option[hydra.core.Binding],
     Option[hydra.core.TypeScheme]](Right(hydra.lexical.lookupBinding(g)(cname)))((mel: Option[hydra.core.Binding]) =>
  hydra.lib.maybes.cases[hydra.core.Binding, Either[T1, Option[hydra.core.TypeScheme]]](mel)(Right(hydra.lib.maybes.map[hydra.graph.Primitive,
     hydra.core.TypeScheme]((prim: hydra.graph.Primitive) => (prim.`type`))(hydra.lib.maps.lookup[hydra.core.Name,
     hydra.graph.Primitive](cname)(g.primitives))))((el: hydra.core.Binding) => Right(el.`type`))))((mts: Option[hydra.core.TypeScheme]) =>
  hydra.lib.maybes.cases[hydra.core.TypeScheme, Either[T1, Seq[hydra.core.Term]]](mts)(Right(argTerms))((ts: hydra.core.TypeScheme) =>
  {
  lazy val schemeType: hydra.core.Type = (ts.`type`)
  {
    lazy val schemeTypeVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.java.coder.collectTypeVars(schemeType)
    {
      lazy val schemeVars: Seq[hydra.core.Name] = hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) => hydra.lib.sets.member[hydra.core.Name](v)(schemeTypeVars))(ts.variables)
      hydra.lib.logic.ifElse[Either[T1, Seq[hydra.core.Term]]](hydra.lib.logic.or(hydra.lib.lists.`null`[hydra.core.Name](schemeVars))(hydra.lib.logic.not(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Name](schemeVars))(hydra.lib.lists.length[hydra.core.Type](tApps)))))(Right(argTerms))({
        lazy val subst: Map[hydra.core.Name, hydra.core.Type] = hydra.lib.maps.fromList[hydra.core.Name,
           hydra.core.Type](hydra.lib.lists.zip[hydra.core.Name, hydra.core.Type](schemeVars)(tApps))
        {
          lazy val expectedTypes: Seq[hydra.core.Type] = hydra.java.coder.peelExpectedTypes(subst)(hydra.lib.lists.length[hydra.core.Term](argTerms))(schemeType)
          Right(hydra.lib.lists.zipWith[hydra.core.Term, hydra.core.Type, hydra.core.Term]((arg: hydra.core.Term) =>
            (mExpected: hydra.core.Type) => hydra.java.coder.propagateType(mExpected)(arg))(argTerms)(hydra.lib.lists.concat2[hydra.core.Type](expectedTypes)(hydra.lib.lists.replicate[hydra.core.Type](hydra.lib.lists.length[hydra.core.Term](argTerms))(hydra.core.Type.variable("unused")))))
        }
      })
    }
  }
})))

def applyCastIfSafe[T0](aliases: hydra.java.environment.Aliases)(castType: hydra.core.Type)(expr: hydra.java.syntax.Expression)(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.Expression] =
  {
  lazy val trusted: scala.collection.immutable.Set[hydra.core.Name] = (aliases.trustedTypeVars)
  lazy val inScope: scala.collection.immutable.Set[hydra.core.Name] = (aliases.inScopeTypeParams)
  lazy val castVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.java.coder.collectTypeVars(castType)
  lazy val javaTypeVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) =>
    hydra.lib.logic.or(hydra.lib.sets.member[hydra.core.Name](v)(inScope))(hydra.java.coder.isLambdaBoundVariable(v)))(hydra.lib.sets.toList[hydra.core.Name](castVars)))
  lazy val isSafe: Boolean = hydra.lib.logic.or(hydra.lib.sets.`null`[hydra.core.Name](trusted))(hydra.lib.logic.or(hydra.lib.sets.`null`[hydra.core.Name](javaTypeVars))(hydra.lib.sets.`null`[hydra.core.Name](hydra.lib.sets.difference[hydra.core.Name](javaTypeVars)(trusted))))
  hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.java.syntax.Expression]](isSafe)(hydra.lib.eithers.bind[hydra.errors.Error,
     hydra.java.syntax.Type, hydra.java.syntax.Expression](hydra.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(castType)(cx)(g))((jtype: hydra.java.syntax.Type) =>
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType, hydra.java.syntax.Expression](hydra.java.utils.javaTypeToJavaReferenceType(jtype)(cx))((rt: hydra.java.syntax.ReferenceType) =>
    Right(hydra.java.utils.javaCastExpressionToJavaExpression(hydra.java.utils.javaCastExpression(rt)(hydra.java.utils.javaExpressionToJavaUnaryExpression(expr)))))))(Right(expr))
}

def applyJavaArg(expr: hydra.java.syntax.Expression)(jarg: hydra.java.syntax.Expression): hydra.java.syntax.Expression =
  hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocation(Some(Right(hydra.java.utils.javaExpressionToJavaPrimary(expr))))(hydra.java.names.applyMethodName)(Seq(jarg)))

def applyOvergenSubstToTermAnnotations[T0, T1](subst: Map[hydra.core.Name, hydra.core.Type])(term0: hydra.core.Term)(cx: T0)(g: hydra.graph.Graph): Either[T1,
   hydra.core.Term] = Right(hydra.java.coder.applyOvergenSubstToTermAnnotations_go(subst)(g)(term0))

def applyOvergenSubstToTermAnnotations_go(subst: Map[hydra.core.Name, hydra.core.Type])(cx: hydra.graph.Graph)(term: hydra.core.Term): hydra.core.Term =
  term match
  case hydra.core.Term.annotated(v_Term_annotated_at) => {
    lazy val inner: hydra.core.Term = (v_Term_annotated_at.body)
    {
      lazy val ann: Map[hydra.core.Name, hydra.core.Term] = (v_Term_annotated_at.annotation)
      {
        lazy val `ann_`: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.maybes.cases[hydra.core.Term,
           Map[hydra.core.Name, hydra.core.Term]](hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Term](hydra.constants.key_type)(ann))(ann)((typeTerm: hydra.core.Term) =>
          hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Type, Map[hydra.core.Name, hydra.core.Term]]((_x: hydra.errors.DecodingError) => ann)((t: hydra.core.Type) =>
          {
          lazy val `t_`: hydra.core.Type = hydra.java.coder.substituteTypeVarsWithTypes(subst)(t)
          hydra.lib.maps.insert[hydra.core.Name, hydra.core.Term](hydra.constants.key_type)(hydra.encode.core.`type`(`t_`))(ann)
        })(hydra.decode.core.`type`(cx)(typeTerm)))
        hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.java.coder.applyOvergenSubstToTermAnnotations_go(subst)(cx)(inner), `ann_`))
      }
    }
  }
  case hydra.core.Term.application(v_Term_application_app) => hydra.core.Term.application(hydra.core.Application(hydra.java.coder.applyOvergenSubstToTermAnnotations_go(subst)(cx)(v_Term_application_app.function),
     hydra.java.coder.applyOvergenSubstToTermAnnotations_go(subst)(cx)(v_Term_application_app.argument)))
  case hydra.core.Term.lambda(v_Term_lambda_lam) => hydra.core.Term.lambda(hydra.core.Lambda(v_Term_lambda_lam.parameter,
     hydra.lib.maybes.map[hydra.core.Type, hydra.core.Type]((d: hydra.core.Type) => hydra.java.coder.substituteTypeVarsWithTypes(subst)(d))(v_Term_lambda_lam.domain),
     hydra.java.coder.applyOvergenSubstToTermAnnotations_go(subst)(cx)(v_Term_lambda_lam.body)))
  case hydra.core.Term.cases(v_Term_cases_cs) => hydra.core.Term.cases(hydra.core.CaseStatement(v_Term_cases_cs.typeName,
     hydra.lib.maybes.map[hydra.core.Term, hydra.core.Term]((d: hydra.core.Term) =>
    hydra.java.coder.applyOvergenSubstToTermAnnotations_go(subst)(cx)(d))(v_Term_cases_cs.default), hydra.lib.lists.map[hydra.core.Field,
       hydra.core.Field]((fld: hydra.core.Field) =>
    hydra.core.Field(fld.name, hydra.java.coder.applyOvergenSubstToTermAnnotations_go(subst)(cx)(fld.term)))(v_Term_cases_cs.cases)))
  case hydra.core.Term.let(v_Term_let_lt) => hydra.core.Term.let(hydra.core.Let(hydra.lib.lists.map[hydra.core.Binding,
     hydra.core.Binding]((b: hydra.core.Binding) =>
    hydra.core.Binding(b.name, hydra.java.coder.applyOvergenSubstToTermAnnotations_go(subst)(cx)(b.term),
       (b.`type`)))(v_Term_let_lt.bindings), hydra.java.coder.applyOvergenSubstToTermAnnotations_go(subst)(cx)(v_Term_let_lt.body)))
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(hydra.java.coder.applyOvergenSubstToTermAnnotations_go(subst)(cx)(v_Term_typeApplication_ta.body),
     hydra.java.coder.substituteTypeVarsWithTypes(subst)(v_Term_typeApplication_ta.`type`)))
  case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.core.Term.typeLambda(hydra.core.TypeLambda(v_Term_typeLambda_tl.parameter,
     hydra.java.coder.applyOvergenSubstToTermAnnotations_go(subst)(cx)(v_Term_typeLambda_tl.body)))
  case _ => term

def applySubstFull(s: Map[hydra.core.Name, hydra.core.Type])(t: hydra.core.Type): hydra.core.Type =
  hydra.strip.deannotateType(t) match
  case hydra.core.Type.variable(v_Type_variable_v) => hydra.lib.maps.findWithDefault[hydra.core.Type, hydra.core.Name](t)(v_Type_variable_v)(s)
  case hydra.core.Type.function(v_Type_function_ft) => hydra.core.Type.function(hydra.core.FunctionType(hydra.java.coder.applySubstFull(s)(v_Type_function_ft.domain),
     hydra.java.coder.applySubstFull(s)(v_Type_function_ft.codomain)))
  case hydra.core.Type.application(v_Type_application_at) => hydra.core.Type.application(hydra.core.ApplicationType(hydra.java.coder.applySubstFull(s)(v_Type_application_at.function),
     hydra.java.coder.applySubstFull(s)(v_Type_application_at.argument)))
  case hydra.core.Type.list(v_Type_list_inner) => hydra.core.Type.list(hydra.java.coder.applySubstFull(s)(v_Type_list_inner))
  case hydra.core.Type.set(v_Type_set_inner) => hydra.core.Type.set(hydra.java.coder.applySubstFull(s)(v_Type_set_inner))
  case hydra.core.Type.maybe(v_Type_maybe_inner) => hydra.core.Type.maybe(hydra.java.coder.applySubstFull(s)(v_Type_maybe_inner))
  case hydra.core.Type.map(v_Type_map_mt) => hydra.core.Type.map(hydra.core.MapType(hydra.java.coder.applySubstFull(s)(v_Type_map_mt.keys),
     hydra.java.coder.applySubstFull(s)(v_Type_map_mt.values)))
  case hydra.core.Type.pair(v_Type_pair_pt) => hydra.core.Type.pair(hydra.core.PairType(hydra.java.coder.applySubstFull(s)(v_Type_pair_pt.first),
     hydra.java.coder.applySubstFull(s)(v_Type_pair_pt.second)))
  case hydra.core.Type.either(v_Type_either_et) => hydra.core.Type.either(hydra.core.EitherType(hydra.java.coder.applySubstFull(s)(v_Type_either_et.left),
     hydra.java.coder.applySubstFull(s)(v_Type_either_et.right)))
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.core.Type.forall(hydra.core.ForallType(v_Type_forall_ft.parameter,
     hydra.java.coder.applySubstFull(hydra.lib.maps.delete[hydra.core.Name, hydra.core.Type](v_Type_forall_ft.parameter)(s))(v_Type_forall_ft.body)))
  case _ => t

def applySubstSimple(subst: Map[hydra.core.Name, hydra.core.Type])(t: hydra.core.Type): hydra.core.Type =
  hydra.strip.deannotateType(t) match
  case hydra.core.Type.variable(v_Type_variable_v) => hydra.lib.maps.findWithDefault[hydra.core.Type, hydra.core.Name](t)(v_Type_variable_v)(subst)
  case _ => t

def arraysCompareExpr(otherVar: scala.Predef.String)(fname: scala.Predef.String): hydra.java.syntax.Expression =
  {
  lazy val header: hydra.java.syntax.MethodInvocation_Header = hydra.java.syntax.MethodInvocation_Header.complex(hydra.java.syntax.MethodInvocation_Complex(hydra.java.syntax.MethodInvocation_Variant.`type`(hydra.java.utils.javaTypeName("java.util.Arrays")),
     Seq(), "compare"))
  lazy val arg1: hydra.java.syntax.Expression = hydra.java.utils.javaExpressionNameToJavaExpression(hydra.java.syntax.ExpressionName(None,
     hydra.java.utils.sanitizeJavaName(fname)))
  lazy val arg2: hydra.java.syntax.Expression = hydra.java.utils.javaExpressionNameToJavaExpression(hydra.java.utils.fieldExpression(hydra.java.utils.javaIdentifier(otherVar))(hydra.java.utils.javaIdentifier(fname)))
  hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.syntax.MethodInvocation(header, Seq(arg1, arg2)))
}

def arraysEqualsClause(tmpName: scala.Predef.String)(fname: scala.Predef.String): hydra.java.syntax.InclusiveOrExpression =
  {
  lazy val thisArg: hydra.java.syntax.Expression = hydra.java.utils.javaExpressionNameToJavaExpression(hydra.java.utils.fieldExpression("this")(hydra.java.utils.javaIdentifier(fname)))
  lazy val otherArg: hydra.java.syntax.Expression = hydra.java.utils.javaExpressionNameToJavaExpression(hydra.java.utils.fieldExpression(hydra.java.utils.javaIdentifier(tmpName))(hydra.java.utils.javaIdentifier(fname)))
  lazy val header: hydra.java.syntax.MethodInvocation_Header = hydra.java.syntax.MethodInvocation_Header.complex(hydra.java.syntax.MethodInvocation_Complex(hydra.java.syntax.MethodInvocation_Variant.`type`(hydra.java.utils.javaTypeName("java.util.Arrays")),
     Seq(), hydra.java.names.equalsMethodName))
  hydra.java.utils.javaPostfixExpressionToJavaInclusiveOrExpression(hydra.java.utils.javaMethodInvocationToJavaPostfixExpression(hydra.java.syntax.MethodInvocation(header,
     Seq(thisArg, otherArg))))
}

def augmentVariantClass(aliases: hydra.java.environment.Aliases)(tparams: Seq[hydra.java.syntax.TypeParameter])(elName: hydra.core.Name)(cd: hydra.java.syntax.ClassDeclaration): hydra.java.syntax.ClassDeclaration =
  cd match
  case hydra.java.syntax.ClassDeclaration.normal(v_ClassDeclaration_normal_ncd) => {
    lazy val args: Seq[hydra.java.syntax.TypeArgument] = hydra.lib.lists.map[hydra.java.syntax.TypeParameter,
       hydra.java.syntax.TypeArgument]((tp: hydra.java.syntax.TypeParameter) => hydra.java.utils.typeParameterToTypeArgument(tp))(tparams)
    {
      lazy val extendsPart: hydra.java.syntax.ClassType = hydra.java.utils.nameToJavaClassType(aliases)(true)(args)(elName)(None)
      {
        lazy val newMods: Seq[hydra.java.syntax.ClassModifier] = Seq(hydra.java.syntax.ClassModifier.public,
           hydra.java.syntax.ClassModifier.static, hydra.java.syntax.ClassModifier.`final`)
        {
          lazy val oldBody: hydra.java.syntax.ClassBody = (v_ClassDeclaration_normal_ncd.body)
          {
            lazy val oldDecls: Seq[hydra.java.syntax.ClassBodyDeclarationWithComments] = oldBody
            {
              lazy val acceptDecl: hydra.java.syntax.ClassBodyDeclarationWithComments = hydra.java.coder.noComment(hydra.java.utils.toAcceptMethod(false)(tparams))
              {
                lazy val newBody: hydra.java.syntax.ClassBody = hydra.lib.lists.concat2[hydra.java.syntax.ClassBodyDeclarationWithComments](oldDecls)(Seq(acceptDecl))
                hydra.java.syntax.ClassDeclaration.normal(hydra.java.syntax.NormalClassDeclaration(newMods,
                   (v_ClassDeclaration_normal_ncd.identifier), tparams, Some(extendsPart), (v_ClassDeclaration_normal_ncd.implements),
                   newBody))
              }
            }
          }
        }
      }
    }
  }
  case _ => cd

def bindingIsFunctionType(b: hydra.core.Binding): Boolean =
  hydra.lib.maybes.maybe[Boolean, hydra.core.TypeScheme](hydra.strip.deannotateTerm(b.term) match
  case hydra.core.Term.lambda(v_Term_lambda__f) => true
  case hydra.core.Term.project(v_Term_project__f) => true
  case hydra.core.Term.cases(v_Term_cases__f) => true
  case hydra.core.Term.unwrap(v_Term_unwrap__f) => true
  case _ => false)((ts: hydra.core.TypeScheme) =>
  hydra.strip.deannotateType(ts.`type`) match
  case hydra.core.Type.function(v_Type_function__ft) => true
  case hydra.core.Type.forall(v_Type_forall_fa) => hydra.strip.deannotateType(v_Type_forall_fa.body) match
    case hydra.core.Type.function(v_Type_function__ft2) => true
    case _ => false
  case _ => false)(b.`type`)

def bindingNameToFilePath(name: hydra.core.Name): scala.Predef.String =
  {
  lazy val qn: hydra.packaging.QualifiedName = hydra.names.qualifyName(name)
  lazy val `ns_`: Option[hydra.packaging.Namespace] = (qn.namespace)
  lazy val local: scala.Predef.String = (qn.local)
  lazy val sanitized: scala.Predef.String = hydra.formatting.sanitizeWithUnderscores(hydra.java.language.reservedWords)(local)
  lazy val unq: hydra.core.Name = hydra.names.unqualifyName(hydra.packaging.QualifiedName(`ns_`, sanitized))
  hydra.names.nameToFilePath(hydra.util.CaseConvention.camel)(hydra.util.CaseConvention.pascal)("java")(unq)
}

def bindingsToStatements(env: hydra.java.environment.JavaEnvironment)(bindings: Seq[hydra.core.Binding])(cx: hydra.context.Context)(g0: hydra.graph.Graph): Either[hydra.errors.Error,
   Tuple2[Seq[hydra.java.syntax.BlockStatement], hydra.java.environment.JavaEnvironment]] =
  {
  lazy val aliases: hydra.java.environment.Aliases = (env.aliases)
  lazy val g: hydra.graph.Graph = (env.graph)
  lazy val flatBindings: Seq[hydra.core.Binding] = hydra.java.coder.dedupBindings(aliases.inScopeJavaVars)(hydra.java.coder.flattenBindings(bindings))
  lazy val gExtended: hydra.graph.Graph = hydra.scoping.extendGraphForLet((g2: hydra.graph.Graph) =>
    (b: hydra.core.Binding) =>
    hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.predicates.isComplexBinding(g2)(b))(Some(hydra.core.Term.literal(hydra.core.Literal.boolean(true))))(None))(g)(hydra.core.Let(flatBindings,
       hydra.core.Term.variable("dummy")))
  lazy val bindingVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.lists.map[hydra.core.Binding,
     hydra.core.Name]((b: hydra.core.Binding) => (b.name))(flatBindings))
  lazy val allDeps: Map[hydra.core.Name, scala.collection.immutable.Set[hydra.core.Name]] = hydra.lib.maps.fromList[hydra.core.Name,
     scala.collection.immutable.Set[hydra.core.Name]](hydra.lib.lists.map[hydra.core.Binding, Tuple2[hydra.core.Name,
     scala.collection.immutable.Set[hydra.core.Name]]]((b: hydra.core.Binding) =>
    {
    lazy val key: hydra.core.Name = (b.name)
    {
      lazy val deps: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.intersection[hydra.core.Name](bindingVars)(hydra.variables.freeVariablesInTerm(b.term))
      Tuple2(key, deps)
    }
  })(flatBindings))
  lazy val sorted: Seq[Seq[hydra.core.Name]] = hydra.sorting.topologicalSortComponents(hydra.lib.lists.map[Tuple2[hydra.core.Name,
     scala.collection.immutable.Set[hydra.core.Name]], Tuple2[hydra.core.Name, Seq[hydra.core.Name]]]((entry: Tuple2[hydra.core.Name,
     scala.collection.immutable.Set[hydra.core.Name]]) =>
    {
    lazy val key: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, scala.collection.immutable.Set[hydra.core.Name]](entry)
    {
      lazy val deps: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.pairs.second[hydra.core.Name,
         scala.collection.immutable.Set[hydra.core.Name]](entry)
      Tuple2(key, hydra.lib.sets.toList[hydra.core.Name](deps))
    }
  })(hydra.lib.maps.toList[hydra.core.Name, scala.collection.immutable.Set[hydra.core.Name]](allDeps)))
  lazy val recursiveVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.lists.concat[hydra.core.Name](hydra.lib.lists.map[Seq[hydra.core.Name],
     Seq[hydra.core.Name]]((names: Seq[hydra.core.Name]) =>
    hydra.lib.logic.ifElse[Seq[hydra.core.Name]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Name](names))(1))({
    lazy val singleName: hydra.core.Name = hydra.lib.lists.head[hydra.core.Name](names)
    hydra.lib.maybes.cases[scala.collection.immutable.Set[hydra.core.Name], Seq[hydra.core.Name]](hydra.lib.maps.lookup[hydra.core.Name,
       scala.collection.immutable.Set[hydra.core.Name]](singleName)(allDeps))(Seq())((deps: scala.collection.immutable.Set[hydra.core.Name]) =>
      hydra.lib.logic.ifElse[Seq[hydra.core.Name]](hydra.lib.sets.member[hydra.core.Name](singleName)(deps))(Seq(singleName))(Seq()))
  })(names))(sorted)))
  lazy val thunkedVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.lists.concat[hydra.core.Name](hydra.lib.lists.map[hydra.core.Binding,
     Seq[hydra.core.Name]]((b: hydra.core.Binding) =>
    {
    lazy val bname: hydra.core.Name = (b.name)
    hydra.lib.logic.ifElse[Seq[hydra.core.Name]](hydra.lib.logic.and(hydra.lib.logic.not(hydra.lib.sets.member[hydra.core.Name](bname)(recursiveVars)))(hydra.lib.logic.and(hydra.java.coder.needsThunking(b.term))(hydra.lib.logic.not(hydra.java.coder.bindingIsFunctionType(b)))))(Seq(bname))(Seq())
  })(flatBindings)))
  lazy val aliasesExtended: hydra.java.environment.Aliases = hydra.java.environment.Aliases(aliases.currentNamespace,
     (aliases.packages), (aliases.branchVars), hydra.lib.sets.union[hydra.core.Name](aliases.recursiveVars)(recursiveVars),
     (aliases.inScopeTypeParams), (aliases.polymorphicLocals), hydra.lib.sets.union[hydra.core.Name](aliases.inScopeJavaVars)(bindingVars),
     (aliases.varRenames), (aliases.lambdaVars), (aliases.typeVarSubst), (aliases.trustedTypeVars), (aliases.methodCodomain),
     hydra.lib.sets.union[hydra.core.Name](aliases.thunkedVars)(thunkedVars))
  lazy val envExtended: hydra.java.environment.JavaEnvironment = hydra.java.environment.JavaEnvironment(aliasesExtended, gExtended)
  hydra.lib.logic.ifElse[Either[hydra.errors.Error, Tuple2[Seq[hydra.java.syntax.BlockStatement], hydra.java.environment.JavaEnvironment]]](hydra.lib.lists.`null`[hydra.core.Binding](bindings))(Right(Tuple2(Seq(),
     envExtended)))(hydra.lib.eithers.bind[hydra.errors.Error, Seq[Seq[hydra.java.syntax.BlockStatement]],
     Tuple2[Seq[hydra.java.syntax.BlockStatement], hydra.java.environment.JavaEnvironment]](hydra.lib.eithers.mapList[Seq[hydra.core.Name],
     Seq[hydra.java.syntax.BlockStatement], hydra.errors.Error]((names: Seq[hydra.core.Name]) =>
    hydra.lib.eithers.bind[hydra.errors.Error, Seq[Option[hydra.java.syntax.BlockStatement]], Seq[hydra.java.syntax.BlockStatement]](hydra.lib.eithers.mapList[hydra.core.Name,
       Option[hydra.java.syntax.BlockStatement], hydra.errors.Error]((n: hydra.core.Name) =>
    hydra.java.coder.toDeclInit(aliasesExtended)(gExtended)(recursiveVars)(flatBindings)(n)(cx)(g))(names))((inits: Seq[Option[hydra.java.syntax.BlockStatement]]) =>
    hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.BlockStatement], Seq[hydra.java.syntax.BlockStatement]](hydra.lib.eithers.mapList[hydra.core.Name,
       hydra.java.syntax.BlockStatement, hydra.errors.Error]((n: hydra.core.Name) =>
    hydra.java.coder.toDeclStatement(envExtended)(aliasesExtended)(gExtended)(recursiveVars)(thunkedVars)(flatBindings)(n)(cx)(g))(names))((decls: Seq[hydra.java.syntax.BlockStatement]) =>
    Right(hydra.lib.lists.concat2[hydra.java.syntax.BlockStatement](hydra.lib.maybes.cat[hydra.java.syntax.BlockStatement](inits))(decls)))))(sorted))((groups: Seq[Seq[hydra.java.syntax.BlockStatement]]) =>
    Right(Tuple2(hydra.lib.lists.concat[hydra.java.syntax.BlockStatement](groups), envExtended))))
}

def boundTypeVariables(typ: hydra.core.Type): Seq[hydra.core.Name] =
  typ match
  case hydra.core.Type.annotated(v_Type_annotated_at) => hydra.java.coder.boundTypeVariables(v_Type_annotated_at.body)
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.lib.lists.cons[hydra.core.Name](v_Type_forall_ft.parameter)(hydra.java.coder.boundTypeVariables(v_Type_forall_ft.body))
  case _ => Seq()

def buildArgSubst[T0](schemeVarSet: scala.collection.immutable.Set[hydra.core.Name])(schemeDoms: Seq[hydra.core.Type])(argTypes: Seq[T0]): Map[hydra.core.Name,
   T0] =
  hydra.lib.maps.fromList[hydra.core.Name, T0](hydra.lib.lists.bind[Tuple2[hydra.core.Type, T0], Tuple2[hydra.core.Name,
     T0]](hydra.lib.lists.zip[hydra.core.Type, T0](schemeDoms)(argTypes))((p: Tuple2[hydra.core.Type,
     T0]) =>
  {
  lazy val sdom: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, T0](p)
  {
    lazy val argType: T0 = hydra.lib.pairs.second[hydra.core.Type, T0](p)
    hydra.strip.deannotateType(sdom) match
      case hydra.core.Type.variable(v_Type_variable_v) => hydra.lib.logic.ifElse[Seq[Tuple2[hydra.core.Name,
         T0]]](hydra.lib.sets.member[hydra.core.Name](v_Type_variable_v)(schemeVarSet))(Seq(Tuple2(v_Type_variable_v,
         argType)))(Seq())
      case _ => Seq()
  }
}))

def buildCurriedLambda(params: Seq[hydra.core.Name])(inner: hydra.java.syntax.Expression): hydra.java.syntax.Expression =
  hydra.lib.lists.foldl[hydra.java.syntax.Expression, hydra.core.Name]((acc: hydra.java.syntax.Expression) => (p: hydra.core.Name) => hydra.java.utils.javaLambda(p)(acc))(inner)(hydra.lib.lists.reverse[hydra.core.Name](params))

def buildSubstFromAnnotations[T0, T1](schemeVarSet: scala.collection.immutable.Set[hydra.core.Name])(term: hydra.core.Term)(cx: T0)(g: hydra.graph.Graph): Either[T1,
   Map[hydra.core.Name, hydra.core.Name]] = Right(hydra.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(term))

def buildSubstFromAnnotations_go(schemeVarSet: scala.collection.immutable.Set[hydra.core.Name])(g: hydra.graph.Graph)(term: hydra.core.Term): Map[hydra.core.Name,
   hydra.core.Name] =
  term match
  case hydra.core.Term.annotated(v_Term_annotated_at) => {
    lazy val body: hydra.core.Term = (v_Term_annotated_at.body)
    {
      lazy val anns: Map[hydra.core.Name, hydra.core.Term] = (v_Term_annotated_at.annotation)
      {
        lazy val bodySubst: Map[hydra.core.Name, hydra.core.Name] = hydra.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(body)
        {
          lazy val annSubst: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.maybes.cases[hydra.core.Term,
             Map[hydra.core.Name, hydra.core.Name]](hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Term](hydra.constants.key_type)(anns))(hydra.lib.maps.empty[hydra.core.Name,
             hydra.core.Name])((typeTerm: hydra.core.Term) =>
            hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Type, Map[hydra.core.Name,
               hydra.core.Name]]((_x: hydra.errors.DecodingError) => hydra.lib.maps.empty[hydra.core.Name,
               hydra.core.Name])((annType: hydra.core.Type) =>
            hydra.strip.deannotateTerm(body) match
            case hydra.core.Term.lambda(v_Term_lambda_lam) => hydra.lib.maybes.cases[hydra.core.Type,
               Map[hydra.core.Name, hydra.core.Name]](v_Term_lambda_lam.domain)(hydra.lib.maps.empty[hydra.core.Name,
               hydra.core.Name])((dom: hydra.core.Type) =>
              hydra.strip.deannotateType(annType) match
              case hydra.core.Type.function(v_Type_function_ft) => hydra.java.coder.buildTypeVarSubst(schemeVarSet)(v_Type_function_ft.domain)(dom)
              case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name])
            case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name])(hydra.decode.core.`type`(g)(typeTerm)))
          hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](annSubst)(bodySubst)
        }
      }
    }
  }
  case hydra.core.Term.application(v_Term_application_app) => hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](hydra.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(v_Term_application_app.function))(hydra.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(v_Term_application_app.argument))
  case hydra.core.Term.lambda(v_Term_lambda_lam) => hydra.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(v_Term_lambda_lam.body)
  case hydra.core.Term.cases(v_Term_cases_cs) => {
    lazy val defSubst: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.maybes.cases[hydra.core.Term,
       Map[hydra.core.Name, hydra.core.Name]](v_Term_cases_cs.default)(hydra.lib.maps.empty[hydra.core.Name,
       hydra.core.Name])((d: hydra.core.Term) =>
      hydra.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(d))
    {
      lazy val caseSubsts: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.lists.foldl[Map[hydra.core.Name,
         hydra.core.Name], hydra.core.Field]((acc: Map[hydra.core.Name, hydra.core.Name]) =>
        (fld: hydra.core.Field) =>
        hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](acc)(hydra.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(fld.term)))(hydra.lib.maps.empty[hydra.core.Name,
           hydra.core.Name])(v_Term_cases_cs.cases)
      hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](defSubst)(caseSubsts)
    }
  }
  case hydra.core.Term.let(v_Term_let_lt) => {
    lazy val bindingSubst: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.lists.foldl[Map[hydra.core.Name,
       hydra.core.Name], hydra.core.Binding]((acc: Map[hydra.core.Name, hydra.core.Name]) =>
      (b: hydra.core.Binding) =>
      hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](acc)(hydra.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(b.term)))(hydra.lib.maps.empty[hydra.core.Name,
         hydra.core.Name])(v_Term_let_lt.bindings)
    hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](bindingSubst)(hydra.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(v_Term_let_lt.body))
  }
  case hydra.core.Term.list(v_Term_list_terms) => hydra.lib.lists.foldl[Map[hydra.core.Name, hydra.core.Name],
     hydra.core.Term]((acc: Map[hydra.core.Name, hydra.core.Name]) =>
    (t: hydra.core.Term) =>
    hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](acc)(hydra.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(t)))(hydra.lib.maps.empty[hydra.core.Name,
       hydra.core.Name])(v_Term_list_terms)
  case hydra.core.Term.maybe(v_Term_maybe_mt) => hydra.lib.maybes.cases[hydra.core.Term, Map[hydra.core.Name,
     hydra.core.Name]](v_Term_maybe_mt)(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name])((t: hydra.core.Term) =>
    hydra.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(t))
  case hydra.core.Term.pair(v_Term_pair_p) => hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](hydra.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(hydra.lib.pairs.first[hydra.core.Term,
     hydra.core.Term](v_Term_pair_p)))(hydra.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(hydra.lib.pairs.second[hydra.core.Term,
     hydra.core.Term](v_Term_pair_p)))
  case hydra.core.Term.record(v_Term_record_r) => hydra.lib.lists.foldl[Map[hydra.core.Name, hydra.core.Name],
     hydra.core.Field]((acc: Map[hydra.core.Name, hydra.core.Name]) =>
    (fld: hydra.core.Field) =>
    hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](acc)(hydra.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(fld.term)))(hydra.lib.maps.empty[hydra.core.Name,
       hydra.core.Name])(v_Term_record_r.fields)
  case hydra.core.Term.set(v_Term_set_terms) => hydra.lib.lists.foldl[Map[hydra.core.Name, hydra.core.Name],
     hydra.core.Term]((acc: Map[hydra.core.Name, hydra.core.Name]) =>
    (t: hydra.core.Term) =>
    hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](acc)(hydra.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(t)))(hydra.lib.maps.empty[hydra.core.Name,
       hydra.core.Name])(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_terms))
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => hydra.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(v_Term_typeApplication_ta.body)
  case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(v_Term_typeLambda_tl.body)
  case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.either[hydra.core.Term, hydra.core.Term,
     Map[hydra.core.Name, hydra.core.Name]]((t: hydra.core.Term) =>
    hydra.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(t))((t: hydra.core.Term) =>
    hydra.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(t))(v_Term_either_e)
  case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name]

def buildTypeSubst(schemeVarSet: scala.collection.immutable.Set[hydra.core.Name])(schemeType: hydra.core.Type)(actualType: hydra.core.Type): Map[hydra.core.Name,
   hydra.core.Type] =
  hydra.java.coder.buildTypeSubst_go(schemeVarSet)(hydra.strip.deannotateType(schemeType))(hydra.strip.deannotateType(actualType))

def buildTypeSubst_go(svs: scala.collection.immutable.Set[hydra.core.Name])(st: hydra.core.Type)(at: hydra.core.Type): Map[hydra.core.Name, hydra.core.Type] =
  {
  def goSub(a: hydra.core.Type)(b: hydra.core.Type): Map[hydra.core.Name, hydra.core.Type] =
    hydra.java.coder.buildTypeSubst_go(svs)(hydra.strip.deannotateType(a))(hydra.strip.deannotateType(b))
  st match
    case hydra.core.Type.variable(v_Type_variable_v) => hydra.lib.logic.ifElse[Map[hydra.core.Name, hydra.core.Type]](hydra.lib.sets.member[hydra.core.Name](v_Type_variable_v)(svs))(hydra.lib.maps.singleton[hydra.core.Name,
       hydra.core.Type](v_Type_variable_v)(at))(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Type])
    case hydra.core.Type.function(v_Type_function_sft) => at match
      case hydra.core.Type.function(v_Type_function_aft) => hydra.lib.maps.union[hydra.core.Name, hydra.core.Type](goSub(v_Type_function_sft.domain)(v_Type_function_aft.domain))(goSub(v_Type_function_sft.codomain)(v_Type_function_aft.codomain))
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Type]
    case hydra.core.Type.application(v_Type_application_sat) => at match
      case hydra.core.Type.application(v_Type_application_aat) => hydra.lib.maps.union[hydra.core.Name,
         hydra.core.Type](goSub(v_Type_application_sat.function)(v_Type_application_aat.function))(goSub(v_Type_application_sat.argument)(v_Type_application_aat.argument))
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Type]
    case hydra.core.Type.list(v_Type_list_sl) => at match
      case hydra.core.Type.list(v_Type_list_al) => goSub(v_Type_list_sl)(v_Type_list_al)
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Type]
    case hydra.core.Type.set(v_Type_set_ss) => at match
      case hydra.core.Type.set(v_Type_set_as_) => goSub(v_Type_set_ss)(`v_Type_set_as_`)
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Type]
    case hydra.core.Type.maybe(v_Type_maybe_sm) => at match
      case hydra.core.Type.maybe(v_Type_maybe_am) => goSub(v_Type_maybe_sm)(v_Type_maybe_am)
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Type]
    case hydra.core.Type.map(v_Type_map_smt) => at match
      case hydra.core.Type.map(v_Type_map_amt) => hydra.lib.maps.union[hydra.core.Name, hydra.core.Type](goSub(v_Type_map_smt.keys)(v_Type_map_amt.keys))(goSub(v_Type_map_smt.values)(v_Type_map_amt.values))
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Type]
    case hydra.core.Type.pair(v_Type_pair_spt) => at match
      case hydra.core.Type.pair(v_Type_pair_apt) => hydra.lib.maps.union[hydra.core.Name, hydra.core.Type](goSub(v_Type_pair_spt.first)(v_Type_pair_apt.first))(goSub(v_Type_pair_spt.second)(v_Type_pair_apt.second))
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Type]
    case hydra.core.Type.either(v_Type_either_set_) => at match
      case hydra.core.Type.either(v_Type_either_aet) => hydra.lib.maps.union[hydra.core.Name, hydra.core.Type](goSub(`v_Type_either_set_`.left)(v_Type_either_aet.left))(goSub(`v_Type_either_set_`.right)(v_Type_either_aet.right))
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Type]
    case hydra.core.Type.forall(v_Type_forall_sfa) => at match
      case hydra.core.Type.forall(v_Type_forall_afa) => goSub(v_Type_forall_sfa.body)(v_Type_forall_afa.body)
      case _ => goSub(v_Type_forall_sfa.body)(at)
    case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Type]
}

def buildTypeVarSubst(schemeVarSet: scala.collection.immutable.Set[hydra.core.Name])(freshTyp: hydra.core.Type)(canonTyp: hydra.core.Type): Map[hydra.core.Name,
   hydra.core.Name] =
  hydra.java.coder.buildTypeVarSubst_go(schemeVarSet)(hydra.strip.deannotateType(freshTyp))(hydra.strip.deannotateType(canonTyp))

def buildTypeVarSubst_go(svs: scala.collection.immutable.Set[hydra.core.Name])(ft: hydra.core.Type)(ct: hydra.core.Type): Map[hydra.core.Name,
   hydra.core.Name] =
  {
  def goSub(a: hydra.core.Type)(b: hydra.core.Type): Map[hydra.core.Name, hydra.core.Name] =
    hydra.java.coder.buildTypeVarSubst_go(svs)(hydra.strip.deannotateType(a))(hydra.strip.deannotateType(b))
  ft match
    case hydra.core.Type.variable(v_Type_variable_fn) => ct match
      case hydra.core.Type.variable(v_Type_variable_cn) => hydra.lib.logic.ifElse[Map[hydra.core.Name,
         hydra.core.Name]](hydra.lib.logic.and(hydra.lib.logic.not(hydra.lib.equality.equal[hydra.core.Name](v_Type_variable_fn)(v_Type_variable_cn)))(hydra.lib.sets.member[hydra.core.Name](v_Type_variable_cn)(svs)))(hydra.lib.maps.singleton[hydra.core.Name,
         hydra.core.Name](v_Type_variable_fn)(v_Type_variable_cn))(hydra.lib.maps.empty[hydra.core.Name,
         hydra.core.Name])
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name]
    case hydra.core.Type.function(v_Type_function_fft) => ct match
      case hydra.core.Type.function(v_Type_function_cft) => hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](goSub(v_Type_function_fft.domain)(v_Type_function_cft.domain))(goSub(v_Type_function_fft.codomain)(v_Type_function_cft.codomain))
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name]
    case hydra.core.Type.application(v_Type_application_fat) => ct match
      case hydra.core.Type.application(v_Type_application_cat) => hydra.lib.maps.union[hydra.core.Name,
         hydra.core.Name](goSub(v_Type_application_fat.function)(v_Type_application_cat.function))(goSub(v_Type_application_fat.argument)(v_Type_application_cat.argument))
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name]
    case hydra.core.Type.list(v_Type_list_fl) => ct match
      case hydra.core.Type.list(v_Type_list_cl) => goSub(v_Type_list_fl)(v_Type_list_cl)
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name]
    case hydra.core.Type.set(v_Type_set_fs) => ct match
      case hydra.core.Type.set(v_Type_set_cs) => goSub(v_Type_set_fs)(v_Type_set_cs)
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name]
    case hydra.core.Type.maybe(v_Type_maybe_fm) => ct match
      case hydra.core.Type.maybe(v_Type_maybe_cm) => goSub(v_Type_maybe_fm)(v_Type_maybe_cm)
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name]
    case hydra.core.Type.map(v_Type_map_fmt) => ct match
      case hydra.core.Type.map(v_Type_map_cmt) => hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](goSub(v_Type_map_fmt.keys)(v_Type_map_cmt.keys))(goSub(v_Type_map_fmt.values)(v_Type_map_cmt.values))
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name]
    case hydra.core.Type.pair(v_Type_pair_fpt) => ct match
      case hydra.core.Type.pair(v_Type_pair_cpt) => hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](goSub(v_Type_pair_fpt.first)(v_Type_pair_cpt.first))(goSub(v_Type_pair_fpt.second)(v_Type_pair_cpt.second))
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name]
    case hydra.core.Type.either(v_Type_either_fet) => ct match
      case hydra.core.Type.either(v_Type_either_cet) => hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](goSub(v_Type_either_fet.left)(v_Type_either_cet.left))(goSub(v_Type_either_fet.right)(v_Type_either_cet.right))
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name]
    case hydra.core.Type.forall(v_Type_forall_ffa) => ct match
      case hydra.core.Type.forall(v_Type_forall_cfa) => goSub(v_Type_forall_ffa.body)(v_Type_forall_cfa.body)
      case _ => hydra.java.coder.buildTypeVarSubst_go(svs)(hydra.strip.deannotateType(v_Type_forall_ffa.body))(ct)
    case _ => ct match
      case hydra.core.Type.forall(v_Type_forall_cfa) => hydra.java.coder.buildTypeVarSubst_go(svs)(ft)(hydra.strip.deannotateType(v_Type_forall_cfa.body))
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name]
}

lazy val classModsPublic: Seq[hydra.java.syntax.ClassModifier] = Seq(hydra.java.syntax.ClassModifier.public)

def classifyDataReference[T0](name: hydra.core.Name)(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error, hydra.java.environment.JavaSymbolClass] =
  hydra.lib.eithers.bind[hydra.errors.Error, Option[hydra.core.Binding], hydra.java.environment.JavaSymbolClass](Right(hydra.lexical.lookupBinding(g)(name)))((mel: Option[hydra.core.Binding]) =>
  hydra.lib.maybes.cases[hydra.core.Binding, Either[hydra.errors.Error, hydra.java.environment.JavaSymbolClass]](mel)(Right(hydra.java.environment.JavaSymbolClass.localVariable))((el: hydra.core.Binding) =>
  hydra.lib.maybes.cases[hydra.core.TypeScheme, Either[hydra.errors.Error, hydra.java.environment.JavaSymbolClass]](el.`type`)(Left(hydra.errors.Error.other(hydra.lib.strings.cat2("no type scheme for element ")(el.name))))((ts: hydra.core.TypeScheme) => Right(hydra.java.coder.classifyDataTerm(ts)(el.term)))))

def classifyDataTerm(ts: hydra.core.TypeScheme)(term: hydra.core.Term): hydra.java.environment.JavaSymbolClass =
  hydra.lib.logic.ifElse[hydra.java.environment.JavaSymbolClass](hydra.dependencies.isLambda(term))({
  lazy val n: Int = hydra.java.coder.classifyDataTerm_countLambdaParams(term)
  hydra.lib.logic.ifElse[hydra.java.environment.JavaSymbolClass](hydra.lib.equality.gt[Int](n)(1))(hydra.java.environment.JavaSymbolClass.hoistedLambda(n))(hydra.java.environment.JavaSymbolClass.unaryFunction)
})({
  lazy val hasTypeParams: Boolean = hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.core.Name](ts.variables))
  hydra.lib.logic.ifElse[hydra.java.environment.JavaSymbolClass](hasTypeParams)({
    lazy val n2: Int = hydra.java.coder.classifyDataTerm_countLambdaParams(hydra.java.coder.classifyDataTerm_stripTypeLambdas(term))
    hydra.lib.logic.ifElse[hydra.java.environment.JavaSymbolClass](hydra.lib.equality.gt[Int](n2)(0))(hydra.java.environment.JavaSymbolClass.hoistedLambda(n2))(hydra.java.environment.JavaSymbolClass.nullaryFunction)
  })(hydra.java.environment.JavaSymbolClass.nullaryFunction)
})

def classifyDataTerm_countLambdaParams(t: hydra.core.Term): Int =
  hydra.strip.deannotateTerm(t) match
  case hydra.core.Term.lambda(v_Term_lambda_lam) => hydra.lib.math.add(1)(hydra.java.coder.classifyDataTerm_countLambdaParams(v_Term_lambda_lam.body))
  case hydra.core.Term.let(v_Term_let_lt) => hydra.java.coder.classifyDataTerm_countLambdaParams(v_Term_let_lt.body)
  case _ => 0

def classifyDataTerm_stripTypeLambdas(t: hydra.core.Term): hydra.core.Term =
  hydra.strip.deannotateTerm(t) match
  case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.java.coder.classifyDataTerm_stripTypeLambdas(v_Term_typeLambda_tl.body)
  case _ => t

def cmpDeclStatement[T0](aliases: T0): hydra.java.syntax.BlockStatement =
  hydra.java.utils.variableDeclarationStatement(aliases)(hydra.java.utils.javaIntType)(hydra.java.utils.javaIdentifier("cmp"))(hydra.java.utils.javaIntExpression(BigInt("0")))

lazy val cmpNotZeroExpr: hydra.java.syntax.Expression = {
  lazy val lhs: hydra.java.syntax.EqualityExpression = hydra.java.utils.javaRelationalExpressionToJavaEqualityExpression(hydra.java.utils.javaPostfixExpressionToJavaRelationalExpression(hydra.java.syntax.PostfixExpression.name(hydra.java.syntax.ExpressionName(None,
     hydra.java.utils.javaIdentifier("cmp")))))
  lazy val rhs: hydra.java.syntax.RelationalExpression = hydra.java.utils.javaPostfixExpressionToJavaRelationalExpression(hydra.java.syntax.PostfixExpression.primary(hydra.java.utils.javaLiteralToJavaPrimary(hydra.java.utils.javaInt(BigInt("0")))))
  hydra.java.utils.javaEqualityExpressionToJavaExpression(hydra.java.syntax.EqualityExpression.notEqual(hydra.java.syntax.EqualityExpression_Binary(lhs, rhs)))
}

def collectForallParams(t: hydra.core.Type): Seq[hydra.core.Name] =
  hydra.strip.deannotateType(t) match
  case hydra.core.Type.forall(v_Type_forall_fa) => hydra.lib.lists.cons[hydra.core.Name](v_Type_forall_fa.parameter)(hydra.java.coder.collectForallParams(v_Type_forall_fa.body))
  case _ => Seq()

def collectLambdaDomains(t: hydra.core.Term): Tuple2[Seq[hydra.core.Type], hydra.core.Term] =
  hydra.strip.deannotateTerm(t) match
  case hydra.core.Term.lambda(v_Term_lambda_lam) => hydra.lib.maybes.cases[hydra.core.Type, Tuple2[Seq[hydra.core.Type],
     hydra.core.Term]](v_Term_lambda_lam.domain)(Tuple2(Seq(), t))((dom: hydra.core.Type) =>
    {
    lazy val rest: Tuple2[Seq[hydra.core.Type], hydra.core.Term] = hydra.java.coder.collectLambdaDomains(v_Term_lambda_lam.body)
    Tuple2(hydra.lib.lists.cons[hydra.core.Type](dom)(hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.core.Term](rest)),
       hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.core.Term](rest))
  })
  case _ => Tuple2(Seq(), t)

def collectTypeApps(t: hydra.core.Term)(acc: Seq[hydra.core.Type]): Tuple2[hydra.core.Term, Seq[hydra.core.Type]] =
  hydra.strip.deannotateTerm(t) match
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => hydra.java.coder.collectTypeApps(v_Term_typeApplication_ta.body)(hydra.lib.lists.cons[hydra.core.Type](v_Term_typeApplication_ta.`type`)(acc))
  case _ => Tuple2(hydra.strip.deannotateTerm(t), acc)

def collectTypeApps0(t: hydra.core.Term)(acc: Seq[hydra.core.Type]): Tuple2[hydra.core.Term, Seq[hydra.core.Type]] =
  hydra.strip.deannotateTerm(t) match
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => hydra.java.coder.collectTypeApps0(v_Term_typeApplication_ta.body)(hydra.lib.lists.cons[hydra.core.Type](v_Term_typeApplication_ta.`type`)(acc))
  case _ => Tuple2(t, acc)

def collectTypeVars(typ: hydra.core.Type): scala.collection.immutable.Set[hydra.core.Name] = hydra.java.coder.collectTypeVars_go(hydra.strip.deannotateType(typ))

def collectTypeVars_go(t: hydra.core.Type): scala.collection.immutable.Set[hydra.core.Name] =
  t match
  case hydra.core.Type.variable(v_Type_variable_name) => hydra.lib.sets.singleton[hydra.core.Name](v_Type_variable_name)
  case hydra.core.Type.function(v_Type_function_ft) => hydra.lib.sets.union[hydra.core.Name](hydra.java.coder.collectTypeVars_go(hydra.strip.deannotateType(v_Type_function_ft.domain)))(hydra.java.coder.collectTypeVars_go(hydra.strip.deannotateType(v_Type_function_ft.codomain)))
  case hydra.core.Type.application(v_Type_application_at) => hydra.lib.sets.union[hydra.core.Name](hydra.java.coder.collectTypeVars_go(hydra.strip.deannotateType(v_Type_application_at.function)))(hydra.java.coder.collectTypeVars_go(hydra.strip.deannotateType(v_Type_application_at.argument)))
  case hydra.core.Type.list(v_Type_list_inner) => hydra.java.coder.collectTypeVars_go(hydra.strip.deannotateType(v_Type_list_inner))
  case hydra.core.Type.set(v_Type_set_inner) => hydra.java.coder.collectTypeVars_go(hydra.strip.deannotateType(v_Type_set_inner))
  case hydra.core.Type.maybe(v_Type_maybe_inner) => hydra.java.coder.collectTypeVars_go(hydra.strip.deannotateType(v_Type_maybe_inner))
  case hydra.core.Type.map(v_Type_map_mt) => hydra.lib.sets.union[hydra.core.Name](hydra.java.coder.collectTypeVars_go(hydra.strip.deannotateType(v_Type_map_mt.keys)))(hydra.java.coder.collectTypeVars_go(hydra.strip.deannotateType(v_Type_map_mt.values)))
  case hydra.core.Type.pair(v_Type_pair_pt) => hydra.lib.sets.union[hydra.core.Name](hydra.java.coder.collectTypeVars_go(hydra.strip.deannotateType(v_Type_pair_pt.first)))(hydra.java.coder.collectTypeVars_go(hydra.strip.deannotateType(v_Type_pair_pt.second)))
  case hydra.core.Type.either(v_Type_either_et) => hydra.lib.sets.union[hydra.core.Name](hydra.java.coder.collectTypeVars_go(hydra.strip.deannotateType(v_Type_either_et.left)))(hydra.java.coder.collectTypeVars_go(hydra.strip.deannotateType(v_Type_either_et.right)))
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.java.coder.collectTypeVars_go(hydra.strip.deannotateType(v_Type_forall_ft.body))
  case _ => hydra.lib.sets.empty[hydra.core.Name]

def comparableCompareExpr(otherVar: scala.Predef.String)(fname: scala.Predef.String): hydra.java.syntax.Expression =
  {
  lazy val thisField: hydra.java.syntax.Expression = hydra.java.utils.javaIdentifierToJavaExpression(hydra.java.utils.sanitizeJavaName(fname))
  lazy val otherField: hydra.java.syntax.Expression = hydra.java.utils.javaExpressionNameToJavaExpression(hydra.java.utils.fieldExpression(hydra.java.utils.javaIdentifier(otherVar))(hydra.java.utils.javaIdentifier(fname)))
  hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocationStatic("hydra.util.Comparing")("compare")(Seq(thisField, otherField)))
}

def compareAndReturnStmts(otherVar: scala.Predef.String)(f: hydra.core.FieldType): Seq[hydra.java.syntax.BlockStatement] =
  Seq(hydra.java.syntax.BlockStatement.statement(hydra.java.utils.javaAssignmentStatement(hydra.java.syntax.LeftHandSide.expressionName(hydra.java.syntax.ExpressionName(None,
     hydra.java.utils.javaIdentifier("cmp"))))(hydra.java.coder.compareFieldExpr(otherVar)(f))), hydra.java.syntax.BlockStatement.statement(hydra.java.syntax.Statement.ifThen(hydra.java.syntax.IfThenStatement(hydra.java.coder.cmpNotZeroExpr,
     hydra.java.utils.javaReturnStatement(Some(hydra.java.utils.javaExpressionNameToJavaExpression(hydra.java.syntax.ExpressionName(None,
     hydra.java.utils.javaIdentifier("cmp")))))))))

def compareFieldExpr(otherVar: scala.Predef.String)(ft: hydra.core.FieldType): hydra.java.syntax.Expression =
  {
  lazy val fname: scala.Predef.String = (ft.name)
  lazy val ftype: hydra.core.Type = (ft.`type`)
  hydra.lib.logic.ifElse[hydra.java.syntax.Expression](hydra.java.coder.isBinaryType(ftype))(hydra.java.coder.arraysCompareExpr(otherVar)(fname))(hydra.lib.logic.ifElse[hydra.java.syntax.Expression](hydra.java.coder.isNonComparableType(ftype))(hydra.java.coder.hashCodeCompareExpr(otherVar)(fname))(hydra.java.coder.comparableCompareExpr(otherVar)(fname)))
}

def compareToBody[T0](aliases: T0)(otherVar: scala.Predef.String)(fields: Seq[hydra.core.FieldType]): Seq[hydra.java.syntax.BlockStatement] =
  hydra.lib.logic.ifElse[Seq[hydra.java.syntax.BlockStatement]](hydra.lib.lists.`null`[hydra.core.FieldType](fields))(Seq(hydra.java.syntax.BlockStatement.statement(hydra.java.utils.javaReturnStatement(Some(hydra.java.utils.javaIntExpression(BigInt("0")))))))(hydra.lib.logic.ifElse[Seq[hydra.java.syntax.BlockStatement]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.FieldType](fields))(1))(Seq(hydra.java.syntax.BlockStatement.statement(hydra.java.utils.javaReturnStatement(Some(hydra.java.coder.compareFieldExpr(otherVar)(hydra.lib.lists.head[hydra.core.FieldType](fields)))))))(hydra.lib.lists.concat2[hydra.java.syntax.BlockStatement](Seq(hydra.java.coder.cmpDeclStatement(aliases)))(hydra.lib.lists.concat2[hydra.java.syntax.BlockStatement](hydra.lib.lists.concat[hydra.java.syntax.BlockStatement](hydra.lib.lists.map[hydra.core.FieldType,
     Seq[hydra.java.syntax.BlockStatement]]((f: hydra.core.FieldType) => hydra.java.coder.compareAndReturnStmts(otherVar)(f))(hydra.lib.lists.init[hydra.core.FieldType](fields))))(Seq(hydra.java.syntax.BlockStatement.statement(hydra.java.utils.javaReturnStatement(Some(hydra.java.coder.compareFieldExpr(otherVar)(hydra.lib.lists.last[hydra.core.FieldType](fields))))))))))

def compareToZeroClause(tmpName: scala.Predef.String)(fname: scala.Predef.String): hydra.java.syntax.InclusiveOrExpression =
  {
  lazy val compareToArg: hydra.java.syntax.Expression = hydra.java.utils.javaExpressionNameToJavaExpression(hydra.java.utils.fieldExpression(hydra.java.utils.javaIdentifier(tmpName))(hydra.java.utils.javaIdentifier(fname)))
  lazy val compareToVar: hydra.java.syntax.MethodInvocation_Variant = hydra.java.syntax.MethodInvocation_Variant.expression(hydra.java.utils.fieldExpression("this")(hydra.java.utils.javaIdentifier(fname)))
  lazy val compareToHeader: hydra.java.syntax.MethodInvocation_Header = hydra.java.syntax.MethodInvocation_Header.complex(hydra.java.syntax.MethodInvocation_Complex(compareToVar,
     Seq(), hydra.java.names.compareToMethodName))
  lazy val lhs: hydra.java.syntax.EqualityExpression = hydra.java.utils.javaRelationalExpressionToJavaEqualityExpression(hydra.java.utils.javaPostfixExpressionToJavaRelationalExpression(hydra.java.utils.javaMethodInvocationToJavaPostfixExpression(hydra.java.syntax.MethodInvocation(compareToHeader,
     Seq(compareToArg)))))
  lazy val rhs: hydra.java.syntax.RelationalExpression = hydra.java.utils.javaPostfixExpressionToJavaRelationalExpression(hydra.java.syntax.PostfixExpression.primary(hydra.java.utils.javaLiteralToJavaPrimary(hydra.java.utils.javaInt(BigInt("0")))))
  hydra.java.utils.javaEqualityExpressionToJavaInclusiveOrExpression(hydra.java.syntax.EqualityExpression.equal(hydra.java.syntax.EqualityExpression_Binary(lhs,
     rhs)))
}

def constantDecl(javaName: scala.Predef.String)(aliases: hydra.java.environment.Aliases)(name: hydra.core.Name)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.ClassBodyDeclarationWithComments] =
  {
  lazy val mods: Seq[hydra.java.syntax.FieldModifier] = Seq(hydra.java.syntax.FieldModifier.public, hydra.java.syntax.FieldModifier.static,
     hydra.java.syntax.FieldModifier.`final`)
  lazy val nameName: hydra.java.syntax.Identifier = hydra.java.utils.nameToJavaName(aliases)("hydra.core.Name")
  lazy val env: hydra.java.environment.JavaEnvironment = hydra.java.environment.JavaEnvironment(aliases, g)
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type, hydra.java.syntax.ClassBodyDeclarationWithComments](hydra.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(hydra.core.Type.variable("hydra.core.Name"))(cx)(g))((jt: hydra.java.syntax.Type) =>
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression, hydra.java.syntax.ClassBodyDeclarationWithComments](hydra.java.coder.encodeTerm(env)(hydra.core.Term.literal(hydra.core.Literal.string(name)))(cx)(g))((arg: hydra.java.syntax.Expression) =>
    {
    lazy val init: hydra.java.syntax.VariableInitializer = hydra.java.syntax.VariableInitializer.expression(hydra.java.utils.javaConstructorCall(hydra.java.utils.javaConstructorName(nameName)(None))(Seq(arg))(None))
    {
      lazy val `var`: hydra.java.syntax.VariableDeclarator = hydra.java.utils.javaVariableDeclarator(javaName)(Some(init))
      Right(hydra.java.coder.noComment(hydra.java.utils.javaMemberField(mods)(jt)(`var`)))
    }
  }))
}

def constantDeclForFieldType(aliases: hydra.java.environment.Aliases)(ftyp: hydra.core.FieldType)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.ClassBodyDeclarationWithComments] =
  {
  lazy val name: hydra.core.Name = (ftyp.name)
  lazy val javaName: scala.Predef.String = hydra.formatting.nonAlnumToUnderscores(hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(hydra.util.CaseConvention.upperSnake)(name))
  hydra.java.coder.constantDecl(javaName)(aliases)(name)(cx)(g)
}

def constantDeclForTypeName(aliases: hydra.java.environment.Aliases)(name: hydra.core.Name)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.ClassBodyDeclarationWithComments] = hydra.java.coder.constantDecl("TYPE_")(aliases)(name)(cx)(g)

def constructElementsInterface(mod: hydra.packaging.Module)(members: Seq[hydra.java.syntax.InterfaceMemberDeclaration]): Tuple2[hydra.core.Name,
   hydra.java.syntax.CompilationUnit] =
  {
  lazy val ns: hydra.packaging.Namespace = (mod.namespace)
  lazy val parentNs: Option[hydra.packaging.Namespace] = hydra.java.coder.namespaceParent(ns)
  lazy val pkg: hydra.java.syntax.PackageDeclaration = hydra.lib.maybes.cases[hydra.packaging.Namespace,
     hydra.java.syntax.PackageDeclaration](parentNs)(hydra.java.utils.javaPackageDeclaration(ns))((pns: hydra.packaging.Namespace) => hydra.java.utils.javaPackageDeclaration(pns))
  lazy val mods: Seq[hydra.java.syntax.InterfaceModifier] = Seq(hydra.java.syntax.InterfaceModifier.public)
  lazy val className: scala.Predef.String = hydra.java.coder.elementsClassName(ns)
  lazy val elName: hydra.core.Name = hydra.java.coder.elementsQualifiedName(ns)
  lazy val body: hydra.java.syntax.InterfaceBody = members
  lazy val itf: hydra.java.syntax.TypeDeclaration = hydra.java.syntax.TypeDeclaration.interface(hydra.java.syntax.InterfaceDeclaration.normalInterface(hydra.java.syntax.NormalInterfaceDeclaration(mods,
     hydra.java.utils.javaTypeIdentifier(className), Seq(), Seq(), body)))
  lazy val decl: hydra.java.syntax.TypeDeclarationWithComments = hydra.java.syntax.TypeDeclarationWithComments(itf, (mod.description))
  Tuple2(elName, hydra.java.syntax.CompilationUnit.ordinary(hydra.java.syntax.OrdinaryCompilationUnit(Some(pkg), Seq(), Seq(decl))))
}

def correctCastType[T0, T1, T2](innerBody: hydra.core.Term)(typeArgs: Seq[hydra.core.Type])(fallback: hydra.core.Type)(cx: T0)(g: T1): Either[T2,
   hydra.core.Type] =
  hydra.strip.deannotateTerm(innerBody) match
  case hydra.core.Term.pair(v_Term_pair__p) => hydra.lib.logic.ifElse[Either[T2, hydra.core.Type]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Type](typeArgs))(2))(Right(hydra.core.Type.pair(hydra.core.PairType(hydra.lib.lists.head[hydra.core.Type](typeArgs),
     hydra.lib.lists.head[hydra.core.Type](hydra.lib.lists.tail[hydra.core.Type](typeArgs))))))(Right(fallback))
  case _ => Right(fallback)

def correctTypeApps[T0, T1](gr: T0)(name: hydra.core.Name)(args: Seq[hydra.core.Term])(fallbackTypeApps: Seq[hydra.core.Type])(cx: T1)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   Seq[hydra.core.Type]] =
  hydra.lib.eithers.bind[hydra.errors.Error, Option[hydra.core.Binding], Seq[hydra.core.Type]](Right(hydra.lexical.lookupBinding(g)(name)))((mel: Option[hydra.core.Binding]) =>
  hydra.lib.maybes.cases[hydra.core.Binding, Either[hydra.errors.Error, Seq[hydra.core.Type]]](mel)(Right(fallbackTypeApps))((el: hydra.core.Binding) =>
  hydra.lib.maybes.cases[hydra.core.TypeScheme, Either[hydra.errors.Error, Seq[hydra.core.Type]]](el.`type`)(Right(fallbackTypeApps))((ts: hydra.core.TypeScheme) =>
  {
  lazy val schemeType: hydra.core.Type = (ts.`type`)
  {
    lazy val allSchemeVars: Seq[hydra.core.Name] = hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) => hydra.java.coder.isSimpleName(v))(ts.variables)
    {
      lazy val schemeTypeVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.java.coder.collectTypeVars(schemeType)
      {
        lazy val usedFlags: Seq[Boolean] = hydra.lib.lists.map[hydra.core.Name, Boolean]((v: hydra.core.Name) => hydra.lib.sets.member[hydra.core.Name](v)(schemeTypeVars))(allSchemeVars)
        {
          lazy val usedSchemeVars: Seq[hydra.core.Name] = hydra.java.coder.filterByFlags(allSchemeVars)(usedFlags)
          {
            lazy val nParams: Int = hydra.java.coder.countFunctionParams(schemeType)
            {
              lazy val peeled: Tuple2[Seq[hydra.core.Type], hydra.core.Type] = hydra.java.coder.peelDomainTypes(nParams)(schemeType)
              {
                lazy val calleeDoms: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.core.Type](peeled)
                {
                  lazy val calleeCod: hydra.core.Type = hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.core.Type](peeled)
                  {
                    lazy val overgenSubst: Map[hydra.core.Name, hydra.core.Type] = hydra.java.coder.detectAccumulatorUnification(calleeDoms)(calleeCod)(usedSchemeVars)
                    {
                      lazy val keepFlags: Seq[Boolean] = hydra.lib.lists.map[hydra.core.Name, Boolean]((v: hydra.core.Name) =>
                        hydra.lib.logic.and(hydra.lib.sets.member[hydra.core.Name](v)(schemeTypeVars))(hydra.lib.logic.not(hydra.lib.maps.member[hydra.core.Name,
                           hydra.core.Type](v)(overgenSubst))))(allSchemeVars)
                      {
                        lazy val schemeVars: Seq[hydra.core.Name] = hydra.java.coder.filterByFlags(allSchemeVars)(keepFlags)
                        {
                          lazy val filteredFallback0: Seq[hydra.core.Type] = hydra.lib.logic.ifElse[Seq[hydra.core.Type]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Name](allSchemeVars))(hydra.lib.lists.length[hydra.core.Type](fallbackTypeApps)))(hydra.java.coder.filterByFlags(fallbackTypeApps)(keepFlags))(fallbackTypeApps)
                          {
                            lazy val filteredFallback: Seq[hydra.core.Type] = hydra.lib.logic.ifElse[Seq[hydra.core.Type]](hydra.lib.maps.`null`[hydra.core.Name,
                               hydra.core.Type](overgenSubst))(filteredFallback0)(hydra.lib.lists.map[hydra.core.Type,
                               hydra.core.Type]((t: hydra.core.Type) =>
                              hydra.java.coder.substituteTypeVarsWithTypes(overgenSubst)(t))(filteredFallback0))
                            hydra.lib.logic.ifElse[Either[hydra.errors.Error, Seq[hydra.core.Type]]](hydra.lib.logic.or(hydra.lib.lists.`null`[hydra.core.Name](schemeVars))(hydra.lib.logic.not(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Name](schemeVars))(hydra.lib.lists.length[hydra.core.Type](filteredFallback)))))(Right(filteredFallback))(hydra.java.coder.correctTypeAppsWithArgs(schemeVars)(filteredFallback)(schemeType)(args)(cx)(g))
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
})))

def correctTypeAppsWithArgs[T0](schemeVars: Seq[hydra.core.Name])(fallbackTypeApps: Seq[hydra.core.Type])(schemeType: hydra.core.Type)(args: Seq[hydra.core.Term])(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   Seq[hydra.core.Type]] =
  {
  lazy val schemeVarSet: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](schemeVars)
  lazy val irSubst: Map[hydra.core.Name, hydra.core.Type] = hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Type](hydra.lib.lists.zip[hydra.core.Name,
     hydra.core.Type](schemeVars)(fallbackTypeApps))
  lazy val peeled: Tuple2[Seq[hydra.core.Type], hydra.core.Type] = hydra.java.coder.peelDomainTypes(hydra.lib.lists.length[hydra.core.Term](args))(schemeType)
  lazy val schemeDoms: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.core.Type](peeled)
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[Option[hydra.core.Type]], Seq[hydra.core.Type]](hydra.lib.eithers.mapList[hydra.core.Term,
     Option[hydra.core.Type], hydra.errors.Error]((arg: hydra.core.Term) =>
    hydra.lib.eithers.bimap[hydra.errors.DecodingError, Option[hydra.core.Type], hydra.errors.Error, Option[hydra.core.Type]]((__de: hydra.errors.DecodingError) => hydra.errors.Error.other(__de))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(hydra.annotations.termAnnotationInternal(arg))))(args))((mArgTypes: Seq[Option[hydra.core.Type]]) =>
    hydra.lib.logic.ifElse[Either[hydra.errors.Error, Seq[hydra.core.Type]]](hydra.lib.logic.not(hydra.lib.lists.`null`[Option[hydra.core.Type]](hydra.lib.lists.filter[Option[hydra.core.Type]]((m: Option[hydra.core.Type]) => hydra.lib.maybes.isNothing[hydra.core.Type](m))(mArgTypes))))(Right(fallbackTypeApps))({
    lazy val argTypes: Seq[hydra.core.Type] = hydra.lib.lists.bind[Option[hydra.core.Type], hydra.core.Type](mArgTypes)((m: Option[hydra.core.Type]) =>
      hydra.lib.maybes.cases[hydra.core.Type, Seq[hydra.core.Type]](m)(Seq())((x: hydra.core.Type) => hydra.lib.lists.pure[hydra.core.Type](x)))
    {
      lazy val irDoms: Seq[hydra.core.Type] = hydra.lib.lists.map[hydra.core.Type, hydra.core.Type]((d: hydra.core.Type) => hydra.java.coder.applySubstSimple(irSubst)(d))(schemeDoms)
      {
        lazy val domsMatch: Boolean = hydra.lib.lists.`null`[Tuple2[hydra.core.Type, hydra.core.Type]](hydra.lib.lists.filter[Tuple2[hydra.core.Type,
           hydra.core.Type]]((p: Tuple2[hydra.core.Type, hydra.core.Type]) =>
          hydra.lib.logic.not(hydra.java.coder.typesMatch(hydra.strip.deannotateType(hydra.lib.pairs.first[hydra.core.Type,
             hydra.core.Type](p)))(hydra.strip.deannotateType(hydra.lib.pairs.second[hydra.core.Type,
             hydra.core.Type](p)))))(hydra.lib.lists.zip[hydra.core.Type, hydra.core.Type](irDoms)(argTypes)))
        hydra.lib.logic.ifElse[Either[hydra.errors.Error, Seq[hydra.core.Type]]](domsMatch)(Right(fallbackTypeApps))(Right(hydra.java.coder.resolveTypeApps(schemeVars)(fallbackTypeApps)(hydra.java.coder.buildArgSubst(schemeVarSet)(schemeDoms)(argTypes))))
      }
    }
  }))
}

def countFunctionParams(t: hydra.core.Type): Int =
  hydra.strip.deannotateType(t) match
  case hydra.core.Type.function(v_Type_function_ft) => hydra.lib.math.add(1)(hydra.java.coder.countFunctionParams(v_Type_function_ft.codomain))
  case _ => 0

def declarationForRecordType(isInner: Boolean)(isSer: Boolean)(aliases: hydra.java.environment.Aliases)(tparams: Seq[hydra.java.syntax.TypeParameter])(elName: hydra.core.Name)(fields: Seq[hydra.core.FieldType])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.ClassDeclaration] =
  hydra.java.coder.`declarationForRecordType_`(isInner)(isSer)(aliases)(tparams)(elName)(None)(fields)(cx)(g)

def `declarationForRecordType_`(isInner: Boolean)(isSer: Boolean)(aliases: hydra.java.environment.Aliases)(tparams: Seq[hydra.java.syntax.TypeParameter])(elName: hydra.core.Name)(parentName: Option[hydra.core.Name])(fields: Seq[hydra.core.FieldType])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.ClassDeclaration] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.ClassBodyDeclaration], hydra.java.syntax.ClassDeclaration](hydra.lib.eithers.mapList[hydra.core.FieldType,
     hydra.java.syntax.ClassBodyDeclaration, hydra.errors.Error]((f: hydra.core.FieldType) => hydra.java.coder.recordMemberVar(aliases)(f)(cx)(g))(fields))((memberVars: Seq[hydra.java.syntax.ClassBodyDeclaration]) =>
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.ClassBodyDeclarationWithComments],
     hydra.java.syntax.ClassDeclaration](hydra.lib.eithers.mapList[Tuple2[hydra.java.syntax.ClassBodyDeclaration,
     hydra.core.FieldType], hydra.java.syntax.ClassBodyDeclarationWithComments, hydra.errors.Error]((p: Tuple2[hydra.java.syntax.ClassBodyDeclaration,
     hydra.core.FieldType]) =>
  hydra.java.coder.addComment(hydra.lib.pairs.first[hydra.java.syntax.ClassBodyDeclaration, hydra.core.FieldType](p))(hydra.lib.pairs.second[hydra.java.syntax.ClassBodyDeclaration,
     hydra.core.FieldType](p))(cx)(g))(hydra.lib.lists.zip[hydra.java.syntax.ClassBodyDeclaration, hydra.core.FieldType](memberVars)(fields)))((`memberVars_`: Seq[hydra.java.syntax.ClassBodyDeclarationWithComments]) =>
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.ClassBodyDeclaration], hydra.java.syntax.ClassDeclaration](hydra.lib.logic.ifElse[Either[hydra.errors.Error,
     Seq[hydra.java.syntax.ClassBodyDeclaration]]](hydra.lib.equality.gt[Int](hydra.lib.lists.length[hydra.core.FieldType](fields))(1))(hydra.lib.eithers.mapList[hydra.core.FieldType,
     hydra.java.syntax.ClassBodyDeclaration, hydra.errors.Error]((f: hydra.core.FieldType) =>
  hydra.java.coder.recordWithMethod(aliases)(elName)(fields)(f)(cx)(g))(fields))(Right(Seq())))((withMethods: Seq[hydra.java.syntax.ClassBodyDeclaration]) =>
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ClassBodyDeclaration, hydra.java.syntax.ClassDeclaration](hydra.java.coder.recordConstructor(aliases)(elName)(fields)(cx)(g))((cons: hydra.java.syntax.ClassBodyDeclaration) =>
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.ClassBodyDeclarationWithComments],
     hydra.java.syntax.ClassDeclaration](hydra.lib.logic.ifElse[Either[hydra.errors.Error, Seq[hydra.java.syntax.ClassBodyDeclarationWithComments]]](isInner)(Right(Seq()))(hydra.lib.eithers.bind[hydra.errors.Error,
     hydra.java.syntax.ClassBodyDeclarationWithComments, Seq[hydra.java.syntax.ClassBodyDeclarationWithComments]](hydra.java.coder.constantDeclForTypeName(aliases)(elName)(cx)(g))((d: hydra.java.syntax.ClassBodyDeclarationWithComments) =>
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.ClassBodyDeclarationWithComments],
     Seq[hydra.java.syntax.ClassBodyDeclarationWithComments]](hydra.lib.eithers.mapList[hydra.core.FieldType,
     hydra.java.syntax.ClassBodyDeclarationWithComments, hydra.errors.Error]((f: hydra.core.FieldType) => hydra.java.coder.constantDeclForFieldType(aliases)(f)(cx)(g))(fields))((dfields: Seq[hydra.java.syntax.ClassBodyDeclarationWithComments]) =>
  Right(hydra.lib.lists.cons[hydra.java.syntax.ClassBodyDeclarationWithComments](d)(dfields))))))((tn: Seq[hydra.java.syntax.ClassBodyDeclarationWithComments]) =>
  {
  lazy val comparableMethods: Seq[hydra.java.syntax.ClassBodyDeclaration] = hydra.lib.maybes.cases[hydra.core.Name,
     Seq[hydra.java.syntax.ClassBodyDeclaration]](parentName)(hydra.lib.logic.ifElse[Seq[hydra.java.syntax.ClassBodyDeclaration]](hydra.lib.logic.and(hydra.lib.logic.not(isInner))(isSer))(Seq(hydra.java.coder.recordCompareToMethod(aliases)(tparams)(elName)(fields)))(Seq()))((pn: hydra.core.Name) =>
    hydra.lib.logic.ifElse[Seq[hydra.java.syntax.ClassBodyDeclaration]](isSer)(Seq(hydra.java.coder.variantCompareToMethod(aliases)(tparams)(pn)(elName)(fields)))(Seq()))
  {
    lazy val bodyDecls: Seq[hydra.java.syntax.ClassBodyDeclarationWithComments] = hydra.lib.lists.concat2[hydra.java.syntax.ClassBodyDeclarationWithComments](tn)(hydra.lib.lists.concat2[hydra.java.syntax.ClassBodyDeclarationWithComments](`memberVars_`)(hydra.lib.lists.map[hydra.java.syntax.ClassBodyDeclaration,
       hydra.java.syntax.ClassBodyDeclarationWithComments]((x: hydra.java.syntax.ClassBodyDeclaration) => hydra.java.coder.noComment(x))(hydra.lib.lists.concat2[hydra.java.syntax.ClassBodyDeclaration](Seq(cons,
       hydra.java.coder.recordEqualsMethod(aliases)(elName)(fields), hydra.java.coder.recordHashCodeMethod(fields)))(hydra.lib.lists.concat2[hydra.java.syntax.ClassBodyDeclaration](comparableMethods)(withMethods)))))
    {
      lazy val ifaces: Seq[hydra.java.syntax.InterfaceType] = hydra.lib.logic.ifElse[Seq[hydra.java.syntax.InterfaceType]](isInner)(hydra.java.coder.serializableTypes(isSer))(hydra.java.coder.interfaceTypes(isSer)(aliases)(tparams)(elName))
      Right(hydra.java.utils.javaClassDeclaration(aliases)(tparams)(elName)(hydra.java.coder.classModsPublic)(None)(ifaces)(bodyDecls))
    }
  }
})))))

def declarationForUnionType(isSer: Boolean)(aliases: hydra.java.environment.Aliases)(tparams: Seq[hydra.java.syntax.TypeParameter])(elName: hydra.core.Name)(fields: Seq[hydra.core.FieldType])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.ClassDeclaration] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.ClassDeclaration], hydra.java.syntax.ClassDeclaration](hydra.lib.eithers.mapList[hydra.core.FieldType,
     hydra.java.syntax.ClassDeclaration, hydra.errors.Error]((ft: hydra.core.FieldType) =>
  {
  lazy val fname: hydra.core.Name = (ft.name)
  {
    lazy val ftype: hydra.core.Type = (ft.`type`)
    {
      lazy val rfields: Seq[hydra.core.FieldType] = hydra.lib.logic.ifElse[Seq[hydra.core.FieldType]](hydra.predicates.isUnitType(hydra.strip.deannotateType(ftype)))(Seq())(Seq(hydra.core.FieldType("value",
         hydra.strip.deannotateType(ftype))))
      {
        lazy val varName: hydra.core.Name = hydra.java.utils.variantClassName(false)(elName)(fname)
        hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ClassDeclaration, hydra.java.syntax.ClassDeclaration](hydra.java.coder.`declarationForRecordType_`(true)(isSer)(aliases)(Seq())(varName)(hydra.lib.logic.ifElse[Option[hydra.core.Name]](isSer)(Some(elName))(None))(rfields)(cx)(g))((innerDecl: hydra.java.syntax.ClassDeclaration) =>
          Right(hydra.java.coder.augmentVariantClass(aliases)(tparams)(elName)(innerDecl)))
      }
    }
  }
})(fields))((variantClasses: Seq[hydra.java.syntax.ClassDeclaration]) =>
  {
  lazy val variantDecls: Seq[hydra.java.syntax.ClassBodyDeclaration] = hydra.lib.lists.map[hydra.java.syntax.ClassDeclaration,
     hydra.java.syntax.ClassBodyDeclaration]((vc: hydra.java.syntax.ClassDeclaration) =>
    hydra.java.syntax.ClassBodyDeclaration.classMember(hydra.java.syntax.ClassMemberDeclaration.`class`(vc)))(variantClasses)
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.ClassBodyDeclarationWithComments],
     hydra.java.syntax.ClassDeclaration](hydra.lib.eithers.mapList[Tuple2[hydra.java.syntax.ClassBodyDeclaration,
     hydra.core.FieldType], hydra.java.syntax.ClassBodyDeclarationWithComments, hydra.errors.Error]((pair: Tuple2[hydra.java.syntax.ClassBodyDeclaration,
     hydra.core.FieldType]) =>
    hydra.java.coder.addComment(hydra.lib.pairs.first[hydra.java.syntax.ClassBodyDeclaration, hydra.core.FieldType](pair))(hydra.lib.pairs.second[hydra.java.syntax.ClassBodyDeclaration,
       hydra.core.FieldType](pair))(cx)(g))(hydra.lib.lists.zip[hydra.java.syntax.ClassBodyDeclaration,
       hydra.core.FieldType](variantDecls)(fields)))((`variantDecls_`: Seq[hydra.java.syntax.ClassBodyDeclarationWithComments]) =>
    {
    lazy val privateConst: hydra.java.syntax.ClassBodyDeclaration = hydra.java.utils.makeConstructor(aliases)(elName)(true)(Seq())(Seq())
    {
      lazy val acceptDecl: hydra.java.syntax.ClassBodyDeclaration = hydra.java.utils.toAcceptMethod(true)(tparams)
      {
        lazy val vtparams: Seq[hydra.java.syntax.TypeParameter] = hydra.lib.lists.concat2[hydra.java.syntax.TypeParameter](tparams)(Seq(hydra.java.utils.javaTypeParameter(hydra.java.names.visitorReturnParameter)))
        {
          lazy val visitorMethods: Seq[hydra.java.syntax.InterfaceMemberDeclaration] = hydra.lib.lists.map[hydra.core.FieldType,
             hydra.java.syntax.InterfaceMemberDeclaration]((ft: hydra.core.FieldType) =>
            {
            lazy val fname: hydra.core.Name = (ft.name)
            {
              lazy val typeArgs: Seq[hydra.java.syntax.TypeArgument] = hydra.lib.lists.map[hydra.java.syntax.TypeParameter,
                 hydra.java.syntax.TypeArgument]((tp: hydra.java.syntax.TypeParameter) => hydra.java.utils.typeParameterToTypeArgument(tp))(tparams)
              {
                lazy val varRef: hydra.java.syntax.Type = hydra.java.utils.javaClassTypeToJavaType(hydra.java.utils.nameToJavaClassType(aliases)(false)(typeArgs)(hydra.java.utils.variantClassName(false)(elName)(fname))(None))
                {
                  lazy val param: hydra.java.syntax.FormalParameter = hydra.java.utils.javaTypeToJavaFormalParameter(varRef)("instance")
                  {
                    lazy val resultR: hydra.java.syntax.Result = hydra.java.utils.javaTypeToJavaResult(hydra.java.syntax.Type.reference(hydra.java.utils.visitorTypeVariable))
                    hydra.java.utils.interfaceMethodDeclaration(Seq())(Seq())(hydra.java.names.visitMethodName)(Seq(param))(resultR)(None)
                  }
                }
              }
            }
          })(fields)
          {
            lazy val visitorBody: hydra.java.syntax.InterfaceBody = visitorMethods
            {
              lazy val visitor: hydra.java.syntax.ClassBodyDeclaration = hydra.java.utils.javaInterfaceDeclarationToJavaClassBodyDeclaration(hydra.java.syntax.NormalInterfaceDeclaration(Seq(hydra.java.syntax.InterfaceModifier.public),
                 hydra.java.names.visitorName, vtparams, Seq(), visitorBody))
              {
                lazy val typeArgs: Seq[hydra.java.syntax.TypeArgument] = hydra.lib.lists.map[hydra.java.syntax.TypeParameter,
                   hydra.java.syntax.TypeArgument]((tp: hydra.java.syntax.TypeParameter) => hydra.java.utils.typeParameterToTypeArgument(tp))(tparams)
                {
                  lazy val visitorClassType: hydra.java.syntax.ClassType = hydra.java.utils.javaClassType(hydra.lib.lists.concat2[hydra.java.syntax.ReferenceType](hydra.lib.lists.map[hydra.java.syntax.TypeParameter,
                     hydra.java.syntax.ReferenceType]((tp: hydra.java.syntax.TypeParameter) => hydra.java.utils.typeParameterToReferenceType(tp))(tparams))(Seq(hydra.java.utils.visitorTypeVariable)))(None)(hydra.java.names.visitorName)
                  {
                    lazy val mainInstanceParam: hydra.java.syntax.FormalParameter = hydra.java.utils.javaTypeToJavaFormalParameter(hydra.java.utils.javaClassTypeToJavaType(hydra.java.utils.nameToJavaClassType(aliases)(false)(typeArgs)(elName)(None)))("instance")
                    {
                      lazy val resultR: hydra.java.syntax.Result = hydra.java.utils.javaTypeToJavaResult(hydra.java.syntax.Type.reference(hydra.java.utils.visitorTypeVariable))
                      {
                        lazy val throwStmt: hydra.java.syntax.BlockStatement = hydra.java.syntax.BlockStatement.statement(hydra.java.utils.javaThrowIllegalStateException(Seq(hydra.java.utils.javaAdditiveExpressionToJavaExpression(hydra.java.utils.addExpressions(Seq(hydra.java.utils.javaStringMultiplicativeExpression("Non-exhaustive patterns when matching: "),
                           hydra.java.syntax.MultiplicativeExpression.unary(hydra.java.utils.javaIdentifierToJavaUnaryExpression("instance"))))))))
                        {
                          lazy val defaultMod: Seq[hydra.java.syntax.InterfaceMethodModifier] = Seq(hydra.java.syntax.InterfaceMethodModifier.default)
                          {
                            lazy val otherwiseDecl: hydra.java.syntax.InterfaceMemberDeclaration = hydra.java.utils.interfaceMethodDeclaration(defaultMod)(Seq())(hydra.java.names.otherwiseMethodName)(Seq(mainInstanceParam))(resultR)(Some(Seq(throwStmt)))
                            {
                              lazy val pvVisitMethods: Seq[hydra.java.syntax.InterfaceMemberDeclaration] = hydra.lib.lists.map[hydra.core.FieldType,
                                 hydra.java.syntax.InterfaceMemberDeclaration]((ft: hydra.core.FieldType) =>
                                {
                                lazy val fname: hydra.core.Name = (ft.name)
                                {
                                  lazy val varRef: hydra.java.syntax.Type = hydra.java.utils.javaClassTypeToJavaType(hydra.java.utils.nameToJavaClassType(aliases)(false)(typeArgs)(hydra.java.utils.variantClassName(false)(elName)(fname))(None))
                                  {
                                    lazy val param: hydra.java.syntax.FormalParameter = hydra.java.utils.javaTypeToJavaFormalParameter(varRef)("instance")
                                    {
                                      lazy val mi: hydra.java.syntax.MethodInvocation = hydra.java.utils.methodInvocation(None)(hydra.java.names.otherwiseMethodName)(Seq(hydra.java.utils.javaIdentifierToJavaExpression("instance")))
                                      {
                                        lazy val returnOtherwise: hydra.java.syntax.BlockStatement = hydra.java.syntax.BlockStatement.statement(hydra.java.utils.javaReturnStatement(Some(hydra.java.utils.javaPrimaryToJavaExpression(hydra.java.utils.javaMethodInvocationToJavaPrimary(mi)))))
                                        hydra.java.utils.interfaceMethodDeclaration(defaultMod)(Seq())(hydra.java.names.visitMethodName)(Seq(param))(resultR)(Some(Seq(returnOtherwise)))
                                      }
                                    }
                                  }
                                }
                              })(fields)
                              {
                                lazy val pvBody: hydra.java.syntax.InterfaceBody = hydra.lib.lists.concat2[hydra.java.syntax.InterfaceMemberDeclaration](Seq(otherwiseDecl))(pvVisitMethods)
                                {
                                  lazy val partialVisitor: hydra.java.syntax.ClassBodyDeclaration = hydra.java.utils.javaInterfaceDeclarationToJavaClassBodyDeclaration(hydra.java.syntax.NormalInterfaceDeclaration(Seq(hydra.java.syntax.InterfaceModifier.public),
                                     hydra.java.names.partialVisitorName, vtparams, Seq(visitorClassType),
                                     pvBody))
                                  hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ClassBodyDeclarationWithComments,
                                     hydra.java.syntax.ClassDeclaration](hydra.java.coder.constantDeclForTypeName(aliases)(elName)(cx)(g))((tn0: hydra.java.syntax.ClassBodyDeclarationWithComments) =>
                                    hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.ClassBodyDeclarationWithComments],
                                       hydra.java.syntax.ClassDeclaration](hydra.lib.eithers.mapList[hydra.core.FieldType,
                                       hydra.java.syntax.ClassBodyDeclarationWithComments, hydra.errors.Error]((ft: hydra.core.FieldType) =>
                                    hydra.java.coder.constantDeclForFieldType(aliases)(ft)(cx)(g))(fields))((tn1: Seq[hydra.java.syntax.ClassBodyDeclarationWithComments]) =>
                                    {
                                    lazy val tn: Seq[hydra.java.syntax.ClassBodyDeclarationWithComments] = hydra.lib.lists.concat2[hydra.java.syntax.ClassBodyDeclarationWithComments](Seq(tn0))(tn1)
                                    {
                                      lazy val otherDecls: Seq[hydra.java.syntax.ClassBodyDeclarationWithComments] = hydra.lib.lists.map[hydra.java.syntax.ClassBodyDeclaration,
                                         hydra.java.syntax.ClassBodyDeclarationWithComments]((d: hydra.java.syntax.ClassBodyDeclaration) => hydra.java.coder.noComment(d))(Seq(privateConst,
                                         acceptDecl, visitor, partialVisitor))
                                      {
                                        lazy val bodyDecls: Seq[hydra.java.syntax.ClassBodyDeclarationWithComments] = hydra.lib.lists.concat[hydra.java.syntax.ClassBodyDeclarationWithComments](Seq(tn,
                                           otherDecls, `variantDecls_`))
                                        {
                                          lazy val mods: Seq[hydra.java.syntax.ClassModifier] = hydra.lib.lists.concat2[hydra.java.syntax.ClassModifier](hydra.java.coder.classModsPublic)(Seq(hydra.java.syntax.ClassModifier.`abstract`))
                                          Right(hydra.java.utils.javaClassDeclaration(aliases)(tparams)(elName)(mods)(None)(hydra.java.coder.interfaceTypes(isSer)(aliases)(tparams)(elName))(bodyDecls))
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
              }
            }
          }
        }
      }
    }
  })
})

def decodeTypeFromTerm(term: hydra.core.Term): Option[hydra.core.Type] =
  hydra.strip.deannotateTerm(term) match
  case hydra.core.Term.inject(v_Term_inject_inj) => hydra.lib.logic.ifElse[Option[hydra.core.Type]](hydra.lib.equality.equal[hydra.core.Name](v_Term_inject_inj.typeName)("hydra.core.Type"))({
    lazy val fname: scala.Predef.String = (v_Term_inject_inj.field.name)
    {
      lazy val fterm: hydra.core.Term = (v_Term_inject_inj.field.term)
      hydra.lib.logic.ifElse[Option[hydra.core.Type]](hydra.lib.equality.equal[scala.Predef.String](fname)("variable"))(fterm match
        case hydra.core.Term.wrap(v_Term_wrap_wt) => v_Term_wrap_wt.body match
          case hydra.core.Term.literal(v_Term_literal_lit) => v_Term_literal_lit match
            case hydra.core.Literal.string(v_Literal_string_s) => Some(hydra.core.Type.variable(v_Literal_string_s))
            case _ => None
          case _ => None
        case _ => None)(hydra.lib.logic.ifElse[Option[hydra.core.Type]](hydra.lib.equality.equal[scala.Predef.String](fname)("annotated"))(fterm match
        case hydra.core.Term.record(v_Term_record_rec) => hydra.lib.maybes.bind[hydra.core.Field, hydra.core.Type](hydra.lib.lists.safeHead[hydra.core.Field](hydra.lib.lists.filter[hydra.core.Field]((f: hydra.core.Field) => hydra.lib.equality.equal[hydra.core.Name](f.name)("body"))(v_Term_record_rec.fields)))((bodyField: hydra.core.Field) => hydra.java.coder.decodeTypeFromTerm(bodyField.term))
        case _ => None)(hydra.lib.logic.ifElse[Option[hydra.core.Type]](hydra.lib.equality.equal[scala.Predef.String](fname)("application"))(fterm match
        case hydra.core.Term.record(v_Term_record_rec) => hydra.lib.maybes.bind[hydra.core.Field, hydra.core.Type](hydra.lib.lists.safeHead[hydra.core.Field](hydra.lib.lists.filter[hydra.core.Field]((f: hydra.core.Field) =>
          hydra.lib.equality.equal[hydra.core.Name](f.name)("function"))(v_Term_record_rec.fields)))((funcField: hydra.core.Field) =>
          hydra.lib.maybes.bind[hydra.core.Type, hydra.core.Type](hydra.java.coder.decodeTypeFromTerm(funcField.term))((func: hydra.core.Type) =>
          hydra.lib.maybes.bind[hydra.core.Field, hydra.core.Type](hydra.lib.lists.safeHead[hydra.core.Field](hydra.lib.lists.filter[hydra.core.Field]((f: hydra.core.Field) =>
          hydra.lib.equality.equal[hydra.core.Name](f.name)("argument"))(v_Term_record_rec.fields)))((argField: hydra.core.Field) =>
          hydra.lib.maybes.map[hydra.core.Type, hydra.core.Type]((arg: hydra.core.Type) =>
          hydra.core.Type.application(hydra.core.ApplicationType(func, arg)))(hydra.java.coder.decodeTypeFromTerm(argField.term)))))
        case _ => None)(hydra.lib.logic.ifElse[Option[hydra.core.Type]](hydra.lib.equality.equal[scala.Predef.String](fname)("function"))(fterm match
        case hydra.core.Term.record(v_Term_record_rec) => hydra.lib.maybes.bind[hydra.core.Field, hydra.core.Type](hydra.lib.lists.safeHead[hydra.core.Field](hydra.lib.lists.filter[hydra.core.Field]((f: hydra.core.Field) => hydra.lib.equality.equal[hydra.core.Name](f.name)("domain"))(v_Term_record_rec.fields)))((domField: hydra.core.Field) =>
          hydra.lib.maybes.bind[hydra.core.Type, hydra.core.Type](hydra.java.coder.decodeTypeFromTerm(domField.term))((dom: hydra.core.Type) =>
          hydra.lib.maybes.bind[hydra.core.Field, hydra.core.Type](hydra.lib.lists.safeHead[hydra.core.Field](hydra.lib.lists.filter[hydra.core.Field]((f: hydra.core.Field) =>
          hydra.lib.equality.equal[hydra.core.Name](f.name)("codomain"))(v_Term_record_rec.fields)))((codField: hydra.core.Field) =>
          hydra.lib.maybes.map[hydra.core.Type, hydra.core.Type]((cod: hydra.core.Type) => hydra.core.Type.function(hydra.core.FunctionType(dom,
             cod)))(hydra.java.coder.decodeTypeFromTerm(codField.term)))))
        case _ => None)(hydra.lib.logic.ifElse[Option[hydra.core.Type]](hydra.lib.equality.equal[scala.Predef.String](fname)("literal"))(fterm match
        case hydra.core.Term.inject(v_Term_inject_litInj) => hydra.lib.logic.ifElse[Option[hydra.core.Type]](hydra.lib.equality.equal[scala.Predef.String](v_Term_inject_litInj.field.name)("string"))(Some(hydra.core.Type.literal(hydra.core.LiteralType.string)))(None)
        case _ => None)(None)))))
    }
  })(None)
  case _ => None

def dedupBindings(inScope: scala.collection.immutable.Set[hydra.core.Name])(bs: Seq[hydra.core.Binding]): Seq[hydra.core.Binding] =
  hydra.lib.logic.ifElse[Seq[hydra.core.Binding]](hydra.lib.lists.`null`[hydra.core.Binding](bs))(Seq())({
  lazy val b: hydra.core.Binding = hydra.lib.lists.head[hydra.core.Binding](bs)
  {
    lazy val rest: Seq[hydra.core.Binding] = hydra.lib.lists.tail[hydra.core.Binding](bs)
    {
      lazy val name: hydra.core.Name = (b.name)
      hydra.lib.logic.ifElse[Seq[hydra.core.Binding]](hydra.lib.sets.member[hydra.core.Name](name)(inScope))({
        lazy val newName: hydra.core.Name = hydra.java.coder.freshJavaName(name)(inScope)
        {
          lazy val subst: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.maps.singleton[hydra.core.Name, hydra.core.Name](name)(newName)
          {
            lazy val rest2: Seq[hydra.core.Binding] = hydra.lib.lists.map[hydra.core.Binding, hydra.core.Binding]((b2: hydra.core.Binding) =>
              hydra.core.Binding(b2.name, hydra.variables.substituteVariables(subst)(b2.term), (b2.`type`)))(rest)
            hydra.lib.lists.cons[hydra.core.Binding](hydra.core.Binding(newName, (b.term), (b.`type`)))(hydra.java.coder.dedupBindings(hydra.lib.sets.insert[hydra.core.Name](newName)(inScope))(rest2))
          }
        }
      })(hydra.lib.lists.cons[hydra.core.Binding](b)(hydra.java.coder.dedupBindings(hydra.lib.sets.insert[hydra.core.Name](name)(inScope))(rest)))
    }
  }
})

def detectAccumulatorUnification(doms: Seq[hydra.core.Type])(cod: hydra.core.Type)(tparams: Seq[hydra.core.Name]): Map[hydra.core.Name, hydra.core.Type] =
  {
  lazy val tparamSet: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](tparams)
  lazy val allPairs: Seq[Tuple2[hydra.core.Name, hydra.core.Name]] = hydra.lib.lists.bind[hydra.core.Type,
     Tuple2[hydra.core.Name, hydra.core.Name]](doms)((d: hydra.core.Type) => hydra.java.coder.extractInOutPair(d))
  lazy val groupedByInput: Map[hydra.core.Name, Seq[hydra.core.Name]] = hydra.java.coder.groupPairsByFirst(allPairs)
  lazy val selfRefSubst: Map[hydra.core.Name, hydra.core.Name] = hydra.java.coder.selfRefSubstitution(groupedByInput)
  lazy val directPairs: Seq[Tuple2[hydra.core.Name, hydra.core.Name]] = hydra.lib.lists.bind[hydra.core.Type,
     Tuple2[hydra.core.Name, hydra.core.Name]](doms)((d: hydra.core.Type) => hydra.java.coder.extractDirectReturn(tparamSet)(d))
  lazy val groupedDirect: Map[hydra.core.Name, Seq[hydra.core.Name]] = hydra.java.coder.groupPairsByFirst(directPairs)
  lazy val directInputVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.lists.map[Tuple2[hydra.core.Name,
     hydra.core.Name], hydra.core.Name]((p: Tuple2[hydra.core.Name, hydra.core.Name]) => hydra.lib.pairs.first[hydra.core.Name,
     hydra.core.Name](p))(directPairs))
  lazy val codVar: Option[hydra.core.Name] = hydra.strip.deannotateType(cod) match
    case hydra.core.Type.variable(v_Type_variable_v) => Some(v_Type_variable_v)
    case _ => None
  lazy val directRefSubst: Map[hydra.core.Name, hydra.core.Name] = hydra.java.coder.directRefSubstitution(directInputVars)(codVar)(groupedDirect)
  lazy val codSubst: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.maybes.maybe[Map[hydra.core.Name,
     hydra.core.Name], hydra.core.Name](hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name])((cv: hydra.core.Name) =>
    hydra.lib.logic.ifElse[Map[hydra.core.Name, hydra.core.Name]](hydra.lib.maps.member[hydra.core.Name,
       hydra.core.Name](cv)(selfRefSubst))(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name])(hydra.lib.maybes.maybe[Map[hydra.core.Name,
       hydra.core.Name], hydra.core.Name](hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name])((refVar: hydra.core.Name) =>
    hydra.lib.logic.ifElse[Map[hydra.core.Name, hydra.core.Name]](hydra.lib.equality.equal[hydra.core.Name](cv)(refVar))(hydra.lib.maps.empty[hydra.core.Name,
       hydra.core.Name])(hydra.lib.maps.singleton[hydra.core.Name, hydra.core.Name](cv)(refVar)))(hydra.java.coder.findSelfRefVar(groupedByInput))))(hydra.java.coder.findPairFirst(cod))
  lazy val domVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.lists.bind[hydra.core.Type,
     hydra.core.Name](doms)((d: hydra.core.Type) =>
    hydra.lib.sets.toList[hydra.core.Name](hydra.java.coder.collectTypeVars(d))))
  lazy val danglingSubst: Map[hydra.core.Name, hydra.core.Type] = hydra.lib.maybes.maybe[Map[hydra.core.Name,
     hydra.core.Type], hydra.core.Name](hydra.lib.maps.empty[hydra.core.Name, hydra.core.Type])((cv: hydra.core.Name) =>
    hydra.lib.logic.ifElse[Map[hydra.core.Name, hydra.core.Type]](hydra.lib.sets.member[hydra.core.Name](cv)(domVars))(hydra.lib.maps.empty[hydra.core.Name,
       hydra.core.Type])(hydra.lib.maybes.maybe[Map[hydra.core.Name, hydra.core.Type], hydra.core.Name](hydra.lib.maps.empty[hydra.core.Name,
       hydra.core.Type])((refVar: hydra.core.Name) =>
    hydra.lib.maps.singleton[hydra.core.Name, hydra.core.Type](cv)(hydra.core.Type.variable(refVar)))(hydra.java.coder.findSelfRefVar(groupedByInput))))(hydra.java.coder.findPairFirst(cod))
  hydra.lib.maps.union[hydra.core.Name, hydra.core.Type](hydra.lib.maps.union[hydra.core.Name, hydra.core.Type](hydra.lib.maps.union[hydra.core.Name,
     hydra.core.Type](hydra.java.coder.nameMapToTypeMap(selfRefSubst))(hydra.java.coder.nameMapToTypeMap(codSubst)))(danglingSubst))(hydra.java.coder.nameMapToTypeMap(directRefSubst))
}

def directRefSubstitution[T0](directInputVars: scala.collection.immutable.Set[T0])(codVar: Option[T0])(grouped: Map[T0, Seq[T0]]): Map[T0, T0] =
  hydra.lib.lists.foldl[Map[T0, T0], Tuple2[T0, Seq[T0]]]((subst: Map[T0, T0]) =>
  (entry: Tuple2[T0, Seq[T0]]) =>
  hydra.java.coder.directRefSubstitution_processGroup(directInputVars)(codVar)(subst)(hydra.lib.pairs.first[T0,
     Seq[T0]](entry))(hydra.lib.pairs.second[T0, Seq[T0]](entry)))(hydra.lib.maps.empty[T0, T0])(hydra.lib.maps.toList[T0,
     Seq[T0]](grouped))

def directRefSubstitution_processGroup[T0](directInputVars: scala.collection.immutable.Set[T0])(codVar: Option[T0])(subst: Map[T0,
   T0])(inVar: T0)(outVars: Seq[T0]): Map[T0, T0] =
  {
  lazy val selfRefCount: Int = hydra.lib.lists.length[T0](hydra.lib.lists.filter[T0]((v: T0) => hydra.lib.equality.equal[T0](v)(inVar))(outVars))
  lazy val nonSelfVars: Seq[T0] = hydra.lib.lists.filter[T0]((v: T0) => hydra.lib.logic.not(hydra.lib.equality.equal[T0](v)(inVar)))(outVars)
  lazy val safeNonSelfVars: Seq[T0] = hydra.lib.lists.filter[T0]((v: T0) =>
    hydra.lib.logic.and(hydra.lib.logic.not(hydra.lib.sets.member[T0](v)(directInputVars)))(hydra.lib.logic.not(hydra.lib.equality.equal[Option[T0]](Some(v))(codVar))))(nonSelfVars)
  hydra.lib.logic.ifElse[Map[T0, T0]](hydra.lib.logic.and(hydra.lib.equality.gte[Int](selfRefCount)(2))(hydra.lib.logic.not(hydra.lib.lists.`null`[T0](safeNonSelfVars))))(hydra.lib.lists.foldl[Map[T0,
     T0], T0]((s: Map[T0, T0]) => (v: T0) => hydra.lib.maps.insert[T0, T0](v)(inVar)(s))(subst)(safeNonSelfVars))(subst)
}

def domTypeArgs[T0](aliases: hydra.java.environment.Aliases)(d: hydra.core.Type)(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   Seq[hydra.java.syntax.TypeArgument]] =
  {
  lazy val args: Seq[hydra.core.Type] = hydra.java.coder.extractTypeApplicationArgs(hydra.strip.deannotateType(d))
  hydra.lib.logic.ifElse[Either[hydra.errors.Error, Seq[hydra.java.syntax.TypeArgument]]](hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.core.Type](args)))(hydra.lib.eithers.mapList[hydra.core.Type,
     hydra.java.syntax.TypeArgument, hydra.errors.Error]((t: hydra.core.Type) =>
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type, hydra.java.syntax.TypeArgument](hydra.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(t)(cx)(g))((jt: hydra.java.syntax.Type) =>
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType, hydra.java.syntax.TypeArgument](hydra.java.utils.javaTypeToJavaReferenceType(jt)(cx))((rt: hydra.java.syntax.ReferenceType) => Right(hydra.java.syntax.TypeArgument.reference(rt)))))(args))(Right(hydra.java.coder.javaTypeArgumentsForType(d)))
}

def elementJavaIdentifier(isPrim: Boolean)(isMethod: Boolean)(aliases: hydra.java.environment.Aliases)(name: hydra.core.Name): hydra.java.syntax.Identifier =
  {
  lazy val qn: hydra.packaging.QualifiedName = hydra.names.qualifyName(name)
  lazy val `ns_`: Option[hydra.packaging.Namespace] = (qn.namespace)
  lazy val local: scala.Predef.String = (qn.local)
  lazy val sep: scala.Predef.String = hydra.lib.logic.ifElse[scala.Predef.String](isMethod)("::")(".")
  hydra.lib.logic.ifElse[hydra.java.syntax.Identifier](isPrim)(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.java.coder.elementJavaIdentifier_qualify(aliases)(`ns_`)(hydra.formatting.capitalize(local)))("."))(hydra.java.names.applyMethodName))(hydra.lib.maybes.cases[hydra.packaging.Namespace,
     hydra.java.syntax.Identifier](`ns_`)(hydra.java.utils.sanitizeJavaName(local))((n: hydra.packaging.Namespace) =>
    hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.java.coder.elementJavaIdentifier_qualify(aliases)(hydra.java.coder.namespaceParent(n))(hydra.java.coder.elementsClassName(n)))(sep))(hydra.java.utils.sanitizeJavaName(local))))
}

def elementJavaIdentifier_qualify(aliases: hydra.java.environment.Aliases)(mns: Option[hydra.packaging.Namespace])(s: scala.Predef.String): scala.Predef.String =
  hydra.java.utils.nameToJavaName(aliases)(hydra.names.unqualifyName(hydra.packaging.QualifiedName(mns, s)))

def elementsClassName(ns: hydra.packaging.Namespace): scala.Predef.String =
  {
  lazy val nsStr: scala.Predef.String = ns
  lazy val parts: Seq[scala.Predef.String] = hydra.lib.strings.splitOn(".")(nsStr)
  hydra.formatting.sanitizeWithUnderscores(hydra.java.language.reservedWords)(hydra.formatting.capitalize(hydra.lib.lists.last[scala.Predef.String](parts)))
}

def elementsQualifiedName(ns: hydra.packaging.Namespace): hydra.core.Name =
  hydra.names.unqualifyName(hydra.packaging.QualifiedName(hydra.java.coder.namespaceParent(ns), hydra.java.coder.elementsClassName(ns)))

def encodeApplication(env: hydra.java.environment.JavaEnvironment)(app: hydra.core.Application)(cx: hydra.context.Context)(g0: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.Expression] =
  {
  lazy val aliases: hydra.java.environment.Aliases = (env.aliases)
  lazy val g: hydra.graph.Graph = (env.graph)
  lazy val gathered: Tuple2[hydra.core.Term, Tuple2[Seq[hydra.core.Term], Seq[hydra.core.Type]]] = hydra.analysis.gatherArgsWithTypeApps(hydra.core.Term.application(app))(Seq())(Seq())
  lazy val fun: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, Tuple2[Seq[hydra.core.Term], Seq[hydra.core.Type]]](gathered)
  lazy val args: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term], Seq[hydra.core.Type]](hydra.lib.pairs.second[hydra.core.Term,
     Tuple2[Seq[hydra.core.Term], Seq[hydra.core.Type]]](gathered))
  lazy val typeApps: Seq[hydra.core.Type] = hydra.lib.pairs.second[Seq[hydra.core.Term], Seq[hydra.core.Type]](hydra.lib.pairs.second[hydra.core.Term,
     Tuple2[Seq[hydra.core.Term], Seq[hydra.core.Type]]](gathered))
  hydra.lib.eithers.bind[hydra.errors.Error, Option[hydra.core.Type], hydra.java.syntax.Expression](hydra.lib.eithers.bimap[hydra.errors.DecodingError,
     Option[hydra.core.Type], hydra.errors.Error, Option[hydra.core.Type]]((__de: hydra.errors.DecodingError) => hydra.errors.Error.other(__de))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(hydra.annotations.termAnnotationInternal(fun))))((mfunTyp: Option[hydra.core.Type]) =>
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Type, hydra.java.syntax.Expression](hydra.lib.maybes.cases[hydra.core.Type,
       Either[hydra.errors.Error, hydra.core.Type]](mfunTyp)(hydra.checking.typeOfTerm(cx)(g)(fun))((t: hydra.core.Type) => Right(t)))((funTyp: hydra.core.Type) =>
    {
    lazy val arity: Int = hydra.arity.typeArity(funTyp)
    {
      lazy val deannotatedFun: hydra.core.Term = hydra.strip.deannotateTerm(fun)
      {
        lazy val calleeName: Option[hydra.core.Name] = deannotatedFun match
          case hydra.core.Term.variable(v_Term_variable_n) => Some(v_Term_variable_n)
          case _ => None
        hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], hydra.java.syntax.Expression](hydra.lib.maybes.cases[hydra.core.Name,
           Either[hydra.errors.Error, Seq[hydra.core.Term]]](calleeName)(Right(args))((cname: hydra.core.Name) =>
          hydra.java.coder.annotateLambdaArgs(cname)(typeApps)(args)(cx)(g)))((annotatedArgs: Seq[hydra.core.Term]) =>
          deannotatedFun match
          case hydra.core.Term.variable(v_Term_variable_name) => hydra.lib.logic.ifElse[Either[hydra.errors.Error,
             hydra.java.syntax.Expression]](hydra.lib.maybes.isJust[hydra.graph.Primitive](hydra.lib.maps.lookup[hydra.core.Name,
             hydra.graph.Primitive](v_Term_variable_name)(g.primitives)))({
            lazy val hargs: Seq[hydra.core.Term] = hydra.lib.lists.take[hydra.core.Term](arity)(annotatedArgs)
            {
              lazy val rargs: Seq[hydra.core.Term] = hydra.lib.lists.drop[hydra.core.Term](arity)(annotatedArgs)
              hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression, hydra.java.syntax.Expression](hydra.java.coder.functionCall(env)(true)(v_Term_variable_name)(hargs)(Seq())(cx)(g))((initialCall: hydra.java.syntax.Expression) =>
                hydra.lib.eithers.foldl[hydra.java.syntax.Expression, hydra.core.Term, hydra.errors.Error]((acc: hydra.java.syntax.Expression) =>
                (h: hydra.core.Term) =>
                hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression, hydra.java.syntax.Expression](hydra.java.coder.encodeTerm(env)(h)(cx)(g))((jarg: hydra.java.syntax.Expression) => Right(hydra.java.coder.applyJavaArg(acc)(jarg))))(initialCall)(rargs))
            }
          })(hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.java.syntax.Expression]](hydra.lib.logic.and(hydra.java.coder.isRecursiveVariable(aliases)(v_Term_variable_name))(hydra.lib.logic.not(hydra.java.coder.isLambdaBoundIn(v_Term_variable_name)(aliases.lambdaVars))))(hydra.java.coder.encodeApplication_fallback(env)(aliases)(g)(typeApps)(app.function)(app.argument)(cx)(g))(hydra.lib.eithers.bind[hydra.errors.Error,
             hydra.java.environment.JavaSymbolClass, hydra.java.syntax.Expression](hydra.java.coder.classifyDataReference(v_Term_variable_name)(cx)(g))((symClass: hydra.java.environment.JavaSymbolClass) =>
            {
            lazy val methodArity: Int = symClass match
              case hydra.java.environment.JavaSymbolClass.hoistedLambda(v_JavaSymbolClass_hoistedLambda_n) => v_JavaSymbolClass_hoistedLambda_n
              case _ => arity
            {
              lazy val hargs: Seq[hydra.core.Term] = hydra.lib.lists.take[hydra.core.Term](methodArity)(annotatedArgs)
              {
                lazy val rargs: Seq[hydra.core.Term] = hydra.lib.lists.drop[hydra.core.Term](methodArity)(annotatedArgs)
                {
                  lazy val trusted: scala.collection.immutable.Set[hydra.core.Name] = (aliases.trustedTypeVars)
                  {
                    lazy val inScope: scala.collection.immutable.Set[hydra.core.Name] = (aliases.inScopeTypeParams)
                    {
                      lazy val filteredTypeApps: Seq[hydra.core.Type] = hydra.lib.logic.ifElse[Seq[hydra.core.Type]](hydra.lib.logic.or(hydra.lib.sets.`null`[hydra.core.Name](trusted))(hydra.lib.sets.`null`[hydra.core.Name](inScope)))(Seq())({
                        lazy val allVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.unions[hydra.core.Name](hydra.lib.lists.map[hydra.core.Type,
                           scala.collection.immutable.Set[hydra.core.Name]]((t: hydra.core.Type) => hydra.java.coder.collectTypeVars(t))(typeApps))
                        hydra.lib.logic.ifElse[Seq[hydra.core.Type]](hydra.lib.logic.not(hydra.lib.sets.`null`[hydra.core.Name](hydra.lib.sets.difference[hydra.core.Name](allVars)(inScope))))(Seq())(hydra.lib.logic.ifElse[Seq[hydra.core.Type]](hydra.lib.sets.`null`[hydra.core.Name](hydra.lib.sets.difference[hydra.core.Name](allVars)(trusted)))(typeApps)(Seq()))
                      })
                      hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Type], hydra.java.syntax.Expression](hydra.lib.logic.ifElse[Either[hydra.errors.Error,
                         Seq[hydra.core.Type]]](hydra.lib.lists.`null`[hydra.core.Type](filteredTypeApps))(Right(Seq()))(hydra.java.coder.correctTypeApps(g)(v_Term_variable_name)(hargs)(filteredTypeApps)(cx)(g)))((safeTypeApps: Seq[hydra.core.Type]) =>
                        hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Type], hydra.java.syntax.Expression](hydra.java.coder.filterPhantomTypeArgs(v_Term_variable_name)(safeTypeApps)(cx)(g))((finalTypeApps: Seq[hydra.core.Type]) =>
                        hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression, hydra.java.syntax.Expression](hydra.java.coder.functionCall(env)(false)(v_Term_variable_name)(hargs)(finalTypeApps)(cx)(g))((initialCall: hydra.java.syntax.Expression) =>
                        hydra.lib.eithers.foldl[hydra.java.syntax.Expression, hydra.core.Term, hydra.errors.Error]((acc: hydra.java.syntax.Expression) =>
                        (h: hydra.core.Term) =>
                        hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression, hydra.java.syntax.Expression](hydra.java.coder.encodeTerm(env)(h)(cx)(g))((jarg: hydra.java.syntax.Expression) => Right(hydra.java.coder.applyJavaArg(acc)(jarg))))(initialCall)(rargs))))
                    }
                  }
                }
              }
            }
          })))
          case _ => hydra.java.coder.encodeApplication_fallback(env)(aliases)(g)(typeApps)(app.function)(app.argument)(cx)(g))
      }
    }
  }))
}

def encodeApplication_fallback(env: hydra.java.environment.JavaEnvironment)(aliases: hydra.java.environment.Aliases)(gr: hydra.graph.Graph)(typeApps: Seq[hydra.core.Type])(lhs: hydra.core.Term)(rhs: hydra.core.Term)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.Expression] =
  hydra.lib.eithers.bind[hydra.errors.Error, Option[hydra.core.Type], hydra.java.syntax.Expression](hydra.lib.eithers.bimap[hydra.errors.DecodingError,
     Option[hydra.core.Type], hydra.errors.Error, Option[hydra.core.Type]]((__de: hydra.errors.DecodingError) => hydra.errors.Error.other(__de))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(hydra.annotations.termAnnotationInternal(lhs))))((mt: Option[hydra.core.Type]) =>
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Type, hydra.java.syntax.Expression](hydra.lib.maybes.cases[hydra.core.Type,
     Either[hydra.errors.Error, hydra.core.Type]](mt)(hydra.checking.typeOfTerm(cx)(g)(lhs))((typ: hydra.core.Type) => Right(typ)))((t: hydra.core.Type) =>
  hydra.strip.deannotateTypeParameters(hydra.strip.deannotateType(t)) match
  case hydra.core.Type.function(v_Type_function_ft) => {
    lazy val dom: hydra.core.Type = (v_Type_function_ft.domain)
    {
      lazy val cod: hydra.core.Type = (v_Type_function_ft.codomain)
      {
        lazy val defaultExpr: Either[hydra.errors.Error, hydra.java.syntax.Expression] = hydra.lib.eithers.bind[hydra.errors.Error,
           hydra.java.syntax.Expression, hydra.java.syntax.Expression](hydra.java.coder.encodeTerm(env)(lhs)(cx)(g))((jfun: hydra.java.syntax.Expression) =>
          hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression, hydra.java.syntax.Expression](hydra.java.coder.encodeTerm(env)(rhs)(cx)(g))((jarg: hydra.java.syntax.Expression) => Right(hydra.java.coder.applyJavaArg(jfun)(jarg))))
        {
          lazy val elimBranch: Either[hydra.errors.Error, hydra.java.syntax.Expression] = hydra.lib.eithers.bind[hydra.errors.Error,
             hydra.java.syntax.Expression, hydra.java.syntax.Expression](hydra.java.coder.encodeTerm(env)(rhs)(cx)(g))((jarg: hydra.java.syntax.Expression) =>
            hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Type, hydra.java.syntax.Expression](hydra.lib.logic.ifElse[Either[hydra.errors.Error,
               hydra.core.Type]](hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.java.syntax.TypeArgument](hydra.java.coder.javaTypeArgumentsForType(dom))))(Right(dom))(hydra.lib.eithers.bind[hydra.errors.Error,
               Option[hydra.core.Type], hydra.core.Type](hydra.lib.eithers.bimap[hydra.errors.DecodingError,
               Option[hydra.core.Type], hydra.errors.Error, Option[hydra.core.Type]]((__de: hydra.errors.DecodingError) => hydra.errors.Error.other(__de))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(hydra.annotations.termAnnotationInternal(rhs))))((mrt: Option[hydra.core.Type]) =>
            hydra.lib.maybes.cases[hydra.core.Type, Either[hydra.errors.Error, hydra.core.Type]](mrt)(hydra.lib.eithers.bind[hydra.errors.Error,
               hydra.core.Type, hydra.core.Type](hydra.checking.typeOfTerm(cx)(g)(rhs))((rt: hydra.core.Type) =>
            Right(hydra.lib.logic.ifElse[hydra.core.Type](hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.java.syntax.TypeArgument](hydra.java.coder.javaTypeArgumentsForType(rt))))(rt)(dom))))((rt: hydra.core.Type) =>
            Right(hydra.lib.logic.ifElse[hydra.core.Type](hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.java.syntax.TypeArgument](hydra.java.coder.javaTypeArgumentsForType(rt))))(rt)(dom))))))((enrichedDom: hydra.core.Type) =>
            hydra.java.coder.encodeElimination(env)(Some(jarg))(enrichedDom)(cod)(hydra.strip.deannotateTerm(lhs))(cx)(g)))
          hydra.strip.deannotateTerm(lhs) match
            case hydra.core.Term.project(v_Term_project__p) => elimBranch
            case hydra.core.Term.cases(v_Term_cases__c) => elimBranch
            case hydra.core.Term.unwrap(v_Term_unwrap__w) => elimBranch
            case _ => defaultExpr
        }
      }
    }
  }
  case _ => hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression, hydra.java.syntax.Expression](hydra.java.coder.encodeTerm(env)(lhs)(cx)(g))((jfun: hydra.java.syntax.Expression) =>
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression, hydra.java.syntax.Expression](hydra.java.coder.encodeTerm(env)(rhs)(cx)(g))((jarg: hydra.java.syntax.Expression) => Right(hydra.java.coder.applyJavaArg(jfun)(jarg))))))

def encodeDefinitions(mod: hydra.packaging.Module)(defs: Seq[hydra.packaging.Definition])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   Map[hydra.core.Name, hydra.java.syntax.CompilationUnit]] =
  {
  lazy val aliases: hydra.java.environment.Aliases = hydra.java.utils.importAliasesForModule(mod)
  lazy val env: hydra.java.environment.JavaEnvironment = hydra.java.environment.JavaEnvironment(aliases, g)
  lazy val pkg: hydra.java.syntax.PackageDeclaration = hydra.java.utils.javaPackageDeclaration(mod.namespace)
  lazy val partitioned: Tuple2[Seq[hydra.packaging.TypeDefinition], Seq[hydra.packaging.TermDefinition]] = hydra.environment.partitionDefinitions(defs)
  lazy val typeDefs: Seq[hydra.packaging.TypeDefinition] = hydra.lib.pairs.first[Seq[hydra.packaging.TypeDefinition],
     Seq[hydra.packaging.TermDefinition]](partitioned)
  lazy val termDefs: Seq[hydra.packaging.TermDefinition] = hydra.lib.pairs.second[Seq[hydra.packaging.TypeDefinition],
     Seq[hydra.packaging.TermDefinition]](partitioned)
  lazy val nonTypedefDefs: Seq[hydra.packaging.TypeDefinition] = hydra.lib.lists.filter[hydra.packaging.TypeDefinition]((td: hydra.packaging.TypeDefinition) =>
    {
    lazy val typ: hydra.core.Type = (td.`type`.`type`)
    hydra.java.coder.isSerializableJavaType(typ)
  })(typeDefs)
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[Tuple2[hydra.core.Name, hydra.java.syntax.CompilationUnit]],
     Map[hydra.core.Name, hydra.java.syntax.CompilationUnit]](hydra.lib.eithers.mapList[hydra.packaging.TypeDefinition,
     Tuple2[hydra.core.Name, hydra.java.syntax.CompilationUnit], hydra.errors.Error]((td: hydra.packaging.TypeDefinition) =>
    hydra.java.coder.encodeTypeDefinition(pkg)(aliases)(td)(cx)(g))(nonTypedefDefs))((typeUnits: Seq[Tuple2[hydra.core.Name,
       hydra.java.syntax.CompilationUnit]]) =>
    hydra.lib.eithers.bind[hydra.errors.Error, Seq[Tuple2[hydra.core.Name, hydra.java.syntax.CompilationUnit]],
       Map[hydra.core.Name, hydra.java.syntax.CompilationUnit]](hydra.lib.logic.ifElse[Either[hydra.errors.Error,
       Seq[Tuple2[hydra.core.Name, hydra.java.syntax.CompilationUnit]]]](hydra.lib.lists.`null`[hydra.packaging.TermDefinition](termDefs))(Right(Seq()))(hydra.lib.eithers.bind[hydra.errors.Error,
       Seq[hydra.java.syntax.InterfaceMemberDeclaration], Seq[Tuple2[hydra.core.Name, hydra.java.syntax.CompilationUnit]]](hydra.lib.eithers.mapList[hydra.packaging.TermDefinition,
       hydra.java.syntax.InterfaceMemberDeclaration, hydra.errors.Error]((td: hydra.packaging.TermDefinition) => hydra.java.coder.encodeTermDefinition(env)(td)(cx)(g))(termDefs))((dataMembers: Seq[hydra.java.syntax.InterfaceMemberDeclaration]) =>
    Right(Seq(hydra.java.coder.constructElementsInterface(mod)(dataMembers))))))((termUnits: Seq[Tuple2[hydra.core.Name, hydra.java.syntax.CompilationUnit]]) =>
    Right(hydra.lib.maps.fromList[hydra.core.Name, hydra.java.syntax.CompilationUnit](hydra.lib.lists.concat2[Tuple2[hydra.core.Name,
       hydra.java.syntax.CompilationUnit]](typeUnits)(termUnits)))))
}

def encodeElimination(env: hydra.java.environment.JavaEnvironment)(marg: Option[hydra.java.syntax.Expression])(dom: hydra.core.Type)(cod: hydra.core.Type)(elimTerm: hydra.core.Term)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.Expression] =
  {
  lazy val aliases: hydra.java.environment.Aliases = (env.aliases)
  hydra.strip.deannotateTerm(elimTerm) match
    case hydra.core.Term.project(v_Term_project_proj) => {
      lazy val fname: hydra.core.Name = (v_Term_project_proj.field)
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type, hydra.java.syntax.Expression](hydra.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(dom)(cx)(g))((jdom0: hydra.java.syntax.Type) =>
        hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType, hydra.java.syntax.Expression](hydra.java.utils.javaTypeToJavaReferenceType(jdom0)(cx))((jdomr: hydra.java.syntax.ReferenceType) =>
        hydra.lib.maybes.cases[hydra.java.syntax.Expression, Either[hydra.errors.Error, hydra.java.syntax.Expression]](marg)({
        lazy val projVar: hydra.core.Name = "projected"
        {
          lazy val jbody: hydra.java.syntax.Expression = hydra.java.utils.javaExpressionNameToJavaExpression(hydra.java.utils.fieldExpression(hydra.java.utils.variableToJavaIdentifier(projVar))(hydra.java.utils.javaIdentifier(fname)))
          Right(hydra.java.utils.javaLambda(projVar)(jbody))
        }
      })((jarg: hydra.java.syntax.Expression) =>
        {
        lazy val qual: hydra.java.syntax.FieldAccess_Qualifier = hydra.java.syntax.FieldAccess_Qualifier.primary(hydra.java.utils.javaExpressionToJavaPrimary(jarg))
        Right(hydra.java.utils.javaFieldAccessToJavaExpression(hydra.java.syntax.FieldAccess(qual, hydra.java.utils.javaIdentifier(fname))))
      })))
    }
    case hydra.core.Term.cases(v_Term_cases_cs) => {
      lazy val tname: hydra.core.Name = (v_Term_cases_cs.typeName)
      {
        lazy val `def_`: Option[hydra.core.Term] = (v_Term_cases_cs.default)
        {
          lazy val fields: Seq[hydra.core.Field] = (v_Term_cases_cs.cases)
          hydra.lib.maybes.cases[hydra.java.syntax.Expression, Either[hydra.errors.Error, hydra.java.syntax.Expression]](marg)({
            lazy val uVar: hydra.core.Name = "u"
            {
              lazy val typedLambda: hydra.core.Term = hydra.core.Term.lambda(hydra.core.Lambda(uVar, Some(dom),
                 hydra.core.Term.application(hydra.core.Application(elimTerm, hydra.core.Term.variable(uVar)))))
              hydra.java.coder.encodeTerm(env)(typedLambda)(cx)(g)
            }
          })((jarg: hydra.java.syntax.Expression) =>
            {
            lazy val prim: hydra.java.syntax.Primary = hydra.java.utils.javaExpressionToJavaPrimary(jarg)
            {
              lazy val consId: hydra.java.syntax.Identifier = hydra.java.coder.innerClassRef(aliases)(tname)(hydra.java.names.partialVisitorName)
              {
                lazy val effectiveCod: hydra.core.Type = cod
                hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type, hydra.java.syntax.Expression](hydra.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(effectiveCod)(cx)(g))((jcod: hydra.java.syntax.Type) =>
                  hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType, hydra.java.syntax.Expression](hydra.java.utils.javaTypeToJavaReferenceType(jcod)(cx))((rt: hydra.java.syntax.ReferenceType) =>
                  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.TypeArgument], hydra.java.syntax.Expression](hydra.java.coder.domTypeArgs(aliases)(dom)(cx)(g))((domArgs: Seq[hydra.java.syntax.TypeArgument]) =>
                  {
                  lazy val targs: hydra.java.syntax.TypeArgumentsOrDiamond = hydra.java.coder.typeArgsOrDiamond(hydra.lib.lists.concat2[hydra.java.syntax.TypeArgument](domArgs)(Seq(hydra.java.syntax.TypeArgument.reference(rt))))
                  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.ClassBodyDeclarationWithComments],
                     hydra.java.syntax.Expression](hydra.lib.maybes.cases[hydra.core.Term, Either[hydra.errors.Error,
                     Seq[hydra.java.syntax.ClassBodyDeclarationWithComments]]](`def_`)(Right(Seq()))((d: hydra.core.Term) =>
                    hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ClassBodyDeclarationWithComments,
                       Seq[hydra.java.syntax.ClassBodyDeclarationWithComments]](hydra.java.coder.otherwiseBranch(env)(aliases)(dom)(cod)(tname)(jcod)(domArgs)(d)(cx)(g))((b: hydra.java.syntax.ClassBodyDeclarationWithComments) => Right(Seq(b)))))((otherwiseBranches: Seq[hydra.java.syntax.ClassBodyDeclarationWithComments]) =>
                    hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.ClassBodyDeclarationWithComments],
                       hydra.java.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Field, hydra.java.syntax.ClassBodyDeclarationWithComments,
                       hydra.errors.Error]((f: hydra.core.Field) =>
                    hydra.java.coder.visitBranch(env)(aliases)(dom)(tname)(jcod)(domArgs)(f)(cx)(g))(fields))((visitBranches: Seq[hydra.java.syntax.ClassBodyDeclarationWithComments]) =>
                    {
                    lazy val body: hydra.java.syntax.ClassBody = hydra.lib.lists.concat2[hydra.java.syntax.ClassBodyDeclarationWithComments](otherwiseBranches)(visitBranches)
                    {
                      lazy val visitor: hydra.java.syntax.Expression = hydra.java.utils.javaConstructorCall(hydra.java.utils.javaConstructorName(consId)(Some(targs)))(Seq())(Some(body))
                      Right(hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocation(Some(Right(prim)))(hydra.java.names.acceptMethodName)(Seq(visitor))))
                    }
                  }))
                })))
              }
            }
          })
        }
      }
    }
    case hydra.core.Term.unwrap(v_Term_unwrap_wrapName) => {
      def withArg(ja: hydra.java.syntax.Expression): hydra.java.syntax.Expression =
        hydra.java.utils.javaFieldAccessToJavaExpression(hydra.java.syntax.FieldAccess(hydra.java.syntax.FieldAccess_Qualifier.primary(hydra.java.utils.javaExpressionToJavaPrimary(ja)),
           hydra.java.utils.javaIdentifier(hydra.java.names.valueFieldName)))
      Right(hydra.lib.maybes.cases[hydra.java.syntax.Expression, hydra.java.syntax.Expression](marg)({
        lazy val wVar: hydra.core.Name = "wrapped"
        {
          lazy val wArg: hydra.java.syntax.Expression = hydra.java.utils.javaIdentifierToJavaExpression(hydra.java.utils.variableToJavaIdentifier(wVar))
          hydra.java.utils.javaLambda(wVar)(withArg(wArg))
        }
      })((jarg: hydra.java.syntax.Expression) => withArg(jarg)))
    }
    case _ => Left(hydra.errors.Error.other(hydra.lib.strings.cat2("unexpected ")(hydra.lib.strings.cat2("elimination case")(hydra.lib.strings.cat2(" in ")("encodeElimination")))))
}

def encodeFunction(env: hydra.java.environment.JavaEnvironment)(dom: hydra.core.Type)(cod: hydra.core.Type)(funTerm: hydra.core.Term)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.Expression] =
  {
  lazy val aliases: hydra.java.environment.Aliases = (env.aliases)
  def encodeLambdaFallback(env2: hydra.java.environment.JavaEnvironment)(lam: hydra.core.Lambda): Either[hydra.errors.Error, hydra.java.syntax.Expression] =
    {
    lazy val lambdaVar: hydra.core.Name = (lam.parameter)
    lazy val body: hydra.core.Term = (lam.body)
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.FunctionStructure[hydra.java.environment.JavaEnvironment],
       hydra.java.syntax.Expression](hydra.java.coder.analyzeJavaFunction(env2)(body)(cx)(g))((fs: hydra.typing.FunctionStructure[hydra.java.environment.JavaEnvironment]) =>
      {
      lazy val bindings: Seq[hydra.core.Binding] = (fs.bindings)
      {
        lazy val innerBody: hydra.core.Term = (fs.body)
        {
          lazy val env3: hydra.java.environment.JavaEnvironment = (fs.environment)
          hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[Seq[hydra.java.syntax.BlockStatement], hydra.java.environment.JavaEnvironment],
             hydra.java.syntax.Expression](hydra.java.coder.bindingsToStatements(env3)(bindings)(cx)(g))((bindResult: Tuple2[Seq[hydra.java.syntax.BlockStatement],
             hydra.java.environment.JavaEnvironment]) =>
            {
            lazy val bindingStmts: Seq[hydra.java.syntax.BlockStatement] = hydra.lib.pairs.first[Seq[hydra.java.syntax.BlockStatement],
               hydra.java.environment.JavaEnvironment](bindResult)
            {
              lazy val env4: hydra.java.environment.JavaEnvironment = hydra.lib.pairs.second[Seq[hydra.java.syntax.BlockStatement],
                 hydra.java.environment.JavaEnvironment](bindResult)
              hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression, hydra.java.syntax.Expression](hydra.java.coder.encodeTerm(env4)(innerBody)(cx)(g))((jbody: hydra.java.syntax.Expression) =>
                {
                lazy val lam1: hydra.java.syntax.Expression = hydra.lib.logic.ifElse[hydra.java.syntax.Expression](hydra.lib.lists.`null`[hydra.core.Binding](bindings))(hydra.java.utils.javaLambda(lambdaVar)(jbody))({
                  lazy val returnSt: hydra.java.syntax.BlockStatement = hydra.java.syntax.BlockStatement.statement(hydra.java.utils.javaReturnStatement(Some(jbody)))
                  hydra.java.utils.javaLambdaFromBlock(lambdaVar)(hydra.lib.lists.concat2[hydra.java.syntax.BlockStatement](bindingStmts)(Seq(returnSt)))
                })
                hydra.java.coder.applyCastIfSafe(aliases)(hydra.core.Type.function(hydra.core.FunctionType(dom, cod)))(lam1)(cx)(g)
              })
            }
          })
        }
      }
    })
  }
  hydra.strip.deannotateTerm(funTerm) match
    case hydra.core.Term.project(v_Term_project__p) => hydra.java.coder.encodeElimination(env)(None)(dom)(cod)(hydra.strip.deannotateTerm(funTerm))(cx)(g)
    case hydra.core.Term.cases(v_Term_cases__c) => hydra.java.coder.encodeElimination(env)(None)(dom)(cod)(hydra.strip.deannotateTerm(funTerm))(cx)(g)
    case hydra.core.Term.unwrap(v_Term_unwrap__w) => hydra.java.coder.encodeElimination(env)(None)(dom)(cod)(hydra.strip.deannotateTerm(funTerm))(cx)(g)
    case hydra.core.Term.lambda(v_Term_lambda_lam) => hydra.java.coder.withLambda(env)(v_Term_lambda_lam)((env2: hydra.java.environment.JavaEnvironment) =>
      {
      lazy val lambdaVar: hydra.core.Name = (v_Term_lambda_lam.parameter)
      {
        lazy val body: hydra.core.Term = (v_Term_lambda_lam.body)
        hydra.strip.deannotateTerm(body) match
          case hydra.core.Term.lambda(v_Term_lambda_innerLam) => hydra.strip.deannotateType(cod) match
            case hydra.core.Type.function(v_Type_function_ft) => {
              lazy val dom2: hydra.core.Type = (v_Type_function_ft.domain)
              {
                lazy val cod2: hydra.core.Type = (v_Type_function_ft.codomain)
                hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression, hydra.java.syntax.Expression](hydra.java.coder.encodeFunction(env2)(dom2)(cod2)(hydra.core.Term.lambda(v_Term_lambda_innerLam))(cx)(g))((innerJavaLambda: hydra.java.syntax.Expression) =>
                  {
                  lazy val lam1: hydra.java.syntax.Expression = hydra.java.utils.javaLambda(lambdaVar)(innerJavaLambda)
                  hydra.java.coder.applyCastIfSafe(aliases)(hydra.core.Type.function(hydra.core.FunctionType(dom, cod)))(lam1)(cx)(g)
                })
              }
            }
            case _ => Left(hydra.errors.Error.other(hydra.lib.strings.cat2("expected function type for lambda body, but got: ")(hydra.show.core.`type`(cod))))
          case _ => encodeLambdaFallback(env2)(v_Term_lambda_lam)
      }
    })
    case _ => Right(hydra.java.coder.encodeLiteral(hydra.core.Literal.string(hydra.lib.strings.cat2("Unimplemented function variant: ")(hydra.show.core.term(funTerm)))))
}

def encodeFunctionFormTerm(env: hydra.java.environment.JavaEnvironment)(anns: Seq[Map[hydra.core.Name,
   hydra.core.Term]])(term: hydra.core.Term)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.Expression] =
  {
  lazy val combinedAnns: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.lists.foldl[Map[hydra.core.Name,
     hydra.core.Term], Map[hydra.core.Name, hydra.core.Term]]((acc: Map[hydra.core.Name, hydra.core.Term]) =>
    (m: Map[hydra.core.Name, hydra.core.Term]) =>
    hydra.lib.maps.union[hydra.core.Name, hydra.core.Term](acc)(m))(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Term])(anns)
  hydra.lib.eithers.bind[hydra.errors.Error, Option[hydra.core.Type], hydra.java.syntax.Expression](hydra.lib.eithers.bimap[hydra.errors.DecodingError,
     Option[hydra.core.Type], hydra.errors.Error, Option[hydra.core.Type]]((__de: hydra.errors.DecodingError) => hydra.errors.Error.other(__de))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(combinedAnns)))((mt: Option[hydra.core.Type]) =>
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Type, hydra.java.syntax.Expression](hydra.lib.maybes.cases[hydra.core.Type,
       Either[hydra.errors.Error, hydra.core.Type]](mt)(hydra.lib.maybes.cases[hydra.core.Type, Either[hydra.errors.Error,
       hydra.core.Type]](hydra.java.coder.tryInferFunctionType(term))(hydra.checking.typeOfTerm(cx)(g)(term))((inferredType: hydra.core.Type) => Right(inferredType)))((t: hydra.core.Type) => Right(t)))((typ: hydra.core.Type) =>
    hydra.strip.deannotateType(typ) match
    case hydra.core.Type.function(v_Type_function_ft) => hydra.java.coder.encodeFunction(env)(v_Type_function_ft.domain)(v_Type_function_ft.codomain)(term)(cx)(g)
    case _ => hydra.java.coder.encodeNullaryConstant(env)(typ)(term)(cx)(g)))
}

def encodeFunctionPrimitiveByName[T0](env: hydra.java.environment.JavaEnvironment)(dom: hydra.core.Type)(cod: hydra.core.Type)(name: hydra.core.Name)(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.Expression] =
  {
  lazy val aliases: hydra.java.environment.Aliases = (env.aliases)
  lazy val classWithApply: scala.Predef.String = hydra.java.coder.elementJavaIdentifier(true)(false)(aliases)(name)
  lazy val suffix: scala.Predef.String = hydra.lib.strings.cat2(".")(hydra.java.names.applyMethodName)
  lazy val className: scala.Predef.String = hydra.lib.strings.fromList(hydra.lib.lists.take[Int](hydra.lib.math.sub(hydra.lib.strings.length(classWithApply))(hydra.lib.strings.length(suffix)))(hydra.lib.strings.toList(classWithApply)))
  lazy val arity: Int = hydra.arity.typeArity(hydra.core.Type.function(hydra.core.FunctionType(dom, cod)))
  hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.java.syntax.Expression]](hydra.lib.equality.lte[Int](arity)(1))(Right(hydra.java.utils.javaIdentifierToJavaExpression(hydra.lib.strings.cat(Seq(className,
     "::", hydra.java.names.applyMethodName)))))({
    lazy val paramNames: Seq[hydra.core.Name] = hydra.lib.lists.map[Int, hydra.core.Name]((i: Int) => hydra.lib.strings.cat2("p")(hydra.lib.literals.showInt32(i)))(hydra.lib.math.range(0)(hydra.lib.math.sub(arity)(1)))
    {
      lazy val paramExprs: Seq[hydra.java.syntax.Expression] = hydra.lib.lists.map[hydra.core.Name, hydra.java.syntax.Expression]((p: hydra.core.Name) =>
        hydra.java.utils.javaIdentifierToJavaExpression(hydra.java.utils.variableToJavaIdentifier(p)))(paramNames)
      {
        lazy val classId: hydra.java.syntax.Identifier = className
        {
          lazy val call: hydra.java.syntax.Expression = hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocationStatic(classId)(hydra.java.names.applyMethodName)(paramExprs))
          {
            lazy val curried: hydra.java.syntax.Expression = hydra.java.coder.buildCurriedLambda(paramNames)(call)
            hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type, hydra.java.syntax.Expression](hydra.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(hydra.core.Type.function(hydra.core.FunctionType(dom,
               cod)))(cx)(g))((jtype: hydra.java.syntax.Type) =>
              hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType, hydra.java.syntax.Expression](hydra.java.utils.javaTypeToJavaReferenceType(jtype)(cx))((rt: hydra.java.syntax.ReferenceType) =>
              Right(hydra.java.utils.javaCastExpressionToJavaExpression(hydra.java.utils.javaCastExpression(rt)(hydra.java.utils.javaExpressionToJavaUnaryExpression(curried))))))
          }
        }
      }
    }
  })
}

def encodeLiteral(lit: hydra.core.Literal): hydra.java.syntax.Expression =
  lit match
  case hydra.core.Literal.binary(v_Literal_binary_bs) => {
    lazy val byteValues: Seq[Int] = hydra.lib.literals.binaryToBytes(v_Literal_binary_bs)
    hydra.java.utils.javaArrayCreation(hydra.java.utils.javaBytePrimitiveType)(Some(hydra.java.utils.javaArrayInitializer(hydra.lib.lists.map[Int,
       hydra.java.syntax.Expression]((w: Int) =>
      hydra.java.utils.javaLiteralToJavaExpression(hydra.java.syntax.Literal.integer(hydra.lib.literals.int32ToBigint(w))))(byteValues))))
  }
  case hydra.core.Literal.boolean(v_Literal_boolean_b) => hydra.java.coder.encodeLiteral_litExp(hydra.java.utils.javaBoolean(v_Literal_boolean_b))
  case hydra.core.Literal.decimal(v_Literal_decimal_v) => hydra.java.utils.javaConstructorCall(hydra.java.utils.javaConstructorName("java.math.BigDecimal")(None))(Seq(hydra.java.coder.encodeLiteral(hydra.core.Literal.string(hydra.lib.literals.showDecimal(v_Literal_decimal_v)))))(None)
  case hydra.core.Literal.float(v_Literal_float_f) => hydra.java.coder.encodeLiteral_encodeFloat(v_Literal_float_f)
  case hydra.core.Literal.integer(v_Literal_integer_i) => hydra.java.coder.encodeLiteral_encodeInteger(v_Literal_integer_i)
  case hydra.core.Literal.string(v_Literal_string_s) => hydra.java.coder.encodeLiteral_litExp(hydra.java.utils.javaString(v_Literal_string_s))

def encodeLiteralType[T0, T1, T2](lt: hydra.core.LiteralType)(cx: T0)(g: T1): Either[T2, hydra.java.syntax.Type] =
  lt match
  case hydra.core.LiteralType.binary => Right(hydra.java.syntax.Type.reference(hydra.java.syntax.ReferenceType.array(hydra.java.syntax.ArrayType(Seq(Seq()),
     hydra.java.syntax.ArrayType_Variant.primitive(hydra.java.syntax.PrimitiveTypeWithAnnotations(hydra.java.syntax.PrimitiveType.numeric(hydra.java.syntax.NumericType.integral(hydra.java.syntax.IntegralType.byte)),
     Seq()))))))
  case hydra.core.LiteralType.boolean => hydra.java.coder.encodeLiteralType_simple("Boolean")(cx)(g)
  case hydra.core.LiteralType.decimal => Right(hydra.java.utils.javaRefType(Seq())(Some(hydra.java.names.javaPackageName(Seq("java", "math"))))("BigDecimal"))
  case hydra.core.LiteralType.float(v_LiteralType_float_ft) => v_LiteralType_float_ft match
    case hydra.core.FloatType.bigfloat => Right(hydra.java.utils.javaRefType(Seq())(Some(hydra.java.names.javaPackageName(Seq("java", "math"))))("BigDecimal"))
    case hydra.core.FloatType.float32 => hydra.java.coder.encodeLiteralType_simple("Float")(cx)(g)
    case hydra.core.FloatType.float64 => hydra.java.coder.encodeLiteralType_simple("Double")(cx)(g)
  case hydra.core.LiteralType.integer(v_LiteralType_integer_it) => v_LiteralType_integer_it match
    case hydra.core.IntegerType.bigint => Right(hydra.java.utils.javaRefType(Seq())(Some(hydra.java.names.javaPackageName(Seq("java", "math"))))("BigInteger"))
    case hydra.core.IntegerType.int8 => hydra.java.coder.encodeLiteralType_simple("Byte")(cx)(g)
    case hydra.core.IntegerType.int16 => hydra.java.coder.encodeLiteralType_simple("Short")(cx)(g)
    case hydra.core.IntegerType.int32 => hydra.java.coder.encodeLiteralType_simple("Integer")(cx)(g)
    case hydra.core.IntegerType.int64 => hydra.java.coder.encodeLiteralType_simple("Long")(cx)(g)
    case hydra.core.IntegerType.uint8 => hydra.java.coder.encodeLiteralType_simple("Short")(cx)(g)
    case hydra.core.IntegerType.uint16 => hydra.java.coder.encodeLiteralType_simple("Character")(cx)(g)
    case hydra.core.IntegerType.uint32 => hydra.java.coder.encodeLiteralType_simple("Long")(cx)(g)
    case hydra.core.IntegerType.uint64 => Right(hydra.java.utils.javaRefType(Seq())(Some(hydra.java.names.javaPackageName(Seq("java", "math"))))("BigInteger"))
  case hydra.core.LiteralType.string => hydra.java.coder.encodeLiteralType_simple("String")(cx)(g)

def encodeLiteralType_simple[T0, T1, T2](n: scala.Predef.String)(cx: T0)(g: T1): Either[T2, hydra.java.syntax.Type] = Right(hydra.java.utils.javaRefType(Seq())(None)(n))

def encodeLiteral_encodeFloat(f: hydra.core.FloatValue): hydra.java.syntax.Expression =
  f match
  case hydra.core.FloatValue.bigfloat(v_FloatValue_bigfloat_v) => hydra.java.utils.javaConstructorCall(hydra.java.utils.javaConstructorName("java.math.BigDecimal")(None))(Seq(hydra.java.coder.encodeLiteral(hydra.core.Literal.string(hydra.lib.literals.showBigfloat(v_FloatValue_bigfloat_v)))))(None)
  case hydra.core.FloatValue.float32(v_FloatValue_float32_v) => hydra.java.coder.encodeLiteral_encodeFloat32(v_FloatValue_float32_v)
  case hydra.core.FloatValue.float64(v_FloatValue_float64_v) => hydra.java.coder.encodeLiteral_encodeFloat64(v_FloatValue_float64_v)

def encodeLiteral_encodeFloat32(v: Float): hydra.java.syntax.Expression =
  {
  lazy val s: scala.Predef.String = hydra.lib.literals.showFloat32(v)
  hydra.lib.logic.ifElse[hydra.java.syntax.Expression](hydra.lib.equality.equal[scala.Predef.String](s)("NaN"))(hydra.java.coder.encodeLiteral_javaSpecialFloatExpr("Float")("NaN"))(hydra.lib.logic.ifElse[hydra.java.syntax.Expression](hydra.lib.equality.equal[scala.Predef.String](s)("Infinity"))(hydra.java.coder.encodeLiteral_javaSpecialFloatExpr("Float")("POSITIVE_INFINITY"))(hydra.lib.logic.ifElse[hydra.java.syntax.Expression](hydra.lib.equality.equal[scala.Predef.String](s)("-Infinity"))(hydra.java.coder.encodeLiteral_javaSpecialFloatExpr("Float")("NEGATIVE_INFINITY"))(hydra.java.coder.encodeLiteral_primCast(hydra.java.syntax.PrimitiveType.numeric(hydra.java.syntax.NumericType.floatingPoint(hydra.java.syntax.FloatingPointType.float)))(hydra.java.coder.encodeLiteral_litExp(hydra.java.syntax.Literal.floatingPoint(hydra.lib.literals.float32ToBigfloat(v)))))))
}

def encodeLiteral_encodeFloat64(v: Double): hydra.java.syntax.Expression =
  {
  lazy val s: scala.Predef.String = hydra.lib.literals.showFloat64(v)
  hydra.lib.logic.ifElse[hydra.java.syntax.Expression](hydra.lib.equality.equal[scala.Predef.String](s)("NaN"))(hydra.java.coder.encodeLiteral_javaSpecialFloatExpr("Double")("NaN"))(hydra.lib.logic.ifElse[hydra.java.syntax.Expression](hydra.lib.equality.equal[scala.Predef.String](s)("Infinity"))(hydra.java.coder.encodeLiteral_javaSpecialFloatExpr("Double")("POSITIVE_INFINITY"))(hydra.lib.logic.ifElse[hydra.java.syntax.Expression](hydra.lib.equality.equal[scala.Predef.String](s)("-Infinity"))(hydra.java.coder.encodeLiteral_javaSpecialFloatExpr("Double")("NEGATIVE_INFINITY"))(hydra.lib.logic.ifElse[hydra.java.syntax.Expression](hydra.lib.equality.equal[scala.Predef.String](s)("-0.0"))(hydra.java.coder.encodeLiteral_javaParseDouble("-0.0"))(hydra.java.coder.encodeLiteral_litExp(hydra.java.syntax.Literal.floatingPoint(hydra.lib.literals.float64ToBigfloat(v)))))))
}

def encodeLiteral_encodeInteger(i: hydra.core.IntegerValue): hydra.java.syntax.Expression =
  i match
  case hydra.core.IntegerValue.bigint(v_IntegerValue_bigint_v) => hydra.java.utils.javaConstructorCall(hydra.java.utils.javaConstructorName("java.math.BigInteger")(None))(Seq(hydra.java.coder.encodeLiteral(hydra.core.Literal.string(hydra.lib.literals.showBigint(v_IntegerValue_bigint_v)))))(None)
  case hydra.core.IntegerValue.int8(v_IntegerValue_int8_v) => hydra.java.coder.encodeLiteral_primCast(hydra.java.syntax.PrimitiveType.numeric(hydra.java.syntax.NumericType.integral(hydra.java.syntax.IntegralType.byte)))(hydra.java.coder.encodeLiteral_litExp(hydra.java.syntax.Literal.integer(hydra.lib.literals.int8ToBigint(v_IntegerValue_int8_v))))
  case hydra.core.IntegerValue.int16(v_IntegerValue_int16_v) => hydra.java.coder.encodeLiteral_primCast(hydra.java.syntax.PrimitiveType.numeric(hydra.java.syntax.NumericType.integral(hydra.java.syntax.IntegralType.short)))(hydra.java.coder.encodeLiteral_litExp(hydra.java.syntax.Literal.integer(hydra.lib.literals.int16ToBigint(v_IntegerValue_int16_v))))
  case hydra.core.IntegerValue.int32(v_IntegerValue_int32_v) => hydra.java.coder.encodeLiteral_litExp(hydra.java.syntax.Literal.integer(hydra.lib.literals.int32ToBigint(v_IntegerValue_int32_v)))
  case hydra.core.IntegerValue.int64(v_IntegerValue_int64_v) => hydra.java.coder.encodeLiteral_primCast(hydra.java.syntax.PrimitiveType.numeric(hydra.java.syntax.NumericType.integral(hydra.java.syntax.IntegralType.long)))(hydra.java.coder.encodeLiteral_litExp(hydra.java.syntax.Literal.integer(hydra.lib.literals.int64ToBigint(v_IntegerValue_int64_v))))
  case hydra.core.IntegerValue.uint8(v_IntegerValue_uint8_v) => hydra.java.coder.encodeLiteral_primCast(hydra.java.syntax.PrimitiveType.numeric(hydra.java.syntax.NumericType.integral(hydra.java.syntax.IntegralType.short)))(hydra.java.coder.encodeLiteral_litExp(hydra.java.syntax.Literal.integer(hydra.lib.literals.uint8ToBigint(v_IntegerValue_uint8_v))))
  case hydra.core.IntegerValue.uint16(v_IntegerValue_uint16_v) => hydra.java.coder.encodeLiteral_litExp(hydra.java.syntax.Literal.character(v_IntegerValue_uint16_v))
  case hydra.core.IntegerValue.uint32(v_IntegerValue_uint32_v) => hydra.java.coder.encodeLiteral_primCast(hydra.java.syntax.PrimitiveType.numeric(hydra.java.syntax.NumericType.integral(hydra.java.syntax.IntegralType.long)))(hydra.java.coder.encodeLiteral_litExp(hydra.java.syntax.Literal.integer(hydra.lib.literals.uint32ToBigint(v_IntegerValue_uint32_v))))
  case hydra.core.IntegerValue.uint64(v_IntegerValue_uint64_v) => hydra.java.utils.javaConstructorCall(hydra.java.utils.javaConstructorName("java.math.BigInteger")(None))(Seq(hydra.java.coder.encodeLiteral(hydra.core.Literal.string(hydra.lib.literals.showBigint(hydra.lib.literals.uint64ToBigint(v_IntegerValue_uint64_v))))))(None)

def encodeLiteral_javaParseDouble(value: scala.Predef.String): hydra.java.syntax.Expression =
  hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocationStatic("Double")("parseDouble")(Seq(hydra.java.coder.encodeLiteral(hydra.core.Literal.string(value)))))

def encodeLiteral_javaSpecialFloatExpr(className: scala.Predef.String)(fieldName: scala.Predef.String): hydra.java.syntax.Expression =
  hydra.java.utils.javaExpressionNameToJavaExpression(hydra.java.syntax.ExpressionName(Some(Seq(className)), fieldName))

def encodeLiteral_litExp(l: hydra.java.syntax.Literal): hydra.java.syntax.Expression = hydra.java.utils.javaLiteralToJavaExpression(l)

def encodeLiteral_primCast(pt: hydra.java.syntax.PrimitiveType)(expr: hydra.java.syntax.Expression): hydra.java.syntax.Expression =
  hydra.java.utils.javaCastExpressionToJavaExpression(hydra.java.utils.javaCastPrimitive(pt)(hydra.java.utils.javaExpressionToJavaUnaryExpression(expr)))

def encodeNullaryConstant[T0, T1, T2, T3, T4](env: T0)(typ: T1)(funTerm: hydra.core.Term)(cx: T2)(g: T3): Either[hydra.errors.Error, T4] =
  Left(hydra.errors.Error.other(hydra.lib.strings.cat2("unexpected ")(hydra.lib.strings.cat2("nullary function")(hydra.lib.strings.cat2(" in ")(hydra.show.core.term(funTerm))))))

def encodeNullaryConstant_typeArgsFromReturnType[T0](aliases: hydra.java.environment.Aliases)(t: hydra.core.Type)(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   Seq[hydra.java.syntax.TypeArgument]] =
  hydra.strip.deannotateType(t) match
  case hydra.core.Type.set(v_Type_set_st) => hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type,
     Seq[hydra.java.syntax.TypeArgument]](hydra.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(v_Type_set_st)(cx)(g))((jst: hydra.java.syntax.Type) =>
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType, Seq[hydra.java.syntax.TypeArgument]](hydra.java.utils.javaTypeToJavaReferenceType(jst)(cx))((rt: hydra.java.syntax.ReferenceType) => Right(Seq(hydra.java.syntax.TypeArgument.reference(rt)))))
  case hydra.core.Type.list(v_Type_list_lt_) => hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type,
     Seq[hydra.java.syntax.TypeArgument]](hydra.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(`v_Type_list_lt_`)(cx)(g))((jlt: hydra.java.syntax.Type) =>
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType, Seq[hydra.java.syntax.TypeArgument]](hydra.java.utils.javaTypeToJavaReferenceType(jlt)(cx))((rt: hydra.java.syntax.ReferenceType) => Right(Seq(hydra.java.syntax.TypeArgument.reference(rt)))))
  case hydra.core.Type.maybe(v_Type_maybe_mt) => hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type,
     Seq[hydra.java.syntax.TypeArgument]](hydra.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(v_Type_maybe_mt)(cx)(g))((jmt: hydra.java.syntax.Type) =>
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType, Seq[hydra.java.syntax.TypeArgument]](hydra.java.utils.javaTypeToJavaReferenceType(jmt)(cx))((rt: hydra.java.syntax.ReferenceType) => Right(Seq(hydra.java.syntax.TypeArgument.reference(rt)))))
  case hydra.core.Type.map(v_Type_map_mp) => hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type,
     Seq[hydra.java.syntax.TypeArgument]](hydra.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(v_Type_map_mp.keys)(cx)(g))((jkt: hydra.java.syntax.Type) =>
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType, Seq[hydra.java.syntax.TypeArgument]](hydra.java.utils.javaTypeToJavaReferenceType(jkt)(cx))((rk: hydra.java.syntax.ReferenceType) =>
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type, Seq[hydra.java.syntax.TypeArgument]](hydra.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(v_Type_map_mp.values)(cx)(g))((jvt: hydra.java.syntax.Type) =>
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType, Seq[hydra.java.syntax.TypeArgument]](hydra.java.utils.javaTypeToJavaReferenceType(jvt)(cx))((rv: hydra.java.syntax.ReferenceType) =>
    Right(Seq(hydra.java.syntax.TypeArgument.reference(rk), hydra.java.syntax.TypeArgument.reference(rv)))))))
  case _ => Right(Seq())

def encodeNullaryPrimitiveByName[T0](env: hydra.java.environment.JavaEnvironment)(typ: hydra.core.Type)(name: hydra.core.Name)(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.Expression] =
  {
  lazy val aliases: hydra.java.environment.Aliases = (env.aliases)
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.TypeArgument], hydra.java.syntax.Expression](hydra.java.coder.encodeNullaryConstant_typeArgsFromReturnType(aliases)(typ)(cx)(g))((targs: Seq[hydra.java.syntax.TypeArgument]) =>
    hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.java.syntax.Expression]](hydra.lib.lists.`null`[hydra.java.syntax.TypeArgument](targs))({
    lazy val header: hydra.java.syntax.MethodInvocation_Header = hydra.java.syntax.MethodInvocation_Header.simple(hydra.java.coder.elementJavaIdentifier(true)(false)(aliases)(name))
    Right(hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.syntax.MethodInvocation(header, Seq())))
  })({
    lazy val fullName: scala.Predef.String = hydra.java.coder.elementJavaIdentifier(true)(false)(aliases)(name)
    {
      lazy val parts: Seq[scala.Predef.String] = hydra.lib.strings.splitOn(".")(fullName)
      {
        lazy val className: hydra.java.syntax.Identifier = hydra.lib.strings.intercalate(".")(hydra.lib.lists.init[scala.Predef.String](parts))
        {
          lazy val methodName: hydra.java.syntax.Identifier = hydra.lib.lists.last[scala.Predef.String](parts)
          Right(hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocationStaticWithTypeArgs(className)(methodName)(targs)(Seq())))
        }
      }
    }
  }))
}

def encodeTerm(env: hydra.java.environment.JavaEnvironment)(term: hydra.core.Term)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.Expression] = hydra.java.coder.encodeTermInternal(env)(Seq())(Seq())(term)(cx)(g)

def encodeTermDefinition(env: hydra.java.environment.JavaEnvironment)(tdef: hydra.packaging.TermDefinition)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.InterfaceMemberDeclaration] =
  {
  lazy val name: hydra.core.Name = (tdef.name)
  lazy val term0: hydra.core.Term = (tdef.term)
  lazy val ts: hydra.core.TypeScheme = hydra.lib.maybes.maybe[hydra.core.TypeScheme, hydra.core.TypeScheme](hydra.core.TypeScheme(Seq(),
     hydra.core.Type.variable("hydra.core.Unit"), None))((x: hydra.core.TypeScheme) => x)(tdef.`type`)
  lazy val term: hydra.core.Term = hydra.variables.unshadowVariables(term0)
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.FunctionStructure[hydra.java.environment.JavaEnvironment],
     hydra.java.syntax.InterfaceMemberDeclaration](hydra.java.coder.analyzeJavaFunction(env)(term)(cx)(g))((fs: hydra.typing.FunctionStructure[hydra.java.environment.JavaEnvironment]) =>
    {
    lazy val schemeVars: Seq[hydra.core.Name] = hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) => hydra.java.coder.isSimpleName(v))(ts.variables)
    {
      lazy val termVars: Seq[hydra.core.Name] = (fs.typeParams)
      {
        lazy val schemeTypeVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.java.coder.collectTypeVars(ts.`type`)
        {
          lazy val usedSchemeVars: Seq[hydra.core.Name] = hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) => hydra.lib.sets.member[hydra.core.Name](v)(schemeTypeVars))(schemeVars)
          {
            lazy val tparams: Seq[hydra.core.Name] = hydra.lib.logic.ifElse[Seq[hydra.core.Name]](hydra.lib.lists.`null`[hydra.core.Name](usedSchemeVars))(termVars)(usedSchemeVars)
            {
              lazy val params: Seq[hydra.core.Name] = (fs.params)
              {
                lazy val bindings: Seq[hydra.core.Binding] = (fs.bindings)
                {
                  lazy val body: hydra.core.Term = (fs.body)
                  {
                    lazy val doms: Seq[hydra.core.Type] = (fs.domains)
                    {
                      lazy val env2: hydra.java.environment.JavaEnvironment = (fs.environment)
                      {
                        lazy val schemeType: hydra.core.Type = (ts.`type`)
                        {
                          lazy val numParams: Int = hydra.lib.lists.length[hydra.core.Name](params)
                          {
                            lazy val peelResult: Tuple2[Seq[hydra.core.Type], hydra.core.Type] = hydra.java.coder.peelDomainsAndCod(numParams)(schemeType)
                            {
                              lazy val schemeDoms: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.core.Type](peelResult)
                              {
                                lazy val cod: hydra.core.Type = hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.core.Type](peelResult)
                                {
                                  lazy val schemeVarSet: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](tparams)
                                  hydra.lib.eithers.bind[hydra.errors.Error, Map[hydra.core.Name, hydra.core.Name],
                                     hydra.java.syntax.InterfaceMemberDeclaration](hydra.lib.logic.ifElse[Either[hydra.errors.Error,
                                     Map[hydra.core.Name, hydra.core.Name]]](hydra.lib.lists.`null`[hydra.core.Name](tparams))(Right(hydra.lib.maps.empty[hydra.core.Name,
                                     hydra.core.Name]))(hydra.java.coder.buildSubstFromAnnotations(schemeVarSet)(term)(cx)(g)))((typeVarSubst: Map[hydra.core.Name,
                                     hydra.core.Name]) =>
                                    {
                                    lazy val overgenSubst: Map[hydra.core.Name, hydra.core.Type] = hydra.java.coder.detectAccumulatorUnification(schemeDoms)(cod)(tparams)
                                    {
                                      lazy val overgenVarSubst: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.maps.fromList[hydra.core.Name,
                                         hydra.core.Name](hydra.lib.maybes.cat[Tuple2[hydra.core.Name,
                                         hydra.core.Name]](hydra.lib.lists.map[Tuple2[hydra.core.Name,
                                         hydra.core.Type], Option[Tuple2[hydra.core.Name, hydra.core.Name]]]((entry: Tuple2[hydra.core.Name,
                                         hydra.core.Type]) =>
                                        {
                                        lazy val k: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, hydra.core.Type](entry)
                                        {
                                          lazy val v: hydra.core.Type = hydra.lib.pairs.second[hydra.core.Name, hydra.core.Type](entry)
                                          v match
                                            case hydra.core.Type.variable(v_Type_variable_n) => Some(Tuple2(k, v_Type_variable_n))
                                            case _ => None
                                        }
                                      })(hydra.lib.maps.toList[hydra.core.Name, hydra.core.Type](overgenSubst))))
                                      {
                                        lazy val fixedCod: hydra.core.Type = hydra.lib.logic.ifElse[hydra.core.Type](hydra.lib.maps.`null`[hydra.core.Name,
                                           hydra.core.Type](overgenSubst))(cod)(hydra.java.coder.substituteTypeVarsWithTypes(overgenSubst)(cod))
                                        {
                                          lazy val fixedDoms: Seq[hydra.core.Type] = hydra.lib.logic.ifElse[Seq[hydra.core.Type]](hydra.lib.maps.`null`[hydra.core.Name,
                                             hydra.core.Type](overgenSubst))(schemeDoms)(hydra.lib.lists.map[hydra.core.Type,
                                             hydra.core.Type]((d: hydra.core.Type) =>
                                            hydra.java.coder.substituteTypeVarsWithTypes(overgenSubst)(d))(schemeDoms))
                                          {
                                            lazy val fixedTparams: Seq[hydra.core.Name] = hydra.lib.logic.ifElse[Seq[hydra.core.Name]](hydra.lib.maps.`null`[hydra.core.Name,
                                               hydra.core.Type](overgenSubst))(tparams)(hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) =>
                                              hydra.lib.logic.not(hydra.lib.maps.member[hydra.core.Name, hydra.core.Type](v)(overgenSubst)))(tparams))
                                            {
                                              lazy val constraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.lib.maybes.fromMaybe[Map[hydra.core.Name,
                                                 hydra.core.TypeVariableMetadata]](hydra.lib.maps.empty[hydra.core.Name,
                                                 hydra.core.TypeVariableMetadata])(ts.constraints)
                                              {
                                                lazy val jparams: Seq[hydra.java.syntax.TypeParameter] = hydra.lib.lists.map[hydra.core.Name,
                                                   hydra.java.syntax.TypeParameter]((v: hydra.core.Name) =>
                                                  hydra.java.utils.javaTypeParameter(hydra.formatting.capitalize(v)))(fixedTparams)
                                                {
                                                  lazy val aliases2base: hydra.java.environment.Aliases = (env2.aliases)
                                                  {
                                                    lazy val trustedVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.unions[hydra.core.Name](hydra.lib.lists.map[hydra.core.Type,
                                                       scala.collection.immutable.Set[hydra.core.Name]]((d: hydra.core.Type) => hydra.java.coder.collectTypeVars(d))(hydra.lib.lists.concat2[hydra.core.Type](fixedDoms)(Seq(fixedCod))))
                                                    {
                                                      lazy val fixedSchemeVarSet: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](fixedTparams)
                                                      {
                                                        lazy val aliases2: hydra.java.environment.Aliases = hydra.java.environment.Aliases(aliases2base.currentNamespace,
                                                           (aliases2base.packages), (aliases2base.branchVars),
                                                           (aliases2base.recursiveVars), fixedSchemeVarSet,
                                                           (aliases2base.polymorphicLocals), (aliases2base.inScopeJavaVars),
                                                           (aliases2base.varRenames), hydra.lib.sets.union[hydra.core.Name](aliases2base.lambdaVars)(hydra.lib.sets.fromList[hydra.core.Name](params)),
                                                           hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](overgenVarSubst)(typeVarSubst),
                                                           hydra.lib.sets.intersection[hydra.core.Name](trustedVars)(fixedSchemeVarSet),
                                                           Some(fixedCod), (aliases2base.thunkedVars))
                                                        {
                                                          lazy val env2WithTypeParams: hydra.java.environment.JavaEnvironment = hydra.java.environment.JavaEnvironment(aliases2,
                                                             (env2.graph))
                                                          hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[Seq[hydra.java.syntax.BlockStatement],
                                                             hydra.java.environment.JavaEnvironment],
                                                             hydra.java.syntax.InterfaceMemberDeclaration](hydra.java.coder.bindingsToStatements(env2WithTypeParams)(bindings)(cx)(g))((bindResult: Tuple2[Seq[hydra.java.syntax.BlockStatement],
                                                             hydra.java.environment.JavaEnvironment]) =>
                                                            {
                                                            lazy val bindingStmts: Seq[hydra.java.syntax.BlockStatement] = hydra.lib.pairs.first[Seq[hydra.java.syntax.BlockStatement],
                                                               hydra.java.environment.JavaEnvironment](bindResult)
                                                            {
                                                              lazy val env3: hydra.java.environment.JavaEnvironment = hydra.lib.pairs.second[Seq[hydra.java.syntax.BlockStatement],
                                                                 hydra.java.environment.JavaEnvironment](bindResult)
                                                              hydra.lib.eithers.bind[hydra.errors.Error,
                                                                 hydra.core.Term, hydra.java.syntax.InterfaceMemberDeclaration](hydra.lib.logic.ifElse[Either[hydra.errors.Error,
                                                                 hydra.core.Term]](hydra.lib.maps.`null`[hydra.core.Name,
                                                                 hydra.core.Type](overgenSubst))(Right(body))(hydra.java.coder.applyOvergenSubstToTermAnnotations(overgenSubst)(body)(cx)(g)))((`body_`: hydra.core.Term) =>
                                                                {
                                                                lazy val annotatedBody: hydra.core.Term = hydra.java.coder.propagateTypesInAppChain(fixedCod)(fixedCod)(`body_`)
                                                                hydra.lib.eithers.bind[hydra.errors.Error,
                                                                   Seq[hydra.java.syntax.FormalParameter],
                                                                   hydra.java.syntax.InterfaceMemberDeclaration](hydra.lib.eithers.mapList[Tuple2[hydra.core.Type,
                                                                   hydra.core.Name], hydra.java.syntax.FormalParameter,
                                                                   hydra.errors.Error]((pair: Tuple2[hydra.core.Type,
                                                                   hydra.core.Name]) =>
                                                                  hydra.lib.eithers.bind[hydra.errors.Error,
                                                                     hydra.java.syntax.Type, hydra.java.syntax.FormalParameter](hydra.java.coder.encodeType(aliases2)(hydra.lib.sets.empty[hydra.core.Name])(hydra.lib.pairs.first[hydra.core.Type,
                                                                     hydra.core.Name](pair))(cx)(g))((jdom: hydra.java.syntax.Type) =>
                                                                  Right(hydra.java.utils.javaTypeToJavaFormalParameter(jdom)(hydra.lib.pairs.second[hydra.core.Type,
                                                                     hydra.core.Name](pair)))))(hydra.lib.lists.zip[hydra.core.Type,
                                                                     hydra.core.Name](fixedDoms)(params)))((jformalParams: Seq[hydra.java.syntax.FormalParameter]) =>
                                                                  hydra.lib.eithers.bind[hydra.errors.Error,
                                                                     hydra.java.syntax.Type, hydra.java.syntax.InterfaceMemberDeclaration](hydra.java.coder.encodeType(aliases2)(hydra.lib.sets.empty[hydra.core.Name])(fixedCod)(cx)(g))((jcod: hydra.java.syntax.Type) =>
                                                                  {
                                                                  lazy val result: hydra.java.syntax.Result = hydra.java.utils.javaTypeToJavaResult(jcod)
                                                                  {
                                                                    lazy val mods: Seq[hydra.java.syntax.InterfaceMethodModifier] = Seq(hydra.java.syntax.InterfaceMethodModifier.static)
                                                                    {
                                                                      lazy val jname: scala.Predef.String = hydra.java.utils.sanitizeJavaName(hydra.formatting.decapitalize(hydra.names.localNameOf(name)))
                                                                      {
                                                                        lazy val isTCO: Boolean = false
                                                                        hydra.lib.eithers.bind[hydra.errors.Error,
                                                                           Seq[hydra.java.syntax.BlockStatement],
                                                                           hydra.java.syntax.InterfaceMemberDeclaration](hydra.lib.logic.ifElse[Either[hydra.errors.Error,
                                                                           Seq[hydra.java.syntax.BlockStatement]]](isTCO)({
                                                                          lazy val tcoSuffix: scala.Predef.String = "_tco"
                                                                          {
                                                                            lazy val snapshotNames: Seq[hydra.core.Name] = hydra.lib.lists.map[hydra.core.Name,
                                                                               hydra.core.Name]((p: hydra.core.Name) => hydra.lib.strings.cat2(p)(tcoSuffix))(params)
                                                                            {
                                                                              lazy val tcoVarRenames: Map[hydra.core.Name,
                                                                                 hydra.core.Name] = hydra.lib.maps.fromList[hydra.core.Name,
                                                                                 hydra.core.Name](hydra.lib.lists.zip[hydra.core.Name,
                                                                                 hydra.core.Name](params)(snapshotNames))
                                                                              {
                                                                                lazy val snapshotDecls: Seq[hydra.java.syntax.BlockStatement] = hydra.lib.lists.map[Tuple2[hydra.core.Name,
                                                                                   hydra.core.Name], hydra.java.syntax.BlockStatement]((pair: Tuple2[hydra.core.Name,
                                                                                   hydra.core.Name]) =>
                                                                                  hydra.java.utils.finalVarDeclarationStatement(hydra.java.utils.variableToJavaIdentifier(hydra.lib.pairs.second[hydra.core.Name,
                                                                                     hydra.core.Name](pair)))(hydra.java.utils.javaIdentifierToJavaExpression(hydra.java.utils.variableToJavaIdentifier(hydra.lib.pairs.first[hydra.core.Name,
                                                                                     hydra.core.Name](pair)))))(hydra.lib.lists.zip[hydra.core.Name,
                                                                                     hydra.core.Name](params)(snapshotNames))
                                                                                {
                                                                                  lazy val tcoBody: hydra.core.Term = hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.lists.`null`[hydra.core.Binding](bindings))(annotatedBody)(hydra.core.Term.let(hydra.core.Let(bindings,
                                                                                     annotatedBody)))
                                                                                  hydra.lib.eithers.bind[hydra.errors.Error,
                                                                                     Seq[hydra.java.syntax.BlockStatement],
                                                                                     Seq[hydra.java.syntax.BlockStatement]](hydra.java.coder.encodeTermTCO(env2WithTypeParams)(name)(params)(tcoVarRenames)(0)(tcoBody)(cx)(g))((tcoStmts: Seq[hydra.java.syntax.BlockStatement]) =>
                                                                                    {
                                                                                    lazy val whileBodyStmts: Seq[hydra.java.syntax.BlockStatement] = hydra.lib.lists.concat2[hydra.java.syntax.BlockStatement](snapshotDecls)(tcoStmts)
                                                                                    {
                                                                                      lazy val whileBodyBlock: hydra.java.syntax.Statement = hydra.java.syntax.Statement.withoutTrailing(hydra.java.syntax.StatementWithoutTrailingSubstatement.block(whileBodyStmts))
                                                                                      {
                                                                                        def noCond[T0]: Option[T0] = None
                                                                                        {
                                                                                          lazy val whileStmt: hydra.java.syntax.BlockStatement = hydra.java.syntax.BlockStatement.statement(hydra.java.syntax.Statement.`while`(hydra.java.syntax.WhileStatement(noCond,
                                                                                             whileBodyBlock)))
                                                                                          Right(Seq(whileStmt))
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                  })
                                                                                }
                                                                              }
                                                                            }
                                                                          }
                                                                        })(hydra.lib.eithers.bind[hydra.errors.Error,
                                                                           hydra.java.syntax.Expression,
                                                                           Seq[hydra.java.syntax.BlockStatement]](hydra.java.coder.encodeTerm(env3)(annotatedBody)(cx)(g))((jbody: hydra.java.syntax.Expression) =>
                                                                          {
                                                                          lazy val returnSt: hydra.java.syntax.BlockStatement = hydra.java.syntax.BlockStatement.statement(hydra.java.utils.javaReturnStatement(Some(jbody)))
                                                                          Right(hydra.lib.lists.concat2[hydra.java.syntax.BlockStatement](bindingStmts)(Seq(returnSt)))
                                                                        })))((methodBody: Seq[hydra.java.syntax.BlockStatement]) =>
                                                                          Right(hydra.java.utils.interfaceMethodDeclaration(mods)(jparams)(jname)(jformalParams)(result)(Some(methodBody))))
                                                                      }
                                                                    }
                                                                  }
                                                                }))
                                                              })
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
              }
            }
          }
        }
      }
    }
  })
}

def encodeTermInternal(env: hydra.java.environment.JavaEnvironment)(anns: Seq[Map[hydra.core.Name, hydra.core.Term]])(tyapps: Seq[hydra.java.syntax.Type])(term: hydra.core.Term)(cx: hydra.context.Context)(g0: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.Expression] =
  {
  lazy val aliases: hydra.java.environment.Aliases = (env.aliases)
  lazy val g: hydra.graph.Graph = (env.graph)
  def encode(t: hydra.core.Term): Either[hydra.errors.Error, hydra.java.syntax.Expression] = hydra.java.coder.encodeTerm(env)(t)(cx)(g)
  term match
    case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.java.coder.encodeTermInternal(env)(hydra.lib.lists.cons[Map[hydra.core.Name,
       hydra.core.Term]](v_Term_annotated_at.annotation)(anns))(tyapps)(v_Term_annotated_at.body)(cx)(g)
    case hydra.core.Term.application(v_Term_application_app) => hydra.java.coder.encodeApplication(env)(v_Term_application_app)(cx)(g)
    case hydra.core.Term.either(v_Term_either_et) => hydra.lib.eithers.bind[hydra.errors.Error, Option[Seq[hydra.java.syntax.TypeArgument]],
       hydra.java.syntax.Expression](hydra.lib.logic.ifElse[Either[hydra.errors.Error, Option[Seq[hydra.java.syntax.TypeArgument]]]](hydra.lib.lists.`null`[hydra.java.syntax.Type](tyapps))(Right(None))(hydra.lib.eithers.bind[hydra.errors.Error,
       Seq[hydra.java.syntax.TypeArgument], Option[Seq[hydra.java.syntax.TypeArgument]]](hydra.java.coder.takeTypeArgs("either")(2)(tyapps)(cx)(g))((ta: Seq[hydra.java.syntax.TypeArgument]) => Right(Some(ta)))))((mtargs: Option[Seq[hydra.java.syntax.TypeArgument]]) =>
      {
      lazy val combinedAnns: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.lists.foldl[Map[hydra.core.Name,
         hydra.core.Term], Map[hydra.core.Name, hydra.core.Term]]((acc: Map[hydra.core.Name, hydra.core.Term]) =>
        (m: Map[hydra.core.Name, hydra.core.Term]) =>
        hydra.lib.maps.union[hydra.core.Name, hydra.core.Term](acc)(m))(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Term])(anns)
      hydra.lib.eithers.bind[hydra.errors.Error, Option[hydra.core.Type], hydra.java.syntax.Expression](hydra.lib.eithers.bimap[hydra.errors.DecodingError,
         Option[hydra.core.Type], hydra.errors.Error, Option[hydra.core.Type]]((__de: hydra.errors.DecodingError) => hydra.errors.Error.other(__de))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(combinedAnns)))((mEitherType: Option[hydra.core.Type]) =>
        {
        lazy val branchTypes: Option[Tuple2[hydra.core.Type, hydra.core.Type]] = hydra.lib.maybes.bind[hydra.core.Type,
           Tuple2[hydra.core.Type, hydra.core.Type]](mEitherType)((etyp: hydra.core.Type) =>
          hydra.strip.deannotateType(etyp) match
          case hydra.core.Type.either(v_Type_either_et2) => Some(Tuple2(v_Type_either_et2.left, (v_Type_either_et2.right)))
          case _ => None)
        {
          def encodeWithType(branchType: hydra.core.Type)(t1: hydra.core.Term): Either[hydra.errors.Error, hydra.java.syntax.Expression] =
            {
            lazy val annotated: hydra.core.Term = hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(branchType)))(t1)
            hydra.java.coder.encodeTermInternal(env)(anns)(Seq())(annotated)(cx)(g)
          }
          {
            def eitherCall(methodName: scala.Predef.String)(expr: hydra.java.syntax.Expression): hydra.java.syntax.Expression =
              hydra.lib.maybes.cases[Seq[hydra.java.syntax.TypeArgument], hydra.java.syntax.Expression](mtargs)(hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocationStatic("hydra.util.Either")(methodName)(Seq(expr))))((targs: Seq[hydra.java.syntax.TypeArgument]) =>
              hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocationStaticWithTypeArgs("hydra.util.Either")(methodName)(targs)(Seq(expr))))
            hydra.lib.eithers.either[hydra.core.Term, hydra.core.Term, Either[hydra.errors.Error, hydra.java.syntax.Expression]]((term1: hydra.core.Term) =>
              hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression, hydra.java.syntax.Expression](hydra.lib.maybes.cases[Tuple2[hydra.core.Type,
                 hydra.core.Type], Either[hydra.errors.Error, hydra.java.syntax.Expression]](branchTypes)(encode(term1))((bt: Tuple2[hydra.core.Type,
                 hydra.core.Type]) =>
              encodeWithType(hydra.lib.pairs.first[hydra.core.Type, hydra.core.Type](bt))(term1)))((expr: hydra.java.syntax.Expression) => Right(eitherCall("left")(expr))))((term1: hydra.core.Term) =>
              hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression, hydra.java.syntax.Expression](hydra.lib.maybes.cases[Tuple2[hydra.core.Type,
                 hydra.core.Type], Either[hydra.errors.Error, hydra.java.syntax.Expression]](branchTypes)(encode(term1))((bt: Tuple2[hydra.core.Type,
                 hydra.core.Type]) =>
              encodeWithType(hydra.lib.pairs.second[hydra.core.Type, hydra.core.Type](bt))(term1)))((expr: hydra.java.syntax.Expression) => Right(eitherCall("right")(expr))))(v_Term_either_et)
          }
        }
      })
    })
    case hydra.core.Term.lambda(v_Term_lambda__lam) => hydra.java.coder.encodeFunctionFormTerm(env)(anns)(term)(cx)(g)
    case hydra.core.Term.project(v_Term_project__p) => hydra.java.coder.encodeFunctionFormTerm(env)(anns)(term)(cx)(g)
    case hydra.core.Term.cases(v_Term_cases__c) => hydra.java.coder.encodeFunctionFormTerm(env)(anns)(term)(cx)(g)
    case hydra.core.Term.unwrap(v_Term_unwrap__w) => hydra.java.coder.encodeFunctionFormTerm(env)(anns)(term)(cx)(g)
    case hydra.core.Term.let(v_Term_let_lt) => {
      lazy val bindings: Seq[hydra.core.Binding] = (v_Term_let_lt.bindings)
      {
        lazy val body: hydra.core.Term = (v_Term_let_lt.body)
        hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.java.syntax.Expression]](hydra.lib.lists.`null`[hydra.core.Binding](bindings))(hydra.java.coder.encodeTermInternal(env)(anns)(Seq())(body)(cx)(g))(hydra.lib.eithers.bind[hydra.errors.Error,
           Tuple2[Seq[hydra.java.syntax.BlockStatement], hydra.java.environment.JavaEnvironment], hydra.java.syntax.Expression](hydra.java.coder.bindingsToStatements(env)(bindings)(cx)(g))((bindResult: Tuple2[Seq[hydra.java.syntax.BlockStatement],
           hydra.java.environment.JavaEnvironment]) =>
          {
          lazy val bindingStmts: Seq[hydra.java.syntax.BlockStatement] = hydra.lib.pairs.first[Seq[hydra.java.syntax.BlockStatement],
             hydra.java.environment.JavaEnvironment](bindResult)
          {
            lazy val env2: hydra.java.environment.JavaEnvironment = hydra.lib.pairs.second[Seq[hydra.java.syntax.BlockStatement],
               hydra.java.environment.JavaEnvironment](bindResult)
            hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression, hydra.java.syntax.Expression](hydra.java.coder.encodeTermInternal(env2)(anns)(Seq())(body)(cx)(g))((jbody: hydra.java.syntax.Expression) =>
              {
              lazy val returnSt: hydra.java.syntax.BlockStatement = hydra.java.syntax.BlockStatement.statement(hydra.java.utils.javaReturnStatement(Some(jbody)))
              {
                lazy val block: hydra.java.syntax.Block = hydra.lib.lists.concat2[hydra.java.syntax.BlockStatement](bindingStmts)(Seq(returnSt))
                {
                  lazy val nullaryLambda: hydra.java.syntax.Expression = hydra.java.syntax.Expression.lambda(hydra.java.syntax.LambdaExpression(hydra.java.syntax.LambdaParameters.tuple(Seq()),
                     hydra.java.syntax.LambdaBody.block(block)))
                  {
                    lazy val combinedAnns: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.lists.foldl[Map[hydra.core.Name,
                       hydra.core.Term], Map[hydra.core.Name, hydra.core.Term]]((acc: Map[hydra.core.Name,
                       hydra.core.Term]) =>
                      (m: Map[hydra.core.Name, hydra.core.Term]) =>
                      hydra.lib.maps.union[hydra.core.Name, hydra.core.Term](acc)(m))(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Term])(anns)
                    {
                      lazy val g2: hydra.graph.Graph = (env2.graph)
                      {
                        lazy val aliases2: hydra.java.environment.Aliases = (env2.aliases)
                        hydra.lib.eithers.bind[hydra.errors.Error, Option[hydra.core.Type], hydra.java.syntax.Expression](hydra.lib.eithers.bimap[hydra.errors.DecodingError,
                           Option[hydra.core.Type], hydra.errors.Error, Option[hydra.core.Type]]((__de: hydra.errors.DecodingError) => hydra.errors.Error.other(__de))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(combinedAnns)))((mt: Option[hydra.core.Type]) =>
                          hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Type, hydra.java.syntax.Expression](hydra.lib.maybes.cases[hydra.core.Type,
                             Either[hydra.errors.Error, hydra.core.Type]](mt)(hydra.checking.typeOfTerm(cx)(g2)(body))((t: hydra.core.Type) => Right(t)))((letType: hydra.core.Type) =>
                          hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type, hydra.java.syntax.Expression](hydra.java.coder.encodeType(aliases2)(hydra.lib.sets.empty[hydra.core.Name])(letType)(cx)(g))((jLetType: hydra.java.syntax.Type) =>
                          hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType,
                             hydra.java.syntax.Expression](hydra.java.utils.javaTypeToJavaReferenceType(jLetType)(cx))((rt: hydra.java.syntax.ReferenceType) =>
                          {
                          lazy val supplierRt: hydra.java.syntax.ReferenceType = hydra.java.syntax.ReferenceType.classOrInterface(hydra.java.syntax.ClassOrInterfaceType.`class`(hydra.java.utils.javaClassType(Seq(rt))(hydra.java.names.javaUtilFunctionPackageName)("Supplier")))
                          {
                            lazy val castExpr: hydra.java.syntax.Expression = hydra.java.utils.javaCastExpressionToJavaExpression(hydra.java.utils.javaCastExpression(supplierRt)(hydra.java.utils.javaExpressionToJavaUnaryExpression(nullaryLambda)))
                            Right(hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocation(Some(Right(hydra.java.utils.javaExpressionToJavaPrimary(castExpr))))("get")(Seq())))
                          }
                        }))))
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
    case hydra.core.Term.list(v_Term_list_els) => hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.java.syntax.Expression]](hydra.lib.lists.`null`[hydra.core.Term](v_Term_list_els))(hydra.lib.logic.ifElse[Either[hydra.errors.Error,
       hydra.java.syntax.Expression]](hydra.lib.lists.`null`[hydra.java.syntax.Type](tyapps))(Right(hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocationStatic("java.util.Collections")("emptyList")(Seq()))))(hydra.lib.eithers.bind[hydra.errors.Error,
       Seq[hydra.java.syntax.TypeArgument], hydra.java.syntax.Expression](hydra.java.coder.takeTypeArgs("list")(1)(tyapps)(cx)(g))((targs: Seq[hydra.java.syntax.TypeArgument]) =>
      Right(hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocationStaticWithTypeArgs("java.util.Collections")("emptyList")(targs)(Seq()))))))(hydra.lib.eithers.bind[hydra.errors.Error,
         Seq[hydra.java.syntax.Expression], hydra.java.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Term,
         hydra.java.syntax.Expression, hydra.errors.Error](encode)(v_Term_list_els))((jels: Seq[hydra.java.syntax.Expression]) =>
      Right(hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocationStatic("java.util.Arrays")("asList")(jels)))))
    case hydra.core.Term.literal(v_Term_literal_l) => Right(hydra.java.coder.encodeLiteral(v_Term_literal_l))
    case hydra.core.Term.map(v_Term_map_m) => hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.java.syntax.Expression]](hydra.lib.maps.`null`[hydra.core.Term,
       hydra.core.Term](v_Term_map_m))(hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.java.syntax.Expression]](hydra.lib.lists.`null`[hydra.java.syntax.Type](tyapps))(Right(hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocationStatic("java.util.Collections")("emptyMap")(Seq()))))(hydra.lib.eithers.bind[hydra.errors.Error,
       Seq[hydra.java.syntax.TypeArgument], hydra.java.syntax.Expression](hydra.java.coder.takeTypeArgs("map")(2)(tyapps)(cx)(g))((targs: Seq[hydra.java.syntax.TypeArgument]) =>
      Right(hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocationStaticWithTypeArgs("java.util.Collections")("emptyMap")(targs)(Seq()))))))(hydra.lib.eithers.bind[hydra.errors.Error,
         Seq[hydra.java.syntax.Expression], hydra.java.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Term,
         hydra.java.syntax.Expression, hydra.errors.Error](encode)(hydra.lib.maps.keys[hydra.core.Term,
         hydra.core.Term](v_Term_map_m)))((jkeys: Seq[hydra.java.syntax.Expression]) =>
      hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.Expression], hydra.java.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Term,
         hydra.java.syntax.Expression, hydra.errors.Error](encode)(hydra.lib.maps.elems[hydra.core.Term,
         hydra.core.Term](v_Term_map_m)))((jvals: Seq[hydra.java.syntax.Expression]) =>
      {
      lazy val pairExprs: Seq[hydra.java.syntax.Expression] = hydra.lib.lists.map[Tuple2[hydra.java.syntax.Expression,
         hydra.java.syntax.Expression], hydra.java.syntax.Expression]((kv: Tuple2[hydra.java.syntax.Expression,
         hydra.java.syntax.Expression]) =>
        hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocationStatic("java.util.Map")("entry")(Seq(hydra.lib.pairs.first[hydra.java.syntax.Expression,
           hydra.java.syntax.Expression](kv), hydra.lib.pairs.second[hydra.java.syntax.Expression, hydra.java.syntax.Expression](kv)))))(hydra.lib.lists.zip[hydra.java.syntax.Expression,
           hydra.java.syntax.Expression](jkeys)(jvals))
      {
        lazy val innerMap: hydra.java.syntax.Expression = hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocationStatic("java.util.Map")("ofEntries")(pairExprs))
        Right(hydra.java.utils.javaConstructorCall(hydra.java.utils.javaConstructorName("java.util.TreeMap")(None))(Seq(innerMap))(None))
      }
    })))
    case hydra.core.Term.maybe(v_Term_maybe_mt) => hydra.lib.maybes.cases[hydra.core.Term, Either[hydra.errors.Error,
       hydra.java.syntax.Expression]](v_Term_maybe_mt)(hydra.lib.logic.ifElse[Either[hydra.errors.Error,
       hydra.java.syntax.Expression]](hydra.lib.lists.`null`[hydra.java.syntax.Type](tyapps))(Right(hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocationStatic("hydra.util.Maybe")("nothing")(Seq()))))(hydra.lib.eithers.bind[hydra.errors.Error,
       Seq[hydra.java.syntax.TypeArgument], hydra.java.syntax.Expression](hydra.java.coder.takeTypeArgs("maybe")(1)(tyapps)(cx)(g))((targs: Seq[hydra.java.syntax.TypeArgument]) =>
      Right(hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocationStaticWithTypeArgs("hydra.util.Maybe")("nothing")(targs)(Seq()))))))((term1: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression, hydra.java.syntax.Expression](encode(term1))((expr: hydra.java.syntax.Expression) =>
      Right(hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocationStatic("hydra.util.Maybe")("just")(Seq(expr))))))
    case hydra.core.Term.pair(v_Term_pair_p) => hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression,
       hydra.java.syntax.Expression](encode(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](v_Term_pair_p)))((jterm1: hydra.java.syntax.Expression) =>
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression, hydra.java.syntax.Expression](encode(hydra.lib.pairs.second[hydra.core.Term,
         hydra.core.Term](v_Term_pair_p)))((jterm2: hydra.java.syntax.Expression) =>
      hydra.lib.eithers.bind[hydra.errors.Error, Option[hydra.java.syntax.TypeArgumentsOrDiamond], hydra.java.syntax.Expression](hydra.lib.logic.ifElse[Either[hydra.errors.Error,
         Option[hydra.java.syntax.TypeArgumentsOrDiamond]]](hydra.lib.lists.`null`[hydra.java.syntax.Type](tyapps))(Right(None))(hydra.lib.eithers.bind[hydra.errors.Error,
         Seq[hydra.java.syntax.ReferenceType], Option[hydra.java.syntax.TypeArgumentsOrDiamond]](hydra.lib.eithers.mapList[hydra.java.syntax.Type,
         hydra.java.syntax.ReferenceType, hydra.errors.Error]((jt: hydra.java.syntax.Type) => hydra.java.utils.javaTypeToJavaReferenceType(jt)(cx))(tyapps))((rts: Seq[hydra.java.syntax.ReferenceType]) =>
      Right(Some(hydra.java.syntax.TypeArgumentsOrDiamond.arguments(hydra.lib.lists.map[hydra.java.syntax.ReferenceType,
         hydra.java.syntax.TypeArgument]((rt: hydra.java.syntax.ReferenceType) => hydra.java.syntax.TypeArgument.reference(rt))(rts)))))))((mtargs: Option[hydra.java.syntax.TypeArgumentsOrDiamond]) =>
      Right(hydra.java.utils.javaConstructorCall(hydra.java.utils.javaConstructorName("hydra.util.Pair")(mtargs))(Seq(jterm1, jterm2))(None)))))
    case hydra.core.Term.record(v_Term_record_rec) => {
      lazy val recName: hydra.core.Name = (v_Term_record_rec.typeName)
      {
        lazy val mRecordType: Option[hydra.core.Type] = hydra.lib.eithers.either[hydra.errors.Error, hydra.core.Type,
           Option[hydra.core.Type]]((_x: hydra.errors.Error) => None)((t: hydra.core.Type) => Some(t))(hydra.resolution.requireType(cx)(g)(recName))
        {
          lazy val strippedRecTyp: Option[hydra.core.Type] = hydra.lib.maybes.map[hydra.core.Type, hydra.core.Type]((recTyp: hydra.core.Type) =>
            hydra.java.coder.stripForalls(hydra.strip.deannotateType(recTyp)))(mRecordType)
          {
            lazy val mFieldTypeMap: Option[Map[hydra.core.Name, hydra.core.Type]] = hydra.lib.maybes.bind[hydra.core.Type,
               Map[hydra.core.Name, hydra.core.Type]](strippedRecTyp)((bodyTyp: hydra.core.Type) =>
              bodyTyp match
              case hydra.core.Type.record(v_Type_record_rt) => Some(hydra.lib.maps.fromList[hydra.core.Name,
                 hydra.core.Type](hydra.lib.lists.map[hydra.core.FieldType, Tuple2[hydra.core.Name, hydra.core.Type]]((ft: hydra.core.FieldType) => Tuple2(ft.name,
                 (ft.`type`)))(v_Type_record_rt)))
              case _ => None)
            {
              lazy val combinedAnnsRec: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.lists.foldl[Map[hydra.core.Name,
                 hydra.core.Term], Map[hydra.core.Name, hydra.core.Term]]((acc: Map[hydra.core.Name, hydra.core.Term]) =>
                (m: Map[hydra.core.Name, hydra.core.Term]) =>
                hydra.lib.maps.union[hydra.core.Name, hydra.core.Term](acc)(m))(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Term])(anns)
              hydra.lib.eithers.bind[hydra.errors.Error, Option[hydra.core.Type], hydra.java.syntax.Expression](hydra.lib.eithers.bimap[hydra.errors.DecodingError,
                 Option[hydra.core.Type], hydra.errors.Error, Option[hydra.core.Type]]((__de: hydra.errors.DecodingError) => hydra.errors.Error.other(__de))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(combinedAnnsRec)))((mAnnotType: Option[hydra.core.Type]) =>
                {
                lazy val mTypeSubst: Option[Map[hydra.core.Name, hydra.core.Type]] = hydra.lib.maybes.bind[hydra.core.Type,
                   Map[hydra.core.Name, hydra.core.Type]](mAnnotType)((annTyp: hydra.core.Type) =>
                  hydra.lib.maybes.bind[hydra.core.Type, Map[hydra.core.Name, hydra.core.Type]](mRecordType)((recTyp: hydra.core.Type) =>
                  {
                  lazy val args: Seq[hydra.core.Type] = hydra.java.coder.extractTypeApplicationArgs(hydra.strip.deannotateType(annTyp))
                  {
                    lazy val params: Seq[hydra.core.Name] = hydra.java.coder.collectForallParams(hydra.strip.deannotateType(recTyp))
                    hydra.lib.logic.ifElse[Option[Map[hydra.core.Name, hydra.core.Type]]](hydra.lib.logic.or(hydra.lib.lists.`null`[hydra.core.Type](args))(hydra.lib.logic.not(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Type](args))(hydra.lib.lists.length[hydra.core.Name](params)))))(None)(Some(hydra.lib.maps.fromList[hydra.core.Name,
                       hydra.core.Type](hydra.lib.lists.zip[hydra.core.Name, hydra.core.Type](params)(args))))
                  }
                }))
                {
                  def encodeField(fld: hydra.core.Field): Either[hydra.errors.Error, hydra.java.syntax.Expression] =
                    hydra.lib.maybes.cases[Map[hydra.core.Name, hydra.core.Type], Either[hydra.errors.Error,
                       hydra.java.syntax.Expression]](mFieldTypeMap)(encode(fld.term))((ftmap: Map[hydra.core.Name,
                       hydra.core.Type]) =>
                    {
                    lazy val mftyp: Option[hydra.core.Type] = hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Type](fld.name)(ftmap)
                    hydra.lib.maybes.cases[hydra.core.Type, Either[hydra.errors.Error, hydra.java.syntax.Expression]](mftyp)(encode(fld.term))((ftyp: hydra.core.Type) =>
                      {
                      lazy val resolvedType: hydra.core.Type = hydra.lib.maybes.cases[Map[hydra.core.Name,
                         hydra.core.Type], hydra.core.Type](mTypeSubst)(ftyp)((subst: Map[hydra.core.Name,
                         hydra.core.Type]) => hydra.java.coder.applySubstFull(subst)(ftyp))
                      {
                        lazy val annotatedFieldTerm: hydra.core.Term = hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(resolvedType)))(fld.term)
                        hydra.java.coder.encodeTermInternal(env)(anns)(Seq())(annotatedFieldTerm)(cx)(g)
                      }
                    })
                  })
                  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.Expression], hydra.java.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Field,
                     hydra.java.syntax.Expression, hydra.errors.Error](encodeField)(v_Term_record_rec.fields))((fieldExprs: Seq[hydra.java.syntax.Expression]) =>
                    {
                    lazy val consId: hydra.java.syntax.Identifier = hydra.java.utils.nameToJavaName(aliases)(recName)
                    hydra.lib.eithers.bind[hydra.errors.Error, Option[hydra.java.syntax.TypeArgumentsOrDiamond],
                       hydra.java.syntax.Expression](hydra.lib.logic.ifElse[Either[hydra.errors.Error,
                       Option[hydra.java.syntax.TypeArgumentsOrDiamond]]](hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.java.syntax.Type](tyapps)))(hydra.lib.eithers.bind[hydra.errors.Error,
                       Seq[hydra.java.syntax.ReferenceType], Option[hydra.java.syntax.TypeArgumentsOrDiamond]](hydra.lib.eithers.mapList[hydra.java.syntax.Type,
                       hydra.java.syntax.ReferenceType, hydra.errors.Error]((jt: hydra.java.syntax.Type) => hydra.java.utils.javaTypeToJavaReferenceType(jt)(cx))(tyapps))((rts: Seq[hydra.java.syntax.ReferenceType]) =>
                      Right(Some(hydra.java.syntax.TypeArgumentsOrDiamond.arguments(hydra.lib.lists.map[hydra.java.syntax.ReferenceType,
                         hydra.java.syntax.TypeArgument]((rt: hydra.java.syntax.ReferenceType) => hydra.java.syntax.TypeArgument.reference(rt))(rts))))))({
                      lazy val combinedAnns: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.lists.foldl[Map[hydra.core.Name,
                         hydra.core.Term], Map[hydra.core.Name, hydra.core.Term]]((acc: Map[hydra.core.Name,
                         hydra.core.Term]) =>
                        (m: Map[hydra.core.Name, hydra.core.Term]) =>
                        hydra.lib.maps.union[hydra.core.Name, hydra.core.Term](acc)(m))(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Term])(anns)
                      hydra.lib.eithers.bind[hydra.errors.Error, Option[hydra.core.Type], Option[hydra.java.syntax.TypeArgumentsOrDiamond]](hydra.lib.eithers.bimap[hydra.errors.DecodingError,
                         Option[hydra.core.Type], hydra.errors.Error, Option[hydra.core.Type]]((__de: hydra.errors.DecodingError) => hydra.errors.Error.other(__de))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(combinedAnns)))((mtyp: Option[hydra.core.Type]) =>
                        hydra.lib.maybes.cases[hydra.core.Type, Either[hydra.errors.Error, Option[hydra.java.syntax.TypeArgumentsOrDiamond]]](mtyp)(Right(None))((annTyp: hydra.core.Type) =>
                        {
                        lazy val typeArgs: Seq[hydra.core.Type] = hydra.java.coder.extractTypeApplicationArgs(hydra.strip.deannotateType(annTyp))
                        hydra.lib.logic.ifElse[Either[hydra.errors.Error, Option[hydra.java.syntax.TypeArgumentsOrDiamond]]](hydra.lib.lists.`null`[hydra.core.Type](typeArgs))(Right(None))(hydra.lib.eithers.bind[hydra.errors.Error,
                           Seq[hydra.java.syntax.ReferenceType], Option[hydra.java.syntax.TypeArgumentsOrDiamond]](hydra.lib.eithers.mapList[hydra.core.Type,
                           hydra.java.syntax.ReferenceType, hydra.errors.Error]((t: hydra.core.Type) =>
                          hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type, hydra.java.syntax.ReferenceType](hydra.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(t)(cx)(g))((jt: hydra.java.syntax.Type) => hydra.java.utils.javaTypeToJavaReferenceType(jt)(cx)))(typeArgs))((jTypeArgs: Seq[hydra.java.syntax.ReferenceType]) =>
                          Right(Some(hydra.java.syntax.TypeArgumentsOrDiamond.arguments(hydra.lib.lists.map[hydra.java.syntax.ReferenceType,
                             hydra.java.syntax.TypeArgument]((rt: hydra.java.syntax.ReferenceType) => hydra.java.syntax.TypeArgument.reference(rt))(jTypeArgs))))))
                      }))
                    }))((mtargs: Option[hydra.java.syntax.TypeArgumentsOrDiamond]) =>
                      Right(hydra.java.utils.javaConstructorCall(hydra.java.utils.javaConstructorName(consId)(mtargs))(fieldExprs)(None)))
                  })
                }
              })
            }
          }
        }
      }
    }
    case hydra.core.Term.set(v_Term_set_s) => hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.java.syntax.Expression]](hydra.lib.sets.`null`[hydra.core.Term](v_Term_set_s))(hydra.lib.logic.ifElse[Either[hydra.errors.Error,
       hydra.java.syntax.Expression]](hydra.lib.lists.`null`[hydra.java.syntax.Type](tyapps))(Right(hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocationStatic("java.util.Collections")("emptySet")(Seq()))))(hydra.lib.eithers.bind[hydra.errors.Error,
       Seq[hydra.java.syntax.TypeArgument], hydra.java.syntax.Expression](hydra.java.coder.takeTypeArgs("set")(1)(tyapps)(cx)(g))((targs: Seq[hydra.java.syntax.TypeArgument]) =>
      Right(hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocationStaticWithTypeArgs("java.util.Collections")("emptySet")(targs)(Seq()))))))({
      lazy val slist: Seq[hydra.core.Term] = hydra.lib.sets.toList[hydra.core.Term](v_Term_set_s)
      hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.Expression], hydra.java.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Term,
         hydra.java.syntax.Expression, hydra.errors.Error](encode)(slist))((jels: Seq[hydra.java.syntax.Expression]) =>
        {
        lazy val innerSet: hydra.java.syntax.Expression = hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocationStatic("java.util.Set")("of")(jels))
        Right(hydra.java.utils.javaConstructorCall(hydra.java.utils.javaConstructorName("java.util.TreeSet")(None))(Seq(innerSet))(None))
      })
    })
    case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.java.coder.withTypeLambda(env)(v_Term_typeLambda_tl)((env2: hydra.java.environment.JavaEnvironment) =>
      {
      lazy val combinedAnns: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.lists.foldl[Map[hydra.core.Name,
         hydra.core.Term], Map[hydra.core.Name, hydra.core.Term]]((acc: Map[hydra.core.Name, hydra.core.Term]) =>
        (m: Map[hydra.core.Name, hydra.core.Term]) =>
        hydra.lib.maps.union[hydra.core.Name, hydra.core.Term](acc)(m))(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Term])(anns)
      hydra.lib.eithers.bind[hydra.errors.Error, Option[hydra.core.Type], hydra.java.syntax.Expression](hydra.lib.eithers.bimap[hydra.errors.DecodingError,
         Option[hydra.core.Type], hydra.errors.Error, Option[hydra.core.Type]]((__de: hydra.errors.DecodingError) => hydra.errors.Error.other(__de))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(combinedAnns)))((mtyp: Option[hydra.core.Type]) =>
        {
        lazy val annotatedBody: hydra.core.Term = hydra.lib.maybes.cases[hydra.core.Type, hydra.core.Term](mtyp)(v_Term_typeLambda_tl.body)((t: hydra.core.Type) =>
          t match
          case hydra.core.Type.forall(v_Type_forall_fa) => hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(v_Type_forall_fa.body)))(v_Term_typeLambda_tl.body)
          case _ => (v_Term_typeLambda_tl.body))
        hydra.java.coder.encodeTerm(env2)(annotatedBody)(cx)(g)
      })
    })
    case hydra.core.Term.inject(v_Term_inject_inj) => {
      lazy val injTypeName: hydra.core.Name = (v_Term_inject_inj.typeName)
      {
        lazy val injField: hydra.core.Field = (v_Term_inject_inj.field)
        {
          lazy val injFieldName: hydra.core.Name = (injField.name)
          {
            lazy val injFieldTerm: hydra.core.Term = (injField.term)
            {
              lazy val typeId: scala.Predef.String = hydra.java.utils.nameToJavaName(aliases)(injTypeName)
              {
                lazy val consId: hydra.java.syntax.Identifier = hydra.lib.strings.cat(Seq(typeId, ".",
                   hydra.java.utils.sanitizeJavaName(hydra.formatting.capitalize(injFieldName))))
                hydra.lib.eithers.bind[hydra.errors.Error, Boolean, hydra.java.syntax.Expression](hydra.java.coder.isFieldUnitType(injTypeName)(injFieldName)(cx)(g))((fieldIsUnit: Boolean) =>
                  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.Expression], hydra.java.syntax.Expression](hydra.lib.logic.ifElse[Either[hydra.errors.Error,
                     Seq[hydra.java.syntax.Expression]]](hydra.lib.logic.or(hydra.predicates.isUnitTerm(hydra.strip.deannotateTerm(injFieldTerm)))(fieldIsUnit))(Right(Seq()))(hydra.lib.eithers.bind[hydra.errors.Error,
                     hydra.java.syntax.Expression, Seq[hydra.java.syntax.Expression]](encode(injFieldTerm))((ex: hydra.java.syntax.Expression) => Right(Seq(ex)))))((args: Seq[hydra.java.syntax.Expression]) =>
                  Right(hydra.java.utils.javaConstructorCall(hydra.java.utils.javaConstructorName(consId)(None))(args)(None))))
              }
            }
          }
        }
      }
    }
    case hydra.core.Term.variable(v_Term_variable_name) => hydra.lib.maybes.cases[hydra.graph.Primitive,
       Either[hydra.errors.Error, hydra.java.syntax.Expression]](hydra.lib.maps.lookup[hydra.core.Name,
       hydra.graph.Primitive](v_Term_variable_name)(g.primitives))(hydra.java.coder.encodeVariable(env)(v_Term_variable_name)(cx)(g))((_prim: hydra.graph.Primitive) =>
      {
      lazy val combinedAnns: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.lists.foldl[Map[hydra.core.Name,
         hydra.core.Term], Map[hydra.core.Name, hydra.core.Term]]((acc: Map[hydra.core.Name, hydra.core.Term]) =>
        (m: Map[hydra.core.Name, hydra.core.Term]) =>
        hydra.lib.maps.union[hydra.core.Name, hydra.core.Term](acc)(m))(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Term])(anns)
      hydra.lib.eithers.bind[hydra.errors.Error, Option[hydra.core.Type], hydra.java.syntax.Expression](hydra.lib.eithers.bimap[hydra.errors.DecodingError,
         Option[hydra.core.Type], hydra.errors.Error, Option[hydra.core.Type]]((__de: hydra.errors.DecodingError) => hydra.errors.Error.other(__de))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(combinedAnns)))((mt: Option[hydra.core.Type]) =>
        hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Type, hydra.java.syntax.Expression](hydra.lib.maybes.cases[hydra.core.Type,
           Either[hydra.errors.Error, hydra.core.Type]](mt)(hydra.checking.typeOfTerm(cx)(g)(term))((t: hydra.core.Type) => Right(t)))((typ: hydra.core.Type) =>
        hydra.strip.deannotateType(typ) match
        case hydra.core.Type.function(v_Type_function_ft) => hydra.java.coder.encodeFunctionPrimitiveByName(env)(v_Type_function_ft.domain)(v_Type_function_ft.codomain)(v_Term_variable_name)(cx)(g)
        case _ => hydra.java.coder.encodeNullaryPrimitiveByName(env)(typ)(v_Term_variable_name)(cx)(g)))
    })
    case hydra.core.Term.unit => Right(hydra.java.utils.javaLiteralToJavaExpression(hydra.java.syntax.Literal.`null`))
    case hydra.core.Term.wrap(v_Term_wrap_wt) => hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression,
       hydra.java.syntax.Expression](encode(v_Term_wrap_wt.body))((jarg: hydra.java.syntax.Expression) =>
      Right(hydra.java.utils.javaConstructorCall(hydra.java.utils.javaConstructorName(hydra.java.utils.nameToJavaName(aliases)(v_Term_wrap_wt.typeName))(None))(Seq(jarg))(None)))
    case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => {
      lazy val atyp: hydra.core.Type = (v_Term_typeApplication_ta.`type`)
      {
        lazy val body: hydra.core.Term = (v_Term_typeApplication_ta.body)
        hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type, hydra.java.syntax.Expression](hydra.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(atyp)(cx)(g))((jatyp: hydra.java.syntax.Type) =>
          {
          lazy val combinedAnns: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.lists.foldl[Map[hydra.core.Name,
             hydra.core.Term], Map[hydra.core.Name, hydra.core.Term]]((acc: Map[hydra.core.Name, hydra.core.Term]) =>
            (m: Map[hydra.core.Name, hydra.core.Term]) =>
            hydra.lib.maps.union[hydra.core.Name, hydra.core.Term](acc)(m))(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Term])(anns)
          hydra.lib.eithers.bind[hydra.errors.Error, Option[hydra.core.Type], hydra.java.syntax.Expression](hydra.lib.eithers.bimap[hydra.errors.DecodingError,
             Option[hydra.core.Type], hydra.errors.Error, Option[hydra.core.Type]]((__de: hydra.errors.DecodingError) => hydra.errors.Error.other(__de))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(combinedAnns)))((mtyp: Option[hydra.core.Type]) =>
            hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Type, hydra.java.syntax.Expression](hydra.lib.maybes.cases[hydra.core.Type,
               Either[hydra.errors.Error, hydra.core.Type]](mtyp)(hydra.checking.typeOfTerm(cx)(g)(term))((t: hydra.core.Type) => Right(t)))((typ: hydra.core.Type) =>
            {
            lazy val collected0: Tuple2[hydra.core.Term, Seq[hydra.core.Type]] = hydra.java.coder.collectTypeApps0(body)(Seq(atyp))
            {
              lazy val innermostBody0: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, Seq[hydra.core.Type]](collected0)
              {
                lazy val allTypeArgs0: Seq[hydra.core.Type] = hydra.lib.pairs.second[hydra.core.Term, Seq[hydra.core.Type]](collected0)
                hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Type, hydra.java.syntax.Expression](hydra.java.coder.correctCastType(innermostBody0)(allTypeArgs0)(typ)(cx)(g))((correctedTyp: hydra.core.Type) =>
                  {
                  lazy val collected: Tuple2[hydra.core.Term, Seq[hydra.core.Type]] = hydra.java.coder.collectTypeApps(body)(Seq(atyp))
                  {
                    lazy val innermostBody: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, Seq[hydra.core.Type]](collected)
                    {
                      lazy val allTypeArgs: Seq[hydra.core.Type] = hydra.lib.pairs.second[hydra.core.Term, Seq[hydra.core.Type]](collected)
                      innermostBody match
                        case hydra.core.Term.variable(v_Term_variable_varName) => hydra.lib.eithers.bind[hydra.errors.Error,
                           hydra.java.environment.JavaSymbolClass, hydra.java.syntax.Expression](hydra.java.coder.classifyDataReference(v_Term_variable_varName)(cx)(g))((cls: hydra.java.environment.JavaSymbolClass) =>
                          hydra.java.coder.typeAppNullaryOrHoisted(env)(aliases)(anns)(tyapps)(jatyp)(body)(correctedTyp)(v_Term_variable_varName)(cls)(allTypeArgs)(cx)(g))
                        case hydra.core.Term.either(v_Term_either_eitherTerm) => hydra.lib.logic.ifElse[Either[hydra.errors.Error,
                           hydra.java.syntax.Expression]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Type](allTypeArgs))(2))({
                          lazy val eitherBranchTypes: Tuple2[hydra.core.Type, hydra.core.Type] = Tuple2(hydra.lib.lists.head[hydra.core.Type](allTypeArgs),
                             hydra.lib.lists.head[hydra.core.Type](hydra.lib.lists.tail[hydra.core.Type](allTypeArgs)))
                          hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.ReferenceType],
                             hydra.java.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Type,
                             hydra.java.syntax.ReferenceType, hydra.errors.Error]((t: hydra.core.Type) =>
                            hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type, hydra.java.syntax.ReferenceType](hydra.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(t)(cx)(g))((jt: hydra.java.syntax.Type) => hydra.java.utils.javaTypeToJavaReferenceType(jt)(cx)))(allTypeArgs))((jTypeArgs: Seq[hydra.java.syntax.ReferenceType]) =>
                            {
                            lazy val eitherTargs: Seq[hydra.java.syntax.TypeArgument] = hydra.lib.lists.map[hydra.java.syntax.ReferenceType,
                               hydra.java.syntax.TypeArgument]((rt: hydra.java.syntax.ReferenceType) => hydra.java.syntax.TypeArgument.reference(rt))(jTypeArgs)
                            {
                              def encodeEitherBranch(branchType: hydra.core.Type)(t1: hydra.core.Term): Either[hydra.errors.Error,
                                 hydra.java.syntax.Expression] =
                                {
                                lazy val annotated: hydra.core.Term = hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(branchType)))(t1)
                                hydra.java.coder.encodeTermInternal(env)(anns)(Seq())(annotated)(cx)(g)
                              }
                              hydra.lib.eithers.either[hydra.core.Term, hydra.core.Term, Either[hydra.errors.Error,
                                 hydra.java.syntax.Expression]]((term1: hydra.core.Term) =>
                                hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression,
                                   hydra.java.syntax.Expression](encodeEitherBranch(hydra.lib.pairs.first[hydra.core.Type,
                                   hydra.core.Type](eitherBranchTypes))(term1))((expr: hydra.java.syntax.Expression) =>
                                Right(hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocationStaticWithTypeArgs("hydra.util.Either")("left")(eitherTargs)(Seq(expr))))))((term1: hydra.core.Term) =>
                                hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression,
                                   hydra.java.syntax.Expression](encodeEitherBranch(hydra.lib.pairs.second[hydra.core.Type,
                                   hydra.core.Type](eitherBranchTypes))(term1))((expr: hydra.java.syntax.Expression) =>
                                Right(hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocationStaticWithTypeArgs("hydra.util.Either")("right")(eitherTargs)(Seq(expr))))))(v_Term_either_eitherTerm)
                            }
                          })
                        })(hydra.java.coder.typeAppFallbackCast(env)(aliases)(anns)(tyapps)(jatyp)(body)(correctedTyp)(cx)(g))
                        case _ => hydra.java.coder.typeAppFallbackCast(env)(aliases)(anns)(tyapps)(jatyp)(body)(correctedTyp)(cx)(g)
                    }
                  }
                })
              }
            }
          }))
        })
      }
    }
    case _ => Right(hydra.java.coder.encodeLiteral(hydra.core.Literal.string("Unimplemented term variant")))
}

def encodeTermTCO(env0: hydra.java.environment.JavaEnvironment)(funcName: hydra.core.Name)(paramNames: Seq[hydra.core.Name])(tcoVarRenames: Map[hydra.core.Name,
   hydra.core.Name])(tcoDepth: Int)(term: hydra.core.Term)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   Seq[hydra.java.syntax.BlockStatement]] =
  {
  lazy val aliases0: hydra.java.environment.Aliases = (env0.aliases)
  lazy val env: hydra.java.environment.JavaEnvironment = hydra.java.environment.JavaEnvironment(hydra.java.environment.Aliases(aliases0.currentNamespace,
     (aliases0.packages), (aliases0.branchVars), (aliases0.recursiveVars), (aliases0.inScopeTypeParams),
     (aliases0.polymorphicLocals), (aliases0.inScopeJavaVars), hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](tcoVarRenames)(aliases0.varRenames),
     (aliases0.lambdaVars), (aliases0.typeVarSubst), (aliases0.trustedTypeVars), (aliases0.methodCodomain),
     (aliases0.thunkedVars)), (env0.graph))
  lazy val stripped: hydra.core.Term = hydra.strip.deannotateAndDetypeTerm(term)
  lazy val gathered: Tuple2[Seq[hydra.core.Term], hydra.core.Term] = hydra.analysis.gatherApplications(stripped)
  lazy val gatherArgs: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term], hydra.core.Term](gathered)
  lazy val gatherFun: hydra.core.Term = hydra.lib.pairs.second[Seq[hydra.core.Term], hydra.core.Term](gathered)
  lazy val strippedFun: hydra.core.Term = hydra.strip.deannotateAndDetypeTerm(gatherFun)
  lazy val isSelfCall: Boolean = strippedFun match
    case hydra.core.Term.variable(v_Term_variable_n) => hydra.lib.equality.equal[hydra.core.Name](v_Term_variable_n)(funcName)
    case _ => false
  hydra.lib.logic.ifElse[Either[hydra.errors.Error, Seq[hydra.java.syntax.BlockStatement]]](hydra.lib.logic.and(isSelfCall)(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Term](gatherArgs))(hydra.lib.lists.length[hydra.core.Name](paramNames))))({
    lazy val changePairs: Seq[Tuple2[hydra.core.Name, hydra.core.Term]] = hydra.lib.lists.filter[Tuple2[hydra.core.Name,
       hydra.core.Term]]((pair: Tuple2[hydra.core.Name, hydra.core.Term]) =>
      hydra.lib.logic.not(hydra.strip.deannotateAndDetypeTerm(hydra.lib.pairs.second[hydra.core.Name, hydra.core.Term](pair)) match
      case hydra.core.Term.variable(v_Term_variable_n) => hydra.lib.equality.equal[hydra.core.Name](v_Term_variable_n)(hydra.lib.pairs.first[hydra.core.Name,
         hydra.core.Term](pair))
      case _ => false))(hydra.lib.lists.zip[hydra.core.Name, hydra.core.Term](paramNames)(gatherArgs))
    {
      lazy val changedParams: Seq[hydra.core.Name] = hydra.lib.lists.map[Tuple2[hydra.core.Name, hydra.core.Term],
         hydra.core.Name](hydra.lib.pairs.first[hydra.core.Name, hydra.core.Term])(changePairs)
      hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.Expression], Seq[hydra.java.syntax.BlockStatement]](hydra.lib.eithers.mapList[Tuple2[hydra.core.Name,
         hydra.core.Term], hydra.java.syntax.Expression, hydra.errors.Error]((pair: Tuple2[hydra.core.Name,
         hydra.core.Term]) =>
        hydra.java.coder.encodeTerm(env)(hydra.lib.pairs.second[hydra.core.Name, hydra.core.Term](pair))(cx)(g))(changePairs))((jChangedArgs: Seq[hydra.java.syntax.Expression]) =>
        {
        lazy val assignments: Seq[hydra.java.syntax.BlockStatement] = hydra.lib.lists.map[Tuple2[hydra.core.Name,
           hydra.java.syntax.Expression], hydra.java.syntax.BlockStatement]((pair: Tuple2[hydra.core.Name,
           hydra.java.syntax.Expression]) =>
          {
          lazy val paramName: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, hydra.java.syntax.Expression](pair)
          {
            lazy val jArg: hydra.java.syntax.Expression = hydra.lib.pairs.second[hydra.core.Name, hydra.java.syntax.Expression](pair)
            hydra.java.syntax.BlockStatement.statement(hydra.java.utils.javaAssignmentStatement(hydra.java.syntax.LeftHandSide.expressionName(hydra.java.utils.javaIdentifierToJavaExpressionName(hydra.java.utils.variableToJavaIdentifier(paramName))))(jArg))
          }
        })(hydra.lib.lists.zip[hydra.core.Name, hydra.java.syntax.Expression](changedParams)(jChangedArgs))
        {
          lazy val continueStmt: hydra.java.syntax.BlockStatement = hydra.java.syntax.BlockStatement.statement(hydra.java.syntax.Statement.withoutTrailing(hydra.java.syntax.StatementWithoutTrailingSubstatement.continue(None)))
          Right(hydra.lib.lists.concat2[hydra.java.syntax.BlockStatement](assignments)(Seq(continueStmt)))
        }
      })
    }
  })(stripped match
    case hydra.core.Term.let(v_Term_let_lt) => {
      lazy val letBindings: Seq[hydra.core.Binding] = (v_Term_let_lt.bindings)
      {
        lazy val letBody: hydra.core.Term = (v_Term_let_lt.body)
        hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[Seq[hydra.java.syntax.BlockStatement], hydra.java.environment.JavaEnvironment],
           Seq[hydra.java.syntax.BlockStatement]](hydra.java.coder.bindingsToStatements(env)(letBindings)(cx)(g))((bindResult: Tuple2[Seq[hydra.java.syntax.BlockStatement],
           hydra.java.environment.JavaEnvironment]) =>
          {
          lazy val letStmts: Seq[hydra.java.syntax.BlockStatement] = hydra.lib.pairs.first[Seq[hydra.java.syntax.BlockStatement],
             hydra.java.environment.JavaEnvironment](bindResult)
          {
            lazy val envAfterLet: hydra.java.environment.JavaEnvironment = hydra.lib.pairs.second[Seq[hydra.java.syntax.BlockStatement],
               hydra.java.environment.JavaEnvironment](bindResult)
            hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.BlockStatement], Seq[hydra.java.syntax.BlockStatement]](hydra.java.coder.encodeTermTCO(envAfterLet)(funcName)(paramNames)(tcoVarRenames)(tcoDepth)(letBody)(cx)(g))((tcoBodyStmts: Seq[hydra.java.syntax.BlockStatement]) =>
              Right(hydra.lib.lists.concat2[hydra.java.syntax.BlockStatement](letStmts)(tcoBodyStmts)))
          }
        })
      }
    }
    case _ => {
      lazy val gathered2: Tuple2[Seq[hydra.core.Term], hydra.core.Term] = hydra.analysis.gatherApplications(term)
      {
        lazy val args2: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term], hydra.core.Term](gathered2)
        {
          lazy val body2: hydra.core.Term = hydra.lib.pairs.second[Seq[hydra.core.Term], hydra.core.Term](gathered2)
          hydra.lib.logic.ifElse[Either[hydra.errors.Error, Seq[hydra.java.syntax.BlockStatement]]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Term](args2))(1))({
            lazy val arg: hydra.core.Term = hydra.lib.lists.head[hydra.core.Term](args2)
            hydra.strip.deannotateAndDetypeTerm(body2) match
              case hydra.core.Term.cases(v_Term_cases_cs) => {
                lazy val aliases: hydra.java.environment.Aliases = (env.aliases)
                {
                  lazy val tname: hydra.core.Name = (v_Term_cases_cs.typeName)
                  {
                    lazy val dflt: Option[hydra.core.Term] = (v_Term_cases_cs.default)
                    {
                      lazy val `cases_`: Seq[hydra.core.Field] = (v_Term_cases_cs.cases)
                      hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.TypeArgument],
                         Seq[hydra.java.syntax.BlockStatement]](hydra.java.coder.domTypeArgs(aliases)(hydra.resolution.nominalApplication(tname)(Seq()))(cx)(g))((domArgs: Seq[hydra.java.syntax.TypeArgument]) =>
                        hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression, Seq[hydra.java.syntax.BlockStatement]](hydra.java.coder.encodeTerm(env)(arg)(cx)(g))((jArgRaw: hydra.java.syntax.Expression) =>
                        {
                        lazy val depthSuffix: scala.Predef.String = hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](tcoDepth)(0))("")(hydra.lib.literals.showInt32(tcoDepth))
                        {
                          lazy val matchVarId: hydra.java.syntax.Identifier = hydra.java.utils.javaIdentifier(hydra.lib.strings.cat(Seq("_tco_match_",
                             hydra.formatting.decapitalize(hydra.names.localNameOf(tname)), depthSuffix)))
                          {
                            lazy val matchDecl: hydra.java.syntax.BlockStatement = hydra.java.utils.varDeclarationStatement(matchVarId)(jArgRaw)
                            {
                              lazy val jArg: hydra.java.syntax.Expression = hydra.java.utils.javaIdentifierToJavaExpression(matchVarId)
                              hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.BlockStatement],
                                 Seq[hydra.java.syntax.BlockStatement]](hydra.lib.eithers.mapList[hydra.core.Field,
                                 hydra.java.syntax.BlockStatement, hydra.errors.Error]((field: hydra.core.Field) =>
                                {
                                lazy val fieldName: hydra.core.Name = (field.name)
                                {
                                  lazy val variantRefType: hydra.java.syntax.ReferenceType = hydra.java.utils.nameToJavaReferenceType(aliases)(true)(domArgs)(tname)(Some(hydra.formatting.capitalize(fieldName)))
                                  hydra.strip.deannotateTerm(field.term) match
                                    case hydra.core.Term.lambda(v_Term_lambda_lam) => hydra.java.coder.withLambda(env)(v_Term_lambda_lam)((env2: hydra.java.environment.JavaEnvironment) =>
                                      {
                                      lazy val lambdaParam: hydra.core.Name = (v_Term_lambda_lam.parameter)
                                      {
                                        lazy val branchBody: hydra.core.Term = (v_Term_lambda_lam.body)
                                        {
                                          lazy val env3: hydra.java.environment.JavaEnvironment = hydra.java.coder.insertBranchVar(lambdaParam)(env2)
                                          {
                                            lazy val varId: hydra.java.syntax.Identifier = hydra.java.utils.variableToJavaIdentifier(lambdaParam)
                                            {
                                              lazy val castExpr: hydra.java.syntax.Expression = hydra.java.utils.javaCastExpressionToJavaExpression(hydra.java.utils.javaCastExpression(variantRefType)(hydra.java.utils.javaExpressionToJavaUnaryExpression(jArg)))
                                              {
                                                lazy val localDecl: hydra.java.syntax.BlockStatement = hydra.java.utils.varDeclarationStatement(varId)(castExpr)
                                                {
                                                  lazy val isBranchTailCall: Boolean = hydra.analysis.isTailRecursiveInTailPosition(funcName)(branchBody)
                                                  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.BlockStatement],
                                                     hydra.java.syntax.BlockStatement](hydra.lib.logic.ifElse[Either[hydra.errors.Error,
                                                     Seq[hydra.java.syntax.BlockStatement]]](isBranchTailCall)(hydra.java.coder.encodeTermTCO(env3)(funcName)(paramNames)(tcoVarRenames)(hydra.lib.math.add(tcoDepth)(1))(branchBody)(cx)(g))(hydra.lib.eithers.bind[hydra.errors.Error,
                                                     hydra.typing.FunctionStructure[hydra.java.environment.JavaEnvironment],
                                                     Seq[hydra.java.syntax.BlockStatement]](hydra.java.coder.analyzeJavaFunction(env3)(branchBody)(cx)(g))((fs: hydra.typing.FunctionStructure[hydra.java.environment.JavaEnvironment]) =>
                                                    {
                                                    lazy val bindings: Seq[hydra.core.Binding] = (fs.bindings)
                                                    {
                                                      lazy val innerBody: hydra.core.Term = (fs.body)
                                                      {
                                                        lazy val env4: hydra.java.environment.JavaEnvironment = (fs.environment)
                                                        hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[Seq[hydra.java.syntax.BlockStatement],
                                                           hydra.java.environment.JavaEnvironment], Seq[hydra.java.syntax.BlockStatement]](hydra.java.coder.bindingsToStatements(env4)(bindings)(cx)(g))((bindResult: Tuple2[Seq[hydra.java.syntax.BlockStatement],
                                                           hydra.java.environment.JavaEnvironment]) =>
                                                          {
                                                          lazy val bindingStmts: Seq[hydra.java.syntax.BlockStatement] = hydra.lib.pairs.first[Seq[hydra.java.syntax.BlockStatement],
                                                             hydra.java.environment.JavaEnvironment](bindResult)
                                                          {
                                                            lazy val env5: hydra.java.environment.JavaEnvironment = hydra.lib.pairs.second[Seq[hydra.java.syntax.BlockStatement],
                                                               hydra.java.environment.JavaEnvironment](bindResult)
                                                            hydra.lib.eithers.bind[hydra.errors.Error,
                                                               hydra.java.syntax.Expression, Seq[hydra.java.syntax.BlockStatement]](hydra.java.coder.encodeTerm(env5)(innerBody)(cx)(g))((jret: hydra.java.syntax.Expression) =>
                                                              {
                                                              lazy val returnStmt: hydra.java.syntax.BlockStatement = hydra.java.syntax.BlockStatement.statement(hydra.java.utils.javaReturnStatement(Some(jret)))
                                                              Right(hydra.lib.lists.concat2[hydra.java.syntax.BlockStatement](bindingStmts)(Seq(returnStmt)))
                                                            })
                                                          }
                                                        })
                                                      }
                                                    }
                                                  })))((bodyStmts: Seq[hydra.java.syntax.BlockStatement]) =>
                                                    {
                                                    lazy val relExpr: hydra.java.syntax.RelationalExpression = hydra.java.utils.javaInstanceOf(hydra.java.utils.javaUnaryExpressionToJavaRelationalExpression(hydra.java.utils.javaExpressionToJavaUnaryExpression(jArg)))(variantRefType)
                                                    {
                                                      lazy val condExpr: hydra.java.syntax.Expression = hydra.java.utils.javaRelationalExpressionToJavaExpression(relExpr)
                                                      {
                                                        lazy val blockStmts: Seq[hydra.java.syntax.BlockStatement] = hydra.lib.lists.cons[hydra.java.syntax.BlockStatement](localDecl)(bodyStmts)
                                                        {
                                                          lazy val ifBody: hydra.java.syntax.Statement = hydra.java.syntax.Statement.withoutTrailing(hydra.java.syntax.StatementWithoutTrailingSubstatement.block(blockStmts))
                                                          Right(hydra.java.syntax.BlockStatement.statement(hydra.java.syntax.Statement.ifThen(hydra.java.syntax.IfThenStatement(condExpr,
                                                             ifBody))))
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
                                    })
                                    case _ => Left(hydra.errors.Error.other("TCO: case branch is not a lambda"))
                                }
                              })(`cases_`))((ifBlocks: Seq[hydra.java.syntax.BlockStatement]) =>
                                hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.BlockStatement],
                                   Seq[hydra.java.syntax.BlockStatement]](hydra.lib.maybes.cases[hydra.core.Term,
                                   Either[hydra.errors.Error, Seq[hydra.java.syntax.BlockStatement]]](dflt)(Right(Seq(hydra.java.syntax.BlockStatement.statement(hydra.java.utils.javaReturnStatement(Some(jArg))))))((d: hydra.core.Term) =>
                                hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression,
                                   Seq[hydra.java.syntax.BlockStatement]](hydra.java.coder.encodeTerm(env)(d)(cx)(g))((dExpr: hydra.java.syntax.Expression) =>
                                Right(Seq(hydra.java.syntax.BlockStatement.statement(hydra.java.utils.javaReturnStatement(Some(dExpr))))))))((defaultStmt: Seq[hydra.java.syntax.BlockStatement]) =>
                                Right(hydra.lib.lists.concat[hydra.java.syntax.BlockStatement](Seq(Seq(matchDecl), ifBlocks, defaultStmt)))))
                            }
                          }
                        }
                      }))
                    }
                  }
                }
              }
              case _ => hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression, Seq[hydra.java.syntax.BlockStatement]](hydra.java.coder.encodeTerm(env)(term)(cx)(g))((expr: hydra.java.syntax.Expression) =>
                Right(Seq(hydra.java.syntax.BlockStatement.statement(hydra.java.utils.javaReturnStatement(Some(expr))))))
          })(hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression, Seq[hydra.java.syntax.BlockStatement]](hydra.java.coder.encodeTerm(env)(term)(cx)(g))((expr: hydra.java.syntax.Expression) =>
            Right(Seq(hydra.java.syntax.BlockStatement.statement(hydra.java.utils.javaReturnStatement(Some(expr)))))))
        }
      }
    })
}

def encodeType[T0](aliases: hydra.java.environment.Aliases)(boundVars: scala.collection.immutable.Set[hydra.core.Name])(t: hydra.core.Type)(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.Type] =
  {
  lazy val inScopeTypeParams: scala.collection.immutable.Set[hydra.core.Name] = (aliases.inScopeTypeParams)
  lazy val typeVarSubst: Map[hydra.core.Name, hydra.core.Name] = (aliases.typeVarSubst)
  hydra.strip.deannotateType(t) match
    case hydra.core.Type.application(v_Type_application_at) => hydra.lib.eithers.bind[hydra.errors.Error,
       hydra.java.syntax.Type, hydra.java.syntax.Type](hydra.java.coder.encodeType(aliases)(boundVars)(v_Type_application_at.function)(cx)(g))((jlhs: hydra.java.syntax.Type) =>
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType, hydra.java.syntax.Type](hydra.lib.eithers.bind[hydra.errors.Error,
         hydra.java.syntax.Type, hydra.java.syntax.ReferenceType](hydra.java.coder.encodeType(aliases)(boundVars)(v_Type_application_at.argument)(cx)(g))((`jt_`: hydra.java.syntax.Type) => hydra.java.utils.javaTypeToJavaReferenceType(`jt_`)(cx)))((jrhs: hydra.java.syntax.ReferenceType) => hydra.java.utils.addJavaTypeParameter(jrhs)(jlhs)(cx)))
    case hydra.core.Type.function(v_Type_function_ft) => hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType,
       hydra.java.syntax.Type](hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type, hydra.java.syntax.ReferenceType](hydra.java.coder.encodeType(aliases)(boundVars)(v_Type_function_ft.domain)(cx)(g))((`jt_`: hydra.java.syntax.Type) => hydra.java.utils.javaTypeToJavaReferenceType(`jt_`)(cx)))((jdom: hydra.java.syntax.ReferenceType) =>
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType, hydra.java.syntax.Type](hydra.lib.eithers.bind[hydra.errors.Error,
         hydra.java.syntax.Type, hydra.java.syntax.ReferenceType](hydra.java.coder.encodeType(aliases)(boundVars)(v_Type_function_ft.codomain)(cx)(g))((`jt_`: hydra.java.syntax.Type) => hydra.java.utils.javaTypeToJavaReferenceType(`jt_`)(cx)))((jcod: hydra.java.syntax.ReferenceType) =>
      Right(hydra.java.utils.javaRefType(Seq(jdom, jcod))(hydra.java.names.javaUtilFunctionPackageName)("Function"))))
    case hydra.core.Type.forall(v_Type_forall_fa) => hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type,
       hydra.java.syntax.Type](hydra.java.coder.encodeType(aliases)(hydra.lib.sets.insert[hydra.core.Name](v_Type_forall_fa.parameter)(boundVars))(v_Type_forall_fa.body)(cx)(g))((jbody: hydra.java.syntax.Type) =>
      hydra.java.utils.addJavaTypeParameter(hydra.java.utils.javaTypeVariable(v_Type_forall_fa.parameter))(jbody)(cx))
    case hydra.core.Type.list(v_Type_list_et) => hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type,
       hydra.java.syntax.Type](hydra.java.coder.encodeType(aliases)(boundVars)(v_Type_list_et)(cx)(g))((jet: hydra.java.syntax.Type) =>
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType, hydra.java.syntax.Type](hydra.lib.eithers.bind[hydra.errors.Error,
         hydra.java.syntax.Type, hydra.java.syntax.ReferenceType](Right(jet))((`jt_`: hydra.java.syntax.Type) => hydra.java.utils.javaTypeToJavaReferenceType(`jt_`)(cx)))((rt: hydra.java.syntax.ReferenceType) =>
      Right(hydra.java.utils.javaRefType(Seq(rt))(hydra.java.names.javaUtilPackageName)("List"))))
    case hydra.core.Type.literal(v_Type_literal_lt) => hydra.java.coder.encodeLiteralType(v_Type_literal_lt)(cx)(g)
    case hydra.core.Type.either(v_Type_either_et) => hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType,
       hydra.java.syntax.Type](hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type, hydra.java.syntax.ReferenceType](hydra.java.coder.encodeType(aliases)(boundVars)(v_Type_either_et.left)(cx)(g))((`jt_`: hydra.java.syntax.Type) => hydra.java.utils.javaTypeToJavaReferenceType(`jt_`)(cx)))((jlt: hydra.java.syntax.ReferenceType) =>
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType, hydra.java.syntax.Type](hydra.lib.eithers.bind[hydra.errors.Error,
         hydra.java.syntax.Type, hydra.java.syntax.ReferenceType](hydra.java.coder.encodeType(aliases)(boundVars)(v_Type_either_et.right)(cx)(g))((`jt_`: hydra.java.syntax.Type) => hydra.java.utils.javaTypeToJavaReferenceType(`jt_`)(cx)))((jrt: hydra.java.syntax.ReferenceType) =>
      Right(hydra.java.utils.javaRefType(Seq(jlt, jrt))(hydra.java.names.hydraUtilPackageName)("Either"))))
    case hydra.core.Type.map(v_Type_map_mt) => hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType,
       hydra.java.syntax.Type](hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type, hydra.java.syntax.ReferenceType](hydra.java.coder.encodeType(aliases)(boundVars)(v_Type_map_mt.keys)(cx)(g))((`jt_`: hydra.java.syntax.Type) => hydra.java.utils.javaTypeToJavaReferenceType(`jt_`)(cx)))((jkt: hydra.java.syntax.ReferenceType) =>
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType, hydra.java.syntax.Type](hydra.lib.eithers.bind[hydra.errors.Error,
         hydra.java.syntax.Type, hydra.java.syntax.ReferenceType](hydra.java.coder.encodeType(aliases)(boundVars)(v_Type_map_mt.values)(cx)(g))((`jt_`: hydra.java.syntax.Type) => hydra.java.utils.javaTypeToJavaReferenceType(`jt_`)(cx)))((jvt: hydra.java.syntax.ReferenceType) =>
      Right(hydra.java.utils.javaRefType(Seq(jkt, jvt))(hydra.java.names.javaUtilPackageName)("Map"))))
    case hydra.core.Type.pair(v_Type_pair_pt) => hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType,
       hydra.java.syntax.Type](hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type, hydra.java.syntax.ReferenceType](hydra.java.coder.encodeType(aliases)(boundVars)(v_Type_pair_pt.first)(cx)(g))((`jt_`: hydra.java.syntax.Type) => hydra.java.utils.javaTypeToJavaReferenceType(`jt_`)(cx)))((jfirst: hydra.java.syntax.ReferenceType) =>
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType, hydra.java.syntax.Type](hydra.lib.eithers.bind[hydra.errors.Error,
         hydra.java.syntax.Type, hydra.java.syntax.ReferenceType](hydra.java.coder.encodeType(aliases)(boundVars)(v_Type_pair_pt.second)(cx)(g))((`jt_`: hydra.java.syntax.Type) => hydra.java.utils.javaTypeToJavaReferenceType(`jt_`)(cx)))((jsecond: hydra.java.syntax.ReferenceType) =>
      Right(hydra.java.utils.javaRefType(Seq(jfirst, jsecond))(hydra.java.names.hydraUtilPackageName)("Pair"))))
    case hydra.core.Type.unit => Right(hydra.java.utils.javaRefType(Seq())(hydra.java.names.javaLangPackageName)("Void"))
    case hydra.core.Type.record(v_Type_record_rt) => hydra.lib.logic.ifElse[Either[hydra.errors.Error,
       hydra.java.syntax.Type]](hydra.lib.lists.`null`[hydra.core.FieldType](v_Type_record_rt))(Right(hydra.java.utils.javaRefType(Seq())(hydra.java.names.javaLangPackageName)("Void")))(Left(hydra.errors.Error.other("unexpected anonymous record type")))
    case hydra.core.Type.maybe(v_Type_maybe_ot) => hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType,
       hydra.java.syntax.Type](hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type, hydra.java.syntax.ReferenceType](hydra.java.coder.encodeType(aliases)(boundVars)(v_Type_maybe_ot)(cx)(g))((`jt_`: hydra.java.syntax.Type) => hydra.java.utils.javaTypeToJavaReferenceType(`jt_`)(cx)))((jot: hydra.java.syntax.ReferenceType) =>
      Right(hydra.java.utils.javaRefType(Seq(jot))(hydra.java.names.hydraUtilPackageName)("Maybe")))
    case hydra.core.Type.set(v_Type_set_st) => hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType,
       hydra.java.syntax.Type](hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type, hydra.java.syntax.ReferenceType](hydra.java.coder.encodeType(aliases)(boundVars)(v_Type_set_st)(cx)(g))((`jt_`: hydra.java.syntax.Type) => hydra.java.utils.javaTypeToJavaReferenceType(`jt_`)(cx)))((jst: hydra.java.syntax.ReferenceType) =>
      Right(hydra.java.utils.javaRefType(Seq(jst))(hydra.java.names.javaUtilPackageName)("Set")))
    case hydra.core.Type.union(v_Type_union__) => Left(hydra.errors.Error.other("unexpected anonymous union type"))
    case hydra.core.Type.variable(v_Type_variable_name0) => {
      lazy val name: hydra.core.Name = hydra.lib.maybes.fromMaybe[hydra.core.Name](v_Type_variable_name0)(hydra.lib.maps.lookup[hydra.core.Name,
         hydra.core.Name](v_Type_variable_name0)(typeVarSubst))
      hydra.lib.eithers.bind[hydra.errors.Error, Option[hydra.core.Type], hydra.java.syntax.Type](hydra.java.coder.encodeType_resolveIfTypedef(aliases)(boundVars)(inScopeTypeParams)(name)(cx)(g))((resolved: Option[hydra.core.Type]) =>
        hydra.lib.maybes.cases[hydra.core.Type, Either[hydra.errors.Error, hydra.java.syntax.Type]](resolved)(Right(hydra.lib.logic.ifElse[hydra.java.syntax.Type](hydra.lib.logic.or(hydra.lib.sets.member[hydra.core.Name](name)(boundVars))(hydra.lib.sets.member[hydra.core.Name](name)(inScopeTypeParams)))(hydra.java.syntax.Type.reference(hydra.java.utils.javaTypeVariable(name)))(hydra.lib.logic.ifElse[hydra.java.syntax.Type](hydra.java.coder.isLambdaBoundVariable(name))(hydra.java.syntax.Type.reference(hydra.java.utils.javaTypeVariable(name)))(hydra.lib.logic.ifElse[hydra.java.syntax.Type](hydra.java.coder.isUnresolvedInferenceVar(name))(hydra.java.syntax.Type.reference(hydra.java.syntax.ReferenceType.classOrInterface(hydra.java.syntax.ClassOrInterfaceType.`class`(hydra.java.utils.javaClassType(Seq())(hydra.java.names.javaLangPackageName)("Object")))))(hydra.java.syntax.Type.reference(hydra.java.utils.nameToJavaReferenceType(aliases)(true)(Seq())(name)(None)))))))((resolvedType: hydra.core.Type) =>
        hydra.java.coder.encodeType(aliases)(boundVars)(resolvedType)(cx)(g)))
    }
    case hydra.core.Type.wrap(v_Type_wrap__) => Left(hydra.errors.Error.other("unexpected anonymous wrap type"))
    case _ => Left(hydra.errors.Error.other(hydra.lib.strings.cat2("can't encode unsupported type in Java: ")(hydra.show.core.`type`(t))))
}

def encodeTypeDefinition(pkg: hydra.java.syntax.PackageDeclaration)(aliases: hydra.java.environment.Aliases)(tdef: hydra.packaging.TypeDefinition)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   Tuple2[hydra.core.Name, hydra.java.syntax.CompilationUnit]] =
  {
  lazy val name: hydra.core.Name = (tdef.name)
  lazy val typ: hydra.core.Type = (tdef.`type`.`type`)
  lazy val serializable: Boolean = hydra.java.coder.isSerializableJavaType(typ)
  lazy val imports: Seq[hydra.java.syntax.ImportDeclaration] = hydra.lib.logic.ifElse[Seq[hydra.java.syntax.ImportDeclaration]](serializable)(Seq(hydra.java.syntax.ImportDeclaration.singleType(hydra.java.utils.javaTypeName("java.io.Serializable"))))(Seq())
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ClassDeclaration, Tuple2[hydra.core.Name,
     hydra.java.syntax.CompilationUnit]](hydra.java.coder.toClassDecl(false)(serializable)(aliases)(Seq())(name)(typ)(cx)(g))((decl: hydra.java.syntax.ClassDeclaration) =>
    hydra.lib.eithers.bind[hydra.errors.Error, Option[scala.Predef.String], Tuple2[hydra.core.Name, hydra.java.syntax.CompilationUnit]](hydra.annotations.getTypeDescription(cx)(g)(typ))((comment: Option[scala.Predef.String]) =>
    {
    lazy val tdecl: hydra.java.syntax.TypeDeclarationWithComments = hydra.java.syntax.TypeDeclarationWithComments(hydra.java.syntax.TypeDeclaration.`class`(decl),
       comment)
    Right(Tuple2(name, hydra.java.syntax.CompilationUnit.ordinary(hydra.java.syntax.OrdinaryCompilationUnit(Some(pkg), imports, Seq(tdecl)))))
  }))
}

def encodeType_resolveIfTypedef[T0, T1, T2](aliases: T0)(boundVars: scala.collection.immutable.Set[hydra.core.Name])(inScopeTypeParams: scala.collection.immutable.Set[hydra.core.Name])(name: hydra.core.Name)(cx: T1)(g: hydra.graph.Graph): Either[T2,
   Option[hydra.core.Type]] =
  hydra.lib.logic.ifElse[Either[T2, Option[hydra.core.Type]]](hydra.lib.logic.or(hydra.lib.sets.member[hydra.core.Name](name)(boundVars))(hydra.lib.sets.member[hydra.core.Name](name)(inScopeTypeParams)))(Right(None))(hydra.lib.logic.ifElse[Either[T2,
     Option[hydra.core.Type]]](hydra.java.coder.isLambdaBoundVariable(name))(Right(None))({
  lazy val schemaTypes: Map[hydra.core.Name, hydra.core.TypeScheme] = (g.schemaTypes)
  hydra.lib.maybes.cases[hydra.core.TypeScheme, Either[T2, Option[hydra.core.Type]]](hydra.lib.maps.lookup[hydra.core.Name,
     hydra.core.TypeScheme](name)(schemaTypes))(Right(None))((ts: hydra.core.TypeScheme) =>
    hydra.lib.logic.ifElse[Either[T2, Option[hydra.core.Type]]](hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.core.Name](ts.variables)))(Right(None))(hydra.strip.deannotateType(ts.`type`) match
    case hydra.core.Type.record(v_Type_record__) => Right(None)
    case hydra.core.Type.union(v_Type_union__) => Right(None)
    case hydra.core.Type.wrap(v_Type_wrap__) => Right(None)
    case _ => Right(Some(ts.`type`))))
}))

def encodeVariable[T0](env: hydra.java.environment.JavaEnvironment)(name: hydra.core.Name)(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.Expression] =
  {
  lazy val aliases: hydra.java.environment.Aliases = (env.aliases)
  lazy val resolvedName: hydra.core.Name = hydra.java.utils.lookupJavaVarName(aliases)(name)
  lazy val jid: hydra.java.syntax.Identifier = hydra.java.utils.javaIdentifier(resolvedName)
  hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.java.syntax.Expression]](hydra.lib.sets.member[hydra.core.Name](name)(aliases.branchVars))(Right(hydra.java.utils.javaFieldAccessToJavaExpression(hydra.java.syntax.FieldAccess(hydra.java.syntax.FieldAccess_Qualifier.primary(hydra.java.utils.javaExpressionToJavaPrimary(hydra.java.utils.javaIdentifierToJavaExpression(jid))),
     hydra.java.utils.javaIdentifier(hydra.java.names.valueFieldName)))))(hydra.lib.logic.ifElse[Either[hydra.errors.Error,
     hydra.java.syntax.Expression]](hydra.lib.logic.and(hydra.lib.equality.equal[hydra.core.Name](name)(hydra.lib.strings.cat(Seq(hydra.java.names.instanceName,
     "_", hydra.java.names.valueFieldName))))(hydra.java.coder.isRecursiveVariable(aliases)(name)))({
    lazy val instanceExpr: hydra.java.syntax.Expression = hydra.java.utils.javaIdentifierToJavaExpression(hydra.java.utils.javaIdentifier(hydra.java.names.instanceName))
    Right(hydra.java.utils.javaFieldAccessToJavaExpression(hydra.java.syntax.FieldAccess(hydra.java.syntax.FieldAccess_Qualifier.primary(hydra.java.utils.javaExpressionToJavaPrimary(instanceExpr)),
       hydra.java.utils.javaIdentifier(hydra.java.names.valueFieldName))))
  })(hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.java.syntax.Expression]](hydra.lib.logic.and(hydra.java.coder.isRecursiveVariable(aliases)(name))(hydra.lib.logic.not(hydra.java.coder.isLambdaBoundIn(name)(aliases.lambdaVars))))(Right(hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocation(Some(Left(hydra.java.syntax.ExpressionName(None,
     jid))))(hydra.java.names.getMethodName)(Seq()))))(hydra.lib.logic.ifElse[Either[hydra.errors.Error,
     hydra.java.syntax.Expression]](hydra.lib.logic.and(hydra.lib.sets.member[hydra.core.Name](name)(aliases.thunkedVars))(hydra.lib.logic.not(hydra.java.coder.isLambdaBoundIn(name)(aliases.lambdaVars))))(Right(hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocation(Some(Left(hydra.java.syntax.ExpressionName(None,
     jid))))(hydra.java.names.getMethodName)(Seq()))))(hydra.lib.logic.ifElse[Either[hydra.errors.Error,
     hydra.java.syntax.Expression]](hydra.java.coder.isLambdaBoundIn(name)(aliases.lambdaVars))({
    lazy val actualName: hydra.core.Name = hydra.java.coder.findMatchingLambdaVar(name)(aliases.lambdaVars)
    {
      lazy val resolvedActual: hydra.core.Name = hydra.java.utils.lookupJavaVarName(aliases)(actualName)
      Right(hydra.java.utils.javaIdentifierToJavaExpression(hydra.java.utils.variableToJavaIdentifier(resolvedActual)))
    }
  })(hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.java.syntax.Expression]](hydra.lib.sets.member[hydra.core.Name](name)(aliases.inScopeJavaVars))(Right(hydra.java.utils.javaIdentifierToJavaExpression(hydra.java.coder.elementJavaIdentifier(false)(false)(aliases)(resolvedName))))(hydra.lib.eithers.bind[hydra.errors.Error,
     hydra.java.environment.JavaSymbolClass, hydra.java.syntax.Expression](hydra.java.coder.classifyDataReference(name)(cx)(g))((cls: hydra.java.environment.JavaSymbolClass) =>
    cls match
    case hydra.java.environment.JavaSymbolClass.hoistedLambda(v_JavaSymbolClass_hoistedLambda_arity) => hydra.java.coder.encodeVariable_hoistedLambdaCase(aliases)(name)(v_JavaSymbolClass_hoistedLambda_arity)(cx)(g)
    case hydra.java.environment.JavaSymbolClass.localVariable => Right(hydra.java.utils.javaIdentifierToJavaExpression(hydra.java.coder.elementJavaIdentifier(false)(false)(aliases)(resolvedName)))
    case hydra.java.environment.JavaSymbolClass.constant => Right(hydra.java.utils.javaIdentifierToJavaExpression(hydra.java.coder.elementJavaIdentifier(false)(false)(aliases)(name)))
    case hydra.java.environment.JavaSymbolClass.nullaryFunction => Right(hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocation(None)(hydra.java.coder.elementJavaIdentifier(false)(false)(aliases)(name))(Seq())))
    case hydra.java.environment.JavaSymbolClass.unaryFunction => Right(hydra.java.utils.javaIdentifierToJavaExpression(hydra.java.coder.elementJavaIdentifier(false)(true)(aliases)(name))))))))))
}

def encodeVariable_buildCurried(params: Seq[hydra.core.Name])(inner: hydra.java.syntax.Expression): hydra.java.syntax.Expression =
  hydra.lib.logic.ifElse[hydra.java.syntax.Expression](hydra.lib.lists.`null`[hydra.core.Name](params))(inner)(hydra.java.utils.javaLambda(hydra.lib.lists.head[hydra.core.Name](params))(hydra.java.coder.encodeVariable_buildCurried(hydra.lib.lists.tail[hydra.core.Name](params))(inner)))

def encodeVariable_hoistedLambdaCase[T0](aliases: hydra.java.environment.Aliases)(name: hydra.core.Name)(arity: Int)(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.Expression] =
  {
  lazy val paramNames: Seq[hydra.core.Name] = hydra.lib.lists.map[Int, hydra.core.Name]((i: Int) => hydra.lib.strings.cat2("p")(hydra.lib.literals.showInt32(i)))(hydra.lib.math.range(0)(hydra.lib.math.sub(arity)(1)))
  lazy val paramExprs: Seq[hydra.java.syntax.Expression] = hydra.lib.lists.map[hydra.core.Name, hydra.java.syntax.Expression]((pn: hydra.core.Name) =>
    hydra.java.utils.javaIdentifierToJavaExpression(hydra.java.utils.variableToJavaIdentifier(pn)))(paramNames)
  lazy val call: hydra.java.syntax.Expression = hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocation(None)(hydra.java.coder.elementJavaIdentifier(false)(false)(aliases)(name))(paramExprs))
  lazy val lam: hydra.java.syntax.Expression = hydra.java.coder.encodeVariable_buildCurried(paramNames)(call)
  hydra.lib.eithers.bind[hydra.errors.Error, Option[hydra.core.Binding], hydra.java.syntax.Expression](Right(hydra.lexical.lookupBinding(g)(name)))((mel: Option[hydra.core.Binding]) =>
    hydra.lib.maybes.cases[hydra.core.Binding, Either[hydra.errors.Error, hydra.java.syntax.Expression]](mel)(Right(lam))((el: hydra.core.Binding) =>
    hydra.lib.maybes.cases[hydra.core.TypeScheme, Either[hydra.errors.Error, hydra.java.syntax.Expression]](el.`type`)(Right(lam))((ts: hydra.core.TypeScheme) =>
    {
    lazy val typ: hydra.core.Type = (ts.`type`)
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type, hydra.java.syntax.Expression](hydra.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(typ)(cx)(g))((jtype: hydra.java.syntax.Type) =>
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType, hydra.java.syntax.Expression](hydra.java.utils.javaTypeToJavaReferenceType(jtype)(cx))((rt: hydra.java.syntax.ReferenceType) =>
      Right(hydra.java.utils.javaCastExpressionToJavaExpression(hydra.java.utils.javaCastExpression(rt)(hydra.java.utils.javaExpressionToJavaUnaryExpression(lam))))))
  })))
}

def eqClause(tmpName: scala.Predef.String)(ft: hydra.core.FieldType): hydra.java.syntax.InclusiveOrExpression =
  {
  lazy val fname: scala.Predef.String = (ft.name)
  lazy val ftype: hydra.core.Type = (ft.`type`)
  hydra.lib.logic.ifElse[hydra.java.syntax.InclusiveOrExpression](hydra.java.coder.isBinaryType(ftype))(hydra.java.coder.arraysEqualsClause(tmpName)(fname))(hydra.lib.logic.ifElse[hydra.java.syntax.InclusiveOrExpression](hydra.java.coder.isBigNumericType(ftype))(hydra.java.coder.compareToZeroClause(tmpName)(fname))(hydra.java.coder.equalsClause(tmpName)(fname)))
}

def equalsClause(tmpName: scala.Predef.String)(fname: scala.Predef.String): hydra.java.syntax.InclusiveOrExpression =
  {
  lazy val thisArg: hydra.java.syntax.Expression = hydra.java.utils.javaExpressionNameToJavaExpression(hydra.java.utils.fieldExpression("this")(hydra.java.utils.javaIdentifier(fname)))
  lazy val otherArg: hydra.java.syntax.Expression = hydra.java.utils.javaExpressionNameToJavaExpression(hydra.java.utils.fieldExpression(hydra.java.utils.javaIdentifier(tmpName))(hydra.java.utils.javaIdentifier(fname)))
  lazy val header: hydra.java.syntax.MethodInvocation_Header = hydra.java.syntax.MethodInvocation_Header.complex(hydra.java.syntax.MethodInvocation_Complex(hydra.java.syntax.MethodInvocation_Variant.`type`(hydra.java.utils.javaTypeName("java.util.Objects")),
     Seq(), hydra.java.names.equalsMethodName))
  hydra.java.utils.javaPostfixExpressionToJavaInclusiveOrExpression(hydra.java.utils.javaMethodInvocationToJavaPostfixExpression(hydra.java.syntax.MethodInvocation(header,
     Seq(thisArg, otherArg))))
}

def extractArgType[T0](_lhs: T0)(typ: hydra.core.Type): hydra.core.Type =
  typ match
  case hydra.core.Type.application(v_Type_application_at1) => v_Type_application_at1.function match
    case hydra.core.Type.application(v_Type_application__at2) => (v_Type_application_at1.argument)
    case _ => typ
  case _ => typ

def extractDirectReturn(tparamSet: scala.collection.immutable.Set[hydra.core.Name])(t: hydra.core.Type): Seq[Tuple2[hydra.core.Name,
   hydra.core.Name]] = hydra.java.coder.extractDirectReturn_go(tparamSet)(t)

def extractDirectReturn_go(tparamSet: scala.collection.immutable.Set[hydra.core.Name])(t: hydra.core.Type): Seq[Tuple2[hydra.core.Name, hydra.core.Name]] =
  hydra.strip.deannotateType(t) match
  case hydra.core.Type.function(v_Type_function_ft) => {
    lazy val dom: hydra.core.Type = hydra.strip.deannotateType(v_Type_function_ft.domain)
    {
      lazy val cod: hydra.core.Type = (v_Type_function_ft.codomain)
      dom match
        case hydra.core.Type.variable(v_Type_variable_inVar) => hydra.lib.logic.ifElse[Seq[Tuple2[hydra.core.Name,
           hydra.core.Name]]](hydra.lib.sets.member[hydra.core.Name](v_Type_variable_inVar)(tparamSet))(hydra.strip.deannotateType(cod) match
          case hydra.core.Type.function(v_Type_function_ft2) => {
            lazy val midArg: hydra.core.Type = hydra.strip.deannotateType(v_Type_function_ft2.domain)
            {
              lazy val retPart: hydra.core.Type = hydra.strip.deannotateType(v_Type_function_ft2.codomain)
              midArg match
                case hydra.core.Type.variable(v_Type_variable_midVar) => hydra.lib.logic.ifElse[Seq[Tuple2[hydra.core.Name,
                   hydra.core.Name]]](hydra.lib.sets.member[hydra.core.Name](v_Type_variable_midVar)(tparamSet))(Seq())(retPart match
                  case hydra.core.Type.variable(v_Type_variable_outVar) => hydra.lib.logic.ifElse[Seq[Tuple2[hydra.core.Name,
                     hydra.core.Name]]](hydra.lib.sets.member[hydra.core.Name](v_Type_variable_outVar)(tparamSet))(Seq(Tuple2(v_Type_variable_inVar,
                     v_Type_variable_outVar)))(Seq())
                  case _ => Seq())
                case _ => retPart match
                  case hydra.core.Type.variable(v_Type_variable_outVar) => hydra.lib.logic.ifElse[Seq[Tuple2[hydra.core.Name,
                     hydra.core.Name]]](hydra.lib.sets.member[hydra.core.Name](v_Type_variable_outVar)(tparamSet))(Seq(Tuple2(v_Type_variable_inVar,
                     v_Type_variable_outVar)))(Seq())
                  case _ => Seq()
            }
          }
          case _ => Seq())(hydra.java.coder.extractDirectReturn_go(tparamSet)(cod))
        case _ => hydra.java.coder.extractDirectReturn_go(tparamSet)(cod)
    }
  }
  case _ => Seq()

def extractInOutPair(t: hydra.core.Type): Seq[Tuple2[hydra.core.Name, hydra.core.Name]] =
  hydra.strip.deannotateType(t) match
  case hydra.core.Type.function(v_Type_function_ft) => hydra.strip.deannotateType(v_Type_function_ft.domain) match
    case hydra.core.Type.variable(v_Type_variable_inVar) => {
      lazy val retType: hydra.core.Type = hydra.java.coder.unwrapReturnType(v_Type_function_ft.codomain)
      hydra.strip.deannotateType(retType) match
        case hydra.core.Type.pair(v_Type_pair_pt) => hydra.strip.deannotateType(v_Type_pair_pt.first) match
          case hydra.core.Type.variable(v_Type_variable_outVar) => Seq(Tuple2(v_Type_variable_inVar, v_Type_variable_outVar))
          case _ => Seq()
        case _ => Seq()
    }
    case _ => Seq()
  case _ => Seq()

def extractTypeApplicationArgs(typ: hydra.core.Type): Seq[hydra.core.Type] =
  hydra.lib.lists.reverse[hydra.core.Type](hydra.java.coder.extractTypeApplicationArgs_go(typ))

def extractTypeApplicationArgs_go(t: hydra.core.Type): Seq[hydra.core.Type] =
  t match
  case hydra.core.Type.application(v_Type_application_at) => hydra.lib.lists.cons[hydra.core.Type](v_Type_application_at.argument)(hydra.java.coder.extractTypeApplicationArgs_go(v_Type_application_at.function))
  case _ => Seq()

def fieldTypeToFormalParam[T0](aliases: hydra.java.environment.Aliases)(ft: hydra.core.FieldType)(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.FormalParameter] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type, hydra.java.syntax.FormalParameter](hydra.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(ft.`type`)(cx)(g))((jt: hydra.java.syntax.Type) =>
  Right(hydra.java.utils.javaTypeToJavaFormalParameter(jt)(ft.name)))

def filterByFlags[T0](xs: Seq[T0])(flags: Seq[Boolean]): Seq[T0] =
  hydra.lib.lists.map[Tuple2[T0, Boolean], T0]((p: Tuple2[T0, Boolean]) => hydra.lib.pairs.first[T0, Boolean](p))(hydra.lib.lists.filter[Tuple2[T0,
     Boolean]]((p: Tuple2[T0, Boolean]) => hydra.lib.pairs.second[T0, Boolean](p))(hydra.lib.lists.zip[T0,
     Boolean](xs)(flags)))

def filterPhantomTypeArgs[T0, T1](calleeName: hydra.core.Name)(allTypeArgs: Seq[hydra.core.Type])(cx: T0)(g: hydra.graph.Graph): Either[T1,
   Seq[hydra.core.Type]] =
  hydra.lib.eithers.bind[T1, Option[hydra.core.Binding], Seq[hydra.core.Type]](Right(hydra.lexical.lookupBinding(g)(calleeName)))((mel: Option[hydra.core.Binding]) =>
  hydra.lib.maybes.cases[hydra.core.Binding, Either[T1, Seq[hydra.core.Type]]](mel)(Right(allTypeArgs))((el: hydra.core.Binding) =>
  hydra.lib.maybes.cases[hydra.core.TypeScheme, Either[T1, Seq[hydra.core.Type]]](el.`type`)(Right(allTypeArgs))((ts: hydra.core.TypeScheme) =>
  {
  lazy val schemeVars: Seq[hydra.core.Name] = hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) => hydra.java.coder.isSimpleName(v))(ts.variables)
  {
    lazy val schemeTypeVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.java.coder.collectTypeVars(ts.`type`)
    {
      lazy val schemeType: hydra.core.Type = (ts.`type`)
      {
        lazy val nParams: Int = hydra.java.coder.countFunctionParams(schemeType)
        {
          lazy val peeled: Tuple2[Seq[hydra.core.Type], hydra.core.Type] = hydra.java.coder.peelDomainTypes(nParams)(schemeType)
          {
            lazy val calleeDoms: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.core.Type](peeled)
            {
              lazy val calleeCod: hydra.core.Type = hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.core.Type](peeled)
              {
                lazy val overgenSubst: Map[hydra.core.Name, hydra.core.Type] = hydra.java.coder.detectAccumulatorUnification(calleeDoms)(calleeCod)(schemeVars)
                {
                  lazy val keepFlags: Seq[Boolean] = hydra.lib.lists.map[hydra.core.Name, Boolean]((v: hydra.core.Name) =>
                    hydra.lib.logic.and(hydra.lib.sets.member[hydra.core.Name](v)(schemeTypeVars))(hydra.lib.logic.not(hydra.lib.maps.member[hydra.core.Name,
                       hydra.core.Type](v)(overgenSubst))))(schemeVars)
                  hydra.lib.logic.ifElse[Either[T1, Seq[hydra.core.Type]]](hydra.lib.logic.not(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Name](schemeVars))(hydra.lib.lists.length[hydra.core.Type](allTypeArgs))))(Right(allTypeArgs))(Right(hydra.java.coder.filterPhantomTypeArgs_filterAndApply(allTypeArgs)(keepFlags)(overgenSubst)))
                }
              }
            }
          }
        }
      }
    }
  }
})))

def filterPhantomTypeArgs_filterAndApply(allTypeArgs: Seq[hydra.core.Type])(keepFlags: Seq[Boolean])(overgenSubst: Map[hydra.core.Name,
   hydra.core.Type]): Seq[hydra.core.Type] =
  {
  lazy val filtered: Seq[hydra.core.Type] = hydra.lib.lists.map[Tuple2[hydra.core.Type, Boolean], hydra.core.Type]((p: Tuple2[hydra.core.Type,
     Boolean]) => hydra.lib.pairs.first[hydra.core.Type, Boolean](p))(hydra.lib.lists.filter[Tuple2[hydra.core.Type,
     Boolean]]((p: Tuple2[hydra.core.Type, Boolean]) => hydra.lib.pairs.second[hydra.core.Type, Boolean](p))(hydra.lib.lists.zip[hydra.core.Type,
     Boolean](allTypeArgs)(keepFlags)))
  hydra.lib.logic.ifElse[Seq[hydra.core.Type]](hydra.lib.logic.not(hydra.lib.maps.`null`[hydra.core.Name,
     hydra.core.Type](overgenSubst)))(hydra.lib.lists.map[hydra.core.Type, hydra.core.Type]((t: hydra.core.Type) =>
    hydra.java.coder.substituteTypeVarsWithTypes(overgenSubst)(t))(filtered))(filtered)
}

def findMatchingLambdaVar(name: hydra.core.Name)(lambdaVars: scala.collection.immutable.Set[hydra.core.Name]): hydra.core.Name =
  hydra.lib.logic.ifElse[hydra.core.Name](hydra.lib.sets.member[hydra.core.Name](name)(lambdaVars))(name)(hydra.lib.logic.ifElse[hydra.core.Name](hydra.java.coder.isLambdaBoundIn_isQualified(name))(hydra.lib.maybes.fromMaybe[hydra.core.Name](name)(hydra.lib.lists.find[hydra.core.Name]((lv: hydra.core.Name) =>
  hydra.lib.logic.and(hydra.java.coder.isLambdaBoundIn_isQualified(lv))(hydra.lib.equality.equal[scala.Predef.String](hydra.names.localNameOf(lv))(hydra.names.localNameOf(name))))(hydra.lib.sets.toList[hydra.core.Name](lambdaVars))))(hydra.lib.logic.ifElse[hydra.core.Name](hydra.lib.sets.member[hydra.core.Name](hydra.names.localNameOf(name))(lambdaVars))(hydra.names.localNameOf(name))(name)))

def findPairFirst(t: hydra.core.Type): Option[hydra.core.Name] =
  hydra.strip.deannotateType(t) match
  case hydra.core.Type.pair(v_Type_pair_pt) => hydra.strip.deannotateType(v_Type_pair_pt.first) match
    case hydra.core.Type.variable(v_Type_variable_v) => Some(v_Type_variable_v)
    case _ => None
  case _ => None

def findSelfRefVar[T0](grouped: Map[T0, Seq[T0]]): Option[T0] =
  {
  lazy val selfRefs: Seq[Tuple2[T0, Seq[T0]]] = hydra.lib.lists.filter[Tuple2[T0, Seq[T0]]]((entry: Tuple2[T0, Seq[T0]]) =>
    hydra.lib.lists.elem[T0](hydra.lib.pairs.first[T0, Seq[T0]](entry))(hydra.lib.pairs.second[T0, Seq[T0]](entry)))(hydra.lib.maps.toList[T0,
       Seq[T0]](grouped))
  hydra.lib.logic.ifElse[Option[T0]](hydra.lib.lists.`null`[Tuple2[T0, Seq[T0]]](selfRefs))(None)(Some(hydra.lib.pairs.first[T0,
     Seq[T0]](hydra.lib.lists.head[Tuple2[T0, Seq[T0]]](selfRefs))))
}

lazy val first20Primes: Seq[BigInt] = Seq(BigInt("2"), BigInt("3"), BigInt("5"), BigInt("7"), BigInt("11"),
   BigInt("13"), BigInt("17"), BigInt("19"), BigInt("23"), BigInt("29"), BigInt("31"), BigInt("37"), BigInt("41"),
   BigInt("43"), BigInt("47"), BigInt("53"), BigInt("59"), BigInt("61"), BigInt("67"), BigInt("71"))

def flattenApps(t: hydra.core.Term)(acc: Seq[hydra.core.Term]): Tuple2[Seq[hydra.core.Term], hydra.core.Term] =
  hydra.strip.deannotateTerm(t) match
  case hydra.core.Term.application(v_Term_application_app) => hydra.java.coder.flattenApps(v_Term_application_app.function)(hydra.lib.lists.cons[hydra.core.Term](v_Term_application_app.argument)(acc))
  case _ => Tuple2(acc, t)

def flattenBindings(bindings: Seq[hydra.core.Binding]): Seq[hydra.core.Binding] =
  hydra.lib.lists.bind[hydra.core.Binding, hydra.core.Binding](bindings)((b: hydra.core.Binding) =>
  hydra.strip.deannotateTerm(b.term) match
  case hydra.core.Term.let(v_Term_let_lt) => hydra.lib.lists.concat2[hydra.core.Binding](hydra.java.coder.flattenBindings(v_Term_let_lt.bindings))(Seq(hydra.core.Binding(b.name,
     (v_Term_let_lt.body), (b.`type`))))
  case _ => Seq(b))

def freshJavaName(base: hydra.core.Name)(avoid: scala.collection.immutable.Set[hydra.core.Name]): hydra.core.Name = hydra.java.coder.freshJavaName_go(base)(avoid)(2)

def freshJavaName_go(base: hydra.core.Name)(avoid: scala.collection.immutable.Set[hydra.core.Name])(i: Int): hydra.core.Name =
  {
  lazy val candidate: hydra.core.Name = hydra.lib.strings.cat2(base)(hydra.lib.literals.showInt32(i))
  hydra.lib.logic.ifElse[hydra.core.Name](hydra.lib.sets.member[hydra.core.Name](candidate)(avoid))(hydra.java.coder.freshJavaName_go(base)(avoid)(hydra.lib.math.add(i)(1)))(candidate)
}

def functionCall(env: hydra.java.environment.JavaEnvironment)(isPrim: Boolean)(name: hydra.core.Name)(args: Seq[hydra.core.Term])(typeApps: Seq[hydra.core.Type])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.Expression] =
  {
  lazy val aliases: hydra.java.environment.Aliases = (env.aliases)
  lazy val isLambdaBound: Boolean = hydra.java.coder.isLambdaBoundIn(name)(aliases.lambdaVars)
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.Expression], hydra.java.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Term,
     hydra.java.syntax.Expression, hydra.errors.Error]((arg: hydra.core.Term) => hydra.java.coder.encodeTerm(env)(arg)(cx)(g))(args))((jargs0: Seq[hydra.java.syntax.Expression]) =>
    {
    lazy val wrapResult: Tuple2[Seq[hydra.java.syntax.Expression], Option[scala.Predef.String]] = hydra.java.coder.wrapLazyArguments(name)(jargs0)
    {
      lazy val jargs: Seq[hydra.java.syntax.Expression] = hydra.lib.pairs.first[Seq[hydra.java.syntax.Expression], Option[scala.Predef.String]](wrapResult)
      {
        lazy val mMethodOverride: Option[scala.Predef.String] = hydra.lib.pairs.second[Seq[hydra.java.syntax.Expression],
           Option[scala.Predef.String]](wrapResult)
        hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.java.syntax.Expression]](hydra.lib.logic.or(hydra.java.coder.isLocalVariable(name))(isLambdaBound))(hydra.lib.eithers.bind[hydra.errors.Error,
           hydra.java.syntax.Expression, hydra.java.syntax.Expression](hydra.java.coder.encodeVariable(env)(name)(cx)(g))((baseExpr: hydra.java.syntax.Expression) =>
          Right(hydra.lib.lists.foldl[hydra.java.syntax.Expression, hydra.java.syntax.Expression]((acc: hydra.java.syntax.Expression) =>
          (jarg: hydra.java.syntax.Expression) => hydra.java.coder.applyJavaArg(acc)(jarg))(baseExpr)(jargs))))({
          def overrideMethodName(jid: hydra.java.syntax.Identifier): hydra.java.syntax.Identifier =
            hydra.lib.maybes.cases[scala.Predef.String, hydra.java.syntax.Identifier](mMethodOverride)(jid)((m: scala.Predef.String) =>
            {
            lazy val s: scala.Predef.String = jid
            hydra.lib.strings.cat2(hydra.lib.strings.fromList(hydra.lib.lists.take[Int](hydra.lib.math.sub(hydra.lib.strings.length(s))(hydra.lib.strings.length(hydra.java.names.applyMethodName)))(hydra.lib.strings.toList(s))))(m)
          })
          hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.java.syntax.Expression]](hydra.lib.lists.`null`[hydra.core.Type](typeApps))({
            lazy val header: hydra.java.syntax.MethodInvocation_Header = hydra.java.syntax.MethodInvocation_Header.simple(overrideMethodName(hydra.java.coder.elementJavaIdentifier(isPrim)(false)(aliases)(name)))
            Right(hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.syntax.MethodInvocation(header, jargs)))
          })({
            lazy val qn: hydra.packaging.QualifiedName = hydra.names.qualifyName(name)
            {
              lazy val mns: Option[hydra.packaging.Namespace] = (qn.namespace)
              {
                lazy val localName: scala.Predef.String = (qn.local)
                hydra.lib.maybes.cases[hydra.packaging.Namespace, Either[hydra.errors.Error, hydra.java.syntax.Expression]](mns)({
                  lazy val header: hydra.java.syntax.MethodInvocation_Header = hydra.java.syntax.MethodInvocation_Header.simple(overrideMethodName(hydra.java.coder.elementJavaIdentifier(isPrim)(false)(aliases)(name)))
                  Right(hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.syntax.MethodInvocation(header, jargs)))
                })((`ns_`: hydra.packaging.Namespace) =>
                  {
                  lazy val classId: hydra.java.syntax.Identifier = hydra.java.utils.nameToJavaName(aliases)(hydra.java.coder.elementsQualifiedName(`ns_`))
                  {
                    lazy val methodId: hydra.java.syntax.Identifier = hydra.lib.logic.ifElse[hydra.java.syntax.Identifier](isPrim)(overrideMethodName(hydra.lib.strings.cat2(hydra.java.utils.nameToJavaName(aliases)(hydra.names.unqualifyName(hydra.packaging.QualifiedName(Some(`ns_`),
                       hydra.formatting.capitalize(localName)))))(hydra.lib.strings.cat2(".")(hydra.java.names.applyMethodName))))(hydra.java.utils.sanitizeJavaName(localName))
                    hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.TypeArgument], hydra.java.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Type,
                       hydra.java.syntax.TypeArgument, hydra.errors.Error]((t: hydra.core.Type) =>
                      hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type, hydra.java.syntax.TypeArgument](hydra.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(t)(cx)(g))((jt: hydra.java.syntax.Type) =>
                      hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType, hydra.java.syntax.TypeArgument](hydra.java.utils.javaTypeToJavaReferenceType(jt)(cx))((rt: hydra.java.syntax.ReferenceType) => Right(hydra.java.syntax.TypeArgument.reference(rt)))))(typeApps))((jTypeArgs: Seq[hydra.java.syntax.TypeArgument]) =>
                      Right(hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocationStaticWithTypeArgs(classId)(methodId)(jTypeArgs)(jargs))))
                  }
                })
              }
            }
          })
        })
      }
    }
  })
}

def getCodomain[T0](ann: Map[hydra.core.Name, hydra.core.Term])(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error, hydra.core.Type] =
  hydra.lib.eithers.map[hydra.core.FunctionType, hydra.core.Type, hydra.errors.Error]((ft: hydra.core.FunctionType) => (ft.codomain))(hydra.java.coder.getFunctionType(ann)(cx)(g))

def getFunctionType[T0](ann: Map[hydra.core.Name, hydra.core.Term])(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error, hydra.core.FunctionType] =
  hydra.lib.eithers.bind[hydra.errors.Error, Option[hydra.core.Type], hydra.core.FunctionType](hydra.lib.eithers.bimap[hydra.errors.DecodingError,
     Option[hydra.core.Type], hydra.errors.Error, Option[hydra.core.Type]]((__de: hydra.errors.DecodingError) => hydra.errors.Error.other(__de))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(ann)))((mt: Option[hydra.core.Type]) =>
  hydra.lib.maybes.cases[hydra.core.Type, Either[hydra.errors.Error, hydra.core.FunctionType]](mt)(Left(hydra.errors.Error.other("type annotation is required for function and elimination terms in Java")))((t: hydra.core.Type) =>
  t match
  case hydra.core.Type.function(v_Type_function_ft) => Right(v_Type_function_ft)
  case _ => Left(hydra.errors.Error.other(hydra.lib.strings.cat2("expected function type, got: ")(hydra.show.core.`type`(t))))))

def groupPairsByFirst[T0, T1](pairs: Seq[Tuple2[T0, T1]]): Map[T0, Seq[T1]] =
  hydra.lib.lists.foldl[Map[T0, Seq[T1]], Tuple2[T0, T1]]((m: Map[T0, Seq[T1]]) =>
  (p: Tuple2[T0, T1]) =>
  {
  lazy val k: T0 = hydra.lib.pairs.first[T0, T1](p)
  {
    lazy val v: T1 = hydra.lib.pairs.second[T0, T1](p)
    hydra.lib.maps.alter[Seq[T1], T0]((mv: Option[Seq[T1]]) =>
      hydra.lib.maybes.maybe[Option[Seq[T1]], Seq[T1]](Some(Seq(v)))((vs: Seq[T1]) => Some(hydra.lib.lists.concat2[T1](vs)(Seq(v))))(mv))(k)(m)
  }
})(hydra.lib.maps.empty[T0, Seq[T1]])(pairs)

def hashCodeCompareExpr(otherVar: scala.Predef.String)(fname: scala.Predef.String): hydra.java.syntax.Expression =
  {
  lazy val header: hydra.java.syntax.MethodInvocation_Header = hydra.java.syntax.MethodInvocation_Header.complex(hydra.java.syntax.MethodInvocation_Complex(hydra.java.syntax.MethodInvocation_Variant.`type`(hydra.java.utils.javaTypeName("Integer")),
     Seq(), "compare"))
  lazy val thisHashCode: hydra.java.syntax.Expression = hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.syntax.MethodInvocation(hydra.java.syntax.MethodInvocation_Header.complex(hydra.java.syntax.MethodInvocation_Complex(hydra.java.syntax.MethodInvocation_Variant.expression(hydra.java.syntax.ExpressionName(None,
     hydra.java.utils.sanitizeJavaName(fname))), Seq(), hydra.java.names.hashCodeMethodName)), Seq()))
  lazy val otherHashCode: hydra.java.syntax.Expression = hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.syntax.MethodInvocation(hydra.java.syntax.MethodInvocation_Header.complex(hydra.java.syntax.MethodInvocation_Complex(hydra.java.syntax.MethodInvocation_Variant.expression(hydra.java.utils.fieldExpression(hydra.java.utils.javaIdentifier(otherVar))(hydra.java.utils.javaIdentifier(fname))),
     Seq(), hydra.java.names.hashCodeMethodName)), Seq()))
  hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.syntax.MethodInvocation(header, Seq(thisHashCode, otherHashCode)))
}

def hashCodeMultPair(i: BigInt)(fname: hydra.core.Name): hydra.java.syntax.MultiplicativeExpression =
  {
  lazy val fnameStr: scala.Predef.String = fname
  lazy val lhs: hydra.java.syntax.MultiplicativeExpression = hydra.java.syntax.MultiplicativeExpression.unary(hydra.java.utils.javaPrimaryToJavaUnaryExpression(hydra.java.utils.javaLiteralToJavaPrimary(hydra.java.utils.javaInt(i))))
  lazy val rhs: hydra.java.syntax.UnaryExpression = hydra.java.utils.javaPostfixExpressionToJavaUnaryExpression(hydra.java.utils.javaMethodInvocationToJavaPostfixExpression(hydra.java.syntax.MethodInvocation(hydra.java.syntax.MethodInvocation_Header.complex(hydra.java.syntax.MethodInvocation_Complex(hydra.java.syntax.MethodInvocation_Variant.`type`(hydra.java.utils.javaTypeName("java.util.Objects")),
     Seq(), hydra.java.names.hashCodeMethodName)), Seq(hydra.java.utils.javaExpressionNameToJavaExpression(hydra.java.syntax.ExpressionName(None,
     hydra.java.utils.sanitizeJavaName(fnameStr)))))))
  hydra.java.syntax.MultiplicativeExpression.times(hydra.java.syntax.MultiplicativeExpression_Binary(lhs, rhs))
}

def innerClassRef(aliases: hydra.java.environment.Aliases)(name: hydra.core.Name)(local: scala.Predef.String): hydra.java.syntax.Identifier =
  {
  lazy val id: scala.Predef.String = hydra.java.utils.nameToJavaName(aliases)(name)
  hydra.lib.strings.cat2(hydra.lib.strings.cat2(id)("."))(local)
}

def insertBranchVar(name: hydra.core.Name)(env: hydra.java.environment.JavaEnvironment): hydra.java.environment.JavaEnvironment =
  {
  lazy val aliases: hydra.java.environment.Aliases = (env.aliases)
  hydra.java.environment.JavaEnvironment(hydra.java.environment.Aliases(aliases.currentNamespace, (aliases.packages),
     hydra.lib.sets.insert[hydra.core.Name](name)(aliases.branchVars), (aliases.recursiveVars), (aliases.inScopeTypeParams),
     (aliases.polymorphicLocals), (aliases.inScopeJavaVars), (aliases.varRenames), (aliases.lambdaVars),
     (aliases.typeVarSubst), (aliases.trustedTypeVars), None, (aliases.thunkedVars)), (env.graph))
}

def interfaceTypes(isSer: Boolean)(aliases: hydra.java.environment.Aliases)(tparams: Seq[hydra.java.syntax.TypeParameter])(elName: hydra.core.Name): Seq[hydra.java.syntax.InterfaceType] =
  {
  lazy val javaSerializableType: hydra.java.syntax.InterfaceType = hydra.java.syntax.ClassType(Seq(),
     hydra.java.syntax.ClassTypeQualifier.none, hydra.java.utils.javaTypeIdentifier("Serializable"), Seq())
  lazy val selfTypeArg: hydra.java.syntax.TypeArgument = hydra.java.syntax.TypeArgument.reference(hydra.java.utils.nameToJavaReferenceType(aliases)(false)(hydra.lib.lists.map[hydra.java.syntax.TypeParameter,
     hydra.java.syntax.TypeArgument]((`tp_`: hydra.java.syntax.TypeParameter) => hydra.java.utils.typeParameterToTypeArgument(`tp_`))(tparams))(elName)(None))
  lazy val javaComparableType: hydra.java.syntax.InterfaceType = hydra.java.syntax.ClassType(Seq(), hydra.java.syntax.ClassTypeQualifier.none,
     hydra.java.utils.javaTypeIdentifier("Comparable"), Seq(selfTypeArg))
  hydra.lib.logic.ifElse[Seq[hydra.java.syntax.InterfaceType]](isSer)(Seq(javaSerializableType, javaComparableType))(Seq())
}

def isBigNumericType(typ: hydra.core.Type): Boolean =
  hydra.strip.deannotateType(typ) match
  case hydra.core.Type.literal(v_Type_literal_lt) => v_Type_literal_lt match
    case hydra.core.LiteralType.decimal => true
    case hydra.core.LiteralType.float(v_LiteralType_float_ft) => v_LiteralType_float_ft match
      case hydra.core.FloatType.bigfloat => true
      case _ => false
    case hydra.core.LiteralType.integer(v_LiteralType_integer_it) => v_LiteralType_integer_it match
      case hydra.core.IntegerType.bigint => true
      case _ => false
    case _ => false
  case _ => false

def isBinaryType(typ: hydra.core.Type): Boolean =
  hydra.strip.deannotateType(typ) match
  case hydra.core.Type.literal(v_Type_literal_lt) => v_Type_literal_lt match
    case hydra.core.LiteralType.binary => true
    case _ => false
  case _ => false

def isFieldUnitType[T0, T1](typeName: hydra.core.Name)(fieldName: hydra.core.Name)(cx: T0)(g: hydra.graph.Graph): Either[T1, Boolean] =
  {
  lazy val schemaTypes: Map[hydra.core.Name, hydra.core.TypeScheme] = (g.schemaTypes)
  hydra.lib.maybes.cases[hydra.core.TypeScheme, Either[T1, Boolean]](hydra.lib.maps.lookup[hydra.core.Name,
     hydra.core.TypeScheme](typeName)(schemaTypes))(Right(false))((ts: hydra.core.TypeScheme) =>
    hydra.strip.deannotateType(ts.`type`) match
    case hydra.core.Type.union(v_Type_union_rt) => Right(hydra.lib.maybes.cases[hydra.core.FieldType,
       Boolean](hydra.lib.lists.find[hydra.core.FieldType]((ft: hydra.core.FieldType) =>
      hydra.lib.equality.equal[hydra.core.Name](ft.name)(fieldName))(v_Type_union_rt))(false)((ft: hydra.core.FieldType) =>
      hydra.predicates.isUnitType(hydra.strip.deannotateType(ft.`type`))))
    case _ => Right(false))
}

def isLambdaBoundIn(name: hydra.core.Name)(lambdaVars: scala.collection.immutable.Set[hydra.core.Name]): Boolean =
  hydra.lib.logic.or(hydra.lib.sets.member[hydra.core.Name](name)(lambdaVars))(hydra.lib.logic.or(hydra.lib.logic.and(hydra.java.coder.isLambdaBoundIn_isQualified(name))(hydra.lib.maybes.isJust[hydra.core.Name](hydra.lib.lists.find[hydra.core.Name]((lv: hydra.core.Name) =>
  hydra.lib.logic.and(hydra.java.coder.isLambdaBoundIn_isQualified(lv))(hydra.lib.equality.equal[scala.Predef.String](hydra.names.localNameOf(lv))(hydra.names.localNameOf(name))))(hydra.lib.sets.toList[hydra.core.Name](lambdaVars)))))(hydra.lib.logic.and(hydra.lib.logic.not(hydra.java.coder.isLambdaBoundIn_isQualified(name)))(hydra.lib.sets.member[hydra.core.Name](hydra.names.localNameOf(name))(lambdaVars))))

def isLambdaBoundIn_isQualified(n: hydra.core.Name): Boolean =
  hydra.lib.maybes.isJust[hydra.packaging.Namespace](hydra.names.qualifyName(n).namespace)

def isLambdaBoundVariable(name: hydra.core.Name): Boolean =
  {
  lazy val v: scala.Predef.String = name
  hydra.lib.equality.lte[Int](hydra.lib.strings.length(v))(4)
}

def isLocalVariable(name: hydra.core.Name): Boolean =
  hydra.lib.maybes.isNothing[hydra.packaging.Namespace](hydra.names.qualifyName(name).namespace)

def isNonComparableType(typ: hydra.core.Type): Boolean =
  hydra.strip.deannotateType(typ) match
  case hydra.core.Type.either(v_Type_either__) => true
  case hydra.core.Type.function(v_Type_function__) => true
  case hydra.core.Type.unit => true
  case hydra.core.Type.literal(v_Type_literal_lt) => v_Type_literal_lt match
    case hydra.core.LiteralType.binary => true
    case _ => false
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.java.coder.isNonComparableType(v_Type_forall_ft.body)
  case _ => false

def isRecursiveVariable(aliases: hydra.java.environment.Aliases)(name: hydra.core.Name): Boolean = hydra.lib.sets.member[hydra.core.Name](name)(aliases.recursiveVars)

def isSerializableJavaType(typ: hydra.core.Type): Boolean = hydra.predicates.isNominalType(typ)

def isSimpleName(name: hydra.core.Name): Boolean =
  hydra.lib.equality.equal[Int](hydra.lib.lists.length[scala.Predef.String](hydra.lib.strings.splitOn(".")(name)))(1)

def isUnresolvedInferenceVar(name: hydra.core.Name): Boolean =
  {
  lazy val chars: Seq[Int] = hydra.lib.strings.toList(name)
  hydra.lib.logic.ifElse[Boolean](hydra.lib.lists.`null`[Int](chars))(false)(hydra.lib.logic.ifElse[Boolean](hydra.lib.logic.not(hydra.lib.equality.equal[Int](hydra.lib.lists.head[Int](chars))(116)))(false)({
    lazy val rest: Seq[Int] = hydra.lib.lists.tail[Int](chars)
    hydra.lib.logic.and(hydra.lib.logic.not(hydra.lib.lists.`null`[Int](rest)))(hydra.lib.lists.`null`[Int](hydra.lib.lists.filter[Int]((c: Int) =>
      hydra.lib.logic.not(hydra.java.coder.isUnresolvedInferenceVar_isDigit(c)))(rest)))
  }))
}

def isUnresolvedInferenceVar_isDigit(c: Int): Boolean =
  hydra.lib.logic.and(hydra.lib.equality.gte[Int](c)(48))(hydra.lib.equality.lte[Int](c)(57))

lazy val java11Features: hydra.java.environment.JavaFeatures = hydra.java.environment.JavaFeatures(true)

lazy val java8Features: hydra.java.environment.JavaFeatures = hydra.java.environment.JavaFeatures(false)

lazy val javaComparableRefType: hydra.java.syntax.ReferenceType = hydra.java.syntax.ReferenceType.classOrInterface(hydra.java.syntax.ClassOrInterfaceType.`class`(hydra.java.syntax.ClassType(Seq(),
   hydra.java.syntax.ClassTypeQualifier.none, hydra.java.utils.javaTypeIdentifier("Comparable"), Seq())))

def javaEnvGetGraph(env: hydra.java.environment.JavaEnvironment): hydra.graph.Graph = (env.graph)

def javaEnvSetGraph(g: hydra.graph.Graph)(env: hydra.java.environment.JavaEnvironment): hydra.java.environment.JavaEnvironment = hydra.java.environment.JavaEnvironment(env.aliases,
   g)

lazy val javaFeatures: hydra.java.environment.JavaFeatures = hydra.java.coder.java11Features

def javaIdentifierToString(id: hydra.java.syntax.Identifier): scala.Predef.String = id

def javaTypeArgumentsForNamedType[T0](tname: hydra.core.Name)(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error, Seq[hydra.java.syntax.TypeArgument]] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Type, Seq[hydra.java.syntax.TypeArgument]](hydra.resolution.requireType(cx)(g)(tname))((typ: hydra.core.Type) =>
  Right(hydra.lib.lists.map[hydra.java.syntax.TypeParameter, hydra.java.syntax.TypeArgument]((`tp_`: hydra.java.syntax.TypeParameter) => hydra.java.utils.typeParameterToTypeArgument(`tp_`))(hydra.java.coder.javaTypeParametersForType(typ))))

def javaTypeArgumentsForType(typ: hydra.core.Type): Seq[hydra.java.syntax.TypeArgument] =
  hydra.lib.lists.reverse[hydra.java.syntax.TypeArgument](hydra.lib.lists.map[hydra.java.syntax.TypeParameter,
     hydra.java.syntax.TypeArgument](hydra.java.utils.typeParameterToTypeArgument)(hydra.java.coder.javaTypeParametersForType(typ)))

def javaTypeParametersForType(typ: hydra.core.Type): Seq[hydra.java.syntax.TypeParameter] =
  {
  def toParam(name: hydra.core.Name): hydra.java.syntax.TypeParameter = hydra.java.utils.javaTypeParameter(hydra.formatting.capitalize(name))
  lazy val boundVars: Seq[hydra.core.Name] = hydra.java.coder.javaTypeParametersForType_bvars(typ)
  lazy val freeVars: Seq[hydra.core.Name] = hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) => hydra.java.coder.isLambdaBoundVariable(v))(hydra.lib.sets.toList[hydra.core.Name](hydra.variables.freeVariablesInType(typ)))
  lazy val vars: Seq[hydra.core.Name] = hydra.lib.lists.nub[hydra.core.Name](hydra.lib.lists.concat2[hydra.core.Name](boundVars)(freeVars))
  hydra.lib.lists.map[hydra.core.Name, hydra.java.syntax.TypeParameter](toParam)(vars)
}

def javaTypeParametersForType_bvars(t: hydra.core.Type): Seq[hydra.core.Name] =
  t match
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.lib.lists.cons[hydra.core.Name](v_Type_forall_ft.parameter)(hydra.java.coder.javaTypeParametersForType_bvars(v_Type_forall_ft.body))
  case _ => Seq()

def moduleToJava(mod: hydra.packaging.Module)(defs: Seq[hydra.packaging.Definition])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   Map[scala.Predef.String, scala.Predef.String]] =
  hydra.lib.eithers.bind[hydra.errors.Error, Map[hydra.core.Name, hydra.java.syntax.CompilationUnit],
     Map[scala.Predef.String, scala.Predef.String]](hydra.java.coder.encodeDefinitions(mod)(defs)(cx)(g))((units: Map[hydra.core.Name,
     hydra.java.syntax.CompilationUnit]) =>
  Right(hydra.lib.maps.fromList[scala.Predef.String, scala.Predef.String](hydra.lib.lists.map[Tuple2[hydra.core.Name,
     hydra.java.syntax.CompilationUnit], Tuple2[scala.Predef.String, scala.Predef.String]]((entry: Tuple2[hydra.core.Name,
     hydra.java.syntax.CompilationUnit]) =>
  {
  lazy val name: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, hydra.java.syntax.CompilationUnit](entry)
  {
    lazy val unit: hydra.java.syntax.CompilationUnit = hydra.lib.pairs.second[hydra.core.Name, hydra.java.syntax.CompilationUnit](entry)
    Tuple2(hydra.java.coder.bindingNameToFilePath(name), hydra.serialization.printExpr(hydra.serialization.parenthesize(hydra.java.serde.writeCompilationUnit(unit))))
  }
})(hydra.lib.maps.toList[hydra.core.Name, hydra.java.syntax.CompilationUnit](units)))))

def nameMapToTypeMap[T0](m: Map[T0, hydra.core.Name]): Map[T0, hydra.core.Type] =
  hydra.lib.maps.map[hydra.core.Name, hydra.core.Type, T0]((v: hydra.core.Name) => hydra.core.Type.variable(v))(m)

def namespaceParent(ns: hydra.packaging.Namespace): Option[hydra.packaging.Namespace] =
  {
  lazy val parts: Seq[scala.Predef.String] = hydra.lib.strings.splitOn(".")(ns)
  hydra.lib.logic.ifElse[Option[hydra.packaging.Namespace]](hydra.lib.lists.`null`[scala.Predef.String](hydra.lib.lists.init[scala.Predef.String](parts)))(None)(Some(hydra.lib.strings.intercalate(".")(hydra.lib.lists.init[scala.Predef.String](parts))))
}

def needsThunking(t: hydra.core.Term): Boolean =
  hydra.strip.deannotateTerm(t) match
  case hydra.core.Term.let(v_Term_let__lt) => true
  case hydra.core.Term.typeApplication(v_Term_typeApplication__ta) => true
  case hydra.core.Term.typeLambda(v_Term_typeLambda__tl) => true
  case _ => hydra.lib.lists.foldl[Boolean, hydra.core.Term]((b: Boolean) =>
    (st: hydra.core.Term) => hydra.lib.logic.or(b)(hydra.java.coder.needsThunking(st)))(false)(hydra.rewriting.subterms(t))

def noComment(decl: hydra.java.syntax.ClassBodyDeclaration): hydra.java.syntax.ClassBodyDeclarationWithComments = hydra.java.syntax.ClassBodyDeclarationWithComments(decl,
   None)

def otherwiseBranch(env: hydra.java.environment.JavaEnvironment)(aliases: hydra.java.environment.Aliases)(dom: hydra.core.Type)(cod: hydra.core.Type)(tname: hydra.core.Name)(jcod: hydra.java.syntax.Type)(targs: Seq[hydra.java.syntax.TypeArgument])(d: hydra.core.Term)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.ClassBodyDeclarationWithComments] =
  {
  lazy val jdom: hydra.java.syntax.Type = hydra.java.syntax.Type.reference(hydra.java.utils.nameToJavaReferenceType(aliases)(true)(targs)(tname)(None))
  lazy val mods: Seq[hydra.java.syntax.MethodModifier] = Seq(hydra.java.syntax.MethodModifier.public)
  lazy val anns: Seq[hydra.java.syntax.Annotation] = Seq(hydra.java.utils.overrideAnnotation)
  lazy val param: hydra.java.syntax.FormalParameter = hydra.java.utils.javaTypeToJavaFormalParameter(jdom)("instance")
  lazy val result: hydra.java.syntax.Result = hydra.java.syntax.Result.`type`(jcod)
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.FunctionStructure[hydra.java.environment.JavaEnvironment],
     hydra.java.syntax.ClassBodyDeclarationWithComments](hydra.java.coder.analyzeJavaFunction(env)(d)(cx)(g))((fs: hydra.typing.FunctionStructure[hydra.java.environment.JavaEnvironment]) =>
    {
    lazy val bindings: Seq[hydra.core.Binding] = (fs.bindings)
    {
      lazy val rawBody: hydra.core.Term = (fs.body)
      {
        lazy val innerBody: hydra.core.Term = hydra.java.coder.annotateBodyWithCod(cod)(rawBody)
        {
          lazy val env2: hydra.java.environment.JavaEnvironment = (fs.environment)
          hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[Seq[hydra.java.syntax.BlockStatement], hydra.java.environment.JavaEnvironment],
             hydra.java.syntax.ClassBodyDeclarationWithComments](hydra.java.coder.bindingsToStatements(env2)(bindings)(cx)(g))((bindResult: Tuple2[Seq[hydra.java.syntax.BlockStatement],
             hydra.java.environment.JavaEnvironment]) =>
            {
            lazy val bindingStmts: Seq[hydra.java.syntax.BlockStatement] = hydra.lib.pairs.first[Seq[hydra.java.syntax.BlockStatement],
               hydra.java.environment.JavaEnvironment](bindResult)
            {
              lazy val env3: hydra.java.environment.JavaEnvironment = hydra.lib.pairs.second[Seq[hydra.java.syntax.BlockStatement],
                 hydra.java.environment.JavaEnvironment](bindResult)
              hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression, hydra.java.syntax.ClassBodyDeclarationWithComments](hydra.java.coder.encodeTerm(env3)(innerBody)(cx)(g))((jret: hydra.java.syntax.Expression) =>
                {
                lazy val returnStmt: hydra.java.syntax.BlockStatement = hydra.java.syntax.BlockStatement.statement(hydra.java.utils.javaReturnStatement(Some(jret)))
                {
                  lazy val allStmts: Seq[hydra.java.syntax.BlockStatement] = hydra.lib.lists.concat2[hydra.java.syntax.BlockStatement](bindingStmts)(Seq(returnStmt))
                  Right(hydra.java.coder.noComment(hydra.java.utils.methodDeclaration(mods)(Seq())(anns)(hydra.java.names.otherwiseMethodName)(Seq(param))(result)(Some(allStmts))))
                }
              })
            }
          })
        }
      }
    }
  })
}

def peelDomainTypes(n: Int)(t: hydra.core.Type): Tuple2[Seq[hydra.core.Type], hydra.core.Type] =
  hydra.lib.logic.ifElse[Tuple2[Seq[hydra.core.Type], hydra.core.Type]](hydra.lib.equality.lte[Int](n)(0))(Tuple2(Seq(), t))(hydra.strip.deannotateType(t) match
  case hydra.core.Type.function(v_Type_function_ft) => {
    lazy val rest: Tuple2[Seq[hydra.core.Type], hydra.core.Type] = hydra.java.coder.peelDomainTypes(hydra.lib.math.sub(n)(1))(v_Type_function_ft.codomain)
    Tuple2(hydra.lib.lists.cons[hydra.core.Type](v_Type_function_ft.domain)(hydra.lib.pairs.first[Seq[hydra.core.Type],
       hydra.core.Type](rest)), hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.core.Type](rest))
  }
  case _ => Tuple2(Seq(), t))

def peelDomainsAndCod(n: Int)(t: hydra.core.Type): Tuple2[Seq[hydra.core.Type], hydra.core.Type] =
  hydra.lib.logic.ifElse[Tuple2[Seq[hydra.core.Type], hydra.core.Type]](hydra.lib.equality.lte[Int](n)(0))(Tuple2(Seq(), t))(hydra.strip.deannotateType(t) match
  case hydra.core.Type.function(v_Type_function_ft) => {
    lazy val rest: Tuple2[Seq[hydra.core.Type], hydra.core.Type] = hydra.java.coder.peelDomainsAndCod(hydra.lib.math.sub(n)(1))(v_Type_function_ft.codomain)
    Tuple2(hydra.lib.lists.cons[hydra.core.Type](v_Type_function_ft.domain)(hydra.lib.pairs.first[Seq[hydra.core.Type],
       hydra.core.Type](rest)), hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.core.Type](rest))
  }
  case _ => Tuple2(Seq(), t))

def peelExpectedTypes(subst: Map[hydra.core.Name, hydra.core.Type])(n: Int)(t: hydra.core.Type): Seq[hydra.core.Type] =
  hydra.lib.logic.ifElse[Seq[hydra.core.Type]](hydra.lib.equality.equal[Int](n)(0))(Seq())(hydra.strip.deannotateType(t) match
  case hydra.core.Type.function(v_Type_function_ft) => hydra.lib.lists.cons[hydra.core.Type](hydra.java.coder.applySubstFull(subst)(v_Type_function_ft.domain))(hydra.java.coder.peelExpectedTypes(subst)(hydra.lib.math.sub(n)(1))(v_Type_function_ft.codomain))
  case _ => Seq())

def propagateType(typ: hydra.core.Type)(term: hydra.core.Term): hydra.core.Term =
  {
  def setTypeAnn(t: hydra.core.Term): hydra.core.Term =
    hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(typ)))(t)
  hydra.strip.deannotateTerm(term) match
    case hydra.core.Term.lambda(v_Term_lambda_lam) => {
      lazy val annotated: hydra.core.Term = setTypeAnn(term)
      hydra.strip.deannotateType(typ) match
        case hydra.core.Type.function(v_Type_function_ft) => hydra.java.coder.propagateType_propagateIntoLambda(v_Type_function_ft.codomain)(annotated)
        case _ => annotated
    }
    case hydra.core.Term.let(v_Term_let_lt) => {
      lazy val propagatedBindings: Seq[hydra.core.Binding] = hydra.lib.lists.map[hydra.core.Binding, hydra.core.Binding]((b: hydra.core.Binding) =>
        hydra.lib.maybes.maybe[hydra.core.Binding, hydra.core.TypeScheme](b)((ts: hydra.core.TypeScheme) =>
        hydra.core.Binding(b.name, hydra.java.coder.propagateType(ts.`type`)(b.term), (b.`type`)))(b.`type`))(v_Term_let_lt.bindings)
      setTypeAnn(hydra.java.coder.propagateType_rebuildLet(term)(propagatedBindings)(hydra.java.coder.propagateType(typ)(v_Term_let_lt.body)))
    }
    case hydra.core.Term.application(v_Term_application_app) => {
      lazy val fun: hydra.core.Term = (v_Term_application_app.function)
      {
        lazy val arg: hydra.core.Term = (v_Term_application_app.argument)
        {
          lazy val annotatedFun: hydra.core.Term = hydra.strip.deannotateTerm(fun) match
            case hydra.core.Term.cases(v_Term_cases_cs) => {
              lazy val dom: hydra.core.Type = hydra.resolution.nominalApplication(v_Term_cases_cs.typeName)(Seq())
              {
                lazy val ft: hydra.core.Type = hydra.core.Type.function(hydra.core.FunctionType(dom, typ))
                hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(ft)))(fun)
              }
            }
            case _ => fun
          setTypeAnn(hydra.core.Term.application(hydra.core.Application(annotatedFun, arg)))
        }
      }
    }
    case _ => setTypeAnn(term)
}

def propagateType_propagateIntoLambda(cod: hydra.core.Type)(t: hydra.core.Term): hydra.core.Term =
  t match
  case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.java.coder.propagateType_propagateIntoLambda(cod)(v_Term_annotated_at.body),
     (v_Term_annotated_at.annotation)))
  case hydra.core.Term.lambda(v_Term_lambda_lam) => hydra.core.Term.lambda(hydra.core.Lambda(v_Term_lambda_lam.parameter,
     (v_Term_lambda_lam.domain), hydra.java.coder.propagateType(cod)(v_Term_lambda_lam.body)))
  case _ => t

def propagateType_rebuildLet(t: hydra.core.Term)(bindings: Seq[hydra.core.Binding])(newBody: hydra.core.Term): hydra.core.Term =
  t match
  case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.java.coder.propagateType_rebuildLet(v_Term_annotated_at.body)(bindings)(newBody),
     (v_Term_annotated_at.annotation)))
  case hydra.core.Term.let(v_Term_let__lt) => hydra.core.Term.let(hydra.core.Let(bindings, newBody))
  case _ => t

def propagateTypesInAppChain(fixedCod: hydra.core.Type)(resultType: hydra.core.Type)(t: hydra.core.Term): hydra.core.Term =
  {
  lazy val flattened: Tuple2[Seq[hydra.core.Term], hydra.core.Term] = hydra.java.coder.flattenApps(t)(Seq())
  lazy val args: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term], hydra.core.Term](flattened)
  lazy val fun: hydra.core.Term = hydra.lib.pairs.second[Seq[hydra.core.Term], hydra.core.Term](flattened)
  lazy val lambdaDomsResult: Tuple2[Seq[hydra.core.Type], hydra.core.Term] = hydra.java.coder.collectLambdaDomains(fun)
  lazy val lambdaDoms: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.core.Term](lambdaDomsResult)
  lazy val nArgs: Int = hydra.lib.lists.length[hydra.core.Term](args)
  lazy val nLambdaDoms: Int = hydra.lib.lists.length[hydra.core.Type](lambdaDoms)
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.logic.and(hydra.lib.equality.gt[Int](nLambdaDoms)(0))(hydra.lib.equality.gt[Int](nArgs)(0)))({
    lazy val bodyRetType: hydra.core.Type = hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.core.Type](hydra.java.coder.peelDomainsAndCod(hydra.lib.math.sub(nLambdaDoms)(nArgs))(resultType))
    {
      lazy val funType: hydra.core.Type = hydra.lib.lists.foldl[hydra.core.Type, hydra.core.Type]((c: hydra.core.Type) =>
        (d: hydra.core.Type) => hydra.core.Type.function(hydra.core.FunctionType(d, c)))(bodyRetType)(hydra.lib.lists.reverse[hydra.core.Type](lambdaDoms))
      {
        lazy val annotatedFun: hydra.core.Term = hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(funType)))(fun)
        hydra.java.coder.rebuildApps(annotatedFun)(args)(funType)
      }
    }
  })(hydra.strip.deannotateTerm(t) match
    case hydra.core.Term.application(v_Term_application_app) => {
      lazy val lhs: hydra.core.Term = (v_Term_application_app.function)
      {
        lazy val rhs: hydra.core.Term = (v_Term_application_app.argument)
        {
          lazy val annotatedLhs: hydra.core.Term = hydra.strip.deannotateTerm(lhs) match
            case hydra.core.Term.cases(v_Term_cases_cs) => {
              lazy val dom: hydra.core.Type = hydra.resolution.nominalApplication(v_Term_cases_cs.typeName)(Seq())
              {
                lazy val ft: hydra.core.Type = hydra.core.Type.function(hydra.core.FunctionType(dom, fixedCod))
                hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(ft)))(lhs)
              }
            }
            case _ => lhs
          hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(resultType)))(hydra.core.Term.application(hydra.core.Application(annotatedLhs,
             rhs)))
        }
      }
    }
    case _ => hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(resultType)))(t))
}

def rebuildApps(f: hydra.core.Term)(args: Seq[hydra.core.Term])(fType: hydra.core.Type): hydra.core.Term =
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.lists.`null`[hydra.core.Term](args))(f)(hydra.strip.deannotateType(fType) match
  case hydra.core.Type.function(v_Type_function_ft) => {
    lazy val arg: hydra.core.Term = hydra.lib.lists.head[hydra.core.Term](args)
    {
      lazy val rest: Seq[hydra.core.Term] = hydra.lib.lists.tail[hydra.core.Term](args)
      {
        lazy val remainingType: hydra.core.Type = (v_Type_function_ft.codomain)
        {
          lazy val app: hydra.core.Term = hydra.core.Term.application(hydra.core.Application(f, arg))
          {
            lazy val annotatedApp: hydra.core.Term = hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(remainingType)))(app)
            hydra.java.coder.rebuildApps(annotatedApp)(rest)(remainingType)
          }
        }
      }
    }
  }
  case _ => hydra.lib.lists.foldl[hydra.core.Term, hydra.core.Term]((acc: hydra.core.Term) =>
    (a: hydra.core.Term) => hydra.core.Term.application(hydra.core.Application(acc, a)))(f)(args))

def recordCompareToMethod[T0](aliases: hydra.java.environment.Aliases)(tparams: T0)(elName: hydra.core.Name)(fields: Seq[hydra.core.FieldType]): hydra.java.syntax.ClassBodyDeclaration =
  {
  lazy val anns: Seq[hydra.java.syntax.Annotation] = Seq(hydra.java.utils.overrideAnnotation, hydra.java.utils.suppressWarningsUncheckedAnnotation)
  lazy val mods: Seq[hydra.java.syntax.MethodModifier] = Seq(hydra.java.syntax.MethodModifier.public)
  lazy val param: hydra.java.syntax.FormalParameter = hydra.java.utils.javaTypeToJavaFormalParameter(hydra.java.utils.javaTypeFromTypeName(aliases)(elName))(hydra.java.names.otherInstanceName)
  lazy val result: hydra.java.syntax.Result = hydra.java.utils.javaTypeToJavaResult(hydra.java.utils.javaIntType)
  hydra.java.utils.methodDeclaration(mods)(Seq())(anns)(hydra.java.names.compareToMethodName)(Seq(param))(result)(Some(hydra.java.coder.compareToBody(aliases)(hydra.java.names.otherInstanceName)(fields)))
}

def recordConstructor[T0](aliases: hydra.java.environment.Aliases)(elName: hydra.core.Name)(fields: Seq[hydra.core.FieldType])(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.ClassBodyDeclaration] =
  {
  lazy val assignStmts: Seq[hydra.java.syntax.BlockStatement] = hydra.lib.lists.map[hydra.core.FieldType,
     hydra.java.syntax.BlockStatement]((f: hydra.core.FieldType) =>
    hydra.java.syntax.BlockStatement.statement(hydra.java.utils.toAssignStmt(f.name)))(fields)
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.FormalParameter], hydra.java.syntax.ClassBodyDeclaration](hydra.lib.eithers.mapList[hydra.core.FieldType,
     hydra.java.syntax.FormalParameter, hydra.errors.Error]((f: hydra.core.FieldType) => hydra.java.coder.fieldTypeToFormalParam(aliases)(f)(cx)(g))(fields))((params: Seq[hydra.java.syntax.FormalParameter]) =>
    Right(hydra.java.utils.makeConstructor(aliases)(elName)(false)(params)(assignStmts)))
}

def recordEqualsMethod(aliases: hydra.java.environment.Aliases)(elName: hydra.core.Name)(fields: Seq[hydra.core.FieldType]): hydra.java.syntax.ClassBodyDeclaration =
  {
  lazy val anns: Seq[hydra.java.syntax.Annotation] = Seq(hydra.java.utils.overrideAnnotation)
  lazy val mods: Seq[hydra.java.syntax.MethodModifier] = Seq(hydra.java.syntax.MethodModifier.public)
  lazy val param: hydra.java.syntax.FormalParameter = hydra.java.utils.javaTypeToJavaFormalParameter(hydra.java.utils.javaRefType(Seq())(None)("Object"))(hydra.java.names.otherInstanceName)
  lazy val result: hydra.java.syntax.Result = hydra.java.utils.javaTypeToJavaResult(hydra.java.utils.javaBooleanType)
  lazy val tmpName: scala.Predef.String = "o"
  lazy val instanceOfStmt: hydra.java.syntax.BlockStatement = hydra.java.syntax.BlockStatement.statement(hydra.java.syntax.Statement.ifThen(hydra.java.syntax.IfThenStatement(hydra.java.utils.javaUnaryExpressionToJavaExpression(hydra.java.syntax.UnaryExpression.other(hydra.java.syntax.UnaryExpressionNotPlusMinus.not(hydra.java.utils.javaRelationalExpressionToJavaUnaryExpression(hydra.java.utils.javaInstanceOf(hydra.java.utils.javaIdentifierToJavaRelationalExpression(hydra.java.utils.javaIdentifier(hydra.java.names.otherInstanceName)))(hydra.java.utils.nameToJavaReferenceType(aliases)(false)(Seq())(elName)(None)))))),
     hydra.java.utils.javaReturnStatement(Some(hydra.java.utils.javaBooleanExpression(false))))))
  lazy val castStmt: hydra.java.syntax.BlockStatement = hydra.java.utils.variableDeclarationStatement(aliases)(hydra.java.utils.javaTypeFromTypeName(aliases)(elName))(hydra.java.utils.javaIdentifier(tmpName))(hydra.java.utils.javaCastExpressionToJavaExpression(hydra.java.utils.javaCastExpression(hydra.java.utils.nameToJavaReferenceType(aliases)(false)(Seq())(elName)(None))(hydra.java.utils.javaIdentifierToJavaUnaryExpression(hydra.java.utils.sanitizeJavaName(hydra.java.names.otherInstanceName)))))
  lazy val returnAllFieldsEqual: hydra.java.syntax.BlockStatement = hydra.java.syntax.BlockStatement.statement(hydra.java.utils.javaReturnStatement(Some(hydra.lib.logic.ifElse[hydra.java.syntax.Expression](hydra.lib.lists.`null`[hydra.core.FieldType](fields))(hydra.java.utils.javaBooleanExpression(true))(hydra.java.utils.javaConditionalAndExpressionToJavaExpression(hydra.lib.lists.map[hydra.core.FieldType,
     hydra.java.syntax.InclusiveOrExpression]((f: hydra.core.FieldType) => hydra.java.coder.eqClause(tmpName)(f))(fields))))))
  hydra.java.utils.methodDeclaration(mods)(Seq())(anns)(hydra.java.names.equalsMethodName)(Seq(param))(result)(Some(Seq(instanceOfStmt,
     castStmt, returnAllFieldsEqual)))
}

def recordHashCodeMethod(fields: Seq[hydra.core.FieldType]): hydra.java.syntax.ClassBodyDeclaration =
  {
  lazy val anns: Seq[hydra.java.syntax.Annotation] = Seq(hydra.java.utils.overrideAnnotation)
  lazy val mods: Seq[hydra.java.syntax.MethodModifier] = Seq(hydra.java.syntax.MethodModifier.public)
  lazy val result: hydra.java.syntax.Result = hydra.java.utils.javaTypeToJavaResult(hydra.java.utils.javaIntType)
  lazy val returnSum: hydra.java.syntax.BlockStatement = hydra.java.syntax.BlockStatement.statement(hydra.lib.logic.ifElse[hydra.java.syntax.Statement](hydra.lib.lists.`null`[hydra.core.FieldType](fields))(hydra.java.utils.javaReturnStatement(Some(hydra.java.utils.javaIntExpression(BigInt("0")))))(hydra.java.utils.javaReturnStatement(Some(hydra.java.utils.javaAdditiveExpressionToJavaExpression(hydra.java.utils.addExpressions(hydra.lib.lists.zipWith[BigInt,
     hydra.core.Name, hydra.java.syntax.MultiplicativeExpression](hydra.java.coder.hashCodeMultPair)(hydra.java.coder.first20Primes)(hydra.lib.lists.map[hydra.core.FieldType,
     hydra.core.Name]((f: hydra.core.FieldType) => (f.name))(fields))))))))
  hydra.java.utils.methodDeclaration(mods)(Seq())(anns)(hydra.java.names.hashCodeMethodName)(Seq())(result)(Some(Seq(returnSum)))
}

def recordMemberVar[T0](aliases: hydra.java.environment.Aliases)(ft: hydra.core.FieldType)(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.ClassBodyDeclaration] =
  {
  lazy val mods: Seq[hydra.java.syntax.FieldModifier] = Seq(hydra.java.syntax.FieldModifier.public, hydra.java.syntax.FieldModifier.`final`)
  lazy val fname: hydra.core.Name = (ft.name)
  lazy val ftype: hydra.core.Type = (ft.`type`)
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type, hydra.java.syntax.ClassBodyDeclaration](hydra.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(ftype)(cx)(g))((jt: hydra.java.syntax.Type) =>
    Right(hydra.java.utils.javaMemberField(mods)(jt)(hydra.java.utils.fieldNameToJavaVariableDeclarator(fname))))
}

def recordWithMethod[T0](aliases: hydra.java.environment.Aliases)(elName: hydra.core.Name)(fields: Seq[hydra.core.FieldType])(field: hydra.core.FieldType)(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.ClassBodyDeclaration] =
  {
  lazy val mods: Seq[hydra.java.syntax.MethodModifier] = Seq(hydra.java.syntax.MethodModifier.public)
  def anns[T1]: Seq[T1] = Seq()
  lazy val methodName: scala.Predef.String = hydra.lib.strings.cat2("with")(hydra.formatting.nonAlnumToUnderscores(hydra.formatting.capitalize(field.name)))
  lazy val result: hydra.java.syntax.Result = hydra.java.utils.referenceTypeToResult(hydra.java.utils.nameToJavaReferenceType(aliases)(false)(Seq())(elName)(None))
  lazy val consId: hydra.java.syntax.Identifier = hydra.java.utils.sanitizeJavaName(hydra.names.localNameOf(elName))
  lazy val fieldArgs: Seq[hydra.java.syntax.Expression] = hydra.lib.lists.map[hydra.core.FieldType, hydra.java.syntax.Expression]((f: hydra.core.FieldType) => hydra.java.utils.fieldNameToJavaExpression(f.name))(fields)
  lazy val returnStmt: hydra.java.syntax.BlockStatement = hydra.java.syntax.BlockStatement.statement(hydra.java.utils.javaReturnStatement(Some(hydra.java.utils.javaConstructorCall(hydra.java.utils.javaConstructorName(consId)(None))(fieldArgs)(None))))
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.FormalParameter, hydra.java.syntax.ClassBodyDeclaration](hydra.java.coder.fieldTypeToFormalParam(aliases)(field)(cx)(g))((param: hydra.java.syntax.FormalParameter) =>
    Right(hydra.java.utils.methodDeclaration(mods)(Seq())(anns)(methodName)(Seq(param))(result)(Some(Seq(returnStmt)))))
}

def resolveTypeApps(schemeVars: Seq[hydra.core.Name])(fallbackTypeApps: Seq[hydra.core.Type])(argSubst: Map[hydra.core.Name,
   hydra.core.Type]): Seq[hydra.core.Type] =
  {
  lazy val resolvedVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.maps.keys[hydra.core.Name,
     hydra.core.Type](argSubst))
  lazy val unresolvedVars: Seq[hydra.core.Name] = hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) =>
    hydra.lib.logic.not(hydra.lib.sets.member[hydra.core.Name](v)(resolvedVars)))(schemeVars)
  lazy val usedTypes: scala.collection.immutable.Set[hydra.core.Type] = hydra.lib.sets.fromList[hydra.core.Type](hydra.lib.maps.elems[hydra.core.Name,
     hydra.core.Type](argSubst))
  lazy val unusedIrTypes: Seq[hydra.core.Type] = hydra.lib.lists.filter[hydra.core.Type]((t: hydra.core.Type) =>
    hydra.lib.logic.not(hydra.lib.sets.member[hydra.core.Type](t)(usedTypes)))(fallbackTypeApps)
  lazy val remainingSubst: Map[hydra.core.Name, hydra.core.Type] = hydra.lib.maps.fromList[hydra.core.Name,
     hydra.core.Type](hydra.lib.lists.zip[hydra.core.Name, hydra.core.Type](unresolvedVars)(unusedIrTypes))
  lazy val fullSubst: Map[hydra.core.Name, hydra.core.Type] = hydra.lib.maps.union[hydra.core.Name, hydra.core.Type](argSubst)(remainingSubst)
  hydra.lib.lists.map[hydra.core.Name, hydra.core.Type]((v: hydra.core.Name) =>
    hydra.lib.maps.findWithDefault[hydra.core.Type, hydra.core.Name](hydra.core.Type.variable(v))(v)(fullSubst))(schemeVars)
}

def selfRefSubstitution[T0](grouped: Map[T0, Seq[T0]]): Map[T0, T0] =
  hydra.lib.lists.foldl[Map[T0, T0], Tuple2[T0, Seq[T0]]]((subst: Map[T0, T0]) =>
  (entry: Tuple2[T0, Seq[T0]]) =>
  hydra.java.coder.selfRefSubstitution_processGroup(subst)(hydra.lib.pairs.first[T0, Seq[T0]](entry))(hydra.lib.pairs.second[T0,
     Seq[T0]](entry)))(hydra.lib.maps.empty[T0, T0])(hydra.lib.maps.toList[T0, Seq[T0]](grouped))

def selfRefSubstitution_processGroup[T0](subst: Map[T0, T0])(inVar: T0)(outVars: Seq[T0]): Map[T0, T0] =
  hydra.lib.logic.ifElse[Map[T0, T0]](hydra.lib.lists.elem[T0](inVar)(outVars))(hydra.lib.lists.foldl[Map[T0, T0], T0]((s: Map[T0, T0]) =>
  (v: T0) =>
  hydra.lib.logic.ifElse[Map[T0, T0]](hydra.lib.equality.equal[T0](v)(inVar))(s)(hydra.lib.maps.insert[T0, T0](v)(inVar)(s)))(subst)(outVars))(subst)

def serializableTypes(isSer: Boolean): Seq[hydra.java.syntax.InterfaceType] =
  {
  lazy val javaSerializableType: hydra.java.syntax.InterfaceType = hydra.java.syntax.ClassType(Seq(),
     hydra.java.syntax.ClassTypeQualifier.none, hydra.java.utils.javaTypeIdentifier("Serializable"), Seq())
  hydra.lib.logic.ifElse[Seq[hydra.java.syntax.InterfaceType]](isSer)(Seq(javaSerializableType))(Seq())
}

def splitConstantInitializer(member: hydra.java.syntax.InterfaceMemberDeclaration): Seq[hydra.java.syntax.InterfaceMemberDeclaration] =
  member match
  case hydra.java.syntax.InterfaceMemberDeclaration.constant(v_InterfaceMemberDeclaration_constant_cd) => hydra.lib.lists.bind[hydra.java.syntax.VariableDeclarator,
     hydra.java.syntax.InterfaceMemberDeclaration](v_InterfaceMemberDeclaration_constant_cd.variables)((v1: hydra.java.syntax.VariableDeclarator) =>
    hydra.java.coder.splitConstantInitializer_splitVar(v_InterfaceMemberDeclaration_constant_cd.modifiers)(v_InterfaceMemberDeclaration_constant_cd.`type`)(v1))
  case _ => Seq(member)

def splitConstantInitializer_splitVar(mods: Seq[hydra.java.syntax.ConstantModifier])(utype: hydra.java.syntax.UnannType)(vd: hydra.java.syntax.VariableDeclarator): Seq[hydra.java.syntax.InterfaceMemberDeclaration] =
  {
  lazy val vid: hydra.java.syntax.VariableDeclaratorId = (vd.id)
  lazy val mInit: Option[hydra.java.syntax.VariableInitializer] = (vd.initializer)
  hydra.lib.maybes.cases[hydra.java.syntax.VariableInitializer, Seq[hydra.java.syntax.InterfaceMemberDeclaration]](mInit)(Seq(hydra.java.syntax.InterfaceMemberDeclaration.constant(hydra.java.syntax.ConstantDeclaration(mods,
     utype, Seq(vd)))))((`init_`: hydra.java.syntax.VariableInitializer) =>
    `init_` match
    case hydra.java.syntax.VariableInitializer.expression(v_VariableInitializer_expression_expr) => {
      lazy val varName: scala.Predef.String = hydra.java.coder.javaIdentifierToString(vid.identifier)
      lazy val helperName: scala.Predef.String = hydra.lib.strings.cat2("_init_")(varName)
      lazy val callExpr: hydra.java.syntax.Expression = hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocation(None)(helperName)(Seq()))
      lazy val field: hydra.java.syntax.InterfaceMemberDeclaration = hydra.java.syntax.InterfaceMemberDeclaration.constant(hydra.java.syntax.ConstantDeclaration(mods,
         utype, Seq(hydra.java.syntax.VariableDeclarator(vid, Some(hydra.java.syntax.VariableInitializer.expression(callExpr))))))
      lazy val returnSt: hydra.java.syntax.BlockStatement = hydra.java.syntax.BlockStatement.statement(hydra.java.utils.javaReturnStatement(Some(v_VariableInitializer_expression_expr)))
      lazy val resultType: hydra.java.syntax.Result = hydra.java.syntax.Result.`type`(utype)
      lazy val helper: hydra.java.syntax.InterfaceMemberDeclaration = hydra.java.utils.interfaceMethodDeclaration(Seq(hydra.java.syntax.InterfaceMethodModifier.static,
         hydra.java.syntax.InterfaceMethodModifier.`private`))(Seq())(helperName)(Seq())(resultType)(Some(Seq(returnSt)))
      Seq(field, helper)
    }
    case _ => Seq(hydra.java.syntax.InterfaceMemberDeclaration.constant(hydra.java.syntax.ConstantDeclaration(mods, utype, Seq(vd)))))
}

def stripForalls(t: hydra.core.Type): hydra.core.Type =
  hydra.strip.deannotateType(t) match
  case hydra.core.Type.forall(v_Type_forall_fa) => hydra.java.coder.stripForalls(v_Type_forall_fa.body)
  case _ => t

def substituteTypeVarsWithTypes(subst: Map[hydra.core.Name, hydra.core.Type])(t: hydra.core.Type): hydra.core.Type =
  hydra.java.coder.substituteTypeVarsWithTypes_go(subst)(hydra.strip.deannotateType(t))

def substituteTypeVarsWithTypes_go(subst: Map[hydra.core.Name, hydra.core.Type])(t: hydra.core.Type): hydra.core.Type =
  hydra.strip.deannotateType(t) match
  case hydra.core.Type.variable(v_Type_variable_v) => hydra.lib.maybes.cases[hydra.core.Type, hydra.core.Type](hydra.lib.maps.lookup[hydra.core.Name,
     hydra.core.Type](v_Type_variable_v)(subst))(t)((rep: hydra.core.Type) => rep)
  case hydra.core.Type.function(v_Type_function_ft) => hydra.core.Type.function(hydra.core.FunctionType(hydra.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_function_ft.domain),
     hydra.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_function_ft.codomain)))
  case hydra.core.Type.application(v_Type_application_at) => hydra.core.Type.application(hydra.core.ApplicationType(hydra.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_application_at.function),
     hydra.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_application_at.argument)))
  case hydra.core.Type.list(v_Type_list_inner) => hydra.core.Type.list(hydra.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_list_inner))
  case hydra.core.Type.set(v_Type_set_inner) => hydra.core.Type.set(hydra.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_set_inner))
  case hydra.core.Type.maybe(v_Type_maybe_inner) => hydra.core.Type.maybe(hydra.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_maybe_inner))
  case hydra.core.Type.map(v_Type_map_mt) => hydra.core.Type.map(hydra.core.MapType(hydra.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_map_mt.keys),
     hydra.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_map_mt.values)))
  case hydra.core.Type.pair(v_Type_pair_pt) => hydra.core.Type.pair(hydra.core.PairType(hydra.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_pair_pt.first),
     hydra.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_pair_pt.second)))
  case hydra.core.Type.either(v_Type_either_et) => hydra.core.Type.either(hydra.core.EitherType(hydra.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_either_et.left),
     hydra.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_either_et.right)))
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.core.Type.forall(hydra.core.ForallType(v_Type_forall_ft.parameter,
     hydra.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_forall_ft.body)))
  case _ => t

lazy val tagCmpNotZeroExpr: hydra.java.syntax.Expression = {
  lazy val lhs: hydra.java.syntax.EqualityExpression = hydra.java.utils.javaRelationalExpressionToJavaEqualityExpression(hydra.java.utils.javaPostfixExpressionToJavaRelationalExpression(hydra.java.syntax.PostfixExpression.name(hydra.java.syntax.ExpressionName(None,
     hydra.java.utils.javaIdentifier("tagCmp")))))
  lazy val rhs: hydra.java.syntax.RelationalExpression = hydra.java.utils.javaPostfixExpressionToJavaRelationalExpression(hydra.java.syntax.PostfixExpression.primary(hydra.java.utils.javaLiteralToJavaPrimary(hydra.java.utils.javaInt(BigInt("0")))))
  hydra.java.utils.javaEqualityExpressionToJavaExpression(hydra.java.syntax.EqualityExpression.notEqual(hydra.java.syntax.EqualityExpression_Binary(lhs, rhs)))
}

lazy val tagCompareExpr: hydra.java.syntax.Expression = {
  lazy val thisGetClass: hydra.java.syntax.MethodInvocation = hydra.java.syntax.MethodInvocation(hydra.java.syntax.MethodInvocation_Header.complex(hydra.java.syntax.MethodInvocation_Complex(hydra.java.syntax.MethodInvocation_Variant.primary(hydra.java.utils.javaExpressionToJavaPrimary(hydra.java.utils.javaThis)),
     Seq(), "getClass")), Seq())
  lazy val thisGetName: hydra.java.syntax.MethodInvocation = hydra.java.syntax.MethodInvocation(hydra.java.syntax.MethodInvocation_Header.complex(hydra.java.syntax.MethodInvocation_Complex(hydra.java.syntax.MethodInvocation_Variant.primary(hydra.java.utils.javaMethodInvocationToJavaPrimary(thisGetClass)),
     Seq(), "getName")), Seq())
  lazy val otherGetClass: hydra.java.syntax.MethodInvocation = hydra.java.syntax.MethodInvocation(hydra.java.syntax.MethodInvocation_Header.complex(hydra.java.syntax.MethodInvocation_Complex(hydra.java.syntax.MethodInvocation_Variant.expression(hydra.java.syntax.ExpressionName(None,
     hydra.java.names.otherInstanceName)), Seq(), "getClass")), Seq())
  lazy val otherGetName: hydra.java.syntax.MethodInvocation = hydra.java.syntax.MethodInvocation(hydra.java.syntax.MethodInvocation_Header.complex(hydra.java.syntax.MethodInvocation_Complex(hydra.java.syntax.MethodInvocation_Variant.primary(hydra.java.utils.javaMethodInvocationToJavaPrimary(otherGetClass)),
     Seq(), "getName")), Seq())
  hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.syntax.MethodInvocation(hydra.java.syntax.MethodInvocation_Header.complex(hydra.java.syntax.MethodInvocation_Complex(hydra.java.syntax.MethodInvocation_Variant.primary(hydra.java.utils.javaMethodInvocationToJavaPrimary(thisGetName)),
     Seq(), hydra.java.names.compareToMethodName)), Seq(hydra.java.utils.javaMethodInvocationToJavaExpression(otherGetName))))
}

def takeTypeArgs[T0, T1](label: scala.Predef.String)(n: Int)(tyapps: Seq[hydra.java.syntax.Type])(cx: T0)(g: T1): Either[hydra.errors.Error,
   Seq[hydra.java.syntax.TypeArgument]] =
  hydra.lib.logic.ifElse[Either[hydra.errors.Error, Seq[hydra.java.syntax.TypeArgument]]](hydra.lib.equality.lt[Int](hydra.lib.lists.length[hydra.java.syntax.Type](tyapps))(n))(Left(hydra.errors.Error.other(hydra.lib.strings.cat(Seq("needed type arguments for ",
     label, ", found too few")))))(hydra.lib.eithers.mapList[hydra.java.syntax.Type, hydra.java.syntax.TypeArgument,
     hydra.errors.Error]((jt: hydra.java.syntax.Type) =>
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType, hydra.java.syntax.TypeArgument](hydra.java.utils.javaTypeToJavaReferenceType(jt)(cx))((rt: hydra.java.syntax.ReferenceType) => Right(hydra.java.syntax.TypeArgument.reference(rt))))(hydra.lib.lists.take[hydra.java.syntax.Type](n)(tyapps)))

def toClassDecl(isInner: Boolean)(isSer: Boolean)(aliases: hydra.java.environment.Aliases)(tparams: Seq[hydra.java.syntax.TypeParameter])(elName: hydra.core.Name)(t: hydra.core.Type)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.ClassDeclaration] =
  {
  def wrap(`t_`: hydra.core.Type): Either[hydra.errors.Error, hydra.java.syntax.ClassDeclaration] =
    hydra.java.coder.declarationForRecordType(isInner)(isSer)(aliases)(tparams)(elName)(Seq(hydra.core.FieldType("value",
       hydra.strip.deannotateType(`t_`))))(cx)(g)
  hydra.strip.deannotateType(t) match
    case hydra.core.Type.record(v_Type_record_rt) => hydra.java.coder.declarationForRecordType(isInner)(isSer)(aliases)(tparams)(elName)(v_Type_record_rt)(cx)(g)
    case hydra.core.Type.union(v_Type_union_rt) => hydra.java.coder.declarationForUnionType(isSer)(aliases)(tparams)(elName)(v_Type_union_rt)(cx)(g)
    case hydra.core.Type.forall(v_Type_forall_fa) => {
      lazy val v: hydra.core.Name = (v_Type_forall_fa.parameter)
      {
        lazy val body: hydra.core.Type = (v_Type_forall_fa.body)
        {
          lazy val param: hydra.java.syntax.TypeParameter = hydra.java.utils.javaTypeParameter(hydra.formatting.capitalize(v))
          hydra.java.coder.toClassDecl(false)(isSer)(aliases)(hydra.lib.lists.concat2[hydra.java.syntax.TypeParameter](tparams)(Seq(param)))(elName)(body)(cx)(g)
        }
      }
    }
    case hydra.core.Type.wrap(v_Type_wrap_wt) => hydra.java.coder.declarationForRecordType(isInner)(isSer)(aliases)(tparams)(elName)(Seq(hydra.core.FieldType("value",
       v_Type_wrap_wt)))(cx)(g)
    case _ => wrap(t)
}

def toDeclInit(aliasesExt: hydra.java.environment.Aliases)(gExt: hydra.graph.Graph)(recursiveVars: scala.collection.immutable.Set[hydra.core.Name])(flatBindings: Seq[hydra.core.Binding])(name: hydra.core.Name)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   Option[hydra.java.syntax.BlockStatement]] =
  hydra.lib.logic.ifElse[Either[hydra.errors.Error, Option[hydra.java.syntax.BlockStatement]]](hydra.lib.sets.member[hydra.core.Name](name)(recursiveVars))({
  lazy val binding: hydra.core.Binding = hydra.lib.lists.head[hydra.core.Binding](hydra.lib.lists.filter[hydra.core.Binding]((b: hydra.core.Binding) => hydra.lib.equality.equal[hydra.core.Name](b.name)(name))(flatBindings))
  {
    lazy val value: hydra.core.Term = (binding.term)
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Type, Option[hydra.java.syntax.BlockStatement]](hydra.lib.maybes.cases[hydra.core.TypeScheme,
       Either[hydra.errors.Error, hydra.core.Type]](binding.`type`)(hydra.checking.typeOfTerm(cx)(gExt)(value))((ts: hydra.core.TypeScheme) => Right(ts.`type`)))((typ: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type, Option[hydra.java.syntax.BlockStatement]](hydra.java.coder.encodeType(aliasesExt)(hydra.lib.sets.empty[hydra.core.Name])(typ)(cx)(g))((jtype: hydra.java.syntax.Type) =>
      {
      lazy val id: hydra.java.syntax.Identifier = hydra.java.utils.variableToJavaIdentifier(name)
      {
        lazy val arid: hydra.java.syntax.Identifier = "java.util.concurrent.atomic.AtomicReference"
        {
          lazy val aid: hydra.java.syntax.AnnotatedIdentifier = hydra.java.syntax.AnnotatedIdentifier(Seq(), arid)
          hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType, Option[hydra.java.syntax.BlockStatement]](hydra.java.utils.javaTypeToJavaReferenceType(jtype)(cx))((rt: hydra.java.syntax.ReferenceType) =>
            {
            lazy val targs: hydra.java.syntax.TypeArgumentsOrDiamond = hydra.java.coder.typeArgsOrDiamond(Seq(hydra.java.syntax.TypeArgument.reference(rt)))
            {
              lazy val ci: hydra.java.syntax.ClassOrInterfaceTypeToInstantiate = hydra.java.syntax.ClassOrInterfaceTypeToInstantiate(Seq(aid), Some(targs))
              {
                lazy val body: hydra.java.syntax.Expression = hydra.java.utils.javaConstructorCall(ci)(Seq())(None)
                {
                  lazy val pkg: hydra.java.syntax.PackageName = hydra.java.names.javaPackageName(Seq("java", "util", "concurrent", "atomic"))
                  {
                    lazy val artype: hydra.java.syntax.Type = hydra.java.utils.javaRefType(Seq(rt))(Some(pkg))("AtomicReference")
                    Right(Some(hydra.java.utils.variableDeclarationStatement(aliasesExt)(artype)(id)(body)))
                  }
                }
              }
            }
          })
        }
      }
    }))
  }
})(Right(None))

def toDeclStatement(envExt: hydra.java.environment.JavaEnvironment)(aliasesExt: hydra.java.environment.Aliases)(gExt: hydra.graph.Graph)(recursiveVars: scala.collection.immutable.Set[hydra.core.Name])(thunkedVars: scala.collection.immutable.Set[hydra.core.Name])(flatBindings: Seq[hydra.core.Binding])(name: hydra.core.Name)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.BlockStatement] =
  {
  lazy val binding: hydra.core.Binding = hydra.lib.lists.head[hydra.core.Binding](hydra.lib.lists.filter[hydra.core.Binding]((b: hydra.core.Binding) => hydra.lib.equality.equal[hydra.core.Name](b.name)(name))(flatBindings))
  lazy val value: hydra.core.Term = (binding.term)
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Type, hydra.java.syntax.BlockStatement](hydra.lib.maybes.cases[hydra.core.TypeScheme,
     Either[hydra.errors.Error, hydra.core.Type]](binding.`type`)(hydra.checking.typeOfTerm(cx)(gExt)(value))((ts: hydra.core.TypeScheme) => Right(ts.`type`)))((typ: hydra.core.Type) =>
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type, hydra.java.syntax.BlockStatement](hydra.java.coder.encodeType(aliasesExt)(hydra.lib.sets.empty[hydra.core.Name])(typ)(cx)(g))((jtype: hydra.java.syntax.Type) =>
    {
    lazy val id: hydra.java.syntax.Identifier = hydra.java.utils.variableToJavaIdentifier(name)
    {
      lazy val annotatedValue: hydra.core.Term = hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(typ)))(value)
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression, hydra.java.syntax.BlockStatement](hydra.java.coder.encodeTerm(envExt)(annotatedValue)(cx)(g))((rhs: hydra.java.syntax.Expression) =>
        hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.java.syntax.BlockStatement]](hydra.lib.sets.member[hydra.core.Name](name)(recursiveVars))(Right(hydra.java.syntax.BlockStatement.statement(hydra.java.utils.javaMethodInvocationToJavaStatement(hydra.java.utils.methodInvocation(Some(Left(hydra.java.syntax.ExpressionName(None,
           id))))(hydra.java.names.setMethodName)(Seq(rhs))))))(hydra.lib.logic.ifElse[Either[hydra.errors.Error,
           hydra.java.syntax.BlockStatement]](hydra.lib.sets.member[hydra.core.Name](name)(thunkedVars))(hydra.lib.eithers.bind[hydra.errors.Error,
           hydra.java.syntax.ReferenceType, hydra.java.syntax.BlockStatement](hydra.java.utils.javaTypeToJavaReferenceType(jtype)(cx))((rt: hydra.java.syntax.ReferenceType) =>
        {
        lazy val lazyType: hydra.java.syntax.Type = hydra.java.utils.javaRefType(Seq(rt))(hydra.java.names.hydraUtilPackageName)("Lazy")
        {
          lazy val lambdaBody: hydra.java.syntax.LambdaBody = hydra.java.syntax.LambdaBody.expression(rhs)
          {
            lazy val supplierLambda: hydra.java.syntax.Expression = hydra.java.syntax.Expression.lambda(hydra.java.syntax.LambdaExpression(hydra.java.syntax.LambdaParameters.tuple(Seq()),
               lambdaBody))
            {
              lazy val targs: hydra.java.syntax.TypeArgumentsOrDiamond = hydra.java.coder.typeArgsOrDiamond(Seq(hydra.java.syntax.TypeArgument.reference(rt)))
              {
                lazy val lazyExpr: hydra.java.syntax.Expression = hydra.java.utils.javaConstructorCall(hydra.java.utils.javaConstructorName("hydra.util.Lazy")(Some(targs)))(Seq(supplierLambda))(None)
                Right(hydra.java.utils.variableDeclarationStatement(aliasesExt)(lazyType)(id)(lazyExpr))
              }
            }
          }
        }
      }))(Right(hydra.java.utils.variableDeclarationStatement(aliasesExt)(jtype)(id)(rhs)))))
    }
  }))
}

def tryInferFunctionType(funTerm: hydra.core.Term): Option[hydra.core.Type] =
  hydra.strip.deannotateTerm(funTerm) match
  case hydra.core.Term.lambda(v_Term_lambda_lam) => hydra.lib.maybes.bind[hydra.core.Type, hydra.core.Type](v_Term_lambda_lam.domain)((dom: hydra.core.Type) =>
    {
    lazy val mCod: Option[hydra.core.Type] = v_Term_lambda_lam.body match
      case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.lib.maybes.bind[hydra.core.Term, hydra.core.Type](hydra.lib.maps.lookup[hydra.core.Name,
         hydra.core.Term](hydra.constants.key_type)(v_Term_annotated_at.annotation))((typeTerm: hydra.core.Term) => hydra.java.coder.decodeTypeFromTerm(typeTerm))
      case hydra.core.Term.lambda(v_Term_lambda__innerLam) => hydra.java.coder.tryInferFunctionType(v_Term_lambda_lam.body)
      case _ => None
    hydra.lib.maybes.map[hydra.core.Type, hydra.core.Type]((cod: hydra.core.Type) => hydra.core.Type.function(hydra.core.FunctionType(dom, cod)))(mCod)
  })
  case _ => None

def typeAppFallbackCast(env: hydra.java.environment.JavaEnvironment)(aliases: hydra.java.environment.Aliases)(anns: Seq[Map[hydra.core.Name,
   hydra.core.Term]])(tyapps: Seq[hydra.java.syntax.Type])(jatyp: hydra.java.syntax.Type)(body: hydra.core.Term)(typ: hydra.core.Type)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.Expression] =
  {
  lazy val annotatedBody: hydra.core.Term = hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(typ)))(body)
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression, hydra.java.syntax.Expression](hydra.java.coder.encodeTermInternal(env)(anns)(hydra.lib.lists.cons[hydra.java.syntax.Type](jatyp)(tyapps))(annotatedBody)(cx)(g))((jbody: hydra.java.syntax.Expression) =>
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type, hydra.java.syntax.Expression](hydra.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(typ)(cx)(g))((jtype: hydra.java.syntax.Type) =>
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType, hydra.java.syntax.Expression](hydra.java.utils.javaTypeToJavaReferenceType(jtype)(cx))((rt: hydra.java.syntax.ReferenceType) =>
    Right(hydra.java.utils.javaCastExpressionToJavaExpression(hydra.java.utils.javaCastExpression(rt)(hydra.java.utils.javaExpressionToJavaUnaryExpression(jbody)))))))
}

def typeAppNullaryOrHoisted(env: hydra.java.environment.JavaEnvironment)(aliases: hydra.java.environment.Aliases)(anns: Seq[Map[hydra.core.Name,
   hydra.core.Term]])(tyapps: Seq[hydra.java.syntax.Type])(jatyp: hydra.java.syntax.Type)(body: hydra.core.Term)(correctedTyp: hydra.core.Type)(varName: hydra.core.Name)(cls: hydra.java.environment.JavaSymbolClass)(allTypeArgs: Seq[hydra.core.Type])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.Expression] =
  {
  lazy val qn: hydra.packaging.QualifiedName = hydra.names.qualifyName(varName)
  lazy val mns: Option[hydra.packaging.Namespace] = (qn.namespace)
  lazy val localName: scala.Predef.String = (qn.local)
  cls match
    case hydra.java.environment.JavaSymbolClass.nullaryFunction => hydra.lib.maybes.cases[hydra.packaging.Namespace,
       Either[hydra.errors.Error, hydra.java.syntax.Expression]](mns)(hydra.java.coder.typeAppFallbackCast(env)(aliases)(anns)(tyapps)(jatyp)(body)(correctedTyp)(cx)(g))((`ns_`: hydra.packaging.Namespace) =>
      {
      lazy val classId: hydra.java.syntax.Identifier = hydra.java.utils.nameToJavaName(aliases)(hydra.java.coder.elementsQualifiedName(`ns_`))
      {
        lazy val methodId: hydra.java.syntax.Identifier = hydra.java.utils.sanitizeJavaName(localName)
        hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Type], hydra.java.syntax.Expression](hydra.java.coder.filterPhantomTypeArgs(varName)(allTypeArgs)(cx)(g))((filteredTypeArgs: Seq[hydra.core.Type]) =>
          hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.TypeArgument], hydra.java.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Type,
             hydra.java.syntax.TypeArgument, hydra.errors.Error]((t: hydra.core.Type) =>
          hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type, hydra.java.syntax.TypeArgument](hydra.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(t)(cx)(g))((jt: hydra.java.syntax.Type) =>
          hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType, hydra.java.syntax.TypeArgument](hydra.java.utils.javaTypeToJavaReferenceType(jt)(cx))((rt: hydra.java.syntax.ReferenceType) => Right(hydra.java.syntax.TypeArgument.reference(rt)))))(filteredTypeArgs))((jTypeArgs: Seq[hydra.java.syntax.TypeArgument]) =>
          Right(hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocationStaticWithTypeArgs(classId)(methodId)(jTypeArgs)(Seq())))))
      }
    })
    case hydra.java.environment.JavaSymbolClass.hoistedLambda(v_JavaSymbolClass_hoistedLambda_arity) => hydra.lib.maybes.cases[hydra.packaging.Namespace,
       Either[hydra.errors.Error, hydra.java.syntax.Expression]](mns)(hydra.java.coder.typeAppFallbackCast(env)(aliases)(anns)(tyapps)(jatyp)(body)(correctedTyp)(cx)(g))((`ns_`: hydra.packaging.Namespace) =>
      {
      lazy val classId: hydra.java.syntax.Identifier = hydra.java.utils.nameToJavaName(aliases)(hydra.java.coder.elementsQualifiedName(`ns_`))
      {
        lazy val methodId: hydra.java.syntax.Identifier = hydra.java.utils.sanitizeJavaName(localName)
        hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Type], hydra.java.syntax.Expression](hydra.java.coder.filterPhantomTypeArgs(varName)(allTypeArgs)(cx)(g))((filteredTypeArgs: Seq[hydra.core.Type]) =>
          hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.java.syntax.TypeArgument], hydra.java.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Type,
             hydra.java.syntax.TypeArgument, hydra.errors.Error]((t: hydra.core.Type) =>
          hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Type, hydra.java.syntax.TypeArgument](hydra.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(t)(cx)(g))((jt: hydra.java.syntax.Type) =>
          hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.ReferenceType, hydra.java.syntax.TypeArgument](hydra.java.utils.javaTypeToJavaReferenceType(jt)(cx))((rt: hydra.java.syntax.ReferenceType) => Right(hydra.java.syntax.TypeArgument.reference(rt)))))(filteredTypeArgs))((jTypeArgs: Seq[hydra.java.syntax.TypeArgument]) =>
          {
          lazy val paramNames: Seq[hydra.core.Name] = hydra.lib.lists.map[Int, hydra.core.Name]((i: Int) => hydra.lib.strings.cat2("p")(hydra.lib.literals.showInt32(i)))(hydra.lib.math.range(0)(hydra.lib.math.sub(v_JavaSymbolClass_hoistedLambda_arity)(1)))
          {
            lazy val paramExprs: Seq[hydra.java.syntax.Expression] = hydra.lib.lists.map[hydra.core.Name, hydra.java.syntax.Expression]((p: hydra.core.Name) =>
              hydra.java.utils.javaIdentifierToJavaExpression(hydra.java.utils.variableToJavaIdentifier(p)))(paramNames)
            {
              lazy val call: hydra.java.syntax.Expression = hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocationStaticWithTypeArgs(classId)(methodId)(jTypeArgs)(paramExprs))
              Right(hydra.java.coder.buildCurriedLambda(paramNames)(call))
            }
          }
        }))
      }
    })
    case _ => hydra.java.coder.typeAppFallbackCast(env)(aliases)(anns)(tyapps)(jatyp)(body)(correctedTyp)(cx)(g)
}

def typeArgsOrDiamond(args: Seq[hydra.java.syntax.TypeArgument]): hydra.java.syntax.TypeArgumentsOrDiamond =
  hydra.lib.logic.ifElse[hydra.java.syntax.TypeArgumentsOrDiamond](hydra.java.coder.javaFeatures.supportsDiamondOperator)(hydra.java.syntax.TypeArgumentsOrDiamond.diamond)(hydra.java.syntax.TypeArgumentsOrDiamond.arguments(args))

def typesMatch(a: hydra.core.Type)(b: hydra.core.Type): Boolean =
  a match
  case hydra.core.Type.variable(v_Type_variable_va) => b match
    case hydra.core.Type.variable(v_Type_variable_vb) => hydra.lib.equality.equal[hydra.core.Name](v_Type_variable_va)(v_Type_variable_vb)
    case _ => true
  case hydra.core.Type.wrap(v_Type_wrap_wa) => b match
    case hydra.core.Type.wrap(v_Type_wrap_wb) => hydra.lib.equality.equal[hydra.core.Type](v_Type_wrap_wa)(v_Type_wrap_wb)
    case _ => true
  case _ => true

def unwrapReturnType(t: hydra.core.Type): hydra.core.Type =
  hydra.strip.deannotateType(t) match
  case hydra.core.Type.function(v_Type_function_ft) => hydra.java.coder.unwrapReturnType(v_Type_function_ft.codomain)
  case hydra.core.Type.application(v_Type_application_at) => hydra.java.coder.unwrapReturnType(v_Type_application_at.argument)
  case _ => t

def variantCompareToMethod[T0](aliases: hydra.java.environment.Aliases)(tparams: T0)(parentName: hydra.core.Name)(variantName: hydra.core.Name)(fields: Seq[hydra.core.FieldType]): hydra.java.syntax.ClassBodyDeclaration =
  {
  lazy val anns: Seq[hydra.java.syntax.Annotation] = Seq(hydra.java.utils.overrideAnnotation, hydra.java.utils.suppressWarningsUncheckedAnnotation)
  lazy val mods: Seq[hydra.java.syntax.MethodModifier] = Seq(hydra.java.syntax.MethodModifier.public)
  lazy val param: hydra.java.syntax.FormalParameter = hydra.java.utils.javaTypeToJavaFormalParameter(hydra.java.utils.javaTypeFromTypeName(aliases)(parentName))(hydra.java.names.otherInstanceName)
  lazy val result: hydra.java.syntax.Result = hydra.java.utils.javaTypeToJavaResult(hydra.java.utils.javaIntType)
  lazy val varTmpName: scala.Predef.String = "o"
  lazy val tagDeclStmt: hydra.java.syntax.BlockStatement = hydra.java.utils.variableDeclarationStatement(aliases)(hydra.java.utils.javaIntType)(hydra.java.utils.javaIdentifier("tagCmp"))(hydra.java.coder.tagCompareExpr)
  lazy val tagReturnStmt: hydra.java.syntax.BlockStatement = hydra.java.syntax.BlockStatement.statement(hydra.java.syntax.Statement.ifThen(hydra.java.syntax.IfThenStatement(hydra.java.coder.tagCmpNotZeroExpr,
     hydra.java.utils.javaReturnStatement(Some(hydra.java.utils.javaExpressionNameToJavaExpression(hydra.java.syntax.ExpressionName(None,
     hydra.java.utils.javaIdentifier("tagCmp"))))))))
  lazy val variantJavaType: hydra.java.syntax.Type = hydra.java.utils.javaTypeFromTypeName(aliases)(variantName)
  lazy val castOtherExpr: hydra.java.syntax.Expression = hydra.java.utils.javaCastExpressionToJavaExpression(hydra.java.utils.javaCastExpression(hydra.java.utils.nameToJavaReferenceType(aliases)(false)(Seq())(variantName)(None))(hydra.java.utils.javaIdentifierToJavaUnaryExpression(hydra.java.names.otherInstanceName)))
  lazy val castDeclStmt: hydra.java.syntax.BlockStatement = hydra.java.utils.variableDeclarationStatement(aliases)(variantJavaType)(hydra.java.utils.javaIdentifier(varTmpName))(castOtherExpr)
  lazy val emptyReturn: Seq[hydra.java.syntax.BlockStatement] = Seq(hydra.java.syntax.BlockStatement.statement(hydra.java.utils.javaReturnStatement(Some(hydra.java.utils.javaIntExpression(BigInt("0"))))))
  lazy val valueCompareStmt: Seq[hydra.java.syntax.BlockStatement] = hydra.lib.logic.ifElse[Seq[hydra.java.syntax.BlockStatement]](hydra.lib.lists.`null`[hydra.core.FieldType](fields))(emptyReturn)(hydra.lib.lists.concat2[hydra.java.syntax.BlockStatement](Seq(castDeclStmt))(hydra.java.coder.compareToBody(aliases)(varTmpName)(fields)))
  lazy val body: Seq[hydra.java.syntax.BlockStatement] = hydra.lib.lists.concat2[hydra.java.syntax.BlockStatement](Seq(tagDeclStmt,
     tagReturnStmt))(valueCompareStmt)
  hydra.java.utils.methodDeclaration(mods)(Seq())(anns)(hydra.java.names.compareToMethodName)(Seq(param))(result)(Some(body))
}

def visitBranch(env: hydra.java.environment.JavaEnvironment)(aliases: hydra.java.environment.Aliases)(dom: hydra.core.Type)(tname: hydra.core.Name)(jcod: hydra.java.syntax.Type)(targs: Seq[hydra.java.syntax.TypeArgument])(field: hydra.core.Field)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.java.syntax.ClassBodyDeclarationWithComments] =
  {
  lazy val jdom: hydra.java.syntax.Type = hydra.java.syntax.Type.reference(hydra.java.utils.nameToJavaReferenceType(aliases)(true)(targs)(tname)(Some(hydra.formatting.capitalize(field.name))))
  lazy val mods: Seq[hydra.java.syntax.MethodModifier] = Seq(hydra.java.syntax.MethodModifier.public)
  lazy val anns: Seq[hydra.java.syntax.Annotation] = Seq(hydra.java.utils.overrideAnnotation)
  lazy val result: hydra.java.syntax.Result = hydra.java.syntax.Result.`type`(jcod)
  hydra.strip.deannotateTerm(field.term) match
    case hydra.core.Term.lambda(v_Term_lambda_lam) => hydra.java.coder.withLambda(env)(v_Term_lambda_lam)((env2: hydra.java.environment.JavaEnvironment) =>
      {
      lazy val lambdaParam: hydra.core.Name = (v_Term_lambda_lam.parameter)
      {
        lazy val body: hydra.core.Term = (v_Term_lambda_lam.body)
        {
          lazy val env3: hydra.java.environment.JavaEnvironment = hydra.java.coder.insertBranchVar(lambdaParam)(env2)
          hydra.lib.eithers.bind[hydra.errors.Error, hydra.typing.FunctionStructure[hydra.java.environment.JavaEnvironment],
             hydra.java.syntax.ClassBodyDeclarationWithComments](hydra.java.coder.analyzeJavaFunction(env3)(body)(cx)(g))((fs: hydra.typing.FunctionStructure[hydra.java.environment.JavaEnvironment]) =>
            {
            lazy val bindings: Seq[hydra.core.Binding] = (fs.bindings)
            {
              lazy val innerBody: hydra.core.Term = (fs.body)
              {
                lazy val env4: hydra.java.environment.JavaEnvironment = (fs.environment)
                hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[Seq[hydra.java.syntax.BlockStatement],
                   hydra.java.environment.JavaEnvironment], hydra.java.syntax.ClassBodyDeclarationWithComments](hydra.java.coder.bindingsToStatements(env4)(bindings)(cx)(g))((bindResult: Tuple2[Seq[hydra.java.syntax.BlockStatement],
                   hydra.java.environment.JavaEnvironment]) =>
                  {
                  lazy val bindingStmts: Seq[hydra.java.syntax.BlockStatement] = hydra.lib.pairs.first[Seq[hydra.java.syntax.BlockStatement],
                     hydra.java.environment.JavaEnvironment](bindResult)
                  {
                    lazy val env5: hydra.java.environment.JavaEnvironment = hydra.lib.pairs.second[Seq[hydra.java.syntax.BlockStatement],
                       hydra.java.environment.JavaEnvironment](bindResult)
                    hydra.lib.eithers.bind[hydra.errors.Error, hydra.java.syntax.Expression, hydra.java.syntax.ClassBodyDeclarationWithComments](hydra.java.coder.encodeTerm(env5)(innerBody)(cx)(g))((jret: hydra.java.syntax.Expression) =>
                      {
                      lazy val param: hydra.java.syntax.FormalParameter = hydra.java.utils.javaTypeToJavaFormalParameter(jdom)(lambdaParam)
                      {
                        lazy val returnStmt: hydra.java.syntax.BlockStatement = hydra.java.syntax.BlockStatement.statement(hydra.java.utils.javaReturnStatement(Some(jret)))
                        {
                          lazy val allStmts: Seq[hydra.java.syntax.BlockStatement] = hydra.lib.lists.concat2[hydra.java.syntax.BlockStatement](bindingStmts)(Seq(returnStmt))
                          Right(hydra.java.coder.noComment(hydra.java.utils.methodDeclaration(mods)(Seq())(anns)(hydra.java.names.visitMethodName)(Seq(param))(result)(Some(allStmts))))
                        }
                      }
                    })
                  }
                })
              }
            }
          })
        }
      }
    })
    case _ => Left(hydra.errors.Error.other(hydra.lib.strings.cat2("visitBranch: field term is not a lambda: ")(hydra.show.core.term(field.term))))
}

def withLambda[T0](env: hydra.java.environment.JavaEnvironment)(lam: hydra.core.Lambda)(k: (hydra.java.environment.JavaEnvironment => T0)): T0 =
  hydra.environment.withLambdaContext(hydra.java.coder.javaEnvGetGraph)(hydra.java.coder.javaEnvSetGraph)(env)(lam)((env1: hydra.java.environment.JavaEnvironment) =>
  {
  lazy val aliases: hydra.java.environment.Aliases = (env1.aliases)
  {
    lazy val aliases2: hydra.java.environment.Aliases = hydra.java.environment.Aliases(aliases.currentNamespace,
       (aliases.packages), (aliases.branchVars), (aliases.recursiveVars), (aliases.inScopeTypeParams),
       (aliases.polymorphicLocals), (aliases.inScopeJavaVars), (aliases.varRenames), hydra.lib.sets.insert[hydra.core.Name](lam.parameter)(aliases.lambdaVars),
       (aliases.typeVarSubst), (aliases.trustedTypeVars), (aliases.methodCodomain), (aliases.thunkedVars))
    {
      lazy val env2: hydra.java.environment.JavaEnvironment = hydra.java.environment.JavaEnvironment(aliases2, (env1.graph))
      k(env2)
    }
  }
})

def withTypeLambda[T0](v1: hydra.java.environment.JavaEnvironment)(v2: hydra.core.TypeLambda)(v3: (hydra.java.environment.JavaEnvironment => T0)): T0 =
  hydra.environment.withTypeLambdaContext(hydra.java.coder.javaEnvGetGraph)(hydra.java.coder.javaEnvSetGraph)(v1)(v2)(v3)

def wrapInSupplierLambda(expr: hydra.java.syntax.Expression): hydra.java.syntax.Expression =
  hydra.java.syntax.Expression.lambda(hydra.java.syntax.LambdaExpression(hydra.java.syntax.LambdaParameters.tuple(Seq()),
     hydra.java.syntax.LambdaBody.expression(expr)))

def wrapLazyArguments(name: hydra.core.Name)(args: Seq[hydra.java.syntax.Expression]): Tuple2[Seq[hydra.java.syntax.Expression], Option[scala.Predef.String]] =
  hydra.lib.logic.ifElse[Tuple2[Seq[hydra.java.syntax.Expression], Option[scala.Predef.String]]](hydra.lib.logic.and(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.logic.ifElse"))(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.java.syntax.Expression](args))(3)))(Tuple2(Seq(hydra.lib.lists.at[hydra.java.syntax.Expression](0)(args),
     hydra.java.coder.wrapInSupplierLambda(hydra.lib.lists.at[hydra.java.syntax.Expression](1)(args)),
     hydra.java.coder.wrapInSupplierLambda(hydra.lib.lists.at[hydra.java.syntax.Expression](2)(args))),
     Some("lazy")))(hydra.lib.logic.ifElse[Tuple2[Seq[hydra.java.syntax.Expression], Option[scala.Predef.String]]](hydra.lib.logic.and(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.maybes.maybe"))(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.java.syntax.Expression](args))(3)))(Tuple2(Seq(hydra.java.coder.wrapInSupplierLambda(hydra.lib.lists.at[hydra.java.syntax.Expression](0)(args)),
     hydra.lib.lists.at[hydra.java.syntax.Expression](1)(args), hydra.lib.lists.at[hydra.java.syntax.Expression](2)(args)),
     Some("applyLazy")))(hydra.lib.logic.ifElse[Tuple2[Seq[hydra.java.syntax.Expression], Option[scala.Predef.String]]](hydra.lib.logic.and(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.maybes.cases"))(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.java.syntax.Expression](args))(3)))(Tuple2(Seq(hydra.lib.lists.at[hydra.java.syntax.Expression](0)(args),
     hydra.java.coder.wrapInSupplierLambda(hydra.lib.lists.at[hydra.java.syntax.Expression](1)(args)),
     hydra.lib.lists.at[hydra.java.syntax.Expression](2)(args)), Some("applyLazy")))(hydra.lib.logic.ifElse[Tuple2[Seq[hydra.java.syntax.Expression],
     Option[scala.Predef.String]]](hydra.lib.logic.and(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.maps.findWithDefault"))(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.java.syntax.Expression](args))(3)))(Tuple2(Seq(hydra.java.coder.wrapInSupplierLambda(hydra.lib.lists.at[hydra.java.syntax.Expression](0)(args)),
     hydra.lib.lists.at[hydra.java.syntax.Expression](1)(args), hydra.lib.lists.at[hydra.java.syntax.Expression](2)(args)),
     Some("applyLazy")))(hydra.lib.logic.ifElse[Tuple2[Seq[hydra.java.syntax.Expression], Option[scala.Predef.String]]](hydra.lib.logic.and(hydra.lib.logic.or(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.maybes.fromMaybe"))(hydra.lib.logic.or(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.eithers.fromLeft"))(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.eithers.fromRight"))))(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.java.syntax.Expression](args))(2)))(Tuple2(Seq(hydra.java.coder.wrapInSupplierLambda(hydra.lib.lists.at[hydra.java.syntax.Expression](0)(args)),
     hydra.lib.lists.at[hydra.java.syntax.Expression](1)(args)), Some("applyLazy")))(Tuple2(args, None))))))
