package hydra.reduction

import hydra.core.*

import hydra.errors.*

import hydra.graph.*

def alphaConvert(vold: hydra.core.Name)(vnew: hydra.core.Name)(term: hydra.core.Term): hydra.core.Term =
  hydra.variables.replaceFreeTermVariable(vold)(hydra.core.Term.variable(vnew))(term)

def betaReduceType[T0](cx: T0)(graph: hydra.graph.Graph)(typ: hydra.core.Type): Either[hydra.errors.Error,
   hydra.core.Type] =
  {
  def reduceApp(app: hydra.core.ApplicationType): Either[hydra.errors.Error, hydra.core.Type] =
    {
    lazy val lhs: hydra.core.Type = (app.function)
    lazy val rhs: hydra.core.Type = (app.argument)
    lhs match
      case hydra.core.Type.annotated(v_Type_annotated_at) => hydra.lib.eithers.bind[hydra.errors.Error,
         hydra.core.Type, hydra.core.Type](reduceApp(hydra.core.ApplicationType(v_Type_annotated_at.body,
         rhs)))((a: hydra.core.Type) =>
        Right(hydra.core.Type.annotated(hydra.core.AnnotatedType(a, (v_Type_annotated_at.annotation)))))
      case hydra.core.Type.forall(v_Type_forall_ft) => hydra.reduction.betaReduceType(cx)(graph)(hydra.variables.replaceFreeTypeVariable(v_Type_forall_ft.parameter)(rhs)(v_Type_forall_ft.body))
      case hydra.core.Type.variable(v_Type_variable_name) => hydra.lib.eithers.bind[hydra.errors.Error,
         hydra.core.Type, hydra.core.Type](hydra.resolution.requireType(cx)(graph)(v_Type_variable_name))((`t_`: hydra.core.Type) =>
        hydra.reduction.betaReduceType(cx)(graph)(hydra.core.Type.application(hydra.core.ApplicationType(`t_`, rhs))))
  }
  def mapExpr[T1](recurse: (T1 => Either[hydra.errors.Error, hydra.core.Type]))(t: T1): Either[hydra.errors.Error,
     hydra.core.Type] =
    {
    def findApp(r: hydra.core.Type): Either[hydra.errors.Error, hydra.core.Type] =
      r match
      case hydra.core.Type.application(v_Type_application_a) => reduceApp(v_Type_application_a)
      case _ => Right(r)
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Type, hydra.core.Type](recurse(t))((r: hydra.core.Type) => findApp(r))
  }
  hydra.rewriting.rewriteTypeM(mapExpr)(typ)
}

def contractTerm(term: hydra.core.Term): hydra.core.Term =
  {
  def rewrite[T0](recurse: (T0 => hydra.core.Term))(t: T0): hydra.core.Term =
    {
    lazy val rec: hydra.core.Term = recurse(t)
    rec match
      case hydra.core.Term.application(v_Term_application_app) => {
        lazy val lhs: hydra.core.Term = (v_Term_application_app.function)
        {
          lazy val rhs: hydra.core.Term = (v_Term_application_app.argument)
          hydra.strip.deannotateTerm(lhs) match
            case hydra.core.Term.lambda(v_Term_lambda_l) => {
              lazy val v: hydra.core.Name = (v_Term_lambda_l.parameter)
              {
                lazy val body: hydra.core.Term = (v_Term_lambda_l.body)
                hydra.lib.logic.ifElse[hydra.core.Term](hydra.variables.isFreeVariableInTerm(v)(body))(body)(hydra.variables.replaceFreeTermVariable(v)(rhs)(body))
              }
            }
            case _ => rec
        }
      }
      case _ => rec
  }
  hydra.rewriting.rewriteTerm(rewrite)(term)
}

lazy val countPrimitiveInvocations: Boolean = true

def etaExpandTerm(tx0: hydra.graph.Graph)(term0: hydra.core.Term): hydra.core.Term =
  {
  lazy val primTypes: Map[hydra.core.Name, hydra.core.TypeScheme] = hydra.lib.maps.fromList[hydra.core.Name,
     hydra.core.TypeScheme](hydra.lib.lists.map[hydra.graph.Primitive, Tuple2[hydra.core.Name,
     hydra.core.TypeScheme]]((_gpt_p: hydra.graph.Primitive) => Tuple2(_gpt_p.name,
     (_gpt_p.`type`)))(hydra.lib.maps.elems[hydra.core.Name, hydra.graph.Primitive](tx0.primitives)))
  def termArityWithContext(tx: hydra.graph.Graph)(term: hydra.core.Term): Int =
    term match
    case hydra.core.Term.annotated(v_Term_annotated_at) => termArityWithContext(tx)(v_Term_annotated_at.body)
    case hydra.core.Term.application(v_Term_application_app) => hydra.lib.math.sub(termArityWithContext(tx)(v_Term_application_app.function))(1)
    case hydra.core.Term.cases(v_Term_cases__) => 1
    case hydra.core.Term.lambda(v_Term_lambda__) => 0
    case hydra.core.Term.project(v_Term_project__) => 1
    case hydra.core.Term.unwrap(v_Term_unwrap__) => 1
    case hydra.core.Term.let(v_Term_let_l) => termArityWithContext(hydra.scoping.extendGraphForLet((_x: hydra.graph.Graph) => (_2: hydra.core.Binding) => None)(tx)(v_Term_let_l))(v_Term_let_l.body)
    case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => termArityWithContext(hydra.scoping.extendGraphForTypeLambda(tx)(v_Term_typeLambda_tl))(v_Term_typeLambda_tl.body)
    case hydra.core.Term.typeApplication(v_Term_typeApplication_tat) => termArityWithContext(tx)(v_Term_typeApplication_tat.body)
    case hydra.core.Term.variable(v_Term_variable_name) => hydra.lib.maybes.maybe[Int,
       hydra.core.Type](hydra.lib.maybes.maybe[Int, hydra.core.TypeScheme](0)(hydra.arity.typeSchemeArity)(hydra.lib.maps.lookup[hydra.core.Name,
       hydra.core.TypeScheme](v_Term_variable_name)(primTypes)))(hydra.arity.typeArity)(hydra.lib.maybes.map[hydra.core.TypeScheme,
       hydra.core.Type](hydra.scoping.typeSchemeToFType)(hydra.lib.maps.lookup[hydra.core.Name,
       hydra.core.TypeScheme](v_Term_variable_name)(tx.boundTypes)))
    case _ => 0
  def domainTypes(n: Int)(mt: Option[hydra.core.Type]): Seq[Option[hydra.core.Type]] =
    hydra.lib.logic.ifElse[Seq[Option[hydra.core.Type]]](hydra.lib.equality.lte[Int](n)(0))(Seq())(hydra.lib.maybes.maybe[Seq[Option[hydra.core.Type]],
       hydra.core.Type](hydra.lib.lists.map[Int, Option[hydra.core.Type]]((_x: Int) => None)(hydra.lib.math.range(1)(n)))((typ: hydra.core.Type) =>
    typ match
    case hydra.core.Type.function(v_Type_function_ftyp) => hydra.lib.lists.cons[Option[hydra.core.Type]](Some(v_Type_function_ftyp.domain))(domainTypes(hydra.lib.math.sub(n)(1))(Some(v_Type_function_ftyp.codomain)))
    case hydra.core.Type.annotated(v_Type_annotated_at) => domainTypes(n)(Some(v_Type_annotated_at.body))
    case hydra.core.Type.application(v_Type_application_atyp) => domainTypes(n)(Some(v_Type_application_atyp.function))
    case hydra.core.Type.forall(v_Type_forall__) => hydra.lib.lists.map[Int, Option[hydra.core.Type]]((_2: Int) => None)(hydra.lib.math.range(1)(n))
    case _ => hydra.lib.lists.map[Int, Option[hydra.core.Type]]((_x: Int) => None)(hydra.lib.math.range(1)(n)))(mt))
  def peelFunctionDomains(mtyp: Option[hydra.core.Type])(n: Int): Option[hydra.core.Type] =
    hydra.lib.logic.ifElse[Option[hydra.core.Type]](hydra.lib.equality.lte[Int](n)(0))(mtyp)(hydra.lib.maybes.maybe[Option[hydra.core.Type],
       hydra.core.Type](None)((typ: hydra.core.Type) =>
    typ match
    case hydra.core.Type.function(v_Type_function_ftyp) => peelFunctionDomains(Some(v_Type_function_ftyp.codomain))(hydra.lib.math.sub(n)(1))
    case hydra.core.Type.annotated(v_Type_annotated_at) => peelFunctionDomains(Some(v_Type_annotated_at.body))(n)
    case hydra.core.Type.application(v_Type_application_atyp) => peelFunctionDomains(Some(v_Type_application_atyp.function))(n)
    case hydra.core.Type.forall(v_Type_forall__) => None
    case _ => None)(mtyp))
  def expand(alwaysPad: Boolean)(args: Seq[hydra.core.Term])(arity: Int)(headTyp: Option[hydra.core.Type])(head: hydra.core.Term): hydra.core.Term =
    {
    lazy val applied: hydra.core.Term = hydra.lib.lists.foldl[hydra.core.Term, hydra.core.Term]((lhs: hydra.core.Term) =>
      (arg: hydra.core.Term) =>
      hydra.core.Term.application(hydra.core.Application(lhs, arg)))(head)(args)
    lazy val numArgs: Int = hydra.lib.lists.length[hydra.core.Term](args)
    lazy val needed: Int = hydra.lib.math.sub(arity)(numArgs)
    hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.logic.and(hydra.lib.equality.gt[Int](needed)(0))(hydra.lib.logic.or(alwaysPad)(hydra.lib.equality.gt[Int](numArgs)(0))))({
      lazy val indices: Seq[Int] = hydra.lib.math.range(1)(needed)
      {
        lazy val remainingType: Option[hydra.core.Type] = peelFunctionDomains(headTyp)(numArgs)
        {
          lazy val domains: Seq[Option[hydra.core.Type]] = domainTypes(needed)(remainingType)
          {
            lazy val codomainType: Option[hydra.core.Type] = peelFunctionDomains(remainingType)(needed)
            {
              lazy val fullyAppliedRaw: hydra.core.Term = hydra.lib.lists.foldl[hydra.core.Term,
                 Int]((body: hydra.core.Term) =>
                (i: Int) =>
                {
                lazy val vn: hydra.core.Name = hydra.lib.strings.cat2("v")(hydra.lib.literals.showInt32(i))
                hydra.core.Term.application(hydra.core.Application(body, hydra.core.Term.variable(vn)))
              })(applied)(indices)
              {
                lazy val fullyApplied: hydra.core.Term = hydra.lib.maybes.maybe[hydra.core.Term,
                   hydra.core.Type](fullyAppliedRaw)((ct: hydra.core.Type) =>
                  hydra.core.Term.annotated(hydra.core.AnnotatedTerm(fullyAppliedRaw,
                     hydra.lib.maps.singleton[hydra.core.Name, hydra.core.Term]("type")(hydra.encode.core.`type`(ct)))))(codomainType)
                {
                  lazy val indexedDomains: Seq[Tuple2[Int, Option[hydra.core.Type]]] = hydra.lib.lists.zip[Int,
                     Option[hydra.core.Type]](indices)(domains)
                  hydra.lib.lists.foldl[hydra.core.Term, Tuple2[Int, Option[hydra.core.Type]]]((body: hydra.core.Term) =>
                    (idPair: Tuple2[Int, Option[hydra.core.Type]]) =>
                    {
                    lazy val i: Int = hydra.lib.pairs.first[Int, Option[hydra.core.Type]](idPair)
                    {
                      lazy val dom: Option[hydra.core.Type] = hydra.lib.pairs.second[Int,
                         Option[hydra.core.Type]](idPair)
                      {
                        lazy val vn: hydra.core.Name = hydra.lib.strings.cat2("v")(hydra.lib.literals.showInt32(i))
                        hydra.core.Term.lambda(hydra.core.Lambda(vn, dom, body))
                      }
                    }
                  })(fullyApplied)(hydra.lib.lists.reverse[Tuple2[Int, Option[hydra.core.Type]]](indexedDomains))
                }
              }
            }
          }
        }
      }
    })(applied)
  }
  def rewriteWithArgs(args: Seq[hydra.core.Term])(tx: hydra.graph.Graph)(term: hydra.core.Term): hydra.core.Term =
    {
    def recurse(tx1: hydra.graph.Graph)(term1: hydra.core.Term): hydra.core.Term = rewriteWithArgs(Seq())(tx1)(term1)
    def termHeadType(tx2: hydra.graph.Graph)(trm2: hydra.core.Term): Option[hydra.core.Type] =
      trm2 match
      case hydra.core.Term.annotated(v_Term_annotated_at2) => termHeadType(tx2)(v_Term_annotated_at2.body)
      case hydra.core.Term.lambda(v_Term_lambda__) => None
      case hydra.core.Term.cases(v_Term_cases__) => None
      case hydra.core.Term.project(v_Term_project__) => None
      case hydra.core.Term.unwrap(v_Term_unwrap__) => None
      case hydra.core.Term.let(v_Term_let_l2) => termHeadType(hydra.scoping.extendGraphForLet((_x: hydra.graph.Graph) => (_2: hydra.core.Binding) => None)(tx2)(v_Term_let_l2))(v_Term_let_l2.body)
      case hydra.core.Term.typeLambda(v_Term_typeLambda_tl2) => termHeadType(hydra.scoping.extendGraphForTypeLambda(tx2)(v_Term_typeLambda_tl2))(v_Term_typeLambda_tl2.body)
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tat2) => hydra.lib.maybes.bind[hydra.core.Type,
         hydra.core.Type](termHeadType(tx2)(v_Term_typeApplication_tat2.body))((htyp2: hydra.core.Type) =>
        htyp2 match
        case hydra.core.Type.forall(v_Type_forall_ft2) => Some(hydra.variables.replaceFreeTypeVariable(v_Type_forall_ft2.parameter)(v_Term_typeApplication_tat2.`type`)(v_Type_forall_ft2.body))
        case _ => Some(htyp2))
      case hydra.core.Term.variable(v_Term_variable_vn2) => hydra.lib.maybes.map[hydra.core.TypeScheme,
         hydra.core.Type](hydra.scoping.typeSchemeToFType)(hydra.lib.maps.lookup[hydra.core.Name,
         hydra.core.TypeScheme](v_Term_variable_vn2)(tx2.boundTypes))
      case _ => None
    def afterRecursion(trm: hydra.core.Term): hydra.core.Term =
      {
      lazy val arity: Int = termArityWithContext(tx)(trm)
      lazy val hType: Option[hydra.core.Type] = termHeadType(tx)(trm)
      expand(false)(args)(arity)(hType)(trm)
    }
    def forField(f: hydra.core.Field): hydra.core.Field = hydra.core.Field(f.name, recurse(tx)(f.term))
    def forCaseBranch(f: hydra.core.Field): hydra.core.Field =
      {
      lazy val branchBody: hydra.core.Term = recurse(tx)(f.term)
      lazy val arty: Int = termArityWithContext(tx)(branchBody)
      lazy val branchHType: Option[hydra.core.Type] = termHeadType(tx)(branchBody)
      hydra.core.Field(f.name, expand(true)(Seq())(arty)(branchHType)(branchBody))
    }
    def forMap(mp: Map[hydra.core.Term, hydra.core.Term]): Map[hydra.core.Term, hydra.core.Term] =
      {
      def forPair(pr: Tuple2[hydra.core.Term, hydra.core.Term]): Tuple2[hydra.core.Term, hydra.core.Term] =
        Tuple2(recurse(tx)(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](pr)),
           recurse(tx)(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](pr)))
      hydra.lib.maps.fromList[hydra.core.Term, hydra.core.Term](hydra.lib.lists.map[Tuple2[hydra.core.Term,
         hydra.core.Term], Tuple2[hydra.core.Term, hydra.core.Term]](forPair)(hydra.lib.maps.toList[hydra.core.Term,
         hydra.core.Term](mp)))
    }
    term match
      case hydra.core.Term.annotated(v_Term_annotated_at) => afterRecursion(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(recurse(tx)(v_Term_annotated_at.body),
         (v_Term_annotated_at.annotation))))
      case hydra.core.Term.application(v_Term_application_app) => {
        lazy val rhs: hydra.core.Term = rewriteWithArgs(Seq())(tx)(v_Term_application_app.argument)
        rewriteWithArgs(hydra.lib.lists.cons[hydra.core.Term](rhs)(args))(tx)(v_Term_application_app.function)
      }
      case hydra.core.Term.either(v_Term_either_e) => afterRecursion(hydra.core.Term.either(hydra.lib.eithers.either[hydra.core.Term,
         hydra.core.Term, Either[hydra.core.Term, hydra.core.Term]]((l: hydra.core.Term) => Left(recurse(tx)(l)))((r: hydra.core.Term) => Right(recurse(tx)(r)))(v_Term_either_e)))
      case hydra.core.Term.cases(v_Term_cases_cs) => {
        lazy val newCs: hydra.core.CaseStatement = hydra.core.CaseStatement(v_Term_cases_cs.typeName,
           hydra.lib.maybes.map[hydra.core.Term, hydra.core.Term]((t1: hydra.core.Term) => recurse(tx)(t1))(v_Term_cases_cs.default),
           hydra.lib.lists.map[hydra.core.Field, hydra.core.Field](forCaseBranch)(v_Term_cases_cs.cases))
        {
          lazy val elimTerm: hydra.core.Term = hydra.core.Term.cases(newCs)
          {
            lazy val elimHeadType: Option[hydra.core.Type] = Some(hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable(v_Term_cases_cs.typeName),
               hydra.core.Type.unit)))
            expand(true)(args)(1)(elimHeadType)(elimTerm)
          }
        }
      }
      case hydra.core.Term.project(v_Term_project_p) => expand(false)(args)(1)(None)(hydra.core.Term.project(v_Term_project_p))
      case hydra.core.Term.unwrap(v_Term_unwrap_nm) => expand(false)(args)(1)(None)(hydra.core.Term.unwrap(v_Term_unwrap_nm))
      case hydra.core.Term.lambda(v_Term_lambda_lm) => {
        lazy val tx1: hydra.graph.Graph = hydra.scoping.extendGraphForLambda(tx)(v_Term_lambda_lm)
        {
          lazy val body: hydra.core.Term = rewriteWithArgs(Seq())(tx1)(v_Term_lambda_lm.body)
          {
            lazy val result: hydra.core.Term = hydra.core.Term.lambda(hydra.core.Lambda(v_Term_lambda_lm.parameter,
               (v_Term_lambda_lm.domain), body))
            {
              lazy val arty: Int = termArityWithContext(tx)(result)
              expand(false)(args)(arty)(None)(result)
            }
          }
        }
      }
      case hydra.core.Term.let(v_Term_let_lt) => {
        lazy val tx1: hydra.graph.Graph = hydra.scoping.extendGraphForLet((_x: hydra.graph.Graph) => (_2: hydra.core.Binding) => None)(tx)(v_Term_let_lt)
        {
          def mapBinding(b: hydra.core.Binding): hydra.core.Binding = hydra.core.Binding(b.name,
             rewriteWithArgs(Seq())(tx1)(b.term), (b.`type`))
          {
            lazy val result: hydra.core.Term = hydra.core.Term.let(hydra.core.Let(hydra.lib.lists.map[hydra.core.Binding,
               hydra.core.Binding](mapBinding)(v_Term_let_lt.bindings), rewriteWithArgs(Seq())(tx1)(v_Term_let_lt.body)))
            afterRecursion(result)
          }
        }
      }
      case hydra.core.Term.list(v_Term_list_els) => afterRecursion(hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Term,
         hydra.core.Term]((el: hydra.core.Term) => recurse(tx)(el))(v_Term_list_els)))
      case hydra.core.Term.literal(v_Term_literal_v) => hydra.core.Term.literal(v_Term_literal_v)
      case hydra.core.Term.map(v_Term_map_mp) => afterRecursion(hydra.core.Term.map(forMap(v_Term_map_mp)))
      case hydra.core.Term.maybe(v_Term_maybe_mb) => afterRecursion(hydra.core.Term.maybe(hydra.lib.maybes.map[hydra.core.Term,
         hydra.core.Term]((v: hydra.core.Term) => recurse(tx)(v))(v_Term_maybe_mb)))
      case hydra.core.Term.pair(v_Term_pair_pr) => afterRecursion(hydra.core.Term.pair(Tuple2(recurse(tx)(hydra.lib.pairs.first[hydra.core.Term,
         hydra.core.Term](v_Term_pair_pr)), recurse(tx)(hydra.lib.pairs.second[hydra.core.Term,
         hydra.core.Term](v_Term_pair_pr)))))
      case hydra.core.Term.record(v_Term_record_rc) => afterRecursion(hydra.core.Term.record(hydra.core.Record(v_Term_record_rc.typeName,
         hydra.lib.lists.map[hydra.core.Field, hydra.core.Field](forField)(v_Term_record_rc.fields))))
      case hydra.core.Term.set(v_Term_set_st) => afterRecursion(hydra.core.Term.set(hydra.lib.sets.fromList[hydra.core.Term](hydra.lib.lists.map[hydra.core.Term,
         hydra.core.Term]((el: hydra.core.Term) => recurse(tx)(el))(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_st)))))
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => afterRecursion(hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(recurse(tx)(v_Term_typeApplication_tt.body),
         (v_Term_typeApplication_tt.`type`))))
      case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => {
        lazy val tx1: hydra.graph.Graph = hydra.scoping.extendGraphForTypeLambda(tx)(v_Term_typeLambda_tl)
        {
          lazy val result: hydra.core.Term = hydra.core.Term.typeLambda(hydra.core.TypeLambda(v_Term_typeLambda_tl.parameter,
             rewriteWithArgs(Seq())(tx1)(v_Term_typeLambda_tl.body)))
          afterRecursion(result)
        }
      }
      case hydra.core.Term.inject(v_Term_inject_inj) => afterRecursion(hydra.core.Term.inject(hydra.core.Injection(v_Term_inject_inj.typeName,
         forField(v_Term_inject_inj.field))))
      case hydra.core.Term.unit => hydra.core.Term.unit
      case hydra.core.Term.variable(v_Term_variable_vn) => {
        lazy val arty: Int = termArityWithContext(tx)(term)
        {
          lazy val varType: Option[hydra.core.Type] = hydra.lib.maybes.map[hydra.core.TypeScheme,
             hydra.core.Type](hydra.scoping.typeSchemeToFType)(hydra.lib.maps.lookup[hydra.core.Name,
             hydra.core.TypeScheme](v_Term_variable_vn)(tx.boundTypes))
          expand(false)(args)(arty)(varType)(term)
        }
      }
      case hydra.core.Term.wrap(v_Term_wrap_wt) => afterRecursion(hydra.core.Term.wrap(hydra.core.WrappedTerm(v_Term_wrap_wt.typeName,
         recurse(tx)(v_Term_wrap_wt.body))))
  }
  hydra.reduction.contractTerm(rewriteWithArgs(Seq())(tx0)(term0))
}

def etaExpandTypedTerm(cx: hydra.context.Context)(tx0: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  {
  def rewrite(topLevel: Boolean)(forced: Boolean)(typeArgs: Seq[hydra.core.Type])(recurse: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.Error,
     hydra.core.Term]))(tx: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.Error,
     hydra.core.Term] =
    {
    def rewriteSpine(term2: hydra.core.Term): Either[hydra.errors.Error, hydra.core.Term] =
      term2 match
      case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.lib.eithers.bind[hydra.errors.Error,
         hydra.core.Term, hydra.core.Term](rewriteSpine(v_Term_annotated_at.body))((body: hydra.core.Term) =>
        {
        lazy val ann: Map[hydra.core.Name, hydra.core.Term] = (v_Term_annotated_at.annotation)
        Right(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(body, ann)))
      })
      case hydra.core.Term.application(v_Term_application_a) => {
        lazy val l: Seq[hydra.core.Type] = hydra.lib.logic.ifElse[Seq[hydra.core.Type]](false)(Seq(hydra.core.Type.literal(hydra.core.LiteralType.string)))(Seq())
        hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.Term](rewriteSpine(v_Term_application_a.function))((lhs: hydra.core.Term) =>
          hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.Term](rewrite(true)(false)(l)(recurse)(tx)(v_Term_application_a.argument))((rhs: hydra.core.Term) =>
          Right(hydra.core.Term.application(hydra.core.Application(lhs, rhs)))))
      }
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tat) => hydra.lib.eithers.bind[hydra.errors.Error,
         hydra.core.Term, hydra.core.Term](rewriteSpine(v_Term_typeApplication_tat.body))((body: hydra.core.Term) =>
        {
        lazy val typ: hydra.core.Type = (v_Term_typeApplication_tat.`type`)
        Right(hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(body, typ)))
      })
      case _ => rewrite(false)(false)(Seq())(recurse)(tx)(term2)
    def arityOf(tx2: hydra.graph.Graph)(term2: hydra.core.Term): Either[hydra.errors.Error, Int] =
      {
      lazy val dflt: Either[hydra.errors.Error, Int] = hydra.lib.eithers.map[Tuple2[hydra.core.Type,
         hydra.context.Context], Int, hydra.errors.Error]((_tc: Tuple2[hydra.core.Type,
         hydra.context.Context]) =>
        hydra.arity.typeArity(hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](_tc)))(hydra.checking.typeOf(cx)(tx2)(Seq())(term2))
      term2 match
        case hydra.core.Term.annotated(v_Term_annotated_at) => arityOf(tx2)(v_Term_annotated_at.body)
        case hydra.core.Term.cases(v_Term_cases__) => Right(1)
        case hydra.core.Term.project(v_Term_project__) => Right(1)
        case hydra.core.Term.unwrap(v_Term_unwrap__) => Right(1)
        case hydra.core.Term.lambda(v_Term_lambda_l) => {
          lazy val txl: hydra.graph.Graph = hydra.scoping.extendGraphForLambda(tx2)(v_Term_lambda_l)
          arityOf(txl)(v_Term_lambda_l.body)
        }
        case hydra.core.Term.let(v_Term_let_l) => {
          lazy val txl: hydra.graph.Graph = hydra.scoping.extendGraphForLet((_x: hydra.graph.Graph) => (_2: hydra.core.Binding) => None)(tx2)(v_Term_let_l)
          arityOf(txl)(v_Term_let_l.body)
        }
        case hydra.core.Term.typeApplication(v_Term_typeApplication_tat) => arityOf(tx2)(v_Term_typeApplication_tat.body)
        case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => {
          lazy val txt: hydra.graph.Graph = hydra.scoping.extendGraphForTypeLambda(tx2)(v_Term_typeLambda_tl)
          arityOf(txt)(v_Term_typeLambda_tl.body)
        }
        case hydra.core.Term.variable(v_Term_variable_name) => hydra.lib.maybes.maybe[Either[hydra.errors.Error,
           Int], hydra.core.Type](hydra.lib.eithers.map[Tuple2[hydra.core.Type, hydra.context.Context],
           Int, hydra.errors.Error]((_tc: Tuple2[hydra.core.Type, hydra.context.Context]) =>
          hydra.arity.typeArity(hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](_tc)))(hydra.checking.typeOf(cx)(tx2)(Seq())(hydra.core.Term.variable(v_Term_variable_name))))((t: hydra.core.Type) => Right(hydra.arity.typeArity(t)))(hydra.lib.maybes.map[hydra.core.TypeScheme,
             hydra.core.Type](hydra.scoping.typeSchemeToFType)(hydra.lib.maps.lookup[hydra.core.Name,
             hydra.core.TypeScheme](v_Term_variable_name)(tx2.boundTypes)))
        case _ => dflt
    }
    def extraVariables(n: Int): Seq[hydra.core.Name] =
      hydra.lib.lists.map[Int, hydra.core.Name]((i: Int) => hydra.lib.strings.cat2("v")(hydra.lib.literals.showInt32(i)))(hydra.lib.math.range(1)(n))
    def pad(vars: Seq[hydra.core.Name])(body: hydra.core.Term): hydra.core.Term =
      hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.lists.`null`[hydra.core.Name](vars))(body)(hydra.core.Term.lambda(hydra.core.Lambda(hydra.lib.lists.head[hydra.core.Name](vars),
         None, pad(hydra.lib.lists.tail[hydra.core.Name](vars))(hydra.core.Term.application(hydra.core.Application(body,
         hydra.core.Term.variable(hydra.lib.lists.head[hydra.core.Name](vars))))))))
    def padn(n: Int)(body: hydra.core.Term): hydra.core.Term = pad(extraVariables(n))(body)
    def unwind(term2: hydra.core.Term): hydra.core.Term =
      hydra.lib.lists.foldl[hydra.core.Term, hydra.core.Type]((e: hydra.core.Term) =>
      (t: hydra.core.Type) =>
      hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(e, t)))(term2)(typeArgs)
    def forceExpansion(t: hydra.core.Term): Either[hydra.errors.Error, hydra.core.Term] =
      hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[hydra.core.Type, hydra.context.Context],
         hydra.core.Term](hydra.checking.typeOf(cx)(tx)(Seq())(t))((typCx: Tuple2[hydra.core.Type,
         hydra.context.Context]) =>
      {
      lazy val arity: Int = hydra.arity.typeArity(hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](typCx))
      Right(padn(arity)(unwind(t)))
    })
    def recurseOrForce(term2: hydra.core.Term): Either[hydra.errors.Error, hydra.core.Term] =
      hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.core.Term]](forced)(forceExpansion(term2))(recurse(tx)(unwind(term2)))
    def forCase(f: hydra.core.Field): Either[hydra.errors.Error, hydra.core.Field] =
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.Field](rewrite(false)(true)(Seq())(recurse)(tx)(f.term))((r: hydra.core.Term) => Right(hydra.core.Field(f.name,
         r)))
    def forCaseStatement(cs: hydra.core.CaseStatement): Either[hydra.errors.Error, hydra.core.Term] =
      {
      lazy val tname: hydra.core.Name = (cs.typeName)
      lazy val dflt: Option[hydra.core.Term] = (cs.default)
      lazy val csCases: Seq[hydra.core.Field] = (cs.cases)
      hydra.lib.eithers.bind[hydra.errors.Error, Option[hydra.core.Term], hydra.core.Term](hydra.lib.eithers.mapMaybe[hydra.core.Term,
         hydra.core.Term, hydra.errors.Error]((v1: hydra.core.Term) => rewrite(false)(false)(Seq())(recurse)(tx)(v1))(dflt))((rdflt: Option[hydra.core.Term]) =>
        hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Field], hydra.core.Term](hydra.lib.eithers.mapList[hydra.core.Field,
           hydra.core.Field, hydra.errors.Error](forCase)(csCases))((rcases: Seq[hydra.core.Field]) =>
        Right(hydra.core.Term.cases(hydra.core.CaseStatement(tname, rdflt, rcases)))))
    }
    def forCases(cs: hydra.core.CaseStatement): Either[hydra.errors.Error, hydra.core.Term] =
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.Term](hydra.lib.eithers.map[hydra.core.Term,
         hydra.core.Term, hydra.errors.Error](unwind)(forCaseStatement(cs)))((base: hydra.core.Term) =>
      Right(hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.logic.or(topLevel)(forced))(padn(1)(base))(base)))
    def forNullaryElim(elimTerm: hydra.core.Term): hydra.core.Term =
      {
      lazy val base: hydra.core.Term = unwind(elimTerm)
      hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.logic.or(topLevel)(forced))(padn(1)(base))(base)
    }
    term match
      case hydra.core.Term.application(v_Term_application_a) => {
        lazy val lhs: hydra.core.Term = (v_Term_application_a.function)
        {
          lazy val rhs: hydra.core.Term = (v_Term_application_a.argument)
          hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.Term](rewrite(true)(false)(Seq())(recurse)(tx)(rhs))((rhs2: hydra.core.Term) =>
            hydra.lib.eithers.bind[hydra.errors.Error, Int, hydra.core.Term](arityOf(tx)(lhs))((lhsarity: Int) =>
            hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.Term](rewriteSpine(lhs))((lhs2: hydra.core.Term) =>
            {
            lazy val a2: hydra.core.Term = hydra.core.Term.application(hydra.core.Application(lhs2, rhs2))
            Right(hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.gt[Int](lhsarity)(1))(padn(hydra.lib.math.sub(lhsarity)(1))(a2))(a2))
          })))
        }
      }
      case hydra.core.Term.cases(v_Term_cases_cs) => forCases(v_Term_cases_cs)
      case hydra.core.Term.project(v_Term_project_p) => Right(forNullaryElim(hydra.core.Term.project(v_Term_project_p)))
      case hydra.core.Term.unwrap(v_Term_unwrap_n) => Right(forNullaryElim(hydra.core.Term.unwrap(v_Term_unwrap_n)))
      case hydra.core.Term.lambda(v_Term_lambda_l) => {
        lazy val txl: hydra.graph.Graph = hydra.scoping.extendGraphForLambda(tx)(v_Term_lambda_l)
        hydra.lib.eithers.map[hydra.core.Term, hydra.core.Term, hydra.errors.Error](unwind)(recurse(txl)(term))
      }
      case hydra.core.Term.let(v_Term_let_l) => {
        lazy val txlt: hydra.graph.Graph = hydra.scoping.extendGraphForLet((_x: hydra.graph.Graph) => (_2: hydra.core.Binding) => None)(tx)(v_Term_let_l)
        recurse(txlt)(term)
      }
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tat) => rewrite(topLevel)(forced)(hydra.lib.lists.cons[hydra.core.Type](v_Term_typeApplication_tat.`type`)(typeArgs))(recurse)(tx)(v_Term_typeApplication_tat.body)
      case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => {
        lazy val txt: hydra.graph.Graph = hydra.scoping.extendGraphForTypeLambda(tx)(v_Term_typeLambda_tl)
        recurse(txt)(term)
      }
      case _ => recurseOrForce(term)
  }
  hydra.rewriting.rewriteTermWithContextM((v1: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.Error,
     hydra.core.Term])) =>
    (v2: hydra.graph.Graph) =>
    (v3: hydra.core.Term) => rewrite(true)(false)(Seq())(v1)(v2)(v3))(tx0)(term0)
}

def etaExpansionArity(graph: hydra.graph.Graph)(term: hydra.core.Term): Int =
  term match
  case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.reduction.etaExpansionArity(graph)(v_Term_annotated_at.body)
  case hydra.core.Term.application(v_Term_application_app) => hydra.lib.math.sub(hydra.reduction.etaExpansionArity(graph)(v_Term_application_app.function))(1)
  case hydra.core.Term.cases(v_Term_cases__) => 1
  case hydra.core.Term.lambda(v_Term_lambda__) => 0
  case hydra.core.Term.project(v_Term_project__) => 1
  case hydra.core.Term.unwrap(v_Term_unwrap__) => 1
  case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => hydra.reduction.etaExpansionArity(graph)(v_Term_typeLambda_ta.body)
  case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => hydra.reduction.etaExpansionArity(graph)(v_Term_typeApplication_tt.body)
  case hydra.core.Term.variable(v_Term_variable_name) => hydra.lib.maybes.maybe[Int,
     hydra.core.TypeScheme](0)((ts: hydra.core.TypeScheme) => hydra.arity.typeArity(ts.`type`))(hydra.lib.maybes.bind[hydra.core.Binding,
     hydra.core.TypeScheme](hydra.lexical.lookupBinding(graph)(v_Term_variable_name))((b: hydra.core.Binding) => (b.`type`)))
  case _ => 0

def etaReduceTerm(term: hydra.core.Term): hydra.core.Term =
  {
  lazy val noChange: hydra.core.Term = term
  def reduceLambda(l: hydra.core.Lambda): hydra.core.Term =
    {
    lazy val v: hydra.core.Name = (l.parameter)
    lazy val d: Option[hydra.core.Type] = (l.domain)
    lazy val body: hydra.core.Term = (l.body)
    hydra.reduction.etaReduceTerm(body) match
      case hydra.core.Term.annotated(v_Term_annotated_at) => reduceLambda(hydra.core.Lambda(v,
         d, (v_Term_annotated_at.body)))
      case hydra.core.Term.application(v_Term_application_app) => {
        lazy val lhs: hydra.core.Term = (v_Term_application_app.function)
        {
          lazy val rhs: hydra.core.Term = (v_Term_application_app.argument)
          hydra.reduction.etaReduceTerm(rhs) match
            case hydra.core.Term.annotated(v_Term_annotated_at) => reduceLambda(hydra.core.Lambda(v,
               d, hydra.core.Term.application(hydra.core.Application(lhs, (v_Term_annotated_at.body)))))
            case hydra.core.Term.variable(v_Term_variable_v1) => hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.logic.and(hydra.lib.equality.equal[scala.Predef.String](v)(v_Term_variable_v1))(hydra.lib.logic.not(hydra.variables.isFreeVariableInTerm(v)(lhs))))(hydra.reduction.etaReduceTerm(lhs))(noChange)
            case _ => noChange
        }
      }
      case _ => noChange
  }
  term match
    case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.reduction.etaReduceTerm(v_Term_annotated_at.body),
       (v_Term_annotated_at.annotation)))
    case hydra.core.Term.lambda(v_Term_lambda_l) => reduceLambda(v_Term_lambda_l)
    case _ => noChange
}

def reduceTerm(cx: hydra.context.Context)(graph: hydra.graph.Graph)(eager: Boolean)(term: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  {
  def reduce(eager2: Boolean)(v1: hydra.core.Term): Either[hydra.errors.Error, hydra.core.Term] = hydra.reduction.reduceTerm(cx)(graph)(eager2)(v1)
  def doRecurse(eager2: Boolean)(term2: hydra.core.Term): Boolean =
    {
    lazy val isNonLambdaTerm: Boolean = term2 match
      case hydra.core.Term.lambda(v_Term_lambda__) => false
      case hydra.core.Term.let(v_Term_let__) => false
      case _ => true
    hydra.lib.logic.and(eager2)(isNonLambdaTerm)
  }
  def reduceArg(eager2: Boolean)(arg: hydra.core.Term): Either[hydra.errors.Error, hydra.core.Term] =
    hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.core.Term]](eager2)(Right(arg))(reduce(false)(arg))
  def applyToArguments(fun: hydra.core.Term)(args: Seq[hydra.core.Term]): hydra.core.Term =
    hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.lists.`null`[hydra.core.Term](args))(fun)(applyToArguments(hydra.core.Term.application(hydra.core.Application(fun,
       hydra.lib.lists.head[hydra.core.Term](args))))(hydra.lib.lists.tail[hydra.core.Term](args)))
  def mapErrorToString(e: hydra.errors.Error): hydra.errors.Error = hydra.errors.Error.other(hydra.show.errors.error(e))
  def applyProjection(proj: hydra.core.Projection)(reducedArg: hydra.core.Term): Either[hydra.errors.Error,
     hydra.core.Term] =
    hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Field], hydra.core.Term](hydra.extract.core.record(proj.typeName)(graph)(hydra.strip.deannotateTerm(reducedArg)))((fields: Seq[hydra.core.Field]) =>
    {
    lazy val matchingFields: Seq[hydra.core.Field] = hydra.lib.lists.filter[hydra.core.Field]((f: hydra.core.Field) =>
      hydra.lib.equality.equal[hydra.core.Name](f.name)(proj.field))(fields)
    hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.core.Term]](hydra.lib.lists.`null`[hydra.core.Field](matchingFields))(Left(hydra.errors.Error.resolution(hydra.errors.ResolutionError.noMatchingField(hydra.errors.NoMatchingFieldError(proj.field)))))(Right(hydra.lib.lists.head[hydra.core.Field](matchingFields).term))
  })
  def applyCases(cs: hydra.core.CaseStatement)(reducedArg: hydra.core.Term): Either[hydra.errors.Error,
     hydra.core.Term] =
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Field, hydra.core.Term](hydra.extract.core.injection(cs.typeName)(graph)(reducedArg))((field: hydra.core.Field) =>
    {
    lazy val matchingFields: Seq[hydra.core.Field] = hydra.lib.lists.filter[hydra.core.Field]((f: hydra.core.Field) =>
      hydra.lib.equality.equal[hydra.core.Name](f.name)(field.name))(cs.cases)
    hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.core.Term]](hydra.lib.lists.`null`[hydra.core.Field](matchingFields))(hydra.lib.maybes.maybe[Either[hydra.errors.Error,
       hydra.core.Term], hydra.core.Term](Left(hydra.errors.Error.resolution(hydra.errors.ResolutionError.noMatchingField(hydra.errors.NoMatchingFieldError(field.name)))))((x: hydra.core.Term) => Right(x))(cs.default))(Right(hydra.core.Term.application(hydra.core.Application(hydra.lib.lists.head[hydra.core.Field](matchingFields).term,
       (field.term)))))
  })
  def applyIfNullary(eager2: Boolean)(original: hydra.core.Term)(args: Seq[hydra.core.Term]): Either[hydra.errors.Error,
     hydra.core.Term] =
    {
    lazy val stripped: hydra.core.Term = hydra.strip.deannotateTerm(original)
    def forProjection(proj: hydra.core.Projection)(args2: Seq[hydra.core.Term]): Either[hydra.errors.Error,
       hydra.core.Term] =
      {
      lazy val arg: hydra.core.Term = hydra.lib.lists.head[hydra.core.Term](args2)
      lazy val remainingArgs: Seq[hydra.core.Term] = hydra.lib.lists.tail[hydra.core.Term](args2)
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.Term](reduceArg(eager2)(hydra.strip.deannotateTerm(arg)))((reducedArg: hydra.core.Term) =>
        hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.Term](hydra.lib.eithers.bind[hydra.errors.Error,
           hydra.core.Term, hydra.core.Term](applyProjection(proj)(reducedArg))((v1: hydra.core.Term) => reduce(eager2)(v1)))((reducedResult: hydra.core.Term) => applyIfNullary(eager2)(reducedResult)(remainingArgs)))
    }
    def forCases(cs: hydra.core.CaseStatement)(args2: Seq[hydra.core.Term]): Either[hydra.errors.Error,
       hydra.core.Term] =
      {
      lazy val arg: hydra.core.Term = hydra.lib.lists.head[hydra.core.Term](args2)
      lazy val remainingArgs: Seq[hydra.core.Term] = hydra.lib.lists.tail[hydra.core.Term](args2)
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.Term](reduceArg(eager2)(hydra.strip.deannotateTerm(arg)))((reducedArg: hydra.core.Term) =>
        hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.Term](hydra.lib.eithers.bind[hydra.errors.Error,
           hydra.core.Term, hydra.core.Term](applyCases(cs)(reducedArg))((v1: hydra.core.Term) => reduce(eager2)(v1)))((reducedResult: hydra.core.Term) => applyIfNullary(eager2)(reducedResult)(remainingArgs)))
    }
    def forUnwrap(name: hydra.core.Name)(args2: Seq[hydra.core.Term]): Either[hydra.errors.Error, hydra.core.Term] =
      {
      lazy val arg: hydra.core.Term = hydra.lib.lists.head[hydra.core.Term](args2)
      lazy val remainingArgs: Seq[hydra.core.Term] = hydra.lib.lists.tail[hydra.core.Term](args2)
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.Term](reduceArg(eager2)(hydra.strip.deannotateTerm(arg)))((reducedArg: hydra.core.Term) =>
        hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.Term](hydra.lib.eithers.bind[hydra.errors.Error,
           hydra.core.Term, hydra.core.Term](hydra.extract.core.wrap(name)(graph)(reducedArg))((v1: hydra.core.Term) => reduce(eager2)(v1)))((reducedResult: hydra.core.Term) => applyIfNullary(eager2)(reducedResult)(remainingArgs)))
    }
    def forLambda(l: hydra.core.Lambda)(args2: Seq[hydra.core.Term]): Either[hydra.errors.Error, hydra.core.Term] =
      {
      lazy val param: hydra.core.Name = (l.parameter)
      lazy val body: hydra.core.Term = (l.body)
      lazy val arg: hydra.core.Term = hydra.lib.lists.head[hydra.core.Term](args2)
      lazy val remainingArgs: Seq[hydra.core.Term] = hydra.lib.lists.tail[hydra.core.Term](args2)
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.Term](reduce(eager2)(hydra.strip.deannotateTerm(arg)))((reducedArg: hydra.core.Term) =>
        hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.Term](reduce(eager2)(hydra.variables.replaceFreeTermVariable(param)(reducedArg)(body)))((reducedResult: hydra.core.Term) => applyIfNullary(eager2)(reducedResult)(remainingArgs)))
    }
    def forPrimitive(prim: hydra.graph.Primitive)(arity: Int)(args2: Seq[hydra.core.Term]): Either[hydra.errors.Error,
       hydra.core.Term] =
      {
      lazy val argList: Seq[hydra.core.Term] = hydra.lib.lists.take[hydra.core.Term](arity)(args2)
      lazy val remainingArgs: Seq[hydra.core.Term] = hydra.lib.lists.drop[hydra.core.Term](arity)(args2)
      hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], hydra.core.Term](hydra.lib.eithers.mapList[hydra.core.Term,
         hydra.core.Term, hydra.errors.Error]((v1: hydra.core.Term) => reduceArg(eager2)(v1))(argList))((reducedArgs: Seq[hydra.core.Term]) =>
        {
        lazy val strippedArgs: Seq[hydra.core.Term] = hydra.lib.lists.map[hydra.core.Term,
           hydra.core.Term](hydra.strip.deannotateTerm)(reducedArgs)
        hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.Term](hydra.lib.eithers.bimap[hydra.errors.Error,
           hydra.core.Term, hydra.errors.Error, hydra.core.Term](mapErrorToString)((x: hydra.core.Term) => x)(prim.implementation(cx)(graph)(strippedArgs)))((primResult: hydra.core.Term) =>
          hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.Term](reduce(eager2)(primResult))((reducedResult: hydra.core.Term) => applyIfNullary(eager2)(reducedResult)(remainingArgs)))
      })
    }
    stripped match
      case hydra.core.Term.application(v_Term_application_app) => applyIfNullary(eager2)(v_Term_application_app.function)(hydra.lib.lists.cons[hydra.core.Term](v_Term_application_app.argument)(args))
      case hydra.core.Term.cases(v_Term_cases_cs) => hydra.lib.logic.ifElse[Either[hydra.errors.Error,
         hydra.core.Term]](hydra.lib.lists.`null`[hydra.core.Term](args))(Right(original))(forCases(v_Term_cases_cs)(args))
      case hydra.core.Term.project(v_Term_project_p) => hydra.lib.logic.ifElse[Either[hydra.errors.Error,
         hydra.core.Term]](hydra.lib.lists.`null`[hydra.core.Term](args))(Right(original))(forProjection(v_Term_project_p)(args))
      case hydra.core.Term.unwrap(v_Term_unwrap_n) => hydra.lib.logic.ifElse[Either[hydra.errors.Error,
         hydra.core.Term]](hydra.lib.lists.`null`[hydra.core.Term](args))(Right(original))(forUnwrap(v_Term_unwrap_n)(args))
      case hydra.core.Term.lambda(v_Term_lambda_l) => hydra.lib.logic.ifElse[Either[hydra.errors.Error,
         hydra.core.Term]](hydra.lib.lists.`null`[hydra.core.Term](args))(Right(original))(forLambda(v_Term_lambda_l)(args))
      case hydra.core.Term.variable(v_Term_variable_v) => {
        lazy val mBinding: Option[hydra.core.Binding] = hydra.lexical.lookupBinding(graph)(v_Term_variable_v)
        hydra.lib.maybes.maybe[Either[hydra.errors.Error, hydra.core.Term], hydra.core.Binding]({
          lazy val mPrim: Option[hydra.graph.Primitive] = hydra.lexical.lookupPrimitive(graph)(v_Term_variable_v)
          hydra.lib.maybes.maybe[Either[hydra.errors.Error, hydra.core.Term], hydra.graph.Primitive](Right(applyToArguments(original)(args)))((prim: hydra.graph.Primitive) =>
            {
            lazy val arity: Int = hydra.arity.primitiveArity(prim)
            hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.core.Term]](hydra.lib.equality.gt[Int](arity)(hydra.lib.lists.length[hydra.core.Term](args)))(Right(applyToArguments(original)(args)))(forPrimitive(prim)(arity)(args))
          })(mPrim)
        })((binding: hydra.core.Binding) => applyIfNullary(eager2)(binding.term)(args))(mBinding)
      }
      case hydra.core.Term.let(v_Term_let_lt) => {
        lazy val bindings: Seq[hydra.core.Binding] = (v_Term_let_lt.bindings)
        {
          lazy val body: hydra.core.Term = (v_Term_let_lt.body)
          {
            def letExpr(b: hydra.core.Binding): hydra.core.Term = hydra.core.Term.let(hydra.core.Let(Seq(b),
               hydra.core.Term.variable(b.name)))
            {
              def expandBinding(b: hydra.core.Binding): hydra.core.Binding =
                hydra.core.Binding(b.name, hydra.variables.replaceFreeTermVariable(b.name)(letExpr(b))(b.term),
                   (b.`type`))
              {
                lazy val expandedBindings: Seq[hydra.core.Binding] = hydra.lib.lists.map[hydra.core.Binding,
                   hydra.core.Binding](expandBinding)(bindings)
                {
                  def substituteBinding(term2: hydra.core.Term)(b: hydra.core.Binding): hydra.core.Term = hydra.variables.replaceFreeTermVariable(b.name)(b.term)(term2)
                  {
                    def substituteAll(bs: Seq[hydra.core.Binding])(term2: hydra.core.Term): hydra.core.Term =
                      hydra.lib.lists.foldl[hydra.core.Term, hydra.core.Binding](substituteBinding)(term2)(bs)
                    {
                      lazy val expandedBody: hydra.core.Term = substituteAll(expandedBindings)(body)
                      hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term,
                         hydra.core.Term](reduce(eager2)(expandedBody))((reducedBody: hydra.core.Term) => applyIfNullary(eager2)(reducedBody)(args))
                    }
                  }
                }
              }
            }
          }
        }
      }
      case _ => Right(applyToArguments(original)(args))
  }
  def mapping(recurse: (hydra.core.Term => Either[hydra.errors.Error, hydra.core.Term]))(mid: hydra.core.Term): Either[hydra.errors.Error,
     hydra.core.Term] =
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.Term](hydra.lib.logic.ifElse[Either[hydra.errors.Error,
       hydra.core.Term]](doRecurse(eager)(mid))(recurse(mid))(Right(mid)))((inner: hydra.core.Term) => applyIfNullary(eager)(inner)(Seq()))
  hydra.rewriting.rewriteTermM(mapping)(term)
}

def termIsClosed(term: hydra.core.Term): Boolean =
  hydra.lib.sets.`null`[hydra.core.Name](hydra.variables.freeVariablesInTerm(term))

def termIsValue(term: hydra.core.Term): Boolean =
  {
  def forList(els: Seq[hydra.core.Term]): Boolean =
    hydra.lib.lists.foldl[Boolean, hydra.core.Term]((b: Boolean) =>
    (t: hydra.core.Term) => hydra.lib.logic.and(b)(hydra.reduction.termIsValue(t)))(true)(els)
  def checkField(f: hydra.core.Field): Boolean = hydra.reduction.termIsValue(f.term)
  def checkFields(fields: Seq[hydra.core.Field]): Boolean =
    hydra.lib.lists.foldl[Boolean, hydra.core.Field]((b: Boolean) =>
    (f: hydra.core.Field) => hydra.lib.logic.and(b)(checkField(f)))(true)(fields)
  hydra.strip.deannotateTerm(term) match
    case hydra.core.Term.application(v_Term_application__) => false
    case hydra.core.Term.cases(v_Term_cases_cs) => hydra.lib.logic.and(checkFields(v_Term_cases_cs.cases))(hydra.lib.maybes.maybe[Boolean,
       hydra.core.Term](true)(hydra.reduction.termIsValue)(v_Term_cases_cs.default))
    case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.either[hydra.core.Term,
       hydra.core.Term, Boolean]((l: hydra.core.Term) => hydra.reduction.termIsValue(l))((r: hydra.core.Term) => hydra.reduction.termIsValue(r))(v_Term_either_e)
    case hydra.core.Term.lambda(v_Term_lambda_l) => hydra.reduction.termIsValue(v_Term_lambda_l.body)
    case hydra.core.Term.literal(v_Term_literal__) => true
    case hydra.core.Term.project(v_Term_project__) => true
    case hydra.core.Term.unwrap(v_Term_unwrap__) => true
    case hydra.core.Term.list(v_Term_list_els) => forList(v_Term_list_els)
    case hydra.core.Term.map(v_Term_map_m) => hydra.lib.lists.foldl[Boolean, Tuple2[hydra.core.Term,
       hydra.core.Term]]((b: Boolean) =>
      (kv: Tuple2[hydra.core.Term, hydra.core.Term]) =>
      hydra.lib.logic.and(b)(hydra.lib.logic.and(hydra.reduction.termIsValue(hydra.lib.pairs.first[hydra.core.Term,
         hydra.core.Term](kv)))(hydra.reduction.termIsValue(hydra.lib.pairs.second[hydra.core.Term,
         hydra.core.Term](kv)))))(true)(hydra.lib.maps.toList[hydra.core.Term, hydra.core.Term](v_Term_map_m))
    case hydra.core.Term.maybe(v_Term_maybe_m) => hydra.lib.maybes.maybe[Boolean,
       hydra.core.Term](true)(hydra.reduction.termIsValue)(v_Term_maybe_m)
    case hydra.core.Term.record(v_Term_record_r) => checkFields(v_Term_record_r.fields)
    case hydra.core.Term.set(v_Term_set_s) => forList(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_s))
    case hydra.core.Term.inject(v_Term_inject_i) => checkField(v_Term_inject_i.field)
    case hydra.core.Term.unit => true
    case hydra.core.Term.variable(v_Term_variable__) => false
    case _ => false
}
