package hydra.reduction

import hydra.context.*

import hydra.core.*

import hydra.error.*

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

def alphaConvert(vold: hydra.core.Name)(vnew: hydra.core.Name)(term: hydra.core.Term): hydra.core.Term =
  hydra.rewriting.replaceFreeTermVariable(vold)(hydra.core.Term.variable(vnew))(term)

def betaReduceType(cx: hydra.context.Context)(graph: hydra.graph.Graph)(typ: hydra.core.Type): Either[hydra.context.InContext[hydra.error.Error],
   hydra.core.Type] =
  {
  def reduceApp(app: hydra.core.ApplicationType): Either[hydra.context.InContext[hydra.error.Error], hydra.core.Type] =
    {
    val lhs: hydra.core.Type = (app.function)
    val rhs: hydra.core.Type = (app.argument)
    lhs match
      case hydra.core.Type.annotated(v_Type_annotated_at) => eithers.bind[hydra.context.InContext[hydra.error.Error],
         hydra.core.Type, hydra.core.Type](reduceApp(hydra.core.ApplicationType(v_Type_annotated_at.body,
         rhs)))((a: hydra.core.Type) =>
        Right(hydra.core.Type.annotated(hydra.core.AnnotatedType(a, (v_Type_annotated_at.annotation)))))
      case hydra.core.Type.forall(v_Type_forall_ft) => hydra.reduction.betaReduceType(cx)(graph)(hydra.rewriting.replaceFreeTypeVariable(v_Type_forall_ft.parameter)(rhs)(v_Type_forall_ft.body))
      case hydra.core.Type.variable(v_Type_variable_name) => eithers.bind[hydra.context.InContext[hydra.error.Error],
         hydra.core.Type, hydra.core.Type](hydra.schemas.requireType(cx)(graph)(v_Type_variable_name))((`t_`: hydra.core.Type) =>
        hydra.reduction.betaReduceType(cx)(graph)(hydra.core.Type.application(hydra.core.ApplicationType(`t_`, rhs))))
  }
  def mapExpr[T0](recurse: (T0 => Either[hydra.context.InContext[hydra.error.Error], hydra.core.Type]))(t: T0): Either[hydra.context.InContext[hydra.error.Error],
     hydra.core.Type] =
    {
    def findApp(r: hydra.core.Type): Either[hydra.context.InContext[hydra.error.Error], hydra.core.Type] =
      r match
      case hydra.core.Type.application(v_Type_application_a) => reduceApp(v_Type_application_a)
      case _ => Right(r)
    eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, hydra.core.Type](recurse(t))((r: hydra.core.Type) => findApp(r))
  }
  hydra.rewriting.rewriteTypeM(mapExpr)(typ)
}

def contractTerm(term: hydra.core.Term): hydra.core.Term =
  {
  def rewrite[T0](recurse: (T0 => hydra.core.Term))(t: T0): hydra.core.Term =
    {
    val rec: hydra.core.Term = recurse(t)
    rec match
      case hydra.core.Term.application(v_Term_application_app) => {
        val lhs: hydra.core.Term = (v_Term_application_app.function)
        {
          val rhs: hydra.core.Term = (v_Term_application_app.argument)
          hydra.rewriting.deannotateTerm(lhs) match
            case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
              case hydra.core.Function.lambda(v_Function_lambda_l) => {
                val v: hydra.core.Name = (v_Function_lambda_l.parameter)
                {
                  val body: hydra.core.Term = (v_Function_lambda_l.body)
                  logic.ifElse[hydra.core.Term](hydra.rewriting.isFreeVariableInTerm(v)(body))(body)(hydra.rewriting.replaceFreeTermVariable(v)(rhs)(body))
                }
              }
              case _ => rec
            case _ => rec
        }
      }
      case _ => rec
  }
  hydra.rewriting.rewriteTerm(rewrite)(term)
}

val countPrimitiveInvocations: Boolean = true

def etaReduceTerm(term: hydra.core.Term): hydra.core.Term =
  {
  val noChange: hydra.core.Term = term
  def reduceLambda(l: hydra.core.Lambda): hydra.core.Term =
    {
    val v: hydra.core.Name = (l.parameter)
    val d: Option[hydra.core.Type] = (l.domain)
    val body: hydra.core.Term = (l.body)
    hydra.reduction.etaReduceTerm(body) match
      case hydra.core.Term.annotated(v_Term_annotated_at) => reduceLambda(hydra.core.Lambda(v, d, (v_Term_annotated_at.body)))
      case hydra.core.Term.application(v_Term_application_app) => {
        val lhs: hydra.core.Term = (v_Term_application_app.function)
        {
          val rhs: hydra.core.Term = (v_Term_application_app.argument)
          hydra.reduction.etaReduceTerm(rhs) match
            case hydra.core.Term.annotated(v_Term_annotated_at) => reduceLambda(hydra.core.Lambda(v, d,
               hydra.core.Term.application(hydra.core.Application(lhs, (v_Term_annotated_at.body)))))
            case hydra.core.Term.variable(v_Term_variable_v1) => logic.ifElse[hydra.core.Term](logic.and(equality.equal[scala.Predef.String](v)(v_Term_variable_v1))(logic.not(hydra.rewriting.isFreeVariableInTerm(v)(lhs))))(hydra.reduction.etaReduceTerm(lhs))(noChange)
            case _ => noChange
        }
      }
      case _ => noChange
  }
  term match
    case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.reduction.etaReduceTerm(v_Term_annotated_at.body),
       (v_Term_annotated_at.annotation)))
    case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
      case hydra.core.Function.lambda(v_Function_lambda_l) => reduceLambda(v_Function_lambda_l)
      case _ => noChange
    case _ => noChange
}

def etaExpandTerm(graph: hydra.graph.Graph)(term: hydra.core.Term): hydra.core.Term =
  {
  def expand(args: Seq[hydra.core.Term])(arity: Int)(t: hydra.core.Term): hydra.core.Term =
    {
    val apps: hydra.core.Term = lists.foldl[hydra.core.Term, hydra.core.Term]((lhs: hydra.core.Term) =>
      (arg: hydra.core.Term) =>
      hydra.core.Term.application(hydra.core.Application(lhs, arg)))(t)(args)
    val is: Seq[Int] = logic.ifElse[Seq[Int]](equality.lte[Int](arity)(lists.length[hydra.core.Term](args)))(Seq())(math.range(1)(math.sub(arity)(lists.length[hydra.core.Term](args))))
    def pad(indices: Seq[Int])(t2: hydra.core.Term): hydra.core.Term =
      logic.ifElse[hydra.core.Term](lists.`null`[Int](indices))(t2)(hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda(strings.cat2("v")(literals.showInt32(lists.head[Int](indices))),
         None, pad(lists.tail[Int](indices))(hydra.core.Term.application(hydra.core.Application(t2, hydra.core.Term.variable(strings.cat2("v")(literals.showInt32(lists.head[Int](indices)))))))))))
    pad(is)(apps)
  }
  def rewrite(args: Seq[hydra.core.Term])(recurse: (hydra.core.Term => hydra.core.Term))(t: hydra.core.Term): hydra.core.Term =
    {
    def afterRecursion(term2: hydra.core.Term): hydra.core.Term = expand(args)(hydra.reduction.etaExpansionArity(graph)(term2))(term2)
    val t2: hydra.core.Term = hydra.rewriting.detypeTerm(t)
    t2 match
      case hydra.core.Term.application(v_Term_application_app) => {
        val lhs: hydra.core.Term = (v_Term_application_app.function)
        {
          val rhs: hydra.core.Term = (v_Term_application_app.argument)
          {
            val erhs: hydra.core.Term = rewrite(Seq())(recurse)(rhs)
            rewrite(lists.cons[hydra.core.Term](erhs)(args))(recurse)(lhs)
          }
        }
      }
      case _ => afterRecursion(recurse(t2))
  }
  hydra.reduction.contractTerm(hydra.rewriting.rewriteTerm((v1: (hydra.core.Term => hydra.core.Term)) => (v2: hydra.core.Term) => rewrite(Seq())(v1)(v2))(term))
}

def etaExpandTermNew(tx0: hydra.graph.Graph)(term0: hydra.core.Term): hydra.core.Term =
  {
  val primTypes: Map[hydra.core.Name, hydra.core.TypeScheme] = maps.fromList[hydra.core.Name, hydra.core.TypeScheme](lists.map[hydra.graph.Primitive,
     Tuple2[hydra.core.Name, hydra.core.TypeScheme]]((_gpt_p: hydra.graph.Primitive) => Tuple2(_gpt_p.name,
     (_gpt_p.`type`)))(maps.elems[hydra.core.Name, hydra.graph.Primitive](tx0.primitives)))
  def termArityWithContext(tx: hydra.graph.Graph)(term: hydra.core.Term): Int =
    term match
    case hydra.core.Term.annotated(v_Term_annotated_at) => termArityWithContext(tx)(v_Term_annotated_at.body)
    case hydra.core.Term.application(v_Term_application_app) => math.sub(termArityWithContext(tx)(v_Term_application_app.function))(1)
    case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
      case hydra.core.Function.elimination(v_Function_elimination__) => 1
      case hydra.core.Function.lambda(v_Function_lambda__) => 0
      case hydra.core.Function.primitive(v_Function_primitive_name) => maybes.maybe[Int, hydra.core.TypeScheme](0)(hydra.arity.typeSchemeArity)(maps.lookup[hydra.core.Name,
         hydra.core.TypeScheme](v_Function_primitive_name)(primTypes))
    case hydra.core.Term.let(v_Term_let_l) => termArityWithContext(hydra.schemas.extendGraphForLet((_x: hydra.graph.Graph) => (_2: hydra.core.Binding) => None)(tx)(v_Term_let_l))(v_Term_let_l.body)
    case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => termArityWithContext(hydra.schemas.extendGraphForTypeLambda(tx)(v_Term_typeLambda_tl))(v_Term_typeLambda_tl.body)
    case hydra.core.Term.typeApplication(v_Term_typeApplication_tat) => termArityWithContext(tx)(v_Term_typeApplication_tat.body)
    case hydra.core.Term.variable(v_Term_variable_name) => maybes.maybe[Int, hydra.core.Type](0)(hydra.arity.typeArity)(maybes.map[hydra.core.TypeScheme,
       hydra.core.Type](hydra.rewriting.typeSchemeToFType)(maps.lookup[hydra.core.Name, hydra.core.TypeScheme](v_Term_variable_name)(tx.boundTypes)))
    case _ => 0
  def domainTypes(n: Int)(mt: Option[hydra.core.Type]): Seq[Option[hydra.core.Type]] =
    logic.ifElse[Seq[Option[hydra.core.Type]]](equality.lte[Int](n)(0))(Seq())(maybes.maybe[Seq[Option[hydra.core.Type]],
       hydra.core.Type](lists.map[Int, Option[hydra.core.Type]]((_x: Int) => None)(math.range(1)(n)))((typ: hydra.core.Type) =>
    typ match
    case hydra.core.Type.function(v_Type_function_ftyp) => lists.cons[Option[hydra.core.Type]](Some(v_Type_function_ftyp.domain))(domainTypes(math.sub(n)(1))(Some(v_Type_function_ftyp.codomain)))
    case hydra.core.Type.annotated(v_Type_annotated_at) => domainTypes(n)(Some(v_Type_annotated_at.body))
    case hydra.core.Type.application(v_Type_application_atyp) => domainTypes(n)(Some(v_Type_application_atyp.function))
    case hydra.core.Type.forall(v_Type_forall__) => lists.map[Int, Option[hydra.core.Type]]((_2: Int) => None)(math.range(1)(n))
    case _ => lists.map[Int, Option[hydra.core.Type]]((_x: Int) => None)(math.range(1)(n)))(mt))
  def peelFunctionDomains(mtyp: Option[hydra.core.Type])(n: Int): Option[hydra.core.Type] =
    logic.ifElse[Option[hydra.core.Type]](equality.lte[Int](n)(0))(mtyp)(maybes.maybe[Option[hydra.core.Type], hydra.core.Type](None)((typ: hydra.core.Type) =>
    typ match
    case hydra.core.Type.function(v_Type_function_ftyp) => peelFunctionDomains(Some(v_Type_function_ftyp.codomain))(math.sub(n)(1))
    case hydra.core.Type.annotated(v_Type_annotated_at) => peelFunctionDomains(Some(v_Type_annotated_at.body))(n)
    case hydra.core.Type.application(v_Type_application_atyp) => peelFunctionDomains(Some(v_Type_application_atyp.function))(n)
    case hydra.core.Type.forall(v_Type_forall__) => None
    case _ => None)(mtyp))
  def expand(alwaysPad: Boolean)(args: Seq[hydra.core.Term])(arity: Int)(headTyp: Option[hydra.core.Type])(head: hydra.core.Term): hydra.core.Term =
    {
    val applied: hydra.core.Term = lists.foldl[hydra.core.Term, hydra.core.Term]((lhs: hydra.core.Term) =>
      (arg: hydra.core.Term) =>
      hydra.core.Term.application(hydra.core.Application(lhs, arg)))(head)(args)
    val numArgs: Int = lists.length[hydra.core.Term](args)
    val needed: Int = math.sub(arity)(numArgs)
    logic.ifElse[hydra.core.Term](logic.and(equality.gt[Int](needed)(0))(logic.or(alwaysPad)(equality.gt[Int](numArgs)(0))))({
      val indices: Seq[Int] = math.range(1)(needed)
      {
        val remainingType: Option[hydra.core.Type] = peelFunctionDomains(headTyp)(numArgs)
        {
          val domains: Seq[Option[hydra.core.Type]] = domainTypes(needed)(remainingType)
          {
            val codomainType: Option[hydra.core.Type] = peelFunctionDomains(remainingType)(needed)
            {
              val fullyAppliedRaw: hydra.core.Term = lists.foldl[hydra.core.Term, Int]((body: hydra.core.Term) =>
                (i: Int) =>
                {
                val vn: hydra.core.Name = strings.cat2("v")(literals.showInt32(i))
                hydra.core.Term.application(hydra.core.Application(body, hydra.core.Term.variable(vn)))
              })(applied)(indices)
              {
                val fullyApplied: hydra.core.Term = maybes.maybe[hydra.core.Term, hydra.core.Type](fullyAppliedRaw)((ct: hydra.core.Type) =>
                  hydra.core.Term.annotated(hydra.core.AnnotatedTerm(fullyAppliedRaw, maps.singleton[hydra.core.Name,
                     hydra.core.Term]("type")(hydra.encode.core.`type`(ct)))))(codomainType)
                {
                  val indexedDomains: Seq[Tuple2[Int, Option[hydra.core.Type]]] = lists.zip[Int, Option[hydra.core.Type]](indices)(domains)
                  lists.foldl[hydra.core.Term, Tuple2[Int, Option[hydra.core.Type]]]((body: hydra.core.Term) =>
                    (idPair: Tuple2[Int, Option[hydra.core.Type]]) =>
                    {
                    val i: Int = pairs.first[Int, Option[hydra.core.Type]](idPair)
                    {
                      val dom: Option[hydra.core.Type] = pairs.second[Int, Option[hydra.core.Type]](idPair)
                      {
                        val vn: hydra.core.Name = strings.cat2("v")(literals.showInt32(i))
                        hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda(vn, dom, body)))
                      }
                    }
                  })(fullyApplied)(lists.reverse[Tuple2[Int, Option[hydra.core.Type]]](indexedDomains))
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
      case hydra.core.Term.function(v_Term_function_f2) => v_Term_function_f2 match
        case hydra.core.Function.primitive(v_Function_primitive_pn2) => maybes.map[hydra.core.TypeScheme,
           hydra.core.Type](hydra.rewriting.typeSchemeToFType)(maps.lookup[hydra.core.Name, hydra.core.TypeScheme](v_Function_primitive_pn2)(primTypes))
        case _ => None
      case hydra.core.Term.let(v_Term_let_l2) => termHeadType(hydra.schemas.extendGraphForLet((_x: hydra.graph.Graph) => (_2: hydra.core.Binding) => None)(tx2)(v_Term_let_l2))(v_Term_let_l2.body)
      case hydra.core.Term.typeLambda(v_Term_typeLambda_tl2) => termHeadType(hydra.schemas.extendGraphForTypeLambda(tx2)(v_Term_typeLambda_tl2))(v_Term_typeLambda_tl2.body)
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tat2) => maybes.bind[hydra.core.Type,
         hydra.core.Type](termHeadType(tx2)(v_Term_typeApplication_tat2.body))((htyp2: hydra.core.Type) =>
        htyp2 match
        case hydra.core.Type.forall(v_Type_forall_ft2) => Some(hydra.rewriting.replaceFreeTypeVariable(v_Type_forall_ft2.parameter)(v_Term_typeApplication_tat2.`type`)(v_Type_forall_ft2.body))
        case _ => Some(htyp2))
      case hydra.core.Term.variable(v_Term_variable_vn2) => maybes.map[hydra.core.TypeScheme, hydra.core.Type](hydra.rewriting.typeSchemeToFType)(maps.lookup[hydra.core.Name,
         hydra.core.TypeScheme](v_Term_variable_vn2)(tx2.boundTypes))
      case _ => None
    def afterRecursion(trm: hydra.core.Term): hydra.core.Term =
      {
      val arity: Int = termArityWithContext(tx)(trm)
      val hType: Option[hydra.core.Type] = termHeadType(tx)(trm)
      expand(false)(args)(arity)(hType)(trm)
    }
    def forField(f: hydra.core.Field): hydra.core.Field = hydra.core.Field(f.name, recurse(tx)(f.term))
    def forCaseBranch(f: hydra.core.Field): hydra.core.Field =
      {
      val branchBody: hydra.core.Term = recurse(tx)(f.term)
      val arty: Int = termArityWithContext(tx)(branchBody)
      val branchHType: Option[hydra.core.Type] = termHeadType(tx)(branchBody)
      hydra.core.Field(f.name, expand(true)(Seq())(arty)(branchHType)(branchBody))
    }
    def forElimination(elm: hydra.core.Elimination): hydra.core.Elimination =
      elm match
      case hydra.core.Elimination.record(v_Elimination_record_p) => hydra.core.Elimination.record(v_Elimination_record_p)
      case hydra.core.Elimination.union(v_Elimination_union_cs) => hydra.core.Elimination.union(hydra.core.CaseStatement(v_Elimination_union_cs.typeName,
         maybes.map[hydra.core.Term, hydra.core.Term]((t1: hydra.core.Term) => recurse(tx)(t1))(v_Elimination_union_cs.default),
         lists.map[hydra.core.Field, hydra.core.Field](forCaseBranch)(v_Elimination_union_cs.cases)))
      case hydra.core.Elimination.wrap(v_Elimination_wrap_nm) => hydra.core.Elimination.wrap(v_Elimination_wrap_nm)
    def forMap(mp: Map[hydra.core.Term, hydra.core.Term]): Map[hydra.core.Term, hydra.core.Term] =
      {
      def forPair(pr: Tuple2[hydra.core.Term, hydra.core.Term]): Tuple2[hydra.core.Term, hydra.core.Term] =
        Tuple2(recurse(tx)(pairs.first[hydra.core.Term, hydra.core.Term](pr)), recurse(tx)(pairs.second[hydra.core.Term, hydra.core.Term](pr)))
      maps.fromList[hydra.core.Term, hydra.core.Term](lists.map[Tuple2[hydra.core.Term, hydra.core.Term],
         Tuple2[hydra.core.Term, hydra.core.Term]](forPair)(maps.toList[hydra.core.Term, hydra.core.Term](mp)))
    }
    term match
      case hydra.core.Term.annotated(v_Term_annotated_at) => afterRecursion(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(recurse(tx)(v_Term_annotated_at.body),
         (v_Term_annotated_at.annotation))))
      case hydra.core.Term.application(v_Term_application_app) => {
        val rhs: hydra.core.Term = rewriteWithArgs(Seq())(tx)(v_Term_application_app.argument)
        rewriteWithArgs(lists.cons[hydra.core.Term](rhs)(args))(tx)(v_Term_application_app.function)
      }
      case hydra.core.Term.either(v_Term_either_e) => afterRecursion(hydra.core.Term.either(eithers.either[hydra.core.Term,
         hydra.core.Term, Either[hydra.core.Term, hydra.core.Term]]((l: hydra.core.Term) => Left(recurse(tx)(l)))((r: hydra.core.Term) => Right(recurse(tx)(r)))(v_Term_either_e)))
      case hydra.core.Term.function(v_Term_function_fn) => v_Term_function_fn match
        case hydra.core.Function.elimination(v_Function_elimination_elm) => {
          val padElim: Boolean = v_Function_elimination_elm match
            case hydra.core.Elimination.record(v_Elimination_record__) => false
            case hydra.core.Elimination.union(v_Elimination_union__) => true
            case hydra.core.Elimination.wrap(v_Elimination_wrap__) => false
          {
            val elimTerm: hydra.core.Term = hydra.core.Term.function(hydra.core.Function.elimination(forElimination(v_Function_elimination_elm)))
            {
              val elimHeadType: Option[hydra.core.Type] = v_Function_elimination_elm match
                case hydra.core.Elimination.union(v_Elimination_union_cs2) => Some(hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable(v_Elimination_union_cs2.typeName),
                   hydra.core.Type.unit)))
                case _ => None
              expand(padElim)(args)(1)(elimHeadType)(elimTerm)
            }
          }
        }
        case hydra.core.Function.lambda(v_Function_lambda_lm) => {
          val tx1: hydra.graph.Graph = hydra.schemas.extendGraphForLambda(tx)(v_Function_lambda_lm)
          {
            val body: hydra.core.Term = rewriteWithArgs(Seq())(tx1)(v_Function_lambda_lm.body)
            {
              val result: hydra.core.Term = hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda(v_Function_lambda_lm.parameter,
                 (v_Function_lambda_lm.domain), body)))
              {
                val arty: Int = termArityWithContext(tx)(result)
                expand(false)(args)(arty)(None)(result)
              }
            }
          }
        }
        case hydra.core.Function.primitive(v_Function_primitive_pn) => {
          val arty: Int = termArityWithContext(tx)(term)
          {
            val primType: Option[hydra.core.Type] = maybes.map[hydra.core.TypeScheme, hydra.core.Type]((ts: hydra.core.TypeScheme) => (ts.`type`))(maps.lookup[hydra.core.Name,
               hydra.core.TypeScheme](v_Function_primitive_pn)(primTypes))
            expand(false)(args)(arty)(primType)(term)
          }
        }
      case hydra.core.Term.let(v_Term_let_lt) => {
        val tx1: hydra.graph.Graph = hydra.schemas.extendGraphForLet((_x: hydra.graph.Graph) => (_2: hydra.core.Binding) => None)(tx)(v_Term_let_lt)
        {
          def mapBinding(b: hydra.core.Binding): hydra.core.Binding = hydra.core.Binding(b.name, rewriteWithArgs(Seq())(tx1)(b.term), (b.`type`))
          {
            val result: hydra.core.Term = hydra.core.Term.let(hydra.core.Let(lists.map[hydra.core.Binding,
               hydra.core.Binding](mapBinding)(v_Term_let_lt.bindings), rewriteWithArgs(Seq())(tx1)(v_Term_let_lt.body)))
            afterRecursion(result)
          }
        }
      }
      case hydra.core.Term.list(v_Term_list_els) => afterRecursion(hydra.core.Term.list(lists.map[hydra.core.Term,
         hydra.core.Term]((el: hydra.core.Term) => recurse(tx)(el))(v_Term_list_els)))
      case hydra.core.Term.literal(v_Term_literal_v) => hydra.core.Term.literal(v_Term_literal_v)
      case hydra.core.Term.map(v_Term_map_mp) => afterRecursion(hydra.core.Term.map(forMap(v_Term_map_mp)))
      case hydra.core.Term.maybe(v_Term_maybe_mb) => afterRecursion(hydra.core.Term.maybe(maybes.map[hydra.core.Term,
         hydra.core.Term]((v: hydra.core.Term) => recurse(tx)(v))(v_Term_maybe_mb)))
      case hydra.core.Term.pair(v_Term_pair_pr) => afterRecursion(hydra.core.Term.pair(Tuple2(recurse(tx)(pairs.first[hydra.core.Term,
         hydra.core.Term](v_Term_pair_pr)), recurse(tx)(pairs.second[hydra.core.Term, hydra.core.Term](v_Term_pair_pr)))))
      case hydra.core.Term.record(v_Term_record_rc) => afterRecursion(hydra.core.Term.record(hydra.core.Record(v_Term_record_rc.typeName,
         lists.map[hydra.core.Field, hydra.core.Field](forField)(v_Term_record_rc.fields))))
      case hydra.core.Term.set(v_Term_set_st) => afterRecursion(hydra.core.Term.set(sets.fromList[hydra.core.Term](lists.map[hydra.core.Term,
         hydra.core.Term]((el: hydra.core.Term) => recurse(tx)(el))(sets.toList[hydra.core.Term](v_Term_set_st)))))
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => afterRecursion(hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(recurse(tx)(v_Term_typeApplication_tt.body),
         (v_Term_typeApplication_tt.`type`))))
      case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => {
        val tx1: hydra.graph.Graph = hydra.schemas.extendGraphForTypeLambda(tx)(v_Term_typeLambda_tl)
        {
          val result: hydra.core.Term = hydra.core.Term.typeLambda(hydra.core.TypeLambda(v_Term_typeLambda_tl.parameter,
             rewriteWithArgs(Seq())(tx1)(v_Term_typeLambda_tl.body)))
          afterRecursion(result)
        }
      }
      case hydra.core.Term.union(v_Term_union_inj) => afterRecursion(hydra.core.Term.union(hydra.core.Injection(v_Term_union_inj.typeName,
         forField(v_Term_union_inj.field))))
      case hydra.core.Term.unit => hydra.core.Term.unit
      case hydra.core.Term.variable(v_Term_variable_vn) => {
        val arty: Int = termArityWithContext(tx)(term)
        {
          val varType: Option[hydra.core.Type] = maybes.map[hydra.core.TypeScheme, hydra.core.Type](hydra.rewriting.typeSchemeToFType)(maps.lookup[hydra.core.Name,
             hydra.core.TypeScheme](v_Term_variable_vn)(tx.boundTypes))
          expand(false)(args)(arty)(varType)(term)
        }
      }
      case hydra.core.Term.wrap(v_Term_wrap_wt) => afterRecursion(hydra.core.Term.wrap(hydra.core.WrappedTerm(v_Term_wrap_wt.typeName,
         recurse(tx)(v_Term_wrap_wt.body))))
  }
  hydra.reduction.contractTerm(rewriteWithArgs(Seq())(tx0)(term0))
}

def etaExpansionArity(graph: hydra.graph.Graph)(term: hydra.core.Term): Int =
  term match
  case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.reduction.etaExpansionArity(graph)(v_Term_annotated_at.body)
  case hydra.core.Term.application(v_Term_application_app) => math.sub(hydra.reduction.etaExpansionArity(graph)(v_Term_application_app.function))(1)
  case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
    case hydra.core.Function.elimination(v_Function_elimination__) => 1
    case hydra.core.Function.lambda(v_Function_lambda__) => 0
    case hydra.core.Function.primitive(v_Function_primitive_name) => hydra.arity.primitiveArity(maybes.fromJust[hydra.graph.Primitive](hydra.lexical.lookupPrimitive(graph)(v_Function_primitive_name)))
  case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => hydra.reduction.etaExpansionArity(graph)(v_Term_typeLambda_ta.body)
  case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => hydra.reduction.etaExpansionArity(graph)(v_Term_typeApplication_tt.body)
  case hydra.core.Term.variable(v_Term_variable_name) => maybes.maybe[Int, hydra.core.TypeScheme](0)((ts: hydra.core.TypeScheme) => hydra.arity.typeArity(ts.`type`))(maybes.bind[hydra.core.Binding,
     hydra.core.TypeScheme](hydra.lexical.lookupElement(graph)(v_Term_variable_name))((b: hydra.core.Binding) => (b.`type`)))
  case _ => 0

def etaExpandTypedTerm(cx: hydra.context.Context)(tx0: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error],
   hydra.core.Term] =
  {
  def rewrite(topLevel: Boolean)(forced: Boolean)(typeArgs: Seq[hydra.core.Type])(recurse: (hydra.graph.Graph => hydra.core.Term => Either[hydra.context.InContext[hydra.error.Error],
     hydra.core.Term]))(tx: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error],
     hydra.core.Term] =
    {
    def rewriteSpine(term2: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error], hydra.core.Term] =
      term2 match
      case hydra.core.Term.annotated(v_Term_annotated_at) => eithers.bind[hydra.context.InContext[hydra.error.Error],
         hydra.core.Term, hydra.core.Term](rewriteSpine(v_Term_annotated_at.body))((body: hydra.core.Term) =>
        {
        val ann: Map[hydra.core.Name, hydra.core.Term] = (v_Term_annotated_at.annotation)
        Right(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(body, ann)))
      })
      case hydra.core.Term.application(v_Term_application_a) => {
        val l: Seq[hydra.core.Type] = logic.ifElse[Seq[hydra.core.Type]](false)(Seq(hydra.core.Type.literal(hydra.core.LiteralType.string)))(Seq())
        eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Term, hydra.core.Term](rewriteSpine(v_Term_application_a.function))((lhs: hydra.core.Term) =>
          eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Term, hydra.core.Term](rewrite(true)(false)(l)(recurse)(tx)(v_Term_application_a.argument))((rhs: hydra.core.Term) =>
          Right(hydra.core.Term.application(hydra.core.Application(lhs, rhs)))))
      }
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tat) => eithers.bind[hydra.context.InContext[hydra.error.Error],
         hydra.core.Term, hydra.core.Term](rewriteSpine(v_Term_typeApplication_tat.body))((body: hydra.core.Term) =>
        {
        val typ: hydra.core.Type = (v_Term_typeApplication_tat.`type`)
        Right(hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(body, typ)))
      })
      case _ => rewrite(false)(false)(Seq())(recurse)(tx)(term2)
    def arityOf(tx2: hydra.graph.Graph)(term2: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error], Int] =
      {
      val dflt: Either[hydra.context.InContext[hydra.error.Error], Int] = eithers.map[Tuple2[hydra.core.Type,
         hydra.context.Context], Int, hydra.context.InContext[hydra.error.Error]]((_tc: Tuple2[hydra.core.Type,
         hydra.context.Context]) =>
        hydra.arity.typeArity(pairs.first[hydra.core.Type, hydra.context.Context](_tc)))(hydra.checking.typeOf(cx)(tx2)(Seq())(term2))
      def forFunction(tx3: hydra.graph.Graph)(f: hydra.core.Function): Either[hydra.context.InContext[hydra.error.Error], Int] =
        f match
        case hydra.core.Function.elimination(v_Function_elimination__) => Right(1)
        case hydra.core.Function.lambda(v_Function_lambda_l) => {
          val txl: hydra.graph.Graph = hydra.schemas.extendGraphForLambda(tx3)(v_Function_lambda_l)
          arityOf(txl)(v_Function_lambda_l.body)
        }
        case hydra.core.Function.primitive(v_Function_primitive_name) => eithers.map[hydra.core.TypeScheme,
           Int, hydra.context.InContext[hydra.error.Error]]((_ts: hydra.core.TypeScheme) => hydra.arity.typeSchemeArity(_ts))(hydra.lexical.requirePrimitiveType(cx)(tx3)(v_Function_primitive_name))
      term2 match
        case hydra.core.Term.annotated(v_Term_annotated_at) => arityOf(tx2)(v_Term_annotated_at.body)
        case hydra.core.Term.function(v_Term_function_f) => forFunction(tx2)(v_Term_function_f)
        case hydra.core.Term.let(v_Term_let_l) => {
          val txl: hydra.graph.Graph = hydra.schemas.extendGraphForLet((_x: hydra.graph.Graph) => (_2: hydra.core.Binding) => None)(tx2)(v_Term_let_l)
          arityOf(txl)(v_Term_let_l.body)
        }
        case hydra.core.Term.typeApplication(v_Term_typeApplication_tat) => arityOf(tx2)(v_Term_typeApplication_tat.body)
        case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => {
          val txt: hydra.graph.Graph = hydra.schemas.extendGraphForTypeLambda(tx2)(v_Term_typeLambda_tl)
          arityOf(txt)(v_Term_typeLambda_tl.body)
        }
        case hydra.core.Term.variable(v_Term_variable_name) => maybes.maybe[Either[hydra.context.InContext[hydra.error.Error],
           Int], hydra.core.Type](eithers.map[Tuple2[hydra.core.Type, hydra.context.Context], Int, hydra.context.InContext[hydra.error.Error]]((_tc: Tuple2[hydra.core.Type,
           hydra.context.Context]) =>
          hydra.arity.typeArity(pairs.first[hydra.core.Type, hydra.context.Context](_tc)))(hydra.checking.typeOf(cx)(tx2)(Seq())(hydra.core.Term.variable(v_Term_variable_name))))((t: hydra.core.Type) => Right(hydra.arity.typeArity(t)))(maybes.map[hydra.core.TypeScheme,
             hydra.core.Type](hydra.rewriting.typeSchemeToFType)(maps.lookup[hydra.core.Name, hydra.core.TypeScheme](v_Term_variable_name)(tx2.boundTypes)))
        case _ => dflt
    }
    def extraVariables(n: Int): Seq[hydra.core.Name] =
      lists.map[Int, hydra.core.Name]((i: Int) => strings.cat2("v")(literals.showInt32(i)))(math.range(1)(n))
    def pad(vars: Seq[hydra.core.Name])(body: hydra.core.Term): hydra.core.Term =
      logic.ifElse[hydra.core.Term](lists.`null`[hydra.core.Name](vars))(body)(hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda(lists.head[hydra.core.Name](vars),
         None, pad(lists.tail[hydra.core.Name](vars))(hydra.core.Term.application(hydra.core.Application(body,
         hydra.core.Term.variable(lists.head[hydra.core.Name](vars)))))))))
    def padn(n: Int)(body: hydra.core.Term): hydra.core.Term = pad(extraVariables(n))(body)
    def unwind(term2: hydra.core.Term): hydra.core.Term =
      lists.foldl[hydra.core.Term, hydra.core.Type]((e: hydra.core.Term) =>
      (t: hydra.core.Type) =>
      hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(e, t)))(term2)(typeArgs)
    def forceExpansion(t: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error], hydra.core.Term] =
      eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context],
         hydra.core.Term](hydra.checking.typeOf(cx)(tx)(Seq())(t))((typCx: Tuple2[hydra.core.Type, hydra.context.Context]) =>
      {
      val arity: Int = hydra.arity.typeArity(pairs.first[hydra.core.Type, hydra.context.Context](typCx))
      Right(padn(arity)(unwind(t)))
    })
    def recurseOrForce(term2: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error], hydra.core.Term] =
      logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], hydra.core.Term]](forced)(forceExpansion(term2))(recurse(tx)(unwind(term2)))
    def forCase(f: hydra.core.Field): Either[hydra.context.InContext[hydra.error.Error], hydra.core.Field] =
      eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Term, hydra.core.Field](rewrite(false)(true)(Seq())(recurse)(tx)(f.term))((r: hydra.core.Term) => Right(hydra.core.Field(f.name,
         r)))
    def forCaseStatement(cs: hydra.core.CaseStatement): Either[hydra.context.InContext[hydra.error.Error], hydra.core.Term] =
      {
      val tname: hydra.core.Name = (cs.typeName)
      val dflt: Option[hydra.core.Term] = (cs.default)
      val cases: Seq[hydra.core.Field] = (cs.cases)
      eithers.bind[hydra.context.InContext[hydra.error.Error], Option[hydra.core.Term], hydra.core.Term](eithers.mapMaybe[hydra.core.Term,
         hydra.core.Term, hydra.context.InContext[hydra.error.Error]]((v1: hydra.core.Term) => rewrite(false)(false)(Seq())(recurse)(tx)(v1))(dflt))((rdflt: Option[hydra.core.Term]) =>
        eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.core.Field], hydra.core.Term](eithers.mapList[hydra.core.Field,
           hydra.core.Field, hydra.context.InContext[hydra.error.Error]](forCase)(cases))((rcases: Seq[hydra.core.Field]) =>
        Right(hydra.core.Term.function(hydra.core.Function.elimination(hydra.core.Elimination.union(hydra.core.CaseStatement(tname, rdflt, rcases)))))))
    }
    def forElimination(elm: hydra.core.Elimination): Either[hydra.context.InContext[hydra.error.Error], hydra.core.Term] =
      {
      def checkBase(elm2: hydra.core.Elimination): Either[hydra.context.InContext[hydra.error.Error], hydra.core.Term] =
        elm2 match
        case hydra.core.Elimination.union(v_Elimination_union_cs) => forCaseStatement(v_Elimination_union_cs)
        case _ => recurse(tx)(term)
      eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Term, hydra.core.Term](eithers.map[hydra.core.Term,
         hydra.core.Term, hydra.context.InContext[hydra.error.Error]](unwind)(checkBase(elm)))((base: hydra.core.Term) =>
        Right(logic.ifElse[hydra.core.Term](logic.or(topLevel)(forced))(padn(1)(base))(base)))
    }
    term match
      case hydra.core.Term.application(v_Term_application_a) => {
        val lhs: hydra.core.Term = (v_Term_application_a.function)
        {
          val rhs: hydra.core.Term = (v_Term_application_a.argument)
          eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Term, hydra.core.Term](rewrite(true)(false)(Seq())(recurse)(tx)(rhs))((rhs2: hydra.core.Term) =>
            eithers.bind[hydra.context.InContext[hydra.error.Error], Int, hydra.core.Term](arityOf(tx)(lhs))((lhsarity: Int) =>
            eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Term, hydra.core.Term](rewriteSpine(lhs))((lhs2: hydra.core.Term) =>
            {
            val a2: hydra.core.Term = hydra.core.Term.application(hydra.core.Application(lhs2, rhs2))
            Right(logic.ifElse[hydra.core.Term](equality.gt[Int](lhsarity)(1))(padn(math.sub(lhsarity)(1))(a2))(a2))
          })))
        }
      }
      case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
        case hydra.core.Function.elimination(v_Function_elimination_elm) => forElimination(v_Function_elimination_elm)
        case hydra.core.Function.lambda(v_Function_lambda_l) => {
          val txl: hydra.graph.Graph = hydra.schemas.extendGraphForLambda(tx)(v_Function_lambda_l)
          eithers.map[hydra.core.Term, hydra.core.Term, hydra.context.InContext[hydra.error.Error]](unwind)(recurse(txl)(term))
        }
        case _ => recurseOrForce(term)
      case hydra.core.Term.let(v_Term_let_l) => {
        val txlt: hydra.graph.Graph = hydra.schemas.extendGraphForLet((_x: hydra.graph.Graph) => (_2: hydra.core.Binding) => None)(tx)(v_Term_let_l)
        recurse(txlt)(term)
      }
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tat) => rewrite(topLevel)(forced)(lists.cons[hydra.core.Type](v_Term_typeApplication_tat.`type`)(typeArgs))(recurse)(tx)(v_Term_typeApplication_tat.body)
      case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => {
        val txt: hydra.graph.Graph = hydra.schemas.extendGraphForTypeLambda(tx)(v_Term_typeLambda_tl)
        recurse(txt)(term)
      }
      case _ => recurseOrForce(term)
  }
  hydra.rewriting.rewriteTermWithContextM((v1: (hydra.graph.Graph => hydra.core.Term => Either[hydra.context.InContext[hydra.error.Error], hydra.core.Term])) =>
    (v2: hydra.graph.Graph) =>
    (v3: hydra.core.Term) => rewrite(true)(false)(Seq())(v1)(v2)(v3))(tx0)(term0)
}

def reduceTerm(cx: hydra.context.Context)(graph: hydra.graph.Graph)(eager: Boolean)(term: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error],
   hydra.core.Term] =
  {
  def reduce(eager2: Boolean)(v1: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error],
     hydra.core.Term] = hydra.reduction.reduceTerm(cx)(graph)(eager2)(v1)
  def doRecurse(eager2: Boolean)(term2: hydra.core.Term): Boolean =
    {
    def isNonLambda(f: hydra.core.Function): Boolean =
      f match
      case hydra.core.Function.lambda(v_Function_lambda__) => false
      case _ => true
    val isNonLambdaTerm: Boolean = term2 match
      case hydra.core.Term.function(v_Term_function_f) => isNonLambda(v_Term_function_f)
      case hydra.core.Term.let(v_Term_let__) => false
      case _ => true
    logic.and(eager2)(isNonLambdaTerm)
  }
  def reduceArg(eager2: Boolean)(arg: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error], hydra.core.Term] =
    logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], hydra.core.Term]](eager2)(Right(arg))(reduce(false)(arg))
  def applyToArguments(fun: hydra.core.Term)(args: Seq[hydra.core.Term]): hydra.core.Term =
    logic.ifElse[hydra.core.Term](lists.`null`[hydra.core.Term](args))(fun)(applyToArguments(hydra.core.Term.application(hydra.core.Application(fun,
       lists.head[hydra.core.Term](args))))(lists.tail[hydra.core.Term](args)))
  def mapErrorToString(ic: hydra.context.InContext[hydra.error.Error]): hydra.context.InContext[hydra.error.Error] =
    hydra.context.InContext(hydra.error.Error.other(hydra.show.error.error(ic.`object`)), (ic.context))
  def applyElimination(elm: hydra.core.Elimination)(reducedArg: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error], hydra.core.Term] =
    elm match
    case hydra.core.Elimination.record(v_Elimination_record_proj) => eithers.bind[hydra.context.InContext[hydra.error.Error],
       Seq[hydra.core.Field], hydra.core.Term](hydra.extract.core.record(cx)(v_Elimination_record_proj.typeName)(graph)(hydra.rewriting.deannotateTerm(reducedArg)))((fields: Seq[hydra.core.Field]) =>
      {
      val matchingFields: Seq[hydra.core.Field] = lists.filter[hydra.core.Field]((f: hydra.core.Field) =>
        equality.equal[hydra.core.Name](f.name)(v_Elimination_record_proj.field))(fields)
      logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], hydra.core.Term]](lists.`null`[hydra.core.Field](matchingFields))(Left(hydra.context.InContext(hydra.error.Error.other(strings.cat(Seq("no such field: ",
         (v_Elimination_record_proj.field), " in ", (v_Elimination_record_proj.typeName), " record"))),
         cx)))(Right(lists.head[hydra.core.Field](matchingFields).term))
    })
    case hydra.core.Elimination.union(v_Elimination_union_cs) => eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.core.Field, hydra.core.Term](hydra.extract.core.injection(cx)(v_Elimination_union_cs.typeName)(graph)(reducedArg))((field: hydra.core.Field) =>
      {
      val matchingFields: Seq[hydra.core.Field] = lists.filter[hydra.core.Field]((f: hydra.core.Field) => equality.equal[hydra.core.Name](f.name)(field.name))(v_Elimination_union_cs.cases)
      logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], hydra.core.Term]](lists.`null`[hydra.core.Field](matchingFields))(maybes.maybe[Either[hydra.context.InContext[hydra.error.Error],
         hydra.core.Term], hydra.core.Term](Left(hydra.context.InContext(hydra.error.Error.other(strings.cat(Seq("no such field ",
         (field.name), " in ", (v_Elimination_union_cs.typeName), " case statement"))), cx)))((x: hydra.core.Term) => Right(x))(v_Elimination_union_cs.default))(Right(hydra.core.Term.application(hydra.core.Application(lists.head[hydra.core.Field](matchingFields).term,
         (field.term)))))
    })
    case hydra.core.Elimination.wrap(v_Elimination_wrap_name) => hydra.extract.core.wrap(cx)(v_Elimination_wrap_name)(graph)(reducedArg)
  def applyIfNullary(eager2: Boolean)(original: hydra.core.Term)(args: Seq[hydra.core.Term]): Either[hydra.context.InContext[hydra.error.Error],
     hydra.core.Term] =
    {
    val stripped: hydra.core.Term = hydra.rewriting.deannotateTerm(original)
    def forElimination(elm: hydra.core.Elimination)(args2: Seq[hydra.core.Term]): Either[hydra.context.InContext[hydra.error.Error], hydra.core.Term] =
      {
      val arg: hydra.core.Term = lists.head[hydra.core.Term](args2)
      val remainingArgs: Seq[hydra.core.Term] = lists.tail[hydra.core.Term](args2)
      eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Term, hydra.core.Term](reduceArg(eager2)(hydra.rewriting.deannotateTerm(arg)))((reducedArg: hydra.core.Term) =>
        eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Term, hydra.core.Term](eithers.bind[hydra.context.InContext[hydra.error.Error],
           hydra.core.Term, hydra.core.Term](applyElimination(elm)(reducedArg))((v1: hydra.core.Term) => reduce(eager2)(v1)))((reducedResult: hydra.core.Term) => applyIfNullary(eager2)(reducedResult)(remainingArgs)))
    }
    def forLambda(l: hydra.core.Lambda)(args2: Seq[hydra.core.Term]): Either[hydra.context.InContext[hydra.error.Error], hydra.core.Term] =
      {
      val param: hydra.core.Name = (l.parameter)
      val body: hydra.core.Term = (l.body)
      val arg: hydra.core.Term = lists.head[hydra.core.Term](args2)
      val remainingArgs: Seq[hydra.core.Term] = lists.tail[hydra.core.Term](args2)
      eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Term, hydra.core.Term](reduce(eager2)(hydra.rewriting.deannotateTerm(arg)))((reducedArg: hydra.core.Term) =>
        eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Term, hydra.core.Term](reduce(eager2)(hydra.rewriting.replaceFreeTermVariable(param)(reducedArg)(body)))((reducedResult: hydra.core.Term) => applyIfNullary(eager2)(reducedResult)(remainingArgs)))
    }
    def forPrimitive(prim: hydra.graph.Primitive)(arity: Int)(args2: Seq[hydra.core.Term]): Either[hydra.context.InContext[hydra.error.Error],
       hydra.core.Term] =
      {
      val argList: Seq[hydra.core.Term] = lists.take[hydra.core.Term](arity)(args2)
      val remainingArgs: Seq[hydra.core.Term] = lists.drop[hydra.core.Term](arity)(args2)
      eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.core.Term], hydra.core.Term](eithers.mapList[hydra.core.Term,
         hydra.core.Term, hydra.context.InContext[hydra.error.Error]]((v1: hydra.core.Term) => reduceArg(eager2)(v1))(argList))((reducedArgs: Seq[hydra.core.Term]) =>
        {
        val strippedArgs: Seq[hydra.core.Term] = lists.map[hydra.core.Term, hydra.core.Term](hydra.rewriting.deannotateTerm)(reducedArgs)
        eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Term, hydra.core.Term](eithers.bimap[hydra.context.InContext[hydra.error.Error],
           hydra.core.Term, hydra.context.InContext[hydra.error.Error], hydra.core.Term](mapErrorToString)((x: hydra.core.Term) => x)(prim.implementation(cx)(graph)(strippedArgs)))((primResult: hydra.core.Term) =>
          eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Term, hydra.core.Term](reduce(eager2)(primResult))((reducedResult: hydra.core.Term) => applyIfNullary(eager2)(reducedResult)(remainingArgs)))
      })
    }
    stripped match
      case hydra.core.Term.application(v_Term_application_app) => applyIfNullary(eager2)(v_Term_application_app.function)(lists.cons[hydra.core.Term](v_Term_application_app.argument)(args))
      case hydra.core.Term.function(v_Term_function_v1) => v_Term_function_v1 match
        case hydra.core.Function.elimination(v_Function_elimination_elm) => logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
           hydra.core.Term]](lists.`null`[hydra.core.Term](args))(Right(original))(forElimination(v_Function_elimination_elm)(args))
        case hydra.core.Function.lambda(v_Function_lambda_l) => logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
           hydra.core.Term]](lists.`null`[hydra.core.Term](args))(Right(original))(forLambda(v_Function_lambda_l)(args))
        case hydra.core.Function.primitive(v_Function_primitive_name) => eithers.bind[hydra.context.InContext[hydra.error.Error],
           hydra.graph.Primitive, hydra.core.Term](hydra.lexical.requirePrimitive(cx)(graph)(v_Function_primitive_name))((prim: hydra.graph.Primitive) =>
          {
          val arity: Int = hydra.arity.primitiveArity(prim)
          logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], hydra.core.Term]](equality.gt[Int](arity)(lists.length[hydra.core.Term](args)))(Right(applyToArguments(original)(args)))(forPrimitive(prim)(arity)(args))
        })
      case hydra.core.Term.variable(v_Term_variable_v) => {
        val mBinding: Option[hydra.core.Binding] = hydra.lexical.dereferenceElement(graph)(v_Term_variable_v)
        maybes.maybe[Either[hydra.context.InContext[hydra.error.Error], hydra.core.Term], hydra.core.Binding](Right(applyToArguments(original)(args)))((binding: hydra.core.Binding) => applyIfNullary(eager2)(binding.term)(args))(mBinding)
      }
      case hydra.core.Term.let(v_Term_let_lt) => {
        val bindings: Seq[hydra.core.Binding] = (v_Term_let_lt.bindings)
        {
          val body: hydra.core.Term = (v_Term_let_lt.body)
          {
            def letExpr(b: hydra.core.Binding): hydra.core.Term = hydra.core.Term.let(hydra.core.Let(Seq(b), hydra.core.Term.variable(b.name)))
            {
              def expandBinding(b: hydra.core.Binding): hydra.core.Binding =
                hydra.core.Binding(b.name, hydra.rewriting.replaceFreeTermVariable(b.name)(letExpr(b))(b.term), (b.`type`))
              {
                val expandedBindings: Seq[hydra.core.Binding] = lists.map[hydra.core.Binding, hydra.core.Binding](expandBinding)(bindings)
                {
                  def substituteBinding(term2: hydra.core.Term)(b: hydra.core.Binding): hydra.core.Term = hydra.rewriting.replaceFreeTermVariable(b.name)(b.term)(term2)
                  {
                    def substituteAll(bs: Seq[hydra.core.Binding])(term2: hydra.core.Term): hydra.core.Term = lists.foldl[hydra.core.Term,
                       hydra.core.Binding](substituteBinding)(term2)(bs)
                    {
                      val expandedBody: hydra.core.Term = substituteAll(expandedBindings)(body)
                      eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Term, hydra.core.Term](reduce(eager2)(expandedBody))((reducedBody: hydra.core.Term) => applyIfNullary(eager2)(reducedBody)(args))
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
  def mapping(recurse: (hydra.core.Term => Either[hydra.context.InContext[hydra.error.Error], hydra.core.Term]))(mid: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error],
     hydra.core.Term] =
    eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Term, hydra.core.Term](logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
       hydra.core.Term]](doRecurse(eager)(mid))(recurse(mid))(Right(mid)))((inner: hydra.core.Term) => applyIfNullary(eager)(inner)(Seq()))
  hydra.rewriting.rewriteTermM(mapping)(term)
}

def termIsClosed(term: hydra.core.Term): Boolean = sets.`null`[hydra.core.Name](hydra.rewriting.freeVariablesInTerm(term))

def termIsValue(term: hydra.core.Term): Boolean =
  {
  def forList(els: Seq[hydra.core.Term]): Boolean =
    lists.foldl[Boolean, hydra.core.Term]((b: Boolean) =>
    (t: hydra.core.Term) => logic.and(b)(hydra.reduction.termIsValue(t)))(true)(els)
  def checkField(f: hydra.core.Field): Boolean = hydra.reduction.termIsValue(f.term)
  def checkFields(fields: Seq[hydra.core.Field]): Boolean =
    lists.foldl[Boolean, hydra.core.Field]((b: Boolean) => (f: hydra.core.Field) => logic.and(b)(checkField(f)))(true)(fields)
  def functionIsValue(f: hydra.core.Function): Boolean =
    f match
    case hydra.core.Function.elimination(v_Function_elimination_e) => v_Function_elimination_e match
      case hydra.core.Elimination.wrap(v_Elimination_wrap__) => true
      case hydra.core.Elimination.record(v_Elimination_record__) => true
      case hydra.core.Elimination.union(v_Elimination_union_cs) => logic.and(checkFields(v_Elimination_union_cs.cases))(maybes.maybe[Boolean,
         hydra.core.Term](true)(hydra.reduction.termIsValue)(v_Elimination_union_cs.default))
    case hydra.core.Function.lambda(v_Function_lambda_l) => hydra.reduction.termIsValue(v_Function_lambda_l.body)
    case hydra.core.Function.primitive(v_Function_primitive__) => true
  hydra.rewriting.deannotateTerm(term) match
    case hydra.core.Term.application(v_Term_application__) => false
    case hydra.core.Term.either(v_Term_either_e) => eithers.either[hydra.core.Term, hydra.core.Term, Boolean]((l: hydra.core.Term) => hydra.reduction.termIsValue(l))((r: hydra.core.Term) => hydra.reduction.termIsValue(r))(v_Term_either_e)
    case hydra.core.Term.literal(v_Term_literal__) => true
    case hydra.core.Term.function(v_Term_function_f) => functionIsValue(v_Term_function_f)
    case hydra.core.Term.list(v_Term_list_els) => forList(v_Term_list_els)
    case hydra.core.Term.map(v_Term_map_m) => lists.foldl[Boolean, Tuple2[hydra.core.Term, hydra.core.Term]]((b: Boolean) =>
      (kv: Tuple2[hydra.core.Term, hydra.core.Term]) =>
      logic.and(b)(logic.and(hydra.reduction.termIsValue(pairs.first[hydra.core.Term, hydra.core.Term](kv)))(hydra.reduction.termIsValue(pairs.second[hydra.core.Term,
         hydra.core.Term](kv)))))(true)(maps.toList[hydra.core.Term, hydra.core.Term](v_Term_map_m))
    case hydra.core.Term.maybe(v_Term_maybe_m) => maybes.maybe[Boolean, hydra.core.Term](true)(hydra.reduction.termIsValue)(v_Term_maybe_m)
    case hydra.core.Term.record(v_Term_record_r) => checkFields(v_Term_record_r.fields)
    case hydra.core.Term.set(v_Term_set_s) => forList(sets.toList[hydra.core.Term](v_Term_set_s))
    case hydra.core.Term.union(v_Term_union_i) => checkField(v_Term_union_i.field)
    case hydra.core.Term.unit => true
    case hydra.core.Term.variable(v_Term_variable__) => false
    case _ => false
}
