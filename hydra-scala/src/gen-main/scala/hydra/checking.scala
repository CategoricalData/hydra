package hydra.checking

import hydra.coders.*

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

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.sets

import hydra.lib.strings

def allEqual[T0](els: Seq[T0]): Boolean =
  hydra.lib.logic.ifElse[Boolean](hydra.lib.lists.`null`[T0](els))(true)(hydra.lib.lists.foldl[Boolean, T0]((b: Boolean) =>
  (t: T0) =>
  hydra.lib.logic.and(b)(hydra.lib.equality.equal[T0](t)(hydra.lib.lists.head[T0](els))))(true)(hydra.lib.lists.tail[T0](els)))

def applyTypeArgumentsToType(cx: hydra.context.Context)(tx: hydra.graph.Graph)(typeArgs: Seq[hydra.core.Type])(t: hydra.core.Type): Either[hydra.context.InContext[hydra.error.Error],
   hydra.core.Type] =
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], hydra.core.Type]](hydra.lib.lists.`null`[hydra.core.Type](typeArgs))(Right(t))({
  val nonnull: Either[hydra.context.InContext[hydra.error.Error], hydra.core.Type] = t match
    case hydra.core.Type.forall(v_Type_forall_ft) => {
      val v: hydra.core.Name = (v_Type_forall_ft.parameter)
      {
        val tbody: hydra.core.Type = (v_Type_forall_ft.body)
        hydra.checking.applyTypeArgumentsToType(cx)(tx)(hydra.lib.lists.tail[hydra.core.Type](typeArgs))(hydra.substitution.substInType(hydra.lib.maps.singleton[hydra.core.Name,
           hydra.core.Type](v)(hydra.lib.lists.head[hydra.core.Type](typeArgs)))(tbody))
      }
    }
    case _ => Left(hydra.context.InContext(hydra.error.Error.other(hydra.lib.strings.cat(Seq("not a forall type: ",
       hydra.show.core.`type`(t), ". Trying to apply ", hydra.lib.literals.showInt32(hydra.lib.lists.length[hydra.core.Type](typeArgs)),
       " type args: ", hydra.formatting.showList(hydra.show.core.`type`)(typeArgs), ". Context has vars: {",
       hydra.lib.strings.intercalate(", ")(hydra.lib.lists.map[hydra.core.Name, scala.Predef.String]((x) => x)(hydra.lib.maps.keys[hydra.core.Name,
       hydra.core.TypeScheme](tx.boundTypes))), "}"))), cx))
  nonnull
})

def checkForUnboundTypeVariables(cx: hydra.context.Context)(tx: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error],
   Unit] =
  {
  val svars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.maps.keys[hydra.core.Name,
     hydra.core.TypeScheme](tx.schemaTypes))
  def checkRecursive(vars: scala.collection.immutable.Set[hydra.core.Name])(trace: Seq[scala.Predef.String])(lbinding: Option[hydra.core.Binding])(term: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error],
     Unit] =
    {
    def recurse(v1: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error], Unit] = checkRecursive(vars)(trace)(lbinding)(v1)
    val dflt: Either[hydra.context.InContext[hydra.error.Error], Unit] = hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       Seq[Unit], Unit](hydra.lib.eithers.mapList[hydra.core.Term, Unit, hydra.context.InContext[hydra.error.Error]](recurse)(hydra.rewriting.subterms(term)))((_x: Seq[Unit]) => Right(()))
    def check(typ: hydra.core.Type): Either[hydra.context.InContext[hydra.error.Error], Unit] =
      {
      val freevars: scala.collection.immutable.Set[hydra.core.Name] = hydra.rewriting.freeVariablesInType(typ)
      val badvars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.difference[hydra.core.Name](hydra.lib.sets.difference[hydra.core.Name](freevars)(vars))(svars)
      hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Unit]](hydra.lib.sets.`null`[hydra.core.Name](badvars))(Right(()))(Left(hydra.context.InContext(hydra.error.Error.checking(hydra.error.CheckingError.unboundTypeVariables(hydra.error.UnboundTypeVariablesError(badvars,
         typ))), cx)))
    }
    def checkOptional(m: Option[hydra.core.Type]): Either[hydra.context.InContext[hydra.error.Error], Unit] =
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Option[Unit], Unit](hydra.lib.eithers.mapMaybe[hydra.core.Type,
         Unit, hydra.context.InContext[hydra.error.Error]](check)(m))((_x: Option[Unit]) => Right(()))
    term match
      case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
        case hydra.core.Function.elimination(v_Function_elimination_e) => dflt
        case hydra.core.Function.lambda(v_Function_lambda_l) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
           Unit, Unit](checkOptional(v_Function_lambda_l.domain))((_x: Unit) => recurse(v_Function_lambda_l.body))
        case _ => dflt
      case hydra.core.Term.let(v_Term_let_l) => {
        def forBinding(b: hydra.core.Binding): Either[hydra.context.InContext[hydra.error.Error], Unit] =
          {
          val bterm: hydra.core.Term = (b.term)
          val newVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.maybes.maybe[scala.collection.immutable.Set[hydra.core.Name],
             hydra.core.TypeScheme](vars)((ts: hydra.core.TypeScheme) =>
            hydra.lib.sets.union[hydra.core.Name](vars)(hydra.lib.sets.fromList[hydra.core.Name](ts.variables)))(b.`type`)
          val newTrace: Seq[scala.Predef.String] = hydra.lib.lists.cons[scala.Predef.String](b.name)(trace)
          checkRecursive(newVars)(newTrace)(Some(b))(bterm)
        }
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[Unit], Unit](hydra.lib.eithers.mapList[hydra.core.Binding,
           Unit, hydra.context.InContext[hydra.error.Error]](forBinding)(v_Term_let_l.bindings))((_x: Seq[Unit]) => recurse(v_Term_let_l.body))
      }
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
         Unit, Unit](check(v_Term_typeApplication_tt.`type`))((_x: Unit) => recurse(v_Term_typeApplication_tt.body))
      case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
         Unit, Unit](check(hydra.core.Type.variable(v_Term_typeLambda_tl.parameter)))((_x: Unit) => recurse(v_Term_typeLambda_tl.body))
      case _ => dflt
  }
  checkRecursive(hydra.lib.sets.empty[hydra.core.Name])(Seq("top level"))(None)(term0)
}

def checkNominalApplication(cx: hydra.context.Context)(tx: hydra.graph.Graph)(tname: hydra.core.Name)(typeArgs: Seq[hydra.core.Type]): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[Unit, hydra.context.Context]] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.TypeScheme, hydra.context.Context],
     Tuple2[Unit, hydra.context.Context]](hydra.schemas.requireSchemaType(cx)(tx.schemaTypes)(tname))((result: Tuple2[hydra.core.TypeScheme,
     hydra.context.Context]) =>
  {
  val schemaType: hydra.core.TypeScheme = hydra.lib.pairs.first[hydra.core.TypeScheme, hydra.context.Context](result)
  {
    val cx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.TypeScheme, hydra.context.Context](result)
    {
      val vars: Seq[hydra.core.Name] = (schemaType.variables)
      {
        val varslen: Int = hydra.lib.lists.length[hydra.core.Name](vars)
        {
          val argslen: Int = hydra.lib.lists.length[hydra.core.Type](typeArgs)
          hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Tuple2[Unit, hydra.context.Context]]](hydra.lib.equality.equal[Int](varslen)(argslen))(Right(Tuple2((),
             cx2)))(Left(hydra.context.InContext(hydra.error.Error.checking(hydra.error.CheckingError.typeArityMismatch(hydra.error.TypeArityMismatchError(hydra.core.Type.variable(tname),
             varslen, argslen, typeArgs))), cx2)))
        }
      }
    }
  }
})

def checkSameType(cx: hydra.context.Context)(tx: hydra.graph.Graph)(desc: scala.Predef.String)(types: Seq[hydra.core.Type]): Either[hydra.context.InContext[hydra.error.Error],
   hydra.core.Type] =
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], hydra.core.Type]](hydra.checking.typesAllEffectivelyEqual(tx)(types))(Right(hydra.lib.lists.head[hydra.core.Type](types)))(Left(hydra.context.InContext(hydra.error.Error.checking(hydra.error.CheckingError.unequalTypes(hydra.error.UnequalTypesError(types,
     desc))), cx)))

def checkType(cx: hydra.context.Context)(tx: hydra.graph.Graph)(term: hydra.core.Term)(typ: hydra.core.Type): Either[hydra.context.InContext[hydra.error.Error],
   Unit] =
  {
  val vars: scala.collection.immutable.Set[hydra.core.Name] = (tx.typeVariables)
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Unit]](hydra.constants.debugInference)(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
     hydra.core.Type, Unit](hydra.lib.eithers.map[Tuple2[hydra.core.Type, hydra.context.Context], hydra.core.Type,
     hydra.context.InContext[hydra.error.Error]]((_p: Tuple2[hydra.core.Type, hydra.context.Context]) =>
    hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](_p))(hydra.checking.typeOf(cx)(tx)(Seq())(term)))((t0: hydra.core.Type) =>
    hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Unit]](hydra.checking.typesEffectivelyEqual(tx)(t0)(typ))(Right(()))(Left(hydra.context.InContext(hydra.error.Error.checking(hydra.error.CheckingError.typeMismatch(hydra.error.TypeMismatchError(typ,
       t0))), cx)))))(Right(()))
}

def checkTypeSubst(cx: hydra.context.Context)(tx: hydra.graph.Graph)(subst: hydra.typing.TypeSubst): Either[hydra.context.InContext[hydra.error.Error],
   hydra.typing.TypeSubst] =
  {
  val s: Map[hydra.core.Name, hydra.core.Type] = subst
  val vars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.maps.keys[hydra.core.Name, hydra.core.Type](s))
  val suspectVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.intersection[hydra.core.Name](vars)(hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.maps.keys[hydra.core.Name,
     hydra.core.TypeScheme](tx.schemaTypes)))
  def isNominal(ts: hydra.core.TypeScheme): Boolean =
    hydra.rewriting.deannotateType(ts.`type`) match
    case hydra.core.Type.record(v_Type_record__) => true
    case hydra.core.Type.union(v_Type_union__) => true
    case hydra.core.Type.wrap(v_Type_wrap__) => true
    case _ => false
  val badVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) =>
    hydra.lib.maybes.maybe[Boolean, hydra.core.TypeScheme](false)(isNominal)(hydra.lexical.dereferenceSchemaType(v)(tx.schemaTypes)))(hydra.lib.sets.toList[hydra.core.Name](suspectVars)))
  val badPairs: Seq[Tuple2[hydra.core.Name, hydra.core.Type]] = hydra.lib.lists.filter[Tuple2[hydra.core.Name,
     hydra.core.Type]]((p: Tuple2[hydra.core.Name, hydra.core.Type]) =>
    hydra.lib.sets.member[hydra.core.Name](hydra.lib.pairs.first[hydra.core.Name, hydra.core.Type](p))(badVars))(hydra.lib.maps.toList[hydra.core.Name,
       hydra.core.Type](s))
  def printPair(p: Tuple2[hydra.core.Name, hydra.core.Type]): scala.Predef.String =
    hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.pairs.first[hydra.core.Name, hydra.core.Type](p))(" --> "))(hydra.show.core.`type`(hydra.lib.pairs.second[hydra.core.Name,
       hydra.core.Type](p)))
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], hydra.typing.TypeSubst]](hydra.lib.sets.`null`[hydra.core.Name](badVars))(Right(subst))(Left(hydra.context.InContext(hydra.error.Error.checking(hydra.error.CheckingError.incorrectUnification(hydra.error.IncorrectUnificationError(subst))),
     cx)))
}

def checkTypeVariables[T0, T1](_tx: T0)(_typ: T1): Unit = ()

def containsInScopeTypeVars(tx: hydra.graph.Graph)(t: hydra.core.Type): Boolean =
  {
  val vars: scala.collection.immutable.Set[hydra.core.Name] = (tx.typeVariables)
  val freeVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.rewriting.freeVariablesInTypeSimple(t)
  hydra.lib.logic.not(hydra.lib.sets.`null`[hydra.core.Name](hydra.lib.sets.intersection[hydra.core.Name](vars)(freeVars)))
}

def normalizeTypeFreeVars(typ: hydra.core.Type): hydra.core.Type =
  {
  def collectVars(acc: Map[hydra.core.Name, hydra.core.Name])(t: hydra.core.Type): Map[hydra.core.Name, hydra.core.Name] =
    t match
    case hydra.core.Type.variable(v_Type_variable_v) => hydra.lib.logic.ifElse[Map[hydra.core.Name, hydra.core.Name]](hydra.lib.maps.member[hydra.core.Name,
       hydra.core.Name](v_Type_variable_v)(acc))(acc)(hydra.lib.maps.insert[hydra.core.Name, hydra.core.Name](v_Type_variable_v)(hydra.lib.strings.cat2("_tv")(hydra.lib.literals.showInt32(hydra.lib.maps.size[hydra.core.Name,
       hydra.core.Name](acc))))(acc))
    case _ => acc
  val subst: Map[hydra.core.Name, hydra.core.Name] = hydra.rewriting.foldOverType(hydra.coders.TraversalOrder.pre)(collectVars)(hydra.lib.maps.empty[hydra.core.Name,
     hydra.core.Name])(typ)
  hydra.rewriting.substituteTypeVariables(subst)(typ)
}

def toFContext(cx: hydra.graph.Graph): Map[hydra.core.Name, hydra.core.Type] =
  hydra.lib.maps.map[hydra.core.TypeScheme, hydra.core.Type, hydra.core.Name](hydra.rewriting.typeSchemeToFType)(cx.boundTypes)

def typeListsEffectivelyEqual(tx: hydra.graph.Graph)(tlist1: Seq[hydra.core.Type])(tlist2: Seq[hydra.core.Type]): Boolean =
  hydra.lib.logic.ifElse[Boolean](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Type](tlist1))(hydra.lib.lists.length[hydra.core.Type](tlist2)))(hydra.lib.lists.foldl[Boolean,
     Boolean](hydra.lib.logic.and)(true)(hydra.lib.lists.zipWith[hydra.core.Type, hydra.core.Type, Boolean]((v1: hydra.core.Type) =>
  (v2: hydra.core.Type) => hydra.checking.typesEffectivelyEqual(tx)(v1)(v2))(tlist1)(tlist2)))(false)

def typeOf(cx: hydra.context.Context)(tx: hydra.graph.Graph)(typeArgs: Seq[hydra.core.Type])(term: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[hydra.core.Type, hydra.context.Context]] =
  {
  val cx1: hydra.context.Context = hydra.context.Context(hydra.lib.lists.cons[scala.Predef.String]("typeOf")(cx.trace), (cx.messages), (cx.other))
  term match
    case hydra.core.Term.annotated(v_Term_annotated_v1) => hydra.checking.typeOfAnnotatedTerm(cx1)(tx)(typeArgs)(v_Term_annotated_v1)
    case hydra.core.Term.application(v_Term_application_v1) => hydra.checking.typeOfApplication(cx1)(tx)(typeArgs)(v_Term_application_v1)
    case hydra.core.Term.either(v_Term_either_v1) => hydra.checking.typeOfEither(cx1)(tx)(typeArgs)(v_Term_either_v1)
    case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
      case hydra.core.Function.elimination(v_Function_elimination_elm) => v_Function_elimination_elm match
        case hydra.core.Elimination.record(v_Elimination_record_v1) => hydra.checking.typeOfProjection(cx1)(tx)(typeArgs)(v_Elimination_record_v1)
        case hydra.core.Elimination.union(v_Elimination_union_v1) => hydra.checking.typeOfCaseStatement(cx1)(tx)(typeArgs)(v_Elimination_union_v1)
        case hydra.core.Elimination.wrap(v_Elimination_wrap_v1) => hydra.checking.typeOfUnwrap(cx1)(tx)(typeArgs)(v_Elimination_wrap_v1)
      case hydra.core.Function.lambda(v_Function_lambda_v1) => hydra.checking.typeOfLambda(cx1)(tx)(typeArgs)(v_Function_lambda_v1)
      case hydra.core.Function.primitive(v_Function_primitive_v1) => hydra.checking.typeOfPrimitive(cx1)(tx)(typeArgs)(v_Function_primitive_v1)
    case hydra.core.Term.let(v_Term_let_v1) => hydra.checking.typeOfLet(cx1)(tx)(typeArgs)(v_Term_let_v1)
    case hydra.core.Term.list(v_Term_list_v1) => hydra.checking.typeOfList(cx1)(tx)(typeArgs)(v_Term_list_v1)
    case hydra.core.Term.literal(v_Term_literal_v1) => hydra.checking.typeOfLiteral(cx1)(tx)(typeArgs)(v_Term_literal_v1)
    case hydra.core.Term.map(v_Term_map_v1) => hydra.checking.typeOfMap(cx1)(tx)(typeArgs)(v_Term_map_v1)
    case hydra.core.Term.maybe(v_Term_maybe_v1) => hydra.checking.typeOfMaybe(cx1)(tx)(typeArgs)(v_Term_maybe_v1)
    case hydra.core.Term.pair(v_Term_pair_v1) => hydra.checking.typeOfPair(cx1)(tx)(typeArgs)(v_Term_pair_v1)
    case hydra.core.Term.record(v_Term_record_v1) => hydra.checking.typeOfRecord(cx1)(tx)(typeArgs)(v_Term_record_v1)
    case hydra.core.Term.set(v_Term_set_v1) => hydra.checking.typeOfSet(cx1)(tx)(typeArgs)(v_Term_set_v1)
    case hydra.core.Term.typeApplication(v_Term_typeApplication_v1) => hydra.checking.typeOfTypeApplication(cx1)(tx)(typeArgs)(v_Term_typeApplication_v1)
    case hydra.core.Term.typeLambda(v_Term_typeLambda_v1) => hydra.checking.typeOfTypeLambda(cx1)(tx)(typeArgs)(v_Term_typeLambda_v1)
    case hydra.core.Term.union(v_Term_union_v1) => hydra.checking.typeOfInjection(cx1)(tx)(typeArgs)(v_Term_union_v1)
    case hydra.core.Term.unit => hydra.checking.typeOfUnit(cx1)(tx)(typeArgs)
    case hydra.core.Term.variable(v_Term_variable_v1) => hydra.checking.typeOfVariable(cx1)(tx)(typeArgs)(v_Term_variable_v1)
    case hydra.core.Term.wrap(v_Term_wrap_v1) => hydra.checking.typeOfWrappedTerm(cx1)(tx)(typeArgs)(v_Term_wrap_v1)
    case _ => Left(hydra.context.InContext(hydra.error.Error.checking(hydra.error.CheckingError.unsupportedTermVariant(hydra.error.UnsupportedTermVariantError(hydra.reflect.termVariant(term)))),
       cx1))
}

def typeOfAnnotatedTerm(cx: hydra.context.Context)(tx: hydra.graph.Graph)(typeArgs: Seq[hydra.core.Type])(at: hydra.core.AnnotatedTerm): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[hydra.core.Type, hydra.context.Context]] = hydra.checking.typeOf(cx)(tx)(typeArgs)(at.body)

def typeOfApplication(cx: hydra.context.Context)(tx: hydra.graph.Graph)(typeArgs: Seq[hydra.core.Type])(app: hydra.core.Application): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[hydra.core.Type, hydra.context.Context]] =
  {
  val fun: hydra.core.Term = (app.function)
  val arg: hydra.core.Term = (app.argument)
  def tryType(cx0: hydra.context.Context)(tfun: hydra.core.Type)(targ: hydra.core.Type): Either[hydra.context.InContext[hydra.error.Error],
     Tuple2[hydra.core.Type, hydra.context.Context]] =
    tfun match
    case hydra.core.Type.forall(v_Type_forall_ft) => tryType(cx0)(v_Type_forall_ft.body)(targ)
    case hydra.core.Type.function(v_Type_function_ft) => {
      val dom: hydra.core.Type = (v_Type_function_ft.domain)
      {
        val cod: hydra.core.Type = (v_Type_function_ft.codomain)
        hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type,
           hydra.context.Context]]](hydra.checking.typesEffectivelyEqual(tx)(dom)(targ))(Right(Tuple2(cod,
           cx0)))(Left(hydra.context.InContext(hydra.error.Error.checking(hydra.error.CheckingError.typeMismatch(hydra.error.TypeMismatchError(dom,
           targ))), cx0)))
      }
    }
    case hydra.core.Type.variable(v_Type_variable_v) => {
      val nameResult: Tuple2[hydra.core.Name, hydra.context.Context] = hydra.schemas.freshName(cx0)
      {
        val freshN: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, hydra.context.Context](nameResult)
        {
          val cx1: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Name, hydra.context.Context](nameResult)
          Right(Tuple2(hydra.core.Type.variable(freshN), cx1))
        }
      }
    }
    case _ => Left(hydra.context.InContext(hydra.error.Error.checking(hydra.error.CheckingError.notAFunctionType(hydra.error.NotAFunctionTypeError(tfun))),
       cx0))
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context],
     Tuple2[hydra.core.Type, hydra.context.Context]](hydra.checking.typeOf(cx)(tx)(Seq())(fun))((result1: Tuple2[hydra.core.Type,
     hydra.context.Context]) =>
    {
    val tfun: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](result1)
    {
      val cx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Type, hydra.context.Context](result1)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context],
         Tuple2[hydra.core.Type, hydra.context.Context]](hydra.checking.typeOf(cx2)(tx)(Seq())(arg))((result2: Tuple2[hydra.core.Type,
         hydra.context.Context]) =>
        {
        val targ: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](result2)
        {
          val cx3: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Type, hydra.context.Context](result2)
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context],
             Tuple2[hydra.core.Type, hydra.context.Context]](tryType(cx3)(tfun)(targ))((result3: Tuple2[hydra.core.Type,
             hydra.context.Context]) =>
            {
            val t: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](result3)
            {
              val cx4: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Type, hydra.context.Context](result3)
              hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, Tuple2[hydra.core.Type,
                 hydra.context.Context]](hydra.checking.applyTypeArgumentsToType(cx4)(tx)(typeArgs)(t))((applied: hydra.core.Type) => Right(Tuple2(applied,
                 cx4)))
            }
          })
        }
      })
    }
  })
}

def typeOfCaseStatement(cx: hydra.context.Context)(tx: hydra.graph.Graph)(typeArgs: Seq[hydra.core.Type])(cs: hydra.core.CaseStatement): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[hydra.core.Type, hydra.context.Context]] =
  {
  val tname: hydra.core.Name = (cs.typeName)
  val dflt: Option[hydra.core.Term] = (cs.default)
  val cases: Seq[hydra.core.Field] = (cs.cases)
  val cterms: Seq[hydra.core.Term] = hydra.lib.lists.map[hydra.core.Field, hydra.core.Term]((x: hydra.core.Field) => (x.term))(cases)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Option[Tuple2[hydra.core.Type, hydra.context.Context]],
     Tuple2[hydra.core.Type, hydra.context.Context]](hydra.lib.eithers.mapMaybe[hydra.core.Term, Tuple2[hydra.core.Type,
     hydra.context.Context], hydra.context.InContext[hydra.error.Error]]((e: hydra.core.Term) => hydra.checking.typeOf(cx)(tx)(Seq())(e))(dflt))((dfltResult: Option[Tuple2[hydra.core.Type,
     hydra.context.Context]]) =>
    {
    val tdflt: Option[hydra.core.Type] = hydra.lib.maybes.map[Tuple2[hydra.core.Type, hydra.context.Context],
       hydra.core.Type](hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context])(dfltResult)
    {
      val cx2: hydra.context.Context = hydra.lib.maybes.maybe[hydra.context.Context, Tuple2[hydra.core.Type,
         hydra.context.Context]](cx)(hydra.lib.pairs.second[hydra.core.Type, hydra.context.Context])(dfltResult)
      {
        val foldResult: Either[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.core.Type],
           hydra.context.Context]] = hydra.lib.lists.foldl[Either[hydra.context.InContext[hydra.error.Error],
           Tuple2[Seq[hydra.core.Type], hydra.context.Context]], hydra.core.Term]((acc: Either[hydra.context.InContext[hydra.error.Error],
           Tuple2[Seq[hydra.core.Type], hydra.context.Context]]) =>
          (term: hydra.core.Term) =>
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.core.Type],
             hydra.context.Context], Tuple2[Seq[hydra.core.Type], hydra.context.Context]](acc)((accR: Tuple2[Seq[hydra.core.Type],
             hydra.context.Context]) =>
          {
          val types: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.context.Context](accR)
          {
            val cxA: hydra.context.Context = hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.context.Context](accR)
            hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type,
               hydra.context.Context], Tuple2[Seq[hydra.core.Type], hydra.context.Context]](hydra.checking.typeOf(cxA)(tx)(Seq())(term))((tResult: Tuple2[hydra.core.Type,
               hydra.context.Context]) =>
              {
              val t: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](tResult)
              {
                val cxB: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Type, hydra.context.Context](tResult)
                Right(Tuple2(hydra.lib.lists.concat2[hydra.core.Type](types)(hydra.lib.lists.pure[hydra.core.Type](t)), cxB))
              }
            })
          }
        }))(Right(Tuple2(Seq(), cx2)))(cterms)
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.core.Type],
           hydra.context.Context], Tuple2[hydra.core.Type, hydra.context.Context]](foldResult)((foldR: Tuple2[Seq[hydra.core.Type],
           hydra.context.Context]) =>
          {
          val tcterms: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.context.Context](foldR)
          {
            val cx3: hydra.context.Context = hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.context.Context](foldR)
            {
              val fcodsResult: Either[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.core.Type],
                 hydra.context.Context]] = hydra.lib.lists.foldl[Either[hydra.context.InContext[hydra.error.Error],
                 Tuple2[Seq[hydra.core.Type], hydra.context.Context]], hydra.core.Type]((acc: Either[hydra.context.InContext[hydra.error.Error],
                 Tuple2[Seq[hydra.core.Type], hydra.context.Context]]) =>
                (t: hydra.core.Type) =>
                hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.core.Type],
                   hydra.context.Context], Tuple2[Seq[hydra.core.Type], hydra.context.Context]](acc)((accR: Tuple2[Seq[hydra.core.Type],
                   hydra.context.Context]) =>
                {
                val cods: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.context.Context](accR)
                hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.FunctionType,
                   Tuple2[Seq[hydra.core.Type], hydra.context.Context]](hydra.extract.core.functionType(cx3)(t))((ft: hydra.core.FunctionType) =>
                  Right(Tuple2(hydra.lib.lists.concat2[hydra.core.Type](cods)(hydra.lib.lists.pure[hydra.core.Type](ft.codomain)), cx3)))
              }))(Right(Tuple2(Seq(), cx3)))(tcterms)
              hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.core.Type],
                 hydra.context.Context], Tuple2[hydra.core.Type, hydra.context.Context]](fcodsResult)((fcodsR: Tuple2[Seq[hydra.core.Type],
                 hydra.context.Context]) =>
                {
                val fcods: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.context.Context](fcodsR)
                {
                  val cods: Seq[hydra.core.Type] = hydra.lib.maybes.cat[hydra.core.Type](hydra.lib.lists.cons[Option[hydra.core.Type]](tdflt)(hydra.lib.lists.map[hydra.core.Type,
                     Option[hydra.core.Type]](hydra.lib.maybes.pure[hydra.core.Type])(fcods)))
                  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type,
                     Tuple2[hydra.core.Type, hydra.context.Context]](hydra.checking.checkSameType(cx3)(tx)("case branches")(cods))((cod: hydra.core.Type) =>
                    Right(Tuple2(hydra.core.Type.function(hydra.core.FunctionType(hydra.schemas.nominalApplication(tname)(typeArgs), cod)), cx3)))
                }
              })
            }
          }
        })
      }
    }
  })
}

def typeOfEither(cx: hydra.context.Context)(tx: hydra.graph.Graph)(typeArgs: Seq[hydra.core.Type])(et: Either[hydra.core.Term,
   hydra.core.Term]): Either[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context]] =
  {
  val n: Int = hydra.lib.lists.length[hydra.core.Type](typeArgs)
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context]]](hydra.lib.equality.equal[Int](n)(2))(hydra.lib.eithers.either[hydra.core.Term,
     hydra.core.Term, Either[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context]]]((leftTerm: hydra.core.Term) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context],
       Tuple2[hydra.core.Type, hydra.context.Context]](hydra.checking.typeOf(cx)(tx)(Seq())(leftTerm))((result: Tuple2[hydra.core.Type,
       hydra.context.Context]) =>
    {
    val leftType: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](result)
    {
      val cx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Type, hydra.context.Context](result)
      Right(Tuple2(hydra.core.Type.either(hydra.core.EitherType(leftType, hydra.lib.lists.at[hydra.core.Type](1)(typeArgs))), cx2))
    }
  }))((rightTerm: hydra.core.Term) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context],
       Tuple2[hydra.core.Type, hydra.context.Context]](hydra.checking.typeOf(cx)(tx)(Seq())(rightTerm))((result: Tuple2[hydra.core.Type,
       hydra.context.Context]) =>
    {
    val rightType: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](result)
    {
      val cx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Type, hydra.context.Context](result)
      Right(Tuple2(hydra.core.Type.either(hydra.core.EitherType(hydra.lib.lists.at[hydra.core.Type](0)(typeArgs), rightType)), cx2))
    }
  }))(et))(Left(hydra.context.InContext(hydra.error.Error.checking(hydra.error.CheckingError.typeArityMismatch(hydra.error.TypeArityMismatchError(hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.unit,
     hydra.core.Type.unit)), 2, n, typeArgs))), cx)))
}

def typeOfInjection(cx: hydra.context.Context)(tx: hydra.graph.Graph)(typeArgs: Seq[hydra.core.Type])(injection: hydra.core.Injection): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[hydra.core.Type, hydra.context.Context]] =
  {
  val tname: hydra.core.Name = (injection.typeName)
  val field: hydra.core.Field = (injection.field)
  val fname: hydra.core.Name = (field.name)
  val fterm: hydra.core.Term = (field.term)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.TypeScheme, hydra.context.Context],
     Tuple2[hydra.core.Type, hydra.context.Context]](hydra.schemas.requireSchemaType(cx)(tx.schemaTypes)(tname))((schemaResult: Tuple2[hydra.core.TypeScheme,
     hydra.context.Context]) =>
    {
    val schemaType: hydra.core.TypeScheme = hydra.lib.pairs.first[hydra.core.TypeScheme, hydra.context.Context](schemaResult)
    {
      val cx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.TypeScheme, hydra.context.Context](schemaResult)
      {
        val svars: Seq[hydra.core.Name] = (schemaType.variables)
        {
          val sbody: hydra.core.Type = (schemaType.`type`)
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.core.FieldType],
             Tuple2[hydra.core.Type, hydra.context.Context]](hydra.extract.core.unionType(cx2)(tname)(sbody))((sfields: Seq[hydra.core.FieldType]) =>
            hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, Tuple2[hydra.core.Type,
               hydra.context.Context]](hydra.schemas.findFieldType(cx2)(fname)(sfields))((ftyp: hydra.core.Type) =>
            Right(Tuple2(hydra.schemas.nominalApplication(tname)(typeArgs), cx2))))
        }
      }
    }
  })
}

def typeOfLambda(cx: hydra.context.Context)(tx: hydra.graph.Graph)(typeArgs: Seq[hydra.core.Type])(l: hydra.core.Lambda): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[hydra.core.Type, hydra.context.Context]] =
  {
  val v: hydra.core.Name = (l.parameter)
  val mdom: Option[hydra.core.Type] = (l.domain)
  val body: hydra.core.Term = (l.body)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context],
     Tuple2[hydra.core.Type, hydra.context.Context]](hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.error.Error],
     Tuple2[hydra.core.Type, hydra.context.Context]], hydra.core.Type](Left(hydra.context.InContext(hydra.error.Error.checking(hydra.error.CheckingError.untypedLambda(hydra.error.UntypedLambdaError())),
     cx)))((dom: hydra.core.Type) =>
    {
    val types2: Map[hydra.core.Name, hydra.core.TypeScheme] = hydra.lib.maps.insert[hydra.core.Name, hydra.core.TypeScheme](v)(hydra.rewriting.fTypeToTypeScheme(dom))(tx.boundTypes)
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context],
       Tuple2[hydra.core.Type, hydra.context.Context]](hydra.checking.typeOf(cx)(hydra.graph.Graph(tx.boundTerms,
       types2, (tx.classConstraints), (tx.lambdaVariables), (tx.metadata), (tx.primitives), (tx.schemaTypes),
       (tx.typeVariables)))(Seq())(body))((codResult: Tuple2[hydra.core.Type, hydra.context.Context]) =>
      {
      val cod: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](codResult)
      {
        val cx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Type, hydra.context.Context](codResult)
        Right(Tuple2(hydra.core.Type.function(hydra.core.FunctionType(dom, cod)), cx2))
      }
    })
  })(mdom))((tbodyResult: Tuple2[hydra.core.Type, hydra.context.Context]) =>
    {
    val tbody: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](tbodyResult)
    {
      val cx3: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Type, hydra.context.Context](tbodyResult)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, Tuple2[hydra.core.Type,
         hydra.context.Context]](hydra.checking.applyTypeArgumentsToType(cx3)(tx)(typeArgs)(tbody))((applied: hydra.core.Type) => Right(Tuple2(applied,
         cx3)))
    }
  })
}

def typeOfLet(cx: hydra.context.Context)(tx: hydra.graph.Graph)(typeArgs: Seq[hydra.core.Type])(letTerm: hydra.core.Let): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[hydra.core.Type, hydra.context.Context]] =
  {
  val bs: Seq[hydra.core.Binding] = (letTerm.bindings)
  val body: hydra.core.Term = (letTerm.body)
  val bnames: Seq[hydra.core.Name] = hydra.lib.lists.map[hydra.core.Binding, hydra.core.Name]((x: hydra.core.Binding) => (x.name))(bs)
  def bindingType(b: hydra.core.Binding): Either[hydra.context.InContext[hydra.error.Error], hydra.core.Type] =
    hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.error.Error], hydra.core.Type], hydra.core.TypeScheme](Left(hydra.context.InContext(hydra.error.Error.checking(hydra.error.CheckingError.untypedLetBinding(hydra.error.UntypedLetBindingError(b))),
       cx)))((ts: hydra.core.TypeScheme) => Right(hydra.rewriting.typeSchemeToFType(ts)))(b.`type`)
  val btypesResult: Either[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.core.Type], Unit]] = hydra.lib.lists.foldl[Either[hydra.context.InContext[hydra.error.Error],
     Tuple2[Seq[hydra.core.Type], Unit]], hydra.core.Binding]((acc: Either[hydra.context.InContext[hydra.error.Error],
     Tuple2[Seq[hydra.core.Type], Unit]]) =>
    (b: hydra.core.Binding) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.core.Type], Unit],
       Tuple2[Seq[hydra.core.Type], Unit]](acc)((accR: Tuple2[Seq[hydra.core.Type], Unit]) =>
    {
    val types: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], Unit](accR)
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, Tuple2[Seq[hydra.core.Type],
       Unit]](bindingType(b))((btype: hydra.core.Type) =>
      Right(Tuple2(hydra.lib.lists.concat2[hydra.core.Type](types)(hydra.lib.lists.pure[hydra.core.Type](btype)), ())))
  }))(Right(Tuple2(Seq(), ())))(bs)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.core.Type], Unit],
     Tuple2[hydra.core.Type, hydra.context.Context]](btypesResult)((btypesR: Tuple2[Seq[hydra.core.Type],
     Unit]) =>
    {
    val btypes: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], Unit](btypesR)
    {
      val tx2: hydra.graph.Graph = hydra.graph.Graph(tx.boundTerms, hydra.lib.maps.union[hydra.core.Name,
         hydra.core.TypeScheme](hydra.lib.maps.fromList[hydra.core.Name, hydra.core.TypeScheme](hydra.lib.lists.zip[hydra.core.Name,
         hydra.core.TypeScheme](bnames)(hydra.lib.lists.map[hydra.core.Type, hydra.core.TypeScheme](hydra.rewriting.fTypeToTypeScheme)(btypes))))(tx.boundTypes),
         (tx.classConstraints), (tx.lambdaVariables), (tx.metadata), (tx.primitives), (tx.schemaTypes),
         (tx.typeVariables))
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context],
         Tuple2[hydra.core.Type, hydra.context.Context]](hydra.checking.typeOf(cx)(tx2)(Seq())(body))((tResult: Tuple2[hydra.core.Type,
         hydra.context.Context]) =>
        {
        val t: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](tResult)
        {
          val cx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Type, hydra.context.Context](tResult)
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, Tuple2[hydra.core.Type,
             hydra.context.Context]](hydra.checking.applyTypeArgumentsToType(cx2)(tx)(typeArgs)(t))((applied: hydra.core.Type) => Right(Tuple2(applied,
             cx2)))
        }
      })
    }
  })
}

def typeOfList(cx: hydra.context.Context)(tx: hydra.graph.Graph)(typeArgs: Seq[hydra.core.Type])(els: Seq[hydra.core.Term]): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[hydra.core.Type, hydra.context.Context]] =
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context]]](hydra.lib.lists.`null`[hydra.core.Term](els))(hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
     Tuple2[hydra.core.Type, hydra.context.Context]]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Type](typeArgs))(1))(Right(Tuple2(hydra.core.Type.list(hydra.lib.lists.head[hydra.core.Type](typeArgs)),
     cx)))(Left(hydra.context.InContext(hydra.error.Error.checking(hydra.error.CheckingError.typeArityMismatch(hydra.error.TypeArityMismatchError(hydra.core.Type.list(hydra.core.Type.unit),
     1, hydra.lib.lists.length[hydra.core.Type](typeArgs), typeArgs))), cx))))({
  val foldResult: Either[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.core.Type], hydra.context.Context]] = hydra.lib.lists.foldl[Either[hydra.context.InContext[hydra.error.Error],
     Tuple2[Seq[hydra.core.Type], hydra.context.Context]], hydra.core.Term]((acc: Either[hydra.context.InContext[hydra.error.Error],
     Tuple2[Seq[hydra.core.Type], hydra.context.Context]]) =>
    (term: hydra.core.Term) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.core.Type], hydra.context.Context],
       Tuple2[Seq[hydra.core.Type], hydra.context.Context]](acc)((accR: Tuple2[Seq[hydra.core.Type], hydra.context.Context]) =>
    {
    val types: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.context.Context](accR)
    {
      val cxA: hydra.context.Context = hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.context.Context](accR)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context],
         Tuple2[Seq[hydra.core.Type], hydra.context.Context]](hydra.checking.typeOf(cxA)(tx)(Seq())(term))((tResult: Tuple2[hydra.core.Type,
         hydra.context.Context]) =>
        {
        val t: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](tResult)
        {
          val cxB: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Type, hydra.context.Context](tResult)
          Right(Tuple2(hydra.lib.lists.concat2[hydra.core.Type](types)(hydra.lib.lists.pure[hydra.core.Type](t)), cxB))
        }
      })
    }
  }))(Right(Tuple2(Seq(), cx)))(els)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.core.Type], hydra.context.Context],
     Tuple2[hydra.core.Type, hydra.context.Context]](foldResult)((foldR: Tuple2[Seq[hydra.core.Type],
     hydra.context.Context]) =>
    {
    val eltypes: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.context.Context](foldR)
    {
      val cx2: hydra.context.Context = hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.context.Context](foldR)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, Tuple2[hydra.core.Type,
         hydra.context.Context]](hydra.checking.checkSameType(cx2)(tx)("list elements")(eltypes))((unifiedType: hydra.core.Type) => Right(Tuple2(hydra.core.Type.list(unifiedType),
         cx2)))
    }
  })
})

def typeOfLiteral(cx: hydra.context.Context)(tx: hydra.graph.Graph)(typeArgs: Seq[hydra.core.Type])(lit: hydra.core.Literal): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[hydra.core.Type, hydra.context.Context]] =
  {
  val t: hydra.core.Type = hydra.core.Type.literal(hydra.reflect.literalType(lit))
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, Tuple2[hydra.core.Type,
     hydra.context.Context]](hydra.checking.applyTypeArgumentsToType(cx)(tx)(typeArgs)(t))((applied: hydra.core.Type) => Right(Tuple2(applied,
     cx)))
}

def typeOfMap(cx: hydra.context.Context)(tx: hydra.graph.Graph)(typeArgs: Seq[hydra.core.Type])(m: Map[hydra.core.Term,
   hydra.core.Term]): Either[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context]] =
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context]]](hydra.lib.maps.`null`[hydra.core.Term,
     hydra.core.Term](m))(hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type,
     hydra.context.Context]]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Type](typeArgs))(2))(Right(Tuple2(hydra.core.Type.map(hydra.core.MapType(hydra.lib.lists.at[hydra.core.Type](0)(typeArgs),
     hydra.lib.lists.at[hydra.core.Type](1)(typeArgs))), cx)))(Left(hydra.context.InContext(hydra.error.Error.checking(hydra.error.CheckingError.typeArityMismatch(hydra.error.TypeArityMismatchError(hydra.core.Type.map(hydra.core.MapType(hydra.core.Type.unit,
     hydra.core.Type.unit)), 2, hydra.lib.lists.length[hydra.core.Type](typeArgs), typeArgs))), cx))))({
  val pairs: Seq[Tuple2[hydra.core.Term, hydra.core.Term]] = hydra.lib.maps.toList[hydra.core.Term, hydra.core.Term](m)
  {
    val keyFoldResult: Either[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.core.Type],
       hydra.context.Context]] = hydra.lib.lists.foldl[Either[hydra.context.InContext[hydra.error.Error],
       Tuple2[Seq[hydra.core.Type], hydra.context.Context]], Tuple2[hydra.core.Term, hydra.core.Term]]((acc: Either[hydra.context.InContext[hydra.error.Error],
       Tuple2[Seq[hydra.core.Type], hydra.context.Context]]) =>
      (p: Tuple2[hydra.core.Term, hydra.core.Term]) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.core.Type],
         hydra.context.Context], Tuple2[Seq[hydra.core.Type], hydra.context.Context]](acc)((accR: Tuple2[Seq[hydra.core.Type],
         hydra.context.Context]) =>
      {
      val types: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.context.Context](accR)
      {
        val cxA: hydra.context.Context = hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.context.Context](accR)
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context],
           Tuple2[Seq[hydra.core.Type], hydra.context.Context]](hydra.checking.typeOf(cxA)(tx)(Seq())(hydra.lib.pairs.first[hydra.core.Term,
           hydra.core.Term](p)))((tResult: Tuple2[hydra.core.Type, hydra.context.Context]) =>
          {
          val t: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](tResult)
          {
            val cxB: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Type, hydra.context.Context](tResult)
            Right(Tuple2(hydra.lib.lists.concat2[hydra.core.Type](types)(hydra.lib.lists.pure[hydra.core.Type](t)), cxB))
          }
        })
      }
    }))(Right(Tuple2(Seq(), cx)))(pairs)
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.core.Type], hydra.context.Context],
       Tuple2[hydra.core.Type, hydra.context.Context]](keyFoldResult)((keyFoldR: Tuple2[Seq[hydra.core.Type],
       hydra.context.Context]) =>
      {
      val keyTypes: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.context.Context](keyFoldR)
      {
        val cx2: hydra.context.Context = hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.context.Context](keyFoldR)
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, Tuple2[hydra.core.Type,
           hydra.context.Context]](hydra.checking.checkSameType(cx2)(tx)("map keys")(keyTypes))((kt: hydra.core.Type) =>
          {
          val valFoldResult: Either[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.core.Type],
             hydra.context.Context]] = hydra.lib.lists.foldl[Either[hydra.context.InContext[hydra.error.Error],
             Tuple2[Seq[hydra.core.Type], hydra.context.Context]], Tuple2[hydra.core.Term, hydra.core.Term]]((acc: Either[hydra.context.InContext[hydra.error.Error],
             Tuple2[Seq[hydra.core.Type], hydra.context.Context]]) =>
            (p: Tuple2[hydra.core.Term, hydra.core.Term]) =>
            hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.core.Type],
               hydra.context.Context], Tuple2[Seq[hydra.core.Type], hydra.context.Context]](acc)((accR: Tuple2[Seq[hydra.core.Type],
               hydra.context.Context]) =>
            {
            val types: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.context.Context](accR)
            {
              val cxA: hydra.context.Context = hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.context.Context](accR)
              hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type,
                 hydra.context.Context], Tuple2[Seq[hydra.core.Type], hydra.context.Context]](hydra.checking.typeOf(cxA)(tx)(Seq())(hydra.lib.pairs.second[hydra.core.Term,
                 hydra.core.Term](p)))((tResult: Tuple2[hydra.core.Type, hydra.context.Context]) =>
                {
                val t: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](tResult)
                {
                  val cxB: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Type, hydra.context.Context](tResult)
                  Right(Tuple2(hydra.lib.lists.concat2[hydra.core.Type](types)(hydra.lib.lists.pure[hydra.core.Type](t)), cxB))
                }
              })
            }
          }))(Right(Tuple2(Seq(), cx2)))(pairs)
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.core.Type],
             hydra.context.Context], Tuple2[hydra.core.Type, hydra.context.Context]](valFoldResult)((valFoldR: Tuple2[Seq[hydra.core.Type],
             hydra.context.Context]) =>
            {
            val valTypes: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.context.Context](valFoldR)
            {
              val cx3: hydra.context.Context = hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.context.Context](valFoldR)
              hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, Tuple2[hydra.core.Type,
                 hydra.context.Context]](hydra.checking.checkSameType(cx3)(tx)("map values")(valTypes))((vt: hydra.core.Type) =>
                hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, Tuple2[hydra.core.Type,
                   hydra.context.Context]](hydra.checking.applyTypeArgumentsToType(cx3)(tx)(typeArgs)(hydra.core.Type.map(hydra.core.MapType(kt,
                   vt))))((applied: hydra.core.Type) => Right(Tuple2(applied, cx3))))
            }
          })
        })
      }
    })
  }
})

def typeOfMaybe(cx: hydra.context.Context)(tx: hydra.graph.Graph)(typeArgs: Seq[hydra.core.Type])(mt: Option[hydra.core.Term]): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[hydra.core.Type, hydra.context.Context]] =
  {
  val forNothing: Either[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context]] = {
    val n: Int = hydra.lib.lists.length[hydra.core.Type](typeArgs)
    hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type,
       hydra.context.Context]]](hydra.lib.equality.equal[Int](n)(1))(Right(Tuple2(hydra.core.Type.maybe(hydra.lib.lists.head[hydra.core.Type](typeArgs)),
       cx)))(Left(hydra.context.InContext(hydra.error.Error.checking(hydra.error.CheckingError.typeArityMismatch(hydra.error.TypeArityMismatchError(hydra.core.Type.maybe(hydra.core.Type.unit),
       1, n, typeArgs))), cx)))
  }
  def forJust(term: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context]] =
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context],
       Tuple2[hydra.core.Type, hydra.context.Context]](hydra.checking.typeOf(cx)(tx)(Seq())(term))((tResult: Tuple2[hydra.core.Type,
       hydra.context.Context]) =>
    {
    val termType: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](tResult)
    {
      val cx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Type, hydra.context.Context](tResult)
      {
        val t: hydra.core.Type = hydra.core.Type.maybe(termType)
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, Tuple2[hydra.core.Type,
           hydra.context.Context]](hydra.checking.applyTypeArgumentsToType(cx2)(tx)(typeArgs)(t))((applied: hydra.core.Type) => Right(Tuple2(applied,
           cx2)))
      }
    }
  })
  hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context]],
     hydra.core.Term](forNothing)(forJust)(mt)
}

def typeOfPair(cx: hydra.context.Context)(tx: hydra.graph.Graph)(typeArgs: Seq[hydra.core.Type])(p: Tuple2[hydra.core.Term,
   hydra.core.Term]): Either[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context]] =
  {
  val n: Int = hydra.lib.lists.length[hydra.core.Type](typeArgs)
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context]]](hydra.lib.equality.equal[Int](n)(2))({
    val pairFst: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](p)
    {
      val pairSnd: hydra.core.Term = hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](p)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context],
         Tuple2[hydra.core.Type, hydra.context.Context]](hydra.checking.typeOf(cx)(tx)(Seq())(pairFst))((result1: Tuple2[hydra.core.Type,
         hydra.context.Context]) =>
        {
        val firstType: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](result1)
        {
          val cx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Type, hydra.context.Context](result1)
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context],
             Tuple2[hydra.core.Type, hydra.context.Context]](hydra.checking.typeOf(cx2)(tx)(Seq())(pairSnd))((result2: Tuple2[hydra.core.Type,
             hydra.context.Context]) =>
            {
            val secondType: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](result2)
            {
              val cx3: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Type, hydra.context.Context](result2)
              Right(Tuple2(hydra.core.Type.pair(hydra.core.PairType(firstType, secondType)), cx3))
            }
          })
        }
      })
    }
  })(Left(hydra.context.InContext(hydra.error.Error.checking(hydra.error.CheckingError.typeArityMismatch(hydra.error.TypeArityMismatchError(hydra.core.Type.pair(hydra.core.PairType(hydra.core.Type.unit,
     hydra.core.Type.unit)), 2, n, typeArgs))), cx)))
}

def typeOfPrimitive(cx: hydra.context.Context)(tx: hydra.graph.Graph)(typeArgs: Seq[hydra.core.Type])(name: hydra.core.Name): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[hydra.core.Type, hydra.context.Context]] =
  {
  val rawTs: Option[hydra.core.TypeScheme] = hydra.lib.maybes.map[hydra.graph.Primitive, hydra.core.TypeScheme]((_p: hydra.graph.Primitive) => (_p.`type`))(hydra.lib.maps.lookup[hydra.core.Name,
     hydra.graph.Primitive](name)(tx.primitives))
  hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context]],
     hydra.core.TypeScheme](Left(hydra.context.InContext(hydra.error.Error.undefinedTerm(hydra.error.UndefinedTermError(name)),
     cx)))((tsRaw: hydra.core.TypeScheme) =>
    {
    val instResult: Tuple2[hydra.core.TypeScheme, hydra.context.Context] = hydra.schemas.instantiateTypeScheme(cx)(tsRaw)
    {
      val ts: hydra.core.TypeScheme = hydra.lib.pairs.first[hydra.core.TypeScheme, hydra.context.Context](instResult)
      {
        val cx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.TypeScheme, hydra.context.Context](instResult)
        {
          val t: hydra.core.Type = hydra.rewriting.typeSchemeToFType(ts)
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, Tuple2[hydra.core.Type,
             hydra.context.Context]](hydra.checking.applyTypeArgumentsToType(cx2)(tx)(typeArgs)(t))((applied: hydra.core.Type) => Right(Tuple2(applied,
             cx2)))
        }
      }
    }
  })(rawTs)
}

def typeOfProjection(cx: hydra.context.Context)(tx: hydra.graph.Graph)(typeArgs: Seq[hydra.core.Type])(p: hydra.core.Projection): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[hydra.core.Type, hydra.context.Context]] =
  {
  val tname: hydra.core.Name = (p.typeName)
  val fname: hydra.core.Name = (p.field)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.TypeScheme, hydra.context.Context],
     Tuple2[hydra.core.Type, hydra.context.Context]](hydra.schemas.requireSchemaType(cx)(tx.schemaTypes)(tname))((schemaResult: Tuple2[hydra.core.TypeScheme,
     hydra.context.Context]) =>
    {
    val schemaType: hydra.core.TypeScheme = hydra.lib.pairs.first[hydra.core.TypeScheme, hydra.context.Context](schemaResult)
    {
      val cx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.TypeScheme, hydra.context.Context](schemaResult)
      {
        val svars: Seq[hydra.core.Name] = (schemaType.variables)
        {
          val sbody: hydra.core.Type = (schemaType.`type`)
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.core.FieldType],
             Tuple2[hydra.core.Type, hydra.context.Context]](hydra.extract.core.recordType(cx2)(tname)(sbody))((sfields: Seq[hydra.core.FieldType]) =>
            hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, Tuple2[hydra.core.Type,
               hydra.context.Context]](hydra.schemas.findFieldType(cx2)(fname)(sfields))((ftyp: hydra.core.Type) =>
            {
            val subst: hydra.typing.TypeSubst = hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Type](hydra.lib.lists.zip[hydra.core.Name,
               hydra.core.Type](svars)(typeArgs))
            {
              val sftyp: hydra.core.Type = hydra.substitution.substInType(subst)(ftyp)
              Right(Tuple2(hydra.core.Type.function(hydra.core.FunctionType(hydra.schemas.nominalApplication(tname)(typeArgs), sftyp)), cx2))
            }
          }))
        }
      }
    }
  })
}

def typeOfRecord(cx: hydra.context.Context)(tx: hydra.graph.Graph)(typeArgs: Seq[hydra.core.Type])(record: hydra.core.Record): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[hydra.core.Type, hydra.context.Context]] =
  {
  val tname: hydra.core.Name = (record.typeName)
  val fields: Seq[hydra.core.Field] = (record.fields)
  val foldResult: Either[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.core.Type], hydra.context.Context]] = hydra.lib.lists.foldl[Either[hydra.context.InContext[hydra.error.Error],
     Tuple2[Seq[hydra.core.Type], hydra.context.Context]], hydra.core.Term]((acc: Either[hydra.context.InContext[hydra.error.Error],
     Tuple2[Seq[hydra.core.Type], hydra.context.Context]]) =>
    (term: hydra.core.Term) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.core.Type], hydra.context.Context],
       Tuple2[Seq[hydra.core.Type], hydra.context.Context]](acc)((accR: Tuple2[Seq[hydra.core.Type], hydra.context.Context]) =>
    {
    val types: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.context.Context](accR)
    {
      val cxA: hydra.context.Context = hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.context.Context](accR)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context],
         Tuple2[Seq[hydra.core.Type], hydra.context.Context]](hydra.checking.typeOf(cxA)(tx)(Seq())(term))((tResult: Tuple2[hydra.core.Type,
         hydra.context.Context]) =>
        {
        val t: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](tResult)
        {
          val cxB: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Type, hydra.context.Context](tResult)
          Right(Tuple2(hydra.lib.lists.concat2[hydra.core.Type](types)(hydra.lib.lists.pure[hydra.core.Type](t)), cxB))
        }
      })
    }
  }))(Right(Tuple2(Seq(), cx)))(hydra.lib.lists.map[hydra.core.Field, hydra.core.Term]((x: hydra.core.Field) => (x.term))(fields))
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.core.Type], hydra.context.Context],
     Tuple2[hydra.core.Type, hydra.context.Context]](foldResult)((foldR: Tuple2[Seq[hydra.core.Type],
     hydra.context.Context]) =>
    {
    val cx2: hydra.context.Context = hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.context.Context](foldR)
    Right(Tuple2(hydra.schemas.nominalApplication(tname)(typeArgs), cx2))
  })
}

def typeOfSet(cx: hydra.context.Context)(tx: hydra.graph.Graph)(typeArgs: Seq[hydra.core.Type])(els: scala.collection.immutable.Set[hydra.core.Term]): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[hydra.core.Type, hydra.context.Context]] =
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context]]](hydra.lib.sets.`null`[hydra.core.Term](els))(hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
     Tuple2[hydra.core.Type, hydra.context.Context]]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Type](typeArgs))(1))(Right(Tuple2(hydra.core.Type.set(hydra.lib.lists.head[hydra.core.Type](typeArgs)),
     cx)))(Left(hydra.context.InContext(hydra.error.Error.checking(hydra.error.CheckingError.typeArityMismatch(hydra.error.TypeArityMismatchError(hydra.core.Type.set(hydra.core.Type.unit),
     1, hydra.lib.lists.length[hydra.core.Type](typeArgs), typeArgs))), cx))))({
  val foldResult: Either[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.core.Type], hydra.context.Context]] = hydra.lib.lists.foldl[Either[hydra.context.InContext[hydra.error.Error],
     Tuple2[Seq[hydra.core.Type], hydra.context.Context]], hydra.core.Term]((acc: Either[hydra.context.InContext[hydra.error.Error],
     Tuple2[Seq[hydra.core.Type], hydra.context.Context]]) =>
    (term: hydra.core.Term) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.core.Type], hydra.context.Context],
       Tuple2[Seq[hydra.core.Type], hydra.context.Context]](acc)((accR: Tuple2[Seq[hydra.core.Type], hydra.context.Context]) =>
    {
    val types: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.context.Context](accR)
    {
      val cxA: hydra.context.Context = hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.context.Context](accR)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context],
         Tuple2[Seq[hydra.core.Type], hydra.context.Context]](hydra.checking.typeOf(cxA)(tx)(Seq())(term))((tResult: Tuple2[hydra.core.Type,
         hydra.context.Context]) =>
        {
        val t: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](tResult)
        {
          val cxB: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Type, hydra.context.Context](tResult)
          Right(Tuple2(hydra.lib.lists.concat2[hydra.core.Type](types)(hydra.lib.lists.pure[hydra.core.Type](t)), cxB))
        }
      })
    }
  }))(Right(Tuple2(Seq(), cx)))(hydra.lib.sets.toList[hydra.core.Term](els))
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.core.Type], hydra.context.Context],
     Tuple2[hydra.core.Type, hydra.context.Context]](foldResult)((foldR: Tuple2[Seq[hydra.core.Type],
     hydra.context.Context]) =>
    {
    val eltypes: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.context.Context](foldR)
    {
      val cx2: hydra.context.Context = hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.context.Context](foldR)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, Tuple2[hydra.core.Type,
         hydra.context.Context]](hydra.checking.checkSameType(cx2)(tx)("set elements")(eltypes))((unifiedType: hydra.core.Type) => Right(Tuple2(hydra.core.Type.set(unifiedType),
         cx2)))
    }
  })
})

def typeOfTypeApplication(cx: hydra.context.Context)(tx: hydra.graph.Graph)(typeArgs: Seq[hydra.core.Type])(tyapp: hydra.core.TypeApplicationTerm): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[hydra.core.Type, hydra.context.Context]] =
  {
  val body: hydra.core.Term = (tyapp.body)
  val t: hydra.core.Type = (tyapp.`type`)
  hydra.checking.typeOf(cx)(tx)(hydra.lib.lists.cons[hydra.core.Type](t)(typeArgs))(body)
}

def typeOfTypeLambda(cx: hydra.context.Context)(tx: hydra.graph.Graph)(typeArgs: Seq[hydra.core.Type])(tl: hydra.core.TypeLambda): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[hydra.core.Type, hydra.context.Context]] =
  {
  val v: hydra.core.Name = (tl.parameter)
  val body: hydra.core.Term = (tl.body)
  val vars: scala.collection.immutable.Set[hydra.core.Name] = (tx.typeVariables)
  val tx2: hydra.graph.Graph = hydra.graph.Graph(tx.boundTerms, (tx.boundTypes), (tx.classConstraints),
     (tx.lambdaVariables), (tx.metadata), (tx.primitives), (tx.schemaTypes), hydra.lib.sets.insert[hydra.core.Name](v)(vars))
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context],
     Tuple2[hydra.core.Type, hydra.context.Context]](hydra.checking.typeOf(cx)(tx2)(Seq())(body))((result1: Tuple2[hydra.core.Type,
     hydra.context.Context]) =>
    {
    val t1: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](result1)
    {
      val cx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Type, hydra.context.Context](result1)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, Tuple2[hydra.core.Type,
         hydra.context.Context]](hydra.checking.applyTypeArgumentsToType(cx2)(tx)(typeArgs)(hydra.core.Type.forall(hydra.core.ForallType(v,
         t1))))((applied: hydra.core.Type) => Right(Tuple2(applied, cx2)))
    }
  })
}

def typeOfUnit(cx: hydra.context.Context)(tx: hydra.graph.Graph)(typeArgs: Seq[hydra.core.Type]): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[hydra.core.Type, hydra.context.Context]] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, Tuple2[hydra.core.Type,
     hydra.context.Context]](hydra.checking.applyTypeArgumentsToType(cx)(tx)(typeArgs)(hydra.core.Type.unit))((applied: hydra.core.Type) => Right(Tuple2(applied,
     cx)))

def typeOfUnwrap(cx: hydra.context.Context)(tx: hydra.graph.Graph)(typeArgs: Seq[hydra.core.Type])(tname: hydra.core.Name): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[hydra.core.Type, hydra.context.Context]] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.TypeScheme, hydra.context.Context],
     Tuple2[hydra.core.Type, hydra.context.Context]](hydra.schemas.requireSchemaType(cx)(tx.schemaTypes)(tname))((schemaResult: Tuple2[hydra.core.TypeScheme,
     hydra.context.Context]) =>
  {
  val schemaType: hydra.core.TypeScheme = hydra.lib.pairs.first[hydra.core.TypeScheme, hydra.context.Context](schemaResult)
  {
    val cx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.TypeScheme, hydra.context.Context](schemaResult)
    {
      val svars: Seq[hydra.core.Name] = (schemaType.variables)
      {
        val sbody: hydra.core.Type = (schemaType.`type`)
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, Tuple2[hydra.core.Type,
           hydra.context.Context]](hydra.extract.core.wrappedType(cx2)(tname)(sbody))((wrapped: hydra.core.Type) =>
          {
          val subst: hydra.typing.TypeSubst = hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Type](hydra.lib.lists.zip[hydra.core.Name,
             hydra.core.Type](svars)(typeArgs))
          {
            val swrapped: hydra.core.Type = hydra.substitution.substInType(subst)(wrapped)
            Right(Tuple2(hydra.core.Type.function(hydra.core.FunctionType(hydra.schemas.nominalApplication(tname)(typeArgs), swrapped)), cx2))
          }
        })
      }
    }
  }
})

def typeOfVariable(cx: hydra.context.Context)(tx: hydra.graph.Graph)(typeArgs: Seq[hydra.core.Type])(name: hydra.core.Name): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[hydra.core.Type, hydra.context.Context]] =
  {
  val rawTypeScheme: Option[hydra.core.TypeScheme] = hydra.lib.maps.lookup[hydra.core.Name, hydra.core.TypeScheme](name)(tx.boundTypes)
  hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context]],
     hydra.core.TypeScheme](Left(hydra.context.InContext(hydra.error.Error.undefinedType(hydra.error.UndefinedTypeError(name)),
     cx)))((ts: hydra.core.TypeScheme) =>
    {
    val tResult: Tuple2[hydra.core.Type, hydra.context.Context] = hydra.lib.logic.ifElse[Tuple2[hydra.core.Type,
       hydra.context.Context]](hydra.lib.lists.`null`[hydra.core.Type](typeArgs))(hydra.schemas.instantiateType(cx)(hydra.rewriting.typeSchemeToFType(ts)))(Tuple2(hydra.rewriting.typeSchemeToFType(ts),
       cx))
    {
      val t: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](tResult)
      {
        val cx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Type, hydra.context.Context](tResult)
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, Tuple2[hydra.core.Type,
           hydra.context.Context]](hydra.checking.applyTypeArgumentsToType(cx2)(tx)(typeArgs)(t))((applied: hydra.core.Type) => Right(Tuple2(applied,
           cx2)))
      }
    }
  })(rawTypeScheme)
}

def typeOfWrappedTerm(cx: hydra.context.Context)(tx: hydra.graph.Graph)(typeArgs: Seq[hydra.core.Type])(wt: hydra.core.WrappedTerm): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[hydra.core.Type, hydra.context.Context]] =
  {
  val tname: hydra.core.Name = (wt.typeName)
  val body: hydra.core.Term = (wt.body)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.core.Type, hydra.context.Context],
     Tuple2[hydra.core.Type, hydra.context.Context]](hydra.checking.typeOf(cx)(tx)(Seq())(body))((result: Tuple2[hydra.core.Type,
     hydra.context.Context]) =>
    {
    val cx2: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Type, hydra.context.Context](result)
    Right(Tuple2(hydra.schemas.nominalApplication(tname)(typeArgs), cx2))
  })
}

def typesAllEffectivelyEqual(tx: hydra.graph.Graph)(tlist: Seq[hydra.core.Type]): Boolean =
  {
  val types: Map[hydra.core.Name, hydra.core.TypeScheme] = (tx.schemaTypes)
  def containsFreeVar(t: hydra.core.Type): Boolean =
    {
    val allVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.rewriting.freeVariablesInTypeSimple(t)
    val schemaNames: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.maps.keys[hydra.core.Name,
       hydra.core.TypeScheme](types))
    hydra.lib.logic.not(hydra.lib.sets.`null`[hydra.core.Name](hydra.lib.sets.difference[hydra.core.Name](allVars)(schemaNames)))
  }
  val anyContainsFreeVar: Boolean = hydra.lib.lists.foldl[Boolean, hydra.core.Type]((acc: Boolean) =>
    (t: hydra.core.Type) => hydra.lib.logic.or(acc)(containsFreeVar(t)))(false)(tlist)
  hydra.lib.logic.ifElse[Boolean](anyContainsFreeVar)(true)(hydra.lib.logic.ifElse[Boolean](hydra.checking.allEqual(hydra.lib.lists.map[hydra.core.Type,
     hydra.core.Type]((t: hydra.core.Type) => hydra.checking.normalizeTypeFreeVars(t))(tlist)))(true)(hydra.checking.allEqual(hydra.lib.lists.map[hydra.core.Type,
     hydra.core.Type]((t: hydra.core.Type) =>
    hydra.checking.normalizeTypeFreeVars(hydra.rewriting.deannotateTypeRecursive(hydra.rewriting.replaceTypedefs(types)(t))))(tlist))))
}

def typesEffectivelyEqual(tx: hydra.graph.Graph)(t1: hydra.core.Type)(t2: hydra.core.Type): Boolean =
  hydra.lib.logic.or(hydra.checking.containsInScopeTypeVars(tx)(t1))(hydra.lib.logic.or(hydra.checking.containsInScopeTypeVars(tx)(t2))(hydra.checking.typesAllEffectivelyEqual(tx)(Seq(hydra.schemas.fullyStripAndNormalizeType(t1),
     hydra.schemas.fullyStripAndNormalizeType(t2)))))
