package hydra.lisp.coder

import hydra.core.*

import hydra.lisp.syntax.*

import hydra.packaging.*

def dialectCadr(d: hydra.lisp.syntax.Dialect): scala.Predef.String =
  d match
  case hydra.lisp.syntax.Dialect.clojure => "second"
  case _ => "cadr"

def dialectCar(d: hydra.lisp.syntax.Dialect): scala.Predef.String =
  d match
  case hydra.lisp.syntax.Dialect.clojure => "first"
  case _ => "car"

def dialectConstructorPrefix(d: hydra.lisp.syntax.Dialect): scala.Predef.String =
  d match
  case hydra.lisp.syntax.Dialect.clojure => "->"
  case _ => "make-"

def dialectEqual(d: hydra.lisp.syntax.Dialect): scala.Predef.String =
  d match
  case hydra.lisp.syntax.Dialect.clojure => "="
  case hydra.lisp.syntax.Dialect.commonLisp => "equal"
  case hydra.lisp.syntax.Dialect.emacsLisp => "equal"
  case _ => "equal?"

def encodeApplication[T0, T1, T2](dialect: hydra.lisp.syntax.Dialect)(cx: T0)(g: T1)(rawFun: hydra.core.Term)(rawArg: hydra.core.Term): Either[T2,
   hydra.lisp.syntax.Expression] =
  {
  lazy val dFun: hydra.core.Term = hydra.strip.deannotateTerm(rawFun)
  lazy val normal: Either[T2, hydra.lisp.syntax.Expression] = hydra.lib.eithers.bind[T2,
     hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(rawFun))((fun: hydra.lisp.syntax.Expression) =>
    hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(rawArg))((arg: hydra.lisp.syntax.Expression) => Right(hydra.lisp.coder.lispApp(fun)(Seq(arg)))))
  def enc(t: hydra.core.Term): Either[T2, hydra.lisp.syntax.Expression] = hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(t)
  dFun match
    case hydra.core.Term.application(v_Term_application_app2) => {
      lazy val midFun: hydra.core.Term = (v_Term_application_app2.function)
      {
        lazy val midArg: hydra.core.Term = (v_Term_application_app2.argument)
        {
          lazy val dMidFun: hydra.core.Term = hydra.strip.deannotateTerm(midFun)
          {
            lazy val isLazy2: Boolean = hydra.lib.logic.or(hydra.lisp.coder.isPrimitiveRef("hydra.lib.eithers.fromLeft")(dMidFun))(hydra.lib.logic.or(hydra.lisp.coder.isPrimitiveRef("hydra.lib.eithers.fromRight")(dMidFun))(hydra.lisp.coder.isPrimitiveRef("hydra.lib.maybes.fromMaybe")(dMidFun)))
            hydra.lib.logic.ifElse[Either[T2, hydra.lisp.syntax.Expression]](isLazy2)(hydra.lib.eithers.bind[T2,
               hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](enc(midFun))((ePrim: hydra.lisp.syntax.Expression) =>
              hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](enc(midArg))((eDef: hydra.lisp.syntax.Expression) =>
              hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](enc(rawArg))((eArg: hydra.lisp.syntax.Expression) =>
              Right(hydra.lisp.coder.lispApp(hydra.lisp.coder.lispApp(ePrim)(Seq(hydra.lisp.coder.wrapInThunk(eDef))))(Seq(eArg)))))))(dMidFun match
              case hydra.core.Term.application(v_Term_application_app3) => {
                lazy val innerFun: hydra.core.Term = (v_Term_application_app3.function)
                {
                  lazy val innerArg: hydra.core.Term = (v_Term_application_app3.argument)
                  {
                    lazy val dInnerFun: hydra.core.Term = hydra.strip.deannotateTerm(innerFun)
                    hydra.lib.logic.ifElse[Either[T2, hydra.lisp.syntax.Expression]](hydra.lisp.coder.isPrimitiveRef("hydra.lib.logic.ifElse")(dInnerFun))(hydra.lib.eithers.bind[T2,
                       hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](enc(innerArg))((eC: hydra.lisp.syntax.Expression) =>
                      hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](enc(midArg))((eT: hydra.lisp.syntax.Expression) =>
                      hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](enc(rawArg))((eE: hydra.lisp.syntax.Expression) =>
                      Right(hydra.lisp.syntax.Expression.`if`(hydra.lisp.syntax.IfExpression(eC,
                         eT, Some(eE))))))))(hydra.lib.logic.ifElse[Either[T2, hydra.lisp.syntax.Expression]](hydra.lisp.coder.isPrimitiveRef("hydra.lib.maybes.maybe")(dInnerFun))(hydra.lib.eithers.bind[T2,
                         hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](enc(innerFun))((eP: hydra.lisp.syntax.Expression) =>
                      hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](enc(innerArg))((eDef: hydra.lisp.syntax.Expression) =>
                      hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](enc(midArg))((eF: hydra.lisp.syntax.Expression) =>
                      hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](enc(rawArg))((eM: hydra.lisp.syntax.Expression) =>
                      Right(hydra.lisp.coder.lispApp(hydra.lisp.coder.lispApp(hydra.lisp.coder.lispApp(eP)(Seq(hydra.lisp.coder.wrapInThunk(eDef))))(Seq(eF)))(Seq(eM))))))))(hydra.lib.logic.ifElse[Either[T2,
                         hydra.lisp.syntax.Expression]](hydra.lisp.coder.isPrimitiveRef("hydra.lib.maybes.cases")(dInnerFun))(hydra.lib.eithers.bind[T2,
                         hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](enc(innerFun))((eP: hydra.lisp.syntax.Expression) =>
                      hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](enc(innerArg))((eM: hydra.lisp.syntax.Expression) =>
                      hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](enc(midArg))((eN: hydra.lisp.syntax.Expression) =>
                      hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](enc(rawArg))((eJ: hydra.lisp.syntax.Expression) =>
                      Right(hydra.lisp.coder.lispApp(hydra.lisp.coder.lispApp(hydra.lisp.coder.lispApp(eP)(Seq(eM)))(Seq(hydra.lisp.coder.wrapInThunk(eN))))(Seq(eJ))))))))(normal)))
                  }
                }
              }
              case _ => normal)
          }
        }
      }
    }
    case _ => normal
}

def encodeFieldDef(ft: hydra.core.FieldType): hydra.lisp.syntax.FieldDefinition =
  {
  lazy val fname: scala.Predef.String = (ft.name)
  hydra.lisp.syntax.FieldDefinition(hydra.formatting.convertCaseCamelToLowerSnake(fname), None)
}

def encodeLambdaTerm[T0, T1, T2](dialect: hydra.lisp.syntax.Dialect)(cx: T0)(g: T1)(lam: hydra.core.Lambda): Either[T2,
   hydra.lisp.syntax.Expression] =
  {
  lazy val param: scala.Predef.String = hydra.formatting.convertCaseCamelOrUnderscoreToLowerSnake(hydra.formatting.sanitizeWithUnderscores(hydra.lisp.language.lispReservedWords)(lam.parameter))
  hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(lam.body))((body: hydra.lisp.syntax.Expression) => Right(hydra.lisp.coder.lispLambdaExpr(Seq(param))(body)))
}

def encodeLetAsLambdaApp[T0, T1, T2](dialect: hydra.lisp.syntax.Dialect)(cx: T0)(g: T1)(bindings: Seq[hydra.core.Binding])(body: hydra.core.Term): Either[T2,
   hydra.lisp.syntax.Expression] =
  hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(body))((bodyExpr: hydra.lisp.syntax.Expression) =>
  hydra.lib.eithers.foldl[hydra.lisp.syntax.Expression, hydra.core.Binding, T2]((acc: hydra.lisp.syntax.Expression) =>
  (b: hydra.core.Binding) =>
  {
  lazy val bname: scala.Predef.String = hydra.formatting.convertCaseCamelOrUnderscoreToLowerSnake(hydra.formatting.sanitizeWithUnderscores(hydra.lisp.language.lispReservedWords)(b.name))
  hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(b.term))((bval: hydra.lisp.syntax.Expression) =>
    Right(hydra.lisp.coder.lispApp(hydra.lisp.coder.lispLambdaExpr(Seq(bname))(acc))(Seq(bval))))
})(bodyExpr)(hydra.lib.lists.reverse[hydra.core.Binding](bindings)))

def encodeLetAsNative[T0, T1, T2](dialect: hydra.lisp.syntax.Dialect)(cx: T0)(g: T1)(bindings: Seq[hydra.core.Binding])(body: hydra.core.Term): Either[T2,
   hydra.lisp.syntax.Expression] =
  {
  lazy val isClojureTop: Boolean = dialect match
    case hydra.lisp.syntax.Dialect.clojure => true
    case _ => false
  hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(body))((bodyExpr: hydra.lisp.syntax.Expression) =>
    {
    lazy val sortedBindings: Seq[hydra.core.Binding] = hydra.lib.logic.ifElse[Seq[hydra.core.Binding]](true)({
      lazy val allNames: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.lists.map[hydra.core.Binding,
         hydra.core.Name]((b: hydra.core.Binding) => (b.name))(bindings))
      lazy val adjList: Seq[Tuple2[hydra.core.Name, Seq[hydra.core.Name]]] = hydra.lib.lists.map[hydra.core.Binding,
         Tuple2[hydra.core.Name, Seq[hydra.core.Name]]]((b: hydra.core.Binding) =>
        Tuple2(b.name, hydra.lib.sets.toList[hydra.core.Name](hydra.lib.sets.intersection[hydra.core.Name](allNames)(hydra.variables.freeVariablesInTerm(b.term)))))(bindings)
      lazy val sortResult: Either[Seq[Seq[hydra.core.Name]], Seq[hydra.core.Name]] = hydra.sorting.topologicalSort(adjList)
      lazy val nameToBinding: Map[hydra.core.Name, hydra.core.Binding] = hydra.lib.maps.fromList[hydra.core.Name,
         hydra.core.Binding](hydra.lib.lists.map[hydra.core.Binding, Tuple2[hydra.core.Name,
         hydra.core.Binding]]((b: hydra.core.Binding) => Tuple2(b.name, b))(bindings))
      hydra.lib.eithers.either[Seq[Seq[hydra.core.Name]], Seq[hydra.core.Name], Seq[hydra.core.Binding]]((_x: Seq[Seq[hydra.core.Name]]) => bindings)((sorted: Seq[hydra.core.Name]) =>
        hydra.lib.maybes.cat[hydra.core.Binding](hydra.lib.lists.map[hydra.core.Name,
           Option[hydra.core.Binding]]((name: hydra.core.Name) =>
        hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Binding](name)(nameToBinding))(sorted)))(sortResult)
    })(bindings)
    hydra.lib.eithers.bind[T2, Seq[Tuple2[scala.Predef.String, hydra.lisp.syntax.Expression]],
       hydra.lisp.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Binding,
       Tuple2[scala.Predef.String, hydra.lisp.syntax.Expression], T2]((b: hydra.core.Binding) =>
      {
      lazy val bname: scala.Predef.String = hydra.formatting.convertCaseCamelOrUnderscoreToLowerSnake(hydra.formatting.sanitizeWithUnderscores(hydra.lisp.language.lispReservedWords)(b.name))
      {
        lazy val isSelfRef: Boolean = hydra.lib.sets.member[hydra.core.Name](b.name)(hydra.variables.freeVariablesInTerm(b.term))
        {
          lazy val isLambda: Boolean = hydra.strip.deannotateTerm(b.term) match
            case hydra.core.Term.lambda(v_Term_lambda__) => true
            case _ => false
          hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, Tuple2[scala.Predef.String,
             hydra.lisp.syntax.Expression]](hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(b.term))((bval: hydra.lisp.syntax.Expression) =>
            {
            lazy val isClojure: Boolean = dialect match
              case hydra.lisp.syntax.Dialect.clojure => true
              case _ => false
            {
              lazy val wrappedVal: hydra.lisp.syntax.Expression = hydra.lib.logic.ifElse[hydra.lisp.syntax.Expression](isClojure)(hydra.lib.logic.ifElse[hydra.lisp.syntax.Expression](isSelfRef)(hydra.lib.logic.ifElse[hydra.lisp.syntax.Expression](isLambda)(bval match
                case hydra.lisp.syntax.Expression.lambda(v_Expression_lambda_lam) => hydra.lisp.syntax.Expression.lambda(hydra.lisp.syntax.Lambda(Some(bname),
                   (v_Expression_lambda_lam.params), (v_Expression_lambda_lam.restParam),
                   (v_Expression_lambda_lam.body)))
                case _ => bval)(hydra.lisp.coder.lispNamedLambdaExpr(bname)(Seq("_arg"))(hydra.lisp.coder.lispApp(bval)(Seq(hydra.lisp.coder.lispVar("_arg"))))))(bval))(hydra.lib.logic.ifElse[hydra.lisp.syntax.Expression](hydra.lib.logic.and(isSelfRef)(hydra.lib.logic.not(isLambda)))(hydra.lisp.coder.lispLambdaExpr(Seq("_arg"))(hydra.lisp.coder.lispApp(bval)(Seq(hydra.lisp.coder.lispVar("_arg")))))(bval))
              Right(Tuple2(bname, wrappedVal))
            }
          })
        }
      }
    })(sortedBindings))((encodedBindings: Seq[Tuple2[scala.Predef.String, hydra.lisp.syntax.Expression]]) =>
      {
      lazy val allBindingNames: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.lists.map[hydra.core.Binding,
         hydra.core.Name]((b: hydra.core.Binding) => (b.name))(bindings))
      {
        lazy val hasCrossRefs: Boolean = hydra.lib.lists.foldl[Boolean, hydra.core.Binding]((acc: Boolean) =>
          (b: hydra.core.Binding) =>
          hydra.lib.logic.or(acc)(hydra.lib.logic.not(hydra.lib.sets.`null`[hydra.core.Name](hydra.lib.sets.intersection[hydra.core.Name](allBindingNames)(hydra.variables.freeVariablesInTerm(b.term))))))(false)(bindings)
        {
          lazy val hasSelfRef: Boolean = hydra.lib.lists.foldl[Boolean, hydra.core.Binding]((acc: Boolean) =>
            (b: hydra.core.Binding) =>
            hydra.lib.logic.or(acc)(hydra.lib.sets.member[hydra.core.Name](b.name)(hydra.variables.freeVariablesInTerm(b.term))))(false)(bindings)
          {
            lazy val isRecursive: Boolean = hasSelfRef
            {
              lazy val letKind: hydra.lisp.syntax.LetKind = hydra.lib.logic.ifElse[hydra.lisp.syntax.LetKind](isRecursive)(hydra.lisp.syntax.LetKind.recursive)(hydra.lib.logic.ifElse[hydra.lisp.syntax.LetKind](hydra.lib.equality.lte[Int](hydra.lib.lists.length[hydra.core.Binding](bindings))(1))(hydra.lisp.syntax.LetKind.parallel)(hydra.lisp.syntax.LetKind.sequential))
              {
                lazy val lispBindings: Seq[hydra.lisp.syntax.LetBinding] = hydra.lib.lists.map[Tuple2[scala.Predef.String,
                   hydra.lisp.syntax.Expression], hydra.lisp.syntax.LetBinding]((eb: Tuple2[scala.Predef.String,
                   hydra.lisp.syntax.Expression]) =>
                  hydra.lisp.syntax.LetBinding.simple(hydra.lisp.syntax.SimpleBinding(hydra.lib.pairs.first[scala.Predef.String,
                     hydra.lisp.syntax.Expression](eb), hydra.lib.pairs.second[scala.Predef.String,
                     hydra.lisp.syntax.Expression](eb))))(encodedBindings)
                Right(hydra.lisp.syntax.Expression.let(hydra.lisp.syntax.LetExpression(letKind,
                   lispBindings, Seq(bodyExpr))))
              }
            }
          }
        }
      }
    })
  })
}

def encodeLiteral(lit: hydra.core.Literal): hydra.lisp.syntax.Expression =
  lit match
  case hydra.core.Literal.boolean(v_Literal_boolean_b) => hydra.lisp.syntax.Expression.literal(hydra.lisp.syntax.Literal.boolean(v_Literal_boolean_b))
  case hydra.core.Literal.decimal(v_Literal_decimal_d) => hydra.lisp.syntax.Expression.literal(hydra.lisp.syntax.Literal.float(hydra.lisp.syntax.FloatLiteral(hydra.lib.literals.float64ToBigfloat(hydra.lib.literals.decimalToFloat64(v_Literal_decimal_d)),
     None)))
  case hydra.core.Literal.string(v_Literal_string_s) => hydra.lisp.syntax.Expression.literal(hydra.lisp.syntax.Literal.string(v_Literal_string_s))
  case hydra.core.Literal.float(v_Literal_float_fv) => v_Literal_float_fv match
    case hydra.core.FloatValue.float32(v_FloatValue_float32_f) => hydra.lisp.syntax.Expression.literal(hydra.lisp.syntax.Literal.float(hydra.lisp.syntax.FloatLiteral(hydra.lib.literals.float32ToBigfloat(v_FloatValue_float32_f),
       None)))
    case hydra.core.FloatValue.float64(v_FloatValue_float64_f) => hydra.lisp.syntax.Expression.literal(hydra.lisp.syntax.Literal.float(hydra.lisp.syntax.FloatLiteral(hydra.lib.literals.float64ToBigfloat(v_FloatValue_float64_f),
       None)))
    case hydra.core.FloatValue.bigfloat(v_FloatValue_bigfloat_f) => hydra.lisp.syntax.Expression.literal(hydra.lisp.syntax.Literal.float(hydra.lisp.syntax.FloatLiteral(v_FloatValue_bigfloat_f,
       None)))
  case hydra.core.Literal.integer(v_Literal_integer_iv) => v_Literal_integer_iv match
    case hydra.core.IntegerValue.int8(v_IntegerValue_int8_i) => hydra.lisp.syntax.Expression.literal(hydra.lisp.syntax.Literal.integer(hydra.lisp.syntax.IntegerLiteral(hydra.lib.literals.int8ToBigint(v_IntegerValue_int8_i),
       false)))
    case hydra.core.IntegerValue.int16(v_IntegerValue_int16_i) => hydra.lisp.syntax.Expression.literal(hydra.lisp.syntax.Literal.integer(hydra.lisp.syntax.IntegerLiteral(hydra.lib.literals.int16ToBigint(v_IntegerValue_int16_i),
       false)))
    case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => hydra.lisp.syntax.Expression.literal(hydra.lisp.syntax.Literal.integer(hydra.lisp.syntax.IntegerLiteral(hydra.lib.literals.int32ToBigint(v_IntegerValue_int32_i),
       false)))
    case hydra.core.IntegerValue.int64(v_IntegerValue_int64_i) => hydra.lisp.syntax.Expression.literal(hydra.lisp.syntax.Literal.integer(hydra.lisp.syntax.IntegerLiteral(hydra.lib.literals.int64ToBigint(v_IntegerValue_int64_i),
       false)))
    case hydra.core.IntegerValue.uint8(v_IntegerValue_uint8_i) => hydra.lisp.syntax.Expression.literal(hydra.lisp.syntax.Literal.integer(hydra.lisp.syntax.IntegerLiteral(hydra.lib.literals.uint8ToBigint(v_IntegerValue_uint8_i),
       false)))
    case hydra.core.IntegerValue.uint16(v_IntegerValue_uint16_i) => hydra.lisp.syntax.Expression.literal(hydra.lisp.syntax.Literal.integer(hydra.lisp.syntax.IntegerLiteral(hydra.lib.literals.uint16ToBigint(v_IntegerValue_uint16_i),
       false)))
    case hydra.core.IntegerValue.uint32(v_IntegerValue_uint32_i) => hydra.lisp.syntax.Expression.literal(hydra.lisp.syntax.Literal.integer(hydra.lisp.syntax.IntegerLiteral(hydra.lib.literals.uint32ToBigint(v_IntegerValue_uint32_i),
       false)))
    case hydra.core.IntegerValue.uint64(v_IntegerValue_uint64_i) => hydra.lisp.syntax.Expression.literal(hydra.lisp.syntax.Literal.integer(hydra.lisp.syntax.IntegerLiteral(hydra.lib.literals.uint64ToBigint(v_IntegerValue_uint64_i),
       false)))
    case hydra.core.IntegerValue.bigint(v_IntegerValue_bigint_i) => hydra.lisp.syntax.Expression.literal(hydra.lisp.syntax.Literal.integer(hydra.lisp.syntax.IntegerLiteral(v_IntegerValue_bigint_i,
       true)))
  case hydra.core.Literal.binary(v_Literal_binary_b) => {
    lazy val byteValues: Seq[Int] = hydra.lib.literals.binaryToBytes(v_Literal_binary_b)
    hydra.lisp.syntax.Expression.vector(hydra.lisp.syntax.VectorLiteral(hydra.lib.lists.map[Int,
       hydra.lisp.syntax.Expression]((bv: Int) =>
      hydra.lisp.syntax.Expression.literal(hydra.lisp.syntax.Literal.integer(hydra.lisp.syntax.IntegerLiteral(hydra.lib.literals.int32ToBigint(bv),
         false))))(byteValues)))
  }

def encodeProjectionElim[T0, T1, T2](dialect: hydra.lisp.syntax.Dialect)(cx: T0)(g: T1)(proj: hydra.core.Projection)(marg: Option[hydra.core.Term]): Either[T2,
   hydra.lisp.syntax.Expression] =
  {
  lazy val fname: scala.Predef.String = hydra.formatting.convertCaseCamelToLowerSnake(proj.field)
  lazy val tname: scala.Predef.String = hydra.lisp.coder.qualifiedSnakeName(proj.typeName)
  hydra.lib.maybes.cases[hydra.core.Term, Either[T2, hydra.lisp.syntax.Expression]](marg)(Right(hydra.lisp.coder.lispLambdaExpr(Seq("v"))(hydra.lisp.syntax.Expression.fieldAccess(hydra.lisp.syntax.FieldAccess(tname,
     fname, hydra.lisp.coder.lispVar("v"))))))((arg: hydra.core.Term) =>
    hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(arg))((sarg: hydra.lisp.syntax.Expression) =>
    Right(hydra.lisp.syntax.Expression.fieldAccess(hydra.lisp.syntax.FieldAccess(tname, fname, sarg)))))
}

def encodeTerm[T0, T1, T2](dialect: hydra.lisp.syntax.Dialect)(cx: T0)(g: T1)(term: hydra.core.Term): Either[T2,
   hydra.lisp.syntax.Expression] =
  term match
  case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(v_Term_annotated_at.body)
  case hydra.core.Term.application(v_Term_application_app) => {
    lazy val rawFun: hydra.core.Term = (v_Term_application_app.function)
    {
      lazy val rawArg: hydra.core.Term = (v_Term_application_app.argument)
      hydra.lisp.coder.encodeApplication(dialect)(cx)(g)(rawFun)(rawArg)
    }
  }
  case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.either[hydra.core.Term,
     hydra.core.Term, Either[T2, hydra.lisp.syntax.Expression]]((l: hydra.core.Term) =>
    hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(l))((sl: hydra.lisp.syntax.Expression) =>
    Right(hydra.lisp.coder.lispApp(hydra.lisp.coder.lispVar("list"))(Seq(hydra.lisp.coder.lispKeyword("left"),
       sl)))))((r: hydra.core.Term) =>
    hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(r))((sr: hydra.lisp.syntax.Expression) =>
    Right(hydra.lisp.coder.lispApp(hydra.lisp.coder.lispVar("list"))(Seq(hydra.lisp.coder.lispKeyword("right"),
       sr)))))(v_Term_either_e)
  case hydra.core.Term.lambda(v_Term_lambda_lam) => hydra.lisp.coder.encodeLambdaTerm(dialect)(cx)(g)(v_Term_lambda_lam)
  case hydra.core.Term.project(v_Term_project_proj) => hydra.lisp.coder.encodeProjectionElim(dialect)(cx)(g)(v_Term_project_proj)(None)
  case hydra.core.Term.cases(v_Term_cases_cs) => hydra.lisp.coder.encodeUnionElim(dialect)(cx)(g)(v_Term_cases_cs)(None)
  case hydra.core.Term.unwrap(v_Term_unwrap_name) => hydra.lisp.coder.encodeUnwrapElim(dialect)(cx)(g)(v_Term_unwrap_name)(None)
  case hydra.core.Term.let(v_Term_let_lt) => {
    lazy val bindings: Seq[hydra.core.Binding] = (v_Term_let_lt.bindings)
    {
      lazy val body: hydra.core.Term = (v_Term_let_lt.body)
      hydra.lisp.coder.encodeLetAsNative(dialect)(cx)(g)(bindings)(body)
    }
  }
  case hydra.core.Term.list(v_Term_list_els) => hydra.lib.eithers.bind[T2, Seq[hydra.lisp.syntax.Expression],
     hydra.lisp.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Term, hydra.lisp.syntax.Expression,
     T2]((v1: hydra.core.Term) => hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(v1))(v_Term_list_els))((sels: Seq[hydra.lisp.syntax.Expression]) => Right(hydra.lisp.coder.lispListExpr(sels)))
  case hydra.core.Term.literal(v_Term_literal_lit) => Right(hydra.lisp.coder.encodeLiteral(v_Term_literal_lit))
  case hydra.core.Term.map(v_Term_map_m) => hydra.lib.eithers.bind[T2, Seq[hydra.lisp.syntax.MapEntry],
     hydra.lisp.syntax.Expression](hydra.lib.eithers.mapList[Tuple2[hydra.core.Term,
     hydra.core.Term], hydra.lisp.syntax.MapEntry, T2]((entry: Tuple2[hydra.core.Term,
     hydra.core.Term]) =>
    hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, hydra.lisp.syntax.MapEntry](hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(hydra.lib.pairs.first[hydra.core.Term,
       hydra.core.Term](entry)))((k: hydra.lisp.syntax.Expression) =>
    hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, hydra.lisp.syntax.MapEntry](hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(hydra.lib.pairs.second[hydra.core.Term,
       hydra.core.Term](entry)))((v: hydra.lisp.syntax.Expression) => Right(hydra.lisp.syntax.MapEntry(k,
       v)))))(hydra.lib.maps.toList[hydra.core.Term, hydra.core.Term](v_Term_map_m)))((pairs: Seq[hydra.lisp.syntax.MapEntry]) =>
    Right(hydra.lisp.syntax.Expression.map(hydra.lisp.syntax.MapLiteral(pairs))))
  case hydra.core.Term.maybe(v_Term_maybe_mt) => hydra.lib.maybes.cases[hydra.core.Term,
     Either[T2, hydra.lisp.syntax.Expression]](v_Term_maybe_mt)(Right(hydra.lisp.coder.lispApp(hydra.lisp.coder.lispVar("list"))(Seq(hydra.lisp.coder.lispKeyword("nothing")))))((`val`: hydra.core.Term) =>
    hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(`val`))((sval: hydra.lisp.syntax.Expression) =>
    Right(hydra.lisp.coder.lispApp(hydra.lisp.coder.lispVar("list"))(Seq(hydra.lisp.coder.lispKeyword("just"), sval)))))
  case hydra.core.Term.pair(v_Term_pair_p) => hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression,
     hydra.lisp.syntax.Expression](hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(hydra.lib.pairs.first[hydra.core.Term,
     hydra.core.Term](v_Term_pair_p)))((f: hydra.lisp.syntax.Expression) =>
    hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(hydra.lib.pairs.second[hydra.core.Term,
       hydra.core.Term](v_Term_pair_p)))((s: hydra.lisp.syntax.Expression) => Right(hydra.lisp.coder.lispListExpr(Seq(f,
       s)))))
  case hydra.core.Term.record(v_Term_record_rec) => {
    lazy val rname: hydra.core.Name = (v_Term_record_rec.typeName)
    {
      lazy val fields: Seq[hydra.core.Field] = (v_Term_record_rec.fields)
      hydra.lib.eithers.bind[T2, Seq[hydra.lisp.syntax.Expression], hydra.lisp.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Field,
         hydra.lisp.syntax.Expression, T2]((f: hydra.core.Field) => hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(f.term))(fields))((sfields: Seq[hydra.lisp.syntax.Expression]) =>
        {
        lazy val constructorName: scala.Predef.String = hydra.lib.strings.cat2(hydra.lisp.coder.dialectConstructorPrefix(dialect))(hydra.lisp.coder.qualifiedSnakeName(rname))
        Right(hydra.lisp.coder.lispApp(hydra.lisp.coder.lispVar(constructorName))(sfields))
      })
    }
  }
  case hydra.core.Term.set(v_Term_set_s) => hydra.lib.eithers.bind[T2, Seq[hydra.lisp.syntax.Expression],
     hydra.lisp.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Term, hydra.lisp.syntax.Expression,
     T2]((v1: hydra.core.Term) => hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(v1))(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_s)))((sels: Seq[hydra.lisp.syntax.Expression]) =>
    Right(hydra.lisp.syntax.Expression.set(hydra.lisp.syntax.SetLiteral(sels))))
  case hydra.core.Term.inject(v_Term_inject_inj) => {
    lazy val tname: scala.Predef.String = hydra.names.localNameOf(v_Term_inject_inj.typeName)
    {
      lazy val field: hydra.core.Field = (v_Term_inject_inj.field)
      {
        lazy val fname: scala.Predef.String = (field.name)
        {
          lazy val fterm: hydra.core.Term = (field.term)
          {
            lazy val dterm: hydra.core.Term = hydra.strip.deannotateTerm(fterm)
            {
              lazy val isUnit: Boolean = dterm match
                case hydra.core.Term.unit => true
                case hydra.core.Term.record(v_Term_record_rt) => hydra.lib.lists.`null`[hydra.core.Field](v_Term_record_rt.fields)
                case _ => false
              hydra.lib.logic.ifElse[Either[T2, hydra.lisp.syntax.Expression]](isUnit)(Right(hydra.lisp.coder.lispApp(hydra.lisp.coder.lispVar("list"))(Seq(hydra.lisp.coder.lispKeyword(hydra.formatting.convertCaseCamelToLowerSnake(fname)),
                 hydra.lisp.coder.lispNilExpr))))(hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression,
                 hydra.lisp.syntax.Expression](hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(fterm))((sval: hydra.lisp.syntax.Expression) =>
                Right(hydra.lisp.coder.lispApp(hydra.lisp.coder.lispVar("list"))(Seq(hydra.lisp.coder.lispKeyword(hydra.formatting.convertCaseCamelToLowerSnake(fname)),
                   sval)))))
            }
          }
        }
      }
    }
  }
  case hydra.core.Term.unit => Right(hydra.lisp.coder.lispNilExpr)
  case hydra.core.Term.variable(v_Term_variable_name) => Right(hydra.lisp.coder.lispVar(hydra.formatting.convertCaseCamelOrUnderscoreToLowerSnake(hydra.formatting.sanitizeWithUnderscores(hydra.lisp.language.lispReservedWords)(v_Term_variable_name))))
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(v_Term_typeApplication_ta.body)
  case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(v_Term_typeLambda_tl.body)
  case hydra.core.Term.wrap(v_Term_wrap_wt) => hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(v_Term_wrap_wt.body)

def encodeTermDefinition[T0, T1, T2](dialect: hydra.lisp.syntax.Dialect)(cx: T0)(g: T1)(tdef: hydra.packaging.TermDefinition): Either[T2,
   hydra.lisp.syntax.TopLevelFormWithComments] =
  {
  lazy val name: hydra.core.Name = (tdef.name)
  lazy val term: hydra.core.Term = (tdef.term)
  lazy val lname: scala.Predef.String = hydra.lisp.coder.qualifiedSnakeName(name)
  lazy val dterm: hydra.core.Term = hydra.strip.deannotateTerm(term)
  dterm match
    case hydra.core.Term.lambda(v_Term_lambda_lam) => hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression,
       hydra.lisp.syntax.TopLevelFormWithComments](hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(term))((sterm: hydra.lisp.syntax.Expression) =>
      Right(hydra.lisp.coder.lispTopForm(hydra.lisp.syntax.TopLevelForm.variable(hydra.lisp.syntax.VariableDefinition(lname,
         sterm, None)))))
    case _ => hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, hydra.lisp.syntax.TopLevelFormWithComments](hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(term))((sterm: hydra.lisp.syntax.Expression) =>
      Right(hydra.lisp.coder.lispTopForm(hydra.lisp.syntax.TopLevelForm.variable(hydra.lisp.syntax.VariableDefinition(lname,
         sterm, None)))))
}

def encodeType[T0, T1, T2](cx: T0)(g: T1)(t: hydra.core.Type): Either[T2, hydra.lisp.syntax.TypeSpecifier] =
  {
  lazy val typ: hydra.core.Type = hydra.strip.deannotateType(t)
  typ match
    case hydra.core.Type.annotated(v_Type_annotated_at) => hydra.lisp.coder.encodeType(cx)(g)(v_Type_annotated_at.body)
    case hydra.core.Type.application(v_Type_application_at) => hydra.lisp.coder.encodeType(cx)(g)(v_Type_application_at.function)
    case hydra.core.Type.unit => Right(hydra.lisp.syntax.TypeSpecifier.unit)
    case hydra.core.Type.literal(v_Type_literal_lt) => Right(v_Type_literal_lt match
      case hydra.core.LiteralType.binary => hydra.lisp.syntax.TypeSpecifier.named("ByteArray")
      case hydra.core.LiteralType.boolean => hydra.lisp.syntax.TypeSpecifier.named("Boolean")
      case hydra.core.LiteralType.decimal => hydra.lisp.syntax.TypeSpecifier.named("Decimal")
      case hydra.core.LiteralType.float(v_LiteralType_float__) => hydra.lisp.syntax.TypeSpecifier.named("Float")
      case hydra.core.LiteralType.integer(v_LiteralType_integer__) => hydra.lisp.syntax.TypeSpecifier.named("Integer")
      case hydra.core.LiteralType.string => hydra.lisp.syntax.TypeSpecifier.named("String"))
    case hydra.core.Type.list(v_Type_list_inner) => hydra.lib.eithers.map[hydra.lisp.syntax.TypeSpecifier,
       hydra.lisp.syntax.TypeSpecifier, T2]((enc: hydra.lisp.syntax.TypeSpecifier) => hydra.lisp.syntax.TypeSpecifier.list(enc))(hydra.lisp.coder.encodeType(cx)(g)(v_Type_list_inner))
    case hydra.core.Type.set(v_Type_set_inner) => hydra.lib.eithers.map[hydra.lisp.syntax.TypeSpecifier,
       hydra.lisp.syntax.TypeSpecifier, T2]((enc: hydra.lisp.syntax.TypeSpecifier) => hydra.lisp.syntax.TypeSpecifier.set(enc))(hydra.lisp.coder.encodeType(cx)(g)(v_Type_set_inner))
    case hydra.core.Type.map(v_Type_map_mt) => Right(hydra.lisp.syntax.TypeSpecifier.named("Map"))
    case hydra.core.Type.maybe(v_Type_maybe_inner) => hydra.lib.eithers.map[hydra.lisp.syntax.TypeSpecifier,
       hydra.lisp.syntax.TypeSpecifier, T2]((enc: hydra.lisp.syntax.TypeSpecifier) => hydra.lisp.syntax.TypeSpecifier.maybe(enc))(hydra.lisp.coder.encodeType(cx)(g)(v_Type_maybe_inner))
    case hydra.core.Type.either(v_Type_either_et) => Right(hydra.lisp.syntax.TypeSpecifier.named("Either"))
    case hydra.core.Type.pair(v_Type_pair_pt) => Right(hydra.lisp.syntax.TypeSpecifier.named("Pair"))
    case hydra.core.Type.function(v_Type_function_ft) => Right(hydra.lisp.syntax.TypeSpecifier.named("Function"))
    case hydra.core.Type.record(v_Type_record__) => Right(hydra.lisp.syntax.TypeSpecifier.named("Record"))
    case hydra.core.Type.union(v_Type_union__) => Right(hydra.lisp.syntax.TypeSpecifier.named("Union"))
    case hydra.core.Type.wrap(v_Type_wrap__) => Right(hydra.lisp.syntax.TypeSpecifier.named("Wrapper"))
    case hydra.core.Type.variable(v_Type_variable_name) => Right(hydra.lisp.syntax.TypeSpecifier.named(v_Type_variable_name))
    case hydra.core.Type.forall(v_Type_forall_fa) => hydra.lisp.coder.encodeType(cx)(g)(v_Type_forall_fa.body)
    case _ => Right(hydra.lisp.syntax.TypeSpecifier.named("Any"))
}

def encodeTypeBody[T0](lname: scala.Predef.String)(origTyp: hydra.core.Type)(typ: hydra.core.Type): Either[T0,
   hydra.lisp.syntax.TopLevelFormWithComments] =
  typ match
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.lisp.coder.encodeTypeBody(lname)(origTyp)(v_Type_forall_ft.body)
  case hydra.core.Type.record(v_Type_record_rt) => {
    lazy val fields: Seq[hydra.lisp.syntax.FieldDefinition] = hydra.lib.lists.map[hydra.core.FieldType,
       hydra.lisp.syntax.FieldDefinition](hydra.lisp.coder.encodeFieldDef)(v_Type_record_rt)
    Right(hydra.lisp.coder.lispTopForm(hydra.lisp.syntax.TopLevelForm.recordType(hydra.lisp.syntax.RecordTypeDefinition(lname,
       fields, None))))
  }
  case hydra.core.Type.union(v_Type_union_rt) => {
    lazy val variantNames: Seq[hydra.lisp.syntax.Expression] = hydra.lib.lists.map[hydra.core.FieldType,
       hydra.lisp.syntax.Expression]((f: hydra.core.FieldType) =>
      hydra.lisp.syntax.Expression.literal(hydra.lisp.syntax.Literal.keyword(hydra.lisp.syntax.Keyword(hydra.formatting.convertCaseCamelToLowerSnake(f.name),
         None))))(v_Type_union_rt)
    Right(hydra.lisp.coder.lispTopForm(hydra.lisp.syntax.TopLevelForm.variable(hydra.lisp.syntax.VariableDefinition(hydra.lib.strings.cat2(lname)("-variants"),
       hydra.lisp.coder.lispListExpr(variantNames), Some(hydra.lib.strings.cat2("Variants of the ")(lname))))))
  }
  case hydra.core.Type.wrap(v_Type_wrap_wt) => Right(hydra.lisp.coder.lispTopForm(hydra.lisp.syntax.TopLevelForm.recordType(hydra.lisp.syntax.RecordTypeDefinition(lname,
     Seq(hydra.lisp.syntax.FieldDefinition("value", None)), None))))
  case _ => Right(hydra.lisp.syntax.TopLevelFormWithComments(None, Some(hydra.lisp.syntax.Comment(hydra.lisp.syntax.CommentStyle.line,
     hydra.lib.strings.cat2(hydra.lib.strings.cat2(lname)(" = "))(hydra.show.core.`type`(origTyp)))),
     hydra.lisp.syntax.TopLevelForm.expression(hydra.lisp.syntax.Expression.literal(hydra.lisp.syntax.Literal.nil))))

def encodeTypeDefinition[T0, T1, T2](cx: T0)(g: T1)(tdef: hydra.packaging.TypeDefinition): Either[T2,
   hydra.lisp.syntax.TopLevelFormWithComments] =
  {
  lazy val name: hydra.core.Name = (tdef.name)
  lazy val typ: hydra.core.Type = (tdef.`type`.`type`)
  lazy val lname: scala.Predef.String = hydra.lisp.coder.qualifiedSnakeName(name)
  lazy val dtyp: hydra.core.Type = hydra.strip.deannotateType(typ)
  hydra.lisp.coder.encodeTypeBody(lname)(typ)(dtyp)
}

def encodeUnionElim[T0, T1, T2](dialect: hydra.lisp.syntax.Dialect)(cx: T0)(g: T1)(cs: hydra.core.CaseStatement)(marg: Option[hydra.core.Term]): Either[T2,
   hydra.lisp.syntax.Expression] =
  {
  lazy val tname: scala.Predef.String = hydra.names.localNameOf(cs.typeName)
  lazy val caseFields: Seq[hydra.core.Field] = (cs.cases)
  lazy val defCase: Option[hydra.core.Term] = (cs.default)
  hydra.lib.eithers.bind[T2, Seq[hydra.lisp.syntax.CondClause], hydra.lisp.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Field,
     hydra.lisp.syntax.CondClause, T2]((cf: hydra.core.Field) =>
    {
    lazy val cfname: scala.Predef.String = hydra.formatting.convertCaseCamelToLowerSnake(cf.name)
    {
      lazy val cfterm: hydra.core.Term = (cf.term)
      {
        lazy val condExpr: hydra.lisp.syntax.Expression = hydra.lisp.coder.lispApp(hydra.lisp.coder.lispVar(hydra.lisp.coder.dialectEqual(dialect)))(Seq(hydra.lisp.coder.lispApp(hydra.lisp.coder.lispVar(hydra.lisp.coder.dialectCar(dialect)))(Seq(hydra.lisp.coder.lispVar("match_target"))),
           hydra.lisp.coder.lispKeyword(cfname)))
        hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, hydra.lisp.syntax.CondClause](hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(hydra.core.Term.application(hydra.core.Application(cfterm,
           hydra.core.Term.variable("match_value")))))((bodyExpr: hydra.lisp.syntax.Expression) => Right(hydra.lisp.syntax.CondClause(condExpr,
           bodyExpr)))
      }
    }
  })(caseFields))((clauses: Seq[hydra.lisp.syntax.CondClause]) =>
    hydra.lib.eithers.bind[T2, Option[hydra.lisp.syntax.Expression], hydra.lisp.syntax.Expression](hydra.lib.maybes.cases[hydra.core.Term,
       Either[T2, Option[hydra.lisp.syntax.Expression]]](defCase)(Right(None))((dt: hydra.core.Term) =>
    hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, Option[hydra.lisp.syntax.Expression]](hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(dt))((defBody: hydra.lisp.syntax.Expression) => Right(Some(defBody)))))((defExpr: Option[hydra.lisp.syntax.Expression]) =>
    {
    lazy val condExpr: hydra.lisp.syntax.Expression = hydra.lisp.syntax.Expression.cond(hydra.lisp.syntax.CondExpression(clauses,
       defExpr))
    {
      lazy val innerExpr: hydra.lisp.syntax.Expression = hydra.lisp.coder.lispApp(hydra.lisp.coder.lispLambdaExpr(Seq("match_value"))(condExpr))(Seq(hydra.lisp.coder.lispApp(hydra.lisp.coder.lispVar(hydra.lisp.coder.dialectCadr(dialect)))(Seq(hydra.lisp.coder.lispVar("match_target")))))
      hydra.lib.maybes.cases[hydra.core.Term, Either[T2, hydra.lisp.syntax.Expression]](marg)(Right(hydra.lisp.coder.lispLambdaExpr(Seq("match_target"))(innerExpr)))((arg: hydra.core.Term) =>
        hydra.lib.eithers.bind[T2, hydra.lisp.syntax.Expression, hydra.lisp.syntax.Expression](hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(arg))((sarg: hydra.lisp.syntax.Expression) =>
        Right(hydra.lisp.coder.lispApp(hydra.lisp.coder.lispLambdaExpr(Seq("match_target"))(innerExpr))(Seq(sarg)))))
    }
  }))
}

def encodeUnwrapElim[T0, T1, T2](dialect: hydra.lisp.syntax.Dialect)(cx: T0)(g: T1)(name: hydra.core.Name)(marg: Option[hydra.core.Term]): Either[T2,
   hydra.lisp.syntax.Expression] =
  hydra.lib.maybes.cases[hydra.core.Term, Either[T2, hydra.lisp.syntax.Expression]](marg)(Right(hydra.lisp.coder.lispLambdaExpr(Seq("v"))(hydra.lisp.coder.lispVar("v"))))((arg: hydra.core.Term) => hydra.lisp.coder.encodeTerm(dialect)(cx)(g)(arg))

def isCasesPrimitive(name: hydra.core.Name): Boolean = hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.maybes.cases")

def isLazy2ArgPrimitive(name: hydra.core.Name): Boolean =
  hydra.lib.logic.or(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.eithers.fromLeft"))(hydra.lib.logic.or(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.eithers.fromRight"))(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.maybes.fromMaybe")))

def isLazy3ArgPrimitive(name: hydra.core.Name): Boolean = hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.maybes.maybe")

def isPrimitiveRef(primName: scala.Predef.String)(term: hydra.core.Term): Boolean =
  term match
  case hydra.core.Term.variable(v_Term_variable_name) => hydra.lib.equality.equal[scala.Predef.String](v_Term_variable_name)(primName)
  case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.lisp.coder.isPrimitiveRef(primName)(v_Term_annotated_at.body)
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => hydra.lisp.coder.isPrimitiveRef(primName)(v_Term_typeApplication_ta.body)
  case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.lisp.coder.isPrimitiveRef(primName)(v_Term_typeLambda_tl.body)
  case _ => false

def lispApp(fun: hydra.lisp.syntax.Expression)(args: Seq[hydra.lisp.syntax.Expression]): hydra.lisp.syntax.Expression =
  hydra.lisp.syntax.Expression.application(hydra.lisp.syntax.Application(fun, args))

def lispKeyword(name: scala.Predef.String): hydra.lisp.syntax.Expression =
  hydra.lisp.syntax.Expression.literal(hydra.lisp.syntax.Literal.keyword(hydra.lisp.syntax.Keyword(name, None)))

def lispLambdaExpr(params: Seq[scala.Predef.String])(body: hydra.lisp.syntax.Expression): hydra.lisp.syntax.Expression =
  hydra.lisp.syntax.Expression.lambda(hydra.lisp.syntax.Lambda(None, hydra.lib.lists.map[scala.Predef.String,
     hydra.lisp.syntax.Symbol]((p: scala.Predef.String) => p)(params), None, Seq(body)))

def lispListExpr(elements: Seq[hydra.lisp.syntax.Expression]): hydra.lisp.syntax.Expression =
  hydra.lisp.syntax.Expression.list(hydra.lisp.syntax.ListLiteral(elements, false))

def lispLitExpr(lit: hydra.lisp.syntax.Literal): hydra.lisp.syntax.Expression = hydra.lisp.syntax.Expression.literal(lit)

def lispNamedLambdaExpr(name: scala.Predef.String)(params: Seq[scala.Predef.String])(body: hydra.lisp.syntax.Expression): hydra.lisp.syntax.Expression =
  hydra.lisp.syntax.Expression.lambda(hydra.lisp.syntax.Lambda(Some(name), hydra.lib.lists.map[scala.Predef.String,
     hydra.lisp.syntax.Symbol]((p: scala.Predef.String) => p)(params), None, Seq(body)))

lazy val lispNilExpr: hydra.lisp.syntax.Expression = hydra.lisp.syntax.Expression.literal(hydra.lisp.syntax.Literal.nil)

def lispSymbol(name: scala.Predef.String): hydra.lisp.syntax.Symbol = name

def lispTopForm(form: hydra.lisp.syntax.TopLevelForm): hydra.lisp.syntax.TopLevelFormWithComments = hydra.lisp.syntax.TopLevelFormWithComments(None,
   None, form)

def lispTopFormWithComments(mdoc: Option[scala.Predef.String])(form: hydra.lisp.syntax.TopLevelForm): hydra.lisp.syntax.TopLevelFormWithComments =
  hydra.lisp.syntax.TopLevelFormWithComments(hydra.lib.maybes.map[scala.Predef.String,
     hydra.lisp.syntax.Docstring]((d: scala.Predef.String) => d)(mdoc), None, form)

def lispVar(name: scala.Predef.String): hydra.lisp.syntax.Expression =
  hydra.lisp.syntax.Expression.variable(hydra.lisp.syntax.VariableReference(name, false))

def moduleExports(forms: Seq[hydra.lisp.syntax.TopLevelFormWithComments]): Seq[hydra.lisp.syntax.ExportDeclaration] =
  {
  lazy val symbols: Seq[hydra.lisp.syntax.Symbol] = hydra.lib.lists.concat[hydra.lisp.syntax.Symbol](hydra.lib.lists.map[hydra.lisp.syntax.TopLevelFormWithComments,
     Seq[hydra.lisp.syntax.Symbol]]((fwc: hydra.lisp.syntax.TopLevelFormWithComments) =>
    {
    lazy val form: hydra.lisp.syntax.TopLevelForm = (fwc.form)
    form match
      case hydra.lisp.syntax.TopLevelForm.variable(v_TopLevelForm_variable_vd) => Seq(v_TopLevelForm_variable_vd.name)
      case hydra.lisp.syntax.TopLevelForm.recordType(v_TopLevelForm_recordType_rdef) => {
        lazy val rname: scala.Predef.String = (v_TopLevelForm_recordType_rdef.name)
        {
          lazy val fields: Seq[hydra.lisp.syntax.FieldDefinition] = (v_TopLevelForm_recordType_rdef.fields)
          {
            lazy val fieldSyms: Seq[hydra.lisp.syntax.Symbol] = hydra.lib.lists.map[hydra.lisp.syntax.FieldDefinition,
               hydra.lisp.syntax.Symbol]((f: hydra.lisp.syntax.FieldDefinition) =>
              {
              lazy val fn: scala.Predef.String = (f.name)
              hydra.lib.strings.cat(Seq(rname, "-", fn))
            })(fields)
            hydra.lib.lists.concat[hydra.lisp.syntax.Symbol](Seq(Seq(hydra.lib.strings.cat2("make-")(rname),
               hydra.lib.strings.cat2(rname)("?")), fieldSyms))
          }
        }
      }
      case _ => Seq()
  })(forms))
  hydra.lib.logic.ifElse[Seq[hydra.lisp.syntax.ExportDeclaration]](hydra.lib.lists.`null`[hydra.lisp.syntax.Symbol](symbols))(Seq())(Seq(hydra.lisp.syntax.ExportDeclaration(symbols)))
}

def moduleImports(focusNs: hydra.packaging.Namespace)(defs: Seq[hydra.packaging.Definition]): Seq[hydra.lisp.syntax.ImportDeclaration] =
  {
  lazy val depNss: Seq[hydra.packaging.Namespace] = hydra.lib.sets.toList[hydra.packaging.Namespace](hydra.lib.sets.delete[hydra.packaging.Namespace](focusNs)(hydra.analysis.definitionDependencyNamespaces(defs)))
  hydra.lib.lists.map[hydra.packaging.Namespace, hydra.lisp.syntax.ImportDeclaration]((ns: hydra.packaging.Namespace) =>
    hydra.lisp.syntax.ImportDeclaration(ns, hydra.lisp.syntax.ImportSpec.all))(depNss)
}

def moduleToLisp[T0, T1, T2](dialect: hydra.lisp.syntax.Dialect)(mod: hydra.packaging.Module)(defs0: Seq[hydra.packaging.Definition])(cx: T0)(g: T1): Either[T2,
   hydra.lisp.syntax.Program] =
  {
  lazy val defs: Seq[hydra.packaging.Definition] = hydra.environment.reorderDefs(defs0)
  lazy val partitioned: Tuple2[Seq[hydra.packaging.TypeDefinition], Seq[hydra.packaging.TermDefinition]] = hydra.environment.partitionDefinitions(defs)
  lazy val allTypeDefs: Seq[hydra.packaging.TypeDefinition] = hydra.lib.pairs.first[Seq[hydra.packaging.TypeDefinition],
     Seq[hydra.packaging.TermDefinition]](partitioned)
  lazy val termDefs: Seq[hydra.packaging.TermDefinition] = hydra.lib.pairs.second[Seq[hydra.packaging.TypeDefinition],
     Seq[hydra.packaging.TermDefinition]](partitioned)
  lazy val typeDefs: Seq[hydra.packaging.TypeDefinition] = hydra.lib.lists.filter[hydra.packaging.TypeDefinition]((td: hydra.packaging.TypeDefinition) => hydra.predicates.isNominalType(td.`type`.`type`))(allTypeDefs)
  hydra.lib.eithers.bind[T2, Seq[hydra.lisp.syntax.TopLevelFormWithComments], hydra.lisp.syntax.Program](hydra.lib.eithers.mapList[hydra.packaging.TypeDefinition,
     hydra.lisp.syntax.TopLevelFormWithComments, T2]((v1: hydra.packaging.TypeDefinition) => hydra.lisp.coder.encodeTypeDefinition(cx)(g)(v1))(typeDefs))((typeItems: Seq[hydra.lisp.syntax.TopLevelFormWithComments]) =>
    hydra.lib.eithers.bind[T2, Seq[hydra.lisp.syntax.TopLevelFormWithComments], hydra.lisp.syntax.Program](hydra.lib.eithers.mapList[hydra.packaging.TermDefinition,
       hydra.lisp.syntax.TopLevelFormWithComments, T2]((v1: hydra.packaging.TermDefinition) => hydra.lisp.coder.encodeTermDefinition(dialect)(cx)(g)(v1))(termDefs))((termItems: Seq[hydra.lisp.syntax.TopLevelFormWithComments]) =>
    {
    lazy val allItems: Seq[hydra.lisp.syntax.TopLevelFormWithComments] = hydra.lib.lists.concat2[hydra.lisp.syntax.TopLevelFormWithComments](typeItems)(termItems)
    {
      lazy val nsName: scala.Predef.String = (mod.namespace)
      {
        lazy val focusNs: hydra.packaging.Namespace = (mod.namespace)
        {
          lazy val imports: Seq[hydra.lisp.syntax.ImportDeclaration] = hydra.lisp.coder.moduleImports(focusNs)(defs)
          {
            lazy val exports: Seq[hydra.lisp.syntax.ExportDeclaration] = hydra.lisp.coder.moduleExports(allItems)
            Right(hydra.lisp.syntax.Program(dialect, Some(hydra.lisp.syntax.ModuleDeclaration(nsName,
               None)), imports, exports, allItems))
          }
        }
      }
    }
  }))
}

def qualifiedSnakeName(name: hydra.core.Name): scala.Predef.String =
  {
  lazy val raw: scala.Predef.String = name
  lazy val parts: Seq[scala.Predef.String] = hydra.lib.strings.splitOn(".")(raw)
  lazy val snakeParts: Seq[scala.Predef.String] = hydra.lib.lists.map[scala.Predef.String,
     scala.Predef.String]((p: scala.Predef.String) => hydra.formatting.convertCaseCamelOrUnderscoreToLowerSnake(p))(parts)
  lazy val joined: scala.Predef.String = hydra.lib.strings.intercalate("_")(snakeParts)
  hydra.formatting.sanitizeWithUnderscores(hydra.lisp.language.lispReservedWords)(joined)
}

def qualifiedTypeName(name: hydra.core.Name): scala.Predef.String = hydra.formatting.capitalize(hydra.names.localNameOf(name))

def wrapInThunk(expr: hydra.lisp.syntax.Expression): hydra.lisp.syntax.Expression =
  hydra.lisp.syntax.Expression.lambda(hydra.lisp.syntax.Lambda(None, Seq(), None, Seq(expr)))
