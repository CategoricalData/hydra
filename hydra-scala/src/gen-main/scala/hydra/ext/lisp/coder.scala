package hydra.ext.lisp.coder

import hydra.core.*

import hydra.ext.lisp.syntax.*

import hydra.module.*

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

def dialectCadr(d: hydra.ext.lisp.syntax.Dialect): scala.Predef.String =
  d match
  case hydra.ext.lisp.syntax.Dialect.clojure => "second"
  case _ => "cadr"

def dialectCar(d: hydra.ext.lisp.syntax.Dialect): scala.Predef.String =
  d match
  case hydra.ext.lisp.syntax.Dialect.clojure => "first"
  case _ => "car"

def dialectConstructorPrefix(d: hydra.ext.lisp.syntax.Dialect): scala.Predef.String =
  d match
  case hydra.ext.lisp.syntax.Dialect.clojure => "->"
  case _ => "make-"

def dialectEqual(d: hydra.ext.lisp.syntax.Dialect): scala.Predef.String =
  d match
  case hydra.ext.lisp.syntax.Dialect.clojure => "="
  case hydra.ext.lisp.syntax.Dialect.commonLisp => "equal"
  case hydra.ext.lisp.syntax.Dialect.emacsLisp => "equal"
  case _ => "equal?"

def encodeApplication[T0, T1, T2](dialect: hydra.ext.lisp.syntax.Dialect)(cx: T0)(g: T1)(rawFun: hydra.core.Term)(rawArg: hydra.core.Term): Either[T2,
   hydra.ext.lisp.syntax.Expression] =
  {
  lazy val dFun: hydra.core.Term = hydra.strip.deannotateTerm(rawFun)
  lazy val normal: Either[T2, hydra.ext.lisp.syntax.Expression] = hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression,
     hydra.ext.lisp.syntax.Expression](hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(rawFun))((fun: hydra.ext.lisp.syntax.Expression) =>
    hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression](hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(rawArg))((arg: hydra.ext.lisp.syntax.Expression) => Right(hydra.ext.lisp.coder.lispApp(fun)(Seq(arg)))))
  def enc(t: hydra.core.Term): Either[T2, hydra.ext.lisp.syntax.Expression] = hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(t)
  dFun match
    case hydra.core.Term.application(v_Term_application_app2) => {
      lazy val midFun: hydra.core.Term = (v_Term_application_app2.function)
      {
        lazy val midArg: hydra.core.Term = (v_Term_application_app2.argument)
        {
          lazy val dMidFun: hydra.core.Term = hydra.strip.deannotateTerm(midFun)
          {
            lazy val isLazy2: Boolean = hydra.lib.logic.or(hydra.ext.lisp.coder.isPrimitiveRef("hydra.lib.eithers.fromLeft")(dMidFun))(hydra.lib.logic.or(hydra.ext.lisp.coder.isPrimitiveRef("hydra.lib.eithers.fromRight")(dMidFun))(hydra.ext.lisp.coder.isPrimitiveRef("hydra.lib.maybes.fromMaybe")(dMidFun)))
            hydra.lib.logic.ifElse[Either[T2, hydra.ext.lisp.syntax.Expression]](isLazy2)(hydra.lib.eithers.bind[T2,
               hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression](enc(midFun))((ePrim: hydra.ext.lisp.syntax.Expression) =>
              hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression](enc(midArg))((eDef: hydra.ext.lisp.syntax.Expression) =>
              hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression](enc(rawArg))((eArg: hydra.ext.lisp.syntax.Expression) =>
              Right(hydra.ext.lisp.coder.lispApp(hydra.ext.lisp.coder.lispApp(ePrim)(Seq(hydra.ext.lisp.coder.wrapInThunk(eDef))))(Seq(eArg)))))))(dMidFun match
              case hydra.core.Term.application(v_Term_application_app3) => {
                lazy val innerFun: hydra.core.Term = (v_Term_application_app3.function)
                {
                  lazy val innerArg: hydra.core.Term = (v_Term_application_app3.argument)
                  {
                    lazy val dInnerFun: hydra.core.Term = hydra.strip.deannotateTerm(innerFun)
                    hydra.lib.logic.ifElse[Either[T2, hydra.ext.lisp.syntax.Expression]](hydra.ext.lisp.coder.isPrimitiveRef("hydra.lib.logic.ifElse")(dInnerFun))(hydra.lib.eithers.bind[T2,
                       hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression](enc(innerArg))((eC: hydra.ext.lisp.syntax.Expression) =>
                      hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression](enc(midArg))((eT: hydra.ext.lisp.syntax.Expression) =>
                      hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression](enc(rawArg))((eE: hydra.ext.lisp.syntax.Expression) =>
                      Right(hydra.ext.lisp.syntax.Expression.`if`(hydra.ext.lisp.syntax.IfExpression(eC,
                         eT, Some(eE))))))))(hydra.lib.logic.ifElse[Either[T2, hydra.ext.lisp.syntax.Expression]](hydra.ext.lisp.coder.isPrimitiveRef("hydra.lib.maybes.maybe")(dInnerFun))(hydra.lib.eithers.bind[T2,
                         hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression](enc(innerFun))((eP: hydra.ext.lisp.syntax.Expression) =>
                      hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression](enc(innerArg))((eDef: hydra.ext.lisp.syntax.Expression) =>
                      hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression](enc(midArg))((eF: hydra.ext.lisp.syntax.Expression) =>
                      hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression](enc(rawArg))((eM: hydra.ext.lisp.syntax.Expression) =>
                      Right(hydra.ext.lisp.coder.lispApp(hydra.ext.lisp.coder.lispApp(hydra.ext.lisp.coder.lispApp(eP)(Seq(hydra.ext.lisp.coder.wrapInThunk(eDef))))(Seq(eF)))(Seq(eM))))))))(hydra.lib.logic.ifElse[Either[T2,
                         hydra.ext.lisp.syntax.Expression]](hydra.ext.lisp.coder.isPrimitiveRef("hydra.lib.maybes.cases")(dInnerFun))(hydra.lib.eithers.bind[T2,
                         hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression](enc(innerFun))((eP: hydra.ext.lisp.syntax.Expression) =>
                      hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression](enc(innerArg))((eM: hydra.ext.lisp.syntax.Expression) =>
                      hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression](enc(midArg))((eN: hydra.ext.lisp.syntax.Expression) =>
                      hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression](enc(rawArg))((eJ: hydra.ext.lisp.syntax.Expression) =>
                      Right(hydra.ext.lisp.coder.lispApp(hydra.ext.lisp.coder.lispApp(hydra.ext.lisp.coder.lispApp(eP)(Seq(eM)))(Seq(hydra.ext.lisp.coder.wrapInThunk(eN))))(Seq(eJ))))))))(normal)))
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

def encodeElimination[T0, T1, T2](dialect: hydra.ext.lisp.syntax.Dialect)(cx: T0)(g: T1)(elim: hydra.core.Elimination)(marg: Option[hydra.core.Term]): Either[T2,
   hydra.ext.lisp.syntax.Expression] =
  elim match
  case hydra.core.Elimination.record(v_Elimination_record_proj) => {
    lazy val fname: scala.Predef.String = hydra.formatting.convertCaseCamelToLowerSnake(v_Elimination_record_proj.field)
    {
      lazy val tname: scala.Predef.String = hydra.ext.lisp.coder.qualifiedSnakeName(v_Elimination_record_proj.typeName)
      hydra.lib.maybes.cases[hydra.core.Term, Either[T2, hydra.ext.lisp.syntax.Expression]](marg)(Right(hydra.ext.lisp.coder.lispLambdaExpr(Seq("v"))(hydra.ext.lisp.syntax.Expression.fieldAccess(hydra.ext.lisp.syntax.FieldAccess(tname,
         fname, hydra.ext.lisp.coder.lispVar("v"))))))((arg: hydra.core.Term) =>
        hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression](hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(arg))((sarg: hydra.ext.lisp.syntax.Expression) =>
        Right(hydra.ext.lisp.syntax.Expression.fieldAccess(hydra.ext.lisp.syntax.FieldAccess(tname, fname, sarg)))))
    }
  }
  case hydra.core.Elimination.union(v_Elimination_union_cs) => {
    lazy val tname: scala.Predef.String = hydra.names.localNameOf(v_Elimination_union_cs.typeName)
    {
      lazy val caseFields: Seq[hydra.core.Field] = (v_Elimination_union_cs.cases)
      {
        lazy val defCase: Option[hydra.core.Term] = (v_Elimination_union_cs.default)
        hydra.lib.eithers.bind[T2, Seq[hydra.ext.lisp.syntax.CondClause], hydra.ext.lisp.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Field,
           hydra.ext.lisp.syntax.CondClause, T2]((cf: hydra.core.Field) =>
          {
          lazy val cfname: scala.Predef.String = hydra.formatting.convertCaseCamelToLowerSnake(cf.name)
          {
            lazy val cfterm: hydra.core.Term = (cf.term)
            {
              lazy val condExpr: hydra.ext.lisp.syntax.Expression = hydra.ext.lisp.coder.lispApp(hydra.ext.lisp.coder.lispVar(hydra.ext.lisp.coder.dialectEqual(dialect)))(Seq(hydra.ext.lisp.coder.lispApp(hydra.ext.lisp.coder.lispVar(hydra.ext.lisp.coder.dialectCar(dialect)))(Seq(hydra.ext.lisp.coder.lispVar("match_target"))),
                 hydra.ext.lisp.coder.lispKeyword(cfname)))
              hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.CondClause](hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(hydra.core.Term.application(hydra.core.Application(cfterm,
                 hydra.core.Term.variable("match_value")))))((bodyExpr: hydra.ext.lisp.syntax.Expression) => Right(hydra.ext.lisp.syntax.CondClause(condExpr,
                 bodyExpr)))
            }
          }
        })(caseFields))((clauses: Seq[hydra.ext.lisp.syntax.CondClause]) =>
          hydra.lib.eithers.bind[T2, Option[hydra.ext.lisp.syntax.Expression], hydra.ext.lisp.syntax.Expression](hydra.lib.maybes.cases[hydra.core.Term,
             Either[T2, Option[hydra.ext.lisp.syntax.Expression]]](defCase)(Right(None))((dt: hydra.core.Term) =>
          hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, Option[hydra.ext.lisp.syntax.Expression]](hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(dt))((defBody: hydra.ext.lisp.syntax.Expression) => Right(Some(defBody)))))((defExpr: Option[hydra.ext.lisp.syntax.Expression]) =>
          {
          lazy val condExpr: hydra.ext.lisp.syntax.Expression = hydra.ext.lisp.syntax.Expression.cond(hydra.ext.lisp.syntax.CondExpression(clauses, defExpr))
          {
            lazy val innerExpr: hydra.ext.lisp.syntax.Expression = hydra.ext.lisp.coder.lispApp(hydra.ext.lisp.coder.lispLambdaExpr(Seq("match_value"))(condExpr))(Seq(hydra.ext.lisp.coder.lispApp(hydra.ext.lisp.coder.lispVar(hydra.ext.lisp.coder.dialectCadr(dialect)))(Seq(hydra.ext.lisp.coder.lispVar("match_target")))))
            hydra.lib.maybes.cases[hydra.core.Term, Either[T2, hydra.ext.lisp.syntax.Expression]](marg)(Right(hydra.ext.lisp.coder.lispLambdaExpr(Seq("match_target"))(innerExpr)))((arg: hydra.core.Term) =>
              hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression](hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(arg))((sarg: hydra.ext.lisp.syntax.Expression) =>
              Right(hydra.ext.lisp.coder.lispApp(hydra.ext.lisp.coder.lispLambdaExpr(Seq("match_target"))(innerExpr))(Seq(sarg)))))
          }
        }))
      }
    }
  }
  case hydra.core.Elimination.wrap(v_Elimination_wrap_name) => hydra.lib.maybes.cases[hydra.core.Term,
     Either[T2, hydra.ext.lisp.syntax.Expression]](marg)(Right(hydra.ext.lisp.coder.lispLambdaExpr(Seq("v"))(hydra.ext.lisp.coder.lispVar("v"))))((arg: hydra.core.Term) => hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(arg))

def encodeFieldDef(ft: hydra.core.FieldType): hydra.ext.lisp.syntax.FieldDefinition =
  {
  lazy val fname: scala.Predef.String = (ft.name)
  hydra.ext.lisp.syntax.FieldDefinition(hydra.formatting.convertCaseCamelToLowerSnake(fname), None)
}

def encodeFunction[T0, T1, T2](dialect: hydra.ext.lisp.syntax.Dialect)(cx: T0)(g: T1)(fun: hydra.core.Function): Either[T2, hydra.ext.lisp.syntax.Expression] =
  fun match
  case hydra.core.Function.lambda(v_Function_lambda_lam) => {
    lazy val param: scala.Predef.String = hydra.formatting.convertCaseCamelOrUnderscoreToLowerSnake(hydra.formatting.sanitizeWithUnderscores(hydra.ext.lisp.language.lispReservedWords)(v_Function_lambda_lam.parameter))
    hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression](hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(v_Function_lambda_lam.body))((body: hydra.ext.lisp.syntax.Expression) => Right(hydra.ext.lisp.coder.lispLambdaExpr(Seq(param))(body)))
  }
  case hydra.core.Function.primitive(v_Function_primitive_name) => Right(hydra.ext.lisp.coder.lispVar(hydra.formatting.convertCaseCamelOrUnderscoreToLowerSnake(hydra.formatting.sanitizeWithUnderscores(hydra.ext.lisp.language.lispReservedWords)(v_Function_primitive_name))))
  case hydra.core.Function.elimination(v_Function_elimination_elim) => hydra.ext.lisp.coder.encodeElimination(dialect)(cx)(g)(v_Function_elimination_elim)(None)

def encodeLetAsLambdaApp[T0, T1, T2](dialect: hydra.ext.lisp.syntax.Dialect)(cx: T0)(g: T1)(bindings: Seq[hydra.core.Binding])(body: hydra.core.Term): Either[T2,
   hydra.ext.lisp.syntax.Expression] =
  hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression](hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(body))((bodyExpr: hydra.ext.lisp.syntax.Expression) =>
  hydra.lib.eithers.foldl[hydra.ext.lisp.syntax.Expression, hydra.core.Binding, T2]((acc: hydra.ext.lisp.syntax.Expression) =>
  (b: hydra.core.Binding) =>
  {
  lazy val bname: scala.Predef.String = hydra.formatting.convertCaseCamelOrUnderscoreToLowerSnake(hydra.formatting.sanitizeWithUnderscores(hydra.ext.lisp.language.lispReservedWords)(b.name))
  hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression](hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(b.term))((bval: hydra.ext.lisp.syntax.Expression) =>
    Right(hydra.ext.lisp.coder.lispApp(hydra.ext.lisp.coder.lispLambdaExpr(Seq(bname))(acc))(Seq(bval))))
})(bodyExpr)(hydra.lib.lists.reverse[hydra.core.Binding](bindings)))

def encodeLetAsNative[T0, T1, T2](dialect: hydra.ext.lisp.syntax.Dialect)(cx: T0)(g: T1)(bindings: Seq[hydra.core.Binding])(body: hydra.core.Term): Either[T2,
   hydra.ext.lisp.syntax.Expression] =
  {
  lazy val isClojureTop: Boolean = dialect match
    case hydra.ext.lisp.syntax.Dialect.clojure => true
    case _ => false
  hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression](hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(body))((bodyExpr: hydra.ext.lisp.syntax.Expression) =>
    {
    lazy val sortedBindings: Seq[hydra.core.Binding] = hydra.lib.logic.ifElse[Seq[hydra.core.Binding]](true)({
      lazy val allNames: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.lists.map[hydra.core.Binding,
         hydra.core.Name]((b: hydra.core.Binding) => (b.name))(bindings))
      lazy val adjList: Seq[Tuple2[hydra.core.Name, Seq[hydra.core.Name]]] = hydra.lib.lists.map[hydra.core.Binding,
         Tuple2[hydra.core.Name, Seq[hydra.core.Name]]]((b: hydra.core.Binding) =>
        Tuple2(b.name, hydra.lib.sets.toList[hydra.core.Name](hydra.lib.sets.intersection[hydra.core.Name](allNames)(hydra.variables.freeVariablesInTerm(b.term)))))(bindings)
      lazy val sortResult: Either[Seq[Seq[hydra.core.Name]], Seq[hydra.core.Name]] = hydra.sorting.topologicalSort(adjList)
      lazy val nameToBinding: Map[hydra.core.Name, hydra.core.Binding] = hydra.lib.maps.fromList[hydra.core.Name,
         hydra.core.Binding](hydra.lib.lists.map[hydra.core.Binding, Tuple2[hydra.core.Name, hydra.core.Binding]]((b: hydra.core.Binding) => Tuple2(b.name,
         b))(bindings))
      hydra.lib.eithers.either[Seq[Seq[hydra.core.Name]], Seq[hydra.core.Name], Seq[hydra.core.Binding]]((_x: Seq[Seq[hydra.core.Name]]) => bindings)((sorted: Seq[hydra.core.Name]) =>
        hydra.lib.lists.map[hydra.core.Name, hydra.core.Binding]((name: hydra.core.Name) =>
        hydra.lib.maybes.fromMaybe[hydra.core.Binding](hydra.lib.lists.head[hydra.core.Binding](bindings))(hydra.lib.maps.lookup[hydra.core.Name,
           hydra.core.Binding](name)(nameToBinding)))(sorted))(sortResult)
    })(bindings)
    hydra.lib.eithers.bind[T2, Seq[Tuple2[scala.Predef.String, hydra.ext.lisp.syntax.Expression]], hydra.ext.lisp.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Binding,
       Tuple2[scala.Predef.String, hydra.ext.lisp.syntax.Expression], T2]((b: hydra.core.Binding) =>
      {
      lazy val bname: scala.Predef.String = hydra.formatting.convertCaseCamelOrUnderscoreToLowerSnake(hydra.formatting.sanitizeWithUnderscores(hydra.ext.lisp.language.lispReservedWords)(b.name))
      {
        lazy val isSelfRef: Boolean = hydra.lib.sets.member[hydra.core.Name](b.name)(hydra.variables.freeVariablesInTerm(b.term))
        {
          lazy val isLambda: Boolean = hydra.strip.deannotateTerm(b.term) match
            case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
              case hydra.core.Function.lambda(v_Function_lambda__) => true
              case _ => false
            case _ => false
          hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, Tuple2[scala.Predef.String, hydra.ext.lisp.syntax.Expression]](hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(b.term))((bval: hydra.ext.lisp.syntax.Expression) =>
            {
            lazy val isClojure: Boolean = dialect match
              case hydra.ext.lisp.syntax.Dialect.clojure => true
              case _ => false
            {
              lazy val wrappedVal: hydra.ext.lisp.syntax.Expression = hydra.lib.logic.ifElse[hydra.ext.lisp.syntax.Expression](isClojure)(hydra.lib.logic.ifElse[hydra.ext.lisp.syntax.Expression](isSelfRef)(hydra.lib.logic.ifElse[hydra.ext.lisp.syntax.Expression](isLambda)(bval match
                case hydra.ext.lisp.syntax.Expression.lambda(v_Expression_lambda_lam) => hydra.ext.lisp.syntax.Expression.lambda(hydra.ext.lisp.syntax.Lambda(Some(bname),
                   (v_Expression_lambda_lam.params), (v_Expression_lambda_lam.restParam), (v_Expression_lambda_lam.body)))
                case _ => bval)(hydra.ext.lisp.coder.lispNamedLambdaExpr(bname)(Seq("_arg"))(hydra.ext.lisp.coder.lispApp(bval)(Seq(hydra.ext.lisp.coder.lispVar("_arg"))))))(bval))(hydra.lib.logic.ifElse[hydra.ext.lisp.syntax.Expression](hydra.lib.logic.and(isSelfRef)(hydra.lib.logic.not(isLambda)))(hydra.ext.lisp.coder.lispLambdaExpr(Seq("_arg"))(hydra.ext.lisp.coder.lispApp(bval)(Seq(hydra.ext.lisp.coder.lispVar("_arg")))))(bval))
              Right(Tuple2(bname, wrappedVal))
            }
          })
        }
      }
    })(sortedBindings))((encodedBindings: Seq[Tuple2[scala.Predef.String, hydra.ext.lisp.syntax.Expression]]) =>
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
              lazy val letKind: hydra.ext.lisp.syntax.LetKind = hydra.lib.logic.ifElse[hydra.ext.lisp.syntax.LetKind](isRecursive)(hydra.ext.lisp.syntax.LetKind.recursive)(hydra.lib.logic.ifElse[hydra.ext.lisp.syntax.LetKind](hydra.lib.lists.`null`[hydra.core.Binding](hydra.lib.lists.tail[hydra.core.Binding](bindings)))(hydra.ext.lisp.syntax.LetKind.parallel)(hydra.ext.lisp.syntax.LetKind.sequential))
              {
                lazy val lispBindings: Seq[hydra.ext.lisp.syntax.LetBinding] = hydra.lib.lists.map[Tuple2[scala.Predef.String,
                   hydra.ext.lisp.syntax.Expression], hydra.ext.lisp.syntax.LetBinding]((eb: Tuple2[scala.Predef.String,
                   hydra.ext.lisp.syntax.Expression]) =>
                  hydra.ext.lisp.syntax.LetBinding.simple(hydra.ext.lisp.syntax.SimpleBinding(hydra.lib.pairs.first[scala.Predef.String,
                     hydra.ext.lisp.syntax.Expression](eb), hydra.lib.pairs.second[scala.Predef.String,
                     hydra.ext.lisp.syntax.Expression](eb))))(encodedBindings)
                Right(hydra.ext.lisp.syntax.Expression.let(hydra.ext.lisp.syntax.LetExpression(letKind, lispBindings, Seq(bodyExpr))))
              }
            }
          }
        }
      }
    })
  })
}

def encodeLiteral(lit: hydra.core.Literal): hydra.ext.lisp.syntax.Expression =
  lit match
  case hydra.core.Literal.boolean(v_Literal_boolean_b) => hydra.ext.lisp.syntax.Expression.literal(hydra.ext.lisp.syntax.Literal.boolean(v_Literal_boolean_b))
  case hydra.core.Literal.string(v_Literal_string_s) => hydra.ext.lisp.syntax.Expression.literal(hydra.ext.lisp.syntax.Literal.string(v_Literal_string_s))
  case hydra.core.Literal.float(v_Literal_float_fv) => v_Literal_float_fv match
    case hydra.core.FloatValue.float32(v_FloatValue_float32_f) => hydra.ext.lisp.syntax.Expression.literal(hydra.ext.lisp.syntax.Literal.float(hydra.ext.lisp.syntax.FloatLiteral(hydra.lib.literals.float32ToBigfloat(v_FloatValue_float32_f),
       None)))
    case hydra.core.FloatValue.float64(v_FloatValue_float64_f) => hydra.ext.lisp.syntax.Expression.literal(hydra.ext.lisp.syntax.Literal.float(hydra.ext.lisp.syntax.FloatLiteral(hydra.lib.literals.float64ToBigfloat(v_FloatValue_float64_f),
       None)))
    case hydra.core.FloatValue.bigfloat(v_FloatValue_bigfloat_f) => hydra.ext.lisp.syntax.Expression.literal(hydra.ext.lisp.syntax.Literal.float(hydra.ext.lisp.syntax.FloatLiteral(v_FloatValue_bigfloat_f,
       None)))
  case hydra.core.Literal.integer(v_Literal_integer_iv) => v_Literal_integer_iv match
    case hydra.core.IntegerValue.int8(v_IntegerValue_int8_i) => hydra.ext.lisp.syntax.Expression.literal(hydra.ext.lisp.syntax.Literal.integer(hydra.ext.lisp.syntax.IntegerLiteral(hydra.lib.literals.int8ToBigint(v_IntegerValue_int8_i),
       false)))
    case hydra.core.IntegerValue.int16(v_IntegerValue_int16_i) => hydra.ext.lisp.syntax.Expression.literal(hydra.ext.lisp.syntax.Literal.integer(hydra.ext.lisp.syntax.IntegerLiteral(hydra.lib.literals.int16ToBigint(v_IntegerValue_int16_i),
       false)))
    case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => hydra.ext.lisp.syntax.Expression.literal(hydra.ext.lisp.syntax.Literal.integer(hydra.ext.lisp.syntax.IntegerLiteral(hydra.lib.literals.int32ToBigint(v_IntegerValue_int32_i),
       false)))
    case hydra.core.IntegerValue.int64(v_IntegerValue_int64_i) => hydra.ext.lisp.syntax.Expression.literal(hydra.ext.lisp.syntax.Literal.integer(hydra.ext.lisp.syntax.IntegerLiteral(hydra.lib.literals.int64ToBigint(v_IntegerValue_int64_i),
       false)))
    case hydra.core.IntegerValue.uint8(v_IntegerValue_uint8_i) => hydra.ext.lisp.syntax.Expression.literal(hydra.ext.lisp.syntax.Literal.integer(hydra.ext.lisp.syntax.IntegerLiteral(hydra.lib.literals.uint8ToBigint(v_IntegerValue_uint8_i),
       false)))
    case hydra.core.IntegerValue.uint16(v_IntegerValue_uint16_i) => hydra.ext.lisp.syntax.Expression.literal(hydra.ext.lisp.syntax.Literal.integer(hydra.ext.lisp.syntax.IntegerLiteral(hydra.lib.literals.uint16ToBigint(v_IntegerValue_uint16_i),
       false)))
    case hydra.core.IntegerValue.uint32(v_IntegerValue_uint32_i) => hydra.ext.lisp.syntax.Expression.literal(hydra.ext.lisp.syntax.Literal.integer(hydra.ext.lisp.syntax.IntegerLiteral(hydra.lib.literals.uint32ToBigint(v_IntegerValue_uint32_i),
       false)))
    case hydra.core.IntegerValue.uint64(v_IntegerValue_uint64_i) => hydra.ext.lisp.syntax.Expression.literal(hydra.ext.lisp.syntax.Literal.integer(hydra.ext.lisp.syntax.IntegerLiteral(hydra.lib.literals.uint64ToBigint(v_IntegerValue_uint64_i),
       false)))
    case hydra.core.IntegerValue.bigint(v_IntegerValue_bigint_i) => hydra.ext.lisp.syntax.Expression.literal(hydra.ext.lisp.syntax.Literal.integer(hydra.ext.lisp.syntax.IntegerLiteral(v_IntegerValue_bigint_i,
       true)))
  case hydra.core.Literal.binary(v_Literal_binary_b) => {
    lazy val byteValues: Seq[Int] = hydra.lib.literals.binaryToBytes(v_Literal_binary_b)
    hydra.ext.lisp.syntax.Expression.vector(hydra.ext.lisp.syntax.VectorLiteral(hydra.lib.lists.map[Int, hydra.ext.lisp.syntax.Expression]((bv: Int) =>
      hydra.ext.lisp.syntax.Expression.literal(hydra.ext.lisp.syntax.Literal.integer(hydra.ext.lisp.syntax.IntegerLiteral(hydra.lib.literals.int32ToBigint(bv),
         false))))(byteValues)))
  }

def encodeTerm[T0, T1, T2](dialect: hydra.ext.lisp.syntax.Dialect)(cx: T0)(g: T1)(term: hydra.core.Term): Either[T2, hydra.ext.lisp.syntax.Expression] =
  term match
  case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(v_Term_annotated_at.body)
  case hydra.core.Term.application(v_Term_application_app) => {
    lazy val rawFun: hydra.core.Term = (v_Term_application_app.function)
    {
      lazy val rawArg: hydra.core.Term = (v_Term_application_app.argument)
      hydra.ext.lisp.coder.encodeApplication(dialect)(cx)(g)(rawFun)(rawArg)
    }
  }
  case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.either[hydra.core.Term, hydra.core.Term,
     Either[T2, hydra.ext.lisp.syntax.Expression]]((l: hydra.core.Term) =>
    hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression](hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(l))((sl: hydra.ext.lisp.syntax.Expression) =>
    Right(hydra.ext.lisp.coder.lispApp(hydra.ext.lisp.coder.lispVar("list"))(Seq(hydra.ext.lisp.coder.lispKeyword("left"), sl)))))((r: hydra.core.Term) =>
    hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression](hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(r))((sr: hydra.ext.lisp.syntax.Expression) =>
    Right(hydra.ext.lisp.coder.lispApp(hydra.ext.lisp.coder.lispVar("list"))(Seq(hydra.ext.lisp.coder.lispKeyword("right"), sr)))))(v_Term_either_e)
  case hydra.core.Term.function(v_Term_function_fun) => hydra.ext.lisp.coder.encodeFunction(dialect)(cx)(g)(v_Term_function_fun)
  case hydra.core.Term.let(v_Term_let_lt) => {
    lazy val bindings: Seq[hydra.core.Binding] = (v_Term_let_lt.bindings)
    {
      lazy val body: hydra.core.Term = (v_Term_let_lt.body)
      hydra.ext.lisp.coder.encodeLetAsNative(dialect)(cx)(g)(bindings)(body)
    }
  }
  case hydra.core.Term.list(v_Term_list_els) => hydra.lib.eithers.bind[T2, Seq[hydra.ext.lisp.syntax.Expression],
     hydra.ext.lisp.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Term, hydra.ext.lisp.syntax.Expression,
     T2]((v1: hydra.core.Term) => hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(v1))(v_Term_list_els))((sels: Seq[hydra.ext.lisp.syntax.Expression]) => Right(hydra.ext.lisp.coder.lispListExpr(sels)))
  case hydra.core.Term.literal(v_Term_literal_lit) => Right(hydra.ext.lisp.coder.encodeLiteral(v_Term_literal_lit))
  case hydra.core.Term.map(v_Term_map_m) => hydra.lib.eithers.bind[T2, Seq[hydra.ext.lisp.syntax.MapEntry],
     hydra.ext.lisp.syntax.Expression](hydra.lib.eithers.mapList[Tuple2[hydra.core.Term, hydra.core.Term],
     hydra.ext.lisp.syntax.MapEntry, T2]((entry: Tuple2[hydra.core.Term, hydra.core.Term]) =>
    hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.MapEntry](hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(hydra.lib.pairs.first[hydra.core.Term,
       hydra.core.Term](entry)))((k: hydra.ext.lisp.syntax.Expression) =>
    hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.MapEntry](hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(hydra.lib.pairs.second[hydra.core.Term,
       hydra.core.Term](entry)))((v: hydra.ext.lisp.syntax.Expression) => Right(hydra.ext.lisp.syntax.MapEntry(k,
       v)))))(hydra.lib.maps.toList[hydra.core.Term, hydra.core.Term](v_Term_map_m)))((pairs: Seq[hydra.ext.lisp.syntax.MapEntry]) =>
    Right(hydra.ext.lisp.syntax.Expression.map(hydra.ext.lisp.syntax.MapLiteral(pairs))))
  case hydra.core.Term.maybe(v_Term_maybe_mt) => hydra.lib.maybes.cases[hydra.core.Term, Either[T2, hydra.ext.lisp.syntax.Expression]](v_Term_maybe_mt)(Right(hydra.ext.lisp.coder.lispApp(hydra.ext.lisp.coder.lispVar("list"))(Seq(hydra.ext.lisp.coder.lispKeyword("nothing")))))((`val`: hydra.core.Term) =>
    hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression](hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(`val`))((sval: hydra.ext.lisp.syntax.Expression) =>
    Right(hydra.ext.lisp.coder.lispApp(hydra.ext.lisp.coder.lispVar("list"))(Seq(hydra.ext.lisp.coder.lispKeyword("just"), sval)))))
  case hydra.core.Term.pair(v_Term_pair_p) => hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression,
     hydra.ext.lisp.syntax.Expression](hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(hydra.lib.pairs.first[hydra.core.Term,
     hydra.core.Term](v_Term_pair_p)))((f: hydra.ext.lisp.syntax.Expression) =>
    hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression](hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(hydra.lib.pairs.second[hydra.core.Term,
       hydra.core.Term](v_Term_pair_p)))((s: hydra.ext.lisp.syntax.Expression) => Right(hydra.ext.lisp.coder.lispListExpr(Seq(f,
       s)))))
  case hydra.core.Term.record(v_Term_record_rec) => {
    lazy val rname: hydra.core.Name = (v_Term_record_rec.typeName)
    {
      lazy val fields: Seq[hydra.core.Field] = (v_Term_record_rec.fields)
      hydra.lib.eithers.bind[T2, Seq[hydra.ext.lisp.syntax.Expression], hydra.ext.lisp.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Field,
         hydra.ext.lisp.syntax.Expression, T2]((f: hydra.core.Field) => hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(f.term))(fields))((sfields: Seq[hydra.ext.lisp.syntax.Expression]) =>
        {
        lazy val constructorName: scala.Predef.String = hydra.lib.strings.cat2(hydra.ext.lisp.coder.dialectConstructorPrefix(dialect))(hydra.ext.lisp.coder.qualifiedSnakeName(rname))
        Right(hydra.ext.lisp.coder.lispApp(hydra.ext.lisp.coder.lispVar(constructorName))(sfields))
      })
    }
  }
  case hydra.core.Term.set(v_Term_set_s) => hydra.lib.eithers.bind[T2, Seq[hydra.ext.lisp.syntax.Expression],
     hydra.ext.lisp.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Term, hydra.ext.lisp.syntax.Expression,
     T2]((v1: hydra.core.Term) => hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(v1))(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_s)))((sels: Seq[hydra.ext.lisp.syntax.Expression]) =>
    Right(hydra.ext.lisp.syntax.Expression.set(hydra.ext.lisp.syntax.SetLiteral(sels))))
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val tname: scala.Predef.String = hydra.names.localNameOf(v_Term_union_inj.typeName)
    {
      lazy val field: hydra.core.Field = (v_Term_union_inj.field)
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
              hydra.lib.logic.ifElse[Either[T2, hydra.ext.lisp.syntax.Expression]](isUnit)(Right(hydra.ext.lisp.coder.lispApp(hydra.ext.lisp.coder.lispVar("list"))(Seq(hydra.ext.lisp.coder.lispKeyword(hydra.formatting.convertCaseCamelToLowerSnake(fname)),
                 hydra.ext.lisp.coder.lispNilExpr))))(hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression,
                 hydra.ext.lisp.syntax.Expression](hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(fterm))((sval: hydra.ext.lisp.syntax.Expression) =>
                Right(hydra.ext.lisp.coder.lispApp(hydra.ext.lisp.coder.lispVar("list"))(Seq(hydra.ext.lisp.coder.lispKeyword(hydra.formatting.convertCaseCamelToLowerSnake(fname)),
                   sval)))))
            }
          }
        }
      }
    }
  }
  case hydra.core.Term.unit => Right(hydra.ext.lisp.coder.lispNilExpr)
  case hydra.core.Term.variable(v_Term_variable_name) => Right(hydra.ext.lisp.coder.lispVar(hydra.formatting.convertCaseCamelOrUnderscoreToLowerSnake(hydra.formatting.sanitizeWithUnderscores(hydra.ext.lisp.language.lispReservedWords)(v_Term_variable_name))))
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(v_Term_typeApplication_ta.body)
  case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(v_Term_typeLambda_tl.body)
  case hydra.core.Term.wrap(v_Term_wrap_wt) => hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(v_Term_wrap_wt.body)

def encodeTermDefinition[T0, T1, T2](dialect: hydra.ext.lisp.syntax.Dialect)(cx: T0)(g: T1)(tdef: hydra.module.TermDefinition): Either[T2,
   hydra.ext.lisp.syntax.TopLevelFormWithComments] =
  {
  lazy val name: hydra.core.Name = (tdef.name)
  lazy val term: hydra.core.Term = (tdef.term)
  lazy val lname: scala.Predef.String = hydra.ext.lisp.coder.qualifiedSnakeName(name)
  lazy val dterm: hydra.core.Term = hydra.strip.deannotateTerm(term)
  dterm match
    case hydra.core.Term.function(v_Term_function_fun) => v_Term_function_fun match
      case hydra.core.Function.lambda(v_Function_lambda_lam) => hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression,
         hydra.ext.lisp.syntax.TopLevelFormWithComments](hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(term))((sterm: hydra.ext.lisp.syntax.Expression) =>
        Right(hydra.ext.lisp.coder.lispTopForm(hydra.ext.lisp.syntax.TopLevelForm.variable(hydra.ext.lisp.syntax.VariableDefinition(lname, sterm, None)))))
      case _ => hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.TopLevelFormWithComments](hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(term))((sterm: hydra.ext.lisp.syntax.Expression) =>
        Right(hydra.ext.lisp.coder.lispTopForm(hydra.ext.lisp.syntax.TopLevelForm.variable(hydra.ext.lisp.syntax.VariableDefinition(lname, sterm, None)))))
    case _ => hydra.lib.eithers.bind[T2, hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.TopLevelFormWithComments](hydra.ext.lisp.coder.encodeTerm(dialect)(cx)(g)(term))((sterm: hydra.ext.lisp.syntax.Expression) =>
      Right(hydra.ext.lisp.coder.lispTopForm(hydra.ext.lisp.syntax.TopLevelForm.variable(hydra.ext.lisp.syntax.VariableDefinition(lname, sterm, None)))))
}

def encodeType[T0, T1, T2](cx: T0)(g: T1)(t: hydra.core.Type): Either[T2, hydra.ext.lisp.syntax.TypeSpecifier] =
  {
  lazy val typ: hydra.core.Type = hydra.strip.deannotateType(t)
  typ match
    case hydra.core.Type.annotated(v_Type_annotated_at) => hydra.ext.lisp.coder.encodeType(cx)(g)(v_Type_annotated_at.body)
    case hydra.core.Type.application(v_Type_application_at) => hydra.ext.lisp.coder.encodeType(cx)(g)(v_Type_application_at.function)
    case hydra.core.Type.unit => Right(hydra.ext.lisp.syntax.TypeSpecifier.unit)
    case hydra.core.Type.literal(v_Type_literal_lt) => Right(v_Type_literal_lt match
      case hydra.core.LiteralType.binary => hydra.ext.lisp.syntax.TypeSpecifier.named("ByteArray")
      case hydra.core.LiteralType.boolean => hydra.ext.lisp.syntax.TypeSpecifier.named("Boolean")
      case hydra.core.LiteralType.float(v_LiteralType_float__) => hydra.ext.lisp.syntax.TypeSpecifier.named("Float")
      case hydra.core.LiteralType.integer(v_LiteralType_integer__) => hydra.ext.lisp.syntax.TypeSpecifier.named("Integer")
      case hydra.core.LiteralType.string => hydra.ext.lisp.syntax.TypeSpecifier.named("String"))
    case hydra.core.Type.list(v_Type_list_inner) => hydra.lib.eithers.map[hydra.ext.lisp.syntax.TypeSpecifier,
       hydra.ext.lisp.syntax.TypeSpecifier, T2]((enc: hydra.ext.lisp.syntax.TypeSpecifier) => hydra.ext.lisp.syntax.TypeSpecifier.list(enc))(hydra.ext.lisp.coder.encodeType(cx)(g)(v_Type_list_inner))
    case hydra.core.Type.set(v_Type_set_inner) => hydra.lib.eithers.map[hydra.ext.lisp.syntax.TypeSpecifier,
       hydra.ext.lisp.syntax.TypeSpecifier, T2]((enc: hydra.ext.lisp.syntax.TypeSpecifier) => hydra.ext.lisp.syntax.TypeSpecifier.set(enc))(hydra.ext.lisp.coder.encodeType(cx)(g)(v_Type_set_inner))
    case hydra.core.Type.map(v_Type_map_mt) => Right(hydra.ext.lisp.syntax.TypeSpecifier.named("Map"))
    case hydra.core.Type.maybe(v_Type_maybe_inner) => hydra.lib.eithers.map[hydra.ext.lisp.syntax.TypeSpecifier,
       hydra.ext.lisp.syntax.TypeSpecifier, T2]((enc: hydra.ext.lisp.syntax.TypeSpecifier) => hydra.ext.lisp.syntax.TypeSpecifier.maybe(enc))(hydra.ext.lisp.coder.encodeType(cx)(g)(v_Type_maybe_inner))
    case hydra.core.Type.either(v_Type_either_et) => Right(hydra.ext.lisp.syntax.TypeSpecifier.named("Either"))
    case hydra.core.Type.pair(v_Type_pair_pt) => Right(hydra.ext.lisp.syntax.TypeSpecifier.named("Pair"))
    case hydra.core.Type.function(v_Type_function_ft) => Right(hydra.ext.lisp.syntax.TypeSpecifier.named("Function"))
    case hydra.core.Type.record(v_Type_record__) => Right(hydra.ext.lisp.syntax.TypeSpecifier.named("Record"))
    case hydra.core.Type.union(v_Type_union__) => Right(hydra.ext.lisp.syntax.TypeSpecifier.named("Union"))
    case hydra.core.Type.wrap(v_Type_wrap__) => Right(hydra.ext.lisp.syntax.TypeSpecifier.named("Wrapper"))
    case hydra.core.Type.variable(v_Type_variable_name) => Right(hydra.ext.lisp.syntax.TypeSpecifier.named(v_Type_variable_name))
    case hydra.core.Type.forall(v_Type_forall_fa) => hydra.ext.lisp.coder.encodeType(cx)(g)(v_Type_forall_fa.body)
    case _ => Right(hydra.ext.lisp.syntax.TypeSpecifier.named("Any"))
}

def encodeTypeBody[T0](lname: scala.Predef.String)(origTyp: hydra.core.Type)(typ: hydra.core.Type): Either[T0, hydra.ext.lisp.syntax.TopLevelFormWithComments] =
  typ match
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.ext.lisp.coder.encodeTypeBody(lname)(origTyp)(v_Type_forall_ft.body)
  case hydra.core.Type.record(v_Type_record_rt) => {
    lazy val fields: Seq[hydra.ext.lisp.syntax.FieldDefinition] = hydra.lib.lists.map[hydra.core.FieldType,
       hydra.ext.lisp.syntax.FieldDefinition](hydra.ext.lisp.coder.encodeFieldDef)(v_Type_record_rt)
    Right(hydra.ext.lisp.coder.lispTopForm(hydra.ext.lisp.syntax.TopLevelForm.recordType(hydra.ext.lisp.syntax.RecordTypeDefinition(lname, fields, None))))
  }
  case hydra.core.Type.union(v_Type_union_rt) => {
    lazy val variantNames: Seq[hydra.ext.lisp.syntax.Expression] = hydra.lib.lists.map[hydra.core.FieldType,
       hydra.ext.lisp.syntax.Expression]((f: hydra.core.FieldType) =>
      hydra.ext.lisp.syntax.Expression.literal(hydra.ext.lisp.syntax.Literal.keyword(hydra.ext.lisp.syntax.Keyword(hydra.formatting.convertCaseCamelToLowerSnake(f.name),
         None))))(v_Type_union_rt)
    Right(hydra.ext.lisp.coder.lispTopForm(hydra.ext.lisp.syntax.TopLevelForm.variable(hydra.ext.lisp.syntax.VariableDefinition(hydra.lib.strings.cat2(lname)("-variants"),
       hydra.ext.lisp.coder.lispListExpr(variantNames), Some(hydra.lib.strings.cat2("Variants of the ")(lname))))))
  }
  case hydra.core.Type.wrap(v_Type_wrap_wt) => Right(hydra.ext.lisp.coder.lispTopForm(hydra.ext.lisp.syntax.TopLevelForm.recordType(hydra.ext.lisp.syntax.RecordTypeDefinition(lname,
     Seq(hydra.ext.lisp.syntax.FieldDefinition("value", None)), None))))
  case _ => Right(hydra.ext.lisp.syntax.TopLevelFormWithComments(None, Some(hydra.ext.lisp.syntax.Comment(hydra.ext.lisp.syntax.CommentStyle.line,
     hydra.lib.strings.cat2(hydra.lib.strings.cat2(lname)(" = "))(hydra.show.core.`type`(origTyp)))),
     hydra.ext.lisp.syntax.TopLevelForm.expression(hydra.ext.lisp.syntax.Expression.literal(hydra.ext.lisp.syntax.Literal.nil))))

def encodeTypeDefinition[T0, T1, T2](cx: T0)(g: T1)(tdef: hydra.module.TypeDefinition): Either[T2, hydra.ext.lisp.syntax.TopLevelFormWithComments] =
  {
  lazy val name: hydra.core.Name = (tdef.name)
  lazy val typ: hydra.core.Type = (tdef.`type`)
  lazy val lname: scala.Predef.String = hydra.ext.lisp.coder.qualifiedSnakeName(name)
  lazy val dtyp: hydra.core.Type = hydra.strip.deannotateType(typ)
  hydra.ext.lisp.coder.encodeTypeBody(lname)(typ)(dtyp)
}

def isCasesPrimitive(name: hydra.core.Name): Boolean = hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.maybes.cases")

def isLazy2ArgPrimitive(name: hydra.core.Name): Boolean =
  hydra.lib.logic.or(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.eithers.fromLeft"))(hydra.lib.logic.or(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.eithers.fromRight"))(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.maybes.fromMaybe")))

def isLazy3ArgPrimitive(name: hydra.core.Name): Boolean = hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.maybes.maybe")

def isPrimitiveRef(primName: scala.Predef.String)(term: hydra.core.Term): Boolean =
  term match
  case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
    case hydra.core.Function.primitive(v_Function_primitive_name) => hydra.lib.equality.equal[scala.Predef.String](v_Function_primitive_name)(primName)
    case _ => false
  case hydra.core.Term.variable(v_Term_variable_name) => hydra.lib.equality.equal[scala.Predef.String](v_Term_variable_name)(primName)
  case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.ext.lisp.coder.isPrimitiveRef(primName)(v_Term_annotated_at.body)
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => hydra.ext.lisp.coder.isPrimitiveRef(primName)(v_Term_typeApplication_ta.body)
  case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.ext.lisp.coder.isPrimitiveRef(primName)(v_Term_typeLambda_tl.body)
  case _ => false

def lispApp(fun: hydra.ext.lisp.syntax.Expression)(args: Seq[hydra.ext.lisp.syntax.Expression]): hydra.ext.lisp.syntax.Expression =
  hydra.ext.lisp.syntax.Expression.application(hydra.ext.lisp.syntax.Application(fun, args))

def lispKeyword(name: scala.Predef.String): hydra.ext.lisp.syntax.Expression =
  hydra.ext.lisp.syntax.Expression.literal(hydra.ext.lisp.syntax.Literal.keyword(hydra.ext.lisp.syntax.Keyword(name, None)))

def lispLambdaExpr(params: Seq[scala.Predef.String])(body: hydra.ext.lisp.syntax.Expression): hydra.ext.lisp.syntax.Expression =
  hydra.ext.lisp.syntax.Expression.lambda(hydra.ext.lisp.syntax.Lambda(None, hydra.lib.lists.map[scala.Predef.String,
     hydra.ext.lisp.syntax.Symbol]((p: scala.Predef.String) => p)(params), None, Seq(body)))

def lispListExpr(elements: Seq[hydra.ext.lisp.syntax.Expression]): hydra.ext.lisp.syntax.Expression =
  hydra.ext.lisp.syntax.Expression.list(hydra.ext.lisp.syntax.ListLiteral(elements, false))

def lispLitExpr(lit: hydra.ext.lisp.syntax.Literal): hydra.ext.lisp.syntax.Expression = hydra.ext.lisp.syntax.Expression.literal(lit)

def lispNamedLambdaExpr(name: scala.Predef.String)(params: Seq[scala.Predef.String])(body: hydra.ext.lisp.syntax.Expression): hydra.ext.lisp.syntax.Expression =
  hydra.ext.lisp.syntax.Expression.lambda(hydra.ext.lisp.syntax.Lambda(Some(name), hydra.lib.lists.map[scala.Predef.String,
     hydra.ext.lisp.syntax.Symbol]((p: scala.Predef.String) => p)(params), None, Seq(body)))

lazy val lispNilExpr: hydra.ext.lisp.syntax.Expression = hydra.ext.lisp.syntax.Expression.literal(hydra.ext.lisp.syntax.Literal.nil)

def lispSymbol(name: scala.Predef.String): hydra.ext.lisp.syntax.Symbol = name

def lispTopForm(form: hydra.ext.lisp.syntax.TopLevelForm): hydra.ext.lisp.syntax.TopLevelFormWithComments = hydra.ext.lisp.syntax.TopLevelFormWithComments(None,
   None, form)

def lispTopFormWithComments(mdoc: Option[scala.Predef.String])(form: hydra.ext.lisp.syntax.TopLevelForm): hydra.ext.lisp.syntax.TopLevelFormWithComments =
  hydra.ext.lisp.syntax.TopLevelFormWithComments(hydra.lib.maybes.map[scala.Predef.String, hydra.ext.lisp.syntax.Docstring]((d: scala.Predef.String) => d)(mdoc),
     None, form)

def lispVar(name: scala.Predef.String): hydra.ext.lisp.syntax.Expression =
  hydra.ext.lisp.syntax.Expression.variable(hydra.ext.lisp.syntax.VariableReference(name, false))

def moduleExports(forms: Seq[hydra.ext.lisp.syntax.TopLevelFormWithComments]): Seq[hydra.ext.lisp.syntax.ExportDeclaration] =
  {
  lazy val symbols: Seq[hydra.ext.lisp.syntax.Symbol] = hydra.lib.lists.concat[hydra.ext.lisp.syntax.Symbol](hydra.lib.lists.map[hydra.ext.lisp.syntax.TopLevelFormWithComments,
     Seq[hydra.ext.lisp.syntax.Symbol]]((fwc: hydra.ext.lisp.syntax.TopLevelFormWithComments) =>
    {
    lazy val form: hydra.ext.lisp.syntax.TopLevelForm = (fwc.form)
    form match
      case hydra.ext.lisp.syntax.TopLevelForm.variable(v_TopLevelForm_variable_vd) => Seq(v_TopLevelForm_variable_vd.name)
      case hydra.ext.lisp.syntax.TopLevelForm.recordType(v_TopLevelForm_recordType_rdef) => {
        lazy val rname: scala.Predef.String = (v_TopLevelForm_recordType_rdef.name)
        {
          lazy val fields: Seq[hydra.ext.lisp.syntax.FieldDefinition] = (v_TopLevelForm_recordType_rdef.fields)
          {
            lazy val fieldSyms: Seq[hydra.ext.lisp.syntax.Symbol] = hydra.lib.lists.map[hydra.ext.lisp.syntax.FieldDefinition,
               hydra.ext.lisp.syntax.Symbol]((f: hydra.ext.lisp.syntax.FieldDefinition) =>
              {
              lazy val fn: scala.Predef.String = (f.name)
              hydra.lib.strings.cat(Seq(rname, "-", fn))
            })(fields)
            hydra.lib.lists.concat[hydra.ext.lisp.syntax.Symbol](Seq(Seq(hydra.lib.strings.cat2("make-")(rname),
               hydra.lib.strings.cat2(rname)("?")), fieldSyms))
          }
        }
      }
      case _ => Seq()
  })(forms))
  hydra.lib.logic.ifElse[Seq[hydra.ext.lisp.syntax.ExportDeclaration]](hydra.lib.lists.`null`[hydra.ext.lisp.syntax.Symbol](symbols))(Seq())(Seq(hydra.ext.lisp.syntax.ExportDeclaration(symbols)))
}

def moduleImports(focusNs: hydra.module.Namespace)(defs: Seq[hydra.module.Definition]): Seq[hydra.ext.lisp.syntax.ImportDeclaration] =
  {
  lazy val depNss: Seq[hydra.module.Namespace] = hydra.lib.sets.toList[hydra.module.Namespace](hydra.lib.sets.delete[hydra.module.Namespace](focusNs)(hydra.analysis.definitionDependencyNamespaces(defs)))
  hydra.lib.lists.map[hydra.module.Namespace, hydra.ext.lisp.syntax.ImportDeclaration]((ns: hydra.module.Namespace) =>
    hydra.ext.lisp.syntax.ImportDeclaration(ns, hydra.ext.lisp.syntax.ImportSpec.all))(depNss)
}

def moduleToLisp[T0, T1, T2](dialect: hydra.ext.lisp.syntax.Dialect)(mod: hydra.module.Module)(defs0: Seq[hydra.module.Definition])(cx: T0)(g: T1): Either[T2,
   hydra.ext.lisp.syntax.Program] =
  {
  lazy val defs: Seq[hydra.module.Definition] = hydra.coderUtils.reorderDefs(defs0)
  lazy val partitioned: Tuple2[Seq[hydra.module.TypeDefinition], Seq[hydra.module.TermDefinition]] = hydra.environment.partitionDefinitions(defs)
  lazy val allTypeDefs: Seq[hydra.module.TypeDefinition] = hydra.lib.pairs.first[Seq[hydra.module.TypeDefinition],
     Seq[hydra.module.TermDefinition]](partitioned)
  lazy val termDefs: Seq[hydra.module.TermDefinition] = hydra.lib.pairs.second[Seq[hydra.module.TypeDefinition], Seq[hydra.module.TermDefinition]](partitioned)
  lazy val typeDefs: Seq[hydra.module.TypeDefinition] = hydra.lib.lists.filter[hydra.module.TypeDefinition]((td: hydra.module.TypeDefinition) => hydra.predicates.isNominalType(td.`type`))(allTypeDefs)
  hydra.lib.eithers.bind[T2, Seq[hydra.ext.lisp.syntax.TopLevelFormWithComments], hydra.ext.lisp.syntax.Program](hydra.lib.eithers.mapList[hydra.module.TypeDefinition,
     hydra.ext.lisp.syntax.TopLevelFormWithComments, T2]((v1: hydra.module.TypeDefinition) => hydra.ext.lisp.coder.encodeTypeDefinition(cx)(g)(v1))(typeDefs))((typeItems: Seq[hydra.ext.lisp.syntax.TopLevelFormWithComments]) =>
    hydra.lib.eithers.bind[T2, Seq[hydra.ext.lisp.syntax.TopLevelFormWithComments], hydra.ext.lisp.syntax.Program](hydra.lib.eithers.mapList[hydra.module.TermDefinition,
       hydra.ext.lisp.syntax.TopLevelFormWithComments, T2]((v1: hydra.module.TermDefinition) =>
    hydra.ext.lisp.coder.encodeTermDefinition(dialect)(cx)(g)(v1))(termDefs))((termItems: Seq[hydra.ext.lisp.syntax.TopLevelFormWithComments]) =>
    {
    lazy val allItems: Seq[hydra.ext.lisp.syntax.TopLevelFormWithComments] = hydra.lib.lists.concat2[hydra.ext.lisp.syntax.TopLevelFormWithComments](typeItems)(termItems)
    {
      lazy val nsName: scala.Predef.String = (mod.namespace)
      {
        lazy val focusNs: hydra.module.Namespace = (mod.namespace)
        {
          lazy val imports: Seq[hydra.ext.lisp.syntax.ImportDeclaration] = hydra.ext.lisp.coder.moduleImports(focusNs)(defs)
          {
            lazy val exports: Seq[hydra.ext.lisp.syntax.ExportDeclaration] = hydra.ext.lisp.coder.moduleExports(allItems)
            Right(hydra.ext.lisp.syntax.Program(dialect, Some(hydra.ext.lisp.syntax.ModuleDeclaration(nsName, None)), imports, exports, allItems))
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
  lazy val snakeParts: Seq[scala.Predef.String] = hydra.lib.lists.map[scala.Predef.String, scala.Predef.String]((p: scala.Predef.String) => hydra.formatting.convertCaseCamelOrUnderscoreToLowerSnake(p))(parts)
  lazy val joined: scala.Predef.String = hydra.lib.strings.intercalate("_")(snakeParts)
  hydra.formatting.sanitizeWithUnderscores(hydra.ext.lisp.language.lispReservedWords)(joined)
}

def qualifiedTypeName(name: hydra.core.Name): scala.Predef.String = hydra.formatting.capitalize(hydra.names.localNameOf(name))

def wrapInThunk(expr: hydra.ext.lisp.syntax.Expression): hydra.ext.lisp.syntax.Expression =
  hydra.ext.lisp.syntax.Expression.lambda(hydra.ext.lisp.syntax.Lambda(None, Seq(), None, Seq(expr)))
