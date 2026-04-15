package hydra.eval.lib.eithers

import hydra.core.*

import hydra.errors.*

def bimap[T0, T1](cx: T0)(g: T1)(leftFun: hydra.core.Term)(rightFun: hydra.core.Term)(eitherTerm: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  eitherTerm match
  case hydra.core.Term.either(v_Term_either_e) => Right(hydra.lib.eithers.either[hydra.core.Term,
     hydra.core.Term, hydra.core.Term]((`val`: hydra.core.Term) =>
    hydra.core.Term.either(Left(hydra.core.Term.application(hydra.core.Application(leftFun,
       `val`)))))((`val`: hydra.core.Term) =>
    hydra.core.Term.either(Right(hydra.core.Term.application(hydra.core.Application(rightFun,
       `val`)))))(v_Term_either_e))
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("either value",
     hydra.show.core.term(eitherTerm)))))

def bind[T0, T1](cx: T0)(g: T1)(eitherTerm: hydra.core.Term)(funTerm: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  eitherTerm match
  case hydra.core.Term.either(v_Term_either_e) => Right(hydra.lib.eithers.either[hydra.core.Term,
     hydra.core.Term, hydra.core.Term]((`val`: hydra.core.Term) => hydra.core.Term.either(Left(`val`)))((`val`: hydra.core.Term) =>
    hydra.core.Term.application(hydra.core.Application(funTerm, `val`)))(v_Term_either_e))
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("either value",
     hydra.show.core.term(eitherTerm)))))

def either[T0, T1](cx: T0)(g: T1)(leftFun: hydra.core.Term)(rightFun: hydra.core.Term)(eitherTerm: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  eitherTerm match
  case hydra.core.Term.either(v_Term_either_e) => Right(hydra.lib.eithers.either[hydra.core.Term,
     hydra.core.Term, hydra.core.Term]((`val`: hydra.core.Term) =>
    hydra.core.Term.application(hydra.core.Application(leftFun, `val`)))((`val`: hydra.core.Term) =>
    hydra.core.Term.application(hydra.core.Application(rightFun, `val`)))(v_Term_either_e))
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("either value",
     hydra.show.core.term(eitherTerm)))))

def foldl[T0](cx: T0)(g: hydra.graph.Graph)(funTerm: hydra.core.Term)(initTerm: hydra.core.Term)(listTerm: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], hydra.core.Term](hydra.extract.core.list(g)(listTerm))((elements: Seq[hydra.core.Term]) =>
  Right(hydra.lib.lists.foldl[hydra.core.Term, hydra.core.Term]((acc: hydra.core.Term) =>
  (el: hydra.core.Term) =>
  hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.eithers.either"),
     hydra.core.Term.lambda(hydra.core.Lambda("err", None, hydra.core.Term.either(Left(hydra.core.Term.variable("err"))))))),
     hydra.core.Term.lambda(hydra.core.Lambda("a", None, hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(funTerm,
     hydra.core.Term.variable("a"))), el)))))), acc)))(hydra.core.Term.either(Right(initTerm)))(elements)))

def fromLeft[T0, T1](cx: T0)(g: T1)(defaultTerm: hydra.core.Term)(eitherTerm: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  eitherTerm match
  case hydra.core.Term.either(v_Term_either_e) => Right(hydra.lib.eithers.either[hydra.core.Term,
     hydra.core.Term, hydra.core.Term]((`val`: hydra.core.Term) => `val`)((_x: hydra.core.Term) => defaultTerm)(v_Term_either_e))
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("either value",
     hydra.show.core.term(eitherTerm)))))

def fromRight[T0, T1](cx: T0)(g: T1)(defaultTerm: hydra.core.Term)(eitherTerm: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  eitherTerm match
  case hydra.core.Term.either(v_Term_either_e) => Right(hydra.lib.eithers.either[hydra.core.Term,
     hydra.core.Term, hydra.core.Term]((_x: hydra.core.Term) => defaultTerm)((`val`: hydra.core.Term) => `val`)(v_Term_either_e))
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("either value",
     hydra.show.core.term(eitherTerm)))))

def isLeft[T0, T1](cx: T0)(g: T1)(eitherTerm: hydra.core.Term): Either[hydra.errors.Error, hydra.core.Term] =
  eitherTerm match
  case hydra.core.Term.either(v_Term_either_e) => Right(hydra.lib.eithers.either[hydra.core.Term,
     hydra.core.Term, hydra.core.Term]((_x: hydra.core.Term) => hydra.core.Term.literal(hydra.core.Literal.boolean(true)))((_x: hydra.core.Term) => hydra.core.Term.literal(hydra.core.Literal.boolean(false)))(v_Term_either_e))
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("either value",
     hydra.show.core.term(eitherTerm)))))

def isRight[T0, T1](cx: T0)(g: T1)(eitherTerm: hydra.core.Term): Either[hydra.errors.Error, hydra.core.Term] =
  eitherTerm match
  case hydra.core.Term.either(v_Term_either_e) => Right(hydra.lib.eithers.either[hydra.core.Term,
     hydra.core.Term, hydra.core.Term]((_x: hydra.core.Term) => hydra.core.Term.literal(hydra.core.Literal.boolean(false)))((_x: hydra.core.Term) => hydra.core.Term.literal(hydra.core.Literal.boolean(true)))(v_Term_either_e))
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("either value",
     hydra.show.core.term(eitherTerm)))))

def lefts[T0](cx: T0)(g: hydra.graph.Graph)(listTerm: hydra.core.Term): Either[hydra.errors.Error, hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], hydra.core.Term](hydra.extract.core.list(g)(listTerm))((elements: Seq[hydra.core.Term]) =>
  Right(hydra.core.Term.list(hydra.lib.lists.foldl[Seq[hydra.core.Term], hydra.core.Term]((acc: Seq[hydra.core.Term]) =>
  (el: hydra.core.Term) =>
  el match
  case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.either[hydra.core.Term,
     hydra.core.Term, Seq[hydra.core.Term]]((`val`: hydra.core.Term) =>
    hydra.lib.lists.concat2[hydra.core.Term](acc)(hydra.lib.lists.pure[hydra.core.Term](`val`)))((_x: hydra.core.Term) => acc)(v_Term_either_e)
  case _ => acc)(Seq())(elements))))

def map[T0, T1](cx: T0)(g: T1)(rightFun: hydra.core.Term)(eitherTerm: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  eitherTerm match
  case hydra.core.Term.either(v_Term_either_e) => Right(hydra.lib.eithers.either[hydra.core.Term,
     hydra.core.Term, hydra.core.Term]((`val`: hydra.core.Term) => hydra.core.Term.either(Left(`val`)))((`val`: hydra.core.Term) =>
    hydra.core.Term.either(Right(hydra.core.Term.application(hydra.core.Application(rightFun,
       `val`)))))(v_Term_either_e))
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("either value",
     hydra.show.core.term(eitherTerm)))))

def mapList[T0](cx: T0)(g: hydra.graph.Graph)(funTerm: hydra.core.Term)(listTerm: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], hydra.core.Term](hydra.extract.core.list(g)(listTerm))((elements: Seq[hydra.core.Term]) =>
  Right(hydra.lib.lists.foldl[hydra.core.Term, hydra.core.Term]((acc: hydra.core.Term) =>
  (el: hydra.core.Term) =>
  hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.eithers.either"),
     hydra.core.Term.lambda(hydra.core.Lambda("err", None, hydra.core.Term.either(Left(hydra.core.Term.variable("err"))))))),
     hydra.core.Term.lambda(hydra.core.Lambda("y", None, hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.eithers.either"),
     hydra.core.Term.lambda(hydra.core.Lambda("accErr", None, hydra.core.Term.either(Left(hydra.core.Term.variable("accErr"))))))),
     hydra.core.Term.lambda(hydra.core.Lambda("ys", None, hydra.core.Term.either(Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.cons"),
     hydra.core.Term.variable("y"))), hydra.core.Term.variable("ys"))))))))), acc)))))),
     hydra.core.Term.application(hydra.core.Application(funTerm, el)))))(hydra.core.Term.either(Right(hydra.core.Term.list(Seq()))))(hydra.lib.lists.reverse[hydra.core.Term](elements))))

def mapMaybe[T0, T1](cx: T0)(g: T1)(funTerm: hydra.core.Term)(maybeTerm: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  maybeTerm match
  case hydra.core.Term.maybe(v_Term_maybe_opt) => Right(hydra.lib.maybes.maybe[hydra.core.Term,
     hydra.core.Term](hydra.core.Term.either(Right(hydra.core.Term.maybe(None))))((`val`: hydra.core.Term) =>
    hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.eithers.either"),
       hydra.core.Term.lambda(hydra.core.Lambda("err", None, hydra.core.Term.either(Left(hydra.core.Term.variable("err"))))))),
       hydra.core.Term.lambda(hydra.core.Lambda("y", None, hydra.core.Term.either(Right(hydra.core.Term.maybe(Some(hydra.core.Term.variable("y"))))))))),
       hydra.core.Term.application(hydra.core.Application(funTerm, `val`)))))(v_Term_maybe_opt))
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("maybe value",
     hydra.show.core.term(maybeTerm)))))

def mapSet[T0](cx: T0)(g: hydra.graph.Graph)(funTerm: hydra.core.Term)(setTerm: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, scala.collection.immutable.Set[hydra.core.Term],
     hydra.core.Term](hydra.extract.core.set(g)(setTerm))((elements: scala.collection.immutable.Set[hydra.core.Term]) =>
  Right(hydra.lib.lists.foldl[hydra.core.Term, hydra.core.Term]((acc: hydra.core.Term) =>
  (el: hydra.core.Term) =>
  hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.eithers.either"),
     hydra.core.Term.lambda(hydra.core.Lambda("err", None, hydra.core.Term.either(Left(hydra.core.Term.variable("err"))))))),
     hydra.core.Term.lambda(hydra.core.Lambda("y", None, hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.eithers.either"),
     hydra.core.Term.lambda(hydra.core.Lambda("accErr", None, hydra.core.Term.either(Left(hydra.core.Term.variable("accErr"))))))),
     hydra.core.Term.lambda(hydra.core.Lambda("ys", None, hydra.core.Term.either(Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.sets.insert"),
     hydra.core.Term.variable("y"))), hydra.core.Term.variable("ys"))))))))), acc)))))),
     hydra.core.Term.application(hydra.core.Application(funTerm, el)))))(hydra.core.Term.either(Right(hydra.core.Term.set(hydra.lib.sets.fromList[hydra.core.Term](Seq())))))(hydra.lib.sets.toList[hydra.core.Term](elements))))

def partitionEithers[T0](cx: T0)(g: hydra.graph.Graph)(listTerm: hydra.core.Term): Either[hydra.errors.Error,
   Tuple2[Seq[hydra.core.Term], Seq[hydra.core.Term]]] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], Tuple2[Seq[hydra.core.Term],
     Seq[hydra.core.Term]]](hydra.extract.core.list(g)(listTerm))((elements: Seq[hydra.core.Term]) =>
  Right(hydra.lib.lists.foldl[Tuple2[Seq[hydra.core.Term], Seq[hydra.core.Term]],
     hydra.core.Term]((acc: Tuple2[Seq[hydra.core.Term], Seq[hydra.core.Term]]) =>
  (el: hydra.core.Term) =>
  {
  lazy val ls: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term], Seq[hydra.core.Term]](acc)
  {
    lazy val rs: Seq[hydra.core.Term] = hydra.lib.pairs.second[Seq[hydra.core.Term], Seq[hydra.core.Term]](acc)
    el match
      case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.either[hydra.core.Term,
         hydra.core.Term, Tuple2[Seq[hydra.core.Term], Seq[hydra.core.Term]]]((`val`: hydra.core.Term) =>
        Tuple2(hydra.lib.lists.concat2[hydra.core.Term](ls)(hydra.lib.lists.pure[hydra.core.Term](`val`)),
           rs))((`val`: hydra.core.Term) =>
        Tuple2(ls, hydra.lib.lists.concat2[hydra.core.Term](rs)(hydra.lib.lists.pure[hydra.core.Term](`val`))))(v_Term_either_e)
      case _ => acc
  }
})(Tuple2(Seq(), Seq()))(elements)))

def rights[T0](cx: T0)(g: hydra.graph.Graph)(listTerm: hydra.core.Term): Either[hydra.errors.Error, hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], hydra.core.Term](hydra.extract.core.list(g)(listTerm))((elements: Seq[hydra.core.Term]) =>
  Right(hydra.core.Term.list(hydra.lib.lists.foldl[Seq[hydra.core.Term], hydra.core.Term]((acc: Seq[hydra.core.Term]) =>
  (el: hydra.core.Term) =>
  el match
  case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.either[hydra.core.Term,
     hydra.core.Term, Seq[hydra.core.Term]]((_x: hydra.core.Term) => acc)((`val`: hydra.core.Term) =>
    hydra.lib.lists.concat2[hydra.core.Term](acc)(hydra.lib.lists.pure[hydra.core.Term](`val`)))(v_Term_either_e)
  case _ => acc)(Seq())(elements))))
