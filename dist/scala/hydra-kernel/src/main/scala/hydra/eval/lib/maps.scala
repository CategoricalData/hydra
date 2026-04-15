package hydra.eval.lib.maps

import hydra.core.*

import hydra.errors.*

def alter[T0, T1](cx: T0)(g: T1)(funTerm: hydra.core.Term)(keyTerm: hydra.core.Term)(mapTerm: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  mapTerm match
  case hydra.core.Term.map(v_Term_map_m) => {
    lazy val currentVal: Option[hydra.core.Term] = hydra.lib.maps.lookup[hydra.core.Term,
       hydra.core.Term](keyTerm)(v_Term_map_m)
    {
      lazy val newVal: hydra.core.Term = hydra.core.Term.application(hydra.core.Application(funTerm,
         hydra.core.Term.maybe(currentVal)))
      Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.maybes.maybe"),
         hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.maps.delete"),
         keyTerm)), mapTerm)))), hydra.core.Term.lambda(hydra.core.Lambda("newV",
         None, hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.maps.insert"),
         keyTerm)), hydra.core.Term.variable("newV"))), mapTerm)))))), newVal)))
    }
  }
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("map value",
     hydra.show.core.term(mapTerm)))))

def bimap[T0, T1](cx: T0)(g: T1)(keyFun: hydra.core.Term)(valFun: hydra.core.Term)(mapTerm: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  mapTerm match
  case hydra.core.Term.map(v_Term_map_m) => {
    lazy val pairs: Seq[Tuple2[hydra.core.Term, hydra.core.Term]] = hydra.lib.maps.toList[hydra.core.Term,
       hydra.core.Term](v_Term_map_m)
    Right(hydra.core.Term.map(hydra.lib.maps.fromList[hydra.core.Term, hydra.core.Term](hydra.lib.lists.map[Tuple2[hydra.core.Term,
       hydra.core.Term], Tuple2[hydra.core.Term, hydra.core.Term]]((p: Tuple2[hydra.core.Term,
       hydra.core.Term]) =>
      {
      lazy val k: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](p)
      {
        lazy val v: hydra.core.Term = hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](p)
        Tuple2(hydra.core.Term.application(hydra.core.Application(keyFun, k)), hydra.core.Term.application(hydra.core.Application(valFun,
           v)))
      }
    })(pairs))))
  }
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("map value",
     hydra.show.core.term(mapTerm)))))

def filter[T0, T1](cx: T0)(g: T1)(valPred: hydra.core.Term)(mapTerm: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  mapTerm match
  case hydra.core.Term.map(v_Term_map_m) => {
    lazy val pairs: Seq[Tuple2[hydra.core.Term, hydra.core.Term]] = hydra.lib.maps.toList[hydra.core.Term,
       hydra.core.Term](v_Term_map_m)
    Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.maps.fromList"),
       hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.concat"),
       hydra.core.Term.list(hydra.lib.lists.map[Tuple2[hydra.core.Term, hydra.core.Term],
       hydra.core.Term]((p: Tuple2[hydra.core.Term, hydra.core.Term]) =>
      {
      lazy val v: hydra.core.Term = hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](p)
      hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.logic.ifElse"),
         hydra.core.Term.application(hydra.core.Application(valPred, v)))), hydra.core.Term.list(hydra.lib.lists.pure[hydra.core.Term](hydra.core.Term.pair(Tuple2(hydra.lib.pairs.first[hydra.core.Term,
         hydra.core.Term](p), v)))))), hydra.core.Term.list(Seq())))
    })(pairs)))))))
  }
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("map value",
     hydra.show.core.term(mapTerm)))))

def filterWithKey[T0, T1](cx: T0)(g: T1)(pred: hydra.core.Term)(mapTerm: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  mapTerm match
  case hydra.core.Term.map(v_Term_map_m) => {
    lazy val pairs: Seq[Tuple2[hydra.core.Term, hydra.core.Term]] = hydra.lib.maps.toList[hydra.core.Term,
       hydra.core.Term](v_Term_map_m)
    Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.maps.fromList"),
       hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.concat"),
       hydra.core.Term.list(hydra.lib.lists.map[Tuple2[hydra.core.Term, hydra.core.Term],
       hydra.core.Term]((p: Tuple2[hydra.core.Term, hydra.core.Term]) =>
      {
      lazy val k: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](p)
      {
        lazy val v: hydra.core.Term = hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](p)
        hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.logic.ifElse"),
           hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(pred,
           k)), v)))), hydra.core.Term.list(hydra.lib.lists.pure[hydra.core.Term](hydra.core.Term.pair(Tuple2(k,
           v)))))), hydra.core.Term.list(Seq())))
      }
    })(pairs)))))))
  }
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("map value",
     hydra.show.core.term(mapTerm)))))

def findWithDefault[T0, T1, T2](cx: T0)(g: T1)(defaultTerm: hydra.core.Term)(keyTerm: hydra.core.Term)(mapTerm: hydra.core.Term): Either[T2,
   hydra.core.Term] =
  Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.maybes.fromMaybe"),
     defaultTerm)), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.maps.lookup"),
     keyTerm)), mapTerm)))))

def map[T0, T1](cx: T0)(g: T1)(valFun: hydra.core.Term)(mapTerm: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  mapTerm match
  case hydra.core.Term.map(v_Term_map_m) => {
    lazy val pairs: Seq[Tuple2[hydra.core.Term, hydra.core.Term]] = hydra.lib.maps.toList[hydra.core.Term,
       hydra.core.Term](v_Term_map_m)
    Right(hydra.core.Term.map(hydra.lib.maps.fromList[hydra.core.Term, hydra.core.Term](hydra.lib.lists.map[Tuple2[hydra.core.Term,
       hydra.core.Term], Tuple2[hydra.core.Term, hydra.core.Term]]((p: Tuple2[hydra.core.Term,
       hydra.core.Term]) =>
      {
      lazy val k: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](p)
      {
        lazy val v: hydra.core.Term = hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](p)
        Tuple2(k, hydra.core.Term.application(hydra.core.Application(valFun, v)))
      }
    })(pairs))))
  }
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("map value",
     hydra.show.core.term(mapTerm)))))

def mapKeys[T0, T1](cx: T0)(g: T1)(keyFun: hydra.core.Term)(mapTerm: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  mapTerm match
  case hydra.core.Term.map(v_Term_map_m) => {
    lazy val pairs: Seq[Tuple2[hydra.core.Term, hydra.core.Term]] = hydra.lib.maps.toList[hydra.core.Term,
       hydra.core.Term](v_Term_map_m)
    Right(hydra.core.Term.map(hydra.lib.maps.fromList[hydra.core.Term, hydra.core.Term](hydra.lib.lists.map[Tuple2[hydra.core.Term,
       hydra.core.Term], Tuple2[hydra.core.Term, hydra.core.Term]]((p: Tuple2[hydra.core.Term,
       hydra.core.Term]) =>
      {
      lazy val k: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](p)
      {
        lazy val v: hydra.core.Term = hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](p)
        Tuple2(hydra.core.Term.application(hydra.core.Application(keyFun, k)), v)
      }
    })(pairs))))
  }
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("map value",
     hydra.show.core.term(mapTerm)))))
