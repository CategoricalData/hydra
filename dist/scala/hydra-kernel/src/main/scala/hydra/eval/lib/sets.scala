package hydra.eval.lib.sets

import hydra.core.*

def difference[T0](cx: T0)(g: hydra.graph.Graph)(set1Term: hydra.core.Term)(set2Term: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, scala.collection.immutable.Set[hydra.core.Term],
     hydra.core.Term](hydra.extract.core.set(g)(set1Term))((elements: scala.collection.immutable.Set[hydra.core.Term]) =>
  Right(hydra.lib.lists.foldl[hydra.core.Term, hydra.core.Term]((acc: hydra.core.Term) =>
  (el: hydra.core.Term) =>
  hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.logic.ifElse"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.sets.member"),
     el)), set2Term)))), acc)), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.sets.insert"),
     el)), acc)))))(hydra.core.Term.set(hydra.lib.sets.fromList[hydra.core.Term](Seq())))(hydra.lib.sets.toList[hydra.core.Term](elements))))

def intersection[T0](cx: T0)(g: hydra.graph.Graph)(set1Term: hydra.core.Term)(set2Term: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, scala.collection.immutable.Set[hydra.core.Term],
     hydra.core.Term](hydra.extract.core.set(g)(set1Term))((elements: scala.collection.immutable.Set[hydra.core.Term]) =>
  Right(hydra.lib.lists.foldl[hydra.core.Term, hydra.core.Term]((acc: hydra.core.Term) =>
  (el: hydra.core.Term) =>
  hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.logic.ifElse"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.sets.member"),
     el)), set2Term)))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.sets.insert"),
     el)), acc)))), acc)))(hydra.core.Term.set(hydra.lib.sets.fromList[hydra.core.Term](Seq())))(hydra.lib.sets.toList[hydra.core.Term](elements))))

def map[T0](cx: T0)(g: hydra.graph.Graph)(fun: hydra.core.Term)(setTerm: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, scala.collection.immutable.Set[hydra.core.Term],
     hydra.core.Term](hydra.extract.core.set(g)(setTerm))((elements: scala.collection.immutable.Set[hydra.core.Term]) =>
  Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.sets.fromList"),
     hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Term, hydra.core.Term]((el: hydra.core.Term) => hydra.core.Term.application(hydra.core.Application(fun,
     el)))(hydra.lib.sets.toList[hydra.core.Term](elements)))))))

def union[T0](cx: T0)(g: hydra.graph.Graph)(set1Term: hydra.core.Term)(set2Term: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, scala.collection.immutable.Set[hydra.core.Term],
     hydra.core.Term](hydra.extract.core.set(g)(set1Term))((elements: scala.collection.immutable.Set[hydra.core.Term]) =>
  Right(hydra.lib.lists.foldl[hydra.core.Term, hydra.core.Term]((acc: hydra.core.Term) =>
  (el: hydra.core.Term) =>
  hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.sets.insert"),
     el)), acc)))(set2Term)(hydra.lib.sets.toList[hydra.core.Term](elements))))

def unions[T0](cx: T0)(g: hydra.graph.Graph)(listTerm: hydra.core.Term): Either[hydra.errors.Error, hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], hydra.core.Term](hydra.extract.core.list(g)(listTerm))((elements: Seq[hydra.core.Term]) =>
  Right(hydra.lib.lists.foldl[hydra.core.Term, hydra.core.Term]((acc: hydra.core.Term) =>
  (s: hydra.core.Term) =>
  hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.sets.union"),
     acc)), s)))(hydra.core.Term.set(hydra.lib.sets.fromList[hydra.core.Term](Seq())))(elements)))
