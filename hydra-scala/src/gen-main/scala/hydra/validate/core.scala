package hydra.validate.core

import hydra.accessors.*

import hydra.core.*

import hydra.error.core.*

import hydra.lib.lists

import hydra.lib.logic

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.sets

def checkDuplicateBindings(path: hydra.accessors.AccessorPath)(bindings: Seq[hydra.core.Binding]): Option[hydra.error.core.InvalidTermError] =
  {
  lazy val names: Seq[hydra.core.Name] = hydra.lib.lists.map[hydra.core.Binding, hydra.core.Name]((x: hydra.core.Binding) => (x.name))(bindings)
  lazy val dup: Option[hydra.core.Name] = hydra.validate.core.findDuplicate(names)
  hydra.lib.maybes.map[hydra.core.Name, hydra.error.core.InvalidTermError]((name: hydra.core.Name) =>
    hydra.error.core.InvalidTermError.duplicateBinding(hydra.error.core.DuplicateBindingError(path, name)))(dup)
}

def checkDuplicateFields(path: hydra.accessors.AccessorPath)(names: Seq[hydra.core.Name]): Option[hydra.error.core.InvalidTermError] =
  {
  lazy val dup: Option[hydra.core.Name] = hydra.validate.core.findDuplicate(names)
  hydra.lib.maybes.map[hydra.core.Name, hydra.error.core.InvalidTermError]((name: hydra.core.Name) =>
    hydra.error.core.InvalidTermError.duplicateField(hydra.error.core.DuplicateFieldError(path, name)))(dup)
}

def checkTerm(path: hydra.accessors.AccessorPath)(term: hydra.core.Term): Option[hydra.error.core.InvalidTermError] =
  term match
  case hydra.core.Term.let(v_Term_let_lt) => hydra.validate.core.checkDuplicateBindings(path)(v_Term_let_lt.bindings)
  case hydra.core.Term.record(v_Term_record_rec) => hydra.validate.core.checkDuplicateFields(path)(hydra.lib.lists.map[hydra.core.Field, hydra.core.Name]((x: hydra.core.Field) => (x.name))(v_Term_record_rec.fields))
  case _ => None

def findDuplicate[T0](names: Seq[T0]): Option[T0] =
  {
  lazy val result: Tuple2[scala.collection.immutable.Set[T0], Option[T0]] = hydra.lib.lists.foldl[Tuple2[scala.collection.immutable.Set[T0], Option[T0]], T0]((acc: Tuple2[scala.collection.immutable.Set[T0], Option[T0]]) =>
    (name: T0) =>
    {
    lazy val seen: scala.collection.immutable.Set[T0] = hydra.lib.pairs.first[scala.collection.immutable.Set[T0], Option[T0]](acc)
    {
      lazy val dup: Option[T0] = hydra.lib.pairs.second[scala.collection.immutable.Set[T0], Option[T0]](acc)
      hydra.lib.maybes.cases[T0, Tuple2[scala.collection.immutable.Set[T0], Option[T0]]](dup)(hydra.lib.logic.ifElse[Tuple2[scala.collection.immutable.Set[T0], Option[T0]]](hydra.lib.sets.member[T0](name)(seen))(Tuple2(seen, Some(name)))(Tuple2(hydra.lib.sets.insert[T0](name)(seen), None)))((_x: T0) => acc)
    }
  })(Tuple2(hydra.lib.sets.empty[T0], None))(names)
  hydra.lib.pairs.second[scala.collection.immutable.Set[T0], Option[T0]](result)
}

def term(g: hydra.graph.Graph)(t: hydra.core.Term): Option[hydra.error.core.InvalidTermError] =
  hydra.rewriting.foldTermWithGraphAndPath((recurse: (Option[hydra.error.core.InvalidTermError] => hydra.core.Term => Option[hydra.error.core.InvalidTermError])) =>
  (path: Seq[hydra.accessors.TermAccessor]) =>
  (cx: hydra.graph.Graph) =>
  (acc: Option[hydra.error.core.InvalidTermError]) =>
  (trm: hydra.core.Term) =>
  hydra.lib.maybes.cases[hydra.error.core.InvalidTermError, Option[hydra.error.core.InvalidTermError]](acc)({
  lazy val checkResult: Option[hydra.error.core.InvalidTermError] = hydra.validate.core.checkTerm(path)(trm)
  hydra.lib.maybes.cases[hydra.error.core.InvalidTermError, Option[hydra.error.core.InvalidTermError]](checkResult)(recurse(None)(trm))((err: hydra.error.core.InvalidTermError) => Some(err))
})((_x: hydra.error.core.InvalidTermError) => acc))(g)(None)(t)
