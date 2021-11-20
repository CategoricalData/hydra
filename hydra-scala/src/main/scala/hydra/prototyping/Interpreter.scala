package hydra.prototyping

import hydra.core.*

def freeVariables[a](term: Term[a]): Set[Variable] = {
  def free(bound: Set[Variable], t: Term[a]): List[Variable] = term.data match
    case Expression.application(Application(t1, t2)) => free(bound, t1) ++ free(bound, t2)
    case Expression.atomic(_) => List()
    case Expression.element(_) => List()
    case Expression.function(fun) => functionFree(bound, fun)
    case Expression.list(els) => els.flatMap(t => free(bound, t)).toList
    case Expression.map(m) => (m map { case (k, v) => free(bound, k) ++ free(bound, v) }).toList.flatten
    case Expression.optional(m) => (m map {t => free(bound, t)}) getOrElse List()
    case Expression.record(fields) => fields.flatMap(f => free(bound, f.term)).toList
    case Expression.set(els) => els.flatMap(t => free(bound, t)).toList
    case Expression.union(f) => free(bound, f.term)
    case Expression.variable(v) => if bound.contains(v) then List() else List(v)

  def functionFree(bound: Set[Variable], fun: Function[a]): List[Variable] = fun match
    case Function.cases(cases) => cases.flatMap(f => free(bound, f.term)).toList
    case Function.compareTo(t) => free(bound, t)
    case Function.data() => List()
    case Function.lambda(Lambda(v, t)) => free(bound + v, t)
    case Function.primitive(_) => List()
    case Function.projection(_) => List()

  free(Set(), term).toSet
}



/**
 * Whether a term is closed, i.e. represents a complete program
 */
def termIsClosed[a](term: Term[a]): Boolean = freeVariables(term).isEmpty
