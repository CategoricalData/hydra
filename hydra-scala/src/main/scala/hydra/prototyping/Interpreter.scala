package hydra.prototyping

import hydra.core.*

def freeVariables(term: Term): Set[Variable] = {
  def free(bound: Set[Variable], t: Term): List[Variable] = term match
    case Term.application(Application(t1, t2)) => free(bound, t1) ++ free(bound, t2)
    case Term.atomic(_) => List()
    case Term.element(_) => List()
    case Term.function(fun) => functionFree(bound, fun)
    case Term.list(els) => els.flatMap(t => free(bound, t)).toList
    case Term.map(m) => (m map { case (k, v) => free(bound, k) ++ free(bound, v) }).toList.flatten
    case Term.record(fields) => fields.flatMap(f => free(bound, f.term)).toList
    case Term.set(els) => els.flatMap(t => free(bound, t)).toList
    case Term.union(f) => free(bound, f.term)
    case Term.variable(v) => if bound.contains(v) then List() else List(v)

  def functionFree(bound: Set[Variable], fun: Function): List[Variable] = fun match
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
def termIsClosed(term: Term): Boolean = freeVariables(term).isEmpty
