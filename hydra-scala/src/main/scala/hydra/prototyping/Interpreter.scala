package hydra.prototyping

import hydra.core.*

def freeVariables(term: Term): Set[Variable] = {
  def free(bound: Set[Variable], t: Term): List[Variable] = term match
    case Term.application(Application(t1, t2)) => free(bound, t1) ++ free(bound, t2)
    case Term.atomic(_) => List()
    case Term.cases(cases) => cases.flatMap(f => free(bound, f.term)).toList
    case Term.compareTo(t) => free(bound, t)
    case Term.data() => List()
    case Term.element(_) => List()
    case Term.function(_) => List()
    case Term.lambda(Lambda(v, t)) => free(bound + v, t)
    case Term.list(els) => els.flatMap(t => free(bound, t)).toList
    case Term.map(m) => (m map { case (k, v) => free(bound, k) ++ free(bound, v) }).toList.flatten
    case Term.projection(_) => List()
    case Term.record(fields) => fields.flatMap(f => free(bound, f.term)).toList
    case Term.set(els) => els.flatMap(t => free(bound, t)).toList
    case Term.union(f) => free(bound, f.term)
    case Term.variable(v) => if bound.contains(v) then List() else List(v)

  free(Set(), term).toSet
}

/**
 * Whether a term is closed, i.e. represents a complete program
 */
def termIsClosed(term: Term): Boolean = freeVariables(term).isEmpty
