// Note: this is an automatically generated file. Do not edit.

package hydra.dsl;

/**
 * DSL functions for hydra.topology
 */
public interface Topology {
  static <A> hydra.phantoms.TTerm<hydra.topology.OrderingIsomorphism<A>> orderingIsomorphism(hydra.phantoms.TTerm<java.util.function.Function<hydra.util.ConsList<A>, hydra.util.ConsList<A>>> encode, hydra.phantoms.TTerm<java.util.function.Function<hydra.util.ConsList<A>, hydra.util.ConsList<A>>> decode) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.topology.OrderingIsomorphism"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("encode"), (encode).value),
      new hydra.core.Field(new hydra.core.Name("decode"), (decode).value)))));
  }

  static <A> hydra.phantoms.TTerm<java.util.function.Function<hydra.util.ConsList<A>, hydra.util.ConsList<A>>> orderingIsomorphismDecode(hydra.phantoms.TTerm<hydra.topology.OrderingIsomorphism<A>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.OrderingIsomorphism"), new hydra.core.Name("decode"))))), (x).value)));
  }

  static <A> hydra.phantoms.TTerm<java.util.function.Function<hydra.util.ConsList<A>, hydra.util.ConsList<A>>> orderingIsomorphismEncode(hydra.phantoms.TTerm<hydra.topology.OrderingIsomorphism<A>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.OrderingIsomorphism"), new hydra.core.Name("encode"))))), (x).value)));
  }

  static <A> hydra.phantoms.TTerm<hydra.topology.OrderingIsomorphism<A>> orderingIsomorphismWithDecode(hydra.phantoms.TTerm<hydra.topology.OrderingIsomorphism<A>> original, hydra.phantoms.TTerm<java.util.function.Function<hydra.util.ConsList<A>, hydra.util.ConsList<A>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.topology.OrderingIsomorphism"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("encode"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.OrderingIsomorphism"), new hydra.core.Name("encode"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("decode"), (newVal).value)))));
  }

  static <A> hydra.phantoms.TTerm<hydra.topology.OrderingIsomorphism<A>> orderingIsomorphismWithEncode(hydra.phantoms.TTerm<hydra.topology.OrderingIsomorphism<A>> original, hydra.phantoms.TTerm<java.util.function.Function<hydra.util.ConsList<A>, hydra.util.ConsList<A>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.topology.OrderingIsomorphism"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("encode"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("decode"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.OrderingIsomorphism"), new hydra.core.Name("decode"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.topology.TarjanState> tarjanState(hydra.phantoms.TTerm<Integer> counter, hydra.phantoms.TTerm<hydra.util.PersistentMap<Integer, Integer>> indices, hydra.phantoms.TTerm<hydra.util.PersistentMap<Integer, Integer>> lowLinks, hydra.phantoms.TTerm<hydra.util.ConsList<Integer>> stack, hydra.phantoms.TTerm<hydra.util.PersistentSet<Integer>> onStack, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.util.ConsList<Integer>>> sccs) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.topology.TarjanState"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("counter"), (counter).value),
      new hydra.core.Field(new hydra.core.Name("indices"), (indices).value),
      new hydra.core.Field(new hydra.core.Name("lowLinks"), (lowLinks).value),
      new hydra.core.Field(new hydra.core.Name("stack"), (stack).value),
      new hydra.core.Field(new hydra.core.Name("onStack"), (onStack).value),
      new hydra.core.Field(new hydra.core.Name("sccs"), (sccs).value)))));
  }

  static hydra.phantoms.TTerm<Integer> tarjanStateCounter(hydra.phantoms.TTerm<hydra.topology.TarjanState> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("counter"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.PersistentMap<Integer, Integer>> tarjanStateIndices(hydra.phantoms.TTerm<hydra.topology.TarjanState> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("indices"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.PersistentMap<Integer, Integer>> tarjanStateLowLinks(hydra.phantoms.TTerm<hydra.topology.TarjanState> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("lowLinks"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.PersistentSet<Integer>> tarjanStateOnStack(hydra.phantoms.TTerm<hydra.topology.TarjanState> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("onStack"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.util.ConsList<Integer>>> tarjanStateSccs(hydra.phantoms.TTerm<hydra.topology.TarjanState> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("sccs"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<Integer>> tarjanStateStack(hydra.phantoms.TTerm<hydra.topology.TarjanState> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("stack"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.topology.TarjanState> tarjanStateWithCounter(hydra.phantoms.TTerm<hydra.topology.TarjanState> original, hydra.phantoms.TTerm<Integer> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.topology.TarjanState"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("counter"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("indices"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("indices"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("lowLinks"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("lowLinks"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("stack"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("stack"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("onStack"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("onStack"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("sccs"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("sccs"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.topology.TarjanState> tarjanStateWithIndices(hydra.phantoms.TTerm<hydra.topology.TarjanState> original, hydra.phantoms.TTerm<hydra.util.PersistentMap<Integer, Integer>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.topology.TarjanState"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("counter"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("counter"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("indices"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("lowLinks"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("lowLinks"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("stack"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("stack"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("onStack"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("onStack"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("sccs"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("sccs"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.topology.TarjanState> tarjanStateWithLowLinks(hydra.phantoms.TTerm<hydra.topology.TarjanState> original, hydra.phantoms.TTerm<hydra.util.PersistentMap<Integer, Integer>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.topology.TarjanState"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("counter"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("counter"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("indices"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("indices"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("lowLinks"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("stack"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("stack"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("onStack"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("onStack"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("sccs"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("sccs"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.topology.TarjanState> tarjanStateWithOnStack(hydra.phantoms.TTerm<hydra.topology.TarjanState> original, hydra.phantoms.TTerm<hydra.util.PersistentSet<Integer>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.topology.TarjanState"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("counter"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("counter"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("indices"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("indices"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("lowLinks"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("lowLinks"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("stack"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("stack"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("onStack"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("sccs"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("sccs"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.topology.TarjanState> tarjanStateWithSccs(hydra.phantoms.TTerm<hydra.topology.TarjanState> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.util.ConsList<Integer>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.topology.TarjanState"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("counter"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("counter"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("indices"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("indices"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("lowLinks"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("lowLinks"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("stack"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("stack"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("onStack"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("onStack"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("sccs"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.topology.TarjanState> tarjanStateWithStack(hydra.phantoms.TTerm<hydra.topology.TarjanState> original, hydra.phantoms.TTerm<hydra.util.ConsList<Integer>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.topology.TarjanState"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("counter"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("counter"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("indices"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("indices"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("lowLinks"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("lowLinks"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("stack"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("onStack"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("onStack"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("sccs"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.topology.TarjanState"), new hydra.core.Name("sccs"))))), (original).value)))))));
  }
}
