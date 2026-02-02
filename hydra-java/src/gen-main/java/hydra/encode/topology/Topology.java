// Note: this is an automatically generated file. Do not edit.

package hydra.encode.topology;

/**
 * Term encoders for hydra.topology
 */
public interface Topology {
  static hydra.core.Term graph(java.util.Map<Integer, java.util.List<Integer>> m) {
    return new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
      (hydra.encode.topology.Topology::vertex),
      (java.util.function.Function<java.util.List<Integer>, hydra.core.Term>) (xs -> new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (hydra.encode.topology.Topology::vertex),
        (xs)))),
      (m)));
  }
  
  static hydra.core.Term tarjanState(hydra.topology.TarjanState x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.topology.TarjanState"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("counter"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(((x)).counter)))),
      new hydra.core.Field(new hydra.core.Name("indices"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        (hydra.encode.topology.Topology::vertex),
        (java.util.function.Function<Integer, hydra.core.Term>) (x2 -> new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32((x2))))),
        ((x)).indices))),
      new hydra.core.Field(new hydra.core.Name("lowLinks"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        (hydra.encode.topology.Topology::vertex),
        (java.util.function.Function<Integer, hydra.core.Term>) (x2 -> new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32((x2))))),
        ((x)).lowLinks))),
      new hydra.core.Field(new hydra.core.Name("stack"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (hydra.encode.topology.Topology::vertex),
        ((x)).stack))),
      new hydra.core.Field(new hydra.core.Name("onStack"), new hydra.core.Term.Set(hydra.lib.sets.Map.apply(
        (hydra.encode.topology.Topology::vertex),
        ((x)).onStack))),
      new hydra.core.Field(new hydra.core.Name("sccs"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<java.util.List<Integer>, hydra.core.Term>) (xs2 -> new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (hydra.encode.topology.Topology::vertex),
          (xs2)))),
        ((x)).sccs))))));
  }
  
  static hydra.core.Term vertex(Integer x) {
    return new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32((x))));
  }
}
