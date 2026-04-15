package hydra.encode.topology

import hydra.core.*

import hydra.topology.*

def graph(m: Map[Int, Seq[Int]]): hydra.core.Term =
  hydra.core.Term.map(hydra.lib.maps.bimap[Int, hydra.core.Term, Seq[Int], hydra.core.Term](hydra.encode.topology.vertex)((xs: Seq[Int]) =>
  hydra.core.Term.list(hydra.lib.lists.map[Int, hydra.core.Term](hydra.encode.topology.vertex)(xs)))(m))

def tarjanState(x: hydra.topology.TarjanState): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.topology.TarjanState", Seq(hydra.core.Field("counter",
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(x.counter)))),
     hydra.core.Field("indices", hydra.core.Term.map(hydra.lib.maps.bimap[Int, hydra.core.Term,
     Int, hydra.core.Term](hydra.encode.topology.vertex)((x2: Int) =>
  hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(x2))))(x.indices))),
     hydra.core.Field("lowLinks", hydra.core.Term.map(hydra.lib.maps.bimap[Int, hydra.core.Term,
     Int, hydra.core.Term](hydra.encode.topology.vertex)((x2: Int) =>
  hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(x2))))(x.lowLinks))),
     hydra.core.Field("stack", hydra.core.Term.list(hydra.lib.lists.map[Int, hydra.core.Term](hydra.encode.topology.vertex)(x.stack))),
     hydra.core.Field("onStack", hydra.core.Term.set(hydra.lib.sets.map[Int, hydra.core.Term](hydra.encode.topology.vertex)(x.onStack))),
     hydra.core.Field("sccs", hydra.core.Term.list(hydra.lib.lists.map[Seq[Int], hydra.core.Term]((xs2: Seq[Int]) =>
  hydra.core.Term.list(hydra.lib.lists.map[Int, hydra.core.Term](hydra.encode.topology.vertex)(xs2)))(x.sccs))))))

def vertex(x: Int): hydra.core.Term =
  hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(x)))
