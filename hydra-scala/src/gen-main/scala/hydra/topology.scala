package hydra.topology

import hydra.core.*

type Graph = Map[hydra.topology.Vertex, Seq[hydra.topology.Vertex]]

case class OrderingIsomorphism[A](encode: (Seq[A] => Seq[A]), decode: (Seq[A] => Seq[A]))

case class TarjanState(counter: Int, indices: Map[hydra.topology.Vertex, Int], lowLinks: Map[hydra.topology.Vertex, Int], stack: Seq[hydra.topology.Vertex], onStack: scala.collection.immutable.Set[hydra.topology.Vertex], sccs: Seq[Seq[hydra.topology.Vertex]])

type Vertex = Int
