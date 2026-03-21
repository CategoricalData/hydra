package hydra.sorting

import hydra.topology.*

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.sets

def adjacencyListToMap[T0, T1](pairs: Seq[Tuple2[T0, Seq[T1]]]): Map[T0, Seq[T1]] =
  hydra.lib.lists.foldl[Map[T0, Seq[T1]], Tuple2[T0, Seq[T1]]]((mp: Map[T0, Seq[T1]]) =>
  (p: Tuple2[T0, Seq[T1]]) =>
  {
  val k: T0 = hydra.lib.pairs.first[T0, Seq[T1]](p)
  {
    val vs: Seq[T1] = hydra.lib.pairs.second[T0, Seq[T1]](p)
    {
      val existing: Seq[T1] = hydra.lib.maybes.maybe[Seq[T1], Seq[T1]](Seq())(hydra.lib.equality.identity[Seq[T1]])(hydra.lib.maps.lookup[T0, Seq[T1]](k)(mp))
      hydra.lib.maps.insert[T0, Seq[T1]](k)(hydra.lib.lists.concat2[T1](existing)(vs))(mp)
    }
  }
})(hydra.lib.maps.empty[T0, Seq[T1]])(pairs)

def createOrderingIsomorphism[T0, T1](sourceOrd: Seq[T0])(targetOrd: Seq[T0]): hydra.topology.OrderingIsomorphism[T1] =
  {
  def sourceToTargetMapping[T2](els: Seq[T2]): Seq[T2] =
    {
    val mp: Map[T0, T2] = hydra.lib.maps.fromList[T0, T2](hydra.lib.lists.zip[T0, T2](sourceOrd)(els))
    hydra.lib.maybes.cat[T2](hydra.lib.lists.map[T0, Option[T2]]((n: T0) => hydra.lib.maps.lookup[T0, T2](n)(mp))(targetOrd))
  }
  def targetToSourceMapping[T2](els: Seq[T2]): Seq[T2] =
    {
    val mp: Map[T0, T2] = hydra.lib.maps.fromList[T0, T2](hydra.lib.lists.zip[T0, T2](targetOrd)(els))
    hydra.lib.maybes.cat[T2](hydra.lib.lists.map[T0, Option[T2]]((n: T0) => hydra.lib.maps.lookup[T0, T2](n)(mp))(sourceOrd))
  }
  hydra.topology.OrderingIsomorphism(sourceToTargetMapping, targetToSourceMapping)
}

def findReachableNodes[T0](adj: (T0 => scala.collection.immutable.Set[T0]))(root: T0): scala.collection.immutable.Set[T0] =
  {
  def visit(visited: scala.collection.immutable.Set[T0])(node: T0): scala.collection.immutable.Set[T0] =
    {
    val toVisit: scala.collection.immutable.Set[T0] = hydra.lib.sets.difference[T0](adj(node))(visited)
    hydra.lib.logic.ifElse[scala.collection.immutable.Set[T0]](hydra.lib.sets.`null`[T0](toVisit))(visited)(hydra.lib.lists.foldl[scala.collection.immutable.Set[T0],
       T0]((v: scala.collection.immutable.Set[T0]) => (n: T0) => visit(hydra.lib.sets.insert[T0](n)(v))(n))(visited)(hydra.lib.sets.toList[T0](toVisit)))
  }
  visit(hydra.lib.sets.singleton[T0](root))(root)
}

def propagateTags[T0, T1](edges: Seq[Tuple2[T0, Seq[T0]]])(nodeTags: Seq[Tuple2[T0, Seq[T1]]]): Seq[Tuple2[T0, scala.collection.immutable.Set[T1]]] =
  {
  val adjMap: Map[T0, Seq[T0]] = hydra.sorting.adjacencyListToMap(edges)
  val tagMap: Map[T0, scala.collection.immutable.Set[T1]] = hydra.lib.maps.map[Seq[T1], scala.collection.immutable.Set[T1],
     T0](hydra.lib.sets.fromList[T1])(hydra.sorting.adjacencyListToMap(nodeTags))
  val allNodes: Seq[T0] = hydra.lib.sets.toList[T0](hydra.lib.sets.fromList[T0](hydra.lib.lists.concat2[T0](hydra.lib.lists.map[Tuple2[T0,
     Seq[T0]], T0](hydra.lib.pairs.first[T0, Seq[T0]])(edges))(hydra.lib.lists.map[Tuple2[T0, Seq[T1]],
     T0](hydra.lib.pairs.first[T0, Seq[T1]])(nodeTags))))
  def getTagsForNode(node: T0): scala.collection.immutable.Set[T1] =
    {
    val reachable: scala.collection.immutable.Set[T0] = hydra.sorting.findReachableNodes((n: T0) =>
      hydra.lib.sets.fromList[T0](hydra.lib.maybes.maybe[Seq[T0], Seq[T0]](Seq())(hydra.lib.equality.identity[Seq[T0]])(hydra.lib.maps.lookup[T0,
         Seq[T0]](n)(adjMap))))(node)
    hydra.lib.sets.unions[T1](hydra.lib.lists.map[T0, scala.collection.immutable.Set[T1]]((n: T0) =>
      hydra.lib.maybes.maybe[scala.collection.immutable.Set[T1], scala.collection.immutable.Set[T1]](hydra.lib.sets.empty[T1])(hydra.lib.equality.identity[scala.collection.immutable.Set[T1]])(hydra.lib.maps.lookup[T0,
         scala.collection.immutable.Set[T1]](n)(tagMap)))(hydra.lib.sets.toList[T0](reachable)))
  }
  hydra.lib.lists.map[T0, Tuple2[T0, scala.collection.immutable.Set[T1]]]((n: T0) => Tuple2(n, getTagsForNode(n)))(allNodes)
}

def topologicalSort[T0](pairs: Seq[Tuple2[T0, Seq[T0]]]): Either[Seq[Seq[T0]], Seq[T0]] =
  {
  val sccs: Seq[Seq[T0]] = hydra.sorting.topologicalSortComponents(pairs)
  def isCycle[T1](scc: Seq[T1]): Boolean = hydra.lib.logic.not(hydra.lib.lists.`null`[T1](hydra.lib.lists.tail[T1](scc)))
  val withCycles: Seq[Seq[T0]] = hydra.lib.lists.filter[Seq[T0]](isCycle)(sccs)
  hydra.lib.logic.ifElse[Either[Seq[Seq[T0]], Seq[T0]]](hydra.lib.lists.`null`[Seq[T0]](withCycles))(Right(hydra.lib.lists.concat[T0](sccs)))(Left(withCycles))
}

def topologicalSortComponents[T0](pairs: Seq[Tuple2[T0, Seq[T0]]]): Seq[Seq[T0]] =
  {
  val graphResult: Tuple2[Map[Int, Seq[Int]], (Int => T0)] = hydra.tarjan.adjacencyListsToGraph(pairs)
  val g: Map[Int, Seq[Int]] = hydra.lib.pairs.first[Map[Int, Seq[Int]], (Int) => T0](graphResult)
  hydra.lib.lists.map[Seq[Int], Seq[T0]]((comp: Seq[Int]) =>
    hydra.lib.lists.map[Int, T0](hydra.lib.pairs.second[Map[Int, Seq[Int]], (Int) => T0](graphResult))(comp))(hydra.tarjan.stronglyConnectedComponents(g))
}

def topologicalSortNodes[T0, T1](getKey: (T0 => T1))(getAdj: (T0 => Seq[T1]))(nodes: Seq[T0]): Seq[Seq[T0]] =
  {
  val nodesByKey: Map[T1, T0] = hydra.lib.maps.fromList[T1, T0](hydra.lib.lists.map[T0, Tuple2[T1, T0]]((n: T0) => Tuple2(getKey(n), n))(nodes))
  val pairs: Seq[Tuple2[T1, Seq[T1]]] = hydra.lib.lists.map[T0, Tuple2[T1, Seq[T1]]]((n: T0) => Tuple2(getKey(n), getAdj(n)))(nodes)
  val comps: Seq[Seq[T1]] = hydra.sorting.topologicalSortComponents(pairs)
  hydra.lib.lists.map[Seq[T1], Seq[T0]]((c: Seq[T1]) =>
    hydra.lib.maybes.cat[T0](hydra.lib.lists.map[T1, Option[T0]]((k: T1) => hydra.lib.maps.lookup[T1, T0](k)(nodesByKey))(c)))(comps)
}
