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
  lists.foldl[Map[T0, Seq[T1]], Tuple2[T0, Seq[T1]]]((mp: Map[T0, Seq[T1]]) =>
  (p: Tuple2[T0, Seq[T1]]) =>
  {
  val k: T0 = pairs.first[T0, Seq[T1]](p)
  {
    val vs: Seq[T1] = pairs.second[T0, Seq[T1]](p)
    {
      val existing: Seq[T1] = maybes.maybe[Seq[T1], Seq[T1]](Seq())(equality.identity[Seq[T1]])(maps.lookup[T0, Seq[T1]](k)(mp))
      maps.insert[T0, Seq[T1]](k)(lists.concat2[T1](existing)(vs))(mp)
    }
  }
})(maps.empty[T0, Seq[T1]])(pairs)

def createOrderingIsomorphism[T0, T1](sourceOrd: Seq[T0])(targetOrd: Seq[T0]): hydra.topology.OrderingIsomorphism[T1] =
  {
  def sourceToTargetMapping[T2](els: Seq[T2]): Seq[T2] =
    {
    val mp: Map[T0, T2] = maps.fromList[T0, T2](lists.zip[T0, T2](sourceOrd)(els))
    maybes.cat[T2](lists.map[T0, Option[T2]]((n: T0) => maps.lookup[T0, T2](n)(mp))(targetOrd))
  }
  def targetToSourceMapping[T2](els: Seq[T2]): Seq[T2] =
    {
    val mp: Map[T0, T2] = maps.fromList[T0, T2](lists.zip[T0, T2](targetOrd)(els))
    maybes.cat[T2](lists.map[T0, Option[T2]]((n: T0) => maps.lookup[T0, T2](n)(mp))(sourceOrd))
  }
  hydra.topology.OrderingIsomorphism(sourceToTargetMapping, targetToSourceMapping)
}

def findReachableNodes[T0](adj: (T0 => scala.collection.immutable.Set[T0]))(root: T0): scala.collection.immutable.Set[T0] =
  {
  def visit(visited: scala.collection.immutable.Set[T0])(node: T0): scala.collection.immutable.Set[T0] =
    {
    val toVisit: scala.collection.immutable.Set[T0] = sets.difference[T0](adj(node))(visited)
    logic.ifElse[scala.collection.immutable.Set[T0]](sets.`null`[T0](toVisit))(visited)(lists.foldl[scala.collection.immutable.Set[T0],
       T0]((v: scala.collection.immutable.Set[T0]) => (n: T0) => visit(sets.insert[T0](n)(v))(n))(visited)(sets.toList[T0](toVisit)))
  }
  visit(sets.singleton[T0](root))(root)
}

def propagateTags[T0, T1](edges: Seq[Tuple2[T0, Seq[T0]]])(nodeTags: Seq[Tuple2[T0, Seq[T1]]]): Seq[Tuple2[T0, scala.collection.immutable.Set[T1]]] =
  {
  val adjMap: Map[T0, Seq[T0]] = hydra.sorting.adjacencyListToMap(edges)
  val tagMap: Map[T0, scala.collection.immutable.Set[T1]] = maps.map[Seq[T1], scala.collection.immutable.Set[T1],
     T0](sets.fromList[T1])(hydra.sorting.adjacencyListToMap(nodeTags))
  val allNodes: Seq[T0] = sets.toList[T0](sets.fromList[T0](lists.concat2[T0](lists.map[Tuple2[T0, Seq[T0]],
     T0](pairs.first[T0, Seq[T0]])(edges))(lists.map[Tuple2[T0, Seq[T1]], T0](pairs.first[T0, Seq[T1]])(nodeTags))))
  def getTagsForNode(node: T0): scala.collection.immutable.Set[T1] =
    {
    val reachable: scala.collection.immutable.Set[T0] = hydra.sorting.findReachableNodes((n: T0) =>
      sets.fromList[T0](maybes.maybe[Seq[T0], Seq[T0]](Seq())(equality.identity[Seq[T0]])(maps.lookup[T0, Seq[T0]](n)(adjMap))))(node)
    sets.unions[T1](lists.map[T0, scala.collection.immutable.Set[T1]]((n: T0) =>
      maybes.maybe[scala.collection.immutable.Set[T1], scala.collection.immutable.Set[T1]](sets.empty[T1])(equality.identity[scala.collection.immutable.Set[T1]])(maps.lookup[T0,
         scala.collection.immutable.Set[T1]](n)(tagMap)))(sets.toList[T0](reachable)))
  }
  lists.map[T0, Tuple2[T0, scala.collection.immutable.Set[T1]]]((n: T0) => Tuple2(n, getTagsForNode(n)))(allNodes)
}

def topologicalSort[T0](pairs: Seq[Tuple2[T0, Seq[T0]]]): Either[Seq[Seq[T0]], Seq[T0]] =
  {
  val sccs: Seq[Seq[T0]] = hydra.sorting.topologicalSortComponents(pairs)
  def isCycle[T1](scc: Seq[T1]): Boolean = logic.not(lists.`null`[T1](lists.tail[T1](scc)))
  val withCycles: Seq[Seq[T0]] = lists.filter[Seq[T0]](isCycle)(sccs)
  logic.ifElse[Either[Seq[Seq[T0]], Seq[T0]]](lists.`null`[Seq[T0]](withCycles))(Right(lists.concat[T0](sccs)))(Left(withCycles))
}

def topologicalSortComponents[T0](pairs: Seq[Tuple2[T0, Seq[T0]]]): Seq[Seq[T0]] =
  {
  val graphResult: Tuple2[Map[Int, Seq[Int]], (Int => T0)] = hydra.tarjan.adjacencyListsToGraph(pairs)
  val g: Map[Int, Seq[Int]] = pairs.first[Map[Int, Seq[Int]], (Int) => T0](graphResult)
  lists.map[Seq[Int], Seq[T0]]((comp: Seq[Int]) =>
    lists.map[Int, T0](pairs.second[Map[Int, Seq[Int]], (Int) => T0](graphResult))(comp))(hydra.tarjan.stronglyConnectedComponents(g))
}

def topologicalSortNodes[T0, T1](getKey: (T0 => T1))(getAdj: (T0 => Seq[T1]))(nodes: Seq[T0]): Seq[Seq[T0]] =
  {
  val nodesByKey: Map[T1, T0] = maps.fromList[T1, T0](lists.map[T0, Tuple2[T1, T0]]((n: T0) => Tuple2(getKey(n), n))(nodes))
  val pairs: Seq[Tuple2[T1, Seq[T1]]] = lists.map[T0, Tuple2[T1, Seq[T1]]]((n: T0) => Tuple2(getKey(n), getAdj(n)))(nodes)
  val comps: Seq[Seq[T1]] = hydra.sorting.topologicalSortComponents(pairs)
  lists.map[Seq[T1], Seq[T0]]((c: Seq[T1]) =>
    maybes.cat[T0](lists.map[T1, Option[T0]]((k: T1) => maps.lookup[T1, T0](k)(nodesByKey))(c)))(comps)
}
