package hydra.sorting

import hydra.topology.*

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.math

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

def adjacencyListsToGraph[T0](edges0: Seq[Tuple2[T0, Seq[T0]]]): Tuple2[Map[Int, Seq[Int]], (Int => T0)] =
  {
  val sortedEdges: Seq[Tuple2[T0, Seq[T0]]] = hydra.lib.lists.sortOn[Tuple2[T0, Seq[T0]], T0](hydra.lib.pairs.first[T0, Seq[T0]])(edges0)
  val indexedEdges: Seq[Tuple2[Int, Tuple2[T0, Seq[T0]]]] = hydra.lib.lists.zip[Int, Tuple2[T0, Seq[T0]]](hydra.lib.math.range(0)(hydra.lib.lists.length[Tuple2[T0,
     Seq[T0]]](sortedEdges)))(sortedEdges)
  val keyToVertex: Map[T0, Int] = hydra.lib.maps.fromList[T0, Int](hydra.lib.lists.map[Tuple2[Int, Tuple2[T0,
     Seq[T0]]], Tuple2[T0, Int]]((vkNeighbors: Tuple2[Int, Tuple2[T0, Seq[T0]]]) =>
    {
    val v: Int = hydra.lib.pairs.first[Int, Tuple2[T0, Seq[T0]]](vkNeighbors)
    {
      val kNeighbors: Tuple2[T0, Seq[T0]] = hydra.lib.pairs.second[Int, Tuple2[T0, Seq[T0]]](vkNeighbors)
      {
        val k: T0 = hydra.lib.pairs.first[T0, Seq[T0]](kNeighbors)
        Tuple2(k, v)
      }
    }
  })(indexedEdges))
  val vertexMap: Map[Int, T0] = hydra.lib.maps.fromList[Int, T0](hydra.lib.lists.map[Tuple2[Int, Tuple2[T0,
     Seq[T0]]], Tuple2[Int, T0]]((vkNeighbors: Tuple2[Int, Tuple2[T0, Seq[T0]]]) =>
    {
    val v: Int = hydra.lib.pairs.first[Int, Tuple2[T0, Seq[T0]]](vkNeighbors)
    {
      val kNeighbors: Tuple2[T0, Seq[T0]] = hydra.lib.pairs.second[Int, Tuple2[T0, Seq[T0]]](vkNeighbors)
      {
        val k: T0 = hydra.lib.pairs.first[T0, Seq[T0]](kNeighbors)
        Tuple2(v, k)
      }
    }
  })(indexedEdges))
  val graph: Map[Int, Seq[Int]] = hydra.lib.maps.fromList[Int, Seq[Int]](hydra.lib.lists.map[Tuple2[Int,
     Tuple2[T0, Seq[T0]]], Tuple2[Int, Seq[Int]]]((vkNeighbors: Tuple2[Int, Tuple2[T0, Seq[T0]]]) =>
    {
    val v: Int = hydra.lib.pairs.first[Int, Tuple2[T0, Seq[T0]]](vkNeighbors)
    {
      val kNeighbors: Tuple2[T0, Seq[T0]] = hydra.lib.pairs.second[Int, Tuple2[T0, Seq[T0]]](vkNeighbors)
      {
        val neighbors: Seq[T0] = hydra.lib.pairs.second[T0, Seq[T0]](kNeighbors)
        Tuple2(v, hydra.lib.maybes.mapMaybe[T0, Int]((k: T0) => hydra.lib.maps.lookup[T0, Int](k)(keyToVertex))(neighbors))
      }
    }
  })(indexedEdges))
  def vertexToKey(v: Int): T0 = hydra.lib.maybes.fromJust[T0](hydra.lib.maps.lookup[Int, T0](v)(vertexMap))
  Tuple2(graph, vertexToKey)
}

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

val initialState: hydra.topology.TarjanState = hydra.topology.TarjanState(0, hydra.lib.maps.empty[Int,
   Int], hydra.lib.maps.empty[Int, Int], Seq(), hydra.lib.sets.empty[Int], Seq())

def popStackUntil(v: Int)(st0: hydra.topology.TarjanState): Tuple2[Seq[Int], hydra.topology.TarjanState] =
  {
  def go(acc: Seq[Int])(st: hydra.topology.TarjanState): Tuple2[Seq[Int], hydra.topology.TarjanState] =
    {
    val x: Int = hydra.lib.lists.head[Int](st.stack)
    val xs: Seq[Int] = hydra.lib.lists.tail[Int](st.stack)
    val newSt: hydra.topology.TarjanState = hydra.topology.TarjanState(st.counter, (st.indices), (st.lowLinks), xs, (st.onStack), (st.sccs))
    val newSt2: hydra.topology.TarjanState = hydra.topology.TarjanState(newSt.counter, (newSt.indices),
       (newSt.lowLinks), (newSt.stack), hydra.lib.sets.delete[Int](x)(st.onStack), (newSt.sccs))
    val `acc_`: Seq[Int] = hydra.lib.lists.cons[Int](x)(acc)
    hydra.lib.logic.ifElse[Tuple2[Seq[Int], hydra.topology.TarjanState]](hydra.lib.equality.equal[Int](x)(v))(Tuple2(hydra.lib.lists.reverse[Int](`acc_`),
       newSt2))(go(`acc_`)(newSt2))
  }
  go(Seq())(st0)
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

def strongConnect(graph: Map[Int, Seq[Int]])(v: Int)(st: hydra.topology.TarjanState): hydra.topology.TarjanState =
  {
  val i: Int = (st.counter)
  val newSt: hydra.topology.TarjanState = hydra.topology.TarjanState(hydra.lib.math.add(i)(1), hydra.lib.maps.insert[Int,
     Int](v)(i)(st.indices), hydra.lib.maps.insert[Int, Int](v)(i)(st.lowLinks), hydra.lib.lists.cons[Int](v)(st.stack),
     hydra.lib.sets.insert[Int](v)(st.onStack), (st.sccs))
  val neighbors: Seq[Int] = hydra.lib.maps.findWithDefault[Seq[Int], Int](Seq())(v)(graph)
  def processNeighbor(`st_`: hydra.topology.TarjanState)(w: Int): hydra.topology.TarjanState =
    {
    def lowLink(s: hydra.topology.TarjanState): hydra.topology.TarjanState =
      {
      val lowV1: Int = hydra.lib.maps.findWithDefault[Int, Int](hydra.constants.maxInt32)(v)(s.lowLinks)
      val idx_w: Int = hydra.lib.maps.findWithDefault[Int, Int](hydra.constants.maxInt32)(w)(s.indices)
      hydra.topology.TarjanState(s.counter, (s.indices), hydra.lib.maps.insert[Int, Int](v)(hydra.lib.equality.min[Int](lowV1)(idx_w))(s.lowLinks),
         (s.stack), (s.onStack), (s.sccs))
    }
    hydra.lib.logic.ifElse[hydra.topology.TarjanState](hydra.lib.logic.not(hydra.lib.maps.member[Int, Int](w)(`st_`.indices)))({
      val stAfter: hydra.topology.TarjanState = hydra.sorting.strongConnect(graph)(w)(`st_`)
      {
        val lowV2: Int = hydra.lib.maps.findWithDefault[Int, Int](hydra.constants.maxInt32)(v)(stAfter.lowLinks)
        {
          val low_w: Int = hydra.lib.maps.findWithDefault[Int, Int](hydra.constants.maxInt32)(w)(stAfter.lowLinks)
          hydra.topology.TarjanState(stAfter.counter, (stAfter.indices), hydra.lib.maps.insert[Int, Int](v)(hydra.lib.equality.min[Int](lowV2)(low_w))(stAfter.lowLinks),
             (stAfter.stack), (stAfter.onStack), (stAfter.sccs))
        }
      }
    })(hydra.lib.logic.ifElse[hydra.topology.TarjanState](hydra.lib.sets.member[Int](w)(`st_`.onStack))(lowLink(`st_`))(`st_`))
  }
  val stAfterNeighbors: hydra.topology.TarjanState = hydra.lib.lists.foldl[hydra.topology.TarjanState, Int](processNeighbor)(newSt)(neighbors)
  val low_v: Int = hydra.lib.maps.findWithDefault[Int, Int](hydra.constants.maxInt32)(v)(stAfterNeighbors.lowLinks)
  val idx_v: Int = hydra.lib.maps.findWithDefault[Int, Int](hydra.constants.maxInt32)(v)(stAfterNeighbors.indices)
  hydra.lib.logic.ifElse[hydra.topology.TarjanState](hydra.lib.equality.equal[Int](low_v)(idx_v))({
    val compResult: Tuple2[Seq[Int], hydra.topology.TarjanState] = hydra.sorting.popStackUntil(v)(stAfterNeighbors)
    {
      val comp: Seq[Int] = hydra.lib.pairs.first[Seq[Int], hydra.topology.TarjanState](compResult)
      {
        val stPopped: hydra.topology.TarjanState = hydra.lib.pairs.second[Seq[Int], hydra.topology.TarjanState](compResult)
        hydra.topology.TarjanState(stPopped.counter, (stPopped.indices), (stPopped.lowLinks), (stPopped.stack),
           (stPopped.onStack), hydra.lib.lists.cons[Seq[Int]](comp)(stPopped.sccs))
      }
    }
  })(stAfterNeighbors)
}

def stronglyConnectedComponents(graph: Map[Int, Seq[Int]]): Seq[Seq[Int]] =
  {
  val verts: Seq[Int] = hydra.lib.maps.keys[Int, Seq[Int]](graph)
  val finalState: hydra.topology.TarjanState = hydra.lib.lists.foldl[hydra.topology.TarjanState, Int]((st: hydra.topology.TarjanState) =>
    (v: Int) =>
    hydra.lib.logic.ifElse[hydra.topology.TarjanState](hydra.lib.maps.member[Int, Int](v)(st.indices))(st)(hydra.sorting.strongConnect(graph)(v)(st)))(hydra.sorting.initialState)(verts)
  hydra.lib.lists.reverse[Seq[Int]](hydra.lib.lists.map[Seq[Int], Seq[Int]](hydra.lib.lists.sort[Int])(finalState.sccs))
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
  val graphResult: Tuple2[Map[Int, Seq[Int]], (Int => T0)] = hydra.sorting.adjacencyListsToGraph(pairs)
  val g: Map[Int, Seq[Int]] = hydra.lib.pairs.first[Map[Int, Seq[Int]], (Int) => T0](graphResult)
  hydra.lib.lists.map[Seq[Int], Seq[T0]]((comp: Seq[Int]) =>
    hydra.lib.lists.map[Int, T0](hydra.lib.pairs.second[Map[Int, Seq[Int]], (Int) => T0](graphResult))(comp))(hydra.sorting.stronglyConnectedComponents(g))
}

def topologicalSortNodes[T0, T1](getKey: (T0 => T1))(getAdj: (T0 => Seq[T1]))(nodes: Seq[T0]): Seq[Seq[T0]] =
  {
  val nodesByKey: Map[T1, T0] = hydra.lib.maps.fromList[T1, T0](hydra.lib.lists.map[T0, Tuple2[T1, T0]]((n: T0) => Tuple2(getKey(n), n))(nodes))
  val pairs: Seq[Tuple2[T1, Seq[T1]]] = hydra.lib.lists.map[T0, Tuple2[T1, Seq[T1]]]((n: T0) => Tuple2(getKey(n), getAdj(n)))(nodes)
  val comps: Seq[Seq[T1]] = hydra.sorting.topologicalSortComponents(pairs)
  hydra.lib.lists.map[Seq[T1], Seq[T0]]((c: Seq[T1]) =>
    hydra.lib.maybes.cat[T0](hydra.lib.lists.map[T1, Option[T0]]((k: T1) => hydra.lib.maps.lookup[T1, T0](k)(nodesByKey))(c)))(comps)
}
