package hydra.tarjan

import hydra.topology.*

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.math

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.sets

def adjacencyListsToGraph[T0](edges0: Seq[Tuple2[T0, Seq[T0]]]): Tuple2[Map[Int, Seq[Int]], (Int => T0)] =
  {
  lazy val sortedEdges: Seq[Tuple2[T0, Seq[T0]]] = hydra.lib.lists.sortOn[Tuple2[T0, Seq[T0]], T0](hydra.lib.pairs.first[T0, Seq[T0]])(edges0)
  lazy val indexedEdges: Seq[Tuple2[Int, Tuple2[T0, Seq[T0]]]] = hydra.lib.lists.zip[Int, Tuple2[T0, Seq[T0]]](hydra.lib.math.range(0)(hydra.lib.lists.length[Tuple2[T0,
     Seq[T0]]](sortedEdges)))(sortedEdges)
  lazy val keyToVertex: Map[T0, Int] = hydra.lib.maps.fromList[T0, Int](hydra.lib.lists.map[Tuple2[Int,
     Tuple2[T0, Seq[T0]]], Tuple2[T0, Int]]((vkNeighbors: Tuple2[Int, Tuple2[T0, Seq[T0]]]) =>
    {
    lazy val v: Int = hydra.lib.pairs.first[Int, Tuple2[T0, Seq[T0]]](vkNeighbors)
    {
      lazy val kNeighbors: Tuple2[T0, Seq[T0]] = hydra.lib.pairs.second[Int, Tuple2[T0, Seq[T0]]](vkNeighbors)
      {
        lazy val k: T0 = hydra.lib.pairs.first[T0, Seq[T0]](kNeighbors)
        Tuple2(k, v)
      }
    }
  })(indexedEdges))
  lazy val vertexMap: Map[Int, T0] = hydra.lib.maps.fromList[Int, T0](hydra.lib.lists.map[Tuple2[Int,
     Tuple2[T0, Seq[T0]]], Tuple2[Int, T0]]((vkNeighbors: Tuple2[Int, Tuple2[T0, Seq[T0]]]) =>
    {
    lazy val v: Int = hydra.lib.pairs.first[Int, Tuple2[T0, Seq[T0]]](vkNeighbors)
    {
      lazy val kNeighbors: Tuple2[T0, Seq[T0]] = hydra.lib.pairs.second[Int, Tuple2[T0, Seq[T0]]](vkNeighbors)
      {
        lazy val k: T0 = hydra.lib.pairs.first[T0, Seq[T0]](kNeighbors)
        Tuple2(v, k)
      }
    }
  })(indexedEdges))
  lazy val graph: Map[Int, Seq[Int]] = hydra.lib.maps.fromList[Int, Seq[Int]](hydra.lib.lists.map[Tuple2[Int,
     Tuple2[T0, Seq[T0]]], Tuple2[Int, Seq[Int]]]((vkNeighbors: Tuple2[Int, Tuple2[T0, Seq[T0]]]) =>
    {
    lazy val v: Int = hydra.lib.pairs.first[Int, Tuple2[T0, Seq[T0]]](vkNeighbors)
    {
      lazy val kNeighbors: Tuple2[T0, Seq[T0]] = hydra.lib.pairs.second[Int, Tuple2[T0, Seq[T0]]](vkNeighbors)
      {
        lazy val neighbors: Seq[T0] = hydra.lib.pairs.second[T0, Seq[T0]](kNeighbors)
        Tuple2(v, hydra.lib.maybes.mapMaybe[T0, Int]((k: T0) => hydra.lib.maps.lookup[T0, Int](k)(keyToVertex))(neighbors))
      }
    }
  })(indexedEdges))
  def vertexToKey(v: Int): T0 = hydra.lib.maybes.fromJust[T0](hydra.lib.maps.lookup[Int, T0](v)(vertexMap))
  Tuple2(graph, vertexToKey)
}

def stronglyConnectedComponents(graph: Map[hydra.topology.Vertex, Seq[hydra.topology.Vertex]]): Seq[Seq[hydra.topology.Vertex]] =
  {
  lazy val verts: Seq[hydra.topology.Vertex] = hydra.lib.maps.keys[hydra.topology.Vertex, Seq[hydra.topology.Vertex]](graph)
  lazy val finalState: hydra.topology.TarjanState = hydra.lib.lists.foldl[hydra.topology.TarjanState, hydra.topology.Vertex]((st: hydra.topology.TarjanState) =>
    (v: hydra.topology.Vertex) =>
    hydra.lib.logic.ifElse[hydra.topology.TarjanState](hydra.lib.maps.member[hydra.topology.Vertex, Int](v)(st.indices))(st)(hydra.tarjan.strongConnect(graph)(v)(st)))(hydra.tarjan.initialState)(verts)
  hydra.lib.lists.reverse[Seq[hydra.topology.Vertex]](hydra.lib.lists.map[Seq[hydra.topology.Vertex],
     Seq[hydra.topology.Vertex]](hydra.lib.lists.sort[hydra.topology.Vertex])(finalState.sccs))
}

lazy val initialState: hydra.topology.TarjanState = hydra.topology.TarjanState(0, hydra.lib.maps.empty[hydra.topology.Vertex,
   Int], hydra.lib.maps.empty[hydra.topology.Vertex, Int], Seq(), hydra.lib.sets.empty[hydra.topology.Vertex],
   Seq())

def popStackUntil(v: hydra.topology.Vertex)(st0: hydra.topology.TarjanState): Tuple2[Seq[hydra.topology.Vertex], hydra.topology.TarjanState] =
  {
  def go(acc: Seq[hydra.topology.Vertex])(st: hydra.topology.TarjanState): Tuple2[Seq[hydra.topology.Vertex], hydra.topology.TarjanState] =
    {
    lazy val x: hydra.topology.Vertex = hydra.lib.lists.head[hydra.topology.Vertex](st.stack)
    lazy val xs: Seq[hydra.topology.Vertex] = hydra.lib.lists.tail[hydra.topology.Vertex](st.stack)
    lazy val newSt: hydra.topology.TarjanState = hydra.topology.TarjanState(st.counter, (st.indices), (st.lowLinks), xs, (st.onStack), (st.sccs))
    lazy val newSt2: hydra.topology.TarjanState = hydra.topology.TarjanState(newSt.counter, (newSt.indices),
       (newSt.lowLinks), (newSt.stack), hydra.lib.sets.delete[hydra.topology.Vertex](x)(st.onStack), (newSt.sccs))
    lazy val `acc_`: Seq[hydra.topology.Vertex] = hydra.lib.lists.cons[hydra.topology.Vertex](x)(acc)
    hydra.lib.logic.ifElse[Tuple2[Seq[hydra.topology.Vertex], hydra.topology.TarjanState]](hydra.lib.equality.equal[hydra.topology.Vertex](x)(v))(Tuple2(hydra.lib.lists.reverse[hydra.topology.Vertex](`acc_`),
       newSt2))(go(`acc_`)(newSt2))
  }
  go(Seq())(st0)
}

def strongConnect(graph: Map[hydra.topology.Vertex, Seq[hydra.topology.Vertex]])(v: hydra.topology.Vertex)(st: hydra.topology.TarjanState): hydra.topology.TarjanState =
  {
  lazy val i: Int = (st.counter)
  lazy val newSt: hydra.topology.TarjanState = hydra.topology.TarjanState(hydra.lib.math.add(i)(1), hydra.lib.maps.insert[hydra.topology.Vertex,
     Int](v)(i)(st.indices), hydra.lib.maps.insert[hydra.topology.Vertex, Int](v)(i)(st.lowLinks), hydra.lib.lists.cons[hydra.topology.Vertex](v)(st.stack),
     hydra.lib.sets.insert[hydra.topology.Vertex](v)(st.onStack), (st.sccs))
  lazy val neighbors: Seq[hydra.topology.Vertex] = hydra.lib.maps.findWithDefault[Seq[hydra.topology.Vertex], hydra.topology.Vertex](Seq())(v)(graph)
  def processNeighbor(`st_`: hydra.topology.TarjanState)(w: hydra.topology.Vertex): hydra.topology.TarjanState =
    {
    def lowLink(s: hydra.topology.TarjanState): hydra.topology.TarjanState =
      {
      lazy val lowV1: Int = hydra.lib.maps.findWithDefault[Int, hydra.topology.Vertex](hydra.constants.maxInt32)(v)(s.lowLinks)
      lazy val idx_w: Int = hydra.lib.maps.findWithDefault[Int, hydra.topology.Vertex](hydra.constants.maxInt32)(w)(s.indices)
      hydra.topology.TarjanState(s.counter, (s.indices), hydra.lib.maps.insert[hydra.topology.Vertex,
         Int](v)(hydra.lib.equality.min[Int](lowV1)(idx_w))(s.lowLinks), (s.stack), (s.onStack), (s.sccs))
    }
    hydra.lib.logic.ifElse[hydra.topology.TarjanState](hydra.lib.logic.not(hydra.lib.maps.member[hydra.topology.Vertex, Int](w)(`st_`.indices)))({
      lazy val stAfter: hydra.topology.TarjanState = hydra.tarjan.strongConnect(graph)(w)(`st_`)
      {
        lazy val lowV2: Int = hydra.lib.maps.findWithDefault[Int, hydra.topology.Vertex](hydra.constants.maxInt32)(v)(stAfter.lowLinks)
        {
          lazy val low_w: Int = hydra.lib.maps.findWithDefault[Int, hydra.topology.Vertex](hydra.constants.maxInt32)(w)(stAfter.lowLinks)
          hydra.topology.TarjanState(stAfter.counter, (stAfter.indices), hydra.lib.maps.insert[hydra.topology.Vertex,
             Int](v)(hydra.lib.equality.min[Int](lowV2)(low_w))(stAfter.lowLinks), (stAfter.stack), (stAfter.onStack),
             (stAfter.sccs))
        }
      }
    })(hydra.lib.logic.ifElse[hydra.topology.TarjanState](hydra.lib.sets.member[hydra.topology.Vertex](w)(`st_`.onStack))(lowLink(`st_`))(`st_`))
  }
  lazy val stAfterNeighbors: hydra.topology.TarjanState = hydra.lib.lists.foldl[hydra.topology.TarjanState,
     hydra.topology.Vertex](processNeighbor)(newSt)(neighbors)
  lazy val low_v: Int = hydra.lib.maps.findWithDefault[Int, hydra.topology.Vertex](hydra.constants.maxInt32)(v)(stAfterNeighbors.lowLinks)
  lazy val idx_v: Int = hydra.lib.maps.findWithDefault[Int, hydra.topology.Vertex](hydra.constants.maxInt32)(v)(stAfterNeighbors.indices)
  hydra.lib.logic.ifElse[hydra.topology.TarjanState](hydra.lib.equality.equal[Int](low_v)(idx_v))({
    lazy val compResult: Tuple2[Seq[hydra.topology.Vertex], hydra.topology.TarjanState] = hydra.tarjan.popStackUntil(v)(stAfterNeighbors)
    {
      lazy val comp: Seq[hydra.topology.Vertex] = hydra.lib.pairs.first[Seq[hydra.topology.Vertex], hydra.topology.TarjanState](compResult)
      {
        lazy val stPopped: hydra.topology.TarjanState = hydra.lib.pairs.second[Seq[hydra.topology.Vertex], hydra.topology.TarjanState](compResult)
        hydra.topology.TarjanState(stPopped.counter, (stPopped.indices), (stPopped.lowLinks), (stPopped.stack),
           (stPopped.onStack), hydra.lib.lists.cons[Seq[hydra.topology.Vertex]](comp)(stPopped.sccs))
      }
    }
  })(stAfterNeighbors)
}
