// Note: this is an automatically generated file. Do not edit.

package sorting

import (
  libequality "hydra.dev/hydra/lib/equality"
  liblists "hydra.dev/hydra/lib/lists"
  liblogic "hydra.dev/hydra/lib/logic"
  libmaps "hydra.dev/hydra/lib/maps"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libpairs "hydra.dev/hydra/lib/pairs"
  libsets "hydra.dev/hydra/lib/sets"
  "hydra.dev/hydra/tarjan"
  "hydra.dev/hydra/topology"
)

func AdjacencyListToMap (pairs []any) []any {
  return liblists.Foldl(func (mp []any) any {
    return func (p any) any {
      return func () any {
        var k any = libpairs.First(p)
        return func () any {
          var vs any = libpairs.Second(p)
          return func () any {
            var existing any = libmaybes.Maybe([]any{}).(func(any) any)(libequality.Identity).(func(any) any)(libmaps.Lookup(k).(func(any) any)(mp))
            return libmaps.Insert(k).(func(any) any)(liblists.Concat2(existing).(func(any) any)(vs)).(func(any) any)(mp)
          }()
        }()
      }()
    }
  }).(func(any) any)(libmaps.Empty).(func(any) any)(pairs).([]any)
}

func CreateOrderingIsomorphism[T1 any] (sourceOrd []any, targetOrd []any) topology.OrderingIsomorphism[T1] {
  return func () any {
    sourceToTargetMapping := func (els []any) any {
      return func () any {
        var mp any = libmaps.FromList(liblists.Zip(sourceOrd).(func(any) any)(els))
        return libmaybes.Cat(liblists.Map(func (n any) any {
          return libmaps.Lookup(n).(func(any) any)(mp)
        }).(func(any) any)(targetOrd))
      }()
    }
    return func () any {
      targetToSourceMapping := func (els []any) any {
        return func () any {
          var mp any = libmaps.FromList(liblists.Zip(targetOrd).(func(any) any)(els))
          return libmaybes.Cat(liblists.Map(func (n any) any {
            return libmaps.Lookup(n).(func(any) any)(mp)
          }).(func(any) any)(sourceOrd))
        }()
      }
      return topology.OrderingIsomorphism[T1]{Encode: func (_p []any) []any {
        return sourceToTargetMapping(_p).([]any)
      }, Decode: func (_p []any) []any {
        return targetToSourceMapping(_p).([]any)
      }}
    }()
  }().(topology.OrderingIsomorphism[T1])
}

func FindReachableNodes[T0 any] (adj func(T0) []any, root T0) []any {
  return func () any {
    var visit func([]any) any
    visit = func (visited []any) any {
      return func (node T0) any {
        return func () any {
          var toVisit any = libsets.Difference(adj(node)).(func(any) any)(visited)
          return liblogic.IfElse(libsets.Null(toVisit)).(func(any) any)(visited).(func(any) any)(liblists.Foldl(func (v []any) any {
            return func (n T0) any {
              return visit(libsets.Insert(n).(func(any) any)(v).([]any)).(func(any) any)(n)
            }
          }).(func(any) any)(visited).(func(any) any)(libsets.ToList(toVisit)))
        }()
      }
    }
    return visit(libsets.Singleton(root).([]any)).(func(any) any)(root)
  }().([]any)
}

func PropagateTags (edges []any, nodeTags []any) []any {
  return func () any {
    var adjMap any = AdjacencyListToMap(edges)
    return func () any {
      var tagMap any = libmaps.Map(libsets.FromList).(func(any) any)(AdjacencyListToMap(nodeTags))
      return func () any {
        var allNodes any = libsets.ToList(libsets.FromList(liblists.Concat2(liblists.Map(libpairs.First).(func(any) any)(edges)).(func(any) any)(liblists.Map(libpairs.First).(func(any) any)(nodeTags))))
        return func () any {
          getTagsForNode := func (node any) any {
            return func () any {
              var reachable any = FindReachableNodes[any](func (_p any) []any {
                return func (n any) []any {
                  return libsets.FromList(libmaybes.Maybe([]any{}).(func(any) any)(libequality.Identity).(func(any) any)(libmaps.Lookup(n).(func(any) any)(adjMap)))
                }(_p).([]any)
              }, node)
              return libsets.Unions(liblists.Map(func (n any) any {
                return libmaybes.Maybe(libsets.Empty).(func(any) any)(libequality.Identity).(func(any) any)(libmaps.Lookup(n).(func(any) any)(tagMap))
              }).(func(any) any)(libsets.ToList(reachable)))
            }()
          }
          return liblists.Map(func (n any) any {
            return [2]any{n, getTagsForNode(n)}
          }).(func(any) any)(allNodes)
        }()
      }()
    }()
  }().([]any)
}

func TopologicalSort (pairs []any) any {
  return func () any {
    var sccs any = TopologicalSortComponents(pairs)
    return func () any {
      isCycle := func (scc []any) any {
        return liblogic.Not(liblists.Null(liblists.Tail(scc)))
      }
      return func () any {
        var withCycles any = liblists.Filter(isCycle).(func(any) any)(sccs)
        return liblogic.IfElse(liblists.Null(withCycles)).(func(any) any)([2]any{"right", liblists.Concat(sccs)}).(func(any) any)([2]any{"left", withCycles})
      }()
    }()
  }()
}

func TopologicalSortComponents (pairs []any) []any {
  return func () any {
    var graphResult any = tarjan.AdjacencyListsToGraph(pairs)
    return func () any {
      var g any = libpairs.First(graphResult)
      return liblists.Map(func (comp []any) any {
        return liblists.Map(libpairs.Second(graphResult)).(func(any) any)(comp)
      }).(func(any) any)(tarjan.StronglyConnectedComponents(g.([]any)))
    }()
  }().([]any)
}

func TopologicalSortNodes[T0, T1 any] (getKey func(T0) T1, getAdj func(T0) []any, nodes []any) []any {
  return func () any {
    var nodesByKey any = libmaps.FromList(liblists.Map(func (n T0) any {
      return [2]any{getKey(n), n}
    }).(func(any) any)(nodes))
    return func () any {
      var pairs any = liblists.Map(func (n T0) any {
        return [2]any{getKey(n), getAdj(n)}
      }).(func(any) any)(nodes)
      return func () any {
        var comps any = TopologicalSortComponents(pairs.([]any))
        return liblists.Map(func (c []any) any {
          return libmaybes.Cat(liblists.Map(func (k T1) any {
            return libmaps.Lookup(k).(func(any) any)(nodesByKey)
          }).(func(any) any)(c))
        }).(func(any) any)(comps)
      }()
    }()
  }().([]any)
}
