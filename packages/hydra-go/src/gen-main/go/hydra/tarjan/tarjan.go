// Note: this is an automatically generated file. Do not edit.

package tarjan

import (
  "hydra.dev/hydra/constants"
  libequality "hydra.dev/hydra/lib/equality"
  liblists "hydra.dev/hydra/lib/lists"
  liblogic "hydra.dev/hydra/lib/logic"
  libmaps "hydra.dev/hydra/lib/maps"
  libmath "hydra.dev/hydra/lib/math"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libpairs "hydra.dev/hydra/lib/pairs"
  libsets "hydra.dev/hydra/lib/sets"
  "hydra.dev/hydra/topology"
)

func AdjacencyListsToGraph (edges0 []any) any {
  return func () any {
    var sortedEdges any = liblists.SortOn(libpairs.First).(func(any) any)(edges0)
    return func () any {
      var indexedEdges any = liblists.Zip(libmath.Range(0).(func(any) any)(liblists.Length(sortedEdges))).(func(any) any)(sortedEdges)
      return func () any {
        var keyToVertex any = libmaps.FromList(liblists.Map(func (vkNeighbors any) any {
          return func () any {
            var v any = libpairs.First(vkNeighbors)
            return func () any {
              var kNeighbors any = libpairs.Second(vkNeighbors)
              return func () any {
                var k any = libpairs.First(kNeighbors)
                return [2]any{k, v}
              }()
            }()
          }()
        }).(func(any) any)(indexedEdges))
        return func () any {
          var vertexMap any = libmaps.FromList(liblists.Map(func (vkNeighbors any) any {
            return func () any {
              var v any = libpairs.First(vkNeighbors)
              return func () any {
                var kNeighbors any = libpairs.Second(vkNeighbors)
                return func () any {
                  var k any = libpairs.First(kNeighbors)
                  return [2]any{v, k}
                }()
              }()
            }()
          }).(func(any) any)(indexedEdges))
          return func () any {
            var graph any = libmaps.FromList(liblists.Map(func (vkNeighbors any) any {
              return func () any {
                var v any = libpairs.First(vkNeighbors)
                return func () any {
                  var kNeighbors any = libpairs.Second(vkNeighbors)
                  return func () any {
                    var neighbors any = libpairs.Second(kNeighbors)
                    return [2]any{v, libmaybes.MapMaybe(func (k any) any {
                      return libmaps.Lookup(k).(func(any) any)(keyToVertex)
                    }).(func(any) any)(neighbors)}
                  }()
                }()
              }()
            }).(func(any) any)(indexedEdges))
            return func () any {
              vertexToKey := func (v int32) any {
                return libmaybes.FromJust(libmaps.Lookup(v).(func(any) any)(vertexMap))
              }
              return [2]any{graph, vertexToKey}
            }()
          }()
        }()
      }()
    }()
  }()
}

func StronglyConnectedComponents (graph []any) []any {
  return func () any {
    var verts any = libmaps.Keys(graph)
    return func () any {
      var finalState any = liblists.Foldl(func (st topology.TarjanState) any {
        return func (v int32) any {
          return liblogic.IfElse(libmaps.Member(v).(func(any) any)(func (v any) any {
            return v.(topology.TarjanState).Indices
          }(st))).(func(any) any)(st).(func(any) any)(StrongConnect(graph, v, st))
        }
      }).(func(any) any)(InitialState).(func(any) any)(verts)
      return liblists.Reverse(liblists.Map(liblists.Sort).(func(any) any)(finalState.(topology.TarjanState).Sccs))
    }()
  }().([]any)
}

var InitialState = topology.TarjanState{Counter: 0, Indices: libmaps.Empty, LowLinks: libmaps.Empty, Stack: []any{}, OnStack: libsets.Empty, Sccs: []any{}}

func PopStackUntil (v int32, st0 topology.TarjanState) any {
  return func () any {
    var go_ func([]any) any
    go_ = func (acc []any) any {
      return func (st topology.TarjanState) any {
        return func () any {
          var x any = liblists.Head(func (v any) any {
            return v.(topology.TarjanState).Stack
          }(st))
          return func () any {
            var xs any = liblists.Tail(func (v any) any {
              return v.(topology.TarjanState).Stack
            }(st))
            return func () any {
              var newSt any = topology.TarjanState{Counter: func (v any) any {
                return v.(topology.TarjanState).Counter
              }(st).(int32), Indices: func (v any) any {
                return v.(topology.TarjanState).Indices
              }(st).([]any), LowLinks: func (v any) any {
                return v.(topology.TarjanState).LowLinks
              }(st).([]any), Stack: xs.([]any), OnStack: func (v any) any {
                return v.(topology.TarjanState).OnStack
              }(st).([]any), Sccs: func (v any) any {
                return v.(topology.TarjanState).Sccs
              }(st).([]any)}
              return func () any {
                var newSt2 any = topology.TarjanState{Counter: newSt.(topology.TarjanState).Counter, Indices: newSt.(topology.TarjanState).Indices, LowLinks: newSt.(topology.TarjanState).LowLinks, Stack: newSt.(topology.TarjanState).Stack, OnStack: libsets.Delete(x).(func(any) any)(func (v any) any {
                  return v.(topology.TarjanState).OnStack
                }(st)).([]any), Sccs: newSt.(topology.TarjanState).Sccs}
                return func () any {
                  var acc_ any = liblists.Cons(x).(func(any) any)(acc)
                  return liblogic.IfElse(libequality.Equal(x).(func(any) any)(v)).(func(any) any)([2]any{liblists.Reverse(acc_), newSt2}).(func(any) any)(go_(acc_.([]any)).(func(any) any)(newSt2))
                }()
              }()
            }()
          }()
        }()
      }
    }
    return go_([]any{}).(func(any) any)(st0)
  }()
}

func StrongConnect (graph []any, v int32, st topology.TarjanState) topology.TarjanState {
  return func () any {
    var i any = func (v any) any {
      return v.(topology.TarjanState).Counter
    }(st)
    return func () any {
      var newSt any = topology.TarjanState{Counter: libmath.Add(i).(func(any) any)(1).(int32), Indices: libmaps.Insert(v).(func(any) any)(i).(func(any) any)(func (v any) any {
        return v.(topology.TarjanState).Indices
      }(st)).([]any), LowLinks: libmaps.Insert(v).(func(any) any)(i).(func(any) any)(func (v any) any {
        return v.(topology.TarjanState).LowLinks
      }(st)).([]any), Stack: liblists.Cons(v).(func(any) any)(func (v any) any {
        return v.(topology.TarjanState).Stack
      }(st)).([]any), OnStack: libsets.Insert(v).(func(any) any)(func (v any) any {
        return v.(topology.TarjanState).OnStack
      }(st)).([]any), Sccs: func (v any) any {
        return v.(topology.TarjanState).Sccs
      }(st).([]any)}
      return func () any {
        var neighbors any = libmaps.FindWithDefault([]any{}).(func(any) any)(v).(func(any) any)(graph)
        return func () any {
          processNeighbor := func (st_ topology.TarjanState) any {
            return func (w int32) any {
              return func () any {
                lowLink := func (s topology.TarjanState) any {
                  return func () any {
                    var lowV1 any = libmaps.FindWithDefault(constants.MaxInt32).(func(any) any)(v).(func(any) any)(func (v any) any {
                      return v.(topology.TarjanState).LowLinks
                    }(s))
                    return func () any {
                      var idx_w any = libmaps.FindWithDefault(constants.MaxInt32).(func(any) any)(w).(func(any) any)(func (v any) any {
                        return v.(topology.TarjanState).Indices
                      }(s))
                      return topology.TarjanState{Counter: func (v any) any {
                        return v.(topology.TarjanState).Counter
                      }(s).(int32), Indices: func (v any) any {
                        return v.(topology.TarjanState).Indices
                      }(s).([]any), LowLinks: libmaps.Insert(v).(func(any) any)(libequality.Min(lowV1).(func(any) any)(idx_w)).(func(any) any)(func (v any) any {
                        return v.(topology.TarjanState).LowLinks
                      }(s)).([]any), Stack: func (v any) any {
                        return v.(topology.TarjanState).Stack
                      }(s).([]any), OnStack: func (v any) any {
                        return v.(topology.TarjanState).OnStack
                      }(s).([]any), Sccs: func (v any) any {
                        return v.(topology.TarjanState).Sccs
                      }(s).([]any)}
                    }()
                  }()
                }
                return liblogic.IfElse(liblogic.Not(libmaps.Member(w).(func(any) any)(func (v any) any {
                  return v.(topology.TarjanState).Indices
                }(st_)))).(func(any) any)(func () any {
                  var stAfter any = StrongConnect(graph, w, st_)
                  return func () any {
                    var lowV2 any = libmaps.FindWithDefault(constants.MaxInt32).(func(any) any)(v).(func(any) any)(stAfter.(topology.TarjanState).LowLinks)
                    return func () any {
                      var low_w any = libmaps.FindWithDefault(constants.MaxInt32).(func(any) any)(w).(func(any) any)(stAfter.(topology.TarjanState).LowLinks)
                      return topology.TarjanState{Counter: stAfter.(topology.TarjanState).Counter, Indices: stAfter.(topology.TarjanState).Indices, LowLinks: libmaps.Insert(v).(func(any) any)(libequality.Min(lowV2).(func(any) any)(low_w)).(func(any) any)(stAfter.(topology.TarjanState).LowLinks).([]any), Stack: stAfter.(topology.TarjanState).Stack, OnStack: stAfter.(topology.TarjanState).OnStack, Sccs: stAfter.(topology.TarjanState).Sccs}
                    }()
                  }()
                }()).(func(any) any)(liblogic.IfElse(libsets.Member(w).(func(any) any)(func (v any) any {
                  return v.(topology.TarjanState).OnStack
                }(st_))).(func(any) any)(lowLink(st_)).(func(any) any)(st_))
              }()
            }
          }
          return func () any {
            var stAfterNeighbors any = liblists.Foldl(processNeighbor).(func(any) any)(newSt).(func(any) any)(neighbors)
            return func () any {
              var low_v any = libmaps.FindWithDefault(constants.MaxInt32).(func(any) any)(v).(func(any) any)(stAfterNeighbors.(topology.TarjanState).LowLinks)
              return func () any {
                var idx_v any = libmaps.FindWithDefault(constants.MaxInt32).(func(any) any)(v).(func(any) any)(stAfterNeighbors.(topology.TarjanState).Indices)
                return liblogic.IfElse(libequality.Equal(low_v).(func(any) any)(idx_v)).(func(any) any)(func () any {
                  var compResult any = PopStackUntil(v, stAfterNeighbors.(topology.TarjanState))
                  return func () any {
                    var comp any = libpairs.First(compResult)
                    return func () any {
                      var stPopped any = libpairs.Second(compResult)
                      return topology.TarjanState{Counter: stPopped.(topology.TarjanState).Counter, Indices: stPopped.(topology.TarjanState).Indices, LowLinks: stPopped.(topology.TarjanState).LowLinks, Stack: stPopped.(topology.TarjanState).Stack, OnStack: stPopped.(topology.TarjanState).OnStack, Sccs: liblists.Cons(comp).(func(any) any)(stPopped.(topology.TarjanState).Sccs).([]any)}
                    }()
                  }()
                }()).(func(any) any)(stAfterNeighbors)
              }()
            }()
          }()
        }()
      }()
    }()
  }().(topology.TarjanState)
}
