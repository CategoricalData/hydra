// Note: this is an automatically generated file. Do not edit.

package showaccessors

import (
  "hydra.dev/hydra/accessors"
  "hydra.dev/hydra/core"
  liblists "hydra.dev/hydra/lib/lists"
  libmaps "hydra.dev/hydra/lib/maps"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libpairs "hydra.dev/hydra/lib/pairs"
  libsets "hydra.dev/hydra/lib/sets"
  libstrings "hydra.dev/hydra/lib/strings"
  "hydra.dev/hydra/names"
  "hydra.dev/hydra/rewriting"
)

func TermAccessor (accessor accessors.TermAccessor) any {
  return func () any {
    idx := func (i any) any {
      return nil
    }
    return func () any {
      idxSuff := func (suffix string) any {
        return func (i int32) any {
          return libmaybes.Map(func (s string) any {
            return libstrings.Cat2(s).(func(any) any)(suffix)
          }).(func(any) any)(idx(i))
        }
      }
      return func (x any) any {
        switch v := x.(type) {
          case accessors.TermAccessorAnnotatedBody:
          return func (_ struct{}) any {
            return nil
          }(v)
          case accessors.TermAccessorApplicationFunction:
          return func (_ struct{}) any {
            return func () any {
              _v := "fun"
              return &_v
            }()
          }(v)
          case accessors.TermAccessorApplicationArgument:
          return func (_ struct{}) any {
            return func () any {
              _v := "arg"
              return &_v
            }()
          }(v)
          case accessors.TermAccessorLambdaBody:
          return func (_ struct{}) any {
            return func () any {
              _v := "body"
              return &_v
            }()
          }(v)
          case accessors.TermAccessorUnionCasesDefault:
          return func (_ struct{}) any {
            return func () any {
              _v := "default"
              return &_v
            }()
          }(v)
          case accessors.TermAccessorUnionCasesBranch:
          return func (name core.Name) any {
            return func () any {
              _v := libstrings.Cat2(".").(func(any) any)(func (v any) any {
                return v
              }(name))
              return &_v
            }()
          }(v.Value)
          case accessors.TermAccessorLetBody:
          return func (_ struct{}) any {
            return func () any {
              _v := "in"
              return &_v
            }()
          }(v)
          case accessors.TermAccessorLetBinding:
          return func (name core.Name) any {
            return func () any {
              _v := libstrings.Cat2(func (v any) any {
                return v
              }(name)).(func(any) any)("=")
              return &_v
            }()
          }(v.Value)
          case accessors.TermAccessorListElement:
          return func (i int32) any {
            return idx(i)
          }(v.Value)
          case accessors.TermAccessorMapKey:
          return func (i int32) any {
            return idxSuff(".key").(func(any) any)(i)
          }(v.Value)
          case accessors.TermAccessorMapValue:
          return func (i int32) any {
            return idxSuff(".value").(func(any) any)(i)
          }(v.Value)
          case accessors.TermAccessorMaybeTerm:
          return func (_ struct{}) any {
            return func () any {
              _v := "just"
              return &_v
            }()
          }(v)
          case accessors.TermAccessorProductTerm:
          return func (i int32) any {
            return idx(i)
          }(v.Value)
          case accessors.TermAccessorRecordField:
          return func (name core.Name) any {
            return func () any {
              _v := libstrings.Cat2(".").(func(any) any)(func (v any) any {
                return v
              }(name))
              return &_v
            }()
          }(v.Value)
          case accessors.TermAccessorSetElement:
          return func (i int32) any {
            return idx(i)
          }(v.Value)
          case accessors.TermAccessorSumTerm:
          return func (_ struct{}) any {
            return nil
          }(v)
          case accessors.TermAccessorTypeLambdaBody:
          return func (_ struct{}) any {
            return nil
          }(v)
          case accessors.TermAccessorTypeApplicationTerm:
          return func (_ struct{}) any {
            return nil
          }(v)
          case accessors.TermAccessorInjectionTerm:
          return func (_ struct{}) any {
            return nil
          }(v)
          case accessors.TermAccessorWrappedTerm:
          return func (_ struct{}) any {
            return nil
          }(v)
        }
        return nil
      }(accessor)
    }()
  }()
}

func TermToAccessorGraph (namespaces []any, term core.Term) accessors.AccessorGraph {
  return func () any {
    var dontCareAccessor any = accessors.TermAccessorAnnotatedBody{}
    var helper func([]any) any
    helper = func (ids []any) any {
      return func (mroot any) any {
        return func (path []any) any {
          return func (state any) any {
            return func (accessorTerm any) any {
              return func () any {
                var accessor any = libpairs.First(accessorTerm)
                var currentTerm any = libpairs.Second(accessorTerm)
                var nodesEdges any = libpairs.First(state)
                var visited any = libpairs.Second(state)
                var nodes any = libpairs.First(nodesEdges)
                var edges any = libpairs.Second(nodesEdges)
                var nextPath any = liblists.Cons(accessor).(func(any) any)(path)
                return func (x any) any {
                  switch v := x.(type) {
                    case core.TermLet:
                    return func (letExpr core.Let) any {
                      return func () any {
                        var bindings any = func (v any) any {
                          return v.(core.Let).Bindings
                        }(letExpr)
                        var env any = func (v any) any {
                          return v.(core.Let).Body
                        }(letExpr)
                        var bindingNames any = liblists.Map(func (v any) any {
                          return v.(core.Binding).Name
                        }).(func(any) any)(bindings)
                        addBindingName := func (nodesVisitedIds any) any {
                          return func (name core.Name) any {
                            return func () any {
                              var currentNodesVisited any = libpairs.First(nodesVisitedIds)
                              var currentIds any = libpairs.Second(nodesVisitedIds)
                              var currentNodes any = libpairs.First(currentNodesVisited)
                              var currentVisited any = libpairs.Second(currentNodesVisited)
                              var rawLabel any = names.CompactName(namespaces, name)
                              var uniqueLabel any = names.UniqueLabel(currentVisited.([]any), rawLabel.(string))
                              var node any = accessors.AccessorNode{Name: name, Label: rawLabel.(string), Id: uniqueLabel.(string)}
                              var newVisited any = libsets.Insert(uniqueLabel).(func(any) any)(currentVisited)
                              var newNodes any = liblists.Cons(node).(func(any) any)(currentNodes)
                              var newIds any = libmaps.Insert(name).(func(any) any)(node).(func(any) any)(currentIds)
                              return [2]any{[2]any{newNodes, newVisited}, newIds}
                            }()
                          }
                        }
                        var nodesVisitedIds1 any = liblists.Foldl(addBindingName).(func(any) any)([2]any{[2]any{[]any{}, visited}, ids}).(func(any) any)(bindingNames)
                        var nodes1 any = libpairs.First(libpairs.First(nodesVisitedIds1))
                        var visited1 any = libpairs.Second(libpairs.First(nodesVisitedIds1))
                        var ids1 any = libpairs.Second(nodesVisitedIds1)
                        addBindingTerm := func (currentState any) any {
                          return func (nodeBinding any) any {
                            return func () any {
                              var root any = libpairs.First(nodeBinding)
                              var binding any = libpairs.Second(nodeBinding)
                              var term1 any = binding.(core.Binding).Term
                              return helper(ids1.([]any)).(func(any) any)(func () any {
                                _v := root
                                return &_v
                              }()).(func(any) any)([]any{}).(func(any) any)(currentState).(func(any) any)([2]any{dontCareAccessor, term1})
                            }()
                          }
                        }
                        var nodeBindingPairs any = liblists.Zip(nodes1).(func(any) any)(bindings)
                        var stateAfterBindings any = liblists.Foldl(addBindingTerm).(func(any) any)([2]any{[2]any{liblists.Concat2(nodes1).(func(any) any)(nodes), edges}, visited1}).(func(any) any)(nodeBindingPairs)
                        return helper(ids1.([]any)).(func(any) any)(mroot).(func(any) any)(nextPath).(func(any) any)(stateAfterBindings).(func(any) any)([2]any{accessors.TermAccessorLetBody{}, env})
                      }()
                    }(v.Value)
                    case core.TermVariable:
                    return func (name core.Name) any {
                      return libmaybes.Maybe(state).(func(any) any)(func (root accessors.AccessorNode) any {
                        return libmaybes.Maybe(state).(func(any) any)(func (node accessors.AccessorNode) any {
                          return func () any {
                            var edge any = accessors.AccessorEdge{Source: root, Path: accessors.AccessorPath(liblists.Reverse(nextPath).([]any)), Target: node}
                            var newEdges any = liblists.Cons(edge).(func(any) any)(edges)
                            return [2]any{[2]any{nodes, newEdges}, visited}
                          }()
                        }).(func(any) any)(libmaps.Lookup(name).(func(any) any)(ids))
                      }).(func(any) any)(mroot)
                    }(v.Value)
                    default:
                    return liblists.Foldl(func (v1 any) any {
                      return func (v2 any) any {
                        return helper(ids).(func(any) any)(mroot).(func(any) any)(nextPath).(func(any) any)(v1).(func(any) any)(v2)
                      }
                    }).(func(any) any)(state).(func(any) any)(rewriting.SubtermsWithAccessors(currentTerm.(core.Term)))
                  }
                  return nil
                }(currentTerm)
              }()
            }
          }
        }
      }
    }
    var initialState any = [2]any{[2]any{[]any{}, []any{}}, libsets.Empty}
    var result any = helper(libmaps.Empty).(func(any) any)(nil).(func(any) any)([]any{}).(func(any) any)(initialState).(func(any) any)([2]any{dontCareAccessor, term})
    var finalNodesEdges any = libpairs.First(result)
    var finalNodes any = libpairs.First(finalNodesEdges)
    var finalEdges any = libpairs.Second(finalNodesEdges)
    return accessors.AccessorGraph{Nodes: finalNodes.([]any), Edges: finalEdges.([]any)}
  }().(accessors.AccessorGraph)
}
