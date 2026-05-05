// Note: this is an automatically generated file. Do not edit.

package codeGeneration

import (
  "hydra.dev/hydra/adapt"
  "hydra.dev/hydra/annotations"
  "hydra.dev/hydra/coders"
  "hydra.dev/hydra/context"
  "hydra.dev/hydra/core"
  decodecore "hydra.dev/hydra/decode/core"
  decodemodule "hydra.dev/hydra/decode/module"
  encodemodule "hydra.dev/hydra/encode/module"
  "hydra.dev/hydra/error"
  "hydra.dev/hydra/graph"
  "hydra.dev/hydra/inference"
  jsondecode "hydra.dev/hydra/json/decode"
  jsonencode "hydra.dev/hydra/json/encode"
  jsonmodel "hydra.dev/hydra/json/model"
  jsonwriter "hydra.dev/hydra/json/writer"
  "hydra.dev/hydra/lexical"
  libeithers "hydra.dev/hydra/lib/eithers"
  libequality "hydra.dev/hydra/lib/equality"
  liblists "hydra.dev/hydra/lib/lists"
  liblogic "hydra.dev/hydra/lib/logic"
  libmaps "hydra.dev/hydra/lib/maps"
  libmath "hydra.dev/hydra/lib/math"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libpairs "hydra.dev/hydra/lib/pairs"
  libsets "hydra.dev/hydra/lib/sets"
  libstrings "hydra.dev/hydra/lib/strings"
  hmodule "hydra.dev/hydra/module"
  "hydra.dev/hydra/rewriting"
  "hydra.dev/hydra/schemas"
  showcore "hydra.dev/hydra/show/core"
  showerror "hydra.dev/hydra/show/error"
)

func NamespaceToPath (ns hmodule.Namespace) string {
  return libstrings.Intercalate("/").(func(any) any)(libstrings.SplitOn(".").(func(any) any)(func (v any) any {
    return v
  }(ns))).(string)
}

func StripModuleTypeSchemes (m hmodule.Module) hmodule.Module {
  return func () any {
    stripIfTerm := func (b core.Binding) any {
      return liblogic.IfElse(annotations.IsNativeType(b)).(func(any) any)(b).(func(any) any)(core.Binding{Name: func (v any) any {
        return v.(core.Binding).Name
      }(b).(core.Name), Term: func (v any) any {
        return v.(core.Binding).Term
      }(b).(core.Term), Type_: nil})
    }
    return hmodule.Module{Namespace: func (v any) any {
      return v.(hmodule.Module).Namespace
    }(m).(hmodule.Namespace), Elements: liblists.Map(stripIfTerm).(func(any) any)(func (v any) any {
      return v.(hmodule.Module).Elements
    }(m)).([]any), TermDependencies: func (v any) any {
      return v.(hmodule.Module).TermDependencies
    }(m).([]any), TypeDependencies: func (v any) any {
      return v.(hmodule.Module).TypeDependencies
    }(m).([]any), Description: func (v any) any {
      return v.(hmodule.Module).Description
    }(m)}
  }().(hmodule.Module)
}

func TransitiveDeps (getDeps func(hmodule.Module) []any, nsMap []any, startMods []any) []any {
  return func () any {
    var initialDeps any = libsets.FromList(liblists.Concat(liblists.Map(func (m hmodule.Module) any {
      return liblists.Filter(func (dep hmodule.Namespace) any {
        return liblogic.Not(libequality.Equal(dep).(func(any) any)(func (v any) any {
          return v.(hmodule.Module).Namespace
        }(m)))
      }).(func(any) any)(getDeps(m))
    }).(func(any) any)(startMods)))
    return func () any {
      var go_ func([]any) any
      go_ = func (pending []any) any {
        return func (visited []any) any {
          return liblogic.IfElse(libsets.Null(pending)).(func(any) any)(visited).(func(any) any)(func () any {
            var newVisited any = libsets.Union(visited).(func(any) any)(pending)
            return func () any {
              var nextDeps any = libsets.FromList(liblists.Concat(liblists.Map(func (nsv hmodule.Namespace) any {
                return libmaybes.Maybe([]any{}).(func(any) any)(func (depMod hmodule.Module) any {
                  return getDeps(depMod)
                }).(func(any) any)(libmaps.Lookup(nsv).(func(any) any)(nsMap))
              }).(func(any) any)(libsets.ToList(pending))))
              return func () any {
                var newPending any = libsets.Difference(nextDeps).(func(any) any)(newVisited)
                return go_(newPending.([]any)).(func(any) any)(newVisited)
              }()
            }()
          }())
        }
      }
      return go_(initialDeps.([]any)).(func(any) any)(libsets.Empty)
    }()
  }().([]any)
}

func ModuleTermDepsTransitive (nsMap []any, modules []any) []any {
  return func () any {
    var closure any = libsets.Union(TransitiveDeps(func (_p hmodule.Module) []any {
      return func (m hmodule.Module) []any {
        return func (v any) any {
          return v.(hmodule.Module).TermDependencies
        }(m)
      }(_p).([]any)
    }, nsMap, modules)).(func(any) any)(libsets.FromList(liblists.Map(func (m hmodule.Module) any {
      return func (v any) any {
        return v.(hmodule.Module).Namespace
      }(m)
    }).(func(any) any)(modules)))
    return libmaybes.Cat(liblists.Map(func (n hmodule.Namespace) any {
      return libmaps.Lookup(n).(func(any) any)(nsMap)
    }).(func(any) any)(libsets.ToList(closure)))
  }().([]any)
}

func ModuleTypeDepsTransitive (nsMap []any, modules []any) []any {
  return func () any {
    var termMods any = ModuleTermDepsTransitive(nsMap, modules)
    return func () any {
      var typeNamespaces any = libsets.ToList(TransitiveDeps(func (_p hmodule.Module) []any {
        return func (m hmodule.Module) []any {
          return func (v any) any {
            return v.(hmodule.Module).TypeDependencies
          }(m)
        }(_p).([]any)
      }, nsMap, termMods.([]any)))
      return libmaybes.Cat(liblists.Map(func (n hmodule.Namespace) any {
        return libmaps.Lookup(n).(func(any) any)(nsMap)
      }).(func(any) any)(typeNamespaces))
    }()
  }().([]any)
}

func ModulesToGraph (bsGraph graph.Graph, universeModules []any, modules []any) graph.Graph {
  return func () any {
    var universe any = libmaps.FromList(liblists.Map(func (m hmodule.Module) any {
      return [2]any{func (v any) any {
        return v.(hmodule.Module).Namespace
      }(m), m}
    }).(func(any) any)(liblists.Concat2(universeModules).(func(any) any)(modules)))
    return func () any {
      var schemaModules any = ModuleTypeDepsTransitive(universe.([]any), modules)
      return func () any {
        var dataModules any = ModuleTermDepsTransitive(universe.([]any), modules)
        return func () any {
          var schemaElements any = liblists.Filter(func (e core.Binding) any {
            return annotations.IsNativeType(e)
          }).(func(any) any)(liblists.Concat(liblists.Map(func (m hmodule.Module) any {
            return func (v any) any {
              return v.(hmodule.Module).Elements
            }(m)
          }).(func(any) any)(liblists.Concat2(schemaModules).(func(any) any)(modules))))
          return func () any {
            var dataElements any = liblists.Filter(func (e core.Binding) any {
              return liblogic.Not(annotations.IsNativeType(e))
            }).(func(any) any)(liblists.Concat(liblists.Map(func (m hmodule.Module) any {
              return func (v any) any {
                return v.(hmodule.Module).Elements
              }(m)
            }).(func(any) any)(dataModules)))
            return func () any {
              var schemaGraph any = lexical.ElementsToGraph(bsGraph, libmaps.Empty, schemaElements.([]any))
              return func () any {
                var schemaTypes any = libeithers.Either(func (_ context.InContext[error.Error]) any {
                  return libmaps.Empty
                }).(func(any) any)(func (_r []any) any {
                  return _r
                }).(func(any) any)(schemas.SchemaGraphToTypingEnvironment(lexical.EmptyContext, schemaGraph.(graph.Graph)))
                return lexical.ElementsToGraph(bsGraph, schemaTypes.([]any), dataElements.([]any))
              }()
            }()
          }()
        }()
      }()
    }()
  }().(graph.Graph)
}

func GenerateSourceFiles (printDefinitions func(hmodule.Module) func([]any) func(context.Context) func(graph.Graph) any, lang coders.Language, doInfer bool, doExpand bool, doHoistCaseStatements bool, doHoistPolymorphicLetBindings bool, bsGraph graph.Graph, universeModules []any, modsToGenerate []any, cx context.Context) any {
  return func () any {
    var namespaceMap any = libmaps.FromList(liblists.Map(func (m hmodule.Module) any {
      return [2]any{func (v any) any {
        return v.(hmodule.Module).Namespace
      }(m), m}
    }).(func(any) any)(liblists.Concat2(universeModules).(func(any) any)(modsToGenerate)))
    return func () any {
      var constraints any = func (v any) any {
        return v.(coders.Language).Constraints
      }(lang)
      return func () any {
        isTypeModule := func (mod hmodule.Module) any {
          return liblogic.Not(liblists.Null(liblists.Filter(func (e core.Binding) any {
            return annotations.IsNativeType(e)
          }).(func(any) any)(func (v any) any {
            return v.(hmodule.Module).Elements
          }(mod))))
        }
        return func () any {
          var partitioned any = liblists.Partition(isTypeModule).(func(any) any)(modsToGenerate)
          return func () any {
            var typeModulesToGenerate any = libpairs.First(partitioned)
            return func () any {
              var termModulesToGenerate any = libpairs.Second(partitioned)
              return func () any {
                var schemaMods any = ModuleTypeDepsTransitive(namespaceMap.([]any), modsToGenerate)
                return func () any {
                  var schemaElements any = liblists.Filter(func (e core.Binding) any {
                    return annotations.IsNativeType(e)
                  }).(func(any) any)(liblists.Concat(liblists.Map(func (m hmodule.Module) any {
                    return func (v any) any {
                      return v.(hmodule.Module).Elements
                    }(m)
                  }).(func(any) any)(liblists.Concat2(schemaMods).(func(any) any)(typeModulesToGenerate))))
                  return func () any {
                    var dataMods any = ModuleTermDepsTransitive(namespaceMap.([]any), modsToGenerate)
                    return func () any {
                      var dataElements any = liblists.Concat(liblists.Map(func (m hmodule.Module) any {
                        return func (v any) any {
                          return v.(hmodule.Module).Elements
                        }(m)
                      }).(func(any) any)(dataMods))
                      return func () any {
                        var schemaGraph any = lexical.ElementsToGraph(bsGraph, libmaps.Empty, schemaElements.([]any))
                        return func () any {
                          var schemaTypes2 any = libeithers.Either(func (_ context.InContext[error.Error]) any {
                            return libmaps.Empty
                          }).(func(any) any)(func (_r []any) any {
                            return _r
                          }).(func(any) any)(schemas.SchemaGraphToTypingEnvironment(lexical.EmptyContext, schemaGraph.(graph.Graph)))
                          return func () any {
                            var dataGraph any = lexical.ElementsToGraph(bsGraph, schemaTypes2.([]any), dataElements.([]any))
                            return libeithers.Bind(liblogic.IfElse(liblists.Null(typeModulesToGenerate)).(func(any) any)([2]any{"right", []any{}}).(func(any) any)(func () any {
                              var nameLists any = liblists.Map(func (m hmodule.Module) any {
                                return liblists.Map(func (e core.Binding) any {
                                  return func (v any) any {
                                    return v.(core.Binding).Name
                                  }(e)
                                }).(func(any) any)(liblists.Filter(func (e core.Binding) any {
                                  return annotations.IsNativeType(e)
                                }).(func(any) any)(func (v any) any {
                                  return v.(hmodule.Module).Elements
                                }(m)))
                              }).(func(any) any)(typeModulesToGenerate)
                              return libeithers.Bind(libeithers.Bimap(func (s string) any {
                                return context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(s)}, Context: cx}
                              }).(func(any) any)(func (r any) any {
                                return r
                              }).(func(any) any)(adapt.SchemaGraphToDefinitions(constraints.(coders.LanguageConstraints), schemaGraph.(graph.Graph), nameLists.([]any), cx))).(func(any) any)(func (schemaResult any) any {
                                return func () any {
                                  var defLists any = libpairs.Second(schemaResult)
                                  return func () any {
                                    var schemaGraphWithTypes any = graph.Graph{BoundTerms: schemaGraph.(graph.Graph).BoundTerms, BoundTypes: schemaGraph.(graph.Graph).BoundTypes, ClassConstraints: schemaGraph.(graph.Graph).ClassConstraints, LambdaVariables: schemaGraph.(graph.Graph).LambdaVariables, Metadata: schemaGraph.(graph.Graph).Metadata, Primitives: schemaGraph.(graph.Graph).Primitives, SchemaTypes: schemaTypes2.([]any), TypeVariables: schemaGraph.(graph.Graph).TypeVariables}
                                    return libeithers.Map(func (xs []any) any {
                                      return liblists.Concat(xs)
                                    }).(func(any) any)(libeithers.MapList(func (p any) any {
                                      return func () any {
                                        var mod any = libpairs.First(p)
                                        return func () any {
                                          var defs any = libpairs.Second(p)
                                          return libeithers.Map(func (m []any) any {
                                            return libmaps.ToList(m)
                                          }).(func(any) any)(printDefinitions(mod.(hmodule.Module))(liblists.Map(func (d hmodule.TypeDefinition) any {
                                            return hmodule.DefinitionType_{Value: d}
                                          }).(func(any) any)(defs))(cx)(schemaGraphWithTypes))
                                        }()
                                      }()
                                    }).(func(any) any)(liblists.Zip(typeModulesToGenerate).(func(any) any)(defLists)))
                                  }()
                                }()
                              })
                            }())).(func(any) any)(func (schemaFiles []any) any {
                              return libeithers.Bind(liblogic.IfElse(liblists.Null(termModulesToGenerate)).(func(any) any)([2]any{"right", []any{}}).(func(any) any)(func () any {
                                var namespaces any = liblists.Map(func (m hmodule.Module) any {
                                  return func (v any) any {
                                    return v.(hmodule.Module).Namespace
                                  }(m)
                                }).(func(any) any)(termModulesToGenerate)
                                return libeithers.Bind(libeithers.Bimap(func (s string) any {
                                  return context.InContext[error.Error]{Object: error.ErrorOther{Value: error.OtherError(s)}, Context: cx}
                                }).(func(any) any)(func (r any) any {
                                  return r
                                }).(func(any) any)(adapt.DataGraphToDefinitions(constraints.(coders.LanguageConstraints), doInfer, doExpand, doHoistCaseStatements, doHoistPolymorphicLetBindings, dataElements.([]any), dataGraph.(graph.Graph), namespaces.([]any), cx))).(func(any) any)(func (dataResult any) any {
                                  return func () any {
                                    var g1 any = libpairs.First(dataResult)
                                    return func () any {
                                      var defLists any = libpairs.Second(dataResult)
                                      return func () any {
                                        refreshModule := func (els []any) any {
                                          return func (m hmodule.Module) any {
                                            return hmodule.Module{Namespace: func (v any) any {
                                              return v.(hmodule.Module).Namespace
                                            }(m).(hmodule.Namespace), Elements: libmaybes.Cat(liblists.Map(func (e core.Binding) any {
                                              return liblists.Find(func (b core.Binding) any {
                                                return libequality.Equal(func (v any) any {
                                                  return v.(core.Binding).Name
                                                }(b)).(func(any) any)(func (v any) any {
                                                  return v.(core.Binding).Name
                                                }(e))
                                              }).(func(any) any)(els)
                                            }).(func(any) any)(func (v any) any {
                                              return v.(hmodule.Module).Elements
                                            }(m))).([]any), TermDependencies: func (v any) any {
                                              return v.(hmodule.Module).TermDependencies
                                            }(m).([]any), TypeDependencies: func (v any) any {
                                              return v.(hmodule.Module).TypeDependencies
                                            }(m).([]any), Description: func (v any) any {
                                              return v.(hmodule.Module).Description
                                            }(m)}
                                          }
                                        }
                                        return func () any {
                                          var refreshedMods any = liblists.Map(func (m hmodule.Module) any {
                                            return refreshModule(lexical.GraphToBindings(g1.(graph.Graph))).(func(any) any)(m)
                                          }).(func(any) any)(termModulesToGenerate)
                                          return libeithers.Map(func (xs []any) any {
                                            return liblists.Concat(xs)
                                          }).(func(any) any)(libeithers.MapList(func (p any) any {
                                            return func () any {
                                              var mod any = libpairs.First(p)
                                              return func () any {
                                                var defs any = libpairs.Second(p)
                                                return libeithers.Map(func (m []any) any {
                                                  return libmaps.ToList(m)
                                                }).(func(any) any)(printDefinitions(mod.(hmodule.Module))(liblists.Map(func (d hmodule.TermDefinition) any {
                                                  return hmodule.DefinitionTerm{Value: d}
                                                }).(func(any) any)(defs))(cx)(g1))
                                              }()
                                            }()
                                          }).(func(any) any)(liblists.Zip(refreshedMods).(func(any) any)(defLists)))
                                        }()
                                      }()
                                    }()
                                  }()
                                })
                              }())).(func(any) any)(func (termFiles []any) any {
                                return [2]any{"right", liblists.Concat2(schemaFiles).(func(any) any)(termFiles)}
                              })
                            })
                          }()
                        }()
                      }()
                    }()
                  }()
                }()
              }()
            }()
          }()
        }()
      }()
    }()
  }()
}

func FormatTermBinding (binding core.Binding) string {
  return func () any {
    var name any = func (v any) any {
      return v.(core.Binding).Name
    }(binding)
    return func () any {
      var typeStr any = libmaybes.Maybe("?").(func(any) any)(func (scheme core.TypeScheme) any {
        return showcore.TypeScheme(scheme)
      }).(func(any) any)(func (v any) any {
        return v.(core.Binding).Type_
      }(binding))
      return libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("  ").(func(any) any)(name)).(func(any) any)(" : ")).(func(any) any)(typeStr)
    }()
  }().(string)
}

func FormatPrimitive (prim graph.Primitive) string {
  return func () any {
    var name any = func (v any) any {
      return v.(graph.Primitive).Name
    }(prim)
    return func () any {
      var typeStr any = showcore.TypeScheme(func (v any) any {
        return v.(graph.Primitive).Type_
      }(prim).(core.TypeScheme))
      return libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("  ").(func(any) any)(name)).(func(any) any)(" : ")).(func(any) any)(typeStr)
    }()
  }().(string)
}

func FormatTypeBinding (graph graph.Graph, binding core.Binding) any {
  return libeithers.Bind(decodecore.Type_(graph, func (v any) any {
    return v.(core.Binding).Term
  }(binding).(core.Term))).(func(any) any)(func (typ core.Type) any {
    return [2]any{"right", libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("  ").(func(any) any)(func (v any) any {
      return v.(core.Binding).Name
    }(binding))).(func(any) any)(" = ")).(func(any) any)(showcore.Type_(typ))}
  })
}

func BuildSchemaMap (g graph.Graph) []any {
  return libmaps.Map(func (ts core.TypeScheme) any {
    return rewriting.DeannotateType(func (v any) any {
      return v.(core.TypeScheme).Type_
    }(ts).(core.Type))
  }).(func(any) any)(func (v any) any {
    return v.(graph.Graph).SchemaTypes
  }(g)).([]any)
}

func ModuleToSourceModule (m hmodule.Module) hmodule.Module {
  return func () any {
    var sourceNs any = hmodule.Namespace(libstrings.Cat2("hydra.sources.").(func(any) any)(libstrings.Intercalate(".").(func(any) any)(liblists.Drop(1).(func(any) any)(libstrings.SplitOn(".").(func(any) any)(func (v any) any {
      return v.(hmodule.Module).Namespace
    }(m))))).(string))
    return func () any {
      var modTypeNs any = hmodule.Namespace("hydra.module")
      return func () any {
        var moduleBinding any = core.Binding{Name: core.Name(libstrings.Cat2(sourceNs).(func(any) any)(".module_").(string)), Term: encodemodule.Module(m), Type_: nil}
        return hmodule.Module{Namespace: sourceNs.(hmodule.Namespace), Elements: []any{moduleBinding}, TermDependencies: []any{modTypeNs}, TypeDependencies: []any{modTypeNs}, Description: func () any {
          _v := libstrings.Cat2("Source module for ").(func(any) any)(func (v any) any {
            return v.(hmodule.Module).Namespace
          }(m))
          return &_v
        }()}
      }()
    }()
  }().(hmodule.Module)
}

func GenerateLexicon (graph graph.Graph) any {
  return func () any {
    var bindings any = lexical.GraphToBindings(graph)
    return func () any {
      var primitives any = libmaps.Elems(func (v any) any {
        return v.(graph.Graph).Primitives
      }(graph))
      return func () any {
        var partitioned any = liblists.Partition(func (b core.Binding) any {
          return annotations.IsNativeType(b)
        }).(func(any) any)(bindings)
        return func () any {
          var typeBindings any = libpairs.First(partitioned)
          return func () any {
            var termBindings any = libpairs.Second(partitioned)
            return func () any {
              var sortedPrimitives any = liblists.SortOn(func (p graph.Primitive) any {
                return func (v any) any {
                  return v.(graph.Primitive).Name
                }(p)
              }).(func(any) any)(primitives)
              return func () any {
                var sortedTypes any = liblists.SortOn(func (b core.Binding) any {
                  return func (v any) any {
                    return v.(core.Binding).Name
                  }(b)
                }).(func(any) any)(typeBindings)
                return func () any {
                  var sortedTerms any = liblists.SortOn(func (b core.Binding) any {
                    return func (v any) any {
                      return v.(core.Binding).Name
                    }(b)
                  }).(func(any) any)(termBindings)
                  return libeithers.Bind(libeithers.MapList(func (b core.Binding) any {
                    return FormatTypeBinding(graph, b)
                  }).(func(any) any)(sortedTypes)).(func(any) any)(func (typeLines []any) any {
                    return func () any {
                      var termLines any = liblists.Map(func (b core.Binding) any {
                        return FormatTermBinding(b)
                      }).(func(any) any)(sortedTerms)
                      return func () any {
                        var primitiveLines any = liblists.Map(func (p graph.Primitive) any {
                          return FormatPrimitive(p)
                        }).(func(any) any)(sortedPrimitives)
                        return [2]any{"right", libstrings.Cat2(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("Primitives:\n").(func(any) any)(libstrings.Unlines(primitiveLines))).(func(any) any)("\nTypes:\n")).(func(any) any)(libstrings.Unlines(typeLines))).(func(any) any)("\nTerms:\n")).(func(any) any)(libstrings.Unlines(termLines))}
                      }()
                    }()
                  })
                }()
              }()
            }()
          }()
        }()
      }()
    }()
  }()
}

func ModuleToJson (m hmodule.Module) any {
  return func () any {
    var term any = encodemodule.Module(m)
    return libeithers.Map(func (json jsonmodel.Value) any {
      return jsonwriter.PrintJson(json)
    }).(func(any) any)(jsonencode.ToJson(term.(core.Term)))
  }()
}

func InferModules (cx context.Context, bsGraph graph.Graph, universeMods []any, targetMods []any) any {
  return func () any {
    var g0 any = ModulesToGraph(bsGraph, universeMods, universeMods)
    return func () any {
      var dataElements any = liblists.Filter(func (e core.Binding) any {
        return liblogic.Not(annotations.IsNativeType(e))
      }).(func(any) any)(liblists.Concat(liblists.Map(func (m hmodule.Module) any {
        return func (v any) any {
          return v.(hmodule.Module).Elements
        }(m)
      }).(func(any) any)(universeMods)))
      return libeithers.Bind(inference.InferGraphTypes(cx, dataElements.([]any), g0.(graph.Graph))).(func(any) any)(func (inferResultWithCx any) any {
        return func () any {
          var inferResult any = libpairs.First(inferResultWithCx)
          return func () any {
            var g1 any = libpairs.First(inferResult)
            return func () any {
              var inferredElements any = libpairs.Second(inferResult)
              return func () any {
                isTypeModule := func (mod hmodule.Module) any {
                  return liblists.Null(liblists.Filter(func (e core.Binding) any {
                    return liblogic.Not(annotations.IsNativeType(e))
                  }).(func(any) any)(func (v any) any {
                    return v.(hmodule.Module).Elements
                  }(mod)))
                }
                return func () any {
                  refreshModule := func (m hmodule.Module) any {
                    return liblogic.IfElse(isTypeModule(m)).(func(any) any)(m).(func(any) any)(hmodule.Module{Namespace: func (v any) any {
                      return v.(hmodule.Module).Namespace
                    }(m).(hmodule.Namespace), Elements: libmaybes.Cat(liblists.Map(func (e core.Binding) any {
                      return liblists.Find(func (b core.Binding) any {
                        return libequality.Equal(func (v any) any {
                          return v.(core.Binding).Name
                        }(b)).(func(any) any)(func (v any) any {
                          return v.(core.Binding).Name
                        }(e))
                      }).(func(any) any)(inferredElements)
                    }).(func(any) any)(func (v any) any {
                      return v.(hmodule.Module).Elements
                    }(m))).([]any), TermDependencies: func (v any) any {
                      return v.(hmodule.Module).TermDependencies
                    }(m).([]any), TypeDependencies: func (v any) any {
                      return v.(hmodule.Module).TypeDependencies
                    }(m).([]any), Description: func (v any) any {
                      return v.(hmodule.Module).Description
                    }(m)})
                  }
                  return [2]any{"right", liblists.Map(refreshModule).(func(any) any)(targetMods)}
                }()
              }()
            }()
          }()
        }()
      })
    }()
  }()
}

func GenerateCoderModules[T0, T1 any] (codec func(T0) func(graph.Graph) func(T1) any, bsGraph graph.Graph, universeModules []any, typeModules []any, cx T0) any {
  return func () any {
    var universe any = libmaps.FromList(liblists.Map(func (m hmodule.Module) any {
      return [2]any{func (v any) any {
        return v.(hmodule.Module).Namespace
      }(m), m}
    }).(func(any) any)(liblists.Concat2(universeModules).(func(any) any)(universeModules)))
    return func () any {
      var schemaModules any = ModuleTypeDepsTransitive(universe.([]any), universeModules)
      return func () any {
        var dataModules any = ModuleTermDepsTransitive(universe.([]any), universeModules)
        return func () any {
          var schemaElements any = liblists.Filter(func (e core.Binding) any {
            return annotations.IsNativeType(e)
          }).(func(any) any)(liblists.Concat(liblists.Map(func (m hmodule.Module) any {
            return func (v any) any {
              return v.(hmodule.Module).Elements
            }(m)
          }).(func(any) any)(liblists.Concat2(schemaModules).(func(any) any)(universeModules))))
          return func () any {
            var dataElements any = liblists.Filter(func (e core.Binding) any {
              return liblogic.Not(annotations.IsNativeType(e))
            }).(func(any) any)(liblists.Concat(liblists.Map(func (m hmodule.Module) any {
              return func (v any) any {
                return v.(hmodule.Module).Elements
              }(m)
            }).(func(any) any)(dataModules)))
            return func () any {
              var schemaGraph any = lexical.ElementsToGraph(bsGraph, libmaps.Empty, schemaElements.([]any))
              return func () any {
                var schemaTypes any = libeithers.Either(func (_ context.InContext[error.Error]) any {
                  return libmaps.Empty
                }).(func(any) any)(func (_r []any) any {
                  return _r
                }).(func(any) any)(schemas.SchemaGraphToTypingEnvironment(lexical.EmptyContext, schemaGraph.(graph.Graph)))
                return func () any {
                  var allElements any = liblists.Concat2(schemaElements).(func(any) any)(dataElements)
                  return func () any {
                    var graph any = lexical.ElementsToGraph(bsGraph, schemaTypes.([]any), allElements.([]any))
                    return libeithers.Map(func (results []any) any {
                      return libmaybes.Cat(results)
                    }).(func(any) any)(libeithers.MapList(func (m T1) any {
                      return codec(cx)(graph)(m)
                    }).(func(any) any)(typeModules))
                  }()
                }()
              }()
            }()
          }()
        }()
      }()
    }()
  }()
}

func InferAndGenerateLexicon (cx context.Context, bsGraph graph.Graph, kernelModules []any) any {
  return func () any {
    var g0 any = ModulesToGraph(bsGraph, kernelModules, kernelModules)
    return func () any {
      var dataElements any = liblists.Filter(func (e core.Binding) any {
        return liblogic.Not(annotations.IsNativeType(e))
      }).(func(any) any)(liblists.Concat(liblists.Map(func (m hmodule.Module) any {
        return func (v any) any {
          return v.(hmodule.Module).Elements
        }(m)
      }).(func(any) any)(kernelModules)))
      return libeithers.Bind(libeithers.Bimap(func (ic context.InContext[error.Error]) any {
        return showerror.Error_(func (v any) any {
          return v.(context.InContext[error.Error]).Object
        }(ic).(error.Error))
      }).(func(any) any)(func (x any) any {
        return x
      }).(func(any) any)(inference.InferGraphTypes(cx, dataElements.([]any), g0.(graph.Graph)))).(func(any) any)(func (inferResultWithCx any) any {
        return func () any {
          var g1 any = libpairs.First(libpairs.First(inferResultWithCx))
          return libeithers.Bimap(func (v any) any {
            return v
          }).(func(any) any)(func (x string) any {
            return x
          }).(func(any) any)(GenerateLexicon(g1.(graph.Graph)))
        }()
      })
    }()
  }()
}

func EscapeControlCharsInJson (input []any) []any {
  return func () any {
    hexDigit := func (n int32) any {
      return liblogic.IfElse(libequality.Lt(n).(func(any) any)(10)).(func(any) any)(libmath.Add(48).(func(any) any)(n)).(func(any) any)(libmath.Add(97).(func(any) any)(libmath.Sub(n).(func(any) any)(10)))
    }
    return func () any {
      escapeToUnicode := func (b int32) any {
        return []any{92, 117, 48, 48, hexDigit(libmath.Div(b).(func(any) any)(16).(int32)), hexDigit(libmath.Mod(b).(func(any) any)(16).(int32))}
      }
      return func () any {
        var go_ func(bool) any
        go_ = func (inStr bool) any {
          return func (esc bool) any {
            return func (bytes []any) any {
              return liblogic.IfElse(liblists.Null(bytes)).(func(any) any)([]any{}).(func(any) any)(func () any {
                var b any = liblists.Head(bytes)
                return func () any {
                  var bs any = liblists.Tail(bytes)
                  return liblogic.IfElse(esc).(func(any) any)(liblists.Cons(b).(func(any) any)(go_(inStr).(func(any) any)(false).(func(any) any)(bs))).(func(any) any)(liblogic.IfElse(liblogic.And(libequality.Equal(b).(func(any) any)(92)).(func(any) any)(inStr)).(func(any) any)(liblists.Cons(b).(func(any) any)(go_(inStr).(func(any) any)(true).(func(any) any)(bs))).(func(any) any)(liblogic.IfElse(libequality.Equal(b).(func(any) any)(34)).(func(any) any)(liblists.Cons(b).(func(any) any)(go_(liblogic.Not(inStr).(bool)).(func(any) any)(false).(func(any) any)(bs))).(func(any) any)(liblogic.IfElse(liblogic.And(inStr).(func(any) any)(libequality.Lt(b).(func(any) any)(32))).(func(any) any)(liblists.Concat2(escapeToUnicode(b.(int32))).(func(any) any)(go_(inStr).(func(any) any)(false).(func(any) any)(bs))).(func(any) any)(liblists.Cons(b).(func(any) any)(go_(inStr).(func(any) any)(false).(func(any) any)(bs))))))
                }()
              }())
            }
          }
        }
        return go_(false).(func(any) any)(false).(func(any) any)(input)
      }()
    }()
  }().([]any)
}

func DecodeModuleFromJson (bsGraph graph.Graph, universeModules []any, doStripTypeSchemes bool, jsonVal jsonmodel.Value) any {
  return func () any {
    var graph any = ModulesToGraph(bsGraph, universeModules, universeModules)
    return func () any {
      var schemaMap any = BuildSchemaMap(graph.(graph.Graph))
      return func () any {
        var modType any = core.TypeVariable{Value: core.Name("hydra.module.Module")}
        return libeithers.Either(func (err string) any {
          return [2]any{"left", err}
        }).(func(any) any)(func (term core.Term) any {
          return libeithers.Either(func (decErr error.DecodingError) any {
            return [2]any{"left", func (v any) any {
              return v
            }(decErr)}
          }).(func(any) any)(func (mod hmodule.Module) any {
            return [2]any{"right", liblogic.IfElse(doStripTypeSchemes).(func(any) any)(StripModuleTypeSchemes(mod)).(func(any) any)(mod)}
          }).(func(any) any)(decodemodule.Module(graph.(graph.Graph), term))
        }).(func(any) any)(jsondecode.FromJson(schemaMap.([]any), core.Name("hydra.module.Module"), modType.(core.Type), jsonVal))
      }()
    }()
  }()
}
