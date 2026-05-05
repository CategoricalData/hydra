// Note: this is an automatically generated file. Do not edit.

package unification

import (
  "hydra.dev/hydra/coders"
  "hydra.dev/hydra/context"
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/error"
  libeithers "hydra.dev/hydra/lib/eithers"
  libequality "hydra.dev/hydra/lib/equality"
  liblists "hydra.dev/hydra/lib/lists"
  liblogic "hydra.dev/hydra/lib/logic"
  libmaps "hydra.dev/hydra/lib/maps"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libstrings "hydra.dev/hydra/lib/strings"
  "hydra.dev/hydra/rewriting"
  showcore "hydra.dev/hydra/show/core"
  "hydra.dev/hydra/substitution"
  "hydra.dev/hydra/typing"
)

func JoinTypes (cx context.Context, left core.Type, right core.Type, comment string) any {
  return func () any {
    var sleft any = rewriting.DeannotateType(left)
    return func () any {
      var sright any = rewriting.DeannotateType(right)
      return func () any {
        joinOne := func (l core.Type) any {
          return func (r core.Type) any {
            return typing.TypeConstraint{Left: l, Right: r, Comment: libstrings.Cat2("join types; ").(func(any) any)(comment).(string)}
          }
        }
        return func () any {
          var cannotUnify any = [2]any{"left", context.InContext[error.UnificationError]{Object: error.UnificationError{LeftType: sleft.(core.Type), RightType: sright.(core.Type), Message: libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("cannot unify ").(func(any) any)(showcore.Type_(sleft.(core.Type)))).(func(any) any)(" with ")).(func(any) any)(showcore.Type_(sright.(core.Type))).(string)}, Context: cx}}
          return func () any {
            var assertEqual any = liblogic.IfElse(libequality.Equal(sleft).(func(any) any)(sright)).(func(any) any)([2]any{"right", []any{}}).(func(any) any)(cannotUnify)
            return func () any {
              joinList := func (lefts []any) any {
                return func (rights []any) any {
                  return liblogic.IfElse(libequality.Equal(liblists.Length(lefts)).(func(any) any)(liblists.Length(rights))).(func(any) any)([2]any{"right", liblists.ZipWith(joinOne).(func(any) any)(lefts).(func(any) any)(rights)}).(func(any) any)(cannotUnify)
                }
              }
              return func () any {
                joinRowTypes := func (left2 []any) any {
                  return func (right2 []any) any {
                    return liblogic.IfElse(liblogic.And(libequality.Equal(liblists.Length(liblists.Map(func (v any) any {
                      return v.(core.FieldType).Name
                    }).(func(any) any)(left2))).(func(any) any)(liblists.Length(liblists.Map(func (v any) any {
                      return v.(core.FieldType).Name
                    }).(func(any) any)(right2)))).(func(any) any)(liblists.Foldl(liblogic.And).(func(any) any)(true).(func(any) any)(liblists.ZipWith(func (left3 core.Name) any {
                      return func (right3 core.Name) any {
                        return libequality.Equal(func (v any) any {
                          return v
                        }(left3)).(func(any) any)(func (v any) any {
                          return v
                        }(right3))
                      }
                    }).(func(any) any)(liblists.Map(func (v any) any {
                      return v.(core.FieldType).Name
                    }).(func(any) any)(left2)).(func(any) any)(liblists.Map(func (v any) any {
                      return v.(core.FieldType).Name
                    }).(func(any) any)(right2))))).(func(any) any)(joinList(liblists.Map(func (v any) any {
                      return v.(core.FieldType).Type_
                    }).(func(any) any)(left2).([]any)).(func(any) any)(liblists.Map(func (v any) any {
                      return v.(core.FieldType).Type_
                    }).(func(any) any)(right2))).(func(any) any)(cannotUnify)
                  }
                }
                return func (x any) any {
                  switch v := x.(type) {
                    case core.TypeApplication:
                    return func (l core.ApplicationType) any {
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.TypeApplication:
                          return func (r core.ApplicationType) any {
                            return [2]any{"right", []any{joinOne(func (v any) any {
                              return v.(core.ApplicationType).Function
                            }(l).(core.Type)).(func(any) any)(func (v any) any {
                              return v.(core.ApplicationType).Function
                            }(r)), joinOne(func (v any) any {
                              return v.(core.ApplicationType).Argument
                            }(l).(core.Type)).(func(any) any)(func (v any) any {
                              return v.(core.ApplicationType).Argument
                            }(r))}}
                          }(v.Value)
                          default:
                          return cannotUnify
                        }
                        return nil
                      }(sright)
                    }(v.Value)
                    case core.TypeEither:
                    return func (l core.EitherType) any {
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.TypeEither:
                          return func (r core.EitherType) any {
                            return [2]any{"right", []any{joinOne(func (v any) any {
                              return v.(core.EitherType).Left
                            }(l).(core.Type)).(func(any) any)(func (v any) any {
                              return v.(core.EitherType).Left
                            }(r)), joinOne(func (v any) any {
                              return v.(core.EitherType).Right
                            }(l).(core.Type)).(func(any) any)(func (v any) any {
                              return v.(core.EitherType).Right
                            }(r))}}
                          }(v.Value)
                          default:
                          return cannotUnify
                        }
                        return nil
                      }(sright)
                    }(v.Value)
                    case core.TypeFunction:
                    return func (l core.FunctionType) any {
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.TypeFunction:
                          return func (r core.FunctionType) any {
                            return [2]any{"right", []any{joinOne(func (v any) any {
                              return v.(core.FunctionType).Domain
                            }(l).(core.Type)).(func(any) any)(func (v any) any {
                              return v.(core.FunctionType).Domain
                            }(r)), joinOne(func (v any) any {
                              return v.(core.FunctionType).Codomain
                            }(l).(core.Type)).(func(any) any)(func (v any) any {
                              return v.(core.FunctionType).Codomain
                            }(r))}}
                          }(v.Value)
                          default:
                          return cannotUnify
                        }
                        return nil
                      }(sright)
                    }(v.Value)
                    case core.TypeList:
                    return func (l core.Type) any {
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.TypeList:
                          return func (r core.Type) any {
                            return [2]any{"right", []any{joinOne(l).(func(any) any)(r)}}
                          }(v.Value)
                          default:
                          return cannotUnify
                        }
                        return nil
                      }(sright)
                    }(v.Value)
                    case core.TypeLiteral:
                    return func (_ core.LiteralType) any {
                      return assertEqual
                    }(v.Value)
                    case core.TypeMap_:
                    return func (l core.MapType) any {
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.TypeMap_:
                          return func (r core.MapType) any {
                            return [2]any{"right", []any{joinOne(func (v any) any {
                              return v.(core.MapType).Keys
                            }(l).(core.Type)).(func(any) any)(func (v any) any {
                              return v.(core.MapType).Keys
                            }(r)), joinOne(func (v any) any {
                              return v.(core.MapType).Values
                            }(l).(core.Type)).(func(any) any)(func (v any) any {
                              return v.(core.MapType).Values
                            }(r))}}
                          }(v.Value)
                          default:
                          return cannotUnify
                        }
                        return nil
                      }(sright)
                    }(v.Value)
                    case core.TypeMaybe:
                    return func (l core.Type) any {
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.TypeMaybe:
                          return func (r core.Type) any {
                            return [2]any{"right", []any{joinOne(l).(func(any) any)(r)}}
                          }(v.Value)
                          default:
                          return cannotUnify
                        }
                        return nil
                      }(sright)
                    }(v.Value)
                    case core.TypePair:
                    return func (l core.PairType) any {
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.TypePair:
                          return func (r core.PairType) any {
                            return [2]any{"right", []any{joinOne(func (v any) any {
                              return v.(core.PairType).First
                            }(l).(core.Type)).(func(any) any)(func (v any) any {
                              return v.(core.PairType).First
                            }(r)), joinOne(func (v any) any {
                              return v.(core.PairType).Second
                            }(l).(core.Type)).(func(any) any)(func (v any) any {
                              return v.(core.PairType).Second
                            }(r))}}
                          }(v.Value)
                          default:
                          return cannotUnify
                        }
                        return nil
                      }(sright)
                    }(v.Value)
                    case core.TypeRecord:
                    return func (l []any) any {
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.TypeRecord:
                          return func (r []any) any {
                            return joinRowTypes(l).(func(any) any)(r)
                          }(v.Value)
                          default:
                          return cannotUnify
                        }
                        return nil
                      }(sright)
                    }(v.Value)
                    case core.TypeSet:
                    return func (l core.Type) any {
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.TypeSet:
                          return func (r core.Type) any {
                            return [2]any{"right", []any{joinOne(l).(func(any) any)(r)}}
                          }(v.Value)
                          default:
                          return cannotUnify
                        }
                        return nil
                      }(sright)
                    }(v.Value)
                    case core.TypeUnion:
                    return func (l []any) any {
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.TypeUnion:
                          return func (r []any) any {
                            return joinRowTypes(l).(func(any) any)(r)
                          }(v.Value)
                          default:
                          return cannotUnify
                        }
                        return nil
                      }(sright)
                    }(v.Value)
                    case core.TypeUnit:
                    return func (_ struct{}) any {
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.TypeUnit:
                          return func (_2 struct{}) any {
                            return [2]any{"right", []any{}}
                          }(v)
                          default:
                          return cannotUnify
                        }
                        return nil
                      }(sright)
                    }(v)
                    case core.TypeWrap:
                    return func (l core.Type) any {
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.TypeWrap:
                          return func (r core.Type) any {
                            return [2]any{"right", []any{joinOne(l).(func(any) any)(r)}}
                          }(v.Value)
                          default:
                          return cannotUnify
                        }
                        return nil
                      }(sright)
                    }(v.Value)
                    default:
                    return cannotUnify
                  }
                  return nil
                }(sleft)
              }()
            }()
          }()
        }()
      }()
    }()
  }()
}

func UnifyTypeConstraints (cx context.Context, schemaTypes []any, constraints []any) any {
  return func () any {
    withConstraint := func (c typing.TypeConstraint) any {
      return func (rest []any) any {
        return func () any {
          var sleft any = rewriting.DeannotateType(func (v any) any {
            return v.(typing.TypeConstraint).Left
          }(c).(core.Type))
          return func () any {
            var sright any = rewriting.DeannotateType(func (v any) any {
              return v.(typing.TypeConstraint).Right
            }(c).(core.Type))
            return func () any {
              var comment any = func (v any) any {
                return v.(typing.TypeConstraint).Comment
              }(c)
              return func () any {
                bind := func (v core.Name) any {
                  return func (t core.Type) any {
                    return func () any {
                      var subst any = substitution.SingletonTypeSubst(v, t)
                      return func () any {
                        withResult := func (s typing.TypeSubst) any {
                          return substitution.ComposeTypeSubst(subst.(typing.TypeSubst), s)
                        }
                        return libeithers.Map(withResult).(func(any) any)(UnifyTypeConstraints(cx, schemaTypes, substitution.SubstituteInConstraints(subst.(typing.TypeSubst), rest)))
                      }()
                    }()
                  }
                }
                return func () any {
                  tryBinding := func (v core.Name) any {
                    return func (t core.Type) any {
                      return liblogic.IfElse(VariableOccursInType(v, t)).(func(any) any)([2]any{"left", context.InContext[error.UnificationError]{Object: error.UnificationError{LeftType: sleft.(core.Type), RightType: sright.(core.Type), Message: libstrings.Cat2(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("Variable ").(func(any) any)(func (v any) any {
                        return v
                      }(v))).(func(any) any)(" appears free in type ")).(func(any) any)(showcore.Type_(t))).(func(any) any)(" (")).(func(any) any)(comment)).(func(any) any)(")").(string)}, Context: cx}}).(func(any) any)(bind(v).(func(any) any)(t))
                    }
                  }
                  return func () any {
                    var noVars any = func () any {
                      withConstraints := func (constraints2 []any) any {
                        return UnifyTypeConstraints(cx, schemaTypes, liblists.Concat2(constraints2).(func(any) any)(rest).([]any))
                      }
                      return libeithers.Bind(JoinTypes(cx, sleft.(core.Type), sright.(core.Type), comment.(string))).(func(any) any)(withConstraints)
                    }()
                    return func () any {
                      var dflt any = func (x any) any {
                        switch v := x.(type) {
                          case core.TypeVariable:
                          return func (name core.Name) any {
                            return tryBinding(name).(func(any) any)(sleft)
                          }(v.Value)
                          default:
                          return noVars
                        }
                        return nil
                      }(sright)
                      return func (x any) any {
                        switch v := x.(type) {
                          case core.TypeVariable:
                          return func (name core.Name) any {
                            return func (x any) any {
                              switch v := x.(type) {
                                case core.TypeVariable:
                                return func (name2 core.Name) any {
                                  return liblogic.IfElse(libequality.Equal(func (v any) any {
                                    return v
                                  }(name)).(func(any) any)(func (v any) any {
                                    return v
                                  }(name2))).(func(any) any)(UnifyTypeConstraints(cx, schemaTypes, rest)).(func(any) any)(liblogic.IfElse(libmaybes.IsJust(libmaps.Lookup(name).(func(any) any)(schemaTypes))).(func(any) any)(liblogic.IfElse(libmaybes.IsJust(libmaps.Lookup(name2).(func(any) any)(schemaTypes))).(func(any) any)([2]any{"left", context.InContext[error.UnificationError]{Object: error.UnificationError{LeftType: sleft.(core.Type), RightType: sright.(core.Type), Message: libstrings.Cat2(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2(libstrings.Cat2("Attempted to unify schema names ").(func(any) any)(func (v any) any {
                                    return v
                                  }(name))).(func(any) any)(" and ")).(func(any) any)(func (v any) any {
                                    return v
                                  }(name2))).(func(any) any)(" (")).(func(any) any)(comment)).(func(any) any)(")").(string)}, Context: cx}}).(func(any) any)(bind(name2).(func(any) any)(sleft))).(func(any) any)(bind(name).(func(any) any)(sright)))
                                }(v.Value)
                                default:
                                return tryBinding(name).(func(any) any)(sright)
                              }
                              return nil
                            }(sright)
                          }(v.Value)
                          default:
                          return dflt
                        }
                        return nil
                      }(sleft)
                    }()
                  }()
                }()
              }()
            }()
          }()
        }()
      }
    }
    return liblogic.IfElse(liblists.Null(constraints)).(func(any) any)([2]any{"right", substitution.IdTypeSubst}).(func(any) any)(withConstraint(liblists.Head(constraints).(typing.TypeConstraint)).(func(any) any)(liblists.Tail(constraints)))
  }()
}

func UnifyTypeLists (cx context.Context, schemaTypes []any, l []any, r []any, comment string) any {
  return func () any {
    toConstraint := func (l2 core.Type) any {
      return func (r2 core.Type) any {
        return typing.TypeConstraint{Left: l2, Right: r2, Comment: comment}
      }
    }
    return UnifyTypeConstraints(cx, schemaTypes, liblists.ZipWith(toConstraint).(func(any) any)(l).(func(any) any)(r).([]any))
  }()
}

func UnifyTypes (cx context.Context, schemaTypes []any, l core.Type, r core.Type, comment string) any {
  return UnifyTypeConstraints(cx, schemaTypes, []any{typing.TypeConstraint{Left: l, Right: r, Comment: comment}})
}

func VariableOccursInType (var_ core.Name, typ0 core.Type) bool {
  return func () any {
    tryType := func (b bool) any {
      return func (typ core.Type) any {
        return func (x any) any {
          switch v := x.(type) {
            case core.TypeVariable:
            return func (v core.Name) any {
              return liblogic.Or(b).(func(any) any)(libequality.Equal(func (v any) any {
                return v
              }(v)).(func(any) any)(func (v any) any {
                return v
              }(var_)))
            }(v.Value)
            default:
            return b
          }
          return nil
        }(typ)
      }
    }
    return rewriting.FoldOverType[bool](coders.TraversalOrderPre{}, func (_p any) func(core.Type) any {
      return tryType(_p.(bool)).(func(core.Type) any)
    }, false, typ0)
  }().(bool)
}
