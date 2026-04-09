// Note: this is an automatically generated file. Do not edit.

package jsondecode

import (
  "hydra.dev/hydra/core"
  jsonmodel "hydra.dev/hydra/json/model"
  libeithers "hydra.dev/hydra/lib/eithers"
  libequality "hydra.dev/hydra/lib/equality"
  liblists "hydra.dev/hydra/lib/lists"
  libliterals "hydra.dev/hydra/lib/literals"
  liblogic "hydra.dev/hydra/lib/logic"
  libmaps "hydra.dev/hydra/lib/maps"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libsets "hydra.dev/hydra/lib/sets"
  libstrings "hydra.dev/hydra/lib/strings"
  "hydra.dev/hydra/rewriting"
  showcore "hydra.dev/hydra/show/core"
  "math/big"
)

func FromJson (types []any, tname core.Name, typ core.Type, value jsonmodel.Value) any {
  return func () any {
    var stripped any = rewriting.DeannotateType(typ)
    return func (x any) any {
      switch v := x.(type) {
        case core.TypeLiteral:
        return func (lt core.LiteralType) any {
          return DecodeLiteral(lt, value)
        }(v.Value)
        case core.TypeList:
        return func (elemType core.Type) any {
          return func () any {
            decodeElem := func (v jsonmodel.Value) any {
              return FromJson(types, tname, elemType, v)
            }
            return func () any {
              var arrResult any = ExpectArray(value)
              return libeithers.Either(func (err string) any {
                return [2]any{"left", err}
              }).(func(any) any)(func (arr []any) any {
                return func () any {
                  var decoded any = libeithers.MapList(decodeElem).(func(any) any)(arr)
                  return libeithers.Map(func (ts []any) any {
                    return core.TermList{Value: ts}
                  }).(func(any) any)(decoded)
                }()
              }).(func(any) any)(arrResult)
            }()
          }()
        }(v.Value)
        case core.TypeSet:
        return func (elemType core.Type) any {
          return func () any {
            decodeElem := func (v jsonmodel.Value) any {
              return FromJson(types, tname, elemType, v)
            }
            return func () any {
              var arrResult any = ExpectArray(value)
              return libeithers.Either(func (err string) any {
                return [2]any{"left", err}
              }).(func(any) any)(func (arr []any) any {
                return func () any {
                  var decoded any = libeithers.MapList(decodeElem).(func(any) any)(arr)
                  return libeithers.Map(func (elems []any) any {
                    return core.TermSet{Value: libsets.FromList(elems).([]any)}
                  }).(func(any) any)(decoded)
                }()
              }).(func(any) any)(arrResult)
            }()
          }()
        }(v.Value)
        case core.TypeMaybe:
        return func (innerType core.Type) any {
          return func () any {
            decodeJust := func (arr []any) any {
              return libeithers.Map(func (v core.Term) any {
                return core.TermMaybe{Value: func () any {
                  _v := v
                  return &_v
                }()}
              }).(func(any) any)(FromJson(types, tname, innerType, liblists.Head(arr).(jsonmodel.Value)))
            }
            return func () any {
              decodeMaybeArray := func (arr []any) any {
                return func () any {
                  var len_ any = liblists.Length(arr)
                  return liblogic.IfElse(libequality.Equal(len_).(func(any) any)(0)).(func(any) any)([2]any{"right", core.TermMaybe{Value: nil}}).(func(any) any)(liblogic.IfElse(libequality.Equal(len_).(func(any) any)(1)).(func(any) any)(decodeJust(arr)).(func(any) any)([2]any{"left", "expected single-element array for Just"}))
                }()
              }
              return func (x any) any {
                switch v := x.(type) {
                  case jsonmodel.ValueNull:
                  return func (_ struct{}) any {
                    return [2]any{"right", core.TermMaybe{Value: nil}}
                  }(v)
                  case jsonmodel.ValueArray:
                  return func (arr []any) any {
                    return decodeMaybeArray(arr)
                  }(v.Value)
                  default:
                  return [2]any{"left", "expected null or single-element array for Maybe"}
                }
                return nil
              }(value)
            }()
          }()
        }(v.Value)
        case core.TypeRecord:
        return func (rt []any) any {
          return func () any {
            var objResult any = ExpectObject(value)
            return libeithers.Either(func (err string) any {
              return [2]any{"left", err}
            }).(func(any) any)(func (obj []any) any {
              return func () any {
                decodeField := func (ft core.FieldType) any {
                  return func () any {
                    var fname any = func (v any) any {
                      return v.(core.FieldType).Name
                    }(ft)
                    return func () any {
                      var ftype any = func (v any) any {
                        return v.(core.FieldType).Type_
                      }(ft)
                      return func () any {
                        var mval any = libmaps.Lookup(fname).(func(any) any)(obj)
                        return func () any {
                          var defaultVal any = jsonmodel.ValueNull{}
                          return func () any {
                            var jsonVal any = libmaybes.FromMaybe(defaultVal).(func(any) any)(mval)
                            return func () any {
                              var decoded any = FromJson(types, tname, ftype.(core.Type), jsonVal.(jsonmodel.Value))
                              return libeithers.Map(func (v core.Term) any {
                                return core.Field{Name: fname.(core.Name), Term: v}
                              }).(func(any) any)(decoded)
                            }()
                          }()
                        }()
                      }()
                    }()
                  }()
                }
                return func () any {
                  var decodedFields any = libeithers.MapList(decodeField).(func(any) any)(rt)
                  return libeithers.Map(func (fs []any) any {
                    return core.TermRecord{Value: core.Record{TypeName: tname, Fields: fs}}
                  }).(func(any) any)(decodedFields)
                }()
              }()
            }).(func(any) any)(objResult)
          }()
        }(v.Value)
        case core.TypeUnion:
        return func (rt []any) any {
          return func () any {
            decodeVariant := func (key string) any {
              return func (val any) any {
                return func (ftype core.Type) any {
                  return func () any {
                    var jsonVal any = libmaybes.FromMaybe(jsonmodel.ValueNull{}).(func(any) any)(val)
                    return func () any {
                      var decoded any = FromJson(types, tname, ftype, jsonVal.(jsonmodel.Value))
                      return libeithers.Map(func (v core.Term) any {
                        return core.TermUnion{Value: core.Injection{TypeName: tname, Field: core.Field{Name: core.Name(key), Term: v}}}
                      }).(func(any) any)(decoded)
                    }()
                  }()
                }
              }
            }
            return func () any {
              tryField := func (key string) any {
                return func (val any) any {
                  return func (ft core.FieldType) any {
                    return liblogic.IfElse(libequality.Equal(func (v any) any {
                      return v.(core.FieldType).Name
                    }(ft)).(func(any) any)(key)).(func(any) any)(func () any {
                      _v := decodeVariant(key).(func(any) any)(val).(func(any) any)(func (v any) any {
                        return v.(core.FieldType).Type_
                      }(ft))
                      return &_v
                    }()).(func(any) any)(nil)
                  }
                }
              }
              return func () any {
                var findAndDecode func(string) any
                findAndDecode = func (key string) any {
                  return func (val any) any {
                    return func (fts []any) any {
                      return liblogic.IfElse(liblists.Null(fts)).(func(any) any)([2]any{"left", libstrings.Cat([]any{"unknown variant: ", key})}).(func(any) any)(libmaybes.Maybe(findAndDecode(key).(func(any) any)(val).(func(any) any)(liblists.Tail(fts))).(func(any) any)(func (r any) any {
                        return r
                      }).(func(any) any)(tryField(key).(func(any) any)(val).(func(any) any)(liblists.Head(fts))))
                    }
                  }
                }
                return func () any {
                  decodeSingleKey := func (obj []any) any {
                    return findAndDecode(liblists.Head(libmaps.Keys(obj)).(string)).(func(any) any)(libmaps.Lookup(liblists.Head(libmaps.Keys(obj))).(func(any) any)(obj)).(func(any) any)(rt)
                  }
                  return func () any {
                    processUnion := func (obj []any) any {
                      return liblogic.IfElse(libequality.Equal(liblists.Length(libmaps.Keys(obj))).(func(any) any)(1)).(func(any) any)(decodeSingleKey(obj)).(func(any) any)([2]any{"left", "expected single-key object for union"})
                    }
                    return func () any {
                      var objResult any = ExpectObject(value)
                      return libeithers.Either(func (err string) any {
                        return [2]any{"left", err}
                      }).(func(any) any)(func (obj []any) any {
                        return processUnion(obj)
                      }).(func(any) any)(objResult)
                    }()
                  }()
                }()
              }()
            }()
          }()
        }(v.Value)
        case core.TypeUnit:
        return func (_ struct{}) any {
          return func () any {
            var objResult any = ExpectObject(value)
            return libeithers.Map(func (_2 []any) any {
              return core.TermUnit{}
            }).(func(any) any)(objResult)
          }()
        }(v)
        case core.TypeWrap:
        return func (wn core.Type) any {
          return func () any {
            var decoded any = FromJson(types, tname, wn, value)
            return libeithers.Map(func (v core.Term) any {
              return core.TermWrap{Value: core.WrappedTerm{TypeName: tname, Body: v}}
            }).(func(any) any)(decoded)
          }()
        }(v.Value)
        case core.TypeMap_:
        return func (mt core.MapType) any {
          return func () any {
            var keyType any = func (v any) any {
              return v.(core.MapType).Keys
            }(mt)
            return func () any {
              var valType any = func (v any) any {
                return v.(core.MapType).Values
              }(mt)
              return func () any {
                var arrResult any = ExpectArray(value)
                return libeithers.Either(func (err string) any {
                  return [2]any{"left", err}
                }).(func(any) any)(func (arr []any) any {
                  return func () any {
                    decodeEntry := func (entryJson jsonmodel.Value) any {
                      return func () any {
                        var objResult any = ExpectObject(entryJson)
                        return libeithers.Either(func (err string) any {
                          return [2]any{"left", err}
                        }).(func(any) any)(func (entryObj []any) any {
                          return func () any {
                            var keyJson any = libmaps.Lookup("@key").(func(any) any)(entryObj)
                            return func () any {
                              var valJson any = libmaps.Lookup("@value").(func(any) any)(entryObj)
                              return libmaybes.Maybe([2]any{"left", "missing @key in map entry"}).(func(any) any)(func (kj jsonmodel.Value) any {
                                return libmaybes.Maybe([2]any{"left", "missing @value in map entry"}).(func(any) any)(func (vj jsonmodel.Value) any {
                                  return func () any {
                                    var decodedKey any = FromJson(types, tname, keyType.(core.Type), kj)
                                    return func () any {
                                      var decodedVal any = FromJson(types, tname, valType.(core.Type), vj)
                                      return libeithers.Either(func (err string) any {
                                        return [2]any{"left", err}
                                      }).(func(any) any)(func (k core.Term) any {
                                        return libeithers.Map(func (v core.Term) any {
                                          return [2]any{k, v}
                                        }).(func(any) any)(decodedVal)
                                      }).(func(any) any)(decodedKey)
                                    }()
                                  }()
                                }).(func(any) any)(valJson)
                              }).(func(any) any)(keyJson)
                            }()
                          }()
                        }).(func(any) any)(objResult)
                      }()
                    }
                    return func () any {
                      var entries any = libeithers.MapList(decodeEntry).(func(any) any)(arr)
                      return libeithers.Map(func (es []any) any {
                        return core.TermMap_{Value: libmaps.FromList(es).([]any)}
                      }).(func(any) any)(entries)
                    }()
                  }()
                }).(func(any) any)(arrResult)
              }()
            }()
          }()
        }(v.Value)
        case core.TypePair:
        return func (pt core.PairType) any {
          return func () any {
            var firstType any = func (v any) any {
              return v.(core.PairType).First
            }(pt)
            return func () any {
              var secondType any = func (v any) any {
                return v.(core.PairType).Second
              }(pt)
              return func () any {
                var objResult any = ExpectObject(value)
                return libeithers.Either(func (err string) any {
                  return [2]any{"left", err}
                }).(func(any) any)(func (obj []any) any {
                  return func () any {
                    var firstJson any = libmaps.Lookup("@first").(func(any) any)(obj)
                    return func () any {
                      var secondJson any = libmaps.Lookup("@second").(func(any) any)(obj)
                      return libmaybes.Maybe([2]any{"left", "missing @first in pair"}).(func(any) any)(func (fj jsonmodel.Value) any {
                        return libmaybes.Maybe([2]any{"left", "missing @second in pair"}).(func(any) any)(func (sj jsonmodel.Value) any {
                          return func () any {
                            var decodedFirst any = FromJson(types, tname, firstType.(core.Type), fj)
                            return func () any {
                              var decodedSecond any = FromJson(types, tname, secondType.(core.Type), sj)
                              return libeithers.Either(func (err string) any {
                                return [2]any{"left", err}
                              }).(func(any) any)(func (f core.Term) any {
                                return libeithers.Map(func (s core.Term) any {
                                  return core.TermPair{Value: [2]any{f, s}}
                                }).(func(any) any)(decodedSecond)
                              }).(func(any) any)(decodedFirst)
                            }()
                          }()
                        }).(func(any) any)(secondJson)
                      }).(func(any) any)(firstJson)
                    }()
                  }()
                }).(func(any) any)(objResult)
              }()
            }()
          }()
        }(v.Value)
        case core.TypeEither:
        return func (et core.EitherType) any {
          return func () any {
            var leftType any = func (v any) any {
              return v.(core.EitherType).Left
            }(et)
            return func () any {
              var rightType any = func (v any) any {
                return v.(core.EitherType).Right
              }(et)
              return func () any {
                var objResult any = ExpectObject(value)
                return libeithers.Either(func (err string) any {
                  return [2]any{"left", err}
                }).(func(any) any)(func (obj []any) any {
                  return func () any {
                    var leftJson any = libmaps.Lookup("@left").(func(any) any)(obj)
                    return func () any {
                      var rightJson any = libmaps.Lookup("@right").(func(any) any)(obj)
                      return libmaybes.Maybe(libmaybes.Maybe([2]any{"left", "expected @left or @right in Either"}).(func(any) any)(func (rj jsonmodel.Value) any {
                        return func () any {
                          var decoded any = FromJson(types, tname, rightType.(core.Type), rj)
                          return libeithers.Map(func (v core.Term) any {
                            return core.TermEither{Value: [2]any{"right", v}}
                          }).(func(any) any)(decoded)
                        }()
                      }).(func(any) any)(rightJson)).(func(any) any)(func (lj jsonmodel.Value) any {
                        return func () any {
                          var decoded any = FromJson(types, tname, leftType.(core.Type), lj)
                          return libeithers.Map(func (v core.Term) any {
                            return core.TermEither{Value: [2]any{"left", v}}
                          }).(func(any) any)(decoded)
                        }()
                      }).(func(any) any)(leftJson)
                    }()
                  }()
                }).(func(any) any)(objResult)
              }()
            }()
          }()
        }(v.Value)
        case core.TypeVariable:
        return func (name core.Name) any {
          return func () any {
            var lookedUp any = libmaps.Lookup(name).(func(any) any)(types)
            return libmaybes.Maybe([2]any{"left", libstrings.Cat([]any{"unknown type variable: ", func (v any) any {
              return v
            }(name)})}).(func(any) any)(func (resolvedType core.Type) any {
              return FromJson(types, name, resolvedType, value)
            }).(func(any) any)(lookedUp)
          }()
        }(v.Value)
        default:
        return [2]any{"left", libstrings.Cat([]any{"unsupported type for JSON decoding: ", showcore.Type_(typ)})}
      }
      return nil
    }(stripped)
  }()
}

func DecodeLiteral (lt core.LiteralType, value jsonmodel.Value) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.LiteralTypeBinary:
      return func (_ struct{}) any {
        return func () any {
          var strResult any = ExpectString(value)
          return libeithers.Map(func (s string) any {
            return core.TermLiteral{Value: core.LiteralBinary{Value: libliterals.StringToBinary(s).([]byte)}}
          }).(func(any) any)(strResult)
        }()
      }(v)
      case core.LiteralTypeBoolean:
      return func (_ struct{}) any {
        return func (x any) any {
          switch v := x.(type) {
            case jsonmodel.ValueBoolean:
            return func (b bool) any {
              return [2]any{"right", core.TermLiteral{Value: core.LiteralBoolean{Value: b}}}
            }(v.Value)
            default:
            return [2]any{"left", "expected boolean"}
          }
          return nil
        }(value)
      }(v)
      case core.LiteralTypeFloat:
      return func (ft core.FloatType) any {
        return DecodeFloat(ft, value)
      }(v.Value)
      case core.LiteralTypeInteger:
      return func (it core.IntegerType) any {
        return DecodeInteger(it, value)
      }(v.Value)
      case core.LiteralTypeString_:
      return func (_ struct{}) any {
        return func () any {
          var strResult any = ExpectString(value)
          return libeithers.Map(func (s string) any {
            return core.TermLiteral{Value: core.LiteralString_{Value: s}}
          }).(func(any) any)(strResult)
        }()
      }(v)
    }
    return nil
  }(lt)
}

func DecodeFloat (ft core.FloatType, value jsonmodel.Value) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.FloatTypeBigfloat:
      return func (_ struct{}) any {
        return func () any {
          var numResult any = ExpectNumber(value)
          return libeithers.Map(func (n float64) any {
            return core.TermLiteral{Value: core.LiteralFloat{Value: core.FloatValueBigfloat{Value: n}}}
          }).(func(any) any)(numResult)
        }()
      }(v)
      case core.FloatTypeFloat32_:
      return func (_ struct{}) any {
        return func () any {
          var strResult any = ExpectString(value)
          return libeithers.Either(func (err string) any {
            return [2]any{"left", err}
          }).(func(any) any)(func (s string) any {
            return func () any {
              var parsed any = libliterals.ReadFloat32(s)
              return libmaybes.Maybe([2]any{"left", libstrings.Cat([]any{"invalid float32: ", s})}).(func(any) any)(func (v float32) any {
                return [2]any{"right", core.TermLiteral{Value: core.LiteralFloat{Value: core.FloatValueFloat32_{Value: v}}}}
              }).(func(any) any)(parsed)
            }()
          }).(func(any) any)(strResult)
        }()
      }(v)
      case core.FloatTypeFloat64_:
      return func (_ struct{}) any {
        return func () any {
          var numResult any = ExpectNumber(value)
          return libeithers.Map(func (n float64) any {
            return core.TermLiteral{Value: core.LiteralFloat{Value: core.FloatValueFloat64_{Value: libliterals.BigfloatToFloat64(n).(float64)}}}
          }).(func(any) any)(numResult)
        }()
      }(v)
    }
    return nil
  }(ft)
}

func DecodeInteger (it core.IntegerType, value jsonmodel.Value) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.IntegerTypeBigint:
      return func (_ struct{}) any {
        return func () any {
          var strResult any = ExpectString(value)
          return libeithers.Either(func (err string) any {
            return [2]any{"left", err}
          }).(func(any) any)(func (s string) any {
            return func () any {
              var parsed any = libliterals.ReadBigint(s)
              return libmaybes.Maybe([2]any{"left", libstrings.Cat([]any{"invalid bigint: ", s})}).(func(any) any)(func (v *big.Int) any {
                return [2]any{"right", core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueBigint{Value: v}}}}
              }).(func(any) any)(parsed)
            }()
          }).(func(any) any)(strResult)
        }()
      }(v)
      case core.IntegerTypeInt64_:
      return func (_ struct{}) any {
        return func () any {
          var strResult any = ExpectString(value)
          return libeithers.Either(func (err string) any {
            return [2]any{"left", err}
          }).(func(any) any)(func (s string) any {
            return func () any {
              var parsed any = libliterals.ReadInt64(s)
              return libmaybes.Maybe([2]any{"left", libstrings.Cat([]any{"invalid int64: ", s})}).(func(any) any)(func (v int64) any {
                return [2]any{"right", core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt64_{Value: v}}}}
              }).(func(any) any)(parsed)
            }()
          }).(func(any) any)(strResult)
        }()
      }(v)
      case core.IntegerTypeUint32_:
      return func (_ struct{}) any {
        return func () any {
          var strResult any = ExpectString(value)
          return libeithers.Either(func (err string) any {
            return [2]any{"left", err}
          }).(func(any) any)(func (s string) any {
            return func () any {
              var parsed any = libliterals.ReadUint32(s)
              return libmaybes.Maybe([2]any{"left", libstrings.Cat([]any{"invalid uint32: ", s})}).(func(any) any)(func (v uint32) any {
                return [2]any{"right", core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueUint32_{Value: v}}}}
              }).(func(any) any)(parsed)
            }()
          }).(func(any) any)(strResult)
        }()
      }(v)
      case core.IntegerTypeUint64_:
      return func (_ struct{}) any {
        return func () any {
          var strResult any = ExpectString(value)
          return libeithers.Either(func (err string) any {
            return [2]any{"left", err}
          }).(func(any) any)(func (s string) any {
            return func () any {
              var parsed any = libliterals.ReadUint64(s)
              return libmaybes.Maybe([2]any{"left", libstrings.Cat([]any{"invalid uint64: ", s})}).(func(any) any)(func (v uint64) any {
                return [2]any{"right", core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueUint64_{Value: v}}}}
              }).(func(any) any)(parsed)
            }()
          }).(func(any) any)(strResult)
        }()
      }(v)
      case core.IntegerTypeInt8_:
      return func (_ struct{}) any {
        return func () any {
          var numResult any = ExpectNumber(value)
          return libeithers.Map(func (n float64) any {
            return core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt8_{Value: libliterals.BigintToInt8(libliterals.BigfloatToBigint(n)).(int8)}}}
          }).(func(any) any)(numResult)
        }()
      }(v)
      case core.IntegerTypeInt16_:
      return func (_ struct{}) any {
        return func () any {
          var numResult any = ExpectNumber(value)
          return libeithers.Map(func (n float64) any {
            return core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt16_{Value: libliterals.BigintToInt16(libliterals.BigfloatToBigint(n)).(int16)}}}
          }).(func(any) any)(numResult)
        }()
      }(v)
      case core.IntegerTypeInt32_:
      return func (_ struct{}) any {
        return func () any {
          var numResult any = ExpectNumber(value)
          return libeithers.Map(func (n float64) any {
            return core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: libliterals.BigintToInt32(libliterals.BigfloatToBigint(n)).(int32)}}}
          }).(func(any) any)(numResult)
        }()
      }(v)
      case core.IntegerTypeUint8_:
      return func (_ struct{}) any {
        return func () any {
          var numResult any = ExpectNumber(value)
          return libeithers.Map(func (n float64) any {
            return core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueUint8_{Value: libliterals.BigintToUint8(libliterals.BigfloatToBigint(n)).(uint8)}}}
          }).(func(any) any)(numResult)
        }()
      }(v)
      case core.IntegerTypeUint16_:
      return func (_ struct{}) any {
        return func () any {
          var numResult any = ExpectNumber(value)
          return libeithers.Map(func (n float64) any {
            return core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueUint16_{Value: libliterals.BigintToUint16(libliterals.BigfloatToBigint(n)).(uint16)}}}
          }).(func(any) any)(numResult)
        }()
      }(v)
    }
    return nil
  }(it)
}

func ExpectString (value jsonmodel.Value) any {
  return func (x any) any {
    switch v := x.(type) {
      case jsonmodel.ValueString_:
      return func (s string) any {
        return [2]any{"right", s}
      }(v.Value)
      default:
      return [2]any{"left", "expected string"}
    }
    return nil
  }(value)
}

func ExpectArray (value jsonmodel.Value) any {
  return func (x any) any {
    switch v := x.(type) {
      case jsonmodel.ValueArray:
      return func (arr []any) any {
        return [2]any{"right", arr}
      }(v.Value)
      default:
      return [2]any{"left", "expected array"}
    }
    return nil
  }(value)
}

func ExpectObject (value jsonmodel.Value) any {
  return func (x any) any {
    switch v := x.(type) {
      case jsonmodel.ValueObject:
      return func (obj []any) any {
        return [2]any{"right", obj}
      }(v.Value)
      default:
      return [2]any{"left", "expected object"}
    }
    return nil
  }(value)
}

func ExpectNumber (value jsonmodel.Value) any {
  return func (x any) any {
    switch v := x.(type) {
      case jsonmodel.ValueNumber:
      return func (n float64) any {
        return [2]any{"right", n}
      }(v.Value)
      default:
      return [2]any{"left", "expected number"}
    }
    return nil
  }(value)
}
