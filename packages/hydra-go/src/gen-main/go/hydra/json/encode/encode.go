// Note: this is an automatically generated file. Do not edit.

package jsonencode

import (
  "hydra.dev/hydra/core"
  jsonmodel "hydra.dev/hydra/json/model"
  libeithers "hydra.dev/hydra/lib/eithers"
  libliterals "hydra.dev/hydra/lib/literals"
  libmaps "hydra.dev/hydra/lib/maps"
  libmaybes "hydra.dev/hydra/lib/maybes"
  libpairs "hydra.dev/hydra/lib/pairs"
  libsets "hydra.dev/hydra/lib/sets"
  libstrings "hydra.dev/hydra/lib/strings"
  "hydra.dev/hydra/rewriting"
  showcore "hydra.dev/hydra/show/core"
  "math/big"
)

func ToJson (term core.Term) any {
  return func () any {
    var stripped any = rewriting.DeannotateTerm(term)
    return func (x any) any {
      switch v := x.(type) {
        case core.TermLiteral:
        return func (lit core.Literal) any {
          return EncodeLiteral(lit)
        }(v.Value)
        case core.TermList:
        return func (terms []any) any {
          return func () any {
            var results any = libeithers.MapList(func (t core.Term) any {
              return ToJson(t)
            }).(func(any) any)(terms)
            return libeithers.Map(func (vs []any) any {
              return jsonmodel.ValueArray{Value: vs}
            }).(func(any) any)(results)
          }()
        }(v.Value)
        case core.TermSet:
        return func (vals []any) any {
          return func () any {
            var terms any = libsets.ToList(vals)
            return func () any {
              var results any = libeithers.MapList(func (t core.Term) any {
                return ToJson(t)
              }).(func(any) any)(terms)
              return libeithers.Map(func (vs []any) any {
                return jsonmodel.ValueArray{Value: vs}
              }).(func(any) any)(results)
            }()
          }()
        }(v.Value)
        case core.TermMaybe:
        return func (opt any) any {
          return libmaybes.Maybe([2]any{"right", jsonmodel.ValueNull{}}).(func(any) any)(func (v core.Term) any {
            return func () any {
              var encodedMaybe any = ToJson(v)
              return libeithers.Map(func (encoded jsonmodel.Value) any {
                return jsonmodel.ValueArray{Value: []any{encoded}}
              }).(func(any) any)(encodedMaybe)
            }()
          }).(func(any) any)(opt)
        }(v.Value)
        case core.TermRecord:
        return func (r core.Record) any {
          return func () any {
            encodeField := func (f core.Field) any {
              return func () any {
                var fname any = func (v any) any {
                  return v.(core.Field).Name
                }(f)
                return func () any {
                  var fterm any = func (v any) any {
                    return v.(core.Field).Term
                  }(f)
                  return func () any {
                    var encodedField any = ToJson(fterm.(core.Term))
                    return libeithers.Map(func (v jsonmodel.Value) any {
                      return [2]any{fname, v}
                    }).(func(any) any)(encodedField)
                  }()
                }()
              }()
            }
            return func () any {
              var fields any = func (v any) any {
                return v.(core.Record).Fields
              }(r)
              return func () any {
                var encodedFields any = libeithers.MapList(encodeField).(func(any) any)(fields)
                return libeithers.Map(func (fs []any) any {
                  return jsonmodel.ValueObject{Value: libmaps.FromList(fs).([]any)}
                }).(func(any) any)(encodedFields)
              }()
            }()
          }()
        }(v.Value)
        case core.TermUnion:
        return func (inj core.Injection) any {
          return func () any {
            var field any = func (v any) any {
              return v.(core.Injection).Field
            }(inj)
            return func () any {
              var fname any = func (v any) any {
                return v
              }(field.(core.Field).Name)
              return func () any {
                var fterm any = field.(core.Field).Term
                return func () any {
                  var encodedUnion any = ToJson(fterm.(core.Term))
                  return libeithers.Map(func (v jsonmodel.Value) any {
                    return jsonmodel.ValueObject{Value: libmaps.FromList([]any{[2]any{fname, v}}).([]any)}
                  }).(func(any) any)(encodedUnion)
                }()
              }()
            }()
          }()
        }(v.Value)
        case core.TermUnit:
        return func (_ struct{}) any {
          return [2]any{"right", jsonmodel.ValueObject{Value: libmaps.Empty}}
        }(v)
        case core.TermWrap:
        return func (wt core.WrappedTerm) any {
          return ToJson(func (v any) any {
            return v.(core.WrappedTerm).Body
          }(wt).(core.Term))
        }(v.Value)
        case core.TermMap_:
        return func (m []any) any {
          return func () any {
            encodeEntry := func (kv any) any {
              return func () any {
                var k any = libpairs.First(kv)
                return func () any {
                  var v any = libpairs.Second(kv)
                  return func () any {
                    var encodedK any = ToJson(k.(core.Term))
                    return func () any {
                      var encodedV any = ToJson(v.(core.Term))
                      return libeithers.Either(func (err string) any {
                        return [2]any{"left", err}
                      }).(func(any) any)(func (ek jsonmodel.Value) any {
                        return libeithers.Map(func (ev jsonmodel.Value) any {
                          return jsonmodel.ValueObject{Value: libmaps.FromList([]any{[2]any{"@key", ek}, [2]any{"@value", ev}}).([]any)}
                        }).(func(any) any)(encodedV)
                      }).(func(any) any)(encodedK)
                    }()
                  }()
                }()
              }()
            }
            return func () any {
              var entries any = libeithers.MapList(encodeEntry).(func(any) any)(libmaps.ToList(m))
              return libeithers.Map(func (es []any) any {
                return jsonmodel.ValueArray{Value: es}
              }).(func(any) any)(entries)
            }()
          }()
        }(v.Value)
        case core.TermPair:
        return func (p any) any {
          return func () any {
            var first any = libpairs.First(p)
            return func () any {
              var second any = libpairs.Second(p)
              return func () any {
                var encodedFirst any = ToJson(first.(core.Term))
                return func () any {
                  var encodedSecond any = ToJson(second.(core.Term))
                  return libeithers.Either(func (err string) any {
                    return [2]any{"left", err}
                  }).(func(any) any)(func (ef jsonmodel.Value) any {
                    return libeithers.Map(func (es jsonmodel.Value) any {
                      return jsonmodel.ValueObject{Value: libmaps.FromList([]any{[2]any{"@first", ef}, [2]any{"@second", es}}).([]any)}
                    }).(func(any) any)(encodedSecond)
                  }).(func(any) any)(encodedFirst)
                }()
              }()
            }()
          }()
        }(v.Value)
        case core.TermEither:
        return func (e any) any {
          return libeithers.Either(func (l core.Term) any {
            return func () any {
              var encodedL any = ToJson(l)
              return libeithers.Map(func (v jsonmodel.Value) any {
                return jsonmodel.ValueObject{Value: libmaps.FromList([]any{[2]any{"@left", v}}).([]any)}
              }).(func(any) any)(encodedL)
            }()
          }).(func(any) any)(func (r core.Term) any {
            return func () any {
              var encodedR any = ToJson(r)
              return libeithers.Map(func (v jsonmodel.Value) any {
                return jsonmodel.ValueObject{Value: libmaps.FromList([]any{[2]any{"@right", v}}).([]any)}
              }).(func(any) any)(encodedR)
            }()
          }).(func(any) any)(e)
        }(v.Value)
        default:
        return [2]any{"left", libstrings.Cat([]any{"unsupported term variant for JSON encoding: ", showcore.Term(term)})}
      }
      return nil
    }(stripped)
  }()
}

func EncodeLiteral (lit core.Literal) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.LiteralBinary:
      return func (b []byte) any {
        return [2]any{"right", jsonmodel.ValueString_{Value: libliterals.BinaryToString(b).(string)}}
      }(v.Value)
      case core.LiteralBoolean:
      return func (b bool) any {
        return [2]any{"right", jsonmodel.ValueBoolean{Value: b}}
      }(v.Value)
      case core.LiteralFloat:
      return func (f core.FloatValue) any {
        return EncodeFloat(f)
      }(v.Value)
      case core.LiteralInteger:
      return func (i core.IntegerValue) any {
        return EncodeInteger(i)
      }(v.Value)
      case core.LiteralString_:
      return func (s string) any {
        return [2]any{"right", jsonmodel.ValueString_{Value: s}}
      }(v.Value)
    }
    return nil
  }(lit)
}

func EncodeFloat (fv core.FloatValue) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.FloatValueBigfloat:
      return func (bf float64) any {
        return [2]any{"right", jsonmodel.ValueNumber{Value: bf}}
      }(v.Value)
      case core.FloatValueFloat32_:
      return func (f float32) any {
        return [2]any{"right", jsonmodel.ValueString_{Value: libliterals.ShowFloat32(f).(string)}}
      }(v.Value)
      case core.FloatValueFloat64_:
      return func (f float64) any {
        return [2]any{"right", jsonmodel.ValueNumber{Value: libliterals.Float64ToBigfloat(f).(float64)}}
      }(v.Value)
    }
    return nil
  }(fv)
}

func EncodeInteger (iv core.IntegerValue) any {
  return func (x any) any {
    switch v := x.(type) {
      case core.IntegerValueBigint:
      return func (bi *big.Int) any {
        return [2]any{"right", jsonmodel.ValueString_{Value: libliterals.ShowBigint(bi).(string)}}
      }(v.Value)
      case core.IntegerValueInt64_:
      return func (i int64) any {
        return [2]any{"right", jsonmodel.ValueString_{Value: libliterals.ShowInt64(i).(string)}}
      }(v.Value)
      case core.IntegerValueUint32_:
      return func (i uint32) any {
        return [2]any{"right", jsonmodel.ValueString_{Value: libliterals.ShowUint32(i).(string)}}
      }(v.Value)
      case core.IntegerValueUint64_:
      return func (i uint64) any {
        return [2]any{"right", jsonmodel.ValueString_{Value: libliterals.ShowUint64(i).(string)}}
      }(v.Value)
      case core.IntegerValueInt8_:
      return func (i int8) any {
        return [2]any{"right", jsonmodel.ValueNumber{Value: libliterals.BigintToBigfloat(libliterals.Int8ToBigint(i)).(float64)}}
      }(v.Value)
      case core.IntegerValueInt16_:
      return func (i int16) any {
        return [2]any{"right", jsonmodel.ValueNumber{Value: libliterals.BigintToBigfloat(libliterals.Int16ToBigint(i)).(float64)}}
      }(v.Value)
      case core.IntegerValueInt32_:
      return func (i int32) any {
        return [2]any{"right", jsonmodel.ValueNumber{Value: libliterals.BigintToBigfloat(libliterals.Int32ToBigint(i)).(float64)}}
      }(v.Value)
      case core.IntegerValueUint8_:
      return func (i uint8) any {
        return [2]any{"right", jsonmodel.ValueNumber{Value: libliterals.BigintToBigfloat(libliterals.Uint8ToBigint(i)).(float64)}}
      }(v.Value)
      case core.IntegerValueUint16_:
      return func (i uint16) any {
        return [2]any{"right", jsonmodel.ValueNumber{Value: libliterals.BigintToBigfloat(libliterals.Uint16ToBigint(i)).(float64)}}
      }(v.Value)
    }
    return nil
  }(iv)
}
