// Note: this is an automatically generated file. Do not edit.

package hydra.json;

/**
 * JSON encoding for Hydra terms. Converts Terms to JSON Values using Either for error handling.
 */
public interface Encode {
  static hydra.util.Either<String, hydra.json.model.Value> encodeFloat(hydra.core.FloatValue fv) {
    return (fv).accept(new hydra.core.FloatValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.FloatValue.Bigfloat bf) {
        String s = hydra.lib.literals.ShowBigfloat.apply((bf).value);
        return hydra.lib.logic.IfElse.lazy(
          hydra.json.Encode.requiresJsonStringSentinel(s),
          () -> hydra.util.Either.<String, hydra.json.model.Value>left(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "JSON cannot represent bigfloat value: ",
            s))),
          () -> hydra.util.Either.<String, hydra.json.model.Value>right(new hydra.json.model.Value.Number_(hydra.lib.literals.Float64ToDecimal.apply(hydra.lib.literals.BigfloatToFloat64.apply((bf).value)))));
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.FloatValue.Float32 f) {
        return hydra.util.Either.<String, hydra.json.model.Value>right(new hydra.json.model.Value.String_(hydra.lib.literals.ShowFloat32.apply((f).value)));
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.FloatValue.Float64 f) {
        String s = hydra.lib.literals.ShowFloat64.apply((f).value);
        return hydra.lib.logic.IfElse.lazy(
          hydra.json.Encode.requiresJsonStringSentinel(s),
          () -> hydra.util.Either.<String, hydra.json.model.Value>right(new hydra.json.model.Value.String_(s)),
          () -> hydra.util.Either.<String, hydra.json.model.Value>right(new hydra.json.model.Value.Number_(hydra.lib.literals.Float64ToDecimal.apply((f).value))));
      }
    });
  }

  static <T0> hydra.util.Either<T0, hydra.json.model.Value> encodeInteger(hydra.core.IntegerValue iv) {
    return (iv).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.IntegerValue.Bigint bi) {
        return hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.String_(hydra.lib.literals.ShowBigint.apply((bi).value)));
      }

      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.IntegerValue.Int64 i) {
        return hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.String_(hydra.lib.literals.ShowInt64.apply((i).value)));
      }

      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.IntegerValue.Uint32 i) {
        return hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.String_(hydra.lib.literals.ShowUint32.apply((i).value)));
      }

      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.IntegerValue.Uint64 i) {
        return hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.String_(hydra.lib.literals.ShowUint64.apply((i).value)));
      }

      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.IntegerValue.Int8 i) {
        return hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.Number_(hydra.lib.literals.BigintToDecimal.apply(hydra.lib.literals.Int8ToBigint.apply((i).value))));
      }

      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.IntegerValue.Int16 i) {
        return hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.Number_(hydra.lib.literals.BigintToDecimal.apply(hydra.lib.literals.Int16ToBigint.apply((i).value))));
      }

      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.IntegerValue.Int32 i) {
        return hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.Number_(hydra.lib.literals.BigintToDecimal.apply(hydra.lib.literals.Int32ToBigint.apply((i).value))));
      }

      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.IntegerValue.Uint8 i) {
        return hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.Number_(hydra.lib.literals.BigintToDecimal.apply(hydra.lib.literals.Uint8ToBigint.apply((i).value))));
      }

      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.IntegerValue.Uint16 i) {
        return hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.Number_(hydra.lib.literals.BigintToDecimal.apply(hydra.lib.literals.Uint16ToBigint.apply((i).value))));
      }
    });
  }

  static hydra.util.Either<String, hydra.json.model.Value> encodeLiteral(hydra.core.Literal lit) {
    return (lit).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Literal.Binary b) {
        return hydra.util.Either.<String, hydra.json.model.Value>right(new hydra.json.model.Value.String_(hydra.lib.literals.BinaryToString.apply((b).value)));
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Literal.Boolean_ b) {
        return hydra.util.Either.<String, hydra.json.model.Value>right(new hydra.json.model.Value.Boolean_((b).value));
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Literal.Decimal d) {
        return hydra.util.Either.<String, hydra.json.model.Value>right(new hydra.json.model.Value.Number_((d).value));
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Literal.Float_ f) {
        return hydra.json.Encode.encodeFloat((f).value);
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Literal.Integer_ i) {
        return hydra.json.Encode.encodeInteger((i).value);
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Literal.String_ s) {
        return hydra.util.Either.<String, hydra.json.model.Value>right(new hydra.json.model.Value.String_((s).value));
      }
    });
  }

  static Boolean requiresJsonStringSentinel(String s) {
    return hydra.lib.logic.Or.apply(
      hydra.lib.equality.Equal.apply(
        s,
        "NaN"),
      hydra.lib.logic.Or.apply(
        hydra.lib.equality.Equal.apply(
          s,
          "Infinity"),
        hydra.lib.logic.Or.apply(
          hydra.lib.equality.Equal.apply(
            s,
            "-Infinity"),
          hydra.lib.equality.Equal.apply(
            s,
            "-0.0"))));
  }

  static hydra.util.Either<String, hydra.json.model.Value> toJson(java.util.Map<hydra.core.Name, hydra.core.Type> types, hydra.core.Name tname, hydra.core.Type typ, hydra.core.Term term) {
    hydra.core.Type stripped = hydra.Strip.deannotateType(typ);
    hydra.core.Term strippedTerm = hydra.Strip.deannotateTerm(term);
    return (stripped).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.json.model.Value> otherwise(hydra.core.Type instance) {
        return hydra.util.Either.<String, hydra.json.model.Value>left(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "unsupported type for JSON encoding: ",
          hydra.show.Core.type(typ))));
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Type.Literal ignored) {
        return (strippedTerm).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Either<String, hydra.json.model.Value> otherwise(hydra.core.Term instance) {
            return hydra.util.Either.<String, hydra.json.model.Value>left("expected literal term");
          }

          @Override
          public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Literal lit) {
            return hydra.json.Encode.encodeLiteral((lit).value);
          }
        });
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Type.List elemType) {
        return (strippedTerm).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Either<String, hydra.json.model.Value> otherwise(hydra.core.Term instance) {
            return hydra.util.Either.<String, hydra.json.model.Value>left("expected list term");
          }

          @Override
          public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.List terms) {
            hydra.util.Lazy<hydra.util.Either<String, java.util.List<hydra.json.model.Value>>> results = new hydra.util.Lazy<>(() -> hydra.lib.eithers.MapList.apply(
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<String, hydra.json.model.Value>>) (t -> hydra.json.Encode.toJson(
                types,
                tname,
                (elemType).value,
                t)),
              (terms).value));
            return hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.util.List<hydra.json.model.Value>, hydra.json.model.Value>) (vs -> new hydra.json.model.Value.Array(vs)),
              results.get());
          }
        });
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Type.Set elemType) {
        return (strippedTerm).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Either<String, hydra.json.model.Value> otherwise(hydra.core.Term instance) {
            return hydra.util.Either.<String, hydra.json.model.Value>left("expected set term");
          }

          @Override
          public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Set vals) {
            hydra.util.Lazy<java.util.List<hydra.core.Term>> terms = new hydra.util.Lazy<>(() -> hydra.lib.sets.ToList.apply((vals).value));
            hydra.util.Lazy<hydra.util.Either<String, java.util.List<hydra.json.model.Value>>> results = new hydra.util.Lazy<>(() -> hydra.lib.eithers.MapList.apply(
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<String, hydra.json.model.Value>>) (t -> hydra.json.Encode.toJson(
                types,
                tname,
                (elemType).value,
                t)),
              terms.get()));
            return hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.util.List<hydra.json.model.Value>, hydra.json.model.Value>) (vs -> new hydra.json.model.Value.Array(vs)),
              results.get());
          }
        });
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Type.Maybe innerType) {
        hydra.core.Type innerStripped = hydra.Strip.deannotateType((innerType).value);
        Boolean isNestedMaybe = (innerStripped).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.Type instance) {
            return false;
          }

          @Override
          public Boolean visit(hydra.core.Type.Maybe ignored) {
            return true;
          }
        });
        return (strippedTerm).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Either<String, hydra.json.model.Value> otherwise(hydra.core.Term instance) {
            return hydra.util.Either.<String, hydra.json.model.Value>left("expected maybe term");
          }

          @Override
          public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Maybe opt) {
            return hydra.lib.maybes.Maybe.applyLazy(
              () -> hydra.util.Either.<String, hydra.json.model.Value>right(new hydra.json.model.Value.Null()),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<String, hydra.json.model.Value>>) (v -> {
                hydra.util.Either<String, hydra.json.model.Value> encoded = hydra.json.Encode.toJson(
                  types,
                  tname,
                  (innerType).value,
                  v);
                return hydra.lib.logic.IfElse.lazy(
                  isNestedMaybe,
                  () -> hydra.lib.eithers.Map.apply(
                    (java.util.function.Function<hydra.json.model.Value, hydra.json.model.Value>) (ev -> new hydra.json.model.Value.Array(java.util.Arrays.asList(ev))),
                    encoded),
                  () -> encoded);
              }),
              (opt).value);
          }
        });
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Type.Record rt) {
        return (strippedTerm).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Either<String, hydra.json.model.Value> otherwise(hydra.core.Term instance) {
            return hydra.util.Either.<String, hydra.json.model.Value>left("expected record term");
          }

          @Override
          public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Record r) {
            java.util.function.Function<hydra.core.Type, Boolean> isSimpleMaybe = (java.util.function.Function<hydra.core.Type, Boolean>) (ftype -> hydra.Strip.deannotateType(ftype).accept(new hydra.core.Type.PartialVisitor<>() {
              @Override
              public Boolean otherwise(hydra.core.Type instance) {
                return false;
              }

              @Override
              public Boolean visit(hydra.core.Type.Maybe innerT) {
                return hydra.Strip.deannotateType((innerT).value).accept(new hydra.core.Type.PartialVisitor<>() {
                  @Override
                  public Boolean otherwise(hydra.core.Type instance) {
                    return true;
                  }

                  @Override
                  public Boolean visit(hydra.core.Type.Maybe ignored) {
                    return false;
                  }
                });
              }
            }));
            java.util.function.Function<hydra.core.FieldType, java.util.function.Function<hydra.core.Field, hydra.util.Either<String, hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>>>> encodeFieldWithType = (java.util.function.Function<hydra.core.FieldType, java.util.function.Function<hydra.core.Field, hydra.util.Either<String, hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>>>>) (ft -> (java.util.function.Function<hydra.core.Field, hydra.util.Either<String, hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>>>) (f -> {
              String fname = (f).name.value;
              hydra.core.Term fterm = (f).term;
              hydra.core.Type ftype = (ft).type;
              return hydra.lib.logic.IfElse.lazy(
                (isSimpleMaybe).apply(ftype),
                () -> hydra.Strip.deannotateTerm(fterm).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<String, hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>> otherwise(hydra.core.Term instance) {
                    return hydra.util.Either.<String, hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>>left("expected maybe term for optional field");
                  }

                  @Override
                  public hydra.util.Either<String, hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>> visit(hydra.core.Term.Maybe opt) {
                    return hydra.lib.maybes.Maybe.applyLazy(
                      () -> hydra.util.Either.<String, hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>>right((hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>) (hydra.util.Maybe.<hydra.util.Pair<String, hydra.json.model.Value>>nothing())),
                      (java.util.function.Function<hydra.core.Term, hydra.util.Either<String, hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>>>) (v -> {
                        hydra.core.Type innerType = hydra.Strip.deannotateType(ftype).accept(new hydra.core.Type.PartialVisitor<>() {
                          @Override
                          public hydra.core.Type otherwise(hydra.core.Type instance) {
                            return ftype;
                          }

                          @Override
                          public hydra.core.Type visit(hydra.core.Type.Maybe it) {
                            return (it).value;
                          }
                        });
                        hydra.util.Either<String, hydra.json.model.Value> encoded = hydra.json.Encode.toJson(
                          types,
                          tname,
                          innerType,
                          v);
                        return hydra.lib.eithers.Map.apply(
                          (java.util.function.Function<hydra.json.model.Value, hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>>) (ev -> hydra.util.Maybe.just((hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>(fname, ev))))),
                          encoded);
                      }),
                      (opt).value);
                  }
                }),
                () -> ((java.util.function.Supplier<hydra.util.Either<String, hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>>>) (() -> {
                  hydra.util.Either<String, hydra.json.model.Value> encoded = hydra.json.Encode.toJson(
                    types,
                    tname,
                    ftype,
                    fterm);
                  return hydra.lib.eithers.Map.apply(
                    (java.util.function.Function<hydra.json.model.Value, hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>>) (ev -> hydra.util.Maybe.just((hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>(fname, ev))))),
                    encoded);
                })).get());
            }));
            java.util.List<hydra.core.FieldType> fieldTypes = (rt).value;
            java.util.List<hydra.core.Field> fields = (r).value.fields;
            hydra.util.Lazy<hydra.util.Either<String, java.util.List<hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>>>> encodedPairs = new hydra.util.Lazy<>(() -> hydra.lib.eithers.MapList.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.FieldType, hydra.core.Field>, hydra.util.Either<String, hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>>>) (ftf -> (encodeFieldWithType).apply(hydra.lib.pairs.First.apply(ftf)).apply(hydra.lib.pairs.Second.apply(ftf))),
              hydra.lib.lists.Zip.apply(
                fieldTypes,
                fields)));
            return hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.util.List<hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>>, hydra.json.model.Value>) (pairs -> new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(hydra.lib.maybes.Cat.apply(pairs)))),
              encodedPairs.get());
          }
        });
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Type.Union rt) {
        return (strippedTerm).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Either<String, hydra.json.model.Value> otherwise(hydra.core.Term instance) {
            return hydra.util.Either.<String, hydra.json.model.Value>left("expected union term");
          }

          @Override
          public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Inject inj) {
            hydra.core.Field field = (inj).value.field;
            String fname = (field).name.value;
            java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<String, hydra.core.Type>>> findFieldType = new java.util.concurrent.atomic.AtomicReference<>();
            findFieldType.set((java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<String, hydra.core.Type>>) (fts -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.lists.Null.apply(fts),
              () -> hydra.util.Either.<String, hydra.core.Type>left(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                "unknown variant: ",
                fname))),
              () -> hydra.lib.logic.IfElse.lazy(
                hydra.lib.equality.Equal.apply(
                  hydra.lib.lists.Head.apply(fts).name.value,
                  fname),
                () -> hydra.util.Either.<String, hydra.core.Type>right(hydra.lib.lists.Head.apply(fts).type),
                () -> findFieldType.get().apply(hydra.lib.lists.Tail.apply(fts))))));
            hydra.core.Term fterm = (field).term;
            hydra.util.Either<String, hydra.core.Type> ftypeResult = findFieldType.get().apply((rt).value);
            return hydra.lib.eithers.Either.apply(
              (java.util.function.Function<String, hydra.util.Either<String, hydra.json.model.Value>>) (err -> hydra.util.Either.<String, hydra.json.model.Value>left(err)),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.json.model.Value>>) (ftype -> {
                hydra.util.Either<String, hydra.json.model.Value> encodedUnion = hydra.json.Encode.toJson(
                  types,
                  tname,
                  ftype,
                  fterm);
                return hydra.lib.eithers.Map.apply(
                  (java.util.function.Function<hydra.json.model.Value, hydra.json.model.Value>) (v -> new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(java.util.Arrays.asList((hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>(fname, v))))))),
                  encodedUnion);
              }),
              ftypeResult);
          }
        });
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Type.Unit ignored) {
        return hydra.util.Either.<String, hydra.json.model.Value>right(new hydra.json.model.Value.Object_((java.util.Map<String, hydra.json.model.Value>) ((java.util.Map<String, hydra.json.model.Value>) (hydra.lib.maps.Empty.<String, hydra.json.model.Value>apply()))));
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Type.Wrap wn) {
        return (strippedTerm).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Either<String, hydra.json.model.Value> otherwise(hydra.core.Term instance) {
            return hydra.util.Either.<String, hydra.json.model.Value>left("expected wrapped term");
          }

          @Override
          public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Wrap wt) {
            return hydra.json.Encode.toJson(
              types,
              tname,
              (wn).value,
              (wt).value.body);
          }
        });
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Type.Map mt) {
        hydra.core.Type keyType = (mt).value.keys;
        hydra.core.Type valType = (mt).value.values;
        return (strippedTerm).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Either<String, hydra.json.model.Value> otherwise(hydra.core.Term instance) {
            return hydra.util.Either.<String, hydra.json.model.Value>left("expected map term");
          }

          @Override
          public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Map m) {
            java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.json.model.Value>> encodeEntry = (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.json.model.Value>>) (kv -> {
              hydra.util.Lazy<hydra.core.Term> k = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(kv));
              hydra.util.Either<String, hydra.json.model.Value> encodedK = hydra.json.Encode.toJson(
                types,
                tname,
                keyType,
                k.get());
              hydra.util.Lazy<hydra.core.Term> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(kv));
              hydra.util.Either<String, hydra.json.model.Value> encodedV = hydra.json.Encode.toJson(
                types,
                tname,
                valType,
                v.get());
              return hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<String, hydra.json.model.Value>>) (err -> hydra.util.Either.<String, hydra.json.model.Value>left(err)),
                (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<String, hydra.json.model.Value>>) (ek -> hydra.lib.eithers.Map.apply(
                  (java.util.function.Function<hydra.json.model.Value, hydra.json.model.Value>) (ev -> new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
                    (hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>("@key", ek))),
                    (hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>("@value", ev))))))),
                  encodedV)),
                encodedK);
            });
            hydra.util.Lazy<hydra.util.Either<String, java.util.List<hydra.json.model.Value>>> entries = new hydra.util.Lazy<>(() -> hydra.lib.eithers.MapList.apply(
              encodeEntry,
              hydra.lib.maps.ToList.apply((m).value)));
            return hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.util.List<hydra.json.model.Value>, hydra.json.model.Value>) (es -> new hydra.json.model.Value.Array(es)),
              entries.get());
          }
        });
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Type.Pair pt) {
        hydra.core.Type firstType = (pt).value.first;
        hydra.core.Type secondType = (pt).value.second;
        return (strippedTerm).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Either<String, hydra.json.model.Value> otherwise(hydra.core.Term instance) {
            return hydra.util.Either.<String, hydra.json.model.Value>left("expected pair term");
          }

          @Override
          public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Pair p) {
            hydra.util.Lazy<hydra.core.Term> first = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply((p).value));
            hydra.util.Either<String, hydra.json.model.Value> encodedFirst = hydra.json.Encode.toJson(
              types,
              tname,
              firstType,
              first.get());
            hydra.util.Lazy<hydra.core.Term> second = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply((p).value));
            hydra.util.Either<String, hydra.json.model.Value> encodedSecond = hydra.json.Encode.toJson(
              types,
              tname,
              secondType,
              second.get());
            return hydra.lib.eithers.Either.apply(
              (java.util.function.Function<String, hydra.util.Either<String, hydra.json.model.Value>>) (err -> hydra.util.Either.<String, hydra.json.model.Value>left(err)),
              (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<String, hydra.json.model.Value>>) (ef -> hydra.lib.eithers.Map.apply(
                (java.util.function.Function<hydra.json.model.Value, hydra.json.model.Value>) (es -> new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
                  (hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>("@first", ef))),
                  (hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>("@second", es))))))),
                encodedSecond)),
              encodedFirst);
          }
        });
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Type.Either et) {
        hydra.core.Type leftType = (et).value.left;
        hydra.core.Type rightType = (et).value.right;
        return (strippedTerm).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Either<String, hydra.json.model.Value> otherwise(hydra.core.Term instance) {
            return hydra.util.Either.<String, hydra.json.model.Value>left("expected either term");
          }

          @Override
          public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Either e) {
            return hydra.lib.eithers.Either.apply(
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<String, hydra.json.model.Value>>) (l -> {
                hydra.util.Either<String, hydra.json.model.Value> encodedL = hydra.json.Encode.toJson(
                  types,
                  tname,
                  leftType,
                  l);
                return hydra.lib.eithers.Map.apply(
                  (java.util.function.Function<hydra.json.model.Value, hydra.json.model.Value>) (v -> new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(java.util.Arrays.asList((hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>("@left", v))))))),
                  encodedL);
              }),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<String, hydra.json.model.Value>>) (r -> {
                hydra.util.Either<String, hydra.json.model.Value> encodedR = hydra.json.Encode.toJson(
                  types,
                  tname,
                  rightType,
                  r);
                return hydra.lib.eithers.Map.apply(
                  (java.util.function.Function<hydra.json.model.Value, hydra.json.model.Value>) (v -> new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(java.util.Arrays.asList((hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>("@right", v))))))),
                  encodedR);
              }),
              (e).value);
          }
        });
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Type.Variable name) {
        hydra.util.Lazy<hydra.util.Maybe<hydra.core.Type>> lookedUp = new hydra.util.Lazy<>(() -> hydra.lib.maps.Lookup.apply(
          (name).value,
          types));
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.json.Encode.toJsonUntyped(term),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.json.model.Value>>) (resolvedType -> hydra.json.Encode.toJson(
            types,
            (name).value,
            resolvedType,
            term)),
          lookedUp.get());
      }
    });
  }

  static hydra.util.Either<String, hydra.json.model.Value> toJsonUntyped(hydra.core.Term term) {
    hydra.core.Term stripped = hydra.Strip.deannotateTerm(term);
    return (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.json.model.Value> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<String, hydra.json.model.Value>left(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "unsupported term variant for JSON encoding: ",
          hydra.show.Core.term(term))));
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Literal lit) {
        return hydra.json.Encode.encodeLiteral((lit).value);
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.List terms) {
        hydra.util.Lazy<hydra.util.Either<String, java.util.List<hydra.json.model.Value>>> results = new hydra.util.Lazy<>(() -> hydra.lib.eithers.MapList.apply(
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<String, hydra.json.model.Value>>) (t -> hydra.json.Encode.toJsonUntyped(t)),
          (terms).value));
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<java.util.List<hydra.json.model.Value>, hydra.json.model.Value>) (vs -> new hydra.json.model.Value.Array(vs)),
          results.get());
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Set vals) {
        hydra.util.Lazy<java.util.List<hydra.core.Term>> terms = new hydra.util.Lazy<>(() -> hydra.lib.sets.ToList.apply((vals).value));
        hydra.util.Lazy<hydra.util.Either<String, java.util.List<hydra.json.model.Value>>> results = new hydra.util.Lazy<>(() -> hydra.lib.eithers.MapList.apply(
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<String, hydra.json.model.Value>>) (t -> hydra.json.Encode.toJsonUntyped(t)),
          terms.get()));
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<java.util.List<hydra.json.model.Value>, hydra.json.model.Value>) (vs -> new hydra.json.model.Value.Array(vs)),
          results.get());
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Maybe opt) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.util.Either.<String, hydra.json.model.Value>right(new hydra.json.model.Value.Null()),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<String, hydra.json.model.Value>>) (v -> {
            hydra.util.Either<String, hydra.json.model.Value> encodedMaybe = hydra.json.Encode.toJsonUntyped(v);
            return hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.json.model.Value, hydra.json.model.Value>) (encoded -> new hydra.json.model.Value.Array(java.util.Arrays.asList(encoded))),
              encodedMaybe);
          }),
          (opt).value);
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Record r) {
        java.util.function.Function<hydra.core.Field, hydra.util.Either<String, hydra.util.Pair<String, hydra.json.model.Value>>> encodeField = (java.util.function.Function<hydra.core.Field, hydra.util.Either<String, hydra.util.Pair<String, hydra.json.model.Value>>>) (f -> {
          hydra.core.Term fterm = (f).term;
          hydra.util.Either<String, hydra.json.model.Value> encodedField = hydra.json.Encode.toJsonUntyped(fterm);
          String fname = (f).name.value;
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<hydra.json.model.Value, hydra.util.Pair<String, hydra.json.model.Value>>) (v -> (hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>(fname, v)))),
            encodedField);
        });
        java.util.List<hydra.core.Field> fields = (r).value.fields;
        hydra.util.Lazy<hydra.util.Either<String, java.util.List<hydra.util.Pair<String, hydra.json.model.Value>>>> encodedFields = new hydra.util.Lazy<>(() -> hydra.lib.eithers.MapList.apply(
          encodeField,
          fields));
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<java.util.List<hydra.util.Pair<String, hydra.json.model.Value>>, hydra.json.model.Value>) (fs -> new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(fs))),
          encodedFields.get());
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Inject inj) {
        hydra.core.Field field = (inj).value.field;
        hydra.core.Term fterm = (field).term;
        hydra.util.Either<String, hydra.json.model.Value> encodedUnion = hydra.json.Encode.toJsonUntyped(fterm);
        String fname = (field).name.value;
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.json.model.Value, hydra.json.model.Value>) (v -> new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(java.util.Arrays.asList((hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>(fname, v))))))),
          encodedUnion);
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Unit ignored) {
        return hydra.util.Either.<String, hydra.json.model.Value>right(new hydra.json.model.Value.Object_((java.util.Map<String, hydra.json.model.Value>) ((java.util.Map<String, hydra.json.model.Value>) (hydra.lib.maps.Empty.<String, hydra.json.model.Value>apply()))));
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Wrap wt) {
        return hydra.json.Encode.toJsonUntyped((wt).value.body);
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Map m) {
        java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.json.model.Value>> encodeEntry = (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.json.model.Value>>) (kv -> {
          hydra.util.Lazy<hydra.core.Term> k = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(kv));
          hydra.util.Either<String, hydra.json.model.Value> encodedK = hydra.json.Encode.toJsonUntyped(k.get());
          hydra.util.Lazy<hydra.core.Term> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(kv));
          hydra.util.Either<String, hydra.json.model.Value> encodedV = hydra.json.Encode.toJsonUntyped(v.get());
          return hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, hydra.util.Either<String, hydra.json.model.Value>>) (err -> hydra.util.Either.<String, hydra.json.model.Value>left(err)),
            (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<String, hydra.json.model.Value>>) (ek -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.json.model.Value, hydra.json.model.Value>) (ev -> new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
                (hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>("@key", ek))),
                (hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>("@value", ev))))))),
              encodedV)),
            encodedK);
        });
        hydra.util.Lazy<hydra.util.Either<String, java.util.List<hydra.json.model.Value>>> entries = new hydra.util.Lazy<>(() -> hydra.lib.eithers.MapList.apply(
          encodeEntry,
          hydra.lib.maps.ToList.apply((m).value)));
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<java.util.List<hydra.json.model.Value>, hydra.json.model.Value>) (es -> new hydra.json.model.Value.Array(es)),
          entries.get());
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Pair p) {
        hydra.util.Lazy<hydra.core.Term> first = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply((p).value));
        hydra.util.Either<String, hydra.json.model.Value> encodedFirst = hydra.json.Encode.toJsonUntyped(first.get());
        hydra.util.Lazy<hydra.core.Term> second = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply((p).value));
        hydra.util.Either<String, hydra.json.model.Value> encodedSecond = hydra.json.Encode.toJsonUntyped(second.get());
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, hydra.util.Either<String, hydra.json.model.Value>>) (err -> hydra.util.Either.<String, hydra.json.model.Value>left(err)),
          (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<String, hydra.json.model.Value>>) (ef -> hydra.lib.eithers.Map.apply(
            (java.util.function.Function<hydra.json.model.Value, hydra.json.model.Value>) (es -> new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
              (hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>("@first", ef))),
              (hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>("@second", es))))))),
            encodedSecond)),
          encodedFirst);
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Either e) {
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<String, hydra.json.model.Value>>) (l -> {
            hydra.util.Either<String, hydra.json.model.Value> encodedL = hydra.json.Encode.toJsonUntyped(l);
            return hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.json.model.Value, hydra.json.model.Value>) (v -> new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(java.util.Arrays.asList((hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>("@left", v))))))),
              encodedL);
          }),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<String, hydra.json.model.Value>>) (r -> {
            hydra.util.Either<String, hydra.json.model.Value> encodedR = hydra.json.Encode.toJsonUntyped(r);
            return hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.json.model.Value, hydra.json.model.Value>) (v -> new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(java.util.Arrays.asList((hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>("@right", v))))))),
              encodedR);
          }),
          (e).value);
      }
    });
  }
}
