// Note: this is an automatically generated file. Do not edit.

package hydra.json.decode;

/**
 * JSON decoding for Hydra terms. Converts JSON Values to Terms using Either for error handling.
 */
public interface Decode {
  static hydra.util.Either<String, hydra.core.Term> fromJson(java.util.Map<hydra.core.Name, hydra.core.Type> types, hydra.core.Type typ, hydra.json.model.Value value) {
    hydra.core.Type stripped = hydra.rewriting.Rewriting.deannotateType((typ));
    return ((stripped)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.core.Term> otherwise(hydra.core.Type instance) {
        return (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left(hydra.lib.strings.Cat.apply(java.util.List.of(
          "unsupported type for JSON decoding: ",
          hydra.show.core.Core.type((typ)))))));
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.Type.Literal lt) {
        return hydra.json.decode.Decode.decodeLiteral(
          ((lt)).value,
          (value));
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.Type.List elemType) {
        hydra.util.Either<String, java.util.List<hydra.json.model.Value>> arrResult = hydra.json.decode.Decode.expectArray((value));
        java.util.function.Function<hydra.json.model.Value, hydra.util.Either<String, hydra.core.Term>> decodeElem = (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<String, hydra.core.Term>>) (v -> hydra.json.decode.Decode.fromJson(
          (types),
          ((elemType)).value,
          (v)));
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, hydra.util.Either<String, hydra.core.Term>>) (err -> (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left((err))))),
          (java.util.function.Function<java.util.List<hydra.json.model.Value>, hydra.util.Either<String, hydra.core.Term>>) (arr -> {
            hydra.util.Either<String, java.util.List<hydra.core.Term>> decoded = hydra.lib.eithers.MapList.apply(
              (decodeElem),
              (arr));
            return hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term>) (ts -> new hydra.core.Term.List((ts))),
              (decoded));
          }),
          (arrResult));
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.Type.Set elemType) {
        hydra.util.Either<String, java.util.List<hydra.json.model.Value>> arrResult = hydra.json.decode.Decode.expectArray((value));
        java.util.function.Function<hydra.json.model.Value, hydra.util.Either<String, hydra.core.Term>> decodeElem = (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<String, hydra.core.Term>>) (v -> hydra.json.decode.Decode.fromJson(
          (types),
          ((elemType)).value,
          (v)));
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, hydra.util.Either<String, hydra.core.Term>>) (err -> (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left((err))))),
          (java.util.function.Function<java.util.List<hydra.json.model.Value>, hydra.util.Either<String, hydra.core.Term>>) (arr -> {
            hydra.util.Either<String, java.util.List<hydra.core.Term>> decoded = hydra.lib.eithers.MapList.apply(
              (decodeElem),
              (arr));
            return hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term>) (elems -> new hydra.core.Term.Set(hydra.lib.sets.FromList.apply((elems)))),
              (decoded));
          }),
          (arrResult));
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.Type.Maybe innerType) {
        java.util.function.Function<java.util.List<hydra.json.model.Value>, hydra.util.Either<String, hydra.core.Term>> decodeJust = (java.util.function.Function<java.util.List<hydra.json.model.Value>, hydra.util.Either<String, hydra.core.Term>>) (arr -> hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v -> new hydra.core.Term.Maybe(hydra.util.Maybe.just((v)))),
          hydra.json.decode.Decode.fromJson(
            (types),
            ((innerType)).value,
            hydra.lib.lists.Head.apply((arr)))));
        java.util.function.Function<java.util.List<hydra.json.model.Value>, hydra.util.Either<String, hydra.core.Term>> decodeMaybeArray = (java.util.function.Function<java.util.List<hydra.json.model.Value>, hydra.util.Either<String, hydra.core.Term>>) (arr -> {
          Integer len = hydra.lib.lists.Length.apply((arr));
          return hydra.lib.logic.IfElse.apply(
            hydra.lib.equality.Equal.apply(
              (len),
              0),
            (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>right(new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))))),
            hydra.lib.logic.IfElse.apply(
              hydra.lib.equality.Equal.apply(
                (len),
                1),
              ((decodeJust)).apply((arr)),
              (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left("expected single-element array for Just")))));
        });
        return ((value)).accept(new hydra.json.model.Value.PartialVisitor<>() {
          @Override
          public hydra.util.Either<String, hydra.core.Term> otherwise(hydra.json.model.Value instance) {
            return (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left("expected null or single-element array for Maybe")));
          }
          
          @Override
          public hydra.util.Either<String, hydra.core.Term> visit(hydra.json.model.Value.Null ignored) {
            return (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>right(new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())))));
          }
          
          @Override
          public hydra.util.Either<String, hydra.core.Term> visit(hydra.json.model.Value.Array arr) {
            return ((decodeMaybeArray)).apply(((arr)).value);
          }
        });
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.Type.Record rt) {
        hydra.util.Either<String, java.util.Map<String, hydra.json.model.Value>> objResult = hydra.json.decode.Decode.expectObject((value));
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, hydra.util.Either<String, hydra.core.Term>>) (err -> (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left((err))))),
          (java.util.function.Function<java.util.Map<String, hydra.json.model.Value>, hydra.util.Either<String, hydra.core.Term>>) (obj -> {
            java.util.function.Function<hydra.core.FieldType, hydra.util.Either<String, hydra.core.Field>> decodeField = (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<String, hydra.core.Field>>) (ft -> {
              hydra.core.Type ftype = ((ft)).type;
              hydra.json.model.Value defaultVal = new hydra.json.model.Value.Null(true);
              hydra.core.Name fname = ((ft)).name;
              hydra.util.Maybe<hydra.json.model.Value> mval = hydra.lib.maps.Lookup.apply(
                ((fname)).value,
                (obj));
              hydra.json.model.Value jsonVal = hydra.lib.maybes.FromMaybe.apply(
                (defaultVal),
                (mval));
              hydra.util.Either<String, hydra.core.Term> decoded = hydra.json.decode.Decode.fromJson(
                (types),
                (ftype),
                (jsonVal));
              return hydra.lib.eithers.Map.apply(
                (java.util.function.Function<hydra.core.Term, hydra.core.Field>) (v -> new hydra.core.Field((fname), (v))),
                (decoded));
            });
            java.util.List<hydra.core.FieldType> fields = (((rt)).value).fields;
            hydra.util.Either<String, java.util.List<hydra.core.Field>> decodedFields = hydra.lib.eithers.MapList.apply(
              (decodeField),
              (fields));
            return hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.util.List<hydra.core.Field>, hydra.core.Term>) (fs -> new hydra.core.Term.Record(new hydra.core.Record((((rt)).value).typeName, (fs)))),
              (decodedFields));
          }),
          (objResult));
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.Type.Union rt) {
        java.util.function.Function<String, java.util.function.Function<hydra.util.Maybe<hydra.json.model.Value>, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.core.Term>>>> decodeVariant = (java.util.function.Function<String, java.util.function.Function<hydra.util.Maybe<hydra.json.model.Value>, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.core.Term>>>>) (key -> (java.util.function.Function<hydra.util.Maybe<hydra.json.model.Value>, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.core.Term>>>) (val -> (java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.core.Term>>) (ftype -> {
          hydra.json.model.Value jsonVal = hydra.lib.maybes.FromMaybe.apply(
            new hydra.json.model.Value.Null(true),
            (val));
          hydra.util.Either<String, hydra.core.Term> decoded = hydra.json.decode.Decode.fromJson(
            (types),
            (ftype),
            (jsonVal));
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v -> new hydra.core.Term.Union(new hydra.core.Injection((((rt)).value).typeName, new hydra.core.Field(new hydra.core.Name((key)), (v))))),
            (decoded));
        })));
        java.util.function.Function<String, java.util.function.Function<hydra.util.Maybe<hydra.json.model.Value>, java.util.function.Function<hydra.core.FieldType, hydra.util.Maybe<hydra.util.Either<String, hydra.core.Term>>>>> tryField = (java.util.function.Function<String, java.util.function.Function<hydra.util.Maybe<hydra.json.model.Value>, java.util.function.Function<hydra.core.FieldType, hydra.util.Maybe<hydra.util.Either<String, hydra.core.Term>>>>>) (key -> (java.util.function.Function<hydra.util.Maybe<hydra.json.model.Value>, java.util.function.Function<hydra.core.FieldType, hydra.util.Maybe<hydra.util.Either<String, hydra.core.Term>>>>) (val -> (java.util.function.Function<hydra.core.FieldType, hydra.util.Maybe<hydra.util.Either<String, hydra.core.Term>>>) (ft -> hydra.lib.logic.IfElse.apply(
          hydra.lib.equality.Equal.apply(
            (((ft)).name).value,
            (key)),
          hydra.util.Maybe.just(((((decodeVariant)).apply((key))).apply((val))).apply(((ft)).type)),
          (hydra.util.Maybe<hydra.util.Either<String, hydra.core.Term>>) (hydra.util.Maybe.<hydra.util.Either<String, hydra.core.Term>>nothing())))));
        java.util.concurrent.atomic.AtomicReference<java.util.function.Function<String, java.util.function.Function<hydra.util.Maybe<hydra.json.model.Value>, java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<String, hydra.core.Term>>>>> findAndDecode = new java.util.concurrent.atomic.AtomicReference<>();
        findAndDecode.set((java.util.function.Function<String, java.util.function.Function<hydra.util.Maybe<hydra.json.model.Value>, java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<String, hydra.core.Term>>>>) (key -> (java.util.function.Function<hydra.util.Maybe<hydra.json.model.Value>, java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<String, hydra.core.Term>>>) (val -> (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<String, hydra.core.Term>>) (fts -> hydra.lib.logic.IfElse.apply(
          hydra.lib.lists.Null.apply((fts)),
          (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left(hydra.lib.strings.Cat.apply(java.util.List.of(
            "unknown variant: ",
            (key)))))),
          hydra.lib.maybes.Maybe.apply(
            (((findAndDecode.get()).apply((key))).apply((val))).apply(hydra.lib.lists.Tail.apply((fts))),
            (java.util.function.Function<hydra.util.Either<String, hydra.core.Term>, hydra.util.Either<String, hydra.core.Term>>) (r -> (r)),
            ((((tryField)).apply((key))).apply((val))).apply(hydra.lib.lists.Head.apply((fts)))))))));
        java.util.function.Function<java.util.Map<String, hydra.json.model.Value>, hydra.util.Either<String, hydra.core.Term>> decodeSingleKey = (java.util.function.Function<java.util.Map<String, hydra.json.model.Value>, hydra.util.Either<String, hydra.core.Term>>) (obj -> (((findAndDecode.get()).apply(hydra.lib.lists.Head.apply(hydra.lib.maps.Keys.apply((obj))))).apply(hydra.lib.maps.Lookup.apply(
          hydra.lib.lists.Head.apply(hydra.lib.maps.Keys.apply((obj))),
          (obj)))).apply((((rt)).value).fields));
        hydra.util.Either<String, java.util.Map<String, hydra.json.model.Value>> objResult = hydra.json.decode.Decode.expectObject((value));
        java.util.function.Function<java.util.Map<String, hydra.json.model.Value>, hydra.util.Either<String, hydra.core.Term>> processUnion = (java.util.function.Function<java.util.Map<String, hydra.json.model.Value>, hydra.util.Either<String, hydra.core.Term>>) (obj -> hydra.lib.logic.IfElse.apply(
          hydra.lib.equality.Equal.apply(
            hydra.lib.lists.Length.apply(hydra.lib.maps.Keys.apply((obj))),
            1),
          ((decodeSingleKey)).apply((obj)),
          (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left("expected single-key object for union")))));
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, hydra.util.Either<String, hydra.core.Term>>) (err -> (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left((err))))),
          (java.util.function.Function<java.util.Map<String, hydra.json.model.Value>, hydra.util.Either<String, hydra.core.Term>>) (obj -> ((processUnion)).apply((obj))),
          (objResult));
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.Type.Unit ignored) {
        hydra.util.Either<String, java.util.Map<String, hydra.json.model.Value>> objResult = hydra.json.decode.Decode.expectObject((value));
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<java.util.Map<String, hydra.json.model.Value>, hydra.core.Term>) (_2 -> new hydra.core.Term.Unit(true)),
          (objResult));
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.Type.Wrap wn) {
        java.util.function.Function<hydra.core.Type, hydra.core.Type> extractInnerType = (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (lt -> ((lt)).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.core.Type otherwise(hydra.core.Type instance) {
            return (lt);
          }
          
          @Override
          public hydra.core.Type visit(hydra.core.Type.Wrap wt) {
            return (((wt)).value).body;
          }
        }));
        java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.core.Term>> decodeAndWrap = (java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.core.Term>>) (lt -> {
          hydra.core.Type innerType = ((extractInnerType)).apply((lt));
          hydra.util.Either<String, hydra.core.Term> decoded = hydra.json.decode.Decode.fromJson(
            (types),
            (innerType),
            (value));
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v -> new hydra.core.Term.Wrap(new hydra.core.WrappedTerm((((wn)).value).typeName, (v)))),
            (decoded));
        });
        hydra.util.Maybe<hydra.core.Type> lookedUp = hydra.lib.maps.Lookup.apply(
          (((wn)).value).typeName,
          (types));
        return hydra.lib.maybes.Maybe.apply(
          (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left(hydra.lib.strings.Cat.apply(java.util.List.of(
            "unknown wrapped type: ",
            ((((wn)).value).typeName).value))))),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.core.Term>>) (lt -> ((decodeAndWrap)).apply((lt))),
          (lookedUp));
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.Type.Map mt) {
        hydra.util.Either<String, java.util.List<hydra.json.model.Value>> arrResult = hydra.json.decode.Decode.expectArray((value));
        hydra.core.Type keyType = (((mt)).value).keys;
        hydra.core.Type valType = (((mt)).value).values;
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, hydra.util.Either<String, hydra.core.Term>>) (err -> (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left((err))))),
          (java.util.function.Function<java.util.List<hydra.json.model.Value>, hydra.util.Either<String, hydra.core.Term>>) (arr -> {
            java.util.function.Function<hydra.json.model.Value, hydra.util.Either<String, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>> decodeEntry = (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<String, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>>) (entryJson -> {
              hydra.util.Either<String, java.util.Map<String, hydra.json.model.Value>> objResult = hydra.json.decode.Decode.expectObject((entryJson));
              return hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<String, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>>) (err -> (hydra.util.Either<String, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>left((err))))),
                (java.util.function.Function<java.util.Map<String, hydra.json.model.Value>, hydra.util.Either<String, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>>) (entryObj -> {
                  hydra.util.Maybe<hydra.json.model.Value> keyJson = hydra.lib.maps.Lookup.apply(
                    "@key",
                    (entryObj));
                  hydra.util.Maybe<hydra.json.model.Value> valJson = hydra.lib.maps.Lookup.apply(
                    "@value",
                    (entryObj));
                  return hydra.lib.maybes.Maybe.apply(
                    (hydra.util.Either<String, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>left("missing @key in map entry"))),
                    (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<String, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>>) (kj -> hydra.lib.maybes.Maybe.apply(
                      (hydra.util.Either<String, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>left("missing @value in map entry"))),
                      (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<String, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>>) (vj -> {
                        hydra.util.Either<String, hydra.core.Term> decodedKey = hydra.json.decode.Decode.fromJson(
                          (types),
                          (keyType),
                          (kj));
                        hydra.util.Either<String, hydra.core.Term> decodedVal = hydra.json.decode.Decode.fromJson(
                          (types),
                          (valType),
                          (vj));
                        return hydra.lib.eithers.Either.apply(
                          (java.util.function.Function<String, hydra.util.Either<String, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>>) (err -> (hydra.util.Either<String, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>left((err))))),
                          (java.util.function.Function<hydra.core.Term, hydra.util.Either<String, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>>) (k -> hydra.lib.eithers.Map.apply(
                            (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>) (v -> (hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>((k), (v))))),
                            (decodedVal))),
                          (decodedKey));
                      }),
                      (valJson))),
                    (keyJson));
                }),
                (objResult));
            });
            hydra.util.Either<String, java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>> entries = hydra.lib.eithers.MapList.apply(
              (decodeEntry),
              (arr));
            return hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>, hydra.core.Term>) (es -> new hydra.core.Term.Map(hydra.lib.maps.FromList.apply((es)))),
              (entries));
          }),
          (arrResult));
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.Type.Pair pt) {
        hydra.core.Type firstType = (((pt)).value).first;
        hydra.util.Either<String, java.util.Map<String, hydra.json.model.Value>> objResult = hydra.json.decode.Decode.expectObject((value));
        hydra.core.Type secondType = (((pt)).value).second;
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, hydra.util.Either<String, hydra.core.Term>>) (err -> (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left((err))))),
          (java.util.function.Function<java.util.Map<String, hydra.json.model.Value>, hydra.util.Either<String, hydra.core.Term>>) (obj -> {
            hydra.util.Maybe<hydra.json.model.Value> firstJson = hydra.lib.maps.Lookup.apply(
              "@first",
              (obj));
            hydra.util.Maybe<hydra.json.model.Value> secondJson = hydra.lib.maps.Lookup.apply(
              "@second",
              (obj));
            return hydra.lib.maybes.Maybe.apply(
              (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left("missing @first in pair"))),
              (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<String, hydra.core.Term>>) (fj -> hydra.lib.maybes.Maybe.apply(
                (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left("missing @second in pair"))),
                (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<String, hydra.core.Term>>) (sj -> {
                  hydra.util.Either<String, hydra.core.Term> decodedFirst = hydra.json.decode.Decode.fromJson(
                    (types),
                    (firstType),
                    (fj));
                  hydra.util.Either<String, hydra.core.Term> decodedSecond = hydra.json.decode.Decode.fromJson(
                    (types),
                    (secondType),
                    (sj));
                  return hydra.lib.eithers.Either.apply(
                    (java.util.function.Function<String, hydra.util.Either<String, hydra.core.Term>>) (err -> (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left((err))))),
                    (java.util.function.Function<hydra.core.Term, hydra.util.Either<String, hydra.core.Term>>) (f -> hydra.lib.eithers.Map.apply(
                      (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (s -> new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>((f), (s)))))),
                      (decodedSecond))),
                    (decodedFirst));
                }),
                (secondJson))),
              (firstJson));
          }),
          (objResult));
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.Type.Either et) {
        hydra.core.Type leftType = (((et)).value).left;
        hydra.util.Either<String, java.util.Map<String, hydra.json.model.Value>> objResult = hydra.json.decode.Decode.expectObject((value));
        hydra.core.Type rightType = (((et)).value).right;
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, hydra.util.Either<String, hydra.core.Term>>) (err -> (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left((err))))),
          (java.util.function.Function<java.util.Map<String, hydra.json.model.Value>, hydra.util.Either<String, hydra.core.Term>>) (obj -> {
            hydra.util.Maybe<hydra.json.model.Value> leftJson = hydra.lib.maps.Lookup.apply(
              "@left",
              (obj));
            hydra.util.Maybe<hydra.json.model.Value> rightJson = hydra.lib.maps.Lookup.apply(
              "@right",
              (obj));
            return hydra.lib.maybes.Maybe.apply(
              hydra.lib.maybes.Maybe.apply(
                (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left("expected @left or @right in Either"))),
                (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<String, hydra.core.Term>>) (rj -> {
                  hydra.util.Either<String, hydra.core.Term> decoded = hydra.json.decode.Decode.fromJson(
                    (types),
                    (rightType),
                    (rj));
                  return hydra.lib.eithers.Map.apply(
                    (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v -> new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right((v)))))),
                    (decoded));
                }),
                (rightJson)),
              (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<String, hydra.core.Term>>) (lj -> {
                hydra.util.Either<String, hydra.core.Term> decoded = hydra.json.decode.Decode.fromJson(
                  (types),
                  (leftType),
                  (lj));
                return hydra.lib.eithers.Map.apply(
                  (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v -> new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left((v)))))),
                  (decoded));
              }),
              (leftJson));
          }),
          (objResult));
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.Type.Variable name) {
        hydra.util.Maybe<hydra.core.Type> lookedUp = hydra.lib.maps.Lookup.apply(
          ((name)).value,
          (types));
        return hydra.lib.maybes.Maybe.apply(
          (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left(hydra.lib.strings.Cat.apply(java.util.List.of(
            "unknown type variable: ",
            (((name)).value).value))))),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.core.Term>>) (resolvedType -> hydra.json.decode.Decode.fromJson(
            (types),
            (resolvedType),
            (value))),
          (lookedUp));
      }
    });
  }
  
  static hydra.util.Either<String, hydra.core.Term> decodeLiteral(hydra.core.LiteralType lt, hydra.json.model.Value value) {
    return ((lt)).accept(new hydra.core.LiteralType.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.LiteralType.Binary ignored) {
        hydra.util.Either<String, String> strResult = hydra.json.decode.Decode.expectString((value));
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<String, hydra.core.Term>) (s -> new hydra.core.Term.Literal(new hydra.core.Literal.Binary(hydra.lib.literals.StringToBinary.apply((s))))),
          (strResult));
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.LiteralType.Boolean_ ignored) {
        return ((value)).accept(new hydra.json.model.Value.PartialVisitor<>() {
          @Override
          public hydra.util.Either<String, hydra.core.Term> otherwise(hydra.json.model.Value instance) {
            return (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left("expected boolean")));
          }
          
          @Override
          public hydra.util.Either<String, hydra.core.Term> visit(hydra.json.model.Value.Boolean_ b) {
            return (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>right(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(((b)).value)))));
          }
        });
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.LiteralType.Float_ ft) {
        return hydra.json.decode.Decode.decodeFloat(
          ((ft)).value,
          (value));
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.LiteralType.Integer_ it) {
        return hydra.json.decode.Decode.decodeInteger(
          ((it)).value,
          (value));
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.LiteralType.String_ ignored) {
        hydra.util.Either<String, String> strResult = hydra.json.decode.Decode.expectString((value));
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<String, hydra.core.Term>) (s -> new hydra.core.Term.Literal(new hydra.core.Literal.String_((s)))),
          (strResult));
      }
    });
  }
  
  static hydra.util.Either<String, hydra.core.Term> decodeFloat(hydra.core.FloatType ft, hydra.json.model.Value value) {
    return ((ft)).accept(new hydra.core.FloatType.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.FloatType.Bigfloat ignored) {
        hydra.util.Either<String, java.math.BigDecimal> numResult = hydra.json.decode.Decode.expectNumber((value));
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<java.math.BigDecimal, hydra.core.Term>) (n -> new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Bigfloat((n))))),
          (numResult));
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.FloatType.Float32 ignored) {
        hydra.util.Either<String, String> strResult = hydra.json.decode.Decode.expectString((value));
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, hydra.util.Either<String, hydra.core.Term>>) (err -> (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left((err))))),
          (java.util.function.Function<String, hydra.util.Either<String, hydra.core.Term>>) (s -> {
            hydra.util.Maybe<Float> parsed = hydra.lib.literals.ReadFloat32.apply((s));
            return hydra.lib.maybes.Maybe.apply(
              (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left(hydra.lib.strings.Cat.apply(java.util.List.of(
                "invalid float32: ",
                (s)))))),
              (java.util.function.Function<Float, hydra.util.Either<String, hydra.core.Term>>) (v -> (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>right(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((v)))))))),
              (parsed));
          }),
          (strResult));
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.FloatType.Float64 ignored) {
        hydra.util.Either<String, java.math.BigDecimal> numResult = hydra.json.decode.Decode.expectNumber((value));
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<java.math.BigDecimal, hydra.core.Term>) (n -> new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(hydra.lib.literals.BigfloatToFloat64.apply((n)))))),
          (numResult));
      }
    });
  }
  
  static hydra.util.Either<String, hydra.core.Term> decodeInteger(hydra.core.IntegerType it, hydra.json.model.Value value) {
    return ((it)).accept(new hydra.core.IntegerType.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.IntegerType.Bigint ignored) {
        hydra.util.Either<String, String> strResult = hydra.json.decode.Decode.expectString((value));
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, hydra.util.Either<String, hydra.core.Term>>) (err -> (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left((err))))),
          (java.util.function.Function<String, hydra.util.Either<String, hydra.core.Term>>) (s -> {
            hydra.util.Maybe<java.math.BigInteger> parsed = hydra.lib.literals.ReadBigint.apply((s));
            return hydra.lib.maybes.Maybe.apply(
              (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left(hydra.lib.strings.Cat.apply(java.util.List.of(
                "invalid bigint: ",
                (s)))))),
              (java.util.function.Function<java.math.BigInteger, hydra.util.Either<String, hydra.core.Term>>) (v -> (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>right(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Bigint((v)))))))),
              (parsed));
          }),
          (strResult));
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.IntegerType.Int64 ignored) {
        hydra.util.Either<String, String> strResult = hydra.json.decode.Decode.expectString((value));
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, hydra.util.Either<String, hydra.core.Term>>) (err -> (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left((err))))),
          (java.util.function.Function<String, hydra.util.Either<String, hydra.core.Term>>) (s -> {
            hydra.util.Maybe<Long> parsed = hydra.lib.literals.ReadInt64.apply((s));
            return hydra.lib.maybes.Maybe.apply(
              (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left(hydra.lib.strings.Cat.apply(java.util.List.of(
                "invalid int64: ",
                (s)))))),
              (java.util.function.Function<Long, hydra.util.Either<String, hydra.core.Term>>) (v -> (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>right(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int64((v)))))))),
              (parsed));
          }),
          (strResult));
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.IntegerType.Uint32 ignored) {
        hydra.util.Either<String, String> strResult = hydra.json.decode.Decode.expectString((value));
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, hydra.util.Either<String, hydra.core.Term>>) (err -> (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left((err))))),
          (java.util.function.Function<String, hydra.util.Either<String, hydra.core.Term>>) (s -> {
            hydra.util.Maybe<Long> parsed = hydra.lib.literals.ReadUint32.apply((s));
            return hydra.lib.maybes.Maybe.apply(
              (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left(hydra.lib.strings.Cat.apply(java.util.List.of(
                "invalid uint32: ",
                (s)))))),
              (java.util.function.Function<Long, hydra.util.Either<String, hydra.core.Term>>) (v -> (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>right(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Uint32((v)))))))),
              (parsed));
          }),
          (strResult));
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.IntegerType.Uint64 ignored) {
        hydra.util.Either<String, String> strResult = hydra.json.decode.Decode.expectString((value));
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, hydra.util.Either<String, hydra.core.Term>>) (err -> (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left((err))))),
          (java.util.function.Function<String, hydra.util.Either<String, hydra.core.Term>>) (s -> {
            hydra.util.Maybe<java.math.BigInteger> parsed = hydra.lib.literals.ReadUint64.apply((s));
            return hydra.lib.maybes.Maybe.apply(
              (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left(hydra.lib.strings.Cat.apply(java.util.List.of(
                "invalid uint64: ",
                (s)))))),
              (java.util.function.Function<java.math.BigInteger, hydra.util.Either<String, hydra.core.Term>>) (v -> (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>right(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Uint64((v)))))))),
              (parsed));
          }),
          (strResult));
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.IntegerType.Int8 ignored) {
        hydra.util.Either<String, java.math.BigDecimal> numResult = hydra.json.decode.Decode.expectNumber((value));
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<java.math.BigDecimal, hydra.core.Term>) (n -> new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int8(hydra.lib.literals.BigintToInt8.apply(hydra.lib.literals.BigfloatToBigint.apply((n))))))),
          (numResult));
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.IntegerType.Int16 ignored) {
        hydra.util.Either<String, java.math.BigDecimal> numResult = hydra.json.decode.Decode.expectNumber((value));
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<java.math.BigDecimal, hydra.core.Term>) (n -> new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int16(hydra.lib.literals.BigintToInt16.apply(hydra.lib.literals.BigfloatToBigint.apply((n))))))),
          (numResult));
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.IntegerType.Int32 ignored) {
        hydra.util.Either<String, java.math.BigDecimal> numResult = hydra.json.decode.Decode.expectNumber((value));
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<java.math.BigDecimal, hydra.core.Term>) (n -> new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(hydra.lib.literals.BigintToInt32.apply(hydra.lib.literals.BigfloatToBigint.apply((n))))))),
          (numResult));
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.IntegerType.Uint8 ignored) {
        hydra.util.Either<String, java.math.BigDecimal> numResult = hydra.json.decode.Decode.expectNumber((value));
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<java.math.BigDecimal, hydra.core.Term>) (n -> new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Uint8(hydra.lib.literals.BigintToUint8.apply(hydra.lib.literals.BigfloatToBigint.apply((n))))))),
          (numResult));
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.IntegerType.Uint16 ignored) {
        hydra.util.Either<String, java.math.BigDecimal> numResult = hydra.json.decode.Decode.expectNumber((value));
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<java.math.BigDecimal, hydra.core.Term>) (n -> new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Uint16(hydra.lib.literals.BigintToUint16.apply(hydra.lib.literals.BigfloatToBigint.apply((n))))))),
          (numResult));
      }
    });
  }
  
  static hydra.util.Either<String, String> expectString(hydra.json.model.Value value) {
    return ((value)).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, String> otherwise(hydra.json.model.Value instance) {
        return (hydra.util.Either<String, String>) ((hydra.util.Either<String, String>) (hydra.util.Either.<String, String>left("expected string")));
      }
      
      @Override
      public hydra.util.Either<String, String> visit(hydra.json.model.Value.String_ s) {
        return (hydra.util.Either<String, String>) ((hydra.util.Either<String, String>) (hydra.util.Either.<String, String>right(((s)).value)));
      }
    });
  }
  
  static hydra.util.Either<String, java.util.List<hydra.json.model.Value>> expectArray(hydra.json.model.Value value) {
    return ((value)).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, java.util.List<hydra.json.model.Value>> otherwise(hydra.json.model.Value instance) {
        return (hydra.util.Either<String, java.util.List<hydra.json.model.Value>>) ((hydra.util.Either<String, java.util.List<hydra.json.model.Value>>) (hydra.util.Either.<String, java.util.List<hydra.json.model.Value>>left("expected array")));
      }
      
      @Override
      public hydra.util.Either<String, java.util.List<hydra.json.model.Value>> visit(hydra.json.model.Value.Array arr) {
        return (hydra.util.Either<String, java.util.List<hydra.json.model.Value>>) ((hydra.util.Either<String, java.util.List<hydra.json.model.Value>>) (hydra.util.Either.<String, java.util.List<hydra.json.model.Value>>right(((arr)).value)));
      }
    });
  }
  
  static hydra.util.Either<String, java.util.Map<String, hydra.json.model.Value>> expectObject(hydra.json.model.Value value) {
    return ((value)).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, java.util.Map<String, hydra.json.model.Value>> otherwise(hydra.json.model.Value instance) {
        return (hydra.util.Either<String, java.util.Map<String, hydra.json.model.Value>>) ((hydra.util.Either<String, java.util.Map<String, hydra.json.model.Value>>) (hydra.util.Either.<String, java.util.Map<String, hydra.json.model.Value>>left("expected object")));
      }
      
      @Override
      public hydra.util.Either<String, java.util.Map<String, hydra.json.model.Value>> visit(hydra.json.model.Value.Object_ obj) {
        return (hydra.util.Either<String, java.util.Map<String, hydra.json.model.Value>>) ((hydra.util.Either<String, java.util.Map<String, hydra.json.model.Value>>) (hydra.util.Either.<String, java.util.Map<String, hydra.json.model.Value>>right(((obj)).value)));
      }
    });
  }
  
  static hydra.util.Either<String, java.math.BigDecimal> expectNumber(hydra.json.model.Value value) {
    return ((value)).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, java.math.BigDecimal> otherwise(hydra.json.model.Value instance) {
        return (hydra.util.Either<String, java.math.BigDecimal>) ((hydra.util.Either<String, java.math.BigDecimal>) (hydra.util.Either.<String, java.math.BigDecimal>left("expected number")));
      }
      
      @Override
      public hydra.util.Either<String, java.math.BigDecimal> visit(hydra.json.model.Value.Number_ n) {
        return (hydra.util.Either<String, java.math.BigDecimal>) ((hydra.util.Either<String, java.math.BigDecimal>) (hydra.util.Either.<String, java.math.BigDecimal>right(((n)).value)));
      }
    });
  }
}
