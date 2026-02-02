// Note: this is an automatically generated file. Do not edit.

package hydra.json.encode;

/**
 * JSON encoding for Hydra terms. Converts Terms to JSON Values using Either for error handling.
 */
public interface Encode {
  static hydra.util.Either<String, hydra.json.model.Value> toJson(hydra.core.Term term) {
    hydra.core.Term stripped = hydra.rewriting.Rewriting.deannotateTerm((term));
    return ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.json.model.Value> otherwise(hydra.core.Term instance) {
        return (hydra.util.Either<String, hydra.json.model.Value>) ((hydra.util.Either<String, hydra.json.model.Value>) (hydra.util.Either.<String, hydra.json.model.Value>left(hydra.lib.strings.Cat.apply(java.util.List.of(
          "unsupported term variant for JSON encoding: ",
          hydra.show.core.Core.term((term)))))));
      }
      
      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Literal lit) {
        return hydra.json.encode.Encode.encodeLiteral(((lit)).value);
      }
      
      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.List terms) {
        hydra.util.Either<String, java.util.List<hydra.json.model.Value>> results = hydra.lib.eithers.MapList.apply(
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<String, hydra.json.model.Value>>) (t -> hydra.json.encode.Encode.toJson((t))),
          ((terms)).value);
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<java.util.List<hydra.json.model.Value>, hydra.json.model.Value>) (vs -> new hydra.json.model.Value.Array((vs))),
          (results));
      }
      
      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Set vals) {
        java.util.List<hydra.core.Term> terms = hydra.lib.sets.ToList.apply(((vals)).value);
        hydra.util.Either<String, java.util.List<hydra.json.model.Value>> results = hydra.lib.eithers.MapList.apply(
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<String, hydra.json.model.Value>>) (t -> hydra.json.encode.Encode.toJson((t))),
          (terms));
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<java.util.List<hydra.json.model.Value>, hydra.json.model.Value>) (vs -> new hydra.json.model.Value.Array((vs))),
          (results));
      }
      
      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Maybe opt) {
        return hydra.lib.maybes.Maybe.apply(
          (hydra.util.Either<String, hydra.json.model.Value>) ((hydra.util.Either<String, hydra.json.model.Value>) (hydra.util.Either.<String, hydra.json.model.Value>right(new hydra.json.model.Value.Null(true)))),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<String, hydra.json.model.Value>>) (v -> {
            hydra.util.Either<String, hydra.json.model.Value> encodedMaybe = hydra.json.encode.Encode.toJson((v));
            return hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.json.model.Value, hydra.json.model.Value>) (encoded -> new hydra.json.model.Value.Array(java.util.List.of((encoded)))),
              (encodedMaybe));
          }),
          ((opt)).value);
      }
      
      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Record r) {
        java.util.function.Function<hydra.core.Field, hydra.util.Either<String, hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>>> encodeField = (java.util.function.Function<hydra.core.Field, hydra.util.Either<String, hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>>>) (f -> {
          hydra.core.Term fterm = ((f)).term;
          hydra.util.Either<String, hydra.json.model.Value> encodedField = hydra.json.encode.Encode.toJson((fterm));
          String fname = (((f)).name).value;
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<hydra.json.model.Value, hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>>) (v -> (hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) ((hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) (new hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>((fname), (v))))),
            (encodedField));
        });
        java.util.List<hydra.core.Field> fields = (((r)).value).fields;
        hydra.util.Either<String, java.util.List<hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>>> encodedFields = hydra.lib.eithers.MapList.apply(
          (encodeField),
          (fields));
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>>, hydra.json.model.Value>) (fs -> new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply((fs)))),
          (encodedFields));
      }
      
      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Union inj) {
        hydra.core.Field field = (((inj)).value).field;
        hydra.core.Term fterm = ((field)).term;
        hydra.util.Either<String, hydra.json.model.Value> encodedUnion = hydra.json.encode.Encode.toJson((fterm));
        String fname = (((field)).name).value;
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.json.model.Value, hydra.json.model.Value>) (v -> new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(java.util.List.of((hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) ((hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) (new hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>((fname), (v)))))))),
          (encodedUnion));
      }
      
      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Unit ignored) {
        return (hydra.util.Either<String, hydra.json.model.Value>) ((hydra.util.Either<String, hydra.json.model.Value>) (hydra.util.Either.<String, hydra.json.model.Value>right(new hydra.json.model.Value.Object_((java.util.Map<String, hydra.json.model.Value>) ((java.util.Map<String, hydra.json.model.Value>) (hydra.lib.maps.Empty.<String, hydra.json.model.Value>apply()))))));
      }
      
      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Wrap wt) {
        return hydra.json.encode.Encode.toJson((((wt)).value).body);
      }
      
      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Map m) {
        java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.json.model.Value>> encodeEntry = (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.json.model.Value>>) (kv -> {
          hydra.core.Term k = hydra.lib.pairs.First.apply((kv));
          hydra.util.Either<String, hydra.json.model.Value> encodedK = hydra.json.encode.Encode.toJson((k));
          hydra.core.Term v = hydra.lib.pairs.Second.apply((kv));
          hydra.util.Either<String, hydra.json.model.Value> encodedV = hydra.json.encode.Encode.toJson((v));
          return hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, hydra.util.Either<String, hydra.json.model.Value>>) (err -> (hydra.util.Either<String, hydra.json.model.Value>) ((hydra.util.Either<String, hydra.json.model.Value>) (hydra.util.Either.<String, hydra.json.model.Value>left((err))))),
            (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<String, hydra.json.model.Value>>) (ek -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.json.model.Value, hydra.json.model.Value>) (ev -> new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(java.util.List.of(
                (hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) ((hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) (new hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>("@key", (ek)))),
                (hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) ((hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) (new hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>("@value", (ev)))))))),
              (encodedV))),
            (encodedK));
        });
        hydra.util.Either<String, java.util.List<hydra.json.model.Value>> entries = hydra.lib.eithers.MapList.apply(
          (encodeEntry),
          hydra.lib.maps.ToList.apply(((m)).value));
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<java.util.List<hydra.json.model.Value>, hydra.json.model.Value>) (es -> new hydra.json.model.Value.Array((es))),
          (entries));
      }
      
      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Pair p) {
        hydra.core.Term first = hydra.lib.pairs.First.apply(((p)).value);
        hydra.util.Either<String, hydra.json.model.Value> encodedFirst = hydra.json.encode.Encode.toJson((first));
        hydra.core.Term second = hydra.lib.pairs.Second.apply(((p)).value);
        hydra.util.Either<String, hydra.json.model.Value> encodedSecond = hydra.json.encode.Encode.toJson((second));
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, hydra.util.Either<String, hydra.json.model.Value>>) (err -> (hydra.util.Either<String, hydra.json.model.Value>) ((hydra.util.Either<String, hydra.json.model.Value>) (hydra.util.Either.<String, hydra.json.model.Value>left((err))))),
          (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<String, hydra.json.model.Value>>) (ef -> hydra.lib.eithers.Map.apply(
            (java.util.function.Function<hydra.json.model.Value, hydra.json.model.Value>) (es -> new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(java.util.List.of(
              (hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) ((hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) (new hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>("@first", (ef)))),
              (hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) ((hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) (new hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>("@second", (es)))))))),
            (encodedSecond))),
          (encodedFirst));
      }
      
      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.core.Term.Either e) {
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<String, hydra.json.model.Value>>) (l -> {
            hydra.util.Either<String, hydra.json.model.Value> encodedL = hydra.json.encode.Encode.toJson((l));
            return hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.json.model.Value, hydra.json.model.Value>) (v -> new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(java.util.List.of((hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) ((hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) (new hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>("@left", (v)))))))),
              (encodedL));
          }),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<String, hydra.json.model.Value>>) (r -> {
            hydra.util.Either<String, hydra.json.model.Value> encodedR = hydra.json.encode.Encode.toJson((r));
            return hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.json.model.Value, hydra.json.model.Value>) (v -> new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(java.util.List.of((hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) ((hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) (new hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>("@right", (v)))))))),
              (encodedR));
          }),
          ((e)).value);
      }
    });
  }
  
  static <T0> hydra.util.Either<T0, hydra.json.model.Value> encodeLiteral(hydra.core.Literal lit) {
    return ((lit)).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.Literal.Binary b) {
        return (hydra.util.Either<T0, hydra.json.model.Value>) ((hydra.util.Either<T0, hydra.json.model.Value>) (hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.String_(hydra.lib.literals.BinaryToString.apply(((b)).value)))));
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.Literal.Boolean_ b) {
        return (hydra.util.Either<T0, hydra.json.model.Value>) ((hydra.util.Either<T0, hydra.json.model.Value>) (hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.Boolean_(((b)).value))));
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.Literal.Float_ f) {
        return hydra.json.encode.Encode.<T0>encodeFloat(((f)).value);
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.Literal.Integer_ i) {
        return hydra.json.encode.Encode.<T0>encodeInteger(((i)).value);
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.Literal.String_ s) {
        return (hydra.util.Either<T0, hydra.json.model.Value>) ((hydra.util.Either<T0, hydra.json.model.Value>) (hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.String_(((s)).value))));
      }
    });
  }
  
  static <T0> hydra.util.Either<T0, hydra.json.model.Value> encodeFloat(hydra.core.FloatValue fv) {
    return ((fv)).accept(new hydra.core.FloatValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.FloatValue.Bigfloat bf) {
        return (hydra.util.Either<T0, hydra.json.model.Value>) ((hydra.util.Either<T0, hydra.json.model.Value>) (hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.Number_(((bf)).value))));
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.FloatValue.Float32 f) {
        return (hydra.util.Either<T0, hydra.json.model.Value>) ((hydra.util.Either<T0, hydra.json.model.Value>) (hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.String_(hydra.lib.literals.ShowFloat32.apply(((f)).value)))));
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.FloatValue.Float64 f) {
        return (hydra.util.Either<T0, hydra.json.model.Value>) ((hydra.util.Either<T0, hydra.json.model.Value>) (hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.Number_(hydra.lib.literals.Float64ToBigfloat.apply(((f)).value)))));
      }
    });
  }
  
  static <T0> hydra.util.Either<T0, hydra.json.model.Value> encodeInteger(hydra.core.IntegerValue iv) {
    return ((iv)).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.IntegerValue.Bigint bi) {
        return (hydra.util.Either<T0, hydra.json.model.Value>) ((hydra.util.Either<T0, hydra.json.model.Value>) (hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.String_(hydra.lib.literals.ShowBigint.apply(((bi)).value)))));
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.IntegerValue.Int64 i) {
        return (hydra.util.Either<T0, hydra.json.model.Value>) ((hydra.util.Either<T0, hydra.json.model.Value>) (hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.String_(hydra.lib.literals.ShowInt64.apply(((i)).value)))));
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.IntegerValue.Uint32 i) {
        return (hydra.util.Either<T0, hydra.json.model.Value>) ((hydra.util.Either<T0, hydra.json.model.Value>) (hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.String_(hydra.lib.literals.ShowUint32.apply(((i)).value)))));
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.IntegerValue.Uint64 i) {
        return (hydra.util.Either<T0, hydra.json.model.Value>) ((hydra.util.Either<T0, hydra.json.model.Value>) (hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.String_(hydra.lib.literals.ShowUint64.apply(((i)).value)))));
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.IntegerValue.Int8 i) {
        return (hydra.util.Either<T0, hydra.json.model.Value>) ((hydra.util.Either<T0, hydra.json.model.Value>) (hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.Number_(hydra.lib.literals.BigintToBigfloat.apply(hydra.lib.literals.Int8ToBigint.apply(((i)).value))))));
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.IntegerValue.Int16 i) {
        return (hydra.util.Either<T0, hydra.json.model.Value>) ((hydra.util.Either<T0, hydra.json.model.Value>) (hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.Number_(hydra.lib.literals.BigintToBigfloat.apply(hydra.lib.literals.Int16ToBigint.apply(((i)).value))))));
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.IntegerValue.Int32 i) {
        return (hydra.util.Either<T0, hydra.json.model.Value>) ((hydra.util.Either<T0, hydra.json.model.Value>) (hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.Number_(hydra.lib.literals.BigintToBigfloat.apply(hydra.lib.literals.Int32ToBigint.apply(((i)).value))))));
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.IntegerValue.Uint8 i) {
        return (hydra.util.Either<T0, hydra.json.model.Value>) ((hydra.util.Either<T0, hydra.json.model.Value>) (hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.Number_(hydra.lib.literals.BigintToBigfloat.apply(hydra.lib.literals.Uint8ToBigint.apply(((i)).value))))));
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.IntegerValue.Uint16 i) {
        return (hydra.util.Either<T0, hydra.json.model.Value>) ((hydra.util.Either<T0, hydra.json.model.Value>) (hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.Number_(hydra.lib.literals.BigintToBigfloat.apply(hydra.lib.literals.Uint16ToBigint.apply(((i)).value))))));
      }
    });
  }
}
