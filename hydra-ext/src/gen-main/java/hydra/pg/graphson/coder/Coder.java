// Note: this is an automatically generated file. Do not edit.

package hydra.pg.graphson.coder;

/**
 * Encoding functions for converting GraphSON syntax to JSON.
 */
public interface Coder {
  static hydra.json.model.Value adjacentEdgeToJson(Boolean out, hydra.pg.graphson.syntax.AdjacentEdge ae) {
    return hydra.pg.graphson.coder.Coder.toJsonObject(hydra.util.ConsList.of(
      (hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) ((hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) (new hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>("id", hydra.util.Maybe.just(hydra.pg.graphson.coder.Coder.valueToJson((ae).id))))),
      (hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) ((hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) (new hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>("inV", hydra.lib.logic.IfElse.lazy(
        out,
        () -> hydra.util.Maybe.just(hydra.pg.graphson.coder.Coder.valueToJson((ae).vertexId)),
        () -> (hydra.util.Maybe<hydra.json.model.Value>) (hydra.util.Maybe.<hydra.json.model.Value>nothing()))))),
      (hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) ((hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) (new hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>("outV", hydra.lib.logic.IfElse.lazy(
        out,
        () -> (hydra.util.Maybe<hydra.json.model.Value>) (hydra.util.Maybe.<hydra.json.model.Value>nothing()),
        () -> hydra.util.Maybe.just(hydra.pg.graphson.coder.Coder.valueToJson((ae).vertexId)))))),
      (hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) ((hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) (new hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>("properties", hydra.pg.graphson.coder.Coder.edgePropertyMapToJson((ae).properties))))));
  }

  static hydra.json.model.Value doubleValueToJson(hydra.pg.graphson.syntax.DoubleValue v1) {
    return (v1).accept(new hydra.pg.graphson.syntax.DoubleValue.PartialVisitor<>() {
      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.DoubleValue.Finite d) {
        return new hydra.json.model.Value.Number_(hydra.lib.literals.Float64ToBigfloat.apply((d).value));
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.DoubleValue.Infinity ignored) {
        return new hydra.json.model.Value.String_("Infinity");
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.DoubleValue.NegativeInfinity ignored) {
        return new hydra.json.model.Value.String_("-Infinity");
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.DoubleValue.NotANumber ignored) {
        return new hydra.json.model.Value.String_("NaN");
      }
    });
  }

  static hydra.util.Maybe<hydra.json.model.Value> edgeMapToJson(Boolean out, hydra.util.PersistentMap<hydra.pg.graphson.syntax.EdgeLabel, hydra.util.ConsList<hydra.pg.graphson.syntax.AdjacentEdge>> m) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.maps.Null.apply(m),
      () -> (hydra.util.Maybe<hydra.json.model.Value>) (hydra.util.Maybe.<hydra.json.model.Value>nothing()),
      () -> hydra.util.Maybe.just(new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Pair<hydra.pg.graphson.syntax.EdgeLabel, hydra.util.ConsList<hydra.pg.graphson.syntax.AdjacentEdge>>, hydra.util.Pair<String, hydra.json.model.Value>>) (p -> (hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>(hydra.lib.pairs.First.apply(p).value, new hydra.json.model.Value.Array(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.pg.graphson.syntax.AdjacentEdge, hydra.json.model.Value>) (v1 -> hydra.pg.graphson.coder.Coder.adjacentEdgeToJson(
            out,
            v1)),
          hydra.lib.pairs.Second.apply(p))))))),
        hydra.lib.maps.ToList.apply(m))))));
  }

  static hydra.util.Maybe<hydra.json.model.Value> edgePropertyMapToJson(hydra.util.PersistentMap<hydra.pg.graphson.syntax.PropertyKey, hydra.pg.graphson.syntax.Value> m) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.maps.Null.apply(m),
      () -> (hydra.util.Maybe<hydra.json.model.Value>) (hydra.util.Maybe.<hydra.json.model.Value>nothing()),
      () -> hydra.util.Maybe.just(new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Pair<hydra.pg.graphson.syntax.PropertyKey, hydra.pg.graphson.syntax.Value>, hydra.util.Pair<String, hydra.json.model.Value>>) (p -> (hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>(hydra.lib.pairs.First.apply(p).value, hydra.pg.graphson.coder.Coder.valueToJson(hydra.lib.pairs.Second.apply(p)))))),
        hydra.lib.maps.ToList.apply(m))))));
  }

  static hydra.json.model.Value floatValueToJson(hydra.pg.graphson.syntax.FloatValue v1) {
    return (v1).accept(new hydra.pg.graphson.syntax.FloatValue.PartialVisitor<>() {
      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.FloatValue.Finite f) {
        return new hydra.json.model.Value.Number_(hydra.lib.literals.Float32ToBigfloat.apply((f).value));
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.FloatValue.Infinity ignored) {
        return new hydra.json.model.Value.String_("Infinity");
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.FloatValue.NegativeInfinity ignored) {
        return new hydra.json.model.Value.String_("-Infinity");
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.FloatValue.NotANumber ignored) {
        return new hydra.json.model.Value.String_("NaN");
      }
    });
  }

  static hydra.json.model.Value mapToJson(hydra.pg.graphson.syntax.Map m) {
    return new hydra.json.model.Value.Array(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.pg.graphson.syntax.ValuePair, hydra.util.ConsList<hydra.json.model.Value>>) (vp -> hydra.util.ConsList.of(
        hydra.pg.graphson.coder.Coder.valueToJson((vp).first),
        hydra.pg.graphson.coder.Coder.valueToJson((vp).second))),
      (m).value)));
  }

  static hydra.json.model.Value toJsonObject(hydra.util.ConsList<hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>> pairs) {
    return new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>, hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>>) (p -> hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.json.model.Value, hydra.util.Pair<String, hydra.json.model.Value>>) (v -> (hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>(hydra.lib.pairs.First.apply(p), v)))),
        hydra.lib.pairs.Second.apply(p))),
      pairs))));
  }

  static hydra.json.model.Value typedValueToJson(String typeName, hydra.json.model.Value valueJson) {
    return hydra.pg.graphson.coder.Coder.toJsonObject(hydra.util.ConsList.of(
      (hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) ((hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) (new hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>("@type", hydra.util.Maybe.just(new hydra.json.model.Value.String_(typeName))))),
      (hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) ((hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) (new hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>("@value", hydra.util.Maybe.just(valueJson))))));
  }

  static hydra.json.model.Value valueToJson(hydra.pg.graphson.syntax.Value v1) {
    return (v1).accept(new hydra.pg.graphson.syntax.Value.PartialVisitor<>() {
      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.Value.BigDecimal bd) {
        return hydra.pg.graphson.coder.Coder.typedValueToJson(
          "g:BigDecimal",
          new hydra.json.model.Value.String_((bd).value.value));
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.Value.BigInteger i) {
        return hydra.pg.graphson.coder.Coder.typedValueToJson(
          "g:BigInteger",
          new hydra.json.model.Value.Number_(hydra.lib.literals.BigintToBigfloat.apply((i).value)));
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.Value.Binary b) {
        return hydra.pg.graphson.coder.Coder.typedValueToJson(
          "g:Binary",
          new hydra.json.model.Value.String_((b).value));
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.Value.Boolean_ b) {
        return new hydra.json.model.Value.Boolean_((b).value);
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.Value.Byte_ b) {
        return hydra.pg.graphson.coder.Coder.typedValueToJson(
          "g:Byte",
          new hydra.json.model.Value.Number_(hydra.lib.literals.BigintToBigfloat.apply(hydra.lib.literals.Uint8ToBigint.apply((b).value))));
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.Value.Char c) {
        return hydra.pg.graphson.coder.Coder.typedValueToJson(
          "g:Char",
          new hydra.json.model.Value.String_(hydra.lib.strings.FromList.apply(hydra.lib.lists.Pure.apply(hydra.lib.literals.BigintToInt32.apply(hydra.lib.literals.Uint32ToBigint.apply((c).value))))));
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.Value.Composite ctv) {
        return hydra.pg.graphson.coder.Coder.typedValueToJson(
          (ctv).value.type.value,
          hydra.pg.graphson.coder.Coder.mapToJson((ctv).value.fields));
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.Value.DateTime dt) {
        return hydra.pg.graphson.coder.Coder.typedValueToJson(
          "g:DateTime",
          new hydra.json.model.Value.String_((dt).value.value));
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.Value.Double_ dv) {
        return hydra.pg.graphson.coder.Coder.typedValueToJson(
          "g:Double",
          hydra.pg.graphson.coder.Coder.doubleValueToJson((dv).value));
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.Value.Duration dur) {
        return hydra.pg.graphson.coder.Coder.typedValueToJson(
          "g:Duration",
          new hydra.json.model.Value.String_((dur).value.value));
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.Value.Float_ fv) {
        return hydra.pg.graphson.coder.Coder.typedValueToJson(
          "g:Float",
          hydra.pg.graphson.coder.Coder.floatValueToJson((fv).value));
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.Value.Integer_ i) {
        return hydra.pg.graphson.coder.Coder.typedValueToJson(
          "g:Int32",
          new hydra.json.model.Value.Number_(hydra.lib.literals.BigintToBigfloat.apply(hydra.lib.literals.Int32ToBigint.apply((i).value))));
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.Value.List vals) {
        return hydra.pg.graphson.coder.Coder.typedValueToJson(
          "g:List",
          new hydra.json.model.Value.Array(hydra.lib.lists.Map.apply(
            hydra.pg.graphson.coder.Coder::valueToJson,
            (vals).value)));
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.Value.Long_ l) {
        return hydra.pg.graphson.coder.Coder.typedValueToJson(
          "g:Long",
          new hydra.json.model.Value.Number_(hydra.lib.literals.BigintToBigfloat.apply(hydra.lib.literals.Int64ToBigint.apply((l).value))));
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.Value.Map m) {
        return hydra.pg.graphson.coder.Coder.typedValueToJson(
          "g:Map",
          hydra.pg.graphson.coder.Coder.mapToJson((m).value));
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.Value.Null ignored) {
        return new hydra.json.model.Value.Null();
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.Value.Primitive ptv) {
        return hydra.pg.graphson.coder.Coder.typedValueToJson(
          "g:PrimitivePdt",
          new hydra.json.model.Value.String_((ptv).value.value));
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.Value.Set vals) {
        return hydra.pg.graphson.coder.Coder.typedValueToJson(
          "g:Set",
          new hydra.json.model.Value.Array(hydra.lib.lists.Map.apply(
            hydra.pg.graphson.coder.Coder::valueToJson,
            (vals).value)));
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.Value.Short_ i) {
        return hydra.pg.graphson.coder.Coder.typedValueToJson(
          "g:Int16",
          new hydra.json.model.Value.Number_(hydra.lib.literals.BigintToBigfloat.apply(hydra.lib.literals.Int16ToBigint.apply((i).value))));
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.Value.String_ s) {
        return new hydra.json.model.Value.String_((s).value);
      }

      @Override
      public hydra.json.model.Value visit(hydra.pg.graphson.syntax.Value.Uuid u) {
        return hydra.pg.graphson.coder.Coder.typedValueToJson(
          "g:UUID",
          new hydra.json.model.Value.String_((u).value.value));
      }
    });
  }

  static hydra.util.Maybe<hydra.json.model.Value> vertexPropertyMapToJson(hydra.util.PersistentMap<hydra.pg.graphson.syntax.PropertyKey, hydra.util.ConsList<hydra.pg.graphson.syntax.VertexPropertyValue>> m) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.maps.Null.apply(m),
      () -> (hydra.util.Maybe<hydra.json.model.Value>) (hydra.util.Maybe.<hydra.json.model.Value>nothing()),
      () -> hydra.util.Maybe.just(new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Pair<hydra.pg.graphson.syntax.PropertyKey, hydra.util.ConsList<hydra.pg.graphson.syntax.VertexPropertyValue>>, hydra.util.Pair<String, hydra.json.model.Value>>) (p -> (hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>(hydra.lib.pairs.First.apply(p).value, new hydra.json.model.Value.Array(hydra.lib.lists.Map.apply(
          hydra.pg.graphson.coder.Coder::vertexPropertyValueToJson,
          hydra.lib.pairs.Second.apply(p))))))),
        hydra.lib.maps.ToList.apply(m))))));
  }

  static hydra.json.model.Value vertexPropertyValueToJson(hydra.pg.graphson.syntax.VertexPropertyValue vpv) {
    return hydra.pg.graphson.coder.Coder.toJsonObject(hydra.util.ConsList.of(
      (hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) ((hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) (new hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>("id", hydra.lib.maybes.Map.apply(
        hydra.pg.graphson.coder.Coder::valueToJson,
        (vpv).id)))),
      (hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) ((hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) (new hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>("value", hydra.util.Maybe.just(hydra.pg.graphson.coder.Coder.valueToJson((vpv).value)))))));
  }

  static hydra.json.model.Value vertexToJson(hydra.pg.graphson.syntax.Vertex v) {
    return hydra.pg.graphson.coder.Coder.toJsonObject(hydra.util.ConsList.of(
      (hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) ((hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) (new hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>("id", hydra.util.Maybe.just(hydra.pg.graphson.coder.Coder.valueToJson((v).id))))),
      (hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) ((hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) (new hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>("label", hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.pg.graphson.syntax.VertexLabel, hydra.json.model.Value>) (lbl -> new hydra.json.model.Value.String_((lbl).value)),
        (v).label)))),
      (hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) ((hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) (new hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>("inE", hydra.pg.graphson.coder.Coder.edgeMapToJson(
        false,
        (v).inEdges)))),
      (hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) ((hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) (new hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>("outE", hydra.pg.graphson.coder.Coder.edgeMapToJson(
        true,
        (v).outEdges)))),
      (hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) ((hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>) (new hydra.util.Pair<String, hydra.util.Maybe<hydra.json.model.Value>>("properties", hydra.pg.graphson.coder.Coder.vertexPropertyMapToJson((v).properties))))));
  }
}
